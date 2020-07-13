/*
 * This file is part of the j4r library.
 *
 * Copyright (C) 2020 Mathieu Fortin for Canadian Forest Service.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed with the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * Please see the license at http://www.gnu.org/copyleft/lesser.html.
 */
package j4r.net.server;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;

import j4r.app.AbstractGenericEngine;
import j4r.net.SocketWrapper;
import j4r.net.TCPSocketWrapper;
import j4r.net.server.ServerTask.ServerTaskID;

public abstract class AbstractServer extends AbstractGenericEngine implements PropertyChangeListener {

	protected static enum ServerReply {IAmBusyCallBackLater, 
		CallAccepted, 
		ClosingConnection,
		Done,
		SecurityChecked,
		SecurityFailed}

	
	/**
	 * The BackDoorThread class processes the request one by one and close the socket after
	 * each one of them leaving the ServerSocket free to accept other calls. 
	 */
	class BackDoorThread extends Thread {
		
		final ServerSocket emergencySocket;
		final int port;
		
		BackDoorThread(int port) throws IOException {
			super("Back door thread");
			setDaemon(true);
			this.port = port;
			emergencySocket = new ServerSocket(port);
			start();
		}
		
		@Override
		public void run() {
			while (true) {
				SocketWrapper clientSocket = null;
				try {
					clientSocket = new TCPSocketWrapper(emergencySocket.accept(), false);
					clientSocket.writeObject(ServerReply.CallAccepted);
					if (AbstractServer.this.checkSecurity(clientSocket)) {
						Object request = clientSocket.readObject();
						if (request.toString().equals("emergencyShutdown")) {
							System.exit(1);
						} else if (request.toString().equals("softExit")) {
							emergencySocket.close();
							break;
						} else if (request.toString().equals("interrupt")) {
							InetAddress clientAddress = clientSocket.getInetAddress();
							for (ClientThread t : AbstractServer.this.whoIsWorkingForWho.keySet()) {
								InetAddress clientOfThisTread = AbstractServer.this.whoIsWorkingForWho.get(t);
								if (clientOfThisTread.equals(clientAddress)) {
									t.interrupt();
								}
							}
						}
					}
				} catch (IOException e1) {
					e1.printStackTrace();
				} catch (Exception e2) {
					e2.printStackTrace();
				} finally {
					try {
						if (clientSocket != null  && !clientSocket.isClosed()) {
							clientSocket.close();
						}
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}

		protected void softExit() {
			try {
				Socket socket = new Socket(InetAddress.getLoopbackAddress(), port);
				SocketWrapper socketWrapper = new TCPSocketWrapper(socket, false);
				socketWrapper.readObject();
				socketWrapper.writeObject("softExit");
				socketWrapper.close();
			} catch (Exception e) {}
		}
	}

	/**
	 * This internal class handles the calls and stores these in the queue.
	 * @author Mathieu Fortin
	 */
	class CallReceiverThread extends Thread {

		private boolean shutdownCall;
		final ServerSocket serverSocket;
		final LinkedBlockingQueue<SocketWrapper> clientQueue;
		final int id;
		
		/**
		 * General constructor. 
		 * @param serverSocket a ServerSocket instance if null then the protocol is assumed to be UDP with a single client
		 * @param clientQueue
		 * @param maxNumberOfWaitingClients
		 */
		private CallReceiverThread(ServerSocket serverSocket, int id) {
			this.serverSocket = serverSocket;
			this.id = id;
			clientQueue = new LinkedBlockingQueue<SocketWrapper>();
			shutdownCall = false;
			setName("Answering call thread " + this.id);
		}
		
		/*
		 * The swingworker
		 */
		@Override
		public void run() {
			try {
				while (!shutdownCall) {
					SocketWrapper clientSocket = new TCPSocketWrapper(serverSocket.accept(), AbstractServer.this.isCallerAJavaApplication);
					clientSocket.writeObject(ServerReply.CallAccepted);
					if (AbstractServer.this.checkSecurity(clientSocket)) {
						clientQueue.add(clientSocket);
					}
				}
				System.out.println("Call receiver thread shut down");
			} catch (Exception e) {
				if (!shutdownCall) {
					e.printStackTrace();
				}
			} finally {
				try {
					if (serverSocket != null) {
						serverSocket.close();
					}	
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}


	private ArrayList<ClientThread> clientThreads;
	protected final List<CallReceiverThread> callReceiverThreads;
	protected final ConcurrentHashMap<ClientThread, InetAddress> whoIsWorkingForWho;

	protected final boolean isCallerAJavaApplication;
	
	private final ServerConfiguration configuration;

	private List<PropertyChangeListener> listeners;

	/**
	 * Constructor.
	 * @param configuration a ServerConfiguration instance that defines the number of threads, the reference path and the filename of the exception rules
	 * @param isCallerAJavaApplication true if the client is a Java app 
	 * @throws Exception
	 */
	protected AbstractServer(ServerConfiguration configuration, boolean isCallerAJavaApplication) throws Exception {
		this.configuration = configuration;
		this.isCallerAJavaApplication = isCallerAJavaApplication;
		clientThreads = new ArrayList<ClientThread>();
		this.whoIsWorkingForWho = new ConcurrentHashMap<ClientThread, InetAddress>();
//		clientQueue = new LinkedBlockingQueue<SocketWrapper>();
		callReceiverThreads = new ArrayList<CallReceiverThread>();
		try {
			List<ServerSocket> serverSockets = configuration.createServerSockets();
			int i = 1;
			for (ServerSocket ss : serverSockets) {
				CallReceiverThread crt = new CallReceiverThread(ss, i);
				callReceiverThreads.add(crt);
				for (int j = 1; j <= configuration.numberOfClientThreadsPerReceiver; j++) {
					clientThreads.add(createClientThread(crt, i * 1000 + j));		// i + 1 serves as id
				}
				i++;
			}
		} catch (IOException e) {
			e.printStackTrace();
			throw new Exception("Unable to initialize the server");
		}
		listeners = new CopyOnWriteArrayList<PropertyChangeListener>();
	}

	
	protected boolean checkSecurity(SocketWrapper clientSocket) {
		if (configuration.isLocal) {
			try {
				Object obj = clientSocket.readObject();
				int key = Integer.parseInt(obj.toString());
				if (configuration.key == key) {
					clientSocket.writeObject(ServerReply.SecurityChecked);
					return true;
				} else {
					clientSocket.writeObject(ServerReply.SecurityFailed);
					return false;
				}
			} catch (Exception e) {
				try {
					clientSocket.writeObject(e);
				} catch (IOException e1) {
					e1.printStackTrace();
				}
				return false;
			}
		} else {
			return true;	// for web server 
		}
	}


	protected abstract ClientThread createClientThread(CallReceiverThread receiverThread, int id);
		

//	/**
//	 * This method waits until the head of the queue is non null and returns the socket.
//	 * @return a Socket instance
//	 * @throws InterruptedException 
//	 */
//	protected synchronized SocketWrapper getWaitingClients() throws InterruptedException {
//		SocketWrapper socket = clientQueue.take();
//		return socket;
//	}

	/**
	 * This method starts the worker thread, which listens to the clients in the queue.
	 */
	protected void listenToClients() {	
		for (ClientThread thread : clientThreads) {
			thread.start();
		}
	}

	/**
	 * This method starts the callReceiver thread that handles the call from the clients.
	 * @throws ExecutionException
	 * @throws InterruptedException
	 */
	protected void startReceiverThread() throws ExecutionException, InterruptedException {
		System.out.println("Server starting");
		listenToClients();
		for (CallReceiverThread t : callReceiverThreads) {
			t.start();
		}
//		callReceiver.join();
//		System.out.println("Server shutting down");
	}


	protected ServerConfiguration getConfiguration() {
		return configuration;
	}


	/**
	 * This method returns the vector of client threads.
	 * @return a Vector of ClientThread instances
	 */
	protected List<ClientThread> getClientThreads() {return clientThreads;}


	protected void closeAndRestartTheseThreads(Collection<ClientThread> connectionsToBeClosed) {
		for (ClientThread thread : connectionsToBeClosed) {
			try {
				thread.getSocket().close();
				thread.restartAction();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}


	@Override
	protected void firstTasksToDo() {
		addTask(new ServerTask(ServerTaskID.StartReceiverThread, this));
		if (configuration.isLocalServer()) {
			addTask(new ServerTask(ServerTaskID.CreateFileInfo, this));
		}
	}

	public void addPropertyChangeListener(PropertyChangeListener listener) {
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}
	
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		listeners.remove(listener);
	}
	
	/*
	 * Just to extend the visibility
	 */
	protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
		PropertyChangeEvent evt = new PropertyChangeEvent("Server event", propertyName, oldValue, newValue);
		for (PropertyChangeListener listener : listeners) {
			listener.propertyChange(evt);
		}
	}


	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		String propertyName = evt.getPropertyName();
		if (propertyName.equals("shutdownServer")) {
			requestShutdown();
		} 
	}

	@Override
	public void requestShutdown() {
		for (CallReceiverThread t : callReceiverThreads) {
			t.shutdownCall = true;
			t.clientQueue.clear();
			try {
				if (t.serverSocket != null) {
					t.serverSocket.close();
				}
			} catch (IOException e) {
				t.interrupt();
			} finally {
				try {
					t.join(5000);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		super.requestShutdown();
	}


	/*
	 * This method can be overriden and left empty for webserver.
	 * @throws IOException
	 */
	protected abstract void createFileInfoForLocalServer() throws IOException;
	

}
