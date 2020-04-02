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

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import j4r.lang.codetranslator.REnvironment;
import j4r.net.SocketWrapper;
import j4r.net.TCPSocketWrapper;
import j4r.net.server.BasicClient.ClientRequest;

/**
 * The JavaLocalGatewayServer class is a one-to-one local server that makes it possible 
 * to create objects and to execute methods in Java from a non-Java application.
 * @author Mathieu Fortin 
 */
public class JavaLocalGatewayServer extends AbstractServer {
	
	
	class REpiceaBackDoorCancellationThread extends Thread {
		
		private final ServerSocket emergencySocket;
		private final int port;
		
		REpiceaBackDoorCancellationThread(int port) throws IOException {
			super("Back door cancellation thread");
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
					Object request = clientSocket.readObject();
					if (request.toString().equals("emergencyShutdown")) {
						System.exit(1);
					} else if (request.toString().equals("softExit")) {
						emergencySocket.close();
						break;
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
	 * A wrapper for Exception.
	 * @author Mathieu Fortin
	 */
	final class JavaGatewayException extends Exception {
		
		final Throwable nestedException;
		
		JavaGatewayException(Throwable e) {
			super(e);
			this.nestedException = e;
		}

		
		public String toString() {
			return this.getClass().getName() + "_" + nestedException.toString();
		}
	}
	
	private class JavaGatewayClientThread extends ClientThread {

		protected JavaGatewayClientThread(AbstractServer caller, int workerID) {
			super(caller, workerID);
		}

		@Override
		public void run() {
			while(true) {
				try {
					firePropertyChange("status", null, "Waiting");
					socketWrapper = caller.getWaitingClients();
					InetAddress clientAddress = socketWrapper.getInetAddress();
					firePropertyChange("status", null, "Connected to client: " + clientAddress.getHostAddress());		// for TCP the client is known for UDP we are not connected yet TODO: find a way to lock the UDP socket until the connection is set
					
					while (!socketWrapper.isClosed()) {
						try {
							Object somethingInParticular = processRequest();
							if (somethingInParticular != null) {
								if (somethingInParticular.equals(BasicClient.ClientRequest.closeConnection) 
										|| somethingInParticular.equals(BasicClient.ClientRequest.closeConnection.name())) {
									socketWrapper.writeObject(ServerReply.ClosingConnection);
									closeSocket();
									caller.requestShutdown();
									break;
								} else {
									socketWrapper.writeObject(somethingInParticular);
								}
							} else {
								socketWrapper.writeObject(ServerReply.RequestReceivedAndProcessed);
							}
						} catch (Exception e) {		// something wrong happened during the processing of the request
							try {
								if (e instanceof IOException) {	// seems that the connection was lost
									closeSocket();
								} else if (!socketWrapper.isClosed()) {
									if (e instanceof InvocationTargetException) {
										socketWrapper.writeObject(new JavaGatewayException(((InvocationTargetException) e).getTargetException()));
									} else {
										socketWrapper.writeObject(new JavaGatewayException(e));
									}
								}
							} catch (IOException e1) {}
						}
					}
					if (JavaLocalGatewayServer.this.shutdownOnClosedConnection) {
						caller.requestShutdown();
						break;
					}
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}

		@Override
		protected Object processRequest() throws Exception {
			Object crudeRequest = getSocket().readObject();
			if (crudeRequest instanceof ClientRequest) {
				return crudeRequest;
			} else if (crudeRequest instanceof String) {
				String request = (String) crudeRequest;
				if (request.startsWith("time")) {
					long startMillisec = Long.parseLong(request.substring(4));
					long finalTime = System.currentTimeMillis();
					double elapsedTime =  (finalTime - startMillisec);
					System.out.println("Elapsed time single received packet:" + elapsedTime);
				}
				return JavaLocalGatewayServer.this.translator.processCode(request);
			}
			return null;
		}

	}

	protected final REnvironment translator;	
	protected final boolean shutdownOnClosedConnection;
	protected final REpiceaBackDoorCancellationThread backdoorThread;
	protected boolean bypassShutdownForTesting;
	
	/**
	 * Constructor.
	 * @param servConf a ServerConfiguration instance
	 * @param translator an instance that implements the REpiceaCodeTranslator interface
	 * @throws Exception
	 */
	public JavaLocalGatewayServer(ServerConfiguration servConf, REnvironment translator) throws Exception {
		this(servConf, translator, true); // true: the server shuts down when the connection is lost
	}

	/**
	 * This method waits until the head of the queue is non null and returns the socket.
	 * @return a Socket instance
	 * @throws InterruptedException 
	 */
	@Override
	protected synchronized SocketWrapper getWaitingClients() throws InterruptedException {
		SocketWrapper socket = clientQueue.take();
		return socket;
	}

	/**
	 * Hidden constructor for test purpose
	 * @param servConf a ServerConfiguration instance
	 * @param translator an instance that implements the REpiceaCodeTranslator interface
	 * @param shutdownOnClosedConnection by default this parameter is set to true so that if the connection is lost, the server is shutdown.
	 * @throws Exception
	 */
	protected JavaLocalGatewayServer(ServerConfiguration servConf, REnvironment translator, boolean shutdownOnClosedConnection) throws Exception {
//		super(new ServerConfiguration(1, 0, outerPort, null), false); // false: the client is not a Java application
		super(servConf, false);
		this.translator = translator;
		this.shutdownOnClosedConnection = shutdownOnClosedConnection;
		backdoorThread = new REpiceaBackDoorCancellationThread(50000);
	}

	@Override
	protected ClientThread createClientThread(AbstractServer server, int id) {
		return new JavaGatewayClientThread(server, id);
	}

	
	
	@Override
	protected void shutdown(int shutdownCode) {
		if (backdoorThread.isAlive()) {
			backdoorThread.softExit();
		}
		if (bypassShutdownForTesting) {
			return;
		}
		super.shutdown(shutdownCode);
	}
	
	
	
	
}
