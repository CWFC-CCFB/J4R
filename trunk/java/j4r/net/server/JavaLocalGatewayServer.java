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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.util.concurrent.ConcurrentHashMap;

import j4r.lang.codetranslator.REnvironment;
import j4r.net.server.BasicClient.ClientRequest;

/**
 * The JavaLocalGatewayServer class is a one-to-one local server that makes it possible 
 * to create objects and to execute methods in Java from a non-Java application.
 * @author Mathieu Fortin 
 */
public class JavaLocalGatewayServer extends AbstractServer {
	
	
	public static final String EXTENSION = "-ext";
	public static final String PORT = "-ports"; 
	public static final String BACKDOORPORT = "-backdoorport";
	public static final String NB_PORTS = "-nbports";
	public static final String WD = "-wd";
	public static final String MEMORY = "-mem";
	public static final String PortSplitter = ":";
	public static final String DEBUG = "-debug";

	
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

		REnvironment translator;
		
		protected JavaGatewayClientThread(AbstractServer.CallReceiverThread receiver, int workerID) {
			super(receiver, workerID);
		}

		@Override
		public void run() {
			while(true) {
				try {
					firePropertyChange("status", null, "Waiting");
					socketWrapper = receiver.clientQueue.take();
					InetAddress clientAddress = socketWrapper.getInetAddress();
					this.translator = registerClient(clientAddress);
					JavaLocalGatewayServer.this.whoIsWorkingForWho.put(this, clientAddress);
					firePropertyChange("status", null, "Connected to client: " + clientAddress.getHostAddress());		
					while (!socketWrapper.isClosed()) {
						try {
							Object somethingInParticular = processRequest();
							if (somethingInParticular != null) {
								if (Thread.interrupted()) {
									throw new InterruptedException();
								}
								if (somethingInParticular.equals(BasicClient.ClientRequest.closeConnection) 
										|| somethingInParticular.equals(BasicClient.ClientRequest.closeConnection.name())) {
									socketWrapper.writeObject(ServerReply.ClosingConnection);
									closeSocket();
									JavaLocalGatewayServer.this.whoIsWorkingForWho.remove(this);
									if (!JavaLocalGatewayServer.this.whoIsWorkingForWho.contains(clientAddress)) {
										JavaLocalGatewayServer.this.translators.remove(clientAddress);
									}
									JavaLocalGatewayServer.this.requestShutdown();
									break;
								} else {
									socketWrapper.writeObject(somethingInParticular);
								}
							} else {
								socketWrapper.writeObject(ServerReply.Done);
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
						JavaLocalGatewayServer.this.requestShutdown();
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
			if (crudeRequest instanceof String) {
				String request = (String) crudeRequest;
				if (ClientRequest.closeConnection.name().equals(request.trim())) {
					return ClientRequest.closeConnection;
				} else {
					if (request.startsWith("time")) {
						long startMillisec = Long.parseLong(request.substring(4));
						long finalTime = System.currentTimeMillis();
						double elapsedTime =  (finalTime - startMillisec);
						System.out.println("Elapsed time single received packet:" + elapsedTime);
					}
					return this.translator.processCode(request);
				}
			} else {
				return null;
			}
		}

	}

	protected final ConcurrentHashMap<InetAddress, REnvironment> translators;	
	protected final boolean shutdownOnClosedConnection;
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


	synchronized REnvironment registerClient(InetAddress clientAddress) {
		if (!translators.containsKey(clientAddress)) {
			translators.put(clientAddress, new REnvironment());
		}
		return(translators.get(clientAddress));
	}


	
	
	
	/**
	 * Hidden constructor for test purpose
	 * @param servConf a ServerConfiguration instance
	 * @param translator an instance that implements the REpiceaCodeTranslator interface
	 * @param shutdownOnClosedConnection by default this parameter is set to true so that if the connection is lost, the server is shutdown.
	 * @throws Exception
	 */
	protected JavaLocalGatewayServer(ServerConfiguration servConf, REnvironment translator, boolean shutdownOnClosedConnection) throws Exception {
		super(servConf, false);
		this.translators = new ConcurrentHashMap<InetAddress, REnvironment>();
		this.shutdownOnClosedConnection = shutdownOnClosedConnection;
	}

	@Override
	protected ClientThread createClientThread(AbstractServer.CallReceiverThread receiver, int id) {
		return new JavaGatewayClientThread(receiver, id);
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

	@Override
	protected void createFileInfoForLocalServer() throws IOException {
		String filename = getConfiguration().wd.trim() + File.separator + "J4RTmpFile";
		File file = new File(filename);
		String realizedListeningPorts = "";
		for (CallReceiverThread t : callReceiverThreads) {
			if (!realizedListeningPorts.isEmpty()) {
				realizedListeningPorts += PortSplitter;
			}
			realizedListeningPorts += t.serverSocket.getLocalPort();
		}
		String outputStr = "" + getConfiguration().key + ";" + 
				backdoorThread.emergencySocket.getLocalPort() + PortSplitter + gcReceiverThread.serverSocket.getLocalPort() + ";" + 
				realizedListeningPorts; 
		FileWriter writer = new FileWriter(file);
		writer.write(outputStr);
		writer.close();
	}
	
	
	
	
}
