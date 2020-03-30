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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.InetAddress;

import j4r.net.SocketWrapper;
import j4r.net.server.AbstractServer.ServerReply;

public abstract class ClientThread implements Runnable, ActionListener {
	
	protected final AbstractServer caller;
	protected SocketWrapper socketWrapper;
	
	private final int workerID;
	@SuppressWarnings("unused")
	private InetAddress clientAddress;
	
	private Thread worker;
	private final Object lock = new Object();
	

	/**
	 * Public constructor.
	 * @param caller a CapsisServer instance
	 * @param workerID an integer that serves to identify this client thread
	 */
	protected ClientThread(AbstractServer caller, int workerID) {
		super();
		this.caller = caller;
		this.workerID = workerID;
	}

	@Override
	public void run() {
		while (true) {
			try {
				try {
					socketWrapper = caller.getWaitingClients();
					clientAddress = socketWrapper.getInetAddress();

					processRequest();

					socketWrapper.writeObject(ServerReply.ClosingConnection);
					closeSocket();
				} catch (Exception e) {
					try {
						e.printStackTrace();
						if (!socketWrapper.isClosed()) {
							socketWrapper.writeObject(e);
						}
						closeSocket();
					} catch (IOException e1) {
						socketWrapper = null;
					}
					synchronized (lock) {
						lock.wait();
					}
				}
			} catch (InterruptedException e) {}
		}
	}

	protected abstract Object processRequest() throws Exception;


	@Override
	public void actionPerformed(ActionEvent arg0) {
		if (arg0.getActionCommand().equals("Restart")) {
				restartAction();
		} 
	}

	protected void restartAction() {
		synchronized(lock) {
			lock.notify();
		}
	}
	
	
	protected void start() {
		worker = new Thread(this);
		worker.setName("Client thread no " + workerID);
		worker.start();
	}
	
	
	/**
	 * This method returns the ID of the worker.
	 * @return an Integer
	 */
	protected int getWorkerID() {return workerID;}
	
	protected void closeSocket() throws IOException {
		if (socketWrapper != null && !socketWrapper.isClosed()) {
			socketWrapper.close();
		}
		clientAddress = null;
	}
	
	
	protected SocketWrapper getSocket() {return socketWrapper;}
	
}
