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
package j4r.net;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;

/**
 * The TCPSocketWrapper class handles the output and input of a TCP/IP socket. 
 * @author Mathieu Fortin
 */
public class TCPSocketWrapper implements SocketWrapper {
	
	private final byte[] buffer = new byte[100000];
	private final boolean isJavaObjectExpected;
	private final Socket socket;
	
	private InputStream basicIn;
	private OutputStream basicOut;
	private ObjectInputStream objectIn;
	private ObjectOutputStream objectOut;
	
	
	/**
	 * General constructor. If the caller is calling from a Java application,
	 * then the isJavaObjectExpected boolean should be set to true. Otherwise,
	 * the incoming stream is converted to String by default.
	 * @param socket a Socket instance
	 * @param isJavaObjectExpected a boolean.
	 */
	public TCPSocketWrapper(Socket socket, boolean isJavaObjectExpected) {
		this.socket = socket;
		this.isJavaObjectExpected = isJavaObjectExpected;
	}

	private Socket getSocket() {return socket;}

	@Override
	public Object readObject() throws Exception {
		return readObject(0);
	}
		
	
	@Override
	public Object readObject(int numberOfSeconds) throws Exception {
		socket.setSoTimeout(numberOfSeconds * 1000);
		Object result;
		if (isJavaObjectExpected) {
			result = getObjectInputStream().readObject();
		} else {
			result = readString();
		}
		return result;
	}
	
	@Override
	public void writeObject(Object obj) throws IOException {
		if (isJavaObjectExpected) {
			getObjectOutputStream().writeObject(obj);
		} else {
			writeString(obj.toString());
		}
	}
	
	private int readBytes(byte[] buffer) throws IOException {
		return getBasicInputStream().read(buffer);
	}

	/**
	 * This method returns a String from the incoming bytes. The buffer is 100000 byte long.
	 * @return a String instance
	 * @throws IOException
	 */
	private String readString() throws IOException {
		int nbBytes = readBytes(buffer);
		if (nbBytes == -1) {
			close();
			throw new IOException("Seems that the connection has been shutdown by the client...");
		}
		String incomingMessage = new String(buffer).substring(0, nbBytes);
		return incomingMessage;
	}

	/**
	 * This method sends a String on the socket connection.
	 * @param str a String instance
	 * @throws IOException
	 */
	private void writeString(String str) throws IOException {
		writeBytes(str.getBytes());
	}
	
	private void writeBytes(byte[] buffer) throws IOException {
		getBasicOutputStream().write(buffer);
	}
	
	@Override
	public boolean isClosed() {return socket.isClosed();}
	
	@Override
	public void close() throws IOException {
//		getObjectOutputStream().flush();  20200117MF removed because it caused a deadlock on Windows OS and does not seem necessary
		socket.close();
	}
	
	private InputStream getBasicInputStream() throws IOException {
		if (basicIn == null) {
			basicIn = socket.getInputStream();
		}
		return basicIn;
	}
	
	private ObjectInputStream getObjectInputStream() throws IOException {
		if (objectIn == null) {
			objectIn = new ObjectInputStream(getBasicInputStream());
		}
		return objectIn;
	}
	
	private OutputStream getBasicOutputStream() throws IOException {
		if (basicOut == null) {
			basicOut = socket.getOutputStream();
		}
		return basicOut;
	}
	
	private ObjectOutputStream getObjectOutputStream() throws IOException {
		if (objectOut == null) {
			objectOut = new ObjectOutputStream(getBasicOutputStream());
		}
		return objectOut;
	}

	@Override
	public InetAddress getInetAddress() {
		return getSocket().getInetAddress();
	}

	
}
