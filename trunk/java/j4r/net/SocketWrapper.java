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

import java.io.Closeable;
import java.io.IOException;
import java.net.InetAddress;

/**
 * This interface ensures that the wrapper can read and write and knows whether
 * it is closed or not.
 * @author Mathieu Fortin 
 */
public interface SocketWrapper extends Closeable {

	/**
	 * This method reads the object from the socket. There is no timeout.
	 * @return the object that was read from the socket
	 * @throws Exception
	 */
	public Object readObject() throws Exception;

	/**
	 * This method reads an object from the socket. There is a timeout after a given number of
	 * seconds. If the number of seconds is equal to or smaller than 0, then there is no timeout.
	 * @param  timeout number of seconds to wait before throwing a TimeoutException
	 * @return the object that was read from the socket
	 * @throws Exception 
	 */
	public Object readObject(int timeout) throws Exception;
	
	/**
	 * This method sends an object through the socket.
	 * @param obj the object to be sent.
	 * @throws IOException
	 */
	public void writeObject(Object obj) throws IOException;

	/**
	 * This method returns true if the socket has been closed.
	 * @return a boolean
	 */
	public boolean isClosed();

	/**
	 * This method returns the address of the socket that is bound to this socket.
	 * @return an InetAddress instance
	 */
	public InetAddress getInetAddress();
}
