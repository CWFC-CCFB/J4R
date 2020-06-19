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

import j4r.app.AbstractGenericTask;

public class ServerTask extends AbstractGenericTask {

	private static final long serialVersionUID = 1L;

	public static enum ServerTaskID {
		StartReceiverThread,
		CreateFileInfo;
	}
	
	private final ServerTaskID taskID;
	private final AbstractServer server;
	
	protected ServerTask(ServerTaskID taskID, AbstractServer server) {
		this.taskID = taskID;
		this.server = server;
		setName(taskID.name());
	}
	
	@Override
	protected void doThisJob() throws Exception {
		switch(taskID) {
		case StartReceiverThread:
			server.startReceiverThread();
			break;
		case CreateFileInfo:
			server.createFileInfoForLocalServer();
			break;
		}
	}

}
