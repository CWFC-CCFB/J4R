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
package j4r.multiprocess;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * The AbstractIndependentProcess is the abstract class for process wrapper in Java. It includes two 
 * daemon threads: one that communicates with the process and the other that runs the process itself.
 * @author Mathieu Fortin
 */
abstract class AbstractIndependentProcess implements Runnable {

	public enum StateValue {DONE, PENDING, STARTED}

	/**
	 * This private class handles the thread that communicates with the process.
	 * @author Mathieu Fortin
	 */
	private static class IndependentProcessCommunicationHandler implements Runnable {

		private AbstractIndependentProcess abstractIndependentProcess;
		private Thread emittingThread;
		private ConcurrentLinkedQueue<Serializable> queue;

		private IndependentProcessCommunicationHandler(AbstractIndependentProcess abstractIndependentProcess) {
			this.abstractIndependentProcess = abstractIndependentProcess;
			queue = new ConcurrentLinkedQueue<Serializable>();
			emittingThread = new Thread(this);
			emittingThread.setDaemon(true);
			emittingThread.setName(abstractIndependentProcess.getClass().getSimpleName() + " - CommHandler");
			emittingThread.start();
		}

		@Override
		public void run() {
			if (abstractIndependentProcess.getState() != StateValue.DONE) {
				try {
					synchronized (abstractIndependentProcess.communicationHandlerLock) {
						while (abstractIndependentProcess.getState() == StateValue.PENDING) {
							abstractIndependentProcess.communicationHandlerLock.wait();
						}
					}
					ObjectOutputStream oos = new ObjectOutputStream(new BufferedOutputStream(abstractIndependentProcess.process.getOutputStream()));
					while (abstractIndependentProcess.getState() == StateValue.STARTED) {
						Object toBeSent = queue.poll();
						if (toBeSent != null) {
							oos.writeObject(toBeSent);
							oos.flush();
						}
					}
				} catch (InterruptedException e) {
					e.printStackTrace();
				} catch (IOException e) {
					String message = "Communication channel shutted down";
					abstractIndependentProcess.firePropertyChange("MessageReceived", null, message);
					if (abstractIndependentProcess.redirectOutputStream) {
						System.out.println("PROCESS " + abstractIndependentProcess.getName() + ": " + message);
					}
				}
			}
		}
		
		private void writeObject(Serializable obj) throws IOException {
			queue.add(obj);
		}

		
	}
	
	
	private StateValue state;
	private boolean isCancelled;
	private int exitValue;
	private Exception exceptionWhileRunning;
	private String name;
	private boolean redirectOutputStream = true;
	
	private transient final Object lock = new Object();
	private transient final Object communicationHandlerLock = new Object();
	private transient Collection<PropertyChangeListener> listeners = new CopyOnWriteArrayList<PropertyChangeListener>();
	private transient Process process;
	private transient IndependentProcessCommunicationHandler communicationHandler;
	private transient Thread worker;
	

	/**
	 * Basic constructor.
	 */
	protected AbstractIndependentProcess() {
		exitValue = -1;
		isCancelled = false;
		setState(StateValue.PENDING);
		redirectOutputStream(true);
		worker = new Thread(this);
		worker.setDaemon(true);
		worker.setName(this.getClass().getSimpleName() + " - ProcessHandler");
	}
	
	public boolean cancel(boolean mayInterruptIfRunning) {
		if (isCancelled || state == StateValue.DONE) {
			return false;
		} else {
			if (state == StateValue.PENDING) {
				isCancelled = true;
				return true;
			} else {
				if (mayInterruptIfRunning) {
					if (process != null) {
						process.destroy();	
						isCancelled = true;
						setState(StateValue.DONE);
					}
					return true;
				} else {
					return false;
				}
			} 
		}
	}

	public Integer get() throws InterruptedException, ExecutionException {
		try {
			return get(0, TimeUnit.MILLISECONDS);
		} catch (TimeoutException e) {
			throw new ExecutionException(e);		// no suppose to happen 
		}			
	}

	public Integer get(long duration, TimeUnit timeUnit) throws InterruptedException, ExecutionException, TimeoutException {
		long timeToWait = timeUnit.toMillis(duration);
		
		synchronized(lock) {
			while (getState() != StateValue.DONE) {
				if (timeToWait == 0l) {
					lock.wait();
				} else {
					lock.wait(timeToWait);
					if (getState() != StateValue.DONE) {
						throw new TimeoutException();
					}
				}
			}
		}
		
		if (exceptionWhileRunning != null) {
			throw new ExecutionException(exceptionWhileRunning);
		}
		return exitValue;
	}

	public boolean isCancelled() {
		return isCancelled;
	}

	/**
	 * This method returns the state of the process which can be either PENDING, STARTED, or DONE. As 
	 * soon as the execute method is called the state changes from PENDING to STARTED. At the end of the
	 * process, the State shifts to DONE.
	 * @return a StateValue enum var
	 */
	public StateValue getState() {return state;}
	
	public boolean isDone() {
		return getState() == StateValue.DONE;
	}

	/**
	 * This method creates the proper Process instance according to the derived class. 
	 * @return a Process object
	 */
	protected abstract Process createIndependentProcess() throws IOException;

	@Override
	public void run() {
		if (getState() == StateValue.PENDING) {
			try {
				process = createIndependentProcess();
				setState(StateValue.STARTED);
				
				synchronized(communicationHandlerLock) {
					communicationHandlerLock.notify();			// unlock the communication handler
				}
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF-8"));
				String lineReceived = reader.readLine();
				String previousMessage = null;
				while (lineReceived != null) {
					firePropertyChange("MessageReceived", previousMessage, lineReceived);
					if (redirectOutputStream) {
						System.out.println("PROCESS " + getName() + ": " + lineReceived);
					}
					previousMessage = lineReceived;
					lineReceived = reader.readLine();
				}
				exitValue = process.waitFor();
			} catch (IOException e) {
				exceptionWhileRunning = e;
			} catch (InterruptedException e) {
				exceptionWhileRunning = e;
			} 
			
			synchronized(lock) {
				setState(StateValue.DONE);
				lock.notify();
			}
		}
	}

	private void setState(StateValue stateValue) {
		StateValue formerValue = state;
		state = stateValue;
		firePropertyChange("state", formerValue, state);
	}
	
	protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
		PropertyChangeEvent evt = new PropertyChangeEvent(this, propertyName, oldValue, newValue);
		for (PropertyChangeListener listener : listeners) {
			listener.propertyChange(evt);
		}
	}
	
	public void addPropertyChangeListener(PropertyChangeListener listener) {
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}
	
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		while (listeners.contains(listener)) {
			listeners.remove(listener);
		}
	}
	
	private IndependentProcessCommunicationHandler getCommunicationHandler() {
		if (communicationHandler == null) {
			communicationHandler = new IndependentProcessCommunicationHandler(this);
		}
		return communicationHandler;
	}
	
	public void execute() {
		worker.start();
	}

	public String getName() {return name;}
	
	public void setName(String name) {this.name = name;}
	
	public void sendObjectToProcess(Serializable obj) throws IOException {
		getCommunicationHandler().writeObject(obj);
	}
	
	/**
	 * This method enables the redirection of the outputstream of the independent process into the outputstream of this outer process (this process). By default, the redirection
	 * is set to true.
	 * @param redirectOutputStream true to enable or false to disable
	 */
	public void redirectOutputStream(boolean redirectOutputStream) {
		this.redirectOutputStream = redirectOutputStream;
	}
	

}	
