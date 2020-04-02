/*
 * This file is part of the j4r library.
 *
 * Copyright (C) 2009-2020 Mathieu Fortin for Canadian Forest Service.
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
package j4r.app;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;

import j4r.util.J4RTranslator;
import j4r.util.J4RTranslator.TextableEnum;

/**
 * The AbstractGenericEngine class implements all the methods to run an application. Some
 * GenericTask instances are stored in a queue, and processed in order by an internal SwingWorker.
 * @author Mathieu Fortin - July 2012
 */
public abstract class AbstractGenericEngine {


	private static enum MessageID implements TextableEnum {
		ErrorMessage("An error of this type occured while running task ", "Une erreur de ce type est survenu pendant l'ex\u00E9cution de la t\u00E2che "),
		CancelMessage("The task has been canceled !", "La t\u00E2che a \u00E9t\u00E9 annul\u00E9e !");
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			J4RTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {
			return J4RTranslator.getString(this);
		}
	}
	
	
	/**
	 * This fake task to ensure the InternalWorker is not waiting for a job before shutting down.
	 * The only job that is carried out while shutting down is the saving of the settings.
	 * @author Mathieu Fortin - December 2011
	 */
	@SuppressWarnings("serial")
	private final class EnsuringShutdownTask extends AbstractGenericTask {

		private EnsuringShutdownTask() {
			setName("FinalTask");
		}

		@Override
		protected void doThisJob() throws Exception {
			synchronized(lock) {
				goAhead = true;
				lock.notifyAll();
			}			
			
			if (worker.isCorrectlyTerminated()) {
				shutdown(0);
			} else {
				worker.getFailureReason().printStackTrace();
				shutdown(1);
			}
		}
				
	}
	
	
	
	/**
	 * This static class handles the different UpdaterTask stored in the queue of the Engine.
	 * @author Mathieu Fortin - November 2011
	 */
	private static class InternalWorker extends Thread {

		private AbstractGenericEngine engine;
		private AbstractGenericTask currentTask;
		private Exception failureReason;
		
		private InternalWorker(AbstractGenericEngine engine) {
			setName("Engine - Internal task processor");
			this.engine = engine;
			this.setDaemon(false);
		}
		
		public void run() {
			try {
				LinkedBlockingQueue<AbstractGenericTask> queue = engine.queue;
				do {
					currentTask = queue.take();			

					if (currentTask.isVerbose()) {
						System.out.println("Running task : " + currentTask.getName());
					}

					currentTask.run();

					if (!currentTask.isCorrectlyTerminated() || currentTask.hasBeenCancelled()) {
						engine.decideWhatToDoInCaseOfFailure(currentTask);
					} else {
						engine.tasksDone.add(currentTask.getName());
					}
				} while (!currentTask.equals(engine.finalTask));
			} catch (InterruptedException e) {
				failureReason = e;
				System.out.println("The Engine has been interrupted!");
				engine.finalTask.run();
			}
		}
		
		protected void requestCancel() {
			if (currentTask != null) {
				currentTask.cancel();
			}
		}
		
		public boolean isCorrectlyTerminated() {
			return failureReason == null;
		}

		public Exception getFailureReason() {
			return failureReason;
		}
	}
	
	protected LinkedBlockingQueue<AbstractGenericTask> queue;
	protected List<String> tasksDone;
	private InternalWorker worker;
	
	private boolean goAhead = true;
	private final Object lock = new Object();
	
	protected EnsuringShutdownTask finalTask = new EnsuringShutdownTask();

	
	/**
	 * Protected constructor for derived class.
	 */
	protected AbstractGenericEngine() {
		queue = new LinkedBlockingQueue<AbstractGenericTask>();
		tasksDone = new CopyOnWriteArrayList<String>();
		worker = new InternalWorker(this);
		worker.start();
	}

	/**
	 * This method is called whenever an exception is thrown while running a task. If 
	 * the Engine has a user interface and this interface is visible, an error message
	 * is displayed. The queue of tasks is cleared.
	 * @param task a GenericTask instance
	 */
	protected void decideWhatToDoInCaseOfFailure(AbstractGenericTask task) {
		String message = null;
		if (task.hasBeenCancelled()) {
			message = MessageID.CancelMessage.toString();
		} else {
			String taskName = task.getName();
			Exception failureCause = task.getFailureReason();
			String errorType = "";
			if (failureCause != null) {
				errorType = failureCause.getClass().getSimpleName();
				failureCause.printStackTrace();
			}
			message = MessageID.ErrorMessage.toString() + taskName + " : " + errorType;
		}
		
		System.out.println(message);
		queue.clear();
	}
	
	/**
	 * This method sets the first tasks to execute when the engine starts. Typically,
	 * it would retrieve the settings, show the user interface and so on.
	 */
	protected abstract void firstTasksToDo();

	
	
	/**
	 * This method starts the client application. The abstract method firstTasksToDo() serves to pile tasks in the queue as
	 * soon as the application starts.
	 */
	public final void startApplication() {
		firstTasksToDo();
	}

	
	protected void shutdown(int shutdownCode) {
		System.out.println("Shutting down application...");
		System.exit(shutdownCode);
	}

	
	/**
	 * This method add a GenericTask instance in the queue of tasks.
	 * @param task a GenericTask instance
	 */
	public void addTask(AbstractGenericTask task) {
		queue.add(task);
	}

	
	/**
	 * This method add a bunch of tasks in the queue of tasks.
	 * @param tasks a List of GenericTask instances
	 */
	public void addTasks(List<AbstractGenericTask> tasks) {
		queue.addAll(tasks);
	}
	
	
	/**
	 * This method locks the engine while the interface can be doing something else.
	 * @param millisec the number of milliseconds to wait
	 * @throws InterruptedException
	 */
	protected void lockEngine(long millisec) throws InterruptedException {
		synchronized(lock) {
			goAhead = false;
			while(!goAhead) {
				lock.wait(millisec);
			}
		}
	}

	
	/**
	 * This method locks the engine while the interface can be doing something else. The engine 
	 * can be locked only if the executing thread is not the internal worker.
	 * @throws InterruptedException
	 */
	protected void lockEngine() throws InterruptedException {
		if (Thread.currentThread() != worker) {
			lockEngine(0);
		}
	}
	
	
	/**
	 * This method unlock the engine if locked.
	 */
	protected void unlockEngine() {
		synchronized(lock) {
			goAhead = true;
			lock.notify();
		}
	}

	
	/**
	 * This method cancels the current task if the queue is not empty.
	 */
	public void cancelRunningTask() {
		worker.requestCancel();
	}

	/**
	 * This method requests the Engine to shut down. It first clears the queue of tasks and then it 
	 * sends the FinalTask static member in the queue in order to ensures the shutting down.
	 */
	public void requestShutdown() {
		queue.clear();
		addTask(finalTask);
		try {
			lockEngine();		
		} catch (Exception e) {}
	}

	
	
}
