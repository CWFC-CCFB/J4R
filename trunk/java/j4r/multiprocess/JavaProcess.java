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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import j4r.lang.J4RSystem;

/**
 * This class has a design similar to the SwingWorker class. It embeds a process that is to be a Java process. The 
 * commands in the constructor majes it possible to specified the runnable jar and its options. </br>
 * </br>
 * An exemple of an instance of this class could be</br>
 * </br>
 * {@code List<String> commands = new ArrayList<String>();}</br>
 * {@code commands.add("myJar.jar");}</br>
 * {@code JavaProcess jp = new JavaProcess(commands);} </br>
 * {@code jp.execute();} </br>
 * {@code int outValueFromRemoteJVM = jp.get();} </br>
 *  </br>
 * The method get() waits for the process to end. The result is actually the output value of the remote Java Virtual Machine (JVM), which is usually
 * 0 if the process ended properly. The class is designed to be run only once. Additional calls of the execute() method
 * will not result in the instantiation of a new independent Java process. As in the SwingWorker class, this class also
 * send PropertyChangeEvent to eventual listeners that can be added through the addPropertyChangeListener method. </br>
 * </br>
 * Objects can be sent to the process using the method sendObjectToProcess(). The objects are sent asynchronously and may not be sent is the process
 * ends before. The incoming stream, ie. the output stream of the remote JVM, is sent to the PropertyChangeListener instances using the "MessageReceived" property. </br>
 * </br>
 * Finally, the class implements the Serializable interface. Actually, only some fields are serializable. A JavaProcess instance can be serialized either before or after 
 * executing the process, but not during its execution.
 * 
 * @author Mathieu Fortin 
 */
public final class JavaProcess extends AbstractIndependentProcess {

	
	public static enum JVM_OPTION {Memory, 
		ClassPath, 
		FileEncoding, 
		/**
		 * @deprecated Java 9 no longer accepts customized classloaders
		 */
		@Deprecated
		SystemClassLoader,
		SplashWindow,
		OpenInternalLoader};
	
	private List<String> commands;
	private Map<String, String> environment;
	private File workingDirectory;
	private Map<JVM_OPTION, String> jvmOptions;
	
	/**
	 * General constructor.
	 * @param commands the list of commands
	 * @param workingDirectory the working directory for the process
	 * @param environment the environment of the process
	 */
	public JavaProcess(List<String> commands, File workingDirectory, Map<String, String> environment) {
		init();
		this.commands.addAll(commands);
		this.workingDirectory = workingDirectory;
		this.environment = environment;
	}
	
	/**
	 * Sub constructor with only the commands and the working directory.
	 * @param commands the list of commands
	 * @param workingDirectory the working directory for the process
	 */
	public JavaProcess(List<String> commands, File workingDirectory) {
		this(commands, workingDirectory, null);
	}

	@Override
	public String getName() {
		if (super.getName() == null) {
			try {
				return commands.get(0); // return the name of the jar file
			} catch (Exception e) {
				return null;
			}
		} else {
			return super.getName();
		}
	}
	
	/**
	 * Sub constructor with the commands only.
	 * @param commands the list of commands
	 */
	public JavaProcess(List<String> commands) {
		this(commands, null, null);
	}

	/**
	 * Sub constructor with only the commands and the environment.
	 * @param commands the list of commands
	 * @param environment the environment of the process
	 */
	public JavaProcess(List<String> commands, Map<String, String> environment) {
		this(commands, null, environment);
	}

	
	private void init() {
		commands = new ArrayList<String>();
		jvmOptions = new HashMap<JVM_OPTION, String>();
	}
	
	
	@Override
	protected Process createIndependentProcess() throws IOException {
		List<String> finalCommands = new ArrayList<String>();
		finalCommands.add("java");
		if (!jvmOptions.isEmpty()) {
			for (String optionalCommand : jvmOptions.values()) {
				for (String optCommand : optionalCommand.split(" ")) {
					finalCommands.add(optCommand);
				}
			}
		}
		
		if (getName().endsWith(".jar")) {
			finalCommands.add("-jar");
		}
		finalCommands.addAll(commands);
		System.out.println(finalCommands);
		ProcessBuilder pb = new ProcessBuilder(finalCommands);
		pb.redirectErrorStream(true);
		if (workingDirectory != null) {
			pb.directory(workingDirectory);
		}
		if (environment != null && !environment.isEmpty()) {
			pb.environment().putAll(environment);
		}
		return pb.start();
	}

	/**
	 * This method sets the maximum memory of the Java Virtual Machine that runs this process. The
	 * method does nothing if the parameter is not larger than 0.
	 * @param nbMegaJVM the number of MegaByte (Integer)
	 */
	public void setJVMMemory(int nbMegaJVM) {
		if (nbMegaJVM > 0) {
			jvmOptions.put(JVM_OPTION.Memory, "-Xmx" + nbMegaJVM + "m");
		}
	}

	/**
	 * This method sets the classpath. It has no effect if the first element of the command list is a jar file.
	 * @param classPath the complete classpath
	 */
	public void setClassPath(String classPath) {
		if (classPath != null) {
			jvmOptions.put(JVM_OPTION.ClassPath, "-cp " + classPath);
		}
	}

	/**
	 * Open the internal loader for versions later than 8.
	 * @param bool it is set to false by default
	 */
	public void setOpenModuleForVersionsLaterThan8Enabled(boolean bool) {
		if (bool && J4RSystem.isCurrentJVMLaterThanThisVersion("1.8")) {
			jvmOptions.put(JVM_OPTION.OpenInternalLoader, "--add-opens java.base/jdk.internal.loader=ALL-UNNAMED");
		}
	}
	
	/**
	 * This method sets the JVM options
	 * @param optionName a JVM_OPTION enum instance
	 * @param option a String that defines the option
	 */
	public void setOption(JVM_OPTION optionName, String option) {
		jvmOptions.put(optionName, option);
	}
	
	/**
	 * This method returns the options of the JVM as set by the user.
	 * @return a Map of JVM_OPTION instances and their settings.
	 */
	public Map<JVM_OPTION, String> getJVMSettings() {return jvmOptions;}
}
