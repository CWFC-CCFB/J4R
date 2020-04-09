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
package j4r.lang.codetranslator;

import java.io.File;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import j4r.lang.J4RSystem;
import j4r.lang.reflect.ReflectUtility;
import j4r.multiprocess.JavaProcess;
import j4r.multiprocess.JavaProcessWrapper;
import j4r.net.server.BasicClient;
import j4r.net.server.JavaLocalGatewayServer;
import j4r.net.server.ServerConfiguration;

@SuppressWarnings("serial")
public class REnvironment extends ConcurrentHashMap<Integer, Object> {


	private static final String EXTENSION = "-ext";
	
	private static final String FIRSTCALL = "-firstcall";
	
	private static final String PORT = "-port";
	
	private static final String MEMORY = "-mem";

	public static final String MainSplitter = "/;";
	
	public static final String SubSplitter = "/,";

	private final static Map<String, Class<?>> PrimitiveTypeMap = new HashMap<String, Class<?>>();
	static {
		PrimitiveTypeMap.put("integer", int.class);
		PrimitiveTypeMap.put("character", String.class);
		PrimitiveTypeMap.put("numeric", double.class);
		PrimitiveTypeMap.put("logical", boolean.class);
	}

	static class InternalShutDownHook extends Thread {
		
		final JavaProcessWrapper rGatewayProcessWrapper;
		
		InternalShutDownHook(JavaProcessWrapper rGatewayProcessWrapper) {
			this.rGatewayProcessWrapper = rGatewayProcessWrapper;
		}
		
		@Override
		public void run() {
			rGatewayProcessWrapper.cancel();
		}
		
	}

	
	static class MethodWrapper implements Comparable<MethodWrapper> {

		final double score; 
		final Method method;
		
		MethodWrapper(double score, Method method) {
			this.score = score;
			this.method = method;
		}
		
		@Override
		public int compareTo(MethodWrapper o) {
			if (score < o.score) {
				return -1;
			} else  if (score == o.score) {
				return 0;
			} else {
				return 1;
			}
		}
		
	}
	
	static class JavaObjectList extends ArrayList<ParameterWrapper> {
		
		@Override
		public String toString() {
			String output = "JavaList" + MainSplitter;
			for (ParameterWrapper obj : this) {
				String toBeAdded = obj.toString();
				if (toBeAdded.startsWith("JavaObject" + MainSplitter)) {
					toBeAdded = toBeAdded.substring(("JavaObject" + MainSplitter).length());
				}
				output = output + toBeAdded + SubSplitter;	
			}
			return output;
		}
	}
	
	static class ParameterList extends ArrayList<List<ParameterWrapper>> {
		int getInnerSize() {
			int currentSize = 0;
			for (int i = 0; i < size(); i++) {
				if (get(i).size() > currentSize) {
					currentSize = get(i).size();
				}
			}
			return currentSize;
		}
		
		Object[] getParameterArray(int i) {
			int currentSize = getInnerSize();
			if (i > currentSize) {
				throw new InvalidParameterException("Inconsistent parameter setup!");
			}
			Object[] parms = new Object[size()];
			for (int j = 0; j < size(); j++) {
				if (get(j).size() == 1) {
					parms[j] = get(j).get(0).value;
				} else {
					parms[j] = get(j).get(i).value;
				}
			}
			return parms;
		}
	}
	
	private class ParameterWrapper {
		
		final Class<?> type;
		final Object value;
		
		private ParameterWrapper(Class<?> type, Object value) {
			this.type = type;
			this.value = value;
		}
		
		@Override
		public String toString() {
			if (ReflectUtility.JavaWrapperToPrimitiveMap.containsKey(type)) {
				if (type.equals(Double.class) || type.equals(Float.class)) {
					return "numeric" + ((Number) value).toString();
				} else if (type.equals(Integer.class) || type.equals(Long.class)) {
					return "integer" + ((Number) value).toString();
				} else if (type.equals(Boolean.class)) {
					return "logical" + ((Boolean) value).toString();
				} else {
					return "character" + value.toString();
				}
			} else {
				String className = type.getName();
				if (className.endsWith(";")) {
					className = className.substring(0, className.length() - 1);
				} else if (className.endsWith(MainSplitter)) {
					className = className.substring(0, className.length() - MainSplitter.length());
				}
				return "JavaObject" + MainSplitter + className + "@" + System.identityHashCode(value);
			}
		}
	}

	
	private class NullWrapper {

		final Class<?> type; 
		
		private NullWrapper(Class<?> type) {
			this.type = type;
		}
		
	}

	
		
	private static String ConstructCode = "create";
	private static String ConstructNullCode = "createnull";
	private static String ConstructArrayCode = "createarray";
	private static String ConstructNullArrayCode = "createnullarray";
	private static String MethodCode = "method";
	private static String SynchronizeEnvironment = "sync";
	private static String FieldCode = "field";

	

	
	public Object processCode(String request) throws Exception {
		String[] requestStrings = request.split(MainSplitter);
		if (requestStrings[0].startsWith(ConstructCode)) {	// can be either create, createarray or createnull here
			return createObjectFromRequestStrings(requestStrings); 
		} else if (requestStrings[0].equals(MethodCode)) {
			return processMethod(requestStrings);
		} else if (requestStrings[0].equals(FieldCode)) {
			return processField(requestStrings);
		} else if (requestStrings[0].equals(SynchronizeEnvironment)) {
			return synchronizeEnvironment(requestStrings);
		} else {
			try {
				return BasicClient.ClientRequest.valueOf(request);
			} catch (Exception e) {
				throw new InvalidParameterException("Request unknown! " + request);
			}
		}

	}
	
	private Object synchronizeEnvironment(String[] requestStrings) {
		Map<Integer, Object> actualMap = new HashMap<Integer, Object>();
		for (int i = 1; i < requestStrings.length; i++) {
			List<ParameterWrapper> wrappers = findObjectInEnvironment(requestStrings[i]);
			if (wrappers != null) {
				for (ParameterWrapper wrapper : wrappers) {
					Object caller = wrapper.value;
					actualMap.put(System.identityHashCode(caller), caller);
				}
			}
		}
		Map<Integer, Object> toBeRemoved = new HashMap<Integer, Object>();
		for (Object value : values()) {
			if (!actualMap.containsKey(System.identityHashCode(value))) {
				toBeRemoved.put(System.identityHashCode(value), value);
			}
		}
		for (Object value : toBeRemoved.values()) {
			remove(System.identityHashCode(value), value);
		}
		JavaObjectList outputList = new JavaObjectList();
		registerMethodOutput(size(), outputList);
		return outputList;
	}


	private List<ParameterWrapper> findObjectInEnvironment(String string) {
		List<ParameterWrapper> wrappers = new ArrayList<ParameterWrapper>();
		String prefix = "java.objecthashcode";
		if (string.startsWith(prefix)) {
			String[] newArgs = string.substring(prefix.length()).split(SubSplitter);
			for (int i = 0; i < newArgs.length; i++) {
				int hashcodeForThisJavaObject = Integer.parseInt(newArgs[i]);
				if (containsKey(hashcodeForThisJavaObject)) {
					Object value = get(hashcodeForThisJavaObject);
					Class<?> type;
					if (value instanceof NullWrapper) {
						type = ((NullWrapper) value).type;
						value = null;
					} else {
						type = value.getClass();
					}
					wrappers.add(new ParameterWrapper(type, value));
				} else {
					throw new InvalidParameterException("This object does not exist: " + string);
				}
			}
		}
		return wrappers;
	}

	
	@SuppressWarnings({ "rawtypes"})
	private Object processField(String[] requestStrings) throws Exception {
		Class clazz = null;
		List<ParameterWrapper> wrappers = null;
		boolean lookingForStaticMethod = false;
		if (requestStrings[1].startsWith("java.object")) {			// presumably non-static method
			wrappers = findObjectInEnvironment(requestStrings[1]);
			Object caller = getCallerAmongWrappers(wrappers);
			clazz = caller.getClass();
		} else {
			wrappers = createFromPrimitiveClass(getPrimitiveClass(requestStrings[1]), requestStrings[1]);
			ParameterWrapper caller = wrappers.get(0);
			if (wrappers.size() == 1 && caller.type.equals(String.class)) { // could be a call to a static method
				try {
					String className = caller.value.toString();
					clazz = ClassLoader.getSystemClassLoader().loadClass(className);
					lookingForStaticMethod = true;
					wrappers = new ArrayList<ParameterWrapper>();
					wrappers.add(new ParameterWrapper(clazz, null));
				} catch (ClassNotFoundException e) {
					clazz = ReflectUtility.PrimitiveToJavaWrapperMap.get(caller.type);
				}
			} else {
				clazz = ReflectUtility.PrimitiveToJavaWrapperMap.get(caller.type);
			}
		}
		List[] outputLists = marshallParameters(requestStrings, 3);
		List<Class<?>> parameterTypes = outputLists[0];
		if (parameterTypes.size() > 1) {
			throw new InvalidParameterException("While setting a field, there cannot be more than a single argument to the function!");
		}
		ParameterList parameters = (ParameterList) outputLists[1];
		String fieldName = requestStrings[2];
		Field field;
		try {
			field = clazz.getField(fieldName);
		} catch (NoSuchFieldException e) {
			if (clazz.equals(String.class)) {
				throw new NoSuchFieldException(e.getMessage() + " - NOTE: the source was treated as a String object!");
			} else {
				throw e;
			}
		} 			

		if (lookingForStaticMethod) {
			if (!Modifier.isStatic(field.getModifiers())) {		// checks if the field is truly static or throws an exception otherwise
				throw new InvalidParameterException("The field is not a static field!");
			}
		}
		
		JavaObjectList outputList = new JavaObjectList();
		if (parameters.isEmpty()) {
			for (int j = 0; j < wrappers.size(); j++) {
				Object result = field.get(wrappers.get(j).value);
				registerMethodOutput(result, outputList);
			}
		} else {
			if (wrappers.size() > 1 && parameters.getInnerSize() > 1 && wrappers.size() != parameters.getInnerSize()) {
				throw new InvalidParameterException("The length of the java.arraylist object is different of the length of the vectors in the parameters!");
			} else {
				int maxSize = Math.max(wrappers.size(), parameters.getInnerSize());
				for (int i = 0; i < maxSize; i++) {
					int j = i;
					if (parameters.getInnerSize() == 1) {
						j = 0;
					}
					int k = i;
					if (wrappers.size() == 1) {
						k = 0;
					}
					field.set(wrappers.get(k).value, parameters.getParameterArray(j)[0]);
//					registerMethodOutput(result, outputList);
				}		
			}
		}
		if (outputList.isEmpty()) {
			return null;
		} else if (outputList.size() == 1) {
			return outputList.get(0);
		} else {
			return outputList;
		}
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private Object processMethod(String[] requestStrings) throws Exception {
		Class clazz = null;
		List<ParameterWrapper> wrappers = null;
		boolean lookingForStaticMethod = false;
		if (requestStrings[1].startsWith("java.object")) {			// presumably non-static method
			wrappers = findObjectInEnvironment(requestStrings[1]);
			Object caller = getCallerAmongWrappers(wrappers);
			clazz = caller.getClass();
		} else {
			wrappers = createFromPrimitiveClass(getPrimitiveClass(requestStrings[1]), requestStrings[1]);
			ParameterWrapper caller = wrappers.get(0);
			if (wrappers.size() == 1 && caller.type.equals(String.class)) { // could be a call to a static method
				try {
					String className = caller.value.toString();
					clazz = ClassLoader.getSystemClassLoader().loadClass(className);
					lookingForStaticMethod = true;
					wrappers = new ArrayList<ParameterWrapper>();
					wrappers.add(new ParameterWrapper(clazz, null));
				} catch (ClassNotFoundException e) {
					clazz = ReflectUtility.PrimitiveToJavaWrapperMap.get(caller.type);
				}
			} else {
				clazz = ReflectUtility.PrimitiveToJavaWrapperMap.get(caller.type);
			}
		}
		List[] outputLists = marshallParameters(requestStrings, 3);
		List<Class<?>> parameterTypes = outputLists[0];
		ParameterList parameters = (ParameterList) outputLists[1];
		String methodName = requestStrings[2];
		Method met;
		try {
			if (parameters.isEmpty()) {
				met = clazz.getMethod(methodName, (Class[]) null);
			} else {
				met = clazz.getMethod(methodName, parameterTypes.toArray(new Class[]{}));
			}
		} catch (NoSuchMethodException e) {		
			if (parameters.isEmpty()) {
				throw e;
			} else {	// the exception might arise from the fact that the types are from derived classes
				met = findNearestMethod(clazz, methodName, parameterTypes);
			}
		} 			

		if (lookingForStaticMethod) {	
			if (!Modifier.isStatic(met.getModifiers())) {		// checks if the method is truly static or throws an exception otherwise
				throw new InvalidParameterException("The method is not a static method!");
			}
		}
		
		JavaObjectList outputList = new JavaObjectList();
		if (parameters.isEmpty()) {
			for (int j = 0; j < wrappers.size(); j++) {
				Object result = met.invoke(wrappers.get(j).value, (Object[]) null);
				registerMethodOutput(result, outputList);
			}
		} else {
			if (wrappers.size() > 1 && parameters.getInnerSize() > 1 && wrappers.size() != parameters.getInnerSize()) {
				throw new InvalidParameterException("The length of the java.arraylist object is different of the length of the vectors in the parameters!");
			} else {
				int maxSize = Math.max(wrappers.size(), parameters.getInnerSize());
				for (int i = 0; i < maxSize; i++) {
					int j = i;
					if (parameters.getInnerSize() == 1) {
						j = 0;
					}
					int k = i;
					if (wrappers.size() == 1) {
						k = 0;
					}
					Object result = met.invoke(wrappers.get(k).value, parameters.getParameterArray(j));
					registerMethodOutput(result, outputList);
				}		
			}
		}
		if (outputList.isEmpty()) {
			return null;
		} else if (outputList.size() == 1) {
			return outputList.get(0);
		} else {
			return outputList;
		}
	}

	private Object getCallerAmongWrappers(List<ParameterWrapper> wrappers) {
		if (wrappers == null || wrappers.isEmpty()) {
			return null;
		} else {
			Object higherLevelObject = null;
			for (ParameterWrapper wrapper : wrappers) {
				if (higherLevelObject == null) {
					higherLevelObject = wrapper.value;
				} else {
					Object newValue = wrapper.value;
					if (newValue.getClass().isAssignableFrom(higherLevelObject.getClass())) {
						higherLevelObject = newValue;
					}
				}
			}
			return higherLevelObject;
		}
	}

	@SuppressWarnings("rawtypes")
	private Method findNearestMethod(Class clazz, String methodName, List<Class<?>> parameterTypes) throws NoSuchMethodException {
		Method[] methods = clazz.getMethods();
		List<MethodWrapper> possibleMatches = new ArrayList<MethodWrapper>();
		for (Method method : methods) {
			if (method.getName().equals(methodName)) {	// possible match
				Class[] classes = method.getParameterTypes();
				double score = doParameterTypesMatch(classes, parameterTypes.toArray(new Class[]{}));
				if (score >= 0) {
					possibleMatches.add(new MethodWrapper(score, method));
				}
			}
		}
		if (possibleMatches.isEmpty()) {
			throw new NoSuchMethodException("Method " + methodName + "cannot be found in the class " + clazz.getSimpleName());
		} else {
			Collections.sort(possibleMatches);
		}
		return possibleMatches.get(0).method;
	}
	
	private void registerMethodOutput(Object result, JavaObjectList outputList) {
		if (result != null) {
			if (!ReflectUtility.JavaWrapperToPrimitiveMap.containsKey(result.getClass())) {
				put(System.identityHashCode(result), result);
			}
			outputList.add(new ParameterWrapper(result.getClass(), result));
		} 
	}
	
	private double doParameterTypesMatch(Class<?>[] ref, Class<?>[] obs) {
		if (ref == null && obs == null) {
			return 0d;
		} else if (ref != null && obs != null) {
			if (ref.length == obs.length) {
				double sumScores = 0d;
				for (int i = 0; i < ref.length; i++) {
					double score = isAssignableOfThisClass(ref[i], obs[i]);
					if (score == -1d) {
						return -1d;
					} else {
						sumScores += score;
					}
				}
				return sumScores;
			}
		}
		return -1d;
	}

	private boolean implementThisClassAsAnInterface(Class<?> refcl1, Class<?> cl) {
		if (ReflectUtility.JavaWrapperToPrimitiveMap.containsKey(refcl1)) {
			if (cl.equals(ReflectUtility.JavaWrapperToPrimitiveMap.get(refcl1))) {
				return true;
			}
		}
		if (ReflectUtility.PrimitiveToJavaWrapperMap.containsKey(refcl1)) {
			if (cl.equals(ReflectUtility.PrimitiveToJavaWrapperMap.get(refcl1))) {
				return true;
			}
		}
		Class<?>[] interfaces = cl.getInterfaces();
		for (Class<?> inter : interfaces) {
			if (inter.getName().equals(refcl1.getName())) {
				return true;
			}
		}
		return false;
	}
	
	private double isAssignableOfThisClass(Class<?> refcl1, Class<?> cl2) {
		int degree = 0;
		Class<?> cl = cl2;
		boolean isInterfaceMatching = implementThisClassAsAnInterface(refcl1, cl);
		while (!refcl1.getName().equals(cl.getName()) && !isInterfaceMatching && !cl.getName().equals("java.lang.Object")) {
			if (cl.isPrimitive()) {	// the superclass of primitive is assumed to be java.lang.Object
				cl = Object.class;
			} else {
				cl = cl.getSuperclass();
			}
			isInterfaceMatching = implementThisClassAsAnInterface(refcl1, cl);
			degree++;
		}
		if (refcl1.getName().equals(cl.getName())) {
			return degree;
		} else if (isInterfaceMatching) {
			return degree + .5;
		} else {
			return -1;
		}
	}
	
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private Object createObjectFromRequestStrings(String[] requestStrings) throws Exception {
		JavaObjectList outputList = new JavaObjectList();
		
		boolean isNull = requestStrings[0].equals(ConstructNullCode);
		boolean isArray = requestStrings[0].equals(ConstructArrayCode);
		boolean isNullArray = requestStrings[0].equals(ConstructNullArrayCode);
		
		String className = requestStrings[1];
		Class<?> clazz;
		if (ReflectUtility.PrimitiveTypeMap.containsKey(className)) {
			clazz = ReflectUtility.PrimitiveTypeMap.get(className);
		} else {
			clazz = ClassLoader.getSystemClassLoader().loadClass(className);
		}
		
		List[] outputLists = marshallParameters(requestStrings, 2);
		List<Class<?>> parameterTypes = outputLists[0];
		ParameterList parameters = (ParameterList) outputLists[1];
		
		if (parameters.isEmpty()) { // constructor with no argument then
			Object newInstance;
			if (isNull) {
				newInstance = new NullWrapper(clazz);
			} else {
				newInstance = getNewInstance(isArray, clazz, null, null);
			}
			registerNewInstance(newInstance, outputList);
		} else {
			for (int i = 0; i < parameters.getInnerSize(); i++) {
				Object newInstance;
				if (isNull) {
					newInstance = new NullWrapper(clazz);
				} else if (isNullArray) {
					Object fakeInstance = getNewInstance(true, clazz, parameterTypes.toArray(new Class[]{}), parameters.getParameterArray(i));	// true: is array
					newInstance = new NullWrapper(fakeInstance.getClass());
				} else {
					newInstance = getNewInstance(isArray, clazz, parameterTypes.toArray(new Class[]{}), parameters.getParameterArray(i));
				}
				registerNewInstance(newInstance, outputList);
			}
		}
		if (outputList.isEmpty()) {
			return null;
		} else if (outputList.size() == 1) {
			return outputList.get(0);
		} else {
			return outputList;
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Object getNewInstance(boolean isArray, Class clazz, Class[] paramTypes, Object[] paramValues) throws Exception {
		if (paramTypes == null) {
			if (isArray) {
				throw new InvalidParameterException("Constructing an array requires at least one parameter, namely an integer that determines the size of the array!");
			}
			return clazz.newInstance();
		} else {
			if (isArray) {
				int[] dimensions = (int[]) ReflectUtility.convertArrayType(paramValues, int.class);
				return Array.newInstance(clazz, dimensions);
			}
			if (clazz.isEnum()) {
				Method met = clazz.getMethod("valueOf", String.class);
				return met.invoke(null, paramValues[0].toString());
			} else {
				Constructor<?> constructor = clazz.getConstructor(paramTypes);
				return constructor.newInstance(paramValues);
			}
		}
	}
	
	
	private void registerNewInstance(Object newInstance, JavaObjectList outputList) {
		put(System.identityHashCode(newInstance), newInstance);
		outputList.add(new ParameterWrapper(newInstance.getClass(), newInstance));
	}
	
	
	@SuppressWarnings("rawtypes")
	private List[] marshallParameters(String[] args, int start) {
		List[] outputLists = new List[2];
		List<Class<?>> parameterTypes = new ArrayList<Class<?>>();
		ParameterList parameters = new ParameterList();
		outputLists[0] = parameterTypes;
		outputLists[1] = parameters;
		for (int i = start; i < args.length; i++) {
			List<ParameterWrapper> subparametersList;
			String primitiveClassIfAny = getPrimitiveClass(args[i]);
			if (primitiveClassIfAny != null) {
				subparametersList = createFromPrimitiveClass(primitiveClassIfAny, args[i]);
			} else {
				subparametersList = findObjectInEnvironment(args[i]);
			}
			parameterTypes.add(subparametersList.get(0).type);
			parameters.add(subparametersList);
		}
		return outputLists;
	}

	private String getPrimitiveClass(String string) {
		for (String key : PrimitiveTypeMap.keySet()) {
			if (string.startsWith(key)) {
				return key;
			}
		}
		return null;
	}

	
	private List<ParameterWrapper> createFromPrimitiveClass(String primitiveTypeClass, String args) {
		List<ParameterWrapper> wrappers = new ArrayList<ParameterWrapper>();
		String[] newArgs = args.substring(primitiveTypeClass.length()).split(SubSplitter);
		for (String value : newArgs) {
			if (primitiveTypeClass == "character") {
				wrappers.add(new ParameterWrapper(String.class, value));
			} else if (primitiveTypeClass == "numeric") {
				wrappers.add(new ParameterWrapper(double.class, Double.parseDouble(value)));
			} else if (primitiveTypeClass == "integer") {
				wrappers.add(new ParameterWrapper(int.class, Integer.parseInt(value)));
			} else if (primitiveTypeClass == "logical") {
				String subString = value.toLowerCase();
				wrappers.add(new ParameterWrapper(boolean.class, Boolean.valueOf(subString).booleanValue()));
			}
		}
		return wrappers;
	}

	
	/**
	 * Main entry point for creating a REnvironment hosted by a Java local gateway server.
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		JavaLocalGatewayServer server = null;
		try {
			List<String> arguments = J4RSystem.setClassicalOptions(args);
			String firstCall = J4RSystem.retrieveArgument(FIRSTCALL, arguments);
			if (firstCall != null && firstCall.toLowerCase().trim().equals("true")) {
				List<String> newCommands = new ArrayList<String>();
				newCommands.add(REnvironment.class.getName());
				String architecture = J4RSystem.getJavaArchitecture();
				String classPath;
				if (architecture.equals("32")) {
					classPath = "j4r_x86.jar";
				} else {
					classPath = "j4r.jar";
				}
				String extensionPath = J4RSystem.retrieveArgument(EXTENSION, arguments);
				if (extensionPath != null) {
					if (new File(extensionPath).exists()) {
						String classPathSeparator = ":";
						if (J4RSystem.isRunningOnWindows()) {
							classPathSeparator = ";";
						}
						classPath  = classPath + classPathSeparator + extensionPath;// + File.separator + "*";
					}
				}
				
				String port = J4RSystem.retrieveArgument(PORT, arguments);
				if (port != null) {
					newCommands.add(PORT);
					newCommands.add(port);
				}
				
				String memorySizeStr = J4RSystem.retrieveArgument(MEMORY, arguments);
				Integer memorySize = null;
				if (memorySizeStr != null) {
					try {
						memorySize = Integer.parseInt(memorySizeStr);
					} catch (Exception e) {
						memorySize = null;
					}
				}
				
				File jarFile = new File(REnvironment.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());
				File rootPath = jarFile.getParentFile();

				JavaProcessWrapper rGatewayProcessWrapper = new JavaProcessWrapper("RGateway Server", newCommands, rootPath);
				Runtime.getRuntime().addShutdownHook(new InternalShutDownHook(rGatewayProcessWrapper));
				JavaProcess rGatewayProcess = rGatewayProcessWrapper.getInternalProcess();
				rGatewayProcess.setClassPath(classPath);
				if (memorySize != null) {
					rGatewayProcess.setJVMMemory(memorySize);
				}
				if (J4RSystem.isCurrentJVMLaterThanThisVersion("1.8")) {
					rGatewayProcess.setOpenModuleForVersionsLaterThan8Enabled(true);
				}
				rGatewayProcessWrapper.run();
				System.exit(0);
			}
			String portStr = J4RSystem.retrieveArgument(PORT, arguments);
			int port;
			if (portStr != null) {
				port = Integer.parseInt(portStr);
			} else {
				port = 18011;		// default port
			}
			server = new JavaLocalGatewayServer(new ServerConfiguration(port), new REnvironment());
			server.startApplication();
		} catch (Exception e) {
			System.exit(1);
		}
	}
	
	
	
}
