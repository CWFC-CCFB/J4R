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
package j4r.util;

import java.net.URISyntaxException;

public final class ObjectUtility {

	public static final String PathSeparator = "/";


	
	/**
	 * This method returns the path of a particular class. 
	 * @param anyClass any class of the system
	 * @return a String instance
	 */
	@SuppressWarnings("rawtypes")
	public static String getBinPath(Class anyClass) {
		String binPath;
		try {
			binPath = anyClass.getProtectionDomain().getCodeSource().getLocation().toURI().getPath();
			int lastPathSeparator = binPath.lastIndexOf(PathSeparator);
			binPath = binPath.substring(0, lastPathSeparator).concat(PathSeparator);
	    	return binPath;
		} catch (URISyntaxException e) {
			return null;
		}
	}

	
	/**
	 * This method returns the root path of the application. 
	 * @param anyClass any class of the system
	 * @return a String instance
	 */
	@SuppressWarnings("rawtypes")
	public static String getTrueRootPath(Class anyClass) {
		try {
			String binPath = getBinPath(anyClass);
			binPath = binPath.substring(0, binPath.length() - 1); // to remove the last separator
			int lastPathSeparator = binPath.lastIndexOf(PathSeparator);
			return binPath.substring(0, lastPathSeparator).concat(PathSeparator);
		} catch (Exception e) {
			return null;
		}
	}
	
	
	/**
	 * This method returns the path of a particular class. 
	 * @param anyClass any class of the system
	 * @return a String instance
	 */
	@SuppressWarnings("rawtypes")
	public static String getPackagePath(Class anyClass) {
		String binPath = getBinPath(anyClass);
		if (binPath == null) {
			return null;
		} else {
			String packagePath = binPath.concat(ObjectUtility.getRelativePackagePath(anyClass));
	    	return packagePath;
		}
	}

	
	/**
	 * This method returns the relative path of a particular class with respect to the rootpath. 
	 * @param anyClass any class of the system
	 * @return a String instance
	 */
	@SuppressWarnings("rawtypes")
	public static String getRelativePackagePath(Class anyClass) {
		return anyClass.getPackage().getName().replace(".", ObjectUtility.PathSeparator) + ObjectUtility.PathSeparator;
	}


	
	
	
	
}