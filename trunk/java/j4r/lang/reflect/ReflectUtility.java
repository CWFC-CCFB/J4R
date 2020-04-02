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
package j4r.lang.reflect;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


public class ReflectUtility {

	public final static Set<Class<?>> PrimitiveWrappers = new HashSet<Class<?>>();
	static {
		PrimitiveWrappers.add(Byte.class);
		PrimitiveWrappers.add(Short.class);
		PrimitiveWrappers.add(Character.class);
		PrimitiveWrappers.add(Integer.class);
		PrimitiveWrappers.add(Long.class);
		PrimitiveWrappers.add(Float.class);
		PrimitiveWrappers.add(Double.class);
		PrimitiveWrappers.add(Boolean.class);
	}
	
	public final static Map<String, Class<?>> PrimitiveTypeMap = new HashMap<String, Class<?>>();
	static {
		PrimitiveTypeMap.put("byte", byte.class);
		PrimitiveTypeMap.put("short", short.class);
		PrimitiveTypeMap.put("char", char.class);
		PrimitiveTypeMap.put("int", int.class);
		PrimitiveTypeMap.put("long", long.class);
		PrimitiveTypeMap.put("float", float.class);
		PrimitiveTypeMap.put("double", double.class);
		PrimitiveTypeMap.put("boolean", boolean.class);
	}

	public final static Map<Class<?>, Class<?>> JavaWrapperToPrimitiveMap = new HashMap<Class<?>, Class<?>>();
	static {
		JavaWrapperToPrimitiveMap.put(Double.class, double.class);
		JavaWrapperToPrimitiveMap.put(Integer.class, int.class);
		JavaWrapperToPrimitiveMap.put(Long.class, long.class);
		JavaWrapperToPrimitiveMap.put(Float.class, float.class);
		JavaWrapperToPrimitiveMap.put(String.class, String.class);
		JavaWrapperToPrimitiveMap.put(Boolean.class, boolean.class);
		JavaWrapperToPrimitiveMap.put(Character.class, char.class);
	}

	public final static Map<Class<?>, Class<?>> PrimitiveToJavaWrapperMap = new HashMap<Class<?>, Class<?>>();
	static {
		PrimitiveToJavaWrapperMap.put(double.class, Double.class);
		PrimitiveToJavaWrapperMap.put(int.class, Integer.class);
		PrimitiveToJavaWrapperMap.put(long.class, Long.class);
		PrimitiveToJavaWrapperMap.put(float.class, Float.class);
		PrimitiveToJavaWrapperMap.put(String.class, String.class);
		PrimitiveToJavaWrapperMap.put(boolean.class, Boolean.class);
		PrimitiveToJavaWrapperMap.put(char.class, Character.class);
	}

	
	/**
	 * This static method returns all the fields from a class including those
	 * inherited.
	 * @param clazz a Class object
	 * @return a List of Field instances
	 */
	public static List<Field> retrieveAllFieldsFromClass(Class<?> clazz) {
		List<Field> fields = new ArrayList<Field>();
		do {
			Field[] fieldFromThisClass = clazz.getDeclaredFields();
			fields.addAll(Arrays.asList(fieldFromThisClass));
		} while ((clazz = clazz.getSuperclass()) != null);
		return fields;
	}

	/**
	 * This method returns true if the class is primitive or 
	 * a wrapper for a primitive.
	 * @param clazz
	 * @return a boolean
	 */
	public static boolean isPrimitive(Class<?> clazz) {
		return PrimitiveWrappers.contains(clazz) || PrimitiveTypeMap.values().contains(clazz);
	}
	
	
	/**
	 * This method returns the dimensions of an array. The array is assumed to be consistent, ie. the
	 * row lengths do not change across the row.
	 * @param array an array
	 * @return an array of integer
	 */
	public static int[] getDimensions(Object[] array) {
		String className = array.getClass().getName();
		int nbDimensions = className.lastIndexOf("[") + 1;
		int[] dimensions = new int[nbDimensions];
		Object current = array;
		for (int dim = 0; dim < nbDimensions; dim++) {
			dimensions[dim] = Array.getLength(current);
			current = Array.get(current, 0);
		}
		return dimensions;
	}
	
	
	/**
	 * This method is used to convert an array of particular class into an array of another
	 * class. Typically, it could be used to convert an array of Object into an array of double
	 * if all the elements of the original array are instances of double.
	 * @param currentArray
	 * @param clazz
	 * @return an Object 
	 */
	public static Object convertArrayType(Object[] currentArray, Class<?> clazz) {
		if (currentArray == null || currentArray.length == 0) {
			throw new InvalidParameterException("The array is either null or empty!");
		} else {
			int[] dimensions = getDimensions(currentArray);
			Object newArray =  Array.newInstance(clazz, dimensions);
			for (int i = 0; i < currentArray.length; i++) {
				if (currentArray[i].getClass().isArray()) {
					Array.set(newArray, i, convertArrayType((Object[]) currentArray[i], clazz));
				} else {
					Object currentValue = currentArray[i];
					if (PrimitiveToJavaWrapperMap.containsKey(clazz)) {
						if (clazz.equals(double.class)) {
							Array.set(newArray, i, ((Number) currentValue).doubleValue());
						} else if (clazz.equals(int.class)) {
							Array.set(newArray, i, ((Number) currentValue).intValue());
						} else if (clazz.equals(long.class)) {
							Array.set(newArray, i, ((Number) currentValue).longValue());
						} else if (clazz.equals(float.class)) {
							Array.set(newArray, i, ((Number) currentValue).floatValue());
						} else if (clazz.equals(String.class)) {
							Array.set(newArray, i, currentValue.toString());
						} else if (clazz.equals(char.class)) {
							Array.set(newArray, i, ((Character) currentValue).charValue());
						} else if (clazz.equals(boolean.class)) {
							Array.set(newArray, i, ((Boolean) currentValue).booleanValue());
						} else {
							throw new InvalidParameterException("The primitive type is not recognized!");
						}
					} else {
						Array.set(newArray, i, clazz.cast(currentArray[i]));
					}
				}
			}
			return newArray;
		}
	}

}
