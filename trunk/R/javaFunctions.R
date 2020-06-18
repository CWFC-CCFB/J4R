#########################################################
# R functions for easier object handling
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################


#'
#' Returns all the elements of a Java instance of List
#'
#' All the elements of a Java List instance are returned.
#'
#' @param object a java.object that represents a Java List instance
#'
#' @return either a java.list object or an R vector
#'
#' @export
getAllValuesFromListObject <- function(object) {
  if (!methods::is(object, "java.object")) {
    stop("The object must be an instance of java.object")
  } else {
    systemClassLoader <- callJavaMethod("java.lang.ClassLoader", "getSystemClassLoader")
    listClass <- callJavaMethod(systemClassLoader, "loadClass", "java.util.List")
    if (callJavaMethod(listClass, "isInstance", object)) {
      size <- callJavaMethod(object, "size")
      if (size == 0) {
        return(c()) ## an empty vector
      } else if (size == 1) {
        javaObj <- callJavaMethod(object, "get", 0:(size-1))
        if (methods::is(javaObj, "java.object")) {
          outputList <- java.list()
          outputList[[1]] <- javaObj
          return(outputList)
        } else {
          return(c(javaObj))
        }
      } else {
        return(callJavaMethod(object, "get", 0:(size-1)))
      }
    } else {
      stop("The object is not an instance of List")
    }
  }
}

#'
#' Check if the java.object instance represents an Array
#'
#' This function returns true if the Java instance represented
#' by this java.object is an Array.
#'
#' @param object a java.object instance
#' @return a logical
#'
#' @export
isArray <- function(object) {
  if (!methods::is(object, "java.object")) {
    stop("The object must be an instance of java.object")
  }
  if (startsWith(object$class,"[")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'
#' Set a value in an array
#'
#' This function sets the value at the location given by the index
#' parameter. It relies on the reflexive methods the Java class Array.
#'
#' @param object a java.object that represents an array
#' @param value the value to be set
#' @param index the index of the location at which the value is retrieved. Note that in Java
#' the first index is 0
#'
#' @export
setValueInArray <- function(object, value, index) {
  if (!isArray(object)) {
    stop("The object parameter must represent an array!")
  }
  J4R::callJavaMethod("java.lang.reflect.Array", "set", object, as.integer(index), value)
}

#'
#' Get a value from an array
#'
#' This function returns the value at location given by
#' the index parameter.
#'
#' @param object a java.object that represents an array
#' @param index the index of the location at which the value is retrieved. Note that in Java
#' the first index is 0
#'
#' @return the value at the location
#'
#' @export
getValueFromArray <- function(object, index) {
  if (!isArray(object)) {
    stop("The object parameter must represent an array!")
  }
  return(J4R::callJavaMethod("java.lang.reflect.Array", "get", object, as.integer(index)))
}

#'
#' Return the length of an Array instance
#'
#' This method returns an integer that is the
#' length of the Array.
#'
#' @param object a java.object instance that represents an array
#' @return an integer that is the length of the array
#'
#' @export
getArrayLength <- function(object) {
  if (!isArray(object)) {
    stop("The object parameter must represent an array!")
  }
  return(J4R::callJavaMethod("java.lang.reflect.Array", "getLength", object))
}



#'
#' Returns all the elements of an array
#'
#' All the elements of an array are returned.
#'
#' @param object a java.object that represents a Java List instance
#'
#' @return either a java.arraylist object or an R vector
#'
#' @export
getAllValuesFromArray <- function(object) {
  if (!isArray(object)) {
    stop("The object parameter must represent an array!")
  }
  length <- getArrayLength(object)
  if (length == 0) {
    return(c())
  } else if (length == 1) {
    javaObj <- J4R::callJavaMethod("java.lang.reflect.Array", "get", object, 0:(length-1))
    if (methods::is(javaObj, "java.object")) {
      outputList <- java.list()
      outputList[[1]] <- javaObj
      return(outputList)
    } else {
      return(c(javaObj))
    }
  } else {
    return(J4R::callJavaMethod("java.lang.reflect.Array", "get", object, 0:(length-1)))
  }
}


