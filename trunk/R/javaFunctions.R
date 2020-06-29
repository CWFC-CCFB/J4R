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
isJavaArray <- function(object) {
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
#' @param index the index of the location at which the value is set. Note that in Java
#' the first index is 0. If this argument is set to NULL, then it is assumed that the value
#' is set to index 0. In case of vectorization, the values are set from 0 to length(value) - 1
#' if this argument is left to NULL.
#'
#' @export
setValueInArray <- function(object, value, index = NULL) {
  if (!isJavaArray(object)) {
    stop("The object parameter must represent an array!")
  }
  if (is.null(index)) {
    index <- 0:(length(value)-1)
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
getValueFromArray <- function(object, ...) {
  if (!isJavaArray(object)) {
    stop("The object parameter must represent an array!")
  }
  parms <- list(...)
  lParms <- length(parms)
  value <- object
  i <- 1
  while (i <= lParms) {
    value <- J4R::callJavaMethod("java.lang.reflect.Array", "get", value, as.integer(parms[[i]]))
    i <- i + 1
  }
  return(value)
}

# .getValueFromJavaArray <- function(object, index) {
#   if (!isJavaArray(object)) {
#     stop("The object parameter must represent an array!")
#   }
#   return(J4R::callJavaMethod("java.lang.reflect.Array", "get", object, as.integer(index)))
# }


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
  if (!isJavaArray(object)) {
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
  if (!isJavaArray(object)) {
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

classMatchForArrayConstruction <- c("numeric" = "double", "integer" = "int", "character" = "java.lang.String", "logical" = "boolean")

#'
#' Create array
#'
#' Creates an array of particular class. The dimension of the
#' array are controled through the length1 and length2 arguments.
#' The argument length2 is set to 0 by default so that the function
#' returns a one-dimension array.
#'
#' If any of the three parameters is a vector, then the method relies on vectorization and the
#' value of the function will be a java.list with all the java.object references.
#'
#' @param class the class name of the instance of the array
#' @param length1 a positive integer that defines the length of the array
#' @param length2 an optional positive integer that defines the second dimension of the array
#'
#' @return a java.object or a java.list object if the vectorization was used.
#'
#' @export
createArray <- function(values) {
  if (!is.null(values)) {
    if (length(values) > 0) {
      if (is.atomic(values)) {
        if (is.array(values)) {
          dimensions <- dim(values)
        } else {
          dimensions <- length(values)
        }
        thisClass <- class(values[1])
        matchNames <- names(classMatchForArrayConstruction)
        if (!thisClass %in% matchNames) {
          stop("The argument values should be of the integer, numeric, logical or character type")
        }
        thisClass <- classMatchForArrayConstruction[thisClass]
        nbDimensions <- length(dimensions)
        if (nbDimensions == 1) {
          myArray <- createJavaObject(thisClass, dimensions, isArray = T)
          setValueInArray(myArray,values)
        } else if (nbDimensions == 2) {
          myArray <- createJavaObject(thisClass, dimensions[1], dimensions[2], isArray = T)
          lapply(1:nbDimensions[1], function(i) {
            mySubArray <- callJavaMethod("java.lang.reflect.Array", "get", myArray, i)
            setValueInArray(mySubArray,values[i,])
          })
        } else if (nbDimensions == 3) {
          myArray <- createJavaObject(thisClass, dimensions[1], dimensions[2], dimensions[3], isArray = T)
        }
        return(myArray)
      } else {
        stop("The argument values should be of atomic types!")
      }
    } else {
      stop("The argument values should at least contain one element!")
    }
  } else {
    stop("The argument values cannot be null!")
  }
}

