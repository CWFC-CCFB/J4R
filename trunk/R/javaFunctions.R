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
          outputList <- list()
          outputList[[1]] <- javaObj
          return(new_java.list(outputList))
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
is.JavaArray <- function(object) {
  if (!methods::is(object, "java.object")) {
    stop("The object must be an instance of java.object")
  }
  if (startsWith(object$.class,"[")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#'
#' Check if the java.object instance represents an Array
#'
#' This function returns true if the Java instance represented
#' by this java.object is an Array.
#'
#' This function is deprecated. Please use the is.JavaArray instead.
#'
#' @param object a java.object instance
#'
#' @export
isJavaArray <- function(object) {
  .Deprecated("is.JavaArray")
  return(is.JavaArray(object))
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
  if (!is.JavaArray(object)) {
    stop("The object parameter must represent an array!")
  }
  if (is.null(index)) {
    index <- 0:(length(value)-1)
  }
  invisible(J4R::callJavaMethod("java.lang.reflect.Array", "set", object, as.integer(index), value))
}

#'
#' Get a value from an array
#'
#' This function returns the value at location given by
#' the index parameter.
#'
#' @param object a java.object that represents an array
#' @param ... a series of integers that correspond to the index of the value. Note that in Java
#' the first index is 0
#'
#' @return the value at the location
#'
#' @export
getValueFromArray <- function(object, ...) {
  if (!is.JavaArray(object)) {
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
#   if (!is.JavaArray(object)) {
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
  if (!is.JavaArray(object)) {
    stop("The object parameter must represent an array!")
  }
  return(J4R::callJavaMethod("java.lang.reflect.Array", "getLength", object))
}


#'
#' Returns all the elements of a Java array
#'
#' All the elements of an array are returned. If these elements are Java instances,
#' then the function value is a java.list of java.object references. Otherwise,
#' the value is either a vector or a matrix
#'
#' @param object a java.object reference pointing to a Java array
#'
#' @return either a java.list object, a vector or a matrix
#'
#' @export
getAllValuesFromArray <- function(object) {
  if (!is.JavaArray(object)) {
    stop("The object parameter must represent an array!")
  }
  is2DArray <- startsWith(object$.class, "[[")
  length <- getArrayLength(object)
  if (length == 0) {
    return(c())
  } else if (length == 1) {
     javaObj <- J4R::callJavaMethod("java.lang.reflect.Array", "get", object, 0:(length-1))
     if (methods::is(javaObj, "java.object")) {
       outputList <- list()
       # outputList <- java.list()
       outputList[[1]] <- javaObj
       return(new_java.list(outputList))
       # return(outputList)
     } else {
       return(c(javaObj))
     }
  } else {
    if (is2DArray) {
      tmpObj <- callJavaMethod("java.lang.reflect.Array", "get", object, 0:(length-1))
      length2d <- getArrayLength(tmpObj[[1]])
      output <- lapply(1:length, function(i, tmpObj) {
        getAllValuesFromArray(tmpObj[[i]])
        # callJavaMethod("java.lang.reflect.Array", "get", tmpObj, rep(i,length(tmpObj)))
      }, tmpObj)
      unlistedOutput <- unlist(output)
      if (is.atomic(unlistedOutput)) {
        return(matrix(unlistedOutput, nrow = length, ncol = length2d, byrow = TRUE))
      } else {
        return(output)
      }
    } else {
      return(callJavaMethod("java.lang.reflect.Array", "get", object, 0:(length-1)))
    }
  }
}


classMatchForArrayConstruction <- c("numeric" = "double", "integer" = "int", "character" = "java.lang.String", "logical" = "boolean")

#'
#' Create a Java array from an R array
#'
#' Converts an R array into a Java array.
#'
#' @param values a vector or a matrix
#'
#' @return a java.object reference that points a Java array
#'
#' @export
as.JavaArray <- function(values) {
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
        if (nbDimensions > 2) {
          stop("The as.JavaArray function is not implemented for arrays with more than two dimensions!")
        }
        if (nbDimensions == 1) {
          myArray <- createJavaObject(thisClass, dimensions, isArray = T)
          setValueInArray(myArray,values)
        } else if (nbDimensions == 2) {
          myArray <- createJavaObject(thisClass, dimensions[1], dimensions[2], isArray = T)
          lapply(1:dimensions[1], function(i, myArray) {
            mySubArray <- callJavaMethod("java.lang.reflect.Array", "get", myArray, as.integer(i-1))
            setValueInArray(mySubArray,values[i,])
          }, myArray)
        } else {
          stop("The as.JavaArray function is not implemented for arrays with more than two dimensions!")
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

