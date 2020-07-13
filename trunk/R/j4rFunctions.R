########################################################
# R functions for connection to Gateway Server in Java
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

createCommandToken <- "co"
createNullArrayToken <- "cona"
createNullToken <- "conu"
createArrayToken <- "coar"
classInfoToken <- "cli"

numericToken <- "nu"
integerToken <- "in"
logicalToken <- "lo"
characterToken <- "ch"
javaObjectToken <- "JO"
javaListToken <- "JL"
javaListAndMainSplitterToken <- paste(javaListToken, MainSplitter, sep="")

numericTokenLength <- nchar(numericToken) + 1
integerTokenLength <- nchar(integerToken) + 1
logicalTokenLength <- nchar(logicalToken) + 1
characterTokenLength <- nchar(characterToken) + 1
javaListAndMainSplitterTokenLength <- nchar(javaListAndMainSplitterToken) + 1

portSplitter <- ":"


.translateJavaObject <- function(javaObject) {
  if (methods::is(javaObject, "java.list")) {
    hashcode <- sapply(javaObject$.innerList, function(obj) {
      as.character(obj$.hashcode)
    })
  } else if (methods::is(javaObject, "java.object")) {
    hashcode <- as.character(javaObject$.hashcode)
  } else {
    stop(".translateJavaObject: the argument should be an instance of java.object or java.list")
  }
  str <- paste("hashcode",paste(hashcode, collapse=SubSplitter), sep="")
  return(str)
}

.getParametersLength <- function(parameters) {
  maxLength <- 0
  if (length(parameters) > 0) {
#    for (i in 1:length(parameters)) {
    for (parm in parameters) {
      thisParameterLength <- length(parm)
      if (thisParameterLength >= maxLength) {
        maxLength <- thisParameterLength
      } else if (thisParameterLength > 1) {
        stop("The parameters are not consistent! Those with sizes greater than 1 should all have the same size!")
      }
    }
  }
  return(maxLength)
}


.getSourceLength <- function(source, parametersLength) {
    lengthSource <- length(source)
    if (lengthSource > 1 && parametersLength > 1 && lengthSource != parametersLength) {
      stop("The length of the java.list object or the vector is inconsistent with the length of the parameters!")
    } else {
      sourceLength <- length(source)
    }
  return(sourceLength)
}


#'
#' Create Java objects
#'
#' This function creates one or many object of a particular class. If the parameters
#' contain vectors, then a series of instances of this class can be created. Primitive
#' type are converted on the fly, numeric to double, integer to int,
#' logical to boolean and character to String. Factors are also converted to String.
#'
#' @param class the Java class of the object (e.g. java.util.ArrayList)
#' @param ... the parameters to be passed to the constructor of the object
#' @param isNullObject a logical that indicates whether the instance should be null (by default it is set to FALSE)
#' @param isArray a logical that indicates whether the instance is an array. By default, it is set to FALSE. When creating an array, the parameters must be integers that define the dimensions of the array
#' @param affinity a parameter used by the mclapply.j4r function in case of multithreading.
#' @return a java.object or java.list instance in the R environment
#' @examples
#' ### starting Java
#' connectToJava(memorySize = 200)
#'
#' ### creating an empty ArrayList object
#' createJavaObject("java.util.ArrayList")
#'
#' ### creating an ArrayList instance with initial capacity of 3
#' createJavaObject("java.util.ArrayList", as.integer(3))
#'
#' ### creating two ArrayList with different capacities
#' createJavaObject("java.util.ArrayList", c(as.integer(3), as.integer(4)))
#'
#' ### creating a 3x3 array of integers
#' myArray <- createJavaObject("int", 3, 3, isArray = TRUE)
#'
#' ### creating two arrays of integers with length 3
#' myArrays <- createJavaObject("int", c(3,3), isArray = TRUE)
#'
#' ### shutting down Java
#' shutdownJava()
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
createJavaObject <- function(class, ..., isNullObject = FALSE, isArray = FALSE, affinity = 1) {
  parameters <- list(...)
  parametersLength <- .getParametersLength(parameters)
  firstCommand <- createCommandToken
  if (isNullObject) {
    if (isArray) {
      firstCommand <- createNullArrayToken
    } else {
      firstCommand <- createNullToken
    }
  } else if (isArray) {
    firstCommand <- createArrayToken
  }

  nbCalls <- ceiling(parametersLength / maxVectorLength)
  if (nbCalls == 0) { ## to make sure it goes through the loop at least once
    nbCalls <- 1
  }
  basicCommand <- paste(firstCommand, class, sep=MainSplitter)
  output <- NULL
  for (i in 1:nbCalls) {
    if (parametersLength > 0) {
      lowerIndex <- (i-1) * maxVectorLength + 1
      upperIndex <- i * maxVectorLength
      if (upperIndex > parametersLength) {
        upperIndex <- parametersLength
      }
      command <- paste(basicCommand, .marshallCommand(parameters, lowerIndex, upperIndex), sep=MainSplitter)
    } else {
      command <- basicCommand
    }
    utils::write.socket(.getSocket(affinity), command)
    callback <- utils::read.socket(.getSocket(affinity), maxlen = bufferLength)
    output <- .processResult(callback, output, affinity)
  }
  return(output)
}


.checkForExceptionInCallback <- function(callback) {
  if (startsWith(callback, ExceptionPrefix)) {
    stop(substring(callback, nchar(ExceptionPrefix) + 2))
  }
}


.marshallCommand <- function(list, lowerBoundIndex, upperBoundIndex) {
  subCommands <- unlist(lapply(list, function(parm) {
    l <- length(parm)
    if (l > 1) {
      if (upperBoundIndex > l) {
        stop("The upperBoundIndex paramerer is larger than the size of the parameter!")
      }
      parm <- parm[lowerBoundIndex:upperBoundIndex]
    }
    class <- class(parm)[1]
    if (methods::is(parm, "java.object") || methods::is(parm, "java.list")) {
      class <- "java.object"
      parm <- .translateJavaObject(parm)
    } else if (methods::is(parm, "factor")) {
      class <- "character"
      parm <- as.character(parm)
    }
    subCommand <- paste(class, paste(parm,collapse=SubSplitter), sep="")
  }), use.names = F)
  command <- paste(subCommands, collapse = MainSplitter)
  return(command)
}

.constructSourcePartCommand <- function(prefix, source, sourceLength, targetName, lowerIndex, upperIndex) {
  if (methods::is(source, "java.object")) {   ### non-static method
    command <- paste(prefix, paste("java.object", .translateJavaObject(source), sep=""), targetName, sep=MainSplitter)
  } else if (methods::is(source, "java.list")) {   ### non-static method
    # subList <- .getSubsetOfJavaArrayList(source, lowerIndex, upperIndex)
    # command <- paste(prefix, paste("java.object", .translateJavaObject(subList), sep=""), targetName, sep=MainSplitter)
    command <- paste(prefix, paste("java.object", .translateJavaObject(source[lowerIndex:upperIndex]), sep=""), targetName, sep=MainSplitter)
  } else {  ### static method or primitive
    clazz <- class(source)
    if (clazz == "factor") {
      clazz <- "character"
      source <- as.character(source)
    }
    if (sourceLength == 1) {
      command <- paste(prefix, paste(class(source), source, sep=""), targetName, sep=MainSplitter)
    } else {
      command <- paste(prefix, paste(class(source), paste(source[lowerIndex:upperIndex], collapse=SubSplitter), sep=""), targetName, sep=MainSplitter)
    }
  }
  return(command)
}

#'
#' Get the value of a public field
#'
#' This function gets the value of a particular field, which can be either static or not. If the field is static,
#' the source should be a valid class name.
#'
#' @param source this should be either a java.list instance or a single java.object instance for non-static methods or
#' a string representing the Java class name in case of static method
#' @param fieldName the name of the field to be set
#' @param affinity a parameter used by the mclapply.j4r function in case of multithreading.
#'
#' @export
getJavaField <- function(source, fieldName, affinity = 1) {
  if (length(fieldName) != 1) {
    stop("The function getJavaField can only take a single field name!" )
  }
  parametersLength <- 0
  maxLength <- parametersLength
  sourceLength <- .getSourceLength(source, parametersLength)
  if (sourceLength > maxLength) {
    maxLength <- sourceLength ### then the source drives the nb of calls
  }

  nbCalls <- ceiling(maxLength / maxVectorLength)

  output <- NULL
  for (i in 1:nbCalls) {
    lowerIndex <- (i-1) * maxVectorLength + 1
    upperIndex <- i * maxVectorLength
    if (upperIndex > maxLength) {
      upperIndex <- maxLength
    }

    command <- .constructSourcePartCommand("field", source, sourceLength, fieldName, lowerIndex, upperIndex)

    utils::write.socket(.getSocket(affinity), command)
    callback <- utils::read.socket(.getSocket(affinity), maxlen=bufferLength)
    output <- .processResult(callback, output, affinity)
  }
  return(output)
}



#'
#' Set the value of a public field
#'
#' This function sets a particular field, which can be either static or not. If the field is static,
#' the source should be a valid class name.
#'
#' @param source this should be either a java.list instance or a single java.object instance for non-static methods or
#' a string representing the Java class name in case of static method
#' @param fieldName the name of the field to be set
#' @param value the new value of the field
#' @param affinity a parameter used by the mclapply.j4r function in case of multithreading.
#'
#' @export
setJavaField <- function(source, fieldName, value, affinity = 1) {
  parameters <- list(value)
  parametersLength <- .getParametersLength(parameters)
  maxLength <- parametersLength
  sourceLength <- .getSourceLength(source, parametersLength)
  if (sourceLength > maxLength) {
    maxLength <- sourceLength ### then the source drives the nb of calls
  }

  nbCalls <- ceiling(maxLength / maxVectorLength)

  output <- NULL
  for (i in 1:nbCalls) {
    lowerIndex <- (i-1) * maxVectorLength + 1
    upperIndex <- i * maxVectorLength
    if (upperIndex > maxLength) {
      upperIndex <- maxLength
    }

    command <- .constructSourcePartCommand("field", source, sourceLength, fieldName, lowerIndex, upperIndex)

    if (length(parameters) > 0) {
      if (maxLength == 1) {
        command <- paste(command, .marshallCommand(parameters, 1, 1), sep=MainSplitter)
      } else {
        command <- paste(command, .marshallCommand(parameters, lowerIndex, upperIndex), sep=MainSplitter)
      }
    }
    utils::write.socket(.getSocket(affinity), command)
    callback <- utils::read.socket(.getSocket(affinity), maxlen=bufferLength)
    output <- .processResult(callback, output, affinity)
  }
  if (!is.null(output)) {
    warning("The Java server has returned something else than NULL!")
    return(output)
  } else {
    return(invisible(output))
  }
}


#'
#' Call a Java method
#'
#' This function calls a public method in a particular class of object. If the javaObject parameters or the additional
#' parameters (...) include vectors, the method is called several times and a vector of primitive or a list of java
#' instances can be returned.
#'
#' There is no need to cast a particular parameter to a super class. Actually, the Java server tries to find the method
#' that best matches the types of the parameters. Primitive type are converted on the fly, numeric to double, integer to int,
#' logical to boolean and character to String. Factors are also converted to String.
#'
#' @param source this should be either a java.list instance or a single java.object instance for non-static methods or
#' a string representing the Java class name in case of static method
#' @param methodName the name of the method
#' @param ... the parameters of the method
#' @param affinity a parameter used by the mclapply.j4r function in case of multithreading.
#' @return It depends on the method. It can return a primitive type (or a vector of primitive), a Java instance (or a list of Java instances) or nothing at all.
#' @examples
#' ### starting Java
#' connectToJava(memorySize = 200)
#'
#' ### creating an empty ArrayList object
#' myList <- createJavaObject("java.util.ArrayList")
#'
#' ### adding 3 to the list
#' callJavaMethod(myList, "add", 3)
#'
#' ### shutting down Java
#' shutdownJava()
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
callJavaMethod <- function(source, methodName, ..., affinity = 1) {
  parameters <- list(...)
  parametersLength <- .getParametersLength(parameters)
  maxLength <- parametersLength
  sourceLength <- .getSourceLength(source, parametersLength)

  if (sourceLength > maxLength) {
    maxLength <- sourceLength ### then the source drives the nb of calls
  }

  nbCalls <- ceiling(maxLength / maxVectorLength)

  output <- NULL
  for (i in 1:nbCalls) {
    lowerIndex <- (i-1) * maxVectorLength + 1
    upperIndex <- i * maxVectorLength
    if (upperIndex > maxLength) {
      upperIndex <- maxLength
    }

    command <- .constructSourcePartCommand("method", source, sourceLength, methodName, lowerIndex, upperIndex)

    if (length(parameters) > 0) {
      if (maxLength == 1) {
        command <- paste(command, .marshallCommand(parameters, 1, 1), sep=MainSplitter)
      } else {
        command <- paste(command, .marshallCommand(parameters, lowerIndex, upperIndex), sep=MainSplitter)
      }
    }
    utils::write.socket(.getSocket(affinity), command)
    callback <- utils::read.socket(.getSocket(affinity), maxlen=bufferLength)
    output <- .processResult(callback, output, affinity)
  }
  if (is.null(output)) {
    return(invisible(output))
  } else {
    return(output)
  }
  return(output)
}

.processResult <- function(callback, output, affinity = 1) {
  result <- .processCallback(callback, affinity)
  if (is.null(output)) {
    output <- result
  } else {
    if (methods::is(output, "java.list")) {
      output <- .addToInnerList(output, result)
    } else {
      output <- c(output, result)
    }
  }
  return(output)
}


.processCallback <- function(callback, affinity = 1) {
  .checkForExceptionInCallback(callback)
  if (startsWith(callback, javaObjectToken)) {  ## a single Java object
    returnObject <- .createJavaObjectReference(callback, affinity)
  } else if (startsWith(callback, javaListToken) && regexpr("@", callback) >= 0) { ## a list of Java objects
    returnObject <- .createJavaObjectReference(callback, affinity)
  } else if (startsWith(callback, "Done")) {
    returnObject <- NULL
  } else {
    returnObject <- .translatePrimitiveType(callback)
  }
  return(returnObject)
}

.translatePrimitiveType <- function(str) {
  if (startsWith(str, javaListAndMainSplitterToken)) {
    str <- substring(str, javaListAndMainSplitterTokenLength)
  }
  inputList <- strsplit(str,SubSplitter)[[1]]
  outputList <- lapply(inputList, function(str) {
    if (startsWith(str, numericToken)) { # starts with numeric
      return(as.numeric(substring(str, numericTokenLength)))
    } else if (startsWith(str, integerToken)) { # starts with integer
      value <- as.double(substring(str, integerTokenLength))  ### to avoid coercion
      if (abs(value) < 2*10^9) {
        value <- as.integer(value)
      }
      return(value)
    } else if (startsWith(str, logicalToken)) { # starts with logical
      return(as.logical(substring(str, logicalTokenLength)))
    } else if (startsWith(str, characterToken)) { # starts with character
      return(as.character(substring(str, characterTokenLength)))
    } else {
      stop(paste("This primitive type is not recognized:", str, sep = " "))
    }
  })

  return(.convertListToVectorIfPossible(outputList))
}


.convertListToVectorIfPossible <- function(myList) {
  classes <- sapply(myList, function(a) { ### check if the classes are the same to avoid coercion
    class(a)
  })
  if (all(classes == classes[1])) {
    return(unlist(myList, use.names = F)) ### use.names set to F to improve performance
  } else {
    return(myList)
  }
}


#'
#' Provide a list of the Java references
#'
#' The function provides the list of the Java references in an environment.
#'
#' By default this function provides the Java reference in the current environment. If
#' there is no Java references then the value of the function is an empty list. If
#' just.names is set to true, the value is a vector with the names of the instances. If false,
#' then the function returns a list with the instances.
#'
#' @param envir the environment to be scanned for java.object and java.list instances. By default, it is the global
#' environment
#' @return a vector with the names of the instances
#'
#' @export
getListOfJavaReferences <- function(envir = .GlobalEnv) {
  listObjectNames <- ls(envir = envir, all.names = T)
  javaReferenceNames <- unlist(lapply(listObjectNames, function(objName) {
    obj <- get(objName, envir = envir)
    if (methods::is(obj, "java.object") || methods::is(obj, "java.list")) {
      return(objName)
    } else {
      return(NULL)
    }
  }), use.names = F)
  if (is.null(javaReferenceNames) || length(javaReferenceNames) == 0) {
    return(c())
  } else {
    return(unlist(javaReferenceNames))
  }
}


#'
#' Using multithreading with J4R
#'
#' Applies the mclapply function in the context of
#' the J4R package.
#'
#' Multithreading a function requires that the Java code is
#' thread safe. The server must listen to at least two ports.
#' Otherwise, this function will reduce to a single thread.
#' Each port is given an affinity to a thread, so that each Java thread
#' matches an R thread. NOTE: the multithreading is not
#' available on Windows. In such a case, the function will proceed
#' in a single thread.
#'
#' @seealso mclapply in the parallel package
#'
#' @param X a vector of numerics
#' @param FUN a two-argument function. The first argument is called by
#' the mclapply function and the second argument defines the affinity and MUST
#' be used in all the calls to the createJavaObject, callJavaMethod,
#' getJavaField and setJavaField functions.
#'
#' @export
mclapply.j4r <- function(X, FUN) {
  if (!is.numeric(X)) {
    stop("The argument X should be a vector of numerics")
  }
  if (Sys.info()["sysname"] == "Windows") {
    warning("The multithreading is not available on Windows. The function will proceed in a single thread.")
    nbCores <- 1
  } else {
    nbCores <- getNbConnections()
  }
  assign("delayDumpPileFlush", TRUE, envir = settingEnv)  ### we disable the garbage collection of java.object instances here to avoid concurrent exceptions in R
  f <- function(i) {
    affinity <- (i-1)%%nbCores + 1
    FUN(i,affinity)
  }
  output <- parallel::mclapply(X, f, mc.cores = nbCores)
  assign("delayDumpPileFlush", FALSE, envir = settingEnv) ### we re enable the garbage collection of java.object instances afterwards
  return(output)
}

.getClassInfo <- function(classname, affinity = 1) {
  # message(paste("Affinity is", affinity))
  command <- paste(classInfoToken, classname, sep=MainSplitter)
  utils::write.socket(.getSocket(affinity), command)
  callback <- utils::read.socket(.getSocket(affinity), maxlen = bufferLength)
  output <- .processCallback(callback, affinity)
  return(output)
}


