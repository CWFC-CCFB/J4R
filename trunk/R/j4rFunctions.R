########################################################
# R functions for connection to Gateway Server in Java
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

# numericToken <- "numeric"
# integerToken <- "integer"
# logicalToken <- "logical"
# characterToken <- "character"
# javaObjectToken <- "JavaObject"
# javaListToken <- "JavaList"

createCommandToken <- "c"
createNullArrayToken <- "cnarr"
createNullToken <- "cnu"
createArrayToken <- "carr"

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


#'
#' Connect to Java environment
#'
#' This function connects the R environment to a gateway server that runs in Java.
#'
#' @param port the ports the server sockets listen to
#' @param extensionPath the path to jar files that can be loaded by the system classloader
#' @param memorySize the memory size of the Java Virtual Machine in Mb (if not specified, the JVM runs with the default memory size)
#' @param debug for debugging only (should be left as is)
#'
#' @return a logical TRUE if the function managed to get connected to the server or if it was already connected or
#' FALSE if the connection has failed
#'
#' @export
connectToJava <- function(port = NULL, extensionPath = NULL, memorySize = NULL, debug = FALSE) {
  if (isConnectedToJava()) {
    message("It seems R is already connected to the Java server.")
    return(TRUE)
  } else {
    if (debug) {
      if (is.null(port)) {
        stop("The port argument cannot be null in debug mode. Please use the ports you specified when you started the server!")
      }
      assign("connectionHandler", J4RConnectionHandler(port, 1000000, 50000), envir = cacheEnv)
    } else {
      message(.checkJavaVersionRequirement())
      message("Starting Java server...")
      parms <- c("-firstcall", "true")
      if (!is.null(port)) {
        parms <- c(parms, "-ports", paste(port,collapse=portSplitter))
      }
      if (!is.null(extensionPath)) {
        parms <- c(parms, "-ext", extensionPath)
      }
      if (!is.null(memorySize)) {
        if (!is.numeric(memorySize) && !is.integer(memorySize)) {
          stop("The memorySize parameter should be either a numeric or an integer!")
        }
        parms <- c(parms, "-mem", as.integer(memorySize))
      }
      parms <- c(parms, "-wd", getwd())
      filename <- file.path(getwd(), "J4RTmpFile")
      if (file.exists(filename)) {
        file.remove(filename)
      }
      if (file.exists(system.file("inst", "java", "j4r.jar", package = "J4R"))) {  ### test mode
        rootPath <- system.file("inst", "java", package = "J4R")
      } else {  ### normal mode
        rootPath <- system.file("java", package = "J4R")
      }
      #    message(rootPath)
      architecture <- suppressMessages(getJavaVersion()$architecture)
      if (architecture == "32-Bit") {
        jarFilename <- "j4r_x86.jar"
        message("Running the 32-Bit version")
      } else {
        jarFilename <- "j4r.jar"
      }
      path <- file.path(rootPath, jarFilename)
      system2(.getJavaPath(), args = c("-Xmx50m", "-jar", path, parms), wait = F)
      initialTime <- Sys.time()
      while (!file.exists(filename)) {
        Sys.sleep(0.5)
        elapsedTime <- Sys.time() - initialTime
        if (elapsedTime > 5) {
          stop("It seems the server has failed to start!")
        }
      }
      info <- suppressWarnings(utils::read.table("J4RTmpFile", header=F, sep=";", stringsAsFactors = F))
      key <- as.integer(info[1,1])
      backdoorport <- as.integer(info[1,2])
      if (is.integer(info[1,3])) { ### happens with a single port
        port <- as.integer(info[1,3])
      } else {
        port <- as.integer(strsplit(info[1,3], split = portSplitter)[[1]])
      }
      assign("connectionHandler", J4RConnectionHandler(port, key, backdoorport), envir = cacheEnv)
    }
    isSecure <- .createAndSecureConnection()

    if (!isSecure) {
      shutdownJava()  ### for a clean exit
    }
    return(isSecure)
  }
}

.getSocket <- function(thread = 1) {
  return(get("connectionHandler", envir = cacheEnv)$connections[[thread]])
}

.translateJavaObject <- function(javaObject) {
  if (methods::is(javaObject, "java.list")) {
    hashcode <- sapply(javaObject, function(obj) {
      as.character(obj$hashcode)
    })
  } else if (methods::is(javaObject, "java.object")) {
    hashcode <- as.character(javaObject$hashcode)
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
#' Checks if the Java server is running
#'
#' This is done by checking f the socket connection to the JVM exists.
#'
#' @return a logical
#'
#' @export
isConnectedToJava <- function() {
  if (exists("connectionHandler", envir = cacheEnv)) {
    stillConnected <- .isThereAtLeastOneConnection(get("connectionHandler", envir = cacheEnv))
    return(stillConnected)
  } else {
    return(FALSE)
  }
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
createJavaObject <- function(class, ..., isNullObject = FALSE, isArray = FALSE, thread = 1) {
  initialTime <- Sys.time()
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
  message(paste("Preparing basic command took", Sys.time() - initialTime))
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
#    initialTime <- Sys.time()
    utils::write.socket(.getSocket(thread), command)
#    message(paste("Sending info through socket took", Sys.time() - initialTime))
#    initialTime <- Sys.time()
    callback <- utils::read.socket(.getSocket(thread), maxlen = bufferLength)
#    message(paste("Waiting and reading info from socket took", Sys.time() - initialTime))
    output <- .processResult(callback, output)
  }
  return(output)
}

.getAvailableNbThreads <- function() {
  if (exists("connectionHandler", envir = cacheEnv)) {
    return(length(get("connectionHandler", envir = cacheEnv)$connections))
  } else {
    return(0)
  }
}

.checkForExceptionInCallback <- function(callback) {
  if (startsWith(callback, ExceptionPrefix)) {
    stop(substring(callback, nchar(ExceptionPrefix) + 2))
  }
}

.dropAllIntoFirstList <- function(javaList, javaSomething) {
  initialLength <- length(javaList)
  if (methods::is(javaSomething, "java.object")) {
    javaList[[initialLength + 1]] <- javaSomething
  } else { ### dealing with a list of java object
    lengthIncomingList <- length(javaSomething)
    javaList[(initialLength + 1):(initialLength + lengthIncomingList)] <- javaSomething
  }
  return(javaList)
}

.marshallCommand <- function(list, lowerBoundIndex, upperBoundIndex) {
  command <- NULL
  for (i in 1:length(list)) {
    parm <- list[[i]]
    l <- length(parm)
    if (l > 1) {
      if (upperBoundIndex > l) {
        stop("The upperBoundIndex paramerer is larger than the size of the parameter!")
      }
      if (methods::is(parm, "java.list")) {
        parm <- .getSubsetOfJavaArrayList(parm, lowerBoundIndex, upperBoundIndex)
      } else {
        parm <- parm[lowerBoundIndex:upperBoundIndex]
      }
    }
    classes <- class(parm)
    class <- (classes[length(classes)])
    if (methods::is(parm, "java.object") || methods::is(parm, "java.list")) {
      class <- "java.object"
      parm <- .translateJavaObject(parm)
    } else if (methods::is(parm, "factor")) {
      class <- "character"
      parm <- as.character(parm)
    }
    subCommand <- paste(class, paste(parm,collapse=SubSplitter), sep="")
    if (is.null(command)) {
      command <- subCommand
    } else {
      command <- paste(command, subCommand, sep=MainSplitter)
    }
  }
  return(command)
}

.constructSourcePartCommand <- function(prefix, source, sourceLength, targetName, lowerIndex, upperIndex) {
  if (methods::is(source, "java.object")) {   ### non-static method
    command <- paste(prefix, paste("java.object", .translateJavaObject(source), sep=""), targetName, sep=MainSplitter)
  } else if (methods::is(source, "java.list")) {   ### non-static method
    subList <- .getSubsetOfJavaArrayList(source, lowerIndex, upperIndex)
    command <- paste(prefix, paste("java.object", .translateJavaObject(subList), sep=""), targetName, sep=MainSplitter)
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
#'
#' @export
getJavaField <- function(source, fieldName, thread = 1) {
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

    utils::write.socket(.getSocket(thread), command)
    callback <- utils::read.socket(.getSocket(thread), maxlen=bufferLength)
    output <- .processResult(callback, output)
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
#'
#' @export
setJavaField <- function(source, fieldName, value, thread = 1) {
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
    utils::write.socket(.getSocket(thread), command)
    callback <- utils::read.socket(.getSocket(thread), maxlen=bufferLength)
    output <- .processResult(callback, output)
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
callJavaMethod <- function(source, methodName, ..., thread = 1) {
  parameters <- list(...)
  parametersLength <- .getParametersLength(parameters)
  maxLength <- parametersLength
  sourceLength <- .getSourceLength(source, parametersLength)

  if (sourceLength > maxLength) {
    maxLength <- sourceLength ### then the source drives the nb of calls
  }

  nbCalls <- ceiling(maxLength / maxVectorLength)

  # output <- NULL
  # for (i in 1:nbCalls) {
  #   lowerIndex <- (i-1) * maxVectorLength + 1
  #   upperIndex <- i * maxVectorLength
  #   if (upperIndex > maxLength) {
  #     upperIndex <- maxLength
  #   }
  #
  #   command <- .constructSourcePartCommand("method", source, sourceLength, methodName, lowerIndex, upperIndex)
  #
  #   if (length(parameters) > 0) {
  #     if (maxLength == 1) {
  #       command <- paste(command, .marshallCommand(parameters, 1, 1), sep=MainSplitter)
  #     } else {
  #       command <- paste(command, .marshallCommand(parameters, lowerIndex, upperIndex), sep=MainSplitter)
  #     }
  #   }
  #   utils::write.socket(.getSocket(thread), command)
  #   callback <- utils::read.socket(.getSocket(thread), maxlen=bufferLength)
  #   output <- .processResult(callback, output)
  # }
  output <- NULL
  callID <- 0
  nbAvailableThreads <- .getAvailableNbThreads()
  while (callID < nbCalls) {
    threadId <- 0
    while (threadId < nbAvailableThreads && callID < nbCalls) {
      callID <- callID + 1
      threadId <- threadId + 1
      lowerIndex <- (callID-1) * maxVectorLength + 1
      upperIndex <- callID * maxVectorLength
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
      utils::write.socket(.getSocket(threadId), command)
    }
    for (threadId2 in 1:threadId) {
      callback <- utils::read.socket(.getSocket(threadId2), maxlen=bufferLength)
      output <- .processResult(callback, output)
    }
  }
  if (is.null(output)) {
    return(invisible(output))
  } else {
    return(output)
  }
  return(output)
}

.processResult <- function(callback, output) {
#  initialTime <- Sys.time()
  result <- .processCallback(callback)
#  message(paste("Processing callback took", Sys.time() - initialTime))
  if (is.null(output)) {
    output <- result
  } else {
    if (methods::is(output, "java.list")) {
      output <- .dropAllIntoFirstList(output, result)
    } else {
      output <- c(output, result)
    }
  }
#  message(paste("Processing result took", Sys.time() - initialTime))
  return(output)
}


.processCallback <- function(callback) {
  .checkForExceptionInCallback(callback)
  if (regexpr(javaObjectToken, callback) >= 0) {  ## a single Java object
    initialTime <- Sys.time()
    returnObject <- .createJavaObjectReference(callback)
    message(paste("Creating java reference took", Sys.time() - initialTime))
  } else if (regexpr(javaListToken, callback) >= 0 && regexpr("@", callback) >= 0) { ## a list of Java objects
    initialTime <- Sys.time()
    returnObject <- .createJavaObjectReference(callback)
    message(paste("Creating list of java reference took", Sys.time() - initialTime))
  } else if (regexpr("Done", callback) >= 0) {
    returnObject <- NULL
  } else {
    initialTime <- Sys.time()
    returnObject <- .translatePrimitiveType(callback)
    message(paste("Translating to primitive type took", Sys.time() - initialTime))
  }
  return(returnObject)
}

.translatePrimitiveType <- function(str) {
  if (regexpr(javaListAndMainSplitterToken, str) == 1) {
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


.getSubsetOfJavaArrayList <- function(javaArrayList, start, end) {
  newList <- javaArrayList[start:end]
  class(newList) <- append(class(newList), "java.list")
  return(newList)
}


.createJavaObjectReference <- function(str) {
  initialTime <- Sys.time()
  inputList <- strsplit(str,MainSplitter)
  innerList <- strsplit(inputList[[1]][2], SubSplitter)
  elapsedTime <- Sys.time() - initialTime
  message(paste("Splitting callback took", elapsedTime))
  initialTime <- Sys.time()
  vecStr <- innerList[[1]]
  argumentList <- strsplit(vecStr,"@")
  outputList <- lapply(argumentList, function(arguments) {
        classname <- arguments[1]
        hashcodeInt <- as.integer(arguments[2])
        javaObject <- java.object(classname, hashcodeInt)
  })
  elapsedTime <- Sys.time() - initialTime
  message(paste("Creating the instances took", elapsedTime))
  if (length(outputList) == 1) {
    return (outputList[[1]])
  } else {
    return(as.java.list(outputList))
  }
}

#'
#' Shut down Java
#'
#' This function shuts down Java and the gateway server.
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
shutdownJava <- function() {
  .killJava()
  listJavaReferences <- getListOfJavaReferences()
  if (!is.null(listJavaReferences) & length(listJavaReferences) > 0) {
    message("Your global environment now contains some useless Java references.")
    message("To delete them, you can use the following line of code:")
    message("rm(list = getListOfJavaReferences())")
  }
}

.internalShutdown <- function() {
  if (isConnectedToJava()) {
    utils::write.socket(.getSocket(), "closeConnection")
    message("Closing connection and removing socket...")
  }
  if (exists("connectionHandler", envir = cacheEnv)) {  # when security is not validated, the connectionhandler object remains
    rm("connectionHandler", envir = cacheEnv)
  }
  filename <- file.path(getwd(), "J4RTmpFile")
  if (file.exists(filename)) {
    file.remove(filename)
  }
  #### Remove because CRAN policy is to leave the global environment untouched
  # nbObjectRemoved <- 0
  # for (objectName in ls(envir = globalenv())) {
  #   # if ("java.object" %in% class(object)) {
  #   if (.getClass(get(objectName, envir = globalenv())) %in% c("java.object", "java.list")) {
  #     nbObjectRemoved <- nbObjectRemoved + 1
  #     if (nbObjectRemoved == 1) {
  #       message("Removing Java objects from global environment...")
  #     }
  #     rm(list = objectName, envir = globalenv())
  #   }
  # }
}

#'
#' Provide a list of the Java references
#'
#' The function provides the list of the Java references in an environment environment.
#'
#' By default this function provides the Java reference in the global environment.
#'
#' @param envir the environment for which the list of Java references is needed
#' @return a vector with the names of the objects that belong to the java.object and java.list classes.
#'
#' @export
getListOfJavaReferences <- function(envir = globalenv()) {
  output <- c()
  for (objectName in ls(envir = envir)) {
    obj <- get(objectName, envir = envir)
    if (methods::is(obj, "java.object") || methods::is(obj, "java.list")) {
      output <- c(output, objectName)
    }
  }
  return(output)
}




#'
#' Synchronize the Java environment with the R environment
#'
#' This function synchronizes the Java environment with the R environment. Objects that
#' are removed from the R environment are not automatically removed from the Java
#' environment. This function scans the R environment for the java.object instance and
#' commands the gateway server to get rid of the Java instances that are not longer referred
#' to in the R environment.
#'
#' To avoid a memory leak, the function should be called on a regular basis.
#'
#' @param ... a list of environment instances if the method is called within a function
#' @return An integer which is the number of Java objects still registered in the Java environment
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
callJavaGC <- function(...) {
  environments <- list(...)
  command <- "sync"
  for (objectName in ls(envir = globalenv())) {
    object <- get(objectName, envir = globalenv())
    if (methods::is(object, "java.object") || methods::is(object, "java.list")) {
      command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=MainSplitter)
    }
  }
  if (length(environments) > 0) {
    for (environment in environments) {
      if (class(environment) == "environment") {
        if (!identical(environment, globalenv())) {
          for (objectName in ls(envir = environment)) {
            object <- get(objectName, envir = environment)
            if (methods::is(object, "java.object") || methods::is(object, "java.list")) {
              command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=MainSplitter)
            }
          }
        }
      }
    }
  }
  utils::write.socket(.getSocket(), command)
  callback <- utils::read.socket(.getSocket(), maxlen=bufferLength)
  return(.processCallback(callback))
}

#'
#' Retrieve the paths of the current classloader
#'
#' This functions returns the paths that are currently included
#' in the System classloader.
#'
#' @export
getClassLoaderPaths <- function() {
  paths <- callJavaMethod("j4r.lang.J4RSystem", "getClassPathURLs")
  pathsList <- getAllValuesFromListObject(paths)
  return(pathsList)
}

#'
#' Retrieve the URLs of the current classloader
#'
#' This function returns the URLs that are currently included
#' in the System classloader.
#'
#' This function is deprecated. Please use the getClassLoaderPaths instead.
#'
#' @export
getClassLoaderURLs <- function() {
  .Deprecated("getClassLoaderPaths")
  return(getClassLoaderPaths())
}


#'
#' Check if a Library has been loaded
#'
#' It checks if a particular library is part of the classpath.
#'
#' @param myJavaLibrary a character string that stands for the java library (e.g. repicea.jar)
#'
#' @export
checkIfClasspathContains <- function(myJavaLibrary) {
  if (isConnectedToJava()) {
    listURLs <- getClassLoaderPaths()
    isLibIn <- FALSE
    if (length(listURLs) > 1) {
      for (i in 1:length(listURLs)) {
        if (grepl(myJavaLibrary, listURLs[i])) {
          isLibIn <- TRUE
          break
        }
      }
    } else {
      isLibIn <- grepl(myJavaLibrary, listURLs)
    }
    return(isLibIn)
  } else {
    message("The Java server is not running.")
  }
}

.getLibraryPath <- function(packageName, myJavaLibrary) {
  filename <- system.file("inst", myJavaLibrary, package = packageName)
  if (file.exists(filename)) {  ### test mode
    filePath <- filename
  } else {
    filename <- system.file(myJavaLibrary, package = packageName)
    if (file.exists(filename)) {  ### normal mode
      filePath <- filename
    } else {
      filePath <- NULL
    }
  }
  return(filePath)
}

.killJava <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::read.socket(emergencySocket, maxlen = bufferLength)
      utils::write.socket(socket = emergencySocket, "emergencyShutdown")
    },
    error=function(cond) {
      message("Unable to contact the server. It might be already down!")
    }
  )
  .internalShutdown()
  Sys.sleep(2)  ### wait two seconds to make sure the server is really shut down
  message("Done.")
}


#'
#' Dynamically adds a path or a jar file to the classpath.
#'
#' This function makes it possible to add a directory or a JAR file
#' to the class path. If the packageName parameter is null then the urlString
#' parameter must be the complete path to the directory. Otherwise, it can be
#' the name of the JAR file and the function will find the path through the package
#' name. A non null packageName parameter is typically used in packages that rely
#' on J4R.
#'
#' @param path a character representing the path to the directory or the JAR file
#' if the packageName parameter is null. Otherwise, it can just be the name of the JAR file. This path
#' is normalized so that expressions like myJar.jar or ./extensions/myJar.jar will be processed.
#' @param packageName a character representing the package.
#'
#' @export
addToClassPath <- function(path, packageName = NULL) {
  if (isConnectedToJava()) {
    if (!is.null(packageName)) {
      path <- .getLibraryPath(packageName, path)
    }
    callJavaMethod("j4r.lang.J4RSystem", "addToClassPath", normalizePath(path))
  } else {
    message("The Java server is not running.")
  }
}


#'
#' Dynamically adds an url to the classpath.
#'
#' This function makes it possible to add a directory or a JAR file
#' to the class path. If the packageName parameter is null then the urlString
#' parameter must be the complete path to the directory. Otherwise, it can be
#' the name of the JAR file and the function will find the path through the package
#' name. A non null packageName parameter is typically used in packages that rely
#' on J4R.
#'
#' This function is deprecated. Use the addToClassPath function instead.
#'
#' @param urlString a character representing the complete path to the directory or the JAR file
#' if the packageName parameter is null. Otherwise, it can just be the name of the JAR file.
#' @param packageName a character representing the package.
#'
#' @export
addUrlToClassPath <- function(urlString, packageName = NULL) {
  .Deprecated("addToClassPath")
  addToClassPath(urlString, packageName)
}


