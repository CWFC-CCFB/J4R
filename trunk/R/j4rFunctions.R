########################################################
# R function for connection to Gateway Server in Java
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

#'
#' The cache environment of this package
#'
#' This environment contains the objects that enable the connection to
#' the gateway server.
#'
#'@export
cacheEnv <- new.env()

#'
#' Length of the buffer when reading from the socket connection.
#'
#' The buffer has a length of 100Kb by default.
#'
#' @export
bufferLength <- 100000

MainSplitter <- "/;"
SubSplitter <- "/,"

#'
#' Maximum length of the vector in the parameters.
#'
#' A maximum length of the vector is set in order to avoid buffer size issues when reading
#'
#' @export
maxVectorLength <- 200

#'
#' Connect to Java environment
#'
#' This function connects the R environment to a gateway server that runs in Java.
#' The extension path must be set before calling this function. See setJavaExtensionPath.
#'
#' @param port the local port (the port is set to 18011 by default)
#' @param extensionPath the path to jar files that can be loaded by the system classloader
#' @param memorySize the memory size of the Java Virtual Machine in Mb (if not specified, the JVM runs with the default memory size)
#' @param debug for debugging only (should be left as is)
#'
#' @return nothing
#'
#' @export
connectToJava <- function(port = 18011, extensionPath = NULL, memorySize = NULL, debug = FALSE) {
  if (exists("j4rSocket", envir = cacheEnv)) {
    message("The object j4rSocket already exists! It seems R is already connected to the Java server.")
  } else {
    if (!debug) {
      message("Starting Java server...")
      parms <- c("-firstcall", "true")
      if (port != 18011) {
        parms <- c(parms, "-port", port)
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
      if (file.exists(paste(find.package("J4R"),"inst/java/repicea.jar", sep="/"))) {  ### test mode
        rootPath <- paste(find.package("J4R"),"inst", "java", sep="/")
      } else {  ### normal mode
        rootPath <- paste(find.package("J4R"), "java", sep="/")
      }
      #    message(rootPath)
      path <- paste(rootPath,"repicea.jar",sep="/")
      completeCommand <- paste("java -jar", path, paste(parms, collapse=" "), sep = " ")
      system(completeCommand, wait=FALSE)
      Sys.sleep(2)
    }
  }
  message(paste("Connecting on port", port))
  assign("j4rSocket", utils::make.socket("localhost", port), envir = cacheEnv)
  utils::read.socket(.getMainSocket(), maxlen = bufferLength)
}


.getMainSocket <- function() {
  return(get("j4rSocket", envir = cacheEnv))
}

.getClass <- function(obj) {
  vector <- class(obj)
  return(vector[length(vector)])
}

.translateJavaObject <- function(javaObject) {
  hashcode <- c()
  clazz <- .getClass(javaObject)
  if (clazz == "java.arraylist") {
    for (i in 1:length(javaObject)) {
      hashcode <- c(hashcode, as.character(javaObject[[i]]$hashcode))
    }
  } else if (clazz == "java.object") {
    hashcode <- as.character(javaObject$hashcode)
  } else {
    stop(".translateJavaObject: the argument should be an instance of java.object or java.arraylist")
  }
  str <- paste("hashcode",paste(hashcode, collapse=SubSplitter), sep="")
  return(str)
}

#'
#' Create Java objects
#'
#' This function creates one or many object of a particular class. If the parameters
#' contain vectors, then a series of instances of this class can be created.
#'
#' @param class the Java class of the object (e.g. java.util.ArrayList)
#' @param ... the parameters to be passed to the constructor of the object
#' @param isNullObject a logical that indicates whether the instance should be null (by default it is set to FALSE)
#' @param isArray a logical that indicates whether the instance is an array. By default, it is set to FALSE. When creating an array, the parameters must be integers that define the dimensions of the array
#' @return a java.object or java.list instance in the R environment
#' @examples
#' ### starting Java
#' connectToJava()
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
createJavaObject <- function(class, ..., isNullObject = FALSE, isArray = FALSE) {
  parameters <- list(...)
  parametersLength <- .getParametersLength(parameters)
  firstCommand <- "create"
  if (isNullObject) {
    firstCommand <- "createnull"
  } else if (isArray) {
    firstCommand <- "createarray"
  }

  nbCalls <- ceiling(parametersLength / maxVectorLength)
  if (nbCalls == 0) { ## to make sure it goes through the loop at least once
    nbCalls <- 1
  }
  basicCommand <- paste(firstCommand, class, sep=MainSplitter)
  javaList <- NULL
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
    utils::write.socket(.getMainSocket(), command)
    callback <- utils::read.socket(.getMainSocket(), maxlen = bufferLength)
    if(regexpr("Exception", callback) >= 0) {
      stop(callback)
    } else {
      if (parametersLength <= 1) {
        return(.createFakeJavaObject(callback)) ## a single object
      } else {
        if (is.null(javaList)) {
          javaList <- .createFakeJavaObject(callback)
        } else {
          javaList <- .dropAllIntoFirstList(javaList, .createFakeJavaObject(callback))
        }
      }
    }
  }
  return(javaList)
}

.dropAllIntoFirstList <- function(javaList, javaSomething) {
  initialLength <- length(javaList)
  if (.getClass(javaSomething) == "java.object") {
    javaList[[initialLength + 1]] <- javaSomething
  } else { ### dealing with a list of java object
    lengthIncomingList <- length(javaSomething)
    for (i in 1:lengthIncomingList) {
      javaList[[initialLength + i]] <- javaSomething[[i]]
    }
  }
  return(javaList)
}

.marshallCommand <- function(list, lowerBoundIndex, upperBoundIndex) {
  command <- NULL
  for (i in 1:length(list)) {
    parm <- list[[i]]
    l <- length(parm)
    if (.getClass(parm) == "java.object") { ## it should not be set to 2
      l <- 1
    }
    if (l > 1) {
      if (upperBoundIndex > l) {
        stop("The upperBoundIndex paramerer is larger than the size of the parameter!")
      }
      parm <- parm[lowerBoundIndex:upperBoundIndex]
    }
    class <- .getClass(parm)
    if (class == "java.object" || class == "java.arraylist") {
      class <- "java.object"
      parm <- .translateJavaObject(parm)
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


#'
#' Call a Java method
#'
#' This method calls a public method in a particular class of object. If the javaObject parameters or the additional
#' parameters (...) include vectors, the method is called several times and a vector of primitive or a list of java
#' instances can be returned.
#'
#' There is no need to cast a particular parameter to a super class. Actually, the Java server tries to find the method
#' that best matches the types of the parameters
#'
#' @param source this should be either a java.arraylist instance or a single java.object instance for non-static methods or
#' a string representing the Java class name in case of static method
#' @param methodName the name of the method
#' @param ... the parameters of the method
#' @return It depends on the method. It can return a primitive type (or a vector of primitive), a Java instance (or a list of Java instances) or nothing at all.
#' @examples
#' ### starting Java
#' connectToJava()
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
callJavaMethod <- function(source, methodName, ...) {
  parameters <- list(...)
  parametersLength <- .getParametersLength(parameters)
  maxLength <- parametersLength
  if (.getClass(source) == "java.arraylist") {   ### check if the size of the source is compatible with the parameters length
    if (length(source) != parametersLength && parametersLength > 1) {
      stop("The length of the list of java object the method is called upon is inconsistent with the length of the parameters!")
    } else {
      sourceLength <- length(source)
    }
  } else { ## either a java.object or a string that represents a class for static methods
    sourceLength <- 1
  }

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

    if (.getClass(source) %in% c("java.object")) {   ### non-static method
      command <- paste("method", paste("java.object", .translateJavaObject(source), sep=""), methodName, sep=MainSplitter)
    } else if (.getClass(source) %in% c("java.arraylist")) {   ### non-static method
      subList <- .getSubsetOfJavaArrayList(source, lowerIndex, upperIndex)
      command <- paste("method", paste("java.object", .translateJavaObject(subList), sep=""), methodName, sep=MainSplitter)
    } else {  ### static method
      command <- paste("method", paste("java.class", source, sep=""), methodName, sep=MainSplitter)
    }
    if (length(parameters) > 0) {
      if (maxLength == 1) {
        command <- paste(command, .marshallCommand(parameters, 1, 1), sep=MainSplitter)
      } else {
        command <- paste(command, .marshallCommand(parameters, lowerIndex, upperIndex), sep=MainSplitter)
      }
    }
    utils::write.socket(.getMainSocket(), command)
    callback <- utils::read.socket(.getMainSocket(), maxlen=bufferLength)
    result <- .processCallback(callback)
    if (maxLength == 1) {
      return(result)
    } else {
      if (is.null(output)) {
        output <- result
      } else {
        if (.getClass(output) == "java.arraylist") {
          output <- .dropAllIntoFirstList(output, result)
        } else {
          output <- c(output, result)
        }
      }
    }
  }
  return(output)
}

.getParametersLength <- function(parameters) {
  maxLength <- 0
  if (length(parameters) > 0) {
    for (i in 1:length(parameters)) {
      thisParameterLength <- length(parameters[[i]])
      if (thisParameterLength >= maxLength) {
        maxLength <- length(parameters[[i]])
#        stop(paste("The J4R package allows for vectors than do not exceed", maxVectorLength, "in length. You can use a loop instead.", sep=" "))
      } else if (thisParameterLength > 1) {
          stop("The parameters are not consistent! Those with sizes greater than 1 should all have the same size!")
      }
    }
  }
  return(maxLength)
}

.processCallback <- function(callback) {
#  print(paste("Processing this callback : ", callback, sep=""))
  if(regexpr("Exception", callback) >= 0) {
    stop(callback)
  } else if(regexpr("Error", callback) >= 0) {
    stop(callback)
  } else if (regexpr("JavaObject", callback) >= 0) {  ## a single Java object
    returnObject <- .createFakeJavaObject(callback)
  } else if (regexpr("JavaList", callback) >= 0 && regexpr("@", callback) >= 0) { ## a list of Java objects
    returnObject <- .createFakeJavaObject(callback)
  } else if (regexpr("RequestReceivedAndProcessed", callback) >= 0) {
    returnObject <- NULL
  } else {
    returnObject <- .translatePrimitiveType(callback)
  }
  return(returnObject)
}

.translatePrimitiveType <- function(str) {
  if (regexpr(paste("JavaList", MainSplitter, sep=""), str) == 1) {
    str <- substring(str, 11)
  }
  inputList <- strsplit(str,SubSplitter)[[1]]
  outputVector <- list()
  for (i in 1:length(inputList)) {
    str <- inputList[i]
    if (regexpr("numeric", str) == 1) { # starts with numeric
      outputVector[[i]] <- as.numeric(substring(str,8))
    } else if (regexpr("integer", str) == 1) { # starts with integer
      value <- as.double(substring(str,8))  ### to avoid coercion
      if (abs(value) < 2*10^9) {
        value <- as.integer(value)
      }
      outputVector[[i]] <- value
    } else if (regexpr("logical", str) == 1) { # starts with logical
      outputVector[[i]] <- as.logical(substring(str,8))
    } else if (regexpr("character", str) == 1) { # starts with character
      outputVector[[i]] <- as.character(substring(str, 10))
    } else {
      stop(paste("This primitive type is not recognized:", str, sep = " "))
    }
  }

  return(.convertListToVectorIfPossible(outputVector))
}


.convertListToVectorIfPossible <- function(myList) {
  if (length(myList) > 0) {
    outputVec <- c()
    for (i in 1:length(myList)) {
      if (i == 1) {
        refClass <- class(myList[[i]])
      } else {
        if (class(myList[[i]]) != refClass) {
          return(myList)
        }
      }
      outputVec <- c(outputVec, myList[[i]])
    }
    return(outputVec)
  } else {
    return(myList)
  }
}

.createJavaList <- function() {
  outputList <- list()
  class(outputList) <- c(class(outputList), "java.arraylist")
  return(outputList)
}

.getSubsetOfJavaArrayList <- function(javaArrayList, start, end) {
  newList <- javaArrayList[start:end]
  class(newList) <- c(class(newList), "java.arraylist")
  return(newList)
}


.createFakeJavaObject <- function(str) {
  inputList <- strsplit(str,MainSplitter)
  innerList <- strsplit(inputList[[1]][2], SubSplitter)
  outputList <- .createJavaList()
#  class(outputList) <- c(class(outputList), "java.arraylist")
  for (i in 1:length(innerList[[1]])) {
    javaObject <- list()
    class(javaObject) <- c(class(javaObject), "java.object")
    arguments <- strsplit(innerList[[1]][i],"@")
    javaObject$class <- arguments[[1]][1]
    javaObject$hashcode <- as.integer(arguments[[1]][2])
    outputList[[i]] <- javaObject
  }
  if (length(outputList) == 1) {
    return (outputList[[1]])
  } else {
    return(outputList)
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
  .internalShutdown()
  Sys.sleep(2)  ### wait two seconds to make sure the server is really shut down
  message("Done.")
}

.internalShutdown <- function() {
  if (exists("j4rSocket", envir = cacheEnv)) {
    utils::write.socket(.getMainSocket(), "closeConnection")
    message("Closing connection and removing socket...")
    rm("j4rSocket", envir = cacheEnv)
  }
  nbObjectRemoved <- 0
  for (objectName in ls(envir = globalenv())) {
    # if ("java.object" %in% class(object)) {
    if (.getClass(get(objectName, envir = globalenv())) %in% c("java.object", "java.arraylist")) {
      nbObjectRemoved <- nbObjectRemoved + 1
      if (nbObjectRemoved == 1) {
        message("Removing Java objects from global environment...")
      }
      rm(list = objectName, envir = globalenv())
    }
  }
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
    if (.getClass(object) %in% c("java.object", "java.arraylist")) {
      command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=MainSplitter)
    }
  }
  if (length(environments) > 0) {
    for (environment in environments) {
      if (class(environment) == "environment") {
        if (!identical(environment, globalenv())) {
          for (objectName in ls(envir = environment)) {
            object <- get(objectName, envir = environment)
            if (.getClass(object) %in% c("java.object", "java.arraylist")) {
              command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=MainSplitter)
            }
          }
        }
      }
    }
  }
  utils::write.socket(.getMainSocket(), command)
  callback <- utils::read.socket(.getMainSocket(), maxlen=bufferLength)
  return(.processCallback(callback))
}

#'
#' Retrieve the URLs of the current classloader
#'
#' This functions returns the URLs that are currently included
#' in the System classloader.
#'
#' @export
getClassLoaderURLs <- function() {
  classLoader <- callJavaMethod("java.lang.ClassLoader", "getSystemClassLoader")
  urls <- callJavaMethod(classLoader, "getURLs")
  urlsList <- getAllValuesFromArray(urls)
  return(callJavaMethod(urlsList, "toString"))
}


#'
#' Returns the maximum, total and free memory in Mb
#'
#' This function calls the Runtime static methods maxMemory(),
#' totalMemory() and freeMemory(). The results are divided by
#' 1024 in order to report the memory sizes in Mb.
#' @return a data.frame object with the maximum, total and free memory in Mb.
#'
#' @export
getMemorySettings <- function() {
  runtime <- callJavaMethod("java.lang.Runtime", "getRuntime")
  maxMemory <- callJavaMethod(runtime, "maxMemory") / 1024^2
  totalMemory <- callJavaMethod(runtime, "totalMemory")  / 1024^2
  freeMemory <- callJavaMethod(runtime, "freeMemory")  / 1024^2
  return(data.frame(maxMemory, totalMemory, freeMemory))
}

.onUnload <- function(libpath) {
  .internalShutdown()
}

.onDetach <- function(libpath) {
  .internalShutdown()
}

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to J4R!")
  packageStartupMessage("Please, make sure that Java (version 8 or greater) is part of the path.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/repiceasource/wiki/J4R/ .")
}


.onAttachLoad <- function(libname, pkgname) {
  .welcomeMessage()
}

#'
#' Check if a Library has been loaded
#'
#' It checks if a particular library is part of the classpath.
#'
#' @param myJavaLibrary a character string that stands for the java library (e.g. repicea.jar)
#' @param packageName a character string that stands for the package if the function is to be used
#' by a package (it is null by default)
#' @param automaticRestart a logical (FALSE by default) that enables an automatic restart of the Java
#' server so that the library is included in the classpath
#'
#' @export
checkIfExtensionsContain <- function(myJavaLibrary, packageName = NULL, automaticRestart = FALSE) {
  if (exists("j4rSocket", envir = cacheEnv)) {
    listURLs <- getClassLoaderURLs()
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
    if (!isLibIn && automaticRestart) {
      message(paste("The", myJavaLibrary, "is not in the extension path! The Java server will shut down and start over again!", sep=" "))
      shutdownJava()
      if (file.exists(paste(find.package(packageName), "inst", myJavaLibrary, sep="/"))) {  ### test mode
        rootPath <- paste(find.package(packageName), "inst", sep="/")
      } else {  ### normal mode
        rootPath <- find.package(packageName)
      }
      connectToJava(extensionPath = rootPath)
    } else {
      return(isLibIn)
    }
  } else {
    message("The Java server is not running.")
    if (automaticRestart) {
      message("The Java server will restart with proper extensions.")
      if (file.exists(paste(find.package(packageName), "inst", myJavaLibrary, sep="/"))) {  ### test mode
        rootPath <- paste(find.package(packageName),"inst", sep="/")
      } else {  ### normal mode
        rootPath <- find.package(packageName)
      }
      connectToJava(extensionPath = rootPath)
    }
  }
}




