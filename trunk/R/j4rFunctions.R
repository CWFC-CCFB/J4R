########################################################
# R function for connection to Gateway Server in Java
# Author Mathieu Fortin - January 2019
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
#' Connect to Java environment
#'
#' This function connects the R environment to a gateway server that runs in Java.
#' The extension path must be set before calling this function. See setJavaExtensionPath.
#'
#' @param port the local port (the port is set to 18011 by default)
#' @return nothing
#'
#' @export
connectToJava <- function(port = 18011) {
  local <- TRUE
  if (exists("j4rSocket", envir = cacheEnv)) {
    print("The object j4rSocket already exists! It seems R is already connected to the Java server.")
  } else {
    if (local) {
      print("Starting Java server...")
      parms <- c("-firstcall", "true")
      if (port != 18011) {
        parms <- c(parms, "-port", port)
      }
      if (exists("extensionPath", envir = cacheEnv)) {
        parms <- c(parms, "-ext", get("extensionPath", envir = cacheEnv))
      }
      if (file.exists(paste(find.package("J4R"),"inst/repicea.jar", sep="/"))) {  ### test mode
        rootPath <- paste(find.package("J4R"),"inst", sep="/")
      } else {  ### normal mode
        rootPath <- find.package("J4R")
      }
      #    print(rootPath)
      path <- paste(rootPath,"repicea.jar",sep="/")
      completeCommand <- paste("java -jar", path, paste(parms, collapse=" "), sep = " ")
      system(completeCommand, wait=FALSE)
      Sys.sleep(2)
    }
  }
  print(paste("Connecting on port", port))
  assign("j4rSocket", utils::make.socket("localhost", port), envir = cacheEnv)
  utils::read.socket(.getMainSocket())
}


#'
#' Set a path for jar extensions.
#'
#' This function sets a path for eventual extensions, i.e. jar files. These
#' extensions are loaded through a custom classloader.
#'
#' @param path the path to the jar files to be loaded by the Java classloader
#' @examples
#' setJavaExtensionPath("/home/fortin/myExternalLibraries")
#' connectToJava()
#' ## your code ##
#' shutdownJava()
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
setJavaExtensionPath <- function(path) {
  assign("extensionPath", path, envir = cacheEnv)
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
  str <- paste("hashcode",paste(hashcode, collapse=","), sep="")
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
#' ### shutting down Java
#' shutdownJava()
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
createJavaObject <- function(class, ...) {
  parameters <- list(...)
  command <- paste("create", class, sep=";")
  if (length(parameters) > 0) {
    command <- paste(command, .marshallCommand(parameters), sep=";")
  }
  utils::write.socket(.getMainSocket(), command)
  callback <- utils::read.socket(.getMainSocket(), maxlen = 10000)
  if(regexpr("Exception", callback) >= 0) {
    stop(callback)
  } else {
    javaObject <- .createFakeJavaObject(callback)
  }
  return(javaObject)
}

.marshallCommand <- function(list) {
  command <- NULL
  lref <- 1
  for (i in 1:length(list)) {
    parm <- list[[i]]
    l <- length(parm)
    if (.getClass(parm) == "java.object") {
      l <- 1
    }
    if (l > 1) {
      if (lref == 1) {
        lref = l
      } else {
        if (l != lref) {
          stop("The parameters should have the same size!")
        }
      }
    }
    class <- .getClass(parm)
    if (class == "java.object" || class == "java.arraylist") {
      class <- "java.object"
      parm <- .translateJavaObject(parm)
    }
    subCommand <- paste(class, paste(parm,collapse=","), sep="")
    if (is.null(command)) {
      command <- subCommand
    } else {
      command <- paste(command, subCommand, sep=";")
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
#' @param javaObject this should be either a list of instances or a single instance of java.object.
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
callJavaMethod <- function(javaObject, methodName, ...) {
  parameters <- list(...)
  command <- paste("method", paste("java.object",.translateJavaObject(javaObject),sep=""), methodName, sep=";")
  if (length(parameters) > 0) {
    command <- paste(command, .marshallCommand(parameters), sep=";")
  }
  utils::write.socket(.getMainSocket(), command)
  callback <- utils::read.socket(.getMainSocket(), maxlen=10000)
  return(.processCallback(callback))
}

.processCallback <- function(callback) {
  if(regexpr("Exception", callback) >= 0) {
    stop(callback)
  } else if (regexpr("JavaObject", callback) >= 0) {
    returnObject <- .createFakeJavaObject(callback)
  } else if (regexpr("RequestReceivedAndProcessed", callback) >= 0) {
    returnObject <- NULL
  } else {
    returnObject <- .translatePrimitiveType(callback)
  }
  return(returnObject)
}


.translatePrimitiveType <- function(str) {
  if (regexpr("JavaList;", str) == 1) {
    str <- substring(str, 10)
  }
  inputList <- strsplit(str,",")[[1]]
  outputVector <- c()
  for (i in 1:length(inputList)) {
    str <- inputList[i]
    if (regexpr("numeric", str) == 1) { # starts with numeric
      outputVector <- c(outputVector, as.numeric(substring(str,8)))
    } else if (regexpr("integer", str) == 1) { # starts with integer
      outputVector <- c(outputVector, as.integer(substring(str,8)))
    } else if (regexpr("logical", str) == 1) { # starts with logical
      outputVector <- c(outputVector, as.logical(substring(str,8)))
    } else if (regexpr("character", str) == 1) { # starts with character
      outputVector <- c(outputVector, as.character(substring(str, 10)))
    } else {
      stop("This primitive type is not recognized!")
    }
  }
  return(outputVector)
}

.createFakeJavaObject <- function(str) {
  inputList <- strsplit(str,";")
  innerList <- strsplit(inputList[[1]][2], ",")
  outputList <- list()
  class(outputList) <- c(class(outputList), "java.arraylist")
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
  if (exists("j4rSocket", envir = cacheEnv)) {
    utils::write.socket(.getMainSocket(), "closeConnection")
    print("Closing connection and removing socket...")
    rm("j4rSocket", envir = cacheEnv)
  }
  print("Removing Java objects from global environment...")
  for (objectName in ls(envir = globalenv())) {
    object <- get(objectName)
    if ("java.object" %in% class(object)) {
      rm(list = objectName, envir = globalenv())
    }
  }
  Sys.sleep(2)  ### wait two seconds to make sure the server is really shut down
  print("Done.")
}

#'
#' This function synchronizes the Java environment with the R environment. Objects that
#' are removed from the R environment are not automatically removed from the Java
#' environment. This function scans the R environment for the java.object instance and
#' commands the gateway server to get rid of the Java instances that are not longer referred
#' to in the R environment.
#'
#' To avoid a memory leak, the function should be called on a regular basis.
#'
#' @param currentEnv the current environment if the method is called within a function
#' @return An integer which is the number of Java objects still registered in the Java environment
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
callJavaGC <- function(currentEnv = NULL) {
  command <- "sync"
  for (objectName in ls(envir = globalenv())) {
    object <- get(objectName, envir = globalenv())
    if (.getClass(object) %in% c("java.object", "java.arraylist")) {
      command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=";")
    }
  }
  if (!is.null(currentEnv) && !identical(currentEnv, globalenv())) {
    for (objectName in ls(envir = currentEnv)) {
      object <- get(objectName, envir = currentEnv)
      if (.getClass(object) %in% c("java.object", "java.arraylist")) {
        command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=";")
      }
    }
  }
  utils::write.socket(.getMainSocket(), command)
  callback <- utils::read.socket(.getMainSocket(), maxlen=10000)
  return(.processCallback(callback))
}

