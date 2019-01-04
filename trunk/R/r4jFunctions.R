########################################################
# R function for connection to Gateway Server in Java
########################################################

######### TODO create documentation
######### Clean memory through gc function
#' Connect to the rgateway server that runs in Java.
#' @param port the local port (by default the port is set to 18011)
#' @param local
#'
r4j.connect <- function(port = 18011, local = T) {
  if (local && (!exists("handle", envir = globalenv()) || process_state(handle) != "running")) {
    require(subprocess)
    print("Starting Java Virtual Machine...")
    parms <- c("-firstcall", "true")
    if (exists(".extensionPath")) {
     parms <- c(parms, "-ext", .extensionPath)
    }
    handle <<- spawn_process("repicea.jar", parms) ### spawn the java server
    Sys.sleep(2)
  }
  print("Connecting to JVM...")
  mainSocket <<- make.socket("localhost", port)
  read.socket(mainSocket)
}

r4j.setExtensionPath <- function(path) {
  .extensionPath <<- path
}


.getMainSocket <- function() {
  return(mainSocket)
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

r4j.createObject <- function(class, ...) {
  parameters <- list(...)
  command <- paste("create", class, sep=";")
  command <- paste(command, .marshallCommand(parameters), sep=";")
  write.socket(.getMainSocket(), command)
  callback <- read.socket(.getMainSocket(), maxlen = 10000)
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

r4j.callMethod <- function(javaObject, methodName, ...) {
  parameters <- list(...)
  command <- paste("method", paste("java.object",.translateJavaObject(javaObject),sep=""), methodName, sep=";")
  command <- paste(command, .marshallCommand(parameters), sep=";")
  write.socket(.getMainSocket(), command)
  callback <- read.socket(.getMainSocket(), maxlen=10000)
  if(regexpr("Exception", callback) >= 0) {
    stop(callback)
  } else if (regexpr("JavaObject", callback) >= 0) {
    returnObject <- .createFakeJavaObject(callback)
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

r4j.shutdownJVM <- function() {
  if (exists("mainSocket", envir = globalenv())) {
    write.socket(.getMainSocket(), "closeConnection")
    print("Closing connection and removing socket...")
    rm("mainSocket", envir = globalenv())
  }
  print("Removing Java objects from global environment...")
  for (objectName in ls(envir = globalenv())) {
    object <- get(objectName)
    if ("java.object" %in% class(object)) {
      rm(list = objectName, envir = globalenv())
    }
  }
  print("Terminating Java Virtual Machine...")
  if (exists("handle", envir = globalenv())) {
    process_terminate(handle)
    rm("handle", envir = globalenv())
  }
  print("Done.")
}

r4j.gc <- function() {
  command <- NULL
  for (objectName in ls(envir = globalenv())) {
    object <- get(objectName)
    if ("java.object" %in% class(object)) {
      if (is.null(command)) {
        command <- paste("sync", paste("java.object",.translateJavaObject(object),sep=""), sep=";")
      } else {
        command <- paste(command, paste("java.object",.translateJavaObject(object),sep=""), sep=";")
      }
    }
  }
  write.socket(.getMainSocket(), command)
}

