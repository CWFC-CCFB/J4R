########################################################
# Class definitions
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2019
########################################################



#
# Constructor of the java.pile class.
#
# The java.pile class is similar to the java.list class,
# except that it disables methods and members automated assignment.
#
new_java.pile <- function(myList) {
  if (missing(myList) || is.null(myList)) {
    myList <- list()
  }
  if (!is.list(myList)) {
    stop("The myList argument must be a list instance!")
  }
  me <- new.env(parent = emptyenv())
  me$.innerList <- myList
  class(me) <- c("java.pile", "java.list")
  return(me)
}

'[.java.pile' <- function(x,y) {
  return(new_java.pile(x$.innerList[y]))
}


#
# Constructor of the java.list class.
#
new_java.list <- function(myList) {
  if (missing(myList) || is.null(myList)) {
    myList <- list()
  }
  if (!is.list(myList)) {
    stop("The myList argument must be a list instance!")
  }
  me <- new.env(parent = emptyenv())
  me$.innerList <- myList
  class(me) <- c("java.list")
  if (length(me$.innerList) > 0) {
    .setFunctionsForThisJavaReference(me, me$.innerList[[1]]$.class) ### no need for affinity here since the object have already called the .getClassInfo function
  }
  return(me)
}

.addToInnerList <- function(javaList, obj) {
  if (!methods::is(obj, "java.list") && !methods::is(obj, "java.object")) {
    stop("This object must be a java.list or a java.object instance")
  }
  innerList <- get(".innerList", envir = javaList)
  initialLength <- length(innerList)
  if (methods::is(obj, "java.object")) {
    innerList[[initialLength + 1]] <- obj
  } else { ### dealing with a list of java object
    lengthIncomingList <- length(obj)
    innerList[(initialLength + 1):(initialLength + lengthIncomingList)] <- get(".innerList", envir = obj)
  }
  assign(".innerList", innerList, envir = javaList) ### update the original list
  return(javaList)
}


#' @export
'[[.java.list' <- function(x,y) {
  if (is.numeric(y)) {
    return(x$.innerList[[y]])
  } else if (is.character(y)) {
    return(get(y, envir = x))
  } else {
    return(NULL)
  }
}

#' @export
'[.java.list' <- function(x,y) {
  return(new_java.list(x$.innerList[y]))
}

#' @export
'$<-.java.list' <- function(x, y, value) {
  if (!exists(y, envir = x)) {
    stop("The variable does not exist within the java.object instance and it cannot be assigned!")
  }
  obj <- get(y, envir = x)
  if (!is.function(obj) & y != ".innerList") {
    setJavaField(x, y, value)
    NextMethod()  ### to synchronize the reference with the true object
  } else {
    stop(paste("The variable or function", y, "cannot be redefined!"))
  }
}

#' @export
'$.java.list' <- function(x, y) {
  returnValue <- NextMethod()
  if (!is.function(returnValue) & y != ".innerList") {
    returnValue <- getJavaField(x, y)
    assign(y, returnValue, envir = x)
  }
  return(returnValue)
}


#'
#' Override the default length function
#'
#' A java.list class is an environment containing an inner list. The
#' length of this inner list is returned by this function.
#'
#' @param x a java.list instance
#' @return the length of the inner list
#'
#' @export
length.java.list <- function(x) {
  return(length(x$.innerList))
}

#'
#' Print a java.list object
#'
#' The java.object instances that are included in
#' this list are displayed up to a maximum number.
#'
#' @param x a java.list instance
#' @param ... additional parameters for consistent overriding
#'
#' @export
print.java.list <- function(x, ...) {
  max <- 100
  i <- 0
  while (i < length(x) && i < max) {
    i <- i + 1
    obj <- x[[i]]
    print(paste("[",i,"] ", .toString(obj), sep=""))
  }
  if (length(x) > max) {
    print(paste("... (", length(x) - max, " Java reference(s) omitted)",sep=""))
  }
}

.toString <- function(x) {
  classname <- x$.class
  if (startsWith(classname, "[")) {
    if (startsWith(classname, "[[")) {
      classname <- paste("Two-dimension array of", substring(classname,3))
    } else {
      classname <- paste("One-dimension array of", substring(classname, 2))
    }
  }
  return(paste(classname, format(x$.hashcode, scientific = F), sep="@"))
}

.flushDumpPileIfNeeded <- function(nbMaxObjects = 100) {
  dumpPile <- .getDumpPile()
  if (!.isDumpPileFlushDelayed() && length(dumpPile) >= nbMaxObjects) {
     .flush(dumpPile)
     assign("dumpPile", new_java.pile(), envir = cacheEnv)
     # print("I've just flushed the dump pile!")
  }
}

.isDumpPileFlushDelayed <- function() {
  return(get("delayDumpPileFlush", envir = settingEnv))
}


.getDumpPile <- function() {
  if (!exists("dumpPile", envir = cacheEnv)) {
    assign("dumpPile", new_java.pile(), envir = cacheEnv)
  }
  return(get("dumpPile", envir = cacheEnv))
}


.finalize <- function(javaObj) {
  if (isConnectedToJava() && .getConnectionId() == javaObj$.connectionId) {  ## the second condition ensures that a java.object from a former connection is simply discarded
    df <- .getDumpPile()
    .addToInnerList(df, javaObj)
    .flushDumpPileIfNeeded()
  }
}

.getConnectionId <- function() {
  if (!exists("connectionId", envir = settingEnv)) {
    assign("connectionId", as.integer(0), envir = settingEnv)
  }
  return(get("connectionId", envir = settingEnv))
}

#
# Constructor of the java.object class
#
new_java.object <- function(classname, hashcodeInt, affinity = 1) {
  me <- new.env(parent = emptyenv())
  me$.class <- classname
  me$.hashcode <- hashcodeInt
  me$.connectionId <- .getConnectionId()
  class(me) <- c("java.object")
  reg.finalizer(me, .finalize)
  .setFunctionsForThisJavaReference(me, me$.class, affinity)
  return(me)
}


.setFunctionsForThisJavaReference <- function(obj, classname, affinity) {
  if (!methods::is(obj, "java.object") && !methods::is(obj, "java.list")) {
    stop("The argument should be a java.object or a java.list instance!")
  }
  if (!exists(classname, envir = .getClassMap())) {
    functionNames <- .getClassInfo(classname, affinity)
    endOfMethodIndex <- which(functionNames == "endOfMethods")
    if (endOfMethodIndex > 1) {
      functions <- functionNames[1:(endOfMethodIndex-1)]
    } else {
      functions <- NULL
    }
    if (endOfMethodIndex < length(functionNames)) {
      members <- functionNames[(endOfMethodIndex + 1):length(functionNames)]
    } else {
      members <- NULL
    }
    frameForThisClass <- new.env(parent = emptyenv())
    frameForThisClass$functions <- functions
    frameForThisClass$members <- members
    assign(classname, frameForThisClass, envir = .getClassMap())
  }
  functionNames <- get(classname, envir = .getClassMap())$functions
  if (!is.null(functionNames)) {
    invisible(lapply(functionNames, function(name, obj) {
      f <- function(..., affinity = 1) {
        callJavaMethod(obj, name, ..., affinity = affinity)
      }
      delayedAssign(name, f, assign.env = obj)
    }, obj))
  }
  memberNames <- get(classname, envir = .getClassMap())$members
  if (!is.null(memberNames)) {
    invisible(lapply(memberNames, function(name, obj) {
      delayedAssign(name, getJavaField(obj, name), assign.env = obj)
    }, obj))
  }
}




#'
#' Print a java.object instance
#'
#' The class name and the hashcode of the reference
#' are displayed.
#'
#' @param x a java.object instance
#' @param ... additional parameters for consistent overriding
#'
#' @export
print.java.object <- function(x, ...) {
  print(.toString(x))
}

#'
#' Override the default length function
#'
#' A java.object class is a list by definition. However, its length
#' is 1.
#'
#' @param x a java.object instance
#' @return 1
#'
#' @export
length.java.object <- function(x) {
  return(1)
}

#' @export
'$<-.java.object' <- function(x, y, value) {
  if (!exists(y, envir = x)) {
    stop("The variable does not exist within the java.object instance and it cannot be assigned!")
  }
  obj <- get(y, envir = x)
  if (!is.function(obj) & y != ".class" & y != ".hashcode") {
    setJavaField(x, y, value)
    NextMethod()
    # assign(y, value, envir = x)  ### to synchronize the reference with the true object
  } else {
    stop(paste("The variable or function", y, "cannot be redefined!"))
  }
}

#' @export
'$.java.object' <- function(x, y) {
  returnValue <- NextMethod()
  if (!is.function(returnValue) && !(y %in% c(".class", ".hashcode", ".connectionId"))) {
    returnValue <- getJavaField(x, y)
    assign(y, returnValue, envir = x)
  }
  return(returnValue)
}


.createJavaObjectReference <- function(str, affinity = 1) {
  inputList <- strsplit(str,MainSplitter)
  innerList <- strsplit(inputList[[1]][2], SubSplitter)
  vecStr <- innerList[[1]]
  argumentList <- strsplit(vecStr,"@")
  outputList <- lapply(argumentList, function(arguments) {
    classname <- arguments[1]
    hashcodeInt <- arguments[2]
    javaObject <- new_java.object(classname, hashcodeInt, affinity)
  })
  if (length(outputList) == 1) {
    return (outputList[[1]])
  } else {
    return(new_java.list(outputList))
  }
}


J4RConnectionHandler <- function(host, port, key, internalports) {
  # me <- list(port = port, key = key, backdoorport = internalPorts[1], gcport = internalports[2], connections = list())
  me <- new.env(parent = emptyenv())
  me$port <- port
  me$key <- key
  me$backdoorport <- internalports[1]
  me$gcport <- internalports[2]
  me$connections <- list()
  me$host <- host
  # me$numberOfSockets <- 0
  class(me) <- c("J4RConnectionHandler")
  assign("connectionId", as.integer(.getConnectionId() + 1), envir = settingEnv)
  return(me)
}

.isThereAtLeastOneConnection <- function(connectionHandler) {
  return(length(connectionHandler$connections) > 0)
}

.getClassMap <- function() {
  if (!exists("classMap", envir = cacheEnv)) {
    assign("classMap", new.env(parent = emptyenv()), envir = cacheEnv)
  }
  return(get("classMap", envir = cacheEnv))
}

.createAndSecureConnection <- function() {
  connectionHandler <- get("connectionHandler", envir = cacheEnv)
  if (is.null(connectionHandler)) {
    stop("The connection handler is null!")
  }
  for (port in connectionHandler$port) {
    if (!.connectAndSecurePort(connectionHandler, port)) {
      return(FALSE)
    }
  }
  if (!.connectAndSecurePort(connectionHandler, connectionHandler$gcport, isGCSocket = T)) {
    return(FALSE)
  }
#  assign("connectionHandler", connectionHandler, envir = cacheEnv)  ### at this point all the connections have been secured
  return(TRUE)
}

.connectAndSecurePort <- function(connectionHandler, port, isGCSocket = F) {
  isConnected <- tryCatch(
    {
      if (.isVerbose()) {
        message(paste("Connecting on port", port))
      }
      socket <- utils::make.socket(connectionHandler$host, port)
      if (isGCSocket) {
        connectionHandler$gcSocket <- socket
      } else {
        nbOfConnections <- length(connectionHandler$connections)
        connectionHandler$connections[[nbOfConnections + 1]] <- socket
      }
      utils::read.socket(socket, maxlen = bufferLength)
      TRUE
    },
    error=function(cond) {
      message("The server has started but it seems the client is unable to get connected to the server.")
      return(FALSE)
    }
  )
  if (isConnected) {
    isSecure <- .testSecurityKey(connectionHandler, socket)
    if (!isSecure) {
      if (isGCSocket) {
        rm("gcSocket", envir = connectionHandler)
      } else {
        connectionHandler$connections[[nbOfConnections + 1]] <- NULL ### we delete this invalid connection
      }
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
  return(TRUE)
}

.getGCSocket <- function() {
  return(get("connectionHandler", envir = cacheEnv)$gcSocket)
}

.testSecurityKey <- function(connectionHandler, socket) {
  isSecure <- tryCatch(
    {
      if (exists(".testKey", envir = cacheEnv)) {
        key <- get(".testKey", envir = cacheEnv)
      } else {
        key <- connectionHandler$key
      }
      key <- format(key, scientific = F)
      utils::write.socket(socket, as.character(key))
      outcome <- utils::read.socket(socket, maxlen = bufferLength)
      if (outcome == "SecurityFailed") {
        message("The client got connected but security could not be confirmed.")
      } else {
        utils::write.socket(socket, "\u00E9\u00E8\u00E0\u00EF\u00FB")  ### check encoding here
        utils::read.socket(socket, maxlen = bufferLength)
      }
      return(outcome == "SecurityChecked")
    },
    error=function(cond) {
      message("An error occurred while checking security key.")
      message(cond)
      return(FALSE)
    }
  )
}

.getSocket <- function(affinity = 1) {
  if (affinity < 1 || !is.numeric(affinity)) {
    stop("The affinity should be a strictly positive integer (e.g. >= 1)!")
  }
  if (!exists("connectionHandler", envir = cacheEnv)) {
    stop("It seems that the client is not connected to the server!")
  }
  connections <- get("connectionHandler", envir = cacheEnv)$connections
  if (affinity > length(connections)) {
    stop("The affinity should be equal to or smaller than the number of connections!")
  }
  return(connections[[affinity]])
}


.getBackdoorSocket <- function() {
  if (!exists("connectionHandler", envir = cacheEnv)) {
    tryCatch({
      .instantiateConnectionHandlerFromFile()
    },
    error=function(cond) {
      stop("The connection handler was null and it could not be instantiated!")
    })
  }
  connectionHandler <- get("connectionHandler", envir = cacheEnv)
  backdoorport <- connectionHandler$backdoorport
  socket <- utils::make.socket(connectionHandler$host, backdoorport)
  utils::read.socket(socket, maxlen = bufferLength)
  isSecure <- .testSecurityKey(connectionHandler, socket)
  if (!isSecure) {
    return(NULL)
  } else {
    return(socket)
  }
}




#'
#' Cast the object into a Java long type
#'
#' @param obj a numeric or a vector of numerics
#'
#' @export
as.long <- function(obj) {
  if (!is.numeric(obj)) {
    stop("The argument obj should be a numeric or a vector of numerics!")
  }
  obj <- format(obj, scientific = F)
  class(obj) <- "long"
  return(obj)
}

#' @export
'[.long' <- function(x,y) {
  return(as.long(as.numeric(x)[y]))
}

#'
#' Cast the object into a Java float type
#'
#' @param obj a numeric or a vector of numerics
#'
#' @export
as.float <- function(obj) {
  if (!is.numeric(obj)) {
    stop("The argument obj should be a numeric or a vector of numerics!")
  }
  class(obj) <- "float"
  return(obj)
}

#' @export
'[.float' <- function(x,y) {
  return(as.float(as.numeric(x)[y]))
}


