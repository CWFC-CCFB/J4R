########################################################
# Class definitions
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2019
########################################################


java.list <- function() {
  me <- list()
  class(me) <- c("java.list", class(me))
  return(me)
}

as.java.list <- function(myList) {
  class(myList) <- c("java.list", class(myList))
  return(myList)
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
  classname <- x$class
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
     assign("dumpPile", java.list(), envir = cacheEnv)
     # print("I've just flushed the dump pile!")
  }
}

.isDumpPileFlushDelayed <- function() {
  return(get("delayDumpPileFlush", envir = settingEnv))
}


.getDumpPile <- function() {
  if (!exists("dumpPile", envir = cacheEnv)) {
    assign("dumpPile", java.list(), envir = cacheEnv)
  }
  return(get("dumpPile", envir = cacheEnv))
}


.finalize <- function(javaObj) {
  if (isConnectedToJava()) {
    df <- .getDumpPile()
    df[[length(df) + 1]] <- javaObj
    assign("dumpPile", df, envir = cacheEnv)
    .flushDumpPileIfNeeded()
  }
}

java.object <- function(classname, hashcodeInt) {
  me <- new.env(parent = emptyenv())
  me$.class <- classname
  me$.hashcode <- hashcodeInt
  class(me) <- c("java.object") ##, class(me))
  reg.finalizer(me, .finalize)
  if (get("delaySettingFunctions", envir = settingEnv) == F) {
    .setFunctionsForThisJavaReference(me)
  }
  return(me)
}

.setFunctionsForThisJavaReference <- function(obj) {
  if (methods::is(java.object, "java.object")) {
    stop("The argument should be a java.object instance!")
  }
  classname <- obj$.class
  if (!exists(classname, envir = .getClassMap())) {
    functionNames <- .getClassInfo(classname)
    frameForThisClass <- new.env(parent = emptyenv())
    frameForThisClass$functionNames <- functionNames
    assign(classname, frameForThisClass, envir = .getClassMap())
  }
  functionNames <- get(classname, envir = .getClassMap())$functionNames
  invisible(lapply(functionNames, function(name, obj) {
    f <- function(..., affinity = 1) {
      callJavaMethod(obj, name, ..., affinity = affinity)
    }
    assign(name, f, envir = obj)
  }, obj))
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

.createJavaObjectReference <- function(str) {
  inputList <- strsplit(str,MainSplitter)
  innerList <- strsplit(inputList[[1]][2], SubSplitter)
  vecStr <- innerList[[1]]
  argumentList <- strsplit(vecStr,"@")
  outputList <- lapply(argumentList, function(arguments) {
    classname <- arguments[1]
    hashcodeInt <- as.integer(arguments[2])
    javaObject <- java.object(classname, hashcodeInt)
  })
  if (length(outputList) == 1) {
    return (outputList[[1]])
  } else {
    return(as.java.list(outputList))
  }
}


J4RConnectionHandler <- function(port, key, backdoorport) {
  me <- list(port = port, key = key, backdoorport = backdoorport, connections = list())
  me$numberOfSockets <- 0
  class(me) <- c("J4RConnectionHandler", class(me))
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
    isConnected <- tryCatch(
      {
        message(paste("Connecting on port", port))
        socket <- utils::make.socket("localhost", port)
        nbOfConnections <- length(connectionHandler$connections)
        connectionHandler$connections[[nbOfConnections + 1]] <- socket
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
        connectionHandler$connections[[nbOfConnections + 1]] <- NULL ### we delete this invalid connection
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
  assign("connectionHandler", connectionHandler, envir = cacheEnv)  ### at this point all the connections have been secured
  return(TRUE)
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


.getBackdoorSocket <- function() {
  if (!exists("connectionHandler", envir = cacheEnv)) {
    tryCatch({
      .instantiateConnectionHandler()
    },
    error=function(cond) {
      stop("The connection handler was null and it could not be instantiated!")
    })
  }
  connectionHandler <- get("connectionHandler", envir = cacheEnv)
  backdoorport <- connectionHandler$backdoorport
  socket <- utils::make.socket("localhost", backdoorport)
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




