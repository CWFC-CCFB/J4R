########################################################
# Class definitions
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2019
########################################################


java.list <- function() {
  me <- list()
  class(me) <- append(class(me), "java.list")
  return(me)
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

# .finalize <- function(e) {
#   print("I am cleaning")
# }

.toString <- function(x) {
  return(paste(x$class, format(x$hashcode, scientific = F), sep="@"))
}

.isFound <- function(objToBeFound, objToBeSearched) {
  if (!is.null(objToBeSearched)) {
    if (methods::is(objToBeSearched,  "java.object")) {
      return(objToBeFound$class == objToBeSearched$class && objToBeFound$hashcode == objToBeSearched$hashcode)
    } else if (methods::is(objToBeSearched, "java.list")) {
      vec <- unlist(lapply(objToBeSearched, function(obj) {
        .isFound(objToBeFound, obj)
      }))
      return(any(vec == TRUE))
    }
  }
  return(FALSE)   ### in all other cases return false
}


.isObjectFoundInThisEnvironment <- function(objToBeFound, envir = globalenv()) {
  javaObjectsInGlobalEnvironment <- lapply(getListOfJavaReferences(),
                                           function(obj) {
                                             get(obj, envir = envir)
                                           })
  vec <- lapply(javaObjectsInGlobalEnvironment, function(obj) {
    .isFound(objToBeFound, obj)
  })
  return(any(vec==TRUE))
}

.flushDumpPileIfNeeded <- function() {
  dumpPile <- .getDumpPile()
  # if (length(dumpPile) > 100) {
  #   .flush(dumpPile)
  #   assign("dumpPile", java.list(), envir = cacheEnv)
  #   print("I've just flushed the dump pile!")
  # }
}

.getDumpPile <- function() {
  if (!exists("dumpPile", envir = cacheEnv)) {
    assign("dumpPile", java.list(), envir = cacheEnv)
  }
  return(get("dumpPile", envir = cacheEnv))
}


.finalize <- function(env) {
  javaObject <- env$me  ### created through the constructor java.object
  isFound <- .isObjectFoundInThisEnvironment(javaObject) #### FIXME this bugs when tested because of the wrong environment
  ### might need to have a list of registered environment
  if (!isFound) {
    df <- .getDumpPile()
    df[[length(df) + 1]] <- javaObject
    assign("dumpPile", df, envir = cacheEnv)
    .flushDumpPileIfNeeded()
  }
}

java.object <- function(classname, hashcodeInt) {
#  reg.finalizer(environment(), .finalize)
  me <- list(class = classname, hashcode = hashcodeInt)
  class(me) <- append(class(me), "java.object")
  return(me)
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


J4RConnectionHandler <- function(port, key, backdoorport) {
  me <- list(port = port, key = key, backdoorport = backdoorport, connections = list())
  me$numberOfSockets <- 0
  class(me) <- append(class(me), "J4RConnectionHandler")
  return(me)
}

.isThereAtLeastOneConnection <- function(connectionHandler) {
  return(length(connectionHandler$connections) > 0)
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
          outcome == "SecurityChecked"
        },
        error=function(cond) {
          message("An error occurred while checking security key.")
          message(cond)
          return(FALSE)
        }
      )
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

.getBackdoorSocket <- function() {
  connectionHandler <- get("connectionHandler", envir = cacheEnv)
  if (is.null(connectionHandler)) {
    stop("The connection handler is null!")
  }
  if (is.null(connectionHandler$backdoorSocket)) {
    backdoorport <- connectionHandler$backdoorport
    socket <- utils::make.socket("localhost", backdoorport)
    connectionHandler$backdoorSocket <- socket
    assign("connectionHandler", connectionHandler, envir = cacheEnv)
  }
  return(connectionHandler$backdoorSocket)
}

as.java.list <- function(javaList) {
   class(javaList) <- append(class(javaList), "java.list")
   return(javaList)
}



