########################################################
# Class definitions
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2019
########################################################


# java.list <- function() {
#   me <- list()
#   class(me) <- c("java.list", class(me))
#   return(me)
# }

# as.java.list <- function(myList) {
#   if (!is.list(myList)) {
#     stop("The myList argument must be a list instance!")
#   }
#   class(myList) <- c("java.list", class(myList))
#   if (get("delaySettingFunctions", envir = settingEnv) == F) {
#     .setFunctionsForThisJavaReference(myList, myList[[1]]$.class)
#   }
#   return(myList)
# }

# new_java.dump <- function() {
#   me <- new.env(parent = emptyenv())
#   me$.innerList <- list()
#   class(me) <- c("java.dump", "java.list")
#   return(me)
# }
#
# '[[<-.java.dump' <- function(x, y, value) {
#   invisible(x$.innerList[[y]] <- value)
# }

#
# Constructor of the java.dumpPile class.
#
# The java.dumpPile class is similar to the java.list class,
# except that it disables methods and members automated assignment.
#
new_java.dumpPile <- function(myList) {
  if (missing(myList) || is.null(myList)) {
    myList <- list()
  }
  if (!is.list(myList)) {
    stop("The myList argument must be a list instance!")
  }
  me <- new.env(parent = emptyenv())
  me$.innerList <- myList
  class(me) <- c("java.dumpPile", "java.list")
  return(me)
}

'[.java.dumpPile' <- function(x,y) {
  return(new_java.dumpPile(x$.innerList[y]))
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
    if (get("delaySettingFunctions", envir = settingEnv) == F) {
      .setFunctionsForThisJavaReference(me, me$.innerList[[1]]$.class)
    }
  }
  return(me)
}

#'
#' Create a subsetting operator for the java.list class.
#'
#' The subsetting operator is delegated to the inner list.
#'
#' @param x a java.list instance
#' @param y either a numeric or a character
#'
#' @return a java.object instance from the inner list
#'
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

#'
#' Create a subsetting operator for the java.list class.
#'
#' The subsetting operator is delegated to the inner list.
#'
#' @param x a java.list instance
#' @param y either a numeric or a character
#'
#' @return a java.list that contains a subset of the original java.list
#'
#' @export
'[.java.list' <- function(x,y) {
  return(new_java.list(x$.innerList[y]))
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
     assign("dumpPile", new_java.dumpPile(), envir = cacheEnv)
     # print("I've just flushed the dump pile!")
  }
}

.isDumpPileFlushDelayed <- function() {
  return(get("delayDumpPileFlush", envir = settingEnv))
}


.getDumpPile <- function() {
  if (!exists("dumpPile", envir = cacheEnv)) {
    assign("dumpPile", new_java.dumpPile(), envir = cacheEnv)
  }
  return(get("dumpPile", envir = cacheEnv))
}


.finalize <- function(javaObj) {
  if (isConnectedToJava()) {
    df <- .getDumpPile()
    df$.innerList[[length(df) + 1]] <- javaObj
    assign("dumpPile", df, envir = cacheEnv)
    .flushDumpPileIfNeeded()
  }
}

#
# Constructor of the java.object class
#
new_java.object <- function(classname, hashcodeInt) {
  me <- new.env(parent = emptyenv())
  me$.class <- classname
  me$.hashcode <- hashcodeInt
  class(me) <- c("java.object")
  reg.finalizer(me, .finalize)
  if (get("delaySettingFunctions", envir = settingEnv) == F) {
    .setFunctionsForThisJavaReference(me, me$.class)
  }
  return(me)
}

.setFunctionsForThisJavaReference <- function(obj, classname) {
  if (!methods::is(obj, "java.object") && !methods::is(obj, "java.list")) {
    stop("The argument should be a java.object or a java.list instance!")
  }
#  classname <- obj$.class
  if (!exists(classname, envir = .getClassMap())) {
    functionNames <- .getClassInfo(classname)
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

.createJavaObjectReference <- function(str) {
  inputList <- strsplit(str,MainSplitter)
  innerList <- strsplit(inputList[[1]][2], SubSplitter)
  vecStr <- innerList[[1]]
  argumentList <- strsplit(vecStr,"@")
  outputList <- lapply(argumentList, function(arguments) {
    classname <- arguments[1]
    hashcodeInt <- as.integer(arguments[2])
    javaObject <- new_java.object(classname, hashcodeInt)
  })
  if (length(outputList) == 1) {
    return (outputList[[1]])
  } else {
    # return(as.java.list(outputList))
    return(new_java.list(outputList))
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
#' Override the subsetting for long instances
#'
#' This function is required. Otherwise the long class
#' is lost after the subsetting.
#'
#' @param x a long instance
#' @param y a numeric that stands for the index
#'
#' @return a long instance that is a subset of x
#'
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

#'
#' Override the subsetting for float instance
#'
#' This function is required. Otherwise the float class
#' is lost after the subsetting.
#'
#' @param x a float instance
#' @param y a numeric that stands for the index
#'
#' @return a float instance that is a subset of x
#'
#' @export
'[.float' <- function(x,y) {
  return(as.float(as.numeric(x)[y]))
}



