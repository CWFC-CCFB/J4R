########################################################
# R functions for connection to Gateway Server in Java
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################



#'
#' Connect to Java environment
#'
#' This function connects the R environment to a gateway server that runs in Java.
#'
#' The first argument of the function provides the listening port for the Java server. A maximum of four ports is
#' allowed. When set to 0, these ports are randomly selected. By default, the server listens to two random
#' ports.
#'
#' The extensionPath can either be set in this function or dynamically changed (see the addToClassPath function).
#'
#' @param port a vector of the listening ports for the Java server
#' @param extensionPath the path to jar files that can be loaded by the system classloader
#' @param memorySize the memory size of the Java Virtual Machine in Mb (if not specified, the JVM runs with the default memory size)
#' @param debug for debugging only (should be left as is)
#'
#' @seealso \code{\link{addToClassPath}}
#'
#' @return a logical TRUE if the function managed to get connected to the server or if it was already connected or
#' FALSE if the connection has failed
#'
#' @export
connectToJava <- function(port = c(0,0), extensionPath = NULL, memorySize = NULL, debug = FALSE) {
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
        if (any(port < 0)) {
          stop("Ports should be integers equal to or greater than 0!")
        }
        if (length(port) > 4) {
          stop("J4R allows for a maximum of 4 ports!")
        }
        parms <- c(parms, "-ports", paste(port,collapse=portSplitter))
      }
      if (!is.null(extensionPath)) {
        parms <- c(parms, "-ext", extensionPath)
      }
      if (!is.null(memorySize)) {
        if (!is.numeric(memorySize) && !is.integer(memorySize)) {
          stop("The memorySize parameter should be either a numeric or an integer!")
        }
        if (memorySize < 50) {
          stop("The minimum memory for the JVM is 50 Mb!")
        }
        parms <- c(parms, "-mem", as.integer(memorySize))
      } else if (exists("defaultJVMMemory", envir = settingEnv)) {
        memorySize <- get("defaultJVMMemory", envir = settingEnv)
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
      .instantiateConnectionHandler()
    }
    isSecure <- .createAndSecureConnection()

    # if (!isSecure) {
    #   shutdownJava()  ### for a clean exit
    # }
    return(isSecure)
  }
}

.instantiateConnectionHandler <- function() {
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

.getSocket <- function(affinity = 1) {
  if (affinity < 1 || !is.numeric(affinity)) {
    stop("The affinity should be a strictly positive integer (e.g. >= 1)!")
  }
  connections <- get("connectionHandler", envir = cacheEnv)$connections
  if (affinity > length(connections)) {
    stop("The affinity should be equal to or smaller than the number of connections!")
  }
  return(connections[[affinity]])
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
#' The number of connections to the server
#'
#' @return the number of sockets connected to the server
#'
#' @export
getNbConnections <- function() {
  if (exists("connectionHandler", envir = cacheEnv)) {
    return(length(get("connectionHandler", envir = cacheEnv)$connections))
  } else {
    return(0)
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
  .softExit()
}

.internalShutdown <- function() {
  if (isConnectedToJava()) {
    for (aff in 1:getNbConnections()) {
      utils::write.socket(.getSocket(affinity = aff), "closeConnection")
    }
    message("Closing connections and removing socket...")
  }
  if (exists("connectionHandler", envir = cacheEnv)) {  # when security is not validated, the connectionhandler object remains
    rm("connectionHandler", envir = cacheEnv)
  }
  if (exists("dumpPile", envir = cacheEnv)) {
    rm("dumpPile", envir = cacheEnv)
  }
  if (exists("classMap", envir = cacheEnv)) {
    rm("classMap", envir = cacheEnv)
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
#' Synchronize the Java environment with the R environment
#'
#' This function call the garbage collector in R and sends the list of Java references
#' that have been collected to the Java server. These references are then removed from
#' the internal map.
#'
#' @seealso \href{https://sourceforge.net/p/repiceasource/wiki/J4R/}{J4R webpage}
#'
#' @export
callJavaGC <- function() {
  gc()
  .flushDumpPileIfNeeded(1)
}


.flush <- function(javaList) {
  prefix <- "flush"
  maxLength <- length(javaList)
  nbCalls <- ceiling(maxLength / maxVectorLength)
  output <- NULL
  for (i in 1:nbCalls) {
    lowerIndex <- (i-1) * maxVectorLength + 1
    upperIndex <- i * maxVectorLength
    if (upperIndex > maxLength) {
      upperIndex <- maxLength
    }
    subList <- javaList[lowerIndex:upperIndex]
    subcommands <- paste("java.object",.translateJavaObject(subList),sep="")
    command <- paste(prefix, subcommands, sep=MainSplitter)
    utils::write.socket(.getSocket(), command)
    callback <- utils::read.socket(.getSocket(), maxlen=bufferLength)
    output <- .processResult(callback, output)
  }
  if (!is.null(output)) {
    warning("The Java server has returned something else than NULL!")
    return(output)
  } else {
    return(invisible(output))
  }
}

#' Return the number of instances stored in the
#' internal map of the Java server
#'
#' @return an integer
#'
#' @export
getNbInstancesInInternalMap <- function() {
  command <- "size"
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

#'
#' Force the JVM to shut down
#'
#' This is the not so gentle way to exit the JVM.
#'
#' In case the JVM is stuck and does not respond to interrupt. It is possible
#' to force the shutdown through this function.
#'
#' @export
killJava <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::write.socket(socket = emergencySocket, "emergencyShutdown")
      utils::close.socket(emergencySocket)
    },
    error=function(cond) {
      message("Unable to connect the server. It might be already down!")
    }
  )
  .internalShutdown()
  Sys.sleep(2)  ### wait two seconds to make sure the server is really shut down
  message("Done.")
}


#'
#' Interrupt the current task on the Java server
#'
#'
#' @export
interruptJava <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::write.socket(socket = emergencySocket, "interrupt")
      invisible(utils::close.socket(emergencySocket))
    },
    error=function(cond) {
      message("Unable to connect to the server. It might be already down!")
    }
  )
}


.softExit <- function() {
  tryCatch(
    {
      emergencySocket <- .getBackdoorSocket()
      utils::write.socket(socket = emergencySocket, "softExit")
      utils::close.socket(emergencySocket)
    },
    error=function(cond) {
      message("Unable to connect the server. It might be already down!")
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


