########################################################
# R functions for J4R configuration
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2020
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
#' The settings environment for this package
#'
#' This environment contains the general settings of the package.
#'
#' @export
settingEnv <- new.env()

#'
#' Length of the buffer when reading from the socket connection.
#'
#' The buffer has a length of 16Kb by default.
#'
#' @export
bufferLength <- 16000

MainSplitter <- "/;"
SubSplitter <- "/,"
ExceptionPrefix <- "j4r.net.server.JavaGatewayServer$JavaGatewayException"


#'
#' Maximum length of the vector in the parameters.
#'
#' A maximum length of the vector is set in order to avoid buffer size issues when reading
#'
#' @export
maxVectorLength <- 200


.getJavaPath <- function() {
  javaPath <- Sys.getenv("JAVA")
  if (javaPath == "") {
    if (.isVerbose()) {
      message("It seems that the JAVA environment variable has not been set. J4R will rely on the OS path instead.")
      message("You can consider defining this variable through the setJavaPath function.")
    }
    return("java")
  } else {
    return(javaPath)
  }
}


#'
#' Set the path to Java
#'
#' This is an option function that makes it possible to set the
#' JAVA environment variable in R, if it is not already set.
#' It first tests if the path ends with java or java.exe and
#' if it is actually a file. Note that if an empty character is
#' passed to this function. It resets the JAVA environment variable.
#'
#' @param path the complete path to Java as in the example below. The file.path function should be used to
#' define the path
#'
#' @seealso file.path
#'
#' @examples
#' myPath <- file.path("C:","Program Files (x86)","Java", "jre1.8.0_221", "bin", "java.exe")
#' # setJavaPath(myPath)  ### not run
#'
#' @export
setJavaPath <- function(path) {
  if (path != "" && !endsWith(path, "java") && !endsWith(path, "java.exe")) {
    stop("The path is incorrect. It should end with java (or java.exe if your OS is windows!")
  } else {
    if (path == "" || (file.exists(path) && !dir.exists(path))) {
      Sys.setenv(JAVA = path)
    } else {
      stop("The path is either a directory or it does not point to a file!")
    }
  }
}

.checkJavaVersionRequirement <- function() {
  version <- suppressMessages(getJavaVersion()$version)
  dotIndices <- gregexpr("\\.", version)
  firstDot <- dotIndices[[1]][1]
  firstInt <- as.integer(substr(version, 1, firstDot-1))
  if (firstInt == 1) {
    secondDot <- dotIndices[[1]][2]
    secondInt <- as.integer(substr(version, firstDot + 1, secondDot - 1))
    if (secondInt < 8) {
      stop(paste("The Java version", version, "does not meet the requirement of the J4R package. Please install Java version 8 or later."))
    }
  } else {
    if (firstInt < 8) {
      stop(paste("The Java version", version, "does not meet the requirement of the J4R package. Please install Java version 8 or later."))
    }
  }
  return(paste("The Java version", version, "meets the requirement of the J4R package."))
}

#'
#' Get the current Java version
#'
#' Returns the current Java version either through the command line if not connected to
#' the Java server or through the Java server if connected.
#'
#' @return a list with the first slot (version) being the version and the second slot (architecture)
#' referring to the 32-Bit or 64-Bit architecture
#'
#' @seealso getJavaArchitecture
#'
#' @export
getJavaVersion <- function() {
  if (isConnectedToJava()) {
    javaVersion <- list()
    javaVersion$version <- callJavaMethod("java.lang.System","getProperty","java.version")
    javaVersion$architecture <- paste(callJavaMethod("java.lang.System", "getProperty", "sun.arch.data.model"), "Bit", sep="-")
    return(javaVersion)
  } else {
    output <- tryCatch(
      {
        javaPath <- suppressWarnings(.getJavaPath())
        out <- list()
        out$result <- system2(javaPath, args = c("-version"), stdout = T, stderr = T, wait = F)
        out$correct <- T
        out
      },
      error=function(cond) {
        out <- list()
        out$result <- cond
        out$correct <- F
        return(out)
      }
    )
    if (output$correct == F) {
      stop(output$result)
    } else {
      output <- output$result
      javaVersion <- list()
      jv <- substring(output[1], first=regexpr("\"", output[1])[[1]] + 1)
      jv <- substring(jv, first=1, last = regexpr("\"", jv)[[1]] - 1)
      javaVersion$version <- jv
      for (str in output) {
        if (grepl("64-Bit", str)) {
          javaVersion$architecture <- "64-Bit"
          break
        }
        javaVersion$architecture <- "32-Bit"
      }
      return(javaVersion)
    }
  }
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
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/repiceasource/wiki/J4R/ .")
}


.onLoad <- function(libname, pkgname) {
  assign("delayDumpPileFlush", FALSE, envir = settingEnv, inherits = F)
  assign("verbose", FALSE, envir = settingEnv, inherits = F)
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

#'
#' Get Java architecture
#'
#' Return the architecture of the Java installation, i.e. either
#' 32-Bit or 64-Bit. It actually returns the second slot of
#' the list produced by the getJavaVersion function.
#'
#' @return the architecture, i.e. 32-Bit or 64-Bit
#'
#' @seealso getJavaVersion
#'
#' @export
getJavaArchitecture <- function() {
  javaVersion <- getJavaVersion()
  return(javaVersion$architecture)
}

#'
#' Set a default memory size for the Java Virtual Machine
#'
#' Allows to specify a default JVM size in Mb so that the option
#' memorySize in hte connectToJava function does not need to be
#' used.
#'
#' @param defaultJVMMemory the number of Mb for the JVM (must be equal to or greater than 50).
#' If set to NULL, this option has no effect.
#'
#' @export
j4r.config.setDefaultJVMMemorySize <- function(defaultJVMMemory) {
  if (is.null(defaultJVMMemory)) {
    if (exists("defaultJVMMemory", envir = settingEnv, inherits = F)) {
      rm("defaultJVMMemory", envir = settingEnv)
    }
  } else {
    if (defaultJVMMemory < 50) {
      stop("The minimum default size for the JVM is 50 Mb")
    }
    assign("defaultJVMMemory", defaultJVMMemory, envir = settingEnv, inherits = F)
  }
}

#'
#' Enabling/disabling Verbose
#'
#' It enables or diasble the verbose in hte J4R package.
#' By default, the verbose is disabled.
#'
#' @param verbose a logical
#'
#' @export
j4r.config.setVerbose <- function(verbose) {
  if (is.logical(verbose)) {
    assign("verbose", verbose, envir = settingEnv, inherits = F)
  }
}

.isVerbose <- function() {
  return(get("verbose", envir = settingEnv, inherits = F))
}

