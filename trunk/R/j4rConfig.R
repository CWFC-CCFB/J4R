########################################################
# R function for J4R configuration
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
#' Length of the buffer when reading from the socket connection.
#'
#' The buffer has a length of 100Kb by default.
#'
#' @export
bufferLength <- 100000

MainSplitter <- "/;"
SubSplitter <- "/,"
ExceptionPrefix <- "j4r.net.server.JavaLocalGatewayServer$JavaGatewayException"



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
    message("It seems that the JAVA environment variable was not set in R. Trying to rely on the OS path instead.")
    message("You can consider defining this variable through the setJavaPath function.")
    return("java")
  } else {
    return(javaPath)
  }
}


#'
#' Set the path to Java
#'
#' This is an option function that makes it possible to set the
#' JAVA environmental variable in R, if it is not already set.
#' It first tests if the path ends with java and if it is actually
#' a file.
#'
#' @param path the complete path to Java as in the example below
#'
#' @examples
#' # setJavaPath("/usr/lib/jvm/java-8-openjdk-amd64/bin/java")  ### not run
#'
#' @export
setJavaPath <- function(path) {
  if (path != "" && !endsWith(path, "java")) {
    stop("The path is incorrect. It should end with java!")
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
        system2(javaPath, args = c("-version"), stdout = T, stderr = T, wait = F)
      },
      error=function(cond) {
        return(NULL)
      }
    )
    if (is.null(output)) {
      stop("Java seems to be missing on this computer. Please check your configuration!")
    } else {
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


.onAttachLoad <- function(libname, pkgname) {
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

