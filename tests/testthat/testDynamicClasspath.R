########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Test on dynamic classpath")

#### Starting the Java server and connecting to it ####

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (!isConnectedToJava()) {
  connectToJava()
}

expectedJar <- paste("j4r_server-", J4R_Server_Version, ".jar", sep="")

test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains(expectedJar), TRUE)
  expect_equal(checkIfClasspathContains("repicea.jar"), FALSE)
})

#urlREpicea <- file.path(getwd(),"tests", "testthat", "javatests", "repicea.jar")
urlREpicea <- file.path(getwd(),"javatests", "repicea.jar")

javaVersion <- as.numeric(strsplit(getJavaVersion()$version,"\\.")[[1]][1])

if (javaVersion < 16) {
  suppressWarnings(addUrlToClassPath(urlREpicea))

  test_that("Check the return value of checkIfClasspathContains", {
    expect_equal(checkIfClasspathContains(expectedJar), TRUE)
    expect_equal(checkIfClasspathContains("repicea.jar"), TRUE)
  })

  myMatrix <- createJavaObject("repicea.math.Matrix", as.integer(3), as.integer(3))

  test_that("Check if the Matrix object has been created", {
    expect_equal(is.null(myMatrix), FALSE)
    expect_equal("java.object" %in% class(myMatrix), TRUE)
  })
  shutdownClient()
} else {
  behaviour <- tryCatch({
    suppressWarnings(addUrlToClassPath(urlREpicea))
    return(F)
  },
  error=function(e) {
    message("The addUrlToClassPath function threw an exception as expected.")
    return(T)
  },
  warning=function(w) {
    return(F)
  })
  test_that("Check if an exception has been thrown.", {
    expect_equal(behaviour, TRUE)
  })
  shutdownClient()
}

connectToJava(extensionPath = c(urlREpicea))
test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains(expectedJar), TRUE)
  expect_equal(checkIfClasspathContains("repicea.jar"), TRUE)
})

shutdownClient()
