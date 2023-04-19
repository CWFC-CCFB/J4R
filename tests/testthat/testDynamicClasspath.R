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

connectToJava(extensionPath = c(urlREpicea))
test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains(expectedJar), TRUE)
  expect_equal(checkIfClasspathContains("repicea.jar"), TRUE)
})

shutdownClient()
