########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Test on dynamic classpath")

#### Starting the Java server and connecting to it ####

library(J4R)
connectToJava()

test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains("j4r.jar"), TRUE)
  expect_equal(checkIfClasspathContains("repicea.jar"), FALSE)
})

urlString <- paste(getwd(),"/javatests/repicea.jar", sep="")
addUrlToClassPath(urlString)

test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains("j4r.jar"), TRUE)
  expect_equal(checkIfClasspathContains("repicea.jar"), TRUE)
})

myMatrix <- createJavaObject("repicea.math.Matrix", as.integer(3), as.integer(3))

test_that("Check if the Matrix object has been created", {
  expect_equal(is.null(myMatrix), FALSE)
  expect_equal("java.object" %in% class(myMatrix), TRUE)
})

shutdownJava()

