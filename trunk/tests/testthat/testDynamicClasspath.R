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
  expect_equal(checkIfClasspathContains("repicea.jar"), TRUE)
  expect_equal(checkIfClasspathContains("mrnf-foresttools.jar"), FALSE)
})

urlString <- paste(getwd(),"/javatests/mrnf-foresttools.jar", sep="")
addUrlToClassPath(urlString)

test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains("repicea.jar"), TRUE)
  expect_equal(checkIfClasspathContains("mrnf-foresttools.jar"), TRUE)
})

msi <- createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex")

test_that("Check if the msi object has been created", {
  expect_equal(is.null(msi), FALSE)
  expect_equal("java.object" %in% class(msi), TRUE)
})

shutdownJava()

