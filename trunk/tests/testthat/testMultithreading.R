########################################################
# Tests for multithreading in J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Tests for multithreading in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)


isConnected <- connectToJava(port = c(0,0), memorySize = 200)
test_that("Testing if the server is properly shutted down when the key is not validated", {
  expect_equal(isConnected, TRUE)
  expect_equal(getNbConnections(), 2)
})

f <- function(i, aff) {
  myArrayList <- createJavaObject("java.util.ArrayList", affinity = aff)
  callJavaMethod(myArrayList, "add", 5, affinity = aff)
}

result <- mclapply.j4r(1:1000, f)
test_that("Testing if the output has the appropriate length", {
  expect_equal(length(result), 1000)
})

shutdownJava()


