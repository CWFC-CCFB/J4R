########################################################
# Tests for multithreading in J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Tests for multithreading in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (isConnectedToJava()) {
  shutdownClient()
}

isConnected <- connectToJava()

test_that("Testing that there are two connections", {
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
  expect_equal(all(unlist(result) == T), TRUE)
})


f <- function(i, aff) {
  myArrayList <- createJavaObject("java.util.ArrayList", affinity = aff)
  myArrayList$add(5, affinity = aff)
}

result <- mclapply.j4r(1:1000, f)
test_that("Testing if the output has the appropriate length", {
  expect_equal(length(result), 1000)
  expect_equal(all(unlist(result) == T), TRUE)
})

shutdownClient()



isConnected <- connectToJava(port = 0)

test_that("Testing that there is only one connection", {
  expect_equal(isConnected, TRUE)
  expect_equal(getNbConnections(), 1)
})

f <- function(i, aff) {
  myArrayList <- createJavaObject("java.util.ArrayList", affinity = aff)
  callJavaMethod(myArrayList, "add", 5, affinity = aff)
}

result <- mclapply.j4r(1:1000, f)
test_that("Testing if the output has the appropriate length", {
  expect_equal(length(result), 1000)
  expect_equal(all(unlist(result) == T), TRUE)
})


f <- function(i, aff) {
  myArrayList <- createJavaObject("java.util.ArrayList", affinity = aff)
  myArrayList$add(5, affinity = aff)
}

result <- mclapply.j4r(1:1000, f)
test_that("Testing if the output has the appropriate length", {
  expect_equal(length(result), 1000)
  expect_equal(all(unlist(result) == T), TRUE)
})

shutdownClient()

