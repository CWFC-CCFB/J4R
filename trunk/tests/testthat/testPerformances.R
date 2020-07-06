########################################################
# Testing performance compared to rJava
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Testing JVM memory")

library(J4R)

if (isConnectedToJava()) {
  shutdownJava()
}

if (getJavaVersion()$architecture == "32-Bit") {
  memorySize <- 800
} else {
  memorySize <- 3000
}

connectToJava(memorySize = memorySize)

test_that("Increased memory", {
  expect_equal(as.numeric(getMemorySettings()[1]) > (memorySize * .85), TRUE)
})

shutdownJava()

j4r.config.setDefaultJVMMemorySize(200)

connectToJava()

initialValue <- 3
initialValue <- callJavaMethod("java.lang.Math", "sqrt", initialValue)^2
diffJava <- initialValue - 3
diffR <- (3^.5)^2 - 3

test_that("Testing numerical precision through square root function", {
  expect_equal(diffR/diffJava == 1, TRUE)
})


initialValue <- 1E-200
initialValue <- exp(callJavaMethod("java.lang.Math", "log", initialValue))
diffJava <- initialValue - 1E-200
diffR <- exp(log(1E-200)) - 1E-200

test_that("Testing numerical precision through log function", {
  expect_equal(diffR/diffJava == 1, TRUE)
})

shutdownJava()
