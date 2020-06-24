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


