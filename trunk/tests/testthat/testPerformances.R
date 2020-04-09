########################################################
# Testing performance compared to rJava
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Testing performance")

library(J4R)

if (!isConnectedToJava()) {
  connectToJava()
}

createObjects <- function(n) {
  elapsedTimeJ4R <- c()

  for (iter in 1:100) {
    start <- Sys.time()
    myArrayListJ4R <- createJavaObject("java.util.ArrayList", rep(as.integer(10),n))
    elapsedTimeJ4R <- c(elapsedTimeJ4R, as.numeric(Sys.time() - start, units="secs"))
  }

  return(c(mean(elapsedTimeJ4R)))
}

#### Average time to create 1, 5, 10, and 50 arraylist in J4R

nbObjects <- c(1,5,10,50,100,200)

elapsedTimes <- c()
for (nbObj in nbObjects) {
  elapsedTimes <- c(elapsedTimes, createObjects(nbObj))
}

times <- data.frame(elapsedTimes, nbObjects)

fit <- lm(elapsedTimes ~ nbObjects, data=times)
slope <- as.numeric(coef(fit)[2])

test_that("Time to create one additional object", {
  expect_equal(slope < 1E-4, TRUE)
})

shutdownJava()


if (getJavaVersion()$architecture == "32-Bit") {
  memorySize <- 800
} else {
  memorySize <- 3000
}

connectToJava(memorySize = memorySize)

test_that("Increased memory", {
  expect_equal(as.numeric(getMemorySettings()[1]) > (memorySize * .85), TRUE)
})



