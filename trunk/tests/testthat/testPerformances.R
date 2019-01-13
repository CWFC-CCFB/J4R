#######################################
# Testing performance compared to rJava
########################################

context("Testing performance")

library(J4R)
library(rJava)

connectToJava()
.jinit()

createObjects <- function(n) {
  elapsedTimeJ4R <- c()
  elapsedTimeRJava <- c()

  for (iter in 1:100) {
    start <- Sys.time()
    myArrayListJ4R <- createJavaObject("java.util.ArrayList", rep(as.integer(10),n))
    elapsedTimeJ4R <- c(elapsedTimeJ4R, as.numeric(Sys.time() - start, units="secs"))

    start <- Sys.time()
    output <- list()
    for (i in 1:n) {
      output[[i]] <- .jnew("java/util/ArrayList")
    }
    elapsedTimeRJava <- c(elapsedTimeRJava, as.numeric(Sys.time() - start, units="secs"))
  }

  return(c(mean(elapsedTimeJ4R), mean(elapsedTimeRJava)))
}

#### Average time to create 1, 5, 10, and 50 objects in J4R and rJava

elapsedTimes <- createObjects(1)
test_that("rJava is faster for the instantiation of 1 object", {
  expect_equal(elapsedTimes[1] > elapsedTimes[2], TRUE)
})

elapsedTimes <- createObjects(5)
test_that("rJava is faster for the instantiation of 5 objects", {
  expect_equal(elapsedTimes[1] < elapsedTimes[2], TRUE)
})

elapsedTimes <- createObjects(10)
test_that("rJava is faster for the instantiation of 10 objects", {
  expect_equal(elapsedTimes[1] < elapsedTimes[2], TRUE)
})

elapsedTimes <- createObjects(50)
test_that("rJava is faster for the instantiation of 50 objects", {
  expect_equal(elapsedTimes[1] < elapsedTimes[2], TRUE)
})

shutdownJava()






