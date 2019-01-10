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

  return(c(mean(elapsedTimeJ4R),mean(elapsedTimeRJava)))
}

#### Average time to create 50, 100, 200 and 300 objects in J4R and rJava

elapsedTimes <- createObjects(50)
test_that("rJava is faster for the instantiation of 50 objects", {
  expect_equal(elapsedTimes[1] > elapsedTimes[2], TRUE)
})

createObjects(100)
test_that("rJava is faster for the instantiation of 100 objects", {
  expect_equal(elapsedTimes[1] > elapsedTimes[2], TRUE)
})

createObjects(200)
test_that("rJava is faster for the instantiation of 200 objects", {
  expect_equal(elapsedTimes[1] < elapsedTimes[2], TRUE)
})

createObjects(300)
test_that("rJava is faster for the instantiation of 300 objects", {
  expect_equal(elapsedTimes[1] < elapsedTimes[2], TRUE)
})

shutdownJava()






