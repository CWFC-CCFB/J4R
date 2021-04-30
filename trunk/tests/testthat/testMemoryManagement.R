########################################################
# Unitary tests for memory management in J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2020
########################################################

context("Memory management tests in J4R")

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (isConnectedToJava()) {
  shutdownClient()
}

rm(list = ls())

connectToJava()
# connectToJava(port = c(18011,18012), debug = T)

i <- 0
while (i < 10) {
  invisible(createJavaObject("java.util.ArrayList", rep(as.integer(10), 1000)))
  # gc()
  nbObjects <- getNbInstancesInInternalMap()
  print(nbObjects)
  # test_that("Test the number of instances is kept at a low level", {
  #   expect_equal(nbObjects <= 1000, T)
  # })
  i <- i + 1
}

gc()
nbObjects <- getNbInstancesInInternalMap()
print(nbObjects)
test_that("Test the number of instances is kept at a low level", {
   expect_equal(nbObjects <= 1000, T)
})


my100ArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 10))

mySecondArrayList <- createJavaObject("java.util.ArrayList")

rm(list = ls())

callJavaGC()

nbObjects <- getNbInstancesInInternalMap()

test_that("Test that there is no instance in memory", {
  expect_equal(nbObjects, 0)
})


assign("delayDumpPileFlush", TRUE, envir = settingEnv)
i <- 0
while (i < 10) {
  i <- i + 1
  invisible(createJavaObject("java.util.ArrayList", rep(as.integer(10), 1000)))
  # gc()
  # gc()
  nbObjects <- getNbInstancesInInternalMap()
  test_that("Test if the delay dump pile flush is enabled", {
    expect_equal(nbObjects >=  i * 1000, T)
  })
  print(nbObjects)
}
assign("delayDumpPileFlush", FALSE, envir = settingEnv)

callJavaGC()

nbObjects <- getNbInstancesInInternalMap()

test_that("Test that there is no instance in memory", {
  expect_equal(nbObjects, 0)
})

myFirstList <- createJavaObject("java.util.ArrayList")
mySecondList <- createJavaObject("java.util.ArrayList")
myFirstList$add(mySecondList)
myFunction <-function() {
  for (i in 1:1000) {
    myFirstList$get(as.integer(0))
  }
}
myFunction()
gc()

out <- tryCatch(
  {
    mySecondList$size()
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Check if the mySecondList is still in memory", {
  expect_equal(out, 0)
})


nbObjects <- getNbInstancesInInternalMap()

test_that("Test that there is only two instances in memory", {
  expect_equal(nbObjects, 2)
})

shutdownClient()

