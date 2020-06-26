########################################################
# Unitary tests for memory management in J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2020
########################################################

context("Memory management tests in J4R")

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (!isConnectedToJava()) {
  connectToJava()
}

rm(list=ls()) ### cleaning up before testing

j4r.config.registerEnvironment(environment())

nbObjects <- callJavaGC()
test_that("Removing all objects before testing", {
  expect_equal(nbObjects, 0)
})

#### Calling the garbage collector ####
mySimpleJavaObject <- createJavaObject("java.util.ArrayList", as.integer(3))

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)

myArrayLists[[2]] <- NULL

nbObjects <- callJavaGC()

test_that("Removing one object from the java.list object and synchronizing yield 3 objects registered in the Java environment", {
  expect_equal(nbObjects, 3)
})

rm("myArrayLists")

nbObjects <- callJavaGC()

test_that("Removing the java.list object and synchronizing yield a single object left in the Java environment", {
  expect_equal(nbObjects, 1)
})

rm(list = ls())

nbObjects <- callJavaGC()

test_that("Removing all the java.list object and synchronizing yield no object left in the Java environment", {
  expect_equal(nbObjects, 0)
})

shutdownJava()

