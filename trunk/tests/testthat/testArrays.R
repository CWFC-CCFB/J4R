########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Tests for arrays in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (!isConnectedToJava()) {
  connectToJava()
}

myArray <- createJavaObject("java.util.HashMap", 1, isArray = TRUE)
setValueInArray(myArray, createJavaObject("java.util.HashMap"),0)
output <- getAllValuesFromArray(myArray)

test_that("Check if the getAllValuesFromArray returns a list even if the original array contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output[[1]]$class, "java.util.HashMap")
})

myArray <- createJavaObject("int", 1, isArray = TRUE)
setValueInArray(myArray, as.integer(5), 0)
output <- getAllValuesFromArray(myArray)

test_that("Check if the getAllValuesFromArray returns a numeric even if the original list contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output[[1]], 5)
})

#### Creating a null array ####

myNullDoubleArray <- createJavaObject("double", 3, 3, isArray=T, isNullObject = T)
test_that("Testing if the array has been produced", {
  expect_equal(myNullDoubleArray$class, "j4r.lang.codetranslator.REnvironment$NullWrapper")
})


### Creating a array of integers and filling it
mySimpleArray <- createJavaObject("int", 3, isArray = TRUE)
setValueInArray(mySimpleArray, 7:9)
diffVector <- getAllValuesFromArray(mySimpleArray) - 7:9

test_that("Check the values returned from the array", {
  expect_equal(length(which(diffVector != 0)), 0)
})

#### Creating a 3x3 array of integers
myArray <- createJavaObject("int", 3, 3, isArray = TRUE)

test_that("Check the class of the array", {
  expect_equal(myArray$class, "[[I")
  expect_equal(getArrayLength(myArray), 3)
  expect_equal(getArrayLength(getValueFromArray(myArray,0)), 3)
})

#### Creating two arrays of length 3 ####

myArrays <- createJavaObject("int", c(3,3), isArray = TRUE)
test_that("Check the class of the first and the second array", {
  expect_equal(myArrays[[1]]$class, "[I")
  expect_equal(myArrays[[2]]$class, "[I")
  expect_equal(getArrayLength(myArrays[[1]]), 3)
  expect_equal(getArrayLength(myArrays[[2]]), 3)
})

for (i in 0:2) {
  setValueInArray(myArrays[[1]], i, i)
}
test_that("Check values in the array", {
  expect_equal(getValueFromArray(myArrays[[1]], 0), 0)
  expect_equal(getValueFromArray(myArrays[[1]], 1), 1)
  expect_equal(getValueFromArray(myArrays[[1]], 2), 2)
})


# myOtherArray <- createArray("int", 3)
#
# test_that("Check the class of the first and the second array", {
#   expect_equal(myOtherArray$class, "[I")
# })
#
#
# myOther2DArray <- createArray("int", 3, 4)
#
# test_that("Check the class of the first and the second array", {
#   expect_equal(myOther2DArray$class, "[[I")
# })
#
# myOther2DArrays <- createArray("int", c(3,3), 4)
#
# test_that("Check the class of the first and the second array", {
#   expect_equal(myOther2DArrays[[1]]$class, "[[I")
#   expect_equal(myOther2DArrays[[2]]$class, "[[I")
# })

shutdownJava()
