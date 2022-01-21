########################################################
# Tests for arrays
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

#setJavaPath("C:\\Program Files\\Java\\jdk-11.0.6\\bin\\java.exe")
#setJavaPath("C:\\Program Files\\Java\\jdk-13.0.2\\bin\\java.exe")

print(getJavaVersion())

context("Tests for arrays in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (!isConnectedToJava()) {
  # connectToJava(c(18011,18012), debug = T)
  connectToJava()
}

myArray <- createJavaObject("java.util.HashMap", 1, isArray = TRUE)
setValueInArray(myArray, createJavaObject("java.util.HashMap"),0)
output <- getAllValuesFromArray(myArray)

test_that("Check if the getAllValuesFromArray returns a list even if the original array contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output[[1]]$.class, "java.util.HashMap")
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
  expect_equal(myNullDoubleArray$.class, "j4r.lang.codetranslator.REnvironment$NullWrapper")
})


### Creating a array of integers and filling it
mySimpleArray <- createJavaObject("int", 3, isArray = TRUE)
setValueInArray(mySimpleArray, 7:9)
diffVector <- getAllValuesFromArray(mySimpleArray) - 7:9

test_that("Check the values returned from the array", {
  expect_equal(any(diffVector != 0), FALSE)
})

test_that("Check the length of the array", {
  expect_equal(getJavaField(mySimpleArray, 'length'), 3)
  expect_equal(mySimpleArray$length, 3)
})

#### Creating a 3x3 array of integers
myArray <- createJavaObject("int", 3, 3, isArray = TRUE)

test_that("Check the class of the array", {
  expect_equal(myArray$.class, "[[I")
  expect_equal(getArrayLength(myArray), 3)
  expect_equal(getJavaField(myArray, "length"), 3)
  expect_equal(getArrayLength(getValueFromArray(myArray,0)), 3)
})

#### Creating two arrays of length 3 ####

myArrays <- createJavaObject("int", c(3,3), isArray = TRUE)
test_that("Check the class of the first and the second array", {
  expect_equal(myArrays[[1]]$.class, "[I")
  expect_equal(myArrays[[2]]$.class, "[I")
  expect_equal(getArrayLength(myArrays[[1]]), 3)
  expect_equal(getArrayLength(myArrays[[2]]), 3)
  expect_equal(all(getJavaField(myArrays, "length") == 3), TRUE)
})

for (i in 0:2) {
  setValueInArray(myArrays[[1]], i, i)
}
test_that("Check values in the array", {
  expect_equal(getValueFromArray(myArrays[[1]], 0), 0)
  expect_equal(getValueFromArray(myArrays[[1]], 1), 1)
  expect_equal(getValueFromArray(myArrays[[1]], 2), 2)
})


myOtherArray <- as.JavaArray(as.integer(3))
returnValue <- getAllValuesFromArray(myOtherArray)

test_that("Check the class of the first and the second array", {
  expect_equal(myOtherArray$.class, "[I")
  expect_equal(returnValue, 3)
})


m <- matrix(1:6, ncol=2, nrow=3)
myOther2DArray <- as.JavaArray(m)
returnValue <- getAllValuesFromArray(myOther2DArray)

test_that("Check the back conversion from array to matrix", {
  expect_equal(myOther2DArray$.class, "[[I")
  expect_equal(all(m == returnValue, TRUE), TRUE)
})

m2 <- matrix(c("carotte", "patate", "choux", "genoux", "hiboux", "tomate"), nrow = 2, ncol=3)
myOther2DArray <- as.JavaArray(m2)
returnValue <- getAllValuesFromArray(myOther2DArray)

test_that("Check the back conversion from array to matrix", {
  expect_equal(myOther2DArray$.class, "[[Ljava.lang.String")
  expect_equal(all(m2 == returnValue, TRUE), TRUE)
})

myArrayOfArrayList <- createJavaObject("java.util.ArrayList", 3, isArray = T)
setValueInArray(myArrayOfArrayList, createJavaObject("java.util.ArrayList", rep(as.integer(10),3)))
returnValue <- getAllValuesFromArray(myArrayOfArrayList)
test_that("Check the instances stored in the array", {
  expect_equal(getValueFromArray(myArrayOfArrayList, 0)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(myArrayOfArrayList, 1)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(myArrayOfArrayList, 2)$.class == "java.util.ArrayList", TRUE)
  expect_equal(methods::is(returnValue , "java.list"), TRUE)
  expect_equal(length(returnValue), 3)
})


my2DArrayOfArrayList <- createJavaObject("java.util.ArrayList", 3, 2, isArray = T)
lapply(1:3, function(i, my2DArrayOfArrayList) {
  setValueInArray(getValueFromArray(my2DArrayOfArrayList, as.integer(i-1)), createJavaObject("java.util.ArrayList", rep(as.integer(10),2)))
}, my2DArrayOfArrayList)
returnValue <- getAllValuesFromArray(my2DArrayOfArrayList)
test_that("Check the instances stored in the array", {
  expect_equal(getValueFromArray(my2DArrayOfArrayList, 0, 0)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(my2DArrayOfArrayList, 1, 0)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(my2DArrayOfArrayList, 2, 0)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(my2DArrayOfArrayList, 0, 1)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(my2DArrayOfArrayList, 1, 1)$.class == "java.util.ArrayList", TRUE)
  expect_equal(getValueFromArray(my2DArrayOfArrayList, 2, 1)$.class == "java.util.ArrayList", TRUE)
})

shutdownClient()
