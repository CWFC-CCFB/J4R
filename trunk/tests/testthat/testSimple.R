########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Simple tests in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)
connectToJava()

####  Creating a single object with a basic constructor ####

# Here, an ArrayList instance is created in Java and a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList")
mySimpleJavaObject

test_that("Returned object is of class java.object", {
  expect_equal(class(mySimpleJavaObject)[length(class(mySimpleJavaObject))], "java.object")
})

#### Creating a single object with a parameterized constructor ####

# Here, an ArrayList instance with a capacity of 3 is created, since R calls the constructor ArrayList(int i). Again, a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList", as.integer(3))
mySimpleJavaObject

test_that("Returned object is of class java.object", {
  expect_equal(class(mySimpleJavaObject)[length(class(mySimpleJavaObject))], "java.object")
})

#### Creating many objects with a parameterized constructor ####

# Here, three ArrayList instances are created with a capacity of 3, 4, and 5 respectively. The reference returned to the R environment is a java.arraylist with three java.object instances in it.

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)
myArrayLists

test_that("myArrayLists object has three java.object instances", {
  expect_equal(class(myArrayLists)[length(class(myArrayLists))], "java.arraylist")
  expect_equal(length(myArrayLists), 3)
  expect_equal(class(myArrayLists[[1]])[length(class(myArrayLists[[1]]))], "java.object")
  expect_equal(class(myArrayLists[[2]])[length(class(myArrayLists[[2]]))], "java.object")
  expect_equal(class(myArrayLists[[3]])[length(class(myArrayLists[[3]]))], "java.object")
})

#### Calling a method on a Java object ####

# In this example, the value of 15 is added to the ArrayList instance that was previously created. The method add returns a boolean. Then we call the method .get(0) on the same object. The value of 15 is then returned to R.

callJavaMethod(mySimpleJavaObject, "add", as.integer(15))

test_that("Adding 15 to mySimpleJavaObject instance", {
  expect_equal(callJavaMethod(mySimpleJavaObject, "get", as.integer(0)), 15)
})

#### Calling a method several times on a Java object ####

# The values of 15, 16, and 17 are added to the ArrayList instance which now has 4 elements.

callJavaMethod(mySimpleJavaObject, "add", 15:17)

# The following code returns those four elements:

test_that("Getting the four first element of my ArrayList object", {
  expect_equal(callJavaMethod(mySimpleJavaObject, "get", 0:3), c(15,15,16,17))
  expect_equal(getAllValuesFromListObject(mySimpleJavaObject), c(15,15,16,17))
})


#### Calling a method on several Java objects of the same class ####

callJavaMethod(myArrayLists, "add", 15)

test_that("Adding 15 to each ArrayList instances in myArrayLists object", {
  expect_equal(callJavaMethod(myArrayLists, "get", as.integer(0)), c(15,15,15))
})

callJavaMethod(myArrayLists, "clear")

#### Calling a method several times on many Java objects of the same class ####

callJavaMethod(myArrayLists, "add", 15:17)

test_that("Adding 15, 16 and 17 to the first, second and third instances of ArrayList in myArrayLists", {
  expect_equal(callJavaMethod(myArrayLists, "get", as.integer(0)), c(15,16,17))
})

#### Calling the garbage collector ####

myArrayLists[[2]] <- NULL

nbObjects <- callJavaGC(environment())

test_that("Removing one object from the java.arraylist object and synchronizing yield 3 objects registered in the Java environment", {
  expect_equal(nbObjects, 3)
})

rm("myArrayLists")

nbObjects <- callJavaGC(environment())

test_that("Removing the java.arraylist object and synchronizing yield a single object left in the Java environment", {
  expect_equal(nbObjects, 1)
})

rm(list = ls(envir = environment()))

nbObjects <- callJavaGC(environment())

test_that("Removing all the java.arraylist object and synchronizing yield no object left in the Java environment", {
  expect_equal(nbObjects, 0)
})

#### Instantiating an Enum variable ####

# In this example, the Enum variable Species.Fagus_sylvatica is instantiated. Then calling the method name() on
# this enum returns:

beechEnum <- createJavaObject("repicea.simulation.species.REpiceaSpecies$Species", "Fagus_sylvatica")
resultNameFunction <- callJavaMethod(beechEnum, "name")

test_that("Testing the method name() on a Species enum variable", {
  expect_equal(resultNameFunction, "Fagus_sylvatica")
})

#### Instantiating many enum variables ####

enumValue <- rep("alive", J4R::maxVectorLength)
enumList <- createJavaObject("repicea.simulation.covariateproviders.treelevel.TreeStatusProvider$StatusClass", enumValue)

test_that(paste("Instantiating", J4R::maxVectorLength,  "times an enum variable", sep=" "), {
  expect_equal(length(enumList), J4R::maxVectorLength)
})

#### Calling static method several time ####

result <- callJavaMethod("java.lang.Math", "sqrt", c(3.5,4))
result

test_that("Call on the sqrt method in the Math class", {
  expect_equal(result[1], 3.5^.5)
  expect_equal(result[2], 4^.5)
})


#### Creating a null instance ####

result <- createJavaObject("java.util.ArrayList", isNullObject = TRUE)
result

test_that("Create a NullWrapper instance", {
  expect_equal(result$class, "repicea.lang.codetranslator.REnvironment$NullWrapper")
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


#### Check if libraries are part of the path ####

test_that("Check the return value of checkIfExtensionsContain", {
  expect_equal(checkIfExtensionsContain("repicea.jar"), TRUE)
  expect_equal(checkIfExtensionsContain("lerfob-foresttools.jar"), FALSE)
})

myNewList <- createJavaObject("java.util.ArrayList")
callJavaMethod(myNewList, "add", createJavaObject("java.util.HashMap"))
output <- getAllValuesFromListObject(myNewList)

test_that("Check if the getAllValuesFromListObject returns a list even if the original list contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output[[1]]$class, "java.util.HashMap")
})


myNewList <- createJavaObject("java.util.ArrayList")
callJavaMethod(myNewList, "add", 5)
output <- getAllValuesFromListObject(myNewList)

test_that("Check if the getAllValuesFromListObject returns a numeric even if the original list contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output[[1]], 5)
})

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


#### Check if inconsistent numbers of parameters will throw an exception ####

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)
myArrayLists

out <- tryCatch(
  {
    callJavaMethod(myArrayLists, "add", c(12,13))
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Check if an exception is raised when the number of parameters is inconsistent
          with the length of the source in the callJavaMethod function", {
  expect_equal(out, "threw an exception")
})


#### Creating more than 200 instances ####

largeNumberOfArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10),601))

test_that("The size of a large java.arraylist object", {
            expect_equal(length(largeNumberOfArrayLists), 601)
          })

largeNumberOfAdding <- callJavaMethod(largeNumberOfArrayLists, "add", 10)

test_that("Adding ten to the 601 ArrayList instances", {
  expect_equal(length(largeNumberOfAdding), 601)
  expect_equal(callJavaMethod(largeNumberOfArrayLists[[1]], "size"), 1)
})

largeNumberOfAdding <- callJavaMethod(largeNumberOfArrayLists, "add", as.numeric(1:601))
test_that("Adding 1 to 601 to the 601 ArrayList instances", {
  expect_equal(length(largeNumberOfAdding), 601)
  expect_equal(callJavaMethod(largeNumberOfArrayLists[[2]], "get", 0:1), c(10, 2))
})

####  Shutting down Java ####

# The server is shutted down through the shutdownJava function:

shutdownJava()

