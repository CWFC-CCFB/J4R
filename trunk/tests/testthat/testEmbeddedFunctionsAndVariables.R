########################################################
# Tests for embedded functions and variables
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: July 2020
########################################################

context("Tests embedded functions and variables in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (!isConnectedToJava()) {
  connectToJava()
}

####  Creating a single object with a basic constructor ####

# Here, an ArrayList instance is created in Java and a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList")
mySimpleJavaObject$add(as.integer(15))

test_that("Adding 15 to mySimpleJavaObject instance", {
  expect_equal(mySimpleJavaObject$get(as.integer(0)), 15)
})

#### Calling a method several times on a Java object ####

# The values of 15, 16, and 17 are added to the ArrayList instance which now has 4 elements.

mySimpleJavaObject$add(15:17)

# The following code returns those four elements:

test_that("Getting the four first element of my ArrayList object", {
  expect_equal(mySimpleJavaObject$get(0:3), c(15,15,16,17))
})


#### Calling a method on several Java objects of the same class ####

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)
myArrayLists$add(15)

test_that("Adding 15 to each ArrayList instances in myArrayLists object", {
  expect_equal(myArrayLists$get(as.integer(0)), c(15,15,15))
})

myArrayLists$clear()

#### Calling a method several times on many Java objects of the same class ####

myArrayLists$add(15:17)

test_that("Adding 15, 16 and 17 to the first, second and third instances of ArrayList in myArrayLists", {
  expect_equal(myArrayLists$get(as.integer(0)), c(15,16,17))
})


myNewList <- createJavaObject("java.util.ArrayList")
myNewList$add(createJavaObject("java.util.HashMap"))
output <- getAllValuesFromListObject(myNewList)

test_that("Check if the getAllValuesFromListObject returns a list even if the original list contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output[[1]]$.class, "java.util.HashMap")
})


myNewList <- createJavaObject("java.util.ArrayList")
myNewList$add(5)
output <- getAllValuesFromListObject(myNewList)

test_that("Check if the getAllValuesFromListObject returns a numeric even if the original list contains a single object", {
  expect_equal(length(output), 1)
  expect_equal(output, 5)
})


#### Check if inconsistent numbers of parameters will throw an exception ####

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)

out <- tryCatch(
  {
    myArrayLists$add(c(12,13))
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

#### Adding complex objects in a list ####

myList <- createJavaObject("java.util.ArrayList")
myList$add(myArrayLists)

test_that("Testing size of ArrayList object after adding two complex objects", {
            expect_equal(myList$size(), 3)
          })


#### Creating more than 200 instances ####

largeNumberOfArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10),601))

test_that("The size of a large java.list object", {
            expect_equal(length(largeNumberOfArrayLists), 601)
          })

largeNumberOfAdding <- largeNumberOfArrayLists$add(10)

test_that("Adding ten to the 601 ArrayList instances", {
  expect_equal(length(largeNumberOfAdding), 601)
  expect_equal(largeNumberOfArrayLists[[1]]$size(), 1)
})

largeNumberOfAdding <- largeNumberOfArrayLists$add(as.numeric(1:601))
test_that("Adding 1 to 601 to the 601 ArrayList instances", {
  expect_equal(length(largeNumberOfAdding), 601)
  expect_equal(largeNumberOfArrayLists[[2]]$get(0:1), c(10, 2))
})

path <- file.path(".","javatests","repicea.jar")
#path <- file.path(".","tests","testthat", "javatests","repicea.jar")  ### for debugging
addToClassPath(path)

myMatrix <- createJavaObject("repicea.math.Matrix", as.integer(3), as.integer(3))
nbColumns <- myMatrix$m_iCols
test_that("Testing that the number of columns was correctly retrieved a Matrix instance", {
  expect_equal(nbColumns, 3)
})

myMatrix$m_iCols <- as.integer(10)
newNbColumns <- myMatrix$m_iCols
test_that("Testing that the number of columns was correctly retrieved a Matrix instance", {
  expect_equal(newNbColumns, 10)
  expect_equal(newNbColumns, getJavaField(myMatrix, "m_iCols"))
})

out <- tryCatch(
  {
    myMatrix$allo <- 1
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Assignment of unknown variables should not be permitted", {
  expect_equal(out, "threw an exception")
})

out <- tryCatch(
  {
    myMatrix$add <- 1
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Redefining a function should not be permitted", {
  expect_equal(out, "threw an exception")
})

out <- tryCatch(
  {
    myMatrix$.hashcode <- 1
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Redefining hashcode should not be permitted", {
  expect_equal(out, "threw an exception")
})


myMatrices <- createJavaObject("repicea.math.Matrix", as.integer(c(3,3)), as.integer(c(3,3)))
nbColumns <- myMatrices$m_iCols
test_that("Testing that the number of columns was correctly retrieved a Matrix instance", {
  expect_equal(nbColumns, c(3,3))
})

myMatrices$m_iCols <- as.integer(c(10,7))
newNbColumns <- myMatrices$m_iCols
test_that("Testing that the number of columns was correctly set a Matrix instance", {
  expect_equal(newNbColumns, c(10,7))
  expect_equal(newNbColumns, getJavaField(myMatrices, "m_iCols"))
})

out <- tryCatch(
  {
    myMatrices$allo <- 1
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Assignment of unknown variables should not be permitted", {
  expect_equal(out, "threw an exception")
})

out <- tryCatch(
  {
    myMatrices$add <- 1
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Redefining a function should not be permitted", {
  expect_equal(out, "threw an exception")
})

out <- tryCatch(
  {
    myMatrices$.innerList <- list()
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("Redefining .innerList should not be permitted", {
  expect_equal(out, "threw an exception")
})


shutdownJava()


