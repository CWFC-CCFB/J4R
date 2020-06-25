########################################################
# Home made tests for J4R
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: January 2019
########################################################

context("Simple tests in J4R")

#### Starting the Java server and connecting to it ####

library(J4R)

if (!isConnectedToJava()) {
  connectToJava(memorySize = 200)
}

####  Creating a single object with a basic constructor ####

# Here, an ArrayList instance is created in Java and a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList")
mySimpleJavaObject

test_that("Returned object is of class java.object", {
  expect_equal(class(mySimpleJavaObject)[length(class(mySimpleJavaObject))], "java.object")
  expect_equal(length(mySimpleJavaObject), 1)
})

#### Creating a single object with a parameterized constructor ####

# Here, an ArrayList instance with a capacity of 3 is created, since R calls the constructor ArrayList(int i). Again, a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList", as.integer(3))
mySimpleJavaObject

test_that("Returned object is of class java.object", {
  expect_equal(class(mySimpleJavaObject)[length(class(mySimpleJavaObject))], "java.object")
})

#### Creating many objects with a parameterized constructor ####

# Here, three ArrayList instances are created with a capacity of 3, 4, and 5 respectively. The reference returned to the R environment is a java.list with three java.object instances in it.

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)
myArrayLists

test_that("myArrayLists object has three java.object instances", {
  expect_equal(class(myArrayLists)[length(class(myArrayLists))], "java.list")
  expect_equal(length(myArrayLists), 3)
  expect_equal(class(myArrayLists[[1]])[length(class(myArrayLists[[1]]))], "java.object")
  expect_equal(class(myArrayLists[[2]])[length(class(myArrayLists[[2]]))], "java.object")
  expect_equal(class(myArrayLists[[3]])[length(class(myArrayLists[[3]]))], "java.object")
})

myListOfJavaReferences <- getListOfJavaReferences(envir = environment())
test_that("There are two java references in the environment", {
  expect_equal(length(myListOfJavaReferences), 2)
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


#### Instantiating an Enum variable ####

# In this example, the Enum variable ExceptionType is instantiated. Then calling the method name() on
# this enum returns:

closeConnectionEnum <- createJavaObject("j4r.net.server.BasicClient$ClientRequest", "closeConnection")
resultNameFunction <- callJavaMethod(closeConnectionEnum, "name")

test_that("Testing the method name() on an ExceptionType enum variable", {
  expect_equal(resultNameFunction, "closeConnection")
})

#### Instantiating many enum variables ####

enumValue <- rep("closeConnection", J4R::maxVectorLength)
enumList <- createJavaObject("j4r.net.server.BasicClient$ClientRequest", enumValue)

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
  expect_equal(result$class, "j4r.lang.codetranslator.REnvironment$NullWrapper")
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


#### Check if libraries are part of the path ####

if (getJavaVersion()$architecture == "32-Bit") {
  expectedJar <- "j4r_x86.jar"
} else {
  expectedJar <- "j4r.jar"
}

test_that("Check the return value of checkIfClasspathContains", {
  expect_equal(checkIfClasspathContains(expectedJar), TRUE)
  expect_equal(checkIfClasspathContains("lerfob-foresttools.jar"), FALSE)
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

#### Adding complex objects in a list ####

myList <- createJavaObject("java.util.ArrayList")
callJavaMethod(myList, "add", myArrayLists)

test_that("Testing size of ArrayList object after adding two complex objects", {
            expect_equal(callJavaMethod(myList, "size"), 3)
          })


#### Creating more than 200 instances ####

largeNumberOfArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10),601))

test_that("The size of a large java.list object", {
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


#### Creating a null array ####

myNullDoubleArray <- createJavaObject("double", 3, 3, isArray=T, isNullObject = T)
test_that("Testing if the array has been produced", {
  expect_equal(myNullDoubleArray$class, "j4r.lang.codetranslator.REnvironment$NullWrapper")
})

#### Testing if a null array is still considered when invoking a method or a constructor

path <- file.path(".","javatests","repicea.jar")
#path <- file.path(".","tests","testthat", "javatests","repicea.jar")
addToClassPath(path)

out <- tryCatch(
  {
    createJavaObject("repicea.math.Matrix", myNullDoubleArray) ### should throw a null pointer exception as it gets to the consctructor but the array of double is null
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("If the null array has been considered then there should be an exception in the Matrix.init()", {
            expect_equal(out, "threw an exception")
          })

#### Testing if an exception can be instantiated

myException <- createJavaObject("java.lang.Exception")
test_that("myException has been instantiated", {
  expect_equal("java.object" %in% class(myException), TRUE)
  expect_equal(myException$class == "java.lang.Exception", TRUE)
})

#### Testing if the call to a non static method throws an exception when looking for a static method

out <- tryCatch(
  {
    callJavaMethod("repicea.stats.REpiceaRandom", "nextDouble") ### should throw an exception that this is not a static method
    "did not throw any exception"
  },
  error = function(cond) {
    return("threw an exception")
  }
)

test_that("If the method was non static, then there should be an exception", {
  expect_equal(out, "threw an exception")
})

#### Calling methods in primitive wrapper in Java

result <- callJavaMethod(c(3.0, 5), "intValue")

test_that("Testing the intValue method of Double.class", {
  expect_equal(result, c(3,5))
})

result <- callJavaMethod(rep("myString",10), "substring", as.integer(1), as.integer(5))
test_that("Testing the substring method of String.class", {
  expect_equal(result, rep("yStr", 10))
})


#### Getting a public static field

myFlowLayout <- getJavaField("java.awt.FlowLayout", "CENTER")
test_that("Testing that the static field was correctly retrieved from FlowLayout", {
  expect_equal(myFlowLayout, 1)
})

myBorderLayout <- getJavaField("java.awt.BorderLayout", "EAST")
test_that("Testing that the static field was correctly retrieved BorderLayout", {
  expect_equal(myBorderLayout, "East")
})




myMatrix <- createJavaObject("repicea.math.Matrix", as.integer(3), as.integer(3))
nbColumns <- getJavaField(myMatrix, "m_iCols")
test_that("Testing that the number of columns was correctly retrieved a Matrix instance", {
  expect_equal(nbColumns, 3)
})

setJavaField(myMatrix, "m_iCols", as.integer(10))
newNbColumns <- getJavaField(myMatrix, "m_iCols")
test_that("Testing that the number of columns was correctly retrieved a Matrix instance", {
  expect_equal(newNbColumns, 10)
})

myMatrices <- createJavaObject("repicea.math.Matrix", as.integer(c(3,3)), as.integer(c(3,3)))
nbColumns <- getJavaField(myMatrices, "m_iCols")
test_that("Testing that the number of columns was correctly retrieved a Matrix instance", {
  expect_equal(nbColumns, c(3,3))
})

setJavaField(myMatrices, "m_iCols", as.integer(c(10,7)))
newNbColumns <- getJavaField(myMatrices, "m_iCols")
test_that("Testing that the number of columns was correctly set a Matrix instance", {
  expect_equal(newNbColumns, c(10,7))
})


vec <- c("carotte", "patate")
expected <- callJavaMethod(vec, "length")
observed <- callJavaMethod(as.factor(vec), "length")
test_that("Testing that a factor source is processed as a character", {
  expect_equal(length(which(expected != observed)) == 0, TRUE)
})


vec2 <- "a"
expected <- callJavaMethod(as.factor(vec), "lastIndexOf", vec2)
observed <- callJavaMethod(as.factor(vec), "lastIndexOf", as.factor(vec2))
test_that("Testing that a factor source and a factor parameter are processed as characters", {
  expect_equal(length(which(expected != observed)) == 0, TRUE)
})

length <- callJavaMethod("Hello world!", "length")
test_that("Testing the length of a  character string", {
  expect_equal(length, 12)
})

## Tests for primitve type wrappers

a <- createJavaObject("java.lang.String", "Hello world!")
test_that("Testing that a String created through the String constructor is returned as a primitive type", {
  expect_equal(methods::is(a, "java.object") | methods::is(a, "java.list"), FALSE)
  expect_equal(a, "Hello world!")
})

b <- createJavaObject("java.lang.String", rep("Hello world!",10))
test_that("Testing that a String created through the String constructor is returned as a primitive type", {
  expect_equal(methods::is(b, "java.object") | methods::is(b, "java.list"), FALSE)
  expect_equal(length(b), 10)
  expect_equal(length(which(b == "Hello world!")), 10)
})

c <- createJavaObject("java.lang.Integer", as.integer(1))
test_that("Testing that a String created through the String constructor is returned as a primitive type", {
  expect_equal(c, as.integer(1))
})

d <- createJavaObject("java.lang.Integer", as.integer(rep(1,10)))
test_that("Testing that a String created through the String constructor is returned as a primitive type", {
  expect_equal(length(d), 10)
  expect_equal(length(which(d == as.integer(1))), 10)
})


shutdownJava()


