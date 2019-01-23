####################################
# Home made tests for J4R
###################################

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

####  Shutting down Java ####

# The server is shutted down through the shutdownJava function:

shutdownJava()

