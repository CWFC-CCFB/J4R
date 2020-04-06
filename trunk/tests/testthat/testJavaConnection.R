########################################################
# Testing connection with the Java server
########################################################

context("Testing Java connection")

library(J4R)

if (isConnectedToJava()) {
  shutdownJava()
}

connectToJava(extensionPath = paste(getwd(),"/javatests", sep=""))

result <- callJavaMethod("J4RTestClass", "testFunction")

test_that("Classpath to J4RTestClass makes it possible to call the testFunction in that class", {
  expect_equal(result, "Hello World!")
})

shutdownJava()

connectionEstablished <- connectToJava()
test_that("The JVM has been properly shutted down by the shutdownJava function", {
  expect_equal(connectionEstablished, TRUE)
})

