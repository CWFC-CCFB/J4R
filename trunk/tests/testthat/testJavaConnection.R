########################################################
# Testing connection with the Java server
########################################################

context("Testing Java connection")

library(J4R)

connectToJava(extensionPath = paste(getwd(),"/javatests", sep=""))

result <- callJavaMethod("J4RTestClass", "testFunction")

test_that("Classpath to J4RTestClass makes it possible to call the testFunction in that class", {
  expect_equal(result, "Hello World!")
})

killJava()

connectionEstablished <- connectToJava()
test_that("The JVM has been properly killed by the killJava function", {
  expect_equal(connectionEstablished, TRUE)
})

shutdownJava()

connectionEstablished <- connectToJava()
test_that("The JVM has been properly shutted down by the shutdownJava function", {
  expect_equal(connectionEstablished, TRUE)
})

#callJavaMethod("java.lang.System","getProperty", "java.version")

shutdownJava()
