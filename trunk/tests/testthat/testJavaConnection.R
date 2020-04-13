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


#### Testing that two calls to connectToJava will not affect the socket connection ####

callback <- connectToJava()
test_that("Testing that the second call to connectToJava returns TRUE", {
  expect_equal(callback, TRUE)
})

jVersion <- getJavaVersion()
versionIn <- jVersion$version
architectureIn <- jVersion$architecture

####  Shutting down Java ####

# The server is shutted down through the shutdownJava function:

shutdownJava()

jVersion <- getJavaVersion()
versionOut <- jVersion$version
architectureOut <- jVersion$architecture

test_that("Testing that the getJavaVersion gives the same result whether or not the server is online", {
  expect_equal(versionIn, versionOut)
  expect_equal(architectureIn, architectureOut)
})

### Testing when the client cannot get connected to the server ###

isConnected <- connectToJava(debug = T)

test_that("Testing that the connectToJava version returns FALSE when it does not connect to the server", {
  expect_equal(isConnected, FALSE)
})


