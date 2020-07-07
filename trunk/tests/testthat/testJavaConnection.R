########################################################
# Testing connection with the Java server
########################################################

context("Testing Java connection")

library(J4R)

j4r.config.setDefaultJVMMemorySize(200)

if (isConnectedToJava()) {
  shutdownJava()
}

connectToJava(extensionPath = file.path(getwd(),"javatests"))
# connectToJava(port=c(18011,18012), debug = T)

result <- callJavaMethod("J4RTestClass", "testFunction")

test_that("Classpath to J4RTestClass makes it possible to call the testFunction in that class", {
  expect_equal(result, "Hello World!")
})

result <- callJavaMethod("J4RTestClass", "testLong", as.long(4000000))
test_that("A long has been properly processed by Java", {
  expect_equal(result, "It worked well!")
})

longs <- as.long(c(5,4))

result <- callJavaMethod("J4RTestClass", "testLong", longs)
test_that("Two longs have been properly processed by Java", {
  expect_equal(length(result), 2)
  expect_equal(all(result == "It worked well!"), TRUE)
})


result <- callJavaMethod("J4RTestClass", "testFloat", as.float(400000))
test_that("A float has been properly processed by Java", {
  expect_equal(result, "It worked well!")
})

floats <- as.float(c(5,4))

result <- callJavaMethod("J4RTestClass", "testFloat", floats)
test_that("Two floats have been properly processed by Java", {
  expect_equal(length(result), 2)
  expect_equal(all(result == "It worked well!"), TRUE)
})

shutdownJava()

connectionEstablished <- connectToJava()
test_that("The JVM has been properly shutted down by the shutdownJava function", {
  expect_equal(connectionEstablished, TRUE)
})


#### Testing that two calls to connectToJava will not affect the socket connection ####

isConnected <- connectToJava()
test_that("Testing if the second call to connectToJava returns TRUE", {
  expect_equal(isConnected, TRUE)
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

test_that("Testing if the getJavaVersion gives the same result whether or not the server is online", {
  expect_equal(versionIn, versionOut)
  expect_equal(architectureIn, architectureOut)
})

### Testing when the client cannot get connected to the server ###

isConnected <- connectToJava(debug = T, port=18013)

test_that("Testing if the connectToJava function returns FALSE when it does not connect to the server", {
  expect_equal(isConnected, FALSE)
})


### Testing connection on another port ###

isConnected <- connectToJava(port = 18013)

test_that("Testing if the connectToJava function with another port works", {
  expect_equal(isConnected, TRUE)
})

shutdownJava()


### Testing if server automatically shut down if the key is not validated ###

assign(".testKey", 1, envir = J4R::cacheEnv)

isConnected <- connectToJava()
remainingObjectsInCache <- ls(envir = J4R::cacheEnv, all.names = T)

test_that("Testing if the server is properly shutted down when the key is not validated", {
  expect_equal(isConnected, FALSE)
  expect_equal(length(remainingObjectsInCache), 2)
  expect_equal(remainingObjectsInCache[1], ".testKey")
})

rm(".testKey", envir = J4R::cacheEnv)

killJava()


