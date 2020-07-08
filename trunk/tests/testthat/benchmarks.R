connectToJava(port=c(18011, 18012), debug = T)
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 50))

system.time(replicate(100, callJavaMethod(myArrayLists, "add", 5)))
system.time(replicate(100, callJavaMethod(myArrayLists, "clear")))

callJavaMethod("java.lang.Math", "sqrt", 3)
callJavaMethod("java.lang.Math", "length")

shutdownJava()


connectToJava()

myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 500))

system.time(doRun(1))
system.time(
result <- sapply(1:2, function (i) {
  replicate(100, callJavaMethod(myArrayLists, "add", 5, thread=i))
})
)

system.time(replicate(100, callJavaMethod(myArrayLists, "clear")))



connectToJava()
myArrayList <- createJavaObject("java.util.ArrayList")
length(myArrayList)
#system.time(myArrayList <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 200)))
initialTime <- Sys.time()
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 200))
message(paste("Total Time", Sys.time() - initialTime))

system.time(allo <- callJavaMethod(myArrayLists, "add", 5))
shutdownJava()

#### single thread test in debug mode ####
connectToJava(port = c(18011), debug = T)
n <- 50
system.time(replicate(1000, createJavaObject("java.util.ArrayList", rep(as.integer(10), n))))
### 50 - 1.95 s.
### 100 - 3.3 s.
### 200 - 6.5 s.
### 400 - 15 s.
system.time(replicate(100, callJavaMethod("myArrayListsmyArrayListsmyArrayListsmyArrayListsmyArrayListsmyArrayLists", "length")))
J4R::callJavaGC()
shutdownJava()


#### single thread test ####
connectToJava()
n <- 400
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), n))
system.time(replicate(1000, callJavaMethod(myArrayLists, "size")))
shutdownJava()


connectToJava()
callJavaMethod("java.lang.Thread", "sleep", as.long(100000))
shutdownJava()


connectToJava(port = c(18011,18012), debug = T)
myArrayList <- createJavaObject("java.util.ArrayList")
setFunctionsForThisJavaReference(myArrayList)

system.time(replicate(100,callJavaMethod(myArrayList, "add", 1:100)))
system.time(replicate(100,myArrayList$add(1:100)))

myArrayList$size()

shutdownJava()



