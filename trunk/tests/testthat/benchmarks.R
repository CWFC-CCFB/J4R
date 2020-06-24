connectToJava()
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 500))

system.time(replicate(100, callJavaMethod(myArrayLists, "add", 5)))
system.time(replicate(100, callJavaMethod(myArrayLists, "clear")))

callJavaMethod("java.lang.Math", "sqrt", 3)
callJavaMethod("java.lang.Math", "length")

shutdownJava()

library(parallel)

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
#system.time(myArrayList <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 200)))
initialTime <- Sys.time()
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 203))
message(paste("Total Time", Sys.time() - initialTime))

system.time(allo <- system.time(callJavaMethod(myArrayLists, "add", 5)))
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

vector <- rep("a@2",100)
splitted <- strsplit(vector,"@")
list <- list(vector)




initialTime <- Sys.time()
for (i in 1:1000000) {
  a <- vector[2]
}
elapsedTime <- Sys.time() - initialTime
print(elapsedTime)

myFirstList <- list(c(0,0), T, 4)
mySecondList <- list(c(2,3), F, 5)
myFirstList[4] <- mySecondList
