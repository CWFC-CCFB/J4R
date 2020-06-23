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
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 5000))
system.time(system.time(callJavaMethod(myArrayLists, "add", 5)))
shutdownJava()

connectToJava(port = c(0,0))
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 5000))
system.time(callJavaMethod(myArrayLists, "add", 5))
shutdownJava()

