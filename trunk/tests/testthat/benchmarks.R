connectToJava()
myArrayLists <- createJavaObject("java.util.ArrayList", rep(as.integer(10), 500))

system.time(replicate(100, callJavaMethod(myArrayLists, "add", 5)))
system.time(replicate(100, callJavaMethod(myArrayLists, "clear")))
