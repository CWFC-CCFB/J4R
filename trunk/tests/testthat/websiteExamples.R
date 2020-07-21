#######################################################
# Examples of code shown on the website of J4R
#######################################################

connectToJava()

shutdownJava()


connectToJava()
mySimpleJavaObject <- createJavaObject("java.util.ArrayList")
mySimpleJavaObject


mySimpleJavaObject <- createJavaObject("java.util.ArrayList", as.integer(3))
mySimpleJavaObject


myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)
myArrayLists


callJavaMethod(mySimpleJavaObject, "add", 15)
callJavaMethod(mySimpleJavaObject, "get", as.integer(0))

mySimpleJavaObject$add(15)
mySimpleJavaObject$get(as.integer(0))

callJavaMethod(mySimpleJavaObject, "add", 15:17)
callJavaMethod(mySimpleJavaObject, "get", 0:4)

getAllValuesFromListObject(mySimpleJavaObject)


callJavaMethod(myArrayLists, "add", 15)
callJavaMethod(myArrayLists, "get", as.integer(0))
callJavaMethod(myArrayLists, "clear")

myArrayLists$add(15)
myArrayLists$get(as.integer(0))
myArrayLists$clear()

myArrayLists$add(15:17)
myArrayLists$get(as.integer(0))

result <- callJavaMethod("java.lang.Math", "sqrt", c(3.5,4))
result


myArray <- createJavaObject("int", 3, 3, isArray = TRUE)
myArray

myArrays <- createJavaObject("int", c(3,3), isArray = TRUE)
myArrays

mySimpleArray <- as.JavaArray(7:9)
mySimpleArray
mySimpleArray$length
getAllValuesFromArray(mySimpleArray)


m <- matrix(data = 1:6, nrow=2, ncol=3)
m
mySecondArray <- as.JavaArray(m)
getAllValuesFromArray(mySecondArray)
