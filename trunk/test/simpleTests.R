####################################
# Home made tests for J4R
###################################

#### Starting the Java server and connecting to it ####

library(J4R)
connectToJava()

####  Creating a single object with a basic constructor ####

# Here, an ArrayList instance is created in Java and a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList")
mySimpleJavaObject

#### Creating a single object with a parameterized constructor ####

# Here, an ArrayList instance with a capacity of 3 is created, since R calls the constructor ArrayList(int i). Again, a reference is returned to the R environment and stored in mySimpleJavaObject.

mySimpleJavaObject <- createJavaObject("java.util.ArrayList", as.integer(3))
mySimpleJavaObject

#### Creating many objects with a parameterized constructor ####

# Here, three ArrayList instances are created with a capacity of 3, 4, and 5 respectively. The reference returned to the R environment is a java.arraylist with three java.object instances in it.

myArrayLists <- createJavaObject("java.util.ArrayList", 3:5)
myArrayLists

#### Calling a method on a Java object ####

# In this example, the value of 15 is added to the ArrayList instance that was previously created. The method add returns a boolean. Then we call the method .get(0) on the same object. The value of 15 is then returned to R.

callJavaMethod(mySimpleJavaObject, "add", 15)
callJavaMethod(mySimpleJavaObject, "get", as.integer(0))

#### Calling a method several times on a Java object ####

# The values of 15, 16, and 17 are added to the ArrayList instance which now has 4 elements.

callJavaMethod(mySimpleJavaObject, "add", 15:17)

# The following code returns those four elements:

callJavaMethod(mySimpleJavaObject, "get", 0:3)

#### Calling a method on several Java objects of the same class ####

callJavaMethod(myArrayLists, "add", 15)

callJavaMethod(myArrayLists, "get", as.integer(0))

callJavaMethod(myArrayLists, "clear")

#### Calling a method several times on many Java objects of the same class ####

callJavaMethod(myArrayLists, "add", 15:17)

callJavaMethod(myArrayLists, "get", as.integer(0))

####  Shutting down Java ####

# The server is shutted down through the shutdownJava function:

shutdownJava()
