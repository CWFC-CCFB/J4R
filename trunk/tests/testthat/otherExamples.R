#######################################################
# Examples for public servers or debug
#######################################################


### Example for debugging (see wiki)
require(J4R)
connectToJava(port = 18011:18012, internalPort = 50000:50001, public=T, key=1000000)

myArray <- createJavaObject("java.util.ArrayList")
myArray$add(5)

shutdownClient()


### Example public sorver running on local host

require(J4R)
connectToJava(port = 18000:18001, internalPort = 50000:50001, public=T, key=212)

myArray <- createJavaObject("java.util.ArrayList")
myArray$add(5)

obj <- getMainInstance()
obj$add(5)

shutdownClient()

connectToJava(port = 18000:18001, internalPort = 50000:50001, public=T, key=212)  ### reconnection
obj <- getMainInstance()
obj$get(as.integer(0))

shutdownClient()


### Example public sorver running on remote host

require(J4R)
connectToJava(host="192.168.0.194", port = 18000:18001, internalPort = 50000:50001, public=T, key=212)

obj <- getMainInstance()
obj$toString()

a <- createJavaObject("java.lang.String", "ééé")
a

shutdownClient()


connectToJava(host="192.168.0.194", port = 18000:18001, internalPort = 50000:50001, public=T, key=212)  ### reconnection

killJava()



