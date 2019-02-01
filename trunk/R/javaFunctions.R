#########################################################
# R function for easier object handling
# Author Mathieu Fortin - January 2019
########################################################



#'
#' Returns all the elements of a Java instance of List
#'
#' All the elements of a Java List instance are returned.
#'
#' @param object a java.object that represents a Java List instance
#'
#' @return either an R list or an R vector
#'
#' @export
getAllValuesFromListObject <- function(object) {
  if (.getClass(object) != "java.object") {
    stop("The object must be an instance of java.object")
  } else {
    systemClassLoader <- callJavaMethod("java.lang.ClassLoader", "getSystemClassLoader")
    listClass <- callJavaMethod(systemClassLoader, "loadClass", "java.util.List")
    if (callJavaMethod(listClass, "isInstance", object)) {
      size <- callJavaMethod(object, "size")
      return(callJavaMethod(object, "get", 0:(size-1)))
    } else {
      stop("The object is not an instance of List")
    }
  }
}




