########################################################
# Class definitions
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: June 2019
########################################################


java.list <- function() {
  me <- list()
  class(me) <- append(class(me), "java.list")
  return(me)
}

#'
#' Print a java.list object
#'
#' The java.object instances that are included in
#' this list are displayed up to a maximum number.
#'
#' @param x a java.list instance
#' @param ... additional parameters for consistent overriding
#'
#' @export
print.java.list <- function(x, ...) {
  max <- 100
  i <- 0
  while (i < length(x) && i < max) {
    i <- i + 1
    obj <- x[[i]]
    str <- paste(obj$class, obj$hashcode, sep=":")
    print(paste("[",i,"] ", str, sep=""))
  }
  if (length(x) > max) {
    print(paste("... (", length(x) - max, " Java reference(s) omitted)",sep=""))
  }
}

# .finalize <- function(e) {
#   print("I am cleaning")
# }

java.object <- function(classname, hashcodeInt) {
  me <- list(class = classname, hashcode = hashcodeInt)
  class(me) <- append(class(me), "java.object")
  return(me)
}

#'
#' Print a java.object instance
#'
#' The class name and the hashcode of the reference
#' are displayed.
#'
#' @param x a java.object instance
#' @param ... additional parameters for consistent overriding
#'
#' @export
print.java.object <- function(x, ...) {
  print(paste("Java_ref",x$class, x$hashcode, sep="_"))
}
