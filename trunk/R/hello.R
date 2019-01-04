# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
source("./R/r4jFunctions.R")

r4j.setExtensionPath("/home/fortin/Documents/7_Developpement/JavaProjects/externallibraries")
.extensionPath
r4j.connect()
r4j.shutdownJVM()
