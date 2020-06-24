% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/j4rFunctions.R
\name{getListOfJavaReferences}
\alias{getListOfJavaReferences}
\title{Provide a list of the Java references}
\usage{
getListOfJavaReferences(envir = globalenv())
}
\arguments{
\item{envir}{the environment for which the list of Java references is needed}
}
\value{
a vector with the names of the objects that belong to the java.object and java.list classes.
}
\description{
The function provides the list of the Java references in an environment environment.
}
\details{
By default this function provides the Java reference in the global environment.
}