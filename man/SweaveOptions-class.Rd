\name{SweaveOptions-class}
\docType{class}
\alias{SweaveOptions-class}
\alias{SweaveOptions}
\alias{getOptions}
\alias{numOptions}
\alias{codeChunk,SweaveOptions-method}
\alias{getOptions,SweaveOptions-method}
\alias{numOptions,SweaveOptions-method}
\alias{show,SweaveOptions-method}
\title{Class "SweaveOptions", a class to handle options in Sweave}
\description{A small class designed to hold a set of Sweave options}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SweaveOptions", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{options}:}{Object of class \code{"list"} A list of
      strings representing options from a Sweave document.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SweaveOptions")}: Outputs the options}
    \item{getOptions}{\code{signature(object = "SweaveOptions")}:
      Retrieves the options}
    \item{numOptions}{\code{signature(object = "SweaveOptions")}:
      Returns the number of options}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Sweave}}, \code{\link{codeChunk}}}
\examples{
##!!! Need an example here
}
\keyword{classes}
