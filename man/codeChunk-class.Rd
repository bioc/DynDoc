\name{codeChunk-class}
\docType{class}
\alias{codeChunk-class}
\alias{codeChunk}
\alias{chunk<-}
\alias{chunk}
\alias{chunkName}
\title{Class "codeChunk"}
\description{A class to wrap necessary information for a code chunk from
  a vignette file.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("codeChunk", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{chunkName}:}{Object of class \code{"character"} The name
    (if one exists) for the code chunk}
    \item{\code{chunk}:}{Object of class \code{"character"} The code
      from the code chunk}
    \item{\code{options}:}{Object of class \code{"SweaveOptions"} Any
      options that were set at the time the code chunk appears in the
      vignette file}
  }
}

\section{Methods}{
  \describe{
    \item{evalChunk}{\code{signature(object = "codeChunk",
	env="environment")}: Will evaluate the code in the code chunk
      using the environment specified.  If no environment is specified,
      .GlobalEnv is used.}
    \item{show}{\code{signature(object = "codeChunk")}: Displays the
      information for the code chunk}
    \item{chunk<-}{\code{signature(object = "codeChunk",
	value="character")}: Edits the \code{chunk} slot of the object}
    \item{chunk}{\code{signature(object = "codeChunk")}: Returns the
      \code{chunk} slot of the object}
    \item{chunkName}{\code{signature(object = "codeChunk")}: Returns the
    name of the code chunk}
    \item{getOptions}{\code{signature(object = "codeChunk")}: Returns
      the actual options from the \code{options} slot. }
    \item{SweaveOptions}{\code{signature(object = "codeChunk")}: Returns
    the object stored in the \code{options} slot.}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Sweave}}, \code{\link{SweaveOptions}}, \code{\link{chunkList}}}

\examples{
    library(tools)
    testfile <- file.path(.path.package("tools"),
                           "Sweave", "Sweave-test-1.Rnw")
    if (file.exists(testfile)) {
        z <- Stangle(testfile,driver=tangleToR)
        getChunk(z,1)
    }
}
\keyword{classes}
