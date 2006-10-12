\name{chunkList-class}
\docType{class}
\alias{chunkList-class}
\alias{chunkList}
\alias{chunks}
\alias{getAllCodeChunks}
\alias{getChunk}
\alias{setChunk}
\alias{setChunk<-}
\alias{numChunks}
\alias{chunks,chunkList-method}
\alias{evalChunk,chunkList-method}
\alias{evalEnv,chunkList-method}
\alias{getAllCodeChunks,chunkList-method}
\alias{getChunk,chunkList-method}
\alias{numChunks,chunkList-method}
\alias{setChunk<-,chunkList-method}
\alias{show,chunkList-method}
\alias{summary,chunkList-method}
\title{Class "chunkList"}
\description{ This class is essentially a wrapper for the
  \code{codeChunk} class.  It contains all of the \code{codeChunks} from
  a vignette file.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("chunkList", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{chunks}:}{Object of class \code{"list"} Stores a list of
    \code{codeChunk} objects, representing all of the code chunks from a
  vignette file.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "chunkList")}: Displays verbose
    information about the code chunks}
    \item{chunks}{\code{signature(object = "chunkList")}:  Retrieves a
      list of \code{codeChunk} objects}
    \item{getAllCodeChunks}{\code{signature(object = "chunkList")}:
      Collapses all of the code chunks into one block of code and
      returns this}
    \item{getChunk}{\code{signature(object = "chunkList")}: Retrieves a
      specific code chunk}
    \item{numChunks}{\code{signature(object = "chunkList")}: Returns the
    number of code chunks in this object}
    \item{setChunk<-}{\code{signature(object = "chunkList", value="character")}: Changes
      the code in a given \code{codeChunk} contained by this object }
    \item{summary}{\code{signature(object = "chunkList")}: A less
      verbose display of information about the object}
    \item{evalChunk}{\code{signature(object = "chunkList",
	pos="numeric")}: Evaluates the code chunk at the specified
      position in the \code{chunkList} object}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Sweave}}, \code{\link{codeChunk}}, \code{\link{vignetteCode}}}

\examples{
    library("utils")
    testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package="utils")
    z <- Stangle(testfile,driver=tangleToR)
}
\keyword{classes}
