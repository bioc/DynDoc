\name{vignetteCode-class}
\docType{class}
\alias{vignetteCode-class}
\alias{vignetteCode}
\alias{getDepends}
\alias{evalChunk}
\alias{evalEnv}
\alias{vigPackage}
\alias{path}
\alias{show,vignetteCode-method}
\alias{chunkList,vignetteCode-method}
\alias{chunks,vignetteCode-method}
\alias{evalChunk,vignetteCode-method}
\alias{evalEnv,vignetteCode-method}
\alias{getChunk,vignetteCode-method}
\alias{getDepends,vignetteCode-method}
\alias{numChunks,vignetteCode-method}
\alias{path,vignetteCode-method}
\alias{setChunk<-,vignetteCode-method}
\alias{summary,vignetteCode-method}
\alias{vigPackage,vignetteCode-method}
\title{Class "vignetteCode"}
\description{This class represents the code chunks and other related
  information from a vignette file.  It also provides for the ability to
  evaulate the code chunks in a separate environment.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("vignetteCode", ...)}
Also, a helper function \code{getVignetteCode} is provided that will do
all of the dirty work required to retrieve a \code{vignetteCode} object
from a vignette file.}
\section{Slots}{
  \describe{
    \item{\code{chunkList}:}{Object of class \code{"chunkList"} Holds
      the code chunks from the vignette file}
    \item{\code{path}:}{Object of class \code{"character"} The path of
      the vignette file }
    \item{\code{package}:}{Object of class \code{"character"} The
      package (if appropriate) that the vignette came from }
    \item{\code{getDepends}:}{Object of class \code{"character"} Any
      package dependencies for the vignette }
    \item{\code{evalEnv}:}{Object of class \code{"environment"} An
      environment used for evaulation of the code chunks.}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "vignetteCode")}: Displays
      information about the code contained in the object}
    \item{chunkList}{\code{signature(object = "vignetteCode")}:
      Retrieves the \code{chunkList} object.}
    \item{chunks}{\code{signature(object = "vignetteCode")}: Retrieves
      the actual code chunks (not wrapped by the \code{chunkList}
      class)} 
    \item{getDepends}{\code{signature(object = "vignetteCode")}: Returns
      the list of package dependencies for this vignette}
    \item{evalChunk}{\code{signature(object =
	"vignetteCode",pos="numeric")}: Will evaulate the specified code
    chunk in the \code{evalEnv} environment}
    \item{evalEnv}{\code{signature(object = "vignetteCode")}: Returns
      the evaluation environment}
    \item{getChunk}{\code{signature(object =
	"vignetteCode",pos="numeric")}: Returns the \code{codeChunk}
      object representing the specified code chunk position} 
    \item{numChunks}{\code{signature(object = "vignetteCode")}: Returns
      the number of chunks in the object }
    \item{vigPackage}{\code{signature(object = "vignetteCode")}: Returns
      the package the vignette is a part of }
    \item{path}{\code{signature(object = "vignetteCode")}: Returns the
      local file path to the vignette }
    \item{setChunk<-}{\code{signature(object =
	"vignetteCode",pos="numeric", value="character")}: Resets the
      code chunk specified by \code{pos} to contain the code specified 
      by \code{value}} 
    \item{summary}{\code{signature(object = "vignetteCode")}: A less
      verbose output of information then with \code{show} }
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Sweave}}, \code{\link{getVignetteCode}},
  \code{\link{editVignetteCode}}, \code{\link{chunkList}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
