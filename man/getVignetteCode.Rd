\name{getVignetteCode}
\alias{getVignetteCode}
\alias{editVignetteCode}
\title{Functionality to manage code chunks from a vignette}
\description{
  These functions allow for processing and management of vignette code
  chunks within R.  Users can directly manipulate the code chunks, as
  well as evaluate them at their option.
}
\usage{
getVignetteCode(vigPath, evalEnv = new.env())
editVignetteCode(vigCode, pos, code)
}
\arguments{
  \item{vigPath}{File path of vignette file to process}
  \item{evalEnv}{An environment to use for chunk evaluations}
  \item{vigCode}{The \code{vignetteCode} object to edit}
  \item{pos}{The position of the code chunk to edit}
  \item{code}{The new code chunk}
}
\details{
  \code{getVignetteCode}:  This function will call \code{Stangle} using
  the \code{tangleToR} driver in order to retrieve the code chunks from
  the specified vignette file.  It will then compile the other pertinent
  information and return a new \code{vignetteCode} object.

  \code{editVignetteCode}:  This function will edit a code chunk
  contained within a \code{vignetteCode} and return a new object
  representing that change.  The evaluation environment in the new
  object is a copy of the original as well, *not* the same environment.
}
\author{Jeff Gentry}
\seealso{\code{\link{Sweave}},\code{\link{vignetteCode}},\code{\link{tangleToR}}}

\keyword{utilities}
