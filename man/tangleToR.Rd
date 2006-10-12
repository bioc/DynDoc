\name{tangleToR}
\alias{tangleToR}
\alias{tangleToRSetup}
\alias{tangleToRRuncode}
\alias{tangleToRFinish}
\title{An Sweave driver to retrieve code chunks}
\description{
  A driver function for \code{Sweave} which will provide the user with
  code chunks from a vignette file within R.  Functionality is very
  similar to that provided by \code{Stangle} except that an R object is
  returned as opposed to the chunks being written to a file.
}
\usage{
tangleToR()
}
\value{
  An object of type \code{chunkList} is returned, which contains the
  code chunks from the vignette file.
}
\author{Jeff Gentry}
\seealso{\code{\link{Stangle}},\code{\link{Sweave}}, \code{\link{chunkList}}}

\examples{
    require("utils")
    testfile <- system.file("Sweave", "Sweave-test-1.Rnw", package="utils")
    z <- Stangle(testfile,driver=tangleToR)
}
\keyword{utilities}

