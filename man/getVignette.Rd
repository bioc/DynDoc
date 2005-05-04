\name{getVignette}
\alias{getVignette}
\title{A function to handle vignette files}
\description{
  This function will take a vignette file and return a Vignette object
  in R which can be manipulated further.
}
\usage{
getVignette(vigPath, eval = TRUE)
}
\arguments{
  \item{vigPath}{The file path of the vignette file}
  \item{eval}{Whether or not to evaluate the code chunks}
}
\details{
  This function should still be considered experimental
}
\value{
  A valid \code{Vignette} object representing this vignette
}
\author{Jeff Gentry}
\seealso{\code{\link{Vignette-class}}}
\keyword{utilities}
