\name{getVignetteHeader}
\alias{getVignetteHeader}
\alias{hasVigHeaderField}
\title{A function to read vignette header information}
\description{
  Given a vignette filename, will read in the vignette header metadata.
}
\usage{
getVignetteHeader(vig, field)
hasVigHeaderField(vig, field="VignetteIndexEntry")
}
\arguments{
  \item{vig}{Vignette filename}
  \item{field}{A specific field to extract}
}
\details{
  The \code{getVignetteHeader} function will extract the metadata from a
  vignette file and return it as a named list, where the names of the
  list elements correspond to the metadata fields, and the elements
  themselves the values.  If a specific field is desired, it can be
  specified with the 'field' argument.

  The \code{hasVigHeaderField} function is a simple wrapper around
  \code{getVignetteHeader} and will most likely be removed in the very
  near future.  It just is a boolean to report if a given header field
  exists or not.
}
\author{Jeff Gentry}
\examples{

   ## !! Need a working vignette somewhere to work this on
}
\keyword{utilities}
