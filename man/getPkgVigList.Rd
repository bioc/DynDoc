\name{getPkgVigList}
\alias{getPkgVigList}
\alias{getVigInfo}
\alias{baseVigDesc}
\alias{print.pkgFileList}
\alias{getVigInfoNames}
\alias{transformVigInfoLine}
\title{A function to retrieve a listing of package vignettes}
\description{
  Functionality to retrive vignette metadata, on a per-vignette or a
  per-package level.
}
\usage{
getPkgVigList(pkg, vigDescFun=baseVigDesc, vigPath = "/doc/", 
vigExt="\\.(Rnw|Snw|rnw|snw)$", pkgVers = TRUE)

getVigInfo(vig,pkg=NULL, vigDescFun=baseVigDesc, pkgVers=TRUE)
}
\arguments{
  \item{pkg}{Path to a package directory}
  \item{vig}{Filename of a vignette}
  \item{vigDescFun}{Function to provide output string for display}
  \item{vigPath}{Path to directory that contains vignettes in the
    package}
  \item{vigExt}{Regular expression pattern to match vignette file extensions}
  \item{pkgVers}{Record the package version with the other vignette metadata}
}
\details{
  getPkgVigList: This function will look at all vignette files in the directory
    \code{<pkg>/<vigPath>}.  It will then extract any header information (using
    \code{getVigInfo}), and return a list of this information.

  getVigInfo:  This function will retrieve the metadata from a
    particular vignette file.  Any line starting with '\%\\Vignette' is
    taken to be metadata.  Common values include VignetteIndexEntry (required),
    VignetteKeywords, VignetteDepends, etc.  A named list of lists is
    returned to the user, where the names correspond to the particular
    metadata variable.

  Both functions take a parameter \code{baseVigDesc}, which is a
  function to provide the output string to correspond with a vignette
  summary.  This function is directly called by \code{getVigInfo}.  It
  takes one parameter, which is a vigInfo list from \code{getVigInfo}.
}
\author{Jeff Gentry}
\seealso{\code{\link{vignette}}}
\examples{
   bioPath <- .path.package("Biobase")
   vigList <- getPkgVigList(bioPath)
   vigList
}
\keyword{utilities}
