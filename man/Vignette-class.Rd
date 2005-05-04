\name{Vignette-class}
\docType{class}
\alias{Vignette-class}
\alias{vigPkgVersion}
\alias{vigPkgVersion,Vignette-method}
\alias{vigPackage,Vignette-method}
\title{A Class To Represent Vignettes}
\description{This is a class that will represent a vignette file in R,
  it extends the DynDoc class}
\section{Slots}{
  \describe{
    \item{\code{package}:}{Object of class \code{"character"} The
      package that this vignette is associated with}
    \item{\code{vigPkgVersion}:}{Object of class \code{"VersionNumber"} The
    version number for this vignette's package}
    \item{\code{indexEntry}:}{Object of class \code{"character", from
	class "DynDoc"} The VignetteIndexEntry field from the document file}
    \item{\code{title}:}{Object of class \code{"character", from class
	"DynDoc"} The title of the vignette}
    \item{\code{path}:}{Object of class \code{"character", from class
	"DynDoc"} The path to the vignette file stored locally}
    \item{\code{pdfPath}:}{Object of class \code{"character", from class
	"DynDoc"} The path to a PDF representation of the vignette}
    \item{\code{depends}:}{Object of class \code{"character", from class
	"DynDoc"} Any package dependencies for this vignette}
    \item{\code{requires}:}{Object of class \code{"character", from
	class "DynDoc"} Any requires level dependencies for this vignette}
    \item{\code{suggests}:}{Object of class \code{"character", from
	class "DynDoc"} Any suggests level dependencies for this vignette}
    \item{\code{keywords}:}{Object of class \code{"character", from
	class "DynDoc"} Any keywords for this vignette}
    \item{\code{codeChunks}:}{Object of class \code{"chunkList", from
	class "DynDoc"} A list of code chunks from this vignette}
  }
}
\section{Extends}{
Class \code{"DynDoc"}, directly.
}
\section{Methods}{
  \describe{
    \item{package}{\code{signature(object = "Vignette")}: Retrieves the
      package name that this vignette is associated with}
    \item{vigPkgVersion}{\code{signature(object = "Vignette")}: Retrieves
      the version of the package that this vignette is associated with}
  }
}
\notes{The \code{Vignette} class is extending the \code{DynDoc} class by
  further associating the \code{DynDoc} concepts with a specific R
  package.
  }
\author{Jeff Gentry}
\seealso{\code{\link{DynDoc-class}}, \code{\link{Sweave}}}
\keyword{classes}
