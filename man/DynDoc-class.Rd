\name{DynDoc-class}
\docType{class}
\alias{DynDoc-class}
\alias{pdfPath}
\alias{getKeywords}
\alias{getRequires}
\alias{getSuggests}
\alias{indexEntry}
\alias{codeChunks}
\alias{summary,DynDoc-method}
\alias{chunks,DynDoc-method}
\alias{codeChunks,DynDoc-method}
\alias{evalChunk,DynDoc-method}
\alias{getChunk,DynDoc-method}
\alias{getDepends,DynDoc-method}
\alias{getKeywords,DynDoc-method}
\alias{getRequires,DynDoc-method}
\alias{getSuggests,DynDoc-method}
\alias{indexEntry,DynDoc-method}
\alias{numChunks,DynDoc-method}
\alias{path,DynDoc-method}
\alias{pdfPath,DynDoc-method}
\alias{setChunk<-,DynDoc-method}
\alias{show,DynDoc-method}
\title{A Class For Dynamic Documents}
\description{The DynDoc class is used to represent dynamic documents and
  vignettes in R.}
\section{Slots}{
  \describe{
    \item{\code{indexEntry}:}{Object of class \code{"character"} The
      IndexEntry value from the document file}
    \item{\code{title}:}{Object of class \code{"character"} The name of
      the document}
    \item{\code{path}:}{Object of class \code{"character"} The path to
      the locally stored file}
    \item{\code{pdfPath}:}{Object of class \code{"character"} The path
      to a PDF rendition of the document}
    \item{\code{depends}:}{Object of class \code{"character"} Any
      package dependencies for the document}
    \item{\code{requires}:}{Object of class \code{"character"} Any
      requires level dependencies for the document}
    \item{\code{suggests}:}{Object of class \code{"character"} Any
      suggests level dependencies for the document}
    \item{\code{keywords}:}{Object of class \code{"character"} Any
      keywords for the document}
    \item{\code{codeChunks}:}{Object of class \code{"chunkList"} The
      code chunks contained in this document}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "DynDoc")}: Display information
    about the dynamic document}
    \item{summary}{\code{signature(object = "DynDoc")}: A more succinct
      informational display}
    \item{chunks}{\code{signature(object = "DynDoc")}: Returns the code
      chunks - currently in only for historical compatability with old code}
    \item{codeChunks}{\code{signature(object = "DynDoc")}: Returns the
      code chunks}
    \item{evalChunk}{\code{signature(object = "DynDoc")}: Will evaluate
      the R code contained in a chunk}
    \item{getChunk}{\code{signature(object = "DynDoc")}: Retrieves a
      specific code chunk}
    \item{getDepends}{\code{signature(object = "DynDoc")}: Obtain the
      Depends slot of the object}
    \item{getKeywords}{\code{signature(object = "DynDoc")}: Obtain the
      keywords slot of the object}
    \item{getRequires}{\code{signature(object = "DynDoc")}: A get method
    for the requires slot of this object}
    \item{getSuggests}{\code{signature(object = "DynDoc")}: Obtain the
      suggests slot of this object}
    \item{indexEntry}{\code{signature(object = "DynDoc")}: Obtain the
      indexEntry slot of this object}
    \item{numChunks}{\code{signature(object = "DynDoc")}: Returns the
      number of code chunks for this document }
    \item{path}{\code{signature(object = "DynDoc")}: Obtain the path
      slot of this object}
    \item{pdfPath}{\code{signature(object = "DynDoc")}: Obtain the
      pdfPath slot of this object}
    \item{setChunk<-}{\code{signature(object = "DynDoc")}: Change the
      code for one of the code chunks.}
  }
}
\author{Jeff Gentry}
\seealso{\code{\link{Sweave}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
