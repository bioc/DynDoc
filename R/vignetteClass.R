### !!! A lot of this functionality needs to become part of a
### !!! DynDoc class, only leaving vignette specific code.


getVignette <- function(vigPath) {
    ## !!! Needs a lot of work right now, this can be made a lot
    ## !!! smarter.  The 'vigInfo' thing also needs to be looked at.

    require(tools) || stop("Requires package tools")
    chunkList <- Stangle(vigPath,driver=tangleToR)
    vigInfo <- getVigInfo(vigPath)
    if ((!is.null(chunkList))&&(!is.null(vigInfo))) {

        if (is.null(vigInfo$VignettePackage))
            vigPkg <- "None"
        else {
            vigPkg <- vigInfo$VignettePackage
            if (vigPkg %IN% installed.packages()[,"Package"])
                vigPkgVers <-
                    buildVersionNumber(package.description(vigPkg,
                                                           fields="Version"))
            else
                vigPkgVers <- buildVersionNumber("0")
        }

        if (is.null(vigInfo$VignetteDepends))
            vigDeps <- character()
        else
            vigDeps <- vigInfo$VignetteDepends


        if (is.null(vigInfo$Requires))
            vigInfo$Requires <- character()

        if (is.null(vigInfo$Suggests))
            vigInfo$Suggests <- character()

        if (is.null(vigInfo$PDFpath))
            vigInfo$PDFpath <- character()

        vigObj <- new("Vignette",
                      indexEntry=vigInfo$VignetteIndexEntry,
                      title=vigInfo$VignetteTitle,
                      path=vigPath,
                      pdfPath=vigInfo$PDFpath,
                      depends=vigDeps,
                      requires=vigInfo$Requires,
                      suggests=vigInfo$Suggests,
                      codeChunks=chunkList,
                      package=vigPkg,
                      pkgVersion=vigPkgVers
                      )

        return(vigObj)
    }
    return(NULL)
}

.initVignette <- function(where) {
    if (is.null(getGeneric("Vignette")))
        setGeneric("Vignette", function(object)
                   standardGeneric("Vignette"), where=where)
    setClass("Vignette", representation(package="character",
                                        pkgVersion="VersionNumber"),
             contains="DynDoc", where=where)

    if (is.null(getGeneric("package")))
        setGeneric("package", function(object)
                   standardGeneric("package"), where=where)
    setMethod("package", "Vignette", function(object)
              object@package, where=where)


    if (is.null(getGeneric("pkgVersion")))
        setGeneric("pkgVersion", function(object)
                   standardGeneric("pkgVersion"), where=where)
    setMethod("pkgVersion", "Vignette", function(object)
              object@pkgVersion, where=where)

