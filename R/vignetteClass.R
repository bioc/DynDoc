getVignette <- function(vigPath,eval=TRUE) {
    ## !!! Needs a lot of work right now, this can be made a lot
    ## !!! smarter.  The 'vigInfo' thing also needs to be looked at.

    require(tools) || stop("Requires package tools")
    chunkList <- Stangle(vigPath,driver=tangleToR)

    ## !! Stangle seems to cut out if no code chunks and return
    ## !! NULL.  Trying to figure out how to return an 'empty' chunkList
    ## !! within the Stangle driver
    if ((is.null(chunkList))||(eval==FALSE))
        chunkList <- new("chunkList", chunks=list(), evalEnv=new.env())

    vigInfo <- getVigInfo(vigPath)
    if (!is.null(vigInfo)) {
        if (is.null(vigInfo$VignettePackage)) {
            vigPkg <- "None"
            vigPkgVers <- buildVersionNumber("0")
        }
        else {
            vigPkg <- vigInfo$VignettePackage
            if (vigPkg %in% installed.packages()[,"Package"])
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
                      vigPkgVersion=vigPkgVers
                      )

        return(vigObj)
    }
    return(NULL)
}

    setClass("Vignette", representation(package="character",
                                        vigPkgVersion="VersionNumber"),
             contains="DynDoc")


    if (is.null(getGeneric("vigPkgVersion")))
        setGeneric("vigPkgVersion", function(object)
                   standardGeneric("vigPkgVersion"))
    setMethod("vigPkgVersion", "Vignette", function(object)
              object@vigPkgVersion)




