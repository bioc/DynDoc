### !!! A lot of this functionality needs to become part of a
### !!! DynDoc class, only leaving vignette specific code.

.initVigClasses <- function(where) {
    .initSweaveOptions(where)
    .initCodeChunk(where)
    .initChunkList(where)
    .initVignette(where)
}

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



editVignetteCode <- function(vigCode, pos, code) {
    chunks <- chunkList(vigCode)
    setChunk(chunks, pos) <- code

    newVig <- new("vignetteCode",
                  chunkList=chunks,
                  path=path(vigCode),
                  depends=depends(vigCode),
                  package=package(vigCode),
                  evalEnv=copyEnv(evalEnv(vigCode)))
    return(newVig)
}


.initVignette <- function(where) {
    if (is.null(getGeneric("Vignette")))
        setGeneric("Vignette", function(object)
                   standardGeneric("Vignette"), where=where)
    setClass("Vignette", representation(indexEntry="character",
                                        title="character",
                                        path="character",
                                        pdfPath="character",
                                        depends="character",
                                        requires="character",
                                        suggests="character",
                                        keywords="character",
                                        codeChunks="chunkList",
                                        package="character",
                                        pkgVersion="VersionNumber"
                                        ),
             where=where)

    ####
    #### Simple Accessors
    ####

    if (is.null(getGeneric("indexEntry")))
        setGeneric("indexEntry", function(object)
                   standardGeneric("indexEntry"), where=where)
    setMethod("indexEntry", "Vignette", function(object)
              object@indexEntry, where=where)

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

    if (is.null(getGeneric("path")))
        setGeneric("path", function(object)
                   standardGeneric("path"), where=where)
    setMethod("path", "Vignette", function(object)
              object@path, where=where)

    if (is.null(getGeneric("pdfPath")))
        setGeneric("pdfPath", function(object)
                   standardGeneric("pdfPath"), where=where)
    setMethod("pdfPath", "Vignette", function(object)
              object@pdfPath, where=where)

    if (is.null(getGeneric("depends")))
        setGeneric("depends", function(object)
                   standardGeneric("depends"), where=where)
    setMethod("depends", "Vignette", function(object)
              object@depends, where=where)

    if (is.null(getGeneric("requires")))
        setGeneric("requires", function(object)
                   standardGeneric("requires"), where=where)
    setMethod("requires", "Vignette", function(object)
              object@requires, where=where)

    if (is.null(getGeneric("suggests")))
        setGeneric("suggests", function(object)
                   standardGeneric("suggests"), where=where)
    setMethod("suggests", "Vignette", function(object)
              object@suggests, where=where)

    if (is.null(getGeneric("keywords")))
        setGeneric("keywords", function(object)
                   standardGeneric("keywords"), where=where)
    setMethod("keywords", "Vignette", function(object)
              object@keywords, where=where)

    ###
    ### Chunk manipulation
    ###

    if (is.null(getGeneric("codeChunks")))
        setGeneric("codeChunks", function(object)
                   standardGeneric("codeChunks"), where=where)
    setMethod("codeChunks", "Vignette", function(object)
              object@codeChunks, where=where)

    if (is.null(getGeneric("chunks")))
        setGeneric("chunks", function(object)
                   standardGeneric("chunks"), where=where)
    setMethod("chunks", "Vignette", function(object)
              chunks(object@codeChunks), where=where)

    if (is.null(getGeneric("setChunk<-")))
        setGeneric("setChunk<-", function(object, pos, value)
                   standardGeneric("setChunk<-"), where=where)
    setReplaceMethod("setChunk","Vignette",
                     function(object, pos,value) {
                         setChunk(object@codeChunks, pos) <- value
                         object
                     }, where=where)
    if (is.null(getGeneric("numChunks")))
        setGeneric("numChunks", function(object)
                   standardGeneric("numChunks"), where=where)
    setMethod("numChunks","Vignette", function(object)
              numChunks(object@codeChunks), where=where)
    if (is.null(getGeneric("getChunk")))
        setGeneric("getChunk", function(object, num)
                   standardGeneric("getChunk"), where=where)
    setMethod("getChunk","Vignette", function(object, num)
        getChunk(object@codeChunks, num), where=where)

    ###
    ### Output methods
    ###

    setMethod("summary","vignetteCode", function(object) {
        summary(object@codeChunks)
    }, where=where)

    setMethod("show","vignetteCode", function(object) {
        show(object@codeChunks)
    }, where=where)

}
.initChunkList <- function(where) {
    setGeneric("chunkList", function(object)
               standardGeneric("chunkList"), where=where)
    setClass("chunkList", representation(chunks="list",
                                            evalEnv="environment"),
             where=where)

    if (is.null(getGeneric("chunks")))
        setGeneric("chunks", function(object)
                   standardGeneric("chunks"), where=where)
    setMethod("chunks", "chunkList", function(object)
              object@chunks, where=where)

    if (is.null(getGeneric("numChunks")))
        setGeneric("numChunks", function(object)
                   standardGeneric("numChunks"), where=where)
    setMethod("numChunks","chunkList", function(object)
              length(object@chunks), where=where)

    if (is.null(getGeneric("evalEnv")))
        setGeneric("evalEnv", function(object)
                   standardGeneric("evalEnv"), where=where)
    setMethod("evalEnv", "chunkList", function(object)
              object@evalEnv, where=where)

    setMethod("summary","chunkList", function(object) {
        num <- numChunks(object)
        print(paste(num,"chunks are available"))
    }, where=where)

    setMethod("show","chunkList", function(object) {
        for (i in seq(along=object@chunks))
            print(getChunk(object,i))
    }, where=where)

    if (is.null(getGeneric("getChunk")))
        setGeneric("getChunk", function(object, num)
                   standardGeneric("getChunk"), where=where)
    setMethod("getChunk","chunkList", function(object, num)
        object@chunks[[num]], where=where)

    if (is.null(getGeneric("setChunk<-")))
        setGeneric("setChunk<-", function(object, pos, value)
                   standardGeneric("setChunk<-"), where=where)
    setReplaceMethod("setChunk", "chunkList", function(object, pos, value){
        oldChunk <- object@chunks[[pos]]
        chunk(oldChunk) <- value
        object@chunks[[pos]] <- oldChunk
        object
    }, where=where)

    if (is.null(getGeneric("getAllCodeChunks")))
        setGeneric("getAllCodeChunks", function(object)
                   standardGeneric("getAllCodeChunks"), where=where)
    setMethod("getAllCodeChunks", "chunkList", function(object)
        unlist(lapply(object@chunks,chunk)), where=where)

    if (is.null(getGeneric("evalChunk")))
        setGeneric("evalChunk", function(object, pos)
                   standardGeneric("evalChunk"), where=where)
    setMethod("evalChunk","chunkList", function(object, pos) {
        chunk <- chunk(getChunk(object, pos))
        chunkexps <- parse(text=chunk)
        outVec <- character()
        if (length(chunkexps) == 0)
            return(outVec)
        tmpCon <- textConnection("output","w")
        sink(file=tmpCon)
        for (nce in 1:length(chunkexps)) {
            ce <- chunkexps[[nce]]
            dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
            cat(getOption("prompt"),
                paste(dce, collapse=paste("\n",
                           getOption("continue"), sep="")),"\n")
            out <- try(.Internal(eval.with.vis(ce,
                                               object@evalEnv,
                                               NULL)))
             if(inherits(out,"try-error")) {
                 sink()
                 close(tmpCon)
                 stop(out)
             }
            if(out$visible) {
                print(out$value)
            }
            cat("\n")
        }
        sink()
        close(tmpCon)
        output <- paste(output,collapse="\n")
        return(paste(output,"\n",sep=""))
    }, where=where)
}

.initSweaveOptions <- function(where) {
    setGeneric("SweaveOptions", function(object)
               standardGeneric("SweaveOptions"), where=where)
    setClass("SweaveOptions", representation(options="list"),
             where=where)

    if (is.null(getGeneric("getOptions")))
        setGeneric("getOptions", function(object)
                   standardGeneric("getOptions"), where=where)

    setMethod("getOptions", "SweaveOptions", function(object)
              object@options, where=where)

    if (is.null(getGeneric("numOptions")))
        setGeneric("numOptions", function(object)
                   standardGeneric("numOptions"), where=where)
    setMethod("numOptions", "SweaveOptions", function(object)
              length(object@options), where=where)

    setMethod("show","SweaveOptions", function(object)
              paste(options,collapse=","), where=where)
}

.initCodeChunk <- function(where) {
    setGeneric("codeChunk", function(object)
               standardGeneric("codeChunk"), where=where)
    setClass("codeChunk", representation(chunkName="character",
                                         chunk="character",
                                         options="SweaveOptions"),
             where=where)

    if (is.null(getGeneric("chunk")))
        setGeneric("chunk", function(object)
                   standardGeneric("chunk"), where=where)
    setMethod("chunk", "codeChunk", function(object)
              object@chunk, where=where)
    if (is.null(getGeneric("chunk<-")))
        setGeneric("chunk<-", function(object, value)
                  standardGeneric("chunk<-"), where=where)
    setReplaceMethod("chunk", "codeChunk", function(object, value) {
                     object@chunk <- value
                     object
                 }, where=where)

    if (is.null(getGeneric("chunkName")))
        setGeneric("chunkName", function(object)
                   standardGeneric("chunkName"), where=where)
    setMethod("chunkName", "codeChunk", function(object)
              object@chunkName, where=where)

    if (is.null(getGeneric("SweaveOptions")))
        setGeneric("SweaveOptions", function(object)
                   standardGeneric("SweaveOptions"), where=where)
    setMethod("SweaveOptions", "codeChunk", function(object)
              object@options, where=where)

    if (is.null(getGeneric("getOptions")))
        setGeneric("getOptions", function(object)
                   standardGeneric("getOptions"), where=where)
    setMethod("getOptions", "codeChunk", function(object)
              SweaveOptions(object)@options, where=where)

    setMethod("show","codeChunk", function(object) {
        cat("Code chunk",object@chunkName,":\n",
            paste(object@chunk,collapse="\n"),"\n")
        if (numOptions(object@options) > 0)
            cat("Options:",object@options,"\n")
    }, where=where)
}

