getVignetteCode <- function(vigPath,evalEnv=new.env()) {
    require(tools) || stop("Requires package tools")
    chunkList <- Stangle(vigPath,driver=tangleToR)
    vigInfo <- getVigInfo(vigPath)
    if ((!is.null(chunkList))&&(!is.null(vigInfo))) {
        if (is.null(vigInfo$VignettePackage))
            vigPkg <- "None"
        else
            vigPkg <- vigInfo$VignettePackage
        if (is.null(vigInfo$VignetteDepends))
            vigDeps <- character()
        else
            vigDeps <- vigInfo$VignetteDepends

        vigCode <- new("vignetteCode",chunkList=chunkList,
                       path=vigPath,
                       depends=vigDeps,
                       package=vigPkg,
                       evalEnv=evalEnv)
        return(vigCode)
    }
    return(NULL)
}

editVignetteCode <- function(vigCode, pos, code) {
    chunks <- chunkList(vigCode)
    setChunk(chunks, pos) <- code

    newVig <- new("vignetteCode",
                  chunkList=chunks,
                  path=path(vigCode),
                  depends=getDepends(vigCode),
                  package=package(vigCode),
                  evalEnv=copyEnv(evalEnv(vigCode)))
    return(newVig)
}

setClass("vignetteCode", representation(chunkList="chunkList",
                                        path="character",
                                        package="character",
                                        depends="character",
                                        evalEnv="environment"))

if (is.null(getGeneric("path")))
    setGeneric("path", function(object)
               standardGeneric("path"))
setMethod("path", "vignetteCode", function(object)
          object@path)

if (is.null(getGeneric("getDepends")))
    setGeneric("getDepends", function(object)
               standardGeneric("getDepends"))
setMethod("getDepends", "vignetteCode", function(object)
          object@depends)

if (is.null(getGeneric("chunks")))
    setGeneric("chunks", function(object)
               standardGeneric("chunks"))
setMethod("chunks", "vignetteCode", function(object)
          chunks(object@chunkList))

if (is.null(getGeneric("chunkList")))
    setGeneric("chunkList", function(object)
               standardGeneric("chunkList"))
setMethod("chunkList", "vignetteCode", function(object)
          object@chunkList)

if (is.null(getGeneric("setChunk<-")))
    setGeneric("setChunk<-", function(object, pos, value)
               standardGeneric("setChunk<-"))
setReplaceMethod("setChunk","vignetteCode",
                 function(object, pos,value) {
                     setChunk(object@chunkList, pos) <- value
                     object
                 })

if (is.null(getGeneric("evalEnv")))
    setGeneric("evalEnv", function(object)
               standardGeneric("evalEnv"))
setMethod("evalEnv", "vignetteCode", function(object)
          object@evalEnv)

if (is.null(getGeneric("numChunks")))
    setGeneric("numChunks", function(object)
               standardGeneric("numChunks"))
setMethod("numChunks","vignetteCode", function(object)
          numChunks(object@chunkList))
if (is.null(getGeneric("getChunk")))
    setGeneric("getChunk", function(object, num)
               standardGeneric("getChunk"))
setMethod("getChunk","vignetteCode", function(object, num)
          getChunk(object@chunkList, num))

if (is.null(getGeneric("evalChunk")))
    setGeneric("evalChunk", function(object, ...)
               standardGeneric("evalChunk"))
setMethod("evalChunk","vignetteCode", function(object, pos) {
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
    paste(output,"\n",sep="")
})

setMethod("summary","vignetteCode", function(object) {
    summary(object@chunkList)
})

setMethod("show","vignetteCode", function(object) {
    show(object@chunkList)
})


.initDynDocMethods <- function(where) {
        setGeneric("package", function(object)
                   standardGeneric("package"), where=where)

    setMethod("package", "vignetteCode", function(object)
              object@package, where=where)

    setMethod("package", "Vignette", function(object)
              object@package, where=where)
}
