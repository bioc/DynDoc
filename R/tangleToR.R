tangleToR <- function() {
        list(setup = tangleToRSetup,
         runcode = tangleToRRuncode,
         writedoc = RtangleWritedoc,
         finish = tangleToRFinish,
         checkopts = RweaveLatexOptions)
}

tangleToRSetup <- function(syntax, ...) {
    options <- list(engine="R")
    list(options=options, syntax=syntax, chunkout=list())
}

tangleToRRuncode <- function(object, chunk, options) {
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }
    outList <- object$chunkList

    if (is.null(outList))
        curList <- list()
    else
        curList <- chunks(outList)

    if (is.null(options$label))
        name <- character()
    else
        name <- options$label
    curChunk <- new("codeChunk", chunkName=name, chunk=chunk,
                    options=new("SweaveOptions",options=options))
    curList[[length(curList)+1]] <- curChunk
    newList <- new("chunkList", chunks=curList, evalEnv=new.env())
    object$chunkList <- newList
    return(object)
}

tangleToRFinish <- function(object, error=FALSE)
{
    return(object$chunkList)
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

