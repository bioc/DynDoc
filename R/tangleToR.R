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
