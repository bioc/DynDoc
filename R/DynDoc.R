.initDynDocClasses <- function(where) {
    .initSweaveOptions(where)
    .initCodeChunk(where)
    .initChunkList(where)
    .initDynDoc(where)
    .initVignette(where)
}


.initDynDoc <- function(where) {
    if (is.null(getGeneric("DynDoc")))
        setGeneric("DynDoc", function(object)
                   standardGeneric("DynDoc"), where=where)
    setClass("DynDoc", representation(indexEntry="character",
                                        title="character",
                                        path="character",
                                        pdfPath="character",
                                        depends="character",
                                        requires="character",
                                        suggests="character",
                                        keywords="character",
                                        codeChunks="chunkList"
                                        ),
             where=where)

    ####
    #### Simple Accessors
    ####

    if (is.null(getGeneric("indexEntry")))
        setGeneric("indexEntry", function(object)
                   standardGeneric("indexEntry"), where=where)
    setMethod("indexEntry", "DynDoc", function(object)
              object@indexEntry, where=where)

    if (is.null(getGeneric("path")))
        setGeneric("path", function(object)
                   standardGeneric("path"), where=where)
    setMethod("path", "DynDoc", function(object)
              object@path, where=where)

    if (is.null(getGeneric("pdfPath")))
        setGeneric("pdfPath", function(object)
                   standardGeneric("pdfPath"), where=where)
    setMethod("pdfPath", "DynDoc", function(object)
              object@pdfPath, where=where)

    if (is.null(getGeneric("getDepends")))
        setGeneric("getDepends", function(object)
                   standardGeneric("getDepends"), where=where)
    setMethod("getDepends", "DynDoc", function(object)
              object@depends, where=where)

    if (is.null(getGeneric("getRequires")))
        setGeneric("getRequires", function(object)
                   standardGeneric("getRequires"), where=where)
    setMethod("getRequires", "DynDoc", function(object)
              object@requires, where=where)

    if (is.null(getGeneric("getSuggests")))
        setGeneric("getSuggests", function(object)
                   standardGeneric("getSuggests"), where=where)
    setMethod("getSuggests", "DynDoc", function(object)
              object@suggests, where=where)

    if (is.null(getGeneric("getKeywords")))
        setGeneric("getKeywords", function(object)
                   standardGeneric("getKeywords"), where=where)
    setMethod("getKeywords", "DynDoc", function(object)
              object@keywords, where=where)

    ###
    ### Chunk manipulation
    ###

    if (is.null(getGeneric("codeChunks")))
        setGeneric("codeChunks", function(object)
                   standardGeneric("codeChunks"), where=where)
    setMethod("codeChunks", "DynDoc", function(object)
              object@codeChunks, where=where)

    if (is.null(getGeneric("chunks")))
        setGeneric("chunks", function(object)
                   standardGeneric("chunks"), where=where)
    setMethod("chunks", "DynDoc", function(object)
              chunks(object@codeChunks), where=where)

    if (is.null(getGeneric("setChunk<-")))
        setGeneric("setChunk<-", function(object, pos, value)
                   standardGeneric("setChunk<-"), where=where)
    setReplaceMethod("setChunk","DynDoc",
                     function(object, pos,value) {
                         setChunk(object@codeChunks,pos) <- value
                         object
                     }, where=where)
    if (is.null(getGeneric("numChunks")))
        setGeneric("numChunks", function(object)
                   standardGeneric("numChunks"), where=where)
    setMethod("numChunks","DynDoc", function(object)
              numChunks(object@codeChunks), where=where)
    if (is.null(getGeneric("getChunk")))
        setGeneric("getChunk", function(object, num)
                   standardGeneric("getChunk"), where=where)
    setMethod("getChunk","DynDoc", function(object, num)
        getChunk(object@codeChunks, num), where=where)

    if (is.null(getGeneric("evalChunk")))
        setGeneric("evalChunk", function(object, pos)
                   standardGeneric("evalChunk"), where=where)
     setMethod("evalChunk","DynDoc",function(object,pos)
               evalChunk(object@codeChunks,pos), where=where)

    ###
    ### Output methods
    ###

    setMethod("summary","DynDoc", function(object) {
        summary(object@codeChunks)
    }, where=where)

    setMethod("show","DynDoc", function(object) {
        show(object@codeChunks)
    }, where=where)

}
