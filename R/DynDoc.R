.initDynDocClasses <- function(where) {
    .initSweaveOptions(where)
    .initCodeChunk(where)
    .initChunkList(where)
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

    if (is.null(getGeneric("depends")))
        setGeneric("depends", function(object)
                   standardGeneric("depends"), where=where)
    setMethod("depends", "DynDoc", function(object)
              object@depends, where=where)

    if (is.null(getGeneric("requires")))
        setGeneric("requires", function(object)
                   standardGeneric("requires"), where=where)
    setMethod("requires", "DynDoc", function(object)
              object@requires, where=where)

    if (is.null(getGeneric("suggests")))
        setGeneric("suggests", function(object)
                   standardGeneric("suggests"), where=where)
    setMethod("suggests", "DynDoc", function(object)
              object@suggests, where=where)

    if (is.null(getGeneric("keywords")))
        setGeneric("keywords", function(object)
                   standardGeneric("keywords"), where=where)
    setMethod("keywords", "DynDoc", function(object)
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
                         setChunk(object@codeChunks, pos) <- value
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
