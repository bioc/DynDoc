    setClass("DynDoc", representation(indexEntry="character",
                                        title="character",
                                        path="character",
                                        pdfPath="character",
                                        depends="character",
                                        requires="character",
                                        suggests="character",
                                        keywords="character",
                                        codeChunks="chunkList"
                                        ))

    ####
    #### Simple Accessors
    ####

    if (is.null(getGeneric("indexEntry")))
        setGeneric("indexEntry", function(object)
                   standardGeneric("indexEntry"))
    setMethod("indexEntry", "DynDoc", function(object)
              object@indexEntry)

    if (is.null(getGeneric("path")))
        setGeneric("path", function(object)
                   standardGeneric("path"))
    setMethod("path", "DynDoc", function(object)
              object@path)

    if (is.null(getGeneric("pdfPath")))
        setGeneric("pdfPath", function(object)
                   standardGeneric("pdfPath"))
    setMethod("pdfPath", "DynDoc", function(object)
              object@pdfPath)

    if (is.null(getGeneric("getDepends")))
        setGeneric("getDepends", function(object)
                   standardGeneric("getDepends"))
    setMethod("getDepends", "DynDoc", function(object)
              object@depends)

    if (is.null(getGeneric("getRequires")))
        setGeneric("getRequires", function(object)
                   standardGeneric("getRequires"))
    setMethod("getRequires", "DynDoc", function(object)
              object@requires)

    if (is.null(getGeneric("getSuggests")))
        setGeneric("getSuggests", function(object)
                   standardGeneric("getSuggests"))
    setMethod("getSuggests", "DynDoc", function(object)
              object@suggests)

    if (is.null(getGeneric("getKeywords")))
        setGeneric("getKeywords", function(object)
                   standardGeneric("getKeywords"))
    setMethod("getKeywords", "DynDoc", function(object)
              object@keywords)

    ###
    ### Chunk manipulation
    ###

    if (is.null(getGeneric("codeChunks")))
        setGeneric("codeChunks", function(object)
                   standardGeneric("codeChunks"))
    setMethod("codeChunks", "DynDoc", function(object)
              object@codeChunks)

    if (is.null(getGeneric("chunks")))
        setGeneric("chunks", function(object)
                   standardGeneric("chunks"))
    setMethod("chunks", "DynDoc", function(object)
              chunks(object@codeChunks))

    if (is.null(getGeneric("setChunk<-")))
        setGeneric("setChunk<-", function(object, pos, value)
                   standardGeneric("setChunk<-"))
    setReplaceMethod("setChunk","DynDoc",
                     function(object, pos,value) {
                         setChunk(object@codeChunks,pos) <- value
                         object
                     })
    if (is.null(getGeneric("numChunks")))
        setGeneric("numChunks", function(object)
                   standardGeneric("numChunks"))
    setMethod("numChunks","DynDoc", function(object)
              numChunks(object@codeChunks))
    if (is.null(getGeneric("getChunk")))
        setGeneric("getChunk", function(object, num)
                   standardGeneric("getChunk"))
    setMethod("getChunk","DynDoc", function(object, num)
        getChunk(object@codeChunks, num))

    if (is.null(getGeneric("evalChunk")))
        setGeneric("evalChunk", function(object, ...)
                   standardGeneric("evalChunk"))
     setMethod("evalChunk","DynDoc",function(object,pos)
               evalChunk(object@codeChunks,pos))

    ###
    ### Output methods
    ###

    setMethod("summary","DynDoc", function(object) {
        summary(object@codeChunks)
    })

    setMethod("show","DynDoc", function(object) {
        show(object@codeChunks)
    })

