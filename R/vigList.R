getPkgVigList <- function(pkg,vigDescFun=baseVigDesc,
                          vigPath="/doc/",vigExt="\\.(Rnw|Rtex|Snw|rnw|snw)$",
                          pkgVers=TRUE) {
    pkgVigList <- list()
    class(pkgVigList) <- "pkgFileList"

    fullVPath <- file.path(pkg,vigPath)
    vigs <- dir(fullVPath,pattern=vigExt)
    if (length(vigs) == 0)
        return(NULL)

    vigPaths <- file.path(fullVPath,vigs)
    ## Take out any broken vignettes
    vigPaths <- vigPaths[sapply(vigPaths,hasVigHeaderField)]
    vigs <- unlist(lapply(vigPaths,basename))
    if (length(vigPaths) == 0)
        return(NULL)
    for (i in seq(along=vigPaths)) {
        tmpVigEntry<- getVigInfo(vigPaths[i],pkg, vigDescFun,
                                 pkgVers=pkgVers)
        if (is.null(tmpVigEntry))
            stop("Vignette ",vigPaths[i]," returned NULL from ",
                 "getVigInfo().")
        else
            pkgVigList[[i]] <- tmpVigEntry
    }
    names(pkgVigList) <- vigs
    return(pkgVigList)
}

getVigInfo <- function(vig,pkg=NULL, vigDescFun=baseVigDesc, pkgVers=TRUE) {
    ## Passed a filename, if that file is a vignette, will create
    ## a named list of lists to hold all the vignette metadata

    file <- readLines(con=vig)
    lines <- grep("^%[[:space:]]*\\\\Vignette",file)

    ## Double check that we at least have the VignetteIndexEntry
    ## (this is probably being caught by the listFilesWithType
    ##  check, but just in case)
    if (length(lines) == 0)
        stop("File ", vig,
             " does not appear to be a vignette file, ",
             "no vignette metadata available.")

    splitLines <- strsplit(file[lines],"{", fixed=TRUE)

    listNames <- vector()
    listNames <- unlist(lapply(splitLines, getVigInfoNames, listNames))
    newLst <- lapply(splitLines,transformVigInfoLine)
    ## Remove any NAs from things like \VignetteXXX{}
    newLst <- lapply(newLst,function(x){if(is.na(x[1])) NULL else x})
    names(newLst) <- listNames

    ## Add in the path starting from the package base
    newLst$VigPath <- vig
    if (!("VignettePackage" %in% names(newLst))) {
        ## Add in the package name
        newLst$VignettePackage <- pkg
    }
    if ((pkgVers==TRUE)&&(!is.null(pkg))) {
        ## Add in package version
        desc <- read.dcf(paste(pkg,"DESCRIPTION",sep="/"))
        newLst$VignettePkgVersion <- desc[,"Version"]
    }
    newLst <- vigDescFun(newLst)

    ## Grab the title as well
    ## !! Should just be merged in with lines above, but need to
    ## figure out generic regexps for the string handling
    line <- grep("\\title{",file)
    if (length(line) > 0) {
        splitLine <- strsplit(file[line],"{", fixed=TRUE)
        tmpTitle <- transformVigInfoLine(splitLine[[1]])
        if ((is.null(tmpTitle))||(is.na(tmpTitle)))
            newLst$VignetteTitle <- "Untitled"
        else {
            ## Remove any latex
            tmpTitle <- gsub("\\\\\\w*[[:space:]]","",tmpTitle)
            newLst$VignetteTitle <- tmpTitle
        }
    }
    else {
        newLst$VignetteTitle <- "Untitled"
    }

    ## Determine if there is a PDF file for this vignette, if so get
    ## the filename
    pdfFile <- gsub("\\.(Rnw|Rtex|Snw|rnw|snw)$",".pdf",vig)
    if (file.exists(pdfFile)) {
        newLst$PDFpath <- pdfFile
    }
    else {
        newLst$PDFpath <- character()
    }

    return(newLst)
}

print.pkgFileList <- function(x,...) {
    outFile <- tempfile("RpkgFileList")
    outConn <- file(outFile, open="w")

    first <- TRUE
    out <- ""
    for (i in seq(along=x)) {
        if (out != x[[i]]$OutString) {
            out <- x[[i]]$OutString
            writeLines(paste(ifelse(first,"","\n\n"),out,sep=""),outConn)
        }
        VIE = ifelse(is.null(x[[i]]$VignetteIndexEntry), "",
                 x[[i]]$VignetteIndexEntry)
        VT  = ifelse(is.null(x[[i]]$VignetteTitle), "",
                 x[[i]]$VignetteTitle)

        z <- try(writeLines(formatDL(VIE, VT),outConn))

        if (is(z, "try-error")) {
            stop("Vignette ", x[[i]]$vigPath,
                 " appears to have a malformed VignetteIndexEntry",
                 " or VignetteTitle")
        }

        first <- FALSE
    }
    if (first) {
        close(outConn)
        unlink(outFile)
        writeLines("no listings found")
    }
    else {
        ## !!! Mimic footer block of print.packageIQR
        close(outConn)

        file.show(outFile, delete.file=TRUE)
    }
    invisible(x)
}

transformVigInfoLine <- function(el) {
    el <- gsub("}","",el[2])
    el <- unlist(strsplit(el,",[[:space:]]*"))
    if (length(el) == 0)
        el <- NA
    return(el)
}

.transformNames <- function(names, vigList) {
    ## passed a set of IndexEntries, will grab out of the
    ## list the set of vignette file paths and return that
    ## instead

    ## !! Trying to get match.vigNames to work on some sort
    ## of apply() butfor now loop
    for (i in seq(along=vigList)) {
        entry <- match(vigList[[i]]$VignetteIndexEntry,names)
        if (!is.na(entry)) {
            names[entry] <- vigList[[i]]$PDFpath
        }
    }
   return(gsub("\/\/","\/",names))
}

getVigInfoNames <- function(el,nmVec) {
    name <- gsub("%[[:space:]]*\\\\","",el[1])
    nmVec <- c(nmVec,name)
    return(nmVec)
}


baseVigDesc <- function(vigInfo) {
    ## Passed a vignette info list, returns a vigInfo list with
    ## an output string attached
    if (is.null(vigInfo$VignettePackage))
        desc <- "Vignettes:\n\n"
    else
        desc <- paste("Vignettes in package ",
                      basename(vigInfo$VignettePackage),
                      ":\n\n", sep="")
    vigInfo$OutString <- desc
    return(vigInfo)
}

hasVigHeaderField <- function(vig,field="VignetteIndexEntry") {
    ## A posisbly soon to be removed function that is now just a wrapper
    ## around getVignetteHeader and returns TRUE/FALSE based on
    ## the length of the return value

    ret <- getVignetteHeader(vig, field)
    if (length(ret) > 0)
        return(TRUE)
    else
        return(FALSE)
}

getVignetteHeader <- function(vig, field) {
    ## Will retrieve the metadata information stored for a particular
    ## keyword (or an empty vector if it does not exist).
    file <- readLines(con=vig)
    origPattern <- "^[[:space:]]*%+[[:space:]]*\\\\"
    if (!missing(field))
        pattern <- paste(origPattern,field,sep="")
    else
        pattern <- origPattern

    lines <- grep(pattern,file)

    if (length(lines) > 0) {
        ## Separate out the lines that are header and
        ## then remove the header symbol
        header <- file[lines]
        header <- gsub(origPattern,"",header)
        ## Remove the trailing '}'
        header <- gsub("}","",header)
        ## Now have format of 'tag{field'
        splitHeader <- strsplit(header,"{", fixed=TRUE)

        headerList <- vector(mode="list",length=length(lines))
        for (i in 1:length(lines)) {
            names(headerList)[i] <- splitHeader[[i]][1]
            headerList[[i]] <- splitHeader[[i]][2]
        }
        return(headerList)
    }
    else
        return(list())
}
