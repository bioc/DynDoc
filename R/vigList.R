vignette <- function(package=.packages(all.available=TRUE),
                     libs, filter, vigDescFun=baseVigDesc,
                     vigPath="/doc") {
    sQuote <- function(s) paste("`", s, "'", sep = "")
    hasPackage <- FALSE

    if (missing(libs))
        libs <- .libPaths()

    if( !missing(package) )
        hasPackage <- TRUE

    ## Get list for packages
    paths <- .find.package(package, libs)
    paths <- unique(paths[file.exists(paths)])
    novigs <- !file.exists(file.path(paths, vigPath))
    if (any(novigs)) {
        if (hasPackage && (length(package) > 0)) {
            packagesWithNoVigs <- package[package %in% sapply(paths[novigs],
                basename)]
            if (length(packagesWithNoVigs) > 1) {
                warning(paste("packages", paste(sQuote(packagesWithNoVigs),
                  collapse = ", "), "contain no vignettes"))
            }
            else if (length(packagesWithNoVigs) == 1) {
                warning(paste("package", sQuote(packagesWithNoVigs),
                  "contains no vignettes"))
            }
        }
        paths <- paths[!novigs]
    }

    vigList <- lapply(paths, function(x) getPkgVigList(x, vigDescFun))
    vigList <- unlist(vigList, recursive=FALSE)
    if (! is.null(vigList))
        class(vigList) <- "pkgFileList"

    ##filter if the user has specified one
    if( !missing(filter) )
	    vigList <- filter(vigList)

    return(vigList)
}

getPkgVigList <- function(pkg,vigDescFun=baseVigDesc,
                          vigPath="/doc/",vigExt="\\.(Rnw|Snw|rnw|snw)$",
                          pkgVers=TRUE) {
    pkgVigList <- list()
    class(pkgVigList) <- "pkgFileList"

    fullVPath <- file.path(pkg,vigPath)
    vigs <- dir(fullVPath,pattern=vigExt)
    if (length(vigs) == 0)
        return(NULL)

    vigPaths <- file.path(fullVPath,vigs)
    ## Take out any broken vignettes
    vigPaths <- vigPaths[sapply(vigPaths,hasVignetteKeyword)]
    vigs <- unlist(lapply(vigPaths,basename))
    if (length(vigPaths) == 0)
        return(NULL)
    for (i in seq(along=vigPaths)) {
        pkgVigList[[i]] <- getVigInfo(vigPaths[i],pkg, vigDescFun,
                                      pkgVers=pkgVers)
    }
    names(pkgVigList) <- vigs
    return(pkgVigList)
}

getVigInfo <- function(vig,pkg=NULL, vigDescFun=baseVigDesc, pkgVers=TRUE) {
    ## Passed a filename, if that file is a vignette, will create
    ## a named list of lists to hold all the vignette metadata

    file <- readLines(con=vig)
    lines <- grep("^%[[:space:]]*\\\\Vignette",file)

    splitLines <- strsplit(file[lines],"{")

    listNames <- vector()
    listNames <- unlist(lapply(splitLines, getVigInfoNames, listNames))
    newLst <- lapply(splitLines,transformVigInfoLine)
    ## Remove any NAs from thinkgs like \VignetteXXX{}
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
        splitLine <- strsplit(file[line],"{")
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
    pdfFile <- gsub("\\.(Rnw|Snw|rnw|snw)$",".pdf",vig)
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
        writeLines(formatDL(x[[i]]$VignetteIndexEntry,
                            x[[i]]$VignetteTitle), outConn)
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

match.vigNames <- function(vigEntry, names) {
    ## !!! Doesnt work, implemented completely in .transformNames
    entry <- match(vigEntry$VignetteIndexEntry,names)
    if (is.na(entry)) {
        return(NULL)
    }
    else {
        names[entry] <- vigEntry$VigPath
        return(names)
    }
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

hasVignetteKeyword <- function(vig,kw="VignetteIndexEntry") {
    file <- readLines(con=vig)
    pattern <- paste("^[[:space:]]*%+[[:space:]]*\\\\",kw,sep="")
    lines <- grep(pattern,file)
    if (length(lines) > 0)
        return(TRUE)
    else
        return(FALSE)
}
