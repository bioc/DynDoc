vignette <- function(..., list=character(0),
                     package=.packages(all.available=TRUE),
                     .lib.loc=.libPaths(), filter=baseVigFilter,
                     vigDescFun=baseVigDesc,
                     verbose=getOption("verbose"),
                     vigPath="/doc", character.only=FALSE) {
    sQuote <- function(s) paste("`", s, "'", sep = "")
    names <- c(as.character(substitute(list(...))[-1]), list)
    if (!missing(package)) {
        if( !character.only)
            if (is.name(y <- substitute(package)))
                package <- as.character(y)
    }
    ## Get list for packages
    paths <- .find.package(package, .lib.loc, verbose = verbose)
    if (is.null(.lib.loc))
        paths <- c(.path.package(package, TRUE), getwd(), paths)
    paths <- unique(paths[file.exists(paths)])
    novigs <- !file.exists(file.path(paths, vigPath))
    if (any(novigs)) {
        if (!missing(package) && (length(package) > 0)) {
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

    vigList <- list()
    for (i in seq(along=paths)) {
        vigList <- c(vigList,getPkgVigList(paths[i],vigDescFun))
    }
    class(vigList) <- "pkgFileList"
    vigList <- filter(vigList)


    if (length(names) == 0) {
        ## If no names are used, just getting a listing of what is
        ## available.
        return(vigList)
    }

    names <- .transformNames(names, vigList)
    .loadNamedVigs(names, paths, vigPath, verbose, sQuote)
}

.loadNamedVigs <- function(names, paths, vigPath, verbose, sQuote) {
    paths <- file.path(paths,vigPath)
    fsep <- .Platform$file.sep
    for (name in names) {
        if (is.na(name)) {
            print(paste("Vignette",name,"does not have a PDF version available."))
            next
        }
        files <- NULL
        for (j in seq(along=paths)) {
            files <- c(files, list.files(paths[j], full=TRUE))
        }
        files <- gsub("\/\/","\/",files)
        files <- files[grep(name, files)]
        found <- FALSE
        if (length(files) > 0) {
            subpre <- paste(".*", fsep, sep="")
            for (file in files) {
                if (verbose)
                    cat("name=", name, ":\t file= ...", fsep, sub(subpre,
                                            "", file), "::\t", sep = "")
                if (found)
                    break
                found <- TRUE
                if (sub(subpre, "", file) != basename(file))
                    found <- FALSE
                else {
                    ## Display file
                    openPDF(file)
                }
                if (verbose) {
                    cat(if (!found) "*NOT* ", "found\n")
                }
            }
        }
        if (!found)
            warning(paste("Vignette", sQuote(name), "not found"))
    }
    invisible(names)
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
        newLst$PDFpath <- NULL
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


baseVigFilter <- function(vigList) {
    return(vigList)
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
