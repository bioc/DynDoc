.First.lib <- function(libname,pkgname,where) {
    library(tools)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initDynDocMethods(where)
}
