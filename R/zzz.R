.First.lib <- function(libname,pkgname,where) {
    where <- match(paste("package:", pkgname, sep=""), search())
     require("Biobase")
     require("reposTools")
     .initDynDocClasses(where)
}
