.First.lib <- function(libname,pkgname,where) {
    require("Biobase", quietly=TRUE) || 
              stop("cannot load DynDoc without Biobase")
    where <- match(paste("package:", pkgname, sep=""), search())
     .initDynDocClasses(where)
     .initVignetteCode(where)
}
