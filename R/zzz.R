.First.lib <- function(libname,pkgname,where) {
    library(tools)
    where <- match(paste("package:", pkgname, sep=""), search())
    .initDynDocMethods(where)
    if(.Platform$OS.type == "windows" && require("Biobase") && interactive()
        && .Platform$GUI ==  "Rgui"){
        addVigs2WinMenu("DynDoc")
    }
}
