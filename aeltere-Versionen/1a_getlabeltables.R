# Mit Dank an Sjewo:
get.labeltables <- function(dat) {
        varnames <- setNames(names(dat), names(dat))
        lapply(varnames, function(varname) get.label(dat, get.label.name(dat, varname)))
}