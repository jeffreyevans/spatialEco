#' @title Package startup message
#' @description Displays package version and news file 
#' @export
.onAttach <- function(libname, pkgname) {
  se.ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                     fields="Version")
  packageStartupMessage(paste(pkgname, se.ver))
  packageStartupMessage("Type se.news() to see new features/changes/bug fixes.")
}
