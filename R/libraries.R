#' @title libraries
#' @description Checks, installs and optionally adds libraries to current environment  
#'
#' @param x               Quoted package name(s)
#' @param add             (FALSE/TRUE) Add package(s) to current R environment using require
#' @param install         (TRUE/FALSE) If missing from library, install package
#' @param check.source    (TRUE/FALSE) Check for recent source compile
#' @param repository      Use specific repository.
#' @param lib             A local R library path, default is the .Library global    
#'
#' @return installed package(s) optionally added to environment
#'
#' @note
#' This function will check if a package is installed in the R library and 
#' optionally install the package and add it to the active environment using require.
#' A check on the package availability in the defined mirror is also performed. This 
#' can be used to manage package installs but, also to merely check the install and add 
#  a vector of defined packages to the namespace environment using require.   
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' \dontrun{
#' # Install packages, add to namespace environment and suppress source check
#' libraries( c("sp","spdep"), add=TRUE, check.source=FALSE)
#' 
#' # Check package(s) install and add to namespace environment but, 
#' #   do not install if missing
#' libraries( c("sp","spdep","blah"), install=FALSE, add=TRUE)
#' } 
#' 
#' @export
libraries <- function(x, add = FALSE, install = TRUE, check.source = TRUE,
                      repository = NULL, lib = NULL) {
	if(is.null(repository)){
      if(getOption("repos")["CRAN"] == "@CRAN@")
        options(repos="https://cloud.r-project.org/") 
	} else {
	  mirrors <- utils::read.csv(file.path(R.home(), "/doc/CRAN_mirrors.csv"))$URL
	    if(!repository %in% mirrors)
		  stop("Not a valid repository mirror")
	  options(repos=repository)  
    }		  
	if(check.source) {
	  options(install.packages.check.source = "yes")
    } else {
	  options(install.packages.check.source = "no")
    }
	ap <- utils::available.packages()
      pmiss <- x[which(!x %in% rownames(ap))]
    if(length(pmiss) > 0) { 
	  cat("\n\n", "The following packages are not available on this mirror:", "\n",  
                paste(pmiss, collapse=", "), "\n\n")    
	    xprint <- x[-which(x %in% pmiss)]
	} else {
	  xprint <- x
	}
    if (is.null(lib)) {
      lib <- .libPaths()[1L]
      if(length(.libPaths()) > 1L) 
        message(sprintf(ngettext(length(x), "Installing package into %s\n(as %s is unspecified)", 
                "Installing packages into %s\n(as %s is unspecified)"), 
                sQuote(lib), sQuote("lib")), domain = NA)
    }
    ok <- dir.exists(lib) & (file.access(lib, 2) == 0L)
    if (length(lib) > 1 && any(!ok)) 
        stop(sprintf(ngettext(sum(!ok), "'lib' element %s is not a writable directory", 
            "'lib' elements %s are not writable directories"), 
            paste(sQuote(lib[!ok]), collapse = ", ")), 
            domain = NA)
    if (length(lib) == 1L && .Platform$OS.type == "windows") {
        ok <- dir.exists(lib)
        if (ok) {
            fn <- file.path(lib, paste0("_test_dir_", Sys.getpid()))
            unlink(fn, recursive = TRUE)
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) 
                ok <- FALSE
            else unlink(fn, recursive = TRUE)
        }
    }
    if (length(lib) == 1L && !ok) {
      warning(gettextf("'lib = \"%s\"' is not writable", lib), 
		      domain = NA, immediate. = TRUE)
      userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep))[1L]
    }
  lib.path <- normalizePath(lib)
  pkg <- as.data.frame(utils::installed.packages())
  for(i in 1:length(x)) {
    if(!x[i] %in% pkg$Package) { 
      cat(x[i], " is not installed","\n\n")
      if(install) {
        cat("    Installing", x[i], "to", file.path(R.home(), "library"), "\n\n")	  
	      try( utils::install.packages(x[i], lib = lib.path) )
		if(add) require(x[i], quietly = TRUE, character.only = TRUE)
      }	  
	} else {
	  cat(x[i], as.character(pkg[which(pkg$Package %in% x[i]),]$Version), "is installed", "\n\n")
	    if(add) require(x[i], quietly = TRUE, character.only = TRUE) 
    }
  }
  if(add) cat("\n\n", "These packages have been added to the current environment: ", "\n",  
              paste(xprint, collapse=", "), "\n\n")   
}
