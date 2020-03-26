#' @title Remove extension
#' @description Removes file extension (and path) from string
#'
#' @param x A character vector representing a file with extension 
#'
#' @return 
#' The file name with extension and file path stripped off
#'
#' @examples 
#' rm.ext("C:/path/file.txt")
#'
#' @export rm.ext 
rm.ext <- function(x) {
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(x))
}
