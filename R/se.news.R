#' @title spatialEco news
#' @description Displays release notes
#' @param ...  not used
#' @export
se.news <- function(...) {
    newsfile <- file.path(system.file(package="spatialEco"), "NEWS")
    file.show(newsfile)
}
