#' @title spatialEco news
#' @description Displays release notes
#' @param ...  not used
#' @return Shows package NEWS file
#' @export
spatialEcoNews <- function(...) {
    newsfile <- file.path(system.file(package="spatialEco"), "NEWS")
    file.show(newsfile)
}
