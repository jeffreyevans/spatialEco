spatialEcoNews <- function() {
    newsfile <- file.path(system.file(package="spatialEco"), "NEWS")
    file.show(newsfile)
}
