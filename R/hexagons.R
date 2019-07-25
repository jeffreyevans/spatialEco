#' @title Hexagons
#' @description Create hexagon polygons
#'                                                                                                                                                                              
#' @param x sp SpatialDataFrame class object
#' @param res Area of resulting hexagons
#' @param ... Additional arguments passed to spsample
#' 
#' @return SpatialPolygonsDataFrame OBJECT
#'
#' @note depends: sp 
#'                                                                  
#' @examples 
#' require(sp)
#'   data(meuse)
#'     coordinates(meuse) <- ~x+y 
#' 
#' hex.polys <- hexagons(meuse, res=100)   
#'   plot(hex.polys)
#'     plot(meuse,pch=20,add=TRUE)
#' 
#' # Points intersecting hexagons  
#' hex.pts <- na.omit(over(meuse,hex.polys))
#' (hex.pts <- data.frame(PTID=rownames(hex.pts), hex.pts)) 
#'
#' @export      
hexagons <- function(x, res = 100, ...) {
    # if(class(x) == "sf") { x <- as(x, "Spatial") }
    if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
        stop("x MUST BE AN sp SpatialDataFrame OBJECT")
    pts <- sp::spsample(x, type = "hexagonal", cellsize = res, ...)
    genPolyList <- function(hexGrid, dx) {
        if (missing(dx)) 
            dx <- 2 * min(diff(sort(unique(hexGrid$x))))
        dy <- dx/sqrt(3)
        x.offset <- c(-dx/2, 0, dx/2, dx/2, 0, -dx/2, -dx/2)
        y.offset <- c(dy/2, dy, dy/2, -dy/2, -dy, -dy/2, dy/2)
        f <- function(i) list(x = hexGrid$x[i] + x.offset, y = hexGrid$y[i] + y.offset)
        ret <- lapply(1:length(hexGrid$x), f)
    }
    ret <- genPolyList(data.frame(sp::coordinates(pts)), dx = res)
    npoly <- length(ret)
    Srl <- vector(mode = "list", length = npoly)
    IDS <- 1:npoly
    for (i in 1:npoly) Srl[[i]] <- sp::Polygons(list(sp::Polygon(ret[[i]])), IDS[i])
    res <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(Srl), data = data.frame(HEXID = IDS))
    sp::proj4string(res) <- sp::CRS(sp::proj4string(x))
    return(res)
} 
