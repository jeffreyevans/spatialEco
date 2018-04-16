#' @title Terrain attributes
#' @description Calculates common terrain attributes slope, aspect, profile and planform curvatures
#'
#' @param x             A raster layer class object
#' @param derivative    Type of derivative c("slope", "aspect", "plan.curvature", "prof.curvature")   
#' @param method        Method for calculating derivative c("zevenbergen", "evans", "shary","moore")
#' @param radians      (FALSE/TRUE) transform slope or aspect to radian units
#' @param edge         (FALSE/TRUE) Perform edge correction (results in raster one cell smaller than original)
#' @param ...           Additional arguments passed to focal (for writing rasters to disk)
#' 
#' @return A raster layer class object of the specified derivative
#'
#' @notes
#' Cell (vector) bracket indexing: z1=m[1], z2=m[2], z3=m[3], z4=m[4], z5=m[5] z6=m[6], z7=m[7], z8=m[8], z9=m[9] where; m represents a vector n=9 representing the values of a 3x3 window  
#' In the Zevenbergen & Thorne (1987) method a Lagrange polynomial is fitted to the topographic surface
#' In the Evans (1980) method a quadratic surface is fitted to a 3x3 window.
#' Moore et al. (1993) and Shary (1995), have similarties to both other methods. For more info refer to Florinsky (1998)
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#' 
#' @references
#' Evans, I.S. (1980). An Integrated System of Terrain Analysis and Slope Mapping. Zeitschrift für Geomorphologie, Suppl. Bd. 36, 274-295.
#' Florinsky, I.V. (1998). Accuracy of Local Topographic Variables Derived from Digital Elevation Models. International Journal of Geographical Information Science, 12(1):47-62.
#' Moore, I.D., P.E. Gessler, G.A. Nielsen, & G.A. Paterson (1993). Soil Attribute Prediction Using Terrain Analysis. Soil Science Society of America Journal, 57:443-452.
#' Shary, P.A. (1991). The Second Derivative Topographic Method. In: The Geometry of the Earth Surface Structures, edited by Stepanov, I. N., 15-29 (in Russian).
#' Wilson, J.P., & J.C. Gallant (2000). Terrain Analysis - Principles and Applications. Wiley.
#' Zevenbergen, L.W. & C.R. Thorne (1987). Quantitative Analysis of Land Surface Topography. Earth Surface Processes and Landforms, 12:47-56.
#'
#' @examples
#'   library(raster)
#'   library(spatialEco)
#'     data(elev)
#'     elev <- projectRaster(elev, crs="+proj=robin +datum=WGS84", res=1000,
#'                           method='bilinear')
#'   ( slp <- terrain.attributes(elev, radians = TRUE) )
#'   ( slp <- terrain.attributes(elev, radians = TRUE, edge = TRUE) )
#'   ( plan.crv <- terrain.attributes(elev, derivative = "plan.curvature") )
#'
#' @export terrain.attributes
terrain.attributes <- function(x, derivative = c("slope", "aspect", "plan.curvature", "profile.curvature"),
                               method = c("zevenbergen", "evans", "shary", "moore"),
							   radians = FALSE, edge = FALSE, ...){ 							  
        if(class(x) != "RasterLayer") stop("x must be a raster object")
		  if( length(derivative) > 1) derivative <- derivative[1] 
	        cat("Calculating", derivative, "\n")
		if(radians == TRUE) {
          if(derivative != "slope" & derivative != "aspect") {
            message("Radians transformation not applied to curvature derivatives")		  
		  }	
        }		  
    flst <- list(method = method[1], derivative = derivative, res = raster::res(x)[1])	
    terrain.method <- function(m, method = unlist(flst[1]), derivative = unlist(flst[2]), 
	                           res = unlist(flst[3]) ) {
      if( method == "evans") {	
        #### Evans (1980)
          r <- (m[1]+m[3]+m[4]+m[6]+m[7]+m[9]-(2*(m[2]+m[5]+m[8])))/(3*(res^2))
            tx <- (m[1]+m[2]+m[3]+m[7]+m[8]+m[9]-(2*(m[4]+m[5]+m[6])))/(3*(res^2))
              s <- (m[3]+m[7]-m[1]-m[9])/(4*(res^2))
            p <- (m[3]+m[6]+m[9]-m[1]-m[4]-m[7])/(6*res)
          q <- (m[1]+m[2]+m[3]-m[7]-m[8]-m[9])/(6*res) 
        } else if(method == "shary") {
        # Shary (1995)  
          r=(m[1]+m[3]+m[7]+m[9]+3*(m[4]+m[6])-2*(m[2]+3*m[5]+m[8]))/(5*(res^2))
            tx=(m[1]+m[3]+m[7]+m[9]+3*(m[2]+m[8])-2*(m[4]+3*m[5]+m[6]))/(5*(res^2))
              s=(m[3]+m[7]-m[1]-m[9])/(4*(res^2))
            p=(m[3]+m[6]+m[9]-m[1]-m[4]-m[7])/(6*res)
          q=(m[1]+m[2]+m[3]-m[7]-m[8]-m[9])/(6*res)
        } else if(method == "zevenbergen") {
        # Zevenbergen and Thorne (1987)
          p=(m[6]-m[4])/(2*res)
            q=(m[2]-m[8])/(2*res)
              r=(m[4]+m[6]-2*m[5])/(2*(res^2))
            s=(m[3]+m[7]-m[1]-m[9])/(4*(res^2))
          tx=(m[2]+m[8]-2*m[5])/(2*(res^2))
        } else if(method == "moore") {
        # Moore et al. (1993) 
          p=(m[6]-m[4])/(2*res)
            q=(m[2]-m[8])/(2*res)
              r=(m[4]+m[6]-2*m[5])/(res^2)
            s=(m[3]+m[7]-m[1]-m[9])/(4*(res^2))
          tx=(m[2]+m[8]-2*m[5])/(res^2)
        } else { stop("not a valid method") }
		terrain <- round(c(atan(sqrt(p^2+q^2)),
    	             180-atan2(q,p)+90*(p/abs(p)),
    	             -(q^2*r-2*p*q*s+p^2*tx)/((p^2+q^2)*sqrt(1+p^2+q^2)),
    	             -(p^2*r+2*p*q*s+q^2*tx)/((p^2+q^2)*sqrt(1+p^2+q^2)^3)),6)
		  names(terrain) <- c("slope","aspect","plan.curvature","profile.curvature")
		if(radians == TRUE) {
          if(derivative != "slope" | derivative != "aspect")		  
		  terrain[1:2] <- (terrain[1:2] / 0.572957795786) * 0.01 
		}
      return( as.numeric(terrain[which(names(terrain) %in%  derivative)]) )
    } 
	if(edge == TRUE) {
      e <- as.vector(extent(x))
      return( raster::crop(raster::focal(x, w=matrix(1,nrow=3,ncol=3), 
                           fun = terrain.method, pad=TRUE, padValue=0),
                           y=c(e[1] + raster::res(x)[1], e[2] - raster::res(x)[1],
						   e[3] + raster::res(x)[1], e[4] - raster::res(x)[1]), ...) )
    } else {
      return( raster::focal(x, w=matrix(1,nrow=3,ncol=3), fun = terrain.method, 
	                        pad = TRUE, padValue = 0, ...) )
    }
}
