#' @title Autocorrelation Plot
#' @description Autocorrelation plot (Anselin 1996), following Chen (2015), 
#'              aka, Moran's-I plot (univariate or bivariate)
#' 
#' @param x               Vector of x response variables
#' @param y               Vector of y response variables
#' @param coords          A matrix of coordinates corresponding to [x,y]
#' @param type.ac         Type of autocorrelation plot ("xy", "yx") 
#' @param dist.function   ("inv.power", "neg.exponent") 
#' @param scale.xy        (TRUE/FALSE) scale the x,y vectors
#' @param scale.morans    (FALSE/TRUE) standardize the Moran's index to an expected [-1 to 1]? 
#' @param ...             Additional arguments passed to plot
#'
#' @return A plot of the scaled variable against its spatially lagged values. 
#'
#' @details
#' The argument "type" controls the plot for x influencing y (type="xy") or y influencing x (type="yx"). 
#' If y is not defined then the statistic is univariate and only the "xy" plot will be available 
#' The linear relationship between x and its spatial lag (Wx) is indicative of the spatial autoregressive process, 
#' underlying the spatial dependence. The statistic can be autocorrelation (univariate or cross-correlation (bivariate). 
#' The quadrants are the zero intercept for random autocorrelation and the red line represents the trend in autocorrelation. 
#' The quadrants in the plot indicate the type of spatial association/interaction (Anselin 1996). For example the 
#' upper-left quadrant represents negative associations of low values surrounded by high and the lower-right quadrant 
#' represents negative associations of high values surrounded by low.  
#'
#' @note
#' if y is not specified the univariate statistic for x is returned. the coords argument is only used if k = NULL. 
#' Can also be an sp object with relevant x,y coordinate slot (ie., points or polygons). If w = NULL, the default 
#' method for deriving spatial weights matrix, options are: inverse power or negative exponent. If scale.xy = FALSE 
#' it is assumed that they are already scaled following Chen (2015).         
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references Chen., Y. (2015) A New Methodology of Spatial Cross-Correlation Analysis. 
#'               PLoS One 10(5):e0126158. doi:10.1371/journal.pone.0126158
#' @references Anselin, L. (1996) The Moran scatterplot as an ESDA tool to assess local instability 
#'               in spatial association. pp. 111-125 in M. M. Fischer, H. J. Scholten and D. Unwin (eds) 
#'               Spatial analytical perspectives on GIS, London, Taylor and Francis
#' @references Anselin, L. (1995) Local indicators of spatial association, Geographical Analysis, 27:93-115
#'
#' @examples
#'  library(sp)
#'  library(spdep)
#'   data(meuse)
#'    coordinates(meuse) <- ~x+y  
#'
#'  # Autocorrelation (univariate)  
#'    morans.plot(meuse$zinc, coords = coordinates(meuse))   
#' 
#'  # Cross-correlation of: x influencing y and y influencing x
#'    par(mfrow=c(1,2)) 
#'      morans.plot(x=meuse$zinc, y=meuse$copper, coords = coordinates(meuse), 
#'                  scale.morans = TRUE)
#'      morans.plot(x=meuse$zinc, y=meuse$copper, coords = coordinates(meuse),
#'                  scale.morans = TRUE, type.ac="yx") 
#'                        
#' @export
morans.plot <- function(x, y = NULL, coords = NULL, 
                        type.ac = c("xy", "yx"), 
                        dist.function = "inv.power",   
                        scale.xy = TRUE, scale.morans = FALSE, ...) { 
	if(missing(x)) stop("x must be specified")
      if(is.null(y)) {
	    type.lab = paste0("Autocorrelation of ", deparse(substitute(x)))
	    y = x	
        type.ac = "xy"
		message("Calculating autocorrelation for x")
	  } else {
	  	type.lab = paste0("Cross-correlation of ", deparse(substitute(x)), " and ", deparse(substitute(y)))
		message("Calculating cross-correlation for [x,y]")	 
      }	 
    if(length(y) != length(x)) stop("[X,Y] are not equal")
      if( length(which(is.na(x))) != 0 | length(which(is.na(y))) != 0) 
	    stop("NA's not permitted in [X,Y]")	
    dots <- as.list(match.call(expand.dots = TRUE)[-1])
	if ( !is.null(dots[["x"]]) ) dots[["x"]] <- NULL
		if ( !is.null(dots[["y"]]) ) dots[["y"]] <- NULL
			if ( !is.null(dots[["coords"]]) ) dots[["coords"]] <- NULL
				if ( !is.null(dots[["type.ac"]]) ) dots[["type.ac"]] <- NULL
					if ( !is.null(dots[["dist.function"]]) ) dots[["dist.function"]] <- NULL
						if ( !is.null(dots[["scale.xy"]]) ) dots[["scale.xy"]] <- NULL
							if ( !is.null(dots[["scale.morans"]]) ) dots[["scale.morans"]] <- NULL
	if(scale.xy == FALSE) warning("It is assumed that [x,y] vectors are already scaled")
      type.ac = type.ac[1]			  
    if( scale.xy ){
      x.scale <- as.numeric(scale(x))	
	  y.scale <- as.numeric(scale(y))
    }	    
    if( is.null(coords) ) stop("A coordinates matrix is required")
      w <- sp::spDists( coords ) 
    if( dist.function == "inv.power" ) {
      message("Calculating spatial weights matrix using inverse power function")
	    w <- 1 / w
          diag(w) <- 0 
        w <- w / sum(w) 
    } else if (dist.function == "neg.exponent") { 
      message("Calculating spatial weights matrix using negative exponent")    
	  diag(w) <- NA 
	  mu <- mean(w, na.rm=TRUE)
      for(i in 1:nrow(w)) {
        for(j in 1:nrow(w)) {
		 w[i,j] <- round(exp( (-2 * w[i,j]) / mu ),6)
        }
      }  
      diag(w) <- 0
    } else {
      stop("Not a valid matrix option")
    }
	
	# standardizes variable -1 to 1
    scale.partial <- function(p) {
      d <- (max(p) + min(p)) / 2
      return((p - d) / (max(p) - d))
    }
  
  plot.ac <- function(x.plot,y.plot,dots) {	  
	   if (is.null(dots[["pch"]]) & "pch" %in% names(dots) == FALSE) dots[["pch"]] <- 20
       if (is.null(dots[["cex"]]) & "cex" %in% names(dots) == FALSE) dots[["cex"]] <- 1
       dots[["x"]] <- x.plot
	   dots[["y"]] <- y.plot
  	do.call("plot", dots)
    }
	   
    if(type.ac[1] == "xy") {	
      if (is.null(dots[["xlab"]]) & "xlab" %in% names(dots) == FALSE) dots[["xlab"]] <- deparse(substitute(x))
	  if (is.null(dots[["ylab"]]) & "ylab" %in% names(dots) == FALSE) dots[["ylab"]] <- "nWy,xx^TWy"
	  if (is.null(dots[["main"]]) & "main" %in% names(dots) == FALSE) dots[["main"]] <- type.lab
	    # gsci.xy <- as.numeric(x*w%*%y) 
        gsci.xy <- as.numeric(x.scale*y.scale%*%w) 
		    if(scale.morans) { 
			  gsci.xy <- scale.partial(gsci.xy)
			  slag <- as.numeric(x.scale*y.scale%*%w)
			    slag <- scale.partial(slag)
            } else {
              slag <- as.numeric(x.scale*y.scale%*%w)
            }			
	      plot.ac(x.scale,gsci.xy,dots)
	        graphics::abline(v=0, lty=3)
            graphics::abline(h=0, lty=3)  
	        graphics::abline(stats::lm( slag ~ x.scale ), col="red")
		   
    } else if (type.ac[1] == "yx") {
	  type.lab <- paste0("Cross-correlation of ", deparse(substitute(y)), " and ", deparse(substitute(x)))
	  if (is.null(dots[["xlab"]]) & "xlab" %in% names(dots) == FALSE) dots[["xlab"]] <- deparse(substitute(y))
	  if (is.null(dots[["ylab"]]) & "ylab" %in% names(dots) == FALSE) dots[["ylab"]] <- "nWx,yy^TWx"
	  if (is.null(dots[["main"]]) & "main" %in% names(dots) == FALSE) dots[["main"]] <- type.lab
	   #gsci.xy <- as.numeric(y*w%*%x) 
	    gsci.yx <- as.numeric(y.scale*x.scale%*%w)
		    if(scale.morans) { 
			  gsci.yx <- scale.partial(gsci.yx)
			  slag <- as.numeric(x.scale*y.scale%*%w)
			    slag <- scale.partial(slag)
            } else {
              slag <- as.numeric(y.scale*x.scale%*%w)
            }	
	      plot.ac(y.scale,gsci.yx,dots)
	        graphics::abline(v=0, lty=3)
            graphics::abline(h=0, lty=3) 
            graphics::abline(stats::lm( slag ~ y.scale ), col="red")			   
    }
} 
