#' @title Random raster
#' @description Create a random raster or raster stack using specified distribution
#'
#' @param r              Optional existing raster defining nrow/ncol                
#' @param n.row          Number of rows             
#' @param n.col          Number of columns
#' @param n.layers       Number of layers in resulting raster stack 
#' @param x              A vector of values to sample if distribution is "sample"
#' @param min            Minimum value of raster
#' @param max            Maximum value of raster
#' @param mean           Mean of centered distribution 
#' @param sd             Standard deviation of centered distribution
#' @param p              p-value for binominal distribution 
#' @param s              sigma value for Gaussian distribution
#' @param distribution   Available distributions, c("random", "normal", "seq", "binominal", "gaussian", "sample")

#' @return RasterLayer or RasterStack object with random rasters
#'
#' @details Options for distributions are for random, normal, seq, binominal, gaussian and sample raster(s)
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(raster)
#' 
#' # Using existing raster to create random binominal  
#' r <- raster(system.file("external/rlogo.grd", package="raster")) 
#' r <- random.raster(r, distribution="binominal")
#' 
#' # default; random, nrows=50, ncols=50, nlayers=1
#' rr <- random.raster(n.layer=5)
#' 
#' # specified; binominal, nrows=20, ncols=20, nlayers=5
#' rr <- random.raster(n.layer=5, n.col=20, n.row=20,  
#'                     distribution="binominal")
#' 
#' # specified; gaussian, nrows=50, ncols=50, nlayers=1
#' rr <- random.raster(n.col=50, n.row=50, s=8,  
#'                     distribution="gaussian")
#'
#' # specified; sample, nrows=50, ncols=50, nlayers=1
#' rr <- random.raster(n.layer=1, x=c(2,6,10,15), distribution="sample" )
#'   freq(rr)
#'
#' @export random.raster 
random.raster <- function(r=NULL, n.row = 50, n.col = 50, n.layers = 1, x = seq(1,10), 
                          min = 0, max = 1, mean = 0, sd = 1, p = 0.5, s = 1.5, 
						  distribution = c("random", "normal", "seq", 
						                   "binominal", "gaussian")){
d <- distribution[1]
  if(!is.null(r)) {											
    if( any(class(r) == c("RasterLayer", "RasterStack", "RasterBrick"))) {  
      i = raster::nrow(r)
      j = raster::ncol(r)
    }
  } else {
    i = n.col
	j = n.row
	if(missing(min)) min = 0
	  if(missing(max)) max = 1
	  if(missing(sd)) sd = 1
	if(missing(mean)) mean = 0
  }
  if(d == "gaussian") {
    if( i != j ) {  
	  stop("rows/columns must be equal for Gaussian symmetrical kernel")
    }
  }
  if(missing(p)) p = 0.5
  n = i*j 
  dist.fun <- function(.n, .min = min, .max = max, .mean = mean, 
                       .x = x, .sd = sd, .p = p, .s = s, .d = d) { 
    if( d == "seq") {
      v <- 1:.n  
    } else if(d == "normal") {
      v <- stats::rnorm(stats::runif(.n, min=.min, max=.max),
                        mean=.mean, sd=.sd)
    } else if(d == "random") {
      v <- stats::runif(.n, min=.min, max=.max)			 
    } else if(d == "binominal") {
      v <- stats::rbinom(.n, 1, .p)
    } else if(d == "sample") {
      v <- sample(x, .n, replace=TRUE)	  
    } else {
      stop("Not an available distribution")
    }
	return(v)
  } 
  message(paste0("Calculating ", d, " distribution with ", 
          i," x ", j," x ", n.layers, " dimensions"))
  if(d == "gaussian") {		  
    return( do.call(raster::stack, replicate(n.layers, raster::raster( 
                    gaussian.kernel(sigma=s, n=max(c(i,j)))))) )		  
  } else {		  
    return( do.call(raster::stack, replicate(n.layers, 
	          raster::raster(matrix(dist.fun(.n=n), i, j)))) )
  }	
}
