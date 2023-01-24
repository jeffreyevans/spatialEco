#' @title Raster Downscale using adaptive regression splines
#' @description Downscales a raster to a higher resolution raster multivariate adaptive 
#'              regression splines (MARS).
#' 
#' @param x            A terra SpatRaster object representing independent variable(s) 
#' @param y            A terra SpatRaster object representing dependent variable
#' @param add.coords   (FALSE/TRUE) Add spatial coordinates to model  
#' @param keep.model   (FALSE/TRUE) Keep MARS model (earth class object)
#' @param grid.search  (FALSE/TRUE) perform a hyper-parameter grid se
#' @param plot         (FALSE/TRUE) Plot results
#' @param ...          Additional arguments passed to earth 
#'
#' @return A list object containing:  
#' * downscale         Downscaled terra SpatRaster object
#' * GCV Generalized   Cross Validation (GCV)
#' * GRSq              Estimate of the predictive power 
#' * RSS               Residual sum-of-squares (RSS) 
#' * RSq               R-square
#' * model             earth MARS model object (if keep.model = TRUE) 
#' @md
#'
#' @note
#' This function uses Multivariate Adaptive Regression Splines, to downscale a raster based 
#' on higher-resolution or more detailed raster data specified as covariate(s). This is similar
#' to the raster.downsample function which uses a robust regression and is a frequentest model for
#' fitting linear asymptotic relationships whereas, this approach is for fitting nonparametric
#' functions and should be used when the distributional relationship are complex/nonlinear.
#' Using add.coords adds spatial coordinates to the model, including creating the associated 
#' rasters for prediction.      
#'
#' @references
#' Friedman (1991) Multivariate Adaptive Regression Splines (with discussion) 
#'   Annals of Statistics 19(1):1–141
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' \dontrun{
#' library(geodata)
#' library(terra)
#' 
#' # Download example data (requires geodata package)
#'   elev <- geodata::elevation_30s(country="SWZ",  path=tempdir())
#'     slp <- terrain(elev, v="slope")
#' 	  x <- c(elev,slp)
#'         names(x) <- c("elev","slope")
#'   tmax <- geodata::worldclim_country(country="SWZ", var="tmax", 
#'                                      path=tempdir())
#'     tmax <- crop(tmax[[1]], ext(elev))
#' 	  names(tmax) <- "tmax"
#' 	  
#' tmax.ds <- aspline.downscale(x, tmax, add.coords=TRUE, keep.model=TRUE)
#'   plot(tmax.ds$model) 
#' 
#'   # plot prediction and parameters	
#'   opar <- par(no.readonly=TRUE)
#'     par(mfrow=c(2,2))
#'       plot(tmax, main="Original Temp max")
#'       plot(x[[1]], main="elevation")
#'       plot(x[[2]], main="slope")
#'       plot(tmax.ds$downscale, main="Downscaled Temp max")
#'   par(opar)
#'  
#' }
#' @export
aspline.downscale <- function(x, y, add.coords = TRUE, keep.model = FALSE, 
                              grid.search = FALSE, plot = FALSE, ...) {
    if (!inherits(x, "SpatRaster")) 
	  stop(deparse(substitute(x)), " must be a terra SpatRaster object")	
    if (!inherits(y, "SpatRaster")) 
	  stop(deparse(substitute(y)), " must be a terra SpatRaster object")
    if (terra::nlyr(y) > 1)  {
      warning("Dependent variable must be a single response, 
	    only the first raster will be used")	
          y <- y[[1]]
    }
    if(grid.search) {
      if(!any(which(utils::installed.packages()[,1] %in% "caret")))
        stop("please install caret package to implement grid search")
    }	
  sub.samp.sp <- terra::as.points(x, na.rm=TRUE)
  if(add.coords == TRUE) {
    sub.samp.sp <- cbind(sub.samp.sp, terra::crds(sub.samp.sp, df=TRUE))
      sub.samp <- data.frame(terra::extract(y, sub.samp.sp), sub.samp.sp)
      sub.samp <- sub.samp[,-which(names(sub.samp) %in% "ID")]
    names(sub.samp) <- c("y", names(x), "xcoord", "ycoord")
  } else {
    sub.samp <- data.frame(terra::extract(y, sub.samp.sp), sub.samp.sp)
      sub.samp <- sub.samp[,-which(names(sub.samp) %in% "ID")]
    names(sub.samp) <- c("y", names(x))
  }  
  na.idx <- unique(which(is.na(sub.samp), arr.ind = TRUE)[,1])
    if(length(na.idx) > 0)
  	  sub.samp <- sub.samp[-na.idx,]
  if(add.coords == TRUE) {
    xcoord <- x[[1]] 
      ycoord <- x[[1]]
        xcoord[] <- xFromCell(xcoord, 1:terra::ncell(xcoord))
          ycoord[] <- yFromCell(ycoord, 1:terra::ncell(ycoord))
    x <- c(x, xcoord, ycoord)
      names(x)[(nlyr(x)-1):nlyr(x)] <- c("xcoord", "ycoord")
  } 
    if(grid.search) {
      if(!any(which(utils::installed.packages()[,1] %in% "caret")))
        stop("please install caret package to implement grid search")
      hyper_grid <- expand.grid(
      degree = 1:3, 
      nprune = floor(seq(2, 100, length.out = 10))
      )
	  message("Running hyper parameter grid search with ", nrow(hyper_grid), 
	          " parameter sets, this may take awhile")
      tuned_mars <- caret::train(
        x <- subset(sub.samp, select = -y),
        y <- sub.samp$y,
        method = "earth",
        metric = "RMSE",
        trControl = caret::trainControl(method = "cv", number = 10),
        tuneGrid = hyper_grid
      )
      ds.fit <- tuned_mars$finalModel
    } else {
      ds.fit <- earth::earth( y ~ ., data = sub.samp, ...)  
    }
  parms <- earth::evimp(ds.fit) 
    p <- terra::predict(x[[which(names(x) %in% rownames(parms))]], ds.fit)
    results <- list(downscale = p, 
                    GCV = ds.fit$gcv, 
                    RSS = ds.fit$rss, 
                    GRSq = ds.fit$grsq,
                    RSq = ds.fit$rsq,
					imp = parms)
    if(keep.model) {
      results$model <- ds.fit 
    }
    if(plot) plot(ds.fit)
  return(results)
}
