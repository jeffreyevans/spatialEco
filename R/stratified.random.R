#' @title Stratified random sample
#' @description Creates a stratified random sample of an sp class object
#'
#' @param x            An sf class object 
#' @param strata       Column in x with stratification factor
#' @param n            Number of random samples
#' @param reps         Number of replicates per strata
#' @param replace      (TRUE/FALSE) Sampling with replacement 
#'
#' @return An sf class object containing random samples
#'
#' @note
#' If replace=FALSE features are removed from consideration in subsequent replicates.
#' Conversely, if replace=TRUE, a feature can be selected multiple times across 
#' replicates. Not applicable if rep=1.
#'
#' @note Depends: sf
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'  
#' @references
#' Hudak, A.T., N.L. Crookston, J.S. Evans, M.J. Falkowski, A.M.S. Smith, P. Gessler 
#'   and P. Morgan. (2006) Regression modelling and mapping of coniferous forest basal 
#'   area and tree density from discrete-return lidar and multispectral satellite data. 
#'   Canadian Journal of Remote Sensing 32: 126-138.
#'
#' @examples 
#' library(sf)
#'  if(require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                     agr = "constant")
#' 
#' # Create stratified variable using quartile breaks
#' x1 <- cut(meuse$cadmium, summary(meuse$cadmium)[-4], 
#'           include.lowest=TRUE)
#'   levels(x1) <- seq(1,nlevels(x1),1)
#' x2 <- cut(meuse$lead, summary(meuse$lead)[-4], 
#'           include.lowest=TRUE)
#'   levels(x2) <- seq(1,nlevels(x2),1) 
#' meuse$STRAT <- paste(x1, x2, sep='.')
#'
#' # Counts for each full strata (note; 2 strata have only 1 observation)
#' tapply(meuse$STRAT, meuse$STRAT, length)
#'    
#' # 2 replicates, 2 samples with replacement
#' ssample <- stratified.random(meuse, strata='STRAT', n=2, reps=2, 
#'                              replace=TRUE)
#'   tapply(ssample$STRAT, ssample$STRAT, length)
#' 
#' # 2 replicates, 2 samples no replacement
#' ssample.nr <- stratified.random(meuse, strata='STRAT', n=2, reps=2)
#'   tapply(ssample.nr$STRAT, ssample.nr$STRAT, length)
#' 
#' # n=1 and reps=10 for sequential numbering of samples 
#' ssample.ct <- stratified.random(meuse, strata='STRAT', n=1, reps=10, 
#'                                 replace=TRUE)
#'   tapply(ssample.ct$STRAT, ssample.ct$STRAT, length)
#' 
#' # Plot random samples colored by replacement
#' ssample$REP <- factor(ssample$REP)
#'   plot(ssample['REP'], pch=20)
#' } 
#' @export stratified.random
stratified.random <- function(x, strata, n = 10, reps = 1, replace = FALSE) {
  gtypes = c("POLYGON", "POINT", "LINESTRING", "MULTIPOLYGON", 
             "MULTIPOINT", "MULTILINESTRING")			 
  if(missing(x)) 
    stop("Must provide an input spatial object (x)")
  if(!inherits(x, c("sf", "sfc")))
    stop(deparse(substitute(x)), " must be an sf object or coercible")	  
  if(any(unique(as.character(sf::st_geometry_type(x))) == gtypes[4:6]))
    stop("Function does not support multi-part geometry")  
  if(!any(unique(as.character(sf::st_geometry_type(x))) != gtypes[1:3]))
    stop(deparse(substitute(x)), " must be one of ", 
	     paste(gtypes[1:3], collopse=""))		     
  spx <- sf::st_drop_geometry(x)
    if(!inherits(spx[,strata], "factor")) 
      spx[,strata] <- factor(spx[,strata])  
    if (nlevels(spx[,strata]) < 2) 
      stop("Not enough levels to stratify by (n<2)")
    spx$REP <- NA
    results <- list()	
      for(j in levels(spx[, strata])) {
        d <- spx[spx[,strata] == j,]
    	  d$rowname <- rownames(d)
          if(nrow(d) > n) {  	
            for (i in 1:reps) {	
              s <- lapply(1, function(ij) {
      	             d[sample(1:nrow(d), n),]})
      	        s[[1]]$REP <- i
             results[[paste(j,i,sep="_")]] <-s[[1]] 
            }
          } else {
    	    d$REP <- 1
    		results[[paste(j,i,sep="_")]] <- d
    	  }
      }
    results <- do.call(rbind, results)
	  if(!replace){
	    didx <- which(duplicated(results$rowname))
	    if(length(didx) > 0) results <- results[-didx,]
	  }
	  results <- stats::na.omit(results[,c("rowname","REP")])	
    results <- merge(x, results, by.y="rowname", by.x = 'row.names', 
	                 all.x = FALSE, all.y = TRUE)
  return(results)
} 
