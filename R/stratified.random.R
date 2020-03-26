#' @title Stratified random sample
#' @description Creates a stratified random sample of an sp class object
#'
#' @param x  sp class SpatialDataFrame object (point, polygon, line, pixel)
#' @param strata  Column in @@data slot with stratification factor
#' @param n Number of random samples
#' @param reps Number of replicates per strata
#' @param replace Sampling with replacement (TRUE|FALSE)
#'
#' @return sp SpatialDataFrame object (same as input feature) containing random samples
#'
#' @note
#' If replace=FALSE features are removed from consideration in subsequent replicates.
#' Conversely, if replace=TRUE, a feature can be selected multiple times across 
#' replicates. Not applicable if rep=1.
#'
#' @note Depends: sp
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
#' require(sp)
#'   data(meuse)
#'     coordinates(meuse) <- ~x+y
#' 
#' # Create stratified variable using quartile breaks
#' x1 <- cut(meuse@@data[,'cadmium'], summary(meuse@@data[,'cadmium'])[-4], 
#'           include.lowest=TRUE)
#'   levels(x1) <- seq(1,nlevels(x1),1)
#' x2 <- cut(meuse@@data[,'lead'], summary(meuse@@data[,'lead'])[-4], 
#'           include.lowest=TRUE)
#'   levels(x2) <- seq(1,nlevels(x2),1) 
#' meuse@@data <- cbind(meuse@@data, STRAT=paste(x1, x2, sep='.') ) 
#'    
#' # 2 replicates and replacement
#' ssample <- stratified.random(meuse, strata='STRAT', n=2, reps=2)
#' 
#' # 2 replicates and no replacement
#' ssample.nr <- stratified.random(meuse, strata='STRAT', n=2, reps=2, 
#'                                 replace=FALSE)
#' 
#' # n=1 and reps=10 for sequential numbering of samples 
#' ssample.ct <- stratified.random(meuse, strata='STRAT', n=1, reps=10, 
#'                                 replace=TRUE)
#' 
#' # Counts for each full strata (note; 2 strata have only 1 observation)
#' tapply(meuse@@data$STRAT, meuse@@data$STRAT, length)
#' 
#' # Counts for each sampled strata, with replacement
#' tapply(ssample@@data$STRAT, ssample@@data$STRAT, length)
#' 
#' # Counts for each sampled strata, without replacement
#' tapply(ssample.nr@@data$STRAT, ssample.nr@@data$STRAT, length)
#' 
#' # Counts for each sampled strata, without replacement
#' tapply(ssample.ct@@data$STRAT, ssample.ct@@data$STRAT, length)
#' 
#' # Plot random samples colored by replacement
#' ssample@@data$REP <- factor(ssample@@data$REP)
#'   spplot(ssample, 'REP', col.regions=c('red','blue'))
#'
#' @export
stratified.random <- function(x, strata, n = 10, reps = 1, replace = TRUE) {
    if (!methods::is(x, "Spatial")) 
      stop(deparse(substitute(x)), " Must be an sp class spatial object")
    spx <- x
    if(!inherits(spx@data[,strata], "factor")) spx@data[,strata] <- factor(spx@data[,strata]) 
    if (nlevels(spx@data[,strata]) < 2) 
        stop("NOT ENOUGH LEVELS TO STRATIFY BY")
    spx@data <- cbind(spx@data, REP = 0)
    results <- spx[1, ]
    for (i in 1:reps) {
      spx@data[, strata] <- factor(spx@data[, strata])
      for (j in 1:nlevels(spx@data[, strata])) {
          f <- levels(spx@data[, strata])[j]
          if (is.na(match(f, levels(spx@data[, strata]))) == FALSE) {
            if ((dim(spx[spx@data[, strata] == f, ])[1] > 0) == TRUE) {
              ssub <- spx[spx@data[, strata] == f, ]
              if ((dim(ssub)[1] >= n) == TRUE) {
                s <- ssub[sample(dim(ssub)[1], size = n), ]
                s@data$REP <- i
                results <- rbind(results, s)
                if (replace == FALSE) {
                  rinx <- which(rownames(spx@data) %in% rownames(s@data))
                  if (length(rinx) > 0) 
                    spx <- spx[-rinx, ]
                }
              } else {
                s <- ssub
                s@data$REP <- i
                results <- rbind(results, s)
                if (replace == FALSE) {
                  rinx <- which(rownames(spx@data) %in% rownames(s@data))
                  if (length(rinx) > 0) 
                    spx <- spx[-rinx, ]
                }
              }
            }
          }
        }
      }
    results <- results[-1, ]
  return(results)
} 
