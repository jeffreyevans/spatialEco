#' @title Logistic and Auto-logistic regression
#' @description Performs a logistic (binomial) or auto-logistic 
#'              (spatially lagged binomial) regression using maximum 
#'              likelihood or penalized maximum likelihood estimation. 
#'
#' @param ldata data.frame object containing variables
#' @param y Dependent variable (y) in ldata
#' @param x Independent variable(s) (x) in ldata  
#' @param penalty Apply regression penalty (TRUE/FALSE)
#' @param autologistic Add auto-logistic term (TRUE/FALSE)  
#' @param coords Geographic coordinates for auto-logistic model matrix 
#'               or sp object.
#' @param bw Distance bandwidth to calculate spatial lags (if empty neighbors 
#'           result, need to increase bandwidth). If not provided it will be 
#'           calculated automatically based on the minimum distance that includes 
#'           at least one neighbor.
#' @param type Neighbor weighting scheme (see autocov_dist)
#' @param style Type of neighbor matrix (Wij), default is mean of neighbors 
#' @param longlat Are coordinates (coords) in geographic, lat/long (TRUE/FALSE)
#' @param ... Additional arguments passed to lrm
#'
#' @return A list class object with the following components: 
#' * model - lrm model object (rms class)
#' * bandwidth - If AutoCov = TRUE returns the distance bandwidth used for the 
#'   auto-covariance function
#' * diagTable - data.frame of regression diagnostics
#' * coefTable - data.frame of regression coefficients
#' * Residuals - data.frame of residuals and standardized residuals
#' * AutoCov - If an auto-logistic model, AutoCov represents lagged 
#'             auto-covariance term
#' @md
#'
#' @description
#' It should be noted that the auto-logistic model (Besag 1972) is intended for 
#' exploratory analysis of spatial effects. Auto-logistic are know to underestimate 
#' the effect of environmental variables and tend to be unreliable (Dormann 2007).     
#' Wij matrix options under style argument - B is the basic binary coding, W is row 
#' standardized (sums over all links to n), C is globally standardized (sums over
#' all links to n), U is equal to C divided by the number of neighbours (sums over 
#' all links to unity) and S is variance-stabilizing. Spatially lagged y defined as:  
#' W(y)ij=sumj_(Wij yj)/ sumj_(Wij) where; Wij=1/Euclidean(i,j)
#' If the object passed to the function is an sp class there is no need to call the data 
#' slot directly via "object@data", just pass the object name.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Besag, J.E., (1972) Nearest-neighbour systems and the auto-logistic model for binary 
#'   data. Journal of the Royal Statistical Society, Series B Methodological 34:75-83
#' @references
#' Dormann, C.F., (2007) Assessing the validity of autologistic regression. Ecological 
#'   Modelling 207:234-242   
#' @references
#' Le Cessie, S., Van Houwelingen, J.C., (1992) Ridge estimators in logistic regression. 
#'   Applied Statistics 41:191-201
#' @references
#' Shao, J., (1993) Linear model selection by cross-validation. JASA 88:486-494
#'
#' @examples
#' require(sp)
#' require(spdep)
#' require(rms)                                                                       
#' data(meuse)
#'   coordinates(meuse) <- ~x+y  
#'     meuse@@data <- data.frame(DepVar=rbinom(dim(meuse)[1], 1, 0.5), 
#'                               meuse@@data)
#'
#' #### Logistic model
#' lmodel <- logistic.regression(meuse, y='DepVar', 
#'                   x=c('dist','cadmium','copper')) 
#'   lmodel$model
#'     lmodel$diagTable
#'       lmodel$coefTable
#'
#' #### Logistic model with factorial variable
#' lmodel <- logistic.regression(meuse, y='DepVar', 
#'             x=c('dist','cadmium','copper', 'soil')) 
#'   lmodel$model
#'     lmodel$diagTable
#'       lmodel$coefTable
#' 
#' ### Auto-logistic model using 'autocov_dist' in 'spdep' package
#' lmodel <- logistic.regression(meuse, y='DepVar', 
#'             x=c('dist','cadmium','copper'), autologistic=TRUE, 
#'             coords=coordinates(meuse), bw=5000) 
#'   lmodel$model
#'     lmodel$diagTable
#'       lmodel$coefTable
#'   est <- predict(lmodel$model, type='fitted.ind')
#' 
#' #### Add residuals, standardized residuals and estimated probabilities
#' VarNames <- rownames(lmodel$model$var)[-1]
#'   meuse@@data$AutoCov <- lmodel$AutoCov
#'     meuse@@data <- data.frame(meuse@@data, Residual=lmodel$Residuals[,1], 
#'                      StdResid=lmodel$Residuals[,2], Probs=predict(lmodel$model, 
#'                      meuse@@data[,VarNames],type='fitted') )  
#' 
#' #### Plot fit and probabilities
#' resid(lmodel$model, "partial", pl="loess") 
#' # plot residuals
#' resid(lmodel$model, "partial", pl=TRUE) 
#' 
#' # global test of goodness of fit 
#' resid(lmodel$model, "gof")
#' 
#' # Approx. leave-out linear predictors
#' lp1 <- resid(lmodel$model, "lp1")            
#' 
#' # Approx leave-out-1 deviance            
#' -2 * sum(meuse@@data$DepVar * lp1 + log(1-plogis(lp1)))
#' 
#' # plot estimated probabilities at points
#' spplot(meuse, c('Probs'))
#'
#'
#' @seealso \code{\link[rms]{lrm}}
#' @seealso \code{\link[spdep]{autocov_dist}}
#'
#' @export
logistic.regression <- function(ldata, y, x, penalty = TRUE, autologistic = FALSE, 
                                coords = NULL, bw = NULL, type = "inverse", style = "W",  
                                longlat = FALSE, ...) {
    if(!any(which(utils::installed.packages()[,1] %in% "rms")))
      stop("please install rms package before running this function")
								
	if(substr(class(ldata),1,7)  == "Spatial"  ) { ldata <- ldata@data }
      if (is.na(match(y, names(ldata)))) 
        stop("Dependent variable not present in data")
    xNames <- intersect(x, names(ldata))
    if (length(xNames) < length(x)) 
        stop("Mismatch in Independent Variable Names")
    if (autologistic == TRUE) {
        if (is.null(coords)) { 
            stop("Need coordinates")
	    } else {
		coords <- as.matrix(coords)	
		}
        if (is.null(bw)) {       
		  k.nn <- spdep::knn2nb(spdep::knearneigh(coords))
          bw <- max(unlist(spdep::nbdists(k.nn, coords)))	
		}	
        ldata$AutoCov <- spdep::autocov_dist(ldata[,y], xy = coords, nbs = bw, 
		                                     style = style, type = type)
        x <- append(x, "AutoCov")
    }
	form <- stats::as.formula(paste(y, paste(x, collapse = "+"), sep = "~"))
    fit <- rms::lrm(form, data = ldata, x = TRUE, y = TRUE, ...)
    bf <- rms::pentrace(fit, seq(0.2, 1, by = 0.05))
    if (penalty) {
        pen <- bf$penalty
    } else {
        pen <- 0
    }
    allPens <- bf$results.all[, 1]
    allAICs <- bf$results.all[, 3]
    for (i in 1:length(allPens)) {
        penValue <- allPens[i]
        if (penValue == pen) {
            aic <- allAICs[i]
        }
    }
    if (penalty) {
        fit <- stats::update(fit, penalty = bf$penalty)
    }
    res <- stats::residuals(fit)
    resSTD <- (res - mean(res))/sqrt(stats::var(res))
    allIndVars <- c("Intercept")
    allIndVars <- append(allIndVars, x)
    k <- length(fit$coefficients)
	d <- matrix(0, k, 4)
	d[, 1] <- fit$coefficients
    d[, 2] <- sqrt(diag(fit$var))
    d[, 3] <- d[, 1]/d[, 2]
    d[, 4] <- stats::pnorm(abs(d[, 3]), lower.tail = FALSE) * 2
    coefList <- list(Variable = allIndVars, Coef = d[, 1], StdError = d[, 2], Wald = d[, 3], Prob = d[, 4])
	coefList$Variable <- names(fit$coefficients)
    coefFrame <- data.frame(coefList)
    diagFrame <- data.frame(Names = c(names(fit$stats), "PEN", "AIC"), Value = c(as.vector(fit$stats), pen, aic))
    if (autologistic == TRUE) {
      return(list(model = fit, bandwidth=bw, diagTable = diagFrame, coefTable = coefFrame,  
             Residuals = data.frame(res = res, resSTD = resSTD), AutoCov = ldata$AutoCov))
    } else {
      return(list(model = fit, diagTable = diagFrame, coefTable = coefFrame, 
             Residuals = data.frame(res = res, resSTD = resSTD)))
    }
} 
