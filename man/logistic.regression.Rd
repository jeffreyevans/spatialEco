% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic.regression.R
\name{logistic.regression}
\alias{logistic.regression}
\title{Logistic and Auto-logistic regression}
\usage{
logistic.regression(
  ldata,
  y,
  x,
  penalty = TRUE,
  autologistic = FALSE,
  coords = NULL,
  bw = NULL,
  type = "inverse",
  style = "W",
  longlat = FALSE,
  ...
)
}
\arguments{
\item{ldata}{data.frame object containing variables}

\item{y}{Dependent variable (y) in ldata}

\item{x}{Independent variable(s) (x) in ldata}

\item{penalty}{Apply regression penalty (TRUE/FALSE)}

\item{autologistic}{Add auto-logistic term (TRUE/FALSE)}

\item{coords}{Geographic coordinates for auto-logistic model matrix
or sp object.}

\item{bw}{Distance bandwidth to calculate spatial lags (if empty neighbors
result, need to increase bandwidth). If not provided it will be
calculated automatically based on the minimum distance that includes
at least one neighbor.}

\item{type}{Neighbor weighting scheme (see autocov_dist)}

\item{style}{Type of neighbor matrix (Wij), default is mean of neighbors}

\item{longlat}{Are coordinates (coords) in geographic, lat/long (TRUE/FALSE)}

\item{...}{Additional arguments passed to lrm}
}
\value{
A list class object with the following components:
\itemize{
\item model - lrm model object (rms class)
\item bandwidth - If AutoCov = TRUE returns the distance bandwidth used for the
auto-covariance function
\item diagTable - data.frame of regression diagnostics
\item coefTable - data.frame of regression coefficients (log-odds)
\item Residuals - data.frame of residuals and standardized residuals
\item AutoCov - If an auto-logistic model, AutoCov represents lagged
auto-covariance term
}
}
\description{
Performs a logistic (binomial) or auto-logistic
(spatially lagged binomial) regression using maximum
likelihood or penalized maximum likelihood estimation.
}
\details{
It should be noted that the auto-logistic model (Besag 1972) is intended for
exploratory analysis of spatial effects. Auto-logistic are know to underestimate
the effect of environmental variables and tend to be unreliable (Dormann 2007).
Wij matrix options under style argument - B is the basic binary coding, W is row
standardized (sums over all links to n), C is globally standardized (sums over
all links to n), U is equal to C divided by the number of neighbours (sums over
all links to unity) and S is variance-stabilizing. Spatially lagged y defined as:
W(y)ij=sumj_(Wij yj)/ sumj_(Wij) where; Wij=1/Euclidean(i,j)
If the object passed to the function is an sp class there is no need to call the data
slot directly via "object@data", just pass the object name.
}
\examples{
 p = c("sf", "sp", "spdep", "rms")
 if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
   m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
   message("Can't run examples, please install ", paste(p[m], collapse = " "))
 } else {
   invisible(lapply(p, require, character.only=TRUE))
 
 data(meuse, package = "sp")
 meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
                   agr = "constant")
   meuse$DepVar <- rbinom(nrow(meuse), 1, 0.5)
 
 #### Logistic model
 lmodel <- logistic.regression(meuse, y='DepVar', 
                   x=c('dist','cadmium','copper')) 
   lmodel$model
     lmodel$diagTable
       lmodel$coefTable
 
 #### Logistic model with factorial variable
 lmodel <- logistic.regression(meuse, y='DepVar', 
             x=c('dist','cadmium','copper', 'soil')) 
   lmodel$model
     lmodel$diagTable
       lmodel$coefTable
 
  ### Auto-logistic model using 'autocov_dist' in 'spdep' package
  lmodel <- logistic.regression(meuse, y='DepVar', 
              x=c('dist','cadmium','copper'), autologistic=TRUE, 
              coords=st_coordinates(meuse), bw=5000) 
    lmodel$model
      lmodel$diagTable
        lmodel$coefTable
    est <- predict(lmodel$model, type='fitted.ind')
  
  #### Add residuals, standardized residuals and estimated probabilities
  VarNames <- rownames(lmodel$model$var)[-1]
    meuse$AutoCov <- lmodel$AutoCov 
    meuse$Residual <- lmodel$Residuals[,1]
    meuse$StdResid <- lmodel$Residuals[,2]
    meuse$Probs <- predict(lmodel$model, 
                          sf::st_drop_geometry(meuse[,VarNames]),
 	                     type='fitted')  
  
 #### Plot fit and probabilities
 
 resid(lmodel$model, "partial", pl="loess") 
 
 # plot residuals
 resid(lmodel$model, "partial", pl=TRUE) 
  
 # global test of goodness of fit 
 resid(lmodel$model, "gof")
  
 # Approx. leave-out linear predictors
 lp1 <- resid(lmodel$model, "lp1")            
  
 # Approx leave-out-1 deviance            
 -2 * sum(meuse$DepVar * lp1 + log(1-plogis(lp1)))
  
 # plot estimated probabilities at points
 plot(meuse['Probs'], pch=20)
 
}
}
\references{
Besag, J.E., (1972) Nearest-neighbour systems and the auto-logistic model for binary
data. Journal of the Royal Statistical Society, Series B Methodological 34:75-83

Dormann, C.F., (2007) Assessing the validity of autologistic regression. Ecological
Modelling 207:234-242

Le Cessie, S., Van Houwelingen, J.C., (1992) Ridge estimators in logistic regression.
Applied Statistics 41:191-201

Shao, J., (1993) Linear model selection by cross-validation. JASA 88:486-494
}
\seealso{
\code{\link[rms]{lrm}}

\code{\link[spdep]{autocov_dist}}
}
\author{
Jeffrey S. Evans  \href{mailto:jeffrey_evans@tnc.org}{jeffrey_evans@tnc.org}
}
