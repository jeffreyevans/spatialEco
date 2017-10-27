#' @title Concordance test for binomial models
#' @description Performs a concordance/disconcordance (C-statistic) test on binomial models.  
#'
#' @param y vector of binomial response variable used in model
#' @param p estimated probabilities from fit binomial model 
#'
#' @return A list class object with the following components: 
#' @return  concordance  percent of positives that are greater than probabilities of nulls
#' @return  isconcordance inverse of concordance representing null class
#' @return  tied number of tied probabilities
#' @return  pairs number of pairs compared
#'
#' @note
#' The C-statistic has been show to be comparable to the area under an ROC.
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references 
#' Austin, P.C. & E.W. Steyerberg (2012) Interpreting the concordance statistic of a logistic regression model: relation to the variance and odds ratio of a continuous explanatory variable. BMC Medical Research Methodology, 12:82 
#' Harrell, F.E. (2001) Regression modelling strategies. Springer, New York, NY. 
#' Royston, P. & D.G. Altman (2010) Visualizing and assessing discrimination in the logistic regression model. Statistics in Medicine 29(24):2508-2520
#'
#' @note 
#' Test of binomial regression for the hypothesis that probabilities of all positives [1], are greater than the probabilities of the nulls [0]. 
#' The concordance would be 100% for a perfect model where, disconcordance is the inverse of concordance, representing the null
#'
#' @examples
#' data(mtcars)
#' dat <- subset(mtcars, select=c(mpg, am, vs))
#' glm.reg <- glm(vs ~ mpg, data = dat, family = binomial)
#' concordance(dat$vs, predict(glm.reg, type = "response")) 
#' 
#' @export
concordance <- function (y, p){
  if(is.factor(y)) y <- as.numeric(as.character(y)) 
    fit <- data.frame(response = y,  p = p)
      ones <- fit[fit$response == 1, ] 
      zeros <- fit[fit$response == 0, ] 
      totalPairs <- nrow (ones) * nrow (zeros) 
    conc <- sum(c(vapply (ones$p, function(x) {((x > zeros$p))}, 
                FUN.VALUE=logical(nrow(zeros)))))
    return(list("concordance" = (conc / totalPairs), 
          "discordance" = ((totalPairs - conc) / totalPairs),
          "tied" = (1 - (conc / totalPairs) - ((totalPairs - conc) / totalPairs)), 
		  "pairs" = totalPairs))
}
