#' @title Collinearity test
#' @description Test for linear or nonlinear collinearity/correlation in data 
#'
#' @param x            A data.frame or matrix containing continuous data     
#' @param p            The correlation cutoff (default is 0.85)
#' @param nonlinear    A boolean flag for calculating nonlinear correlations 
#'                     (FALSE/TRUE)
#' @param p.value      If nonlinear is TRUE, the p value to accept as the 
#'                     significance of the correlation
#' 
#' @return Messages and a vector of correlated variables 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'
#' @note 
#' Evaluation of the pairwise linear correlated variables to remove is accomplished  
#' through calculating the mean correlations of each variable and selecting the 
#' variable with higher mean. 
#'
#' @note
#' Nonlinear correlations assume the model form:
#'     \eqn{E(Y_i | X_i) = \alpha + f(X_i) + \varepsilon_i}
#' With the hypothesis: 
#'     \eqn{H_{0} : f(x) = 0, \ \forall x$$} 
#' 
#' @examples
#' 
#' data(cor.data)
#'
#' # Evaluate linear correlations on linear data
#' head( dat <- cor.data[[4]] ) 
#' pairs(dat, pch=20)
#'   ( cor.vars <- collinear( dat ) )
#' 
#' # Remove identified variable(s)
#' head( dat[,-which(names(dat) %in% cor.vars)] )
#'
#' # Evaluate linear correlations on nonlinear data
#' #   using nonlinear correlation function
#' plot(cor.data[[1]], pch=20) 
#'   collinear(cor.data[[1]], p=0.80, nonlinear = TRUE ) 		       
#'
#' @export collinear
collinear <- function (x, p = 0.85, nonlinear = FALSE, p.value = 0.001) {
  if(!class(x) == "matrix" && !class(x) == "data.frame")
    stop("x does not appear to be a matrix or data.frame object") 
  if(any(sapply(x, is.numeric) == FALSE)) 
    stop("x contains non-numeric data") 
    if(is.null(colnames(x))) { 
      colnames(x) <- paste0("X", 1:ncol(x))
    }	
  if(nonlinear == TRUE) {
    nlcor <- function(x, y, pv = 0.05) {
      g <- mgcv::gam(y ~ s(x))
      g.summ <- summary(g)
        if(g.summ$p.table[4] > pv) {
          nl.corr = 0
        } else {
          nl.corr = ( g$null.deviance - g$deviance ) / g$null.deviance  
        }
      return(nl.corr)
    }
    perm <- expand.grid(colnames(x), colnames(x))
      # perm <- perm[!duplicated(t(apply(perm, 1, sort))), ]
        perm <- perm[-which(perm[,1] == perm[,2]),]
	deletecol <- rep(FALSE,length(colnames(x)))
  	  names(deletecol) <- colnames(x)			
      for (i in 1:nrow(perm)) {
        x.name = paste0("^", as.vector(perm[,1])[i], "$")
		y.name = paste0("^", as.vector(perm[,2])[i], "$") 
		cat("evaluating", x.name, "and", y.name, "\n") 
        nl.cor <- nlcor(x[,grep(y.name, colnames(x))], 
		                x[,grep(x.name, colnames(x))], 
						pv = p.value )
		  if(nl.cor > p) {
            deletecol[grep(y.name, names(deletecol))] <- TRUE
            cat("Nonlinear correlation between ", x.name, "and ", y.name,
                 " = ", nl.cor, "\n")
            cat("   recommend dropping", y.name, "\n", "\n")
          } 
        }
    deletecol <- unique(names(deletecol[deletecol==TRUE]))
      if( length(deletecol) < 1 )
        message("No nonlinear correlations found")	 
  } else if(nonlinear == FALSE) {
    x <- stats::cor(x)  
      diag(x) <- 0
    if (!isTRUE(all.equal(x, t(x)))) 
      stop("correlation matrix is not symmetric")  	
    if (!any(x[!is.na(x)] > p))
      stop("\r All correlations are <=", p, "\n")
    if (dim(x)[1] < 2) 
      stop("There is only one variable")
        x2 <- abs(x)
          originalOrder <- 1:ncol(x2)
            averageCorr <- function(x) mean(x, na.rm = TRUE)
              tmp <- x2
                diag(tmp) <- NA
                  maxAbsCorOrder <- order(apply(tmp, 2, averageCorr), 
  				                        decreasing = TRUE)
                  x2 <- x2[maxAbsCorOrder, maxAbsCorOrder]
                newOrder <- originalOrder[maxAbsCorOrder]
              rm(tmp)
        diag(x2) <- NA
    combine.vars <- expand.grid(colnames(x), colnames(x))
      combine.vars <- combine.vars[!duplicated(t(apply(combine.vars, 1, sort))), ]
        combine.vars <- combine.vars[-which(combine.vars[,1] == combine.vars[,2]),]
	deletecol <- rep(FALSE,length(colnames(x2)))
  	  names(deletecol) <- colnames(x)		
      for (i in 1:nrow(x2)) {
        i.name = rownames(x2)[i]
        if( deletecol[grep(i.name, names(deletecol))][1] == TRUE ) {
          next
        }
        for(j in (1:ncol(x2))[-i]){
          idx.names = c(i.name,colnames(x2)[j])
          if( deletecol[grep(colnames(x2)[j], names(deletecol))][1] == TRUE ) {
            next
          }  	
            if(x2[i, j] > p) {
              rc1 <- mean(x2[i, ], na.rm = TRUE)
              cc2 <- mean(x2[-j, ], na.rm = TRUE)
      	      deletecol[grep(idx.names[which.max(c(rc1,cc2))], names(deletecol))] <- TRUE
                cat("Collinearity between ", idx.names[1], "and ", 
                  idx.names[2], "correlation = ", round(x2[i,j], 4), "\n")
                cat("  Correlation means: ", round(rc1, 3), "vs", round(cc2, 3), "\n")	
                cat("   recommend dropping", idx.names[which.max(c(rc1,cc2))], "\n", "\n")			
              } 
          } 
      }
	  deletecol <- unique(names(deletecol[deletecol==TRUE]))
    }
  return(deletecol)
}
