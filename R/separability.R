#' @title separability
#' @description Calculates variety of two-class sample separability metrics 
#'           
#' @param x         X vector
#' @param y         Y vector
#' @param plot      plot separability (TRUE/FALSE)
#' @param cols      colors for plot (must be equal to number of classes)
#' @param clabs     labels for two classes
#' @param ...       additional arguments passes to plot
#'
#' @return A data.frame with the following separability metrics: 
#' *  B - Bhattacharryya distance statistic 
#' * JM - Jeffries-Matusita distance statistic
#' *  M - M-Statistic
#' *  D - Divergence index
#' *  TD - Transformed Divergence index
#' @md
#'
#' @description
#' Available statistics:
#' * M-Statistic (Kaufman & Remer 1994) - This is a measure of the difference of the 
#'   distributional peaks. A large M-statistic indicates good separation between the 
#'   two classes as within-class variance is minimized and between-class variance 
#'   maximized (M <1 poor, M >1 good).
#'    
#' * Bhattacharyya distance (Bhattacharyya 1943; Harold 2003) - Measures the similarity
#'   of two discrete or continuous probability distributions.   
#'    
#' * Jeffries-Matusita distance (Bruzzone et al., 2005; Swain et al., 1971) - The J-M 
#'   distance is a function of separability that directly relates to the probability of 
#'   how good a resultant classification will be. The J-M distance is asymptotic to v2, 
#'   where values of v2 suggest complete separability
#'    
#' * Divergence and transformed Divergence (Du et al., 2004) - Maximum likelihood approach. 
#'   Transformed divergence gives an exponentially decreasing weight to increasing distances 
#'   between the classes.
#'
#' @md 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @references
#' Anderson, M. J., & Clements, A. (2000) Resolving environmental disputes: a  
#'   statistical method for choosing among competing cluster models. Ecological  
#'   Applications 10(5):1341-1355
#' @references
#' Bhattacharyya, A. (1943) On a measure of divergence between two statistical 
#'   populations defined by their probability distributions'. Bulletin of the 
#'   Calcutta Mathematical Society 35:99-109
#' @references
#' Bruzzone, L., F. Roli, S.B. Serpico (1995) An extension to multiclass cases of 
#'   the Jefferys-Matusita distance. IEEE Transactions on Pattern Analysis and  
#'   Machine Intelligence 33:1318-1321
#' @references
#' Du, H., C.I. Chang, H. Ren, F.M. D'Amico, J. O. Jensen, J., (2004) New 
#'   Hyperspectral Discrimination Measure for Spectral Characterization. Optical 
#'   Engineering 43(8):1777-1786.
#' @references
#' Kailath, T., (1967) The Divergence and Bhattacharyya measures in signal  
#'   selection. IEEE Transactions on Communication Theory 15:52-60  
#' @references
#' Kaufman Y., and L. Remer (1994) Detection of forests using mid-IR reflectance: 
#'   An application for aerosol studies. IEEE T. Geosci.Remote. 32(3):672-683.
#' 
#' @examples 
#'    norm1 <- dnorm(seq(-20,20,length=5000),mean=0,sd=1) 
#'    norm2 <- dnorm(seq(-20,20,length=5000),mean=0.2,sd=2)                          
#'      separability(norm1, norm2) 
#'            
#'    s1 <- c (1362,1411,1457,1735,1621,1621,1791,1863,1863,1838)
#'    s2 <- c (1362,1411,1457,10030,1621,1621,1791,1863,1863,1838)
#'      separability(s1, s2, plot=TRUE) 
#'       
#' @export                            
separability <- function(x, y, plot = FALSE, cols = c("red", "blue"), clabs = c("Class1", "Class2"), ...) {
    if (length(cols) > 2) 
      stop("Too many colors")
    if (length(clabs) > 2) 
      stop("Too many class labels")
    trace.of.matrix <- function(SquareMatrix) {
      sum(diag(SquareMatrix))
    }
	
    x <- as.matrix(x)
    y <- as.matrix(y)
    
	mdif <- mean(x) - mean(y)
    p <- (stats::cov(x) + stats::cov(y))/2
    bh.distance <- 0.125 * t(mdif) * p^(-1) * mdif + 0.5 * log(det(p)/sqrt(det(stats::cov(x)) * det(stats::cov(y))))
    
	m <- (abs(mean(x) - mean(y)))/(stats::sd(x) + stats::sd(y))
    jm.distance <- 2 * (1 - exp(-bh.distance))
    
	dt1 <- 1/2 * trace.of.matrix((stats::cov(x) - stats::cov(y)) * (stats::cov(y)^(-1) - stats::cov(x)^(-1)))
    dt2 <- 1/2 * trace.of.matrix((stats::cov(x)^(-1) + stats::cov(y)^(-1)) * 
	                            (mean(x) - mean(y)) * t(mean(x) - mean(y)))
    divergence <- dt1 + dt2
    transformed.divergence <- 2 * (1 - exp(-(divergence/8)))
    
	if (plot == TRUE) {
        color1 <- as.vector(grDevices::col2rgb(cols[1])/255)
        color2 <- as.vector(grDevices::col2rgb(cols[2])/255)
        d1 <- stats::density(x)
        d2 <- stats::density(y)
        graphics::plot(d1, type = "n", ylim = c(min(c(d1$y, d2$y)), max(c(d1$y, d2$y))),  
		               xlim = c(min(c(d1$x, d2$x)), max(c(d1$x, d2$x))), ...)
        graphics::polygon(d1, col = grDevices::rgb(color1[1], color1[2], color1[3], 1/4))
        graphics::polygon(d2, col = grDevices::rgb(color2[1], color2[2], color2[3], 1/4))
        graphics::abline(v = mean(x), lty = 1, col = "black")
        graphics::abline(v = mean(y), lty = 2, col = "black")
        graphics::legend("topright", legend = clabs, fill = c(grDevices::rgb(color1[1], color1[2], color1[3], 1/4), 
		                 grDevices::rgb(color2[1], color2[2], color2[3], 1/4)))
    }
    return(data.frame(B = bh.distance, JM = jm.distance, M = m, mdif = abs(mdif), 
	                  D = divergence, TD = transformed.divergence))
} 
