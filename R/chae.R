#' @title Canine-Human Age Equivalent
#' @description Calculates canines equivalent human age (for fun) 
#'
#' @param x  numeric vector, dog age
#' @return numeric vector, equivalent human age  
#'
#' @references
#' Wang, T., J. M, A.N. Hogan, S. Fong, K. Licon et al.  (2020) quantitative 
#'   translation of dog-to-human aging by conserved remodeling of epigenetic 
#'   networks
#'     
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' dat <- data.frame(DogAge = seq(0,18,0.25),
#'              HumanAge=chae(seq(0,18,0.25)))[-1,]
#' 
#' plot(dat$DogAge, dat$HumanAge, "l",
#'      main="Canine-Human Age Equivalence",
#' 	 ylab="Human Age", xlab="Dog Age")
#'   points( 12, chae(12), col="red", pch=19, cex=1.5)
#'   points( 7, chae(7), col="blue", pch=19, cex=1.5)
#'   points( 0.5, chae(0.5), col="black", pch=19, cex=1.5)
#' legend("bottomright", legend=c("Camas (12-YO)", "Kele (7-YO)", "Aster (0.5-YO)"), 
#'       pch=c(19,19,19), cex=c(1.5,1.5,1.5), 
#' 	     col=c("red","blue","black"))  
#' 
#' @export  
chae <- function(x) { round(16 * log(x) + 31, 2) }
