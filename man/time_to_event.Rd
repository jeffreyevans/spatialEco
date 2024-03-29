% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_to_event.R
\name{time_to_event}
\alias{time_to_event}
\title{Time to event}
\usage{
time_to_event(
  x,
  y = 1,
  dir = c("LR", "RL"),
  int = FALSE,
  up.to = FALSE,
  na.action = c("fail", "ignore")
)
}
\arguments{
\item{x}{A vector, representing time-series, to evaluate}

\item{y}{Threshold value tor return position for}

\item{dir}{Direction of evaluation c("LR", "RL")}

\item{int}{FALSE | TRUE - Evaluate as integer (rounds to 0 decimal places)}

\item{up.to}{FALSE | TRUE - Return value before event}

\item{na.action}{c("fail", "ignore"), if "fail" function will return error with NA's
with "ignore" NA values will be included in count to event}
}
\value{
A vector value representing the time to event
}
\description{
Returns the time (sum to position) to a specified value
}
\details{
The time to event represents the sum of positions, in the vector,
until the specified value is found ie., (0,0,1) would be 3 or, 
2 with up.to=TRUE. The int argument allows for rounding a continuous  
variable. Since it may be difficult to find an exact match to a floating 
point value rounding mitigates the problem. If you want a specific rounding 
value (eg., 1 decimal place) you can apply it to x first then pass it to 
the function. The up.to argument will stop one value before the specified value 
of (y) regardless of integer or float. For NA handling, na.action defines the
function behavior, causing it to fail or count NAs. Note that it makes no
sense to actually remove NAs as it will make the run uninterpretable.
}
\examples{
# Binomial instance
time_to_event(c(0,0,0,0,1,0,0,0,1,0))
time_to_event(c(0,0,0,0,1,0,0,0,1,0), up.to = TRUE)
time_to_event(c(0,0,0,0,1,0,0,0,1,0), dir="RL")
time_to_event(c(NA,0,0,0,1,0,0,0,1,0), na.action="ignore")

# Continuous threshold instance
( x <- runif(100, 0,7) ) 
time_to_event(x, y = 5, int=TRUE)

# raster example
library(terra)

# Binomial instance
r <- do.call(c, replicate(20,terra::rast(matrix(sample(
             c(0,1), 1000, replace=TRUE), 100, 100))))             
  ( t2e <- app(r, fun=time_to_event) )

# Continuous threshold instance
r <- do.call(c, replicate(20,terra::rast(matrix(
              runif(1000,0,7), 100, 100))))
  ( t2e <- app(r, fun=time_to_event, y=5) )

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
