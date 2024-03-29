% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterDistance.R
\name{rasterDistance}
\alias{rasterDistance}
\title{Raster Distance}
\usage{
rasterDistance(x, y, scale = FALSE)
}
\arguments{
\item{x}{A terra SpatRast or sf class object}

\item{y}{Value(s) in x to to calculate distance to}

\item{scale}{(FALSE/TRUE) Perform a row standardization on results}
}
\value{
A terra SpatRast raster representing distances
}
\description{
Calculates the Euclidean distance of a defined raster class and
             all the other cells in a taster
}
\details{
This replicates the terra distance function but uses the Arya & Mount
Approximate Near Neighbor (ANN) C++ library for calculating distances. Where this
results in a notable increase in performance it is not memory safe, needing to read
in the entire raster and does not use the GeographicLib (Karney, 2013) spheroid 
distance method for geographic data.
}
\examples{
\donttest{
library(sf)
library(terra)

# read, project and subset 10 polygons
nc <- suppressWarnings(st_cast(st_read(system.file("shape/nc.shp", 
         package="sf")), "POLYGON"))
  nc <- st_transform(nc, st_crs("ESRI:102008"))
    nc.sub <- nc[sample(1:nrow(nc),10),]

# create 1000m reference raster, rasterize subset polygons
ref <- rast(ext(nc), resolution=1000)
  rnc <- mask(rasterize(vect(nc.sub), field="CNTY_ID",
              ref, background=9999), vect(nc)) 
    crs(rnc) <- "ESRI:102008"  
  
# Calculate distance to class 1 in rnc raster, plot results
ids <- nc.sub$CNTY_ID 
rd <- rasterDistance(rnc, y=ids) 
  plot(rd)
    plot( st_geometry(nc.sub), add=TRUE)

}
}
\references{
Arya S., Mount D. M., Netanyahu N. S., Silverman R. and Wu A. Y (1998), An 
  optimal algorithm for approximate nearest neighbor searching, Journal of 
  the ACM, 45, 891-923.
}
\seealso{
\code{\link[terra]{distance}, \link[terra]{distance}}
}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
