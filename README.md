# spatialEco

[![CRAN
status](http://www.r-pkg.org/badges/version/spatialEco)](https://cran.r-project.org/package=spatialEco)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/grand-total/spatialEco)](https://cran.r-project.org/package=spatialEco)

spatialEco R package with utilities to support spatial data manipulation, query, sampling
    and modelling. Functions include models for species population density, download
    utilities for climate and global deforestation spatial products, spatial
    smoothing, multivariate separability, point process model for creating pseudo-
    absences and sub-sampling, polygon and point-distance landscape metrics,
    auto-logistic model, sampling models, cluster optimization and statistical
    exploratory tools.
    
# Available functions in spatialEco 1.2-0 are:


          1. bearing.distance - Calculate new point based on bearing/distance 
		  
		  1. breeding.density - Calculates n-th percent breeding density areas base on a kernel density estimate of 
                             population counts.     

          1. classBreaks - for finding class breaks in a distribution
          
          1. class.comparison - Compares two nominal rasters
          
          1. correlogram - Calculates and plots a correlogram (spatially lagged correlations, "pearson", 
                        "kendall" or "spearman")

          1. concordance - Performs a concordance/disconcordance (C-statistic) test on binomial models.

          1. conf.interval - Calculates confidence interval for the mean or median of a distribution with unknown 
                           population variance
          
          1. convexHull - Derives a convex hull of points using the alpha hull approach with adjustable tension. 
                       Please note that due to licensing reasons, this function is only available in the GitHub 
                       development version and not on CRAN.  

          1. crossCorrelation - Calculates the partial spatial cross-correlation function
          
          1. csi - Calculates the cosine similarity and angular similarity on two vectors or a matrix

          1. curvature - Zevenbergen & Thorne, McNab's or Bolstad's surface (raster) curvature  

          1. daymet.point - Downloads DAYMET climate variables for specified point and timeperiod

          1. daymet.tiles - Returns a vector of DAYMET tile id's within a specified extent

          1. dispersion - Calculates the dispersion ("rarity") of targets associated with planning units

          1. dissection - Evans (1972) Martonne's modified dissection                                  

          1. divergence - Kullback-Leibler Divergence (Cross-entropy)                               

          1. download.daymet - Batch download of daily gridded DAYMET climate data    

          1. download.hansen - Download of Hansen Global Forest Change 2000-2013  

          1. download.prism - Batch download of monthly gridded PRISM climate data

          1. effect.size - Cohen's-d effect size with pooled sd for a control and experimental group 
          
          1. erase.points - Erases points inside or outside a polygon feature class
          
          1. focal.lmetrics - Landscape metrics using a focal window
          
          1. fuzzySum - Calculates the fuzzy sum of a vector

          1. gaussian.kernel - Creates a Gaussian Kernel of specified size and sigma

          1. group.pdf - Creates a probability density plot of y for each group of x           

          1. hexagons - Create hexagon polygon “fishnet” of defined size and extent. 
		  
		  1. hli - Heat Load Index
		  
          1. hsp - Hierarchical Slope Position
          
          1. hybrid.kmeans - Clustering using hierarchical clustering to define cluster-centers in k-means 

          1. idw.smoothing - Distance weighted smoothing (IDW) of a variable in a spatial point object. 
                          The function is a smoothing interpolator at the point observation(s) level using 
                          a distance-weighted mean.   

          1. insert.values - Inserts new values into a vector at specified positions  
        
	      1. kendall - Kendall tau trend with continuity correction for time-series
		  
          1. kl.divergence - Calculates the Kullback-Leibler divergence (relative entropy) between unweighted theoretical 
                          component distributions. Divergence is calculated as: int [f(x) (log f(x) - log g(x)) dx]
                          for distributions with densities f() and g().       

          1. land.metrics - Calculates a variety of landscape metrics, on binary rasters, for polygons or points with a buffer 
                         distance. This is similar to the moving window in Fragstats but, uses either a buffer for each 
                         point or a zonal approach with polygons, to derive local metrics. 

          1. local.min.max - Calculates the local minimums and maximums in a numeric vector, indicating inflection points 
                          in the distribution.   

          1. loess.boot - Bootstrap of a Local Polynomial Regression (loess)

          1. loess.ci - Calculates a local polynomial regression fit with associated confidence intervals   

          1. logistic.regression - Performs a logistic (binomial) and autologistic (spatially lagged binomial) regression 
                                using maximum likelihood estimation or penalized maximum likelihood estimation.

          1. moments - Calculate statistical moments of a distribution including percentiles, arithmetic-geometric-harmonic 
                    means, coefficient of variation, median absolute deviation, skewness, kurtosis, mode and number of modes.    

          1. morans.plot - Autocorrelation plot 
          
          1. nni - Calculates the nearest neighbor index (NNI) as a measure of clustering or dispersal
          
          1. oli.aws - Download Landsat 8 - OLI from AWS.   

          1. o.ring - Calculates the inhomogeneous O-ring point pattern statistic (Wiegand & Maloney 2004)               

          1. optimal.k - Find optimal k of k-Medoid partitions using silhouette widths 
          
          1. optimized.sample.variance - Draws an optimal sample that minimizes or maximizes the sample variance 

          1. outliers - Identify outliers using modified Z-score  

          1. parea.sample - Creates a systematic or random point sample of polygons where n is based on percent area of 
                         each polygon

          1. plot.effect.size - Plot generic for effect size

          1. plot.loess.boot - Plot generic for loess boot     

          1. point.in.poly - Intersects point and polygon feature classes and adds polygon attributes to the points     

          1. polyPerimeter - Calculates the perimeter length(s) for a polygon object
          
          1. poly.regression - smoothing data in time-series and imputing missing (NA) values using polynomial regression

          1. pp.subsample - Generates random subsample based on point process intensity function of the observed data. 
                         This is a spatially informed data thinning model that can be used to reduce pseudo-replication 
                         or autocorrelation.  

          1. proximity.index - Proximity index for a set of polygons                             

          1. pseudo.absence - Generates pseudo-absence samples based on the spatial intensity function of known species locations. 
                           This is akin to distance constrained but is informed by the spatial process of the observed data 
                           and is drawn from a probabilistic sample following the intensity function.       

          1. raster.change - Compares two categorical rasters with a variety of statistical options 
		  
          1. raster.deviation - Local deviation from the raster based on specified global statistic or a polynomial trend.                          

          1. raster.downscale - Downscale raster to a higher resolution raster using robust regression
          
          1. raster.entropy - Calculates entropy on integer raster (i.e., 8 bit 0-255)  

          1. raster.gaussian.smooth - Applies a Gaussian smoothing kernel to smooth raster.                     
          
		  1. raster.invert - Inverts value of a raster                              

          1. raster.kendall - Calculates Kendall's tau trend with continuity correction for raster time-series

          1. raster.mds - Multidimensional scaling of raster values within an N x N focal window                                 

          1. raster.modified.ttest - Bivariate moving window correlation using Dutilleul's modified t-test 

          1. raster.moments - Calculates focal statistical moments of a raster                              

          1. raster.transformation - Applies specified statistical transformation to a raster                       

          1. raster.vol - Calculates a percent volume on a raster or based on the entire raster or a systematic sample

          1. raster.Zscore - Calculates the modified z-score for all cells in a raster                               

          1. rasterCorrelation - Performs a simple moving window correlation between two rasters		  
		  
          1. remove.holes - Removes all holes (null geometry) in polygon sp class objects 

          1. sa.trans - Trigonometric transformation of a slope and aspect interaction 

          1. sample.annulus - Creates sample points based on annulus with defined inner and outer radius

          1. sample.line - Creates a systematic or random point sample of an sp SpatialLinesDataFrame object based on 
                        distance spacing, fixed size or proportional size

          1. sample.poly - Creates an equal sample of n for each polygon in an sp Polygon class object

          1. sampleTransect - Creates random transects from points and generates sample points along each transect

          1. separability - Calculates variety of univariate or multivariate separability metrics for nominal class samples
          
          1. sg.smooth - Smoothing time-series data using a Savitzky-Golay filter 

          1. shannons - Calculates Shannon's Diversity Index and Shannon's Evenness Index

          1. similarity - Uses row imputation to identify "k" ecological similar observations
          
          1. smooth.time.series - Smoothing and imputing missing (NA) of pixel-level data in raster time-series 
                                using (local polynomial) LOESS regression
          
		  1. sobal - Applies an isotropic image gradient operator (Sobel-Feldman) using a 3x3 window  
		  
          1. sp.kde - A weighted or un-weighted kernel density estimate

          1. sp.na.omit  - Removes row or column NA's in sp object. The standard R na.omit function will not propagate through 
                        all slots of an sp class object. This function removes the spatial objects, in all slots, corresponding 
                        to NA's in the @data data.frame object.        

          1. srr - Surface Relief Ratio 

          1. stratified.random - Creates a stratified random sample of an sp class object using a factor.
          
          1. subsample.distance - Minimum, and optional maximum, distance constrained sub-sampling
          
          1. swvi - Senescence weighted MSAVI or MTVI

          1. topo.distance - Calculates topographic corrected distance for a SpatialLinesDataFrame object  

          1. tpi - Calculates topographic position using mean deviations within specified window  

          1. trasp - Solar-radiation Aspect Index 

          1. trend.line - Calculated specified (linear, exponential, logarithmic, polynomial) trend line of x,y 
                          and plots results.

          1. tri - Implementation of the Riley et al (1999) Terrain Ruggedness Index

          1. vrm - Implementation of the Sappington et al., (2007) vector ruggedness measure
          
          1. winsorize - Removes extreme outliers using a winsorization transformation

          1. wt.centroid - Creates centroid of [x,y] coordinates, of a random field, based on a weights field in 
                        a point sample.      

          1. zonal.stats - Polygon "zonal" statistics of a raster. Function can accept custom “vectorized” function. 


**Bugs**: Users are encouraged to report bugs here. Go to [issues](https://github.com/jeffreyevans/spatialEco/issues) in the menu above, and press new issue to start a new bug report, documentation correction or feature request. You can direct questions to <jeffrey_evans@tnc.org>.

**To install `spatialEco` in R use install.packages() to download curent stable release from CRAN** 

**or, for the development version, run the following (requires the remotes package):**
`remotes::install_github("jeffreyevans/spatialEco")`
