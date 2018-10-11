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
    
    Available functions in spatialEco are:

​

          breeding.density - Calculates n-th percent breeding density areas base on a kernel density estimate of population counts.     

          correlogram - Calculates and plots a correlogram (spatially lagged correlations, "pearson", "kendall" or "spearman")

          concordance - Performs a concordance/disconcordance (C-statistic) test on binomial models.

          conf.interval - Calculates confidence interval for the mean or median of a distribution with unknown population variance

          csi - Calculates the cosine similarity and angular similarity on two vectors or a matrix

          daymet.point - Downloads DAYMET climate variables for specified point and timeperiod

          daymet.tiles - Returns a vector of DAYMET tile id's within a specified extent

          dispersion - Calculates the dispersion ("rarity") of targets associated with planning units

          download.daymet - Batch download of daily gridded DAYMET climate data    

          download.hansen - Download of Hansen Global Forest Change 2000-2013  

          download.prism - Batch download of monthly gridded PRISM climate data

          effect.size - Cohen's-d effect size with pooled sd for a control and experimental group   

          gaussian.kernel - Creates a Gaussian Kernel of specified size and sigma

          group.pdf - Creates a probability density plot of y for each group of x           

          hexagons - Create hexagon polygon “fishnet” of defined size and extent.             

          idw.smoothing - Distance weighted smoothing (IDW) of a variable in a spatial point object. The function is a smoothing interpolator at the point observation(s) level using a distance-weighted mean.   

          insert.values - Inserts new values into a vector at specified positions   

          kl.divergence - Calculates the Kullback-Leibler divergence (relative entropy) between unweighted theoretical component distributions. Divergence is calculated as: int [f(x) (log f(x) - log g(x)) dx] for distributions with densities f() and g().       

          land.metrics - Calculates a variety of landscape metrics, on binary rasters, for polygons or points with a buffer distance. This is similar to the moving window in Fragstats but, uses either a buffer for each point or a zonal approach with polygons, to derive local metrics. 

           local.min.max - Calculates the local minimums and maximums in a numeric vector, indicating inflection points in the distribution.   

          loess.boot - Bootstrap of a Local Polynomial Regression (loess)

          loess.ci - Calculates a local polynomial regression fit with associated confidence intervals   

          logistic.regression - Performs a logistic (binomial) and autologistic (spatially lagged binomial) regression using maximum likelihood estimation or penalized maximum likelihood estimation.

          moments - Calculate statistical moments of a distribution including percentiles, arithmetic-geometric-harmonic means, coefficient of variation, median absolute deviation, skewness, kurtosis, mode and number of modes.    

          mwCorr - A bivariate raster correlation using Dutilleul's modified t-test           

          nni - Calculates the nearest neighbor index (NNI) as a measure of clustering or dispersal               

          o.ring - Calculates the inhomogeneous O-ring point pattern statistic (Wiegand & Maloney 2004)               

          optimal.k - Find optimal k of k-Medoid partitions using silhouette widths  

          outliers - Identify outliers using modified Z-score  

          parea.sample - Creates a systematic or random point sample of polygons where n is based on percent area of each polygon

          plot.effect.size - Plot generic for effect size

          plot.loess.boot - Plot generic for loess boot     

          point.in.poly - Intersects point and polygon feature classes and adds polygon attributes to the points     

          polyPerimeter - Calculates the perimeter length(s) for a polygon object

          pp.subsample - Generates random subsample based on point process intensity function of the observed data. This is a spatially informed data thinning model that can be used to reduce pseudo-replication or autocorrelation.  

          pseudo.absence - Generates pseudo-absence samples based on the spatial intensity function of known species locations. This is akin to distance constrained but is informed by the spatial process of the observed data and is drawn from a probabilistic sample following the intensity function.       

          raster.entropy - Calculates entropy on integer raster (i.e., 8 bit 0-255)  

          raster.vol - Calculates a percent volume on a raster or based on the entire raster or a systematic sample

          rasterCorrelation - Performs a simple moving window correlation between two rasters

          sample.annulus - Creates sample points based on annulus with defined inner and outer radius

          sample.line - Creates a systematic or random point sample of an sp SpatialLinesDataFrame object based on distance spacing, fixed size or proportional size

          sample.poly - Creates an equal sample of n for each polygon in an sp Polygon class object

          sampleTransect - Creates random transects from points and generates sample points along each transect

          separability - Calculates variety of univariate or multivariate separability metrics for nominal class samples  

          shannons - Calculates Shannon's Diversity Index and Shannon's Evenness Index

          similarity - Uses row imputation to identify "k" ecological similar observations     

          sp.na.omit  - Removes row or column NA's in sp object. The standard R na.omit function will not propagate through all slots of an sp class object. This function removes the spatial objects, in all slots, corresponding to NA's in the @data data.frame object.        

          stratified.random - Creates a stratified random sample of an sp class object using a factor.

          trend.line - Calculated specified (linear, exponential, logarithmic, polynomial) trend line of x,y and plots results.       

          tpi - Calculates topographic position using mean deviations within specified window  

          tri - Implementation of the Riley et al (1999) Terrain Ruggedness Index

          trig.rtans - The trigonometric Stage (1978) slope * cos(aspect) or slope * sin(aspect)

          vrm - Implementation of the Sappington et al., (2007) vector ruggedness measure

          wt.centroid - Creates centroid of [x,y] coordinates, of a random field, based on a weights field in a point sample.      

          zonal.stats - Polygon "zonal" statistics of a raster. Function can accept custom “vectorized” function. 


**Bugs**: Users are encouraged to report bugs here. Go to [issues](https://github.com/jeffreyevans/spatialEco/issues) in the menu above, and press new issue to start a new bug report, documentation correction or feature request. You can direct questions to <jeffrey_evans@tnc.org>.

**To install `spatialEco` in R use install.packages() to download curent stable release from CRAN** 

**or, for the development version, run the following (requires the devtools package):**
`devtools::install_github("jeffreyevans/spatialEco")`
