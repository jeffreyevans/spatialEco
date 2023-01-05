# spatialEco (dev 2.0-01) <img src="man/figures/logo.png" align="right" height="132" />
<!-- badges: start -->
[![R-CMD-check](https://github.com/jeffreyevans/spatialEco/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeffreyevans/spatialEco/actions/workflows/R-CMD-check.yaml)
[![CRAN status](http://www.r-pkg.org/badges/version/spatialEco)](https://cran.r-project.org/package=spatialEco)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/spatialEco)](https://cran.r-project.org/package=spatialEco)

<!-- badges: end -->
spatialEco R package with utilities to support spatial data manipulation, query, sampling
    and modeling. Functions include models for species population density, qudrat-based 
	analysis and sampling, spatial smoothing, multivariate separability, point process model 
	for creating pseudo-  absences and sub-sampling, polygon and point-distance structural metrics,
    auto-logistic model, sampling models, cluster optimization and statistical
    exploratory tools.

I jumped to a major release and pushed the version to 2.0-0. All spatial functions are now using the 
`sf` and `terra` packages due to `sp`, `rgeos`, `rgdal`, `maptools` and `raster` being retired. Sorry 
but, for the most part I removed backwards compatibility with these depreciated object classes so, you 
will need to make sure that you are using modern spatial object classes. In terra, there is now only 
one class type for multi or single band raster objects "SpatRaster" which can be read or coerced using 
`terra::rast`. For coercing sp class vector objects to sf you can use `sf::st_as:sf` or `as(x, "sf")` 
and, going from sf to sp you use `as(x, "Spatial")` 
   
 
## Available functions in spatialEco 2.0-01

| `spatialEco` Function        | Description                                                                             |
|:-----------------------------|:----------------------------------------------------------------------------------------|
| `all_pairwise`               | Creates a list of all pairwise combinations of a vector  |
| `annulus.matrix`             | Creates a 0,1 matrix based on defined annulus parameters, can be used as a window matrix in a raster focal function  |
| `aspline.downscale`          | Downscale raster to a higher resolution using multivariate adaptive regression splines (MARS) |
| `background`                 | Creates a point sample that can be used as a NULL for SDM's and other modeling approaches (see pseudo.absence for alternate approach). |
| `bbox_extent`                | Creates a bounding box polygon representing the extent of a feature or raster  |                                                     
| `bearing.distance`           | Calculate new point based on bearing/distance                                     
| `breeding.density`           | Calculates n-th percent breeding density areas base on a kernel density estimate of population counts. |
| `built.index`                | remote sensing built-up index |      
| `cgls_urls`                  | Based on query, provide URL's for Copernicus Global Land Service datasets         
| `chae`                       | The Canine-Human Age Equivalent (for fun)                                         
| `class.comparison`           | Depreciated, with migration to terra, I collapsed into raster.change                                                      
| `classBreaks`                | for finding class breaks in a distribution                                        
| `collinear`                  | Test for linear or nonlinear collinearity/correlation in data                     
| `combine`                    | Combines multiple rasters into an "all possible combinations" raster emulation the ESRI combine function
| `concordance`                | Performs a concordance/disconcordance (C-statistic) test on binomial models.      
| `conf.interval`              | Calculates confidence interval for the mean or median of a distribution with with unknown population variance
| `convexHull`                 | Derives a convex hull of points using the alpha hull approach with adjustable tension. Please note that due to licensing reasons, this function is only available in the GitHub development version and not on CRAN. You must call the function from the package namespace using spatialEco:::convexHull
| `correlogram`                | Calculates and plots a correlogram (spatially lagged correlations, "pearson", "kendall" or "spearman") 
| `cross.tab`                  | Cross tabulate two rasters, labels outputs                                        
| `crossCorrelation`           | Calculates the partial spatial cross-correlation function                         
| `csi`                        | Calculates cosine similarity and angular similarity on two vectors or a matrix    
| `curvature`                  | Zevenbergen & Thorne, McNab's or Bolstad's surface (raster) curvature             
| `dahi`                       | Calculates the DAHI (Diurnal Anisotropic Heat Index)                              
| `date_seq`                   | Creates date sequence, given defined start and stop dates, with options for day, week, month, quarter, year or, minute.
| `daymet.point`               | Downloads DAYMET climate variables for specified point and timeperiod             
| `daymet.tiles`               | Returns a vector of DAYMET tile id's within a specified extent                    
| `dispersion`                 | Calculates the dispersion ("rarity") of targets associated with planning units    
| `dissection`                 | Evans (1972) Martonne's modified dissection                                       
| `divergence`                 | Kullback-Leibler Divergence (Cross-entropy)                                       
| `download.daymet`            | Depreciated                             
| `download.hansen`            | Depreciated                                 
| `download.prism`             | Depreciated                             
| `effect.size`                | Cohen's-d effect size with pooled sd for a control and experimental group         
| `erase.points`               | Erases points inside or outside a polygon feature class                           
| `explode`                    | Depreciated due to redundancy with sf::st_cast                              
| `extract.vertices`           | extracts (x,y) vertices coordinates from polygons and linesa                      
| `fuzzySum`                   | Calculates the fuzzy sum of a vector                                              
| `gaussian.kernel`            | Creates a Gaussian Kernel of specified size and sigma                             
| `geo.buffer`                 | Buffers data in geographic coordinate space using a temporary projection          
| `group.pdf`                  | Creates a probability density plot of y for each group of x                       
| `hexagons`                   | Create hexagon polygon “fishnet” of defined size and extent.                      
| `hli.pt`                     | Heat Load Index for tabular "point" data with slope and aspect                  
| `hli`                        | Heat Load Index, now with support for southern hemisphere data                    
| `hsp`                        | Hierarchical Slope Position                                                       
| `hybrid.kmeans`              | Clustering using hierarchical clustering to define cluster-centers in k-means     
| `idw.smoothing`              | Distance weighted smoothing (IDW) of a variable in a spatial point object. The function is a smoothing interpolator at the point observation(s) level using a distance-weighted mean.
| `impute.loess`               | Imputes NA's or smooths data (or both) for a vector, intended mostly for time-series or serial data.
| `insert`                     | Inserts a row or column into a data.frame                                         
| `insert.values`              | Inserts new values into a vector at specified positions                           
| `is.empty`                   | Method, evaluates if vector is empty                                              
| `is.whole`                   | Depreciated after R release of base::is.whole in 4.1.0                             
| `kendall`                    | Kendall tau trend with continuity correction for time-series                      
| `kl.divergence`              | Calculates the Kullback-Leibler divergence (relative entropy) between unweighted theoretical component distributions. Divergence is calculated as: int[f(x) (log f(x) |log g(x)) dx] for distributions with densities f() and g().
| `knn`                        | returns ids, rownames and distance of nearest neighbors in two (or single) spatial objects. Optional radius distance constraint. Added optional covariates (weights)
| `local.min.max`              | Calculates the local minimums and maximums in a numeric vector, indicating inflection points in the distribution.
| `loess.boot`                 | Bootstrap of a Local Polynomial Regression (loess)                                
| `loess.ci`                   | Calculates a local polynomial regression fit with associated confidence intervals 
| `logistic.regression`        | Performs a logistic (binomial) and autologistic (spatially lagged binomial) regression using maximum likelihood estimation or penalized maximum likelihood estimation.
| `max_extent`                 | Returns the maximum extent of multiple spatial inputs                             
| `mean_angle`                 | Returns the mean of a vector of angles. Intended for focal and zonal functions on slope or aspect                           
| `moments`                    | Calculate statistical moments of a distribution including percentiles, arithmetic-geometric-harmonic means, coefficient of variation, median absolute deviation, skewness, kurtosis, mode and number of modes. 
| `morans.plot`                | Autocorrelation plot                                                              
| `nni`                        | Calculates the nearest neighbor index (NNI) measure of clustering or dispersal    
| `nth.vlaue`                  | Returns the Nth (smallest/largest) values in a numeric vector                     
| `oli.aws`                    | Download Landsat 8 OLI from AWS.                                                  
| `o.ring`                     | Calculates inhomogeneous O-ring point pattern statistic (Wiegand & Maloney 2004)  
| `optimal.k`                  | Find optimal k of k-Medoid partitions using silhouette widths                     
| `optimized.sample.variance`  | Draws an optimal sample that minimizes or maximizes the sample variance           
| `outliers`                   | Identify outliers using modified Z-score                                          
| `overlap`                    | For comparing the similarity of two niche estimates using Warren's-I              
| `parea.sample`               | Creates a systematic or random point sample of polygons where n is based on percent area of each polygon
| `parse.bits`                 | Based on integer value, pulls value(s) of specified bit(s)                        
| `parial.cor`                 | Partial and Semi-partial correlation                                              
| `plot.effect.size`           | Plot generic for effect size                                                      
| `plot.loess.boot`            | Plot generic for loess boot                                                       
| `point.in.poly`              | Depreciated because function is redundant with sf::st_intersection
| `polygon_extract`            | Depreciated because of migration to terra. Required package only supports raster class                               
| `polyPerimeter`              | Calculates the perimeter length(s) for a polygon object                           
| `poly.regression`            | smoothing data in time-series and imputing missing (NA) values using polynomial regression
| `poly_trend`                 | Derives Nth order polynomial trend with confidence intervals                        
| `pp.subsample`               | Generates random subsample based on point process intensity function of the observed data. This is a spatially informed data thinning model that can be used to reduce pseudo-replication or autocorrelation.
| `proximity.index`            | Proximity index for a set of polygons                                             
| `pseudo.absence`             | Generates pseudo-absence samples based on the spatial intensity function of known species locations. This is akin to distance constrained but is informed by the spatial process of the observed data and is drawn from a probabilistic sample following the intensity function.
| `quadrats`                   | Quadrat sampling or analysis, variable size and angle options                     
| `random.raster`              | creates random raster/stack of defined dimensions and statistical distributions   
| `raster.change`              | Compares two categorical rasters with a variety of statistical options            
| `raster.deviation`           | Local deviation from the raster based on specified global statistic or a polynomial trend.
| `rasterDistance`             | This replicates the raster distanceFromPoints function but uses the Arya & Mount Approximate Near Neighbor (ANN) C++ library for calculating distances. Which results in a notable increase in performance. It is not memory safe and does not use the GeographicLib (Karney, 2013) spheroid distance method for geographic data
| `raster.downscale`           | Downscale raster to a higher resolution raster using robust regression            
| `raster.entropy`             | Calculates entropy on integer raster (i.e., 8 bit 0-255)                          
| `raster.gaussian.smooth`     | Applies a Gaussian smoothing kernel to smooth raster.h                            
| `raster.invert`              | Inverts value of a raster                                                         
| `raster.kendall`             | Calculates Kendall's tau trend with continuity correction for raster time-series  
| `raster.mds`                 | Multidimensional scaling of raster values within an N x N focal window            
| `raster.modified.ttest`      | Bivariate moving window correlation using Dutilleul's modified t-test             
| `raster.moments`             | Calculates focal statistical moments of a raster                                  
| `raster.transformation`      | Applies specified statistical transformation to a raster                          
| `raster.vol`                 | Calculates a percent volume on a raster or based on the entire raster or a systematic sample   
| `raster.Zscore`              | Calculates the modified z-score for all cells in a raster                         
| `rasterCorrelation`          | Performs a simple moving window correlation between two rasters		           
| `remove_duplicates`          | Removes duplicate duplicate feature geometries         
| `remove.holes`               | Removes all holes (null geometry) in polygon sf class objects                     
| `rm.ext`                     | Removes file extentions from text string                    
| `rotate.polygon`             | Rotates a polygon by specified angle                                              
| `sa.trans`                   | Trigonometric transformation of a slope and aspect interaction                    
| `sample.annulus`             | Creates sample points based on annulus with defined inner and outer radius        
| `sample.line`                | Depreciated because sf::st_sample can aggregate samples by feature
| `sample.poly`                | Depreciated because sf::st_sample can aggregate samples by feature       
| `sampleTransect`             | Creates random transects from points, generates sample points along each transect 
| `separability`               | Calculates variety of univariate separability metrics for nominal class samples   
| `sf_dissolve`                | Dissolves polygon geometry using attribute, globally or overlap 
| `sg.smooth`                  | Smoothing time-series data using a Savitzky-Golay filter                          
| `shannons`                   | Calculates Shannon's Diversity Index and Shannon's Evenness Index                 
| `shift`                      | Shifts a vector by n lags without changing its length, can specify fill values    
| `sieve`                      | Creates a minimum mapping unit by removing pixel clusters < specified area
| `similarity`                 | Uses row imputation to identify "k" ecological similar observations               
| `smooth.time.series`         | Smoothing and imputing missing (NA) of pixel-level data in raster time-series using (local polynomial) LOESS regression
| `sobal`                      | Applies an isotropic image gradient operator (Sobel-Feldman) using a 3x3 window   
| `spatial.select`             | Performs a spatial select (feature subset) similar to ArcGIS                      
| `spectral.separability`      | Calculates class-wise multivariate spectral separability                          
| `sf.kde`                     | A weighted or un-weighted kernel density estimate (previously sp.kde now as alias)                                 
| `sp.na.omit `                | Depreciated as only relevant to sp class objects, for sf use base na.omit 
| `squareBuffer`               | Creates a square buffer of feature class
| `srr`                        | Surface Relief Ratio                                                              
| `stratified.random`          | Creates a stratified random sample of an sp class object using a factor.          
| `subsample.distance`         | Minimum, and optional maximum, distance constrained sub-sampling                  
| `swvi`                       | Senescence weighted MSAVI or MTVI                                                 
| `time_to_event`              | Returns the time (sum to position) to a specified value                           
| `topo.distance`              | Calculates topographic corrected distance for a SpatialLinesDataFrame object      
| `tpi`                        | Calculates topographic position using mean deviations within specified window     
| `trasp`                      | Solar-radiation Aspect Index                                                      
| `trend.line`                 | Calculated specified (linear, exponential, logarithmic, polynomial) trend line of x,y and plots results.
| `tri`                        | Implementation of the Riley et al (1999) Terrain Ruggedness Index                 
| `vrm`                        | Implementation of the Sappington et al., (2007) vector ruggedness measure         
| `winsorize`                  | Removes extreme outliers using a winsorization transformation                     
| `wt.centroid`                | Creates centroid of [x,y] coordinates, of a random field, based on a weights field in a point sample.
| `zonal.stats`                | Depreciated in leu of exactextractr library

**Bugs**: Users are encouraged to report bugs here. Go to [issues](https://github.com/jeffreyevans/spatialEco/issues) in the menu above, and press new issue to start a new bug report, documentation correction or feature request. You can direct questions to <jeffrey_evans@tnc.org>.

**To install `spatialEco` in R use install.packages() to download current stable release from CRAN** 

**for the development version, run the following (requires the remotes package):**
`remotes::install_github("jeffreyevans/spatialEco")`

**You can also install from ROpenSci (R-Universe):**

```
# Enable repository from jeffreyevans
options(repos = c(
  jeffreyevans = 'https://jeffreyevans.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install spatialEco in R
install.packages('spatialEco')
```
