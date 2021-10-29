#' @include subsample.distance.R
NULL
#' @rdname subsample.distance
#' @export
setGeneric('subsample.distance')
#' @rdname subsample.distance
#' @export
setMethod('subsample.distance', 'SpatialPoints', function(x, size, d, d.max = NULL, replacement = FALSE,
                                                          latlong = FALSE, echo = FALSE) {

  if (missing(x)) stop("Must define a spatial object")
  if (missing(d)) stop("Must define minimum separation distance")
  if (!is.null(d.max) && d.max <= d) stop("Maximum distance must be larger than minimum")
  if (latlong) message("geographic projection distances must be in kilometers")
  if (size >= length(x)) stop("subsample size must be smaller than population")

  # First iteration - sets up `rs`, `s` and subsets `x`
  rs <- sample(1:length(x), 1)
  sa <- x[rs]
  if (!replacement) x <- x[-rs, ]

  for (i in 2:size) {

    nsamp <- 0
    deval <- T

    while(deval == T) {

      # Draw new `rs` and find its distance to the previous point
      rs <- sample(1:length(x), 1)
      pts.dist <- sp::spDists(sa, x[rs], longlat = latlong)

      if(is.null(d.max)) {
        deval <- any(pts.dist < d, na.rm = T)
      } else {
        deval <- any(pts.dist < d, na.rm = T) | any(pts.dist > d.max, na.rm = T)
      }

      nsamp <- nsamp + 1
      if (echo) cat("Sample iteration=", nsamp, "\n")
      if (nsamp == length(x)) break

    }

    if (echo) {
      cat("\n","Min distance for", i, "=", min(pts.dist, na.rm = T), "\n")
      cat(" Max distance for", i, "=", max(pts.dist, na.rm = T), "\n")
    }
    if (nsamp == length(x)) {
      message(paste0("Warning: sampling cannot converge at n=", size, " returning n=", length(sa)))
      return(sa)
    }

    # Add sample to results, drop it out if without replacement
    sa <- rbind(sa, x[rs])
    if (!replacement) x <- x[-rs]

  }

  return(sa)

})
