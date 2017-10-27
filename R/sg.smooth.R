# Savitzky-Golay smoothing filter 
#  Smoothin of timeseries data using a Savitzky-Golay smoothing filter  
#  x          Vector to be smoothed
#  ts.start   Start of timeseries (see ts for correct arguments)
#  ts.end     End of timeseries (see ts for correct arguments)
#  freq       Frequency of ts object
#  gap.size   The cutoff for the maximum number of sequential NA values to be interpolated
#  na.rm      (FALSE/TRUE) Should NA's that cannot be interpolated be removed
#  ...        Additional arguments passed to sgolayfilt
# 
#  # Smoothed monthly summaries 
#  sg.smooth(runif(365), ts.start = c(2004,1), ts.end = c(2004,12), freq = 365) 
#
sg.smooth <- function(x, ts.start = c(2004,1), ts.end = c(2004,12), 
                      freq = 12, gap.size = 0, na.rm = FALSE, ...) {
  v = zoo::na.spline(as.vector(x), maxgap = gap.size, na.rm = na.rm) 
  v = ts(v, start = ts.start, end = ts.end, frequency = freq)
  return( signal::sgolayfilt(v) ) 
}
