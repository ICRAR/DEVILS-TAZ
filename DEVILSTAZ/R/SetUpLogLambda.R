# Define scale for rebinning in cross correlation.
# Written By Ivan Baldry
# Translated by Leon Drygala
SetUpLogLambda = function(oversample=5, lowValue=3.3, highValue=4.0, verbose = TRUE){
  sdssGap <- 0.0001
  gap <- NULL
  # oversample the SDSS log wavelength gap, default is a factor of 5
  if (oversample != 0){
    gap <- sdssGap / oversample
  } #TODO throw div 0 exception
  
  velocityGap <- gap*log(10)*2.998E5
  if(verbose) cat('\nSET_UP_logLambda: Velocity rebinned gap', velocityGap)
  num <- round( (highValue - lowValue)/gap )

  logLambda <- lowValue + gap*(0:(num-1))
  
  if(verbose){ 
    cat('\nSET_UP_logLambda: Num points and range', num, 
      10.^logLambda[0], 10.^logLambda[num-1])
  }
  
  return = list("logLambda" = logLambda, "gap" = gap, "velocity_gap" = velocityGap)
}
