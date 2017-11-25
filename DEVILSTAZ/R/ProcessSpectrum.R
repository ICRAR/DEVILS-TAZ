# Input a spectrum, and create filtered version for cross-correlation.
# Written by Ivan Baldry. 
# Translated to R by Leon Drygala
ProcessSpectrum = function(specIn, stLambda = 3726, endLambda = 8850, os1 = 10, os2 = 60, 
                           minval = -1.0e4, maxval = 1.0e6, clipvalue = 25, baddataErrorValue = 1e10, 
                           useInvCorrection = TRUE, verbose = TRUE){
  
  
  spec <- specIn
  # Criteria for bad spectral points: non-finite - flagged as bad - 
  #   or large deviations of sky-subtracted data - assumed bad pixels.
  criteriaBadspec <- !is.finite(spec$flux) | (spec$flux < minval) | (spec$flux > maxval)
  # Criteria for bad errors: non-finite or <= zero
  criteriaBaderror = !is.finite(spec$error) | (spec$error <= 0)
  
  # Set approximate relative flux 'calibration' as a function of microns.
  # Was obtained usng GAMA standard stars. 
  if (useInvCorrection) {
    res <- c(  -3.406,  22.175, -49.208,  55.037, -23.441)
    invCorrection <- PolyCalc(res, spec$lambda/10000)
    # Apply relative flux calibration.
    spec$flux <- spec$flux / invCorrection
    spec$error <- spec$error / invCorrection
  }
  
  # Robustly high-pass filter the spectrum.
  r <- FitandFilter(spec, stLambda, endLambda, os1, os2, verbose = verbose)
  spec <- r[[1]]
  useFit <- r[[2]]

  
  useOkError <- which(!criteriaBaderror)
  # Broaden error over all lines
  spec$error[useOkError] <- AdjustBroaden(spec$error[useOkError])
  # Set minimum error to avoid anomalously low errors, i.e. take the larger of adjustedError and spec$error
  adjustedError <- 0.7*MedianAdjust(spec$error[useOkError], 13)
  booArray <- adjustedError > spec$error[useOkError]
  spec$error[useOkError][booArray] <- adjustedError[booArray]
  
  
  
  # Set bad-data error value where there exists any bad data .
  useBaddata <- which(criteriaBadspec | criteriaBaderror)
  spec$countBadPix <- length(useBaddata)
  spec$error[useBaddata] <- baddataErrorValue
  spec$flux[useBaddata] <- 0.0
  
  
  
  # divide by sigma^2 as suggested by Saunders, Cannon and Sutherland
  spec$flux = spec$flux / spec$error^2
  # clip value, was 30. 
  spec$flux = NormaliseMeandev(spec$flux, clipvalue=clipvalue, use=useFit)

  return = spec
}


# Set up region to use for fitting between stLambda and endLambda.
# Also only use finite values. 
FitandFilter = function(spec, stLambda, endLambda, os1, os2, verbose = TRUE){
  if(stLambda == -1){
    stLambda <- spec$lambda[1]
  } 
  if(endLambda == -1){
    endLambda <- tail(spec$lambda,1)
  }
  cFitRegion <- CriteriaBetween(spec$lambda, c(stLambda, endLambda))
  cFinite <- is.finite(spec$flux)
  useFit <- which(cFitRegion & cFinite)
  useNaN <- which(!cFinite)
  countNaN <- length(useNaN)
  # First fit ndegree polynomial and subtract fit. 
  # This uses an iterative rejection routine to remove outliers. 
  coeffs <- PolyReject(x = spec$lambda[useFit], y = spec$flux[useFit], verbose = verbose)

  specFit <- PolyCalc(coeffs, spec$lambda)
  spec$flux <- spec$flux - specFit
  spec$flux[useNaN] <- 0.0

  # Create smooth version of resulting spectrum - using median filtering --
  # and then a trapezium smoothing effectively - 
  # and then subtract smoothed spectrum. 
  fluxSmooth <- spec$flux
  #if numReject GE 1 then fluxSmooth[use_fit[reject_pts]] = 0.0
  fluxSmooth <- MedSmooth(fluxSmooth, 51, 121)
  fluxSmooth <- RunningMeanSmooth(x = fluxSmooth, width = 21)
  spec$flux <- spec$flux - fluxSmooth
  
  # Set end points smoothly to zero between stLambda+os2 and stLambda+os1
  use <- which(spec$lambda < stLambda+os1)
  countStLambda <- length(use)
  spec$flux[use] <- 0.0
  use <- which(CriteriaBetween(spec$lambda, c(stLambda+os1,stLambda+os2)))
  count <- length(use)
  spec$flux[use] <- spec$flux[use] * CosineFilter(count,reverse = FALSE)
  
  # Set end points smoothly to zero between endLambda-os2 and endLambda-os1
  use <- which(spec$lambda > endLambda-os1)
  countEndLambda <- length(use)
  spec$flux[use] <- 0.0
  use <- which(CriteriaBetween(spec$lambda, c(endLambda-os2,endLambda-os1)))
  count <- length(use)
  spec$flux[use] <- spec$flux[use] * CosineFilter(count, reverse = TRUE)
  
  # Reset bad regions again to zero. 
  spec$flux[useNaN] = 0.0  
  return = list(spec, useFit)
}

# Polynomial fitting with sigma rejection, return lm() object
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
PolyReject = function(x, y, niterate = 15, degree = 4, highSigma=3.5, 
                         lowSigma=3.5, verbose=TRUE){ 
  # Initial setting - sigma=0 everywhere.
  sigmas <- rep(0.0, length(y))
  i <- 1
  iterationFinished <- FALSE
  
  while (!iterationFinished) {
    #weights <- rep(0,length(y)) + (sigmas < highSigma & sigmas > -lowSigma)
    #pts <- which(weights == TRUE)
    #res <- lm(y ~ poly(x, degree, raw=TRUE), weights=weights)
    
    
    pts <- sigmas < highSigma & sigmas > -lowSigma
    res <- lm(y[pts] ~ poly(x[pts], degree, raw=TRUE))
    
    
    sdUse <- sd( res$residuals )
    sigmas[pts] <- (res$residuals) / sdUse
    # test if finished iteration and print if required
    
    
    if(i == niterate) iterationFinished <- TRUE
    if( !sum( (sigmas[pts] > highSigma) | (sigmas[pts] < -lowSigma) ) ){
        if(verbose) cat('\nPOLY_REJECT: Everything within sigma bounds')
        iterationFinished <- TRUE
    }
    if(iterationFinished){
      numReject <- sum(!pts)
      if (verbose) { 
        cat('\nPOLY_REJECT: iteration=', i, '/', niterate, '  : rejected=', numReject)
      }
    }
    i <- i + 1
  }
  #reject_pts <- which(sigmas >= highSigma | sigmas <= -lowSigma)\
  #yfit <- poly_calc(res, x)
  
  return = res$coefficients
}



# Median filter a spectrum then smooth. 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
MedSmooth = function(spec, medWidth = 51, smoothWidth = 121) {
  # Smooth with median filter. 
  outSpec <- MedianAdjust(spec,medWidth)
  
  # Further smooth using standard boxcar smoothing. 
  outSpec <- RunningMeanSmooth(x = outSpec, width = smoothWidth)
  
  return = outSpec
}


# Perform median filter with edge adjustment on a vector spectrum. 
# IDL median does not run a median filter over the edge points.
# This routine gives the edge points the same value using a median
# value over those points plus one more point. 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
MedianAdjust = function(x, width){
  
  num <- length(x) # example num=1000
  outX <- runmed(x, width, endrule = 'keep')[1:length(x)]   # example width=51
  
  # use a filter of half the width for the edge points
  i1 <- (width %/% 2 - 1)   # example i1=24
  if(i1<1) i1 <- 0
  i2 <- width %/% 2             # example i2=25
  
  # Replace edge points not covered by idl median.pro (runmed in R doesn't have an endrule consistent with this code)
  # example 0:24    becomes median of 0:25
  outX[1:(i1+1)]     <- median(x[1:(i2+1)])   
  # example 975:999 becomes median of 974:999
  outX[(num-i1):num] <- median(x[(num-i2):num])
  
  return = outX

}

# Translation of IDL's SMOOTH function into R
# Uses IDL's '/EDGE_TRUNCATE' option
# Written by Leon Drygala
RunningMeanSmooth = function(x,width){
  length <- length(x)
  #filter doesn't seem to be able to deal with edge values
  outX <- filter(x, rep(1/width,width), method = "convolution", sides = 2)
  
  # Truncate left edge
  for(i in 1:(width%/%2)){
    index <- (i-width%/%2):(i+width%/%2)
    for(j in 1:width){
      if(index[j]<1) index[j] <- 1
    }
    outX[i] <- mean(x[index])
  }
  
  # Truncate right edge
  for(i in (length - width%/%2 + 1):length){
    index <- (i-width%/%2):(i+width%/%2)
    for(j in 1:width){
      if(index[j]>length) index[j] <- length
    }
    outX[i] <- mean(x[index])
  }
  
  return = outX
}


# Rising or falling cosine filter, i.e., a cosine bell taper. 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
CosineFilter = function(num, reverse=FALSE){
  if(num<2){
    return = 0
  }
  phase <- (0:(num-1))  /(num-1) * pi
  if (!reverse){
    filter <- 0.5 * (1.0 - cos(phase)) 
  } else { 
    filter <- 0.5 * (1.0 + cos(phase))
  }
  return = filter
}


# Return 1 where data is in range.
# Written by Ivan Baldry. 
# Translated to R by Leon Drygala
CriteriaBetween = function(data, data_range){
  criteria <- (data > data_range[1]) & (data <= data_range[2])
  return = criteria
}

# Normalise a spectrum by dividing by mean absolute deviation.
# Iterate for clipping if clipvalue keyword is set. 
# Written by Ivan Baldry. 
# Translated to R by Leon Drygala
NormaliseMeandev = function(flux, clipvalue, use, percentvalue = 0.95){
  
  specOut <- flux
  
  # If use keyword for indexes is not defined then 
  # set indexes to use for calculation of mean absolute deviation
  # default is to reject largest 5 percent
  if (!missing(use)) {
    cFinite <- is.finite(specOut)
    testval <- quantile( abs(specOut[which(cFinite)]), percentvalue )
    use <- which( cFinite & (abs(specOut) <= testval) )
  }

  # Iterate mean deviation clipping each time until convergence within
  # a tolerance of 0.01.
  count <- 0
  if (!missing(clipvalue)){
    testDev <- 1
    while (testDev == 1){
      meanDeviation <- mean( abs(specOut[use]) )
      specOut <- specOut / meanDeviation
      if (max(abs(specOut)) > clipvalue+0.01) {
        testDev = 1 
      } else {
        testDev = 0
      }
      specOut[which(specOut > clipvalue )] <- clipvalue
      specOut[which(specOut < -clipvalue )] <- -clipvalue
      count <- count + 1
      if (count > 10000)
        break
    }
  } else {
    meanDeviation <- mean( abs(specOut[use]) )
    specOut <- specOut / meanDeviation
  }
  
  return = specOut
}

# Broaden spectrum by 1 pixel using a maximum filter kernel of width 3. 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
AdjustBroaden = function(spec) {
  specOut <- spec
  length <- length(specOut)
  max <- max(specOut)
  index <- which(specOut == max)

  specOut[index-1] <- max #TODO may set specOut[0] to max.. is this okay?
  specOut[index+1] <- max
  # above may add an extra piece to the array, below will ignore it
  return = specOut[1:length]
}