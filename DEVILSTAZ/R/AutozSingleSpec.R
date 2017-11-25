# Process spectrum, calculate redshifts and best fit templates using
# cross-correlation. The code is known as Autoz, used for the GAMA survey. 
# TODO FIX THIS DESC
# Inputs are the spectrum, error spectrum, and air wavelength scale. 
# The templates are on a vacuum wavelength scale. Nan in the spectrum
# is assumed to represent bad pixels. 
#
# Essential keywords: log_lambda_rebin is the vacuum wavelength scale to 
# rebin to. gap is logarithmic wavelength gap. tdata_rebin is a structure 
# containing the filtered templates. helio_velocity is the heliocentric 
# velocity for the observation to correct for. 
# 
# num_peaks is the number of cross-correlation peaks to calculate, the 
# default is four. inv_correction is the relative flux calibration.
# ccinfo, peaks return these structures to the calling routine. 
# highz is set if a larger cross-correlation range is needed. 
# st_lambda, end_lambda for maximum range of initial fitting for the 
# high-pass filtering. Pixels outside wavelength range st_lambda+os1 and 
# end_lambda-os1 are set to zero in the high-pass filtered (HPF) spectra, 
# os1=10 is default set in process_spectrum.pro. minval, maxval are the 
# minimum and maximum pixel values that are assumed to be bad pixels and 
# therefore set to zero in the HPF spectra. 
# 
# Written by Ivan Baldry. 
# Translated to R by Leon Drygala.
AutozSingleSpec = function(specRaw, lambda, logLambdaData, logVlambda, tempData, plan, verbose = TRUE,
                           tileFile = paste(.libPaths(), '/auto.z/data/','SG23_Y7_003.fits',sep=''), specNum = 2,
                           tempFile = paste(.libPaths(), '/auto.z/data/','filtered-templates.fits',sep=''),
                           oversample = 5, num = 5, templateNumbers = c(2:14,16:22,40:47),
                           useInvCorrection = TRUE, stLambda = 3726, endLambda = 8850, 
                           minval = -1.0e4, maxval = 1.0e6, z_prior=c(-1,1000), doHelio=T){
  # TODO remove timing
  TOTALTIME <- proc.time()
  
  #set up new lambda scale to rebin spectrum and templates to
  if(missing(logLambdaData)) logLambdaData <- SetUpLogLambda(verbose = verbose, oversample = oversample)
  newLogLambda <- logLambdaData$logLambda
  
  #load in unknown redshift spectrum if not provided
  if(missing(specRaw))
    specRaw <- GetSingleSpec(file = tileFile, colmn = specNum, verbose = verbose)
  else
    specRaw$lambda <- lambda
PROCESSTIME <- proc.time()
  spec <- ProcessSpectrum(specRaw, stLambda = stLambda, endLambda = endLambda, minval = minval, maxval = maxval, 
                          useInvCorrection = useInvCorrection,  verbose = verbose,)
  PROCESSTIME <- proc.time()[3] - PROCESSTIME[3]
  
  spec$countHighval  <- length(which(spec$flux > 20))
  spec$countLowval   <- length(which(spec$flux < -20))
  spec$meanadNorm    <- mean(abs(spec$flux))
  spec$rmsNorm       <- sqrt( mean(spec$flux^2) )
  
  
  # Convert spectral lambda to vacuum wavelength from air wavelength input.
  if(missing(logVlambda)) logVlambda <- log(VacuumFromAir(spec$lambda),10)  
  length <- length(logVlambda)
  # Rebin filtered spectrum ensuring zero outside range.
  specRebin <- approx(x = c(3.0, logVlambda[1] - 0.001, logVlambda, logVlambda[length]+0.001, 4.5), 
                      y = c(0, 0, spec$flux, 0, 0),
                      xout = newLogLambda, method = "linear", yleft=0.0, yright=0.0)#TODO check interpol is correct
  spec$lambda <- specRebin$x
  spec$flux <- specRebin$y
  
  # load rebinned template data if missing
  if (missing(tempData)) tempData <- RebinTempData(newLogLambda, templateNumbers=templateNumbers, 
                                                   file = tempFile, verbose = verbose)
  
  if (doHelio==T) {helioVel <- Heliocentric(spec$RA*180/pi, spec$DEC*180/pi, 2000, jd = spec$UTMJD, longitude = spec$longitude, 
                          latitude = spec$latitude, altitude = spec$altitude)}else{helioVel<-0}

  #if(missing(plan)) plan = planFFT(length(spec$flux), 0)
  if(missing(plan)) plan = 0
CROSSTIME <- proc.time()
  #get cross correlation info and find highest peaks in data
  ccinfo <- DoCrossCorr(spec = spec, gap = logLambdaData$gap, tempData = tempData, helioVel = helioVel, plan = plan, z_prior=z_prior)
CROSSTIME <- CROSSTIME[3] - proc.time()[3]
  peaks <- FindHighestPeaks(ccinfo, num=num)
  #fit quadratic and adjust redshift slightly. Also save ccSigma data
  spec$ccSigma <- rep(-1,length(peaks))
  i <- 0
  for( peak in peaks ){
      
    peak$redshift   <- FindMax(xArray = ccinfo[[peak$templateID]]$shifts[(peak$shiftIndex-3):(peak$shiftIndex+3)],
                               yArray = ccinfo[[peak$templateID]]$crossCorrRaw[(peak$shiftIndex-3):(peak$shiftIndex+3)],
                               n = 2)
    spec$ccSigma[i <- i+1] <- peak$crossCorr
  }
  spec$z <- peaks[[1]]$redshift
  #calculate FOM and Probability
  spec <- CalculateCertainty(spec)
  
  #print results
  if(verbose){
    cat('\nRedshifts for spectrum number', specNum,'are as follows\n')
    cat("Template\tRedshift\t\tCrossCorr\t\tShiftIndex\n")
    for( peak in peaks ){
      cat(peak$template, peak$redshift, peak$crossCorr, peak$shiftIndex,"\n", sep="\t\t")
    }
    cat("Probabilty on best match is ",spec$prob,"\n")
  }
  
  spec$results        <- c(peaks[[1]]$redshift, peaks[[1]]$crossCorr, peaks[[1]]$template, peaks[[2]]$redshift, 
                           peaks[[2]]$crossCorr, peaks[[2]]$template, peaks[[3]]$crossCorr, peaks[[4]]$crossCorr)
  names(spec$results) <- c('Z','CC_SIGMA','TEMPLATE','Z2','CC_SIGMA2','TEMPLATE2','CC_SIGMA3','CC_SIGMA4')
  TOTALTIME <- proc.time()[3] - TOTALTIME[3]
  spec$timings <- c(TOTALTIME, CROSSTIME, PROCESSTIME)
  names(spec$timings) <- c('totalTime', 'crossTime', 'proccessTime')
  return = spec
}

# Find highest peaks across the cross-correlation functions. 
# Note that in this routine peaks.template and templateList refers
# to the index within the ccinfo structure and NOT the template number that
# is finally assigned. 
# Written by Ivan Baldry.
# Translated to R by Leon Drygalaa
FindHighestPeaks = function(ccinfo, num = 4){
  templateList    <- c(-1)
  templateIDList  <- c(-1)
  indexesList     <- c(-1)
  redshiftList    <- c(-1)
  crosscorrList   <- c(0)
  
  # first extract all peaks in normalised cross correlation value
  # across all templates
  for (i in 1:length(ccinfo)) {
    # cross correlation values of peaks in allowed range (not a peak = 0)
    #testvals <- ccinfo[[i]]$crossCorr * ((ccinfo[[i]]$maskInfo & 5) == 5)#TODO what on earth does this do?
    testvals <- ccinfo[[i]]$crossCorr * (ccinfo[[i]]$maskInfo == 23)#TODO I think this is what it's meant to do...
    # save only certain peaks
    use <- which(testvals >= 1)
    count <- length(use)
    if (count >= 1) {
      templateList    <- append(templateList, rep(0,count) + ccinfo[[i]]$templateNumber)
      templateIDList  <- append(templateIDList, rep(0,count) + i)
      indexesList     <- append(indexesList, use)
      redshiftList    <- append(redshiftList, ccinfo[[i]]$shifts[use])
      crosscorrList   <- append(crosscorrList, testvals[use])
    }
  }
  
  peaks <- vector('list',num)
  for (i in 1:num) {
    # find highest peak remaining
    maxcorr <- max(crosscorrList)
    pt = which(crosscorrList == maxcorr)[[1]]
    #[[1]] ignores any multiple maximums
    peaks[[i]] <- list("template" = templateList[pt], "redshift" = redshiftList[pt], 
                       "crossCorr"=maxcorr, "shiftIndex"=indexesList[pt], "templateID" = templateIDList[pt])
    
    # exclude +- 600 km/s in next search
    zrange  <- (1 + peaks[[i]]$redshift) * (1 + c(-0.002,0.002)) - 1.
    criteriaZRange <- (redshiftList > zrange[1]) & (redshiftList <= zrange[2])
    use <- which(criteriaZRange)
    crosscorrList[use] <- -1.
  }
  return = peaks
}

# Find maximum of yArray and fit quadratic using nearby points.
# Written by Ivan Baldry. 
# Translated by to R Leon Drygala
FindMax = function(xArray, yArray, n = 2){
  length <- length(xArray)
  xPts <- 1:length
  # find highest point of array, integer wise
  maxY <- max(yArray)
  xMaxPt = which(yArray == maxY)[[1]]
  #[[1]] ignores multiple maximums
  yAdjusted <- yArray - maxY
  # xAdjustment is a hack to make lm work properly on small x scales
  # TODO fix hack
  xAdjustment <- xArray[1]
  xAdjusted <- xArray - xAdjustment
  
  # fit quadratic to points +-n of xMaxPt
  usePts <- which( (xPts >= xMaxPt-n) & (xPts <= xMaxPt+n) )
  result <- lm(yAdjusted[usePts] ~ poly(xAdjusted[usePts], 2, raw = TRUE))
  if (result$rank == 3) {
    # Solve for dY/dX <- 0:  2*result$coefficients[[3]]*X + result$coefficients[[2]] = 0
    xCenter = -0.5 * result$coefficients[[2]] / result$coefficients[[3]]
  } else {
    cat('\nFindMax: quadratic fit failed\n')
    xCenter = xArray[xMaxPt]
  }
  return = xCenter + xAdjustment
}

# Output result of polymonial function. 
# For general use with any order of polynomial. 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
PolyCalc = function( coefficients, xValues){
  yValues <- rep(0, length(xValues)) + coefficients[1]
  for( i in 2:length(coefficients))
    yValues <- yValues + coefficients[i]*xValues^(i - 1)
  return = yValues
}

# Calculate figure of merit and and certainty of redshift
# Translated from I. Baldry AutoZ code
# Written by Leon Drygala
CalculateCertainty = function(spec){
  # Calculate root mean square to mean absolute deviation ratio. 
  spec$rmsMadRatio   <- spec$rmsNorm / spec$meanadNorm
  
  # Calculate ratio of first peak compared to RMS of 2nd/3rd/4th. 
  spec$ccSigma1to234 <- spec$ccSigma[1] / rms(spec$ccSigma[2:4])
  
  # The following calibrations were determined for the GAMA survey. 
  # It is unclear how they will perform with different surveys. 
  
  # Define FOM, lower of cc_sigma and a converted cc_sigma1to234.
  spec$ccFOM <- PolyCalc(c(0.4, 2.8), spec$ccSigma1to234)[1]
  if(spec$ccFOM > spec$ccSigma[1]) spec$ccFOM <- spec$ccSigma[1]
  
  # Adjustment to FOM for large rms_mad_ratio. 
  rmrAdjustment <- (1.5 * (spec$rmsMadRatio - 1.8)) 
  if(rmrAdjustment < 0) rmrAdjustment <- 0.0
  if(rmrAdjustment > 2.1) rmrAdjustment <- 2.1
  if(spec$ccFOM - rmrAdjustment < 2.6 ){
    spec$ccFOM <- 2.6
  } else {
    spec$ccFOM <- (spec$ccFOM - rmrAdjustment)
  }
  
  # Set redshift confidence from CC_FOM. 
  x = (spec$ccFOM - 3.70) / 0.70
  spec$prob <- (tanh(x) + 1) / 2
  
  return = spec
}
