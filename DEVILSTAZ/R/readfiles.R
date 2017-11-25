# Read in and rebin template data to new scale. 
# Select templates to use. Wavelengths are in vacuum as per SDSS. 
# Written in IDL by Ivan Baldry and Translated to R by Leon Drygala
RebinTempData = function(logLambda, templateNumbers = c(2:14, 16:22,40:47), 
                         file = "../autozResouces/autoz-code/data/filtered-templates.fits",
                         num_templates, verbose = TRUE){
  # set default template numbers to use: 20 stellar and 8 galaxy templates
  
  if(verbose) cat('\nREBIN_TEMPLATE_DATA: Reading ', file)
  #tdata <- readFITS(file)
  data('filtered_templates', package='auto.z.v03')
  
  specData <- vector("list", length(templateNumbers))
  templateID <- vector("integer", length(templateNumbers))
  count <- 1
  for (j in templateNumbers){
    i = which(tdata$col[[1]] == j)
    #TODO ADD CHECK TO MAKE SURE SPECIFIED TEMP EXISTS
    numPoints <- tdata$col[[4]][i]
    templateID[count] <- i
    specData[[count]] <- list("lambda"=tdata$col[[5]][i,],"flux" = tdata$col[[7]][i,],
                              "redshift"=tdata$col[[3]][i],"specName" = tdata$col[[2]][i],"numPoints" = numPoints, "templateNum" = tdata$col[[1]][i])
    count <- count + 1
  }
  names(specData) <- c(tdata$col[[2]][templateID])
  
  numTemplates <- length(specData)
  totalNumTemplates <- tdata$hdr[which(tdata$hdr=='NAXIS2')+1]
  if(verbose){
    cat('\nREBIN_TEMPLATE_DATA: Using ',numTemplates,' out of ', totalNumTemplates,
        '. Specifically using templates numbered: ', templateNumbers)
  }
  
  outdata <- list(specData,'log space angstroms','', file, numTemplates, templateNumbers)
  names(outdata) <- c('specData', 'xunit', 'yunit', 'file', 'numTemplates', 'templateNumbers')
  
  #interpolate each template to parameter logLambda
  for (i in 1:numTemplates) {
    if (outdata$specData[[i]]$lambda[1] > 0){
      specRebin <- approx(x = outdata$specData[[i]]$lambda, y = outdata$specData[[i]]$flux,
                          xout = logLambda, method = "linear",rule=2)#TODO check interpol is correct
      outdata$specData[[i]]$lambda <- specRebin$x
      outdata$specData[[i]]$flux <- specRebin$y
    }
  }
  return = outdata
}
# Returns 'spec' which should contain all relevent information about a single spec 
GetSingleSpec = function(file = "../autozResouces/SG23_Y7_003.fits", colmn=1, logw=F, sc=1, xunit='ang', 
                         yunit='ang', z=NA, verbose = TRUE) { 
  #Read in first level of file
  specf<-readFITS(file, hdu = 1)  
  # Attempt to read information from header
  if(missing(z) && !(z <- GetHeaderValue(specf$hdr, 'Z')) )
    cat('\nGetSingleSpec(): Can\'t find z in header and not supplied')
  
  if ( !(UTMJD <- GetHeaderValue(specf$hdr, 'UTMJD')) )
    cat('\nGetSingleSpec(): Can\'t find UTMJD in header')
  
  if ( !(latitude <- GetHeaderValue(specf$hdr, 'LAT_OBS')) )
    cat('\nGetSingleSpec(): Can\'t find latitude in header')
  
  if ( !(longitude <- GetHeaderValue(specf$hdr, 'LONG_OBS')) )
    cat('\nGetSingleSpec(): Can\'t find longitude in header')
  
  if ( !(altitude <- GetHeaderValue(specf$hdr, 'ALT_OBS')) )
    cat('\nGetSingleSpec(): Can\'t find altitude in header')
  
  if (length(dim(specf$imDat))>0) 
    flux <- specf$imDat[,colmn]*sc
  
  if (length(dim(specf$imDat))==0) 
    flux <- specf$imDat*sc
  
  lambda <- specf$axDat$crval[1] + (c(1:(specf$axDat$len[1]) - specf$axDat$crpix[1]  ) * specf$axDat$cdelt[1])
  
  # Read in second line of file, extract variance
  specf2    <- readFITS(file, hdu = 2)
  if (length(dim(specf2$imDat))>0) {
    variance <- specf2$imDat[,colmn]*sc
  }
  if (length(dim(specf2$imDat))==0) {
    variance <- specf2$imDat*sc
  }
  
  # Read in third line of file, extract name, RA, DEC, comment. Also pin lat/long info to structure
  specf3    <- readFITS(file, hdu = 3)
  info      <- c(specf3$col[[1]][colmn], specf3$col[[2]][colmn], specf3$col[[3]][colmn], 
                 specf3$col[[13]][colmn], UTMJD, latitude, longitude, altitude)
  infoNames <- c(specf3$colNames[c(1:3,13)], 'UTMJD', 'latitude', 'longitude', 'altitude')
  
  if (logw==T) {lambda<-10.^lambda}  
  spec <- list(lambda, flux, xunit, yunit, z, sqrt(variance), info[1], as.numeric(info[2]), as.numeric(info[3]), 
               info[4], as.numeric(info[5]), as.numeric(info[6]), as.numeric(info[7]), as.numeric(info[8]))
  names(spec) <- c('lambda', 'flux', 'xunit', 'yunit', 'z', 'error', infoNames)
  return = spec      
}


# Returns 'specAll' which should contain all relevent information about a all spectrums
GetAllSpec = function(file = "../autozResouces/SG23_Y7_003.fits", logw=F, sc=1, xunit='ang', yunit='ang', 
                      z=NA, verbose = TRUE) { 
  #Read in first three levels of file
  specf     <- readFITS(file, hdu = 1)  #contains flux data
  specf2    <- readFITS(file, hdu = 2)  #contains varience
  specf3    <- readFITS(file, hdu = 3)  #contains misc data eg RA DEC
  
  # Attempt to read information from header
  if(missing(z) && !(z <- GetHeaderValue(specf$hdr, 'Z')) )
    cat('\nGetAllSpec(): Can\'t find z in header and not supplied')
  
  if ( !(UTMJD <- GetHeaderValue(specf$hdr, 'UTMJD')) )
    cat('\nGetAllSpec(): Can\'t find UTMJD in header')
  
  if ( !(latitude <- GetHeaderValue(specf$hdr, 'LAT_OBS')) )
    cat('\nGetAllSpec(): Can\'t find latitude in header')
  
  if ( !(longitude <- GetHeaderValue(specf$hdr, 'LONG_OBS')) )
    cat('\nGetAllSpec(): Can\'t find longitude in header')
  
  if ( !(altitude <- GetHeaderValue(specf$hdr, 'ALT_OBS')) )
    cat('\nGetAllSpec(): Can\'t find altitude in header')
  
  lambda <- specf$axDat$crval[1] + (c(1:(specf$axDat$len[1]) - specf$axDat$crpix[1]  ) * specf$axDat$cdelt[1])
  if (logw==T) 
    lambda <- 10.^lambda
  
  #load in spectrums (fluxes)
  dimensions <- dim(specf$imDat)
  # Select target fibres only. 
  indexUse <- which(specf3$col[[9]] == 'P')
  specAll <- vector("list", dimensions[2])
  # TODO not sure if all files will have the same dimension format .. ? test with other files
  # note the below for loops checks the name of a spectrum, if it starts with 'G' it should be used
  for(colmn in indexUse){
    flux <- specf$imDat[,colmn]*sc
    variance <- specf2$imDat[,colmn]*sc
    
    # Using third line of file, extract name, RA, DEC, comment. Also pin lat/long info to structure
    info      <- c(specf3$col[[1]][colmn], specf3$col[[2]][colmn], specf3$col[[3]][colmn], 
                   specf3$col[[13]][colmn], UTMJD, latitude, longitude, altitude)
    infoNames <- c(specf3$colNames[c(1:3,13)], 'UTMJD', 'latitude', 'longitude', 'altitude')
    
    spec <- list(flux, xunit, yunit, z, suppressWarnings(sqrt(variance)), info[1], as.numeric(info[2]), as.numeric(info[3]), 
                 info[4], as.numeric(info[5]), as.numeric(info[6]), as.numeric(info[7]), as.numeric(info[8]))
    names(spec) <- c('flux', 'xunit', 'yunit', 'z', 'error', infoNames)
    specAll[[colmn]] <- spec  
  }
  
  specAll$lambda <- lambda
  specAll$indexUse <- indexUse
  return = specAll  
}

# Returns numeric value from a header array. Will return the
# Will return the following value after valueName has been found
# in the array.
# Returns FALSE if value not found
# Written by Leon Drygala
GetHeaderValue = function(header, valueName){
  if (length(as.numeric(header[which(header==valueName)+1])) > 0) {
    return = as.numeric(header[which(header==valueName)+1])
  } else {
    return = FALSE
  }
}
