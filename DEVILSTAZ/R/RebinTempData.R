RebinTempData = function(logLambda, templateNumbers = c(2:14, 16:22,40:47),
                         num_templates, verbose = TRUE){
  # set default template numbers to use: 20 stellar and 8 galaxy templates
  
  if(verbose) cat('\nREBIN_TEMPLATE_DATA: Reading template file \n')

  data('filtered_templates', package='auto.z')
  
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
