#' Make AAT master calibration files
#'
#' @description Reduce and combine all bias and dark frames associated with a givne directory. 
#' Function finds the run identifier in the DEVILS data structure that matches the current 
#' night to be reduced. Then identifies the corresponding bais and dark files. In no MASTER 
#' bias and/or dark is present, the function reduces all bias and dark frames and combines
#' to form a master bias and dark.  

#' @param spec An R stucture containing spec$wave = vector of spectrum wavelengths
#' and spec$flux = vector of spectrum fluxes.     
#' @param filter A filter with which to convolve with. A 2d matrix with column
#' 1 as the filter wevelength (in the same units as spec$wave) and column
#' 2 the filter response. Could be prodcued by \code{getfilt}.
#' @param toReduce directory path of raw data that needs to be reduced by TAZ in the DEVILS
#' data structure 
#' @param doCalibQC TRUE/FALSE, run checkCal.R over the found calibration directory.
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @examples 
#' makeCalib(toReduce='data/raw/run1_2017_12/2017_12_18/', doCalibQC=TRUE,logName='tempLog.txt', verbose=1)
#' @export
makeCalib<-function(toReduce=toReduce, doCalibQC=FALSE, logName=logName, verbose=verbose){

 
  
    if (verbose>0){cat('     - Running makeCalib.....', '\n')}

    write('     - Running makeCalib.....', file=logName, append=T)

    
                       
    for (i in 1:length(toReduce)){

        if (verbose>1){cat(paste('        - Finding Calibration for ',toReduce, sep=''), '\n')}
        write(paste('        - Finding Calibration for ',toReduce, sep=''), file=logName, append=T)

       

        darkDir<-paste('data/darks/', strsplit(toReduce, '/')[[1]][3],sep='')
        biasDir<-paste('data/biases/', strsplit(toReduce, '/')[[1]][3],sep='')

        
        
        biasDate<-strsplit(toReduce, '/')[[1]][4]
        darkDate<-strsplit(darkDir, '/')[[1]][3]
        biasFileBlue<-paste(biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep='')
        darkFileBlue<-paste(darkDir,'/',darkDate,'_blue_DARKcombined.fits', sep='')
        biasFileRed<-paste(biasDir,'/',biasDate,'_red_BIAScombined.fits', sep='')
        darkFileRed<-paste(darkDir,'/',darkDate,'_red_DARKcombined.fits', sep='')
        calib<-data.frame(rawDir=toReduce, biasFileBlue=biasFileBlue, darkFileBlue=darkFileBlue, biasFileRed=biasFileRed, darkFileRed=darkFileRed)
        
        if (length(list.files(path=biasDir, pattern=paste(biasDate,'_blue_BIAScombined.fits',sep='')))==0){

            
            write('            - No master bias. Making *BAIScombined.fits......', file=logName, append=T)
            if (verbose>1){cat('            - No master bias. Making *BAIScombined.fits......', '\n')}
            
            reducDay<-as.numeric(strsplit(biasDate, '_')[[1]][3])
            reducMonth<-as.numeric(strsplit(biasDate, '_')[[1]][2])
            reducYear<-as.numeric(strsplit(biasDate, '_')[[1]][1])
            reducMonth<-tolower(substr(months(as.Date(paste(reducYear, '-',reducMonth,'-',reducDay,sep=''))),1,3))
            
            biasfiles<-list.files(path=biasDir, pattern=paste(reducDay,reducMonth,'*',sep=''))
            
            biasfilesBlue<-paste(biasDir,'/',biasfiles[which(substr(biasfiles, 6,6)=='1')],sep='')
            for (j in 1:length(biasfilesBlue)){
                write(paste('              - Reducing Bias File:',biasfilesBlue[j], sep=''), file=logName, append=T)
                system(paste('aaorun reduce_bias ',biasfilesBlue[j], ' ', sep=''))
            }

            biasfilesRed<-paste(biasDir,'/',biasfiles[which(substr(biasfiles, 6,6)=='2')],sep='')
            for (j in 1:length(biasfilesRed)){
                write(paste('              - Reducing Bias File:',biasfilesRed[j], sep=''), file=logName, append=T)
                system(paste('aaorun reduce_bias ',biasfilesRed[j], ' ', sep=''))
            }
            
            count<-0
            while(count<length(biasfiles)){
                count<-length(list.files(path=biasDir, pattern='*red.fits'))
            }
            
            tmp<-list.files(path=biasDir, pattern=paste(reducDay,reducMonth,'*',sep=''))
            tmp<-tmp[which(substr(tmp, nchar(tmp)-7,nchar(tmp)-5)=='red')]
            tmp<-tmp[which(substr(tmp, 6,6)=='1')]
            tmp<-paste(biasDir,'/',tmp, sep='', collapse=' ')
            
            write(paste('              - Making Master Bias:',biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep=''), file=logName, append=T)
            system(paste('aaorun combine_image ',tmp, '  -COMBINEDFILE ',biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep='"'))

            tmp<-list.files(path=biasDir, pattern=paste(reducDay,reducMonth,'*',sep=''))
            tmp<-tmp[which(substr(tmp, nchar(tmp)-7,nchar(tmp)-5)=='red')]
            tmp<-tmp[which(substr(tmp, 6,6)=='2')]
            tmp<-paste(biasDir,'/',tmp, sep='', collapse=' ')
            
            write(paste('              - Making Master Bias:',biasDir,'/',biasDate,'_red_BIAScombined.fits', sep=''), file=logName, append=T)
            system(paste('aaorun combine_image ',tmp, '   -COMBINEDFILE ',biasDir,'/',biasDate,'_red_BIAScombined.fits', sep='"'))



 

        }else{
              write(paste('           - Blue master bias found, using ', biasDir,'/',biasDate,'_blue_BIAScombined.fits',sep=''), file=logName, append=T) 
              write(paste('           - Red master bias found, using ', biasDir,'/',biasDate,'_red_BIAScombined.fits',sep=''), file=logName, append=T) 
            if (verbose>1){cat(paste('            - Blue master bias found, using ', biasDir,'/',biasDate,'_blue_BIAScombined.fits',sep=''), '\n')}
            if (verbose>1){cat(paste('            - Red master bias found, using ', biasDir,'/',biasDate,'_red_BIAScombined.fits',sep=''), '\n')}
        }
        

        biasFileBlue<-paste(biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep='')
        biasFileRed<-paste(biasDir,'/',biasDate,'_red_BIAScombined.fits', sep='')

         if (length(list.files(path=darkDir, pattern='*DARKcombined.fits'))==0){
            
             write('            - No master dark. Making *DARKcombined.fits......', file=logName, append=T)
             if (verbose>1){cat('            - No master dark. Making *DARKcombined.fits......', '\n')}
             darkfiles<-list.files(path=darkDir, pattern='*.fits')
             darkfilesBlue<-paste(darkDir,'/',darkfiles[which(substr(darkfiles, 6,6)=='1')],sep='')
             for (j in 1:length(darkfilesBlue)){
                 write(paste('              - Reducing Dark File:',darkfilesBlue[j], sep=''), file=logName, append=T)
                 system(paste('aaorun reduce_dark ',darkfilesBlue[j], ' -usebiasim 1 -bias_filename ',biasFileBlue,sep=''))
             }

            darkfilesRed<-paste(darkDir,'/',darkfiles[which(substr(darkfiles, 6,6)=='2')],sep='')
             for (j in 1:length(darkfilesRed)){
                 write(paste('              - Reducing Dark File:',darkfilesRed[j], sep=''), file=logName, append=T)
                 system(paste('aaorun reduce_dark ',darkfilesRed[j], '  -usebiasim 1 -bias_filename ',biasFileRed,sep=''))
             }

             
             count<-0
             while(count<length(darkfiles)){
                 count<-length(list.files(path=darkDir, pattern='*red.fits'))
             }
             
             tmp<-list.files(path=darkDir, pattern='*red.fits')
             tmp<-tmp[which(substr(tmp, 6,6)=='1')]
             tmp<-paste(darkDir,'/',tmp, sep='', collapse=' ')
             write(paste('              - Making Master Dark:',darkDir,'/',darkDate,'_blue_DARKcombined.fits', sep=''), file=logName, append=T)
             system(paste('aaorun combine_image ',tmp, '  -COMBINEDFILE ',darkDir,'/',darkDate,'_blue_DARKcombined.fits', sep='"'))

             tmp<-list.files(path=darkDir, pattern='*red.fits')
             tmp<-tmp[which(substr(tmp, 6,6)=='2')]
             tmp<-paste(darkDir,'/',tmp, sep='', collapse=' ')
             write(paste('              - Making Master Dark:',darkDir,'/',darkDate,'_red_DARKcombined.fits', sep=''), file=logName, append=T)
             system(paste('aaorun combine_image ',tmp, '  -COMBINEDFILE ',darkDir,'/',darkDate,'_red_DARKcombined.fits', sep='"'))



                                        #cleanup

             count<-0
               while(count<1){
                 count<-length(list.files(path=darkDir, pattern='*_red_DARKcombined.fits'))
             }
             
             system(paste('rm ',darkDir,'/*red.fits',sep=''))
             system(paste('rm ',biasDir,'/*red.fits',sep=''))
             

         }else{
           
           
           write(paste('            - Blue master dark found, using ', darkDir,'/',darkDate,'_blue_DARKcombined.fits',sep=''), file=logName, append=T)
           write(paste('            - Red master dark found, using ', darkDir,'/',darkDate,'_red_DARKcombined.fits',sep=''), file=logName, append=T)
           if (verbose>1){cat(paste('            - Blue master dark found, using ', darkDir,'/',darkDate,'_blue_DARKcombined.fits',sep=''), '\n')}
           if (verbose>1){cat(paste('            - Red master dark found, using ', darkDir,'/',darkDate,'_red_DARKcombined.fits',sep=''), '\n')}
             }

                                       
        

    }

    if (doCalibQC==T){checkCal(biasDir=biasDir, darksDir=darkDir, verbose=verbose)}
        
    return(calib)
}
