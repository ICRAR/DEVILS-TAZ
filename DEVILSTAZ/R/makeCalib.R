makeCalib<-function(toReduce=toReduce, logName=logName, verbose=verbose){

    if (verbose>0){cat('  - Running makeCalib.....', '\n')}

    write('  - Running makeCalib.....', file=logName, append=T)
                       
    for (i in 1:length(toReduce)){

        if (verbose>1){cat(paste('  - Finding Calibration for ',toReduce, sep=''), '\n')}
        write(paste('  - Finding Calibration for ',toReduce, sep=''), file=logName, append=T)

       

        darkDir<-paste('data/darks/', strsplit(toReduce, '/')[[1]][3],sep='')
        biasDir<-paste('data/biases/', strsplit(toReduce, '/')[[1]][3],sep='')
        
        biasDate<-strsplit(biasDir, '/')[[1]][3]
        darkDate<-strsplit(darkDir, '/')[[1]][3]
        biasFileBlue<-paste(biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep='')
        darkFileBlue<-paste(darkDir,'/',darkDate,'_blue_DARKcombined.fits', sep='')
        biasFileRed<-paste(biasDir,'/',biasDate,'_red_BIAScombined.fits', sep='')
        darkFileRed<-paste(darkDir,'/',darkDate,'_red_DARKcombined.fits', sep='')
        calib<-data.frame(rawDir=toReduce, biasFileBlue=biasFileBlue, darkFileBlue=darkFileBlue, biasFileRed=biasFileRed, darkFileRed=darkFileRed)
        
        if (length(list.files(path=biasDir, pattern='*BIAScombined.fits'))==0){

            
            write('         - No master bias. Making *BAIScombined.fits......', file=logName, append=T)
            if (verbose>1){cat('         - No master bias. Making *BAIScombined.fits......', '\n')}
            
            biasfiles<-list.files(path=biasDir, pattern='*.fits')
            biasfilesBlue<-paste(biasDir,'/',biasfiles[which(substr(biasfiles, 6,6)=='1')],sep='')
            for (j in 1:length(biasfilesBlue)){
                write(paste('           - Reducing Bias File:',biasfilesBlue[j], sep=''), file=logName, append=T)
                system(paste('aaorun reduce_bias ',biasfilesBlue[j], ' -idxfile data/idxFiles/ozdes_blue.idx', sep=''))
            }

            biasfilesRed<-paste(biasDir,'/',biasfiles[which(substr(biasfiles, 6,6)=='2')],sep='')
            for (j in 1:length(biasfilesRed)){
                write(paste('           - Reducing Bias File:',biasfilesRed[j], sep=''), file=logName, append=T)
                system(paste('aaorun reduce_bias ',biasfilesRed[j], ' -idxfile data/idxFiles/ozdes_red.idx', sep=''))
            }
            
            count<-0
            while(count<length(biasfiles)){
                count<-length(list.files(path=biasDir, pattern='*red.fits'))
            }
            
            tmp<-list.files(path=biasDir, pattern='*red.fits')
            tmp<-tmp[which(substr(tmp, 6,6)=='1')]
            tmp<-paste(biasDir,'/',tmp, sep='', collapse=' ')
            
            write(paste('           - Making Master Bias:',biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep=''), file=logName, append=T)
            system(paste('aaorun combine_image ',tmp, ' -idxfile data/idxFiles/ozdes_blue.idx -COMBINEDFILE ',biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep='"'))

            tmp<-list.files(path=biasDir, pattern='*red.fits')
            tmp<-tmp[which(substr(tmp, 6,6)=='2')]
            tmp<-paste(biasDir,'/',tmp, sep='', collapse=' ')
            
            write(paste('           - Making Master Bias:',biasDir,'/',biasDate,'_red_BIAScombined.fits', sep=''), file=logName, append=T)
            system(paste('aaorun combine_image ',tmp, ' -idxfile data/idxFiles/ozdes_red.idx -COMBINEDFILE ',biasDir,'/',biasDate,'_red_BIAScombined.fits', sep='"'))



 

        }else{
            write(paste('         - Master bias found, using ', biasDir,'/',biasDate,'_BIAScombined.fits',sep=''), file=logName, append=T)
            if (verbose>1){cat(paste('         - Master bias found, using ', biasDir,'/',biasDate,'_BIAScombined.fits',sep=''), '\n')}
        }
        

        biasFileBlue<-paste(biasDir,'/',biasDate,'_blue_BIAScombined.fits', sep='')
        biasFileRed<-paste(biasDir,'/',biasDate,'_red_BIAScombined.fits', sep='')

         if (length(list.files(path=darkDir, pattern='*DARKcombined.fits'))==0){
            
             write('         - No master dark. Making *DARKcombined.fits......', file=logName, append=T)
             if (verbose>1){cat('         - No master dark. Making *DARKcombined.fits......', '\n')}
             darkfiles<-list.files(path=darkDir, pattern='*.fits')
             darkfilesBlue<-paste(darkDir,'/',darkfiles[which(substr(darkfiles, 6,6)=='1')],sep='')
             for (j in 1:length(darkfilesBlue)){
                 write(paste('           - Reducing Dark File:',darkfilesBlue[j], sep=''), file=logName, append=T)
                 system(paste('aaorun reduce_dark ',darkfilesBlue[j], ' -idxfile data/idxFiles/ozdes_blue.idx -usebiasim 1 -bias_filename ',biasFileBlue,sep=''))
             }

              darkfilesRed<-paste(darkDir,'/',darkfiles[which(substr(darkfiles, 6,6)=='2')],sep='')
             for (j in 1:length(darkfilesRed)){
                 write(paste('           - Reducing Dark File:',darkfilesRed[j], sep=''), file=logName, append=T)
                 system(paste('aaorun reduce_dark ',darkfilesRed[j], ' -idxfile data/idxFiles/ozdes_red.idx -usebiasim 1 -bias_filename ',biasFileRed,sep=''))
             }

             
             count<-0
             while(count<length(darkfiles)){
                 count<-length(list.files(path=darkDir, pattern='*red.fits'))
             }
             
             tmp<-list.files(path=darkDir, pattern='*red.fits')
             tmp<-tmp[which(substr(tmp, 6,6)=='1')]
             tmp<-paste(darkDir,'/',tmp, sep='', collapse=' ')
             write(paste('           - Making Master Dark:',darkDir,'/',darkDate,'_blue_DARKcombined.fits', sep=''), file=logName, append=T)
             system(paste('aaorun combine_image ',tmp, ' -idxfile data/idxFiles/ozdes_blue.idx -COMBINEDFILE ',darkDir,'/',darkDate,'_blue_DARKcombined.fits', sep='"'))

             tmp<-list.files(path=darkDir, pattern='*red.fits')
             tmp<-tmp[which(substr(tmp, 6,6)=='2')]
             tmp<-paste(darkDir,'/',tmp, sep='', collapse=' ')
             write(paste('           - Making Master Dark:',darkDir,'/',darkDate,'_red_DARKcombined.fits', sep=''), file=logName, append=T)
             system(paste('aaorun combine_image ',tmp, ' -idxfile data/idxFiles/ozdes_red.idx -COMBINEDFILE ',darkDir,'/',darkDate,'_red_DARKcombined.fits', sep='"'))



                                        #cleanup

             count<-0
               while(count<1){
                 count<-length(list.files(path=darkDir, pattern='*_red_DARKcombined.fits'))
             }
             
             system(paste('rm ',darkDir,'/',darkDate,'*red.fits'),sep='')
             system(paste('rm ',biasDir,'/',biasDate,'*red.fits'),sep='')
             

         }else{
             write(paste('         - Master dark found, using ', darkDir,'/',darkDate,'_DARKcombined.fits',sep=''), file=logName, append=T)
             if (verbose>1){cat(paste('         - Master dark found, using ', darkDir,'/',darkDate,'_DARKcombined.fits',sep=''), '\n')}
             }

                                       
        

    }
    return(calib)
}
