checkCal<-function(biasDir=biasDir, darksDir=darksDir, verbose=1){

    if (verbose>0){
        cat('****************************************************************', '\n')
        cat('********** Running checkCal for calibration diagnositcs *********', '\n')
        cat('****************************************************************', '\n')
        cat( '\n')
        
    }

    biasList<-list.files(path=biasDir, pattern='*.fits')   
    biasList<-biasList[which(substr(biasList, nchar(biasList)-12,nchar(biasList)-5)!='combined')]

    darkList<-list.files(path=darksDir, pattern='*.fits')   
    darkList<-darkList[which(substr(darkList, nchar(darkList)-12,nchar(darkList)-5)!='combined')]
    darkList<-darkList[which(substr(darkList, nchar(darkList)-7,nchar(darkList)-5)!='red')]


    File_M<-c()
    CCD_M<-c()
    Exposure_M<-c()
    Gain_M<-c()
    Noise_M<-c()
    mean_M<-c()
    median_M<-c()
    SD_M<-c()
    max_M<-c()
    min_M<-c()

    if (verbose>0){
        cat('- Determining bias stats for directory:', biasDir,  '\n')
        cat('   - ',length(biasList),' biases found in directory', '\n')
        cat('   - running checks....', '\n')
    }
    
    pdf(paste(biasDir,'/', 'biasQC.pdf',sep=''), width=12, height=ceiling(length(biasList)/2.0)*6)

    par(mfrow = c(ceiling(length(biasList)/2.0), 2))
    par(mar=c(3.1,3.1,1.1,1.1))

    layout(matrix(seq(1,length(biasList),1), ceiling(length(biasList)/2.0), 2, byrow = TRUE))

    for (i in 1:length(biasList)){
        tmp<-read.fits(paste(biasDir,'/',biasList[i], sep=''), hdu=1)
        magimage(tmp$dat[[1]], main=biasList[i], xlim=c(-1500,2000), ylim=c(0,4000))
        legend('topleft', legend=c(paste('File: ',biasList[i],sep=''), paste('CCD =',get.fitskey('SPECTID', tmp$hdr[[1]]), sep=''), paste('Grating =',get.fitskey('GRATID', tmp$hdr[[1]]), sep=''), paste('Dectector =',get.fitskey('DETECTOR', tmp$hdr[[1]]), sep=''), paste('Exposure =',get.fitskey('EXPOSED', tmp$hdr[[1]]),'sec', sep=''),  paste('Readout amplifier (e-/ADU) =',get.fitskey('RO_GAIN', tmp$hdr[[1]]), sep=''), paste('Readout noise (electrons) =',get.fitskey('RO_NOISE', tmp$hdr[[1]]), sep=''), paste('Mean Counts =', format(mean(tmp$dat[[1]], na.rm=T), nsmall=3, digits=3), sep=''), paste('Median Counts =', median(tmp$dat[[1]], na.rm=T), sep=''), paste('NA pixels =', length(which(is.finite(tmp$dat[[1]])==F)), sep=''), paste('SD counts =', format(sd(tmp$dat[[1]], na.rm=T), nsmall=3, digits=3), sep=''),  paste('Max counts =', max(tmp$dat[[1]], na.rm=T), sep=''),paste('Min counts =', min(tmp$dat[[1]], na.rm=T), sep='')))

        File_M<-c(File_M, biasList[i])
        CCD_M<-c(CCD_M, get.fitskey('SPECTID', tmp$hdr[[1]]))
        Exposure_M<-c(Exposure_M, get.fitskey('EXPOSED', tmp$hdr[[1]]))
        Gain_M<-c(Gain_M, get.fitskey('RO_GAIN', tmp$hdr[[1]]))
        Noise_M<-c(Noise_M, get.fitskey('RO_NOISE', tmp$hdr[[1]]))
        mean_M<-c(mean_M, mean(tmp$dat[[1]], na.rm=T))
        median_M<-c(median_M, median(tmp$dat[[1]], na.rm=T))
        SD_M<-c(SD_M, sd(tmp$dat[[1]], na.rm=T))
        max_M<-c(max_M, max(tmp$dat[[1]], na.rm=T))
        min_M<-c(min_M, min(tmp$dat[[1]], na.rm=T))
    }

    dev.off()


    pdf(paste(biasDir,'/', 'biasQC_summary.pdf', sep=''), width=15, height=length(File_M)/3)

    tab<-data.frame(File_M,CCD_M,Exposure_M,Gain_M, Noise_M, mean_M,median_M,SD_M, max_M,min_M)
    tt<-tableGrob(tab)
    grid.draw(tt)
    dev.off()


     File_M<-c()
    CCD_M<-c()
    Exposure_M<-c()
    Gain_M<-c()
    Noise_M<-c()
    mean_M<-c()
    median_M<-c()
    SD_M<-c()
    max_M<-c()
    min_M<-c()

    if (verbose>0){
        cat('- Determining darks stats for directory:', darksDir,  '\n')
        cat('   - ',length(darkList),' darks found in directory', '\n')
        cat('   - running checks....', '\n')
    }
    
    
    pdf(paste(darksDir,'/', 'darkQC.pdf', sep=''), width=12, height=ceiling(length(darkList)/2.0)*6)

    par(mfrow = c(ceiling(length(darkList)/2.0), 2))
    par(mar=c(3.1,3.1,1.1,1.1))

    layout(matrix(seq(1,length(darkList),1), ceiling(length(darkList)/2.0), 2, byrow = TRUE))

    for (i in 1:length(darkList)){
        tmp<-read.fits(paste(darksDir,'/',darkList[i], sep=''), hdu=1)
        magimage(tmp$dat[[1]], main=darkList[i], xlim=c(-1500,2000), ylim=c(0,4000))
        legend('topleft', legend=c(paste('File: ',darkList[i],sep=''), paste('CCD =',get.fitskey('SPECTID', tmp$hdr[[1]]), sep=''), paste('Grating =',get.fitskey('GRATID', tmp$hdr[[1]]), sep=''), paste('Dectector =',get.fitskey('DETECTOR', tmp$hdr[[1]]), sep=''), paste('Exposure =',get.fitskey('EXPOSED', tmp$hdr[[1]]),'sec', sep=''),  paste('Readout amplifier (e-/ADU) =',get.fitskey('RO_GAIN', tmp$hdr[[1]]), sep=''), paste('Readout noise (electrons) =',get.fitskey('RO_NOISE', tmp$hdr[[1]]), sep=''), paste('Mean Counts =', format(mean(tmp$dat[[1]], na.rm=T), nsmall=3, digits=3), sep=''), paste('Median Counts =', median(tmp$dat[[1]], na.rm=T), sep=''), paste('NA pixels =', length(which(is.finite(tmp$dat[[1]])==F)), sep=''), paste('SD counts =', format(sd(tmp$dat[[1]], na.rm=T), nsmall=3, digits=3), sep=''),  paste('Max counts =', max(tmp$dat[[1]], na.rm=T), sep=''),paste('Min counts =', min(tmp$dat[[1]], na.rm=T), sep='')))

        File_M<-c(File_M, darkList[i])
        CCD_M<-c(CCD_M, get.fitskey('SPECTID', tmp$hdr[[1]]))
        Exposure_M<-c(Exposure_M, get.fitskey('EXPOSED', tmp$hdr[[1]]))
        Gain_M<-c(Gain_M, get.fitskey('RO_GAIN', tmp$hdr[[1]]))
        Noise_M<-c(Noise_M, get.fitskey('RO_NOISE', tmp$hdr[[1]]))
        mean_M<-c(mean_M, mean(tmp$dat[[1]], na.rm=T))
        median_M<-c(median_M, median(tmp$dat[[1]], na.rm=T))
        SD_M<-c(SD_M, sd(tmp$dat[[1]], na.rm=T))
        max_M<-c(max_M, max(tmp$dat[[1]], na.rm=T))
        min_M<-c(min_M, min(tmp$dat[[1]], na.rm=T))

    }


    dev.off()


    
    pdf(paste(darksDir,'/', 'darkQC_summary.pdf', sep=''), width=15, height=length(File_M)/3)

    tab<-data.frame(File_M,CCD_M,Exposure_M,Gain_M, Noise_M, mean_M,median_M,SD_M, max_M,min_M)
    tt<-tableGrob(tab)
    grid.draw(tt)
    dev.off()


     if (verbose>0){
         cat('**** FINISHED ***',  '\n')
         cat('Bias QC plots saved as: ', paste(darksDir,'/', 'biasQC.pdf', sep=''), '\n')
         cat('Bias QC table saved as: ', paste(darksDir,'/', 'biasQC_summary.pdf', sep=''), '\n')
         cat('Darks QC plots saved as: ', paste(darksDir,'/', 'darkQC.pdf', sep=''), '\n')
         cat('Darks QC table saved as: ', paste(darksDir,'/', 'darkQC_summary.pdf', sep=''), '\n')
    }
    

}
