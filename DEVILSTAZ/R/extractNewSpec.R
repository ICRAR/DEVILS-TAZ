#' Extract 1D spectra from a reduced FITS file 
#'
#' @description Function extracts 1D spectra from a reduced 2dF+AAOmega frame and
#' writes these to the TAZ data structure. Internal function to TAZ
#' 
#' @param file reduced 2dF+AAOmega frame to extract spectra from
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param makePlot make 1D plots of extracted spectra
#' @param zeroPoint TRUE/FALSE apply pre-calcuated (from fluxCal.R) zeroPoint flux scaling 
#' @param doCosmic  TRUE/FALSE do additonal cosmic ray rejection 
#' @examples 
#'extractNewSpec(file='objected2df_red.fits', logName='tmpLog.txt', verbose=1, makePlot=F, zeroPoint=F)
#' @export
extractNewSpec<-function(file=file, logName=logName, verbose=verbose, makePlot=F, zeroPoint=T, NowDate=NowDate, doCosmic=doCosmic){

    
    if (zeroPoint==T){
        
        if (verbose>0){cat('    -Loading zeroPoint data from: ', paste(strsplit(file, '.fits')[[1]][1],'_zeroPoints.Rdata', sep=''), '\n')}
        write(paste('    -Loading zeroPoint data from: ', strsplit(file, '.fits')[[1]][1],'_zeroPoints.Rdata',sep=''), file=logName, append=T)
        load(paste(strsplit(file, '.fits')[[1]][1],'_zeroPoints.Rdata', sep=''))

    }

    if (zeroPoint==F){
        if (verbose>0){cat('    - zeroPoint==F - no zeropoint scaling applied', '\n')}
        write('    - zeroPoint==F - no zeropoint scaling applied', file=logName, append=T)

    }

    if (verbose>0){cat('     - Loading sensitivity functions: aaomega_TF580V_X5700.fits, aaomega_TF385R_X5700.fits', '\n')}
    write('     - Loading sensitivity functions: aaomega_TF580V_X5700.fits, aaomega_TF385R_X5700.fits', file=logName, append=T)

    sensBlue<-readFITS('data/calibrators/sensfuncs/aaomega_TF580V_X6700.fits')
    sensBlueF<-sensBlue$imDat
    CRVAL1 <- as.numeric(sensBlue$hdr[which(sensBlue$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(sensBlue$hdr[which(sensBlue$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(sensBlue$hdr[which(sensBlue$hdr=="CDELT1")+1])
    sensBlueW<-CRVAL1+((c(1:dim(sensBlue$imDat)[1])-CRPIX1)*CDELT1)
    sensBlueF[which(sensBlueF<1.0e-4)]<-1.0e-4

    sensRed<-readFITS('data/calibrators/sensfuncs/aaomega_TF385R_X6700.fits')
    sensRedF<-sensRed$imDat
    CRVAL1 <- as.numeric(sensRed$hdr[which(sensRed$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(sensRed$hdr[which(sensRed$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(sensRed$hdr[which(sensRed$hdr=="CDELT1")+1])
    sensRedW<-CRVAL1+((c(1:dim(sensRed$imDat)[1])-CRPIX1)*CDELT1)
    sensRedF[which(sensRedF<1.0e-4)]<-1.0e-4
 
    
    system(paste('mkdir ', strsplit(file, '.fits')[[1]][1],'_spec', sep=''))

    dir<-paste(strsplit(file, '.fits')[[1]][1],'_spec', sep='')
    date<-strsplit(file, '/')[[1]][4]
    #date2<-paste(strsplit(file, '/')[[1]][3],'/',strsplit(file, '/')[[1]][4],sep='')

    if (verbose>0){cat('     - Loading data....', '\n')}
    write('     - Loading data....', file=logName, append=T)


    tab <- readFITS(file=file,hdu=3)
    im <- readFITS(file=file,hdu=1)
    sn <- readFITS(file=file,hdu=2)
    sky <- readFITS(file=file,hdu=8)
    CRVAL1 <- as.numeric(im$hdr[which(im$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(im$hdr[which(im$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(im$hdr[which(im$hdr=="CDELT1")+1])
    wave <- CRVAL1+((c(1:dim(im$imDat)[1])-CRPIX1)*CDELT1)
    UTMJD<-as.double(tab$hdr[which(tab$hdr=='CONFMJD')+1])
    CONFIG<-im$hdr[which(im$hdr=="CFG_FILE")+1]
    EXP<-im$hdr[which(im$hdr=="EXPOSED")+1]
    
    if (doCosmic==T) {
        RO_GAIN<-as.numeric(im$hdr[which(im$hdr=="RO_GAIN")+1])
        if (verbose>0){cat('     - Running Cosmic rejection....', '\n')}
        write('     - Running Cosmic rejection....', file=logName, append=T)
        
        CosSub<-RCosmic(im$imDat, im$hdr, sn$imDat, sigma_det=5, rlim=0.8, iter=6, fwhm_gauss=2.0, gain=RO_GAIN, verbose=FALSE)
        im$imDat<-CosSub
        if (verbose>0){cat('     - Finished Cosmic rejection.', '\n')}
    }
 
    
    
    if (zeroPoint==T){fluxSc<-zeroPoints$FLUXSC}else{fluxSc<-1}
    

    fileBlue<-paste(paste(strsplit(file, '/')[[1]][1:4], sep='',collapse='/'), '/ccd1/', strsplit(strsplit(file, '/')[[1]][5], '.fits')[[1]][1], '_blue.fits', sep='') 

    imBlue <- readFITS(file=fileBlue,hdu=1)
    snBlue <- readFITS(file=fileBlue,hdu=2)
    skyBlue <- readFITS(file=fileBlue,hdu=8)
    CRVAL1Blue <- as.numeric(imBlue$hdr[which(imBlue$hdr=="CRVAL1")+1])
    CRPIX1Blue <- as.numeric(imBlue$hdr[which(imBlue$hdr=="CRPIX1")+1])
    CDELT1Blue <- as.numeric(imBlue$hdr[which(imBlue$hdr=="CDELT1")+1])
    waveBlue <- CRVAL1Blue+((c(1:dim(imBlue$imDat)[1])-CRPIX1Blue)*CDELT1Blue)
  
    if (doCosmic==T) {
        if (verbose>0){cat('     - Running Cosmic rejection for Blue CCD....', '\n')}
        RO_GAIN<-as.numeric(imBlue$hdr[which(imBlue$hdr=="RO_GAIN")+1])
        CosSub<-RCosmic(imBlue$imDat, imBlue$hdr, snBlue$imDat, sigma_det=5, rlim=1.0, iter=6, fwhm_gauss=2.0, gain=RO_GAIN, verbose=FALSE)
        CosMaskBlue<-array(1,dim=dim(imBlue$imDat))
        CosMaskBlue[which(is.na(CosSub)==T & is.na(imBlue$imDat)==F, arr.ind = TRUE)]<-NA
        imBlue$imDat<-CosSub
        if (verbose>0){cat('     - Finished Cosmic rejection for Blue CCD.', '\n')}
    }

    if (zeroPoint==T){fluxScBlue<-zeroPoints$FLUXSC_blue}else{fluxScBlue<-1}

    fileRed<-paste( paste(strsplit(file, '/')[[1]][1:4], sep='',collapse='/'), '/ccd2/', strsplit(strsplit(file, '/')[[1]][5], '.fits')[[1]][1], '_red.fits', sep='') 


    imRed <- readFITS(file=fileRed,hdu=1)
    snRed <- readFITS(file=fileRed,hdu=2)
    skyRed <- readFITS(file=fileRed,hdu=8)
    CRVAL1Red <- as.numeric(imRed$hdr[which(imRed$hdr=="CRVAL1")+1])
    CRPIX1Red <- as.numeric(imRed$hdr[which(imRed$hdr=="CRPIX1")+1])
    CDELT1Red <- as.numeric(imRed$hdr[which(imRed$hdr=="CDELT1")+1])
    waveRed <- CRVAL1Red+((c(1:dim(imRed$imDat)[1])-CRPIX1Red)*CDELT1Red)
  
    if (doCosmic==T) {
        if (verbose>0){cat('     - Running Cosmic rejection for Red CCD....', '\n')}
        RO_GAIN<-as.numeric(imRed$hdr[which(imRed$hdr=="RO_GAIN")+1])
        CosSub<-RCosmic(imRed$imDat, imRed$hdr, snRed$imDat, sigma_det=5, rlim=1.0, iter=6, fwhm_gauss=2.0, gain=RO_GAIN, verbose=FALSE)
        CosMaskRed<-array(1,dim=dim(imRed$imDat))
        CosMaskRed[which(is.na(CosSub)==T & is.na(imRed$imDat)==F, arr.ind = TRUE)]<-NA
        imRed$imDat<-CosSub
        if (verbose>0){cat('     - Finished Cosmic rejection for Red CCD.', '\n')}
     }
    
    write('     - Finished Cosmic rejection.', file=logName, append=T)
    
    if (zeroPoint==T){fluxScRed<-zeroPoints$FLUXSC_red}else{fluxScRed<-1}

    
    
    newSpec<-c()
    newID<-c()



    ID_M<-c()
    RA_M<-c()
    DEC_M<-c()
    X_M<-c()
    Y_M<-c()
    XERR_M<-c()
    YERR_M<-c()
    THETA_M<-c()
    TYPE_M<-c()
    PIVOT_M<-c()
    MAG_M<-c()
    FIBRE_M<-c()
    FILE_M<-c()
    EXP_M<-c()
   

    if (verbose>0){cat('     - Looping over spectra....', '\n')}
    write('     - Looping over spectra.....', file=logName, append=T)

    

    for (j in 1:dim(im$imDat)[2]) {
        specName<-strsplit(tab$col[[which(tab$colNames=='NAME')]][j],' ')[[1]][1]

        if (verbose>0){cat('       - Extracting spectrum ', j, ' of ',dim(im$imDat)[2],': ',specName, '\n')}
        write(paste('       - Extracting spectrum ', j, ' of ',dim(im$imDat)[2],': ',specName,sep=''), file=logName, append=T)
        
        if (specName!='FIBRE'){

            if (verbose>0){cat('         - Getting spectrum meta information....', '\n')}
            write('         - Getting spectrum meta information.....', file=logName, append=T)
            
            RA<-tab$col[which(tab$colNames=='RA')][[1]][j]*(180/pi)
            DEC<-tab$col[which(tab$colNames=='DEC')][[1]][j]*(180/pi)
            X<-tab$col[which(tab$colNames=='X')][[1]][j]
            Y<-tab$col[which(tab$colNames=='Y')][[1]][j]
            XERR<-tab$col[which(tab$colNames=='XERR')][[1]][j]
            YERR<-tab$col[which(tab$colNames=='YERR')][[1]][j]
            THETA<-tab$col[which(tab$colNames=='THETA')][[1]][j]
            TYPE<-tab$col[which(tab$colNames=='TYPE')][[1]][j]

            if (TYPE=='P' & substr(specName,1,1)!='S'){TYPE<-'target'}
            if (TYPE=='P' & substr(specName,1,1)=='S'){TYPE<-'std'}
            if (TYPE=='F'){TYPE<-'guide'}
            if (TYPE=='S'){TYPE<-'sky'}
            
            PIVOT<-tab$col[which(tab$colNames=='PIVOT')][[1]][j]
            MAG<-tab$col[which(tab$colNames=='MAGNITUDE')][[1]][j]
            FIBRE<-j
    
            fluxSpec<-im$imDat[,j]
            snSpec <- sn$imDat[,j]
            skySpec <- sky$imDat

          
            fluxBlue<-(imBlue$imDat[,j]/sensBlueF)
            snBlueS <- snBlue$imDat[,j]
            skyBlueS <- skyBlue$imDat

            fluxRed<-(imRed$imDat[,j]/sensRedF)
            snRedS <- snRed$imDat[,j]
            skyRedS <- skyRed$imDat
            
            if (doCosmic==T) {
                MaskWaveBlue<-waveBlue[which(is.na(CosMaskBlue[,j])==T)]
                MaskWaveRed<-waveRed[which(is.na(CosMaskRed[,j])==T)]
                for (k in 1:length(MaskWaveBlue)){
                  fluxSpec[which(abs(wave-MaskWaveBlue[k])==min(abs(wave-MaskWaveBlue[k])))]<-NA
                }
                for (k in 1:length(MaskWaveRed)){
                  fluxSpec[which(abs(wave-MaskWaveRed[k])==min(abs(wave-MaskWaveRed[k])))]<-NA
                }
            }
      

            if (length(which(is.finite(fluxSpec)==T))>0){ 
                
                spec<-list(wave=wave,flux=fluxSpec,sn=snSpec,sky=skySpec, ID=specName, origfile=file, RA=RA, DEC=DEC, X=X, Y=Y,XERR=XERR,YERR=YERR,THETA=THETA, TYPE=TYPE,PIVOT=PIVOT,MAG=MAG, FIBRE=FIBRE, UTMJD=UTMJD,DATE=date, CONFIG=CONFIG, xunit='ang', yunit='ang', z=NA, EXP=EXP, waveBlue=waveBlue, fluxBlue=fluxBlue, snBlue=snBlueS, skyBlue=skyBlueS, waveRed=waveRed, fluxRed=fluxRed, snRed=snRedS, skyRed=skyRedS, fluxSc=fluxSc,fluxScBlue=fluxScBlue,fluxScRed=fluxScRed)



                ID_M<-c(ID_M,specName)
                RA_M<-c(RA_M,RA)
                DEC_M<-c(DEC_M,DEC)
                X_M<-c(X_M,X)
                Y_M<-c(Y_M,Y)
                XERR_M<-c(XERR_M,XERR)
                YERR_M<-c(YERR_M,YERR)
                THETA_M<-c(THETA_M,THETA)
                TYPE_M<-c(TYPE_M,TYPE)
                PIVOT_M<-c(PIVOT_M,PIVOT)
                MAG_M<-c(MAG_M,MAG)
                FIBRE_M<-c(FIBRE_M,FIBRE)
                FILE_M<-c(FILE_M, paste(dir,'/', date, '_',specName,'.Rdata',sep=''))
                EXP_M<-c(EXP_M,EXP)


                if (verbose>0){cat('         - Saving spectrum to ', paste(dir,'/', date, '_',specName,'.Rdata',sep=''), '\n')}
                write(paste('         - Saving spectrum to ', dir,'/', date, '_',specName,'.Rdata',sep=''), file=logName, append=T)

                save(spec, file=paste(dir,'/', date, '_',specName,'.Rdata',sep=''))
                system(paste('cp ', dir,'/', date, '_',specName,'.Rdata', ' data/reduced/allSpec/',sep=''))
              

                if (makePlot==T){

                    if (verbose>0){cat('         - Plotting spectrum to ', paste(dir,'/', date, '_',specName,'.pdf',sep=''), '\n')}
                    write(paste('         - Saving spectrum to ', dir,'/', date, '_',specName,'.pdf',sep=''), file=logName, append=T)
                    
                    peak<-max(spec$flux[which(spec$wave>6000)], na.rm=T)
                    pdf(paste(dir,'/', date, '_',specName,'.pdf',sep=''), width=12,height=5)
                    magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak*-2,peak*2.0))
                    lines(spec$waveBlue, spec$fluxBlue, col='blue')
                    lines(spec$waveRed, spec$fluxRed, col='red')
                    tmpSn<-(spec$sn/max(spec$sn, na.rm=T))*0.4*peak
                    tmpSky<-(spec$sky/max(spec$sky, na.rm=T))*0.4*peak
                    lines(spec$wave, tmpSn, col='indianred2')
                    lines(spec$wave, tmpSky, col='darkgreen')
                    legend('bottomright', legend=c(paste('ID=',spec$ID,sep=''), paste('z=',spec$z,sep=''), paste('mag=',spec$MAG,sep='')), bg='white')
                    dev.off()
                }



                
                newSpec<-c(newSpec, paste(dir,'/', date, '_',specName,sep=''))
                newID<-c(newID, specName)
                
                write(specName, file=paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newIDs.csv', sep=''), append=T)
                
            }
        }                              
        
    }


    if (verbose>0){cat('    - Saving meta information of spectra to ',paste(strsplit(file, '.fits')[[1]][1],'_meta.Rdata',sep=''), '\n')}
    write(paste('    - Saving spectrum to ', strsplit(file, '.fits')[[1]][1],'_meta.Rdata',sep=''), file=logName, append=T)

    meta<-data.frame(ID=ID_M,RA=RA_M, DEC=DEC_M, X=X_M, Y=Y_M,XERR=XERR_M,YERR=YERR_M,THETA=THETA_M, TYPE=TYPE_M,MAG=MAG_M, FIBRE=FIBRE_M, FILE=FILE_M, EXP=EXP_M)
    save(meta, file=paste(strsplit(file, '.fits')[[1]][1],'_meta.Rdata', sep=''))

    out<-data.frame(newSpec=newSpec,newID=newID)
    return(out)

}
