fluxCal<-function(file=file, stdStars=stdStars, logName=logName, verbose=verbose){

    if (verbose>0){cat('   ** Running fluxCal....', '\n')}
    write('   ** Running fluxCal....', file=logName, append=T)

    if (verbose>0){cat('     - Loading sensitivy functions: aaomega_TF580V_X5700.fits, aaomega_TF385R_X5700.fits', '\n')}
    write('     - Loading sensitivy functions: aaomega_TF580V_X5700.fits, aaomega_TF385R_X5700.fits', file=logName, append=T)

    sensBlue<-readFITS('data/calibrators/sensfuncs/aaomega_TF580V_X5700.fits')
    sensBlueF<-sensBlue$imDat
    CRVAL1 <- as.numeric(sensBlue$hdr[which(sensBlue$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(sensBlue$hdr[which(sensBlue$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(sensBlue$hdr[which(sensBlue$hdr=="CDELT1")+1])
    sensBlueW<-CRVAL1+((c(1:dim(sensBlue$imDat)[1])-CRPIX1)*CDELT1)
    sensBlueF[which(sensBlueF<1.0e-4)]<-1.0e-4

    sensRed<-readFITS('data/calibrators/sensfuncs/aaomega_TF385R_X5700.fits')
    sensRedF<-sensRed$imDat
    CRVAL1 <- as.numeric(sensRed$hdr[which(sensRed$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(sensRed$hdr[which(sensRed$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(sensRed$hdr[which(sensRed$hdr=="CDELT1")+1])
    sensRedW<-CRVAL1+((c(1:dim(sensRed$imDat)[1])-CRPIX1)*CDELT1)
    sensRedF[which(sensRedF<1.0e-4)]<-1.0e-4

    
    std_ID<-stdStars[,'ID']
    std_RA<-stdStars[,'RA']
    std_DEC<-stdStars[,'DEC']
    std_magG<-stdStars[,'magG']
    std_magR<-stdStars[,'magR']
    std_magI<-stdStars[,'magI']
    std_magG_filter<-stdStars[,'magG_filter']
    std_magR_filter<-stdStars[,'magR_filter']
    std_magI_filter<-stdStars[,'magI_filter']
    

    if (verbose>0){cat('     - Loading data....', '\n')}
    write('     - Loading data....', file=logName, append=T)


    tab <- readFITS(file=file,hdu=3)
    im <- readFITS(file=file,hdu=1)
    CRVAL1 <- as.numeric(im$hdr[which(im$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(im$hdr[which(im$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(im$hdr[which(im$hdr=="CDELT1")+1])
    wave <- CRVAL1+((c(1:dim(im$imDat)[1])-CRPIX1)*CDELT1)

    
    fileBlue<-paste( paste(strsplit(file, '/')[[1]][1:4], sep='',collapse='/'), '/ccd1/', strsplit(strsplit(file, '/')[[1]][5], '.fits')[[1]][1], '_blue.fits', sep='') 

    imBlue <- readFITS(file=fileBlue,hdu=1)
    CRVAL1Blue <- as.numeric(imBlue$hdr[which(imBlue$hdr=="CRVAL1")+1])
    CRPIX1Blue <- as.numeric(imBlue$hdr[which(imBlue$hdr=="CRPIX1")+1])
    CDELT1Blue <- as.numeric(imBlue$hdr[which(imBlue$hdr=="CDELT1")+1])
    waveBlue <- CRVAL1Blue+((c(1:dim(imBlue$imDat)[1])-CRPIX1Blue)*CDELT1Blue)


    fileRed<-paste( paste(strsplit(file, '/')[[1]][1:4], sep='',collapse='/'), '/ccd2/', strsplit(strsplit(file, '/')[[1]][5], '.fits')[[1]][1], '_red.fits', sep='') 


    imRed <- readFITS(file=fileRed,hdu=1)
    CRVAL1Red <- as.numeric(imRed$hdr[which(imRed$hdr=="CRVAL1")+1])
    CRPIX1Red <- as.numeric(imRed$hdr[which(imRed$hdr=="CRPIX1")+1])
    CDELT1Red <- as.numeric(imRed$hdr[which(imRed$hdr=="CDELT1")+1])
    waveRed <- CRVAL1Red+((c(1:dim(imRed$imDat)[1])-CRPIX1Red)*CDELT1Red)

    if (verbose>0){cat('     - Finding standards....', '\n')}
    write('     - Finding standards....', file=logName, append=T)

    stdNum<-which(substr(tab$col[which(tab$colNames=='NAME')][[1]], 1,1)=='S')

    mag_diffG<-c(1:length(stdNum))
    mag_diffR<-c(1:length(stdNum))
    mag_diffI<-c(1:length(stdNum))
    fluxScG<-c(1:length(stdNum))
    fluxScR<-c(1:length(stdNum))
    fluxScI<-c(1:length(stdNum))
    
    for (i in 1:length(stdNum)){

        if (verbose>0){cat('        - Finding match for standrard ', i, ' of ',length(stdNum), '\n')}
        write(paste('        - Finding match for standrard ', i, ' of ',length(stdNum), sep=''), file=logName, append=T)

        RA<-tab$col[which(tab$colNames=='RA')][[1]]*(180/pi)
        DEC<-tab$col[which(tab$colNames=='DEC')][[1]]*(180/pi)

        dist<-sqrt((std_RA-RA[stdNum[i]])^2+(std_DEC-DEC[stdNum[i]])^2)
        matchStd<-which(dist==min(dist))
        
        fluxSpec<-im$imDat[,stdNum[i]]  
        fluxBlue<-imBlue$imDat[,stdNum[i]]/sensBlueF
        fluxRed<-imRed$imDat[,stdNum[i]]/sensRedF

        stdSpec<-data.frame(flux=fluxSpec, wave=wave, xunit='ang', yunit='ang')
        stdSpecBlue<-data.frame(flux=fluxBlue, wave=waveBlue, xunit='ang', yunit='ang')
        stdSpecRed<-data.frame(flux=fluxRed, wave=waveRed, xunit='ang', yunit='ang')

         if (verbose>0){cat('        - calculating standard magnitudes...', '\n')}
        write('        - calculating standard magnitudes...', file=logName, append=T)

        obs_stdMagG<-magABspec(stdSpecBlue, filter=as.character(std_magG_filter[matchStd]))
        obs_stdMagR<-magABspec(stdSpec, filter=as.character(std_magR_filter[matchStd]))
        obs_stdMagI<-magABspec(stdSpecRed, filter=as.character(std_magI_filter[matchStd]))

        mag_diffG[i] = (obs_stdMagG-std_magG[matchStd])
        mag_diffR[i] = (obs_stdMagR-std_magR[matchStd])
        mag_diffI[i] = (obs_stdMagI-std_magI[matchStd])


        wavefac=1e-10
        c<-299792458

         if (verbose>0){cat('        - calculating flux scaling for data...', '\n')}
        write('        - calculating flux scaling for data...', file=logName, append=T)

        ### Blue Flux Scaling ####
        filter=getfilt(as.character(std_magG_filter[matchStd]))[,2:3]
        filt_wave <- filter[,1]
        filt_trans <- filter[,2]

        interp<-approx(filt_wave, filt_trans, stdSpecBlue$wave)
        filt_trans_interp <- interp$y
        filt_trans_interp[is.na(filt_trans_interp)] = 0

        fluxnu=(wavefac*stdSpecBlue$flux*stdSpecBlue$wave^2)/c

        flux_conv <- fluxnu*filt_trans_interp
 
        temp <- fluxnu*filt_trans_interp*stdSpecBlue$wave
        temp2 <- filt_trans_interp*stdSpecBlue$wave
        flux_filt <- sum(temp[which(is.finite(temp)==T)])/sum(temp2[which(is.finite(temp2)==T)])

        
        flux_need_G<- 10.^(-0.4*(std_magG[matchStd]+48.6))

        flux_sc_Hz <- fluxnu*(flux_need_G/flux_filt)
        flux_sc <- (flux_sc_Hz*(2.998e18))/(stdSpecBlue$wave^2)
       
        
        fluxScG[i]=median(flux_sc/stdSpecBlue$flux,na.rm=T)
##############

         ### Comb Flux Scaling ####
        filter=getfilt(as.character(std_magR_filter[matchStd]))[,2:3]
        filt_wave <- filter[,1]
        filt_trans <- filter[,2]

        interp<-approx(filt_wave, filt_trans, stdSpec$wave)
        filt_trans_interp <- interp$y
        filt_trans_interp[is.na(filt_trans_interp)] = 0

        fluxnu=(wavefac*stdSpec$flux*stdSpec$wave^2)/c

        flux_conv <- fluxnu*filt_trans_interp

        temp <- fluxnu*filt_trans_interp*stdSpec$wave
        temp2 <- filt_trans_interp*stdSpec$wave
        flux_filt <- sum(temp[which(is.finite(temp)==T)])/sum(temp2[which(is.finite(temp2)==T)])

        
        flux_need_R<- 10.^(-0.4*(std_magR[matchStd]+48.6))

        flux_sc_Hz <- fluxnu*(flux_need_R/flux_filt)
        flux_sc <- (flux_sc_Hz*(2.998e18))/(stdSpec$wave^2)
       
        
        fluxScR[i]=median(flux_sc/stdSpec$flux,na.rm=T)
##############

         ### Red Flux Scaling ####
        filter=getfilt(as.character(std_magI_filter[matchStd]))[,2:3]
        filt_wave <- filter[,1]
        filt_trans <- filter[,2]

        interp<-approx(filt_wave, filt_trans, stdSpecRed$wave)
        filt_trans_interp <- interp$y
        filt_trans_interp[is.na(filt_trans_interp)] = 0

        fluxnu=(wavefac*stdSpecRed$flux*stdSpecRed$wave^2)/c

        flux_conv <- fluxnu*filt_trans_interp

        temp <- fluxnu*filt_trans_interp*stdSpecRed$wave
        temp2 <- filt_trans_interp*stdSpecRed$wave
        flux_filt <- sum(temp[which(is.finite(temp)==T)])/sum(temp2[which(is.finite(temp2)==T)])

        
        flux_need_I<- 10.^(-0.4*(std_magI[matchStd]+48.6))

        flux_sc_Hz <- fluxnu*(flux_need_I/flux_filt)
        flux_sc <- (flux_sc_Hz*(2.998e18))/(stdSpecRed$wave^2)
       
        
        fluxScI[i]=median(flux_sc/stdSpecRed$flux,na.rm=T)
##############

        

        }

    ZP<-median(mag_diffR, na.rm=T)
    ZPMAD<-mad(mag_diffR, na.rm=T)
    ZPRMS<-sd(mag_diffR, na.rm=T)
    ZPNUM<-length(which(is.finite(mag_diffR))==T)
    FLUXSC<-median(fluxScR, na.rm=T)

    ZP_blue<-median(mag_diffG, na.rm=T)
    ZPMAD_blue<-mad(mag_diffG, na.rm=T)
    ZPRMS_blue<-sd(mag_diffG, na.rm=T)
    ZPNUM_blue<-length(which(is.finite(mag_diffG))==T)
    FLUXSC_blue<-median(fluxScG, na.rm=T)
    
    ZP_red<-median(mag_diffI, na.rm=T)
    ZPMAD_red<-mad(mag_diffI, na.rm=T)
    ZPRMS_red<-sd(mag_diffI, na.rm=T)
    ZPNUM_red<-length(which(is.finite(mag_diffI))==T)
    FLUXSC_red<-median(fluxScI, na.rm=T)

        if (verbose>0){cat('     - Recording zeropoint data as:', paste(strsplit(file, '.fits')[[1]][1],'_zeroPoints.Rdata',sep=''), '\n')}
        write(paste('     - Recording zeropoint data as:', strsplit(file, '.fits')[[1]][1],'_zeroPoints.Rdata',sep=''), file=logName, append=T)


    zeroPoints<-data.frame(ZP=ZP,ZPMAD=ZPMAD, ZPRMS=ZPRMS, ZPNUM=ZPNUM, FLUXSC=FLUXSC, ZP_red=ZP_red,ZPMAD_red=ZPMAD_red, ZPRM_redS=ZPRMS_red, ZPNUM_red=ZPNUM_red, FLUXSC_red=FLUXSC_red,ZP_blue=ZP_blue,ZPMAD_blue=ZPMAD_blue, ZPRM_blue=ZPRMS_blue, ZPNUM_blue=ZPNUM_blue, FLUXSC_blue=FLUXSC_blue)

    save(zeroPoints, file=paste(strsplit(file, '.fits')[[1]][1],'_zeroPoints.Rdata', sep=''))
   
    
    
    #write.fitskey(key=c("ZP","ZPMAD", "ZPRMS", "ZPNUM", "FLUXSC"), value=c(ZP,ZPMAD, ZPRMS, ZPNUM, FLUXSC), file=file,comment=c("Magnitude zero point","Zero point median average deviation", "Zero point standard deviation", "Number of sources used in zero point calculation", "Flux scale from pixels to ergs/sec/cm^2/ang"), hdu=1)

    #write.fitskey(key=c("ZP","ZPMAD", "ZPRMS", "ZPNUM", "FLUXSC"), value=c(ZP_blue,ZPMAD_blue, ZPRMS_blue, ZPNUM_blue, FLUXSC_blue), file=fileBlue, comment=c("Magnitude zero point","Zero point median average deviation", "Zero point standard deviation", "Number of sources used in zero point calculation", "Flux scale from pixels to ergs/sec/cm^2/ang"), hdu=1)

    #write.fitskey(key=c("ZP","ZPMAD", "ZPRMS", "ZPNUM", "FLUXSC"), value=c(ZP_red,ZPMAD_red, ZPRMS_red, ZPNUM_red, FLUXSC_red), file=fileRed, comment=c("Magnitude zero point","Zero point median average deviation", "Zero point standard deviation", "Number of sources used in zero point calculation", "Flux scale from pixels to ergs/sec/cm^2/ang"), hdu=1)
    
}
