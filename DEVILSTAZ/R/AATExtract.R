#' Extract 1D spectra from reduced 2dF+AAOmega Frame
#'
#' This function extracts 1D spectra from a 2dF+AAOmega frame,
#' runs automatic redshifting using AutoZ, and produced 1D diagnositc 
#' plots for all spectra. Function will produce a directory called
#' *fileN*_extracted/ which will contain all extracted spectra.
#' The function can be run is parallel with a specified number of 
#' cores.
#'
#' @param fileN Path to the input 2dF+AAOmega reduced file to be extracted
#' @param cores Number of cores to run over
#' @examples 
#' AATExtract(fileN='object2dF_red.fits', cores=4)
#' @export
AATExtract<-function(fileN=fileN, cores=1){

    registerDoParallel(cores=cores)

    system(paste('mkdir ',substr(fileN, 1, nchar(fileN)-5),'_extracted/',sep=''))
    
    tab <- readFITS(file=fileN,hdu=3)
    im <- readFITS(file=fileN,hdu=1)
    sn <- readFITS(file=fileN,hdu=2)
    sky <- readFITS(file=fileN,hdu=8)
    CRVAL1 <- as.numeric(im$hdr[which(im$hdr=="CRVAL1")+1])
    CRPIX1 <- as.numeric(im$hdr[which(im$hdr=="CRPIX1")+1])
    CDELT1 <- as.numeric(im$hdr[which(im$hdr=="CDELT1")+1])
    wave <- CRVAL1+((c(1:dim(im$imDat)[1])-CRPIX1)*CDELT1)
    UTMJD<-as.double(tab$hdr[which(tab$hdr=='CONFMJD')+1])
    CONFIG<-im$hdr[which(im$hdr=="CFG_FILE")+1]
    EXP<-im$hdr[which(im$hdr=="EXPOSED")+1]

    cat(' Loading data....', '\n')


    cat(' Looping over spectra....', '\n')

    a = foreach(j=1:dim(im$imDat)[2]) %dopar%  {
    #for (j in 1:dim(im$imDat)[2]) {

        specName<-strsplit(tab$col[[which(tab$colNames=='NAME')]][j],' ')[[1]][1]
        cat('       - Extracting spectrum ', j, ' of ',dim(im$imDat)[2],': ',specName, '\n')

        
        if (specName!='FIBRE'){

            cat('         - Getting spectrum meta information....', '\n')
            
            
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

            

            if (length(which(is.finite(fluxSpec)==T))>0){ 
                
                spec<-list(wave=wave,flux=fluxSpec,sn=snSpec,sky=skySpec, ID=specName, origfile=file, RA=RA, DEC=DEC, X=X, Y=Y,XERR=XERR,YERR=YERR,THETA=THETA, TYPE=TYPE,PIVOT=PIVOT,MAG=MAG, FIBRE=FIBRE, UTMJD=UTMJD,DATE=date, CONFIG=CONFIG, xunit='ang', yunit='ang', z=NA, EXP=EXP)


                cat('       - Running AutoZ for spectrum....', '\n')
                
                spec$error<-spec$sn
                spec$longitude =149.0661
                spec$latitude = -31.27704
                spec$altitude = 1164
                autoz_out<-AutozSingleSpec(spec,tempFile = 'data/calibrators/AutoZTemp/filtered-templates.fits',doHelio=T, verbose=F)
                
                spec$z<-autoz_out$results[1]
                spec$prob<-autoz_out$prob
                spec$cc<-autoz_out$results[2]
                spec$z2<-autoz_out$results[4]
                spec$cc2<-autoz_out$results[5]
                spec$Temp<-autoz_out$results[3]

                cat('       - Plotting spectrum....', '\n')

                pdf(paste(substr(fileN, 1, nchar(fileN)-5),'_extracted/', spec$ID,'.pdf',sep=''), width=18, height=18)

                par(mfrow = c(3, 3))
                par(mar=c(3.1,3.1,1.1,1.1))

                layout(matrix(c(1,1,1,2,3,4, 5, 6,7), 3, 3, byrow = TRUE))

                
                lines<-load.lines()
                line_x <- as.numeric(lines$wave_ang)*(1+spec$z)
                peak_F<-max(spec$flux, na.rm=T)
                
                magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak_F*-1.2,peak_F*1.2), lwd=2, main=paste(spec$ID, ' - Hanning Smoothed, degree=9',sep=''))
                
                plotLines(z=spec$z, xunit='ang', labPos=0.8*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-50)
                
                
                legend('bottomright', legend=c(paste('ID=',spec$ID,sep=''), paste('z=',spec$z,sep=''), paste('mag=',spec$MAG,sep=''), paste('Prob=',spec$prob,sep=''), paste('TEXP=',spec$EXP,sep='')), bg='white')

                degSmooth<-7
                
                range<-250
                wavePoint<-3727
                peak<-1.2*max(spec$flux[which(spec$wave > ((1+spec$z)*wavePoint)-range & spec$wave < ((1+spec$z)*wavePoint)+range)],na.rm=T)
                if (is.finite(peak)==F){peak=peak_F}
                magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='OII', lwd=2)
                plotLines(z=spec$z, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

                range<-500
                wavePoint<-4950
                peak<-1.2*max(spec$flux[which(spec$wave > ((1+spec$z)*wavePoint)-range & spec$wave < ((1+spec$z)*wavePoint)+range)],na.rm=T)
                if (is.finite(peak)==F){peak=peak_F}
                magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='OIII/H-beta', lwd=2)
                plotLines(z=spec$z, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

                range<-300
                wavePoint<-6650
                peak<-1.2*max(spec$flux[which(spec$wave > ((1+spec$z)*wavePoint)-range & spec$wave < ((1+spec$z)*wavePoint)+range)],na.rm=T)
                if (is.finite(peak)==F){peak=peak_F}
                magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='H-alpha, NII, SII', lwd=2)
                plotLines(z=spec$z, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)
                
                range<-500
                wavePoint<-4150
                peak<-1.2*max(spec$flux[which(spec$wave > ((1+spec$z)*wavePoint)-range & spec$wave < ((1+spec$z)*wavePoint)+range)],na.rm=T)
                if (is.finite(peak)==F){peak=peak_F}
                magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='Ca H&K, G-band', lwd=2)
                plotLines(z=spec$z, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

                
                range<-250
                wavePoint<-5175
                peak<-1.2*max(spec$flux[which(spec$wave > ((1+spec$z)*wavePoint)-range & spec$wave < ((1+spec$z)*wavePoint)+range)],na.rm=T)
                if (is.finite(peak)==F){peak=peak_F}
                magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='Mg', lwd=2)
                plotLines(z=spec$z, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

                range<-250
                wavePoint<-5895
                peak<-1.2*max(spec$flux[which(spec$wave > ((1+spec$z)*wavePoint)-range & spec$wave < ((1+spec$z)*wavePoint)+range)],na.rm=T)
                if (is.finite(peak)==F){peak=peak_F}
                magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='Na', lwd=2)
                plotLines(z=spec$z, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)
                
                

                dev.off()


            }



        }
        
    }
}
