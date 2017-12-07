runAutoZ<-function(specs=specs, logName=logName, verbose=verbose, makePlots=T, cores=cores){


    if (specs[1]=='all'){
        specs<-c()
        
        listSpec<-list.files(path='data/reduced/stackedSpec/', pattern='*.Rdata')
        specs<-paste('data/reduced/stackedSpec/', listSpec, sep='')
        
    }
    
    
    registerDoParallel(cores=cores)

    a = foreach(i=1:length(specs)) %dopar%  {
    #for (i in 1:length(specs)){

        if (verbose>1){cat('    - Running AutoZ for spectrum: ',specs[i],  '\n')}
        write(paste('    - Running AutoZ for spectrum: ',specs[i],sep=''), file=logName, append=T)

        load(specs[i])
        
        spec2<-spec
        spec2$flux<-spec$fluxSub
        spec2$error<-spec$sn
        spec2$longitude =149.0661
        spec2$latitude = -31.27704
        spec2$altitude = 1164
        autoz_out<-AutozSingleSpec(spec2,tempFile = 'data/calibrators/AutoZTemp/filtered-templates.fits',doHelio=F, verbose=F)
        
        spec$z<-autoz_out$results[1]
        spec$prob<-autoz_out$prob
        spec$cc<-autoz_out$results[2]
        spec$z2<-autoz_out$results[4]
        spec$cc2<-autoz_out$results[5]
        spec$Temp<-autoz_out$results[3]

        if (verbose>1){cat('        - AutoZ found redshift of ',spec$z,  '\n')}
        write(paste('        - AutoZ found redshift of ',spec$z, sep=''), file=logName, append=T)
        if (verbose>1){cat('        ...with probability of ',spec$prob,  '\n')}
        write(paste('        ...with probability of : ',spec$prob,sep=''), file=logName, append=T)
 
        save(spec,file=specs[i])

        if (makePlots==T){

            if (verbose>1){cat('        - Plotting AutoZ outputs as: ',paste('data/reduced/stackedSpec/AutoZplots/', spec$ID,'.pdf',sep=''),  '\n')}
            write(paste('        - Plotting AutoZ outputs as: data/reduced/stackedSpec/AutoZplots/', spec$ID,'.pdf', sep=''), file=logName, append=T)
            
            pdf(paste('data/reduced/stackedSpec/AutoZplots/', spec$ID,'.pdf',sep=''), width=18, height=18)

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
