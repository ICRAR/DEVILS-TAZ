runAutoZ<-function(specs=specs, logName=logName, verbose=verbose, makePlots=T){

    for (i in 1:length(specs)){

        if (verbose>1){cat('    - Running AutoZ for spectrum: ',specs[i],  '\n')}
        write(paste('    - Running AutoZ for spectrum: ',specs[i],sep=''), file=logName, append=T)

        load(specs[i])
        
        spec2<-spec
        spec2$flux<-spec$fluxSub
        spec2$error<-spec$sn
        spec2$longitude =149.0661
        spec2$latitude = -31.27704
        spec2$altitude = 1164
        autoz_out<-AutozSingleSpec(spec2,spec2$wave,tempFile = 'data/calibrators/AutoZTemp/filtered-templates.fits',doHelio=F, verbose=F)
        
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
            
            pdf(paste('data/reduced/stackedSpec/AutoZplots/', spec$ID,'.pdf',sep=''), width=20, height=12)

            par(mfrow = c(2, 4))
            par(mar=c(3.1,3.1,1.1,1.1))

            layout(matrix(c(1,1,1,1,2,3,4,5), 2, 4, byrow = TRUE))

            lines<-load.lines()
            line_x <- as.numeric(lines$wave_ang)*(1+spec$z)
            peak<-max(spec$flux, na.rm=T)
            
            magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak*-1.2,peak*1.2), lwd=2, main=paste(spec$ID, ' - Hanning Smoothed, degree=9',sep=''))
            
            plotLines(z=spec$z, xunit='ang', labPos=0.8*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-50)
            
            
            legend('bottomright', legend=c(paste('ID=',spec$ID,sep=''), paste('z=',spec$z,sep=''), paste('mag=',spec$MAG,sep=''), paste('Prob=',spec$prob,sep=''), paste('TEXP=',spec$EXP,sep='')), bg='white')

            magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*3727+c(-150,150), main='OII', lwd=2)
            plotLines(z=spec$z, xunit='ang', labPos=0.3*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

            
             magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+spec$z)*3950+c(-500,500), main='Ca H&K', lwd=2)

            plotLines(z=spec$z, xunit='ang', labPos=0.3*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-30)
            
            magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l',xlim=(1+spec$z)*5007+c(-500,500),main='OII H-beta', lwd=2)

            plotLines(z=spec$z, xunit='ang', labPos=0.3*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-30)

            magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l',xlim=(1+spec$z)*6568+c(-300,300),main='H-alpha', lwd=2)

            plotLines(z=spec$z, xunit='ang', labPos=0.3*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-15)
            
            

            dev.off()

            }
        
    }

}
