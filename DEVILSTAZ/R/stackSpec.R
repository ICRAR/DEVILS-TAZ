stackSpec<-function(ids=ids, logName=logName, verbose=verbose, makePlot=T){

    newStacks<-c()
    
    for (i in 1:length(ids)){

        listSpec<-list.files(path='data/reduced/allSpec/', pattern=paste('*',ids[i],'.Rdata',sep=''))

        
        
        for (j in 1:length(listSpec)){

            load(paste('data/reduced/allSpec/',listSpec[j],sep=''))

            if (j==1){

                ID<-spec$ID
                RA<-spec$RA
                DEC<-spec$DEC
                MAG<-spec$MAG
                UTMJD<-spec$UTMJD
                
                wave<-spec$wave
                waveBlue<-spec$waveBlue   
                waveRed<-spec$waveRed

                vcorr<-Heliocentric(RA, DEC, epoch = 2000.0, tai = UTMJD, longitude =149.0661, latitude = -31.27704, altitude = 1164)
                wave<-wave+(wave*(vcorr/2.99792458e5))
                waveBlue<-waveBlue+(waveBlue*(vcorr/2.99792458e5))
                waveRed<-waveRed+(waveRed*(vcorr/2.99792458e5))
                
                flux<-spec$flux
                fluxBlue<-spec$fluxBlue
                fluxRed<-spec$fluxRed
                sn<-spec$sn
                snBlue<-spec$snBlue
                snRed<-spec$snRed
                sky<-spec$sky
                skyBlue<-spec$skyBlue
                skyRed<-spec$skyRed
                EXP<-spec$EXP
                
                fit1 <- lm(flux~ poly(wave, 10, raw=TRUE))
                fluxSub<- flux-predict(fit1, data.frame(x=wave))
                for (kk in 1:4) {
                    fit1 <- lm(fluxSub ~ poly(wave, 10, raw=TRUE))
                    fluxSub <- fluxSub-predict(fit1, data.frame(x=wave))
                }
   
                fit1 <- lm(fluxBlue~ poly(waveBlue, 10, raw=TRUE))
                fluxSubBlue<- fluxBlue-predict(fit1, data.frame(x=waveBlue))
                for (kk in 1:4) {
                    fit1 <- lm(fluxSubBlue ~ poly(waveBlue, 10, raw=TRUE))
                    fluxSubBlue <- fluxSubBlue-predict(fit1, data.frame(x=waveBlue))
                }
              

                fit1 <- lm(fluxRed~ poly(waveRed, 10, raw=TRUE))
                fluxSubRed<- fluxRed-predict(fit1, data.frame(x=waveRed))
                for (kk in 1:4) {
                    fit1 <- lm(fluxSubRed ~ poly(waveRed, 10, raw=TRUE))
                    fluxSubRed <- fluxSubRed-predict(fit1, data.frame(x=waveRed))
                }
        
                sc<-1
                scBlue<-1
                scRed<-1
                fluxSc=spec$fluxSc
                fluxScBlue=spec$fluxScBlue
                fluxScRed=spec$fluxScRed

             
                
            }

            if (j>1){

                wave_N<-spec$wave
                waveBlue_N<-spec$waveBlue   
                waveRed_N<-spec$waveRed

                vcorr<-Heliocentric(RA, DEC, epoch = 2000.0, tai = UTMJD, longitude =149.0661, latitude = -31.27704, altitude = 1164)
                wave_N<-wave_N+(wave_N*(vcorr/2.99792458e5))
                waveBlue_N<-waveBlue_N+(waveBlue_N*(vcorr/2.99792458e5))
                waveRed_N<-waveRed_N+(waveRed_N*(vcorr/2.99792458e5))
                
                
                flux_N<-spec$flux
                fluxBlue_N<-spec$fluxBlue
                fluxRed_N<-spec$fluxRed
                sn_N<-spec$sn
                snBlue_N<-spec$snBlue
                snRed_N<-spec$snRed
                EXP_n<-spec$EXP

                fluxInt<-approx(wave_N, flux_N, wave)$y
                fluxIntBlue<-approx(waveBlue_N, fluxBlue_N, waveBlue)$y
                fluxIntRed<-approx(waveRed_N, fluxRed_N, waveRed)$y

                snInt<-approx(wave_N, sn_N, wave)$y
                snIntBlue<-approx(waveBlue_N, snBlue_N, waveBlue)$y
                snIntRed<-approx(waveRed_N, snRed_N, waveRed)$y

                fit1 <- lm(fluxInt ~ poly(wave, 6, raw=TRUE))
                fluxSub_N<- fluxInt-predict(fit1, data.frame(x=wave))
                for (kk in 1:2) {
                    fit1 <- lm(fluxSub_N~ poly(wave, 6, raw=TRUE))
                    fluxSub_N <- fluxSub_N-predict(fit1, data.frame(x=wave))
                }
                
                fit1 <- lm(fluxIntBlue~ poly(waveBlue, 6, raw=TRUE))
                fluxSubBlue_N<- fluxIntBlue-predict(fit1, data.frame(x=waveBlue))
                for (kk in 1:2) {
                    fit1 <- lm(fluxSubBlue_N~ poly(waveBlue, 6, raw=TRUE))
                    fluxSubBlue_N <- fluxSubBlue_N-predict(fit1, data.frame(x=waveBlue))
                }

                
                fit1 <- lm(fluxRed ~ poly(waveRed, 6, raw=TRUE))
                fluxSubRed_N<- fluxIntRed-predict(fit1, data.frame(x=waveRed))
                 for (kk in 1:2) {
                    fit1 <- lm(fluxSubRed_N~ poly(waveRed, 6, raw=TRUE))
                    fluxSubRed_N <- fluxSubRed_N-predict(fit1, data.frame(x=waveRed))
                }
                

                weight<-sn/snInt
                weightBlue<-snBlue/snIntBlue
                weightRed<-snRed/snIntRed
                
                sc<-sc+weight
                scBlue<-scBlue+weightBlue
                scRed<-scRed+weightRed

               
                flux<-(flux+(fluxInt*weight))
                fluxBlue<-(fluxBlue+(fluxIntBlue*weightBlue))
                fluxRed<-(fluxRed+(fluxIntRed*weightRed))


                fluxSub<-(fluxSub+(fluxSub_N*weight))
                fluxSubBlue<-(fluxSubBlue+(fluxSubBlue_N*weightBlue))
                fluxSubRed<-(fluxSubRed+(fluxSubRed_N*weightRed))

                sn<-sn+(snInt*weight)
                snBlue<-snBlue+(snIntBlue*weightBlue)
                snRed<-snRed+(snIntRed*weightRed)

                
            }
            


        }

        flux<-flux/sc
      
        fluxBlue<-fluxBlue/scBlue
        fluxRed<-fluxRed/scRed

        fluxSub<-fluxSub/sc
        fluxSubBlue<-fluxSubBlue/scBlue
        fluxSubRed<-fluxSubRed/scRed
        
        sn<-sn/(sc*sqrt(length(listSpec)))
        snBlue<-snBlue/(scBlue*sqrt(length(listSpec)))
        snRed<-snRed/(scRed*sqrt(length(listSpec)))

        NStack<-length(listSpec)
        

        spec<-list(wave=wave,flux=flux,sn=sn,sky=sky, ID=ID, RA=RA, DEC=DEC, MAG=MAG, xunit='ang', yunit='ang', z=NA, EXP=EXP, NStack=NStack,waveBlue=waveBlue, fluxBlue=fluxBlue, snBlue=snBlue, skyBlue=skyBlue, waveRed=waveRed, fluxRed=fluxRed, snRed=snRed, skyRed=skyRed, fluxSub=fluxSub, fluxSubBlue=fluxSubBlue, fluxSubRed=fluxSubRed, file=paste('data/reduced/stackedSpec/',ID,'.Rdata',sep=''), fluxSc=fluxSc,fluxScBlue=fluxScBlue,fluxScRed=fluxScRed)

        save(spec, file=paste('data/reduced/stackedSpec/',ID,'.Rdata',sep=''))
        newStacks<-c(newStacks, paste('data/reduced/stackedSpec/',ID,'.Rdata',sep=''))

        specStack<-spec
        
        if (makePlot==T){

            peak<-max(spec$flux[which(spec$wave>6000)], na.rm=T)
            pdf(paste('data/reduced/stackedSpec/plots/',ID,'.pdf',sep=''), width=12,height=5*(NStack)+2)

            par(mfrow = c(NStack+2, 1))
            par(mar=c(3.1,3.1,1.1,1.1))
            layout(matrix(seq(1,NStack+2,1), NStack+2, 1, byrow = TRUE))
          
            for (j in 1:length(listSpec)){

                load(paste('data/reduced/allSpec/',listSpec[j],sep=''))
                magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak*-2,peak*2.0), main=listSpec[j])
                
            }

            

            magplot(specStack$wave, hanning.smooth(specStack$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak*-2,peak*2.0), main='Stacked')
            #lines(specStack$waveBlue, specStack$fluxBlue, col='blue')
            #lines(specStack$waveRed, specStack$fluxRed, col='red')
            tmpSn<-(specStack$sn/max(specStack$sn, na.rm=T))*0.4*peak
            tmpSky<-(specStack$sky/max(specStack$sky, na.rm=T))*0.4*peak
            lines(specStack$wave, tmpSn, col='indianred2')
            lines(specStack$wave, tmpSky, col='darkgreen')
            legend('bottomright', legend=c(paste('ID=',spec$ID,sep=''), paste('z=',specStack$z,sep=''), paste('mag=',specStack$MAG,sep=''), paste('Nstack=',NStack,sep='')), bg='white')

            magplot(specStack$wave, hanning.smooth(specStack$fluxSub, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak*-2,peak*2.0), main='Stacked - cont Subracted')
            #lines(specStack$waveBlue, hanning.smooth(specStack$fluxSubBlue, degree=9), col='blue')
            #lines(specStack$waveRed, hanning.smooth(specStack$fluxSubRed, degree=9), col='red')
            tmpSn<-(specStack$sn/max(specStack$sn, na.rm=T))*0.4*peak
            tmpSky<-(specStack$sky/max(specStack$sky, na.rm=T))*0.4*peak
            lines(specStack$wave, tmpSn, col='indianred2')
            lines(specStack$wave, tmpSky, col='darkgreen')
            legend('bottomright', legend=c(paste('ID=',spec$ID,sep=''), paste('z=',specStack$z,sep=''), paste('mag=',specStack$MAG,sep=''), paste('Nstack=',NStack,sep='')), bg='white')
            
            dev.off()

            }
        
    }


    return(newStacks)
}
