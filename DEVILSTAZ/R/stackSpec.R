#' Stack repeat obervations with 2dF
#'
#' @description This is the highlevel main TAZ function for stacking spectra. Function searches for 
#' repeat observations of a given ID in the DEVILS directory structure, and inverse variance weights
#' the full splice spectrum, each ccd arm individually, and the continuum extracted spectrum. Will also
#' produce diagnositc output plots for each spectrum to show individual spectra and stacks.
#'  
#' @param ids vector list of IDs to stack. Can be a list of IDs or set to 'all' which will stack all unique IDs in
#' the 'data/reduced/allSpec/' directory.
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param makePlot TRUE/FALSE, make diagnositc plots of each spectrum. These wil be written to data/reduced/stackedSpec/plots/
#' @examples 
#' stackSpec(ids='all',  logName='tempLog.txt', verbose=1,makePlots=T)
#' @export
stackSpec<-function(ids=ids, logName=logName, verbose=verbose, makePlot=T, cores=cores){

  
  registerDoParallel(cores=cores)
    
  if (ids[1]=='all'){
        ids<-c()
        
        listSpec<-list.files(path='data/reduced/allSpec/', pattern=paste('*.Rdata',sep=''))
        for (j in 1:length(listSpec)){
            tmp<-strsplit(listSpec[j], '_')[[1]][4]
            tmp<-strsplit(tmp, '.Rdata')[[1]][1]
            ids<-c(ids, tmp)

        }
    }
    

    newStacks<-as.character(paste('data/reduced/stackedSpec/',ids,'.Rdata',sep=''))
    
    
    #a = foreach(i=1:length(ids)) %dopar%  {
    for (i in 1:length(ids)){

        if (verbose>1){cat('    - Stacking spectrum: ', ids[i], '\n')}
        write(paste('    - Stacking spectrum: ', ids[i],sep=''), file=logName, append=T)

        listSpec<-list.files(path='data/reduced/allSpec/', pattern=paste('*',ids[i],'.Rdata',sep=''))


        if (verbose>1){cat('       - Found ', length(listSpec), ' matching spectra', '\n')}
        write(paste('       - Found ', length(ids[i]), ' matching spectra',sep=''), file=logName, append=T)
        
        
        for (j in 1:length(listSpec)){

            load(paste('data/reduced/allSpec/',listSpec[j],sep=''))

            if (j==1){

                if (verbose>1){cat('           - Loading spectrum... ', j,  '\n')}
                write(paste('           - Loading spectrum...', j, sep=''), file=logName, append=T)

                ID<-spec$ID
                RA<-spec$RA
                DEC<-spec$DEC
                MAG<-spec$MAG
                UTMJD<-spec$UTMJD
                
                wave<-spec$wave
                waveBlue<-spec$waveBlue   
                waveRed<-spec$waveRed

                if (verbose>1){cat('           - Calculating helocentric velcoity correction for spec...',j, '\n')}
                write(paste('           - Calculating helocentric velcoity correction for spec...',j ,sep=''), file=logName, append=T)

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

                if (verbose>1){cat('           - Extracting continuum of spec...',j, '\n')}
                write(paste('           - Extracting continuum of spec...',j, sep=''), file=logName, append=T)
                
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

                if (verbose>1){cat('           - Loading spectrum... ', j,  '\n')}
                write(paste('           - Loading spectrum...', j, sep=''), file=logName, append=T)

                wave_N<-spec$wave
                waveBlue_N<-spec$waveBlue   
                waveRed_N<-spec$waveRed

                if (verbose>1){cat('           - Calculating helocentric velcoity correction for spec...',j, '\n')}
                write(paste('           - Calculating helocentric velcoity correction for spec...',j ,sep=''), file=logName, append=T)

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
                EXP<-EXP+spec$EXP
                
                fluxInt<-approx(wave_N, flux_N, wave)$y
                fluxIntBlue<-approx(waveBlue_N, fluxBlue_N, waveBlue)$y
                fluxIntRed<-approx(waveRed_N, fluxRed_N, waveRed)$y

                snInt<-approx(wave_N, sn_N, wave)$y
                snIntBlue<-approx(waveBlue_N, snBlue_N, waveBlue)$y
                snIntRed<-approx(waveRed_N, snRed_N, waveRed)$y


                if (verbose>1){cat('           - Extracting continuum of spec...',j, '\n')}
                write(paste('           - Extracting continuum of spec...',j, sep=''), file=logName, append=T)
                

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

                if (verbose>1){cat('           - Calculating weighting of spec...',j, '\n')}
                write(paste('           - Calculating weighting of spec...',j, sep=''), file=logName, append=T)

                weight<-sn/snInt
                weightBlue<-snBlue/snIntBlue
                weightRed<-snRed/snIntRed
                
                sc<-sc+weight
                scBlue<-scBlue+weightBlue
                scRed<-scRed+weightRed


                if (verbose>1){cat('           - Adding to stack. Spec...',j, '\n')}
                write(paste('           - Adding to stack. Spec...',j, sep=''), file=logName, append=T)

                
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

        if (verbose>1){cat('       - Scaling final stack ', '\n')}
        write(paste('       - Scaling final stack', sep=''), file=logName, append=T)

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

        if (verbose>1){cat('   - Writing stack as: ',paste('data/reduced/stackedSpec/',ID,'.Rdata',sep=''), '\n')}
        write(paste('   - Writing stack as: data/reduced/stackedSpec/',ID,'.Rdata', sep=''), file=logName, append=T)
        

        spec<-list(wave=wave,flux=flux,sn=sn,sky=sky, ID=ID, RA=RA, DEC=DEC, MAG=MAG, xunit='ang', yunit='ang', z=NA, EXP=EXP, NStack=NStack,waveBlue=waveBlue, fluxBlue=fluxBlue, snBlue=snBlue, skyBlue=skyBlue, waveRed=waveRed, fluxRed=fluxRed, snRed=snRed, skyRed=skyRed, fluxSub=fluxSub, fluxSubBlue=fluxSubBlue, fluxSubRed=fluxSubRed, file=paste('data/reduced/stackedSpec/',ID,'.Rdata',sep=''), fluxSc=fluxSc,fluxScBlue=fluxScBlue,fluxScRed=fluxScRed)

        save(spec, file=paste('data/reduced/stackedSpec/',ID,'.Rdata',sep=''))
        

        specStack<-spec
        
        if (makePlot==T){

            if (verbose>1){cat('   - Plotting stack as: ',paste('data/reduced/stackedSpec/plots/',ID,'.pdf',sep=''), '\n')}
            write(paste('   - Plotting stack as: data/reduced/stackedSpec/plots/',ID,'.pdf', sep=''), file=logName, append=T)

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
