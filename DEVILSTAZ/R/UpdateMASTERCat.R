#' Update DMcats with new redshifts and make DOCats
#'
#' @description This is the high-level main TAZ function for updating the DMCat with
#' new redshifts in a directory and generating DOCats for the 
#' next observing night.
#'  
#' @param cat Path to current DMCat 
#' @param specDir Path to directory containing R list spectra with value spec$z. 
#' Usually will be data/reduced/stackedSpec 
#' @param OzDESGRC Either NA (do not update) or path to OzDES GRC cat to update to  
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param makePlots make current survey progress plots

#' @examples 
#' 
#' @export
UpdateMASTERCat<-function(cat=cat, specDir=specDir, OzDESGRC=OzDESGRC, logName=logName, verbose=verbose, makePlots=F,probGood=0.90, useVIS=F, bumpPos=NA){


    load(cat)
    
    specs<-list.files(path=specDir,pattern='*.Rdata')

    if (verbose>1){cat('    - Spectra found in directory:',length(specs), '\n')}
    write(paste('    - Spectra found in directory:',length(specs) ,sep=''), file=logName, append=T)

    
    if (verbose>1){cat('    - Updating redshifts....', '\n')}
    write(paste('    - Updating redshifts....',sep=''), file=logName, append=T)
    
    if (length(specs)>0){
    
    for (i in 1:length(specs)){

        if (verbose>1){cat('        - ',i,' of ', length(specs), '\n')}
        write(paste('        - ',i,' of ', length(specs),sep=''), file=logName, append=T)

        load(paste(specDir,specs[i], sep=''))

        ID<-as.numeric(substr(spec$ID, 2,nchar(spec$ID)))

        match<-which(DMCat$CATAID==ID)

        if (length(match)>0){
            RA<-spec$RA
            DEC<-spec$DEC

            DMCat$DEVILS_z[match]<-spec$z
            DMCat$DEVILS_prob[match]<-spec$prob
            DMCat$DEVILS_EXP[match]<-spec$EXP
            DMCat$DEVILS_cc<-spec$cc
            DMCat$DEVILS_z2[match]<-spec$z2
            DMCat$DEVILS_cc2[match]<-spec$cc2
            DMCat$DEVILS_temp[match]<-spec$Temp
            DMCat$DEVILS_file[match]<-paste(specDir,specs[i], sep='')
            DMCat$DEVILS_lastObs[match]<-spec$UTMJDLast
        }
            

    }
      
      
      
    if (is.na(OzDESGRC)==F) {
        OzDESCat<-fread(OzDESGRC)  
        use<-c('ZCOS','hCOSMOS','VUDS','DES_AAOmega','GAMA','SDSS','VIPERS','SNLS_AAOmega','VVDS_DEEP','NOAO_0522','NOAO_0334','Baker','PanSTARRS_AAOmega','2dFGRS','VVDS_CDFS','VUDS_ECDFS')
        OzDESCat<-OzDESCat[which(OzDESCat$z>-9 & OzDESCat$source %in% use==T),]
        match<-coordmatch(cbind(DMCat$RA, DMCat$DEC), cbind(OzDESCat$RA, OzDESCat$DEC))
        good=match$bestmatch$refID %in% seq(1,dim(DMCat)[1],1)
        goodmatch=match$bestmatch[good,]
        
        DMCat$ZSPEC_Prev[goodmatch[,1]]<-OzDESCat$z[goodmatch[,2]]
        
        IDTmp<-as.character(unlist(DMCat$ZSPECID_Prev))
        IDTmp[goodmatch[,1]]<-as.character(OzDESCat$ID[goodmatch[,2]])
        DMCat$ZSPECID_Prev<-IDTmp
        
        IDTmp<-as.character(unlist(DMCat$ZSPECORIG_Prev))
        IDTmp[goodmatch[,1]]<-OzDESCat$source[goodmatch[,2]]
        DMCat$ZSPECORIG_Prev<-IDTmp
          
        IDTmp<-as.numeric(unlist(DMCat$ZSPECQ_Prev))
        IDTmp[goodmatch[,1]]<-as.numeric(OzDESCat$flag[goodmatch[,2]])
        DMCat$ZSPECQ_Prev<-IDTmp
        
        DMCat$PRIORITY[goodmatch[which(DMCat$PRIORITY[goodmatch[,1]]>1),1]]<-1
    }
    
    DMCat$PRIORITY[which(DMCat$DEVILS_prob>probGood)]<-1
    DMCat$PRIORITY[which(DMCat$DEVILS_prob<=probGood & is.finite(DMCat$DEVILS_prob)==T & DMCat$ZSPEC_Prev<0)]<-8


    #### Stop observing if TEXP>10h
    DMCat$PRIORITY[which(as.numeric(DMCat$DEVILS_EXP)>=12*60*60)] <- -1
    
    ### removing a few things found to be junk
    Vis<-read.csv('data/catalogues/VIS_Bad.csv', header=T)
    
    DMCat$VISCLASS[which(DMCat$CATAID %in% Vis[,1])]<-2
    DMCat$PRIORITY[which(DMCat$CATAID %in% Vis[,1])]<-0
    
    ### removing things with low AutoZ prob but a visually inspected good redshift, and high auto-z but visually bad redshift:
    
    if (useVIS==T){
      
      
        selVISGood<-which(DMCat$DEVILS_lastObs<DMCat$DEVILS_UTMJD_VIS & DMCat$DEVILS_CurrentVIS==1 & DMCat$DEVILS_prob<=probGood & is.na(DMCat$DEVILS_CurrentVIS)==F)
        DMCat$PRIORITY[selVISGood]<-1
        DMCat$DEVILS_prob[selVISGood]<-2.0

        selVISBad<-which(DMCat$DEVILS_lastObs<DMCat$DEVILS_UTMJD_VIS & DMCat$DEVILS_CurrentVIS==0 & DMCat$DEVILS_prob>probGood)
        DMCat$PRIORITY[selVISBad]<-8
        DMCat$DEVILS_prob[selVISBad]<-0
    }
    
    if (is.na(bumpPos)==F) {
      
      match<-coordmatch(cbind(DMCat$RA, DMCat$DEC), cbind(bumpPos[,1], bumpPos[,2]), rad = 2)
      good=match$bestmatch$refID %in% seq(1,dim(DMCat)[1],1)
      goodmatch=match$bestmatch[good,]
      DMCat$PRIORITY[goodmatch[,1]]<-9
      
    }
    
    
    #VisGood<-read.csv('data/catalogues/VIS_GoodSpec.csv', header=T)
    
    #DMCat$VISRED<-DMCat$VISCLASS
    #DMCat$VISRED[]<-NA
    #DMCat$VIS_SPECFLAG<-DMCat$VISRED
    
    #DMCat$VIS_SPECFLAG[which(DMCat$CATAID %in% VisGood[,1])]<-VisGood[,3]
    #DMCat$PRIORITY[which(DMCat$VIS_SPECFLAG>1)]<-1
    #DMCat$DEVILS_z[which(DMCat$VIS_SPECFLAG>1)]<-VisGood[,2]
    
    if (verbose>1){
      cat(length(which(DMCat$DEVILS_prob>probGood | DMCat$VISCLASS>8 | DMCat$DEVILS_prob==2)), '    - Sources with successful redshfits', '\n')
      cat(length(which(DMCat$DEVILS_prob<=probGood & is.finite(DMCat$DEVILS_prob)==T & (DMCat$VISCLASS<8 | is.finite(DMCat$VISCLASS)==FALSE) & DMCat$DEVILS_prob!=2)), '    - Sources with unsuccessful redshfits being prioritised', '\n')
      cat('    - Saving new MASTERcat as:', paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''), '\n')
    }
    write(paste(length(which(DMCat$DEVILS_prob>probGood | DMCat$VISCLASS>8 | DMCat$DEVILS_prob==2)), '    - Sources with successful redshfits',sep=''), file=logName, append=T)
    write(paste(length(which(DMCat$DEVILS_prob<=probGood & is.finite(DMCat$DEVILS_prob)==T & (DMCat$VISCLASS<8 | is.finite(DMCat$VISCLASS)==FALSE) & DMCat$DEVILS_prob!=2)), '    - Sources with unsuccessful redshfits being prioritised',sep=''), file=logName, append=T)
    write(paste('    - Saving new MASTERcat as: data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''),file=logName, append=T)
    
    
    }
    
    host<-system('hostname',intern = TRUE)
    if (host=="munro"){
      nowTime<-strsplit(as.character(Sys.time()), ' ')[[1]][2]
      nowHour<-as.numeric(strsplit(as.character(nowTime), ':')[[1]][1])+as.numeric(strsplit(as.character(nowTime), ':')[[1]][2])/60
      JD_UTC<-date2jd(year=as.numeric(strsplit(as.character(Sys.Date()),'-')[[1]][1]),mon=as.numeric(strsplit(as.character(Sys.Date()),'-')[[1]][2]), mday=as.numeric(strsplit(as.character(Sys.Date()),'-')[[1]][3]), hour=nowHour)
      JD_Perth<-JD_UTC+0.33333
      nowDate<-jd2date(JD=JD_Perth)
      nowDate<-paste(nowDate$year,'-',nowDate$mon,'-',nowDate$mday,sep='')
    }else{
      nowDate<-Sys.Date()
    }
    
    
    save(DMCat, file=paste('data/catalogues/MASTERcats/DMCat',nowDate,'.rda', sep=''))

 
    if (makePlots==T){

        
        DMCat<-DMCat[which(DMCat$STARCLASS==0, DMCat$MASK_FLAG==0 & DMCat$VIS_CLASS!=4), ]
        
        
        selGal<-which(DMCat$STARCLASS==0)
        selPrev<-which(DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
        selGood<-which(DMCat$DEVILS_prob>probGood)
        selBad<-which(DMCat$DEVILS_prob<=probGood & is.finite(as.numeric(DMCat$DEVILS_EXP)==T))
        selTotGood<-which((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$PRIORITY==1 & is.finite(as.numeric(DMCat$DEVILS_EXP)==T))) 
        
        system(paste('mkdir data/ProgressPlots/',nowDate, sep=''))
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_YMAG_vs_EXP_inu.pdf',sep=''),width=8,height=8)
        magplot(DMCat$YMAG[selBad], as.numeric(DMCat$DEVILS_EXP[selBad])/60, pch=16, col=rgb(1,0,0,0.4), xlab='Y-mag', ylab='Exposure Time, min', xlim=c(15,21.5), main='Exposure Times')
        points(DMCat$YMAG[selGood], as.numeric(DMCat$DEVILS_EXP[selGood])/60, pch=16, col=rgb(24/255,128/255,127/255,0.3))
        
        legend('topleft', legend=c('DEVILS Redshift', 'DEVILS-observed no redshift'), text.col=c(rgb(24/255,128/255,127/255),'red'))
        
        dev.off()
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_YMAG_successHist.pdf',sep=''),width=8,height=8)
        
        
        
        allHist_a<-maghist(DMCat$YMAG[selGal],plot=F)
        allHist<-maghist(DMCat$YMAG[selGal], col=rgb(0.3,0.3,0.3,1), xlab='Y-mag', ylab='#', log='y', main='Number histogram', ylim=c(1,max(allHist_a$counts,na.rm=T)*5.0))
        totGoodHist<-maghist(DMCat$YMAG[selTotGood], col=rgb(0,0.6,0,1), add=T, log='y',breaks=allHist$breaks)
        prevHist<-maghist(DMCat$YMAG[selPrev], col=rgb(0,0,1,1), add=T, log='y',breaks=allHist$breaks)
        badHist<-maghist(DMCat$YMAG[selBad], col=rgb(1,0,0,1), add=T, log='y',breaks=allHist$breaks)
        goodHist<-maghist(DMCat$YMAG[selGood], col=rgb(24/255,128/255,127/255,1), add=T, log='y',breaks=allHist$breaks)
        
        badHist2<-maghist(DMCat$YMAG[selBad], col=rgb(1,0,0,1), add=T, log='y',breaks=allHist$breaks[which(allHist$breaks<21)])
        
        legend('topleft', legend=c('All Galaxies','Total Good Redshifts', 'Pre-DEVILS redshift','DEVILS Redshift', 'DEVILS-observed no redshift'), text.col=c('dimgrey','darkgreen','blue', rgb(24/255,128/255,127/255),'red'))
        
        for (i in 1:length(allHist$mids)){
            text(allHist$mids[i], 10^(log10(allHist$counts[i])+0.1), round(allHist$counts[i]), col='dimgrey', cex=0.8)
            text(allHist$mids[i], 10^(log10(allHist$counts[i])+0.2), round(totGoodHist$counts[i]), col='darkgreen', cex=0.8)
            text(allHist$mids[i], 10^(log10(allHist$counts[i])+0.3), round(prevHist$counts[i]), col='blue', cex=0.8)
            text(allHist$mids[i], 10^(log10(allHist$counts[i])+0.4), round(badHist$counts[i]), col='red', cex=0.8)
            text(allHist$mids[i], 10^(log10(allHist$counts[i])+0.5), round(goodHist$counts[i]), col=rgb(24/255,128/255,127/255), cex=0.8)
        }
        
        dev.off()
        
        
        
        
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_YMAG_EXP_total.pdf',sep=''),width=8,height=8)
        
        histEXPgood<-allHist$mids
        histEXPbad<-allHist$mids
        
        for (i in 1:length(allHist$mids)){
            
            selGood2<-which(DMCat$DEVILS_prob>probGood & DMCat$YMAG>allHist$breaks[i] & DMCat$YMAG<=allHist$breaks[i]+0.5)
            selBad2<-which(DMCat$DEVILS_prob<=probGood & is.finite(as.numeric(DMCat$DEVILS_EXP)==T) & DMCat$YMAG>allHist$breaks[i] & DMCat$YMAG<=allHist$breaks[i]+0.5)
            
            histEXPgood[i]<-sum(as.numeric(DMCat$DEVILS_EXP[selGood2]))/60
            histEXPbad[i]<-sum(as.numeric(DMCat$DEVILS_EXP[selBad2]))/60.
        }
        
        magplot(allHist$mids,histEXPgood,col='white', main='Invested Exposure Time', xlab='Y-mag', ylab='Total Exposure Time, min', ylim=c(1,max(c(histEXPgood,histEXPbad),na.rm=T)*1.2), log='y')
        
        for (i in 1:length(allHist$mids)){
            polygon(c(allHist$breaks[i],allHist$breaks[i]+0.5, allHist$breaks[i]+0.5, allHist$breaks[i]), c(1,1,histEXPgood[i], histEXPgood[i]), col=rgb(24/255,128/255,127/255, 0.4))
            polygon(c(allHist$breaks[i],allHist$breaks[i]+0.5, allHist$breaks[i]+0.5, allHist$breaks[i]), c(1,1,histEXPbad[i], histEXPbad[i]), col=rgb(1,0,0,0.4))
        }
        
        legend('topleft', legend=c('DEVILS Redshift', 'DEVILS-observed no redshift'), text.col=c(rgb(24/255,128/255,127/255),'red'))
        
        dev.off()
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_z_Hist.pdf',sep=''),width=8,height=8)
        
        zAll<-DMCat$ZSPEC_Prev
        zAll[which(DMCat$DEVILS_prob>probGood)]<-DMCat$DEVILS_z[which(DMCat$DEVILS_prob>probGood)]
        
        totGoodHist<-maghist(zAll[selTotGood], col=rgb(0,0.6,0,1), xlab='Redshift', ylab='#', main='Redshift histogram', xlim=c(0,1.5), log='y')
        maghist(zAll[selPrev], col=rgb(0,0,1,1), add=T, log='y',breaks=totGoodHist$breaks)
        maghist(zAll[selGood], col=rgb(24/255,128/255,127/255),add=T, log='y',breaks=totGoodHist$breaks)
        
        
        legend('topright', legend=c('Total Good Redshifts', 'Pre-DEVILS redshift','DEVILS Redshift'), text.col=c('darkgreen','blue', rgb(24/255,128/255,127/255)))
        
        dev.off()
        
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_predicted_remaining_time.pdf',sep=''),width=8,height=8)
        
                                        #pdf(paste(nowDate,'_predicted_remaining_time.pdf',sep=''),width=8,height=8)
        
        remnum<-allHist$counts-totGoodHist$counts
        medEXPGood<-allHist$counts
        meanEXPGood<-allHist$counts
        medEXPAll<-allHist$counts
        meanEXPAll<- allHist$counts
        for (j in 1:length(allHist$counts)){
            selGood2<-which(DMCat$DEVILS_prob>probGood & DMCat$YMAG>allHist$breaks[j] & DMCat$YMAG<=allHist$breaks[j]+0.5)
            selAll<-which(is.finite(as.numeric(DMCat$DEVILS_EXP)==T) & DMCat$YMAG>allHist$breaks[j] & DMCat$YMAG<=allHist$breaks[j]+0.5)
            medEXPGood[j]<-median(as.numeric(DMCat$DEVILS_EXP[selGood2]), na.rm=T)/60/60
            medEXPAll[j]<-median(as.numeric(DMCat$DEVILS_EXP[selAll]), na.rm=T)/60/60  
            meanEXPGood[j]<-mean(as.numeric(DMCat$DEVILS_EXP[selGood2]), na.rm=T)/60/60
            meanEXPAll[j]<-mean(as.numeric(DMCat$DEVILS_EXP[selAll]), na.rm=T)/60/60 
        }
        
        histMedGood<-remnum*medEXPGood
        histMedAll<-remnum*medEXPAll
        histMeanGood<-remnum*meanEXPGood
        histMeanAll<-remnum*meanEXPAll
        
        magplot(allHist$mids,histMedGood,col='white', main='Predicted Future Exposure Time', xlab='Y-mag', ylab='Total Exposure Time Left, Fibre hours', ylim=c(1,max(c(histMedGood,histMedAll,histMeanGood,histMeanAll),na.rm=T)*1.2), xlim=c(17,21.5))
        
        for (j in 1:length(allHist$mids)){
            polygon(c(allHist$breaks[j],allHist$breaks[j]+0.5, allHist$breaks[j]+0.5, allHist$breaks[j]), c(1,1,histMedGood[j], histMedGood[j]), border=rgb(0,0,1, 0.4))
            polygon(c(allHist$breaks[j],allHist$breaks[j]+0.5, allHist$breaks[j]+0.5, allHist$breaks[j]), c(1,1,histMedAll[j], histMedAll[j]), border=rgb(1,0,0,0.4))
            polygon(c(allHist$breaks[j],allHist$breaks[j]+0.5, allHist$breaks[j]+0.5, allHist$breaks[j]), c(1,1,histMeanGood[j], histMeanGood[j]), border=rgb(0.0,0.6,0., 0.4))
            polygon(c(allHist$breaks[j],allHist$breaks[j]+0.5, allHist$breaks[j]+0.5, allHist$breaks[j]), c(1,1,histMeanAll[j], histMeanAll[j]), border=rgb(0.6,0.6,0,0.4))
        }
        
        TotTimeMedGood<-(sum(histMedGood, na.rm=T)/360/7.)*1.5
        TotTimeMedAll<-(sum(histMedAll, na.rm=T)/360/7.)*1.5
        TotTimeMeanGood<-(sum(histMeanGood, na.rm=T)/360/7.)*1.5
        TotTimeMeanAll<-(sum(histMeanAll, na.rm=T)/360/7.)*1.5
        
        legend('topleft', legend=c('Time Left = 72 nights', paste('Predicted (Median with z) = ', ceiling(TotTimeMedGood),' nights',sep=''), paste('Predicted (Median All Current) = ', ceiling(TotTimeMedAll),' nights',sep=''), paste('Predicted (Mean with z) = ', ceiling(TotTimeMeanGood),' nights',sep=''), paste('Predicted (Mean All Current) = ', ceiling(TotTimeMeanAll),' nights',sep=''), '(Assuming 33% bad weather, 7h per night)'), text.col=c('black',rgb(0,0,1),rgb(1,0,0),rgb(0,0.6,0),rgb(0.6,0.6,0)))
        
        dev.off()
        
        
        
        selD02<-which((DMCat$FIELD=='D02A' | DMCat$FIELD=='D02B') & DMCat$PRIORITY>0)
        selD02Prev<-which((DMCat$FIELD=='D02A' | DMCat$FIELD=='D02B') & DMCat$PRIORITY>0 & DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
        selD02Good<-which((DMCat$FIELD=='D02A' | DMCat$FIELD=='D02B') & DMCat$PRIORITY>0 & ((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$DEVILS_prob>probGood)))
        
        selD03<-which(DMCat$FIELD=='D03' & DMCat$PRIORITY>0)
        selD03Prev<-which((DMCat$FIELD=='D03') & DMCat$PRIORITY>0 & DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
        selD03Good<-which((DMCat$FIELD=='D03') & DMCat$PRIORITY>0 & ((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$DEVILS_prob>probGood)))
        
        selD10<-which(DMCat$FIELD=='D10' & DMCat$PRIORITY>0)
        selD10Prev<-which((DMCat$FIELD=='D10') & DMCat$PRIORITY>0 & DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
        selD10Good<-which((DMCat$FIELD=='D10') & DMCat$PRIORITY>0 & ((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$DEVILS_prob>probGood)))
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_D02_comp.pdf',sep=''),width=24,height=8)
        
        
        par(mfrow = c(1, 3))
        par(mar=c(3.1,3.1,1.1,1.1))
        
        bin<-1/(30)
        
        layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
        RABins<-seq(min(DMCat$RA[selD02],na.rm=T), max(DMCat$RA[selD02],na.rm=T), bin)
        DECBins<-seq(min(DMCat$DEC[selD02],na.rm=T), max(DMCat$DEC[selD02],na.rm=T), bin)
        
        targetDensity<-array(NA, dim=c(length(RABins),length(DECBins)))
        prevDensity<-targetDensity
        goodDensity<-targetDensity
        
        for (i in 1:length(RABins)){
            for (j in 1:length(DECBins)){
                targetDensity[i,j]<-length(which(DMCat$RA[selD02]>RABins[i] & DMCat$RA[selD02]<=RABins[i]+bin & DMCat$DEC[selD02]>DECBins[j] & DMCat$DEC[selD02]<=DECBins[j]+bin))
                prevDensity[i,j]<-length(which(DMCat$RA[selD02Prev]>RABins[i] & DMCat$RA[selD02Prev]<=RABins[i]+bin & DMCat$DEC[selD02Prev]>DECBins[j] & DMCat$DEC[selD02Prev]<=DECBins[j]+bin))
                goodDensity[i,j]<-length(which(DMCat$RA[selD02Good]>RABins[i] & DMCat$RA[selD02Good]<=RABins[i]+bin & DMCat$DEC[selD02Good]>DECBins[j] & DMCat$DEC[selD02Good]<=DECBins[j]+bin))
            }
        } 
        
        size<-max(c(max(RABins)-min(RABins),max(DECBins)-min(DECBins)))
        image(RABins+bin/2,DECBins+bin/2, targetDensity/max(targetDensity,na.rm=T), main='Sample Density', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = grey.colors(255))
        
        magbar('topleft', range=c(0, max(targetDensity,na.rm=T)), col=grey.colors(255), title='Y mag<21.2 # targets')
        
        preComp<-prevDensity/targetDensity
        image(RABins+bin/2,DECBins+bin/2, preComp/max(preComp,na.rm=T) , main='Pre-DEVILS Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = heat.colors(255))
        
        magbar('topleft', range=c(0,1), col=heat.colors(255), title='Pre-DEVILS redshfit density')
        
        nowComp<-goodDensity/targetDensity
        image(RABins+bin/2,DECBins+bin/2, nowComp/max(nowComp[which(is.finite(nowComp)==T)],na.rm=T), main='Current Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = heat.colors(255))
        
        magbar('topleft', range=c(0,1), col=heat.colors(255), title='Current redshfit density')
        
        
        dev.off()
        
        
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_D03_comp.pdf',sep=''),width=24,height=8)
        
        par(mfrow = c(1, 3))
        par(mar=c(3.1,3.1,1.1,1.1))
        
        bin<-1/(30)
        
        layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
        RABins<-seq(min(DMCat$RA[selD03],na.rm=T), max(DMCat$RA[selD03],na.rm=T), bin)
        DECBins<-seq(min(DMCat$DEC[selD03],na.rm=T), max(DMCat$DEC[selD03],na.rm=T), bin)
        
        targetDensity<-array(NA, dim=c(length(RABins),length(DECBins)))
        prevDensity<-targetDensity
        goodDensity<-targetDensity
        
        for (i in 1:length(RABins)){
            for (j in 1:length(DECBins)){
                targetDensity[i,j]<-length(which(DMCat$RA[selD03]>RABins[i] & DMCat$RA[selD03]<=RABins[i]+bin & DMCat$DEC[selD03]>DECBins[j] & DMCat$DEC[selD03]<=DECBins[j]+bin))
                prevDensity[i,j]<-length(which(DMCat$RA[selD03Prev]>RABins[i] & DMCat$RA[selD03Prev]<=RABins[i]+bin & DMCat$DEC[selD03Prev]>DECBins[j] & DMCat$DEC[selD03Prev]<=DECBins[j]+bin))
                goodDensity[i,j]<-length(which(DMCat$RA[selD03Good]>RABins[i] & DMCat$RA[selD03Good]<=RABins[i]+bin & DMCat$DEC[selD03Good]>DECBins[j] & DMCat$DEC[selD03Good]<=DECBins[j]+bin))
            }
        } 
        
        size<-max(c(max(RABins)-min(RABins),max(DECBins)-min(DECBins)))
        image(RABins+bin/2,DECBins+bin/2, targetDensity/max(targetDensity,na.rm=T), main='Sample Density', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = grey.colors(255))
        
        preComp<-prevDensity/targetDensity
        image(RABins+bin/2,DECBins+bin/2, preComp/max(preComp,na.rm=T) , main='Pre-DEVILS Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = heat.colors(255))
        
        nowComp<-goodDensity/targetDensity
        image(RABins+bin/2,DECBins+bin/2, nowComp/max(nowComp[which(is.finite(nowComp)==T)],na.rm=T), main='Current Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = heat.colors(255))
        
        dev.off()
        
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_D10_comp.pdf',sep=''),width=24,height=8)
        
        par(mfrow = c(1, 3))
        par(mar=c(3.1,3.1,1.1,1.1))
        
        bin<-1/(30)
        
        layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
        RABins<-seq(min(DMCat$RA[selD10],na.rm=T), max(DMCat$RA[selD10],na.rm=T), bin)
        DECBins<-seq(min(DMCat$DEC[selD10],na.rm=T), max(DMCat$DEC[selD10],na.rm=T), bin)
        
        targetDensity<-array(NA, dim=c(length(RABins),length(DECBins)))
        prevDensity<-targetDensity
        goodDensity<-targetDensity
        
        for (i in 1:length(RABins)){
            for (j in 1:length(DECBins)){
                targetDensity[i,j]<-length(which(DMCat$RA[selD10]>RABins[i] & DMCat$RA[selD10]<=RABins[i]+bin & DMCat$DEC[selD10]>DECBins[j] & DMCat$DEC[selD10]<=DECBins[j]+bin))
                prevDensity[i,j]<-length(which(DMCat$RA[selD10Prev]>RABins[i] & DMCat$RA[selD10Prev]<=RABins[i]+bin & DMCat$DEC[selD10Prev]>DECBins[j] & DMCat$DEC[selD10Prev]<=DECBins[j]+bin))
                goodDensity[i,j]<-length(which(DMCat$RA[selD10Good]>RABins[i] & DMCat$RA[selD10Good]<=RABins[i]+bin & DMCat$DEC[selD10Good]>DECBins[j] & DMCat$DEC[selD10Good]<=DECBins[j]+bin))
            }
        } 
        
        size<-max(c(max(RABins)-min(RABins),max(DECBins)-min(DECBins)))
        image(RABins+bin/2,DECBins+bin/2, targetDensity/max(targetDensity,na.rm=T), main='Sample Density', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = grey.colors(255))
        
        preComp<-prevDensity/targetDensity
        image(RABins+bin/2,DECBins+bin/2, preComp/max(preComp,na.rm=T) , main='Pre-DEVILS Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = heat.colors(255))
        
        nowComp<-goodDensity/targetDensity
        image(RABins+bin/2,DECBins+bin/2, nowComp/max(nowComp[which(is.finite(nowComp)==T)],na.rm=T), main='Current Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2), col = heat.colors(255))
        
        dev.off()

        selD02A<-selD02[which(DMCat$FIELD[selD02]=='D02A')]
        selD02AGood<-selD02Good[which(DMCat$FIELD[selD02Good]=='D02A')]
        selD02APrev<-selD02Prev[which(DMCat$FIELD[selD02Prev]=='D02A')]
        selD02B<-selD02[which(DMCat$FIELD[selD02]=='D02B')]
        selD02BGood<-selD02Good[which(DMCat$FIELD[selD02Good]=='D02B')]
        selD02BPrev<-selD02Prev[which(DMCat$FIELD[selD02Prev]=='D02B')]
 
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_comp_Mag.pdf',sep=''),width=12,height=12)
        
      
        breaks<-seq(0,30,0.2)
        
        layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
        par(oma=c(4.1,5.1,3,3))
        par(mar=c(0,0,0,0))

        D02ACompGood<-maghist(DMCat$YMAG[selD02AGood], breaks=breaks, plot=F)
        D02ACompAll<-maghist(DMCat$YMAG[selD02A], breaks=breaks, plot=F)
        D02ACompPrev<-maghist(DMCat$YMAG[selD02APrev], breaks=breaks, plot=F)
        D02AComp<-D02ACompGood$counts/D02ACompAll$counts
        D02APrev<-D02ACompPrev$counts/D02ACompAll$counts
        magplot(D02ACompGood$mids, D02AComp, type='l', ylim=c(0,1), col=rgb(24/255,128/255,127/255), xlab='', ylab='Completeness', xlim=c(18,21.5), xaxt='n', yaxt=NULL, ann=TRUE)
        lines(D02ACompGood$mids, D02APrev, type='l', ylim=c(0,1), col='red')
        text(21,0.95, 'D02A', cex=2.5)
        magaxis(side=c(1,2,3,4), labels=FALSE)

         D02BCompGood<-maghist(DMCat$YMAG[selD02BGood], breaks=breaks, plot=F)
        D02BCompAll<-maghist(DMCat$YMAG[selD02B], breaks=breaks, plot=F)
        D02BCompPrev<-maghist(DMCat$YMAG[selD02BPrev], breaks=breaks, plot=F)
        D02BComp<-D02BCompGood$counts/D02BCompAll$counts
        D02BPrev<-D02BCompPrev$counts/D02BCompAll$counts
        magplot(D02BCompGood$mids, D02BComp, type='l', ylim=c(0,1), col=rgb(24/255,128/255,127/255), xlab='', ylab='', xlim=c(18,21.5), xaxt='n', yaxt='n', ann=TRUE)
        lines(D02BCompGood$mids, D02BPrev, type='l', ylim=c(0,1), col='red')
        text(21,0.95, 'D02B', cex=2.5)
        magaxis(side=c(1,2,3,4), labels=FALSE)


        D03CompGood<-maghist(DMCat$YMAG[selD03Good], breaks=breaks, plot=F)
        D03CompAll<-maghist(DMCat$YMAG[selD03], breaks=breaks, plot=F)
        D03CompPrev<-maghist(DMCat$YMAG[selD03Prev], breaks=breaks, plot=F)
        D03Comp<-D03CompGood$counts/D03CompAll$counts
        D03Prev<-D03CompPrev$counts/D03CompAll$counts
        magplot(D03CompGood$mids, D03Comp, type='l', ylim=c(0,1), col=rgb(24/255,128/255,127/255), xlab='Y-mag', ylab='Completeness', xlim=c(18,21.5), xaxt=NULL, yaxt=NULL, ann=TRUE)
        lines(D03CompGood$mids, D03Prev, type='l', ylim=c(0,1), col='red')
        text(21,0.95, 'D03', cex=2.5)
        magaxis(side=c(1,2,3,4), labels=FALSE)

         D10CompGood<-maghist(DMCat$YMAG[selD10Good], breaks=breaks, plot=F)
        D10CompAll<-maghist(DMCat$YMAG[selD10], breaks=breaks, plot=F)
        D10CompPrev<-maghist(DMCat$YMAG[selD10Prev], breaks=breaks, plot=F)
        D10Comp<-D10CompGood$counts/D10CompAll$counts
        D10Prev<-D10CompPrev$counts/D10CompAll$counts
        magplot(D10CompGood$mids, D10Comp, type='l', ylim=c(0,1), col=rgb(24/255,128/255,127/255), xlab='Y-mag', ylab='', xlim=c(18,21.5), xaxt=NULL, yaxt=NULL, ann=TRUE)
        lines(D10CompGood$mids, D10Prev, type='l', ylim=c(0,1), col='red')
        text(21,0.95, 'D10', cex=2.5)
        magaxis(side=c(1,2,3,4), labels=FALSE)

        legend('bottomright', legend=c('Existing', 'Existing+DEVILS'), text.col=c('red',rgb(24/255,128/255,127/255)), cex=2.0)
          
        dev.off()
        
        
        
        
        pdf(paste('data/ProgressPlots/',nowDate,'/',nowDate,'_RedshiftProgress.pdf',sep=''),width=8,height=8)
        
        
        previousMASTERS<-list.files(path='data/catalogues/MASTERcats/',pattern='*.rda')
        
        
        year<-c()
        month<-c()
        day<-c()
        
        for (j in 1:length(previousMASTERS)){
            yeartmp<-strsplit(previousMASTERS[j],'-')[[1]][1]
            year<-c(year,as.numeric(strsplit(yeartmp,'DMCat')[[1]][2]))
            month<-c(month,as.numeric(strsplit(previousMASTERS[j],'-')[[1]][2]))
            daytmp<-strsplit(previousMASTERS[j],'-')[[1]][3]
            day<-c(day,as.numeric(strsplit(daytmp,'.rda')[[1]][1]))
        }
        
        dateNew<-as.Date(paste(year,'-',month,'-',day,sep=''))
        
        MASTERDates<-date2jd(year=year, mon=month, mday=day, hour=12)
        lastMASTER<-paste('data/catalogues/MASTERcats/',previousMASTERS[which(MASTERDates==max(MASTERDates))],sep='')
        load(lastMASTER)
        
        
        
        dateNew<-dateNew[sort.int(MASTERDates, index.return = TRUE)$ix]
        previousMASTERS<-previousMASTERS[sort.int(MASTERDates, index.return = TRUE)$ix]
        MASTERDates<-MASTERDates[sort.int(MASTERDates, index.return = TRUE)$ix]
        
        
        runCount<-MASTERDates
        nightCount<-MASTERDates
        for (j in 1:length(previousMASTERS)){
            load(paste('data/catalogues/MASTERcats/',previousMASTERS[j], sep=''))
            runCount[j]<-length(which(DMCat$DEVILS_prob >=probGood & is.finite(DMCat$DEVILS_prob)==T))
            if (j>1){nightCount[j]<-runCount[j]-runCount[j-1]} else{nightCount[j]<-runCount[j]} 
        }
        
        
        dateAll<-jd2date(seq(round(min(MASTERDates)), round(max(MASTERDates)), 1))
        
        
        dateAllNew<-as.Date(paste(dateAll$year,'-',dateAll$mon,'-',dateAll$mday,sep=''))
        
        
        plot(dateNew,runCount, col=rgb(24/255,128/255,127/255), ylab='Cumulative Number of Redshfits', xlab='Date', type='l', lwd=2)
        
        for (j in 1:length(dateAllNew)){
            
            lines(c(dateAllNew[j],dateAllNew[j]), c(-10000,10000), col='grey', lty=2, lwd=0.5)
            
        }
        
        points(dateNew,nightCount, pch=21,bg=rgb(24/255,128/255,127/255))
        
        
        for (j in 1:length(nightCount)){
            
            polygon(c(dateNew[j]-1, dateNew[j]+1, dateNew[j]+1, dateNew[j]-1),c(runCount[j]-70, runCount[j]-70, runCount[j]+70, runCount[j]+70), col='white', border=NA) 
            
        }
        
        for (j in 1:length(nightCount)){
            
            if (nightCount[j]>0) {text(dateNew[j], runCount[j], nightCount[j], col=rgb(24/255,128/255,127/255), cex=0.8)}
            
        }
        

        
        
        dev.off()
        
    }
    
    
    return(paste('data/catalogues/MASTERcats/DMCat',nowDate,'.rda', sep=''))


}
