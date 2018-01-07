#' Update DMcats with new redshifts and make DOCats
#'
#' @description This is the high-level main TAZ function for updating the DMCat with
#' new redshifts in a directory and generating DOCats for the 
#' next observing night.
#'  
#' @param cat Path to current DMCat 
#' @param specDir Path to directory containing R list spectra with value spec$z. 
#' Usually will be data/reduced/stackedSpec 
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param makePlots make current survey progress plots

#' @examples 
#' 
#' @export
UpdateMASTERCat<-function(cat=cat, specDir=specDir, logName=logName, verbose=verbose, makePlots=F){

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
            DMCat$DEVILS_file[match]<-spec$file
        }
            

    }

    
    DMCat$PRIORITY[which(DMCat$DEVILS_prob>0.96)]<-1
    DMCat$PRIORITY[which(DMCat$DEVILS_prob<=0.96 & is.finite(DMCat$DEVILS_prob)==T)]<-8


    
    Vis<-read.csv('data/catalogues/VIS_Bad.csv', header=T)
    
    DMCat$VISCLASS[which(DMCat$CATAID %in% Vis[,1])]<-2
    DMCat$PRIORITY[which(DMCat$CATAID %in% Vis[,1])]<-0
    
    VisGood<-read.csv('data/catalogues/VIS_GoodSpec.csv', header=T)
    
    DMCat$VISCLASS[which(DMCat$CATAID %in% VisGood[,1])]<-VisGood[,2]
    DMCat$PRIORITY[which(DMCat$CATAID %in% VisGood[,1])]<-1
    
    if (verbose>1){
      cat(length(which(DMCat$DEVILS_prob>0.97 | DMCat$VISCLASS>8)), '    - Sources with successful redshfits', '\n')
      cat(length(which(DMCat$DEVILS_prob<=0.97 & is.finite(DMCat$DEVILS_prob)==T & (DMCat$VISCLASS<8 | is.finite(DMCat$VISCLASS)==FALSE))), '    - Sources with unsuccessful redshfits being prioritised', '\n')
      cat('    - Saving new MASTERcat as:', paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''), '\n')
    }
    write(paste(length(which(DMCat$DEVILS_prob>0.97 | DMCat$VISCLASS>8)), '    - Sources with successful redshfits',sep=''), file=logName, append=T)
    write(paste(length(which(DMCat$DEVILS_prob<=0.97 & is.finite(DMCat$DEVILS_prob)==T & DMCat$VISCLASS<8)), '    - Sources with unsuccessful redshfits being prioritised',sep=''), file=logName, append=T)
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
    
    if (makePlots=T){
      selGal<-which(DMCat$STARCLASS==0)
      selPrev<-which(DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
      selGood<-which(DMCat$DEVILS_prob>0.96)
      selBad<-which(DMCat$DEVILS_prob<=0.96 & is.finite(as.numeric(DMCat$DEVILS_EXP)==T))
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
        
        selGood2<-which(DMCat$DEVILS_prob>0.96 & DMCat$YMAG>allHist$breaks[i] & DMCat$YMAG<=allHist$breaks[i]+0.5)
        selBad2<-which(DMCat$DEVILS_prob<=0.96 & is.finite(as.numeric(DMCat$DEVILS_EXP)==T) & DMCat$YMAG>allHist$breaks[i] & DMCat$YMAG<=allHist$breaks[i]+0.5)
        
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
      zAll[which(DMCat$DEVILS_prob>0.96)]<-DMCat$DEVILS_z[which(DMCat$DEVILS_prob>0.96)]
        
      totGoodHist<-maghist(zAll[selTotGood], col=rgb(0,0.6,0,1), xlab='Redshift', ylab='#', main='Redshift histogram', xlim=c(0,1.5), log='y')
      maghist(zAll[selPrev], col=rgb(0,0,1,1), add=T, log='y',breaks=totGoodHist$breaks)
      maghist(zAll[selGood], col=rgb(24/255,128/255,127/255),add=T, log='y',breaks=totGoodHist$breaks)
      
      
      legend('topright', legend=c('Total Good Redshifts', 'Pre-DEVILS redshift','DEVILS Redshift'), text.col=c('darkgreen','blue', rgb(24/255,128/255,127/255)))
      
      dev.off()
      
      
      
      selD02<-which((DMCat$FIELD=='D02A' | DMCat$FIELD=='D02B') & DMCat$PRIORITY>0)
      selD02Prev<-which((DMCat$FIELD=='D02A' | DMCat$FIELD=='D02B') & DMCat$PRIORITY>0 & DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
      selD02Good<-which((DMCat$FIELD=='D02A' | DMCat$FIELD=='D02B') & DMCat$PRIORITY>0 & ((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$DEVILS_prob>0.96)))
      
      selD03<-which(DMCat$FIELD=='D03' & DMCat$PRIORITY>0)
      selD03Prev<-which((DMCat$FIELD=='D03') & DMCat$PRIORITY>0 & DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
      selD03Good<-which((DMCat$FIELD=='D03') & DMCat$PRIORITY>0 & ((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$DEVILS_prob>0.96)))
      
      selD10<-which(DMCat$FIELD=='D10' & DMCat$PRIORITY>0)
      selD10Prev<-which((DMCat$FIELD=='D10') & DMCat$PRIORITY>0 & DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0)
      selD10Good<-which((DMCat$FIELD=='D10') & DMCat$PRIORITY>0 & ((DMCat$ZSPEC_Prev>0 & DMCat$ZSPEC_Prev<8 & DMCat$STARCLASS==0) | (DMCat$DEVILS_prob>0.96)))
      
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
      image(RABins+bin/2,DECBins+bin/2, targetDensity/max(targetDensity,na.rm=T), main='Sample Density', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      
      preComp<-prevDensity/targetDensity
      image(RABins+bin/2,DECBins+bin/2, preComp/max(preComp,na.rm=T) , main='Pre-DEVILS Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      
      nowComp<-goodDensity/targetDensity
      nowComp[which(nowComp>1)]<-1
      
      image(RABins+bin/2,DECBins+bin/2, nowComp/max(nowComp[which(is.finite(nowComp)==T)],na.rm=T), main='Current Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      
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
      image(RABins+bin/2,DECBins+bin/2, targetDensity/max(targetDensity,na.rm=T), main='Sample Density', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      image(RABins+bin/2,DECBins+bin/2, prevDensity/targetDensity, main='Pre-DEVILS Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      image(RABins+bin/2,DECBins+bin/2, goodDensity/targetDensity, main='Current Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      
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
      image(RABins+bin/2,DECBins+bin/2, targetDensity/max(targetDensity,na.rm=T), main='Sample Density', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      image(RABins+bin/2,DECBins+bin/2, prevDensity/targetDensity, main='Pre-DEVILS Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      image(RABins+bin/2,DECBins+bin/2, goodDensity/targetDensity, main='Current Completeness', xlab='R.A., deg', ylab='Declination, deg', xlim=c(median(RABins)-size/2, median(RABins)+size/2),ylim=c(median(DECBins)-size/2, median(DECBins)+size/2))
      
      dev.off()
      
      
      
      
    }
    
    
    return(paste('data/catalogues/MASTERcats/DMCat',nowDate,'.rda', sep=''))


    }
