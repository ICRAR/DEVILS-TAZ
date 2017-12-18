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


#' @examples 
#' 
#' @export
UpdateMASTERCat<-function(cat=cat, specDir=specDir, logName=logName, verbose=verbose){

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

    if (verbose>1){
        cat(length(which(DMCat$DEVILS_prob>0.96)), '    - Sources with successful redshfits', '\n')
        cat(length(which(DMCat$DEVILS_prob<=0.96 & is.finite(DMCat$DEVILS_prob)==T)), '    - Sources with unsuccessful redshfits being prioritised', '\n')
        cat('    - Saving new MASTERcat as:', paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''), '\n')
    }
    write(paste(length(which(DMCat$DEVILS_prob>0.96)), '    - Sources with successful redshfits',sep=''), file=logName, append=T)
    write(paste(length(which(DMCat$DEVILS_prob<=0.96 & is.finite(DMCat$DEVILS_prob)==T)), '    - Sources with unsuccessful redshfits being prioritised',sep=''), file=logName, append=T)
    write(paste('    - Saving new MASTERcat as: data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''),file=logName, append=T)
    
    
    }
    
    save(DMCat, file=paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''))
    
    return(paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''))


    }
