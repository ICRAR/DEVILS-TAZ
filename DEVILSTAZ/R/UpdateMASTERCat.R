UpdateMASTERCat<-function(cat=cat, specDir=specDir){

    load(cat)
    
    specs<-list.files(path=specDir,pattern='*.Rdata')
    
    for (i in 1:length(specs)){

        load(paste(specDir,specs[i], sep=''))

        ID<-as.numeric(substr(spec$ID, 2,nchar(spec$ID)))

        match<-which(DMCat$CATAID==ID)
        match<-round(runif(1,1,length(DMCat$CATAID)))

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
    DMCat$PRIORITY[which(DMCat$DEVILS_prob<=0.96 & is.finite(DMCat$DEVILS_prob)==T)]<-7
    
    cat(length(which(DMCat$DEVILS_prob>0.96)), ' - Sources with successful redshfits', '\n')
    cat(length(which(DMCat$DEVILS_prob<=0.96 & is.finite(DMCat$DEVILS_prob)==T)), ' - Sources with unsuccessful redshfits being prioritised', '\n')
        
        save(DMCat, file=paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''))
        
    return(paste('data/catalogues/MASTERcats/DMCat',Sys.Date(),'.rda', sep=''))


    }
