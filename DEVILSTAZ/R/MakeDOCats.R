makeDOCats<-function(MASTERCat=MASTERCat,UserName=UserName,dateFor=dateFor, year=2017, semester='B', run=1, logName=logName, verbose=verbose){

     if (verbose>1){cat('    - Loading Guide, sky and std files from data/calibrators/', '\n')}
    write(paste('    - Loading Guide, sky and std files from data/calibrators/',sep=''), file=logName, append=T)
    
    GuideFile<-'data/calibrators/GuideStars/DATAguide.tab'
    SkyFile<-'data/calibrators/SkyFibres/DATAsky.tab'
    StdFile<-'data/calibrators/StdStars/DATAstspec.tab'

    

    dir<-paste('data/observing/',dateFor,'/DOCats/',sep='')
    system(paste('mkdir ',dir,sep=''))
    system(paste('mkdir ',dir,'Tiling',sep=''))
    system(paste('mkdir ',dir,'Tiling/TileFiles',sep=''))

    if (verbose>1){cat('    - Making DOcat directory as:',dir, '\n')}
    write(paste('    - Making DOcat directory as:',dir,sep=''), file=logName, append=T)

    
    date<-strsplit(dateFor, '/')[[1]][2]
    time<-Sys.time()

    D10_RA<-c(149.38,150.7)
    D10_DEC<-c(1.65,2.79)
    D02_RA<-c(34.0,37.05)
    D02_DEC<-c(-5.2,-4.2)

    D02A_RA<-c(D02_RA[1], sum(D02_RA)/2)
    D02A_DEC<-D02_DEC

    D02B_RA<-c(sum(D02_RA)/2,D02_RA[2])
    D02B_DEC<-D02_DEC

    D03_RA<-c(52.3,54.0)
    D03_DEC<-c(-28.5,-27.5)

    Region<-c('D02A','D02B', 'D03', 'D10')
    RAmin<-format(c(min(D02A_RA), min(D02B_RA), min(D03_RA), min(D10_RA)), digits=4, nsmall=4)
    RArange<-format(c(D02A_RA[2]-D02A_RA[1],D02B_RA[2]-D02B_RA[1], D03_RA[2]-D03_RA[1], D10_RA[2]-D10_RA[1]), digits=4, nsmall=4)
    DECmin<-format(c(min(D02A_DEC), min(D02B_DEC), min(D03_DEC), min(D10_DEC)), digits=4, nsmall=4)
    DECrange<-format(c(D02A_DEC[2]-D02A_DEC[1], D02B_DEC[2]-D02B_DEC[1], D03_DEC[2]-D03_DEC[1], D10_DEC[2]-D10_DEC[1]), digits=4, nsmall=4)
    Skirt<-c(0.4,0.4,0.4,0.4)
    Year<-rep(year, 4)
    Sem<-rep(semester,4)
    Run<- rep(run,4)
    Loc<-c('D02A','D02B', 'D03', 'D10')
    Denpri<-c(4,4,4,4)
    MainSclass<-c(1,1,1,1)
    LoPclass<-c(4,4,4,4)
    ByDen<-c(FALSE,FALSE,FALSE, FALSE)

    InfoTab<-data.frame(Region,RAmin,RArange,DECmin,DECrange,Skirt,Year, Sem, Run, Loc, Denpri, MainSclass,LoPclass,ByDen)


     if (verbose>1){cat('    - Making DOcat README as:',paste(dir,date,"_README.txt", sep=''), '\n')}
    write(paste('    - Making DOcat README as:',dir,date,"_README.txt",sep=''), file=logName, append=T)

    fileConn<-file(paste(dir,date,"_README.txt", sep=''))
    writeLines(paste('Date & Time: ',time,sep=''), fileConn)
    close(fileConn)
    write(paste('User: ',UserName,sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)

    write(paste('Input MASTERCat Table: ',MASTERCat, sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('Input GuideStar Table: ', GuideFile, sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('Input Standards Table: ', StdFile, sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('Input Sky Fibre Table: ', SkyFile, sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)


    if (verbose>1){cat('    - Making SurveyInfo file as:',paste(dir, "SurveyInfo.txt", sep=''), '\n')}
    write(paste('    - Making SurveyInfo file as:', dir, "SurveyInfo.txt",sep=''), file=logName, append=T)

    fileConn<-file(paste(dir,"SurveyInfo.txt", sep=''))
    writeLines(paste('# Date: ',time,sep=''), fileConn)
    close(fileConn)
    write(paste('# User: ',UserName,sep=''), file=paste(dir,"SurveyInfo.txt", sep=''), append=T)
    suppressWarnings(write.table(InfoTab, file=paste(dir,"SurveyInfo.txt", sep=''), append=T, row.names=F, quote=FALSE, sep = "\t"))

    DSkyCat<-read.table(SkyFile, header=T)
    SkyNames<-names(DSkyCat)
    DSkyCat<-data.frame(format(DSkyCat[,1], digits=12, nsmall=12), format(DSkyCat[,2], digits=12, nsmall=12))
    names(DSkyCat)<-SkyNames


    DGuideCat<-read.table(GuideFile, header=T)
    GuideNames<-names(DGuideCat)
    DGuideCat<-data.frame(DGuideCat[,1], format(DGuideCat[,2], digits=12, nsmall=12), format(DGuideCat[,3], digits=12, nsmall=12), format(DGuideCat[,4], digits=12, nsmall=12))
    names(DGuideCat)<-GuideNames

    DStdCat<-read.table(StdFile, header=T)
    StdNames<-names(DStdCat)
    DStdCat<-data.frame(DStdCat[,1], format(DStdCat[,2], digits=12, nsmall=12), format(DStdCat[,3], digits=12, nsmall=12), format(DStdCat[,4], digits=12, nsmall=12), DStdCat[,5])
    names(DStdCat)<-StdNames

    if (verbose>1){cat('    - Writing DSkyCat as:',paste(dir,'DSkyCat_',date,".tab", sep=''), '\n')}
    write(paste('    - Writing DSkyCat as:', dir,'DSkyCat_',date,".tab", sep=''), file=logName, append=T)
    if (verbose>1){cat('    - Writing DGuideCat as:',paste(dir,'DGuideCat_',date,".tab", sep=''), '\n')}
    write(paste('    - Writing DGuideCat as:', dir,'DGuideCat_',date,".tab", sep=''), file=logName, append=T)
    if (verbose>1){cat('    - Writing DStdCat as:',paste(dir,'DStdCat_',date,".tab", sep=''), '\n')}
    write(paste('    - Writing DStdCat as:', dir,'DStdCat_',date,".tab", sep=''), file=logName, append=T)

    write.table(DSkyCat, file=paste(dir,'DSkyCat_',date,".tab", sep=''), row.names=F, quote=FALSE, sep = "\t")
    write.table(DGuideCat, file=paste(dir,'DGuideCat_',date,".tab", sep=''), row.names=F, quote=FALSE, sep = "\t")
    write.table(DStdCat, file=paste(dir,'DStdCat_',date,".tab", sep=''), row.names=F, quote=FALSE, sep = "\t")

    load(MASTERCat)

    write(' ', file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Objects in MASTER Cat: ', dim(MASTERCat)[2], sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)


    selPrevSpec<-which(DMCat[,'ZSPEC_Prev']>0.0)
    selStar<-which((DMCat[,'MASK_FLAG']==0) & (DMCat[,'STARCLASS']==1 | DMCat[,'VISCLASS']==1))
    selMask<-which(DMCat[,'MASK_FLAG']==1)
    selJunk<-which(DMCat[,'VISCLASS']==4)
    PRIORITY<-DMCat[,'PRIORITY']
    write(paste('# Objects left to observe: ', length(which(PRIORITY>1)), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(' ', file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Previous Spec (PRIORITY=1) =',length(selPrevSpec), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Stars (PRIORITY=0) =',length(selStar), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Mask Region (PRIORITY=0) =',length(selMask), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Junk (PRIORITY=0) =',length(selJunk), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Y<19 Gal (PRIORITY=4) =',length(which(PRIORITY==4)), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# 19<Y<20 Gal (PRIORITY=5) =',length(which(PRIORITY==5)), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# 20<Y<21 Gal (PRIORITY=6) =',length(which(PRIORITY==6)), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Y>21 Gal (PRIORITY=7) =',length(which(PRIORITY==7 & DMCat[,'YMAG']>21)), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Observed but no redshift (PRIORITY=7) =',length(which(PRIORITY==7 & DMCat[,'YMAG']<=21)), sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)

    

    DMCat<-DMCat[which(DMCat[,'PRIORITY']>0),]

    SURVEY_CLASS<-rep(3, length(DMCat[,'RA']))
    SURVEY_CLASS[which(DMCat[,'PRIORITY']==0)]<-0

    tab<-data.frame(DMCat[,'CATAID'], format(DMCat[,'RA'], digits=12, nsmall=12), format(DMCat[,'DEC'], digits=12, nsmall=12),  format(DMCat[,'YMAG'], digits=12, nsmall=12), SURVEY_CLASS, DMCat[,'PRIORITY'],DMCat[,'FIELD'])
    names(tab)<-c('CATAID','RA','DEC','MAG','SURVEY_CLASS','PRIORITY_CLASS','POSITION')


    if (verbose>1){cat('    - Writing Target cat as:',paste(dir,'DObjCat_',date,".tab", sep=''), '\n')}
    write(paste('    - Writing Target cat as:', dir,'DObjCat_',date,".tab", sep=''), file=logName, append=T)
    
    suppressWarnings(write.table(tab, file=paste(dir,'DObjCat_',date,'.tab', sep=''), append=F, col.names=T, row.names=F, quote=FALSE, sep = "\t"))


    #system(paste('tar -cf ',substr(dir, 1, nchar(dir)-1), '.tar ', substr(dir, 1, nchar(dir)-1),sep=''))

   
    
    write('', file=paste(dir,date,"_README.txt",sep=''), append=T)
    write(paste('# Objects in DOcat (PRIORITY>0)  =',dim(DMCat)[1], sep=''), file=paste(dir,date,"_README.txt",sep=''), append=T)
          
    
    system(paste('cp ', dir,'SurveyInfo.txt ', dir,'Tiling/',sep=''))
    return(dir)

}
