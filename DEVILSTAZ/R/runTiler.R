#' Produce fibre configuration files for a givne set of DOCats
#'
#' @description This is the highlevel main TAZ function for running running the Tiler software
#' to gnerate fibre configuration files. Users must provide DOcats for targets, sky, standards and guides.
#'  
#' @param configdir Directory path location of Configure software
#' @param workingDir The directory you want to do the tiling in
#' @param DOcat A target catalogue
#' @param DATAguide A guide star catalogue
#' @param DATAstspec A standard star catalogue
#' @param DATAsky A sky potions catalogue
#' @param N_D02A Number of configurations to generate in D02A
#' @param N_D02B Number of configurations to generate in D02B
#' @param N_D03 Number of configurations to generate in D03
#' @param N_D10 Number of configurations to generate in D10
#' @param D02A_startPlate Start plate number of D02A configurations (0 or 1)
#' @param D02A_startPlate Start plate number of D02B configurations (0 or 1)
#' @param D03_startPlate Start plate number of D03 configurations (0 or 1)
#' @param D10_startPlate Start plate number of D10 configurations (0 or 1)
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param cores number of cores to use (max four in this case) - currently redundant
#' @param makeNormal Make general configurations 
#' @param makeBackUp Also make configuration files for bright sources. 
#' @param BrightCut Magnitude to cut at for bright sources. Only takes affect it makeBackUp==TRUE.
#' @return List of paths to new configuration files
#' @examples 
#' runTiler(configdir='/Applications/configure-8.4-MacOsX_ElCapitan_x86_64',workingDir='.', DOcat='data/observing/run1_2017_12/2017_12_19/DOCats/DObjCat_2017_12_19.tab, $
#'  DATAguide=data/observing/run1_2017_12/2017_12_19/DOCats/DGuideCat_2017_12_19.tab,DATAstspec=data/observing/run1_2017_12/2017_12_19/DOCats/DStdCat_2017_12_19.tab, $ 
#'  DATAsky=data/observing/run1_2017_12/2017_12_19/DOCats/DGSkyCat_2017_12_19.tab, N_D02A=1, N_D02B=1, N_D03=2, N_D10=3,  D02A_startPlate=0, D0BA_startPlate=1, D03_startPlate=0, $  
#'  D10_startPlate=1, logName='tempLog.txt', verbose=1, cores=4)
#'  # will make one configuration in D02A and D02B, two in D03, and 3 in D10.
#' @export
runTiler<-function(configdir=configdir, workingDir=workingDir, DOcat=DOcat, DATAguide=DATAguide, DATAstspec=DATAstspec, DATAsky=DATAsky, N_D02A=N_D02A, N_D02B=N_D02B, N_D03=N_D03, N_D10=N_D10, D02A_startPlate=0, D02B_startPlate=0, D03_startPlate=0, D10_startPlate=0, logName=logName, verbose=verbose, cores=cores, makeNormal=TRUE, makeBackUp=FALSE,BrightCut=20){

    #registerDoParallel(cores=cores)

    if (verbose>1){cat('        - Setting up plate sequence....', '\n')}
    write('        - Setting up plate sequence....', file=logName, append=T)
    
    plate_D02A<-c()
    if (N_D02A==1) {plate_D02A<-D02A_startPlate}
    if (N_D02A>1) {
        int<-D02A_startPlate        
        for (i in 1:N_D02A){
            plate_D02A<-c(plate_D02A,int)
            if (int==0){int<-1}else{int<-0}
        }
    }

    plate_D02B<-c()
    if (N_D02B==1) {plate_D02B<-D02B_startPlate}
    if (N_D02B>1) {
        int<-D02B_startPlate        
        for (i in 1:N_D02B){
            plate_D02B<-c(plate_D02B,int)
            if (int==0){int<-1}else{int<-0}
        }
    }

    plate_D03<-c()
    if (N_D03==1) {plate_D03<-D03_startPlate}
    if (N_D03>1) {
        int<-D03_startPlate        
        for (i in 1:N_D03){
            plate_D03<-c(plate_D03,int)
            if (int==0){int<-1}else{int<-0}
        }
    }

    plate_D10<-c()
    if (N_D10==1) {plate_D10<-D10_startPlate}
    if (N_D10>1) {
        int<-D10_startPlate        
        for (i in 1:N_D10){
            plate_D10<-c(plate_D10,int)
            if (int==0){int<-1}else{int<-0}
        }
    }

    if (verbose>1){cat('        - Running Tiling....', '\n')}
    write('        - Running Tiling....', file=logName, append=T)

    #oldWD<-getwd()
                                        #setwd(workingDir)

    tileplus_M<-c(N_D02A,N_D02B,N_D03,N_D10)
    position_M<-c('D02A','D02B', 'D03', 'D10')
    plate_M<-c(plate_D02A, plate_D02B, plate_D03, plate_D10)
    
    DOcat=read.table(DOcat,header=T)
    DATAguide<<-read.table(DATAguide,header=T) 
    DATAstspec<<-read.table(DATAstspec,header=T)
    DATAsky<<-read.table(DATAsky,header=T)
    
    DOcatBright<-DOcat[which((DOcat[,'MAG']<BrightCut & DOcat[,'PRIORITY_CLASS']>1) | DOcat[,'PRIORITY_CLASS']==9 | DOcat[,'PRIORITY_CLASS']==10),]
    
    #**** Mapping down to bump up OzDES fillers:
    DOcat[which(DOcat[,'PRIORITY_CLASS']==4),'PRIORITY_CLASS']<-3
    DOcat[which(DOcat[,'PRIORITY_CLASS']==5),'PRIORITY_CLASS']<-4
    DOcat[which(DOcat[,'PRIORITY_CLASS']==6),'PRIORITY_CLASS']<-5
    DOcat[which(DOcat[,'PRIORITY_CLASS']==7),'PRIORITY_CLASS']<-6
    DOcat[which(DOcat[,'PRIORITY_CLASS']==8),'PRIORITY_CLASS']<-7
    
    DOcat[which(DOcat[,'PRIORITY_CLASS']==9),'PRIORITY_CLASS']<-8
    DOcat[which(DOcat[,'PRIORITY_CLASS']==10),'PRIORITY_CLASS']<-9
    
    #**** Flip for bright:
    tmp<-DOcatBright
    tmp[which(DOcatBright[,'PRIORITY_CLASS']==4),'PRIORITY_CLASS']<-7
    tmp[which(DOcatBright[,'PRIORITY_CLASS']==5),'PRIORITY_CLASS']<-6
    tmp[which(DOcatBright[,'PRIORITY_CLASS']==6),'PRIORITY_CLASS']<-5
    tmp[which(DOcatBright[,'PRIORITY_CLASS']==7),'PRIORITY_CLASS']<-4
    tmp[which(DOcatBright[,'PRIORITY_CLASS']==8),'PRIORITY_CLASS']<-3
    DOcatBright<-tmp
    
    DOcatBright[which(DOcatBright[,'PRIORITY_CLASS']==9),'PRIORITY_CLASS']<-8
    DOcatBright[which(DOcatBright[,'PRIORITY_CLASS']==10),'PRIORITY_CLASS']<-9
    
    
    #configdirFiles=paste(configdir,'/data_files',sep='')
    updateExtFibs(configdir=configdir)
    
    
    #a = foreach(i=1:length(tileplus_M)) %dopar%  {
    #  Tiler(tileplus=tileplus_M[i], position=position_M[i], plate=plate_M[i], runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',tileplus_M[i]), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')
    #}
      
    #if (N_D02A>0){Tiler(tileplus=N_D02A, position='D02A', plate=plate_D02A, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D02A), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')}
    #if (N_D02B>0){Tiler(tileplus=N_D02B, position='D02B', plate=plate_D02B, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D02B), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')}
    #if (N_D03>0){Tiler(tileplus=N_D03, position='D03', plate=plate_D03, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D03), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')}
    #if (N_D10>0){Tiler(tileplus=N_D10, position='D10', plate=plate_D10, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D10), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')}

    ConfigNames<-c()
    
    if (makeNormal==TRUE){
    
    if (N_D02A>0){
      for (i in 1:N_D02A){
        Tiler(tileplus=1, position='D02A', plate=plate_D02A[i], runfolder=TRUE, TileCat=DOcat, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')
        
        configFile<-list.files(path=paste(workingDir,'/D02A/TargetFork',i,'-',i,'P',plate_D02A[i],sep=''), pattern='*.lis')
        configFile<-paste(workingDir,'/D02A/TargetFork',i,'-',i,'P',plate_D02A[i],'/',configFile, sep='')
        
        #cat(red('New Config File=',configFile),'\n')
        #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        
        Config<-readLines(configFile)
        Config<-Config[10:409]
        ID<-c()
        TYPE<-c()
        
        for (i in 1:length(Config)){
          tmp<-strsplit(Config[i], ' ')[[1]]
          tmp<-tmp[which(tmp!="")]
          if (length(tmp)==16){
            ID<-c(ID,tmp[3])
            TYPE<-c(TYPE,tmp[16])
          }
        }
        obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D2')],2,nchar(ID[which(substr(ID,1,2)=='D2')])))
        DOcat[which(DOcat[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
      }
      #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
      #cat('\n')
      
      
      
    }
    
    if (N_D02B>0){
      for (i in 1:N_D02B){
        Tiler(tileplus=1, position='D02B', plate=plate_D02B[i], runfolder=TRUE, TileCat=DOcat, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')
        
        configFile<-list.files(path=paste(workingDir,'/D02B/TargetFork',i,'-',i,'P',plate_D02B[i],sep=''), pattern='*.lis')
        configFile<-paste(workingDir,'/D02B/TargetFork',i,'-',i,'P',plate_D02B[i],'/',configFile, sep='')
        
        #cat(red('New Config File=',configFile),'\n')
        #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        
        Config<-readLines(configFile)
        Config<-Config[10:409]
        ID<-c()
        TYPE<-c()
        
        for (i in 1:length(Config)){
          tmp<-strsplit(Config[i], ' ')[[1]]
          tmp<-tmp[which(tmp!="")]
          if (length(tmp)==16){
            ID<-c(ID,tmp[3])
            TYPE<-c(TYPE,tmp[16])
          }
        }
        obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D2')],2,nchar(ID[which(substr(ID,1,2)=='D2')])))
        DOcat[which(DOcat[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
      }
      #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
      #cat('\n')
      
      
      
    }
    
    
    if (N_D03>0){
      for (i in 1:N_D03){
        Tiler(tileplus=1, position='D03', plate=plate_D03[i], runfolder=TRUE, TileCat=DOcat, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')
        
        configFile<-list.files(path=paste(workingDir,'/D03/TargetFork',i,'-',i,'P',plate_D03[i],sep=''), pattern='*.lis')
        configFile<-paste(workingDir,'/D03/TargetFork',i,'-',i,'P',plate_D03[i],'/',configFile, sep='')
        
        #cat(red('New Config File=',configFile),'\n')
        #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        
        Config<-readLines(configFile)
        Config<-Config[10:409]
        ID<-c()
        TYPE<-c()
        
        for (i in 1:length(Config)){
          tmp<-strsplit(Config[i], ' ')[[1]]
          tmp<-tmp[which(tmp!="")]
          if (length(tmp)==16){
            ID<-c(ID,tmp[3])
            TYPE<-c(TYPE,tmp[16])
          }
        }
        obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D3')],2,nchar(ID[which(substr(ID,1,2)=='D3')])))
        DOcat[which(DOcat[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
      }
      #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
      #cat('\n')
      
      
      
    }
    
    
    if (N_D10>0){
      for (i in 1:N_D10){
        Tiler(tileplus=1, position='D10', plate=plate_D10[i], runfolder=TRUE, TileCat=DOcat, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDir, configdir=configdir, append_letter='D')
        
        configFile<-list.files(path=paste(workingDir,'/D10/TargetFork',i,'-',i,'P',plate_D10[i],sep=''), pattern='*.lis')
        configFile<-paste(workingDir,'/D10/TargetFork',i,'-',i,'P',plate_D10[i],'/',configFile, sep='')
        
        #cat(red('New Config File=',configFile),'\n')
        #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        
        Config<-readLines(configFile)
        Config<-Config[10:409]
        ID<-c()
        TYPE<-c()
        
        for (i in 1:length(Config)){
          tmp<-strsplit(Config[i], ' ')[[1]]
          tmp<-tmp[which(tmp!="")]
          if (length(tmp)==16){
            ID<-c(ID,tmp[3])
            TYPE<-c(TYPE,tmp[16])
          }
        }
        obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D1')],2,nchar(ID[which(substr(ID,1,2)=='D1')])))
        DOcat[which(DOcat[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
      }
      #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
      #cat('\n')
      
      
      
    }
    #setwd(oldWD)

    pathD02A<-paste(workingDir,'/D02A/',list.files(path=paste(workingDir,'/D02A',sep=''), pattern='Targ*'), sep='')
    pathD02B<-paste(workingDir,'/D02B/',list.files(path=paste(workingDir,'/D02B',sep=''), pattern='Targ*'), sep='')
    pathD03<-paste(workingDir,'/D03/',list.files(path=paste(workingDir,'/D03',sep=''), pattern='Targ*'), sep='')
    pathD10<-paste(workingDir,'/D10/',list.files(path=paste(workingDir,'/D10',sep=''), pattern='Targ*'), sep='')
    
    count<-c(0,0,0,0)

    while (sum(count)<4){
      
      countD02A<-0
      for (i in 1:length(pathD02A)){
        countD02A<-countD02A+length(list.files(path=pathD02A[i], pattern='*.lis'))
      }
      countD02B<-0
      for (i in 1:length(pathD02B)){
        countD02B<-countD02B+length(list.files(path=pathD02B[i], pattern='*.lis'))
      }
      countD03<-0
      for (i in 1:length(pathD03)){
        countD03<-countD03+length(list.files(path=pathD03[i], pattern='*.lis'))
      }
      countD10<-0
      for (i in 1:length(pathD10)){
        countD10<-countD10+length(list.files(path=pathD10[i], pattern='*.lis'))
      }
        
        if (countD02A>=N_D02A) {count[1]<-1}
        if (countD02B>=N_D02B) {count[2]<-1}
        if (countD03>=N_D03) {count[3]<-1}
        if (countD10>=N_D10) {count[4]<-1}

    }
    

    if (verbose>1){cat('        - Tiling Complete', '\n')}
    write('        - Tiling Complete', file=logName, append=T)

    dateF<-strsplit(workingDir, '/')[[1]][4]
    dateF2<-paste(c(strsplit(workingDir, '/')[[1]][1:4], dateF), sep='', collapse='/')

    if (N_D02A>0){
      for (i in 1:length(pathD02A)){
        system(paste('cp ',pathD02A[i],'/D02A* ', workingDir,'/TileFiles/',sep=''))
      }
    }
    if (N_D02B>0){
      for (i in 1:length(pathD02B)){
      system(paste('cp ',pathD02B[i],'/D02B* ', workingDir,'/TileFiles/',sep=''))
      }
    }
    if (N_D03>0){
      for (i in 1:length(pathD03)){
        system(paste('cp ',pathD03[i],'/D03* ', workingDir,'/TileFiles/',sep=''))
      }
    }
    if (N_D10>0){
      for (i in 1:length(pathD10)){
        system(paste('cp ',pathD10[i],'/D10* ', workingDir,'/TileFiles/',sep=''))
      }
    }

    listF<-list.files(path=paste(workingDir,'/TileFiles/',sep=''),pattern='*')
    listM<-list.files(path=paste(workingDir,'/TileFiles/',sep=''),pattern='*.lis')

    for (j in 1:length(listF)){
        system(paste('mv ',workingDir,'/TileFiles/',listF[j] ,' ', workingDir,'/TileFiles/', substr(listF[j],1,3), '_', dateF,'_',substr(listF[j],5,nchar(listF[j])), sep=''))
    }
    
    for (j in 1:length(listM)){
      ConfigNames<-c(ConfigNames,paste(workingDir,'/TileFiles/', substr(listM[j],1,3), '_', dateF,'_',substr(listM[j],5,nchar(listM[j])), sep=''))
    }
    
}
    
    
    
    
    
    if (makeBackUp==TRUE){
      
      workingDirBright<-paste(workingDir,'Backup', sep='')
      system(paste('mkdir ',workingDirBright, sep=''))
      system(paste('mkdir ',workingDirBright,'/TileFiles', sep=''))
      system(paste('cp ', workingDir,'/SurveyInfo.txt ', workingDirBright,'/',sep=''))
      
      if (N_D02A>0){
        for (i in 1:N_D02A){
          Tiler(tileplus=1, position='D02A', plate=plate_D02A[i], runfolder=TRUE, TileCat=DOcatBright, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDirBright, configdir=configdir, append_letter='D')
          
          configFile<-list.files(path=paste(workingDirBright,'/D02A/TargetFork',i,'-',i,'P',plate_D02A[i],sep=''), pattern='*.lis')
          configFile<-paste(workingDirBright,'/D02A/TargetFork',i,'-',i,'P',plate_D02A[i],'/',configFile, sep='')
          
          #cat(red('New Config File=',configFile),'\n')
          #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcatBright[,'PRIORITY_CLASS']==1))),'\n')
          
          Config<-readLines(configFile)
          Config<-Config[10:409]
          ID<-c()
          TYPE<-c()
          
          for (i in 1:length(Config)){
            tmp<-strsplit(Config[i], ' ')[[1]]
            tmp<-tmp[which(tmp!="")]
            if (length(tmp)==16){
              ID<-c(ID,tmp[3])
              TYPE<-c(TYPE,tmp[16])
            }
          }
          obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D2')],2,nchar(ID[which(substr(ID,1,2)=='D2')])))
          DOcatBright[which(DOcatBright[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
        }
        #cat(red('New PRIORITY CLASS=',length(which(DOcatBright[,'PRIORITY_CLASS']==1))),'\n')
        #cat('\n')
        
        
        
      }
      
      if (N_D02B>0){
        for (i in 1:N_D02B){
          Tiler(tileplus=1, position='D02B', plate=plate_D02B[i], runfolder=TRUE, TileCat=DOcatBright, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDirBright, configdir=configdir, append_letter='D')
          
          configFile<-list.files(path=paste(workingDirBright,'/D02B/TargetFork',i,'-',i,'P',plate_D02B[i],sep=''), pattern='*.lis')
          configFile<-paste(workingDirBright,'/D02B/TargetFork',i,'-',i,'P',plate_D02B[i],'/',configFile, sep='')
          
          #cat(red('New Config File=',configFile),'\n')
          #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
          
          Config<-readLines(configFile)
          Config<-Config[10:409]
          ID<-c()
          TYPE<-c()
          
          for (i in 1:length(Config)){
            tmp<-strsplit(Config[i], ' ')[[1]]
            tmp<-tmp[which(tmp!="")]
            if (length(tmp)==16){
              ID<-c(ID,tmp[3])
              TYPE<-c(TYPE,tmp[16])
            }
          }
          obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D2')],2,nchar(ID[which(substr(ID,1,2)=='D1')])))
          DOcatBright[which(DOcatBright[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
        }
        #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        #cat('\n')
        
        
        
      }
      
      
      if (N_D03>0){
        for (i in 1:N_D03){
          Tiler(tileplus=1, position='D03', plate=plate_D03[i], runfolder=TRUE, TileCat=DOcatBright, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDirBright, configdir=configdir, append_letter='D')
          
          configFile<-list.files(path=paste(workingDirBright,'/D03/TargetFork',i,'-',i,'P',plate_D03[i],sep=''), pattern='*.lis')
          configFile<-paste(workingDirBright,'/D03/TargetFork',i,'-',i,'P',plate_D03[i],'/',configFile, sep='')
          
          #cat(red('New Config File=',configFile),'\n')
          #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
          
          Config<-readLines(configFile)
          Config<-Config[10:409]
          ID<-c()
          TYPE<-c()
          
          for (i in 1:length(Config)){
            tmp<-strsplit(Config[i], ' ')[[1]]
            tmp<-tmp[which(tmp!="")]
            if (length(tmp)==16){
              ID<-c(ID,tmp[3])
              TYPE<-c(TYPE,tmp[16])
            }
          }
          obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D3')],2,nchar(ID[which(substr(ID,1,2)=='D3')])))
          DOcatBright[which(DOcatBright[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
        }
        #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        #cat('\n')
        
        
        
      }
      
      
      if (N_D10>0){
        for (i in 1:N_D10){
          Tiler(tileplus=1, position='D10', plate=plate_D10[i], runfolder=TRUE, TileCat=DOcatBright, runoffset=i, restrict=rep('all',1), updatefib=!exists('Fibres'), basedir=workingDirBright, configdir=configdir, append_letter='D')
          
          configFile<-list.files(path=paste(workingDirBright,'/D10/TargetFork',i,'-',i,'P',plate_D10[i],sep=''), pattern='*.lis')
          configFile<-paste(workingDirBright,'/D10/TargetFork',i,'-',i,'P',plate_D10[i],'/',configFile, sep='')
          
          #cat(red('New Config File=',configFile),'\n')
          #cat(red('Previous PRIORITY CLASS = 1:',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
          
          Config<-readLines(configFile)
          Config<-Config[10:409]
          ID<-c()
          TYPE<-c()
          
          for (i in 1:length(Config)){
            tmp<-strsplit(Config[i], ' ')[[1]]
            tmp<-tmp[which(tmp!="")]
            if (length(tmp)==16){
              ID<-c(ID,tmp[3])
              TYPE<-c(TYPE,tmp[16])
            }
          }
          obsID<-as.numeric(substr(ID[which(substr(ID,1,2)=='D1')],2,nchar(ID[which(substr(ID,1,2)=='D1')])))
          DOcatBright[which(DOcatBright[,'CATAID'] %in% obsID ==TRUE),'PRIORITY_CLASS']<-1
        }
        #cat(red('New PRIORITY CLASS=',length(which(DOcat[,'PRIORITY_CLASS']==1))),'\n')
        #cat('\n')
        
        
        
      }
      #setwd(oldWD)
      
      pathD02ABright<-paste(workingDirBright,'/D02A/',list.files(path=paste(workingDirBright,'/D02A',sep=''), pattern='Targ*'), sep='')
      pathD02BBright<-paste(workingDirBright,'/D02B/',list.files(path=paste(workingDirBright,'/D02B',sep=''), pattern='Targ*'), sep='')
      pathD03Bright<-paste(workingDirBright,'/D03/',list.files(path=paste(workingDirBright,'/D03',sep=''), pattern='Targ*'), sep='')
      pathD10Bright<-paste(workingDirBright,'/D10/',list.files(path=paste(workingDirBright,'/D10',sep=''), pattern='Targ*'), sep='')
      
      count<-c(0,0,0,0)
      
      while (sum(count)<4){
        
        countD02A<-0
        for (i in 1:length(pathD02ABright)){
          countD02A<-countD02A+length(list.files(path=pathD02ABright[i], pattern='*.lis'))
        }
        countD02B<-0
        for (i in 1:length(pathD02BBright)){
          countD02B<-countD02B+length(list.files(path=pathD02BBright[i], pattern='*.lis'))
        }
        countD03<-0
        for (i in 1:length(pathD03Bright)){
          countD03<-countD03+length(list.files(path=pathD03Bright[i], pattern='*.lis'))
        }
        countD10<-0
        for (i in 1:length(pathD10Bright)){
          countD10<-countD10+length(list.files(path=pathD10Bright[i], pattern='*.lis'))
        }
        
        if (countD02A>=N_D02A) {count[1]<-1}
        if (countD02B>=N_D02B) {count[2]<-1}
        if (countD03>=N_D03) {count[3]<-1}
        if (countD10>=N_D10) {count[4]<-1}
        
      }
      
      if (verbose>1){cat('        - Bright Tiling Complete', '\n')}
      write('        - Bright Tiling Complete', file=logName, append=T)
      
      dateF<-strsplit(workingDirBright, '/')[[1]][4]
      dateF2<-paste(c(strsplit(workingDirBright, '/')[[1]][1:4], dateF), sep='', collapse='/')
      
      if (N_D02A>0){
        for (i in 1:length(pathD02ABright)){
          system(paste('cp ',pathD02ABright[i],'/D02A* ', workingDirBright,'/TileFiles/',sep=''))
        }
      }
      if (N_D02B>0){
        for (i in 1:length(pathD02BBright)){
          system(paste('cp ',pathD02BBright[i],'/D02B* ', workingDirBright,'/TileFiles/',sep=''))
        }
      }
      if (N_D03>0){
        for (i in 1:length(pathD03Bright)){
          system(paste('cp ',pathD03Bright[i],'/D03* ', workingDirBright,'/TileFiles/',sep=''))
        }
      }
      if (N_D10>0){
        for (i in 1:length(pathD10Bright)){
          system(paste('cp ',pathD10Bright[i],'/D10* ', workingDirBright,'/TileFiles/',sep=''))
        }
      }
      
      listF<-list.files(path=paste(workingDirBright,'/TileFiles/',sep=''),pattern='*')
      listM<-list.files(path=paste(workingDirBright,'/TileFiles/',sep=''),pattern='*.lis')

      
      for (j in 1:length(listF)){
        system(paste('mv ',workingDirBright,'/TileFiles/',listF[j] ,' ', workingDirBright,'/TileFiles/', substr(listF[j],1,3), '_', dateF,'_BackUp_',substr(listF[j],5,nchar(listF[j])), sep=''))
      }
      
      for (j in 1:length(listM)){
        ConfigNames<-c(ConfigNames,paste(workingDirBright,'/TileFiles/', substr(listM[j],1,3), '_', dateF,'_BackUp_',substr(listM[j],5,nchar(listM[j])), sep=''))
      }
      
      
    } 
    

    return(ConfigNames)
    

}
