#' Produce fibre configuration files for a givne set of DOCats
#'
#' @description This is the highlevel main TAZ function for running running the Tiler software
#' to gnerate fibre configuration files. Users must provide DOcats for targets, sky, standards and guides.
#'  
#' @param configdir Directory path location of Configure software
#' @param workigDir The directory you want to do the tiling in
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
#' @return List of paths to new configuration files
#' @examples 
#' runTiler(configdir='/Applications/configure-8.4-MacOsX_ElCapitan_x86_64',workigDir='.', DOcat='data/observing/run1_2017_12/2017_12_19/DOCats/DObjCat_2017_12_19.tab, $
#'  DATAguide=data/observing/run1_2017_12/2017_12_19/DOCats/DGuideCat_2017_12_19.tab,DATAstspec=data/observing/run1_2017_12/2017_12_19/DOCats/DStdCat_2017_12_19.tab, $ 
#'  DATAsky=data/observing/run1_2017_12/2017_12_19/DOCats/DGSkyCat_2017_12_19.tab, N_D02A=1, N_D02B=1, N_D03=2, N_D10=3,  D02A_startPlate=0, D0BA_startPlate=1, D03_startPlate=0, $  
#'  D10_startPlate=1, logName='tempLog.txt', verbose=1, cores=4)
#'  # will make one configuration in D02A and D02B, two in D03, and 3 in D10.
#' @export
runTiler<-function(configdir=configdir, workigDir=workigDir, DOcat=DOcat, DATAguide=DATAguide, DATAstspec=DATAstspec, DATAsky=DATAsky, N_D02A=N_D02A, N_D02B=N_D02B, N_D03=N_D03, N_D10=N_D10, D02A_startPlate=0, D02B_startPlate=0, D03_startPlate=0, D10_startPlate=0, logName=logName, verbose=verbose, cores=cores){

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
                                        #setwd(workigDir)

    tileplus_M<-c(N_D02A,N_D02B,N_D03,N_D10)
    position_M<-c('D02A','D02B', 'D03', 'D10')
    plate_M<-c(plate_D02A, plate_D02B, plate_D03, plate_D10)
    
    DOcat=read.table(DOcat,header=T)
    DATAguide<<-read.table(DATAguide,header=T) 
    DATAstspec<<-read.table(DATAstspec,header=T)
    DATAsky<<-read.table(DATAsky,header=T)
    
    #a = foreach(i=1:length(tileplus_M)) %dopar%  {
    #  Tiler(tileplus=tileplus_M[i], position=position_M[i], plate=plate_M[i], runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',tileplus_M[i]), updatefib=!exists('Fibres'), basedir=workigDir, configdir=configdir, append_letter='D')
    #}
      
    if (N_D02A>0){Tiler(tileplus=N_D02A, position='D02A', plate=plate_D02A, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D02A), updatefib=!exists('Fibres'), basedir=workigDir, configdir=configdir, append_letter='D')}
    if (N_D02B>0){Tiler(tileplus=N_D02B, position='D02B', plate=plate_D02B, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D02B), updatefib=!exists('Fibres'), basedir=workigDir, configdir=configdir, append_letter='D')}
    if (N_D03>0){Tiler(tileplus=N_D03, position='D03', plate=plate_D03, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D03), updatefib=!exists('Fibres'), basedir=workigDir, configdir=configdir, append_letter='D')}
    if (N_D10>0){Tiler(tileplus=N_D10, position='D10', plate=plate_D10, runfolder=TRUE, TileCat=DOcat, runoffset=1, restrict=rep('all',N_D10), updatefib=!exists('Fibres'), basedir=workigDir, configdir=configdir, append_letter='D')}

    #setwd(oldWD)

    pathD02A<-paste(workigDir,'/D02A/',list.files(path=paste(workigDir,'/D02A',sep=''), pattern='Targ*'), sep='')
    pathD02B<-paste(workigDir,'/D02B/',list.files(path=paste(workigDir,'/D02B',sep=''), pattern='Targ*'), sep='')
    pathD03<-paste(workigDir,'/D03/',list.files(path=paste(workigDir,'/D03',sep=''), pattern='Targ*'), sep='')
    pathD10<-paste(workigDir,'/D10/',list.files(path=paste(workigDir,'/D10',sep=''), pattern='Targ*'), sep='')
    
    count<-c(0,0,0,0)

    while (sum(count)<4){     
        
        if (length(list.files(path=pathD02A, pattern='*.lis'))>=N_D02A) {count[1]<-1}
        if (length(list.files(path=pathD02B, pattern='*.lis'))>=N_D02B) {count[2]<-1}
        if (length(list.files(path=pathD03, pattern='*.lis'))>=N_D03) {count[3]<-1}
        if (length(list.files(path=pathD10, pattern='*.lis'))>=N_D10) {count[4]<-1}

    }

    if (verbose>1){cat('        - Tiling Complete', '\n')}
    write('        - Tiling Complete', file=logName, append=T)

    dateF<-strsplit(workigDir, '/')[[1]][4]
    dateF2<-paste(c(strsplit(workigDir, '/')[[1]][1:4], dateF), sep='', collapse='/')

    if (N_D02A>0){system(paste('cp ',pathD02A,'/D02A* ', workigDir,'/TileFiles/',sep=''))}
    if (N_D02B>0){system(paste('cp ',pathD02B,'/D02B* ', workigDir,'/TileFiles/',sep=''))}
    if (N_D03>0){system(paste('cp ',pathD03,'/D03* ', workigDir,'/TileFiles/',sep=''))}
    if (N_D10>0){system(paste('cp ',pathD10,'/D10* ', workigDir,'/TileFiles/',sep=''))}

    listF<-list.files(path=paste(workigDir,'/TileFiles/',sep=''),pattern='*')
    listM<-list.files(path=paste(workigDir,'/TileFiles/',sep=''),pattern='*.lis')
    ConfigNames<-c()
    
    for (j in 1:length(listF)){
        system(paste('mv ',workigDir,'/TileFiles/',listF[j] ,' ', workigDir,'/TileFiles/', substr(listF[j],1,3), '_', dateF,'_',substr(listF[j],5,nchar(listF[j])), sep=''))
    }
    
    for (j in 1:length(listM)){
      ConfigNames<-c(ConfigNames,paste(workigDir,'/TileFiles/', substr(listM[j],1,3), '_', dateF,'_',substr(listM[j],5,nchar(listM[j])), sep=''))
    }
    
    return(ConfigNames)
    

}
