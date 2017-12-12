#' Top level funtion for running full DEVILS Tool for Analaysis and Redshifting Pipeline.
#'
#' @description This is the highest level main TAZ function for running all over functions
#' in an easily mangable mode. With the inputs provided in this function and the DEVILS directory 
#' strucutre the full reducution and and analysis of DEVILS data can be undertaken. 
#'  
#' @param user The current use identification. This is added to all logs.
#' @param workingDir The top level directory where the DEVILS data/... directory is found

#' @param dobizCheck TRUE/FALSE, Check files are in correct part of file structure using bizCheck.R
#' @param bizStopError TRUE/FLASE if a file is ofund in the wrong place, should the code be stopped?
#' @param doCalibQC TRUE/FALSE, Produce bias and dark frame QC plots using checkQC.R
#' @param doReduce TRUE/FALSE, Perform 2dFDR reduction using run2dfDR.R 
#' @param toReduceDirs directory path to reduce in the DEVILS data structure. Must be of the form, 
#' data/rawdata/run#_YYYY_MM/YYYY_MM_DD (i.e. data/rawdata/run1_2017_12/2017_12_18). *** NOTE: IF YOU
#' WITH TAZ TO IDENTIFY ALL UNDREDUCED DATES SET toReduceDirs='NA'** Also, this will overwrite any data in the 
#' corresponding reduced directory for that night, so use with care!  
#' @param zeroPoint 
#' @param doExtract TRUE/FALSE, Perform 1D spectral extraction using extractNewSpec.R 
#' @param toExtractFiles If doReduce=F and doExtract=T, provide a string vector of the reduced files to 
#' extract. These must have the full path (i.e. data/reduced/run1_2017_12/2017_12_18/2017_12_18_config_1_reduced.fits) 
#' @param doStack TRUE/FALSE, Perform stacking of spectra with IDs using stackSpec.R 
#' @param toStackIDs If doExtract=F and doStack=T, provide a string vector of IDs you wish to stack. Can be a list of IDs 
#' or set to 'all' which will stack all unique IDs in the 'data/reduced/allSpec/' directory.
#' @param doAutoZ TRUE/FALSE, Perform AutoZ redshift fitting using runAutoZ.R 
#' @param toAutoZStacks  If doStack=F and doAutoZ=T, provide vector list of file paths to run AutoZ over. 
#' Must either be full directory path, or can set to 'all' to run over all spectra in the 'data/reduced/stackedSpec/' directory.  
#' @param doUpdateMaster TRUE/FALSE, Update and generate a new DMCat (with timestamp) using the redshift measurement in the 
#' data/reduced/stackedSpec/ directory. Then make DOCats for the next observing date.
#' @param doTiler TRUE/FALSE, Run tiling software to generate new fibre configuration files using runTiler.R
#' @param DODir If doUpdateMaster=F and doTiler=T, provide directory path to DOCats directory you with to uses for configurations
#' @param N_D02A Number of configurations to generate in D02A for runTiler.R
#' @param N_D02B Number of configurations to generate in D02B for runTiler.R
#' @param N_D03 Number of configurations to generate in D03 for runTiler.R
#' @param N_D10 Number of configurations to generate in D10 for runTiler.R
#' @param D02A_startPlate Start plate number of D02A configurations (0 or 1) for runTiler.R
#' @param D02A_startPlate Start plate number of D02B configurations (0 or 1) for runTiler.R
#' @param D03_startPlate Start plate number of D03 configurations (0 or 1) for runTiler.R
#' @param D10_startPlate Start plate number of D10 configurations (0 or 1) for runTiler.R
#' @param configdir Directory path to Configuration software 
#' @param addOzDES Add OzDES filer tragetes to DOCat wil high priority
#' @param OzDESCat Path to current OzDES filler catalogue
#' @param cores number of cores to use in run2dfDR.R, runAutoZ.R, and runTiler.R 
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @examples 
#' # Detailed descriptions of the this function and how to use it can be found in the TAZ manual 
#' here: https://github.com/ICRAR/DEVILS-TAZ/blob/master/DEVILS_Manuals/TAZ_Manual.pdf . However, 
#' below are a few quick examples:
#' 
#' # Basic running of simple reduction and 1D extraction of data:
#' TAZ(user='NewUser', workingDir='.', doReduce=T, zeroPoint=T, doExtract=T, doStack=F, doAutoZ=F, cores=4, doUpdateMaster=F, doTiler=F,verbose=2)
#' 
#' Reduce data in a specific raw directory:
#' TAZ(user='NewUser', workingDir='.', doReduce=T, toReduceDirs='data/rawdata/run1_2017_12/2017_12_18', zeroPoint=F, doExtract=F, doStack=F, doAutoZ=F, cores=4, doUpdateMaster=F, doTiler=F,verbose=2)
#' 
#' # Stacking already reduced and extracted spectra and running AutoZ:
#' TAZ(user='NewUser', workingDir='.', doReduce=F, zeroPoint=F, doExtract=F, doStack=T, toStackIDs='all', doAutoZ=T, cores=4, doUpdateMaster=F, doTiler=F,verbose=2)
#'   
#' # Updateing catalogues with the new redshifts and running the tiling:
#' TAZ(user='NewUser', workingDir='.', doReduce=F, zeroPoint=F, doExtract=F, doStack=F, doAutoZ=F, cores=4, doUpdateMaster=T, doTiler=T, verbose=2, N_D02A=1, N_D02B=1, $
#' N_D03=2, N_D10=3,  D02A_startPlate=0, D0BA_startPlate=1, D03_startPlate=0, D10_startPlate=1, configdir='/Applications/configure-8.4-MacOsX_ElCapitan_x86_64') 
#' 
#' # Running the full TAZ pipeline from end to end with everyhting turned on:
#' TAZ(user='NewUser', workingDir='.', doCalibQC=T, doReduce=T, zeroPoint=T, doExtract=T, doStack=T, doAutoZ=T, cores=4, doUpdateMaster=T, doTiler=T, verbose=2, N_D02A=1, N_D02B=1, $
#' N_D03=2, N_D10=3,  D02A_startPlate=0, D0BA_startPlate=1, D03_startPlate=0, D10_startPlate=1, configdir='/Applications/configure-8.4-MacOsX_ElCapitan_x86_64') 
#' 
#' @export
#' 
TAZ<-function(user='ldavies', workingDir='/Users/luke/work/DEVILS/TAZ/',  dobizCheck=T, bizStopError=F, doCalibQC=F, doReduce=T, toReduceDirs='NA', zeroPoint=T, doExtract=T, toExtractFiles='NA', doStack=T, toStackIDs='NA', doAutoZ=T, toAutoZStacks='NA', doUpdateMaster=T, doTiler=T, DODir='NA',N_D02A=1,N_D02B=1, N_D03=1, N_D10=1, D02A_startPlate=0, D02B_startPlate=0, D03_startPlate=0, D10_startPlate=0,configdir='/Applications/configure-8.4-MacOsX_ElCapitan_x86_64',  addOzDES=FALSE, OzDESCat='NA', cores=cores, verbose=2){
  
  if (doReduce==T){
    tmp<-tryCatch(system2('which', args='aaorun', stdout=TRUE))
    tmp<-tmp[1]
    if (is.na(tmp)==TRUE){
      cat('**** WARNING doReduce=TRUE AND NO AAORUN COMMAND FOUND ****', '\n')
      cat('Please check you have 2dFDR reuction software installed from here -  https: //www.aao.gov.au/science/software/2dfdr', '\n')
      cat('If so, please check you have the aaorun function correctly set in your path, try "which aaorun" in a terminal', '\n')
      cat('**** ENDING TAZ ****', '\n', '\n')
      return(NULL)
    }
  }
  
  
  if (doTiler==T){
    tmp<-tryCatch(system2('which', args='configure', stdout=TRUE))
    tmp<-tmp[1]
    if (is.na(tmp)==TRUE){
      cat('**** WARNING doTiler=TRUE AND NO CONFIGURE COMMAND FOUND ****', '\n')
      cat('Please check you have 2dFDR configuration software installed from here -  https://www.aao. gov.au/science/software/configure', '\n')
      cat('If so, please check you have the configure function correctly set in your path, try "which configure" in a terminal', '\n')
      cat('**** ENDING TAZ ****', '\n', '\n')
      return(NULL)
    }
  }
  
  registerDoParallel(cores=cores)
  
  version<-0.1
  cat('\n')
  
  if (verbose==0){
    cat('** Running TAZ version',version, 'verbose set to 0 - you will recieve no messages **' , '\n')
  }
  
  if (verbose>0){
    cat('****************************************************', '\n')
    cat('********* STARTING TAZ version',version,'*************', '\n')
    cat('****************************************************', '\n')
    cat('\n')
    cat('Ready to reduce DEVILS data.....', '\n')
    cat('\n')
  }
  
  check<-list.files(path=paste(.libPaths(), '/DEVILSTAZ/data/calibrators/filters',sep=''), pattern='*.tab')
  if (length(check)==0){
    
    cat('*** WARNING calibration files not unpacked in DEVILSTAZ Data Directory ***', '\n')
    cat('   - Unpacking now.....', '\n')
    
    LibPaths<-.libPaths()
    
    DEVILSPATH<-NA
    
    for (jj in 1:length(LibPaths)){
      packagesList<-list.files(path=LibPaths[jj], pattern='*')
      if (length(which(packagesList=='DEVILSTAZ'))>0){DEVILSPATH<-LibPaths[jj]}
    }
    
    system(paste('tar -xvf ', DEVILSPATH, '/DEVILSTAZ/data/calibrators.tar --directory ', DEVILSPATH, '/DEVILSTAZ/data/',sep='')) 
    system(paste('tar -xvf ', DEVILSPATH, '/DEVILSTAZ/data/idxFiles.tar --directory ', DEVILSPATH, '/DEVILSTAZ/data/' ,sep=''))
    
  }
  
  
  setwd(workingDir)
  
  system('open /Applications/Utilities/XQuartz.app/')
  
  NowDate<-Sys.time()
  logName<-paste('data/logs/LogFile_',substr(NowDate, 1,10),'_',substr(NowDate, 12,13),'h',substr(NowDate, 15,16),'m.txt',sep='')
  
  if (verbose>0){
    cat('Setting up Log file as - ',logName, '\n')
  }
  
  
  fileConn<-file(logName)
  writeLines(paste('# Running DEVILS-TAZ on ', NowDate, sep=''), fileConn)
  close(fileConn)
  write(paste('# Version:',version, sep=''), file=logName, append=T)
  write(paste('# User:',user, sep=''), file=logName, append=T)
  write(paste('# Working Directory:',workingDir, sep=''), file=logName, append=T)
  write(paste('#Runnig TAZ with line: TAZ<-function(user=',user,', workingDir=',workingDir,',  dobizCheck=',dobizCheck,', bizStopError=',bizStopError,', doCalibQC=',doCalibQC,', doReduce=',doReduce,', toReduceDirs=',toReduceDirs,', zeroPoint=',zeroPoint,', doExtract=',doExtract,', toExtractFiles=',toExtractFiles,', doStack=',doStack,', toStackIDs=',toStackIDs,', doAutoZ=',doAutoZ,', toAutoZStacks=',toAutoZStacks,', doUpdateMaster=',doUpdateMaster,', doTiler=',doTiler,', DODir=',DODir,',N_D02A=',N_D02A,',N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02B_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,',configdir=',configdir,',  cores=',cores,', verbose=',verbose,')',sep=''), file=logName, append=T)
    
  write(' ', file=logName, append=T)
  
  
  if (dobizCheck==T){
    a<-1
    a<-bizCheck(workingDir='.', stopError=bizStopError, logName=logName, verbose=verbose)
    if (is.null(a)){return(NULL)}
  }
  
  if (doReduce==T){
    
    if (toReduceDirs=='NA'){
      
      if (verbose>0){cat('Finding unreduced raw datasets....', '\n')}
      write('Finding unreduced raw datasets....', file=logName, append=T)
    
      toReduce<-findNewData(logName=logName, verbose=verbose)
      
    }else{
      
      toReduce<-toReduceDirs
    }
    
    if (length(toReduce)>0){
      
      if (verbose>0){cat('Reducing new datasets....', '\n')}
      write('Reducing new datasets....', file=logName, append=T)
      
      newReduce<-run2dfDR(toReduce=toReduce, doCalibQC=doCalibQC, logName=logName, verbose=verbose, cores=cores)
      
      stdStars<-read.csv('data/calibrators/stdstars/stdStarCat.csv')
      
      if (verbose>0){cat('Flux calibrating new datasets....', '\n')}
      write('Flux calibrating new datasets....', file=logName, append=T)
      
      for (i in 1:length(newReduce)){
        
        if (verbose>0){cat('  -Flux calibrating: ', newReduce[i], '\n')}
        write(paste('  -Flux calibrating: ', newReduce[i],sep=''), file=logName, append=T)
        
        fluxCal(file=newReduce[i], stdStars=stdStars,logName=logName, verbose=verbose)    
      }
    }else{
      
      cat('*** WARNING NO NEW NIGHT TO REDUCE ***',  '\n')
      cat('Exiting TAZ, please set doReduce=F and provide a vector to toExtractFiles', '\n')
      write(paste('**** WARNING NO NEW NIGHT TO REDUCE ***',sep=''), file=logName, append=T)
      write(paste('Exiting TAZ, please set doReduce=F and provide a vector to toExtractFiles',sep=''), file=logName, append=T)
      return(NULL)
      
    }
    
    
  }
  
  if (doReduce==F){
    
    if (verbose>0){cat('*** doReduce=F so no reduction undertaken.', '\n')}
    write('*** doReduce=F so no reduction undertaken.', file=logName, append=T)
    
    if (doExtract==T){newReduce<-toExtractFiles}
    
  }
  
  if (doExtract==T){
    
    if (verbose>0){cat('Extracting 1D spectra....', '\n')}
    write('Extracting 1D spectra....', file=logName, append=T)
    
    newSpec<-c()
    newIds<-c()
    
    
    for (i in 1:length(newReduce)){
      
      if (verbose>0){cat('  -Extracting 1D spectra from: ', newReduce[i], '\n')}
      write(paste('  -Extracting 1D spectra from: ', newReduce[i],sep=''), file=logName, append=T)
      
      tmpnewSpec<-extractNewSpec(file=newReduce[i], logName=logName, verbose=verbose, makePlot=T, zeroPoint=zeroPoint)
      newSpec<-c(newSpec, as.character(tmpnewSpec$newSpec))
      newIds<-c(newIds, as.character(tmpnewSpec$newID))
      
    }
    
    write.csv(newSpec, file=paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newSpec.csv', sep=''), row.names=F, quote=F)
    write.csv(newIds, file=paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newIDs.csv', sep=''), row.names=F, quote=F)
    
    
  }
  
  if (doExtract==F){
    
    if (verbose>0){cat('*** doExtract=F so no extraction undertaken.', '\n')}
    write('*** doExtract=F so no extraction undertaken.', file=logName, append=T)
    
    if (doStack==T){ newIds<-toStackIDs}
    
  }
  
  
  
  
  if (doStack==T){
    
    
    if (verbose>0){cat('Stacking 1D spectra....', '\n')}
    write('Stacking 1D spectra....', file=logName, append=T)      
    
    newStacks<-stackSpec(ids=newIds,logName=logName, verbose=verbose, makePlot=T)
    write.csv(newStacks, file=paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newStacks.csv', sep=''), row.names=F, quote=F)
    
  }
  
  if (doStack==F & doAutoZ==T){
    
    if (verbose>0){cat('*** doStack=F so no stacking undertaken.', '\n')}
    write('*** doStack=F so no stacking undertaken.', file=logName, append=T)
    
    newStacks<-toAutoZStacks
    
  }
  
  
  if (doAutoZ==T){
    if (verbose>0){cat('Running AutoZ for new spectra....', '\n')}
    write('Running AutoZ for new spectra....', file=logName, append=T)
    
    runAutoZ(specs=newStacks, logName=logName, verbose=verbose, cores=cores)
  }
  
  if (doUpdateMaster==T){
    
    if (verbose>0){cat('Updating Master Catalogues....', '\n')}
    write('Updating Master Catalogues....', file=logName, append=T)
    
    previousMASTERS<-list.files(path='data/catalogues/MASTERcats/',pattern='*.rda')
    
    MASTERDates<-date2jd(year=as.numeric(substr(previousMASTERS,14,15) ), mon=as.numeric(substr(previousMASTERS,11,12)), mday=as.numeric(substr(previousMASTERS,6,9)), hour=12)
    lastMASTER<-paste('data/catalogues/MASTERcats/',previousMASTERS[which(MASTERDates==max(MASTERDates))],sep='')
    
    if (verbose>0){cat('    - Using previous MASTER catalogue as:',lastMASTER, '\n')}
    write(paste('    - Using previous MASTER catalogue as:',lastMASTER,sep=''), file=logName, append=T)
    
    newMaster<-UpdateMASTERCat(cat=lastMASTER, specDir='data/reduced/stackedSpec/', logName=logName, verbose=verbose)
    
    
    if (verbose>0){cat('Finding next observing night....', '\n')}
    write('Finding next observing night....', file=logName, append=T)
    
    runs<-list.files(path='data/observing/',pattern='run*')
    nights<-c()
    runName<-c()
    for (i in 1:length(runs)){
      nights<-c(nights, list.files(path=paste('data/observing/',runs[i],sep=''),pattern='*'))
      runName<-c(runName,runs[i])
    }
    nightsYears<-c(1:length(nights))
    nightsMonths<-c(1:length(nights))
    nightsDays<-c(1:length(nights))
    
    for (i in 1:length(nights)){
      nightsYears[i]<-as.numeric(strsplit(nights[i],'_')[[1]][1])
      nightsMonths[i]<-as.numeric(strsplit(nights[i],'_')[[1]][2])
      nightsDays[i]<-as.numeric(strsplit(nights[i],'_')[[1]][3])
    }
    
    JD<-date2jd(year=nightsYears,mon=nightsMonths, mday=nightsDays, hour=0)
    JDMaster<-date2jd(year=as.numeric(strsplit(as.character(Sys.Date()),'-')[[1]][1]),mon=as.numeric(strsplit(as.character(Sys.Date()),'-')[[1]][2]), mday=as.numeric(strsplit(as.character(Sys.Date()),'-')[[1]][3]), hour=0)
    
    JD<-JD[which(JD-JDMaster>=0)]
    runName<-runName[which(JD-JDMaster>=0)]
    dateFor_a<-jd2date(JD[which(JD-JDMaster==min(JD-JDMaster))])
    runFor<-runName[which(JD-JDMaster==min(JD-JDMaster))]
    dateFor<-paste(runFor,'/',dateFor_a$year,'_', dateFor_a$mon,'_',dateFor_a$mday, sep='')
    
    if (dateFor_a$mon>=2 & dateFor_a$mon<8) {semester='A'}
    if ((dateFor_a$mon>=8 & dateFor_a$mon<=12) |dateFor_a$mon==1) {semester='B'}
    
    run<-as.numeric(substr(runFor, 4,4))
    
    if (verbose>0){cat('Making DO cats....', '\n')}
    write('Making DO cats....', file=logName, append=T)
    
    DODir<-makeDOCats(MASTERCat=newMaster, UserName=user, dateFor=dateFor, year=dateFor_a$year, semester=semester, run=run, logName=logName, verbose=verbose)
    
  }
  
  if (doUpdateMaster==F & doTiler==T){
    
    if (verbose>0){cat('*** doUpdateMaster=F so no updating undertaken.', '\n')}
    write('*** doUpdateMaster=F so no updating undertaken.', file=logName, append=T)
    
    DODir<-DODir
  }
  
  if (addOzDES==T){
    
    addOzDES(OzDESCat=OzDESCat, DODir=DODir, num=10, logName=logName, verbose=verbose)
    
  }
  
  
  if (doTiler==T){
    
    if (verbose>0){cat('Running Tiler....', '\n')}
    write('Running Tiler....', file=logName, append=T)
    
    DOcat<-paste(DODir, list.files(path=DODir, pattern='DObj*'),sep='')
    DATAguide<-paste(DODir,list.files(path=DODir, pattern='DGui*'),sep='')
    DATAstspec<-paste(DODir,list.files(path=DODir, pattern='DStd*'),sep='')
    DATAsky<-paste(DODir, list.files(path=DODir, pattern='DSky*'),sep='')
    
    
    if (verbose>1){cat(paste('    - Tiler run with command: runTiler(workigDir=',DODir,'Tiling, DOcat=',DOcat,',DATAguide=',DATAguide,', DATAstspec=',DATAstspec,', DATAsky=',DATAsky,', N_D02A=',N_D02A,', N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02A_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,')',sep=''),'\n')}
    write(paste('    - Tiler run with command: runTiler(workigDir=',DODir,'Tiling, DOcat=',DOcat,',DATAguide=',DATAguide,', DATAstspec=',DATAstspec,', DATAsky=',DATAsky,', N_D02A=',N_D02A,', N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02A_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,')',sep=''),file=logName, append=T)
    
    
    runTiler(workigDir=paste(DODir, 'Tiling', sep=''), DOcat=DOcat, DATAguide=DATAguide, DATAstspec=DATAstspec, DATAsky=DATAsky, N_D02A=N_D02A, N_D02B=N_D02B, N_D03=N_D03, N_D10=N_D10, D02A_startPlate=D02A_startPlate, D02B_startPlate=D02A_startPlate, D03_startPlate=D03_startPlate, D10_startPlate=D10_startPlate, logName=logName, verbose=verbose, cores=cores, configdir=configdir)
  }
  
  if (verbose==0){cat('** You have reached the end **' , '\n')}
  if (verbose>0){
    
    cat('TAZ sucessfully finished running.', '\n')
    cat('\n')
    cat('****************************************************', '\n')
    cat('********* Ending TAZ version',version,'*************', '\n')
    cat('****************************************************', '\n')
    cat('\n')
    
  } 
  
}

