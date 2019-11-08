#' Top level funtion for running full DEVILS Tool for Analaysis and Redshifting Pipeline.
#'
#' @description This is the highest level main TAZ function for running all over functions
#' in an easily mangable mode. With the inputs provided in this function and the DEVILS directory 
#' strucutre the full reducution and and analysis of DEVILS data can be undertaken. 
#'  
#' @param user The current use identification. This is added to all logs.
#' @param workingDir The top level directory where the DEVILS data/... directory is found

#' @param dobizCheck TRUE/FALSE, Check files are in correct part of file structure using bizCheck.R
#' @param bizStopError TRUE/FLASE if a file is found in the wrong place, should the code be stopped?
#' @param doCalibQC TRUE/FALSE, Produce bias and dark frame QC plots using checkQC.R
#' @param doReduce TRUE/FALSE, Perform 2dFDR reduction using run2dfDR.R 
#' @param toReduceDirs directory path to reduce in the DEVILS data structure. Must be of the form, 
#' data/rawdata/run#_YYYY_MM/YYYY_MM_DD (i.e. data/rawdata/run1_2017_12/2017_12_18). *** NOTE: IF YOU
#' WITH TAZ TO IDENTIFY ALL UNDREDUCED DATES SET toReduceDirs='NA'** Also, this will overwrite any data in the 
#' corresponding reduced directory for that night, so use with care!  
#' @param doDark TRUE/FALSE, do dark frame subtraction in 2dFDR reduction
#' @param zeroPoint TRUE/FALSE, Do zeroPoint flux scaling
#' @param doExtract TRUE/FALSE, Perform 1D spectral extraction using extractNewSpec.R 
#' @param toExtractFiles If doReduce=F and doExtract=T, provide a string vector of the reduced files to 
#' extract. These must have the full path (i.e. data/reduced/run1_2017_12/2017_12_18/2017_12_18_config_1_reduced.fits). 
#' This can be set to 'all' to extract from all reduced configurations in the 'data/reduced/' directory  
#' @param doStack TRUE/FALSE, Perform stacking of spectra with IDs using stackSpec.R 
#' @param toStackIDs If doExtract=F and doStack=T, provide a string vector of IDs you wish to stack. Can be a list of IDs 
#' or set to 'all' which will stack all unique IDs in the 'data/reduced/allSpec/' directory.
#' @param doAutoZ TRUE/FALSE, Perform AutoZ redshift fitting using runAutoZ.R 
#' @param highZ TRUE/FALSE, run auto-z in high redshfit (z>0.9) mode.
#' @param toAutoZStacks  If doStack=F and doAutoZ=T, provide vector list of file paths to run AutoZ over. 
#' Must either be full directory path, or can set to 'all' to run over all spectra in the 'data/reduced/stackedSpec/' directory.  
#' @param doUpdateMaster TRUE/FALSE, Update and generate a new DMCat (with timestamp) using the redshift measurement in the 
#' data/reduced/stackedSpec/ directory. Then make DOCats for the next observing date.
#' @param OzDESGRC Either NA (do not update) or path to new OzDES GRC cat to add to DMCat
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
#' @param docheckConfig TRUE/FALSE, check final configuration files for obvious errors with checkConfig.R
#' @param docutoutConfig TRUE/FALSE, make cutout images of configured objects (currently only works on munro and Luke's laptop)
#' @param cores number of cores to use in runAutoZ.R, and runTiler.R
#' @param reducCores number of cores to use in run2dfDR.R (different from above as can be problem for 2dfDR if large)
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param makeNormalTiles Make general configurations using all catalogues 
#' @param makeBackUpTiles Also make bad weather configuration files for just bright sources. 
#' @param BrightCut Magnitude to cut at for bright sources. Only takes affect it makeBackUpTiles==TRUE.
#' @param FaintCut Magnitude to cut at for fiant sources. Tiling will ignore all sources below this cut
#' @param email If you want TAZ to send you email updates enter your email here.
#' @param emailPassword If you want TAZ to send you email updates you must enter the TAZ email password here.
#' @param doCosmic TRUE/FALSE do aditional cosmic ray rejection in extract.spec()
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
TAZ<-function(user='ldavies', workingDir='/Users/luke/work/DEVILS/TAZ/',  dobizCheck=T, bizStopError=F, doCalibQC=F, doReduce=T, toReduceDirs='NA',doDark=T, zeroPoint=T, doExtract=T, toExtractFiles='NA', doStack=T, toStackIDs='NA', doAutoZ=T, toAutoZStacks='NA', doUpdateMaster=T, OzDESGRC=NA, doTiler=T, DODir='NA',N_D02A=1,N_D02B=1, N_D03=1, N_D10=1, D02A_startPlate=0, D02B_startPlate=0, D03_startPlate=0, D10_startPlate=0,configdir='/Applications/configure-8.4-MacOsX_ElCapitan_x86_64',  addOzDES=FALSE, OzDESCat='NA',docheckConfig=T, docutoutConfig=F, cores=10, reducCores=10, verbose=2, makeNormalTiles=TRUE, makeBackUpTiles=FALSE, BrightCut=22, FaintCut=30, email=NA, emailPassword=NA, doCosmic=F, highZ=T, useVIS=F, bumpPos=NA){
  
  stopState<-NA
  
  system('cleanup')
  
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
  write(paste('#Running TAZ with line: TAZ<-function(user=',user,', workingDir=',workingDir,',  dobizCheck=',dobizCheck,', bizStopError=',bizStopError,', doCalibQC=',doCalibQC,', doReduce=',doReduce,', toReduceDirs=',toReduceDirs,', zeroPoint=',zeroPoint,', doExtract=',doExtract,', toExtractFiles=',toExtractFiles,', doStack=',doStack,', toStackIDs=',toStackIDs,', doAutoZ=',doAutoZ,', toAutoZStacks=',toAutoZStacks,', doUpdateMaster=',doUpdateMaster,', doTiler=',doTiler,', DODir=',DODir,',N_D02A=',N_D02A,',N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02B_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,',configdir=',configdir,',  cores=',cores,', verbose=',verbose,')',sep=''), file=logName, append=T)
    
  write(' ', file=logName, append=T)
  
  stopState<-'Started'
  
  if (is.na(email[1])==F){
    if (is.na(emailPassword)==F){
      
      for (jj in 1:length(email)){
        subject<-'Update From TAZ - TAZ Started Running'
        bodyText<-paste('TAZ starting running at ',date(), '. \n \n Running TAZ with line: TAZ<-function(user=',user,', workingDir=',workingDir,',  dobizCheck=',dobizCheck,', bizStopError=',bizStopError,', doCalibQC=',doCalibQC,', doReduce=',doReduce,', toReduceDirs=',toReduceDirs,', zeroPoint=',zeroPoint,', doExtract=',doExtract,', toExtractFiles=',toExtractFiles,', doStack=',doStack,', toStackIDs=',toStackIDs,', doAutoZ=',doAutoZ,', toAutoZStacks=',toAutoZStacks,', doUpdateMaster=',doUpdateMaster,', doTiler=',doTiler,', DODir=',DODir,',N_D02A=',N_D02A,',N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02B_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,',configdir=',configdir,',  cores=',cores,', verbose=',verbose,') \n\n Trace:',paste(stopState, collapse='.....'),'\n \n LogName=',logName,sep='')
        
        TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
      }
    
      
    }else{
      
      cat('*** WARNING You must provied emial password if "email" is set - no email updates sending  ***',  '\n')
      write(paste('*** WARNING You must provied emial password if "email" is set - no email updates sending  ***',sep=''), file=logName, append=T)
      email<-NA
      
    }
  }
  
  
  CheckSync(dir='data/rawdata', pauseTime=1, verbose=2, logName=logName) # checking that raw data has finshed synching
  
  
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
      
      newReduce<-run2dfDR(toReduce=toReduce, doCalibQC=doCalibQC, logName=logName, verbose=verbose, reducCores=reducCores, doDark=doDark, doCosmic=doCosmic)
      
      stopState<-c(stopState, 'Reduced')
      
      ## newReduce not passing from run2dfDR() correctly - find manually....
      newReduce<-c()
      for (j in 1:length(toReduce)){
        tmp<-list.files(path=paste(strsplit(toReduce[j],'/')[[1]][1],'/reduced/', strsplit(toReduce[j],'/')[[1]][3], '/', strsplit(toReduce[j],'/')[[1]][4],'/',sep=''), pattern='*reduced.fits')
        newReduce<-c(newReduce, paste(strsplit(toReduce[j],'/')[[1]][1],'/reduced/', strsplit(toReduce[j],'/')[[1]][3], '/', strsplit(toReduce[j],'/')[[1]][4],'/',tmp,sep=''))
      }
      
      stdStars<-read.csv('data/calibrators/stdstars/stdStarCat.csv')
      
      if (verbose>0){cat('Flux calibrating new datasets....', '\n')}
      write('Flux calibrating new datasets....', file=logName, append=T)
      
      for (i in 1:length(newReduce)){
        
        if (verbose>0){cat('  -Flux calibrating: ', newReduce[i], '\n')}
        write(paste('  -Flux calibrating: ', newReduce[i],sep=''), file=logName, append=T)
        
        fluxCal(file=newReduce[i], stdStars=stdStars,logName=logName, verbose=verbose)    
      }
      
      if (is.na(email[1])==F){
        if (is.na(emailPassword)==F){
          
          for (jj in 1:length(email)){
            subject<-'Update From TAZ - Data Reduction Completed'
            bodyText<-paste('run2dfDR() finished at ',date(), '\n \n ',length(newReduce), ' configuration(s) were reduced: \n', paste(newReduce, collapse='\n'), '\n\n Trace: ', paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
            
            TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
          }
        }
      }
      
      
      
    }else{
      
      cat('*** WARNING NO NEW NIGHT TO REDUCE ***',  '\n')
      cat('Exiting TAZ, please set doReduce=F and provide a vector to toExtractFiles', '\n')
      write(paste('**** WARNING NO NEW NIGHT TO REDUCE ***',sep=''), file=logName, append=T)

      doReduce<-F
      doExtract<-F
      doStack<-F
      doAutoZ<-F
      
      #write(paste('Exiting TAZ, please set doReduce=F and provide a vector to toExtractFiles',sep=''), file=logName, append=T)
      #return(NULL)
      
      if (is.na(email[1])==F){
        if (is.na(emailPassword)==F){
          
          for (jj in 1:length(email)){
            subject<-'Update From TAZ - No New Files To Reduce'
            bodyText<-paste('TAZ tried to run at ',date(), '\n \n There were no new data to reduce.', '\n\n Trace: ', paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
            
            TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
          }
        }
      }
      
    }
    
    
   
    
    
  }
 
  
  
  if (doReduce==F){
    
    if (verbose>0){cat('*** doReduce=F so no reduction undertaken.', '\n')}
    write('*** doReduce=F so no reduction undertaken.', file=logName, append=T)
    
    if (doExtract==T & toExtractFiles!='all'){newReduce<-toExtractFiles}
    if (doExtract==T & toExtractFiles=='all'){
      
      if (verbose>0){cat('toExtractFiles=="all" - will extract 1D spectra from all configurations....', '\n')}
      write('toExtractFiles=="all" - will extract 1D spectra from all configurations....', file=logName, append=T)
      
      listRuns<-paste('data/reduced/', list.files(path='data/reduced', pattern='run*'), sep='')
      
      listDates<-c()
      for (i in 1:length(listRuns)){
        listDates<-c(listDates, paste(listRuns[i],'/',list.files(path=listRuns[i], pattern='*'),sep=''))
      }
      
      newReduce<-c()
      for (i in 1:length(listDates)){
        newReduce<-c(newReduce, paste(listDates[i],'/',list.files(path=listDates[i], pattern='*_reduced.fits'),sep=''))
      }
      
 
      }
  }
  
  if (doExtract==T){
    
    if (verbose>0){cat('Extracting 1D spectra....', '\n')}
    write('Extracting 1D spectra....', file=logName, append=T)
    
    newSpec<-c()
    newIds<-c()
    
    
    write.csv('ID', file=paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newIDs.csv', sep=''), row.names=F, quote=F)
    
    ExtractedDates<-c()
    a = foreach(i=1:length(newReduce)) %dopar%  {
    #for (i in 1:length(newReduce)){
      
      if (verbose>0){cat('  -Extracting 1D spectra from: ', newReduce[i], '\n')}
      write(paste('  -Extracting 1D spectra from: ', newReduce[i],sep=''), file=logName, append=T)
      tmpnewSpec<-extractNewSpec(file=newReduce[i], logName=logName, verbose=verbose, makePlot=F, zeroPoint=zeroPoint,NowDate=NowDate)
      
      
    }
    
    stopState<-c(stopState, 'Extracted')
    
    ExtractedDates<-c()
    for (i in 1:length(newReduce)){
      ExtractedDates<-c(ExtractedDates, strsplit(newReduce[i], '/')[[1]][4])
    }
    ExtractedDates<-unique(ExtractedDates)
    
   
    


      #newIds<-as.matrix(read.csv(paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newIDs.csv', sep=''),header=T))
      #newIds<-newIds[2:length(newIds)]
    
    # horrible hack to sort out issues:
      #tmpD<-newIds[which(nchar(newIds)>12)]
      #tmpD2<-unlist(strsplit(tmpD,'D'))
      #tmpD3<-tmpD2[which(tmpD2!="")]
      #tmpD3<-paste('D',tmpD3,sep='')
      #tmpS<-tmpD3[which(nchar(tmpD3)>12)]
      #tmpD3<-tmpD3[which(nchar(tmpD3)<=12)]
    
      #tmpS2<-unlist(strsplit(tmpS,'S'))
      #tmpS2[which(substr(tmpS2,1,1)!='D')]<-paste('S', tmpS2[which(substr(tmpS2,1,1)!='D')],sep='')
    
      #newIds<-unique(c(newIds[which(nchar(newIds)<=12)], tmpD3, tmpS2))
    
    # better way to sort out issues:
    allSpecList<-list.files(path='data/reduced/allSpec/', pattern='*')
    newIds_tmp<-c()
    for (j in 1:length(ExtractedDates)){
      newIds_tmp<-c(newIds_tmp, allSpecList[which(substr(allSpecList, 1,nchar(ExtractedDates[j]))==ExtractedDates[j])])
    }

    newIds<-c()
    for (j in 1:length(newIds_tmp)){
      tmp<-strsplit(newIds_tmp[j],'_')[[1]][4]
      newIds<-c(newIds, strsplit(tmp[[1]],'.Rdata')[[1]])    
    }
    newIds<-newIds[which(is.na(newIds)==F)]
    newIds<-unique(newIds)
    #######
    
    if (is.na(email[1])==F){
      if (is.na(emailPassword)==F){
        
        for (jj in 1:length(email)){
          subject<-'Update From TAZ - 1D spectra Extracted'
          bodyText<-paste('TAZ finished extractNewSpec() at ',date(), '\n \n Nights extracted:',ExtractedDates, '\n \n # New IDs:', length(newIds), '\n\n Trace: ', paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
          
          TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
        }
      }
    }
    
    
  }
  

    
    
  if (doExtract==F){
    
    if (verbose>0){cat('*** doExtract=F so no extraction undertaken.', '\n')}
    write('*** doExtract=F so no extraction undertaken.', file=logName, append=T)
    
    if (doStack==T){ newIds<-toStackIDs}
    
  }
  
  
  
  
  if (doStack==T){
    
    
    if (verbose>0){cat('Stacking 1D spectra....', '\n')}
    write('Stacking 1D spectra....', file=logName, append=T)      
    
    if (toStackIDs=='all'){newIds<-'all'}
    
    newStacks<-stackSpec(ids=newIds,logName=logName, verbose=verbose, makePlot=F, cores=cores)
    write.csv(newStacks, file=paste('data/reduced/newSpec/', substr(NowDate, 1,10),'_newStacks.csv', sep=''), row.names=F, quote=F)
    
    stopState<-c(stopState, 'Stacked')
    
    if (is.na(email[1])==F){
      if (is.na(emailPassword)==F){
        
        for (jj in 1:length(email)){
        
          subject<-'Update From TAZ - stackSpec Completed'
          bodyText<-paste('stackSpec() finished at ',date(), '\n\n # New Stacks:',length(newStacks), '\n\n Trace: ',paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
        
          TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
        }
        
      }
    }
    
  }
 
  if (doStack==F & doAutoZ==T){
    
    if (verbose>0){cat('*** doStack=F so no stacking undertaken.', '\n')}
    write('*** doStack=F so no stacking undertaken.', file=logName, append=T)
    
    newStacks<-toAutoZStacks
    
  }
  
  
  if (doAutoZ==T){
    if (verbose>0){cat('Running AutoZ for new spectra....', '\n')}
    write('Running AutoZ for new spectra....', file=logName, append=T)
    
  
    if (toAutoZStacks!='all'){newStacks<-paste('data/reduced/stackedSpec/',newIds,'.Rdata',sep='')}
    if (toAutoZStacks=='all'){newStacks<-'all'}
    
    
    runAutoZ(specs=newStacks, logName=logName, verbose=verbose, cores=cores, highZ=highZ,makePlots=F)
    
    stopState<-c(stopState, 'AutoZ')
    
    if (is.na(email[1])==F){
      if (is.na(emailPassword)==F){
 
        for (jj in 1:length(email)){
          subject<-'Update From TAZ - AutoZ Finished Running'
          bodyText<-paste('runAutoZ() finished running at ', date(), '\n\n Trace: ',paste(stopState, collapse='.....'), '\n\n LogName: ',logName,  sep='')
          TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
        }
        
      }
    }
    
    
    
  }
  

  
  
  if (doUpdateMaster==T){
    
    if (verbose>0){cat('Updating Master Catalogues....', '\n')}
    write('Updating Master Catalogues....', file=logName, append=T)
    
    previousMASTERS<-list.files(path='data/catalogues/MASTERcats/',pattern='*.rda')
    
    year<-c()
    month<-c()
    day<-c()
    
    for (i in 1:length(previousMASTERS)){
      yeartmp<-strsplit(previousMASTERS[i],'-')[[1]][1]
      year<-c(year,as.numeric(strsplit(yeartmp,'DMCat')[[1]][2]))
      month<-c(month,as.numeric(strsplit(previousMASTERS[i],'-')[[1]][2]))
      daytmp<-strsplit(previousMASTERS[i],'-')[[1]][3]
      day<-c(day,as.numeric(strsplit(daytmp,'.rda')[[1]][1]))
    }
      
    
    MASTERDates<-date2jd(year=year, mon=month, mday=day, hour=12)
    lastMASTER<-paste('data/catalogues/MASTERcats/',previousMASTERS[which(MASTERDates==max(MASTERDates))],sep='')
    
    if (verbose>0){cat('    - Using previous MASTER catalogue as:',lastMASTER, '\n')}
    write(paste('    - Using previous MASTER catalogue as:',lastMASTER,sep=''), file=logName, append=T)
    
    newMaster<-UpdateMASTERCat(cat=lastMASTER, specDir='data/reduced/stackedSpec/', OzDESGRC=OzDESGRC, logName=logName, verbose=verbose, makePlots=T, probGood=0.90, useVIS=useVIS, bumpPos=bumpPos)
    
    
    if (verbose>0){cat('Finding next observing night....', '\n')}
    write('Finding next observing night....', file=logName, append=T)
    
    runs<-list.files(path='data/observing/',pattern='run*')
    nights<-c()
    runName<-c()
    for (i in 1:length(runs)){
      nights<-c(nights, list.files(path=paste('data/observing/',runs[i],sep=''),pattern='*'))
      runName<-c(runName,rep(runs[i],length(list.files(path=paste('data/observing/',runs[i],sep=''),pattern='*'))))
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
    
    JDMaster<-date2jd(year=as.numeric(strsplit(as.character(nowDate),'-')[[1]][1]),mon=as.numeric(strsplit(as.character(nowDate),'-')[[1]][2]), mday=as.numeric(strsplit(as.character(nowDate),'-')[[1]][3]), hour=0)
    
    runName<-runName[which(JD-JDMaster>=0)]
    JD<-JD[which(JD-JDMaster>=0)]
    dateFor_a<-jd2date(JD[which(JD-JDMaster==min(JD-JDMaster))])
    runFor<-runName[which(JD-JDMaster==min(JD-JDMaster))]
    dateFor<-paste(runFor,'/',dateFor_a$year,'_', dateFor_a$mon,'_',dateFor_a$mday, sep='')
    
    if (dateFor_a$mon>=2 & dateFor_a$mon<8) {semester='A'}
    if ((dateFor_a$mon>=8 & dateFor_a$mon<=12) |dateFor_a$mon==1) {semester='B'}
    
    run<-as.numeric(substr(runFor, 4,4))
    
    stopState<-c(stopState, 'Updated')
    
    if (verbose>0){cat('Making DO cats....', '\n')}
    write('Making DO cats....', file=logName, append=T)
  
    
    DODir<-makeDOCats(MASTERCat=newMaster, UserName=user, dateFor=dateFor, year=dateFor_a$year, semester=semester, run=run, logName=logName, verbose=verbose)
    
    stopState<-c(stopState, 'DOCats')
    
    if (is.na(email[1])==F){
      if (is.na(emailPassword)==F){
        
        for (jj in 1:length(email)){
        
          subject<-'Update From TAZ - Catalogues Updated'
          bodyText<-paste('All Catlogues updated at ', date(), '\n\n New Master Cat:',newMaster, '\n Writing to DODir:',DODir, '\n\n Trace: ', paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
          TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
        }
        
      }
    }
    
  
 
  }
  
  if (doUpdateMaster==F & doTiler==T){
    
    if (verbose>0){cat('*** doUpdateMaster=F so no updating undertaken.', '\n')}
    write('*** doUpdateMaster=F so no updating undertaken.', file=logName, append=T)
    
    DODir<-DODir
  }
  
  if (addOzDES==T){
    
    if (verbose>0){cat('Adding OzDES targets....', '\n')}
    write('Adding OzDES targets....', file=logName, append=T)
    
    addOzDES(OzDESCat=OzDESCat, DODir=DODir, num=30,logName=logName, verbose=verbose)
    
    stopState<-c(stopState, 'OzDES')
    
    if (is.na(email[1])==F){
      if (is.na(emailPassword)==F){
        
        for (jj in 1:length(email)){
          
          subject<-'Update From TAZ - OzDES Objects Added'
          bodyText<-paste('OzDES objects added at ', date(), '\n\n OzDES Cat used:',OzDESCat, '\n\n Trace: ', paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
          TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
        }
        
      }
    }
    
    
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
  
    
    ConfigNames<-runTiler(workingDir=paste(DODir, 'Tiling', sep=''), DOcat=DOcat, DATAguide=DATAguide, DATAstspec=DATAstspec, DATAsky=DATAsky, N_D02A=N_D02A, N_D02B=N_D02B, N_D03=N_D03, N_D10=N_D10, D02A_startPlate=D02A_startPlate, D02B_startPlate=D02A_startPlate, D03_startPlate=D03_startPlate, D10_startPlate=D10_startPlate, logName=logName, verbose=verbose, cores=cores, configdir=configdir, makeNormal=makeNormalTiles, makeBackUp=makeBackUpTiles, BrightCut=BrightCut, FaintCut=FaintCut)
    
    stopState<-c(stopState, 'Tiler')
    
    if (is.na(email[1])==F){
      if (is.na(emailPassword)==F){
        
        for (jj in 1:length(email)){
        
          subject<-'Update From TAZ - Tiling Finished'
          bodyText<-paste('runTiler()  generated ',length(ConfigNames), ' at ', date(), '\n\n Trace: ', paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
          TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
        }
        
      }
    }
    
      if (docutoutConfig==T){
      
        host<-system('hostname',intern = TRUE)
        if (host=="EMS-37993.local"){location<-'lukeLap'}
        if (host=="munro"){location<-'munro'}
        if (host!="munro" & host!="EMS-37993.local"){
          if (verbose>0){cat('*** WARNING - cannot run cutoutConfig.R while not on munro or lukes laptop. Skipping....', '\n')}
          write('*** WARNING - cannot run cutoutConfig.R while not on munro or lukes laptop. Skipping....', file=logName, append=T)
        }
        if (host=="munro" | host=="EMS-37993.local"){
          for (i in 1:length(ConfigNames)){
              cutoutConfig(configFile=ConfigNames[i], size=15, outDir=paste(strsplit(ConfigNames[i],'.lis')[[1]][1],'_cutouts',sep=''), cores=cores, location=location)
          }
        }
        
        stopState<-c(stopState, 'CutoutConfig')
        
      }
    

      
  }
  
  
  
    if (docheckConfig==T){
      
      if (verbose>0){cat('Running checkConfig....', '\n')}
      write('Running checkConfig....', file=logName, append=T)
      
      previousMASTERS<-list.files(path='data/catalogues/MASTERcats/',pattern='*.rda')
      
      year<-c()
      month<-c()
      day<-c()
      
      for (i in 1:length(previousMASTERS)){
        yeartmp<-strsplit(previousMASTERS[i],'-')[[1]][1]
        year<-c(year,as.numeric(strsplit(yeartmp,'DMCat')[[1]][2]))
        month<-c(month,as.numeric(strsplit(previousMASTERS[i],'-')[[1]][2]))
        daytmp<-strsplit(previousMASTERS[i],'-')[[1]][3]
        day<-c(day,as.numeric(strsplit(daytmp,'.rda')[[1]][1]))
      }
      
      
      MASTERDates<-date2jd(year=year, mon=month, mday=day, hour=12)
      lastMASTER<-paste('data/catalogues/MASTERcats/',previousMASTERS[which(MASTERDates==max(MASTERDates))],sep='')
      
      
      checkConfig(configFiles = ConfigNames, DMCatN = lastMASTER, logName=logName, verbose=verbose)
      
      stopState<-c(stopState, 'checkConfig')
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
  
  stopState<-c(stopState, 'Finished')
  
  if (is.na(email[1])==F){
    if (is.na(emailPassword)==F){
      
      for (jj in 1:length(email)){
      
        subject<-'Update From TAZ - TAZ Finished'
        bodyText<-paste('TAZ finished running at ', date(), '\n\n Trace: ',paste(stopState, collapse='.....'), '\n\n LogName: ',logName, sep='')
        TAZemail(user=user, recipient=email[jj], password=emailPassword, subject=subject, bodyText=bodyText)
      }
      
    }
  }
  
  return('Finished')
  
}

