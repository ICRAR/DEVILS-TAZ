TAZ<-function(user='ldavies', workingDir='/Users/luke/work/DEVILS/TAZ/', verbose=2, N_D02A=1,N_D02B=1, N_D03=1, N_D10=1, D02A_startPlate=0, D02B_startPlate=0, D03_startPlate=0, D10_startPlate=0, doCalibQC=F, doReduce=T, doExtract=T, toExtractFiles='NA', doStack=T, toStackIDs='NA', doAutoZ=T, toAutoZStacks='NA', doUpdateMaster=T, doTiler=T, DODir='NA',zeroPoint=T, cores=cores){

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
        system(paste('tar -xvf ', LibPaths, '/DEVILSTAZ/data/calibrators.tar --directory ', LibPaths, '/DEVILSTAZ/data/',sep='')) 
        system(paste('tar -xvf ', LibPaths, '/DEVILSTAZ/data/idxFiles.tar --directory ', LibPaths, '/DEVILSTAZ/data/' ,sep=''))

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
    write(' ', file=logName, append=T)


    
    if (doReduce==T){

        if (verbose>0){cat('Finding unreduced raw datasets....', '\n')}
        write('Finding unreduced raw datasets....', file=logName, append=T)
    
        toReduce<-findNewData(logName=logName, verbose=verbose)
       
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
    

    if (doTiler==T){

        if (verbose>0){cat('Running Tiler....', '\n')}
        write('Running Tiler....', file=logName, append=T)
        
        DOcat<-paste(DODir, list.files(path=DODir, pattern='DObj*'),sep='')
        DATAguide<-paste(DODir,list.files(path=DODir, pattern='DGui*'),sep='')
        DATAstspec<-paste(DODir,list.files(path=DODir, pattern='DStd*'),sep='')
        DATAsky<-paste(DODir, list.files(path=DODir, pattern='DSky*'),sep='')

        
        if (verbose>1){cat(paste('    - Tiler run with command: runTiler(workigDir=',DODir,'Tiling, DOcat=',DOcat,',DATAguide=',DATAguide,', DATAstspec=',DATAstspec,', DATAsky=',DATAsky,', N_D02A=',N_D02A,', N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02A_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,')',sep=''),'\n')}
        write(paste('    - Tiler run with command: runTiler(workigDir=',DODir,'Tiling, DOcat=',DOcat,',DATAguide=',DATAguide,', DATAstspec=',DATAstspec,', DATAsky=',DATAsky,', N_D02A=',N_D02A,', N_D02B=',N_D02B,', N_D03=',N_D03,', N_D10=',N_D10,', D02A_startPlate=',D02A_startPlate,', D02B_startPlate=',D02A_startPlate,', D03_startPlate=',D03_startPlate,', D10_startPlate=',D10_startPlate,')',sep=''),file=logName, append=T)
        
        
        runTiler(workigDir=paste(DODir, 'Tiling', sep=''), DOcat=DOcat, DATAguide=DATAguide, DATAstspec=DATAstspec, DATAsky=DATAsky, N_D02A=N_D02A, N_D02B=N_D02B, N_D03=N_D03, N_D10=N_D10, D02A_startPlate=D02A_startPlate, D02B_startPlate=D02A_startPlate, D03_startPlate=D03_startPlate, D10_startPlate=D10_startPlate, logName=logName, verbose=verbose, cores=cores)
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
