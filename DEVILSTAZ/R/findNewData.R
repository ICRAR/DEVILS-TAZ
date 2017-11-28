findNewData<-function(logName=logName, verbose=verbose){


    if (verbose>0){cat('Running findNewData.....', '\n')}
    
    
    listRuns<-list.files(path='data/rawdata', pattern='*')

    listDates<-c()
    for (i in 1:length(listRuns)){
        listDates<-c(listDates, paste(listRuns[i],'/',list.files(path=paste('data/rawdata/',listRuns[i],sep=''), pattern='*'),sep=''))
    }
    
    toReduce<-c()
    
    for (i in 1:length(listDates)){
        if (length(list.files(path=paste('data/rawdata/',listDates[i],sep=''), pattern='*.fits'))>0 & length(list.files(path=paste('data/reduced/',listDates[i],sep=''), pattern='*'))==0) {            
           toReduce<-c(toReduce, paste('data/rawdata/',listDates[i],sep=''))           
        }

    }

    if (length(toReduce)>0){

        if (verbose>1){
            cat(paste('     - ',length(toReduce), ' new dates to reduce', sep=''), '\n')            
        }

        write('Running findNewData.....', file=logName, append=T)
        write(paste('     - ',length(toReduce), ' new date to reduce', sep=''), file=logName, append=T)
        for (i in 1:length(toReduce)){
            if (verbose>1){cat(paste('            - ',toReduce[i],sep=''), '\n')}
            write(paste('            - ',toReduce[i],sep=''), file=logName, append=T)
        }
    }else{

        cat('\n')
        cat('***** No New Data to Reduce!', '\n')
        cat('\n')
        }

    
    
    return(toReduce)

}
