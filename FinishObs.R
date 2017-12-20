FinishObs<-function() {

    basePath<-'.'

    load(paste(basePath,'/data/logs/syncLog.Rdata', sep=''))

                                        #syncLogNew<-list(syncDate=date(),numFiles=0,numNew=0,fileList=NA,newFileList=NA )
                                        #syncLog<-list(syncLogNew)
                                        #save(syncLog,file=paste(basePath,'/data/logs/syncLog.Rdata',sep=''))


    runs<-list.files(path=paste(basePath,'/data/rawdata',sep=''), pattern='*')
    rawFiles<-c()
    for (i in 1:length(runs)) {
        nights<-list.files(path=paste(basePath,'/data/rawdata/',runs[i],sep=''), pattern='*')
        for (j in 1:length(nights)) {
            rawFiles<-c(rawFiles, list.files(path=paste(basePath,'/data/rawdata/',runs[i],'/',nights[j],sep=''), pattern='*.fits'))
        }
    }

    syncNumPrev<-size(syncLog)[2]

    oldFileList<-syncLog[[syncNumPrev]]$fileList
    newFileList=rawFiles[which(rawFiles %in% oldFileList==F)]


    syncDate=date()
    numFiles=length(rawFiles)
    numNew=length(newFileList)
    fileList=rawFiles

    syncLogNew<-list(syncDate=date(),numFiles=numFiles,numNew=numNew,fileList=rawFiles,newFileList=newFileList )
    syncLog[[syncNumPrev+1]]<-syncLogNew

    save(syncLog,file=paste(basePath,'/data/logs/syncLog.Rdata',sep=''))


}
