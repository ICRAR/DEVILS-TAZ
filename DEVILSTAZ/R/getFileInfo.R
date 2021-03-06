#' Get meta data information for the 2dF+AAOmega file.
#'
#' @description Function searches through files in a given directory and 
#' returns a data table contining relavent meta data. Also saves meta data
#' to a given file. 
#' 
#' @param dir directory contianing files for which meta data should be retreived
#' @param saveFile filename to save meta data to
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @return meta dataframe for files in directoyr, dir.  
#' @examples 
#' metaData<-getFileInfo(dir='data/reduced/run1_2017_12/2017_12_18/', saveFile='2017_12_18_metaData.Rdata', logName='tempLog.txt', verbose=1)
#' @export
getFileInfo<-function(dir=dir, saveFile=saveFile,logName=logName, verbose=verbose){

    fileNames<-list.files(path=dir, pattern='*.fits')
    ccd<-c(1:length(fileNames))
    field<-c(1:length(fileNames))
    RA<-c(1:length(fileNames))
    DEC<-c(1:length(fileNames))
    config<-c(1:length(fileNames))
    type<-c(1:length(fileNames))
    date<-c(1:length(fileNames))
    grating<-c(1:length(fileNames))
    tExp<-c(1:length(fileNames))

    if (logName!='NA'){write(paste('          - Getting File info for directory: ',dir, sep=''), file=logName, append=T)}
    if (verbose >0) {cat(paste('          - Getting File info for directory: ',dir, sep=''), '\n')}
    
    
    for (i in 1:length(fileNames)){

        if (logName!='NA'){write(paste('            - Reading File: ',fileNames[i], sep=''), file=logName, append=T)}
        if (verbose >1) {cat(paste('            - Reading File: ',fileNames[i], sep=''), '\n')}

        hdr<-read.fitshdr(paste(dir,'/',fileNames[i],sep=''))
        ccd[i]<-read.fitskey('SPECTID',file=paste(dir,'/',fileNames[i],sep=''))
        if (ccd[i]=='BL'){ccd[i]<-'blue'}
        if (ccd[i]=='RD'){ccd[i]<-'red'}

        config[i]<-read.fitskey('CFG_FILE',file=paste(dir,'/',fileNames[i],sep=''))
        field[i]<-strsplit(config[i], '_')[[1]][1]
        type[i]<-read.fitskey('RUNCMD',file=paste(dir,'/',fileNames[i],sep=''))
        if (type[i]=='RUN'){type[i]<-'TARGET'}
        date[i]<-read.fitskey('UTDATE',file=paste(dir,'/',fileNames[i],sep=''))
        grating[i]<-read.fitskey('GRATID',file=paste(dir,'/',fileNames[i],sep=''))

        tExp[i]<-read.fitskey('EXPOSED',file=paste(dir,'/',fileNames[i],sep=''))
        
        RA[i]<-read.fitskey('MEANRA',file=paste(dir,'/',fileNames[i],sep=''))
        DEC[i]<-read.fitskey('MEANDEC',file=paste(dir,'/',fileNames[i],sep=''))
            
    }
    metaData<-data.frame(fileName=fileNames,ccd=ccd, field=field,RA=RA,DEC=DEC, tExp=tExp, type=type, config=config, date=date,grating=grating)
    
    save(metaData, file=saveFile)
    saveFile

    if (logName!='NA'){write(paste('          - Saving meta data as: ', saveFile, sep=''), file=logName, append=T)}
    if (verbose >0) {cat(paste('          - Saving meta data as: ', saveFile, sep=''), '\n')}
   
    
    return(metaData)

}
