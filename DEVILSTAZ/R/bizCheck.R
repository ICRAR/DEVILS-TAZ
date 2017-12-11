#'  Check DEVILS directory sturucture and print warnings for wrong file types
#'
#' This function perfoms the data reduction on a 2dfDR arc file.
#'  You must provide an idx filename and tramline file name (potentially produced by aaorunTLM.R). 
#'  Optionally you can also perform a bias subtraction and dark correction if provided (this is recomended)
#'
#' @param workingDir Path to top level of DEVILS directory structure
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @examples 
#' bizCheck(workingDir='.', logName='tempLog.txt', verbose=1)
#' @export
bizCheck<-function(workingDir='.', stopError=T, logName=logName, verbose=verbose){
  
  if (verbose>0){cat('*** Running bizCheck on DEVILS data structure', '\n')}
  write('*** Running bizCheck on DEVILS data structure', file=logName, append=T)  
  
  origDir<-getwd()
  setwd(workingDir)
  runs<-list.files(path='data/rawdata', pattern='*')
  
  for (i in 1:length(runs)){
    
    if (verbose>1){cat('  - Checking raw files for run:',runs[i], '\n')}
    write(paste('  - Checking raw files for run:',runs[i],sep=''), file=logName, append=T)  
    
    
    biases<-list.files(path=paste('data/biases/',runs[i], sep=''), pattern='*.fits')

    if (verbose>1){cat('     - ',length(biases),' FITS files found in biases directory', '\n')}
    write(paste('     - ',length(biases),' FITS files found in biases directory',sep=''), file=logName, append=T)
    
    if (length(biases)>0) {
    
    for (j in 1:length(biases)){
     name<- paste('data/biases/',runs[i],'/', biases[j],sep='')
     hdr<-read.fitshdr(name)
     date<-get.fitskey('UTDATE', hdr)
     year<-as.numeric(strsplit(date,':')[[1]][1])
     month<-as.numeric(strsplit(date,':')[[1]][2])
     day<-as.numeric(strsplit(date,':')[[1]][3])
     folderYear<-as.numeric(strsplit(runs[i],'_')[[1]][2])
     folderMonth<-as.numeric(strsplit(runs[i],'_')[[1]][3])
     
     typeF<-get.fitskey('OBJECT', hdr)
     
     if (verbose>1){cat('        - Bias File:',biases[j],  ', date taken: ',date,', Type:',typeF, '\n')}
     write(paste('        - Bias File:',biases[j],  ', date taken: ',date,', Type:',typeF,sep=''), file=logName, append=T)
     
     if (typeF!='Bias Frame'){
       
       if (verbose>1){cat('*** WARNING FRAME ',name, 'DOES NOT APPEAR TO BE A BIAS ***',  '\n')}
       if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
       write(paste('*** WARNING FRAME ',name, 'DOES NOT APPEAR TO BE A BIAS ***',sep=''), file=logName, append=T)
       if (stopError==T) {
         write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
         return(NULL)
       }
     }
     
     if (folderYear!=year){
       if (month!=1 | folderMonth!=12){
            if (verbose>1){cat('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME YEAR AS ITS DIRECTORY ***',  '\n')}
            if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
            write(paste('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME YEAR AS ITS DIRECTORY ***',sep=''), file=logName, append=T)
            
            if (stopError==T) {
              write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
              return(NULL)
              }
         
       }
     }
     
     if (folderMonth!=month & folderMonth!=(month-1)){
       if (verbose>1){cat('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME OR FOLLOWING MONTH AS ITS DIRECTORY ***',  '\n')}
       if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
       write(paste('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME OR PRECEEDING MONTH AS ITS DIRECTORY ***',sep=''), file=logName, append=T)
       if (stopError==T) {
         write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
         return(NULL)
       }
     }
     
     
     
    }
    }
      
    darks<-list.files(path=paste('data/darks/',runs[i], sep=''), pattern='*.fits')
    
    
    if (length(darks)>0) {
      
    
    for (j in 1:length(darks)){
      name<- paste('data/darks/',runs[i],'/', darks[j],sep='')
      hdr<-read.fitshdr(name)
      date<-get.fitskey('UTDATE', hdr)
      year<-as.numeric(strsplit(date,':')[[1]][1])
      month<-as.numeric(strsplit(date,':')[[1]][2])
      day<-as.numeric(strsplit(date,':')[[1]][3])
      typeF<-get.fitskey('OBJECT', hdr)
      folderYear<-as.numeric(strsplit(runs[i],'_')[[1]][2])
      folderMonth<-as.numeric(strsplit(runs[i],'_')[[1]][3])
      
      if (verbose>1){cat('        - Dark File:',biases[j],  ', date taken: ',date,', Type:',typeF, '\n')}
      write(paste('        - Dark File:',biases[j],  ', date taken: ',date,', Type:',typeF,sep=''), file=logName, append=T)
      
      if (typeF!='Dark Frame'){
        if (verbose>1){cat('*** WARNING FRAME ',name, 'DOES NOT APPEAR TO BE A DARK ***',  '\n')}
        if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
        write(paste('*** WARNING FRAME ',name, 'DOES NOT APPEAR TO BE A DARK ***',sep=''), file=logName, append=T)
        if (stopError==T) {
          write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
          return(NULL)
        }
      }
      
      if (folderYear!=year){
        if (month!=1 | folderMonth!=12){
          if (verbose>1){cat('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME YEAR AS ITS DIRECTORY ***',  '\n')}
          if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
          write(paste('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME YEAR AS ITS DIRECTORY ***',sep=''), file=logName, append=T)
          
          if (stopError==T) {
            write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
            return(NULL)
          }
          
        }
      }
      
      if (folderMonth!=month & folderMonth!=(month-1)){
        if (verbose>1){cat('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME OR FOLLOWING MONTH AS ITS DIRECTORY ***',  '\n')}
        if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
        write(paste('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME OR PRECEEDING MONTH AS ITS DIRECTORY ***',sep=''), file=logName, append=T)
        if (stopError==T) {
          write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
          return(NULL)
        }
      }
      
      
    }
    }
      
    
    
    nights<-list.files(path=paste('data/rawdata/',runs[i], sep=''), pattern='*')
    
    if (length(nights)>0) {
      
    
    for (j in 1:length(nights)){
      
      if (verbose>1){cat('     - Checking raw files for night:',nights[j], '\n')}
      write(paste('     - Checking raw files for night:',nights[j],sep=''), file=logName, append=T)  
      
      Targets<-list.files(path=paste('data/rawdata/',runs[i],'/',nights[j], sep=''), pattern='*.fits')
      
      if (length(Targets)>0) {
      
      for (k in 1:length(Targets)){
        
        
        name<- paste('data/rawdata/',runs[i],'/', nights[j],'/',Targets[k], sep='')
        hdr<-read.fitshdr(name)
        date<-get.fitskey('UTDATE', hdr)
        year<-as.numeric(strsplit(date,':')[[1]][1])
        month<-as.numeric(strsplit(date,':')[[1]][2])
        day<-as.numeric(strsplit(date,':')[[1]][3])
        typeF<-get.fitskey('RUNCMD', hdr)
        folderYear<-as.numeric(strsplit(runs[i],'_')[[1]][2])
        folderMonth<-as.numeric(strsplit(runs[i],'_')[[1]][3])
        folderDay<-as.numeric(strsplit(nights[j],'_')[[1]][3])
        
        if (verbose>1){cat('        - Target File:',Targets[k],  ', date taken: ',date,', Type:',typeF, '\n')}
        write(paste('        - Target File:',Targets[k],  ', date taken: ',date,', Type:',typeF,sep=''), file=logName, append=T) 
        

      if (typeF!='FLAT' & typeF!='ARC' & typeF!='RUN'){
          if (verbose>1){cat('*** WARNING FRAME ',name, 'DOES NOT APPEAR TO BE AN ARC, FLAT OR RUN (TARGET) FILE ***',  '\n')}
          if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
          write(paste('*** WARNING FRAME ',name, 'DOES NOT APPEAR TO BE AN ARC, FLAT OR RUN (TARGET) FILE ***',sep=''), file=logName, append=T)
          if (stopError==T) {
            write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
            return(NULL)
          }
      }
        
        dateState<-'BAD'
        if (folderYear==year & folderMonth==month & folderDay==day){dateState<-'GOOD'}
        if (dateState=='BAD'){
          if (verbose>1){cat('*** WARNING FRAME ',name, ' WAS NOT TAKEN ON THE SAME DAY AS ITS DIRECTORY ***',  '\n')}
          if (verbose>1 & stopError==T){cat('Exiting TAZ, please check data locations and re-run', '\n')}
          write(paste('*** WARNING FRAME ',name, ' WAS NOT TAKEN IN THE SAME DAY AS ITS DIRECTORY ***',sep=''), file=logName, append=T)
          if (stopError==T) {
            write(paste('Exiting TAZ, please check data locations and re-run',sep=''), file=logName, append=T)
            return(NULL)
          }
        }
      
      }
      
      
      
    }
    
    
    }
    }
  }
  a<-1
  return(a)
}
  
  

  