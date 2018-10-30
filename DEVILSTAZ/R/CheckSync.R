#' Check if OwnCloud Sync is still running
#'
#' @description Checks to see if a directory is still changing in size. If it is, 
#' it waits for an allocated amount of time and checks again. 
#'
#'  
#' @param dir Path to directory to check 
#' @param pauseTime Time to wait between checks in min 
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @examples 
#' CheckSync()
#' @export
CheckSync<-function(dir='data/rawdata', pauseTime=3, verbose=2, logName='tmp.txt'){
  
  if (verbose>0){cat('** Running CheckSync on', dir, '** \n', '\n')}
  write(paste('** Running CheckSync on', dir, '**', sep=''), file=logName, append=T)
  
  cont<-F
  
  while (cont==F){
    
    check<-system(paste('du -s ',dir,sep=''),intern = TRUE )
    check<-as.numeric(strsplit(check, '\t')[[1]][1])
    if (verbose>0){
      cat(dir, ' size at ',date(),': ',check, '\n')
      cat('    - Waiting for ',pauseTime,'min', '\n')
    }
    write(paste(dir, ' size at ',date(),': ',check, sep=''), file=logName, append=T)
    write(paste('    - Waiting for ',pauseTime,'min', sep=''), file=logName, append=T)
    
    Sys.sleep(pauseTime*60)
    check2<-system(paste('du -s ',dir,sep=''),intern = TRUE )
    check2<-as.numeric(strsplit(check2, '\t')[[1]][1])
    cat(dir, ' size at ',date(),': ',check2, '\n')
    write(paste(dir, ' size at ',date(),': ',check2, sep=''), file=logName, append=T)
    if (check==check2){
      cont=T
      if (verbose>0){
          cat('\n', 'Directory not changed in size. Continuing.....','\n')
      }
      write(paste('\n','Directory not changed in size. Continuing.....', sep=''), file=logName, append=T)
    }
    if (check!=check2){
      if (verbose>0){
        cat('\n', '***WARNING** Directory size changed. Pausing for ',pauseTime, 'min....', '\n')
      }
      write(paste('\n', '***WARNING** Directory size changed. Pausing for ',pauseTime, 'min....', sep=''), file=logName, append=T)
      
    }
    
  }
  
  
  
}