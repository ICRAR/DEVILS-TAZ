#' Check Configuration Files are consistent with DMCat
#'
#' @description Checks that final output configuration files are consistent with
#' the current DMCat in terms of fields, IDs, priorities, star-galaxy flags, VISCLASS flags and
#' mask flags.
#'
#' @param configFiles Vector of paths to config files to be checked.
#' @param DMCat Path to current DMCat to compare against
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything

#' @examples

#' @export
checkConfig <- function(configFiles = configFiles, DMCatN = DMCatN, logName=logName, verbose=verbose) {
  load(DMCatN)
  
  for (i in 1:length(configFiles)) {
    
    if (verbose>0){cat('   -Checking Config File:',configFiles[i], '\n')}
    write('   -Checking Config File:', file=logName, append=T)
    
    Config <- readLines(configFiles[i])
    Field <- substr(Config[1], 7, 9)
    Config <- Config[10:409]
    FIBRE <- c()
    ID <- c()
    RA <- c()
    DEC <- c()
    TYPE <- c()
    
    for (j in 1:length(Config)) {
      tmp <- strsplit(Config[j], ' ')[[1]]
      tmp <- tmp[which(tmp != "")]
      if (length(tmp) == 16) {
        FIBRE <- c(FIBRE, as.numeric(tmp[2]))
        ID <- c(ID, tmp[3])
        RA <-c(RA, hms2deg(as.numeric(tmp[4]), as.numeric(tmp[5]), as.numeric(tmp[6])))
        DEC <-c(DEC, dms2deg(as.numeric(tmp[7]), as.numeric(tmp[8]), as.numeric(tmp[9])))
        TYPE <- c(TYPE, tmp[16])
      }
      
    }
    
   
    
    for (j in 1:length(FIBRE)){
      
      if (verbose>1){cat('       -Checking Fibre:',FIBRE[j], '\n')}
      write(paste('       -Checking Fibre:',FIBRE[j],sep=''), file=logName, append=T)
      
      sel <- which(DMCat$CATAID == as.numeric(substr(ID[j], 2, nchar(ID[i]))))
      
      TYPE2<-TYPE[j]
      if (substr(TYPE[j], 1, 1)=='D'){TYPE2<-'Target'}
      
      if (verbose > 1) {
        cat('          - Fibre found as type:', TYPE2, '\n')
      }
      write(paste('          - Fibre found as type:', TYPE2, sep = ''),
            file = logName,
            append = T)
      
      warningsF <- 0
      
      if (length(sel) > 1) {
        if (DMCat$FIELD[sel] != Field) {
          if (verbose > 0) {
            cat(
              '*** WARNING CONFIG FILENME FOR FOR FIELD NOT THE SAME AS TARGET FIELD FOR ID:',
              ID[j],
              '\n'
            )
            warningsF <- warningsF + 1
          }
          write(
            paste(
              '*** WARNING CONFIG FILENME FOR FOR FIELD NOT THE SAME AS TARGET FIELD FOR ID:',
              ID[j],
              sep = ''
            ),
            file = logName,
            append = T
          )
        }
        if (DMCat$STARCLASS[sel] != 0 &
            substr(TYPE[i], 1, 1) == 'D') {
          if (verbose > 0) {
            cat('*** WARNING TARGET HAS STARCLASS!=0 FOR ID:', ID[j], '\n')
          }
          write(
            paste('*** WARNING TARGET HAS STARCLASS!=0 FOR ID:', ID[j], sep = ''),
            file = logName,
            append = T
          )
        }
        if (DMCat$MASK_FLAG[sel] != 0 &
            substr(TYPE[i], 1, 1) == 'D') {
          if (verbose > 0) {
            cat('*** WARNING TARGET HAS MASK_FLAG!=0 FOR ID:', ID[j], '\n')
          }
          write(
            paste('*** WARNING TARGET HAS MASK_FLAG!=0 FOR ID:', ID[j], sep = ''),
            file = logName,
            append = T
          )
          warningsF <- warningsF + 1
        }
        if (is.na(DMCat$VISCLASS[sel]) == FALSE) {
          if (DMCat$VISCLASS[sel] != 1 & substr(TYPE[i], 1, 1) == 'D') {
            if (verbose > 0) {
              cat('*** WARNING TARGET HAS VISCLASS!=1 FOR ID:', ID[j], '\n')
            }
            write(
              paste('*** WARNING TARGET HAS VISCLASS!=1 FOR ID:', ID[j], sep = ''),
              file = logName,
              append = T
            )
            warningsF <- warningsF + 1
          }
          
        }
      }
      
      if (warningsF==0){
        
        if (verbose > 1) {cat('          - No warnings for ID:', ID[j], '\n')}
        write(paste('          - No warnings for ID:', ID[j], sep = ''),file = logName, append = T)
      }
      
    }
    
  }
  
  
  
  
  
  
}