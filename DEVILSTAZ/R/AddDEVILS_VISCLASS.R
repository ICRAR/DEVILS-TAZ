#' Updates DEVILS_CurrentVIS and DMCat$DEVILS_UTMJD_VIS to DMCat
#'
#' @description Function takes in ID from a given directory in folders called 'Good' and 'Bad'
#' everything in 'Good' gets assinged a DEVILS_CurrentVIS==1, everything that is in 'Bad' gets a 
#' DEVILS_CurrentVIS==0. The UTMJD of the current or a specific date is added to DEVILS_UTMJD_VIS.   
#' 
#' @param DMCatPath Path to the location of the DMCat you want to update
#' @param VisDir The directory path of the visual calisifications (must contain sub-folder 'Good' and 'Bad')
#' @param UTMJD UTMJD to add to DEVILS_UTMJD_VIS column. If NA, current date will be added.
#' @param vebose Tell me what's going on
#' @examples 
#'AddDEVILS_VISCLASS(DMCatPath='catalogues/MASTERcats/DMCat2018-12-31.rda', VisDir='/Users/luke/work/DEVILS/VIS_CHECKS_copy/', UTMJD=NA)
#' @export
AddDEVILS_VISCLASS<-function(DMCatPath=DMCatPath, VisDir=VisDir, UTMJD=NA, verbose=T){
  
  if (verbose==T){
    cat('\n','***Running AddDEVILS_VISCLASS***','\n\n')
    cat('- Updating catalogue:',DMCatPath,'\n')
    cat('- Finding VISCLASS in:',VisDir,'\n')
  }
  load(DMCatPath)
  
  goodIDs<-as.numeric(substr(list.files(path=paste(VisDir,'Good/',sep=''), pattern='*.pdf'),2,nchar(list.files(path=paste(VisDir,'Good/',sep=''), pattern='*.pdf'))-4))
  badIDs<-as.numeric(substr(list.files(path=paste(VisDir,'Bad/',sep=''), pattern='*.pdf'),2,nchar(list.files(path=paste(VisDir,'Bad/',sep=''), pattern='*.pdf'))-4))
  
  DMCat$DEVILS_CurrentVIS[which(DMCat$CATAID %in% goodIDs ==TRUE)]<-1
  DMCat$DEVILS_CurrentVIS[which(DMCat$CATAID %in% badIDs ==TRUE)]<-0
  
  if (is.na(UTMJD)==T){
    tmp<-strsplit(as.character(Sys.Date()), '-')
    UTMJD<-date2jd(year=as.numeric(tmp[[1]][1]), mon=as.numeric(tmp[[1]][2]), mday=as.numeric(tmp[[1]][3]))-2400000.5
  }
  
  if (verbose==T){
    cat('\n','Adding UTMJD=',UTMJD,' to DEVILS_UTMJD_VIS column....' )
  }
  DMCat$DEVILS_UTMJD_VIS[which(DMCat$CATAID %in% goodIDs ==TRUE | DMCat$CATAID %in% badIDs ==TRUE)]<-UTMJD
  
  if (verbose==T){
    cat('\n','Saving new version of ', DMCatPath, '...')
  }
  save(DMCat, file=DMCatPath)
  
  if (verbose==T){
    cat('\n','***Finishing AddDEVILS_VISCLASS***','\n\n')
  }
  
}