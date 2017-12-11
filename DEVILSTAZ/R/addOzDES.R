addOzDES<-function(OzDESCat=OzDESCat, DODir=DoDir, logName=logName, verbose=verbose){
  
  OzDES<-read.table(OzDESCat)
  DOcat<-paste(DODir, list.files(path=DODir, pattern='DObj*'),sep='')
  
  
}