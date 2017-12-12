addOzDES<-function(OzDESCat=OzDESCat, DODir=DoDir, num=10, logName=logName, verbose=verbose){
  
  
  D10_RA<-c(149.38,150.7)
  D10_DEC<-c(1.65,2.79)
  D02_RA<-c(34.0,37.05)
  D02_DEC<-c(-5.2,-4.2)
  
  D02A_RA<-c(D02_RA[1], sum(D02_RA)/2)
  D02A_DEC<-D02_DEC
  
  D02B_RA<-c(sum(D02_RA)/2,D02_RA[2])
  D02B_DEC<-D02_DEC
  
  D03_RA<-c(52.3,54.0)
  D03_DEC<-c(-28.5,-27.5)
  
  
  if (verbose>1){cat('    -Reading OzDES catalogue:', OzDESCat, '\n')}
  write(paste('   -Reading OzDES catalogue:', OzDESCat,sep=''), file=logName, append=T)
  
  OzDES<-read.csv(OzDESCat, skip=3, header=F)
  
  DOcatName<-paste(DODir,'/', list.files(path=DODir, pattern='DObj*'),sep='')
  
  if (verbose>1){cat('    -Reading DOCat catalogue:', DOcatName, '\n')}
  write(paste('   -Reading DOCat catalogue:', DOcatName,sep=''), file=logName, append=T)
  
  DOCat<-read.table(DOcatName, header=T)
  sel<-0
  while (length(unique(sel))<num){
    sel<-round(runif(num,1,length(OzDES[,1])))
  }
  NewCATAID<-OzDES[sel,1]
  NewRA<-OzDES[sel,2]
  NewDEC<-OzDES[sel,3]
  NewMAG<-OzDES[sel,4]
  NewSURVEY_CLASS<-rep(3,num)
  NewPRIORITY_CLASS<-rep(9,num)
  
  if (verbose>1){cat('        -Adding OzDES sources:', '\n')}
  write(paste('        -Adding OzDES sources:',sep=''), file=logName, append=T)
  
  for (i in 1:num){
    
    if (verbose>1){cat('           ',NewCATAID[i], NewRA[i], NewDEC[i], '\n')}
    write(paste('           ',NewCATAID[i], NewRA[i], NewDEC[i] ,sep=' '), file=logName, append=T)

  }
  
  
  NewPOSITION<-rep('D02A',num)
  NewPOSITION[which(NewRA>D02B_RA[1] & NewRA<D02B_RA[2])]<-'D02B'
  NewPOSITION[which(NewRA>D03_RA[1] & NewRA<D03_RA[2])]<-'D03'
  NewPOSITION[which(NewRA>D10_RA[1] & NewRA<D10_RA[2])]<-'D10'
  
  newRows<-data.frame(NewCATAID, NewRA, NewDEC, NewMAG, NewSURVEY_CLASS, NewPRIORITY_CLASS, NewPOSITION)
  names(newRows)<-c('CATAID', 'RA', 'DEC', 'MAG', 'SURVEY_CLASS', 'PRIORITY_CLASS', 'POSITION')
  DOCat<-rbind(DOCat, newRows)
  
  suppressWarnings(write.table(DOCat, file=DOcatName, append=F, col.names=T, row.names=F, quote=FALSE, sep = "\t"))
  
 
}