visSpec<-function(){
  path='data/reduced/stackedSpec/'
  listFiles<-list.files(path=path, pattern='*.Rdata')
  
  for (i in 1:length(listFiles)){
    load(paste(path,'/',listFiles[i],sep=''))
    if (spec$prob>0.5 & spec$prob<0.97){
      plotSpec(spec, leg.cex=0.8)
      s<-1
      zOrig<-spec$z
      s<-readline('Enter Selection (default=nothing, 2=galaxy with correct z but low prob, 3=galaxy with incorrect z, 4=QSO with correct z but low prob, 5=QSO with incorrect z:')
      if (s==''){s<-1}
      
      if (s==3 | s==5){
        good<-'n'
        while (good!='y') {
          z<-readline("Enter New Redshift:")
          
          spec$z<-as.numeric(z)
          cat('New redshfit set as: ', spec$z, '\n')
          plotSpec(spec, leg.cex=0.8)
          good<-readline('Keep new redshift? Yes, No, Return to orginal (y/n/e)')
          if (good=='e'){
            spec$z<-zOrig
            good<-'y'
          }
        }
      }
      
      VISRED<-spec$z
      
      write(paste(spec$ID,',',VISRED,',',s, sep=''),file='data/catalogues/VIS_GoodSpec_tmp.csv', append=T)
      }
  }
  
}

