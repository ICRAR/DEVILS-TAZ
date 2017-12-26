visSpec<-function(){
  path='data/reduced/stackedSpec/'
  listFiles<-list.files(path=path, pattern='*.Rdata')
  
  for (i in 1:length(listFiles)){
    load(paste(path,'/',listFiles[i],sep=''))
    if (spec$prob>0.5 & spec$prob<0.97){
      plotSpec(spec)
      s<-1
      s<-readline('Enter Selection (default=nothing, 9=galaxy with correct z, 10=QSO with correct z, 11=QSO with incorrect z:')
     
      if (s!=1){
        write(paste(spec$ID,',',s, sep=''),file='data/catalogues/VIS_GoodSpec_tmp.csv', append=T)
      }
      }
  }
  
}

