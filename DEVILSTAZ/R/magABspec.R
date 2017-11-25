magABspec=function(spec, filter='r'){
  if (spec$xunit[1]=='ang') {wavefac=1e-10}
  if (spec$xunit[1]=='mircon') {wavefac=1e-6}
  if (spec$xunit[1]=='m') {wavefac=1}
  if (spec$xunit[1]=='nm') {wavefac=1e-9} 
  c<-299792458

  wave<-spec$wave
  flux<-spec$flux

  mag<-c(1:length(filter))
  for (i in 1:length(filter)){
    
    filter2=getfilt(filter[i])[,2:3]

    if (spec$yunit[1]=='ang') {fluxnu=(wavefac*flux*wave^2)/c}
    if (spec$yunit[1]=='hz') {fluxnu=flux}
    if (spec$yunit[1]=='Jy') {fluxnu=flux*1e-23}
    
    spec$flux<-fluxnu
    
    totlumnu = bandpass(spec, filter = filter2, lum = T)
    
    mag[i]= -2.5 * log10(totlumnu) - 48.6
  }
  return=mag

}

