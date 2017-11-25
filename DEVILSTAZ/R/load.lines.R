load.lines=function(){
  c<-299792458
  names <- c('Lya', 'OIIB','OIIR', 'HB', 'OIIIB', 'OIIIR', 'HA', 'NIIB', 'NIIR', 'SIIB', 'SIIR', 'K', 'H', 'G', 'Mg', 'Na')
  stellar <- c(F,F,F,F,F,F,F,F,F,F,F,T,T,T,T,T)
  wave_ang <- c(1215.6, 3726.1, 3728.8, 4862.721,4960.295, 5008.239, 6564.614, 6549.86, 6585.27, 6718.29, 6732.68, 3933.663, 3964.468, 4304.4,5175.3,5894.0)
  freq_hz <- c/(wave_ang/(10.^10))
  wave_m <- wave_ang/(10.^10)
  wave_micron <- wave_ang/(10.^4)
  wave_nm <- wave_ang/(10.^1)
  lines<-data.frame(names, wave_ang,wave_m,wave_micron,wave_nm, freq_hz, stellar)
  return(lines) 
}
