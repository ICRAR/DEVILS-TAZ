#' Load common spectral feature list
#'
#' @description Function to load common galaxy emission and absorption features. 
#' 
#' @return dataframe of line names, wavelengths in various units, frequency and emission/absorption flag  
#' @examples 
#' load(paste(.libPaths(),'/DEVILSTAZ/data/ExampleSpec.Rdata',sep=''))
#' #plot rest-frame spectrum
#' plot(spec$wave/(1+spec$z), hanning.smooth(spec$flux, degree=9), type='l', xlab='Wavelength, ang', ylab='Counts', ylim=c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*1.1))
#' lineList<-load.lines()
#' print(lineList)
#' lines(c(lineList$wave_ang[2], lineList$wave_ang[2]), c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*0.8), lty=2, col='blue')
#' text(lineList$wave_ang[2], max(spec$flux,na.rm=T),lineList$names[2], col='blue', cex=0.3)
#' lines(c(lineList$wave_ang[4], lineList$wave_ang[4]), c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*0.8), lty=2, col='blue')
#' text(lineList$wave_ang[4], max(spec$flux,na.rm=T),lineList$names[4], col='blue', cex=0.3)
#' lines(c(lineList$wave_ang[5], lineList$wave_ang[5]), c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*0.8), lty=2, col='blue')
#' text(lineList$wave_ang[5], max(spec$flux,na.rm=T),lineList$names[5], col='blue', cex=0.3)
#' lines(c(lineList$wave_ang[6], lineList$wave_ang[6]), c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*0.8), lty=2, col='blue')
#' text(lineList$wave_ang[6], max(spec$flux,na.rm=T),lineList$names[6], col='blue', cex=0.3)
#' lines(c(lineList$wave_ang[12], lineList$wave_ang[12]), c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*0.8), lty=2, col='darkgreen')
#' text(lineList$wave_ang[12], max(spec$flux,na.rm=T),lineList$names[12], col='darkgreen', cex=0.3)
#' lines(c(lineList$wave_ang[13], lineList$wave_ang[13]), c(-max(spec$flux,na.rm=T),max(spec$flux,na.rm=T)*0.8), lty=2, col='darkgreen')
#' text(lineList$wave_ang[13], max(spec$flux,na.rm=T),lineList$names[13], col='darkgreen', cex=0.3)
#' @export
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
