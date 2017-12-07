#' Plots a spectrum object generated as part of TAZ
#'
#' @description Function takes a 'spec' R list with sepc$wave, spec$flux, ...
#' and plots the full 1D spectrum and zoom ins of key emission and absorption 
#' lines. Lines will be scaled to the correct redshfit in spec$z is provided. If 
#' no spec$z is provided, line positions and zooms are plotted in the rest-frame. 
#' 
#' @param spec An R stucture containing spec$wave = vector of spectrum wavelengths
#' and spec$flux = vector of spectrum fluxes.     
#' @examples 
#' load(paste(.libPaths(),'/DEVILSTAZ/data/ExampleSpec.Rdata',sep=''))
#' plotSpec(spec)
#' @export
plotSpec<-function(spec=spec){
  
options(warn=-1)  
  
  textZ<-spec$z
  
if (is.null(spec$MAG)==TRUE){spec$MAG<-NA}
if (is.null(spec$ID)==TRUE){spec$ID<-'No ID Provided'}  
if (is.null(spec$EXP)==TRUE){spec$EXP<-NA}
if (is.null(spec$prob)==TRUE){spec$prob<-NA} 
  
if (is.null(spec$z)==TRUE){
    spec$z<-0
    textZ<-NA
}
  
par(mfrow = c(3, 3))
par(mar=c(3.1,3.1,1.1,1.1))

layout(matrix(c(1,1,1,2,3,4, 5, 6,7), 3, 3, byrow = TRUE))


lines<-load.lines()
line_x <- as.numeric(lines$wave_ang)*(1+spec$z)
peak_F<-max(spec$flux, na.rm=T)

magplot(spec$wave, hanning.smooth(spec$flux, degree=9), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=c(3600,9000), ylim=c(peak_F*-1.2,peak_F*1.2), lwd=2, main=paste(spec$ID, ' - Hanning Smoothed, degree=9',sep=''))

plotZ<-spec$z
if (is.finite(plotZ)==FALSE){
  plotZ<-0
  textZ<-NA
  }

plotLines(z=plotZ, xunit='ang', labPos=0.8*max(spec$flux,na.rm=T), lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-50)


legend('bottomright', legend=c(paste('ID=',spec$ID,sep=''), paste('z=',textZ,sep=''), paste('mag=',spec$MAG,sep=''), paste('Prob=',spec$prob,sep=''), paste('TEXP=',spec$EXP,sep='')), bg='white')

degSmooth<-7

range<-250
wavePoint<-3727
peak<-1.2*max(spec$flux[which(spec$wave > ((1+plotZ)*wavePoint)-range & spec$wave < ((1+plotZ)*wavePoint)+range)],na.rm=T)
if (is.finite(peak)==F){peak=peak_F}
magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+plotZ)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='OII', lwd=2)
plotLines(z=plotZ, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

range<-500
wavePoint<-4950
peak<-1.2*max(spec$flux[which(spec$wave > ((1+plotZ)*wavePoint)-range & spec$wave < ((1+plotZ)*wavePoint)+range)],na.rm=T)
if (is.finite(peak)==F){peak=peak_F}
magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+plotZ)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='OIII/H-beta', lwd=2)
plotLines(z=plotZ, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

range<-300
wavePoint<-6650
peak<-1.2*max(spec$flux[which(spec$wave > ((1+plotZ)*wavePoint)-range & spec$wave < ((1+plotZ)*wavePoint)+range)],na.rm=T)
if (is.finite(peak)==F){peak=peak_F}
magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+plotZ)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='H-alpha, NII, SII', lwd=2)
plotLines(z=plotZ, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

range<-500
wavePoint<-4150
peak<-1.2*max(spec$flux[which(spec$wave > ((1+plotZ)*wavePoint)-range & spec$wave < ((1+plotZ)*wavePoint)+range)],na.rm=T)
if (is.finite(peak)==F){peak=peak_F}
magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+plotZ)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='Ca H&K, G-band', lwd=2)
plotLines(z=plotZ, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)


range<-250
wavePoint<-5175
peak<-1.2*max(spec$flux[which(spec$wave > ((1+plotZ)*wavePoint)-range & spec$wave < ((1+plotZ)*wavePoint)+range)],na.rm=T)
if (is.finite(peak)==F){peak=peak_F}
magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+plotZ)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='Mg', lwd=2)
plotLines(z=plotZ, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

range<-250
wavePoint<-5895
peak<-1.2*max(spec$flux[which(spec$wave > ((1+plotZ)*wavePoint)-range & spec$wave < ((1+plotZ)*wavePoint)+range)],na.rm=T)
if (is.finite(peak)==F){peak=peak_F}
magplot(spec$wave, hanning.smooth(spec$flux, degree=degSmooth), xlab='Wavelength, Ang', ylab='Counts', grid=T, type='l', xlim=(1+plotZ)*wavePoint+c(-range,range), ylim=c(-peak,peak), main='Na', lwd=2)
plotLines(z=plotZ, xunit='ang', labPos=0.3*peak, lty=2, cex=1, EmCol='blue', AbsCol='darkgreen', labOff=-20)

options(warn=0)

}