% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutozSingleSpec.R
\name{AutozSingleSpec}
\alias{AutozSingleSpec}
\title{Automatic redshifting tool for 1D spectra}
\usage{
AutozSingleSpec(specRaw,
  tempFile = "data/calibrators/AutoZTemp/filtered-templates.fits",
  oversample = 5, num = 5, templateNumbers = c(2:14, 16:22, 40:47),
  stLambda = 3726, endLambda = 8850, minval = -10000,
  maxval = 1e+06, z_prior = c(-1, 1000), doHelio = T, highZ = T,
  verbose = TRUE)
}
\arguments{
\item{specRaw}{R struture contianing information required information.
Must have the following componenents: specRaw$wave=vector of spectrum wavelengths, 
specRaw$flux=vector spectrum fluxes the same size as specRaw$wave, specRaw$error=vector 
spectrum errors the same size as specRaw$wave, and if doHelio=T, specRaw$RA=spectrum RA,
specRaw$DEC=spectrum DEC, specRaw$UTMJD = observation Jullian date, specRaw$longitude = 
observatory longitude, specRaw$latitude = observatory latitude, specRaw$altitude = 
observatory altitude.}

\item{tempFile}{Path to file containing spectral template data}

\item{oversample}{wavelength oversampling rate}

\item{num}{number of crosscorrelation peaks to identify}

\item{templateNumbers}{template numbers to use in fitting}

\item{stLambda}{lower bound of the wavelength range to fit over}

\item{endLambda}{= upper bound of the wavelength range to fit over}

\item{minval}{minmum value to reject croos correlations}

\item{maxval}{maximum value to reject croos correlations}

\item{z_prior}{redshift prior, two element vector with c(lo, hi)}

\item{doHelio}{TRUE/FALSE perform helocentric correction. If TRUE you must 
provide RA,DEC,UTMJD, longitude, latitude and altitue in the specRaw structure.}

\item{verbose}{TRUE/FLASE - let me know what's going on.}
}
\description{
Redshfiting tool originaly developed by Ivan Baldry in IDL (Baldry et al., 2014 MNRAS). 
Repurposed in R by Luke Davies and  Leon Drygala. Takes a 1D spectrum and perfoms a 
Fourier cross corellation with a template spectra set. Returns the best fit redshifts
and probabilities.
}
\examples{
load(paste(.libPaths(),'/DEVILSTAZ/data/ExampleSpec.Rdata',sep=''))
plot(spec$wave, hanning.smooth(spec$flux, degree=9), type='l', xlab='Wavelength, ang', ylab='Counts')
spec$error<-spec$sn
autoz_out<-AutozSingleSpec(spec, doHelio=F)
plotLines(z=autoz_out$z)
cat('Probability of correct redshift is: ', autoz_out$prob)
}
