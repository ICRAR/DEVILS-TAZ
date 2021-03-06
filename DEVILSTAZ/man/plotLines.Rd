% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotLines.R
\name{plotLines}
\alias{plotLines}
\title{Overplot the position of common spectral features on the currently plotted spectrum}
\usage{
plotLines(z = 0, xunit = "ang", labPos = 100, labOff = -50,
  EmCol = "blue", AbsCol = "darkgreen", lty = 2, lwd = 1,
  cex = 0.3, ...)
}
\arguments{
\item{z}{redshift at which to overplot lines}

\item{xunit}{units of the xaxis. Options are ang,hz,micron,m,nm}

\item{labPos}{y axis value of where to print line labels}

\item{labOff}{xaxis offset to apply to labels}

\item{EmCol}{colour to plot emission lines}

\item{AbsCol}{colour to plot absorption lines}

\item{lty}{line type for plotting}

\item{lwd}{line thinckness for plotting}

\item{cex}{label text size}
}
\description{
Function to plot the position of common spectral absorption and emission line features
over the current plotting window.
}
\examples{
load(paste(.libPaths(),'/DEVILSTAZ/data/ExampleSpec.Rdata',sep=''))
plot(spec$wave, hanning.smooth(spec$flux, degree=9), type='l', xlab='Wavelength, ang', ylab='Counts') 
plotLines(z=spec$z, abPos=100, labOff=-50, EmCol='blue', AbsCol='darkgreen', lty=2, lwd=2, cex=0.5)
}
