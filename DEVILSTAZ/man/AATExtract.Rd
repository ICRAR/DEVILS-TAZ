% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AATExtract.R
\name{AATExtract}
\alias{AATExtract}
\title{Extract 1D spectra from reduced 2dF+AAOmega Frame}
\usage{
AATExtract(fileN = fileN, cores = 1)
}
\arguments{
\item{fileN}{Path to the input 2dF+AAOmega reduced file to be extracted}

\item{cores}{Number of cores to run over}
}
\description{
This function extracts 1D spectra from a 2dF+AAOmega frame,
runs automatic redshifting using AutoZ, and produced 1D diagnositc 
plots for all spectra. Function will produce a directory called
*fileN*_extracted/ which will contain all extracted spectra.
The function can be run is parallel with a specified number of 
cores.
}
\examples{
AATExtract(fileN='object2dF_red.fits', cores=4)
}
