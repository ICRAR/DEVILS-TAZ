% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runTiler.R
\name{runTiler}
\alias{runTiler}
\title{Produce fibre configuration files for a givne set of DOCats}
\usage{
runTiler(configdir = configdir, workingDir = workingDir,
  DOcat = DOcat, DATAguide = DATAguide, DATAstspec = DATAstspec,
  DATAsky = DATAsky, N_D02A = N_D02A, N_D02B = N_D02B,
  N_D03 = N_D03, N_D10 = N_D10, D02A_startPlate = 0,
  D02B_startPlate = 0, D03_startPlate = 0, D10_startPlate = 0,
  logName = logName, verbose = verbose, cores = cores,
  makeNormal = TRUE, makeBackUp = FALSE, BrightCut = 20)
}
\arguments{
\item{configdir}{Directory path location of Configure software}

\item{workingDir}{The directory you want to do the tiling in}

\item{DOcat}{A target catalogue}

\item{DATAguide}{A guide star catalogue}

\item{DATAstspec}{A standard star catalogue}

\item{DATAsky}{A sky potions catalogue}

\item{N_D02A}{Number of configurations to generate in D02A}

\item{N_D02B}{Number of configurations to generate in D02B}

\item{N_D03}{Number of configurations to generate in D03}

\item{N_D10}{Number of configurations to generate in D10}

\item{D02A_startPlate}{Start plate number of D02A configurations (0 or 1)}

\item{D03_startPlate}{Start plate number of D03 configurations (0 or 1)}

\item{D10_startPlate}{Start plate number of D10 configurations (0 or 1)}

\item{logName}{log filename to write progress to}

\item{verbose}{tell me whats going on: 0=nothing, 1=somethings, 2=everything}

\item{cores}{number of cores to use (max four in this case) - currently redundant}

\item{makeNormal}{Make general configurations}

\item{makeBackUp}{Also make configuration files for bright sources.}

\item{BrightCut}{Magnitude to cut at for bright sources. Only takes affect it makeBackUp==TRUE.}

\item{D02A_startPlate}{Start plate number of D02B configurations (0 or 1)}
}
\value{
List of paths to new configuration files
}
\description{
This is the highlevel main TAZ function for running running the Tiler software
to gnerate fibre configuration files. Users must provide DOcats for targets, sky, standards and guides.
}
