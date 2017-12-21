#' Reduce an AAT 2df+AAOmega flat file using 2dfDR function aaorun
#'
#' This function perfoms the data reduction on a 2dfDR flat file.
#'  You must provide an idx filename and arc file name (potentially produced by aaorunARC.R). 
#'  Optionally you can also perform a bias subtraction and dark correction if provided (this is recomended)
#'
#' @param file Path to the input file to be reduced
#' @param idx Path to the idx aaorun paramter file
#' @param doDark TRUE/FLASE, do you want to perform dark frame correction
#' @param darkFile If doDark=TRUE, path to the input dark frame 
#' @param doBias TRUE/FLASE, do you want to perform bais frame subtraction
#' @param biasFile If doBias=TRUE, path to the input bias frame 
#' @param arcFile Path to the input arc file
#' @param waveStart The lower bound of the wavelemngth range to use in the reduction
#' @param waveEnd The upper bound of the wavelemngth range to use in the reduction
#' @examples 
#' aaorunFLAT(file='flat2dF.fits', idx='gama_blue.idx', doDark=T,darkFile='DARKmaster.fits',doBias=T, biasFile='BIASmaster.fits', arcFile='arc2dF.fits')
#' @export
aaorunFLAT<-function(file=file, idx=idx, doDark=T,darkFile=darkFile,doBias=T, biasFile=biasFile, arcFile=arcFile, waveStart='NA',waveEnd='NA'){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    oldWD<-getwd()
    setwd(paste('runZone',runZone,sep=''))
    file<-paste('../',file,sep='')
    idx<-paste('../',idx,sep='')
    biasFile<-paste('../',biasFile,sep='')
    darkFile<-paste('../',darkFile,sep='')
    arcFile<-paste('../',arcFile,sep='')

    if (is.numeric(waveStart)==FALSE){ 
        cmd<-paste('aaorun reduce_fflat ', file, ' -idxfile ',idx, ' -useflatim 0 -do_tlmap 0 -do_extra 0 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -wavel_filename ', arcFile, sep='')
    }

    if (is.numeric(waveStart)==TRUE){ 
        cmd<-paste('aaorun reduce_fflat ', file, ' -idxfile ',idx, ' -useflatim 0 -do_tlmap 0 -do_extra 0 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -wavel_filename ', arcFile, '-WAVE_START ', waveStart, '-WAVE_END ', waveEnd, sep='')
        }
  
    system(cmd)

    setwd(oldWD)

}
