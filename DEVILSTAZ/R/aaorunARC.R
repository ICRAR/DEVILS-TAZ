#' Reduce an AAT 2df+AAOmega arc file using 2dfDR function aaorun
#'
#' This function perfoms the data reduction on a 2dfDR arc file.
#'  You must provide an idx filename and tramline file name (potentially produced by aaorunTLM.R). 
#'  Optionally you can also perform a bias subtraction and dark correction if provided (this is recomended)
#'
#' @param file Path to the input fits file to be reduced
#' @param idx Path to the idx aaorun paramter file
#' @param doDark TRUE/FLASE, do you want to perform dark frame correction
#' @param darkFile If doDark=TRUE, path to the input FITS dark frame 
#' @param doBias TRUE/FLASE, do you want to perform bais frame subtraction
#' @param biasFile If doBias=TRUE, path to the input FITS bias frame 
#' @param tlmFile Path to the input FITS tramline map
#' @examples 
#' aaorunARC(file='arc2dF.fits', idx='gama_blue.idx', doDark=T,darkFile='DARKmaster.fits',doBias=T, biasFile='BIASmaster.fits', tlmFile='TLM.fits')
#' @export
aaorunARC<-function(file=file, idx=idx, doDark=T,darkFile=darkFile,doBias=T, biasFile=biasFile, tlmFile=tlmFile){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    
    cmd<-paste('aaorun reduce_arc ', file, ' -idxfile ',idx, ' -useflatim 0 -useflat 1 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -tlmap_filename ', tlmFile, sep='')
    system(cmd)

}
