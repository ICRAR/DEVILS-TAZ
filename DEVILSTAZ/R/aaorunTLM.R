#' Reduce an AAT 2df+AAOmega tramline map file using 2dfDR function aaorun
#'
#' This function perfoms produces and aaorun tramline map form a given file.
#'  You must provide an idx filename. 
#'  Optionally you can also perform a bias subtraction and dark correction if provided (this is recomended)
#'
#' @param file Path to the input file to be reduced
#' @param idx Path to the idx aaorun paramter file
#' @param doDark TRUE/FLASE, do you want to perform dark frame correction
#' @param darkFile If doDark=TRUE, path to the input dark frame 
#' @param doBias TRUE/FLASE, do you want to perform bais frame subtraction
#' @param biasFile If doBias=TRUE, path to the input bias frame 
#' @param outname output filename of tramline map
#' @examples 
#' aaorunTML(file='arc2dF.fits', idx='gama_blue.idx', doDark=T,darkFile='DARKmaster.fits',doBias=T, biasFile='BIASmaster.fits',outname='TLM.fits')
#' @export
#' @export
aaorunTLM<-function(file=file, idx=idx, doDark=T, darkFile=darkFile, doBias=T, biasFile=biasFile, outname=outname, runZone=1){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    
    oldWD<-getwd()
    setwd(paste('runZone',runZone,sep=''))
    file<-paste('../',file,sep='')
    idx<-paste('../',idx,sep='')
    if (doBias==T) {biasFile<-paste('../',biasFile,sep='')}else{biasFile<-'NA'}
    if (doDark==T) {darkFile<-paste('../',darkFile,sep='')}else{darkFile<-'NA'}

    outname<-paste('../',outname,sep='')
    
    cmd<-paste('aaorun reduce_fflat ', file, ' -idxfile ',idx, ' -useflatim 0 -do_redfl 0 -lacosmic NO -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -tlmap_filename ',   outname, sep='')
    
    
    system(cmd)
    setwd(oldWD)

}
