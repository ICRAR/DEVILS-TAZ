#' Reduce an AAT 2df+AAOmega object file using 2dfDR function aaorun
#'
#' This function perfoms the data reduction on a 2dfDR object file.
#'  You must provide an idx filename, flat file (potentially produced by aaorunFLAT.R), 
#'  arc file name (potentially produced by aaorunARC.R), and tramline map (potentially produced by aaorunTLM.R). 
#'  Optionally you can also perform a bias subtraction and dark correction if provided (this is recomended)
#'
#' @param file Path to the input file to be reduced
#' @param idx Path to the idx aaorun paramter file
#' @param doDark TRUE/FLASE, do you want to perform dark frame correction
#' @param darkFile If doDark=TRUE, path to the input dark frame 
#' @param doBias TRUE/FLASE, do you want to perform bais frame subtraction
#' @param biasFile If doBias=TRUE, path to the input bias frame 
#' @param flatFile Path to the input flat file
#' @param tlmFile Path to the input tramline map file
#' @param arcFile Path to the input arc file
#' @examples 
#' aaorunObj(file='object2dF.fits', idx='gama_blue.idx', doDark=T,darkFile='DARKmaster.fits',doBias=T, biasFile='BIASmaster.fits', flatFile='flat2dF.fits',tmlFile='TLM.fits',arcFile='arc2dF.fits')
#' @export
aaorunObj<-function(file=file, idx=idx, doDark=T, darkFile=darkFile, doBias=T, biasFile=biasFile, flatFile=flatFile, tlmFile=tlmFile, arcFile=arcFile){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    
    cmd<-paste('aaorun reduce_object ', file, ' -idxfile ',idx, ' -useflatim 0 -usefflat 1 -fflat_filename ',flatFile,' -tlmap_filename ',tlmFile,' -wavel_filename ',arcFile,'    -do_bias ',biasNum,' -do_extra 1 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, sep='')
    
    system(cmd)

}
