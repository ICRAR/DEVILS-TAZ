aaorunTLM<-function(file=file, idx=idx, doDark=T, darkFile=darkFile, doBias=T, biasFile=biasFile, outname=outname){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    
    cmd<-paste('aaorun reduce_fflat ', file, ' -idxfile ',idx, ' -useflatim 0 -do_redfl 0 -lacosmic NO -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -tlmap_filename ',   outname, sep='')
    
 
    system(cmd)

}
