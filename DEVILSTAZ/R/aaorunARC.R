aaorunARC<-function(file=file, idx=idx, doDark=T,darkFile=darkFile,doBias=T, biasFile=biasFile, tlmFile=tlmFile){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    
    cmd<-paste('aaorun reduce_arc ', file, ' -idxfile ',idx, ' -useflatim 0 -useflat 1 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -tlmap_filename ', tlmFile, sep='')
    system(cmd)

}
