aaorunObj<-function(file=file, idx=idx, doDark=T, darkFile=darkFile, doBias=T, biasFile=biasFile, flatFile=flatFile, tlmFile=tlmFile, arcFile=arcFile){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}
    
    
    cmd<-paste('aaorun reduce_object ', file, ' -idxfile ',idx, ' -useflatim 0 -usefflat 1 -fflat_filename ',flatFile,' -tlmap_filename ',tlmFile,' -wavel_filename ',arcFile,'    -do_bias ',biasNum,' -do_extra 1 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, sep='')
    
    system(cmd)

}
