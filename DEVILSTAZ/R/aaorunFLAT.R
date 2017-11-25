aaorunFLAT<-function(file=file, idx=idx, doDark=T,darkFile=darkFile,doBias=T, biasFile=biasFile, arcFile=arcFile, waveStart='NA',waveEnd='NA'){

    darkNum<-1
    biasNum<-1
    if (doDark==F){darkNum<-0}
    if (doBias==F){biasNum<-0}

    if (is.numeric(waveStart)==FALSE){ 
        cmd<-paste('aaorun reduce_fflat ', file, ' -idxfile ',idx, ' -useflatim 0 -do_tlmap 0 -do_extra 0 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -wavel_filename ', arcFile, sep='')
    }

    if (is.numeric(waveStart)==TRUE){ 
        cmd<-paste('aaorun reduce_fflat ', file, ' -idxfile ',idx, ' -useflatim 0 -do_tlmap 0 -do_extra 0 -usebiasim ',biasNum,' -bias_filename ',biasFile, ' -usedarkim ',darkNum,' -dark_filename ',darkFile, ' -wavel_filename ', arcFile, '-WAVE_START ', waveStart, '-WAVE_END ', waveEnd, sep='')
        }
  
    system(cmd)

}
