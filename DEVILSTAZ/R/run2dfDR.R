#' Run 2dFDR over a target directory
#'
#' @description This is the highlevel main TAZ function for running 2dFDR (via aaorun) data reduction over 
#' a target directory. Function will find and reduce calibrators, make master bias and 
#' dark frames, produce QC plots, reduce arc, flat and object files, combine data files
#' for each ccd arm and splice to form final spectrum.
#'  
#' @param toReduce directory path to reduce in the DEVILS data structure. Must be of the form, 
#' data/rawdata/run#_YYYY_MM/YYYY_MM_DD (i.e. data/rawdata/run1_2017_12/2017_12_18).
#' @param doCalibQC TRUE/FLASE, produce QC check plots for biases and darks
#' @param logName log filename to write progress to
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything
#' @param reducCores number of cores to use
#' @param doCosmic  TRUE/FALSE do additonal cosmic ray rejection 
#' @examples 
#' run2dfDR(toReduce='data/rawdata/run1_2017_12/2017_12_18', doCalibQC=F, logName='tempLog.txt', verbose=1, reducCores=4)
#' @export
run2dfDR<-function(toReduce=toReduce, doCalibQC=doCalibQC, logName=logName, verbose=verbose, reducCores=reducCores, doDark=T, doCosmic=doCosmic){

                                        #****** need to add to log file in run2dfDR and vebose outputs *****

    system('cleanup')  
  
    host<-system('hostname',intern = TRUE)
    #if (host=="munro"){  
     # registerDoParallel(cores=1)
    #}
    
    registerDoParallel(cores=reducCores)
    
    write('', file=logName, append=T)
    if (verbose>0){cat(' **** Running run2dfDR.....', '\n')}
    write(' Running run2dfDR.....', file=logName, append=T)


    newReduce<-c()


                                        # loop over all new dates to reduce

    
    for (i in 1:length(toReduce)){
      
       system(paste('rm -rf ', toReduce[i],'/*im.fits',sep=''))
       system(paste('rm -rf ', toReduce[i],'/*ex.fits',sep=''))
       system(paste('rm -rf ', toReduce[i],'/*red*.fits',sep=''))
       system(paste('rm -rf /tmp/AAO*',sep=''))

        if (verbose>1){cat(paste('   - Reducing date: ',toReduce[i], sep=''), '\n')}
        write(paste(' - Reducing date: ',toReduce[i], sep=''), file=logName, append=T)

        dateReduc<-paste(strsplit(toReduce[i],'/')[[1]][3],'/',strsplit(toReduce[i],'/')[[1]][4],sep='')
        dateReduc2<-strsplit(toReduce[i],'/')[[1]][4]
        
        calib<-makeCalib(toReduce=toReduce[i], doCalibQC=doCalibQC, logName=logName, verbose=verbose)
        

        saveFile<-paste(toReduce[i],'/', strsplit(toReduce[i],"/")[[1]][4],'_metaData.Rdata', sep='')
        metaData<-getFileInfo(dir=toReduce[i],saveFile=saveFile, logName=logName, verbose=verbose)
        configList<-as.vector(unique(metaData$config))
        configList<-configList[which(configList!='NONE')]

        
        
        system(paste('mkdir data/reduced/',dateReduc,'/ccd1',sep=''))
        system(paste('mkdir data/reduced/',dateReduc,'/ccd2',sep=''))

        if (verbose>1){cat(paste('    - ',length(configList), ' configuration(s) found in ',toReduce[i], sep=''), '\n')}
        write(paste('    - ',length(configList), ' configuration(s) found in ',toReduce[i], sep=''), file=logName, append=T)

       
        
        
        
        if (host=="munro"){
          system('rm -rf /home/ldavies/imp_scratch/*')
        }
        if (host=="lukeLap"){
          system('rm -rf /Users/luke/imp_scratch/*')
        }
        
        system('rm -rf runZone*')
        


                
       newReduce = foreach(j=1:length(configList)) %dopar%  {
         
        
         
                                        #for (j in 1:length(configList)){

            if (verbose>1){cat(paste('         - Reducing configuration: ', configList[j], sep=''), '\n')}
            write(paste('         - Reducing configuration: ', configList[j], sep=''), file=logName, append=T)
            
            flat_ccd1<-metaData$fileName[which(metaData$config==configList[j] & metaData$type=='FLAT' & metaData$ccd=='blue')]
            arc_ccd1<-metaData$fileName[which(metaData$config==configList[j] & metaData$type=='ARC' & metaData$ccd=='blue')]
            targets_ccd1<-metaData$fileName[which(metaData$config==configList[j] & metaData$type=='TARGET'& metaData$ccd=='blue')]
            flat_ccd2<-metaData$fileName[which(metaData$config==configList[j] & metaData$type=='FLAT' & metaData$ccd=='red')]
            arc_ccd2<-metaData$fileName[which(metaData$config==configList[j] & metaData$type=='ARC' & metaData$ccd=='red')]
            targets_ccd2<-metaData$fileName[which(metaData$config==configList[j] & metaData$type=='TARGET'& metaData$ccd=='red')]

            if (length(targets_ccd1)>0){

                                        # reduce calibrations ccd1

                
                
                idx<-'data/idxFiles/ozdes_blue.idx'

                if (verbose>1){cat('             ** For blue ccd using: ', idx, '\n')}
                write(paste('             ** For blue ccd using: ', idx,sep=''), file=logName, append=T)

                
                inFile<-paste(toReduce[i], '/',flat_ccd1,sep='')

                tlmFile<-paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_tlm.fits', sep='')
                
                
               
                if (host=="munro"){
                  system(paste('cp ', calib$darkFileBlue, ' ', paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_darkBlue.fits',sep=''),sep=''))
                  calib$darkFileBlue<-paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_darkBlue.fits',sep='')
                  system(paste('cp ', calib$darkFileRed, ' ', paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_darkRed.fits',sep=''),sep=''))
                  calib$darkFileRed<-paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_darkRed.fits',sep='')
                  system(paste('cp ', calib$biasFileBlue, ' ', paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_biasBlue.fits',sep=''),sep=''))
                  calib$biasFileBlue<-paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_biasBlue.fits',sep='')
                  system(paste('cp ', calib$biasFileRed, ' ', paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_biasRed.fits',sep=''),sep=''))
                  calib$biasFileRed<-paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_biasRed.fits',sep='')
                  
                }


                if (verbose>1){
                    cat('             - Reducing TLM file for blue ccd with line:', paste('aaorunTLM(file=',toReduce[i], '/',flat_ccd1, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileBlue,', doBias=T,biasFile=',calib$biasFileBlue,', outname=',tlmFile,')', sep=''), '\n')
                }

                write(paste('             - Reducing TLM file for blue ccd with line: aaorunTLM(file=',toReduce[i], '/',flat_ccd1, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileBlue,', doBias=T,biasFile=',calib$biasFileBlue,', outname=',tlmFile,')', sep=''), file=logName, append=T)

                system(paste('rm -rf runZone', j, sep=''))
                system(paste('mkdir runZone', j, sep=''))
                
                
                
                PID<-Sys.getpid()
                
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                
                
                
                if (length(list.files(path=paste('data/reduced/',dateReduc,'/ccd1/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_tlm.fits',sep='')))==0){       
                    aaorunTLM(file=paste(toReduce[i], '/',flat_ccd1,sep=''), idx=idx, doDark=doDark,darkFile=calib$darkFileBlue, doBias=T, biasFile=calib$biasFileBlue, outname=tlmFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=paste('data/reduced/',dateReduc,'/ccd1/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_tlm.fits',sep='')))
                    }
                    
                }
                
                
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                
                
                
                arcFile<-paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_arc.fits', sep='')

                
                if (verbose>1){
                    cat('             - Reducing ARC file for blue ccd with line:', paste('aaorunARC(file=',toReduce[i], '/',flat_ccd1,' idx=idx, doDark=doDark,darkFile=',calib$darkFileBlue,', doBias=T,biasFile=',calib$biasFileBlue,', outname=',tlmFile,')', sep=''), '\n')
                }

                write(paste('             - Reducing ARC file for blue ccd with line: aaorunARC(file=',toReduce[i], '/',flat_ccd1,' idx=idx, doDark=doDark,darkFile=',calib$darkFileBlue,', doBias=T,biasFile=',calib$biasFileBlue,', outname=',tlmFile,')', sep=''), file=logName, append=T)
                
                
                if (length(list.files(path=paste('data/reduced/',dateReduc,'/ccd1/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_arc.fits',sep='')))==0){
                    
                    aaorunARC(file=paste(toReduce[i], '/',arc_ccd1,sep=''), idx=idx,  doDark=doDark,darkFile=calib$darkFileBlue,  doBias=T,biasFile=calib$biasFileBlue, tlmFile=tlmFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=toReduce[i], pattern=paste(strsplit(as.character(arc_ccd1),'.fits')[[1]][1],'red.fits', sep='')))
                    }
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(arc_ccd1),'.fits')[[1]][1],'red.fits ', arcFile ,sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(arc_ccd1),'.fits')[[1]][1],'im.fits data/reduced/',dateReduc,'/ccd1/', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(arc_ccd1),'.fits')[[1]][1],'ex.fits data/reduced/',dateReduc,'/ccd1/', sep=''))
                }
                
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                
                
                flatFile<-paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_flat.fits', sep='')

                if (verbose>1){
                    cat('             - Reducing Flat file for blue ccd with line:', paste('aaorunFLAT(file=',toReduce[i], '/',flat_ccd1, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileBlue,',doBias=T, biasFile=',calib$biasFileBlue,', arcFile=',arcFile,')', sep=''), '\n')
                }

                write(paste('             - Reducing Flat file for blue ccd with line: aaorunFLAT(file=',toReduce[i], '/',flat_ccd1, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileBlue,',doBias=T, biasFile=',calib$biasFileBlue,', arcFile=',arcFile,')', sep=''), file=logName, append=T)

                
                
                if (length(list.files(path=paste('data/reduced/',dateReduc,'/ccd1/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_flat.fits',sep='')))==0){
                    
                    aaorunFLAT(file=paste(toReduce[i], '/',flat_ccd1,sep=''), idx=idx,  doDark=doDark, darkFile=calib$darkFileBlue,  doBias=T,biasFile=calib$biasFileBlue, arcFile=arcFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=toReduce[i], pattern=paste(strsplit(as.character(flat_ccd1),'.fits')[[1]][1],'red.fits', sep='')))
                    }
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(flat_ccd1),'.fits')[[1]][1],'red.fits ', flatFile ,sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(flat_ccd1),'.fits')[[1]][1],'im.fits data/reduced/',dateReduc,'/ccd1/', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(flat_ccd1),'.fits')[[1]][1],'ex.fits data/reduced/',dateReduc,'/ccd1/', sep=''))
                }

                
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                


                                        # reduce target files ccd1

                if (verbose>1){cat('             - Reducing target data for blue ccd....', '\n')}
                write('             - Reducing target data for blue ccd....', file=logName, append=T)
                
                for (k in 1:length(targets_ccd1)){

                    if (verbose>1){cat('               - Reducing blue ccd target file: ',k, ' of ',length(targets_ccd1), '\n')}
                    write(paste('               - Reducing blue ccd target file: ',k, ' of ',length(targets_ccd1),sep=''), file=logName, append=T)

                    aaorunObj(file=paste(toReduce[i], '/',targets_ccd1[k],sep=''), idx=idx, doDark=doDark,darkFile=calib$darkFileBlue, doBias=T, biasFile=calib$biasFileBlue, flatFile=flatFile, tlmFile=tlmFile, arcFile=arcFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=toReduce[i], pattern=paste(strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'red.fits', sep='')))
                    }
                    
                    
                    if (doCosmic==T) {
                      if (verbose>0){cat('     - Running Cosmic rejection for Blue CCD....', '\n')}
                        fileBlue<-paste(toReduce[i], '/',targets_ccd1[k],'red.fits',sep='')
                        imBlue <- readFITS(file=fileBlue,hdu=1)
                        snBlue <- readFITS(file=fileBlue,hdu=2)
                        
                          #RO_GAIN<-as.numeric(imBlue$hdr[which(imBlue$hdr=="RO_GAIN")+1])
                          #RO_NOISE<-as.numeric(imBlue$hdr[which(imBlue$hdr=="RO_NOISE")+1])
                          RO_GAIN<-1.9
                        RO_NOISE<-1.8
                      CosSub<-RCosmic(imBlue$imDat, imBlue$hdr, snBlue$imDat, rdnoise=RO_NOISE, sigma_det=5, rlim=1.0, iter=6, fwhm_gauss=2.0, gain=RO_GAIN, verbose=FALSE)
                      CosMaskBlue<-array(1,dim=dim(imBlue$imDat))
                      CosMaskBlue[which(is.na(CosSub)==T & is.na(imBlue$imDat)==F, arr.ind = TRUE)]<-NA
                      imBlue$imDat<-CosSub
                      if (verbose>0){cat('     - Finished Cosmic rejection for Blue CCD.', '\n')}
                    }
                    
                    
                    
                    
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'red.fits data/reduced/',dateReduc,'/ccd1/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'red_config',j,'.fits', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'im.fits data/reduced/',dateReduc,'/ccd1/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'im_config',j,'.fits', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'ex.fits data/reduced/',dateReduc,'/ccd1/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'ex_config',j,'.fits', sep=''))
                    
                  
                    
                    
                    
                    if (host=="munro"){
                      system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                      system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                      Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                      system(paste('rm -rf /tmp/AAO',PID,sep=''))
                    }
                    if (host=="lukeLap"){
                      system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                      system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                      Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                      system(paste('rm -rf /tmp/AAO',PID,sep=''))
                    }
                    

                }
                
                
             

                if (verbose>1){cat('             - Combining target files....', '\n')}
                write('             - Combining target files....', file=logName, append=T)


                combineList<-paste('data/reduced/',dateReduc,'/ccd1/',list.files(path=paste('data/reduced/',dateReduc,'/ccd1/',sep=''), pattern=paste('*red_config',j,'.fits',sep='')), sep='', collapse=' ')

                cmd<-paste('aaorun combine_spectra "', combineList, '" -idxfile ',idx, ' -combinedfile data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_reduced_blue.fits', sep='')
                system(cmd)

                count<-0
                while(count<1){
                    count<-length(list.files(path=paste('data/reduced/',dateReduc,'/ccd1',sep=''), pattern=paste(dateReduc2,'_config_',j,'_reduced_blue.fits', sep='')))
                }
               
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                
             
                
###########################################



                

                                        # reduce calibrations ccd2
                idx<-'data/idxFiles/ozdes_red.idx'

                if (verbose>1){cat('             ** For red ccd using: ', idx, '\n')}
                write(paste('             ** For red ccd using: ', idx,sep=''), file=logName, append=T)
                

                if (verbose>1){
                    cat('             - Reducing TLM file for red ccd with line:', paste('aaorunTLM(file=',toReduce[i], '/',flat_ccd2, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,', doBias=T,biasFile=',calib$biasFileRed,', outname=',tlmFile,')', sep=''), '\n')
                }

                write(paste('             - Reducing TLM file for red ccd with line: aaorunTLM(file=',toReduce[i], '/',flat_ccd2, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,', doBias=T,biasFile=',calib$biasFileRed,', outname=',tlmFile,')', sep=''), file=logName, append=T)

                
                inFile<-paste(toReduce[i], '/',flat_ccd2,sep='')

                tlmFile<-paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_tlm.fits', sep='')
                
                if (length(list.files(path=paste('data/reduced/',dateReduc,'/ccd2/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_tlm.fits',sep='')))==0){       
                    aaorunTLM(file=paste(toReduce[i], '/',flat_ccd2,sep=''), idx=idx, doDark=doDark,darkFile=calib$darkFileRed, doBias=T,biasFile=calib$biasFileRed, outname=tlmFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=paste('data/reduced/',dateReduc,'/ccd2/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_tlm.fits',sep='')))
                    }
                    
                }

                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                

                if (verbose>1){
                    cat('             - Reducing ARC file for red ccd with line:', paste('aaorunARC(file=',toReduce[i], '/',flat_ccd2,' idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,', doBias=T,biasFile=',calib$biasFileRed,', tlmFile=',tlmFile,')', sep=''), '\n')
                }

                write(paste('             - Reducing ARC file for red ccd with line: aaorunARC(file=',toReduce[i], '/',flat_ccd2,' idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,', doBias=T,biasFile=',calib$biasFileRed,', tlmFile=',tlmFile,')', sep=''), file=logName, append=T)
                
                
                arcFile<-paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_arc.fits', sep='')
                
                if (length(list.files(path=paste('data/reduced/',dateReduc,'/ccd2/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_arc.fits',sep='')))==0){
                    
                    aaorunARC(file=paste(toReduce[i], '/',arc_ccd2,sep=''), idx=idx, doDark=doDark,darkFile=calib$darkFileRed, doBias=T,biasFile=calib$biasFileRed, tlmFile=tlmFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=toReduce[i], pattern=paste(strsplit(as.character(arc_ccd2),'.fits')[[1]][1],'red.fits', sep='')))
                    }
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(arc_ccd2),'.fits')[[1]][1],'red.fits ', arcFile ,sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(arc_ccd2),'.fits')[[1]][1],'im.fits data/reduced/',dateReduc,'/ccd2/', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(arc_ccd2),'.fits')[[1]][1],'ex.fits data/reduced/',dateReduc,'/ccd2/', sep=''))
                }
                
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                
                

                if (verbose>1){
                    cat('             - Reducing Flat file for red ccd with line:', paste('aaorunFLAT(file=',toReduce[i], '/',flat_ccd2, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,',doBias=T, biasFile=',calib$biasFileRed,', arcFile=',arcFile,')', sep=''), '\n')
                }

                write(paste('             - Reducing Flat file for red ccd with line: aaorunFLAT(file=',toReduce[i], '/',flat_ccd2, ' idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,',doBias=T, biasFile=',calib$biasFileRed,', arcFile=',arcFile,')', sep=''), file=logName, append=T)

                

                flatFile<-paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_flat.fits', sep='')
                
                if (length(list.files(path=paste('data/reduced/',dateReduc,'/ccd2/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_flat.fits',sep='')))==0){
                    
                    aaorunFLAT(file=paste(toReduce[i], '/',flat_ccd2,sep=''), idx=idx, doDark=doDark,darkFile=calib$darkFileRed,doBias=T, biasFile=calib$biasFileRed, arcFile=arcFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=toReduce[i], pattern=paste(strsplit(as.character(flat_ccd2),'.fits')[[1]][1],'red.fits', sep='')))
                    }
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(flat_ccd2),'.fits')[[1]][1],'red.fits ', flatFile ,sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(flat_ccd2),'.fits')[[1]][1],'im.fits data/reduced/',dateReduc,'/ccd2/', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(flat_ccd2),'.fits')[[1]][1],'ex.fits data/reduced/',dateReduc,'/ccd2/', sep=''))
                }

                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                

                                        # reduce target files ccd2

                if (verbose>1){cat('             - Reducing target data for red ccd', '\n')}
                write('             - Reducing target data for red ccd', file=logName, append=T)

                
                for (k in 1:length(targets_ccd2)){

                    if (verbose>1){cat('                 - Reducing red ccd target file: ',k, ' of ',length(targets_ccd2), '\n')}
                    write(paste('                 - Reducing red ccd target file: ',k, ' of ',length(targets_ccd2),sep=''), file=logName, append=T)

                    if (verbose>1){
                        cat('             - Using line:', paste('aaorunObj(file=',toReduce[i], '/',targets_ccd2[k],', idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,', doBias=T, biasFile=',calib$biasFileRed,', flatFile=',flatFile,', tlmFile=',tlmFile,', arcFile=',arcFile,')', sep=''), '\n')
                    }

                    write(paste('             - Using line: aaorunObj(file=',toReduce[i], '/',targets_ccd2[k],', idx=idx, doDark=doDark,darkFile=',calib$darkFileRed,', doBias=T, biasFile=',calib$biasFileRed,', flatFile=',flatFile,', tlmFile=',tlmFile,', arcFile=',arcFile,')', sep=''), file=logName, append=T)
                    

                    aaorunObj(file=paste(toReduce[i], '/',targets_ccd2[k],sep=''), idx=idx, doDark=doDark,darkFile=calib$darkFileRed, doBias=T, biasFile=calib$biasFileRed, flatFile=flatFile, tlmFile=tlmFile, arcFile=arcFile, runZone=j)
                    count<-0
                    while(count<1){
                        count<-length(list.files(path=toReduce[i], pattern=paste(strsplit(as.character(targets_ccd2[k]),'.fits')[[1]][1],'red.fits', sep='')))
                    }
                    
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(targets_ccd2[k]),'.fits')[[1]][1],'red.fits data/reduced/',dateReduc,'/ccd2/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'red_config',j,'.fits', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(targets_ccd2[k]),'.fits')[[1]][1],'im.fits data/reduced/',dateReduc,'/ccd2/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'im_config',j,'.fits', sep=''))
                    system(paste('mv ',toReduce[i], '/',strsplit(as.character(targets_ccd2[k]),'.fits')[[1]][1],'ex.fits data/reduced/',dateReduc,'/ccd2/',strsplit(as.character(targets_ccd1[k]),'.fits')[[1]][1],'ex_config',j,'.fits', sep=''))
                    
                    if (host=="munro"){
                      system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                      system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                      Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                      system(paste('rm -rf /tmp/AAO',PID,sep=''))
                    }
                    if (host=="lukeLap"){
                      system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                      system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                      Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                      system(paste('rm -rf /tmp/AAO',PID,sep=''))
                    }
                    

                }


                if (verbose>1){cat('             - Combining target files', '\n')}
                write('             - Combining target files', file=logName, append=T)

                
                combineList<-paste('data/reduced/',dateReduc,'/ccd2/',list.files(path=paste('data/reduced/',dateReduc,'/ccd2/',sep=''), pattern=paste('*red_config',j,'.fits',sep='')), sep='', collapse=' ')

                cmd<-paste('aaorun combine_spectra "', combineList, '" -idxfile ',idx, ' -combinedfile data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_reduced_red.fits', sep='')
                system(cmd)

                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                

                

                count<-0
                while(count<1){
                    count<-length(list.files(path=paste('data/reduced/',dateReduc,'/ccd2/',sep=''), pattern=paste(dateReduc2,'_config_',j,'_reduced_red.fits', sep='')))
                }


                

                if (verbose>1){cat('         - Splicing blue and red arms.....', '\n')}
                write('         -  Splicing blue and red arms.....', file=logName, append=T)


                spliceList<-paste(c(paste('data/reduced/',dateReduc,'/ccd1/',dateReduc2,'_config_',j,'_reduced_blue.fits', sep=''), paste('data/reduced/',dateReduc,'/ccd2/',dateReduc2,'_config_',j,'_reduced_red.fits', sep='')),sep='',collapse=' ')

                idx<-'data/idxFiles/gama_blue.idx'
                cmd<-paste('aaorun splice "', spliceList, '" -idxfile ',idx, ' -output_file data/reduced/',dateReduc,'/',dateReduc2,'_config_',j,'_reduced.fits', sep='')

                system(cmd)
                
                if (host=="munro"){
                  system(paste('rm -rf /home/ldavies/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /home/ldavies/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/home/ldavies/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                if (host=="lukeLap"){
                  system(paste('rm -rf /Users/luke/imp_scratch/runZone', j, sep=''))
                  system(paste('mkdir /Users/luke/imp_scratch/runZone', j, sep=''))
                  Sys.setenv(IMP_SCRATCH=paste('/Users/luke/imp_scratch/runZone',j,sep=''))
                  system(paste('rm -rf /tmp/AAO',PID,sep=''))
                }
                

                
                newReduce2<-paste('data/reduced/',dateReduc,'/',dateReduc2,'_config_',j,'_reduced.fits', sep='')
                return(newReduce2)
                
                system(paste('rm -rf runZone', j, sep=''))
                
            }
       }
       
       
       if (verbose>1){cat('             - Cleaning up files.....', '\n')}
       write('             -  Cleaning up files.....', file=logName, append=T)
       
       #cleanup
       
       tmp<-list.files(path=paste('data/reduced/',dateReduc,'/ccd1',sep=''), pattern='*.fits')
       tmpKeep<-tmp[which(substr(tmp,nchar(tmp)-8, nchar(tmp)-5)=='blue')]
       tmpDel<-tmp[which(substr(tmp,nchar(tmp)-8, nchar(tmp)-5)!='blue')]
       for (kk in 1:length(tmpDel)){                    
         system(paste('rm data/reduced/',dateReduc,'/ccd1/', tmpDel[kk],sep=''))
       }
       
       tmp<-list.files(path=paste('data/reduced/',dateReduc,'/ccd2',sep=''), pattern='*.fits')
       tmpKeep<-tmp[which(substr(tmp,nchar(tmp)-15, nchar(tmp)-5)=='reduced_red')]
       tmpDel<-tmp[which(substr(tmp,nchar(tmp)-15, nchar(tmp)-5)!='reduced_red')]
       for (kk in 1:length(tmpDel)){                    
         #system(paste('rm data/reduced/',dateReduc,'/ccd2/', tmpDel[kk],sep=''))
       }
       

        if  (is.null(dim(newReduce))==FALSE) {newReduce<-as.vector(newReduce[,1])}
        if  (is.null(dim(newReduce))==TRUE) {newReduce<-newReduce}

        metaData2<-getFileInfo(dir=paste('data/reduced/',dateReduc,'/',sep=''),saveFile=paste('data/reduced/',dateReduc,'/',dateReduc2,'_metaData.Rdata',sep=''), logName=logName, verbose=verbose)
        
        
    }

    if (verbose>1){
        cat('\n')
        cat('   ** run2dfDR complete.', '\n')
        cat('\n')
    }
    write('', file=logName, append=T)
    write('   ** run2dfDR complete.', file=logName, append=T)
    write('', file=logName, append=T)

    return(newReduce)


}


