#' Produces cutout YJK RGB images for all sources in a fibre configuration 
#'
#' @description This is function takes a specified DEVILS fibre configuration
#' file (.lis) and produces YJK cutout images of all sources in that file. 
#' This also notes which fibres are placed on targerts, standards, sky, or
#' guidestars. NOTE: This must be run on Munro!
#'  
#' @param configFile Path to configuration file to read
#' @param size Size of the desired cutouts in Arcsec
#' @param outDir Path for directory to write files to
#' @param cores Number of cores to run over

#' @examples 

#' @export
cutoutConfig<-function(configFile=configFile, size=size, outDir=outDir, cores=cores){
  
  registerDoParallel(cores=cores)
  
  Config<-readLines(configFile)
  Field<-substr(Config[1],7,9)
  Config<-Config[10:409]
  FIBRE<-c()
  ID<-c()
  RA<-c()
  DEC<-c()
  TYPE<-c()
  
  for (i in 1:length(Config)){
    tmp<-strsplit(Config[i], ' ')[[1]]
    tmp<-tmp[which(tmp!="")]
      if (length(tmp)==16){
        FIBRE<-c(FIBRE,as.numeric(tmp[2]))
        ID<-c(ID,tmp[3])
        RA<-c(RA,hms2deg(as.numeric(tmp[4]), as.numeric(tmp[5]), as.numeric(tmp[6])))
        DEC<-c(DEC,dms2deg(as.numeric(tmp[7]), as.numeric(tmp[8]), as.numeric(tmp[8])))
        TYPE<-c(TYPE,tmp[16])
      }
   
  }
  
  if (Field=='D10'){
    
    hdr<-read.fitshdr('/mnt/jaws/DEVILS/imaging/D10/UltraVISTA_DR3/UVISTA_Y_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits')
    a = foreach(i=1:length(D10Stars)) %dopar%  {
      
    }
    
  }
  
  
  
  
  
  
}