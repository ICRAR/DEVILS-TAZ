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
#' @param location either 'munro' or 'lukeLap'. This only runs on munro or luke's laptop. 

#' @examples 
#' configFile='data/observing/run1_2017_12/2017_12_18/DOCats/Tiling/D03/TargetFork1-1P0/D03_Y2017_SB_R1tile001-53--28P0.lis'
#' cutoutConfig(configFile=configFile, size=15, outDir='D03_Y2017_SB_R1tile001-53--28P0_cutouts', cores=4)
#' @export
cutoutConfig<-function(configFile=configFile, size=size, outDir=outDir, cores=cores, location='munro'){
  
  if (location=='munro'){
    PathD10<-'/mnt/jaws/DEVILS/imaging/D10/UltraVISTA_DR3/'
    stubYD10<-'Y'
    stubJD10<-'J'
    stubKD10<-'Ks'
    PathD02<-'/mnt/jaws/DEVILS/imaging/D02/VIDEO2017/'
    stubYD02<-'Y'
    stubJD02<-'J'
    stubKD02<-'Ks'
    PathD03<-'/mnt/jaws/DEVILS/imaging/D03/VIDEO2017/'
    stubYD03<-'Y'
    stubJD03<-'J'
    stubKD03<-'Ks'
  }
  if (location=='lukeLap'){
    PathD10<-'/Users/luke/work/DEVILS/Imaging/D10/UltraVISTA/'
    stubYD10<-'Y'
    stubJD10<-'J'
    stubKD10<-'Ks'
    PathD02<-'/Users/luke/work/DEVILS/Imaging/D02/'
    stubYD02<-'Y'
    stubJD02<-'Y'
    stubKD02<-'Y'
    PathD03<-'/Users/luke/work/DEVILS/Imaging/D02/'
    stubYD03<-'Y'
    stubJD03<-'Y'
    stubKD03<-'Y'
  }
  
  system(paste('mkdir ',outDir, sep=''))
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
      DEC<-c(DEC,dms2deg(as.numeric(tmp[7]), as.numeric(tmp[8]), as.numeric(tmp[9])))
      TYPE<-c(TYPE,tmp[16])
    }
    
  }
  

  
  if (Field=='D10'){

    imNameY<-paste(PathD10,'UVISTA_',stubYD10,'_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',sep='')
    imNameJ<-paste(PathD10,'UVISTA_',stubJD10,'_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',sep='')
    imNameK<-paste(PathD10,'UVISTA_',stubKD10,'_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',sep='')
    hdr<-read.fitshdr(imNameY)
  }
  
  if (Field=='D02'){
    imNameY<-paste(PathD02,'xmm_',stubYD02,'_maxseeing0p90_2017-02-12.fits',sep='')
    imNameJ<-paste(PathD02,'xmm_',stubJD02,'_maxseeing0p90_2017-02-12.fits',sep='')
    imNameK<-paste(PathD02,'xmm_',stubKD02,'_maxseeing0p90_2017-02-12.fits',sep='')
    hdr<-read.fitshdr(imNameY)
  }
  
  if (Field=='D03'){
    
    imNameY<-paste(PathD03,'cdfs_',stubYD03,'_maxseeing0p90_2017-02-12.fits',sep='')
    imNameJ<-paste(PathD03,'cdfs_',stubJD03,'_maxseeing0p90_2017-02-12.fits',sep='')
    imNameK<-paste(PathD03,'cdfs_',stubKD03,'_maxseeing0p90_2017-02-12.fits',sep='')
    hdr<-read.fitshdr(imNameY)
  }
  
  
  
  
  
  PixSc<-as.numeric(get.fitskey('CD2_2', hdr))
  XSz<-as.numeric(get.fitskey('NAXIS1', hdr))
  YSz<-as.numeric(get.fitskey('NAXIS2', hdr))
  
  xyPos<-radec2xy(RA, DEC, hdr)
  xySc<-size/(PixSc*60*60)
  fibSc<-2/(PixSc*60*60)
  
  
  a = foreach(i=1:length(RA)) %dopar%  {
  #a = foreach(i=1:10) %dopar%  {
    
    xlo<-round(xyPos[i,1]-xySc)
    xhi<-round(xyPos[i,1]+xySc)
    ylo<-round(xyPos[i,2]-xySc)
    yhi<-round(xyPos[i,2]+xySc)
    
    
    
    imY<-read.fits(imNameY,xlo=xlo, ylo=ylo,xhi=xhi, yhi=yhi, hdu = 0)
    hdr<-imY$hdr[[1]]
    imY<-imY$dat[[1]]
    imJ<-read.fits(imNameJ,xlo=xlo, ylo=ylo,xhi=xhi, yhi=yhi,hdu = 0)
    imJ<-imJ$dat[[1]]
    imK<-read.fits(imNameK,xlo=xlo, ylo=ylo,xhi=xhi, yhi=yhi,hdu = 0)
    imK<-imK$dat[[1]]
    
    CairoPNG(paste(outDir,'/',ID[i],'.png', sep=''))
    magimageWCSRGB(imK,imJ,imY, hdr,hdr,hdr,hdr, type='num', locut=0, hicut=35, stretchscale = 1.0, main=paste(ID[i], '-', TYPE[i], sep=''))
    
    sourcePix<-radec2xy(RA[i], DEC[i], hdr)
    
    if (substr(TYPE[i],1,1)=='D'){
      col='green'
      colrgb<-col2rgb(col)/255
      }
    if (substr(TYPE[i],1,2)=='st'){
      col='orange'
      colrgb<-col2rgb(col)/255
      }
    if (substr(TYPE[i],1,2)=='sk'){
      col='cyan'
      colrgb<-col2rgb(col)/255
      }
    if (substr(TYPE[i],1,2)=='gu'){
      col='indianred2'
      colrgb<-col2rgb(col)/255
      }
    
   
    
    #draw.circle(xyPos[i,1]-xlo, xyPos[i,2]-ylo,fibSc/2, nv=100, border=col,col=rgb(colrgb[1],colrgb[2],colrgb[3],0.1),lty=1,density=NULL,angle=45,lwd=1)
    draw.circle(sourcePix[1],sourcePix[2],fibSc/2, nv=100, border=col,col=NA,lty=1,density=NULL,angle=45,lwd=1)
    
    legend('bottomright', legend=c(paste('ID = ', ID[i], sep=''), paste('FIBRE = ', FIBRE[i], sep=''), paste('TYPE = ', TYPE[i], sep='')))
    dev.off()
    
    return(NULL)
  }
  
}





