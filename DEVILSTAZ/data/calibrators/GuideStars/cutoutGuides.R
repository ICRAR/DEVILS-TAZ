
location='lukeLap'
system('mkdir cutouts')
outDir<-'cutouts'
guides<-read.table('DATAguide.tab', header=T)
size<-15



if (location=='lukeLap'){
    PathD10<-'/Users/luke/work/DEVILS/Imaging/D10/UltraVISTA/'
    stubYD10<-'Y'
    stubJD10<-'J'
    stubKD10<-'Ks'
    PathD02<-'/Users/luke/work/DEVILS/Imaging/D02/'
    stubYD02<-'Y'
    stubJD02<-'Y'
    stubKD02<-'Y'
    PathD03<-'/Users/luke/work/DEVILS/Imaging/D03/'
    stubYD03<-'Y'
    stubJD03<-'Y'
    stubKD03<-'Y'
}


RA<-guides[,'RA']
DEC<-guides[,'DEC']
ID<-guides[,'ROWID']
Field<-rep('D10', length(RA))
Field[which(RA>30 & RA<49)]<-'D02'
Field[which(RA>50 & RA<70)]<-'D03'


  
#a = foreach(i=1:length(RA)) %dopar%  {
for (i in 1:length(RA)){


    
if (Field[i]=='D10'){

    imNameY<-paste(PathD10,'UVISTA_',stubYD10,'_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',sep='')
    imNameJ<-paste(PathD10,'UVISTA_',stubJD10,'_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',sep='')
    imNameK<-paste(PathD10,'UVISTA_',stubKD10,'_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',sep='')
    hdr<-read.fitshdr(imNameY)
}

if (Field[i]=='D02'){
    imNameY<-paste(PathD02,'xmm_',stubYD02,'_maxseeing0p90_2017-02-12.fits',sep='')
    imNameJ<-paste(PathD02,'xmm_',stubJD02,'_maxseeing0p90_2017-02-12.fits',sep='')
    imNameK<-paste(PathD02,'xmm_',stubKD02,'_maxseeing0p90_2017-02-12.fits',sep='')
    hdr<-read.fitshdr(imNameY)
}

if (Field[i]=='D03'){
    
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
    magimageWCSRGB(imK,imJ,imY, hdr,hdr,hdr,hdr, type='num', locut=0, hicut=35, stretchscale = 1.0)
    
    sourcePix<-radec2xy(RA[i], DEC[i], hdr)
    
 
    
    #draw.circle(xyPos[i,1]-xlo, xyPos[i,2]-ylo,fibSc/2, nv=100, border=col,col=rgb(colrgb[1],colrgb[2],colrgb[3],0.1),lty=1,density=NULL,angle=45,lwd=1)
    draw.circle(sourcePix[1],sourcePix[2],fibSc/2, nv=100, border='red',col=NA,lty=1,density=NULL,angle=45,lwd=1)
    
    legend('bottomright', legend=c(paste('ID = ', ID[i], sep='')))
    dev.off()
    
    
  }
  
