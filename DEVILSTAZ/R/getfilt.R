getfilt=function(filter){
  out=NA
  if(filter=='GALEX_FUV'){
      out<-read.table('data/calibrators/filters/filt_FUV_GALEX.tab')     
  }
  if(filter=='GALEX_NUV'){
      out<-read.table('data/calibrators/filters/filt_NUV_GALEX.tab')     
  }
  if(filter=='sdss_u'){
      out<-read.table('data/calibrators/filters/filt_u_sdss.tab')     
  }
 if(filter=='sdss_g'){
      out<-read.table('data/calibrators/filters/filt_g_sdss.tab')     
 }
   if(filter=='sdss_r'){
      out<-read.table('data/calibrators/filters/filt_r_sdss.tab')     
   }
   if(filter=='sdss_i'){
      out<-read.table('data/calibrators/filters/filt_i_sdss.tab')     
   }
   if(filter=='sdss_z'){
      out<-read.table('data/calibrators/filters/filt_z_sdss.tab')     
   }
   if(filter=='VISTA_Z'){
      out<-read.table('data/calibrators/filters/filt_Z_VISTA.tab')     
   }
   if(filter=='VISTA_Y'){
      out<-read.table('data/calibrators/filters/filt_Z_VISTA.tab')     
  }

   if(filter=='VISTA_J'){
      out<-read.table('data/calibrators/filters/filt_Z_VISTA.tab')     
  }

   if(filter=='VISTA_H'){
      out<-read.table('data/calibrators/filters/filt_Z_VISTA.tab')     
  }

   if(filter=='VISTA_K' | filter=='VISTA_Ks'){
      out<-read.table('data/calibrators/filters/filt_Z_VISTA.tab')     
   }
   if(filter=='WISE_W1'){
      out<-read.table('data/calibrators/filters/filt_W1_WISE.tab')     
   }
   if(filter=='WISE_W2'){
      out<-read.table('data/calibrators/filters/filt_W2_WISE.tab')     
   }
   if(filter=='WISE_W3'){
      out<-read.table('data/calibrators/filters/filt_W3_WISE.tab')     
   }
   if(filter=='WISE_W4'){
      out<-read.table('data/calibrators/filters/filt_W4_WISE.tab')     
  }
  
  if(filter==100 | filter=='Herschel_100'){
      out<-read.table('data/calibrators/filters/filt_P100_Herschel.tab')     
  }
   if(filter==160 | filter=='Herschel_160'){
      out<-read.table('data/calibrators/filters/filt_P160_Herschel.tab')     
   }
   if(filter==250 | filter=='Herschel_250'){
      out<-read.table('data/calibrators/filters/filt_S250_Herschel.tab')     
   }
   if(filter==350 | filter=='Herschel_350'){
      out<-read.table('data/calibrators/filters/filt_S350_Herschel.tab')     
   }
   if(filter==500 | filter=='Herschel_500'){
      out<-read.table('data/calibrators/filters/filt_PS450_Herschel.tab')     
  }
  
 
  colnames(out)<-c('index','Wavelength', 'Trans')
  return=out
}
