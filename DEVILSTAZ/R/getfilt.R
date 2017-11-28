getfilt=function(filter){
  out=NA
  if(filter=='GALEX_FUV'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_FUV_GALEX.tab', sep=''))     
  }
  if(filter=='GALEX_NUV'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_NUV_GALEX.tab', sep=''))     
  }
  if(filter=='sdss_u'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_u_SDSS.tab', sep=''))     
  }
 if(filter=='sdss_g'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_g_SDSS.tab', sep=''))     
 }
   if(filter=='sdss_r'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_r_SDSS.tab', sep=''))     
   }
   if(filter=='sdss_i'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_i_SDSS.tab', sep=''))     
   }
   if(filter=='sdss_z'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_z_SDSS.tab', sep=''))     
   }

  
  if(filter=='des_u'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_u_DES.tab', sep=''))     
  }
 if(filter=='des_g'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_g_DES.tab', sep=''))     
 }
   if(filter=='des_r'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_r_DES.tab', sep=''))     
   }
   if(filter=='des_i'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_i_DES.tab', sep=''))     
   }
   if(filter=='des_z'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_z_DES.tab', sep=''))     
   }

     if(filter=='des_Y'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_Y_DES.tab', sep=''))     
   }


  
   if(filter=='VISTA_Z'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_Z_VISTA.tab', sep=''))     
   }
   if(filter=='VISTA_Y'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_Z_VISTA.tab', sep=''))     
  }

   if(filter=='VISTA_J'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_Z_VISTA.tab', sep=''))     
  }

   if(filter=='VISTA_H'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_Z_VISTA.tab', sep=''))     
  }

   if(filter=='VISTA_K' | filter=='VISTA_Ks'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_Z_VISTA.tab', sep=''))     
   }
   if(filter=='WISE_W1'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_W1_WISE.tab', sep=''))     
   }
   if(filter=='WISE_W2'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_W2_WISE.tab', sep=''))     
   }
   if(filter=='WISE_W3'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_W3_WISE.tab', sep=''))     
   }
   if(filter=='WISE_W4'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_W4_WISE.tab', sep=''))     
  }
  
  if(filter==100 | filter=='Herschel_100'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_P100_Herschel.tab', sep=''))     
  }
   if(filter==160 | filter=='Herschel_160'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_P160_Herschel.tab', sep=''))     
   }
   if(filter==250 | filter=='Herschel_250'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_S250_Herschel.tab', sep=''))     
   }
   if(filter==350 | filter=='Herschel_350'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_S350_Herschel.tab', sep=''))     
   }
   if(filter==500 | filter=='Herschel_500'){
      out<-read.table(paste(.libPaths(),'/DEVILSTAZ/data/calibrators/filters/filt_PS450_Herschel.tab', sep=''))     
  }
  
 
  colnames(out)<-c('index','Wavelength', 'Trans')
  return=out
}
