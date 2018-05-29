#' Cosmic Ray Rejection Functions, rewritten from PyCosmic (Copyright 2012 Bernd Husemann)
#'
#' @description Cosmic ray rejection function. Detects and removes cosmics from astronomical 
#' images based on Laplacian edge detection scheme combined with a PSF convolution approach (Husemann  et al. in prep.).
#'  
#' @param image image to find cosmics
#' @examples 
#' 
#' @export
RCosmic<-function(image, rdnoise=1.8, sigma_det=5, rlim=1.2, iter=5, fwhm_gauss=2.0, gain=1.0, verbose=0){
  

  sigma = fwhm_gauss/2.354
  
  BaseIm<-image
  
  image = image*gain

  #rdnoise <- as.numeric(hdr[which(hdr=="RO_NOISE")+1])
  LA_kernel<-array(c(0,-1,0,-1,4,-1,0,-1,0), dim=c(3,3))/4
  
  
  if (verbose>1){cat('         - Starting RCosmic iterations....', '\n')}
  
  for (i in 1:iter){
    
    if (verbose>1){cat(paste('               - ',i,' of ',iter,'....',sep=''), '\n')}
    tmpIm<-image
    tmpIm[which(is.na(tmpIm)==T, arr.ind=TRUE)]<-0
    noise<-medianFilter(tmpIm, 2)
    select_neg2 <- which(noise<=0, arr.ind = TRUE)
    noise<-sqrt(noise+rdnoise^2)

    
    subIm<-EBImage::resize(image, w=dim(image)[1]*2, h=dim(image)[2]*2)
    


    convIm<- as.matrix(imager::convolve(as.cimg(subIm), as.cimg(LA_kernel)))
    convError<- as.matrix(imager::convolve(as.cimg(subIm), as.cimg(LA_kernel)))

    
    select_neg <- which(convIm<0, arr.ind = TRUE)
    convIm[select_neg]<-0
    convError[select_neg]<-0

    
    
    LapIm<-EBImage::resize(convIm, w=dim(convIm)[1]/2, h=dim(convIm)[2]/2)
    LapError<-EBImage::resize(convError, w=dim(convIm)[1]/2, h=dim(convIm)[2]/2)

    
    S<-LapIm/(noise*2)
    tmpIm<-S
    tmpIm[which(is.na(tmpIm)==T, arr.ind=TRUE)]<-0
    S_prime <- S-medianFilter(tmpIm,2)
    
    
    S_prime[which(is.nan(S_prime)==T, arr.ind=TRUE)]<-0
    S_prime[which(is.na(S_prime)==T, arr.ind=TRUE)]<-0
    fine<-as.matrix(gblur(S_prime, sigma))
    
    fine_norm = image/fine
    select_neg <- which(fine_norm<0, arr.ind = TRUE)
    fine_norm[select_neg]<-0
    
    subNormIm<-EBImage::resize(fine_norm, w=dim(fine_norm)[1]*2, h=dim(fine_norm)[2]*2)
    Lap2<-as.matrix(imager::convolve(as.cimg(subNormIm), as.cimg(LA_kernel)))
    Lap2<-EBImage::resize(Lap2, w=dim(Lap2)[1]/2, h=dim(Lap2)[2]/2)
    
    select<-which(Lap2>rlim & S_prime>sigma_det,arr.ind = TRUE)
  
   
   if (dim(select)[1]>0){
    
      for (j in 1:dim(select)[1]){
        tmp<-select[j,]
        lowX<-tmp[1]-2
        highX<-tmp[1]+2
        lowY<-tmp[2]-2
        highY<-tmp[2]+2
        if(lowX<=0){lowX<-1}
        if(highX>dim(Lap2)[1]){highX<-dim(Lap2)[1]}
        if(lowY<=0){lowY<-1}
        if(highY>dim(Lap2)[2]){highY<-dim(Lap2)[2]}
        
        
        image[lowX:highX,lowY:highY]<-NA
        BaseIm[lowX:highX,lowY:highY]<-NA
        
      }
   }
    
    if (dim(select)[1]==0){
      return(BaseIm)
    }
    
    
  }
 
  if (verbose>1){cat('         - Finished RCosmic iterations, solution converged....', '\n')}
    
  return(BaseIm)
  
}



