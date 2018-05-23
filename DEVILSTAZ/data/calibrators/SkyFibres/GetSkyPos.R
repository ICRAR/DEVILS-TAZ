
D10_RA<-c(149.38,150.7)
D10_DEC<-c(1.65,2.79)
D02_RA<-c(34.0,37.05)
D02_DEC<-c(-5.2,-4.2)
D03_RA<-c(52.3,54.0)
D03_DEC<-c(-28.5,-27.5)


D10Dir<-'ProFound_D10_stack_segments/'
D10Files<-list.files(path=D10Dir, pattern='SubRegSegments_Total.rda')
D10Files<-D10Files[which(substr(D10Files, 1,5)=='Stack')]
load('D10_TotalPhotometry.rda')
D10CatTotal<-TotalPhotoCat
D10CatTotal<-D10CatTotal[which(D10CatTotal$Y_mag+0.0474 <21.5),]
D10CatRA<-D10CatTotal$RAmax
D10CatDEC<-D10CatTotal$Decmax
D10CatRABright<-D10CatTotal$RAmax[which(D10CatTotal$Y_mag+0.0474 <15.0)]
D10CatDECBright<-D10CatTotal$Decmax[which(D10CatTotal$Y_mag+0.0474 <15.0)]


skyRA<-c()
skyDEC<-c()




for (i in 1:length(D10Files)){


    

        load(paste(D10Dir,D10Files[i],sep=''))
        load(paste(D10Dir, substr(D10Files[i], 1,nchar(D10Files[i])-18),'.rda',sep=''))
        tmp<-Im_cut$dat[[1]]/Im_cut$dat[[1]]
        ProOut$objects_redo<-ProOut$objects_redo/tmp

    cat('Running D10 File ',i, ' of ',length(D10Files),' -',D10Files[i],' .....', '\n')
    tmp<-ProOut$objects_redo

    D10Lo<-radec2xy(D10_RA[2], D10_DEC[1], header=ProOut$header)
    D10Hi<-radec2xy(D10_RA[1], D10_DEC[2], header=ProOut$header)

    if (D10Lo[1]>1 & D10Lo[1]<dim(ProOut$objects_redo)[1]){
        tmp[(1:D10Lo[1]),]<-1
    }
    if (D10Hi[1]>1 & D10Hi[1]<dim(ProOut$objects_redo)[1]){
        tmp[(D10Hi[1]:dim(ProOut$objects_redo)[1]),]<-1
    }
    
     if (D10Lo[2]>1 & D10Lo[2]<dim(ProOut$objects_redo)[2]){
        tmp[,(1:D10Lo[2])]<-1
    }
    if (D10Hi[2]>1 & D10Hi[2]<dim(ProOut$objects_redo)[2]){
        tmp[,(D10Hi[2]:dim(ProOut$objects_redo)[2])]<-1       
    }
    
    ProOut$objects_redo<-tmp
    
        tmp2<-tmp
        tmp2[(2:(dim(tmp)[1]-1)),(2:(dim(tmp)[2]-1))]<-(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(3:(dim(tmp)[1])),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(1:(dim(tmp)[1]-2)),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(3:(dim(tmp)[2]))])+(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(1:(dim(tmp)[2]-2))])
        selSky<-which(tmp2==0, arr.ind=T)

        SelPos<-xy2radec(selSky[,1],selSky[,2], header=ProOut$header)
        LimLo<-xy2radec(1,1,header=ProOut$header)
        LimHi<-xy2radec(dim(ProOut$objects_redo)[1],dim(ProOut$objects_redo)[2],header=ProOut$header)
        D10CatRASel<-D10CatRA[which(D10CatRA>LimHi[1] & D10CatRA<LimLo[1] & D10CatDEC>LimLo[2] & D10CatDEC<LimHi[2])]
        D10CatDECSel<-D10CatDEC[which(D10CatRA>LimHi[1] & D10CatRA<LimLo[1] & D10CatDEC>LimLo[2] & D10CatDEC<LimHi[2])]

        D10CatRABrightSel<-D10CatRABright[which(D10CatRABright>LimHi[1] & D10CatRABright<LimLo[1] & D10CatDECBright>LimLo[2] & D10CatDECBright<LimHi[2])]
        D10CatDECBrightSel<-D10CatDECBright[which(D10CatRABright>LimHi[1] & D10CatRABright<LimLo[1] & D10CatDECBright>LimLo[2] & D10CatDECBright<LimHi[2])]

        tempmatch=coordmatch(SelPos, cbind(D10CatRASel, D10CatDECSel),rad = 20)
        sel<-which(tempmatch$Nmatch==0)

        selSky<-selSky[sel,]

        if (length(D10CatRABrightSel)>0){
            SelPos<-xy2radec(selSky[,1],selSky[,2], header=ProOut$header)
            tempmatch=coordmatch(SelPos, cbind(D10CatRABrightSel, D10CatDECBrightSel),rad = 90)
            sel<-which(tempmatch$Nmatch==0)
            

            selSky<-selSky[sel,]
        }
        

        RandSel<-round(runif(10, 1,dim(selSky)[1]))
        SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)

        good<-0
        while (good<10){
            SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)
            
            for (j in 1:10){
                dist<-sqrt((SelPos[j,1]-SelPos[,1])^2+(SelPos[j,2]-SelPos[,2])^2)
                if (length(which(dist<(1/60)))>1){
                    SelPos[j,]<-c(NA,NA)
                }                
            }
            good<-length(which(is.finite(SelPos[,1])==T))
            RandSel[which(is.finite(SelPos[,1])==F)]<-runif(length(which(is.finite(SelPos[,1])==F)), 1,dim(selSky)[1])
        }
        SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)
        
        skyRA<-c(skyRA,SelPos[,1])
        skyDEC<-c(skyDEC,SelPos[,2])
    }

D10hdr<-read.fitshdr('/mnt/jaws/DEVILS/imaging/D10/UltraVISTA_DR3/UVISTA_Y_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits')

D10xyPosSky<-radec2xy(skyRA, skyDEC, D10hdr)
D10RadPixSky<-150

cores<-8
registerDoParallel(cores=cores)
system('mkdir CutoutsSky')

cat('Making D10 cutouts .....', '\n')

a = foreach(i=1:length(skyRA)) %dopar%  {


    xlo<-round(D10xyPosSky[i,1]-D10RadPixSky)
    xhi<-round(D10xyPosSky[i,1]+D10RadPixSky)
    ylo<-round(D10xyPosSky[i,2]-D10RadPixSky)
    yhi<-round(D10xyPosSky[i,2]+D10RadPixSky)
    

    imY<-read.fits('/mnt/jaws/DEVILS/imaging/D10/UltraVISTA_DR3/UVISTA_Y_21_01_16_allpaw_skysub_015_dr3_rc_v5.fits',xlo=xlo, ylo=ylo,xhi=xhi, yhi=yhi, hdu = 0)
    hdr<-imY$hdr[[1]]
    imY<-imY$dat[[1]]

    CairoPNG(paste('CutoutsSky/D10Sky_',i,'.png', sep=''))
    magimageWCS(imY, header=hdr)
    dev.off()
    


}

skyRA_D10<-skyRA
skyDEC_D10<-skyDEC


skyRA<-c()
skyDEC<-c()


D02Dir<-'ProFound_D02_stack_segments/'
D02Files<-list.files(path=D02Dir, pattern='SubRegSegments_Total.rda')
D02Files<-D02Files[which(substr(D02Files, 1,5)=='Stack')]
load('D02_TotalPhotometry.rda')
D02CatTotal<-TotalPhotoCat
D02CatTotal<-D02CatTotal[which(D02CatTotal$Y_mag+0.0943 <21.5),]
D02CatRA<-D02CatTotal$RAmax
D02CatDEC<-D02CatTotal$Decmax
D02CatRABright<-D02CatTotal$RAmax[which(D02CatTotal$Y_mag+0.0943 <15.0)]
D02CatDECBright<-D02CatTotal$RAmax[which(D02CatTotal$Y_mag+0.0943 <15.0)]



for (i in 1:length(D02Files)){

    
    cat('Running D02 File ',i, ' of ',length(D02Files),' -',D02Files[i],' .....', '\n')
    
    load(paste(D02Dir,D02Files[i],sep=''))
    load(paste(D02Dir, substr(D02Files[i], 1,nchar(D02Files[i])-18),'.rda',sep=''))
    tmp<-Im_cut$dat[[1]]/Im_cut$dat[[1]]
    ProOut$objects_redo<-ProOut$objects_redo/tmp


    
    tmp<-ProOut$objects_redo

    D02Lo<-radec2xy(D02_RA[2], D02_DEC[1], header=ProOut$header)
    D02Hi<-radec2xy(D02_RA[1], D02_DEC[2], header=ProOut$header)

    if (D02Lo[1]>1 & D02Lo[1]<dim(ProOut$objects_redo)[1]){
        tmp[(1:D02Lo[1]),]<-1
    }
    if (D02Hi[1]>1 & D02Hi[1]<dim(ProOut$objects_redo)[1]){
        tmp[(D02Hi[1]:dim(ProOut$objects_redo)[1]),]<-1
    }
    
    if (D02Lo[2]>1 & D02Lo[2]<dim(ProOut$objects_redo)[2]){
        tmp[,(1:D02Lo[2])]<-1
    }
    if (D02Hi[2]>1 & D02Hi[2]<dim(ProOut$objects_redo)[2]){
        tmp[,(D02Hi[2]:dim(ProOut$objects_redo)[2])]<-1       
    }
    
    ProOut$objects_redo<-tmp

    
    tmp2<-tmp
    tmp2[(2:(dim(tmp)[1]-1)),(2:(dim(tmp)[2]-1))]<-(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(3:(dim(tmp)[1])),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(1:(dim(tmp)[1]-2)),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(3:(dim(tmp)[2]))])+(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(1:(dim(tmp)[2]-2))])
    selSky<-which(tmp2==0, arr.ind=T)

    SelPos<-xy2radec(selSky[,1],selSky[,2], header=ProOut$header)
    LimLo<-xy2radec(1,1,header=ProOut$header)
    LimHi<-xy2radec(dim(ProOut$objects_redo)[1],dim(ProOut$objects_redo)[2],header=ProOut$header)
    D02CatRASel<-D02CatRA[which(D02CatRA>LimHi[1] & D02CatRA<LimLo[1] & D02CatDEC>LimLo[2] & D02CatDEC<LimHi[2])]
    D02CatDECSel<-D02CatDEC[which(D02CatRA>LimHi[1] & D02CatRA<LimLo[1] & D02CatDEC>LimLo[2] & D02CatDEC<LimHi[2])]

    D02CatRABrightSel<-D02CatRABright[which(D02CatRABright>LimHi[1] & D02CatRABright<LimLo[1] & D02CatDECBright>LimLo[2] & D02CatDECBright<LimHi[2])]
    D02CatDECBrightSel<-D02CatDECBright[which(D02CatRABright>LimHi[1] & D02CatRABright<LimLo[1] & D02CatDECBright>LimLo[2] & D02CatDECBright<LimHi[2])]

    tempmatch=coordmatch(SelPos, cbind(D02CatRASel, D02CatDECSel),rad = 20)
    sel<-which(tempmatch$Nmatch==0)

    selSky<-selSky[sel,]

         if (length(D02CatRABrightSel)>0){
             SelPos<-xy2radec(selSky[,1],selSky[,2], header=ProOut$header)
             tempmatch=coordmatch(SelPos, cbind(D02CatRABrightSel, D02CatDECBrightSel),rad = 90)
             sel<-which(tempmatch$Nmatch==0)
             selSky<-selSky[sel,]
         }
    

    RandSel<-round(runif(10, 1,dim(selSky)[1]))
    SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)

    good<-0
    while (good<10){
        SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)
        
        for (j in 1:10){
            dist<-sqrt((SelPos[j,1]-SelPos[,1])^2+(SelPos[j,2]-SelPos[,2])^2)
            if (length(which(dist<(1/60)))>1){
                SelPos[j,]<-c(NA,NA)
            }                
        }
        good<-length(which(is.finite(SelPos[,1])==T))
        RandSel[which(is.finite(SelPos[,1])==F)]<-runif(length(which(is.finite(SelPos[,1])==F)), 1,dim(selSky)[1])
    }
    SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)
    
    skyRA<-c(skyRA,SelPos[,1])
    skyDEC<-c(skyDEC,SelPos[,2])
}

D02hdr<-read.fitshdr('/mnt/jaws/DEVILS/imaging/D02/VIDEO2017/xmm_Y_maxseeing0p90_2017-02-12.fits')

D02xyPosSky<-radec2xy(skyRA, skyDEC, D02hdr)
D02RadPixSky<-150

cores<-8
registerDoParallel(cores=cores)

cat('Making D02 cutouts .....', '\n')

a = foreach(i=1:length(skyRA)) %dopar%  {


    xlo<-round(D02xyPosSky[i,1]-D02RadPixSky)
    xhi<-round(D02xyPosSky[i,1]+D02RadPixSky)
    ylo<-round(D02xyPosSky[i,2]-D02RadPixSky)
    yhi<-round(D02xyPosSky[i,2]+D02RadPixSky)
    

    imY<-read.fits('/mnt/jaws/DEVILS/imaging/D02/VIDEO2017/xmm_Y_maxseeing0p90_2017-02-12.fits',xlo=xlo, ylo=ylo,xhi=xhi, yhi=yhi, hdu = 0)
    hdr<-imY$hdr[[1]]
    imY<-imY$dat[[1]]

    CairoPNG(paste('CutoutsSky/D02Sky_',i,'.png', sep=''))
    magimageWCS(imY, header=hdr)
    dev.off()
    


}

skyRA_D02<-skyRA
skyDEC_D02<-skyDEC


D03Dir<-'ProFound_D03_stack_segments/'
D03Files<-list.files(path=D03Dir, pattern='SubRegSegments_Total.rda')
D03Files<-D03Files[which(substr(D03Files, 1,5)=='Stack')]
load('D03_TotalPhotometry.rda')
D03CatTotal<-TotalPhotoCat
D03CatTotal<-D03CatTotal[which(D03CatTotal$Y_mag+0.1 <21.5),]
D03CatRA<-D03CatTotal$RAmax
D03CatDEC<-D03CatTotal$Decmax
D03CatRABright<-D03CatTotal$RAmax[which(D03CatTotal$Y_mag+0.1 <15.0)]
D03CatDECBright<-D03CatTotal$RAmax[which(D03CatTotal$Y_mag+0.1 <15.0)]



skyRA<-c()
skyDEC<-c()

for (i in 1:length(D03Files)){

    cat('Running D03 File ',i, ' of ',length(D03Files),' -',D03Files[i],' .....', '\n')

    load(paste(D03Dir,D03Files[i],sep=''))

    load(paste(D03Dir, substr(D03Files[i], 1,nchar(D03Files[i])-18),'.rda',sep=''))
    tmp<-Im_cut$dat[[1]]/Im_cut$dat[[1]]
    ProOut$objects_redo<-ProOut$objects_redo/tmp

        tmp<-ProOut$objects_redo
    D03Lo<-radec2xy(D03_RA[2], D03_DEC[1], header=ProOut$header)
    D03Hi<-radec2xy(D03_RA[1], D03_DEC[2], header=ProOut$header)

    if (D03Lo[1]>1 & D03Lo[1]<dim(ProOut$objects_redo)[1]){
        tmp[(1:D03Lo[1]),]<-1
    }
    if (D03Hi[1]>1 & D03Hi[1]<dim(ProOut$objects_redo)[1]){
        tmp[(D03Hi[1]:dim(ProOut$objects_redo)[1]),]<-1
    }
    
     if (D03Lo[2]>1 & D03Lo[2]<dim(ProOut$objects_redo)[2]){
        tmp[,(1:D03Lo[2])]<-1
    }
    if (D03Hi[2]>1 & D03Hi[2]<dim(ProOut$objects_redo)[2]){
        tmp[,(D03Hi[2]:dim(ProOut$objects_redo)[2])]<-1       
    }
    
    ProOut$objects_redo<-tmp

    
    tmp2<-tmp
    tmp2[(2:(dim(tmp)[1]-1)),(2:(dim(tmp)[2]-1))]<-(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(3:(dim(tmp)[1])),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(1:(dim(tmp)[1]-2)),(2:(dim(tmp)[2]-1))])+(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(3:(dim(tmp)[2]))])+(ProOut$objects_redo[(2:(dim(tmp)[1]-1)),(1:(dim(tmp)[2]-2))])
    selSky<-which(tmp2==0, arr.ind=T)

    SelPos<-xy2radec(selSky[,1],selSky[,2], header=ProOut$header)
    LimLo<-xy2radec(1,1,header=ProOut$header)
    LimHi<-xy2radec(dim(ProOut$objects_redo)[1],dim(ProOut$objects_redo)[2],header=ProOut$header)
    D03CatRASel<-D03CatRA[which(D03CatRA>LimHi[1] & D03CatRA<LimLo[1] & D03CatDEC>LimLo[2] & D03CatDEC<LimHi[2])]
    D03CatDECSel<-D03CatDEC[which(D03CatRA>LimHi[1] & D03CatRA<LimLo[1] & D03CatDEC>LimLo[2] & D03CatDEC<LimHi[2])]

    D03CatRABrightSel<-D03CatRABright[which(D03CatRABright>LimHi[1] & D03CatRABright<LimLo[1] & D03CatDECBright>LimLo[2] & D03CatDECBright<LimHi[2])]
    D03CatDECBrightSel<-D03CatDECBright[which(D03CatRABright>LimHi[1] & D03CatRABright<LimLo[1] & D03CatDECBright>LimLo[2] & D03CatDECBright<LimHi[2])]

    tempmatch=coordmatch(SelPos, cbind(D03CatRASel, D03CatDECSel),rad = 20)
    sel<-which(tempmatch$Nmatch==0)

    selSky<-selSky[sel,]

    if (length(D03CatRABrightSel)>0){
        SelPos<-xy2radec(selSky[,1],selSky[,2], header=ProOut$header)
        tempmatch=coordmatch(SelPos, cbind(D03CatRABrightSel, D03CatDECBrightSel),rad = 90)
        sel<-which(tempmatch$Nmatch==0)


        selSky<-selSky[sel,]
    }

    RandSel<-round(runif(10, 1,dim(selSky)[1]))
    SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)

    good<-0
    while (good<10){
        SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)
        
        for (j in 1:10){
            dist<-sqrt((SelPos[j,1]-SelPos[,1])^2+(SelPos[j,2]-SelPos[,2])^2)
            if (length(which(dist<(1/60)))>1){
                SelPos[j,]<-c(NA,NA)
            }                
        }
        good<-length(which(is.finite(SelPos[,1])==T))
        RandSel[which(is.finite(SelPos[,1])==F)]<-runif(length(which(is.finite(SelPos[,1])==F)), 1,dim(selSky)[1])
    }
    SelPos<-xy2radec(selSky[RandSel,1],selSky[RandSel,2], header=ProOut$header)
    
    skyRA<-c(skyRA,SelPos[,1])
    skyDEC<-c(skyDEC,SelPos[,2])
}

D03hdr<-read.fitshdr('/mnt/jaws/DEVILS/imaging/D03/VIDEO2017/cdfs_Y_maxseeing0p90_2017-02-12.fits')

D03xyPosSky<-radec2xy(skyRA, skyDEC, D03hdr)
D03RadPixSky<-150

cores<-8
registerDoParallel(cores=cores)
system('mkdir CutoutsSky')

cat('Making D03 cutouts .....', '\n')

a = foreach(i=1:length(skyRA)) %dopar%  {


    xlo<-round(D03xyPosSky[i,1]-D03RadPixSky)
    xhi<-round(D03xyPosSky[i,1]+D03RadPixSky)
    ylo<-round(D03xyPosSky[i,2]-D03RadPixSky)
    yhi<-round(D03xyPosSky[i,2]+D03RadPixSky)
    

    imY<-read.fits('/mnt/jaws/DEVILS/imaging/D03/VIDEO2017/cdfs_Y_maxseeing0p90_2017-02-12.fits',xlo=xlo, ylo=ylo,xhi=xhi, yhi=yhi, hdu = 0)
    hdr<-imY$hdr[[1]]
    imY<-imY$dat[[1]]

    CairoPNG(paste('CutoutsSky/D03Sky_',i,'.png', sep=''))
    magimageWCS(imY, header=hdr)
    dev.off()
    


}

skyRA_D03<-skyRA
skyDEC_D03<-skyDEC

skyRA<-c(skyRA_D10, skyRA_D02, skyRA_D03)
skyDEC<-c(skyDEC_D10, skyDEC_D02, skyDEC_D03)

sky<-data.frame(skyRA,skyDEC)
names(sky)<-c('RA','DEC')
write.table(sky, file='DATAsky.tab', row.names=F)
