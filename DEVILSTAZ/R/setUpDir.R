#' Set up DEVILS directory structure
#'
#' @description This is the highlevel main TAZ function for setting up the directory structure. 
#' The majority of the other TAZ functions rely on this directory structure being in place.
#' Users provide a list of runs, and start/end dates for each run and the fucntion will produce
#' the correct directories, make logs and observability plots and unpack all required calibration 
#' files.
#'  
#' @param workingDir Directory you wish to produce DEVILS dtaa structure
#' @param runs vector of run names with the format run#_YYYY_MM (i.e. 'run1_2017_12')
#' @param dateStart vector of start dates for each run with the format YYYY_MM_DD (i.e. 2017_12_18). Must be the same size as runs
#' @param dateEnd vector of end dates for each run with the format YYYY_MM_DD (i.e. 2017_12_22). Must be the same size as runs
#' @param verbose tell me whats going on: 0=nothing, 1=somethings, 2=everything

#' @examples 
#' setUpDir(workingDir='.', runs=c('run1_2017_12','run2_2018_01'),dateStart=c('2017_12_18','2018_01_09'),dateEnd=c('2017_12_26','2018_01_22'), verbose=1)
#' @export
setUpDir<-function(workingDir='.', runs=c('run1_2017_12','run2_2018_01'),dateStart=c('2017_12_18','2018_01_09'),dateEnd=c('2017_12_26','2018_01_22'), verbose=1){
    
    DEVILSGreen <- make_style(rgb(0, 112/255, 82/255), bg = FALSE)
    
    setwd(workingDir)
    workingDir<-getwd()

     if (verbose==0){
        cat(DEVILSGreen('** Running DEVILS directory setup (setUpDir) verbose set to 0 - you will recieve no progress messages **' , '\n'))
    }

    if (verbose>0){
        cat('\n')
        cat(DEVILSGreen('*****************************************************'), '\n')
        cat(DEVILSGreen('********* STARTING DEVILS DIRECTORY SETUP ***********'), '\n')
        cat(DEVILSGreen('*****************************************************'), '\n')
        cat('\n')
        cat('\n')
        cat(DEVILSGreen(paste('Making DEVILS top level directory structure in ', workingDir,'/data', sep='')), '\n')

        }

    if (verbose>0){}
    
    
    system('mkdir data')
    system('mkdir data/rawdata')
    system('mkdir data/reduced')
    system('mkdir data/reduced/allSpec')
    system('mkdir data/reduced/newSpec')
    system('mkdir data/reduced/stackedSpec')
    system('mkdir data/reduced/stackedSpec/plots')
    system('mkdir data/reduced/stackedSpec/AutoZplots')
    system('mkdir data/biases')
    system('mkdir data/biases/junk')
    system('mkdir data/darks')
    system('mkdir data/darks/junk')
    system('mkdir data/observing')
    system('mkdir data/logs')
    system('mkdir data/catalogues')
    system('mkdir data/catalogues/MASTERcats')
    #system('mkdir data/calibrators')
   #system('mkdir data/idxFiles')

    if (verbose>0){
        cat(DEVILSGreen('Top level directory structure complete.'), '\n')
        cat(DEVILSGreen('Copying calibrators and IDX files.'), '\n')
        cat('', '\n')
        cat(DEVILSGreen('Unpacking.....'), '\n')
    }
    
    LibPaths<-.libPaths()[1]
    
    system(paste('cp ', LibPaths, '/DEVILSTAZ/data/calibrators.tar data/',sep='')) 
    system(paste('cp ', LibPaths, '/DEVILSTAZ/data/idxFiles.tar data/',sep=''))

    setwd(paste(workingDir, '/data',sep=''))
    system(paste('tar -xvf calibrators.tar',sep='')) 
    system(paste('tar -xvf idxFiles.tar',sep=''))
    system(paste('rm -rf calibrators.tar',sep=''))
    system(paste('rm -rf idxFiles.tar',sep=''))
    setwd(workingDir)


    
    if (verbose>0){
        cat('', '\n')
        cat(DEVILSGreen('    - Making year observing plans for DEVILS fields......'), '\n')
    }

    CairoPNG('data/observing/DO2_yrPlan2017.png', width=1000, height=600)
    tmp<-yearup(RA="02:21:59.4", Dec="-04:49:30.0", Target='user', Date=c(2017,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/DO2_yrPlan2018.png', width=1000, height=600)
    tmp<-yearup(RA="02:21:59.4", Dec="-04:49:30.0", Target='user', Date=c(2018,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/DO2_yrPlan2019.png', width=1000, height=600)
    tmp<-yearup(RA="02:21:59.4", Dec="-04:49:30.0", Target='user', Date=c(2019,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()

    CairoPNG('data/observing/DO3_yrPlan2017.png', width=1000, height=600)
    tmp<-yearup(RA="03:34:36.0", Dec="-28:06:00.0", Target='user', Date=c(2017,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/DO3_yrPlan2018.png', width=1000, height=600)
    tmp<-yearup(RA="03:34:36.0", Dec="-28:06:00.0", Target='user', Date=c(2018,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/DO3_yrPlan2019.png', width=1000, height=600)
    tmp<-yearup(RA="03:34:36.0", Dec="-28:06:00.0", Target='user', Date=c(2019,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/D10_yrPlan2019.png', width=1000, height=600)
    tmp<-yearup(RA="03:34:36.0", Dec="-28:06:00.0", Target='user', Date=c(2019,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()

    CairoPNG('data/observing/D10_yrPlan2017.png', width=1000, height=600)
    tmp<-yearup(RA="10:00:24.0", Dec="02:06:00.0", Target='user', Date=c(2017,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/D10_yrPlan2018.png', width=1000, height=600)
    tmp<-yearup(RA="10:00:24.0", Dec="02:06:00.0", Target='user', Date=c(2018,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()
    CairoPNG('data/observing/D10_yrPlan2019.png', width=1000, height=600)
    tmp<-yearup(RA="10:00:24.0", Dec="02:06:00.0", Target='user', Date=c(2019,1,1),Loc='AAT', UTCdiff=10)
    plotyearup(tmp)
    dev.off()


    if (verbose>0){
        cat('', '\n')
        cat(DEVILSGreen('Making requestested runs.....'), '\n')
    }
    
    for (i in 1:length(runs)){

        if (verbose>0){
            cat(DEVILSGreen(paste('    - Making data/biases/',runs[i],sep='')), '\n')
            cat(DEVILSGreen(paste('    - Making data/darks/',runs[i],sep='')), '\n')
            cat(DEVILSGreen(paste('    - Making data/rawdata/',runs[i],sep='')), '\n')
            cat(DEVILSGreen(paste('    - Making data/reduced/',runs[i],sep='')), '\n')
            cat(DEVILSGreen(paste('    - Making data/observing/',runs[i],sep='')), '\n')
            cat(paste(' ', '\n'))
        }
        
        system(paste('mkdir data/biases/',runs[i],sep=''))       
        system(paste('mkdir data/darks/',runs[i],sep=''))
        system(paste('mkdir data/rawdata/',runs[i],sep=''))
        system(paste('mkdir data/reduced/',runs[i],sep=''))
        system(paste('mkdir data/observing/',runs[i],sep=''))

        if (verbose>0){
            cat(DEVILSGreen('    - Calculating dates.....'), '\n')
        }
        

        days<-seq(as.numeric(substr(dateStart[i],9,10)),as.numeric(substr(dateEnd[i],9,10)),1)
        month<-as.numeric(substr(dateStart[i],6,7))
        year<-as.numeric(substr(dateStart[i],1,4))

        dayStart<-as.numeric(substr(dateStart[i],9,10))
        dayEnd<-as.numeric(substr(dateEnd[i],9,10))
        monthStart<-as.numeric(substr(dateStart[i],6,7))
        monthEnd<-as.numeric(substr(dateEnd[i],6,7))
        yearStart<-as.numeric(substr(dateStart[i],1,4))
        yearEnd<-as.numeric(substr(dateEnd[i],1,4))
        
        JDStart<-date2jd(year=yearStart, mon=monthStart, mday=dayStart, hour=12)
        JDEnd<-date2jd(year=yearEnd, mon=monthEnd, mday=dayEnd, hour=12)
        JDs<-seq(JDStart,JDEnd,1)
        datesSeq<-jd2date(JDs)

        if (verbose>0){
            cat(DEVILSGreen('        - Making indviual date directory structure......'), '\n')
        }

        for (j in 1:length(datesSeq$mday)){

            if (verbose>0){
                cat(DEVILSGreen(paste('                    - Making directory for: ',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j], sep='')), '\n')
            }
            
            
            system(paste('mkdir data/rawdata/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j], sep=''))
            system(paste('mkdir data/rawdata/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/junk', sep=''))
            system(paste('mkdir data/reduced/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j], sep=''))
            system(paste('mkdir data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j], sep=''))
            system(paste('mkdir data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DOCats', sep=''))
            
            system(paste('mkdir data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DOCats/TileFiles', sep=''))
            
            if (verbose>0){
                cat(DEVILSGreen('                    - Making log file....'), '\n')
            }

            
            LogFilename=paste('data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/',datesSeq$year[j],'_',datesSeq$mon[j],'_', datesSeq$mday[j],'_DEVILS_obs_log.txt', sep='')
            makeLog(run=runs[i], date=paste(datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j], sep=''),fileName=LogFilename)

            if (verbose>0){
                cat(DEVILSGreen('                - Calculating observability....'), '\n')
            }

            
            dayupUT00<-dayupmulti(Targets=cbind(Names=c('D02', 'D03', 'D10'), RA=c("02:21:59.4", "03:34:36.0", "10:00:24.0"), Dec=c("-04:49:30.0", "-28:06:00.0", "02:06:00.0")), Date=c(datesSeq$year[j],datesSeq$mon[j],datesSeq$mday[j]),Loc='AAT', UTCdiff=00, Time=c(0,0,0))
            dayupUT08<-dayupmulti(Targets=cbind(Names=c('D02', 'D03', 'D10'), RA=c("02:21:59.4", "03:34:36.0", "10:00:24.0"), Dec=c("-04:49:30.0", "-28:06:00.0", "02:06:00.0")), Date=c(datesSeq$year[j],datesSeq$mon[j],datesSeq$mday[j]),Loc='AAT', UTCdiff=8, Time=c(8,0,0))
            dayupUT10<-dayupmulti(Targets=cbind(Names=c('D02', 'D03', 'D10'), RA=c("02:21:59.4", "03:34:36.0", "10:00:24.0"), Dec=c("-04:49:30.0", "-28:06:00.0", "02:06:00.0")), Date=c(datesSeq$year[j],datesSeq$mon[j],datesSeq$mday[j]),Loc='AAT', UTCdiff=10, Time=c(10,0,0))
            dayupUT11<-dayupmulti(Targets=cbind(Names=c('D02', 'D03', 'D10'), RA=c("02:21:59.4", "03:34:36.0", "10:00:24.0"), Dec=c("-04:49:30.0", "-28:06:00.0", "02:06:00.0")), Date=c(datesSeq$year[j],datesSeq$mon[j],datesSeq$mday[j]),Loc='AAT', UTCdiff=10, Time=c(10,0,0))

            darkHoursUT10<-c(seq(as.numeric(substr(dayupUT10[[1]]$set[1],12,13)),23,1),  seq(0, as.numeric(substr(dayupUT10[[1]]$rise[2],12,13)),1))


            
            if (verbose>0){
                cat(DEVILSGreen('                     - Making observation plan....'), '\n')
            }

            altD02<-darkHoursUT10
            altD03<-darkHoursUT10
            altD10<-darkHoursUT10
            fieldNames<-c('D02', 'D03', 'D10')
            bestName<-altD02
            bestAlt<-altD02   

            for (k in 1:length(altD02)){
                altD02[k]<-median(dayupUT10[[1]]$obs$Alt[which(as.numeric(substr(dayupUT10[[1]]$obs$LTPOSIX,12,13)) >= darkHoursUT10[k] & as.numeric(substr(dayupUT10[[1]]$obs$LTPOSIX,12,13)) < darkHoursUT10[k]+1)])
                altD03[k]<-median(dayupUT10[[2]]$obs$Alt[which(as.numeric(substr(dayupUT10[[2]]$obs$LTPOSIX,12,13)) >= darkHoursUT10[k] & as.numeric(substr(dayupUT10[[2]]$obs$LTPOSIX,12,13)) < darkHoursUT10[k]+1)])
                altD10[k]<-median(dayupUT10[[3]]$obs$Alt[which(as.numeric(substr(dayupUT10[[3]]$obs$LTPOSIX,12,13)) >= darkHoursUT10[k] & as.numeric(substr(dayupUT10[[3]]$obs$LTPOSIX,12,13)) < darkHoursUT10[k]+1)])
                tmp<-c(altD02[k],altD03[k],altD10[k])
                bestName[k]<-fieldNames[which(tmp==max(tmp))]
                bestAlt[k]<-tmp[which(tmp==max(tmp))]
            }
            UT<-darkHoursUT10-10
            UT[which(UT<0)]<-24+UT[which(UT<0)]

            obsPlan<-data.frame(LocalTime=darkHoursUT10, UT=UT, BestField=bestName, Alt=bestAlt)
            obsPlanFile<-paste('data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DEVILS-ObsPlan-',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'.txt',sep='')
            write.csv(obsPlan, file=obsPlanFile, row.names=F, quote=F)
            
            write(" ",file=obsPlanFile ,append=TRUE)
            write(paste('# DEVILS Observing plan for night - ',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j], sep=''),file=obsPlanFile,append=TRUE)
            
            setH<-as.numeric(substr(dayupUT10[[1]]$set[1],12,13))-10
            setH[which(setH<0)]<-24+setH[which(setH<0)]
            setM<-as.numeric(substr(dayupUT10[[1]]$set[1],15,16))
            setS<-as.numeric(substr(dayupUT10[[1]]$set[1],18,19))

            riseH<-as.numeric(substr(dayupUT10[[1]]$rise[2],12,13))-10
            riseH[which(riseH<0)]<-24+riseH[which(riseH<0)]
            riseM<-as.numeric(substr(dayupUT10[[1]]$rise[2],15,16))
            riseS<-as.numeric(substr(dayupUT10[[1]]$rise[2],18,19))
            
            write(paste('# Sun Set Time (UT) - ',setH,':',setM,':',setS, sep=''),file=obsPlanFile,append=TRUE)
            write(paste('# Sun Rise Time (UT) - ',riseH,':',riseM,':',riseS, sep=''),file=obsPlanFile,append=TRUE)

            write('# **NOTE** This only takes into account the Altitude of the field, please check the observability plots for moon location',file=obsPlanFile,append=TRUE)

            if (verbose>0){
                cat(DEVILSGreen('                    - Making observability plots....'), '\n')
            }
            
            pdf(paste('data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DEVILS-Obs-UT00-',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'.pdf',sep=''), width=18, height=12)
            par(mfrow = c(1, 2))
            par(mar=c(3.1,3.1,1.1,1.1))

            layout(matrix(c(1,2), 1, 2, byrow = TRUE))
            plotdayupmulti(dayupUT00, ytype='Alt')
            plotdayupmulti(dayupUT00, ytype='AM')
            dev.off()

            pdf(paste('data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DEVILS-Obs-UT08-',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'.pdf',sep=''), width=18, height=12)
            par(mfrow = c(1, 2))
            par(mar=c(3.1,3.1,1.1,1.1))

            layout(matrix(c(1,2), 1, 2, byrow = TRUE))
            plotdayupmulti(dayupUT08, ytype='Alt')
            plotdayupmulti(dayupUT08, ytype='AM')
            dev.off()

            pdf(paste('data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DEVILS-Obs-UT10-',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'.pdf',sep=''), width=18, height=12)
            par(mfrow = c(1, 2))
            par(mar=c(3.1,3.1,1.1,1.1))

            layout(matrix(c(1,2), 1, 2, byrow = TRUE))
            plotdayupmulti(dayupUT10, ytype='Alt')
            plotdayupmulti(dayupUT10, ytype='AM')
            dev.off()

            pdf(paste('data/observing/',runs[i],'/',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'/DEVILS-Obs-UT11-',datesSeq$year[j], '_',datesSeq$mon[j],'_', datesSeq$mday[j],'.pdf',sep=''), width=18, height=12)
            par(mfrow = c(1, 2))
            par(mar=c(3.1,3.1,1.1,1.1))

            layout(matrix(c(1,2), 1, 2, byrow = TRUE))
            plotdayupmulti(dayupUT11, ytype='Alt')
            plotdayupmulti(dayupUT11, ytype='AM')
            dev.off()
            
        }

    }

    if (verbose==0){
        cat(DEVILSGreen('** setUpDir finished **'), '\n')
    }

    if (verbose>0){
        cat(DEVILSGreen('** DEVILS set up of directory structure finished. **'), '\n')
        cat(DEVILSGreen('** Please find data strucutre in ', workingDir,'/data/ **'), '\n')
    }


}
