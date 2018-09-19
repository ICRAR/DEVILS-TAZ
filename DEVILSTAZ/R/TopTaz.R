CMD<-readLines('TAZRunCmd.R')

cont<-F
dir<-'/mnt/jaws/DEVILS/AAT/datacentral-aao-gov-au_cloud/DEVILS/data'
#dir<-'/Users/luke/work/DEVILS/TAZ/data'
pauseTime<-5 #min

while (cont==F){

    check<-system(paste('du -s ',dir,sep=''),intern = TRUE )
    check<-as.numeric(strsplit(check, '\t')[[1]][1])
    cat('Directory size t=1:',check, '\n')
    cat('    - Waiting for',pauseTime,'min', '\n')
    Sys.sleep(pauseTime*60)
    check2<-system(paste('du -s ',dir,sep=''),intern = TRUE )
    check2<-as.numeric(strsplit(check2, '\t')[[1]][1])
    cat('Directory size t=2:',check2, '\n')
    if (check==check2){
        cont=T
        cat('File Size Not Changed. Continuing.....','\n')
        
    }
    if (check!=check2){
        cat('***WARNING** File Size Changed. Pausing.','\n')
        
    }
    
}


cont<-F
printed<-F

while (cont==F){
    check<-list.files(path=dir,pattern='DoneSync.txt')
    if (length(check)>0){
        cont=T
        cat('DoneSync.txt file found. Continuing.....','\n')
        printed<-F
    }
    if (length(check)==0){
        if (printed==F) {cat('***WARNING** DoneSync.txt file NOT found. Waiting.....','\n')}
        printed<-T
    }
    
}


source('TAZRunCmd.R')

system(paste('rm -f ',dir, '/DoneSync.txt', sep=''))

system('RESULTS=$(ssh -i ~/.ssh/id_rsaTmp -t ldavies@store08 "ps aux | grep -i "[o]wncloudcmd"")')
system('RESULTS=$(ssh -i ~/.ssh/id_rsaTmp -t ldavies@store08 "ls")')
tmp<-system('echo $RESULTS',intern = TRUE )

