

install.packages('crayon',type='source', repos='http://cran.us.r-project.org')
install.packages('astro',type='source', repos='http://cran.us.r-project.org')
install.packages('sphereplot',type='source', repos='http://cran.us.r-project.org')
install.packages('rgl',type='source', repos='http://cran.us.r-project.org')
install.packages('plotrix',type='source', repos='http://cran.us.r-project.org')
install.packages('fields',type='source', repos='http://cran.us.r-project.org')
install.packages('xtable',type='source', repos='http://cran.us.r-project.org')
install.packages('doParallel',type='source', repos='http://cran.us.r-project.org')
install.packages('gridExtra',type='source', repos='http://cran.us.r-project.org')
install.packages('astrolibR',type='source', repos='http://cran.us.r-project.org')
install.packages('moonsun',type='source', repos='http://cran.us.r-project.org')
install.packages('mailR',type='source', repos='http://cran.us.r-project.org')
install.packages('imager',type='source', repos='http://cran.us.r-project.org')
install.packages('EBImage',type='source', repos='http://cran.us.r-project.org')
#install.packages('spatstat',type='source', repos='http://cran.us.r-project.org')
#install.packages('XML',type='source', repos='http://cran.us.r-project.org')


install.packages('devtools', repos='http://cran.us.r-project.org')
library('devtools')

install_github('asgr/Tiler')
install_github('asgr/celestial')
install_github('asgr/magicaxis')
install_github('asgr/whatsup')

install_github('ICRAR/DEVILS-TAZ', subdir ='DEVILSTAZ')


### This is really important to make sure the flux calibration works when running sub functions of DEVILSTAZ!!!! ######
### If running high level TAZ it will do it for you ######

cat('\n','\n')
cat('***************************************************************','\n')
cat('******** UNPACKING DEVILS CALIBRATORS AND IDX FILES *********','\n')
cat('***************************************************************','\n')
cat('\n','\n')

LibPaths<-.libPaths()

DEVILSPATH<-NA

for (jj in 1:length(LibPaths)){
    packagesList<-list.files(path=LibPaths[jj], pattern='*')
    if (length(which(packagesList=='DEVILSTAZ'))>0){DEVILSPATH<-LibPaths[jj]}
}

if (is.na(DEVILSPATH)==T){
    cat('*** WARNING COULD NOT FIND DEVILSTAZ PACKAGE INSTALATION ***', '\n')
    cat('Exiting TAZ, please check you have DEVILSTAZ installed correctly', '\n')
    return(NULL)
}


system(paste('tar -xvf ', DEVILSPATH, '/DEVILSTAZ/data/calibrators.tar --directory ', DEVILSPATH, '/DEVILSTAZ/data/',sep='')) 
system(paste('tar -xvf ', DEVILSPATH, '/DEVILSTAZ/data/idxFiles.tar --directory ', DEVILSPATH, '/DEVILSTAZ/data/' ,sep=''))


warningsF<-0

                                        # Check installation:
cat('\n','\n')
cat('***************************************************************','\n')
cat('******** CHECKING CORRECT INSTALLTION OF TAZ PACKAGES *********','\n')
cat('***************************************************************','\n')
cat('\n','\n')

packageList<-c('crayon','devtools', 'Tiler', 'astro','sphereplot','rgl','plotrix','fields','xtable','doParallel','gridExtra','celestial','magicaxis','whatsup', 'astrolibR', 'moonsun', 'XML', 'DEVILSTAZ', 'mailR','imager', 'EBImage')

LibList<-installed.packages()[,1]

for (i in 1:length(packageList)){

    Found<-FALSE
    if (length(which(LibList==packageList[i]))>0){Found=TRUE}

    if (i==1 & Found==FALSE) {
            cat('*** WARNING PACKAGE ',packageList[i], ' NOT INSTALLED, PLEASE CHECK INSTALATION ***', '\n')
            return(NULL)           
    }
    
    if (i==1 & Found==TRUE) {
        library('crayon')
    }
    
    if (i>1 & Found==TRUE){
        cat(green('Package ',packageList[i], ' installed.....'), '\n') 
    }
    if (i>1 & Found==FALSE){
       
        cat(red('*** WARNING PACKAGE ',packageList[i], ' NOT INSTALLED, PLEASE CHECK INSTALATION ***'), '\n')
        warningsF<-warningsF+1
    }


}

cat('\n','\n')
cat('************************************************************************','\n')
cat('******** CHECKING CORRECT UNPACKING OF TAZ CALIBRATORS AND IDX *********','\n')
cat('************************************************************************','\n')
cat('\n','\n')

listCal<-list.files(path=paste(DEVILSPATH,'/DEVILSTAZ/data/',sep=''), pattern='*')
listCal<-listCal[which(substr(listCal,nchar(listCal)-3,nchar(listCal)) !='.tar' & substr(listCal,nchar(listCal)-3,nchar(listCal)) !='data')]

if (length(listCal)==0){

    cat(red('*** WARNING CALIBRATORS AND IDX FILES DO NOT APPEAR TO BE UNPACKED PLEASE CHECK INSTALLATION ***'), '\n') 
    warningsF<-warningsF+1
}

if (length(listCal)>0){
    cat(green('Calibrators and IDX files unpacked correctly'), '\n') 
}


cat('\n')
if (warningsF==0) {cat(green('TAZ installation finished. There were',warningsF,'major warnings (ignore below)'),'\n')}
if (warningsF>0) {cat(green('TAZ installation finished. There were ',warningsF,'major warnings. Please check installation.'),'\n')}
