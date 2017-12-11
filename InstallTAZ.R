install.packages('devtools', repos='http://cran.us.r-project.org')

library('devtools')

install_github('asgr/Tiler')
install.packages('astro',type='source', repos='http://cran.us.r-project.org')
install.packages('sphereplot',type='source', repos='http://cran.us.r-project.org')
install.packages('rgl',type='source', repos='http://cran.us.r-project.org')
install.packages('plotrix',type='source', repos='http://cran.us.r-project.org')
install.packages('fields',type='source', repos='http://cran.us.r-project.org')
install.packages('xtable',type='source', repos='http://cran.us.r-project.org')
install.packages('doParallel',type='source', repos='http://cran.us.r-project.org')
install.packages('gridExtra',type='source', repos='http://cran.us.r-project.org')
install.packages('crayon',type='source', repos='http://cran.us.r-project.org')


install_github('asgr/celestial')
install_github('asgr/magicaxis')
install_github('asgr/whatsup')

install_github('ICRAR/DEVILS-TAZ', subdir ='DEVILSTAZ')


### This is really important to make sure the flux calibration works!!!! ######
LibPaths<-.libPaths()[1]
system(paste('tar -xvf ', LibPaths, '/DEVILSTAZ/data/calibrators.tar --directory ', LibPaths, '/DEVILSTAZ/data/',sep='')) 
system(paste('tar -xvf ', LibPaths, '/DEVILSTAZ/data/idxFiles.tar --directory ', LibPaths, '/DEVILSTAZ/data/' ,sep=''))


