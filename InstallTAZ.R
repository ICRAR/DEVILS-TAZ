install.packages('devtools')
install.packages('astro')
install_github('asgr/Tiler')
install.packages('sphereplot',type='source')
install.packages('rgl',type='source')
install.packages('plotrix',type='source')
install.packages('fields',type='source')
install.packages('xtable',type='source')
install.packages('doParallel',type='source')
install.packages('gridExtra',type='source')


install_github('asgr/celestial')
install_github('asgr/magicaxis')
install_github('asgr/whatsup')

install_github('ICRAR/DEVILS-TAZ')


### This is really important to make sure the flux calibratin works!!!! ######
LibPaths<-.libPaths()
system(paste('tar -xvf ', LibPaths, '/DEVILSTAZ/data/calibrators.tar --directory ', LibPaths, '/DEVILSTAZ/data/',sep='')) 
system(paste('tar -xvf ', LibPaths, '/DEVILSTAZ/data/idxFiles.tar --directory ', LibPaths, '/DEVILSTAZ/data/' ,sep=''))


