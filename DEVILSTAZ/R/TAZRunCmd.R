name<-'LDavies' # name of lead observer
N_D02A<-6 # number of tiles to generate for D02A for next night
N_D02B<-0 # number of tiles to generate for D02B for next night
N_D03<-2 # number of tiles to generate for D03 for next night
N_D10<-0 # number of tiles to generate for D10 for next night
D02A_startPlate<-0 # starting plate number for D02A (0 or 1)
D02B_startPlate<-0 # starting plate number for D02A (0 or 1)
D03_startPlate<-0 # starting plate number for D02A (0 or 1)
D10_startPlate<-0 # starting plate number for D02A (0 or 1)
addOzDES<-TRUE  # add in OzDES targets? (TRUE/FALSE)
OzDESCat<-'C123_source20180119.csv' # OzDES catalogue (must be in /mnt/jaws/DEVILS/AAT/datacentral-aao-gov-au_cloud/DEVILS/AAT/data/catalogues/OzDESCats/'
makeNormalTiles<-TRUE # make tiles with normal priorties?  (TRUE/FALSE)
makeBackUpTiles<-FALSE # make tiles with backup priorties?  (TRUE/FALSE)


#### DON'T CHANGE BELOW HERE ####

OzDESCat<-paste('/mnt/jaws/DEVILS/AAT/datacentral-aao-gov-au_cloud/DEVILS/AAT/data/catalogues/OzDESCats/',OzDESCat,sep='')

TAZ(user=name, workingDir='.', dobizCheck=F, bizStopError=F, doReduce=T, doDark=T, zeroPoint=T, doCosmic=T, doExtract=T, doStack=T, toAutoZStacks='all', doAutoZ=T, highZ=T, cores=10, reducCores=10, doUpdateMaster=T, doTiler=T, docheckConfig=F, N_D02A = N_D02A, N_D02B = N_D02B, N_D03 = N_D03, N_D10 = N_D10, D02A_startPlate = D02A_startPlate, D02B_startPlate = D02B_startPlate, D03_startPlate = D03_startPlate, D10_startPlate = D10_startPlate, addOzDES=addOzDES, OzDESCat=OzDESCat, docutoutConfig=F, verbose=2, makeNormalTiles=makeNormalTiles, makeBackUpTiles=makeBackUpTiles)
