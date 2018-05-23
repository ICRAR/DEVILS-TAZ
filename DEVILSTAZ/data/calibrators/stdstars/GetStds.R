D10SDSS<-read.csv('D10SDSS_standards.csv')
D02_03OzDES<-read.csv('OzDESFstars.dat', skip=3, header=F)

load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/2017-12-14_BaseMasterCat/DMCat2017-12-14.rda')

D02OzDES<-D02_03OzDES[which(D02_03OzDES[,2]>34.0 & D02_03OzDES[,2]<37.05 & D02_03OzDES[,3]>-5.2 & D02_03OzDES[,3]< -4.2),]

D03OzDES<-D02_03OzDES[which(D02_03OzDES[,2]>52.3 & D02_03OzDES[,2]<54.0 & D02_03OzDES[,3]>-28.5 & D02_03OzDES[,3]< -27.5),]


RA_a<-c(D10SDSS[,'ra'], D02OzDES[,2], D03OzDES[,2])
DEC_a<-c(D10SDSS[,'dec'], D02OzDES[,3], D03OzDES[,3])
MAG<-c(D10SDSS[,'psfMag_r'], D02OzDES[,4], D03OzDES[,4])
PRIORITY_FLAG<-rep(3,length(RA_a))
ROWID<-seq(1,length(RA_a),1)



tempmatch=coordmatch(cbind(RA_a, DEC_a), cbind(DMCat$RA, DMCat$DEC),rad = 2)
good=tempmatch$bestmatch$refID %in% seq(1,length(RA_a),1)
goodmatch=tempmatch$bestmatch[good,]


RA<-RA_a
DEC<-DEC_a
RA[goodmatch[,1]]<-DMCat$RA[goodmatch[,2]]
DEC[goodmatch[,1]]<-DMCat$DEC[goodmatch[,2]]

Stds<-data.frame(ROWID, RA, DEC, MAG,  PRIORITY_FLAG)
write.table(Stds, file='DATAstspec.tab', row.names=F)

