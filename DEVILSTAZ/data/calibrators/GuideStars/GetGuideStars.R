
Mag1<-14.0
Mag2<-14.7

Mag1<-13.2
Mag2<-14.4



D10Stars<-read.table('USNO-B1_COSMOS.txt', header=T, sep='|')
D10Stars<-D10Stars[which(D10Stars[,'R1mag']>Mag1 & D10Stars[,'R1mag']<=Mag2 & D10Stars[,'pmRA']<15),]

load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/D10_TotalPhotometry.rda')
load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/D10_ColPhotometry.rda')


D10CatCol<-ColPhotoCat
D10CatTotal<-TotalPhotoCat
#### NOTE: CORRECT FOR MAGZEROPOINT OFFSET ERROR IN UVISTA !!!!!!! #####

D10CatTotal$Y_mag<-D10CatTotal$Y_mag+0.0474
D10CatCol$Y_mag<-D10CatCol$Y_mag+0.0474

D10CatTotal$J_mag<-D10CatTotal$J_mag+0.0489
D10CatCol$J_mag<-D10CatCol$J_mag+0.0489

D10CatTotal$H_mag<-D10CatTotal$H_mag+0.023
D10CatCol$H_mag<-D10CatCol$H_mag+0.023

D10CatTotal$Ks_mag<-D10CatTotal$Ks_mag+0.027
D10CatCol$Ks_mag<-D10CatCol$Ks_mag+0.027



tempmatch=coordmatch(D10Stars[,c('RAJ2000','DEJ2000')],D10CatTotal[,c("RAmax","Decmax")], rad = 2)
good=tempmatch$bestmatch$refID %in% seq(1,length(D10CatTotal$segID),1)
goodmatch=tempmatch$bestmatch[good,]
D10CatTotal<-D10CatTotal[goodmatch[,2],]
D10CatCol<-D10CatCol[goodmatch[,2],]
D10Stars<-D10Stars[goodmatch[,1],]


col<-(D10CatCol$H_mag-D10CatCol$Ks_mag)-(D10CatCol$Y_mag-D10CatCol$J_mag)

GuidesD10<-which(col < -0.25 & D10CatCol$axrat>0.9)

GuideTabRA<-D10CatTotal[GuidesD10,'RAmax']
GuideTabDEC<-D10CatTotal[GuidesD10,'Decmax']
GuideTabRMAG<-D10Stars[GuidesD10,'R1mag']


D02Stars<-read.table('USNO-B1_XMMLSS.txt', header=T, sep='|')
D02Stars<-D02Stars[which(D02Stars[,'R1mag']>Mag1 & D02Stars[,'R1mag']<=Mag2 & D02Stars[,'pmRA']<15),]

load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/D02_TotalPhotometry.rda')
load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/D02_ColPhotometry.rda')

D02CatCol<-ColPhotoCat
D02CatTotal<-TotalPhotoCat

#### NOTE: CORRECT FOR MAGZEROPOINT OFFSET ERROR IN VIDEO !!!!!!! #####
D02CatTotal$Y_mag<-D02CatTotal$Y_mag+0.0943
D02CatCol$Y_mag<-D02CatCol$Y_mag+0.0943

D02CatTotal$J_mag<-D02CatTotal$J_mag+0.0512
D02CatCol$J_mag<-D02CatCol$J_mag+0.0512

D02CatTotal$H_mag<-D02CatTotal$H_mag+0.0442
D02CatCol$H_mag<-D02CatCol$H_mag+0.0442

D02CatTotal$Ks_mag<-D02CatTotal$Ks_mag+0.0498
D02CatCol$Ks_mag<-D02CatCol$Ks_mag+0.0498



tempmatch=coordmatch(D02Stars[,c('RAJ2000','DEJ2000')],D02CatTotal[,c("RAmax","Decmax")], rad = 3)
good=tempmatch$bestmatch$refID %in% seq(1,length(D02CatTotal$segID),1)
goodmatch=tempmatch$bestmatch[good,]
D02CatTotal<-D02CatTotal[goodmatch[,2],]
D02CatCol<-D02CatCol[goodmatch[,2],]
D02Stars<-D02Stars[goodmatch[,1],]

col<-(D02CatCol$H_mag-D02CatCol$Ks_mag)-(D02CatCol$Y_mag-D02CatCol$J_mag)

GuidesD02<-which(col < -0.25 & D02CatCol$axrat>0.9)

GuideTabRA<-c(GuideTabRA,D02CatTotal[GuidesD02,'RAmax'])
GuideTabDEC<-c(GuideTabDEC, D02CatTotal[GuidesD02,'Decmax'])
GuideTabRMAG<-c(GuideTabRMAG, D02Stars[GuidesD02,'R1mag'])


D03Stars<-read.table('USNO-B1_ECDFS.txt', header=T, sep='|')
D03Stars<-D03Stars[which(D03Stars[,'R1mag']>Mag1 & D03Stars[,'R1mag']<=Mag2 & D03Stars[,'pmRA']<15),]

load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/D03_TotalPhotometry.rda')
load('/Users/luke/work/DEVILS/Imaging/MakeInputCats/D03_ColPhotometry.rda')

D03CatCol<-ColPhotoCat
D03CatTotal<-TotalPhotoCat


D03CatTotal$Y_mag<-D03CatTotal$Y_mag+0.1
D03CatCol$Y_mag<-D03CatCol$Y_mag+0.1

D03CatTotal$J_mag<-D03CatTotal$J_mag+0.051
D03CatCol$J_mag<-D03CatCol$J_mag+0.051

D03CatTotal$H_mag<-D03CatTotal$H_mag+0.0537
D03CatCol$H_mag<-D03CatCol$H_mag+0.0537

D03CatTotal$Ks_mag<-D03CatTotal$Ks_mag+0.0421
D03CatCol$Ks_mag<-D03CatCol$Ks_mag+0.0421



tempmatch=coordmatch(D03Stars[,c('RAJ2000','DEJ2000')],D03CatTotal[,c("RAmax","Decmax")], rad = 3)
good=tempmatch$bestmatch$refID %in% seq(1,length(D03CatTotal$segID),1)
goodmatch=tempmatch$bestmatch[good,]
D03CatTotal<-D03CatTotal[goodmatch[,2],]
D03CatCol<-D03CatCol[goodmatch[,2],]
D03Stars<-D03Stars[goodmatch[,1],]

col<-(D03CatCol$H_mag-D03CatCol$Ks_mag)-(D03CatCol$Y_mag-D03CatCol$J_mag)

GuidesD03<-which(col < -0.25 & D03CatCol$axrat>0.9)

RA<-c(GuideTabRA,D03CatTotal[GuidesD03,'RAmax'])
DEC<-c(GuideTabDEC, D03CatTotal[GuidesD03,'Decmax'])
MAG<-c(GuideTabRMAG, D03Stars[GuidesD03,'R1mag'])
ROWID<-seq(1,length(RA),1)

GuideTab<-data.frame(ROWID,RA, DEC, MAG)
print(dim(GuideTab))

#USNOD10<-read.table('USNO-B1_COSMOS.txt', header=T, sep='|')
#USNOD02<-read.table('USNO-B1_XMMLSS.txt', header=T, sep='|')
#USNOD03<-read.table('USNO-B1_ECDFS.txt', header=T, sep='|')

#USNO<-rbind(USNOD10,USNOD02,USNOD03)

#USNO<-USNO[which(abs(USNO[,'pmRA'])<15),]

#tempmatch=coordmatch(GuideTab[,c('RA','DEC')],USNO[,c("RAJ2000","DEJ2000")], rad = 2)
#good=tempmatch$bestmatch$refID %in% seq(1,length(GuideTab$RA),1)
#goodmatch=tempmatch$bestmatch[good,]

#GuideTab<-GuideTab[goodmatch[,1],]
#print(dim(GuideTab))

Bad<-as.vector(read.table('GuidesVISBad.txt'))


GuideTab<-GuideTab[which(GuideTab$ROWID %in% Bad[,1]==FALSE),]

print(dim(GuideTab))

write.table(GuideTab, file='DATAguide.tab', row.names=F)


#system('rm -rf cutouts/*.png')
#source('cutoutGuides.R')
