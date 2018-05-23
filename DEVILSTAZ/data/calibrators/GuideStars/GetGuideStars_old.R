
Mag1<-14.2
Mag2<-14.8

D10Stars<-read.csv('D10AndrewsStars.csv')
D10Stars<-D10Stars[which(D10Stars[,'r_mag']>Mag1 & D10Stars[,'r_mag']<=Mag2),]


load('../D10_TotalPhotometry.rda')
D10CatTotal<-TotalPhotoCat

tempmatch=coordmatch(D10Stars[,c('RA','DEC')],D10CatTotal[,c("RAmax","Decmax")], rad = 3)
good=tempmatch$bestmatch$refID %in% seq(1,length(D10CatTotal$segID),1)
goodmatch=tempmatch$bestmatch[good,]
D10CatTotal<-D10CatTotal[goodmatch[,2],]
D10Stars<-D10Stars[goodmatch[,1],]

GuidesD10<-which(D10CatTotal$edge_frac>=0.0)

GuideTabRA<-D10CatTotal[GuidesD10,'RAmax']
GuideTabDEC<-D10CatTotal[GuidesD10,'Decmax']
GuideTabRMAG<-D10Stars[GuidesD10,'r_mag']


D02Stars<-read.csv('D02CFHTLSStars.csv')
D02Stars<-D02Stars[which(D02Stars[,'r']>Mag1 & D02Stars[,'r']<=Mag2),]

load('../D02_TotalPhotometry.rda')
D02CatTotal<-TotalPhotoCat



tempmatch=coordmatch(D02Stars[,c('ra','dec')],D02CatTotal[,c("RAmax","Decmax")], rad = 3)
good=tempmatch$bestmatch$refID %in% seq(1,length(D02CatTotal$segID),1)
goodmatch=tempmatch$bestmatch[good,]
D02CatTotal<-D02CatTotal[goodmatch[,2],]
D02Stars<-D02Stars[goodmatch[,1],]

GuidesD02<-which(D02CatTotal$edge_frac>=0.0)

GuideTabRA<-c(GuideTabRA,D02CatTotal[GuidesD02,'RAmax'])
GuideTabDEC<-c(GuideTabDEC, D02CatTotal[GuidesD02,'Decmax'])
GuideTabRMAG<-c(GuideTabRMAG, D02Stars[GuidesD02,'r'])


D03Stars<-read.csv('D03VST_ATLASStars.csv')
D03Stars<-D03Stars[which(D03Stars[,'RPETROMAG']>Mag1 & D03Stars[,'RPETROMAG']<=Mag2),]

load('../D03_TotalPhotometry.rda')
D03CatTotal<-TotalPhotoCat


tempmatch=coordmatch(D03Stars[,c('RA2000','DEC2000')],D03CatTotal[,c("RAmax","Decmax")], rad = 3)
good=tempmatch$bestmatch$refID %in% seq(1,length(D03CatTotal$segID),1)
goodmatch=tempmatch$bestmatch[good,]
D03CatTotal<-D03CatTotal[goodmatch[,2],]
D03Stars<-D03Stars[goodmatch[,1],]

GuidesD03<-which(D03CatTotal$edge_frac>=0.0)

RA<-c(GuideTabRA,D03CatTotal[GuidesD03,'RAmax'])
DEC<-c(GuideTabDEC, D03CatTotal[GuidesD03,'Decmax'])
MAG<-c(GuideTabRMAG, D03Stars[GuidesD03,'RPETROMAG'])
ROWID<-seq(1,length(RA),1)

GuideTab<-data.frame(ROWID,RA, DEC, MAG)
print(dim(GuideTab))

USNOD10<-read.table('USNO-B1_COSMOS.txt', header=T, sep='|')
USNOD02<-read.table('USNO-B1_XMMLSS.txt', header=T, sep='|')
USNOD03<-read.table('USNO-B1_ECDFS.txt', header=T, sep='|')

USNO<-rbind(USNOD10,USNOD02,USNOD03)

USNO<-USNO[which(abs(USNO[,'pmRA'])<15),]

tempmatch=coordmatch(GuideTab[,c('RA','DEC')],USNO[,c("RAJ2000","DEJ2000")], rad = 2)
good=tempmatch$bestmatch$refID %in% seq(1,length(GuideTab$RA),1)
goodmatch=tempmatch$bestmatch[good,]

GuideTab<-GuideTab[goodmatch[,1],]
print(dim(GuideTab))

#Bad<-as.vector(read.table('GuidesVISBad.txt'))


#GuideTab<-GuideTab[which(GuideTab$ROWID %in% Bad[,1]==FALSE),]

print(dim(GuideTab))

write.table(GuideTab, file='DATAguide.tab', row.names=F)



