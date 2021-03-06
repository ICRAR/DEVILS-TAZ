#' Make Template Nightly Logs for DEVILS
#'
#' @description Function produces template night logs for DEVILS obsering
#' for a given run and date.
#' 
#' @param run the run date in the format run#_YYYY_MM
#' @param date the date in the format YYYY_MM_DD
#' @param fileName the output log filename
#' 
#' @examples 
#' makeLog(run='run1_2017_12', date='2017_12_18',fileName='data/observing/run1_2017_12/2017_12_18/2017_12_18_DEVILS_obs_log.txt')
#' @export
makeLog<-function(run=run, date=date, fileName=fileName){

fileConn<-file(fileName)
writeLines('=================================================', fileConn)
           close(fileConn)
           
write(paste('DEVILS Run:',run,' Date:',date,' Nightly Log', sep=''), file=fileName, append=T)
write('=================================================', file=fileName, append=T)			   
write(' ', file=fileName, append=T)
write(' ', file=fileName, append=T)
write('Observers: **FILL IN**', file=fileName, append=T)
write('Support Astronmer: **FILL IN**', file=fileName, append=T)
write('Night Assistant: **FILL IN**', file=fileName, append=T)
write(' ', file=fileName, append=T)

write('Conditions at start: 	**FILL IN**', file=fileName, append=T)
write('Conditions at end: 	**FILL IN**', file=fileName, append=T)
write('Useful fraction:	**FILL IN**', file=fileName, append=T)
write('Twilight: 		**FILL IN**', file=fileName, append=T)
write('Seeing: 		**FILL IN**', file=fileName, append=T)
write(' ', file=fileName, append=T)
write(' ', file=fileName, append=T)
write('Fibres: 393 on Plate 0, 395 on Plate 1', file=fileName, append=T)
write('Central Wavelength: 	6000A', file=fileName, append=T)
write('Central Blue: 		4801A', file=fileName, append=T)
write('Blaze Blue:		4801A', file=fileName, append=T)
write('Central Red:		7250A', file=fileName, append=T)
write('Blaze Red:		7250A', file=fileName, append=T)
write('Gratings: 		580V, 385R', file=fileName, append=T)
write('ARC lamps:		Standard (FeAr_1, FeAr_2, CuAr_1, CuAr_2, CuHe_1, CuNe_1)', file=fileName, append=T)
write('FLAT lamp:		Quartz_20_2, 75W', file=fileName, append=T)
write(' ', file=fileName, append=T)
write(' ', file=fileName, append=T)

write('*** EXAMPLES PLEASE AMMEND ***', file=fileName, append=T)
write('Field    Configuration                         Plate    Start    End    Frames    Targets    Comment', file=fileName, append=T)
write('D10	D10_Y2017_SB_R1tile002-149.98-2.25P1  0        18:28    20:43  3x2400s   xxx        xxx    ', file=fileName, append=T)
write(' ', file=fileName, append=T)
write(' ', file=fileName, append=T)

write('*** EXAMPLES PLEASE AMMEND ***', file=fileName, append=T)
write('FileName       Configuration                         Type    Comment ', file=fileName, append=T)
write('18dec10001-30  NA                                    bias            ', file=fileName, append=T)
write('18dec10031     D10_Y2017_SB_R1tile002-149.98-2.25P1  flat    JUNK    ', file=fileName, append=T)
write('18dec10032     D10_Y2017_SB_R1tile002-149.98-2.25P1  flat    Good    ', file=fileName, append=T)
write('18dec10033     D10_Y2017_SB_R1tile002-149.98-2.25P1  arc             ', file=fileName, append=T)
write('18dec10034     D10_Y2017_SB_R1tile002-149.98-2.25P1  target  1800sec ', file=fileName, append=T)
write('18dec10035     D10_Y2017_SB_R1tile002-149.98-2.25P1  target  1800sec ', file=fileName, append=T)
write('18dec10036     D10_Y2017_SB_R1tile002-149.98-2.25P1  target  1800sec ', file=fileName, append=T)
}
