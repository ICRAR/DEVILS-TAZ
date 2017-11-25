plotLines<-function(z=0, xunit='ang', labPos=100,lty=2, cex=0.5, labOff=-50, EmCol='blue', AbsCol='darkgreen'){


    lines<-load.lines()
    
    if (xunit=='ang') {line_x <- as.numeric(lines$wave_ang)*(1+z)}
    if (xunit=='hz') {line_x <- as.numeric(lines$freq_hz)/(1+z)}
    if (xunit=='micron') {line_x <- as.numeric(lines$wave_micron)*(1+z)}
    if (xunit=='m') {line_x <- as.numeric(lines$wave_m)*(1+z)}
    if (xunit=='nm') {line_x <- as.numeric(lines$wave_nm)*(1+z)}

    
    
    for (i in 1:length(lines$names)) {
        if (lines$stellar[i]==F) {
            
            abline(v=line_x[i], col=EmCol, lty=lty)
            text(line_x[i]+labOff, labPos, lines$names[i], col=EmCol,cex=cex, srt=270)
        }
        if (lines$stellar[i]==T) {
            abline(v=line_x[i], col=AbsCol, lty=lty)
            text(line_x[i]+labOff, labPos, lines$names[i], col=AbsCol,cex=cex, srt=270)
        }
    }
    
   
    
          


}

