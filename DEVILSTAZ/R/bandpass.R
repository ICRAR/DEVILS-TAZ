bandpass=function(spec, filter, lum = T){
  wave<-spec$wave
  flux<-spec$flux
  tempfunc = approxfun(x = filter[, 1], y = abs(filter[, 2]))
  tempremap = tempfunc(wave)
  tempremap[is.na(tempremap)] = 0
  flux[is.na(flux)]= 0
  if (lum) {
    return = sum(tempremap * wave * flux)/sum(tempremap * wave)
  }
  else {
    return = tempremap * wave * flux/sum(tempremap * wave)
  }
}
