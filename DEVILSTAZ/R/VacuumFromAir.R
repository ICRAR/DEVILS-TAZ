# Convert from vacuum wavelengths to air wavelengths by interpolating
# from a lookup table.
# Equation from SDSS web page 
# http://www.sdss.org/dr7/products/spectra/vacwavelength.html. 
# Reference to Morton (1991, ApJS, 77, 119). 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
VacuumFromAir = function(air){
  
  vacLookup <- 0:7999 + 3500
  convertLookup <- (1.0 + 2.735182E-4 + 131.4182 / vacLookup^2 + 2.76249E8 / vacLookup^4)
  airLookup <- vacLookup / convertLookup
  convert <- approx(x = airLookup, y = convertLookup, xout = air, method = "linear",rule=2)$y
  vac <- air * convert
  
  return = vac
}

# Convert from vacuum wavelengths to air wavelengths.
# Equation from SDSS web page 
# http://www.sdss.org/dr7/products/spectra/vacwavelength.html. 
# Reference to Morton (1991, ApJS, 77, 119). 
# Written by Ivan Baldry.
# Translated to R by Leon Drygala
AirFromVacuum = function(vac){
  air <- vac / (1.0 + 2.735182E-4 + 131.4182 / vac^2 + 2.76249E8 / vac^4)
  return = air
}