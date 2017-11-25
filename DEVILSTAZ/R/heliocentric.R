#+
# NAME:
#   heliocentric
#
# PURPOSE:
#   Compute correction term to add to velocities to convert to heliocentric.
#
# CALLING SEQUENCE:
#   vcorr = heliocentric( ra, dec, [ epoch, jd=, tai=, $
                                         #    longitude=, latitude=, altitude= ] )
#
# INPUTS:
#   ra             - Right ascension [degrees]
#   dec            - Declination [degrees]
#   epoch          - Epoch of observation for RA, DEC# default to 2000.
#
# OPTIONAL KEYWORDS:
#   jd             - Decimal Julian date.  Note this should probably be
#                    type DOUBLE.
#   tai            - Number of seconds since Nov 17 1858# either JD or TAI
#                    must be specified.  Note this should probably either
#                    be type DOUBLE or LONG64.
#   longitude      - Longitude of observatory#
#                    default to (360-105.820417) deg for APO
#   latitute       - Latitude of observatory# default to 32.780361 deg for APO
#   altitude       - Altitude of observatory in meters#
#                    default to 2788 m for APO
#
# OUTPUTS:
#   vcorr          - Velocity correction term, in km/s, to add to measured
#                    radial velocity to convert it to the heliocentric frame.
#
# OPTIONAL OUTPUTS:
#
# COMMENTS:
#
# EXAMPLES:
#
# BUGS:
#
# PROCEDURES CALLED:
#   baryvel
#   ct2lst
#
# REVISION HISTORY:
#   09-May-2000  Written by S. Burles & D. Schlegel
#   2010ish      Sign changed in return statement by I. Baldry.
#   Mar 2015     Translated to R by Leon Drygala?
#-
#------------------------------------------------------------------------------

Heliocentric = function(ra, dec, epoch = 2000.0, jd = FALSE, tai = FALSE, longitude = 360 - 105.820417, 
                        latitude = 32.780361, altitude = 2788){

  
  if (!jd && tai){
    jd <- 2400000 + tai / (24*3600)
  } else if (!jd){
    cat('\n *** Heliocentric ***Must specify either JD or TAI\n')
    return = 0
  }

  
  DRADEG <- 180 / pi
  
  #----------
  # Compute baryocentric velocity
  
  r <- baryvel(jd, epoch)
  dvelh <- r$dvelh
  dvelb <- r$dvelb

  # Project velocity toward star
  vbarycen <- dvelb[1]*cos(dec/DRADEG)*cos(ra/DRADEG) + 
    dvelb[2]*cos(dec/DRADEG)*sin(ra/DRADEG) + dvelb[3]*sin(dec/DRADEG) 
  
  #----------
  # Compute rotational velocity of observer on the Earth
  
  # LAT is the latitude in radians.
  latrad <- latitude / DRADEG
  
  # Reduction of geodetic latitude to geocentric latitude (radians).
  # DLAT is in arcseconds.
  
  dlat    <- -(11 * 60 + 32.743000) * sin(2 * latrad) + 
    1.163300 * sin(4 * latrad) -0.002600 * sin(6 * latrad)
  latrad  <- latrad + (dlat / 3600) / DRADEG
  
  # R is the radius vector from the Earth's center to the observer (meters).
  # VC is the corresponding circular velocity
  # (meters/sidereal day converted to km / sec).
  # (sidereal day = 23.934469591229 hours (1986))
  
  r <- 6378160 * (0.998327073 + 0.00167643800 * cos(2 * latrad) - 
                    0.00000351 * cos(4 * latrad) + 0.000000008 * cos(6 * latrad)) + altitude
  vc <- 2 * pi * (r / 1000)  / (23.934469591229 * 3600)
  
  # Compute the hour angle, HA, in degrees
  LST = ct2lst(longitude, 0, jd) # TODO 0 to make three params?

  LST <- 15 * LST # convert from hours to degrees
  HA <- LST - ra
  
  # Project the velocity onto the line of sight to the star.
  vrotate <- vc * cos(latrad) * cos(dec/DRADEG) * sin(HA/DRADEG)
  
  return = (vbarycen + vrotate)
  ##   return, vbarycen 
}                                                      