
;# REFERENCE
  ; http://www.stargazing.net/kepler/sun.html     (Sun's position)
  ; http://www2.arnes.si/~gljsentvid10/moon.html  (Moon's position)
  ; >>> QBASIC codes have been conveted to IDL codes.
  ; https://www.subsystems.us/uploads/9/8/9/4/98948044/moonphase.pdf (Moon phase calculation)
  ; https://planetcalc.com/524/ (Moon phase calculation)
  ; Peter Duffertt-Smith and Jonathan Zwart, 'Practical Astronomy with your Calculator or Spreadsheet', p.171 (Moon phase calculation)
;#

;# EXPLANATION
;'*********************************************************
;'   Moon positions to a quarter of a degree on the sky
;'   From page D76 of Astronomical Almanac
;'   
;'   max RA error 97 (seconds of time), rms error 22 s 
;'   max dec error 811 arcseconds, rms error 224 arcseconds
;' compared to Integrated Computer Ephemeris for 18.6 years
;' either side of J2000
;'
;'*********************************************************
;'*********************************************************
;'   This program will calculate the position of the Sun
;'   using a low precision method found on page C24 of the
;'   1996 Astronomical Almanac.
;'
;'   The method is good to 0.01 degrees in the sky over the
;'   period 1950 to 2050.
;'
;'   QBASIC program by Keith Burnett (kburnett@geocity.com)
;'
;'
;'   Work in double precision and define some constants
;'*********************************************************
;#


;'
;'   FNday only works between 1901 to 2099 - see Meeus chapter 7
;'
function FNday, y, m, d, h
  RETURN,367 * y - 7 * (y + (m + 9) / 12) / 4 + 275 * m / 9 + d - 730531.5 + h/ 24  
end
;'
;'   the atn2 function below RETURNs an angle in the range 0 to two pidepending on the signs of x and y.
;'
function FNatn2, y, x
  a = ATAN(y / x)
  IF x lt 0 THEN a = a + !dpi
  IF y lt 0 AND x gt 0 THEN a = a + 2*!dpi
  RETURN,a
end
;'
;'   the function below RETURNs the true integer part, even for negative numbers
;'
function FNrange, x  
 RETURN,x - FIX(x / (2 * !dpi)) * 2 * !dpi
end
;
;   the function below RETURNs the true integer part, even for negative numbers
;
function FNipart, x
  IF x GT 0 THEN BEGIN
    RETURN, FIX(ABS(x))
  ENDIF ELSE IF x LT 0 THEN BEGIN
    RETURN, -FIX(ABS(x))
  ENDIF ELSE BEGIN
    RETURN, 0
  ENDELSE
  
end
;
;   Find the ecliptic longitude of the Sun
;
function FNsun, d
;   mean longitude of the Sun
L = FNrange(280.461d * !dpi / 180 + .9856474d * !dpi / 180 * d)
;   mean anomaly of the Sun
g = FNrange(357.528d * !dpi / 180 + .9856003d * !dpi / 180 * d)
;   Ecliptic longitude of the Sun
  RETURN, FNrange(L + 1.915d * !dpi / 180 * SIN(g) + .02d * !dpi / 180 * SIN(2 * g))
;
;   Ecliptic latitude is assumed to be zero by definition
;
end

function calculate_moon_and_sun_position, glat, glong, y, m, day, h, mins
;'
;'    get the date and planet number from the user
;'
;print, "    lat : " & read, glat
;print, "   long : " & read, glong
;print, "  year  : " & read, y
;print, "  month : " & read, m
;print, "  day   : " & read, day
;print, "hour UT : " & read, h
;print, " minute : " & read, mins

glat=double(glat)
glong=double(glong)
y=long(y)
m=long(m)
day=double(day)
h=double(h)
mins=double(mins)
;glat=-74.62d;-1.91667d
;glong=164.23d;52.5d
;y=2020L
;m=9L
;day=5d
;h=0d
;mins=0d

h = h + mins / 60
d = FNday(y, m, day, h)
t = d / 36525

; ########## CALCULATE MOON's POSITION
;'
;'   calculate the geocentric longitude
;' writing the coefficients in the series as angles in !dpi / 180
;' would save 29 multiplications per position!
;'
l = FNrange(!dpi / 180 * (218.32d + 481267.883d * t))
l = l + !dpi / 180 * 6.29d * SIN(FNrange((134.9d + 477198.85d * t) * !dpi / 180))
l = l - !dpi / 180 * 1.27d * SIN(FNrange((259.2d - 413335.38d * t) * !dpi / 180))
l = l + !dpi / 180 * .66d * SIN(FNrange((235.7d + 890534.23d * t) * !dpi / 180))
l = l + !dpi / 180 * .21d * SIN(FNrange((269.9d + 954397.7d * t) * !dpi / 180))
l = l - !dpi / 180 * .19d * SIN(FNrange((357.5d + 35999.05d * t) * !dpi / 180))
l = l - !dpi / 180 * .11d * SIN(FNrange((186.6d + 966404.05d * t) * !dpi / 180))
l = FNrange(l)
;'
;'   calculate the geocentric latitude
;'
bm = !dpi / 180 * 5.13d * SIN(FNrange((93.3d + 483202.03d * t) * !dpi / 180))
bm = bm + !dpi / 180 * .28d * SIN(FNrange((228.2d + 960400.87d * t) * !dpi / 180))
bm = bm - !dpi / 180 * .28d * SIN(FNrange((318.3d + 6003.18d * t) * !dpi / 180))
bm = bm - !dpi / 180 * .17d * SIN(FNrange((217.6d - 407332.2d * t) * !dpi / 180))
;'
;'   get the parallax
;'
gp = .9508d * !dpi / 180
gp = gp + !dpi / 180 * .0518d * COS(FNrange((134.9d + 477198.85d * t) * !dpi / 180))
gp = gp + !dpi / 180 * .0095d * COS(FNrange((259.2d - 413335.38d * t) * !dpi / 180))
gp = gp + !dpi / 180 * .0078d * COS(FNrange((235.7d + 890534.23d * t) * !dpi / 180))
gp = gp + !dpi / 180 * .0028d * COS(FNrange((269.9d + 954397.7d * t) * !dpi / 180))
;'
;'   from the parallax, get the semidiameter and the radius vector
;'
sdia = .2725d * gp
rm = 1d / (SIN(gp))
xg = rm * COS(l) * COS(bm)
yg = rm * SIN(l) * COS(bm)
zg = rm * SIN(bm)
;'
;'   rotate to equatorial coords
;'   obliquity of ecliptic
;'
ecl = (23.4393d - 3.563E-07 * d) * !dpi / 180

xe = xg
ye = yg * COS(ecl) - zg * SIN(ecl)
ze = yg * SIN(ecl) + zg * COS(ecl)
;'
;'   geocentric RA and Dec
;'
ra = FNatn2(ye, xe)
dec = ATAN(ze / SQRT(xe * xe + ye * ye))
;'
;'   topocentric RA and DEC (spherical earth)
;'   Local Siderial Time in degrees
;'
lst = 100.46d + .985647352d * d + h * 15d + glong
glat = glat * !dpi / 180
glong = glong * !dpi / 180
lst = FNrange(lst * !dpi / 180)
xtop = xe - COS(glat) * COS(lst)
ytop = ye - COS(glat) * SIN(lst)
ztop = ze - SIN(glat)
rtop = SQRT(xtop * xtop + ytop * ytop + ztop * ztop)
ratop = FNatn2(ytop, xtop)
dectop = ATAN(ztop / SQRT(xtop * xtop + ytop * ytop))
;'
;'   topocentric distance
;'
rmtop = SQRT(xtop * xtop + ytop * ytop + ztop * ztop)
;'
;'   print output in readable form - the 'formatting strings'
;' only work with Microsoft basics and FirstBas compiler
;' remove them for other basics.
;'
;PRINT,''
;PRINT,"days from J2000 ....................... ", d
;PRINT,''
;PRINT, "Position of Moon"
;PRINT, "==============="
;;PRINT,"geocentric ecliptic coords (l,b) ...... ", l * 180 / !dpi, bm * 180 / !dpi
;;PRINT,"lunar parallax (deg) .................. ", gp * 180 / !dpi
;;PRINT,"diameter (arcmin) ..................... ", sdia * 180 / !dpi * 60 * 2
;;PRINT,"geocentric distance (lun dia) ......... ", rm
;;PRINT,"equator of date (deg) ................. ", ecl * 180 / !dpi
;PRINT,"geocentric equatorial coords (ra [Hour], dec [Degree]). ", ra * 180 / !dpi / 15, ' h', dec * 180 / !dpi, ' deg.'
;;PRINT,"local siderial time (hrs) ............. ", lst * 180 / !dpi / 24
;;PRINT,"topocentric equatorial coords (ra, dec) ", ratop * 180 / !dpi / 15, dectop * 180 / !dpi
;;PRINT,"topocentric lunar distance (lun dia) .. ", rmtop
;PRINT, "==============="
;; ####################################
;; ########### CALCULATE SUN's POSITION

; Use FNsun to find the ecliptic longitude of the Sun
lambda = FNsun(d)

;   mean longitude of the Sun
L = FNrange(280.461d * !dpi / 180 + .9856474d * !dpi / 180 * d)
;   mean anomaly of the Sun
g = FNrange(357.528d * !dpi / 180 + .9856003d * !dpi / 180 * d)

; Obliquity of the ecliptic
obliq = 23.439d * !dpi / 180 - .0000004d * !dpi / 180 * d

; Find the RA and DEC of the Sun
alpha = FNatn2(COS(obliq) * SIN(lambda), COS(lambda))
delta = ASIN(SIN(obliq) * SIN(lambda))

; Find the Earth - Sun distance
r = 1.00014d - .01671d * COS(g) - .00014d * COS(2 * g)

; Find the Equation of Time
equation = (L - alpha) * 180 / !dpi * 4

;;   print results in decimal form
;PRINT, "Position of Sun"
;PRINT, "==============="
;;PRINT, "     days : ", d
;;PRINT, "longitude : ", lambda * 180 / !dpi
;PRINT, "equatorial coords (ra [Hour], dec [Degree]).: ", alpha * 180 / !dpi / 15, ' h' ,delta * 180 / !dpi, ' deg.'
;;PRINT, " distance : ", r
;;PRINT, "eq time   : ", equation
;PRINT, "==============="

LST_hour=LST * 180 / !dpi / 15

RA_sun_hour=alpha * 180 / !dpi / 15
DEC_sun_deg=delta * 180 / !dpi
HA_sun=LST_hour - RA_sun_hour
AZI_sun_deg= ATAN(-COS(delta) * SIN(HA_sun / 24 * 2 * !dpi), SIN(delta) * COS(glat) - COS(delta) * COS(HA_sun / 24 * 2 * !dpi) * SIN(glat)) / !dpi * 180
ELE_sun_deg= ASIN(SIN(delta) * SIN(glat) + COS(delta) * COS(HA_sun / 24 * 2 * !dpi) * COS(glat)) / !dpi * 180

RA_moon_hour=ra * 180 / !dpi / 15
DEC_moon_deg=dec * 180 / !dpi
HA_moon=LST_hour - RA_moon_hour
AZI_moon_deg= ATAN(-COS(DEC) * SIN(HA_moon / 24 * 2 * !dpi), SIN(DEC) * COS(glat) - COS(DEC) * COS(HA_moon / 24 * 2 * !dpi) * SIN(glat)) / !dpi * 180
ELE_moon_deg= ASIN(SIN(DEC) * SIN(glat) + COS(DEC) * COS(HA_moon / 24 * 2 * !dpi) * COS(glat)) / !dpi * 180
; ####################################

;;   print results in decimal form
;PRINT,'sun (Azi, Ele): ',AZI_sun_deg,ELE_sun_deg
;PRINT,'moon (Azi ELe): ',AZI_moon_deg,ELE_moon_deg

; ########### CALCULATE MOON PHASE

;; Not precise
;if y eq 1 or y eq 2 then y=y-1 & m=m+12
;A=long(y / 100)
;B=long(A / 4)
;C=2d - A + B
;E=long(365.25d * (y + 4716))
;F=fix(30.6001d * (m + 1))
;JD=C + D + E + F - 1524.5d
;day_since_new_moon=JD - 2451549.5d
;;

JD=JULDAY(m,day,y,h)
day_since_new_moon=JD - 2451550.1d
num_new_moon=day_since_new_moon/29.530588853d
phase=num_new_moon-LONG(num_new_moon)
phase=(1-cos(phase*2*!dpi))/2
;print,(1-cos(phase*2*!dpi))/2
;if phase gt 0.5d then begin
;  phase=phase-0.5
;  phase=0.5-phase
;endif
;phase=phase*2
;PRINT,'Moon Phase= ',phase
; ################################

RETURN, create_struct('AZI_moon_deg',AZI_moon_deg,'ELE_moon_deg',ELE_moon_deg,'moon_phase',phase,'AZI_sun_deg',AZI_sun_deg,'ELE_sun_deg',ELE_sun_deg)

end