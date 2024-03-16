;It calculates GMST.
;output will be returned in hh:mm:ss string format


function cal_GMST,mon,day,yr,hr,min,sec
  JD=julday(mon,day,yr,hr,min,sec)
;;IAU1982
  GMST = 280.46061837d + 360.98564736629d*(JD - 2451545d) + 0.000387933d*$
    ((JD - 2451545d)/36525d)^2 - ((JD - 2451545d)/36525d)^3/38710000d 

;;;IAU2006
;  Du = JD-2451545d
;  T=Du/36525d
;  frac_Du=Du mod floor(Du)
;  GMST = 1296000d/3600d*(0.7790572732640d + 0.00273781191135448d * Du + frac_Du)+$
;    (0.014506d + (4612.156534d + (1.3915817d + (-0.00000044d + (-0.000029956d $
;    -3.68d * 10^(-8.)*T)*T)*T)*T)*T)/3600d
 ; GMST=280.46061837d + 360.98564736629d*(julday(mon,day,yr,hr,min,sec)-2451545d)
  while GMST gt 360 do begin
    GMST=GMST-360d
  endwhile
  while GMST lt 0 do begin
    GMST=GMST+360d
  endwhile
  GMST_hr=GMST/15d
  hh=floor(GMST_hr) & mm=floor((GMST_hr-hh)*60d) & ss=((GMST_hr-hh)*60d - mm)*60d
  GMST_str=string(hh,format='(i2.2)')+':'+string(mm,format='(i2.2)')+':'+$
    string(ss,format='(f5.2)')
  return,GMST_str
end