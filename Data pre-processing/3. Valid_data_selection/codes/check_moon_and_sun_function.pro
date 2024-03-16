
; Explanation:
;    Check the moon's and sun's informations and
;    determine whether they are in appropriate
;    condition for the optical observation (JBS-AASC).
;    For the moon, the criteria are
;      elevation angle<5 & phase<0.25
;    For the sun, the criterion is
;      elevation angle < -15
function check_moon_and_sun_function,yr,mon,day,hh,mm
  
  lat=-74.62d & lon=164.23d
      
  num_time=n_elements(hh)
  hh=fix(hh)
  mm=fix(mm)
  
  moon_ele=(sun_ele=(moon_phase=dblarr(num_time)))+!values.d_NaN
  for j=0L,num_time-1 do begin
    dat=calculate_moon_and_sun_position(lat[0], lon[0], yr[0], mon[0], day[0], hh[j], mm[j])
    moon_ele[j]=dat.ELE_moon_deg
    sun_ele[j]=dat.ELE_sun_deg
    moon_phase[j]=dat.moon_phase
  endfor
  
  if mean(moon_phase) lt 0.25 then begin
    return,where(sun_ele lt -15)
  endif else begin
    return,where(moon_ele lt 5 and sun_ele lt -15)
  endelse
end