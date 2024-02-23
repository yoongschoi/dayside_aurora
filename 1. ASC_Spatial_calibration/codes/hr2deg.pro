;This function receives time(which represents angle) and then returns corresponding angle in degree
;only scalar input available
;

function hr2deg,time
  if strmatch(time,'*-*') eq 0 then sign=1 else sign=-1
  timesplit=double(strsplit(time,'- :',count=num,/extract))
  degree=0
  for i=0,num-1 do begin
    if i eq 0 then degree=degree+timesplit[i] else if i eq 1 then $
    degree=degree+timesplit[i]/60d else degree=degree+timesplit[i]/3600d
    ;degree=sign*(timesplit[0]+timesplit[1]/60+timesplit[2]/3600)*15
  endfor
  return,sign*degree*15d
end