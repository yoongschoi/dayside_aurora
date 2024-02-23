; New version for capability of process any ASC images

pro find_coefficients_new,coef_a,coef_b,poly_coe,fit_type,lat_ob,lon_ob
  lat=(lon='')
  print,'Enter the geographic latitude of the observation site in degress (North: +, South: -):' & read,lat
  print,'Enter the geographic longitude of the observation site in degress (East: +, West: -):' & read,lon
  lat=double(lat) & lon=double(lon)
  lat_ob=lat & lon_ob=lon
  
  UT=(xy_dir=(RaDec_dir=''))
  print,'Enter the time information of the image in UT (YYYY/MM/DD HH:MM:SS)'
  read,UT  
  UTspl=strsplit(UT,':/() ',/extract)
  UT_jday=JULDAY(UTspl[1],UTspl[2],UTspl[0],UTspl[3],UTspl[4],UTspl[5])
  
  
  input_fname=''
  print,'Input text file must have parameters and the form like below.'
  print,'------Example of the input file------'
  print,'No. RA Dec pixel_x pixel_y'
  print,'001 20h27m2.06s '+"-56°40'"+'29.4" 119 172'
  print,'002 0h26m29.63s '+"-77°10'"+'01.6" 164 149'
  print,'                 .                    '
  print,'                 .                    '
  print,'                 .                    '
  print,'-------------------------------------'
  print,'Enter the full path of the text file which contains the information of the selected standard stars:'
  read,input_fname
  
  input_fname=file_search((strsplit(input_fname,"'",/ext))[0],count=num_file)
  if num_file eq 0 then message,'Check the input file path.'
  fline=file_lines(input_fname[0])
  
  rline=''
  openr,lunr,input_fname[0],/get_lun
  readf,lunr,rline ; Skip header line
  for i=1,fline-1 do begin
    readf,lunr,rline
    if rline eq '' then break
    spl_rline=strsplit(rline,' '+string(9B),/ext) ; 9B is tab character
    if i eq 1 then begin
      RA=hr2deg(strjoin(strsplit(spl_rline[1],'hms',/ext),':'))
      Dec=hr2deg(strjoin(strsplit(spl_rline[2],"°'"+'"',/ext),':'))/15
      x=fix(spl_rline[3])
      y=fix(spl_rline[4])
    endif else begin
      RA=[RA,hr2deg(strjoin(strsplit(spl_rline[1],'hms',/ext),':'))]
      Dec=[Dec,hr2deg(strjoin(strsplit(spl_rline[2],"°'"+'"',/ext),':'))/15]
      x=[x,fix(spl_rline[3])]
      y=[y,fix(spl_rline[4])]
    endelse
  endfor
  free_lun,lunr
  x=transpose(x) & y=transpose(y)
  
  
; ---Calculate Local Sidereal Time (LST).  
  caldat,UT_jday,mon,day,yr,hr,min,sec
  GMST=cal_GMST(mon,day,yr,hr,min,floor(sec))
  GMSTsplit=strsplit(GMST,':',/extract)
  LST=hr2deg(GMSTsplit[0]+' '+GMSTsplit[1]+' '+GMSTsplit[2])+lon
; ---
  
  
; ---Calculate azimuth and elevation of each standard star.
  H=(LST)-Ra
  ele=asin(sin(Dec*!dtor)*sin(lat*!dtor)+cos(Dec*!dtor)*cos(lat*!dtor)*cos(H*!dtor))
  azi=atan(-cos(Dec*!dtor)*cos(lat*!dtor)*sin(H*!dtor),(sin(Dec*!dtor)-sin(lat*!dtor)*sin(ele)))
  azi(Where(azi lt 0))=azi(where(azi lt 0))+2*!pi
; ---
  find_coefficients_linear_routine,result,azi,ele,x,y
  a=result.coef_a & b=result.coef_b
  fn=a[0]+a[1]*x+a[2]*y & gn=b[0]+b[1]*x+b[2]*y
  coe=1d
  ele_lin_assump=!pi/2.*coe*(1-sqrt(fn^2+gn^2))
  zenx=result.zenith_xy[0] & zeny=result.zenith_xy[1]
;; Check the relation between distance and elevation
  dis=sqrt((zenx-x)^2d +(zeny-y)^2d)
  screen_dimen=get_screen_size(resolution=resolution)
  win=window(location=[screen_dimen[0]-500,0],dimensions=[500,500])
  sym=plot(ele*!radeg,dis,symbol='D',xrange=[0,90],yrange=[0,300],/xstyle,/ystyle,xtitle='elevation(degree)',$
  ytitle='distance(pixels)',title='Comparison of Lens Functions',linestyle=6,name='Real data',/current,color='black',$
    xtickinterval=30,xminor=2)
  lin_coe=linfit(ele_lin_assump,dis,chisq=chisq,/double)
  poly_coe=1d
  xx=dindgen(100)/99*!pi/2d & yy=lin_coe[0]+lin_coe[1]*xx
  fig_lin=plot(xx*!radeg,yy,/current,/overplot,name='Linearity assumption',color='red')
  l=legend(target=[sym,fig_lin],position=[0.9,0.86],/normal)
  yrange=[0,max(yy)]
  sym.yrange=yrange
  
  print,'Is the relation linear?(1:Yes, else:No)'
  read,ans
  if ans ne 1 then begin
    fig_lin.close
    ;; Find lens function using 3rd polynomial fitting
    poly_coe=poly_fit(ele,dis,3,chisq=chisq,/double)
    pyy=poly_coe[0]+xx*poly_coe[1]+xx^2*poly_coe[2]+xx^3*poly_coe[3]
    win=window(location=[screen_dimen[0]-500,0],dimensions=[500,500])
    sym=plot(ele*!radeg,dis,symbol='D',xrange=[0,90],yrange=yrange,/xstyle,/ystyle,xtitle='elevation(degree)',$
    ytitle='distance(pixels)',title='Comparison of Lens Functions',linestyle=6,name='Real data',/current)
    fig_lin=plot(xx*!radeg,yy,/current,/overplot,name='Linearity assumption',color='red',xtickinterval=30,xminor=2)
    fig_pol_3rd=plot(xx*!radeg,pyy,color='gold',/current,/overplot,name='3rd order poly. fitting')
    l=legend(target=[sym,fig_lin,fig_pol_3rd],position=[0.9,0.86],/normal)
    ;;
    poly_coe=poly_coe/poly_coe[0]
    find_coefficients_3rd_poly_routine,result,azi,ele,x,y,poly_coe
  endif
  coef_a=result.coef_a[0:2] & coef_b=result.coef_b[0:2] & fit_type=result.fit_type
end
