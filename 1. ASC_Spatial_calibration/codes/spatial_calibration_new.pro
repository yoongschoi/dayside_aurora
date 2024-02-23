
; NAME: SPATIAL_CALIBRATION_NEW
; PURPOSE:
;     Performs the spatial calibration for the All Sky Camera images.
; EXPLANATION:
;     It performs the spatial calibration for the All Sky Camera image data.
;     The spatical calibration is an essential process that must done before
;     analyzing the data. The first version of the code is written based on
;     the KSS Airglow ASC data. After several updates, it looks like it can
;     perform the spatial calibration for any ASC data.
;     - List of the tested data
;     1) KSS Airglow ASC
;     2) JBS KASI (Airglow) ASC
;     3) JBS Aurora ASC
; INPUTS:
;     ASCII file that contains the information (pixel coordinates & equatorial
;     coordinates) of the slected standard stars.
; RETURNS:
;     Generate output files in the NetCDF format containing the result of the
;     spatial calibration.
; HISTORY:
;     First version made: ???/??/2014
;     Final version made: Mar/??/2020
; -
; ## Emission height
; NA-5893: typical peak -> ~92 km (ref.: LONG TERM TREND IN NOCTURNAL AIRGLOW EMISSION OF 589.3 nm OVER MID LATITUDE JAPANESE STATION)
; N2+-4278: typical peak -> ~130 km [very energetic P.P. -> ~ 80km & very weak P.P. -> ~200 km] (ref.: Photometric observations of 630.0‐nm OI and
;                                                                                                427.8‐nm N2+ emission from South Pole and McMurdo
;                                                                                                Stations during winter: Analysis of temporal
;                                                                                                variations spanning minutes to hourly timescales)
; ##
; -

pro Spatial_calibration_new
  
;- Specify the information of the ASC to process

;; For KSS Airglow ASC
;  ASC_name='KSS_Airglow_ASC'
;  filt_arr=['OH','OI-5577','OI-6300','NO-FILTER']
;  Emission_Height_arr=[85,97,250,97] ;Last one is for No-Filter.
;;

;; For JBS KASI (Airglow) ASC
;  ASC_name='JBS_KASI_ASC'
;  filt_arr=['OI-5577','OI-6300','NO-FILTER'] ; OI-6343 line doesn't exist. (ref. https://physics.nist.gov/PhysRefData/ASD/lines_form.html)
;  Emission_Height_arr=[97,250,97] ;Last one is for No-Filter.
;;
;; For JBS Aurora ASC (Because AASC doesn't use any optical filter, the emission height is arbitrary)
  ASC_name='JBS_Aurora_ASC'
  filter_arr=['NO-FILTER']
  Emission_Height_arr=[97]
;;
; - 

; Specify the output directory
  out_parent_dir='E:\JBS_AASC_spatial_calibration_data\'
  out_parent_dir=strjoin(strsplit(out_parent_dir,'/\',/ext),path_sep())+path_sep()
;



  file_mkdir,out_parent_dir
  calib_data_fname=out_parent_dir+ASC_name+'_spatial_calibration_data_new.nc'
  
  
  print,'Enter the x-size of the image:' & read,img_xs
  print,'Enter the y-size of the image:' & read,img_ys
;  file_mkdir,out_parent_dir
  ;No-Filter has no emission height.
  RE=double(6370) ; Radius of the Earth. 6370km
  x=lindgen(img_xs,img_ys) mod img_xs
  y=transpose(lindgen(img_ys,img_xs)) mod img_ys
  x=double(x) & y=double(y)
  
  find_coefficients_new,a,b,coe,fit_type,lat_ob,lon_ob
  fn=a[0]+a[1]*x+a[2]*y & gn=b[0]+b[1]*x+b[2]*y
  azi=atan(fn,gn)
  if fit_type eq 'linear' then begin
    ele=!pi/2.*coe*(1-sqrt(fn^2+gn^2))
  endif else if fit_type eq '3rd_poly' then begin
    print,'Lens function is 3rd polynomial.'
    print,'Start Newton-rhapson method to find elevation of each pixel.'
    ele=dblarr((size(fn,/dimen))[0],(size(fn,/dimen))[1])
    eps=10.^(-10d)
    for i=0,(size(fn,/dimen))[0]-1 do begin
      for j=0,(size(fn,/dimen))[1]-1 do begin
        if sqrt((fn[i,j])^2+(gn[i,j])^2) gt 1.0 then begin
        ;  ele[i,j]=!values.f_NaN
          goto,next
        endif
        p0=!pi/2d
        while 1 do begin
          pn=p0-(coe[0]+coe[1]*p0+coe[2]*p0^2+coe[3]*p0^3-sqrt((fn[i,j])^2+(gn[i,j])^2))/(coe[1]+2*coe[2]*p0+3*coe[3]*p0^2)
          if abs(pn-p0) lt eps then begin
            ele[i,j]=pn
            break
          endif else begin
            p0=pn
          endelse
        endwhile
        next:
      endfor
    endfor
  endif
  
;;;;;;;;;;;;;;;;
;  azi_minus=where(azi lt 0) & azi[azi_minus]=azi[azi_minus]+2.*!pi
  nan_pos=where(sqrt(fn^2+gn^2) gt 1.0)
  ele[nan_pos]=-!pi/2
;;;;;;;;;;;;;;;;
  
  fid=ncdf_create(calib_data_fname,/clob,/netcdf4)
  
  gid_lat=ncdf_groupdef(fid,'Latitude')
  gid_lon=ncdf_groupdef(fid,'Longitude')
  gid_zon=ncdf_groupdef(fid,'Zonal_distance')
  gid_mer=ncdf_groupdef(fid,'Meridional_distance')
  
  ASC_info_gid=ncdf_groupdef(fid,'ASC_info')
  ncdf_write_var,ASC_info_gid,lat_ob,'Obs_latitude','dbl',att='Latitude of the observation site [Degree]'
  ncdf_write_var,ASC_info_gid,lon_ob,'Obs_longitude','dbl',att='Longitude of the observation site [Degree]'
  ncdf_write_var,ASC_info_gid,[img_xs,img_ys],'Image_size','uint',att='x and y dimensions of the image [Pixel]'
  
  ncdf_write_var,fid,ele,'Elevation','dbl',att='Elevation angle of the image [Radian]'
  ncdf_write_var,fid,azi,'Azimuth','dbl',att='Azimuth angle of the image [Radian]'
  
  for filt_itter=0,n_elements(filt_arr)-1 do begin
  
    filter=filt_arr[filt_itter]
    EH=Emission_Height_arr[filt_itter]
    print,'Emission Height = ',EH

    ;; Calculate lat. & lon. values and meridional & zonal distance for all pixels
    ;Latitude & longitude of the observation site
    print,'lat & lon of the observing site : lat = ',lat_ob,', lon = ',lon_ob
    ECA=!pi/2-ele-asin(RE/(RE+EH)*cos(ele)) ;Earth's center angle
    arc_l=(RE+EH)*ECA ;Arc length
    zonal_dist=arc_l*sin(azi)
    merid_dist=arc_l*cos(azi)
    lat=asin(sin(lat_ob*!dtor)*cos(ECA)+cos(lat_ob*!dtor)*sin(ECA)*cos(azi))
    lon=asin(sin(ECA)*sin(azi)/cos(lat))
    lat=lat*!radeg & lon=lon*!radeg+lon_ob
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ncdf_write_var,gid_lat,lat,filter,'dbl',att='Latitude of the '+filter+' image [Degree]'
    ncdf_write_var,gid_lon,lon,filter,'dbl',att='Longitude of the '+filter+' image [Degree]'
    ncdf_write_var,gid_zon,zonal_dist,filter,'dbl',att='Zonal distance of the '+filter+' image [km]'
    ncdf_write_var,gid_mer,merid_dist,filter,'dbl',att='Meridional distance of the '+filter+' image [km]'
    
;    ;;for analysis
;    azi[nan_pos]=0 & device,decomposed=0
;    ele[nan_pos]=0
;    loadct,13,/silent
;    window,0,xs=img_xs,ys=img_ys,title='Azimuth'
;    tvscl,azi
;    window,1,xs=img_xs,ys=img_ys,title='Elevation'
;    tvscl,ele
;    window,2,xs=img_xs,ys=img_ys,title='Dif. between azi and shifted_azi'
;    azi[nan_pos]=!values.f_NaN
;    dif_azi=azi-shift(azi,5,5)
;    check_azi=finite(dif_azi) & nan_pos2=where(check_azi eq 0)
;    dif_azi[nan_pos2]=0 & tvscl,abs(dif_azi)
;    print,'For azimuth and shifted azimuth'
;    print,'max dif = ',max(dif_azi),', min dif = ',min(dif_azi)
;    window,3,xs=img_xs,ys=img_ys,title='Dif. between ele and shifted_ele'
;    ele[nan_pos]=!values.f_NaN
;    dif_ele=ele-shift(ele,5,5)
;    check_ele=finite(dif_ele) & nan_pos3=where(check_ele eq 0)
;    dif_ele[nan_pos3]=0 & tvscl,abs(dif_ele)
;    print,'For elevation and shifted elevation'
;    print,'max dif = ',max(dif_ele),', min dif = ',min(dif_ele)
;    window,4,xs=img_xs,ys=img_ys,title='Dif. between ele and transposed_ele'
;    ;ele[nan_pos]=!values.f_NaN
;    dif_ele2=ele-transpose(ele)
;    check_ele2=finite(dif_ele2) & nan_pos4=where(check_ele2 eq 0)
;    dif_ele2[nan_pos4]=0 & tvscl,abs(dif_ele2)
;    print,'For elevation and transposed elevation'
;    print,'max dif = ',max(dif_ele2),', min dif = ',min(dif_ele2)
;    azi_minus=where(azi lt 0)
;    if azi_minus[0] ne -1 then azi[azi_minus]=azi[azi_minus]+2*!pi
;    window,5,xs=img_xs,ys=img_ys,title='Dif. between raw azih and shifted_transposed_azi'
;    dif_azi2=azi-transpose(shift(azi,5,5))
;    check_azi2=finite(dif_azi2) & nan_pos5=where(check_azi2 eq 0)
;    dif_azi2[nan_pos5]=0 & tvscl,abs(dif_azi2)
;    print,'For azimuth and shifted_transposed_azi'
;    print,'max dif =',max(dif_azi2),', min dif = ',min(dif_azi2)
;    loadct,0,/silent
;    stop
;    ;;
  endfor
  
  ncdf_close,fid
end