;+
; 
; NAME: PROCESS_JBS_AASC_RAW_IAMGES_REV1
; 
; PURPOSE:
;     Calibration of JBS-A-ASC images
;     
; EXPLANATION:
;     This code calibrates JBS Color-CCD Aurora ASC images.
;     It uses spatial calibration data (coef_a and coef_b) to find the pixel coord-
;     inates of the zenith position, to find the rotation angle, and to determine
;     whether flipping is need or not (These procedures is obsolute now, but it is
;     conducted to determine the size of the output images). But now, result images
;     are achieved by conducting 2D-interpolation as IDL default 'ROT' function gives
;     poor results (But the speed of 2D-interpolation is very slow when it is compared
;     to simple rotation scheme). The 2D-interpolation is done by IDL default 'TRIGRID'
;     function. Because the size of the raw image is too large (1936 by 1216), this code
;     minifies the image into 968 by 608, and the spatial callibration data (coef_a,
;     coef_b) are only available to this size of image.
;     To make the aurora be more visible, contrast-stretching can be used. It can
;     be done by using 'bytscl' function (max=50 and min=0 are recommended) simply.
;     
;     
;     This code is optimized for below AURORA ASC images
;       - Size: '1936 by 1216' (Has RGB Channels)
;       - File naming rule: 'JBS_YYYYMMDD_HHMMSS_1.png' (HHMMSS in UT)
;       - Time resolution: 1 min.
; 
; NOTE:
;     'CATCH' procedure is used because of the errorneous images that cannot be read
;     by the IDL. It may prevent the code rasing errors. If the code doen't operate well
;     but raise no error, turn off 'CATCH' procedure.
; HISTORY
;     Geomgraphic -> Geomagnetic conversion added. Jul/09/2018
;     Rotating, clipping, and clipping procedures are replaced by 2D-interpolation. Feb/16/2019
;-
pro Process_JBS_AASC_raw_images_rev1
  
  num_underbar=3 ; Number of underbar (_) in the file name {It is 3 in 2018,2020 and 4 in 2019}
                 ; There may be some variations in the 2020 data. The tail of the file name
                 ; has been adjusted on the way of the 2020 observation.
                 ; This variable is used to filter out useless images.

;- Specify the input and the output directory
; Right after the 'in_dir' structure, folder whose name is the year (for example, 2018) must be followed.
; On Windows OS, downloaded raw data have the directory structure like below.
; 'E:\JBS_COLOR_ASC\2018\05\11\05\JBS_20180511_050500_1.png'
;  in_dir='E:\JBS_COLOR_ASC\'
;  out_dir='E:\AASC_processed\'
;-

target_yr=2018

in_dir='IDLWorkspace85/Default/JBS_AASC_codes_for_YSChoi/aasc_jbs_'+string(target_yr,format='(i4)')+'_raw/'
out_dir='IDLWorkspace85/Default/JBS_AASC_codes_for_YSChoi/aasc_jbs_'+string(target_yr,format='(i4)')+'_raw/processed2/'
file_mkdir, out_dir
  
  in_dir=strjoin(strsplit(in_dir,'/\',/ext),path_sep())+path_sep()
  out_dir=strjoin(strsplit(out_dir,'/\',/ext),path_sep())+path_sep()
  

  sjday=julday(03,27,2018)
  ejday=julday(03,27,2018)
  num_day=ejday-sjday+1
  jday0=sjday
; Angle for geographic to geomagnetic coordinate conversion
  coord_conv_ang=72.5
;
; Default settings for the current JBS AURORA ASC (July,2018)
  coef_a=[1.0914453, -3.0447496d-005,  -0.0035302962] ; This coefficient only works for the minified images (968 by 608 images)
  coef_b=[1.5956212, -0.0035355680  , 2.7382046e-005] ; This coefficient only works for the minified images (968 by 608 images)
;
; minified JBS A-ASC image size
  img_siz=[3,968,608]
;
; Upper and lower intensity limits for the contrast-stretching of the output images
  img_max_inten=50 & img_min_inten=0
;

  x=lindgen(img_siz[1],img_siz[2]) mod img_siz[1]
  y=transpose(lindgen(img_siz[2],img_siz[1])) mod img_siz[2]
  x=double(x) & y=double(y) ; Pixel coordinates of the image
  ; normalized coordinates of the image
  fn=coef_a[0]+coef_a[1]*x+(temporary(coef_a))[2]*y
  gn=coef_b[0]+coef_b[1]*temporary(x)+(temporary(coef_b))[2]*temporary(y)
  ;## fn: negative: western sector, positive: eastern sector
  ;## gn: negative: southern sector, positive: northern sector
  ;## sqrt(fn^2+gn^2) > 1: elevation angle > 0 degree
  ;## sqrt(fn^2+gn^2) = 0: zenith
  ;
  azi=atan(fn,gn)*!radeg ;Azimuth angle [0: North, 90: East, 180(-180): South, -90(270): West]
  azi_geomag=azi-coord_conv_ang
  pos=where(azi_geomag lt -180)
  if pos[0] ne -1 then azi_geomag[pos]=azi_geomag[pos]+360
  pos=where(azi_geomag gt 180)
  if pos[0] ne -1 then azi_geomag[pos]=azi_geomag[pos]-360
  ele=!pi/2.*(1-sqrt(fn^2+gn^2))*!radeg ; elevation engle [0: horizon, 90: zenith]
  ele_geomag=ele
  zenith=array_indices(ele,where(ele eq max(ele))) ; pixel coordinates of the zenith position
  north=array_indices(azi,where(abs(azi) eq min(abs(azi)))) ; pixel coordinates of the geographic north direction
;  rot_ang=atan(north[1]-zenith[1],(temporary(north))[0]-zenith[0])*!radeg-90d ; rotation angle in clockwise
;  fn_geo_rot=rot(fn,rot_ang,1,zenith[0],zenith[1])
;  gn_geo_rot=rot(gn,rot_ang,1,zenith[0],zenith[1])
;; Rotate images to align geomagnetic north to be top
;  ele=rot(ele,rot_ang+coord_conv_ang,1,zenith[0],zenith[1])
;  azi=rot(azi,rot_ang+coord_conv_ang,1,zenith[0],zenith[1])
;  fn=rot(fn,rot_ang+coord_conv_ang,1,zenith[0],zenith[1])
;  gn=rot(gn,rot_ang+coord_conv_ang,1,zenith[0],zenith[1])
;
;; determine whether flipping is needed or not. -> activate & deactivate makes different size of the out image [activate: 567x567, deactivate: 566x566]
;  flip_flag=0 ; flip_flag (0: Do not flip, 1: Do flip)
;  if fn_geo_rot[0,0] gt 0 then begin
;    flip_flag=1
;    ele=reverse(ele)
;    azi=reverse(azi)
;    fn=reverse(fn)
;    gn=reverse(gn)
;  endif
;;
;
;; Find clipping area of the image (elevation > 0)
;  pos=where(sqrt(fn^2+gn^2) lt 0.9999d)
;  clip_idx=array_indices(fn,temporary(pos))
;  clip_idx_x=[min(clip_idx[0,*]),max(clip_idx[0,*])] ;clipping position
;  clip_idx_y=[min(clip_idx[1,*]),max((temporary(clip_idx))[1,*])] ;clipping position
;  clip_siz=[clip_idx_x[1]-clip_idx_x[0]+1,clip_idx_y[1]-clip_idx_y[0]+1] ; image size after clipping
;  if clip_siz[0] ne clip_siz[1] then begin
;    zenith=array_indices(ele,where(ele eq max(ele)))
;    dif_x=abs(clip_idx_x-zenith[0])
;    dif_y=abs(clip_idx_x-zenith[0])
;    if abs(clip_siz[0]-clip_siz[1]) ne 1 then message,'Unexpected error occurred. The difference between x and y dimension exceed 1.'
;    if clip_siz[0] mod 2 eq 0 then begin
;      zenith_even_odd=zenith[0] mod 2
;      pos=where((abs(clip_idx_x-zenith[0]) mod 2) eq zenith_even_odd)
;      if pos eq 0 then begin
;        clip_idx_x[pos]=clip_idx_x[pos]-1
;      endif else begin
;        clip_idx_x[pos]=clip_idx_x[pos]+1
;      endelse
;    endif else begin
;      zenith_even_odd=zenith[1] mod 2
;      pos=where((abs(clip_idx_y-zenith[1]) mod 2) eq zenith_even_odd)
;      if pos eq 0 then begin
;        clip_idx_y[pos]=clip_idx_y[pos]-1
;      endif else begin
;        clip_idx_y[pos]=clip_idx_y[pos]+1
;      endelse
;    endelse
;    clip_siz=[clip_idx_x[1]-clip_idx_x[0]+1,clip_idx_y[1]-clip_idx_y[0]+1]
;  endif
  clip_siz=[567,567]
  grid=(dindgen(clip_siz[0])/(clip_siz[0]-1)-0.5)*2
  xgrid=(ygrid=grid)
  ygrid_2D=rebin(transpose(grid),clip_siz[0],clip_siz[1],/sam)
  xgrid_2D=transpose(ygrid_2D)
  corr_r=sqrt(xgrid_2D^2+ygrid_2D^2)
  pos=where(corr_r le 1)
  circle=bytarr(clip_siz[0],clip_siz[1])
  circle[pos]=1
  r=(90-ele_geomag)/90
  fn_geomag=r*sin(azi_geomag*!dtor)
  gn_geomag=r*cos(azi_geomag*!dtor)
  triangulate,fn_geomag,gn_geomag,tr
for jd=0L,num_day-1 do begin
  caldat,jday0,cmon,cday,cyr
  print,string(cyr,format='(i4)')+'/'+string(cmon,format='(i2.2)')+'/'+string(cday,format='(i2.2)')
  device,decom=0
  window,0,xs=clip_siz[0],ys=clip_siz[1]
  
;  fname=file_search(in_dir+string(cyr,format='(i4)')+path_sep()+string(cmon,format='(i2.2)')+path_sep()+$ ; Find images of the input date
;    string(cday,format='(i2.2)')+path_sep()+'*'+path_sep()+'*.png',count=num_file)   
    fname=file_search(in_dir+path_sep()+string(cmon,format='(i2.2)')+path_sep()+$ ; Find images of the input date
    string(cday,format='(i2.2)')+path_sep()+'*'+path_sep()+'*.png',count=num_file)
    stop
  if num_file eq 0 then goto,no_image
  fbname=file_basename(fname) ; get file base name
; Filter out dark images from the image list
  check_dark=strmatch(fbname,'*DARK*')
  pos=where(check_dark eq 0)
  if pos[0] eq -1 then begin
    num_file=0L
    goto,no_image
  endif else begin
    fname=fname[pos]
    fbname=fbname[pos]
  endelse
  num_file=n_elements(pos) ; This cannot be -1L
  pos=!NULL & check_dark=!NULL
;
; Get universal time info of the images (UT in [minute])
  if num_file eq 1 then begin
    spl_fbname=strsplit(temporary(fbname),'_',/ext)
    HHMMSS=(temporary(spl_fbname))[2]
    UT=long(strmid(HHMMSS,0,2))*60 + long(strmid(HHMMSS,2,2))
  endif else if num_file gt 1 then begin
    spl_fbname=strsplit(temporary(fbname),'_',/ext)
    ; Filter out images whose file names don't follow common file name syntax (e.g., JBS 201807crop.png)
    num_spl_fbname=n_elements(spl_fbname)
    ccursor=0L
    while 1 do begin
      if n_elements(spl_fbname[ccursor]) ne num_underbar+1 then begin
        print,'Reject file ('+strjoin(spl_fbname[ccursor],'_')+')'
        spl_fbname.remove,ccursor & num_spl_fbname--
        ccursor--
      endif
      ccursor++
      if ccursor eq num_spl_fbname then break
    endwhile
    spl_fbname=spl_fbname.toArray()
    HHMMSS=(temporary(spl_fbname))[*,2]
    UT=long(strmid(HHMMSS,0,2))*60 + long(strmid(HHMMSS,2,2))
  endif else begin
    goto, no_image
  endelse
  sor=sort(UT)
  UT=UT[sor]
  fname=fname[temporary(sor)]
;
  dstring=string(cyr,format='(i4)')+'_'+string(cmon,format='(i2.2)')+'_'+string(cday,format='(i2.2)')
  sub_dir=strjoin(strsplit(dstring,'_',/ext),path_sep())+path_sep()
  print,'Process '+strjoin(strsplit(dstring,'_',/ext),'/')
; process the images
  counter=0L
  for i=0L,num_file-1 do begin
    catch,err_stat
    if err_stat ne 0 then goto,skip_img
    if (file_search(out_dir+sub_dir+'JBS_Aurora_ASC_'+dstring+'_'+HHMMSS[i]+'.png'))[0] ne '' then goto, skip_img
    img=read_png(fname[i])
    img_siz=size(img,/dimen)
    if n_elements(img_siz) lt 3 then goto,skip_day
    counter++
    if counter eq 1 then file_mkdir,out_dir+sub_dir
    img=rebin(img,img_siz[0],img_siz[1]/2,img_siz[2]/2) ; minifying the image
    img_siz=size(img,/dimen)
    Rch=(Gch=(Bch=bytarr(img_siz[1],img_siz[2])))
    Rch[*,*]=img[0,*,*]
    Gch[*,*]=img[1,*,*]
    Bch[*,*]=img[2,*,*]
    
    gridded_Rch=trigrid(fn_geomag,gn_geomag,Rch,tr,xout=grid,yout=grid)
    gridded_Gch=trigrid(fn_geomag,gn_geomag,Gch,tr,xout=grid,yout=grid)
    gridded_Bch=trigrid(fn_geomag,gn_geomag,Bch,tr,xout=grid,yout=grid)
    corr_img=bytarr(3,clip_siz[0],clip_siz[1])
    corr_img[0,*,*]=temporary(gridded_Rch)*circle
    corr_img[1,*,*]=temporary(gridded_Gch)*circle
    corr_img[2,*,*]=temporary(gridded_Bch)*circle
;
    tv,bytscl(corr_img,max=img_max_inten,min=img_min_inten),/true
;    loadct,13,/sil
;    xyouts,0.01,0.96,align=0,'JBS A-ASC',chars=1.5,/normal,font=1
;    xyouts,0.01,0.93,align=0,strjoin(strsplit(dstring,'_',/ext),'/'),chars=1.4,/normal,font=1
;    xyouts,0.01,0.90,align=0,strmid(HHMMSS[i],0,2)+':'+strmid(HHMMSS[i],2,2)+' UT',chars=1.4,/normal,font=1
;    loadct,0,/sil

    write_png,out_dir+sub_dir+'JBS_Aurora_ASC_'+dstring+'_'+HHMMSS[i]+'.png',corr_img
;    write_png,out_dir+sub_dir+'JBS_Aurora_ASC_'+dstring+'_'+HHMMSS[i]+'.png',tvrd(/true) ; For contrast-stretched images
    skip_img:
    catch,/cancel
  ;
  endfor
;
  no_image:
  if num_file eq 0 then begin
    print,'There is no image to process for the input date.'
  endif
  skip_day:
jday0++
endfor
end