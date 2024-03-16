pro dayside_aurora_keogram_IMF

  target_yr=2020
  target_mon=05
  target_day=30
  target_stime=16
  target_etime=20  ;graph time
  
  dir='Desktop/Dayside aurora/figure sets/'
  out_dir=dir
 ; file_mkdir,out_dir

 ; coord_conv_ang=72.5 ; Angle for geographic to geomagnetic coordinate conversion
  sjday=julday(target_mon,target_day,target_yr)
  ejday=julday(target_mon,target_day,target_yr)
  num_day=ejday-sjday+1
  jday0=sjday
  for jd=0L,num_day-1 do begin
    caldat,jday0,cmon,cday,cyr

    Time_resol=1 ; [minute]
    strip_width=5 ; sampling width of the strip for making keogram line
    
;     fname=file_search('/Volumes/Backup Plus/Processed/'+$
;           string(cyr,format='(i4)')+'/'+string(cmon,format='(i2.2)')+'/'+string(cday,format='(i2.2)')+'/scaled/*.png',count=num_file) ; 2018
;           
     fname=file_search('/Volumes/Backup Plus/Processed/'+$
           string(cyr,format='(i4)')+'/scaled/'+string(cmon,format='(i2.2)')+'/'+string(cday,format='(i2.2)')+'/*.png',count=num_file)  ; 2019,2020

     fbname=file_basename(fname,'.png')
     
    ; reading the files of the whole day
;    for i=0, num_file-1 do begin
;      file_str=strsplit(fbname[i],'_',/ext)
;      hhmmss=file_str[6]
;      hr[i]=strmid(hhmmss,0,2)
;      mn[i]=strmid(hhmmss,2,2)
;    endfor

    ; finding an event day in the whole year dayside
    ;event_file=fname[where(mon eq cmon and day eq cday,nfile)]
    
    ; Get universal time info of the images (UT in [minute])
    if num_file eq 1 then begin
      spl_fbname=strsplit(temporary(fbname),'_',/ext)
      HHMMSS=(temporary(spl_fbname))[6]
      UT=long(strmid(HHMMSS,0,2))*60 + long(strmid(HHMMSS,2,2))
      
    endif else if num_file gt 1 then begin
      spl_fbname=(strsplit(temporary(fbname),'_',/ext)).toArray()
      HHMMSS=(temporary(spl_fbname))[*,6]
      HH=strmid(hhmmss,0,2)
      MM=strmid(hhmmss,2,2)
      UT=long(strmid(HHMMSS,0,2))*60 + long(strmid(HHMMSS,2,2))
      time=long(strmid(HHMMSS,0,2)) + long(strmid(HHMMSS,2,2))/60.

    endif else begin
      goto, no_image
    endelse
    sor=sort(UT)
    UT=UT[sor]
 ;   event_file=event_file[temporary(sor)]
    fname=fname[temporary(sor)]
    counter=0L

    ; finding an event day in the whole year dayside
 ;   event_file=fname[where(UT ge target_stime and UT le target_etime,nfile)]
 ;   fbname=file_basename(event_file,'.png') ; get file base name

    ; process the images and make keograms
   
    event_file=where(time ge target_stime and time le target_etime, nfile)   ; to find targeted time zone within a whole day 

    for i=0, nfile-1 do begin
      ;img=read_png(fname[i])
      img=read_png(fname[event_file[i]])
      img_siz=size(img,/dimen)


 ;   for i=0L,num_file-1 do begin
      ;    catch,err_stat
      ;    if err_stat ne 0 then goto,skip_img
      
;      event_file=where(time ge target_stime and time le target_etime, nfile)
;

;      if n_elements(img_siz) lt 3 then goto,skip_img
;      img=rebin(img,img_siz[0],img_siz[1]/2,img_siz[2]/2) ; minifying the image
;      img_siz=size(img,/dimen)

      if counter eq 0 then begin
;        dstring=string(cyr,format='(i4)')+'_'+string(cmon,format='(i2.2)')+'_'+string(cday,format='(i2.2)')
;        file_mkdir,img_out_dir+strjoin(strsplit(dstring,'_',/ext),'/')
;        print,'Process '+strjoin(strsplit(dstring,'_',/ext),'/')
;        x=lindgen(img_siz[1],img_siz[2]) mod img_siz[1]
;        y=transpose(lindgen(img_siz[2],img_siz[1])) mod img_siz[2]
;        x=double(x) & y=double(y) ; Pixel coordinates of the image
;        fn=coef_a[0]+coef_a[1]*x+(temporary(coef_a))[2]*y & gn=coef_b[0]+coef_b[1]*temporary(x)+(temporary(coef_b))[2]*temporary(y) ; normalized coordinates of the image
        ;## fn: negative: western sector, positive: eastern sector
        ;## gn: negative: southern sector, positive: northern sector
        ;## sqrt(fn^2+gn^2) > 1: elevation angle > 0 degree
        ;## sqrt(fn^2+gn^2) = 0: zenith
;        azi=atan(fn,gn)*!radeg ;Azimuth
;        ele=!pi/2.*(1-sqrt(fn^2+gn^2))*!radeg

;        zenith=array_indices(ele,where(ele eq max(ele))) ; pixel coordinates of the zenith position
;        north=array_indices(azi,where(abs(azi) eq min(abs(azi)))) ; pixel coordinates of the north direction
;        rot_ang=atan(north[1]-zenith[1],(temporary(north))[0]-zenith[0])*!radeg-90d ; rotation angle in clockwise
;        ele=rot(ele,rot_ang,1,zenith[0],zenith[1])
;        azi=!NULL
;        ;azi=rot(azi,rot_ang,1,zenith[0],zenith[1])
;        fn=rot(fn,rot_ang,1,zenith[0],zenith[1])
;        gn=rot(gn,rot_ang,1,zenith[0],zenith[1])
;        ; determine wheter flipping is needed or not.
;        flip_flag=0 ; flip_flag (0: Do not flip, 1: Do flip)
;        if fn[0,0] gt 0 then begin
;          flip_flag=1
;          ele=reverse(ele)
;          ;azi=reverse(azi)
;          fn=reverse(fn)
;          gn=reverse(gn)
;        endif
        ; Find clipping area of the image (elevation > 0)
;        pos=where(sqrt(fn^2+temporary(gn)^2) lt 0.9999d)
;        clip_idx=array_indices(temporary(fn),temporary(pos))
;        clip_idx_x=[min(clip_idx[0,*]),max(clip_idx[0,*])] ;clipping position
;        clip_idx_y=[min(clip_idx[1,*]),max((temporary(clip_idx))[1,*])] ;clipping position
;        clip_siz=[clip_idx_x[1]-clip_idx_x[0]+1,clip_idx_y[1]-clip_idx_y[0]+1]                ; image size after clipping
;        corr_zenith=array_indices(ele,where(ele eq max(ele)))-[clip_idx_x[0],clip_idx_y[0]]   ; corrected zenith position after rotating, fliping, and clipping
;        ele=!NULL
        ;

        ;###################
        ; prepare Keogram arrays
        WE_keogram=bytarr(3, (target_etime-target_stime)*60, 567)
        SN_keogram=bytarr(3, (target_etime-target_stime)*60, 567)
        WE_keo_siz=size(WE_keogram,/dimen)
        SN_keo_siz=size(SN_keogram,/dimen)
      ;  keogram_xaxis=lindgen((target_etime-target_stime)*60)+ut[0]; in minute
        keogram_xaxis=ut
        ;
      endif
      ;###################

      ; extract individual RGB channels
      Rch=(Gch=(Bch=bytarr(img_siz[1],img_siz[2])))
      Rch[*,*]=img[0,*,*]
      Gch[*,*]=img[1,*,*]
      Bch[*,*]=img[2,*,*]
  
      corr_Rch=Rch
      corr_Gch=Gch
      corr_Bch=Bch
      corr_zenith=[((567-1)/2),((567-1)/2)]

      ;###################
      ; get zonal and meridional strips from the corrected image for keograms (averaging is taken in the direction of the strip width)
      zonal_Rch=mean(transpose(corr_Rch[*,corr_zenith[1]-(strip_width-1)/2  :  corr_zenith[1]+(strip_width-1)/2]),dimen=1)
      zonal_Gch=mean(transpose(corr_Gch[*,corr_zenith[1]-(strip_width-1)/2:corr_zenith[1]+(strip_width-1)/2]),dimen=1)
      zonal_Bch=mean(transpose(corr_Bch[*,corr_zenith[1]-(strip_width-1)/2:corr_zenith[1]+(strip_width-1)/2]),dimen=1)

      merid_Rch=mean(corr_Rch[corr_zenith[0]-(strip_width-1)/2:corr_zenith[0]+(strip_width-1)/2,*],dimen=1)
      merid_Gch=mean(corr_Gch[corr_zenith[0]-(strip_width-1)/2:corr_zenith[0]+(strip_width-1)/2,*],dimen=1)
      merid_Bch=mean(corr_Bch[corr_zenith[0]-(strip_width-1)/2:corr_zenith[0]+(strip_width-1)/2,*],dimen=1)

      ; make keograms
      pos=where(keogram_xaxis ge ut[i] and keogram_xaxis lt ut[i]+time_resol,num_pos)
      if num_pos eq 0 then message,'Unexpected error occurred.'
      zonal_Rch=transpose(zonal_Rch) & zonal_Gch=transpose(zonal_Gch) & zonal_Bch=transpose(zonal_Bch)
      merid_Rch=transpose(merid_Rch) & merid_Gch=transpose(merid_Gch) & merid_Bch=transpose(merid_Bch)
 ;stop ; to see if ut[0] is 960
      zonal_Rch=rebin(zonal_Rch,num_pos,(size(zonal_Rch,/dimen))[1],/sam)
      zonal_Gch=rebin(zonal_Gch,num_pos,(size(zonal_Gch,/dimen))[1],/sam)
      zonal_Bch=rebin(zonal_Bch,num_pos,(size(zonal_Bch,/dimen))[1],/sam)

      merid_Rch=rebin(merid_Rch,num_pos,(size(merid_Rch,/dimen))[1],/sam)
      merid_Gch=rebin(merid_Gch,num_pos,(size(merid_Gch,/dimen))[1],/sam)
      merid_Bch=rebin(merid_Bch,num_pos,(size(merid_Bch,/dimen))[1],/sam)

      WE_keogram[0,pos,*]=zonal_Rch & WE_keogram[1,pos,*]=zonal_Gch & WE_keogram[2,pos,*]=zonal_Bch
      SN_keogram[0,pos,*]=merid_Rch & SN_keogram[1,pos,*]=merid_Gch & SN_keogram[2,pos,*]=merid_Bch

      counter++
      skip_img:
      ;  catch,/cancel

    endfor
   
   ;############################################################################
   ; 1-1. Keogram - WE
;   
   img=bytscl(WE_keogram)+40
   img_siz=size(img,/dim)
   resiz_img=congrid(img,img_siz[0],img_siz[1]*2,img_siz[2]/4)      ; S-N below to be edited as well
   resiz_siz=size(resiz_img,/dim)

   set_plot,'PS'
   device,filename=out_dir+string(target_yr,format='(i4)')+string(target_mon,format='(i2.2)')+string(target_day,format='(i2.2)')+'_'+$
          'imf graph.ps',$
          decom=1,xoffset=1.5,yoffset=0.5,xsize=20,ysize=27,/portrait,bit=8,/color
          !p.multi=[0,1,4,0,0]
          
          

 ; window,0,xs=resiz_siz[1]+xmar*2,ys=resiz_siz[2]*2+ymar*2,tit='0 W-E Keogram'

   ;xtickname=['17:00','18:00','19:00','20:00','21:00','22:00']
   xtickname=['16:00','17:00','18:00','19:00','20:00']
   xminor_num=12
   ytickname=['W 90','60','30','0','30','60','E 90']
   
   xcharsize=2.5
   ycharsize=2

   ; tv,resiz_img,xmar,ymar,/true,/device
   ; tv,resiz_img,xmar+38,resiz_siz[2]+ymar+10,/true,/device              ; xmar+'10' --> 10 = 5 mins when xminor=12
   
   
 ;  tv,resiz_img,xmar,resiz_siz[2]+ymar+10,/true,/device                  ; if image shown right along the axis
    tv,resiz_img,0.03,0.812,xsize=0.94,ysize=0.158,/true,/normal          ; same size as set in pos in the plot below
    
     plot,[0,0],[0,0],xran=[target_stime,target_etime],yran=[-90,90],/xst,/yst,yticks=n_elements(ytickname)-1,$
     xticklen=0.01,yticklen=0.005,xchars=xcharsize,ychars=ycharsize,charth=1,ytickn=ytickname,ytit='Zenith angle (Degree)',/nodata,/noe,$     ;image not 
     xtickn=replicate(' ',n_elements(xtickname)+1),xticks=n_elements(xtickname)-1,xminor=xminor_num,$
     pos=[0.03,0.812,0.97,0.97],/normal ;,ytickformat="(a1)"
     
     plot,[0,0],[0,0],xran=[target_stime,target_etime],yran=[-90,90],/xst,/yst,yticks=n_elements(ytickname)-1,$
     xticklen=0.01,yticklen=0.010,chars=2,charth=1,ytickn=ytickname,/nodata,/noe,$     ;image not
     xtickn=replicate(' ',n_elements(xtickname)+1),xticks=n_elements(xtickname)-1,xminor=xminor_num,$
     pos=[0.03,0.812,0.97,0.97],/normal,ytickformat="(a1)",color='ffffff'x
    
 ; write_png,keo_out_dir+string(cyr,format='(i4)')+string(cmon,format='(i2.2)')+string(cday,format='(i2.2)')+'_0_WE_keogram_2.png',tvrd(/true)

   ;############################################################################  
   ; 1-2. Keogram - SN
    img=bytscl(SN_keogram)+40
    img_siz=size(img,/dim)
    resiz_img=congrid(img,img_siz[0],img_siz[1]*2,img_siz[2]/4)
    resiz_siz=size(resiz_img,/dim)

 ;  window,1,xs=resiz_siz[1]+xmar*2,ys=resiz_siz[2]*2+ymar*2,tit='1 S-N Keogram'

    ytickname=['S 90','60','30','0','30','60','N 90']
    
   ; tv,resiz_img,xmar,ymar,/true,/device
   ; tv,resiz_img,xmar+38,resiz_siz[2]+ymar+10,/true,/device              ; xmar+'10' --> 10 = 5 mins when xminor=12
  
  ;  tv,resiz_img,xmar,resiz_siz[2]+ymar+10,/true,/device                  ; if image shown right along the axis
 
    tv,resiz_img,0.03,0.644,xsize=0.94,ysize=0.158,/true,/normal
    
    plot,[0,0],[0,0],xran=[target_stime,target_etime],yran=[-90,90],/xst,/yst,yticks=n_elements(ytickname)-1,$   
      xticklen=0.01,yticklen=0.005,xchars=xcharsize,ychars=ycharsize,charth=1,ytickn=ytickname,ytit='Zenith angle (Degree)',/nodata,/noe,$
      xtickn=replicate(' ',n_elements(xtickname)+1),xticks=n_elements(xtickname)-1,xminor=xminor_num,$
      pos=[0.03,0.644,0.97,0.802],/normal;
      
    plot,[0,0],[0,0],xran=[target_stime,target_etime],yran=[-90,90],/xst,/yst,yticks=n_elements(ytickname)-1,$
      xticklen=0.01,yticklen=0.010,chars=2,charth=1,ytickn=ytickname,/nodata,/noe,$
      xtickn=replicate(' ',n_elements(xtickname)+1),xticks=n_elements(xtickname)-1,xminor=xminor_num,$
      pos=[0.03,0.644,0.97,0.802],/normal,ytickformat="(a1)",color='ffffff'x
     
    no_image:
    if num_file eq 0 then begin
      print,'There is no image to process for the input date.'
    endif

    skip_day:
    jday0++
  endfor
 ; ######################################################################################################
 ; 2. IMF plot

  target_stime1=target_stime
  target_etime1=target_etime             
                                                                  
  ; 1. IMF
  dir='IDLWorkspace85/Default/JBS_AASC_codes_for_YSChoi/AASC_available_sorted/manual_sorted/good/'
  IMF=dir+string(target_yr,format='(i4)')+'_IMF(GSM).txt'
  
 ; out_dir=dir+string(target_yr,format='(i4)')+'/IMF/'
 ; file_mkdir,out_dir

  openr,lunr,IMF,/get_lun
  header=''
  readf,lunr,header

  fline = file_lines(IMF)

  hh=intarr(fline-1)
  mn=intarr(fline-1)
  By=fltarr(fline-1)
  Bz=By
  Bx=By
  mm=intarr(fline-1)
  dd=intarr(fline-1)
  time=fltarr(60*24)
  event_Bx=time
  event_By=time
  event_Bz=time

  for i=0,fline-2 do begin
    readf,lunr,yr,idy,hr,mi,av,iBx,iBy,iBz,format='(i4,i4,i3,i3,4f8.2)'
    
    hh[i]=hr
    mn[i]=mi
    dayofyear=fix(strmid(strtrim(idy,1),0,3))
    caldat,julday(1,dayofyear,yr),mon,day
    Bx[i]=iBx
    By[i]=iBy
    Bz[i]=iBz
    mm[i]=mon
    dd[i]=day
  endfor

  event=where(mm eq target_mon and dd eq target_day)
  for j=0,60*24-1 do begin

    time[j]=hh[event[j]]+mn[event[j]]/60.

    if (Bx[event[j]] or By[event[j]] or Bz[event[j]]) eq 9999.99 then begin
        Bx[event[j]]=!values.f_nan & By[event[j]]=!values.f_nan & Bz[event[j]]=!values.f_nan
    endif
     
    event_Bx[j]=Bx[event[j]]
    event_By[j]=By[event[j]]
    event_Bz[j]=Bz[event[j]]

  endfor

  target=where(time ge target_stime1 and time le target_etime1, n_file)
  
  target_Bx=fltarr(n_file)
  target_By=fltarr(n_file)
  target_Bz=fltarr(n_file)

  ; extracting targeted time
  for k=0,n_file-1 do begin
    target_Bx[k]=event_Bx[target[k]]
    target_By[k]=event_By[target[k]]
    target_Bz[k]=event_Bz[target[k]]

  endfor

zerobar=intarr(1440)
  
;plot,[0,0],[0,0],xrange=[target_stime,target_etime],yran=[-5,5],/xst,/yst,yticks=2,yminor=5,$
  plot,[0,0],[0,0],xrange=[target_stime,target_etime],yran=[-10,10],/xst,/yst,yticks=10,yminor=2,$
     yticklen=0.005,xchars=xcharsize,ychars=ycharsize,charth=1,ytickname=ytname,ytit='IMF (nT)',/nodata,$ 
     xticks=n_elements(xtickname)-1,xminor=xminor_num,xtickn=replicate(' ',n_elements(xtickname)+1),$
     pos=[0.03,0.476,0.97,0.634],/normal,/noe
     
oplot,time[target],target_Bx,color='005400'x     
oplot,time[target],target_By,color='0068ff'x    ; By orange
oplot,time[target],target_Bz,color='00c8ff'x
oplot,time,zerobar,linestyle=0,thick=1

free_lun,lunr

; ################################################################################################
; 3. clock angle

num_data=n_elements(target_Bz)
new_clock=fltarr(num_data)
for j=0, num_data-1 do begin

if target_Bz[j] gt 0 and target_By[j] gt 0 then new_clock[j]=atan(target_By[j]/target_Bz[j])*180./!pi
if target_Bz[j] gt 0 and target_By[j] lt 0 then new_clock[j]=-atan((abs(target_By[j]))/target_Bz[j])*180./!pi
if target_Bz[j] lt 0 and target_By[j] gt 0 then new_clock[j]=90+atan((abs(target_Bz[j]))/target_By[j])*180./!pi
if target_Bz[j] lt 0 and target_By[j] lt 0 then new_clock[j]=-90-atan((abs(target_bz[j]))/(abs(target_by[j])))*180./!pi

endfor  

nan=where(new_clock eq 0)
new_clock[nan]=!values.f_nan

;window,3,xs=resiz_siz[1]+xmar*2,ys=resiz_siz[2]*2+ymar*2,tit='3 clock angle1'

plot,[0,0],[0,0],xrange=[target_stime,target_etime],yran=[-180,180],/xst,/yst,$
     yticklen=0.005,xchars=xcharsize,ychars=ycharsize,charth=1,yticks=4,yminor=2,ytickn=['-180','-90','0','90','180'],ytit='clock angle (Degree)',/nodata,$
     xticks=n_elements(xtickname)-1,xminor=xminor_num,xtickn=xtickname,$
     pos=[0.03,0.308,0.97,0.466],/normal,/noe

pos_90_bar=intarr(2000)+90
neg_90_bar=intarr(2000)-90
;pos_135_bar=intarr(2000)+135
;neg_135_bar=intarr(2000)-135

oplot,time[target],new_clock
oplot,time[target],zerobar,linestyle=1
oplot,time[target],pos_90_bar,linestyle=1
oplot,time[target],neg_90_bar,linestyle=1
;oplot,time[target],pos_135_bar,linestyle=1
;oplot,time[target],neg_135_bar,linestyle=1

;write_png,keo_out_dir+string(cyr,format='(i4)')+string(cmon,format='(i2.2)')+string(cday,format='(i2.2)')+'_new_clock_poster.png',tvrd(/true)
 
;;clock_angle1=atan(imf_ratio)*(180./!pi)   ; in degrees which was deemed 180-degree ambiguity still within
;;
;;clock_angle=atan(IMF_ratio)               ; still in rad
;;a0=clock_angle                            ; still in rad (should be! according to arctan.pro)
;;x =cos(a0)
;;y =sin(a0)
;;arctan,x,y,a,a_deg
;;
;;nan=where(a_deg eq 0)
;;a_deg[nan]=!values.f_nan
;
;; #######################3
;
;; ################################################################################################
;; 4. imf_ratio
;IMF_ratio=target_By/target_Bz
;ratio_abs=abs(target_By)/abs(target_Bz)
;  
;;window,4,xs=resiz_siz[1]+xmar*2,ys=resiz_siz[2]*2+ymar*2,tit='4 imf ratio'     
;plot,time[target],ratio_abs,xrange=[target_stime,target_etime],yrange=[0.01,100],/ylog,/device,$
;     xticks=n_elements(xtickname)-1,xminor=xminor_num,xtickn=replicate(' ',n_elements(xtickname)+1),ytitle='|By|/|Bz|',$
;     xst=1,yst=1,yticks=4,ycharsize=1,pos=[xmar,ymar,xmar+resiz_siz[1],ymar+resiz_siz[2]],yticklen=0.005,color='ffffff'x,ytickformat="(a1)"
;  
;onebar=zerobar+1
;oplot,time,onebar,linestyle=0  
;
;write_png,keo_out_dir+string(cyr,format='(i4)')+string(cmon,format='(i2.2)')+string(cday,format='(i2.2)')+'_4_imf ratio.png',tvrd(/true)
;
;
;; ################################################################################################
;; 5. SME
;
;SML=dir+string(target_yr,format='(i4)')+'_SML_supermag.txt'
;openr,lunr2,SML,/get_lun
;header=''
;readf,lunr2,header
;
;fline=file_lines(SML)
;month=intarr(fline-1)
;day=month
;hour=month
;mins=month
;sml=month
;smu=month
;
;for i=0,fline-2 do begin
;  readf,lunr2,iyr,imon,iday,ihr,imin,sec,isml,ismu
;  month[i]=imon
;  day[i]=iday
;  hour[i]=ihr
;  mins[i]=imin
;  sml[i]=isml
;  smu[i]=ismu
;endfor
;
;event=where(month eq target_mon and day eq target_day,n_file)
;event_sml=intarr(n_file)
;event_smu=intarr(n_file)
;for j=0,n_file-1 do begin
;  
;  time[j]=hh[event[j]]+mn[event[j]]/60.
;  
;  event_sml[j]=sml[event[j]]
;  event_smu[j]=smu[event[j]]
;endfor
;
;target=where(time ge target_stime and time le target_etime,n_file1)
;target_sml=intarr(n_file1)
;target_smu=intarr(n_file1)
;
;for k=0,n_file1-1 do begin
;  target_sml[k]=event_sml[target[k]]
;  target_smu[k]=event_smu[target[k]]
;endfor
;
;window,5,xs=resiz_siz[1]+xmar*2,ys=resiz_siz[2]*2+ymar*2,tit='5 SME'
;;
;plot,time[target],target_sml,/nodata,xrange=[target_stime,target_etime],$
;  xticks=n_elements(xtickname)-1,xminor=xminor_num,xtickname=xtickname,/device,$
;  yrange=[-500,300],yticks=4,yminor=2,yticklen=0.005,ytitle='SME (nT)',$
;  xst=1,yst=1,pos=[xmar,ymar,xmar+resiz_siz[1],ymar+resiz_siz[2]],xcharsize=1.5,ytickformat="(a1)"

;plot,time[target],target_sml,/noe,xrange=[target_stime,target_etime],$
;  xticks=n_elements(xtickname)-1,xminor=xminor_num,xtickname=xtickname,/device,$
;  yrange=[-500,300],yticks=4,yminor=2,yticklen=0.005,ytitle='SME (nT)',$
;  xst=5,yst=5,pos=[xmar,ymar,xmar+resiz_siz[1],ymar+resiz_siz[2]],color='0000ff'x,ytickformat="(a1)"; SML red
  
;oplot,time[target],target_sml,color='0000ff'x
;  xcharsize=1.5
;oplot,time[target],target_smu
;oplot,time,zerobar,linestyle=1
;pos_200_bar=zerobar+200
;pos_100_bar=zerobar+100
;neg_100_bar=zerobar-100
;neg_200_bar=zerobar-200
;neg_300_bar=zerobar-300
;neg_400_bar=zerobar-400
;oplot,time,pos_200_bar,linestyle=1
;oplot,time,pos_100_bar,linestyle=1
;oplot,time,neg_100_bar,linestyle=1
;oplot,time,neg_200_bar,linestyle=1
;oplot,time,neg_300_bar,linestyle=1
;oplot,time,neg_400_bar,linestyle=1

;write_png,keo_out_dir+string(cyr,format='(i4)')+string(cmon,format='(i2.2)')+string(cday,format='(i2.2)')+'_5_SME_2.png',tvrd(/true)
;free_lun,lunr2 

device,/close
set_plot,'x'

stop
end