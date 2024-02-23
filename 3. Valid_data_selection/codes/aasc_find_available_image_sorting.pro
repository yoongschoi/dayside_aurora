
; NAME: AASC_FIND_AVAILABLE_IMAGE_SORTING
; PURPOSE:
;     Determine whether the input images are valid or not.
; EXPLANATION:
;     Find valid images in the JBS A-ASC images and copy both of the
;     good and the bad images in the separate directories. The input
;     images are processed images (3x567x567). The coordinates used
;     in generating the processed images must be the geomagnetic
;     coordinates. If it is the geographic coordinates, you must
;     adjust the 'FFT part' or turn it off. The FFT is used to mitigate
;     the stripe-shaped noises.
; INPUTS:
;     The processed images whose dimension is 3 by 567 by 567.
; HISTORY:
;     First version made: ???/??/2019
;
pro AASC_find_available_image_sorting


; Input Directory
  aasc_dir='E:\AASC_processed\'
;
; Output Directory
  out_dir_good='E:\AASC_available_sorted\good\'
  out_dir_bad='E:\AASC_available_sorted\bad\'
;
  
  aasc_dir=strjoin(strsplit(aasc_dir,'/\',/ext),path_sep())+path_sep()
  out_dir_good=strjoin(strsplit(out_dir_good,'/\',/ext),path_sep())+path_sep()
  out_dir_bad=strjoin(strsplit(out_dir_bad,'/\',/ext),path_sep())+path_sep()
  
sdate=(edate='')
print,'Enter the start date (YYYY MM DD):' & read,sdate
print,'Enter the end date (YYYY MM DD):' & read,edate
spl_sdate=strsplit(sdate,' :-/',/ext)
spl_edate=strsplit(edate,' :-/',/ext)
sjday=julday(spl_sdate[1],spl_sdate[2],spl_sdate[0])
ejday=julday(spl_edate[1],spl_edate[2],spl_edate[0])
jday0=sjday
num_day=ejday-sjday+1
  
  
  img_sz=[3,567,567] ; size of the processed image
; variables for image processing used in star-recognition
  ;random_noise=randomu(0,img_sz[1],img_sz[2],/double)*0.001d + 0.999d
  pattern=[[0,1,0],[1,1,1],[0,1,0]]
  Rch_raw=(Gch_raw=(Bch_raw=fltarr(img_sz[1],img_sz[2])))
  FFT_mask=bytarr(img_sz[1],img_sz[2])+1
  a=3.0813798032898232d
  b=-191*a
  y1d=indgen(img_sz[2])
  x1d=(y1d-b)/a
  FFT_mask[long(x1d),long(y1d)]=0
  a=3.0813798032898232d
  b=-190*a
  y1d=indgen(img_sz[2])
  x1d=(y1d-b)/a
  FFT_mask[long(x1d),long(y1d)]=0
  a=3.0813798032898232d
  b=-192*a
  y1d=indgen(img_sz[2])
  x1d=(y1d-b)/a
  FFT_mask[long(x1d),long(y1d)]=0
  FFT_mask[img_sz[1]/2,img_sz[2]/2]=0
  filter=1
;

;  Set FOV used in star recognition
  ele_limit=20
  r_limit=(90-ele_limit)/90d
  
  circle3D=bytarr(img_sz[0],img_sz[1],img_sz[2])
  circle=bytarr(img_sz[1],img_sz[2])
  y_idx=rebin(transpose(dindgen(img_sz[2])/(img_sz[2]-1)*2-1),img_sz[1],img_sz[2],/sam)
  x_idx=transpose(y_idx)
  radi=sqrt(x_idx^2 + y_idx^2)
  FOV=where(radi lt r_limit)
  circle[FOV]=1
  circle3D[0,*,*]=circle
  circle3D[1,*,*]=circle
  circle3D[2,*,*]=circle
  ele=90d*(1-radi)
  inavail=where(ele lt ele_limit)
;



for dd=0L,num_day-1 do begin
  caldat,jday0,mon,day,yr
  dstring=string(yr,format='(i4)')+'/'+string(mon,format='(i2.2)')+'/'+string(day,format='(i2.2)')
  
  aasc_fname=file_search(aasc_dir+strjoin(strsplit(dstring,'/',/ext),path_sep())+$
    path_sep()+'*.png',count=num_file)
  if num_file eq 0 then goto, skip_day
  img_sz=size(read_png(aasc_fname[0]),/dimen)
  
  
  spl_fbname=(strsplit(file_basename(aasc_fname),'_.',/ext)).toArray()
  hhmmss=strmid(spl_fbname[*,6],0,2)+':'+strmid(spl_fbname[*,6],2,2)+':'+strmid(spl_fbname[*,6],4,2)
  spl_hhmmss=(strsplit(hhmmss,':',/ext)).toArray()
  
  t24=spl_hhmmss[*,0] + spl_hhmmss[*,1]/60d + spl_hhmmss[*,2]/3600d
  
  pos=check_moon_and_sun_function(yr,mon,day,spl_hhmmss[*,0],spl_hhmmss[*,1])
  if pos[0] eq -1 then begin
    print,dstring+' ------> No images satisfying sun and moon condition.'
    num_file=0L
  endif else begin
    print,dstring+' ------> Sorting started......'
    st=systime(/sec)
    num_file=n_elements(pos)
    aasc_fname=aasc_fname[pos]
    spl_fbname=spl_fbname[pos,*]
    hhmmss=hhmmss[pos]
    spl_hhmmss=spl_hhmmss[pos,*]
    t24=t24[pos]
    sub_fname=strjoin(transpose(((strsplit(aasc_fname,'\',/ext)).toArray())[*,2:-1]),'\')
    file_mkdir,out_dir_good+file_dirname(sub_fname[0])
    file_mkdir,out_dir_bad+file_dirname(sub_fname[0])
  endelse  
; ################################# Star Recognition #################################
  for i=0L,num_file-1 do begin
    
    img=read_png(aasc_fname[i])
    Bch_raw[*,*]=img[2,*,*]
    recon_Bch=double(Bch_raw);*circle
    recon_Bch[inavail]=median(recon_Bch[FOV])
    
    recon_Bch=FFT(recon_Bch,/cen)
    recon_Bch[where(ele gt 89.5)]=0
    
    recon_Bch[img_sz[1]/2,img_sz[1]/2]=0
    recon_Bch=recon_Bch*FFT_mask
    
    recon_Bch=FFT(recon_Bch*filter*FFT_mask,/inv,/cen)
    recon_Bch=real_part(recon_Bch);*circle
    recon_Bch[where(recon_Bch lt 0)]=0    
    
    recon_Bch=dilate(recon_Bch,pattern,/gray)
    
    recon_Bch=bytscl(recon_Bch,max=255,min=3)
    recon_Bch[where(recon_Bch gt 0)]=1
    recon_Bch[inavail]=0    

    num_star=max(label_region(recon_Bch))
;    if num_star gt max_numr then num_star=max_numr
    
    if num_star gt 30 then begin
      ;print,'good'
      file_copy,aasc_fname[i],out_dir_good+sub_fname[i]
    endif else begin
      ;print,'bad'
      file_copy,aasc_fname[i],out_dir_bad+sub_fname[i]
    endelse
    
  endfor
  if num_file ne 0 then print,'Processing finished (Elapsed time= '+string(systime(/sec)-st,format='(f5.1)')+' seconds).'
; ####################################################################################
  skip_day:
  jday0++
endfor
end