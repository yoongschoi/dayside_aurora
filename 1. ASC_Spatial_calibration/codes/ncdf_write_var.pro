;+

; NAME: NCDF_WRITE_VAR
; PURPOSE:
;     Write variable in the NetCDF file.
; EXPLANATION:
;     Write variable in the NetCDF file by simply giving variable, variable name, variable type.
;     Multi dimensional variable is also available.
;     When defining variable using IDL defualt function 'NCDF_VARDEF', it specifies 'SHUFFLE' keyword for higher compression density.
;     As a default, it also specifies 'GZIP' keyword to be 9 for the highest compression density.
; CALLING SEQUENCE:
;     NCDF_WRITE_VAR,FID,VAR,VAR_NAME,VAR_TYPE,[ATT=atributes] ('attributes': String that describe input 'var')
; ARGUMENTS:
;     FID       -  File ID (returned by IDL default function, 'NCDF_CREATE' or 'NCDF_OPEN') or
;                  Group ID (returned by IDL default function, 'NCDF_GROUPDEF')
;     VAR       -  Variable to be saved in 'fid'
;     VAR_NAME  -  Name of the variable (String)
;     VAR_TYPE  -  Type of the variable to be saved (string)
;                  -------- List of the acceptable strings for 'var_type' argument --------
;                  1) 'str'    (String)
;                  2) 'flt'    (Float)
;                  3) 'dbl'    (Double)
;                  4) 'int'    (Integer)
;                  5) 'uint'   (Unsigned Integer)
;                  6) 'byt'    (Byte)
;                  7) 'lon'    (longword Integer)
;                  8) 'ulon'   (Unsigned Longword Integer)
;                  9) 'ulon64' (64-bit Unsigned Longword Integer)
;                  ------------------------------------------------------------------------
; KEYWORDS:
;     ATT       -  Set this keyword to give an attribute for the variable
;     GZIP      -  Set this keyword to an integer between zero and nine to specify the level of
;                  GZIP compression applied to the variable. Lower compression values result in
;                  faster but less efficient compression.
;                  This keyword is ignored if the created NCDF file is not in NetCDF4 format.
;                  Default value is 9 (Highest compression is intended).
;     NO_GZIP   -  Set this keyword to prevent GZIP compression for the fastest operation.
;                  It is not recommended to set this keyword, because output NC file can be huge size.
;                  IT is recommended to set 'GZIP' keyword to be 1 rather than setting this keyword.
; HISTORY:
;     Mar/20/2018, First version made.
;-
pro NCDF_write_var,fid,var,var_name,var_type,att=att,gzip=gzip,no_gzip=no_gzip
  if keyword_set(gzip) eq 0 then gzip=9
  if keyword_set(no_gzip) then gzip=0
  var_size=size(var,/dimen)
  if n_elements(var_size) eq 1 then begin
    did=ncdf_dimdef(fid,var_name+' dimen',var_size)
  endif else begin
    for i=0L,n_elements(var_size)-1 do begin
      if i eq 0 then begin
        did=ncdf_dimdef(fid,var_name+' dimen'+strtrim(string(i),2),var_size[i])
      endif else begin
        did=[did,ncdf_dimdef(fid,var_name+' dimen'+strtrim(string(i),2),var_size[i])]
      endelse
    endfor
  endelse
  if var_type eq 'str' then begin
    vid=ncdf_vardef(fid,var_name,did,/string,gzip=gzip,/shuffle)
  endif else if var_type eq 'flt' then begin
    vid=ncdf_vardef(fid,var_name,did,/float,gzip=gzip,/shuffle)
  endif else if var_type eq 'dbl' then begin
    vid=ncdf_vardef(fid,var_name,did,/double,gzip=gzip,/shuffle)
  endif else if var_type eq 'int' then begin
    vid=ncdf_vardef(fid,var_name,did,/short,gzip=gzip,/shuffle)
  endif else if var_type eq 'uint' then begin
    vid=ncdf_vardef(fid,var_name,did,/ushort,gzip=gzip,/shuffle)
  endif else if var_type eq 'byt' then begin
    vid=ncdf_vardef(fid,var_name,did,/byte,gzip=gzip,/shuffle)
  endif else if var_type eq 'lon' then begin
    vid=ncdf_vardef(fid,var_name,did,/long,gzip=gzip,/shuffle)
  endif else if var_type eq 'ulon' then begin
    vid=ncdf_vardef(fid,var_name,did,/ulong,gzip=gzip,/shuffle)
  endif else if var_type eq 'ulon64' then begin
    vid=ncdf_vardef(fid,var_name,did,/uint64,gzip=gzip,/shuffle)
  endif else begin
    message,var_type+' is not allowed as a variable type.'
  endelse
  ncdf_varput,fid,vid,var
  if keyword_set(att) then ncdf_attput,fid,vid,'Description',att,/char
end