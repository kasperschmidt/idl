;+
;----------------------------
;   NAME
;----------------------------
; modelslopeprofile.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure reading the output files from chisquareDavismodels.pro to
;retrieve the slopes, from which it will create a model slope
;"average" profile (as a function of z)
;----------------------------
;   COMMENTS
;----------------------------
; The three models are from Davis et al. 2007. They are:
;
; Model 1) Relativistic model of accretion onto a Scwartzschild black
;          hole (spin = 0). Emission based on Non-LTE atmosphere calculations.
; Model 2) Relativistic model of accretion onto a Scwartzschild black
;          hole (spin = 0). Disk emitting as a black body
; Model 3) Model of accretion onto a spinning black hole (spin = 0.9). 
;          Emission based on Non-LTE atmosphere calculations.
;
; For further details please refer to Davis et al. 2007
;----------------------------
;   INPUTS:
;----------------------------
;
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE      : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; ModSloPro       : 7xNzbin array with the model slope profiles. The content of the array is:
;                         ModSloPro[0,*]  = the redshift values
;                         ModSloPro[1,*]  = mean of slopes, model 1
;                         ModSloPro[2,*]  = mean of slopes, model 2
;                         ModSloPro[3,*]  = mean of slopes, model 3
;                         ModSloPro[1,*]  = median of slopes, model 1
;                         ModSloPro[2,*]  = median of slopes, model 2
;                         ModSloPro[3,*]  = median of slopes, model 3

;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> modelslopeprofile,ModSloPro,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-28  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO modelslopeprofile,ModSloPro,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

VB = n_elements(VERBOSE)

Nzval     = 30
Zarr      = (findgen(Nzval)*2+1)/10.

; - - - just doing it for a subset - - -
Zarr      = Zarr[2:12]
Nzval     = n_elements(Zarr)
; - - - - - - - - - - - - - - - - - - - -

ModSloPro = fltarr(7,Nzval)
ModSloPro(0,*) = Zarr
slopesALL = fltarr(9093,3,Nzval)

for ii=0,Nzval-1 do begin
   Zstring  = STRJOIN(STRSPLIT(Zarr(ii),'.',/extract),'p')
   file     =   'chisquareDavismodels'+strtrim(Zstring,2)+'_for_0z7AND0p0A2p0AND0p0gamma5p0_Wed_Dec_8_12:54:21_2010.idlsave'
   restore,file,DESCRIPTION=DESCRIP  ; restoring data
   slopesALL(*,*,ii) = slopes
   if vb eq 1 then print,':: modelslopeprofile.pro :: Restored data from '+strtrim(file,2)
endfor

for jj=0,2 do begin
   for kk=0,Nzval-1 do begin
      ;removing entries for non-existing fits
      goodent = where( slopesALL(*,jj,kk) ne -9999)
      ModSloPro(jj+1,kk) = mean( slopesALL(goodent,jj,kk) )
      ModSloPro(jj+4,kk) = median( slopesALL(goodent,jj,kk) )
   endfor
endfor

save,ModSloPro,filename='davismodels_GRslopeprofiles.idlsave'


if vb eq 1 then print,' '
if vb eq 1 then print,':: modelslopeprofile.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
