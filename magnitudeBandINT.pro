;+
;----------------------------
;   NAME
;----------------------------
; magnitudeBandINT.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure etimating the AB magnitude over a given band as described by eq. (7) 
; in Fukugita et al 1996.
;----------------------------
;   COMMENTS
;----------------------------
; if the returned magnitude has the value -9999 no transmission was present for the given
; frequency/wavelenght range
;----------------------------
;   INPUTS:
;----------------------------
; BandX           : The frequency (or wavelength) vector of the band to integrate over
;                   given in 1/s (or m) 
; Trans           : Vector giving the transmission of the band corresponding to the 
;                   BandX vector.
; DataX           : The frequency (or wavelength) vector of the flux data given in 1/s
;                   (or m) 
; Flux            : The flux to estimate the magnitude for corresponding to the DataX
;                   vector given in [erg/s/cm^2/Hz] (or [erg/s/cm^2/Å])
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /WAVE           : set /WAVE if the input is given in wavelengths instead of frequency
;                   which is the default input
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; magAB           : The estimated AB magnitude for the input flux over the given band
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> magnitudeBandINT,BandX,Trans,DataX,Flux,magAB,/WAVE,VERBOSE
;----------------------------
;   BUGS
;----------------------------
; - 101026 - Am not sure I trust the results... there seems to be
;   something fishy with the trapetsoidal summation; it doesn't
;   give the same result as the IDL routine TSUM.
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-04-30  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO magnitudeBandINT,BandXIN,TransIN,DataXIN,FluxIN,magAB,WAVE=WAVE,VERBOSE=VERBOSE

WV = n_elements(WAVE)
VB = n_elements(VERBOSE)
CC = 299792458.   ; m/s

; Storing input in new vectors not to corrupt the input data when called from another procedure
BandX = BandXIN
Trans = TransIN
DataX = DataXIN
Flux  = FluxIN

if WV eq 0 then begin  ; selecting between frequency and wavelength mode
   if vb eq 1 then print,':: magnitudeBandINT.pro :: The input flux is assumed given in [erg/s/cm^2/Hz] and X values in [Hz]'
endif else begin
   if vb eq 1 then print,':: magnitudeBandINT.pro :: The input flux is assumed given in [erg/s/cm^2/A] and X values in [m]'
   ; turning flux into nJy
   for ii=0,n_elements(Flux)-1 do begin
      ;print,'1',Flux(ii)
      Flux(ii) = Flux(ii) * 3.34*1d21 * (DataX(ii)*10^(6.))^2.    ; (erg/s/cm^2/A to nJy) from App. in P. Jakobsen 2004
      ;print,'2',Flux(ii)
   endfor
   ; turning vectors around (wavelengths into frequency)
   DataX = REVERSE(CC/DataX)
   BandX = REVERSE(CC/BandX)
   Trans = REVERSE(Trans)
   Flux  = REVERSE(Flux)*1d-32   ; going from nJy to erg/s/cm^2/Hz from App. in P. Jakobsen 2004
endelse

; removing entries outside the BandX range (here the transmission will be 0 either way)
Xent  = where(DataX gt BandX(0) and DataX lt BandX(n_elements(BandX)-1) )
if Xent eq [-1] then begin
   print,':: magnitudeBandINT.pro :: ERROR! -- No spectral values within bands'
   magAB = -9999.
   goto,endoffile
endif else begin
   DataX = DataX(Xent)
   Flux  = Flux(Xent)
endelse

; interpolating the filter to the x spacing of the data
TransINT = SPLINE(BandX,Trans,DataX)

; checking that some transmission will be present - if not magAB set to -9999
xx = where(transINT ne 0)
if xx eq [-1] then begin
   magAB = -9999.
endif else begin
   ; resetting sum totals
   NOMtot    = 0.0
   DENOMtot  = 0.0

   ; Calculating the integral as trapeziodal sums
   for jj=0,n_elements(Flux)-2 do begin
      NOMtot   = NOMtot + (FLUX(jj+1)*TransINT(jj+1)/DataX(jj+1) + FLUX(jj)*TransINT(jj)/DataX(jj))*(DataX(jj+1)-DataX(jj))
      DENOMtot = DENOMtot + (TransINT(jj+1)/DataX(jj+1) + TransINT(jj)/DataX(jj))*(DataX(jj+1)-DataX(jj))
;print,' NOMtot: ',NOMtot,' DENOMtot : ',DENOMtot
;if jj lt 10 then print,jj,' NOMtot: ',NOMtot,' DENOMtot : ',DENOMtot,'---',FLUX(jj+1),TransINT(jj+1),DataX(jj+1),DataXIN(jj+1), FLUX(jj),TransINT(jj),DataX(jj),DataXin(jj)
;print,'1',FLUX(jj+1),TransINT(jj+1),DataX(jj+1),FLUX(jj+1)*TransINT(jj+1)/DataX(jj+1) & print,'2',FLUX(jj)*TransINT(jj)/DataX(jj) & print,'3',(DataX(jj+1)-DataX(jj))
   endfor
   magAB = -2.5*alog10(NOMtot/DENOMtot) - 48.60
endelse

endoffile:
if vb eq 1 then print,' '
if vb eq 1 then print,':: magnitudeBandINT.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
