;+
;----------------------------
;   NAME
;----------------------------
; kendalltau.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Calculating the kendall tau of a set of data. Kendall tau is a
; rather simple method to determine whether a set of points is
; dependent or not. In other words you can test how well your points
; (for instance a time sequence) is ordered
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; dataX          : vector containing the x components of the data (time)
; dataY          : vector containing the y components of the data (measurement)
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE       : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; Ktau           : The kendall tau value returned
;                  Ktau = 1         perfectly ordered      (concordant)
;                  Ktau = 0         dataX and dataY are independent
;                  Ktau = -1        perfectly disordered   (discordant)
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> kendalltau,[1,2,3,4,5,6,7,8,9,10],[12,10,35,20,53,64,78,80,91,98],Ktau,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-08-09  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO kendalltau,dataX,dataY,Ktau,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)

if n_elements(dataX) ne n_elements(dataY) then begin
   print,':: kendalltau.pro :: ERROR the dimensions of dataX and dataY are different '
   print,'                     skipping calculation'
   goto,lastline
endif else begin
   NN = n_elements(dataX)
endelse

numerator = 0.0 ; resetting

for ii=0,NN-1 do begin
   for jj=ii+1,NN-1 do begin
      if dataX(ii)-dataX(jj) lt 0 AND dataY(ii)-dataY(jj) lt 0 then numerator = numerator + 1.  ; concordant
      if dataX(ii)-dataX(jj) gt 0 AND dataY(ii)-dataY(jj) gt 0 then numerator = numerator + 1.  ; concordant
      if dataX(ii)-dataX(jj) lt 0 AND dataY(ii)-dataY(jj) gt 0 then numerator = numerator - 1.  ; discordant
      if dataX(ii)-dataX(jj) gt 0 AND dataY(ii)-dataY(jj) lt 0 then numerator = numerator - 1.  ; discordant
      if dataX(ii)-dataX(jj) eq 0 AND dataY(ii)-dataY(jj) eq 0 then begin
         print,':: kendalltau.pro :: A tied pair was found (dataX(i)=dataX(j) AND dataY(i)=dataY(j)).'
         print,'                     This code does NOT take this into account! '
      endif
   endfor
endfor

denominator = NN*(NN-1)/2.

Ktau = numerator/denominator

if vb eq 1 then print,":: kendalltau.pro :: The estimated Kendall's tau returned is: ",strtrim(Ktau,2)

lastline:
if vb eq 1 then print,' '
if vb eq 1 then print,':: kendalltau.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
