;+
;----------------------------
;   NAME
;----------------------------
; bootstrappercent.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure returning bootstrapping erros on a list of values.
; It does this by drawing a random vector from the input vector of the same
; size N times and using this distrbution of values to define an errorbar.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; values          : vector containg values to obtain errors on
; Nboot           : the number of bootstrappings to make
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; result          : Array containing percentage and bootstrapped error
;                   of the individual values in the 'values' vector on
;                   the format:
;                      result(*,0) = [Value0 , percent in 'values' vec , 1sigma- , 1sigma+ , 2sigma- , 2sigma+]
;                      result(*,1) = [Value1 , percent in 'values' vec , 1sigma- , 1sigma+ , 2sigma- , 2sigma+]
;                          ...
;                      result(*,N) = [ValueN , percent in 'values' vec , 1sigma- , 1sigma+ , 2sigma- , 2sigma+]
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; You want to get an uncertaintiy on the result that 25% of your 100
; draws have the value 10 in the vector defined by: 
; IDL> vec = fltarr(100)+10
; IDL> vec(0:74) = 5
;
; IDL> bootstrappercent,vec,1000,result,/VERBOSE,/STP
;
; which returns the result array:
; IDL> print,result 
;      5.00000      75.0000      5.00000      2.00000      7.00000      10.0000
;      10.0000      25.0000      4.00000      8.00000      9.00000      10.0000
;
; SF type for simulations in paper. 
; IDL> sim30  = fltarr(132)+1.0 & sim30(0:89) = 2.0 & sim30(90:111) = 5.0
; IDL> sim90  = fltarr(105)+1.0 & sim90(0:50) = 2.0 & sim90(51:63) = 5.0
; IDL> sim150 = fltarr(59)+1.0 & sim150(0:33) = 2.0 & sim150(34:36) = 5.0
;
; IDL> bootstrappercent,sim30,1000,result,/VERBOSE
;
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2012-06-07  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
;@ xxx.pro
;----------------------------
;-
PRO bootstrappercent,values,Nboot,result,VERBOSE=VERBOSE,STP=STP
VB = n_elements(VERBOSE)

values = values(sort(values)) ; sorting vector

Nval    = n_elements(values)
valuniq = values(uniq(values))
Nuniq   = n_elements(valuniq)
if vb eq 1 then print,' '
if vb eq 1 then print,':: bootstrappercent.pro :: Found '+trim(Nval)+' values in input vector'
if vb eq 1 then print,'                           among which there were '+trim(Nuniq)+' unique values'
if vb eq 1 then print,' '

result = fltarr(6,Nuniq)

for ii=0,Nuniq-1 do begin   ; looping over uniqe values

   BSvalues = fltarr(Nboot)  ; vector for bootstrap values

   for jj=0,Nboot-1 do begin  ; bootstrapping
      entrand      = Round( RandomU(seed,Nval)*Nval )          ; randomly chosen objects (each object can be chosen multiple times) 
      BSvals       = values(entrand)
      match        = where(BSvals eq valuniq(ii), Nmatch)
      BSvalues(jj) = (Nmatch+0.0)/(Nval+0.0)*100.
   endfor
   ; getting percentile of distribution
   pctile  = 0.682  ; 1 sigma
   onesig  = PERCENTILES(BSvalues(*),CONFLIMIT=pctile)
   pctile  = 0.956  ; 2 sigma
   twosig  = PERCENTILES(BSvalues(*),CONFLIMIT=pctile)

   valuniqent   = where(values eq valuniq(ii),Nvaluniq)   ; counting values in input vector
   inputP       = (Nvaluniq+0.0)/(Nval+0.0)*100           ; percentage in input vector
   result(*,ii) = [valuniq(ii),inputP,inputP-onesig(0),onesig(1)-inputP,inputP-twosig(0),twosig(1)-inputP]

   if vb eq 1 then print,'                           Value: '+trim(result(0,ii),'(f8.1)')+' was seen in '+trim(result(1,ii),'(f8.1)')+' -'+trim(result(2,ii),'(f8.1)')+'/+'+trim(result(3,ii),'(f8.1)')+' (1sig)  -'+trim(result(4,ii),'(f8.1)')+'/+'+trim(result(5,ii),'(f8.1)')+' (2sig) % of the input vector'

endfor


if vb eq 1 then print,' '
if vb eq 1 then print,':: bootstrappercent.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END

