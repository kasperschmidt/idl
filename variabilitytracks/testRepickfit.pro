;+
;----------------------------
;   NAME
;----------------------------
; testRepickfit.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure defining a perfect data set (i.e.) it has a perfekt
; relation, and then re-picking gaussian derivatives of that data set
; to se the dependence scatter has on the estimated fit.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; Derr            : Vector containg the change of the intital errors
;                   when re picking data, i.e.  the size of the gaussian 
;                   clouds to pick the new data sets from. 
;                   Derr should be given as a percentage of existing errors 
;                   (multiplication factor, e.g. Derr=2 means twice as large
;                   error bars. The code will run Nrepicks per Derr value.
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Nrepicks        : the number of data sets to repick and fit with linearfitMCMC.
;                   default is 2
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> testRepickfit,[3.0,5.0],Nrepicks=10,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-17  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ linearfitMCMC.pro
@ plottestRepickfit.pro
;----------------------------
;-
PRO testRepickfit,Derr,Nrepicks=Nrepicks,EPS=EPS,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

; creating perfect linaer relation as intial data set
Npoints  = 40             ; number of data points
DATARRAY = fltarr(Npoints,4)   ; data array containing x,y,xerr,yerr

; x-component picked randomly
Xmindat                = 19                                 ; the minimum magnitude of x component
XNmags                 = 1.5                                  ; the number of magnitudes the x component span
Xran                   = RANDOMU(seed,Npoints)              ; random points drawn from [0,1] distribution
DATARRAY(*,0)          = Xmindat + XNmags*Xran              ; x component
MeanX                  = mean(DATARRAY(*,0))
; y-component from linear relation
slope                  = 0.76                               ; slope of linear relation
offset                 = 0.0                                ; offset of linear relation
Dxymean                = -0.5                               ; deiffenrece between x and y mean magnitudes
meanY                  = MeanX+Dxymean                      ; mena of y 
DATARRAY(*,1)          = ( slope*(DATARRAY(*,0)-MeanX) + offset ) + meanY

; estimating analytic expression of (r) errors for mags 17-20.5ish 
expr = 'P[0]*X^2 + P[1]*X + P[2]'        ; epression to fit
start = [1e-4,-3,0]                      ; intial guess
xx = [17,18,19,19.5,20,20.5]             ; read off r mags
yy = [0.015,0.017,0.02,0.03,0.035,0.04]  ; corresponding r mag errors
result = MPFITEXPR(expr,xx,yy,yyerr,start,/QUIET) 

; x erros drawn from gaussian with sigma dependent on mag
SIGMAX                 = 0.001                              ; standard deviation of gaussian distribution
MEANGAUSSX             = result[0]*DATARRAY(*,0)^2+result[1]*DATARRAY(*,0)+result[2] ; mean set by empirical expression for (r) photometric error
GAUSSX                 = RANDOMN(seed,Npoints)              ; random gaussian point drawn from [0,1] distribution
DATARRAY(*,2)          = GAUSSX * SIGMAX + MEANGAUSSX       ; gaussian drawn errors
; y erros drawn from gaussian with sigma dependent on mag
SIGMAY                 = 0.001                              ; standard deviation of gaussian distribution
MEANGAUSSY             = result[0]*DATARRAY(*,1)^2+result[1]*DATARRAY(*,1)+result[2] ; mean set by empirical expression for (r) photometric error
GAUSSY                 = RANDOMN(seed,Npoints)              ; random gaussian point drawn from [0,1] distribution
DATARRAY(*,3)          = GAUSSY * SIGMAY + MEANGAUSSY       ; gaussian drawn errors

if vb eq 1 then print,':: testRepickfit.pro :: The drawn magnitudes are:'
if vb eq 1 then print,DATARRAY(sort(DATARRAY(*,0)),0)
if vb eq 1 then print,'                        And:'
if vb eq 1 then print,DATARRAY(sort(DATARRAY(*,1)),1)
if vb eq 1 then print,':: testRepickfit.pro :: With the (empirical) errors:'
if vb eq 1 then print,DATARRAY(sort(DATARRAY(*,0)),2)
if vb eq 1 then print,'                        And:'
if vb eq 1 then print,DATARRAY(sort(DATARRAY(*,1)),3)  



if n_elements(Nrepicks) eq 0 then Nrepicks = 2 ; the number of data sets to re-pick and retrieve slope and offset for
Nerrcase = n_elements(Derr)                    ; the number of different errors to run for

for kk=0,Nerrcase-1 do begin  ; looping over the number of cases to run
   if vb eq 1 then print,':: testRepickfit.pro :: Repicking '+strtrim(Nrepicks,2)+' new data sets from intial data with '+strtrim(Derr(kk),1)+' times '
   if vb eq 1 then print,'                        the intial error as gaussian widht and fitting them via MCMC' 
   MCMCloops = 10000
   Derrstring = STRJOIN(STRSPLIT(Derr(kk),'.',/extract),'p')
   for ii=0,Nrepicks-1 do begin
      repickdata,datarray,Derr(kk),datarrayOUT;,/plot,/VERBOSE
      datarrayREPICK = datarrayOUT
      outPDF = 'testRepickfitfiles/testRepickfit_Derr'+strtrim(Derrstring,2)+'_'+strtrim(ii,2)+'.dat'
      linearfitMCMC,datarrayOUT,fitres,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT
      if ii eq 0 then begin
         DATTOT = [datarrayREPICK]            ; starting array with all data set
         RESTOT = [fitres]                    ; starting array with all fits
      endif else begin
         DATTOT = [[DATTOT],[datarrayREPICK]] ; appending new data set
         RESTOT = [[RESTOT],[fitres]]         ; appending new fit
      endelse
   endfor
   ; --- saving parameters ---
   savefile = 'testRepickfitfiles/FitresultANDdataarrays_Nrepicks'+strtrim(Nrepicks,2)+'_Derr'+strtrim(Derrstring,2)+'.idlsave'
   save,DATARRAY,DATTOT,RESTOT,filename=savefile
   ;restore,savefile  ;  restoring variables
   ; -------------------------
   if VB eq 1 then plottestRepickfit,savefile,[slope,offset],/verbose
   if VB eq 0 then plottestRepickfit,savefile,[slope,offset]
endfor


if vb eq 1 then print,' '
if vb eq 1 then print,':: testRepickfit.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
