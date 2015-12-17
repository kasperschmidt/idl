;+
;----------------------------
;   NAME
;----------------------------
; linearfitMCMC.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This prcedure fits a 2D data set with errors in both directions to a
; linear relation (y = a*x +b), following the chapter 7 in Hogg et al. 2010
; (astro-ph 1008.4686v1) with the possibility of accounting for outliers. 
; The code provides the MCMC propability distributions in a fits
; binary table for plotting and further analysis.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; DATARRAY        : array containing data points and there errors to
;                   which the lines is to be fitted. The array is on
;                   the form:
;                         DATARRAY(N,0) = x-components
;                         DATARRAY(N,1) = y-components
;                         DATARRAY(N,2) = x-component uncertainties
;                         DATARRAY(N,3) = y-component uncertainties
;                   where N is the number of data points
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Nmcmc           : The number of parameters to draw in the MCMC fit
;                   (default is only 1000 - i.e. for testing)
; /OUTLIERS       : set /OUTLIERS for pruning the outliers when fitting
; COVAR           : if covariances are present use this keyword to
;                   provide the individual covariances (default
;                   is assuming that all covariances are 0).
;                   The input vector shall be on the form:
;                   COVAR = fltaarr(N) where N number of data points
;                   and COVAR(i) = dxy_i   ; assuming dxy_i = dyx_i 
; /PLOT           : set /PLOT to plot the MCMC probability
;                   distributions for the fits
; /EPS            : set /EPS to write plots to .eps files (only works with /PLOT)
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; output          : Name of the binary fits output from the fit
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; Run with data from table one in Hogg et al. 2010
; IDL> w/o 4 first point... datarray=[[203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]]
; IDL> datarray=[[201,244,47,287,203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[592,401,583,402,495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]]
; IDL> linearfitMCMC,DATARRAY,/PLOT,/OUTLIERS,/VERBOSE,Nmcmc=10000
; COVAR=[-0.84,0.31,0.64,-0.27,-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]

; w/o 4 first points... COVAR=[-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*[5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-22  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ contourarray.pro
;----------------------------
;-
PRO linearfitMCMC,DATARRAY,Nmcmc=Nmcmc,OUTLIERS=OUTLIERS,PLOT=PLOT,EPS=EPS,VERBOSE=VERBOSE,COVAR=COVAR
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

PO  = n_elements(OUTLIERS)
NM  = n_elements(Nmcmc)
CV  = n_elements(COVAR)
PL  = n_elements(PLOT)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)

Ndat = n_elements(DATARRAY(*,0))        ; number of data points
if CV eq 0 then COVAR = fltarr(Ndat)    ; setting covariances to 0 if not given
if NM eq 0 then Nmcmc = 1000            ; setting number of iterations if not given

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; inital guess (chi2 fit from chapter 1 in Hogg et al 2010)
Yinit  = DATARRAY(*,1)
onevec = [fltarr(Ndat)+1]
Ainit  = [[onevec],[DATARRAY(*,0)]]
Cinit  = fltarr(Ndat,Ndat)         ; defining covariance matrix
for ii=0L,Ndat-1 do begin
   Cinit(ii,ii) = DATARRAY(ii,3)*DATARRAY(ii,3)  ; filling covariance matrix with y errors
endfor
ab_init = invert( transpose(Ainit)#invert(Cinit)#Ainit ) # ( transpose(Ainit)#invert(Cinit)#Yinit )
;ab_init = [100,1.1]
theta_init  = atan(ab_init(1))              ; angle between line and x-axis
b_perp_init = ab_init(0)*cos(theta_init)    ; perpendicular distance between origin (0,0) and line
guess_init = [b_perp_init,theta_init]       ; the inital guess of parameters to MCMC over [theta,b_perp,...
if PO eq 0 then logL_init = LNL(guess_init,DATARRAY,COVAR)  ; Calculating the initial likelihood
if PO eq 1 then logL_init = LNL_PO(guess_init,DATARRAY,COVAR)  ; Calculating the initial likelihood
bestL     = logL_init                       ; setting the best likelihood to be updated in MCMC loop
bestfit   = ab_init                         ; setting the best line parameters to be ubdated in MCMC loop
guessesALL = [bestfit,bestL]                ; defining first line of array with all MCMC values

if PL eq 1 then begin
   col=getcolor(/load)
   window, 0, xsize=600, ysize=500
   plot,DATARRAY(*,0),DATARRAY(*,1),psym=2
   oplot,findgen(300),ab_init(1)*(findgen(300))+ab_init(0),linestyle=2,col=col.blue
endif
if vb eq 1 then print,':: linearfitMCMC.pro :: The intial guess is         : y = ',strtrim(ab_init(1),2),' x + ',strtrim(ab_init(0),2)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; MCMC'ing through parameter space
guess_new = guess_init*0.0 ; vector for MCMC guesses
if PO eq 0 then SIG       = [5.,!pi/100]   ; width of the gaussian distributions to draw new parameters from
if PO eq 1 then SIG       = [5.,!pi/100]
guess_old = guess_init
logL_old  = logL_init
Naccept   = 0               ; resetting counter
for jj=0L,Nmcmc-1 do begin  ; start of MCMC loop
   ; drawing new paramters from specified Gaussians around current paramters
   guess_new[0] = guess_old[0] + RANDOMN(seed) * SIG[0]
   guess_new[1] = guess_old[1] + RANDOMN(seed) * SIG[1]

   if PO eq 0 then begin
      logL_new   = LNL(guess_new,DATARRAY,COVAR)     ; Calculating the likelihood
      guessesALL = [[guessesALL],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]] ; appending guess to array
   endif else begin
      logL_new   = LNL_PO(guess_new,DATARRAY,COVAR)  ; Calculating the likelihood
      guessesALL = [[guessesALL],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]] ; appending guess to array
   endelse

   ; testing and selecting
   RR    = RANDOMU(seed)  ; random number between 0 and 1
   Lfrac = logL_new/logL_old
   if RR gt Lfrac then begin
      logL_old  = logL_new
      guess_old = guess_new
      Naccept = Naccept+1   ; counting the number of accepted gue guesses
   endif

   if logL_old lt bestL then begin
      bestL     = logL_new                                                     ; updating best L
      bestfit   = [guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]  ; updating best guess
   endif

   if PL eq 1 then oplot,findgen(300),tan(guess_new[1])*findgen(300)+guess_new[0]/cos(guess_new[1]),linestyle=0,col=col.green
   if PL eq 1 then oplot,DATARRAY(*,0),DATARRAY(*,1),psym=2
;wait,0.1
endfor                     ; end of MCMC loop
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if vb eq 1 then print,':: linearfitMCMC.pro :: ',strtrim(Naccept,2),' steps were accepted'
if vb eq 1 then print,':: linearfitMCMC.pro :: The best fit overall gives  : y = ',strtrim(bestfit(1),2),' x + ',strtrim(bestfit(0),2)
if vb eq 1 then print,'                        Corresponding to a K+lnL of ',strtrim(bestL,2)

if PO ne 0 then begin
   if vb eq 1 then print,':: linearfitMCMC.pro :: Accounting for outliers not enabled. '
endif

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if vb eq 1 then print,':: linearfitMCMC.pro :: Fits output not enabled. '


if PL ne 0 then begin
   if vb eq 1 then print,':: linearfitMCMC.pro :: Plotting not enabled. '
   oplot,findgen(300),ab_init(1)*(findgen(300))+ab_init(0),linestyle=2,col=col.blue
   oplot,findgen(300),bestfit[1]*findgen(300)+bestfit[0],linestyle=0,col=col.red,thick=2
endif

openw,55,'guessesALL.txt'
printf,55,guessesALL
close,55
if PL eq 1 then begin
   window, 1, xsize=600, ysize=500
   plot,guessesALL(0,*),guessesALL(1,*),psym=3,xrange=[min(guessesALL(0,*)),max(guessesALL(0,*))],yrange=[min(guessesALL(1,*)),max(guessesALL(1,*))],/xstyle,/ystyle
   contourarray,guessesALL(0,*),min(guessesALL(0,*)),max(guessesALL(0,*)),guessesALL(1,*),min(guessesALL(1,*)),max(guessesALL(1,*)),40,40,4,contarr,levelbin,xrange,yrange
   contour,contarr,xrange,yrange,/overplot,levels=levelbin, C_COLOR=[col.green],thick=3  
endif

if vb eq 1 then print,' '
if vb eq 1 then print,':: linearfitMCMC.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END

FUNCTION LNL,param,DATARRAY,COVAR
   ; Function calculating the log likelihood given in equation 32 of Hogg et al. 2010
   ; expression for DELTA_i (eq 30 in Hogg et al. 2010)
   DELTA  = -sin(param[1])*DATARRAY(*,0)+cos(param[1])*DATARRAY(*,1)-param[0]  
   ; expression for SIGMA_i (eq 31 in Hogg et al. 2010)
   SIGMA2 = sin(param[1])^2.*DATARRAY(*,2)^2.+cos(param[1])^2.*DATARRAY(*,3)^2.-2*cos(param[1])*sin(param[1])*COVAR
   ; expression for lnL (eq 32 in Hogg et al. 2010)
   vecLNL = DELTA^2./(2*SIGMA2)  ; is in fact the chi2 value so should be minimized!
   RETURN, total(vecLNL)  ; calculating the total sum
END

FUNCTION LNL_PO,param,DATARRAY,COVAR  ; LNL for pruning outliers
   ; Function calculating the log likelihood given in equation 32 of Hogg et al. 2010
   vec = [-sin(param[1]),cos(param[1])]
   ZZ  = [[DATARRAY(*,0)],[DATARRAY(*,0)]]
   ; expression for DELTA_i (eq 30 in Hogg et al. 2010)
   DELTA  = 
   ; expression for SIGMA_i (eq 31 in Hogg et al. 2010)
   SIGMA2 = 
   ; expression for lnL (eq 32 in Hogg et al. 2010)
   vecLNL =   ; is in fact the chi2 value so should be minimized!
   RETURN, total(vecLNL)  ; calculating the total sum
END



















;+
;----------------------------
;   NAME
;----------------------------
; linearfitMCMC.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This prcedure fits a 2D data set with errors in both directions to a
; linear relation (y = a*x +b), following the chapter 7 in Hogg et al. 2010
; (astro-ph 1008.4686v1) with the possibility of accounting for outliers. 
; The code provides the MCMC propability distributions in a fits
; binary table for plotting and further analysis.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; DATARRAY        : array containing data points and there errors to
;                   which the lines is to be fitted. The array is on
;                   the form:
;                         DATARRAY(N,0) = x-components
;                         DATARRAY(N,1) = y-components
;                         DATARRAY(N,2) = x-component uncertainties
;                         DATARRAY(N,3) = y-component uncertainties
;                   where N is the number of data points
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Nmcmc           : The number of parameters to draw in the MCMC fit
;                   (default is only 1000 - i.e. for testing)
; /OUTLIERS       : set /OUTLIERS to take pruning outliers into
;                   account when fitting the data 
; COVAR           : if covariances are present use this keyword to
;                   provide the individual covariances (default
;                   is assuming that all covariances are 0).
;                   The input vector shall be on the form:
;                   COVAR = fltaarr(N) where N number of data points
;                   and COVAR(i) = dxy_i   ; assuming dxy_i = dyx_i 
; /PLOT           : set /PLOT to plot the MCMC probability
;                   distributions for the fits
; /EPS            : set /EPS to write plots to .eps files (only works with /PLOT)
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; output          : Name of the binary fits output from the fit
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; Run with data from table one in Hogg et al. 2010
; IDL> w/o 4 first point... datarray=[[203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]]
; IDL> datarray=[[201,244,47,287,203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[592,401,583,402,495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]]
; IDL> linearfitMCMC,DATARRAY,/PLOT,/OUTLIERS,/VERBOSE,Nmcmc=10000
; COVAR=[-0.84,0.31,0.64,-0.27,-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]

; w/o 4 first points... COVAR=[-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*[5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-22  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ contourarray.pro
;----------------------------
;-
PRO linearfitMCMC,DATARRAY,Nmcmc=Nmcmc,OUTLIERS=OUTLIERS,PLOT=PLOT,EPS=EPS,VERBOSE=VERBOSE,COVAR=COVAR
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

OUT = n_elements(OUTLIERS)
NM  = n_elements(Nmcmc)
CV  = n_elements(COVAR)
PL  = n_elements(PLOT)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)

Ndat = n_elements(DATARRAY(*,0))        ; number of data points
if CV eq 0 then COVAR = fltarr(Ndat)    ; setting covariances to 0 if not given
if NM eq 0 then Nmcmc = 1000            ; setting number of iterations if not given

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; inital guess (chi2 fit from chapter 1 in Hogg et al 2010)
Yinit  = DATARRAY(*,1)
onevec = [fltarr(Ndat)+1]
Ainit  = [[onevec],[DATARRAY(*,0)]]
Cinit  = fltarr(Ndat,Ndat)         ; defining covariance matrix
for ii=0L,Ndat-1 do begin
   Cinit(ii,ii) = DATARRAY(ii,3)*DATARRAY(ii,3)  ; filling covariance matrix with y errors
endfor
ab_init = invert( transpose(Ainit)#invert(Cinit)#Ainit ) # ( transpose(Ainit)#invert(Cinit)#Yinit )
;ab_init = [100,1.1]
theta_init  = atan(ab_init(1))              ; angle between line and x-axis
b_perp_init = ab_init(0)*cos(theta_init)    ; perpendicular distance between origin (0,0) and line
guess_init = [b_perp_init,theta_init]       ; the inital guess of parameters to MCMC over [theta,b_perp,...
logL_init = LNL(guess_init,DATARRAY,COVAR)  ; Calculating the initial likelihood
bestL     = logL_init                       ; setting the best likelihood to be updated in MCMC loop
bestfit   = ab_init                         ; setting the best line parameters to be ubdated in MCMC loop
guessesALL = [bestfit,bestL]                ; defining first line of array with all MCMC values

if PL eq 1 then begin
   col=getcolor(/load)
   window, 0, xsize=600, ysize=500
   plot,DATARRAY(*,0),DATARRAY(*,1),psym=2
   oplot,findgen(300),ab_init(1)*(findgen(300))+ab_init(0),linestyle=2,col=col.blue
endif
if vb eq 1 then print,':: linearfitMCMC.pro :: The intial guess is         : y = ',strtrim(ab_init(1),2),' x + ',strtrim(ab_init(0),2)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; MCMC'ing through parameter space
guess_new = guess_init*0.0 ; vector for MCMC guesses
SIG       = [5.,!pi/100]   ; width of the gaussian distributions to draw new parameters from
guess_old = guess_init
logL_old  = logL_init
Naccept   = 0              ; resetting counter
for jj=0L,Nmcmc-1 do begin  ; start of MCMC loop
   ; drawing new paramters from specified Gaussians around current paramters
   guess_new[0] = guess_old[0] + RANDOMN(seed) * SIG[0]
   guess_new[1] = guess_old[1] + RANDOMN(seed) * SIG[1]

   logL_new = LNL(guess_new,DATARRAY,COVAR)  ; Calculating the initial likelihood

   ; write MCMC paramters to array
   guessesALL = [[guessesALL],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]]                     ; appending guess to array

   ; testing and selecting
   RR    = RANDOMU(seed)  ; random number between 0 and 1
   Lfrac = logL_new/logL_old
   if RR gt Lfrac then begin
      logL_old  = logL_new
      guess_old = guess_new
      Naccept = Naccept+1   ; counting the number of accepted gue guesses
   endif

   if logL_old lt bestL then begin
      bestL     = logL_new                                                     ; updating best L
      bestfit   = [guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]  ; updating best guess
   endif


   if PL eq 1 then oplot,findgen(300),tan(guess_new[1])*findgen(300)+guess_new[0]/cos(guess_new[1]),linestyle=0,col=col.green
   if PL eq 1 then oplot,DATARRAY(*,0),DATARRAY(*,1),psym=2

;wait,0.1
endfor                     ; end of MCMC loop
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if vb eq 1 then print,':: linearfitMCMC.pro :: ',strtrim(Naccept,2),' steps were accepted'
if vb eq 1 then print,':: linearfitMCMC.pro :: The best fit overall gives  : y = ',strtrim(bestfit(1),2),' x + ',strtrim(bestfit(0),2)
if vb eq 1 then print,'                        Corresponding to a K+lnL of ',strtrim(bestL,2)

if OUT ne 0 then begin
   if vb eq 1 then print,':: linearfitMCMC.pro :: Accounting for outliers not enabled. '
endif

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if vb eq 1 then print,':: linearfitMCMC.pro :: Fits output not enabled. '


if PL ne 0 then begin
   if vb eq 1 then print,':: linearfitMCMC.pro :: Plotting not enabled. '
   oplot,findgen(300),ab_init(1)*(findgen(300))+ab_init(0),linestyle=2,col=col.blue
   oplot,findgen(300),bestfit[1]*findgen(300)+bestfit[0],linestyle=0,col=col.red,thick=2
endif

openw,55,'guessesALL.txt'
printf,55,guessesALL
close,55
if PL eq 1 then begin
   window, 1, xsize=600, ysize=500
   plot,guessesALL(0,*),guessesALL(1,*),psym=3,xrange=[min(guessesALL(0,*)),max(guessesALL(0,*))],yrange=[min(guessesALL(1,*)),max(guessesALL(1,*))],/xstyle,/ystyle
   contourarray,guessesALL(0,*),min(guessesALL(0,*)),max(guessesALL(0,*)),guessesALL(1,*),min(guessesALL(1,*)),max(guessesALL(1,*)),40,40,4,contarr,levelbin,xrange,yrange
   contour,contarr,xrange,yrange,/overplot,levels=levelbin, C_COLOR=[col.green],thick=3  
endif

if vb eq 1 then print,' '
if vb eq 1 then print,':: linearfitMCMC.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END

FUNCTION LNL,param,DATARRAY,COVAR
   ; Function calculating the log likelihood given in equation 32 of Hogg et al. 2010
   ; expression for DELTA_i (eq 30 in Hogg et al. 2010)
   DELTA  = -sin(param[1])*DATARRAY(*,0)+cos(param[1])*DATARRAY(*,1)-param[0]  
   ; expression for SIGMA_i (eq 31 in Hogg et al. 2010)
   SIGMA2 = sin(param[1])^2.*DATARRAY(*,2)^2.+cos(param[1])^2.*DATARRAY(*,3)^2.-2*cos(param[1])*sin(param[1])*COVAR
   ; expression for lnL (eq 32 in Hogg et al. 2010)
   vecLNL = DELTA^2./(2*SIGMA2)  ; is in fact the chi2 value so should be minimized!
   RETURN, total(vecLNL)  ; calculating the total sum
END

