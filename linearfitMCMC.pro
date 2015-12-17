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
; The code can provide the MCMC propability distributions in a fits
; binary table if asked for, for plotting and further analysis.
;----------------------------
;   COMMENTS
;----------------------------
; NB!
; The input dataarray will be output as the changed/shifted dataarray
; when using the /SHIFT keyword. Thus if the dataarray is to be used
; after the linearfitMCMC command save it to another array before.
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
; RESULT          : Contains a vector with the 'best-fit' and error estimates
;                   The vector contains:
;                      RESULT(0)  = best fit a in expression y=a*x+b
;                      RESULT(1)  = median value of a
;                      RESULT(2)  = mean   value of a
;                      RESULT(3)  = minus 68% confidence interval of a PDF
;                      RESULT(4)  = plus 68% confidence interval of a PDF
;                      RESULT(5)  = minus 95% confidence interval of a PDF
;                      RESULT(6)  = plus 95% confidence interval of a PDF
;                      RESULT(7)  = best fit b in expression y=a*x+b
;                      RESULT(8)  = median value of b
;                      RESULT(9)  = mean   value of b
;                      RESULT(10) = minus 68% confidence interval of b PDF
;                      RESULT(11) = plus 68% confidence interval of b PDF
;                      RESULT(12) = minus 95% confidence interval of b PDF
;                      RESULT(13) = plus 95% confidence interval of b PDF
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Nmcmc           : The number of parameters to draw in the MCMC fit
;                   (default is only 1000+burn-in, i.e. for testing)
; /OUTLIERS       : set /OUTLIERS for pruning the outliers when fitting
; COVAR           : if covariances are present use this keyword to
;                   provide the individual covariances (default
;                   is assuming that all covariances are 0).
;                   The input vector shall be on the form:
;                   COVAR = fltaarr(N) where N number of data points
;                   and COVAR(i) = dxy_i   ; assuming dxy_i = dyx_i 
; /PLOT           : set /PLOT to plot the MCMC steps and probability distributions
;                   (only plotting on screen - use plotPDFs.pro for .eps creation
; /SHIFT          : shiting the data to its mean x and y values before fitting
; /INITGUESSALT   : Set this keyword to perform an alternative initial
;                   guess. Byt default a chi2 fit is made to the data. However, this
;                   might fail for small data sets far away from the coordinate origin
;                   (0,0). The alternative initial guess of a nd b is performed by
;                   shifting the data to (0,0), fixing b to 0 and using the a which
;                   minimizes chi2 here. 
; /CLIP           : Set this keyword to clip objects when calculating the
;                   initial guess (to make the initial guess better with outliers)
;                   When using the clipping function make sure that
;                   there are more than 8-10 data points
; /SIGMAS         : Vector contaning the witdths of the parameter
;                   proposal distributions to the draw MCMC steps from.
;                   Contains distribution widths for [bperp,theta] (see Hogg et al 2010)
;                   (for /OUTLIERS add Pb,Xb,Yb,Vbx,Vby and corr in this order)
; /SIGMAADJUST    : Setting this keyword will ignore the burn-in phase
;                   and adjust the SIGMAS, ie. the proposal distribution width (for a
;                   and b) after 100 MCMC loops until the number of accepted steps 
;                   are more than 15%. Using this feature is dangerous with too few 
;                   MCMCloops (Nmcmc) since the (right) minimum might not be
;                   reached after adjusting the SIGMAS.
; ERRCORRECT      : set ERRCORRECT=somearray which will be returned as an array similar
;                   to the RESULTS array but with the correct errors, i.e. errors estimated
;                   from the accepted steps instead of all steps (see bug section)
;                   NB! needs the PDF output from the output keyword
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
; output          : Use this keyword to name the output file (and path) if the
;                   PDFs are to be written to an ASCII file
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; Run with data from table one in Hogg et al. 2010
; IDL> w/o 4 first point... datarray=[[203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]]
; IDL> datarray=[[201,244,47,287,203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[592,401,583,402,495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]] &  linearfitMCMC,DATARRAY,RESULT,/PLOT,/VERBOSE,OUTPUT='test.dat',/OUTLIERS,/CLIP,/SHIFT, COVAR=[-0.84,0.31,0.64,-0.27,-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*sqrt([9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]),Nmcmc=1000

; IDL> linearfitMCMC,DATARRAY,RESULT,/PLOT,/VERBOSE,Nmcmc=10000,OUTPUT='test.dat',/OUTLIERS,/CLIP,/SIGMAADJUST,/INITGUESSALT,/SHIFT

; IDL> linearfitMCMC,DATARRAY,RESULT,/PLOT,/VERBOSE,OUTPUT='test.dat',/OUTLIERS,/CLIP,/SHIFT, COVAR=[-0.84,0.31,0.64,-0.27,-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*sqrt([9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]),Nmcmc=10000

; w/o 4 first points... COVAR=[-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]**sqrt([5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.])

; IDL> mag1  = RANDOMN(seed,20)*0.3+18          ; GAUSS*SIG+MEAN
; IDL> mag2  = RANDOMN(seed,20)*0.3+19          ; GAUSS*SIG+MEAN
; IDL> dmag1 = abs(RANDOMN(seed,20)*0.2+0.0)    ; GAUSS*SIG+MEAN
; IDL> dmag2 = abs(RANDOMN(seed,20)*0.3+0)      ; GAUSS*SIG+MEAN
; IDL> datarray=[[mag1],[mag2],[dmag1],[dmag2]]

; running with random gaussian distributions
; IDL> SEED=10 & NP=10 & mag1=RANDOMN(seed,NP)*0.3+18 & mag2=RANDOMN(seed,NP)*0.3+19 & dmag1=abs(RANDOMN(seed,NP)*0.2+0.0) & dmag2=abs(RANDOMN(seed,NP)*0.2+0) & datarray=[[mag1],[mag2],[dmag1],[dmag2]] & linearfitMCMC,DATARRAY,RESULT,/PLOT,/VERBOSE,Nmcmc=10000,OUTPUT='test.dat',/OUTLIERS,/CLIP,/SHIFT;,/INITGUESSALT,/SIGMAADJUST   

;----------------------------
;   BUGS
;----------------------------
; The output errors are estimated on all attempts instead of just the
; accepted MCMC steps, so the estimated 68 and 95% probability
; distributions are worng! The right errors can be onbtained via the
; small procedure correctMCMCerrorsInCat.pro; Alternatively one can
; use the ERRCORRECT keyword and get the right errors that way.
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-22  started by K. B. Schmidt (MPIA)
; 2010-12-06  added /INITGUESSALT keyword. K. B. Schmidt (MPIA)
; 2011-05-26  added /ERRCORRECT keyword. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ contourarray.pro
@ ellipse.pro
@ calcBadprob.pro
;----------------------------
;-
;--------------------------------------------------------------------------------------------------------------------
FUNCTION LNL,param,DATARRAY,COVAR
   ; Function calculating the log likelihood given in equation 32 of Hogg et al. 2010
   ; expression for DELTA_i (eq 30 in Hogg et al. 2010)
   DELTA  = -sin(param[1])*DATARRAY(*,0)+cos(param[1])*DATARRAY(*,1)-param[0]  
   ; expression for SIGMA_i (eq 31 in Hogg et al. 2010)
   SIGMA2 = sin(param[1])^2.*DATARRAY(*,2)^2.+cos(param[1])^2.*DATARRAY(*,3)^2.-2*cos(param[1])*sin(param[1])*COVAR
   ; expression for lnL (eq 32 in Hogg et al. 2010)
   vecLNL = DELTA^2./(2*SIGMA2)  ; is in principle the chi2 value so should be minimized!
   RETURN, total(vecLNL)  ; calculating the total sum
END
;--------------------------------------------------------------------------------------------------------------------
FUNCTION LNL_PO,param,DATARRAY,COVAR  ; LNL for pruning outliers
   x        = DATARRAY(*,0) ; x component uncertainties
   y        = DATARRAY(*,1) ; y component uncertainties
   dx       = DATARRAY(*,2) ; x component uncertainties
   dy       = DATARRAY(*,3) ; y component uncertainties
   dxy      = COVAR         ; covariances of data (dxy=dyx)
   bperp    = param[0]      ; perpendicular distance between origin and line
   theta    = param[1]      ; angle between line and x-axis
   Pb       = param[2]      ; probability of point beeing bad
   Xb       = param[3]      ; mean of bad points in x direction
   Yb       = param[4]      ; mean of bad points in y direction
   Vbx      = exp(param[5])      ; variance of bad points in x direction
   Vby      = exp(param[6])      ; variance of bad points in y direction
   corr     = param[7]      ; correlation factor giving the magnitude of the covariances of the bad points
   vec      = [-sin(theta),cos(theta)]
   ZZb      = [[Xb],[Yb]]
   Varr     = [[Vbx,sqrt(Vbx*Vby)*corr],[sqrt(Vbx*Vby)*corr,Vby]]
   KlnL     = fltarr(n_elements(x))

   if Pb lt 0. or Pb gt 1. then begin
      RETURN, 1.0e32 ; if probability is unphysical break out and return huge chi^2
   endif

   if corr lt -1. or corr gt 1. then begin
      RETURN, 1.0e32 ; if the covariance correlation factor is too large/small break out and return huge chi^2
   endif

   for ii=0,n_elements(x)-1 do begin ; looping over data pairs
      ZZ       = [[x(ii)],[y(ii)]]  
      SS       = [[dx(ii)^2.,dxy(ii)],[dxy(ii),dy(ii)^2.]]
      SSout    = Varr+SS
      DELTA    = double(transpose(vec)#transpose(ZZ))-bperp

      SIGMA2   = double(SS#(vec))
      SIGMA2   = double(transpose(vec)#SIGMA2)

      DELTAOUT = double(transpose(ZZ-ZZb))
      DELTAOUT = double(invert(SSout)#DELTAOUT)
      DELTAOUT = double((ZZ-ZZb)#DELTAOUT)

      term1    = (1.-Pb)/sqrt(2.*!pi*SIGMA2)     *exp(-0.5*DELTA^2./SIGMA2)
      term2    = Pb/(2.*!pi*sqrt(determ(SSout))) *exp(-0.5*DELTAOUT)
      KlnL(ii) = -1*alog(term1+term2) ; K-lnL     i.e. in principle chi2 - to be minimized
   endfor

   ;if total(KlnL) eq 'infinity' then stop

   RETURN, total(KlnL)  ; calculating the total sum
END
;--------------------------------------------------------------------------------------------------------------------

PRO linearfitMCMC,DATARRAY,RESULT,Nmcmc=Nmcmc,OUTLIERS=OUTLIERS,PLOT=PLOT,EPS=EPS,VERBOSE=VERBOSE,COVAR=COVAR,OUTPUT=OUTPUT,SIGMAS=SIGMAS,CLIP=CLIP,SIGMAADJUST=SIGMAADJUST,INITGUESSALT=INITGUESSALT,SHIFT=SHIFT,ERRCORRECT=ERRCORRECT
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

WD  = n_elements(SIGMAS)
SAD = n_elements(SIGMAADJUST)
OUT = n_elements(OUTPUT)
IG  = n_elements(INITGUESSALT)
SH  = n_elements(SHIFT)
PO  = n_elements(OUTLIERS)
NM  = n_elements(Nmcmc)
CV  = n_elements(COVAR)
PL  = n_elements(PLOT)
VB  = n_elements(VERBOSE)
CL  = n_elements(CLIP)
EC  = n_elements(ERRCORRECT)

Ndat0 = n_elements(DATARRAY(*,0))        ; number of data points
if CV eq 0 then COVAR = fltarr(Ndat0)    ; setting covariances to 0 if not given
if NM eq 0 then Nmcmc = 1000            ; setting number of iterations if not given

if IG ne 0 or SH eq 1 then begin   ; Shifting data to (0,0)
   Mxcomp          = mean(DATARRAY(*,0))  ; mean value of x components              ---------- use median??
   Mycomp          = mean(DATARRAY(*,1))  ; mean value of x components              ---------- use median??
   ;shifting with other offset as test:
   ;Mxcomp          = 12 
   ;Mycomp          = 12
   DATARRAY(*,0)   = DATARRAY(*,0) - Mxcomp
   DATARRAY(*,1)   = DATARRAY(*,1) - Mycomp
endif


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if IG eq 0 then begin   ; making the default initial guess, i.e. chi2 fit from chapter 1 in Hogg et al 2010
   Xinit   = DATARRAY(*,0)
   Yinit   = DATARRAY(*,1)
   ;Xinit0   = DATARRAY(*,0)
   ;Yinit0   = DATARRAY(*,1)

   if CL eq 1 then begin                                   ; clipping for initial guess
      Nclip   = ROUND(0.1*Ndat0)                            ; number of data points to clip
      if Nclip eq 0 then Nclip=1                           ; make sure to remove at least one point
      XsortE  = sort(Xinit)                                ; sorted entries of x-components
      Xinit(XsortE(0:Nclip-1)) = -555                      ; marking the 10% smallest x-values
      Xinit(XsortE(Ndat0-Nclip:Ndat0-1)) = -555              ; marking the 10% largest x-values

      YsortE  = sort(Yinit)                                ; sorted entries of y-components
      Yinit(YsortE(0:Nclip-1)) = -555                      ; marking the 10% smallest x-values
      Yinit(YsortE(Ndat0-Nclip:Ndat0-1)) = -555              ; marking the 10% largest y-values

      clipent = where(Xinit ne -555 and Yinit ne -555,Nleft); entries left after clipping
      Xinit   = Xinit(clipent)                             ; x-components after clipping for initial guess
      Yinit   = Yinit(clipent)                             ; y-components after clipping for initial guess

      Ndat    = Nleft                                      ; array sizes after clipping

      onevec = [fltarr(Ndat)+1]                            ; vector with 0s
      Ainit  = [[onevec],[Xinit]]                          ; defining A array
      Cinit  = fltarr(Ndat,Ndat)                           ; defining covariance matrix
      for ii=0L,Ndat-1 do begin
         Cinit(ii,ii) = DATARRAY(clipent(ii),3)*DATARRAY(clipent(ii),3); filling covariance matrix with y errors
      endfor
   endif else begin
      onevec = [fltarr(Ndat0)+1]                            ; vector with 0s
      Ainit  = [[onevec],[Xinit]]                          ; defining A array
      Cinit  = fltarr(Ndat0,Ndat0)                           ; defining covariance matrix
      for ii=0L,Ndat0-1 do begin
         Cinit(ii,ii) = DATARRAY(ii,3)*DATARRAY(ii,3)      ; filling covariance matrix with y errors
      endfor
   endelse
   ;if Ndat eq 8 then stop

   ; Calculating initial guess
   ab1      = double( invert(Cinit)#Yinit    )   ; IDL screws up if each matrix multiplication is not defined as double
   ab1      = double( transpose(Ainit)#ab1   )
   ab2      = double( invert(Cinit)#Ainit    )
   ab2      = double( transpose(Ainit)#ab2   )
   ab2      = double( invert(ab2)            )
   ab_init  = double( ab2#ab1                )
   ;ab_init = invert( double(transpose(Ainit))#invert(Cinit)#Ainit)) # ( transpose(Ainit)#invert(Cinit)#Yinit)
   ;ab_init = [100,1.1]
   ;ab_init = [ -0.11342760  ,   0.22449342  ]   ;-0.11342760   0.82449342
   theta_init  = atan(ab_init(1))                       ; angle between line and x-axis
   b_perp_init = ab_init(0)*cos(theta_init)             ; perpendicular distance between origin (0,0) and line

   ; Calculating chi2 of initial guess
   chi21    = double( Ainit#ab_init          )
   chi21    = double( transpose(Yinit)-chi21 )
   chi22    = double( Ainit#ab_init          )
   chi22    = double( transpose(transpose(Yinit)-chi22) )
   chi22    = double( invert(Cinit)#chi22    )
   chi2     = double( chi21#chi22            )
   ;chi2     = double(transpose(Yinit)-Ainit#ab_init) # double(invert(Cinit)) # double(transpose(transpose(Yinit)-Ainit#ab_init))
   ;chi2     = total((Yinit-ab_init(1)*Xinit-ab_init(0))^2./datarray(clipent,3)^2)

   if vb eq 1 then print,':: linearfitMCMC.pro :: The initial guess is        : y    = ',strtrim(ab_init(1),2),' x + ',strtrim(ab_init(0),2)
   if vb eq 1 then print,':: linearfitMCMC.pro :: with a chi2 value of        : chi2 = ',strtrim(chi2,2)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
endif else begin  ; meking alternative initial guess by moving object to (0,0), fixing b and runnning through all angles
   Xinit           = DATARRAY(*,0)
   Yinit           = DATARRAY(*,1)

   if CL eq 1 then begin                                    ; clipping for initial guess
      Nclip   = ROUND(0.1*Ndat0)                            ; number of data points to clip
      if Nclip eq 0 then Nclip=1                            ; make sure to remove at least one point
      XsortE  = sort(Xinit)                                 ; sorted entries of x-components
      Xinit(XsortE(0:Nclip-1)) = -555                       ; marking the 10% smallest x-values
      Xinit(XsortE(Ndat0-Nclip:Ndat0-1)) = -555             ; marking the 10% largest x-values
      YsortE  = sort(Yinit)                                 ; sorted entries of y-components
      Yinit(YsortE(0:Nclip-1)) = -555                       ; marking the 10% smallest x-values
      Yinit(YsortE(Ndat0-Nclip:Ndat0-1)) = -555             ; marking the 10% largest y-values
      clipent = where(Xinit ne -555 and Yinit ne -555,Nleft); entries left after clipping
      Xinit   = Xinit(clipent)                              ; x-components after clipping for initial guess
      Yinit   = Yinit(clipent)                              ; y-components after clipping for initial guess
      Ndat    = Nleft                                       ; array sizes after clipping
   endif

   Ntheta          = 360.                 ; number of thetas to check
   dTheta          = !pi/Ntheta
   guessCHI2 = fltarr(Ntheta,2)           ; array to contain the angles and chi2 values for guesses

   ; chi2 of the initial 
   for ll=0,Ntheta-1 do begin
      a_guess  = tan(ll*dTheta)
      if CL eq 1 then chi2     = total((Yinit-a_guess*Xinit)^2./datarray(clipent,3)^2) ; chi2 with slope atan(ll*dTheta) and b=2 (fixed)
      if CL eq 0 then chi2     = total((Yinit-a_guess*Xinit)^2./datarray(*,3)^2) ; chi2 with slope atan(ll*dTheta) and b=2 (fixed)
      guessCHI2(ll,0) = chi2              ; storing chi2
      guessCHI2(ll,1) = a_guess           ; storing slope
      ;oplot,xinit,a_guess*Xinit,linestyle=0 & wait,0.01
   endfor

   bestent = where(guessCHI2(*,0) eq min(guessCHI2(*,0)))
   a_init  = guessCHI2(bestent,1) ; taking the entry with smallest chi2 as initial slope
   b_init      = 0                ; the corresponding initial intersection with y-axis
   ab_init     = [b_init,a_init]
   theta_init  = atan(a_init)
   b_perp_init = ab_init(0)*theta_init

   if vb eq 1 then print,':: linearfitMCMC.pro :: The initial guess is        : y    = ',strtrim(ab_init(1),2),' x + ',strtrim(ab_init(0),2)
   if vb eq 1 then print,':: linearfitMCMC.pro :: with a chi2 value of        : chi2 = ',strtrim(chi2,2)

   Npfit   = 40
   fitent  = findgen(Npfit)-(Npfit/2.)+bestent(0)
   xfitval = atan(guessCHI2(*,1))
   yfitval = (guessCHI2(*,0)-max(guessCHI2(fitent,0)))*(-1.)

   Gfit = GAUSSFIT(xfitval(fitent),yfitval(fitent),coeff,NTERMS=3) 
   ;plot,xfitval(fitent),yfitval(fitent),psym=2
   ;oplot,xfitval(fitent),coeff(0)*exp(-0.5*((xfitval(fitent)-coeff(1))/coeff(2))^2.),linestyle=0,thick=3
endelse  ; end of initial guess
;stop
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; creating initial guess arrays.
if PO eq 0 then begin
   guess_init   = [b_perp_init,theta_init]             ; the inital guess of parameters to MCMC over [theta,b_perp]
   logL_init    = LNL(guess_init,DATARRAY,COVAR)       ; Calculating the initial likelihood
   bestfit_init = [ab_init,logL_init]                  ; setting the best fit parameters to be ubdated in MCMC loop
endif else begin
   MOMx         = MOMENT(DATARRAY(*,0))                ; calculating the moments of the x-vector: 0=mean,1=variance etc. 
   MOMy         = MOMENT(DATARRAY(*,1))                ; calculating the moments of the y-vector: 0=mean,1=variance etc.
   guess_init   = [b_perp_init,theta_init,0.0,MOMx[0],MOMy[0],alog(MOMx[1]),alog(MOMy[1]),0.0]
   logL_init    = LNL_PO(guess_init,DATARRAY,COVAR) 
   bestfit_init = [ab_init,logL_init,guess_init[2:7]]  ; setting the best fit parameters to be ubdated in MCMC loop
endelse

; MCMC'ing through parameter space
guess_new = fltarr(2+PO*6) ; vector for MCMC guesses
if WD eq 0 then begin ; using default gaussian widths
   if PO eq 0 then SIGMAS       = [5.,!pi/100]                              ; width of gaussian to draw new parameters from
   if PO eq 1 then SIGMAS       = [1.,!pi/200.,0.01,0.5,1.0,0.05,0.1,0.005] ; width of gaussian to draw new parameters from
   SIG = SIGMAS
endif else begin
   SIG = SIGMAS
   if n_elements(SIGMAS) ne 2+PO*6 then begin
      print,':: linearfitMCMC.pro :: - ERROR - Too few SIGMAS given. ',strtrim(2+PO*6,2),' expected --> Aborting'
      stop
   endif
endelse

if IG ne 0 then SIG[1] = coeff(2)  ;  !pi/4.   ; rough estimate of the 1sigma width of the chi2 valley from initial guess 


Nburnin = 1000                  ; Number of burn-in loops to perform
if SAD eq 1 then Nburnin = 0    ; no burn in if sigma adjustment selected
Nadjust = 0                     ; resetting SIGMA adjustment counter in case SIGMAADJUST is set

MCMCstart:

if PL eq 1 then begin
   !p.multi = [0,0,0]
   col=getcolor(/load)
   window, 0, xsize=600, ysize=500
   DDATX = max(DATARRAY(*,0))-min(DATARRAY(*,0))
   DDATY = max(DATARRAY(*,1))-min(DATARRAY(*,1))
   plot,DATARRAY(*,0),DATARRAY(*,1),psym=2, $ 
   Xrange=[min(DATARRAY(*,0))-0.1*DDATX,max(DATARRAY(*,0))+0.1*DDATX],/xstyle, $
   Yrange=[min(DATARRAY(*,1))-0.1*DDATY,max(DATARRAY(*,1))+0.1*DDATY],/ystyle
   xvalues = findgen(100)/100*1000-500. 
   oplot,xvalues,ab_init(1)*xvalues+ab_init(0),linestyle=2,col=col.blue
endif

bestfit       = bestfit_init    ; initializing bestfit value
guess_old     = guess_init      ; initialising guesses
logL_old      = logL_init       ; initialising guesses
guessesALL    = bestfit         ; defining first line of array with all MCMC values
guessesACCEPT = bestfit         ; defining first line of array with all MCMC values
Naccept       = 0               ; resetting counter
if vb eq 1 then print,':: linearfitMCMC.pro :: Proposal distribution widths before MCMC looping: ',strtrim(SIG,2)


for jj=0L,Nmcmc+Nburnin-1 do begin   ; start of MCMC loop
   if PO eq 0 then begin
      ; drawing new paramters from specified Gaussians around current paramters
      guess_new[0] = guess_old[0] + RANDOMN(seed) * SIG[0] ;  bperp : perpendicular distance between origin and line
      guess_new[1] = guess_old[1] + RANDOMN(seed) * SIG[1] ;  theta : angle between line and x-axis
      logL_new     = LNL(guess_new,DATARRAY,COVAR)         ; Calculating the likelihood
      b_new        = guess_new[0]/cos(guess_new[1])
      guessesALL   = [[guessesALL],[b_new,tan(guess_new[1]),logL_new]] ; appending guess to array
   endif else begin
      ; drawing new paramters from specified Gaussians around current paramters
      guess_new[0] = guess_old[0] + RANDOMN(seed) * SIG[0] ;  bperp : perpendicular distance between origin and line
      guess_new[1] = guess_old[1] + RANDOMN(seed) * SIG[1] ;  theta : angle between line and x-axis
      guess_new[2] = guess_old[2] + RANDOMN(seed) * SIG[2] ;  Pb    : probability of point beeing bad
      guess_new[3] = guess_old[3] + RANDOMN(seed) * SIG[3] ;  Xb    : mean of bad points in x direction
      guess_new[4] = guess_old[4] + RANDOMN(seed) * SIG[4] ;  Yb    : mean of bad points in y direction
      guess_new[5] = guess_old[5] + RANDOMN(seed) * SIG[5] ;  Vxb   : variance of bad points in x direction
      guess_new[6] = guess_old[6] + RANDOMN(seed) * SIG[6] ;  Vyb   : variance of bad points in y direction
      guess_new[7] = guess_old[7] + RANDOMN(seed) * SIG[7] ;  corr  : correlation factor giving covariances of bad points
      logL_new     = LNL_PO(guess_new,DATARRAY,COVAR)        ; Calculating the likelihood
      b_new        = guess_new[0]/cos(guess_new[1])
      guessesALL   = [[guessesALL],[b_new,tan(guess_new[1]),logL_new,guess_new[2:7]]] ; appending guess to array
   endelse
;print,logL_old,logL_new
   ; testing and selecting
   DlogL = logL_old - logL_new   
   if DlogL gt 0 then begin     ; if the new logL (corresponds to chi2) is smaller than the old then select it
      ;print,logL_old,logL_new  
      logL_old  = logL_new
      guess_old = guess_new
      Naccept   = Naccept+1   ; counting the number of accepted guesses
      ;print, [guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new,guess_new[2:7]]
      if logL_new lt bestfit(2) then begin  ; checking if the new lnL is smaller than the overall best lnL 
         if PO eq 0 then bestfit   = [guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]                   ; updating best guess
         if PO eq 1 then bestfit   = [guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new,guess_new[2:7]]    ; updating best guess
      endif
      if PO eq 0 then guessesACCEPT = [[guessesACCEPT],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]]                ; appending accepted guess
      if PO eq 1 then guessesACCEPT = [[guessesACCEPT],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new,guess_new[2:7]]] ; appending accepted guess
   endif else begin
      RR    = RANDOMU(seed)  ; random number between 0 and 1
      if alog(RR) - DlogL lt 0 then begin ; keep the values eventhough no imporvement of dlogL was obtained
      ;if RR lt logL_old/logL_new then begin ; keep the values eventhough no imporvement of dlogL was obtained
         logL_old  = logL_new
         guess_old = guess_new
         Naccept   = Naccept+1   ; counting the number of accepted guesses
         ;print, [guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new,guess_new[2:7]]                 
         if PO eq 0 then guessesACCEPT = [[guessesACCEPT],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new]]                ; appending accepted guess
         if PO eq 1 then guessesACCEPT = [[guessesACCEPT],[guess_new[0]/cos(guess_new[1]),tan(guess_new[1]),logL_new,guess_new[2:7]]] ; appending accepted guess 
      endif
   endelse

   NloopsSAD = jj ; -Nburn  ; Loop number to check for sigma adjustment
   if SAD eq 0 and jj eq Nburnin then begin  ; shrinking proposal widths when burn-in phase is over
      SIG[0] = 0.5*SIGMAS[0]
      SIG[1] = 0.5*SIGMAS[1]
      if vb eq 1 then print,':: linearfitMCMC.pro :: Burn-in phase over; initial proposal distribution widths of a and b halved'
   endif

   ; adjusting to sigmas (of a and b) if too few steps were accepted
   if SAD eq 1 and NloopsSAD eq 100 and Naccept le ROUND(NloopsSAD*0.15) and Nadjust ne 555 then begin
      SIG[0] = SIG[0]*0.66  ; taking two thirds of the old SIGMA
      SIG[1] = SIG[1]*0.66  ; taking two thirds of the old SIGMA as NEW SIGMA
      if vb eq 1 then print,':: linearfitMCMC.pro :: Only ',strtrim(Naccept,2),' steps out of ',strtrim(NloopsSAD,2),' accepted; adjusting the given a and b SIGMAS'
      if vb eq 1 then print,':: linearfitMCMC.pro :: the new SIGMAs are: ',SIG
      Nadjust = Nadjust + 1
      NadjustMAX = 50
      if Nadjust lt NadjustMAX then begin
         goto,MCMCstart
      endif else begin
         if vb eq 1 then print,':: linearfitMCMC.pro :: ',strtrim(NadjustMAX,2),' adjustments of the SIGMAS did not improve the fraction of acceptances'
         if vb eq 1 then print,'                        enough, hence the MCMC will be run with the original (default) input SIGMAS! '
         SIG[0] = SIGMAS[0]
         SIG[1] = SIGMAS[1]
         Nadjust = 555       ; setting counter to 555 so the adjustment will be ignored
         goto,MCMCstart
      endelse
   endif

   if PL eq 1 then begin
      Nlines = 100.   ; number of lines to draw
      NN = round(Nmcmc/Nlines)+0.0
      if jj/NN eq ceil(jj/NN) then begin  ; only plotting every NN'th guess as green line
         oplot,xvalues,tan(guess_new[1])*xvalues+guess_new[0]/cos(guess_new[1]),linestyle=0,col=col.green
         oplot,DATARRAY(*,0),DATARRAY(*,1),psym=2
      endif
   endif
;wait,0.1
;print,jj
endfor                     ; end of MCMC loop

if PL eq 1 and PO eq 1 then begin
   calcBadprob,[bestfit(0)*cos(atan(bestfit(1))),atan(bestfit(1)),bestfit(3:8)],DATARRAY,Pbad,/VERBOSE,COVAR=COVAR  ; calculate probability that the point is an outlier
   entbad  = where(Pbad gt 0.3)
   if entbad ne [-1] then oplot,DATARRAY(entbad,0),DATARRAY(entbad,1),psym=2,col=col.red
endif 

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if vb eq 1 then print,':: linearfitMCMC.pro :: ',strtrim(Naccept,2),' steps were accepted'
if vb eq 1 then print,':: linearfitMCMC.pro :: The best fit overall gives  : y = ',strtrim(bestfit(1),2),' x + ',strtrim(bestfit(0),2)
if vb eq 1 then print,'                        Corresponding to a K+lnL of ',strtrim(bestfit(2),2)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if OUT ne 0 then begin
   ; writing PDFs to ascii file
   openw,55,OUTPUT,width=500
   printf,55,'# This file contains the probability distribution functions (PDFs) of the linear fit'
   printf,55,'# done with linearfitMCMC.pro. It was created on '+systime( )
   printf,55,                 '# The proposal distribution widths (PDW) of the parameters were : b,m,Pb,Xb,Yb,Vbx,Vby,corr = ',SIG
   if SAD eq 1 then printf,55,'# Adjusted from the initial input PDW                           : b,m,Pb,Xb,Yb,Vbx,Vby,corr = ',SIGMAS
   if SAD eq 0 then printf,55,'# Halfed from the initial input PDW (only b and m parameters) after burn-in phase of ',strtrim(Nburnin,2),' MCMC loops'
   printf,55,'# The columns contain (see Hogg, Bovy and Lang 2010 for further information):'
   if PO gt 0 then printf,55,'# b   m   KlnL   Pb   Xb   Yb   Vbx   Vby   corr'
   if PO eq 0 then printf,55,'# b   m   KlnL'
   printf,55,guessesACCEPT
   close,55

   ; writing PDFs to ascii file
   outname = strsplit(OUTPUT,'.',/extract)
   if n_elements(outname) gt 2 then print,':: linearfitMCMC.pro :: Only one dot (.) in the output name is allowed'
   openw,65,+outname(0)+'_ALLsteps'+'.'+outname(1),width=500
   printf,65,'# This file contains all the MCMC steps taken when performing the the linear fit'
   printf,65,'# with linearfitMCMC.pro. It was created on '+systime( )
   printf,65,'# The columns contain (see Hogg, Bovy and Lang 2010 for further information):'
   if PO gt 0 then printf,65,'# b   m   KlnL   Pb   Xb   Yb   Vbx   Vby   corr'
   if PO eq 0 then printf,65,'# b   m   KlnL'
   printf,65,guessesALL
   close,65
endif
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if PL ne 0 then begin
   oplot,xvalues,ab_init(1)*xvalues+ab_init(0),linestyle=2,col=col.blue
   oplot,xvalues,bestfit[1]*xvalues+bestfit[0],linestyle=0,col=col.red,thick=2

   for kk=0,Ndat0-1 do begin ; drawing error ellipse around points
      cent  = [DATARRAY(kk,0),DATARRAY(kk,1)]
      arr   = [[DATARRAY(kk,2),COVAR(kk)],[COVAR(kk),DATARRAY(kk,3)]]   ; covariance matrix for kk'th object
      eval  = EIGENQL(arr,EIGENVECTORS=evec)  ; calculating the eigenvalues and eigenvectors of covariance matrix
      rad   = [DATARRAY(kk,2),DATARRAY(kk,3)]
;      rad   = [2*sqrt(eval(0)),2*sqrt(eval(1))]
      arot  = atan(evec(0,0)/evec(0,1))  ; rotation angle
      ell   = ELLIPSE(cent,rad,arot=arot)
      oplot,ell(0,*),ell(1,*),linestyle=0
   endfor
endif


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;XXXXXXXXXXXXXXXXXXX ERRORS WRONG - SHOULD BE DONE ON guessesACCEPT instead of guessesALL XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
RESULT     = fltarr(14)
; entries for plus and minus confidence intervals
ent68m     = round(Nmcmc*0.16)
ent68p     = round(Nmcmc*0.84)
ent95m     = round(Nmcmc*0.025)
ent95p     = round(Nmcmc*0.975)

RESULT(0)  = bestfit[1]                           ; the value with lowest LnL
RESULT(1)  = median(guessesALL(1,*))              ; the median
RESULT(2)  = mean(guessesALL(1,*))                ; the mean
asort      = guessesALL(1,sort(guessesALL(1,*)))  ; sorting a values
RESULT(3)  = RESULT(1)-asort(ent68m)
RESULT(4)  = asort(ent68p)-RESULT(1)
RESULT(5)  = RESULT(1)-asort(ent95m)
RESULT(6)  = asort(ent95p)-RESULT(1)

RESULT(7)  = bestfit[0]                           ; the value with lowest LnL
RESULT(8)  = median(guessesALL(0,*))              ; the median
RESULT(9)  = mean(guessesALL(0,*))                ; the mean
bsort      = guessesALL(0,sort(guessesALL(0,*)))  ; sorting b values
RESULT(10) = RESULT(8)-bsort(ent68m)
RESULT(11) = bsort(ent68p)-RESULT(8)
RESULT(12) = RESULT(8)-bsort(ent95m)
RESULT(13) = bsort(ent95p)-RESULT(8)
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if PL eq 1 then begin
   window, 2, xsize=600, ysize=500
   XR = [min(guessesALL(0,*)),max(guessesALL(0,*))]
   YR = [min(guessesALL(1,*)),max(guessesALL(1,*))]
   DX = max(guessesALL(0,*))-min(guessesALL(0,*))
   DY = max(guessesALL(1,*))-min(guessesALL(1,*))
   plot,guessesALL(0,*),guessesALL(1,*),psym=3,xrange=XR,/xstyle,yrange=YR,/ystyle,xtitle='b',ytitle='a'
   contourarray,guessesALL(0,*),min(guessesALL(0,*)),max(guessesALL(0,*)),guessesALL(1,*),min(guessesALL(1,*)),max(guessesALL(1,*)),40,40,4,contarr,levelbin,xrange,yrange
   contour,contarr,xrange,yrange,/overplot,levels=levelbin, C_COLOR=[col.green],thick=3  

   xx = fltarr(2)
   PLOTSYM,0,1.5,/FILL
   oplot  ,xx+ab_init(0),xx+ab_init(1),psym=8,col=col.blue    ; overplotting initial guess
   oplot  ,xx+RESULT(7),xx+RESULT(0),psym=8,col=col.red       ; overplotting best fit
   oplot  ,xx+RESULT(9),xx+RESULT(2),psym=8,col=col.orange    ; overplotting mean fit
   oplot  ,xx+RESULT(8),xx+RESULT(1),psym=8,col=col.magenta   ; overplotting median fit
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(10),xx+RESULT(3),/LOBAR,errcol=col.magenta,errthick=2
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(11),xx+RESULT(4),/HIBAR,errcol=col.magenta,errthick=2
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(12),xx+RESULT(5),/LOBAR,errcol=col.magenta,errthick=2,errstyle=2
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(13),xx+RESULT(6),/HIBAR,errcol=col.magenta,errthick=2,errstyle=2

   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.95,"PDF median fit with 68% and 95% confidence intervals",col=col.magenta,charsize=1.5,charthick=2
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.90,"PDF mean fit",col=col.orange,charsize=1.5,charthick=2
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.85,"Best fit",col=col.red,charsize=1.5,charthick=2
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.80,"Initial guess",col=col.blue,charsize=1.5,charthick=2
endif
 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if PL eq 1 then begin
   window, 3, xsize=600, ysize=500
   thetavalues = atan(guessesALL(1,*))
   XR = [min(guessesALL(0,*)),max(guessesALL(0,*))]
   YR = [min(thetavalues),max(thetavalues)]
;   plot,guessesALL(0,*),thetavalues,psym=3,xrange=XR,/xstyle,yrange=YR,/ystyle,xtitle='b',ytitle='theta'
   plothist,thetavalues,xhistval,yhistval,bin=0.1,xtitle='theta',ytitle='#'
endif


if IG ne 0 then begin  ; moving data back to input position
   DATARRAY(*,0)   = DATARRAY(*,0) + Mxcomp
   DATARRAY(*,1)   = DATARRAY(*,1) + Mycomp
endif


; - - - - - - - - - - - - - - - - - - - - CORRECTING ERRORS IF REQUESTED - - - - - - - - - - - - - - - - - - - - - - 
if EC ne 0 then begin
   PDFfile = output

   ; --- Reading repick ASCII file ---
   Nrows        = File_lines(PDFfile)               ; number of rows in file
   Nheaderlines = 6                                 ; number of header lines
   Ncol         = 9                                 ; number of columns in file
   PDFarr       = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
   openr,lun,PDFfile, /GET_LUN                      ; open file for reading     
   header = STRARR(Nheaderlines)                    ; string array for header
   readf,lun,header                                 ; reading header into string array
   readf,lun,PDFarr                                 ; reading data into array
   free_lun,lun  
   ; ----------------------------------

   Nlines = Nrows-Nheaderlines
   bb     = PDFarr(0,*)
   mm     = PDFarr(1,*) 
   KlnL   = PDFarr(2,*)

   ent     = where(KlnL eq min(KlnL))
   bestent = ent(0)  ; entry of fit with lowest 'chi^2'

   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ; calculating mean and median values plus confidence intervals
   RESULTec     = fltarr(14)
   ; entries for plus and minus confidence intervals
   ent68m     = round(Nlines*0.16)
   ent68p     = round(Nlines*0.84)
   ent95m     = round(Nlines*0.025)
   ent95p     = round(Nlines*0.975)

   if ent95p eq Nlines then ent95p = Nlines-1

   RESULTec(0)  = mm(bestent)                          ; the value with lowest LnL
   RESULTec(1)  = median(mm)                           ; the median
   RESULTec(2)  = mean(mm)                             ; the mean
   msort      = mm(sort(mm))                         ; sorting a values
   RESULTec(3)  = RESULTec(1)-msort(ent68m)
   RESULTec(4)  = msort(ent68p)-RESULTec(1)
   RESULTec(5)  = RESULT(1)-msort(ent95m)
   RESULTec(6)  = msort(ent95p)-RESULTec(1)

   RESULTec(7)  = bb(bestent)                          ; the value with lowest LnL
   RESULTec(8)  = median(bb)                           ; the median
   RESULTec(9)  = mean(bb)                             ; the mean
   bsort      = bb(sort(bb))                         ; sorting b values
   RESULTec(10) = RESULTec(8)-bsort(ent68m)
   RESULTec(11) = bsort(ent68p)-RESULTec(8)
   RESULTec(12) = RESULTec(8)-bsort(ent95m)
   RESULTec(13) = bsort(ent95p)-RESULTec(8)
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ERRCORRECT = RESULTec
endif


if vb eq 1 then print,' '
if vb eq 1 then print,':: linearfitMCMC.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END


