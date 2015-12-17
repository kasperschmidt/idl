;+
;----------------------------
;   NAME
;----------------------------
; calcBadprob.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure calculating the probability that a given points in a data
; set using the output PDFs from running linearfitMCMC.pro with the
; /outliers keyword set.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; param           : Vector with the parameters of the MCMC. The vector should contain:
;                      param[0] = bperp : perpendicular distance between origin and line
;                      param[1] = theta : angle between line and x-axis
;                      param[2] = Pb    : probability of point beeing bad
;                      param[3] = Xb    : mean of bad points in x direction
;                      param[4] = Yb    : mean of bad points in y direction
;                      param[5] = Vxb   : variance of bad points in x direction
;                      param[6] = Vyb   : variance of bad points in y direction
;                      param[7] = corr  : correlation factor giving covariances of bad points
;                   These parameters are stored in the PDF file returned by linearfitMCMC.pro
;                   NB! the m column in the PDF file is       m = tan(theta)
;                   NB! the b column in the PDF file is       b = bperp/cos(atan(m) 
; DATARRAY        : array containing data points and there errors to
;                   which the line has to be fitted. The array is on
;                   the form:
;                         DATARRAY(N,0) = x-components
;                         DATARRAY(N,1) = y-components
;                         DATARRAY(N,2) = x-component uncertainties
;                         DATARRAY(N,3) = y-component uncertainties
;                   where N is the number of data points
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; COVAR           : if covariances are present use this keyword to
;                   provide the individual covariances (default
;                   is assuming that all covariances are 0).
;                   The input vector shall be on the form:
;                   COVAR = fltaarr(N) where N number of data points
;                   and COVAR(i) = dxy_i   ; assuming dxy_i = dyx_i 
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; Pbad            : Vector containing a probability for each point beeing bad
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> calcBadprob,PARAM,DATARRAY,PbadObj0,/VERBOSE;,COVAR=Fltarr(Nobj)
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-12-16  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO calcBadprob,param,DATARRAY,Pbad,COVAR=COVAR,VERBOSE=VERBOSE

CV  = n_elements(COVAR)
VB = n_elements(VERBOSE)

Ndat0 = n_elements(DATARRAY(*,0))        ; number of data points
if CV eq 0 then COVAR = fltarr(Ndat0)    ; setting covariances to 0 if not given

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
Vbx      = exp(param[5]) ; variance of bad points in x direction
Vby      = exp(param[6]) ; variance of bad points in y direction
corr     = param[7]      ; correlation factor giving the magnitude of the covariances of the bad points
vec      = [-sin(theta),cos(theta)]
ZZb      = [[Xb],[Yb]]
Varr     = [[Vbx,sqrt(Vbx*Vby)*corr],[sqrt(Vbx*Vby)*corr,Vby]]

Pbad     = fltarr(n_elements(x))
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

   Pbad0    = Pb/2./!pi/sqrt(determ(SSout))*exp(-0.5*deltaOUT)
   Pbad(ii) = Pbad0/(Pbad0+(1.-Pb)/sqrt(2.*!pi*SIGMA2)*exp(-0.5*DELTA^2./SIGMA2))  ; filling vector with propability of point beeing bad
;print,'point '+strtrim(ii,1),Pbad(ii),Pb,determ(SSout),DELTA,    deltaOUT,SIGMA2,exp(-0.5*deltaOUT),exp(-0.5*DELTA^2./SIGMA2),sqrt(2.*!pi*SIGMA2)
endfor

if vb eq 1 then print,' '
if vb eq 1 then print,':: calcBadprob.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END

