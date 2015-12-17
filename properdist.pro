;+
;----------------------------
;   NAME
;----------------------------
; properdist.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure calculating the proper distance of an object at a given redshift
; by numerically integration. It also returns the correpsonding angular diameter
; distance and the luminosity distance using Dproper = Dang*(1+z) = Dlum/(1+z)
;----------------------------
;   COMMENTS
;----------------------------
; NB!
; Since QSIMP is used the cosmology is hardcoded into the function to be integrated
; (QSIMP only takes functions with one argument) and providing Omegas manually 
; therefore makes no difference. However you can manually change the function to be
; integrated to use another cosmology.
;
; The input redshift should be a decimal/float number NOT and integer
;----------------------------
;   INPUTS:
;----------------------------
; z              : the redzhift of the object
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Omat           : Set Omat = to the manually provided value of Omega matter
;                  If not set the WMAP 5 years default value is used
; Olam           : Set Olam = to the manually provided value of Omega lambda
;                  If not set the WMAP 5 years default value is used
; Orad           : Set Orad = to the manually provided value of Omega radiation
;                  If not set the WMAP 5 years default value is used
; H0             : Set H0 = to the value of the present Hubble constant in (km/s/Mpc)
;                  If not set the WMAP 5 years default value is used
; /VERBOSE       : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; Dp             : The numerically calcultaed proper distance in Mpc
; Dl             : The luminosity distance corresponding to Dp*(1+z) in Mpc
; Da             : The angular diameter distance corresponding to Dp/(1+z) in Mpc
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> properdist,2.45,Dp,Dl,Da,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2009-11-18  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ properfct.pro
;----------------------------
;-
; input/output:i  o  o  o  opt. i    opt. i    opt. i   opt. i     opt. i
PRO properdist,z,Dp,Dl,Da,Omat=Omat,Olam=Olam,Orad=Orad,H0=H0,VERBOSE=VERBOSE

OM  = n_elements(Omat)
OL  = n_elements(Olam)
ORA = n_elements(Orad)
Hn  = n_elements(H0)
VB  = n_elements(VERBOSE)

;setting default cosmology to WMAP 5 values
if OM  eq 0 then Omat = 0.27
if OL  eq 0 then Olam = 0.73
if ORA eq 0 then Orad = 0.0
if Hn  eq 0 then H0   = 70.5
; the total energy density Omega_0
Otot = Orad+Omat+Olam

; defining the speed of light
c = 299792.458 ; km / s

; integration limits
lower = 1/(1+z)
upper = 1

; Performing numerical integration via QSIMP
; QSIMP is based on the routine qsimp described in section 4.2 of Numerical Recipes
intval = QSIMP('properfct',lower,upper)

; calculating proper distance in units of Mpc
Dp = c/H0 * intval
Dl = Dp*(1+z)
Da = Dp/(1+z)
if VB eq 1 then print,':: properdist.pro :: The proper distance at z=',strtrim(z,1),' is ',strtrim(Dp,1),' Mpc'
if VB eq 1 then print,'                     The corresponding luminosity distance is ',strtrim(Dl,1),' Mpc'
if VB eq 1 then print,'                     The corresponding angular diameter distance is ',strtrim(Da,1),' Mpc'

END

; expression to be integrated (da/dt isolated in the Friedmann equation (without H0)
; times 1/a for a being the scale facter 
FUNCTION properfct, a
   ;setting default cosmology to WMAP 5 values
   Omat = 0.27
   Olam = 0.73
   Orad = 0.0
   ; the total energy density Omega_0
   Otot = Orad+Omat+Olam
   res = 1/(sqrt(Orad+Omat*a+Olam*a^(4.)+a^(2.)*(1-Otot)))
   RETURN, res
END
