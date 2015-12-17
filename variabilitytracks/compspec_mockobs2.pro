;+
;----------------------------
;   NAME
;----------------------------
; compspec_mockobs2.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Condensed and modified version of compspec_mockobs.pro making a
; simple variability model from the the composit spectrum from Vanden
; Berk et al 2001. From that mock observations are done in the SDSS bands
;----------------------------
;   COMMENTS
;----------------------------
; ZOBS            : the redshift to do the 'observations' at 
;----------------------------
;   INPUTS:
;----------------------------
; 
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
;
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> compspec_mockobs2,1.5,/VERBOSE
;
; creating frames for movies
; IDL> openw,55,'compspec_mockobs2_estimatedslopes.dat',width=300
; IDL> zs=findgen(250)/50.+0.1    ; 0.10 -- 5.08
; IDL> zs=findgen(200)/100.+0.4   ; 0.40 -- 2.39
; IDL> for i=0,n_elements(zs)-1 do begin & compspec_mockobs2,zs(i),/eps & endfor
; IDL> close,55

; IDL> openw,55,'compspec_mockobs2_estimatedslopes_PIVOT4xlammax_ui.dat',width=300
; IDL> zs=findgen(250)/50.+0.1    ; 0.10 -- 5.08
; IDL> for i=0,n_elements(zs)-1 do begin & compspec_mockobs2,zs(i),/eps & endfor
; IDL> close,55

;
;----------------------------
;   BUGS
;----------------------------
; 
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-03-10  started by K. B. Schmidt (MPIA)
; 2011-07-21  add the plot slopesVSmodeslopes_ui.eps. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ LOGLEVELS.pro
;----------------------------
;-
FUNCTION PWRLAW, X, P
  RETURN, P[0]*X^P[1]
END

FUNCTION LINE, X, P
  RETURN, P[0]*X + P[1]
END

PRO compspec_mockobs2,zobs,EPS=EPS,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)


;----------------------------------------------------------------------------------------------------------------------------------------
; read composite spectrum
compositespec = '/Users/kasperborelloschmidt/work/SDSScompositespectrum.txt'
readcol,compositespec,lam0,flux0,Ferr0,/silent

goodent = where(lam0 gt 1150 and lam0 lt 4500)         ; entries where a power-law continuum is a good approximation
lam     = lam0(goodent)
flux    = flux0(goodent)
Ferr    = Ferr0(goodent)

lamz0 = lam0*(1.+zobs)
lamz  = lam*(1.+zobs)
;----------------------------------------------------------------------------------------------------------------------------------------
; reading SDSS filters ; respt is the quantum efficiency for the given wavelength
uband = '/Users/kasperborelloschmidt/work/SDSSfilterset/sdss_jun2001_u_atm.dat'
readcol,uband,lam_u,respt_u,resbig_u,resnoa_u,xatm_u,comment='#',/silent
gband = '/Users/kasperborelloschmidt/work/SDSSfilterset/sdss_jun2001_g_atm.dat'
readcol,gband,lam_g,respt_g,resbig_g,resnoa_g,xatm_g,comment='#',/silent
rband = '/Users/kasperborelloschmidt/work/SDSSfilterset/sdss_jun2001_r_atm.dat'
readcol,rband,lam_r,respt_r,resbig_r,resnoa_r,xatm_r,comment='#',/silent
iband = '/Users/kasperborelloschmidt/work/SDSSfilterset/sdss_jun2001_i_atm.dat'
readcol,iband,lam_i,respt_i,resbig_i,resnoa_i,xatm_i,comment='#',/silent
zband = '/Users/kasperborelloschmidt/work/SDSSfilterset/sdss_jun2001_z_atm.dat'
readcol,zband,lam_z,respt_z,resbig_z,resnoa_z,xatm_z,comment='#',/silent

; calculating the continuum flux absorbed in each band at the given redshift
Lent_u = where(lamz gt min(lam_u) and lamz lt max(lam_u))   ; the entries of redshifted lambda values in the u band
Lent_g = where(lamz gt min(lam_g) and lamz lt max(lam_g))   ; the entries of redshifted lambda values in the g band
Lent_r = where(lamz gt min(lam_r) and lamz lt max(lam_r))   ; the entries of redshifted lambda values in the r band
Lent_i = where(lamz gt min(lam_i) and lamz lt max(lam_i))   ; the entries of redshifted lambda values in the i band
Lent_z = where(lamz gt min(lam_z) and lamz lt max(lam_z))   ; the entries of redshifted lambda values in the z band

if Lent_u eq [-1] then begin        ; if band outside spectral range create dymmy vectors
   Lent_u = findgen(10)             ; vector with 'dummy' entries for calculations 
   resp_uinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
endif else begin                    ; if band inside spectral range interpolate band respons
   resp_uinter  =  SPLINE(lam_u,respt_u,lamz(Lent_u))   ; interpolating the band to spectral wavelength spacing
endelse

if Lent_g eq [-1] then begin        ; if band outside spectral range create dymmy vectors
   Lent_g = findgen(10)             ; vector with 'dummy' entries for calculations 
   resp_ginter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
endif else begin                    ; if band inside spectral range interpolate band respons
   resp_ginter  =  SPLINE(lam_g,respt_g,lamz(Lent_g))   ; interpolating the band to spectral wavelength spacing
endelse

if Lent_r eq [-1] then begin        ; if band outside spectral range create dymmy vectors
   Lent_r = findgen(10)             ; vector with 'dummy' entries for calculations 
   resp_rinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
endif else begin                    ; if band inside spectral range interpolate band respons
   resp_rinter  =  SPLINE(lam_r,respt_r,lamz(Lent_r))   ; interpolating the band to spectral wavelength spacing
endelse

if Lent_i eq [-1] then begin        ; if band outside spectral range create dymmy vectors
   Lent_i = findgen(10)             ; vector with 'dummy' entries for calculations 
   resp_iinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
endif else begin                    ; if band inside spectral range interpolate band respons
   resp_iinter  =  SPLINE(lam_i,respt_i,lamz(Lent_i))   ; interpolating the band to spectral wavelength spacing
endelse

if Lent_z eq [-1] then begin        ; if band outside spectral range create dymmy vectors
   Lent_z = findgen(10)             ; vector with 'dummy' entries for calculations 
   resp_zinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
endif else begin                    ; if band inside spectral range interpolate band respons
   resp_zinter  =  SPLINE(lam_z,respt_z,lamz(Lent_z))   ; interpolating the band to spectral wavelength spacing
endelse
;----------------------------------------------------------------------------------------------------------------------------------------
; continuum powerlaw for Vanden Berk composite spectrum (between 1200 and 5000 A or so)
alpha1  = -1.528     ; spextral power law index - estimated from the spectral points (1286.50,6.235) and (4203.5,1.021)
F0      = 351572.        ; flux value at 0          - estimated from the spectral points (1286.50,6.235) and (4203.5,1.021)
PP      = [F0,alpha1]
xpiv    = 4*max(lam)                                 ; pivot point to change alpha around x-coordinates
;xpiv    = 16*max(lam)                                 ; pivot point to change alpha around x-coordinates
ypiv    = PWRLAW(xpiv,PP)                            ; pivot point to change alpha around y-coordinate
cont    = PWRLAW(lam,PP)                             ; Vanden Berk continuum
lines   = flux - cont                                ; flux subtracted power-law continuum
;----------------------------------------------------------------------------------------------------------------------------------------
; creating variable spectra
Nspec = 10 ; number of variability epochs, i.e. number of spectra to create
VarspecL  = fltarr(n_elements(lam),Nspec)
VarspecC  = fltarr(n_elements(lam),Nspec)
VarspecLC = fltarr(n_elements(lam),Nspec,Nspec)

FbandL    = fltarr(n_elements(lam),Nspec,5)
FbandC    = fltarr(n_elements(lam),Nspec,5)
FbandLC   = fltarr(n_elements(lam),Nspec,Nspec,5)

FtotC      = fltarr(Nspec,5)
FtotL      = fltarr(Nspec,5)
FtotLC     = fltarr(Nspec,Nspec,5)

linefrac = (findgen(Nspec))/(Nspec/2)                    ; amount of line flux to add
;alphas   = (findgen(Nspec)/Nspec-1/2.)*alpha1+alpha1     ; continuum power law slopes
alphas   = (findgen(Nspec)/Nspec-1/2.)*alpha1/7.+alpha1 ; continuum power law slopes
F0piv    = ypiv/(xpiv^alphas)                            ; F0 for different alphas
LCfrac   = findgen(Nspec)/Nspec+1.0/Nspec                ; Fixed fractions between lines and continuum to loop over

for ii=0,Nspec-1 do begin
   VarspecC(*,ii)     = PWRLAW(lam,[F0piv(ii),alphas(ii)])                          ; only slope of spectrum varies
   VarspecL(*,ii)     = lines*linefrac(ii)                                          ; only line flux varies

   for jj=0,Nspec-1 do begin
      VarspecLC(*,ii,jj) = PWRLAW(lam,[F0piv(ii),alphas(ii)])+lines*linefrac(jj)     ; both line flux and continuum slope varies

      FbandLC(Lent_u,ii,jj,0)   = resp_uinter*VarspecLC(Lent_u,ii,jj)     ; the flux multiplied with the respond of each filter
      FtotLC(ii,jj,0)      = TSUM(FbandLC(Lent_u,ii,jj,0))                 ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLC(Lent_g,ii,jj,1)   = resp_ginter*VarspecLC(Lent_g,ii,jj)     ; the flux multiplied with the respond of each filter
      FtotLC(ii,jj,1)      = TSUM(FbandLC(Lent_g,ii,jj,1))                 ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLC(Lent_r,ii,jj,2)   = resp_rinter*VarspecLC(Lent_r,ii,jj)     ; the flux multiplied with the respond of each filter
      FtotLC(ii,jj,2)      = TSUM(FbandLC(Lent_r,ii,jj,2))                 ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLC(Lent_i,ii,jj,3)   = resp_iinter*VarspecLC(Lent_i,ii,jj)     ; the flux multiplied with the respond of each filter
      FtotLC(ii,jj,3)      = TSUM(FbandLC(Lent_i,ii,jj,3))                 ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLC(Lent_z,ii,jj,4)   = resp_zinter*VarspecLC(Lent_z,ii,jj)     ; the flux multiplied with the respond of each filter
      FtotLC(ii,jj,4)      = TSUM(FbandLC(Lent_z,ii,jj,4))                 ; trapeziodal integration of the area under the curve for the actual spectrum values


   endfor

   FbandC(Lent_u,ii,0)   = resp_uinter*VarspecC(Lent_u,ii)    ; the flux multiplied with the respond of each filter
   FtotC(ii,0)      = TSUM(FbandC(Lent_u,ii,0))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandC(Lent_g,ii,1)   = resp_ginter*VarspecC(Lent_g,ii)    ; the flux multiplied with the respond of each filter
   FtotC(ii,1)      = TSUM(FbandC(Lent_g,ii,1))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandC(Lent_r,ii,2)   = resp_rinter*VarspecC(Lent_r,ii)    ; the flux multiplied with the respond of each filter
   FtotC(ii,2)      = TSUM(FbandC(Lent_r,ii,2))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandC(Lent_i,ii,3)   = resp_iinter*VarspecC(Lent_i,ii)    ; the flux multiplied with the respond of each filter
   FtotC(ii,3)      = TSUM(FbandC(Lent_i,ii,3))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandC(Lent_z,ii,4)   = resp_zinter*VarspecC(Lent_z,ii)    ; the flux multiplied with the respond of each filter
   FtotC(ii,4)      = TSUM(FbandC(Lent_z,ii,4))               ; trapeziodal integration of the area under the curve for the actual spectrum values

   FbandL(Lent_u,ii,0)   = resp_uinter*VarspecL(Lent_u,ii)    ; the flux multiplied with the respond of each filter
   FtotL(ii,0)      = TSUM(FbandL(Lent_u,ii,0))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandL(Lent_g,ii,1)   = resp_ginter*VarspecL(Lent_g,ii)    ; the flux multiplied with the respond of each filter
   FtotL(ii,1)      = TSUM(FbandL(Lent_g,ii,1))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandL(Lent_r,ii,2)   = resp_rinter*VarspecL(Lent_r,ii)    ; the flux multiplied with the respond of each filter
   FtotL(ii,2)      = TSUM(FbandL(Lent_r,ii,2))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandL(Lent_i,ii,3)   = resp_iinter*VarspecL(Lent_i,ii)    ; the flux multiplied with the respond of each filter
   FtotL(ii,3)      = TSUM(FbandL(Lent_i,ii,3))               ; trapeziodal integration of the area under the curve for the actual spectrum values
   FbandL(Lent_z,ii,4)   = resp_zinter*VarspecL(Lent_z,ii)    ; the flux multiplied with the respond of each filter
   FtotL(ii,4)      = TSUM(FbandL(Lent_z,ii,4))               ; trapeziodal integration of the area under the curve for the actual spectrum values
endfor
; --- the gband fluxes ---
; ii is continuum and jj is lines in FtotLC(ii,jj,*) where * is the 5 SDSS bands
;FtotLC(5,5,1) is VdB cont.+1.0*lines, i.e. the VdB spectrum
;FtotLC(0,5,1) is shallowest cont.+1.0*lines
;FtotLC(9,5,1) is steepest cont.+1.0*lines
;FtotLC(5,8,1) is VdB cont.+most lines
;FtotLC(5,*,1) is VdB cont.+all*lines
;FtotLC(*,5,*) is all cont.+1.0*lines
; --- the spectra ---
; same entries as above but for VarspecLC(*,ii,jj)

;----------------------------------------------------------------------------------------------------------------------------------------
; slope varies and lines added corresponding to fixed ratio between cont. and lines
VarspecLfix  = fltarr(n_elements(lam),Nspec,Nspec-1)
VarspecCfix  = fltarr(n_elements(lam),Nspec,Nspec-1)
VarspecLCfix = fltarr(n_elements(lam),Nspec,Nspec-1)
FbandLCfix   = fltarr(n_elements(lam),Nspec,Nspec-1,5)
FtotLCfix    = fltarr(Nspec,Nspec-1,5)
LCfrac   = findgen(Nspec)/Nspec+1.0/Nspec                ; Fixed fractions between lines and continuum to loop over
NLC      = 10000.
LineCon  = (findgen(NLC)/NLC)*10.                    ; vector with line contribution 

for aa=0,Nspec-1 do begin
   CfixM1   = PWRLAW(lam,[F0piv(0),alphas(0)])       ; first power-law continuum
   LfixM1   = Lines                                  ; the intial line spec
   for bb=1,Nspec-1 do begin
      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      Cfix    = PWRLAW(lam,[F0piv(bb),alphas(bb)])
      LfixTOT = tsum(LCfrac(aa)*(Cfix-CfixM1)+LfixM1)                        ; The integrated flux the lines need to represent; see notes 110327
      if LfixTOT lt 0 then print,':: compspec_mockobs.pro :: Something is not right - the integrated flux is < 0 even though it should always be > 0',stop

      LineVec  = tsum(LfixM1)*LineCon                                        ; vector with line contributions to check
      bestline = where(abs(LineVec-LfixTOT) eq min(abs(LineVec-LfixTOT)))    ; finding line contribution closest to the intergrated flux LfixTOT

      if bestline eq 0     then print,':: compspec_mockobs.pro :: Watch out - the best line contribution is the min value of LineCon'
      if bestline eq NLC-1 then print,':: compspec_mockobs.pro :: Watch out - the best line contribution is the max value of LineCon'
      Lfix     = LineCon(bestline(0))*LfixM1                                 ; the lines matchin LfixTOT the best

      VarspecLfix(*,aa,bb-1)  = Lfix
      VarspecCfix(*,aa,bb-1)  = Cfix
      VarspecLCfix(*,aa,bb-1) = Cfix+Lfix

      FbandLCfix(Lent_u,aa,bb-1,0)   = resp_uinter*VarspecLCfix(Lent_u,aa,bb-1)  ; the flux multiplied with the respond of each filter
      FtotLCfix(aa,bb-1,0)      = TSUM(FbandLCfix(Lent_u,aa,bb-1,0))             ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLCfix(Lent_g,aa,bb-1,1)   = resp_ginter*VarspecLCfix(Lent_g,aa,bb-1)  ; the flux multiplied with the respond of each filter
      FtotLCfix(aa,bb-1,1)      = TSUM(FbandLCfix(Lent_g,aa,bb-1,1))             ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLCfix(Lent_r,aa,bb-1,2)   = resp_rinter*VarspecLCfix(Lent_r,aa,bb-1)  ; the flux multiplied with the respond of each filter
      FtotLCfix(aa,bb-1,2)      = TSUM(FbandLCfix(Lent_r,aa,bb-1,2))             ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLCfix(Lent_i,aa,bb-1,3)   = resp_iinter*VarspecLCfix(Lent_i,aa,bb-1)  ; the flux multiplied with the respond of each filter
      FtotLCfix(aa,bb-1,3)      = TSUM(FbandLCfix(Lent_i,aa,bb-1,3))             ; trapeziodal integration of the area under the curve for the actual spectrum values
      FbandLCfix(Lent_z,aa,bb-1,4)   = resp_zinter*VarspecLCfix(Lent_z,aa,bb-1)  ; the flux multiplied with the respond of each filter
      FtotLCfix(aa,bb-1,4)      = TSUM(FbandLCfix(Lent_z,aa,bb-1,4))             ; trapeziodal integration of the area under the curve for the actual spectrum values

      ;print,'the integrated values:',LCfrac(aa),LfixTOT,tsum(Lfix),tsum(Cfix),tsum(LfixM1),tsum(CfixM1),(tsum(Lfix)-tsum(LfixM1))/(tsum(Cfix)-tsum(CfixM1))

      CfixM1 = Cfix  ; saving continuum for next calculation
      LfixM1 = Lfix  ; saving lines for next calculation
   endfor
endfor
;----------------------------------------------------------------------------------------------------------------------------------------
; open file with tracks
;datafile     = 'compspec_mockobs2_estimatedslopes_PIVOT2xlammax.dat'
datafile     = 'compspec_mockobs2_estimatedslopesALL.dat' ; 'compspec_mockobs2_estimatedslopes.dat'
;datafile     = 'compspec_mockobs2_estimatedslopes_PIVOT8xlammax.dat'
;datafile     = 'compspec_mockobs2_estimatedslopes_PIVOT16xlammax.dat'
Nrows        = File_lines(datafile)              ; number of rows in file
Nheaderlines = 2                                 ; number of header lines
Ncol         = 12                                ; number of columns in file
TRACKS       = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
header       = STRARR(Nheaderlines)              ; string array for header
openr,lun,datafile, /GET_LUN                     ; open file for reading     
readf,lun,header                                 ; reading header into string array
readf,lun,TRACKS                                 ; reading data into array
free_lun,lun  

; open file with tracks
datafile     = 'compspec_mockobs2_estimatedslopes_PIVOT4xlammax_ui.dat'
Nrows        = File_lines(datafile)              ; number of rows in file
Nheaderlines = 2                                 ; number of header lines
Ncol         = 12                                ; number of columns in file
TRACKSui     = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
header       = STRARR(Nheaderlines)              ; string array for header
openr,lun,datafile, /GET_LUN                     ; open file for reading     
readf,lun,header                                 ; reading header into string array
readf,lun,TRACKSui                               ; reading data into array
free_lun,lun

;TRACKSui = tracks

;----------------------------------------------------------------------------------------------------------------------------------------
restore,'GRslopeVSzNO2.idlsave'  ; restoring values to plot. Returns the vectors redshift, GRslope, zsort and slopemeanz
slopemeanz_gr = slopemeanz
restore,'UIslopeVSz.idlsave'     ; restoring values to plot. Returns the vectors redshift, UIslope, zsort and slopemeanz
slopemeanz_ui = slopemeanz
COLVEC = ['blue','green','yellow','orange','red'] ; vector with band colors

;----------------------------------------------------------------------------------------------------------------------------------------
; estimating the color variability (slopes)
; GR
slope0p1      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(0,*,1)),-2.5*alog10(FtotLCfix(0,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p2      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(1,*,1)),-2.5*alog10(FtotLCfix(1,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p3      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(2,*,1)),-2.5*alog10(FtotLCfix(2,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p4      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(3,*,1)),-2.5*alog10(FtotLCfix(3,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p5      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(4,*,1)),-2.5*alog10(FtotLCfix(4,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p6      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(5,*,1)),-2.5*alog10(FtotLCfix(5,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p7      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(6,*,1)),-2.5*alog10(FtotLCfix(6,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p8      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(7,*,1)),-2.5*alog10(FtotLCfix(7,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p9      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(8,*,1)),-2.5*alog10(FtotLCfix(8,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope1p0      = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(9,*,1)),-2.5*alog10(FtotLCfix(9,*,2)),fltarr(Nspec-1)+1.,[1,0],/QUIET)

slopeL        = MPFITFUN('LINE',-2.5*alog10(FtotLC(5,*,1)),-2.5*alog10(FtotLC(5,*,2)),fltarr(Nspec)+1.,[1,0],/QUIET)      ; online lines varying   - w VdB continuum
slopeC        = MPFITFUN('LINE',-2.5*alog10(FtotLC(*,5,1)),-2.5*alog10(FtotLC(*,5,2)),fltarr(Nspec)+1.,[1,0],/QUIET)      ; only continuum varying - w. VdB lines
slopeConly    = MPFITFUN('LINE',-2.5*alog10(FtotLC(*,0,1)),-2.5*alog10(FtotLC(*,0,2)),fltarr(Nspec)+1.,[1,0],/QUIET)  ; only continuum varying - w/o lines

; UI
slope0p1_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(0,*,0)),-2.5*alog10(FtotLCfix(0,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p2_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(1,*,0)),-2.5*alog10(FtotLCfix(1,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p3_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(2,*,0)),-2.5*alog10(FtotLCfix(2,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p4_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(3,*,0)),-2.5*alog10(FtotLCfix(3,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p5_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(4,*,0)),-2.5*alog10(FtotLCfix(4,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p6_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(5,*,0)),-2.5*alog10(FtotLCfix(5,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p7_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(6,*,0)),-2.5*alog10(FtotLCfix(6,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p8_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(7,*,0)),-2.5*alog10(FtotLCfix(7,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope0p9_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(8,*,0)),-2.5*alog10(FtotLCfix(8,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)
slope1p0_ui   = MPFITFUN('LINE',-2.5*alog10(FtotLCfix(9,*,0)),-2.5*alog10(FtotLCfix(9,*,3)),fltarr(Nspec-1)+1.,[1,0],/QUIET)

slopeL_ui     = MPFITFUN('LINE',-2.5*alog10(FtotLC(5,*,0)),-2.5*alog10(FtotLC(5,*,3)),fltarr(Nspec)+1.,[1,0],/QUIET)      ; online lines varying   - w VdB continuum
slopeC_ui     = MPFITFUN('LINE',-2.5*alog10(FtotLC(*,5,0)),-2.5*alog10(FtotLC(*,5,3)),fltarr(Nspec)+1.,[1,0],/QUIET)      ; only continuum varying - w. VdB lines
slopeConly_ui = MPFITFUN('LINE',-2.5*alog10(FtotLC(*,0,0)),-2.5*alog10(FtotLC(*,0,3)),fltarr(Nspec)+1.,[1,0],/QUIET)  ; only continuum varying - w/o lines


Nw = 0
;=============================================================================================
;                                    MULTIPLOT NUMBER 2
;=============================================================================================

!p.multi = [0,2,2]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/multiplot_CM2_'+strtrim(zobs,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=45, ysize=45;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=900, ysize=800, title = 'multiplot from comspec_mockobs2'
   thickall = 2
endelse

;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(lamz0),max(lamz0)]
YR = [0.1,50]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [\AA]') $
        , ytitle ='flux [arbitraty units]' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog, /ylog $
        , xticks = nticks-1 , xtickv=ticks $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

for ll=0,Nspec-1 do begin
   ;oplot,lamz,VarspecL(*,ll),col=col.lightgray,thick=thickall
   ;oplot,lamz,VarspecC(*,ll),col=col.lightgray,thick=thickall
   oplot,lamz,VarspecLC(*,5,ll),col=col.gray,thick=thickall       ; lines vary
   oplot,lamz,VarspecLC(*,ll,5),col=col.darkgray,thick=thickall   ; cont vary
endfor


; overplotting the lines
;oplot,lamz,lines,col=col.charcoal,thick=thickall
; overplotting the spectrum
oplot,lam0*(1.+zobs),flux0,col=col.black,thick=thickall
oplot,lamz,F0*lam^(alpha1),col=col.black,linestyle=2,thick=thickall

;overplotting sdss filters
ysc = 50  ; (abitrary) scaling factor
oplot,lamz(Lent_u),resp_uinter*ysc,col=col.blue,thick=thickall*2,linestyle=0
oplot,lamz(Lent_g),resp_ginter*ysc,col=col.green,thick=thickall*2,linestyle=0
oplot,lamz(Lent_r),resp_rinter*ysc,col=col.yellow,thick=thickall*2,linestyle=0
oplot,lamz(Lent_i),resp_iinter*ysc,col=col.orange,thick=thickall*2,linestyle=0
oplot,lamz(Lent_z),resp_zinter*ysc,col=col.red,thick=thickall*2,linestyle=0

oplot,fltarr(2)+min(lamz),findgen(2)*100+0.001,col=col.black,thick=thickall,linestyle=2
oplot,fltarr(2)+max(lamz),findgen(2)*100+0.001,col=col.black,thick=thickall,linestyle=2

XYOUTS,XR[0]+DX*0.90,YR[0]+DY*0.65,textoidl('z_{"obs"} = ')+trim(zobs),col=col.black,charthick=thickall,charsize=2,alignment=1.0
;------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------
; setting plot range
;XR = [max(-2.5*alog10(FtotC(*,1))),min(-2.5*alog10(FtotC(*,1)))]
;YR = [max(-2.5*alog10(FtotC(*,2))),min(-2.5*alog10(FtotC(*,2)))]
XR = [-4,-10]
YR = [-4,-10]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('-2.5*log F_{g}') $
        , ytitle =textoidl('-2.5*log F_{r}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white


xx = findgen(21)-10
oplot,xx,xx,thick=thickall,col=col.black,linestyle=2


;CCC = GETCOLOR(COLVEC(kk), 100)  ; setting color of epoch

oplot,xx,slopeC(0)*xx+slopeC(1),thick=thickall,col=col.darkgray
oplot,xx,slopeL(0)*xx+slopeL(1),thick=thickall,col=col.gray
oplot,xx,slopeConly(0)*xx+slopeConly(1),thick=thickall,col=col.magenta

PLOTSYM,0,2.0,/FILL
oplot,-2.5*alog10(FtotLC(5,*,1)),-2.5*alog10(FtotLC(5,*,2)),thick=thickall,col=col.gray,psym=8
oplot,-2.5*alog10(FtotLC(*,5,1)),-2.5*alog10(FtotLC(*,5,2)),thick=thickall,col=col.darkgray,psym=8

;------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('s_{gr}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

PLOTSYM,0,1.0,/FILL
oplot,redshift,GRslope,col=col.gray,psym=8,thick=thickall 
oplot,zsort,slopemeanz_gr,col=col.red,thick=thickall
oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

;dummyadd = 0.6

oplot,xx,fltarr(n_elements(xx))+1.0,thick=thickall,col=col.black,linestyle=2

dummyadd=0.6

PLOTSYM,0,2.0,/FILL
oplot,fltarr(2)+zobs,fltarr(2)+FtotL(5,0)/FtotC(5,0)+dummyadd,col=col.blue,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+FtotL(5,3)/FtotC(5,3)+dummyadd,col=col.orange,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+FtotL(5,4)/FtotC(5,4)+dummyadd,col=col.red,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+FtotL(5,2)/FtotC(5,2)+dummyadd,col=col.yellow,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+FtotL(5,1)/FtotC(5,1)+dummyadd,col=col.green,psym=8,thick=thickall 


zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one
zint = ROUND(slopemeanz_gr(mindif)*100.)/100.
XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('<s_{gr}> @ z_{"obs"} = ')+trim(zint),col=col.black,charsize=1.5,charthick=thickall,alignment=1.0

XYOUTS,XR[0]+0.05*DX,YR[0]+0.05*DY,textoidl('Filled circles: F_{line}/F_{continuum}+')+trim(dummyadd),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

;------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('s_{gr}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

PLOTSYM,0,1.0,/FILL
oplot,redshift,GRslope,col=col.charcoal,psym=8,thick=thickall 

oplot,zsort,slopemeanz_gr,col=col.red,thick=thickall

oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

; overplotting slopes for all redshifts
PLOTSYM,0,0.5,/FILL
oplot,findgen(250)/50.+0.1,TRACKS(0,*),thick=thickall,col=col.darkgray,psym=8
oplot,findgen(250)/50.+0.1,TRACKS(1,*),thick=thickall,col=col.gray,psym=8

; overplotting slope for given redshift
PLOTSYM,0,2.0,/FILL
oplot,fltarr(2)+zobs,fltarr(2)+slopeC(0),thick=thickall,psym=8,col=col.darkgray
oplot,fltarr(2)+zobs,fltarr(2)+slopeL(0),thick=thickall,psym=8,col=col.gray
oplot,fltarr(2)+zobs,fltarr(2)+slopeConly(0),thick=thickall,psym=8,col=col.magenta
PLOTSYM,0,2.0,thick=thickall
oplot,fltarr(2)+zobs,fltarr(2)+slopeC(0),thick=thickall,psym=8,col=col.black
oplot,fltarr(2)+zobs,fltarr(2)+slopeL(0),thick=thickall,psym=8,col=col.black
oplot,fltarr(2)+zobs,fltarr(2)+slopeConly(0),thick=thickall,psym=8,col=col.black

print,'The slope from continuum only variation: ',trim(slopeConly(0))

oplot,xx,fltarr(n_elements(xx))+1.0,thick=thickall,col=col.black,linestyle=2

zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one
zint = ROUND(slopemeanz_gr(mindif)*100.)/100.
XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('<s_{gr}> @ z_{"obs"} = ')+trim(zint),col=col.black,charsize=1.5,charthick=thickall,alignment=1.0

XYOUTS,XR[0]+0.05*DX,YR[0]+0.05*DY,textoidl('Filled circles: Line and Continuum Variability'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

;------------------------------------------------------------------------------------------------------

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


;=============================================================================================
;                                    MULTIPLOT NUMBER 2.2
;=============================================================================================

!p.multi = [0,3,1]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/multiplot_CM2p2_'+strtrim(zobs,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=40, ysize=15;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1300, ysize=450, title = 'multiplot from comspec_mockobs2.2'
   thickall = 2
endelse


;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(lamz0),max(lamz0)]
YR = [0.1,50]
;YR = [0.0,10]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [\AA]') $
        , ytitle ='flux [arbitraty units]' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog, /ylog $
        , xticks = nticks-1 , xtickv=ticks $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

for ll=0,Nspec-2 do begin
   oplot,lamz,VarspecLCfix(*,0,ll),col=col.lightgray,thick=thickall      ; dFline/dFcont = 0.1
   oplot,lamz,VarspecLCfix(*,4,ll),col=col.gray,thick=thickall           ; dFline/dFcont = 0.5
   oplot,lamz,VarspecLCfix(*,9,ll),col=col.darkgray,thick=thickall       ; dFline/dFcont = 1.0

   ;oplot,lamz,VarspecLfix(*,0,ll),col=col.red,thick=thickall             ; dFline/dFcont = 0.1
   ;oplot,lamz,VarspecLfix(*,4,ll),col=col.green,thick=thickall           ; dFline/dFcont = 0.5
   ;oplot,lamz,VarspecLfix(*,9,ll),col=col.blue,thick=thickall            ; dFline/dFcont = 1.0
endfor


; overplotting the lines
;oplot,lamz,lines,col=col.charcoal,thick=thickall
; overplotting the spectrum
oplot,lam0*(1.+zobs),flux0,col=col.black,thick=thickall
oplot,lamz,F0*lam^(alpha1),col=col.black,linestyle=2,thick=thickall

;overplotting sdss filters
ysc = 50  ; (abitrary) scaling factor
oplot,lamz(Lent_u),resp_uinter*ysc,col=col.blue,thick=thickall*2,linestyle=0
oplot,lamz(Lent_g),resp_ginter*ysc,col=col.green,thick=thickall*2,linestyle=0
oplot,lamz(Lent_r),resp_rinter*ysc,col=col.yellow,thick=thickall*2,linestyle=0
oplot,lamz(Lent_i),resp_iinter*ysc,col=col.orange,thick=thickall*2,linestyle=0
oplot,lamz(Lent_z),resp_zinter*ysc,col=col.red,thick=thickall*2,linestyle=0

oplot,fltarr(2)+min(lamz),findgen(2)*100+0.001,col=col.black,thick=thickall,linestyle=2
oplot,fltarr(2)+max(lamz),findgen(2)*100+0.001,col=col.black,thick=thickall,linestyle=2

XYOUTS,XR[0]+DX*0.90,YR[0]+DY*0.65,textoidl('z_{"obs"} = ')+trim(zobs),col=col.black,charthick=thickall,charsize=2,alignment=1.0
;------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------
; setting plot range
;XR = [max(-2.5*alog10(FtotC(*,1))),min(-2.5*alog10(FtotC(*,1)))]
;YR = [max(-2.5*alog10(FtotC(*,2))),min(-2.5*alog10(FtotC(*,2)))]
XR = [-4,-10]
YR = [-4,-10]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('-2.5*log F_{g}') $
        , ytitle =textoidl('-2.5*log F_{r}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white


xx = findgen(21)-10
oplot,xx,xx,thick=thickall,col=col.black,linestyle=2


;CCC = GETCOLOR(COLVEC(kk), 100)  ; setting color of epoch

oplot,xx,slope0p1(0)*xx+slope0p1(1),thick=thickall,col=col.lightgray
oplot,xx,slope0p5(0)*xx+slope0p5(1),thick=thickall,col=col.gray
oplot,xx,slope1p0(0)*xx+slope1p0(1),thick=thickall,col=col.darkgray

PLOTSYM,0,2.0,/FILL
oplot,-2.5*alog10(FtotLCfix(0,*,1)),-2.5*alog10(FtotLCfix(0,*,2)),thick=thickall,col=col.lightgray,psym=8
oplot,-2.5*alog10(FtotLCfix(4,*,1)),-2.5*alog10(FtotLCfix(4,*,2)),thick=thickall,col=col.gray,psym=8
oplot,-2.5*alog10(FtotLCfix(9,*,1)),-2.5*alog10(FtotLCfix(9,*,2)),thick=thickall,col=col.darkgray,psym=8

;------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.0,1.4]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('s_{gr}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

PLOTSYM,0,0.5,/FILL
oplot,redshift,GRslope,col=col.charcoal,psym=8,thick=thickall 

oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

three = 1
if three eq 1 then begin  ; only plotting 3 slope tracks
   ; overplotting slopes for all redshifts
   oplot,zsort,slopemeanz_gr,col=col.red,thick=thickall

   PLOTSYM,0,1.0,/FILL
   oplot,findgen(250)/50.+0.1,TRACKS(2,*),thick=thickall,col=col.lightgray,psym=8
   oplot,findgen(250)/50.+0.1,TRACKS(6,*),thick=thickall,col=col.gray,psym=8
   oplot,findgen(250)/50.+0.1,TRACKS(11,*),thick=thickall,col=col.darkgray,psym=8
   ; overplotting slope for given redshift
   PLOTSYM,0,2.0,/FILL
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p1(0),thick=thickall,psym=8,col=col.lightgray
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p5(0),thick=thickall,psym=8,col=col.gray
   oplot,fltarr(2)+zobs,fltarr(2)+slope1p0(0),thick=thickall,psym=8,col=col.darkgray
   PLOTSYM,0,2.0,thick=thickall
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p1(0),thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p5(0),thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+zobs,fltarr(2)+slope1p0(0),thick=thickall,psym=8,col=col.black

   oplot,xx,fltarr(n_elements(xx))+1.0,thick=thickall,col=col.black,linestyle=2


   PLOTSYM,0,2.0,/FILL
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.06*DY,thick=thickall,psym=8,col=col.lightgray
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.11*DY,thick=thickall,psym=8,col=col.gray
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.16*DY,thick=thickall,psym=8,col=col.darkgray
   PLOTSYM,0,2.0,thick=thickall
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.06*DY,thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.11*DY,thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.16*DY,thick=thickall,psym=8,col=col.black

   XYOUTS,XR[0]+0.10*DX,YR[0]+0.05*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.1'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.10*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.5'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.15*DY,textoidl('\delta F_{line} / \delta F_{cont} = 1.0'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

endif else begin  ; overplotting all slope tracks
   device,decomposed=0
   LOADCT,39
   Ncolors  = 12
   Crainbow = BYTSCL(findgen(Ncolors)) 
   PLOTSYM,0,2.0,/FILL
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p1(0),thick=thickall,psym=8,col=Crainbow(1)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p2(0),thick=thickall,psym=8,col=Crainbow(2)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p3(0),thick=thickall,psym=8,col=Crainbow(3)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p4(0),thick=thickall,psym=8,col=Crainbow(4)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p5(0),thick=thickall,psym=8,col=Crainbow(5)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p6(0),thick=thickall,psym=8,col=Crainbow(6)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p7(0),thick=thickall,psym=8,col=Crainbow(7)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p8(0),thick=thickall,psym=8,col=Crainbow(8)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p9(0),thick=thickall,psym=8,col=Crainbow(9)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope1p0(0),thick=thickall,psym=8,col=Crainbow(10)

   oplot,findgen(250)/50.+0.1,TRACKS(2,*),thick=thickall,col=Crainbow(1)
   oplot,findgen(250)/50.+0.1,TRACKS(3,*),thick=thickall,col=Crainbow(2)
   oplot,findgen(250)/50.+0.1,TRACKS(4,*),thick=thickall,col=Crainbow(3)
   oplot,findgen(250)/50.+0.1,TRACKS(5,*),thick=thickall,col=Crainbow(4)
   oplot,findgen(250)/50.+0.1,TRACKS(6,*),thick=thickall,col=Crainbow(5)
   oplot,findgen(250)/50.+0.1,TRACKS(7,*),thick=thickall,col=Crainbow(6)
   oplot,findgen(250)/50.+0.1,TRACKS(8,*),thick=thickall,col=Crainbow(7)
   oplot,findgen(250)/50.+0.1,TRACKS(9,*),thick=thickall,col=Crainbow(8)
   oplot,findgen(250)/50.+0.1,TRACKS(10,*),thick=thickall,col=Crainbow(9)
   oplot,findgen(250)/50.+0.1,TRACKS(11,*),thick=thickall,col=Crainbow(10)

   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.06*DY,thick=thickall,psym=8,col=Crainbow(1)
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.11*DY,thick=thickall,psym=8,col=Crainbow(10)

   col=getcolor(/load)  
   oplot,findgen(250)/50.+0.1,TRACKS(1,*),thick=thickall,col=col.darkgray  ; lines only
   oplot,findgen(250)/50.+0.1,TRACKS(0,*),thick=thickall,col=col.lightgray  ; cont only

   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.16*DY,thick=thickall,psym=8,col=col.darkgray
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.21*DY,thick=thickall,psym=8,col=col.lightgray

   XYOUTS,XR[0]+0.10*DX,YR[0]+0.05*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.1'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.10*DY,textoidl('\delta F_{line} / \delta F_{cont} = 1.0'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.15*DY,textoidl('F_{cont} fixed'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.20*DY,textoidl('F_{line} fixed'),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

   oplot,zsort,slopemeanz_gr,col=col.black,thick=thickall+3
endelse


zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one
zint = ROUND(slopemeanz_gr(mindif)*100.)/100.
XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('<s_{gr}> @ z_{"obs"} = ')+trim(zint),col=col.black,charsize=1.5,charthick=thickall,alignment=1.0




;------------------------------------------------------------------------------------------------------

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


;=============================================================================================
;                                    Single plot
;=============================================================================================

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/slopesVSmodeslopes.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw,  xsize=600, ysize=500, title = 'multiplot from comspec_mockobs2.2'
   thickall = 2
endelse

; setting plot range
XR = [0.3,3.0]
YR = [0.0,1.4]

;YR = [0.4,1.2]  ; alternative range...

SM = 1
if SM eq 1 then begin
   YR = YR - 1
   TRACKS = TRACKs-1
   GRslope = GRslope - 1
   slopemeanz_gr = slopemeanz_gr - 1
endif
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
;        , ytitle =textoidl('s_{gr}   \equiv   \deltam_r/\deltam_g - 1') $
        , ytitle =textoidl('Obs. quasar color variability  (\deltam_r/\deltam_g - 1)') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

contourarray,redshift,XR[0],XR[1],GRslope,YR[0],YR[1],40,40,4,contarr,levelbin,xrange,yrange
contour,contarr,xrange,yrange,/overplot,/fill $     ; you can use the command /overplot if needed
	, levels=[0.0,4.0,8.0,16.0,32.0] $ ;  levelbin $
	, C_COLOR=[col.white,col.lightgray,col.mediumgray,col.gray,col.darkgray]  $
        , C_THICK = thickall 
;	, C_LABELS = levelbin

PLOTSYM,0,0.5,/FILL
;oplot,redshift,GRslope,col=col.charcoal,psym=8,thick=thickall 

three = 0
if three eq 1 then begin  ; only plotting 3 slope tracks
   ; overplotting slopes for all redshifts
   oplot,zsort,slopemeanz_gr,col=col.red,thick=thickall

   PLOTSYM,0,1.0,/FILL
   oplot,findgen(250)/50.+0.1,TRACKS(2,*),thick=thickall,col=col.lightgray,psym=8
   oplot,findgen(250)/50.+0.1,TRACKS(6,*),thick=thickall,col=col.gray,psym=8
   oplot,findgen(250)/50.+0.1,TRACKS(11,*),thick=thickall,col=col.darkgray,psym=8
   ; overplotting slope for given redshift
   PLOTSYM,0,2.0,/FILL
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p1(0),thick=thickall,psym=8,col=col.lightgray
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p5(0),thick=thickall,psym=8,col=col.gray
   oplot,fltarr(2)+zobs,fltarr(2)+slope1p0(0),thick=thickall,psym=8,col=col.darkgray
   PLOTSYM,0,2.0,thick=thickall
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p1(0),thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+zobs,fltarr(2)+slope0p5(0),thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+zobs,fltarr(2)+slope1p0(0),thick=thickall,psym=8,col=col.black

   oplot,xx,fltarr(n_elements(xx))+1.0,thick=thickall,col=col.black,linestyle=2

   PLOTSYM,0,2.0,/FILL
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.06*DY,thick=thickall,psym=8,col=col.lightgray
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.11*DY,thick=thickall,psym=8,col=col.gray
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.16*DY,thick=thickall,psym=8,col=col.darkgray
   PLOTSYM,0,2.0,thick=thickall
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.06*DY,thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.11*DY,thick=thickall,psym=8,col=col.black
   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.16*DY,thick=thickall,psym=8,col=col.black

   XYOUTS,XR[0]+0.10*DX,YR[0]+0.05*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.1'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.10*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.5'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.10*DX,YR[0]+0.15*DY,textoidl('\delta F_{line} / \delta F_{cont} = 1.0'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0

endif else begin  ; overplotting all slope tracks
   device,decomposed=0
   LOADCT,39
   Ncolors  = 12
   Crainbow = BYTSCL(findgen(Ncolors)) 
   PLOTSYM,0,2.0,/FILL
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p1(0),thick=thickall,psym=8,col=Crainbow(1)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p2(0),thick=thickall,psym=8,col=Crainbow(2)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p3(0),thick=thickall,psym=8,col=Crainbow(3)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p4(0),thick=thickall,psym=8,col=Crainbow(4)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p5(0),thick=thickall,psym=8,col=Crainbow(5)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p6(0),thick=thickall,psym=8,col=Crainbow(6)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p7(0),thick=thickall,psym=8,col=Crainbow(7)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p8(0),thick=thickall,psym=8,col=Crainbow(8)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope0p9(0),thick=thickall,psym=8,col=Crainbow(9)
   ;oplot,fltarr(2)+zobs,fltarr(2)+slope1p0(0),thick=thickall,psym=8,col=col.blue ;Crainbow(10)

   col=getcolor(/load)     ; get color table for plot
   ; plot simulated color variabilties
   ;oplot,findgen(250)/50.+0.1,TRACKS(2,*),thick=thickall,col=Crainbow(1)
   ;oplot,findgen(250)/50.+0.1,TRACKS(3,*),thick=thickall,col=Crainbow(2)
   ;oplot,findgen(250)/50.+0.1,TRACKS(4,*),thick=thickall,col=Crainbow(3)
   ;oplot,findgen(250)/50.+0.1,TRACKS(5,*),thick=thickall,col=Crainbow(4)
   ;oplot,findgen(250)/50.+0.1,TRACKS(6,*),thick=thickall,col=Crainbow(5)
   ;oplot,findgen(250)/50.+0.1,TRACKS(7,*),thick=thickall,col=Crainbow(6)
   ;oplot,findgen(250)/50.+0.1,TRACKS(8,*),thick=thickall,col=Crainbow(7)
   ;oplot,findgen(250)/50.+0.1,TRACKS(9,*),thick=thickall,col=Crainbow(8)
   ;oplot,findgen(250)/50.+0.1,TRACKS(10,*),thick=thickall,col=Crainbow(9)
   oplot,findgen(250)/50.+0.1,TRACKS(11,*),thick=thickall,col=col.blue ;Crainbow(10)

   ;oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.11,0.11]*DY,thick=thickall,col=Crainbow(1);,psym=8
   ;oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.16,0.16]*DY,thick=thickall,col=Crainbow(5);,psym=8
   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.14,0.14]*DY,thick=thickall,col=col.blue ; Crainbow(10);,psym=8

   col=getcolor(/load)  
   oplot,findgen(250)/50.+0.1,TRACKS(0,*),thick=thickall*2,col=col.black,linestyle=5 ; cont only
;   oplot,findgen(250)/50.+0.1,TRACKS(1,*),thick=thickall,col=col.darkgray  ; lines only

   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.07,0.07]*DY,thick=thickall*2,col=col.black,linestyle=5;,psym=8
;   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.21*DY,thick=thickall,psym=8,col=col.darkgray

;   XYOUTS,XR[0]+0.15*DX,YR[0]+0.05*DY,textoidl('Only continuum variation'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
;   XYOUTS,XR[0]+0.15*DX,YR[0]+0.10*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.1'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
;   XYOUTS,XR[0]+0.15*DX,YR[0]+0.15*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.5'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
;   XYOUTS,XR[0]+0.15*DX,YR[0]+0.20*DY,textoidl('\delta F_{line} / \delta F_{cont} = 1.0'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
;   XYOUTS,XR[0]+0.15*DX,YR[0]+0.25*DY,textoidl('<s_{gr}>'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0



   XYOUTS,XR[0]+0.15*DX,YR[0]+0.19*DY,textoidl('<s_{gr}> observed'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.15*DX,YR[0]+0.12*DY,textoidl('Lines respond to continuum'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.15*DX,YR[0]+0.05*DY,textoidl('No line response; only continuum'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0


;   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.26,0.26]*DY,thick=thickall*2,col=col.black
   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.21,0.21]*DY,thick=thickall*2,col=col.black
   oplot,zsort,slopemeanz_gr,col=col.black,thick=thickall*2
endelse

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================



;=============================================================================================
;                                    Single plot
;=============================================================================================

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/slopesVSmodeslopes_ui.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw,  xsize=600, ysize=500, title = 'multiplot from comspec_mockobs2.2 ui'
   thickall = 2
endelse

; setting plot range
XR = [0.3,3.0]
YR = [0.0,1.4]
SM = 1
if SM eq 1 then begin
   YR = YR - 1
   TRACKSui = TRACKSui-1
   UIslope = UIslope - 1
   slopemeanz_ui = slopemeanz_ui - 1
endif
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('s_{gr}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

contourarray,redshift,XR[0],XR[1],UIslope,YR[0],YR[1],40,40,4,contarr,levelbin,xrange,yrange
contour,contarr,xrange,yrange,/overplot,/fill $     ; you can use the command /overplot if needed
	, levels=[0.0,4.0,8.0,10.0,20.0] $ ;  levelbin $
	, C_COLOR=[col.white,col.lightgray,col.mediumgray,col.gray,col.darkgray]  $
        , C_THICK = thickall 
;	, C_LABELS = levelbin

PLOTSYM,0,0.5,/FILL
;oplot,redshift,UIslope,col=col.charcoal,psym=8,thick=thickall 

three = 0
if three eq 1 then begin  ; only plotting 3 slope tracks
   ; empty
endif else begin  ; overplotting all slope tracks
   device,decomposed=0
   LOADCT,39
   Ncolors  = 12
   Crainbow = BYTSCL(findgen(Ncolors)) 
   PLOTSYM,0,2.0,/FILL

   oplot,findgen(250)/50.+0.1,TRACKSui(2,*),thick=thickall,col=Crainbow(1)
   oplot,findgen(250)/50.+0.1,TRACKSui(3,*),thick=thickall,col=Crainbow(2)
   oplot,findgen(250)/50.+0.1,TRACKSui(4,*),thick=thickall,col=Crainbow(3)
   oplot,findgen(250)/50.+0.1,TRACKSui(5,*),thick=thickall,col=Crainbow(4)
   oplot,findgen(250)/50.+0.1,TRACKSui(6,*),thick=thickall,col=Crainbow(5)
   oplot,findgen(250)/50.+0.1,TRACKSui(7,*),thick=thickall,col=Crainbow(6)
   oplot,findgen(250)/50.+0.1,TRACKSui(8,*),thick=thickall,col=Crainbow(7)
   oplot,findgen(250)/50.+0.1,TRACKSui(9,*),thick=thickall,col=Crainbow(8)
   oplot,findgen(250)/50.+0.1,TRACKSui(10,*),thick=thickall,col=Crainbow(9)
   oplot,findgen(250)/50.+0.1,TRACKSui(11,*),thick=thickall,col=Crainbow(10)

   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.81,0.81]*DY,thick=thickall,col=Crainbow(1);,psym=8
   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.86,0.86]*DY,thick=thickall,col=Crainbow(5);,psym=8
   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.91,0.91]*DY,thick=thickall,col=Crainbow(10);,psym=8

   col=getcolor(/load)  
   oplot,findgen(250)/50.+0.1,TRACKSui(0,*),thick=thickall*2,col=col.black,linestyle=5 ; cont only
;   oplot,findgen(250)/50.+0.1,TRACKSui(1,*),thick=thickall,col=col.darkgray  ; lines only

   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.96,0.96]*DY,thick=thickall*2,col=col.black,linestyle=5;,psym=8
;   oplot,fltarr(2)+XR[0]+0.05*DX,fltarr(2)+YR[0]+0.21*DY,thick=thickall,psym=8,col=col.darkgray

   XYOUTS,XR[0]+0.15*DX,YR[0]+0.95*DY,textoidl('Only continuum variation'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.15*DX,YR[0]+0.80*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.1'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.15*DX,YR[0]+0.85*DY,textoidl('\delta F_{line} / \delta F_{cont} = 0.5'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.15*DX,YR[0]+0.90*DY,textoidl('\delta F_{line} / \delta F_{cont} = 1.0'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0
   XYOUTS,XR[0]+0.15*DX,YR[0]+0.75*DY,textoidl('<s_{gr}>'),col=col.black,charsize=2.5,charthick=thickall,alignment=0.0

   oplot,fltarr(2)+XR[0]+[0.03,0.13]*DX,fltarr(2)+YR[0]+[0.76,0.76]*DY,thick=thickall*2,col=col.black
   oplot,zsort,slopemeanz_ui,col=col.black,thick=thickall*2
endelse

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


; printing estimated slopes to pre-opened file
; printf,55,slopeC(0),slopeL(0),slope0p1(0),slope0p2(0),slope0p3(0),slope0p4(0),slope0p5(0),slope0p6(0),slope0p7(0),slope0p8(0),slope0p9(0),slope1p0(0)
; printf,55,slopeC_ui(0),slopeL_ui(0),slope0p1_ui(0),slope0p2_ui(0),slope0p3_ui(0),slope0p4_ui(0),slope0p5_ui(0),slope0p6_ui(0),slope0p7_ui(0),slope0p8_ui(0),slope0p9_ui(0),slope1p0_ui(0)

if vb eq 1 then print,' '
if vb eq 1 then print,':: compspec_mockobs.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '

;stop
END
