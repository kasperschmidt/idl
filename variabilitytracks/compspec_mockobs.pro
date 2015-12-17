;+
;----------------------------
;   NAME
;----------------------------
; compspec_mockobs.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure reads the composit spectrum from Vanden Berk et al 2001
; and creates mock observations at a given redshigt using the SDSS
; filter set. The 'Observations' are created for a decomposit spectrum as
; well, i.e. for continuum and emission lines only, and for various
; scalings of these.
;
; The mock quasar observations are written to an output file.
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
; /MULTIPLOT      : set /MULTIPLOT to create a multiplot of the
;                   individual plots
; /OBSSLOPE       : set /OBSSLOPE to overplot the estimated observed
;                   long and short term slopes for the given redshift
;                   note however that the observed slopes are from a 
;                   spline interpolation of the mean slopes in distinct
;                   z-bins, so only values at z=findgen(200)/100.+0.4
;                   are 'known'
; /GRSLOPE        : this keyword reads the average gr slopes fitted with
;                   linearfitMCMC.pro as a function of redhift for the 9000 stripe 82
;                   QSOs and plot this with an indication of the given redshift for
;                   comparison.  
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; mockobs         : Name of the output file with the mock observations
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> compspec_mockobs,1.5,mockobs,/VERBOSE,/MULTIPLOT,/OBSSLOPE
;
; creating frames for movies
; IDL> zs=findgen(250)/50.+0.1   ; 0.10 -- 5.08
; IDL> zs=findgen(200)/100.+0.4   ; 0.40 -- 2.39
; IDL> for i=0,n_elements(zs)-1 do begin & compspec_mockobs,zs(i),mockobs,/VERBOSE,/eps,/MULTIPLOT,/OBSSLOPE,/GRSLOPE & endfor
;----------------------------
;   BUGS
;----------------------------
; No rescaling, dimming or k-correction is perforemed when redshifting
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-08-13  started by K. B. Schmidt (MPIA)
; 2011-01-25  GRSLOPE keyword added K. B. Schmidt (MPIA)
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

PRO compspec_mockobs,zobs,mockobs,OBSSLOPE=OBSSLOPE,MULTIPLOT=MULTIPLOT,GRSLOPE=GRSLOPE,EPS=EPS,VERBOSE=VERBOSE

OBS   = n_elements(OBSSLOPE)
MPLOT = n_elements(MULTIPLOT)
GRS   = n_elements(GRslope)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

; read composite spectrum
compositespec = '/Users/kasperborelloschmidt/work/SDSScompositespectrum.txt'
readcol,compositespec,lam,flux,Ferr,/silent

lamz = lam*(1.+zobs)

; continuum powerlaw for Vanden Berk composite spectrum between 1200 and 5000 A or so
alpha1 = -1.528     ; spextral power law index - estimated from the spectral points (1286.50,6.235) and (4203.5,1.021)
F0 = 351572.        ; flux value at 0          - estimated from the spectral points (1286.50,6.235) and (4203.5,1.021)
PP = [F0,alpha1]

; flux subtracted power law continuum
lines = flux - PWRLAW(lam,PP)

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

; the flux multiplied with the respond of each filter
u01_tot = resp_uinter*Flux(Lent_u)
g01_tot = resp_ginter*Flux(Lent_g)
r01_tot = resp_rinter*Flux(Lent_r)
i01_tot = resp_iinter*Flux(Lent_i)
z01_tot = resp_zinter*Flux(Lent_z)
; trapeziodal integration of the area under the curve for the actual spectrum values
F_utot = TSUM(u01_tot)
F_gtot = TSUM(g01_tot)
F_rtot = TSUM(r01_tot)
F_itot = TSUM(i01_tot)
F_ztot = TSUM(z01_tot)

; the continuum power law parameters to loop over:
;NF0      = 21
;F0Min    = 0.5*PP[0]
;F0Max    = 2.0*PP[0]
NF0      = 3
F0Min    = 0.5*PP[0]
F0Max    = 2.0*PP[0]
F0val    = (findgen(NF0)/NF0)*(F0Max-F0Min)+F0Min

;Nalpha   = 21
;alphaMin = 0.5*PP[1]
;alphaMax = 2.0*PP[1]
Nalpha   = 3
alphaMin = 0.5*PP[1]
alphaMax = 2.0*PP[1]
alphaVal = (findgen(Nalpha)/Nalpha)*(alphaMax-alphaMin)+alphaMin
; fraction of line flux to be added in loop
;Nlinefrac= 7
;Lfrac    = findgen(Nlinefrac)/3.
Nlinefrac= 4
Lfrac    = findgen(Nlinefrac)/2.


; opening output file and writing its header
mockobs = 'compspecRUN/compspec_mockobs_'+strtrim(NF0,2)+'F0_x_'+strtrim(Nalpha,2)+'alpha_z'+strtrim(zobs,2)+'.dat' 
openw,44,mockobs,width=300
printf,44,'# '
printf,44,'#       = = = file created with compspec_mockobs.pro on '+strtrim(systime(),2)+' = = ='
printf,44,'# '
printf,44,'# This file contains the estimated flux values form the Vanden Berk et al 2001 composit QSO spectrum '
printf,44,'# for ',strtrim(NF0*Nalpha*Nlinefrac,2),' different continuum power laws on the form F0*lambda^alpha (+ Lfrac*Flines)'
printf,44,'# where Lfrac adds some fraction of the lines'
printf,44,'# The '+strtrim(Nalpha,2)+' alpha values are equally space between alphaMin = '+strtrim(alphaMin,2)+' and alphaMax = '+strtrim(alphaMax,2)
;printf,44,'# The '+strtrim(NF0,2)+' flux values are equally spaced between F0Min = '+strtrim(F0Min,2)+' and F0Max = '+strtrim(F0Max,2)
printf,44,'# The '+strtrim(Nlinefrac,2)+' fractions of the lines are equally space between LfracMin = '+strtrim(min(Lfrac),2)+' and LfracMax = '+strtrim(Max(Lfrac),2)
printf,44,'# For each power law the FxTOT, the flux values estimated using the actual spectrum, is given for comparison'
printf,44,'# '
printf,44,'# The columns are:'
printf,44,'# redshift     F0     alpha     Lfrac     FuTOT     FgTOT     FrTOT     FiTOT     FzTOT     Fuband     Fgband     Frband     Fiband     Fzband     FubandLine     FgbandLine     FrbandLine     FibandLine     FzbandLine     '
printf,44,'# '

; ensuring that estimates are only made in the range where the power law is a good approximation
lmin = 1280.*(1.+zobs)
lmax = 4700.*(1.+zobs)
Rz_u  = resp_uinter
Rz_g  = resp_ginter
Rz_r  = resp_rinter
Rz_i  = resp_iinter
Rz_z  = resp_zinter
; gettign the entries outside range
entout_u = where( (lamz(Lent_u) lt lmin) or (lamz(Lent_u) gt lmax) )
entout_g = where( (lamz(Lent_g) lt lmin) or (lamz(Lent_g) gt lmax) )
entout_r = where( (lamz(Lent_r) lt lmin) or (lamz(Lent_r) gt lmax) )
entout_i = where( (lamz(Lent_i) lt lmin) or (lamz(Lent_i) gt lmax) )
entout_z = where( (lamz(Lent_z) lt lmin) or (lamz(Lent_z) gt lmax) )
; set quantum efficiency to 0 outside powerlaw range 
;if entout_u ne [-1] then Rz_u(entout_u) = 0.0  
;if entout_g ne [-1] then Rz_g(entout_g) = 0.0  
;if entout_r ne [-1] then Rz_r(entout_r) = 0.0  
;if entout_i ne [-1] then Rz_i(entout_i) = 0.0  
;if entout_z ne [-1] then Rz_z(entout_z) = 0.0  

; calculating total flux for various continuum power laws
for mm=0,NF0-1 do begin                                   ;   looping over F0s
   for nn=0,Nalpha-1 do begin                             ;   looping over alphas

      Cpivot = [F0val(mm),PP[1]]                          ; coordinates for pivot powerlaw
;      xpiv   = (lmax-lmin)/3.+lmin                        ; pivot point to change alpha around x-coordinate
;      ypiv   = PWRLAW(xpiv,Cpivot)                        ; pivot point to change alpha around y-coordinate
      xpiv   = 4*lmax                                     ; pivot point to change alpha around x-coordinate
      ypiv   = PWRLAW(xpiv,Cpivot)                        ; pivot point to change alpha around y-coordinate
      F0piv  = ypiv/(xpiv^alphaval(nn))                   ; the F0 for pivot point and slope alphaval(nn)

      coef   = [F0piv,alphaval(nn)]                       ; powerlaw coefficients
;      coef   = [F0val(mm),alphaval(nn)]                  ; powerlaw coefficients

      u01    = Rz_u*PWRLAW(lam(Lent_u),coef)             ; calculating the powerlaw 'convolved' with reponds 
      F01_u  = TSUM(u01)                                  ; the trapezoidal integration over curve
      g01    = Rz_g*PWRLAW(lam(Lent_g),coef)
      F01_g  = TSUM(g01)
      r01    = Rz_r*PWRLAW(lam(Lent_r),coef)
      F01_r  = TSUM(r01)
      i01    = Rz_i*PWRLAW(lam(Lent_i),coef)
      F01_i  = TSUM(i01)
      z01    = Rz_z*PWRLAW(lam(Lent_z),coef)
      F01_z  = TSUM(z01)

      for ll=0,Nlinefrac-1 do begin                       ; looping over fraction of lines added
         u01L   = u01 + Rz_u * Lfrac(ll)*lines(Lent_u)    ; the power law flux added some fraction of the line flux 
         F01L_u = TSUM(u01L)                              ; the trapezoidal integration over curve
         g01L   = g01 + Rz_g * Lfrac(ll)*lines(Lent_g)   
         F01L_g = TSUM(g01L)                   
         r01L   = r01 + Rz_r * Lfrac(ll)*lines(Lent_r)   
         F01L_r = TSUM(r01L)                   
         i01L   = i01 + Rz_i * Lfrac(ll)*lines(Lent_i)   
         F01L_i = TSUM(i01L)                   
         z01L   = z01 + Rz_z * Lfrac(ll)*lines(Lent_z)   
         F01L_z = TSUM(z01L)    

         ;if strtrim(coef[0],2) eq PP[0] and strtrim(coef[1],2) eq PP[1] and Lfrac(ll) eq 1.0 then stop

         printf,44,zobs,coef[0],coef[1],Lfrac(ll),F_utot,F_gtot,F_rtot,F_itot,F_ztot,F01_u,F01_g,F01_r,F01_i,F01_z,F01L_u,F01L_g,F01L_r,F01L_i,F01L_z
; --     illustrating/plotting the individual power laws --
;          if nn eq 0 and mm eq 0 then plot,lamz(Lent_g),PWRLAW(lamz(Lent_g),coef),/xlog,/ylog
;          if nn ne 0 then oplot,lamz(Lent_g),PWRLAW(lamz(Lent_g),coef)
;          wait,0.05
      endfor
   endfor
endfor

close,44
;goto,here

; reading the observed gVSgr slopes and estimating the slope at the given z
if OBS eq 1 then begin
   SHORTfile='meanSHORTtermslopes_grXax_zbin30DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010_seasonslopes.dat'
   LONGfile='meanLONGtermslopes_grXax_zbin30DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010_seasonslopes.dat'

   readcol,SHORTfile,zcS,meanS,stndS,medS,p68S,m68S,p95S,m95S,comment='#'
   readcol,LONGfile,zcL,meanL,stndL,medL,p68L,m68L,p95L,m95L,comment='#'

   zsplinex = findgen(200)/100.+0.4

   betaSobs = spline(zcS,medS,zsplinex)
   betaLobs = spline(zcL,medL,zsplinex)

   zobsENT = where(zsplinex eq zobs)

   if zobsENT ne [-1] then begin
      betaSobs = betaSobs(zobsENT)
      betaLobs = betaLobs(zobsENT)
   endif else begin
      if vb eq 1 then print,':: compspec_mockobs.pro :: No spline interpolated observed slope at z = '+strtrim(zobs,2)+'  -> no obs-slope plotted'
   endelse
endif


readcol,mockobs,zin,Fin,Ain,Lfr,uinTOT,ginTOT,rinTOT,iinTOT,zinTOT,uin,gin,rin,iin,zin,uinL,ginL,rinL,iinL,zinL,comment='#',/silent   ; reading data for plotting from file written above

Nw = 0
;=============================================================================================
;= = = spectrum = = =
!p.multi = [0,0,0]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/spectrum.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'spectrum'
   thickall = 2
endelse
; setting plot range
XR = [min(lamz),max(lamz)]
YR = [0.1,50]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]


ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,lamz,flux, col=col.black    $
        , /NODATA $
;        , xtitle =textoidl('\lambda [\AA]') $
        , xtitle =textoidl('\lambda')+' [A]' $
        , ytitle ='flux [arbitraty units]' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog, /ylog $
        , xticks = nticks-1 , xtickv=ticks $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white



;oplot,lam,F0*lam^(alpha1),col=col.black,linestyle=2,thick=thickall

;overplotting sdss filters
ysc = 50  ; (abitrary) scaling factor
oplot,lam_u,respt_u*ysc,col=col.blue,thick=thickall
oplot,lam_g,respt_g*ysc,col=col.darkgreen,thick=thickall
oplot,lam_r,respt_r*ysc,col=col.gold,thick=thickall
oplot,lam_i,respt_i*ysc,col=col.orange,thick=thickall
oplot,lam_z,respt_z*ysc,col=col.red,thick=thickall

oplot,lamz,flux,col=col.black,thick=thickall

; overplotting the lines
;oplot,lam,lines,col=col.gray,thick=thickall

XYOUTS,XR[0]+DX*0.01,YR[0]+DY*0.60,'z = '+trim(zobs),col=col.black,charthick=thickall,charsize=2.5

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif

Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;= = = redshifted spectrum = = =
!p.multi = [0,0,0]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/spectrum_z'+strtrim(zobs,2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=20;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=800, ysize=500, title = 'spectrum redshifted'
   thickall = 2
endelse
; setting plot range
XR = [min(lamz),max(lamz)]
YR = [0.1,50]
;YR = [0.5,30]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
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


if GRS eq 1 then begin  ; plotting line for a given 'form of variability' at the chosen redshift 

   Nmodels = NF0*Nalpha*Nlinefrac
   for pp=0,Nmodels-1 do begin
      oplot,lamz,Fin(pp)*lam^(Ain(pp))+Lfr(pp)*lines,col=col.gray,linestyle=0,thick=thickall
   endfor

   ent = [0,12,24]
   ;oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.olive,thick=thickall   
   ;oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.olive,thick=thickall   
   ;oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.olive,thick=thickall   
   ent = [1,13,25]
   ;oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.olive,thick=thickall   
   ;oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.olive,thick=thickall   
   ;oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.olive,thick=thickall   
   ent = [2,14,26]
   ;oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.olive,thick=thickall   
   ;oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.olive,thick=thickall   
   ;oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.olive,thick=thickall   
   ent = [7,19,31]
   oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.olive,thick=thickall   
   oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.olive,thick=thickall   
   oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.olive,thick=thickall   



   ent = [0,1,2,3]+0*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   ;oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ;oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ;oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ;oplot,lamz,Fin(ent(3))*lam^(Ain(ent(3)))+Lfr(ent(3))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ent = [0,1,2,3]+1*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   ;oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ;oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ;oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ;oplot,lamz,Fin(ent(3))*lam^(Ain(ent(3)))+Lfr(ent(3))*lines,linestyle=0,col=col.orchid,thick=thickall   
   ent = [0,1,2,3]+2*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.orchid,thick=thickall   
   oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.orchid,thick=thickall   
   oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.orchid,thick=thickall   
   oplot,lamz,Fin(ent(3))*lam^(Ain(ent(3)))+Lfr(ent(3))*lines,linestyle=0,col=col.orchid,thick=thickall   

   ent = [0,4,8]
   ;oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.cyan,thick=thickall   
   ;oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.cyan,thick=thickall   
   ;oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.cyan,thick=thickall   
   ent = [1,5,9]+3*4
   oplot,lamz,Fin(ent(0))*lam^(Ain(ent(0)))+Lfr(ent(0))*lines,linestyle=0,col=col.cyan,thick=thickall   
   oplot,lamz,Fin(ent(1))*lam^(Ain(ent(1)))+Lfr(ent(1))*lines,linestyle=0,col=col.cyan,thick=thickall   
   oplot,lamz,Fin(ent(2))*lam^(Ain(ent(2)))+Lfr(ent(2))*lines,linestyle=0,col=col.cyan,thick=thickall   

   ;XYOUTS,DX*0.90+XR[0],DY*0.80+YR[0],'Models:',col=col.gray,charsize=1.5,charthick=thickall,alignment=1.0
   ;XYOUTS,DX*0.90+XR[0],DY*0.60+YR[0],'F0 fixed, alpha fixed and Lfrac=[0.0,0.5,1.0,1.5]',col=col.orchid,charsize=1.5,charthick=thickall,alignment=1.0
   ;XYOUTS,DX*0.90+XR[0],DY*0.45+YR[0],'F0 varies, alpha fixed and Lfrac fixed',col=col.olive,charsize=1.5,charthick=thickall,alignment=1.0
   ;XYOUTS,DX*0.90+XR[0],DY*0.35+YR[0],'F0 varies, alpha varies and Lfrac fixed',col=col.cyan,charsize=1.5,charthick=thickall,alignment=1.0
endif

oplot,lamz,F0*lam^(alpha1),col=col.black,linestyle=2,thick=thickall
;overplotting vertical line through pivot point
oplot,[xpiv,xpiv],[0.0001,1000],col=col.black,thick=thickall,linestyle=2

if GRS ne 1 then begin
   ;overplotting sdss filters
   ysc = 50  ; (abitrary) scaling factor
   oplot,lam_u,respt_u*ysc,col=col.blue,thick=thickall,linestyle=2
   oplot,lam_g,respt_g*ysc,col=col.green,thick=thickall,linestyle=2
   oplot,lam_r,respt_r*ysc,col=col.yellow,thick=thickall,linestyle=2
   oplot,lam_i,respt_i*ysc,col=col.orange,thick=thickall,linestyle=2
   oplot,lam_z,respt_z*ysc,col=col.red,thick=thickall,linestyle=2

   oplot,lamz(Lent_u),Rz_u*ysc,col=col.blue,thick=thickall,linestyle=0
   oplot,lamz(Lent_g),Rz_g*ysc,col=col.green,thick=thickall,linestyle=0
   oplot,lamz(Lent_r),Rz_r*ysc,col=col.yellow,thick=thickall,linestyle=0
   oplot,lamz(Lent_i),Rz_i*ysc,col=col.orange,thick=thickall,linestyle=0
   oplot,lamz(Lent_z),Rz_z*ysc,col=col.red,thick=thickall,linestyle=0
endif

; overplotting the lines
oplot,lamz,lines,col=col.charcoal,thick=thickall
; overplotting the spectrum
oplot,lamz,flux,col=col.black,thick=thickall



;XYOUTS,XR[0]+DX*0.01,YR[0]+DY*0.5,'z = '+strtrim(zobs,2),col=col.black,charthick=thickall,charsize=2

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;= = = g VS gr mock = = =
!p.multi = [0,0,0]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/gVSgr_compspec_mockobs'+strtrim(zobs,2)+'.eps'     ; name of eps
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'g vs gr of continuum data'
   thickall = 2
endelse
; setting plot range
;XR = [min(alog10(gin)-alog10(rin)),max(alog10(gin)-alog10(rin))]
;YR = [max(alog10(gin)),min(alog10(gin))]
XR = [-1.5,1.5]
YR = [-4,-11]


DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
        , /NODATA $
        , xtitle ='(-2.5)*log(Flux_g)-(-2.5)*log(Flux_r) ; mockobs ' $
        , ytitle ='(-2.5)*log(Flux_g) ; mockobs ' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , /xlog, /ylog $
        , background = col.white

if GRS eq 1 then begin  ; plotting line for a given 'form of variability' at the chosen redshift 
   ent = [0,4,8]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.cyan,thick=thickall   

   ent = [0,4,8]+3*4
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.cyan,thick=thickall   

   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'F0 varies, alpha varies and Lfrac fixed',col=col.cyan,charsize=1.5,charthick=thickall

   ent = [0,12,24]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.olive,thick=thickall   

   ent = [1,13,25]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.olive,thick=thickall   

   ent = [2,14,26]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.olive,thick=thickall   

   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'F0 varies, alpha fixed and Lfrac fixed',col=col.olive,charsize=1.5,charthick=thickall

   ent = [0,1,2,3]+0*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.orchid,thick=thickall   

   ent = [0,1,2,3]+1*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.orchid,thick=thickall   

   ent = [0,1,2,3]+2*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.orchid,thick=thickall   

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'F0 fixed, alpha fixed and Lfrac=[0.0,0.5,1.0,1.5]',col=col.orchid,charsize=1.5,charthick=thickall
endif

PLOTSYM,0,0.7,/fill
oplot,(-2.5)*alog10(ginL)-(-2.5)*alog10(rinL),(-2.5)*alog10(ginL),psym=8,col=col.gray
oplot,(-2.5)*alog10(gin)-(-2.5)*alog10(rin),(-2.5)*alog10(gin),psym=8,col=col.black

entA = where(Ain eq alpha1)
oplot,(-2.5)*alog10(gin(entA))-(-2.5)*alog10(rin(entA)),(-2.5)*alog10(gin(entA)),psym=8,col=col.green

entF = where(Fin eq F0)
if entF ne [-1] then oplot,(-2.5)*alog10(gin(entF))-(-2.5)*alog10(rin(entF)),(-2.5)*alog10(gin(entF)),psym=8,col=col.red

; overplotting the true value from spectrum
PLOTSYM,3,2.0,/FILL
oplot,(-2.5)*alog10(ginTOT)-(-2.5)*alog10(rinTOT),(-2.5)*alog10(ginTOT),psym=8,col=col.magenta

XYOUTS,DX*0.60+XR[0],DY*0.05+YR[0],'F0 varies, alpha fixed',col=col.green,charsize=1.5,charthick=thickall
XYOUTS,DX*0.60+XR[0],DY*0.10+YR[0],'g VS g-r from spectrum',col=col.magenta,charsize=1.5,charthick=thickall
if entF ne [-1] then XYOUTS,DX*0.60+XR[0],DY*0.15+YR[0],'alpha varies, F0 fixed',col=col.red,charsize=1.5,charthick=thickall

; overplotting observed slopes
if OBS eq 1 then begin
   if zobsENT ne [-1] then begin
      GRvals = findgen(100)/250.-0.2
      oplot,GRvals,betaSobs#GRvals-9,col=col.blue,thick=thickall
      oplot,GRvals,-9+betaLobs#GRvals,col=col.navy,thick=thickall

      XYOUTS,DX*0.35+XR[0],DY*0.95+YR[0],'Observed short term slope',col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,DX*0.35+XR[0],DY*0.90+YR[0],'Observed long term slope',col=col.navy,charsize=1.5,charthick=thickall
   endif
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;= = = g VS r mock = = =
!p.multi = [0,0,0]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/gVSr_compspec_mockobs'+strtrim(zobs,2)+'.eps'     ; name of eps
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'g vs r of continuum data'
   thickall = 2
endelse
; setting plot range
XR = [-4,-11]
YR = [-4,-11]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
        , /NODATA $
        , xtitle ='(-2.5)*log(Flux_g) ; mockobs ' $
        , ytitle ='(-2.5)*log(Flux_r) ; mockobs ' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , /xlog, /ylog $
        , background = col.white
;xxx
if GRS eq 1 then begin  ; plotting line for a given 'form of variability' at the chosen redshift 
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ent = [0,4,8]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.cyan,thick=thickall   
   result1 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   ent = [0,4,8]+3*4
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.cyan,thick=thickall   
   result2 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'F0 varies, alpha varies and Lfrac fixed',col=col.cyan,charsize=1.5,charthick=thickall
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ent = [0,12,24]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.olive,thick=thickall   
   result3 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   ent = [1,13,25]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.olive,thick=thickall   
   result4 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   ent = [2,14,26]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.olive,thick=thickall   
   result5 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'F0 varies, alpha fixed and Lfrac fixed',col=col.olive,charsize=1.5,charthick=thickall
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ent = [0,1,2,3]+0*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.orchid,thick=thickall   
   result6 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   ent = [0,1,2,3]+1*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.orchid,thick=thickall   
   result7 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   ent = [0,1,2,3]+2*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.orchid,thick=thickall   
   result8 = MPFITFUN('LINE',(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),fltarr(3)+1.,[1,0],/QUIET)

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'F0 fixed, alpha fixed and Lfrac=[0.0,0.5,1.0,1.5]',col=col.orchid,charsize=1.5,charthick=thickall
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ; estimating slopes of 'variability' lines
   SlopeLfrac  = (result6(0)+result7(0)+result8(0) ) / 3.0
   SlopeF0andA = (result1(0)+result2(0) ) / 2.0
   SlopeF0frac = (result3(0)+result4(0)+result5(0) ) / 3.0

endif

PLOTSYM,0,0.7,/fill
oplot,(-2.5)*alog10(ginL),(-2.5)*alog10(rinL),psym=8,col=col.gray
oplot,(-2.5)*alog10(gin),(-2.5)*alog10(rin),psym=8,col=col.black

entA = where(Ain eq alpha1)
oplot,(-2.5)*alog10(gin(entA)),(-2.5)*alog10(rin(entA)),psym=8,col=col.green

entF = where(Fin eq F0)
if entF ne [-1] then oplot,(-2.5)*alog10(gin(entF)),(-2.5)*alog10(rin(entF)),psym=8,col=col.red

oplot,findgen(100)-50,findgen(100)-50,col=col.blue,thick=thickall

; overplotting the true value from spectrum
PLOTSYM,3,2.0,/FILL
oplot,(-2.5)*alog10(ginTOT),(-2.5)*alog10(rinTOT),psym=8,col=col.magenta

if entF ne [-1] then XYOUTS,DX*0.60+XR[0],DY*0.05+YR[0],'alpha varies, F0 fixed',col=col.red,charsize=1.5,charthick=thickall
XYOUTS,DX*0.60+XR[0],DY*0.10+YR[0],'F0 varies, alpha fixed',col=col.green,charsize=1.5,charthick=thickall
XYOUTS,DX*0.60+XR[0],DY*0.15+YR[0],'r VS g from spectrum',col=col.magenta,charsize=1.5,charthick=thickall
XYOUTS,DX*0.60+XR[0],DY*0.20+YR[0],'x=x Line',col=col.blue,charsize=1.5,charthick=thickall

if Nalpha gt 5 and NF0 gt 5 then begin
;--------------------------------------------------------------------------------------
; plotting zoom-in insert
XR = [max((-2.5)*alog10(gin(entA))),min((-2.5)*alog10(gin(entA)))]
YR = [max((-2.5)*alog10(rin(entA))),min((-2.5)*alog10(rin(entA)))]

; plotting the zoom-in box in big plot
oplot,[XR[0],XR[0]],[YR[0],YR[1]],col=col.black,linestyle=2,thick=thickall
oplot,[XR[0],XR[1]],[YR[1],YR[1]],col=col.black,linestyle=2,thick=thickall
oplot,[XR[0],XR[1]],[YR[0],YR[0]],col=col.black,linestyle=2,thick=thickall
oplot,[XR[1],XR[1]],[YR[0],YR[1]],col=col.black,linestyle=2,thick=thickall

plot,lam,flux, col=col.black    $
        , /NODATA $
        , /noerase $
;        , xtitle ='log(Flux_g) ; mockobs ' $
;        , ytitle ='log(Flux_r) ; mockobs ' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 1.5 $
        , charthick = thickall $
        , yminor = 2 $
        , position = [0.27,0.62,0.55,0.9] $
;        , /xlog, /ylog $
        , background = col.white

PLOTSYM,0,0.7,/fill
oplot,(-2.5)*alog10(gin(entA)),(-2.5)*alog10(rin(entA)),psym=8,col=col.green

if entF ne [-1] then oplot,(-2.5)*alog10(gin(entF)),(-2.5)*alog10(rin(entF)),psym=8,col=col.red

oplot,findgen(100)-50,findgen(100)-50,col=col.blue,thick=thickall

;--------------------------------------------------------------------------------------
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;                           PLOTTING GR SLOPES IF REQUESTED
;=============================================================================================
if GRS eq 1 then begin
restore,'GRslopeVSz.idlsave'  ; restoring values to plot. Returns the vectors redshift, GRslope, zsort and slopemeanz

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/gVSr_observed_slopes'+strtrim(zobs,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'MCMC fitted slopes'
   thickall = 2
endelse
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('m_{gr}') $
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

oplot,zsort,slopemeanz,col=col.red,thick=thickall

oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one

XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('z_{obs} slope = ')+strtrim(slopemeanz(mindif),2),col=col.charcoal,charsize=1.5,charthick=thickall,alignment=1.0

PLOTSYM,0,1.5,/fill
oplot,fltarr(2)+ZOBS,fltarr(2)+SlopeLfrac  ,psym=8,thick=thickall,col=col.orchid
oplot,fltarr(2)+ZOBS,fltarr(2)+SlopeF0andA ,psym=8,thick=thickall,col=col.cyan
oplot,fltarr(2)+ZOBS,fltarr(2)+SlopeF0frac ,psym=8,thick=thickall,col=col.olive
oplot,fltarr(2)+ZOBS,fltarr(2)+1.0         ,psym=8,thick=thickall,col=col.blue

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
endif
;=============================================================================================

;=============================================================================================
;                           PLOTTING GR SLOPES IF REQUESTED
;=============================================================================================
if GRS eq 1 then begin
restore,'GRslopeVSz.idlsave'  ; restoring values to plot. Returns the vectors redshift, GRslope, zsort and slopemeanz

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/gVSr_observed_slopes'+strtrim(zobs,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'MCMC fitted slopes'
   thickall = 2
endelse
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle ='(-2.5)*log(Flux_g)' $
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



XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('z_{obs} slope = ')+strtrim(slopemeanz(mindif),2),col=col.charcoal,charsize=1.5,charthick=thickall,alignment=1.0

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
endif
;=============================================================================================


;=============================================================================================
;                           CREATING MULTIPLOT IF REQUESTED
;=============================================================================================
if MPLOT eq 1 then begin
!p.multi = [0,2,2]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/compspec_mockobs_multiplot'+strtrim(zobs,2)+'.eps'     ; name of eps
   device,  file=plot1 ,/color , /encapsulated, xsize=45, ysize=45;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=900, ysize=800, title = 'multiplot continuum data'
   thickall = 2
endelse

; setting plot range
XR = [min(lamz),max(lamz)]
YR = [0.1,50]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
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

; overplotting the lines
oplot,lamz,lines,col=col.gray,thick=thickall
; overplotting the spectrum
oplot,lamz,flux,col=col.black,thick=thickall

oplot,lamz,F0*lam^(alpha1),col=col.black,linestyle=2,thick=thickall
;overplotting vertical line through pivot point
oplot,[xpiv,xpiv],[0.0001,1000],col=col.black,thick=thickall,linestyle=2

;overplotting sdss filters
ysc = 50  ; (abitrary) scaling factor
oplot,lam_u,respt_u*ysc,col=col.blue,thick=thickall,linestyle=2
oplot,lam_g,respt_g*ysc,col=col.green,thick=thickall,linestyle=2
oplot,lam_r,respt_r*ysc,col=col.yellow,thick=thickall,linestyle=2
oplot,lam_i,respt_i*ysc,col=col.orange,thick=thickall,linestyle=2
oplot,lam_z,respt_z*ysc,col=col.red,thick=thickall,linestyle=2

oplot,lamz(Lent_u),Rz_u*ysc,col=col.blue,thick=thickall,linestyle=0
oplot,lamz(Lent_g),Rz_g*ysc,col=col.green,thick=thickall,linestyle=0
oplot,lamz(Lent_r),Rz_r*ysc,col=col.yellow,thick=thickall,linestyle=0
oplot,lamz(Lent_i),Rz_i*ysc,col=col.orange,thick=thickall,linestyle=0
oplot,lamz(Lent_z),Rz_z*ysc,col=col.red,thick=thickall,linestyle=0


XYOUTS,XR[0]+DX*0.01,YR[0]+DY*0.70,textoidl('\lambda_{pivot}')+' = '+strtrim(xpiv,2),col=col.black,charthick=thickall,charsize=2
XYOUTS,XR[0]+DX*0.01,YR[0]+DY*0.5,'z = '+strtrim(zobs,2),col=col.black,charthick=thickall,charsize=2

;XR = [-1.5,1.5]
XR = [-0.5,0.3]
YR = [-3,-11]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
        , /NODATA $
        , xtitle ='-2.5*log(Flux_g)-(-2.5)*log(Flux_r) ; mockobs ' $
        , ytitle ='-2.5*log(Flux_g) ; mockobs ' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , /xlog, /ylog $
        , background = col.white

if GRS eq 1 then begin  ; plotting line for a given 'form of variability' at the chosen redshift 
   ent = [0,4,8]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.cyan,thick=thickall   

   ent = [0,4,8]+3*4
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.cyan,thick=thickall   

   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'F0 varies, alpha varies and Lfrac fixed',col=col.cyan,charsize=1.5,charthick=thickall

   ent = [0,12,24]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.olive,thick=thickall   

   ent = [1,13,25]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.olive,thick=thickall   

   ent = [2,14,26]
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.olive,thick=thickall   

   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'F0 varies, alpha fixed and Lfrac fixed',col=col.olive,charsize=1.5,charthick=thickall

   ent = [0,1,2,3]+0*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.orchid,thick=thickall   

   ent = [0,1,2,3]+1*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.orchid,thick=thickall   

   ent = [0,1,2,3]+2*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent))-(-2.5)*alog10(rinL(ent)),(-2.5)*alog10(ginL(ent)),col=col.orchid,thick=thickall   

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'F0 fixed, alpha fixed and Lfrac=[0.0,0.5,1.0,1.5]',col=col.orchid,charsize=1.5,charthick=thickall
endif

PLOTSYM,0,0.7,/fill
oplot,(-2.5)*alog10(ginL)-(-2.5)*alog10(rinL),(-2.5)*alog10(ginL),psym=8,col=col.gray
oplot,(-2.5)*alog10(gin)-(-2.5)*alog10(rin),(-2.5)*alog10(gin),psym=8,col=col.black

entA = where(Ain eq alpha1)
oplot,(-2.5)*alog10(gin(entA))-(-2.5)*alog10(rin(entA)),(-2.5)*alog10(gin(entA)),psym=8,col=col.green

entF = where(Fin eq F0)
;if entF ne [-1] then oplot,(-2.5)*alog10(gin(entF))-(-2.5)*alog10(rin(entF)),(-2.5)*alog10(gin(entF)),psym=8,col=col.red

; overplotting the true value from spectrum
PLOTSYM,3,2.0,/FILL
oplot,(-2.5)*alog10(ginTOT)-(-2.5)*alog10(rinTOT),(-2.5)*alog10(ginTOT),psym=8,col=col.magenta

XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'F0 varies, alpha fixed',col=col.green,charsize=1.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'g VS g-r from spectrum',col=col.magenta,charsize=1.5,charthick=thickall,alignment=1.0
;if entF ne [-1] then XYOUTS,DX*0.40+XR[0],DY*0.15+YR[0],'alpha varies, F0 fixed',col=col.red,charsize=1.5,charthick=thickall

; overplotting observed slopes
if OBS eq 1 then begin
   if zobsENT ne [-1] then begin
      GRvals = findgen(100)/250.-0.2
      oplot,GRvals,betaSobs#GRvals-9,col=col.blue,thick=thickall
      oplot,GRvals,-9+betaLobs#GRvals,col=col.navy,thick=thickall

      XYOUTS,DX*0.35+XR[0],DY*0.95+YR[0],'Observed short term slope',col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,DX*0.35+XR[0],DY*0.90+YR[0],'Observed long term slope',col=col.navy,charsize=1.5,charthick=thickall
   endif
endif


if GRS eq 1 then begin
restore,'GRslopeVSz.idlsave'  ; restoring values to plot. Returns the vectors redshift, GRslope, zsort and slopemeanz
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle ='gr slope' $
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
oplot,zsort,slopemeanz,col=col.red,thick=thickall
oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0
XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('z_{obs} slope = ')+strtrim(slopemeanz(mindif),2),col=col.charcoal,charsize=1.5,charthick=thickall,alignment=1.0

PLOTSYM,0,1.5,/fill
oplot,fltarr(2)+ZOBS,fltarr(2)+SlopeLfrac  ,psym=8,thick=thickall,col=col.orchid
oplot,fltarr(2)+ZOBS,fltarr(2)+SlopeF0andA ,psym=8,thick=thickall,col=col.cyan
oplot,fltarr(2)+ZOBS,fltarr(2)+SlopeF0frac ,psym=8,thick=thickall,col=col.olive
oplot,fltarr(2)+ZOBS,fltarr(2)+1.0         ,psym=8,thick=thickall,col=col.blue
endif

restore,'davismodels_GRslopeprofiles.idlsave'   ; getting Knecht version of Davis model gr slopes; is in the array ModSloPro

PLOTSYM,0,1.5,thick=thickall
oplot,ModSloPro(0,*),ModSloPro(4,*)  ,Psym=8,thick=thickall,col=col.green
oplot,ModSloPro(0,*),ModSloPro(5,*)  ,Psym=8,thick=thickall,col=col.red
oplot,ModSloPro(0,*),ModSloPro(6,*)  ,Psym=8,thick=thickall,col=col.orange

; setting plot range
XR = [-4,-11]
YR = [-4,-11]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
        , /NODATA $
        , xtitle ='-2.5*log(Flux_g) ; mockobs ' $
        , ytitle ='-2.5*log(Flux_r) ; mockobs ' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , /xlog, /ylog $
        , background = col.white

if GRS eq 1 then begin  ; plotting line for a given 'form of variability' at the chosen redshift 
   ent = [0,4,8]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.cyan,thick=thickall   
   ent = [0,4,8]+3*4
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.cyan,thick=thickall   
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'F0 varies, alpha varies and Lfrac fixed',col=col.cyan,charsize=1.5,charthick=thickall

   ent = [0,12,24]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.olive,thick=thickall   
   ent = [1,13,25]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.olive,thick=thickall   
   ent = [2,14,26]
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.olive,thick=thickall   
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'F0 varies, alpha fixed and Lfrac fixed',col=col.olive,charsize=1.5,charthick=thickall

   ent = [0,1,2,3]+0*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.orchid,thick=thickall   
   ent = [0,1,2,3]+1*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.orchid,thick=thickall   
   ent = [0,1,2,3]+2*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   oplot,(-2.5)*alog10(ginL(ent)),(-2.5)*alog10(rinL(ent)),col=col.orchid,thick=thickall   
   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'F0 fixed, alpha fixed and Lfrac=[0.0,0.5,1.0,1.5]',col=col.orchid,charsize=1.5,charthick=thickall

   xvec = findgen(30)-15.
   yvec = slopemeanz(mindif)#(xvec+7.5)-7.5
   oplot,xvec,yvec,thick=thickall,linestyle=2,col=col.charcoal
   XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],'line through (-7.5,-7.5) with obs. gr slope',col=col.charcoal,charsize=1.5,charthick=thickall
endif

PLOTSYM,0,0.7,/fill
oplot,(-2.5)*alog10(ginL),(-2.5)*alog10(rinL),psym=8,col=col.gray
oplot,(-2.5)*alog10(gin),(-2.5)*alog10(rin),psym=8,col=col.black

entA = where(Ain eq alpha1)
oplot,(-2.5)*alog10(gin(entA)),(-2.5)*alog10(rin(entA)),psym=8,col=col.green

entF = where(Fin eq F0)
;if entF ne [-1] then oplot,(-2.5)*alog10(gin(entF)),(-2.5)*alog10(rin(entF)),psym=8,col=col.red

oplot,findgen(100)-50,findgen(100)-50,col=col.blue,thick=thickall

; overplotting the true value from spectrum
PLOTSYM,3,2.0,/FILL
oplot,(-2.5)*alog10(ginTOT),(-2.5)*alog10(rinTOT),psym=8,col=col.magenta

XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'F0 varies, alpha fixed',col=col.green,charsize=1.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'r VS g from spectrum',col=col.magenta,charsize=1.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.15+YR[0],'x=x Line',col=col.blue,charsize=1.5,charthick=thickall,alignment=1.0
;if entF ne [-1] then XYOUTS,DX*0.40+XR[0],DY*0.20+YR[0],'alpha varies, F0 fixed',col=col.red,charsize=1.5,charthick=thickall



if Nalpha gt 5 and NF0 gt 5 then begin
;--------------------------------------------------------------------------------------
; plotting zoom-in insert
XR = [max((-2.5)*alog10(gin(entA))),min((-2.5)*alog10(gin(entA)))]
YR = [max((-2.5)*alog10(rin(entA))),min((-2.5)*alog10(rin(entA)))]


; plotting the zoom-in box in big plot
oplot,[XR[0],XR[0]],[YR[0],YR[1]],col=col.black,linestyle=2,thick=thickall
oplot,[XR[0],XR[1]],[YR[1],YR[1]],col=col.black,linestyle=2,thick=thickall
oplot,[XR[0],XR[1]],[YR[0],YR[0]],col=col.black,linestyle=2,thick=thickall
oplot,[XR[1],XR[1]],[YR[0],YR[1]],col=col.black,linestyle=2,thick=thickall

plot,lam,flux, col=col.black    $
        , /NODATA $
        , /noerase $
;        , xtitle ='log(Flux_g) ; mockobs ' $
;        , ytitle ='log(Flux_r) ; mockobs ' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 1.5 $
        , charthick = thickall $
        , yminor = 2 $
        , position = [0.75,0.62,0.85,0.9] $
;        , /xlog, /ylog $
        , background = col.white

PLOTSYM,0,0.7,/fill
oplot,(-2.5)*alog10(gin(entA)),(-2.5)*alog10(rin(entA)),psym=8,col=col.green

if entF ne [-1] then oplot,(-2.5)*alog10(gin(entF)),(-2.5)*alog10(rin(entF)),psym=8,col=col.red

oplot,findgen(100)-50,findgen(100)-50,col=col.blue,thick=thickall
;--------------------------------------------------------------------------------------
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endif                           ; end of MPLOT multi statement
;=============================================================================================

here:
zs           = zobs
Nfiles       = n_elements(zs)
Nheaderlines = 13                                      ; number of header lines
Ncol         = 19                                      ; number of columns in 
Nrows        = 49                                      ; number of rows in files

ALLDATA      = FLTARR(Ncol,Nrows-Nheaderlines,Nfiles)  ; array to read file into
header       = STRARR(Nheaderlines)                    ; string array for header

for ii=0,n_elements(zs)-1 do begin
   filename = mockobs
   openr,lun,filename, /GET_LUN                        ; open file for reading     
   readf,lun,header                                    ; reading header into string array (overwriting)
   DATAARR = FLTARR(Ncol,Nrows-Nheaderlines)
   readf,lun,DATAARR                                    ; reading data into array
   ALLDATA(*,*,ii) = DATAARR
   free_lun,lun  
endfor

Fu_vdb  = ALLDATA(4,18,*)   ; the total flux of the Vanden Berk spectrum in the u band
Fg_vdb  = ALLDATA(5,18,*)   ; the total flux of the Vanden Berk spectrum in the g band
Fr_vdb  = ALLDATA(6,18,*)   ; the total flux of the Vanden Berk spectrum in the r band
Fi_vdb  = ALLDATA(7,18,*)   ; the total flux of the Vanden Berk spectrum in the i band
Fz_vdb  = ALLDATA(8,18,*)   ; the total flux of the Vanden Berk spectrum in the z band

Fu_cont = ALLDATA(9,18,*)   ; the continuum flux of Vanden Berk spectrum in the u band
Fg_cont = ALLDATA(10,18,*)  ; the continuum flux of Vanden Berk spectrum in the g band
Fr_cont = ALLDATA(11,18,*)  ; the continuum flux of Vanden Berk spectrum in the r band
Fi_cont = ALLDATA(12,18,*)  ; the continuum flux of Vanden Berk spectrum in the i band
Fz_cont = ALLDATA(13,18,*)  ; the continuum flux of Vanden Berk spectrum in the z band

Fu_line = Fu_vdb-Fu_cont    ; the emission line flux of Vanden Berk spectrum in the u band
Fg_line = Fg_vdb-Fg_cont    ; the emission line flux of Vanden Berk spectrum in the g band
Fr_line = Fr_vdb-Fr_cont    ; the emission line flux of Vanden Berk spectrum in the r band
Fi_line = Fi_vdb-Fi_cont    ; the emission line flux of Vanden Berk spectrum in the i band
Fz_line = Fz_vdb-Fz_cont    ; the emission line flux of Vanden Berk spectrum in the z band

; vectors with continuum fluxes
FuC     = ALLDATA(9,*,0)
FgC     = ALLDATA(10,*,0)
FrC     = ALLDATA(11,*,0)
FiC     = ALLDATA(12,*,0)
FzC     = ALLDATA(13,*,0)
; vectors with continuum+line fluxes
FuCL    = ALLDATA(14,*,0)
FgCL    = ALLDATA(15,*,0)
FrCL    = ALLDATA(16,*,0)
FiCL    = ALLDATA(17,*,0)
FzCL    = ALLDATA(18,*,0)
; vectors with line fluxes
FuL     = FuCL-FuC
FgL     = FgCL-FgC
FrL     = FrCL-FrC
FiL     = FiCL-FiC
FzL     = FzCL-FzC


Nw =1 
;=============================================================================================
;                                    MULTIPLOT NUMBER 2
;=============================================================================================
restore,'GRslopeVSzNO2.idlsave'  ; restoring values to plot. Returns the vectors redshift, GRslope, zsort and slopemeanz

!p.multi = [0,2,2]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'compspecRUN/multiplot2_'+strtrim(zobs,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=45, ysize=45;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=900, ysize=800, title = 'multiplot no 2'
   thickall = 2
endelse


;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(lamz),max(lamz)]
YR = [0.1,50]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,lam,flux, col=col.black    $
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

; overplotting the lines
oplot,lamz,lines,col=col.gray,thick=thickall
; overplotting the spectrum
oplot,lamz,flux,col=col.black,thick=thickall

oplot,lamz,F0*lam^(alpha1),col=col.black,linestyle=2,thick=thickall

;overplotting sdss filters
ysc = 50  ; (abitrary) scaling factor
oplot,lam_u,respt_u*ysc,col=col.blue,thick=thickall,linestyle=2
oplot,lam_g,respt_g*ysc,col=col.green,thick=thickall,linestyle=2
oplot,lam_r,respt_r*ysc,col=col.yellow,thick=thickall,linestyle=2
oplot,lam_i,respt_i*ysc,col=col.orange,thick=thickall,linestyle=2
oplot,lam_z,respt_z*ysc,col=col.red,thick=thickall,linestyle=2

oplot,lamz(Lent_u),Rz_u*ysc,col=col.blue,thick=thickall,linestyle=0
oplot,lamz(Lent_g),Rz_g*ysc,col=col.green,thick=thickall,linestyle=0
oplot,lamz(Lent_r),Rz_r*ysc,col=col.yellow,thick=thickall,linestyle=0
oplot,lamz(Lent_i),Rz_i*ysc,col=col.orange,thick=thickall,linestyle=0
oplot,lamz(Lent_z),Rz_z*ysc,col=col.red,thick=thickall,linestyle=0

XYOUTS,XR[0]+DX*0.01,YR[0]+DY*0.70,textoidl('z_{"obs"} = ')+trim(zobs),col=col.black,charthick=thickall,charsize=2
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
        , ytitle =textoidl('m_{gr}') $
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

oplot,zsort,slopemeanz,col=col.red,thick=thickall

oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one
zint = ROUND(slopemeanz(mindif)*100.)/100.
XYOUTS,XR[0]+0.95*DX,YR[0]+0.90*DY,textoidl('<m_{gr}> @ z_{"obs"} = ')+trim(zint),col=col.black,charsize=1.5,charthick=thickall,alignment=1.0

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
        , ytitle =textoidl('m_{gr}') $
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

oplot,zsort,slopemeanz,col=col.red,thick=thickall

oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one
zint = ROUND(slopemeanz(mindif)*100.)/100.
XYOUTS,XR[0]+0.05*DX,YR[0]+0.10*DY,textoidl('<m_{gr}> @ z_{"obs"} = ')+trim(zint),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

; plotting model gr slopes (flux ratios)  ... uin,gin,rin,iin,zin,uinL,ginL,rinL,iinL,zinL

dummyadd = 0.6

PLOTSYM,0,2.0,/FILL
oplot,fltarr(2)+zobs,fltarr(2)+Fu_line/Fu_cont+dummyadd,col=col.blue,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+Fi_line/Fi_cont+dummyadd,col=col.orange,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+Fz_line/Fz_cont+dummyadd,col=col.red,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+Fr_line/Fr_cont+dummyadd,col=col.yellow,psym=8,thick=thickall 
oplot,fltarr(2)+zobs,fltarr(2)+Fg_line/Fg_cont+dummyadd,col=col.green,psym=8,thick=thickall 

XYOUTS,XR[0]+0.05*DX,YR[0]+0.05*DY,textoidl('Filled cicles: F_{line}/F_{continuum}+')+trim(dummyadd),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

;------------------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------------------
; setting plot range
XR = [min(redshift)-0.1*(max(redshift)-min(redshift)),max(redshift)+0.1*(max(redshift)-min(redshift))]
YR = [0.0,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('m_{gr}') $
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

oplot,zsort,slopemeanz,col=col.red,thick=thickall

oplot,fltarr(4)+ZOBS,findgen(4),col=col.black,thick=thickall,linestyle=0

zDIFF  = abs(zsort - zobs)
mindif = where(zDIFF eq min(zDIFF))
mindif = mindif(0)  ; in case of multiple entries only take the first one
zint = ROUND(slopemeanz(mindif)*100.)/100.
XYOUTS,XR[0]+0.05*DX,YR[0]+0.10*DY,textoidl('<m_{gr}> @ z_{"obs"} = ')+trim(zint),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

; plotting model gr slopes (flux ratios)  ... uin,gin,rin,iin,zin,uinL,ginL,rinL,iinL,zinL

dummyadd = 0.5




   ; minent    = ent(0)
   ; maxent    = ent(2)
   ; lineslope = alog10(FrL(maxent)/FrL(minent))/alog10(FgL(maxent)/FgL(minent)) 
   ; contslope = alog10(FrC(maxent)/FrC(minent))/alog10(FgC(maxent)/FgC(minent))
   ; CLslope   = alog10(FrCL(maxent)/FrCL(minent))/alog10(FgCL(maxent)/FgCL(minent))

PLOTSYM,0,2.0,/FILL
   ent = [0,4,8]        ; F0 varies, alpha varies and Lfrac fixed
   minent    = ent(0)
   maxent    = ent(2)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrC(maxent)/FrC(minent))/alog10(FgC(maxent)/FgC(minent)),col=col.cyan,thick=thickall,psym=8
   ent = [0,4,8]+3*4    ; F0 varies, alpha varies and Lfrac fixed
   minent    = ent(0)
   maxent    = ent(2)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrC(maxent)/FrC(minent))/alog10(FgC(maxent)/FgC(minent)),col=col.cyan,thick=thickall,psym=8 
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'F0 varies, alpha varies and Lfrac fixed',col=col.cyan,charsize=1.5,charthick=thickall

PLOTSYM,0,2.0,/FILL
   ent = [0,12,24]      ; F0 varies, alpha fixed and Lfrac fixed
   minent    = ent(0)
   maxent    = ent(2)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrC(maxent)/FrC(minent))/alog10(FgC(maxent)/FgC(minent)),col=col.olive,thick=thickall,psym=8
   ent = [1,13,25]      ; F0 varies, alpha fixed and Lfrac fixed
   minent    = ent(0)
   maxent    = ent(2)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrC(maxent)/FrC(minent))/alog10(FgC(maxent)/FgC(minent)),col=col.olive,thick=thickall,psym=8
   ent = [2,14,26]      ; F0 varies, alpha fixed and Lfrac fixed
   minent    = ent(0)
   maxent    = ent(2)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrC(maxent)/FrC(minent))/alog10(FgC(maxent)/FgC(minent)),col=col.olive,thick=thickall,psym=8
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'F0 varies, alpha fixed and Lfrac fixed',col=col.olive,charsize=1.5,charthick=thickall

PLOTSYM,0,2.0,thick=thickall;,/FILL
   ent = [0,1,2,3]+0*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   minent    = ent(1)
   maxent    = ent(3)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrL(maxent)/FrL(minent))/alog10(FgL(maxent)/FgL(minent)) ,col=col.orchid,thick=thickall,psym=8
   ent = [0,1,2,3]+1*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   minent    = ent(1)
   maxent    = ent(3)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrL(maxent)/FrL(minent))/alog10(FgL(maxent)/FgL(minent)) ,col=col.orchid,thick=thickall,psym=8
   ent = [0,1,2,3]+2*4  ; F0 fixed ; alpha fixed ; Lfrac varies
   minent    = ent(1)
   maxent    = ent(3)
   oplot,fltarr(2)+zobs,fltarr(2)+alog10(FrL(maxent)/FrL(minent))/alog10(FgL(maxent)/FgL(minent)) ,col=col.orchid,thick=thickall,psym=8
   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'F0 fixed, alpha fixed and Lfrac=[0.0,0.5,1.0,1.5]',col=col.orchid,charsize=1.5,charthick=thickall

;XYOUTS,XR[0]+0.05*DX,YR[0]+0.05*DY,textoidl('Filled cicles: F_{line}/F_{continuum}+')+trim(dummyadd),col=col.black,charsize=1.5,charthick=thickall,alignment=0.0

;------------------------------------------------------------------------------------------------------










if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

stop












if vb eq 1 then print,' '
if vb eq 1 then print,':: compspec_mockobs.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '

;stop
END
