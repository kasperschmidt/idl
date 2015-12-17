;+
;----------------------------
;   NAME
;----------------------------
; compspec_bandpos.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure plotting the SDSS bands on top of the composit spectrum from Vanden
; Berk et al 2001 to estimate (cont and emission line) flux received for a given redshift range.
;----------------------------
;   COMMENTS
;----------------------------
; zrange            : Two component vector with zmin and zmax of
;                     redshift range to investigate.
;----------------------------
;   INPUTS:
;----------------------------
; zplot             : This keyword set the position of the SDSS
;                     filters in the plotted version of spectrum.
;                     Default is at a redshift half way between
;                     zrange[0] and zrange[1]
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
; IDL> compspec_bandpos,[0.2,5.0],zplot=2.0,/VERBOSE
;
;----------------------------
;   BUGS
;----------------------------
; 
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-07-04  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ LOGLEVELS.pro
;----------------------------
;-
FUNCTION PWRLAW, X, P
  RETURN, P[0]*X^P[1]
END
;----------------------------
PRO compspec_bandpos,zrange,EPS=EPS,VERBOSE=VERBOSE,zplot=zplot

LZP = n_elements(zplot)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)

;----------------------------------------------------------------------------------------------------------------------------------------
; read composite spectrum
compositespec = '/Users/kasperborelloschmidt/work/SDSScompositespectrum.txt'
readcol,compositespec,lam0,flux0,Ferr0,/silent

;----------------------------------------------------------------------------------------------------------------------------------------
; continuum powerlaw for Vanden Berk composite spectrum (between 1200 and 5000 A or so)
alpha1  = -1.528     ; spextral power law index - estimated from the spectral points (1286.50,6.235) and (4203.5,1.021)
F0      = 351572.        ; flux value at 0          - estimated from the spectral points (1286.50,6.235) and (4203.5,1.021)
PP      = [F0,alpha1]
xpiv    = 4*max(lam0)                                ; pivot point to change alpha around x-coordinates
ypiv    = PWRLAW(xpiv,PP)                            ; pivot point to change alpha around y-coordinate
cont    = PWRLAW(lam0,PP)                             ; Vanden Berk continuum
lines   = flux0 - cont                                ; flux subtracted power-law continuum

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

Nz   = 100
zobs = findgen(Nz)/Nz*(zrange[1]-zrange[0])+zrange[0]
Farr_EL = fltarr(5,Nz)  ; array to contain flux values

Lamgood = [1200,7000]
entgood = where(lam0 gt lamgood(0) and lam0 lt lamgood(1))

xx = lam_u(where(respt_u eq max(respt_u)))/lamgood(0)
yy = lam_u(where(respt_u eq max(respt_u)))/lamgood(1)
entgood_zu = where(1.+zobs lt xx(0) and 1.+zobs gt yy(0) )
xx = lam_g(where(respt_g eq max(respt_g)))/lamgood(0)
yy = lam_g(where(respt_g eq max(respt_g)))/lamgood(1)
entgood_zg = where(1.+zobs lt xx(0) and 1.+zobs gt yy(0) )
xx = lam_r(where(respt_r eq max(respt_r)))/lamgood(0)
yy = lam_r(where(respt_r eq max(respt_r)))/lamgood(1)
entgood_zr = where(1.+zobs lt xx(0) and 1.+zobs gt yy(0) )
xx = lam_i(where(respt_i eq max(respt_i)))/lamgood(0)
yy = lam_i(where(respt_i eq max(respt_i)))/lamgood(1)
entgood_zi = where(1.+zobs lt xx(0) and 1.+zobs gt yy(0) )
xx = lam_z(where(respt_z eq max(respt_z)))/lamgood(0)
yy = lam_z(where(respt_z eq max(respt_z)))/lamgood(1)
entgood_zz = where(1.+zobs lt xx(0) and 1.+zobs gt yy(0) )

for ii=0,Nz-1 do begin

   lamz  = lam0*(1.+zobs(ii)) ; the wavelengths at redshift

   Lent_u = where(lamz gt min(lam_u) and lamz lt max(lam_u))   ; the entries of redshifted lambda values in the u band
   Lent_g = where(lamz gt min(lam_g) and lamz lt max(lam_g))   ; the entries of redshifted lambda values in the g band
   Lent_r = where(lamz gt min(lam_r) and lamz lt max(lam_r))   ; the entries of redshifted lambda values in the r band
   Lent_i = where(lamz gt min(lam_i) and lamz lt max(lam_i))   ; the entries of redshifted lambda values in the i band
   Lent_z = where(lamz gt min(lam_z) and lamz lt max(lam_z))   ; the entries of redshifted lambda values in the z band

   if Lent_u eq [-1] then begin        ; if band outside spectral range create dymmy vectors
      Lent_u = findgen(10)             ; vector with 'dummy' entries for calculations 
      resp_uinter   = fltarr(10)*0.0   ; respons array of 0.0-values for calculations
      Farr_EL(0,ii) = 0.0001
   endif else begin                    ; if band inside spectral range interpolate band respons
      resp_uinter  =  SPLINE(lam_u,respt_u,lamz(Lent_u))   ; interpolating the band to spectral wavelength spacing
      Farr_EL(0,ii) = TSUM(resp_uinter*lines(Lent_u))/total(lines(Lent_u))  ; integrated flux in band
   endelse

   if Lent_g eq [-1] then begin        ; if band outside spectral range create dymmy vectors
      Lent_g = findgen(10)             ; vector with 'dummy' entries for calculations 
      resp_ginter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
      Farr_EL(1,ii) = 0.0001
   endif else begin                    ; if band inside spectral range interpolate band respons
      resp_ginter  =  SPLINE(lam_g,respt_g,lamz(Lent_g))   ; interpolating the band to spectral wavelength spacing
      Farr_EL(1,ii) = TSUM(resp_ginter*lines(Lent_g))/total(lines(Lent_g))  ; integrated flux in band
   endelse

   if Lent_r eq [-1] then begin        ; if band outside spectral range create dymmy vectors
      Lent_r = findgen(10)             ; vector with 'dummy' entries for calculations 
      resp_rinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
      Farr_EL(2,ii) = 0.0001
   endif else begin                    ; if band inside spectral range interpolate band respons
      resp_rinter  =  SPLINE(lam_r,respt_r,lamz(Lent_r))   ; interpolating the band to spectral wavelength spacing
      Farr_EL(2,ii) = TSUM(resp_rinter*lines(Lent_r))/total(lines(Lent_r))  ; integrated flux in band
   endelse

   if Lent_i eq [-1] then begin        ; if band outside spectral range create dymmy vectors
      Lent_i = findgen(10)             ; vector with 'dummy' entries for calculations 
      resp_iinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
      Farr_EL(3,ii) = 0.0001
   endif else begin                    ; if band inside spectral range interpolate band respons
      resp_iinter  =  SPLINE(lam_i,respt_i,lamz(Lent_i))   ; interpolating the band to spectral wavelength spacing
      Farr_EL(3,ii) = TSUM(resp_iinter*lines(Lent_i))/total(lines(Lent_i))  ; integrated flux in band
   endelse

   if Lent_z eq [-1] then begin        ; if band outside spectral range create dymmy vectors
      Lent_z = findgen(10)             ; vector with 'dummy' entries for calculations 
      resp_zinter  =  fltarr(10)*0.0   ; respons array of 0.0-values for calculations
      Farr_EL(4,ii) = 0.0001
   endif else begin                    ; if band inside spectral range interpolate band respons
      resp_zinter  =  SPLINE(lam_z,respt_z,lamz(Lent_z))   ; interpolating the band to spectral wavelength spacing
      Farr_EL(4,ii) = TSUM(resp_zinter*lines(Lent_z))/total(lines(Lent_z))  ; integrated flux in band
   endelse

endfor

;----------------------------------------------------------------------------------------------------------------------------------------
; calculating flux over given redshift range


Nw = 0
;=============================================================================================
;= = = spectrum = = =
!p.multi = [0,0,0]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/spectrum.eps' ; name for movie frames
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

if LZP eq 0 then zplot = zobs(round(Nz/2.))

; setting plot range
XR = [min(lam0),max(lam0)]
YR = [0.1,50]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

ticks = LOGLEVELS([XR[0],XR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

;=== PLOTTING LIGHT CURVE(S) ===
plot,fltarr(2),fltarr(2), col=col.black    $
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

polyfill,[XR[0],lamgood(0),lamgood(0),XR[0]],[YR[0],YR[0],YR[1],YR[1]],col=col.gray
polyfill,[lamgood(1),XR[1],XR[1],lamgood(1)],[YR[0],YR[0],YR[1],YR[1]],col=col.gray

oplot,lam0,cont,col=col.black,linestyle=2,thick=thickall

;overplotting sdss filters
ysc = 50  ; (abitrary) scaling factor
oplot,lam_u/(1.+zplot),respt_u*ysc,col=col.blue,thick=thickall
oplot,lam_g/(1.+zplot),respt_g*ysc,col=col.darkgreen,thick=thickall
oplot,lam_r/(1.+zplot),respt_r*ysc,col=col.gold,thick=thickall
oplot,lam_i/(1.+zplot),respt_i*ysc,col=col.orange,thick=thickall
oplot,lam_z/(1.+zplot),respt_z*ysc,col=col.red,thick=thickall

oplot,lam0,flux0,col=col.black,thick=thickall


XYOUTS,XR[0]+DX*0.1,YR[0]+DY*0.60,'z = '+trim(zplot),col=col.black,charthick=thickall,charsize=2.5




if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif

Nw = Nw+1                ; incremeting window number by 1
;=============================================================================================

;=============================================================================================

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/fluxSDSSbands_EL.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw,  xsize=600, ysize=500, title = 'Emission line flux of SDSS bands'
   thickall = 2
endelse

; setting plot range
XR = [zrange[0]-0.05*(zrange[1]-zrange[0]),zrange[1]+0.05*(zrange[1]-zrange[0])]
YR = [0.001,10.0]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

; creating propoer log tickmarks
ticks = LOGLEVELS([YR[0],YR[1]])   ; determining tickmarks values
nticks = N_Elements(ticks)         ; number of ticks

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('f_{EL,SDSS}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , yticks = nticks-1 , ytickv=ticks $;, xtickname = [' ',' '] $      ; removing the x ticks
        , /ylog $
        , background = col.white

;oplot,zobs,Farr_EL(0,*),thick=thickall,col=col.blue,linestyle=1
;oplot,zobs,Farr_EL(1,*),thick=thickall,col=col.darkgreen,linestyle=1
;oplot,zobs,Farr_EL(2,*),thick=thickall,col=col.gold,linestyle=1
;oplot,zobs,Farr_EL(3,*),thick=thickall,col=col.orange,linestyle=1
;oplot,zobs,Farr_EL(4,*),thick=thickall,col=col.red,linestyle=1

oplot,zobs(entgood_zu),Farr_EL(0,entgood_zu),thick=thickall,col=col.blue
oplot,zobs(entgood_zg),Farr_EL(1,entgood_zg),thick=thickall,col=col.darkgreen
oplot,zobs(entgood_zr),Farr_EL(2,entgood_zr),thick=thickall,col=col.gold
oplot,zobs(entgood_zi),Farr_EL(3,entgood_zi),thick=thickall,col=col.orange
oplot,zobs(entgood_zz),Farr_EL(4,entgood_zz),thick=thickall,col=col.red

PLOTSYM,0,0.5,/FILL
oplot,[zplot,zplot],YR,col=col.charcoal,linestyle=3,thick=thickall 

XYOUTS,XR[0]+DX*0.08,YR[0]+DY*0.0001,textoidl('Band:'),col=col.black,charthick=thickall,charsize=2,alignment=0.0
XYOUTS,XR[0]+DX*0.25,YR[0]+DY*0.0001,'u',col=col.blue,charthick=thickall,charsize=2,alignment=0.0
XYOUTS,XR[0]+DX*0.30,YR[0]+DY*0.0001,'g',col=col.darkgreen,charthick=thickall,charsize=2,alignment=0.0
XYOUTS,XR[0]+DX*0.35,YR[0]+DY*0.0001,'r',col=col.gold,charthick=thickall,charsize=2,alignment=0.0
XYOUTS,XR[0]+DX*0.40,YR[0]+DY*0.0001,'i',col=col.orange,charthick=thickall,charsize=2,alignment=0.0
XYOUTS,XR[0]+DX*0.45,YR[0]+DY*0.0001,'z',col=col.red,charthick=thickall,charsize=2,alignment=0.0

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


;=============================================================================================

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/fluxSDSSbands_ELcolor.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw,  xsize=600, ysize=500, title = 'Emission line flux of SDSS bands - colors'
   thickall = 2
endelse

; setting plot range
XR = [zrange[0]-0.05*(zrange[1]-zrange[0]),zrange[1]+0.05*(zrange[1]-zrange[0])]
YR = [-1.0,6.0]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =textoidl('f_{SDSS}') $
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


Fug = Farr_EL(0,*)-Farr_EL(1,*)
Fgr = Farr_EL(1,*)-Farr_EL(2,*)
Fri = Farr_EL(2,*)-Farr_EL(3,*)
Fiz = Farr_EL(3,*)-Farr_EL(4,*)
Fui = Farr_EL(0,*)-Farr_EL(4,*)

oplot,zobs(entgood_zu),Fug(entgood_zu),thick=thickall,col=col.black
oplot,zobs(entgood_zg),Fgr(entgood_zg),thick=thickall,col=col.charcoal
oplot,zobs(entgood_zr),Fri(entgood_zr),thick=thickall,col=col.darkgray
oplot,zobs(entgood_zi),Fiz(entgood_zi),thick=thickall,col=col.gray
oplot,zobs(entgood_zu),Fui(entgood_zu),thick=thickall,col=col.lightgray

PLOTSYM,0,0.5,/FILL
oplot,[zplot,zplot],YR,col=col.charcoal,linestyle=3,thick=thickall 

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: compspec_bandpos.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
