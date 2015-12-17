;+
;----------------------------
;   NAME
;----------------------------
; estimateRRLslope.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure modeling the RR Lyrae variability as a black body
; temperature oscillation/pulsation and from that estimating the slope
; such a variabilit would trace in SDSS gr space
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
;
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> estimateRRLslope,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-03-23  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ blackbody.pro
@ linearfitMCMC.pro
;----------------------------
;-
;----------------------------
FUNCTION LINE, X, P
  RETURN, P[0] + P[1]*X 
END
;----------------------------
PRO estimateRRLslope,EPS=EPS,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

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

Nlam = 1000
lam  = findgen(Nlam)*15       ; wavelength range for plotting

Nepochs = 15                     ; number of variability epochs
Tvar    = findgen(Nepochs)*100+6200 ; vector with variability of temperature A8 to F7 according to http://stars.astro.illinois.edu/sow/rrlyr.html

BBarr   = fltarr(Nepochs,Nlam)
INTg    = fltarr(Nepochs)
INTr    = fltarr(Nepochs) 


for ii=0,Nepochs-1 do begin
   BBarr(ii,*) = blackbody(lam,Tvar(ii))  ; ergs/s cm^-2 Ang^-1 ; calculating black body for specified temperature
   INTg(ii)    = TSUM(blackbody(lam_g,Tvar(ii))*respt_g)  ; numerical integration of blackbody multiplied with band efficiency
   INTr(ii)    = TSUM(blackbody(lam_r,Tvar(ii))*respt_r)  ; numerical integration of blackbody multiplied with band efficiency
endfor

Gmag = -2.5*alog10(INTg)
Rmag = -2.5*alog10(INTr)

; y erros drawn from gaussian with sigma dependent on mag
SIGMA = 0.02
MEAN  = 0.0
GAUSS = RANDOMN(seed,Nepochs)
Gerr  = abs(GAUSS * SIGMA + MEAN)   ; gaussian drawn errors
GAUSS = RANDOMN(seed,Nepochs)
Rerr  = abs(GAUSS * SIGMA + MEAN)   ; gaussian drawn errors

initP    = [0.0,1.0] ; intial guess of parameters
resultMP = MPFITFUN('LINE',Gmag,Rmag,Rerr,initP) ; fitting line to magnitudes

DATARRAY = [[Gmag],[Rmag],[Gerr],[Rerr]] ; create input array
linearfitMCMC,DATARRAY,RESULT,Nmcmc=MCMCloops,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT,/VERBOSE

Nw = 0
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/BBvsSDSS.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'BB vs SDSS filters'
   thickall = 2
endelse
; setting plot range
XR = [min(lam),max(lam)]
YR = [0,8e-8]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda') $
        , ytitle =textoidl('ergs s^{-1} cm^{-2} A^{-1} ') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CS $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

;overplotting sdss filters
ysc = 1e-7  ; (abitrary) scaling factor
oplot,lam_u,respt_u*ysc,col=col.blue,thick=thickall
oplot,lam_g,respt_g*ysc,col=col.green,thick=thickall
oplot,lam_r,respt_r*ysc,col=col.yellow,thick=thickall
oplot,lam_i,respt_i*ysc,col=col.orange,thick=thickall
oplot,lam_z,respt_z*ysc,col=col.red,thick=thickall

oplot,lam,blackbody(lam,5000),col=col.lightgray,thick=thickall
oplot,lam,blackbody(lam,6000),col=col.gray,thick=thickall
oplot,lam,blackbody(lam,6500),col=col.slategray,thick=thickall
oplot,lam,blackbody(lam,7000),col=col.darkgray,thick=thickall
oplot,lam,blackbody(lam,7500),col=col.charcoal,thick=thickall
oplot,lam,blackbody(lam,8000),col=col.black,thick=thickall

XYOUTS,DX*0.95+XR[0],DY*0.92+YR[0],'8000K',col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.85+YR[0],'7500K',col=col.charcoal,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.78+YR[0],'7000K',col=col.darkgray,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.71+YR[0],'6500K',col=col.slategray,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.64+YR[0],'6000K',col=col.gray,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.57+YR[0],'5000K',col=col.lightgray,charsize=2.5,charthick=thickall,alignment=1.0

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
   plot1 = 'epsplots/BBvsSDSS_integrated.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'BB vs SDSS filters'
   thickall = 2
endelse
; setting plot range
XR = [min(lam),max(lam)]
YR = [0,6e-8]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda') $
        , ytitle =textoidl('ergs s^{-1} cm^{-2} A^{-1} ') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CS $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

for jj=0,Nepochs-1 do begin
   oplot,lam,BBarr(jj,*),col=col.black,thick=thickall,linestyle=2
   oplot,lam_g,blackbody(lam_g,Tvar(jj))*respt_g,col=col.green,thick=thickall
   oplot,lam_r,blackbody(lam_r,Tvar(jj))*respt_r,col=col.yellow,thick=thickall
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

if vb eq 1 then print,' '
if vb eq 1 then print,':: estimateRRLslope.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
