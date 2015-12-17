;+
;----------------------------
;   NAME
;----------------------------
; errortracks.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure simulating color-magnitude tracks arising solely from
; photometric errors on the magnitudes via drawing 'observations'
; randomly from a gaussian error distribution.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; mags            : vector containing magnitudes to create simulated
;                   observation for                    ; [16,17,18,19,20,21,22]
; sigma           : the errors of the given magnitudes ; [0.02,0.02,0.02,0.025,0.04,0.06,0.15]
; Nobs            : vector with the number of observations to draw
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; mag2nd          : vector containing magnitudes of second bans if the
;                   two bands are not be set equal; [16,16.9,17.8,18.7,19.6,20.5,21]
; sigma2nd        ; errors of the mag2nd if different from sigma
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> errortracks,[18,19,20,21,22],[0.02,0.025,0.04,0.06,0.15],30,mag2nd=[17.8,18.7,19.6,20.5,21],/VERBOSE

; IDL> errortracks,[16,17,18,19,20,21,22],[0.02,0.02,0.02,0.025,0.04,0.06,0.15],30,mag2nd=[16,16.9,17.8,18.7,19.6,20.5,21],/VERBOSE
;
; IDL> errortracks,[20.4,20.2,20.0,19.8,19.6],[0.02,0.02,0.02,0.02,0.02],30,/VERBOSE,mag2nd=[20.1,19.9,19.7,19.65,19.4]
; IDL> errortracks,[20.2,20.0,19.875,19.7],[0.02,0.02,0.02,0.02],8,/VERBOSE,mag2nd=[19.87,19.68,19.58,19.4]  


; IDL> errortracks,[19.7391,19.9315,20.2716,19.8925,19.9245,19.8638,20.0165,19.9511,19.9386,20.1812,20.3344,20.2960,20.2541,20.2698,19.8867,19.7790,19.8000,19.9602,20.0345,20.0051,20.1448,19.8120,20.2471,19.7655,19.8433,19.8103,20.1791,19.9484,19.9985,20.2830,19.6100,20.0111,20.2232,20.0017], [19.4197,19.7828,19.9834,19.5637,19.5548,19.6072,19.6494,19.6309,19.6550,19.8727,19.8930,19.8772,19.9247,19.9099,19.5009,19.5445,19.4822,19.6475,19.6865,19.6887,19.8151,19.4919,19.8546,19.4644,19.5647,19.5082,19.8418,19.6726,19.6452,19.9282,19.3665,19.6663,19.7668,19.6729], [0.0158212,0.0211760,0.0383172,0.0486138,0.0212528,0.0214100,0.0383409,0.0270589,0.0271486,0.0337459,0.0725818,0.0295096,0.0267409,0.0502368,0.0204997,0.0182415,0.0169831,0.0214761,0.0351146,0.0618257,0.0410413,0.0224072,0.0499850,0.0230960,0.0236991,0.0276111,0.0316084,0.0210652,0.0213840,0.0264657,0.0175575,0.0206944,0.0241938,0.0239192], [0.0158212,0.0211760,0.0383172,0.0486138,0.0212528,0.0214100,0.0383409,0.0270589,0.0271486,0.0337459,0.0725818,0.0295096,0.0267409,0.0502368,0.0204997,0.0182415,0.0169831,0.0214761,0.0351146,0.0618257,0.0410413,0.0224072,0.0499850,0.0230960,0.0236991,0.0276111,0.0316084,0.0210652,0.0213840,0.0264657,0.0175575,0.0206944,0.0241938,0.0239192]



;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-19  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ linearfitMCMC.pro
;----------------------------
;-
PRO errortracks,mags,sigma1,Nobs,mag2nd=mag2nd,sigma2nd=sigma2nd,EPS=EPS,VERBOSE=VERBOSE

S2 = n_elements(sigma2nd)
M2 = n_elements(mag2nd)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

Nmags = n_elements(mags)

if M2 gt 0 and n_elements(mag2nd) ne Nmags then print,':: errortracks.pro :: Dimension of the two magnitude vectors are not the same --> Aborting',stop

mag1 = fltarr(Nmags,Nobs)   ; array to contain the gaussian drawn observations of 1st band
mag2 = fltarr(Nmags,Nobs)   ; array to contain the gaussian drawn observations of 2nd band

mag1err = fltarr(Nmags,Nobs)   ; array to contain the gaussian drawn observations of 1st band
mag2err = fltarr(Nmags,Nobs)   ; array to contain the gaussian drawn observations of 2nd band


for ii=0,Nmags-1 do begin
   SIG              = sigma1(ii)
   MEAN             = mags(ii)
   GAUSS            = RANDOMN(seed, Nobs)
   mag1(ii,*)       = GAUSS * SIG + MEAN
   mag1err(ii,*)    = SIG

   if M2 gt 0 then begin
      if S2 gt 0 then begin 
         SIG2       = sigma2nd(ii)
      endif else begin
         SIG2       = SIG
      endelse
      MEAN2         = mag2nd(ii)
      GAUSS2        = RANDOMN(seed, Nobs)
      mag2(ii,*)    = GAUSS2 * SIG2 + MEAN2
      mag2err(ii,*) = SIG2
   endif else begin
      GAUSS2        = RANDOMN(seed, Nobs)
      mag2(ii,*)    = GAUSS2 * SIG + MEAN
      mag2err(ii,*) = SIG2
   endelse
endfor

;fwhm = sigma * ( 2 * SQRT(2* ALOG(2)) )


Nw = 0
;goto,skipplots
;=============================================================================================
; = = = MAG MAG = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'mag1vsmag2.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'magnitude-magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [max(mag1)+0.5,min(mag1)-0.5]
YR = [max(mag2)+0.5,min(mag2)-0.5]
;XR = [mags(1)+0.5,mags(1)-0.5]
;YR = [mags(1)+0.5,mags(1)-0.5]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='g' $
        , ytitle ='r' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

plotsym,0,1.5,/fill
for jj=0,Nmags-1 do begin
   oplot,mag1(jj,*),mag2(jj,*),psym=8,thick=thickall,col=col.black
endfor
oplot,[0,30],[0,30],thick=thickall,col=col.black

;oplot,fltarr(2),

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = MAG MAG = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'mag2vsmag1.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'magnitude-magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [max(mag2)+0.5,min(mag2)-0.5]
YR = [max(mag1)+0.5,min(mag1)-0.5]
;XR = [mags(1)+0.5,mags(1)-0.5]
;YR = [mags(1)+0.5,mags(1)-0.5]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='r' $
        , ytitle ='g' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

plotsym,0,1.5,/fill
for jj=0,Nmags-1 do begin
   oplot,mag2(jj,*),mag1(jj,*),psym=8,thick=thickall,col=col.black
endfor
oplot,[0,30],[0,30],thick=thickall,col=col.black

;oplot,fltarr(2),

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


;=============================================================================================
; = = = COL MAG1 = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'colvsmag1.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'color-magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [min(mag1-mag2)-0.1,max(mag1-mag2)+0.1]
YR = [max(mag1)+0.5,min(mag1)-0.5]
;XR = [0,1]
;YR = [max(mags(4))+0.5,min(mags(4))-0.5]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='g' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white
plotsym,0,1.5,/fill
for jj=0,Nmags-1 do begin
   oplot,mag1(jj,*)-mag2(jj,*),mag1(jj,*),psym=8,thick=thickall,col=col.black
endfor


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = COL MAG2 = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'colvsmag2.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'color-magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [min(mag1-mag2)-0.1,max(mag1-mag2)+0.1]
YR = [max(mag2)+0.5,min(mag2)-0.5]
;XR = [0,1]
;YR = [max(mags(4))+0.5,min(mags(4))-0.5]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='r' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white
plotsym,0,1.5,/fill
for jj=0,Nmags-1 do begin
   oplot,mag1(jj,*)-mag2(jj,*),mag2(jj,*),psym=8,thick=thickall,col=col.black
endfor


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = COL (MAG1+MAG2)/2 = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'colvsmag1mag2.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'color-magmag plot'
   thickall = 2
endelse
; setting plot range
XR = [min(mag1-mag2)-0.1,max(mag1-mag2)+0.1]
YR = [max((mag2+mag1)/2.)+0.5,min((mag2+mag1)/2.)-0.5]


DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='(g-r) / 2' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

plotsym,0,1.5,/fill
for jj=0,Nmags-1 do begin
   oplot,mag1(jj,*)-mag2(jj,*),(mag2(jj,*)+mag1(jj,*))/2.,psym=8,thick=thickall,col=col.black
   oplot,[(XR(1)-XR(0))*0.05+XR(0),(XR(1)-XR(0))*0.05+XR(0)+sigma1(jj)],[(YR(1)-YR(0))*0.05+YR(0),(YR(1)-YR(0))*0.05+YR(0)+sigma1(jj)]
endfor


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                   ; incremeting window number by 1
;=============================================================================================
print,':: errortracks.pro :: fast plots done'
skipplots:




; fitting data
DATARRAYgr      = fltarr(n_elements(mags)*Nobs,4)
for jj=0,Nmags-1 do begin
   DATARRAYgr(jj*Nobs:jj*Nobs+Nobs-1,0) = mag1(jj,*)
   DATARRAYgr(jj*Nobs:jj*Nobs+Nobs-1,1) = mag2(jj,*)
   DATARRAYgr(jj*Nobs:jj*Nobs+Nobs-1,2) = mag1err(jj,*)
   DATARRAYgr(jj*Nobs:jj*Nobs+Nobs-1,3) = mag2err(jj,*)
endfor

gmag  = DATARRAYgr(*,0)
rmag  = DATARRAYgr(*,1)
gerr  = DATARRAYgr(*,2)
rerr  = DATARRAYgr(*,3)


MCMCloops = 10000
linearfitMCMC,DATARRAYgr,RESULTgr,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT


; fitting data
DATARRAY      = fltarr(n_elements(mags)*Nobs,4)
for jj=0,Nmags-1 do begin
   DATARRAY(jj*Nobs:jj*Nobs+Nobs-1,0) = mag1(jj,*)
   DATARRAY(jj*Nobs:jj*Nobs+Nobs-1,1) = mag1(jj,*)-mag2(jj,*)
   DATARRAY(jj*Nobs:jj*Nobs+Nobs-1,2) = mag1err(jj,*)
   DATARRAY(jj*Nobs:jj*Nobs+Nobs-1,3) = sqrt(mag1err(jj,*)^2.+mag2err(jj,*)^2.)
endfor

grcol = DATARRAY(*,1)
grcolerr = DATARRAY(*,3)

MCMCloops = 10000
linearfitMCMC,DATARRAY,RESULT,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT



;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/modVSobj_g_simulated.eps' ; name of eps file
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Model vs object'
   thickall = 2
endelse
; setting plot range
XR = [max(mag1)+1,min(mag1)-1]
YR = [min(mag1-mag2)-0.25,max(mag1-mag2)+0.25]
   XR = [20.4,19.5] ; cutout of object 1
   YR = [-0.1,0.65] ; cutout of object 1


DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='g' $
        , ytitle ='g-r' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

ggrslope  = 0.0806890 
ggroffset = 0.321779
XX = findgen(30)-15.
oplot,XX+19.9859,ggrslope*XX+ggroffset,linestyle=0,thick=thickall,col=col.blue

oplot,xx+mean(gmag),result(0)*xx+mean(grcol)+result(7),linestyle=0,thick=thickall,col=col.black

oplot,XX+mean(gmag),(1-resultgr(0))*XX+mean(grcol)-resultgr(7),linestyle=2,thick=thickall,col=col.red

plotsym,0,1.5,/fill
oplot,gmag,grcol,psym=8,thick=thickall,col=col.black
for jj=0,Nmags-1 do begin
   ;oplot,mag1(jj,*),mag1(jj,*)-mag2(jj,*),psym=8,thick=thickall,col=col.black
endfor

for nn=0,n_elements(gmag)-1 do begin
   ell = ELLIPSE([gmag(nn),grcol(nn)],[gerr(nn),grcolerr(nn)])
   oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/modVSobj_simulated.eps' ; name of eps file
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot      
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'g VS r'
   thickall = 2
endelse
; setting plot range
XR = [max(mag1)+0.05,min(mag1)-0.05]
YR = [max(mag2)+0.05,min(mag2)-0.05]
   XR = [20.4,19.5] ; cutout of object 1
   YR = [20.35,19.3] ; cutout of object 1

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,mag1,mag2, col=col.black    $
        , /NODATA $
        , xtitle ='g' $
        , ytitle ='r' $
        , thick  = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

abest    = 0.919311
groffset = 19.6641 ; bbest = -0.117365
XX = findgen(30)-15
oplot,XX+19.9859,Abest*XX+groffset,linestyle=0,thick=thickall,col=col.blue

oplot,XX+mean(gmag),(1-result(0))*XX+mean(rmag)-result(7),linestyle=2,thick=thickall,col=col.black

oplot,XX+mean(gmag),resultgr(0)*XX+mean(rmag)+resultgr(7),linestyle=0,thick=thickall,col=col.red

plotsym,0,1.5,/fill
oplot,gmag,rmag,psym=8,thick=thickall,col=col.black
for jj=0,Nmags-1 do begin
   ;oplot,mag1(jj,*),mag2(jj,*),psym=8,thick=thickall,col=col.black
endfor

for nn=0,n_elements(gmag)-1 do begin
   ell = ELLIPSE([gmag(nn),rmag(nn)],[gerr(nn),rerr(nn)])
   oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================



if vb eq 1 then print,' '
if vb eq 1 then print,':: errortracks.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
