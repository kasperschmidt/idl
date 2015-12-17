;+
;----------------------------
;   NAME
;----------------------------
; slopesBovyVSSchmidt.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Small code to compare the A and Gamma values from Jo Bovy's fit with
; the ones presented in Schmidt et al. 2010
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
; IDL> slopesBovyVSSchmidt,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-05-26  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ matchRADEC.pro
;----------------------------
;-
PRO slopesBovyVSSchmidt,EPS=EPS,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

bovy      = mrdfits('/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/powerlawSF_constmean/powerlawSF_constmean_r.fits',1) 
schmidt   = mrdfits('/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/slopeANDshencatThu_Feb_3_163401_2011_season0_ERRcorrected.fits',1)
schmidtui = mrdfits('/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/slopeANDshencatThu_Feb_3_163447_2011_season0_ERRcorrected.fits',1)

Narcsec = 3 ; the number of arc seconds to match objects within
matchRADEC,schmidt.ra,schmidt.dec,bovy.ra,bovy.dec,Narcsec*0.0002777778,MATCH,/VERBOSE

Aschmidt    = schmidt(match(0,*)).A
Gschmidt    = schmidt(match(0,*)).gamma
Abovy       = exp(bovy(match(1,*)).loga/2.)   ; according to bovy the loga in the fits file is 2 ln A_schmidt
Gbovy       = bovy(match(1,*)).gamma

SgrSchmidt  = schmidt(match(0,*)).abest-1
SuiSchmidt  = schmidtui(match(0,*)).abest-1

Nw = 0 ; resetting window numbering
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'AGbovyANDschmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'A vs Gamma'
   thickall = 2
endelse
; setting plot range
XR = [0.01,1.0]
YR = [-0.1,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

; --- plotting data in main window ---
plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log A') $
        , ytitle =textoidl('\gamma') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize =cs $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , pos = [0.15,0.15,0.8,0.8] $
        , xticks = 2 , xtickname = ['0.01','0.10',' '] $
        , /xlog $
;        ,/ylog  $
        , background = col.white

POLYFILL,[XR[0],0.05,0.05,XR[0]],[YR[0],YR[0],YR[1],YR[1]],col=col.lightgray

PLOTSYM,0,0.5
oplot,Aschmidt,Gschmidt,psym=8,thick=thickall,col=col.blue  ; overplotting line at 0
oplot,Abovy,Gbovy,psym=8,thick=thickall,col=col.red  ; overplotting line at 0

NAlowBovy    = n_elements(where(Abovy lt 0.05))
NAlowSchmidt = n_elements(where(Aschmidt lt 0.05))

XYOUTS,DX*0.001+XR[0],DY*0.92+YR[0],textoidl('N_{A<0.05;Schmidt} = ')+trim(NalowSchmidt),col=col.black,charsize=2,charthick=thickall
XYOUTS,DX*0.001+XR[0],DY*0.85+YR[0],textoidl('N_{A<0.05;Bovy} = ')+trim(NalowBovy),col=col.black,charsize=2,charthick=thickall

;==============
;Legend box in upper right corner
;==============
plot,fltarr(2),fltarr(2),psym=2,col=col.white $
        , /NODATA $
	, xtitle=' ' $
	, ytitle=' '$
        , xrange=[0,1] , /xstyle $
        , yrange =[0,1], /ystyle $
	, charsize =2.5 $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , pos = [0.8,0.8,0.98,0.98] $
        ,/noerase $
        , background = col.white

xyouts,0.1,0.3,'Bovy',col=col.red,charsize=2.,charthick=thickall
xyouts,0.1,0.6,'Schmidt',col=col.blue,charsize=2.,charthick=thickall
;==============


; --- X axis hist ---
binS = 0.05
ASvals = Aschmidt(where(Aschmidt gt XR[0] and Aschmidt lt XR[1]))
LAS    = alog10(ASvals)                                                     ; to get log bins easy
plothist,LAS,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,LAS,bin=binS,col=col.blue, thick=thickall $
        , axiscolor=col.black $
;        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange = [0.0,1400.], /xstyle $
        , yticks = 2 , ytickname = ['0','7','14'] $
        , xrange=[alog10(XR[0]),alog10(XR[1])] , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

ABvals = Abovy(where(Abovy gt XR[0] and Abovy lt XR[1]))
LAB    = alog10(ABvals)
plothist,LAB,bin=binS,col=col.red,/overplot,thick=thickall

XYOUTS,-1.98,1100,'x100',col=col.black,charsize=2,charthick=thickall

; --- Y axis hist ---
binS = 0.05
GSvals = Gschmidt(where(Gschmidt gt YR[0] and Gschmidt lt YR[1]))
plothist,GSvals,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,GSvals,bin=binS,col=col.blue, thick=thickall $
        , /rotate  $
        , axiscolor=col.black $
;        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , xrange = [0.0,1400.0], /xstyle $
        , xticks = 2 , xtickname = ['0','7','14'] $
        , yrange=YR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the y ticks
        , pos=[0.8,0.15,0.97,0.8]

GBvals = Gbovy(where(Gbovy gt YR[0] and Gbovy lt YR[1]))
plothist,GBvals,bin=binS,col=col.red,/overplot,/rotate,thick=thickall

XYOUTS,700,-0.05,'x100',col=col.black,charsize=2,charthick=thickall

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
   plot1 = 'AbovyVSAschmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'A vs A'
   thickall = 2
endelse
; setting plot range
XR = [0.01,1]
YR = [0.01,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log A_{Schmidt}') $
        , ytitle =textoidl('log A_{Bovy}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog , /ylog $
        , background = col.white

POLYFILL,[XR[0],0.05,0.05,XR[0]],[YR[0],YR[0],0.05,0.05],col=col.lightgray

PLOTSYM,0,1.
oplot,Aschmidt,Abovy,psym=8,col=col.black,thick=thickall
oplot,[0.00001,10],[0.00001,10.],col=col.black

;XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],textoidl('objects with \deltas_{gr} < ')+trim(SNlim),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0

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
   plot1 = 'GbovyVSGschmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Gamma vs Gamma'
   thickall = 2
endelse
; setting plot range
XR = [0.0,1.5]
YR = [0.0,1.5]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\gamma_{Schmidt}') $
        , ytitle =textoidl('\gamma_{Bovy}') $
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

PLOTSYM,0,1.
oplot,Gschmidt,Gbovy,psym=8,col=col.black,thick=thickall
oplot,[0.00001,10],[0.00001,10.],col=col.black

;XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],textoidl('objects with \deltas_{gr} < ')+trim(SNlim),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0

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
   plot1 = 'AbovyVSSgr_schmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Abovy vs Sgr schmidt'
   thickall = 2
endelse
; setting plot range
XR = [0.01,1]
YR = [-0.85,0.4]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log A_{Bovy}') $
        , ytitle =textoidl('s_{gr,Schmidt}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , background = col.white

POLYFILL,[XR[0],0.05,0.05,XR[0]],[YR[0],YR[0],YR[1],YR[1]],col=col.lightgray

PLOTSYM,0,1.
oplot,schmidt.A,schmidt.abest-1,psym=8,col=col.blue,thick=thickall
oplot,Abovy,SgrSchmidt,psym=8,col=col.red,thick=thickall

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
   plot1 = 'AbovyVSSui_schmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Abovy vs Sui schmidt'
   thickall = 2
endelse
; setting plot range
XR = [0.01,1]
YR = [-1.1,0.2]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log A_{Bovy}') $
        , ytitle =textoidl('s_{ui,Schmidt}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , background = col.white

POLYFILL,[XR[0],0.05,0.05,XR[0]],[YR[0],YR[0],YR[1],YR[1]],col=col.lightgray

PLOTSYM,0,1.
oplot,schmidtui.A,schmidtui.abest-1,psym=8,col=col.blue,thick=thickall
oplot,Abovy,SuiSchmidt,psym=8,col=col.red,thick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

; loading the saved files
restore,'AprimeAvg_plot30schmidt.sav'
APRIMEavg_schmidtgr         = APRIMEavg
restore,'GammaAvg_plot31schmidt.sav'
GAMMAavg_schmidtgr          = GAMMAavg


restore,'AprimeAvg_plot30bovy.sav'
APRIMEavg_bovygr            = APRIMEavg
restore,'GammaAvg_plot31bovy.sav'
GAMMAavg_bovygr             = GAMMAavg

restore,'AprimeAvg_plot30schmidt_ui.sav'
APRIMEavg_schmidtui         = APRIMEavg
restore,'GammaAvg_plot31schmidt_ui.sav'
GAMMAavg_schmidtui          = GAMMAavg

restore,'AprimeAvg_plot30bovy_ui.sav'
APRIMEavg_bovyui            = APRIMEavg
restore,'GammaAvg_plot31bovy_ui.sav'
GAMMAavg_bovyui             = GAMMAavg

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSAprime_grBovyAndSchmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS Aprime zcorrected ZOOM'
   thickall = 2
endelse
; setting plot range
XR = [alog10(0.1),alog10(0.7)]
YR = [-0.4,0]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ="A'" $
;        , ytitle ='a' $
        , ytitle =textoidl('<s_{gr}>') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , xtickname = ['0.1','0.16','0.25','0.40','0.63'] $  ; gr ticks
;        , xtickname = ['0.25','0.32','0.40','0.50','0.63','0.79'] $  ; ui ticks
;        , /xlog $
        , background = col.white

oploterror,APRIMEavg_schmidtgr(0,*),APRIMEavg_schmidtgr(1,*),APRIMEavg_schmidtgr(2,*),psym=3, col=col.blue, ERRCOLOR=col.blue, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation

oploterror,APRIMEavg_bovygr(0,*),APRIMEavg_bovygr(1,*),APRIMEavg_bovygr(2,*),psym=3, col=col.red, ERRCOLOR=col.red, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation


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
   plot1 = 'epsplots/slopeVSgamma_grBovyAndSchmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS gamma zcorrected ZOOM'
   thickall = 2
endelse
; setting plot range
XR = [0,0.65]
YR = [-0.4,0]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('<s_{gr}>') $
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

oploterror,GAMMAavg_schmidtgr(0,*),GAMMAavg_schmidtgr(1,*),GAMMAavg_schmidtgr(2,*),psym=3, col=col.blue, ERRCOLOR=col.blue, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation

oploterror,GAMMAavg_bovygr(0,*),GAMMAavg_bovygr(1,*),GAMMAavg_bovygr(2,*),psym=3, col=col.red, ERRCOLOR=col.red, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                   ; incremeting window number by 1
;=============================================================================================



;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSAprime_uiBovyAndSchmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS Aprime zcorrected ZOOM'
   thickall = 2
endelse
; setting plot range
XR = [alog10(0.21),alog10(0.8)]
YR = [-0.5,-0.2]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ="A'" $
;        , ytitle ='a' $
        , ytitle =textoidl('<s_{ui}>') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , xtickname = ['0.25','0.32','0.40','0.50','0.63','0.79'] $  ; ui ticks
;        , /xlog $
        , background = col.white

oploterror,APRIMEavg_schmidtui(0,*),APRIMEavg_schmidtui(1,*),APRIMEavg_schmidtui(2,*),psym=3, col=col.blue, ERRCOLOR=col.blue, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation

oploterror,APRIMEavg_bovyui(0,*),APRIMEavg_bovyui(1,*),APRIMEavg_bovyui(2,*),psym=3, col=col.red, ERRCOLOR=col.red, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation


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
   plot1 = 'epsplots/slopeVSgamma_uiBovyAndSchmidt.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS gamma zcorrected ZOOM'
   thickall = 2
endelse
; setting plot range
XR = [0,0.65]
YR = [-0.7,-0.3]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('<s_{ui}>') $
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

oploterror,GAMMAavg_schmidtui(0,*),GAMMAavg_schmidtui(1,*),GAMMAavg_schmidtui(2,*),psym=3, col=col.blue, ERRCOLOR=col.blue, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation

oploterror,GAMMAavg_bovyui(0,*),GAMMAavg_bovyui(1,*),GAMMAavg_bovyui(2,*),psym=3, col=col.red, ERRCOLOR=col.red, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                   ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,':: slopesBovyVSSchmidt.pro :: Number of objects with A < 0.05: '
if vb eq 1 then print,'                                 Bovy     : ',trim(NAlowBovy)
if vb eq 1 then print,'                                 Schmidt  : ',trim(NAlowSchmidt)







if vb eq 1 then print,' '
if vb eq 1 then print,':: slopesBovyVSSchmidt.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
