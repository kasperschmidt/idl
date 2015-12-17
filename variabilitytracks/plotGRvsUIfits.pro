;+
;----------------------------
;   NAME
;----------------------------
; plotGRvsUIfits.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Plotting the gr VS ui fitted slopes (or two other slopeANDshencat* files)
; against one another.
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
; /HIGHSN         : set /HIGHSN to only plot objects with high S/N
; /SLOPEM1        : set /SLOPEM1 to plot the obtained slopes mines 1 instead of just the slopes
; /WAVENORM       : set /WAVENORM to normalize the fitted slopes to the bands wavelength ration
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotGRvsUIfits,/VERBOSE,/HIGHSN,/SLOPEM1,/WAVENORM
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-02-03  started by K. B. Schmidt (MPIA)
; 2011-04-09  keyword slopeM1 added K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ contourarray.pro
;----------------------------
;-
PRO plotGRvsUIfits,EPS=EPS,HIGHSN=HIGHSN,VERBOSE=VERBOSE,SLOPEM1=SLOPEM1,WAVENORM=WAVENORM

WN = n_elements(WAVENORM)
SM1 = n_elements(slopeM1)
SN = n_elements(HIGHSN)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

; reading data files
GRfile = 'slopeANDshencatThu_Feb_3_163401_2011_season0_ERRcorrected.fits'
UIfile = 'slopeANDshencatThu_Feb_3_163447_2011_season0_ERRcorrected.fits'
; testing GR vs RG run
;GRfile = 'slopeANDshencatMon_Feb_7_135524_2011_season0_TOP10gr.fits'
;UIfile = 'slopeANDshencatMon_Feb_7_132648_2011_season0_TOP10rg.fits'
;plot,findgen(10),UIdat.abest*GRdat.abest,psym=2,yrange=[0.8,1.2],xrange=[-0.5,10.5] & oploterr,findgen(10),UIdat.abest*GRdat.abest,sqrt(UIdat.APLUS68^2.+GRdat.APLUS68^2.) & oplot,findgen(10),fltarr(10)+1.0

; reading files
GRdat  = mrdfits(GRfile,1)
UIdat  = mrdfits(UIfile,1)

; restoring redshift corrected slopes
restore,'acorrect_grRIGHT.sav'
acorrected0gr = acorrected0
restore,'acorrect_uiRIGHT.sav'
acorrected0ui = acorrected0
; overwriting read values with z-corrected ones
GRdat(sort(GRdat.z)).abest = acorrected0gr
UIdat(sort(UIdat.z)).abest = acorrected0ui

if SM1 eq 1 then begin
   GRdat.abest = GRdat.abest - 1
   UIdat.abest = UIdat.abest - 1
endif

if WN eq 1 then begin
   uwave = 3551.  ;Å
   gwave = 4686.  ;Å
   rwave = 6165.  ;Å
   iwave = 7481.  ;Å
   grrat = gwave/rwave   ; the ration between the g and r band wavelengths
   uirat = uwave/iwave   ; the ration between the u and i band wavelengths
;   GRdat.abest = GRdat.abest/alog10(rwave-gwave) ; /grrat
;   UIdat.abest = UIdat.abest/alog10(iwave-uwave) ; /uirat
   GRdat.abest = GRdat.abest*grrat
   UIdat.abest = UIdat.abest*uirat
endif

if SN eq 1 then begin  ; only plotting high S/N objects
   ;SNlim   = 19.5
   ;plotent = where(GRdat.psfmag_r lt SNlim)

   SNlim   = 0.04
   plotent = where(GRdat.APLUS68 lt SNlim,Csel)

   ;SNlim   = 0.07
   ;plotent = where(GRdat.APLUS95 lt SNlim,Csel)
   if vb eq 1 then print,':: plotGRvsUIfits.pro :: Object that satisfies SN (i.e. error on slope estimate) cut : ',trim(Csel)
endif else begin
   plotent = findgen(n_elements(UIdat.abest))   ; all entries
endelse


Nw = 0 ; resetting window numbering
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/grslopeVSuislope.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'gr slope VS ui slope'
   thickall = 2
endelse
; setting plot range
XR = [-0.25,1.25]
YR = [0.25,1.2]
if SM1 eq 1 and WN eq 0 then begin 
   XR = XR - 1
   YR = YR - 1
endif 

if WN eq 1 and SM1 eq 0 then begin 
   XR = [0.4,1.8]
   YR = [0.6,1.6]
endif 

if SM1 eq 1 and WN eq 1 then begin 
   XR = XR - 1 ; [-1.8,0.0]
   YR = YR - 1 
   ;XR = [-0.4,0.2]
   ;YR = [-0.4,0.2]
endif


DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('s_{ui}') $
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

POLYLIM = 1.
if SM1 eq 1 then POLYLIM = 0
POLYFILL,[XR[0],POLYLIM,POLYLIM,XR[0]],[YR[0],YR[0],POLYLIM,POLYLIM],col=col.lightgray

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA,/NOERASE $
        , xtitle =textoidl('s_{ui}') $
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

oplot,findgen(10)-5,findgen(10)-5,thick=thickall,linestyle=2,col=col.black
;oplot,findgen(10)-8,fltarr(10)+1,thick=thickall,linestyle=2,col=col.black
;oplot,fltarr(10)+1,findgen(10)-8,thick=thickall,linestyle=2,col=col.black

PLOTSYM,0,0.7,/fill
oplot,UIdat(plotent).abest,GRdat(plotent).abest,thick=thickall,psym=8,col=col.darkgray

;-- plotting contours --
contourarray,UIdat(plotent).abest,XR[0],XR[1],GRdat(plotent).abest,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
contour,contarr2,xrange,yrange,/overplot $
	, levels=[5.,15.,30.,50.] $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=1.5 $ 
        , thick=thickall
;------------------------

if SN eq 1 then XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],textoidl('objects with \deltas_{gr} < ')+trim(SNlim),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
;XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'epoch color  : ',col=col.black,charsize=1.5,charthick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: plotGRvsUIfits.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
