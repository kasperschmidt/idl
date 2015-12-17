;+
;----------------------------
;   NAME
;----------------------------
; plotObservationHist.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Plotting MJD hisotograms of a fits file
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : string containing name and path of input data file
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
; IDL> plotObservationHist,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010.fits',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-18  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotObservationHist,datafile,EPS=EPS,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

s = mrdfits(datafile,1)

Nw = 0
;=============================================================================================
; = = = MJD hist = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'obsMJDhist.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=15;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=800, ysize=500, title = 'Mean color magnitude plot'
   thickall = 2
endelse
; setting plot range
offset = 51000.
XR = [min(s.MJD_u)-offset-100,max(s.MJD_u)-offset+100]
YR = [0,16000.]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,s.MJD_u,s.MJD_u, col=col.black    $
        , /NODATA $
        , xtitle ='MJD - 51000 [days]' $
        , ytitle ='N' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

plothist,(s.mjd_r-offset),xh,yh,BIN=15.,col=col.black,thick=thickall,/fill,Fcolor=col.blue,/overplot


sep1st99   = 51422
jan1st00   = 51573

for ii=0,9 do begin
   oplot,[sep1st99+ii*365,sep1st99+ii*365]-offset,[YR(0),YR(1)],col=col.black,thick=thickall,linestyle=2
   oplot,[jan1st00+ii*365,jan1st00+ii*365]-offset,[YR(0),YR(1)],col=col.black,thick=thickall,linestyle=1
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: plotObservationHist.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
