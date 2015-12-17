;+
;----------------------------
;   NAME
;----------------------------
; plottestRepickfit.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure plotting the resutls save to the idl save file in testRepickfit.pro
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; savefile         : IDL save file with data to plot 
; initialval       : vectore containing the intial slope and offset [slope,offset]
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ERR             : indicate which convidence intervals to plot (ERR=95 or ERR=68).
;                   Default is no convidence intervals (error bars) shown.
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plottestRepickfit,'testRepickfitfiles/FitresultANDdataarrays_Nrepicks250_Derr0p500000.idlsave',[0.76,0.0],ERR=68,/verbose
; IDL> plottestRepickfit,'testRepickfitfiles/FitresultANDdataarrays_Nrepicks250_Derr1p00000.idlsave',[0.76,0.0],ERR=68,/verbose
; IDL> plottestRepickfit,'testRepickfitfiles/FitresultANDdataarrays_Nrepicks250_Derr3p00000.idlsave',[0.76,0.0],ERR=68,/verbose
; IDL> plottestRepickfit,'testRepickfitfiles/FitresultANDdataarrays_Nrepicks250_Derr5p00000.idlsave',[0.76,0.0],ERR=68,/verbose
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-18  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
; @
;----------------------------
;-
PRO plottestRepickfit,savefile,initialval,ERR=ERR,EPS=EPS,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

if n_elements(ERR) eq 0 then ERR = 0

restore,savefile                      ; restoring IDL save file. See testRepickfit.pro for details on the content
Nrepicks = n_elements(RESTOT(0,*))    ; number of data sets in save file
BYTCOL   = BYTSCL(FINDGEN(Nrepicks))  ; array used to color datasets
MEANX = MEAN(DATARRAY(*,0))
MEANY = MEAN(DATARRAY(*,1))

slope  = initialval[0]
offset = initialval[1]

Nw = 0
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'perfectdata.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'The intial perfect data set'
   thickall = 2
endelse
; setting plot range
XR = [max(datarray(*,0))+0.2, min(datarray(*,0))-0.2]
YR = [max(datarray(*,1))+0.2, min(datarray(*,1))-0.2]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,DATARRAY(*,0),DATARRAY(*,1), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('"g mag"') $
        , ytitle ='"r mag"' $
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
LOADCT,39
device,decomposed=0
for jj=0,Nrepicks-1 do begin
;for jj=30,30 do begin
   oplot,DATTOT(*,0+jj*4),DATTOT(*,1+jj*4),psym=8,thick=thickall,col=BYTCOL(jj)  ;color24([BYTCOL(jj),BYTCOL(jj),BYTCOL(jj)])
   ;oploterror,DATTOT(*,0+jj*4),DATTOT(*,1+jj*4),DATTOT(*,2+jj*4),DATTOT(*,3+jj*4),psym=3,errthick=thickall,col=BYTCOL(jj),errcol=BYTCOL(jj)
endfor
col = getcolor(/load)

oplot,DATARRAY(sort(DATARRAY(*,0)),0),(slope*(DATARRAY(sort(DATARRAY(*,0)),0)-MeanX)+offset)+meanY,linestyle=0,col=col.black,thick=thickall
PLOTSYM,0,1.5,/FILL
oplot,datarray(*,0),datarray(*,1),psym=8,thick=thickall,col=col.black
oploterror,datarray(*,0),datarray(*,1),datarray(*,2),datarray(*,3),psym=3,errthick=thickall,col=col.black,errcol=col.black

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopeVSoffsetPlane.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS offset'
   thickall = 2
endelse
; setting plot range
XR = [min(RESTOT(0,*))-0.1,max(RESTOT(0,*))+0.1]
YR = [min(RESTOT(7,*))-0.01,max(RESTOT(7,*))+0.01]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,DATARRAY(*,0),DATARRAY(*,1), col=col.black    $
        , /NODATA $
        , xtitle ='"gr" slope' $
        , ytitle ='offset' $
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

PLOTSYM,0,2.5,/FILL
oplot,fltarr(2)+slope,fltarr(2)+offset,psym=8,thick=thickall,col=col.black

LOADCT,39
device,decomposed=0

PLOTSYM,0,1.0,/FILL
xx = fltarr(2)
for jj=0,Nrepicks-1 do begin
   oplot,xx+RESTOT(0,jj),xx+RESTOT(7,jj),psym=8,thick=thickall,col=BYTCOL(jj)

   if ERR eq 68 then begin ; plotting 68% confidence intervals if requested
      oploterror,xx+RESTOT(0,jj),xx+RESTOT(7,jj),xx+RESTOT(3,jj),xx+RESTOT(10,jj),/LOBAR,errcol=BYTCOL(jj),errthick=thickall,psym=8,col=BYTCOL(jj)
      oploterror,xx+RESTOT(0,jj),xx+RESTOT(7,jj),xx+RESTOT(4,jj),xx+RESTOT(11,jj),/HIBAR,errcol=BYTCOL(jj),errthick=thickall,psym=8,col=BYTCOL(jj)
   endif

   if ERR eq 95 then begin ; plotting 95% confidence intervals if requested
      oploterror,xx+RESTOT(0,jj),xx+RESTOT(7,jj),xx+RESTOT(5,jj),xx+RESTOT(12,jj),/LOBAR,errcol=BYTCOL(jj),errthick=thickall,psym=8,col=BYTCOL(jj);,errstyle=2
      oploterror,xx+RESTOT(0,jj),xx+RESTOT(7,jj),xx+RESTOT(6,jj),xx+RESTOT(13,jj),/HIBAR,errcol=BYTCOL(jj),errthick=thickall,psym=8,col=BYTCOL(jj);,errstyle=2
   endif
endfor
col=getcolor(/load)

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: plottestRepickfit.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
