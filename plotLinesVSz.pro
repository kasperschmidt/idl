;+
;----------------------------
;   NAME
;----------------------------
; plotLinesVSz.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; procedure plotting the "appearance" of a set of (hardcoded) lines 
; to indicate when they will enter the HST WFC3 G141 passband
; (1.05-1.7 micron)
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
; zin             : set to some redshift. The code will then print the
;                   lines appearing in the G141 spectral range for
;                   that redshift (and mark it on the plot)
; IMGPATH         : Give string with path for eps files if not tobe
;                   put in present directory (end with slash)
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotLinesVSz,zin=1.5
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-10-14  started by K. B. Schmidt (MPIA)
; 2011-03-24 IMGPATH keyword added. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO  plotLinesVSz,ZIN=ZIN,EPS=EPS,VERBOSE=VERBOSE,IMGPATH=IMGPATH

ZZ = n_elements(ZIN)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

zrange = findgen(10000)/2000+1    ; (redshift+1) range

; lines to add (taken from x_specplot galaxy template)
OII     = 0.3727
A4break = 0.4  ; 4000 A break
Hgamma  = 0.4342
Hbeta   = 0.4863 ; 0.4861
OIII1   = 0.4960 ; 0.5007 ; doublet at 4960.2 & 5008.2
OIII2   = 0.5008 
Halpha  = 0.6563
SII1    = 0.6719
SII2    = 0.6733

if ZZ eq 0 then ZIN = 'NONE'

if ZZ eq 1 then begin
   if vb eq 1 then print,':: plotLinesVSz.pro :: The following lines are present in the G141 spectrum for z=',strtrim(zin,2),':'
   if vb eq 1 and OII*(zin+1) gt 1.05 and OII*(zin+1) lt 1.7 then print,'    OII             @  0.3727 micron ->  ',strtrim(OII*(zin+1),2),' micron'
   if vb eq 1 and A4break*(zin+1) gt 1.05 and A4break*(zin+1) lt 1.7 then print,'    4000 A break    @  0.4000 micron ->  ',strtrim(A4break*(zin+1),2),' micron'
   if vb eq 1 and Hgamma*(zin+1) gt 1.05 and Hgamma*(zin+1) lt 1.7 then print,'    H gamma         @  0.4342 micron ->  ',strtrim(Hgamma*(zin+1),2),' micron'
   if vb eq 1 and Hbeta*(zin+1) gt 1.05 and Hbeta*(zin+1) lt 1.7 then print,'    H beta          @  0.4863 micron ->  ',strtrim(Hbeta*(zin+1),2),' micron'
   if vb eq 1 and OIII1*(zin+1) gt 1.05 and OIII1*(zin+1) lt 1.7 then print,'    OIII 1          @  0.4960 micron ->  ',strtrim(OIII1*(zin+1),2),' micron'
   if vb eq 1 and OIII2*(zin+1) gt 1.05 and OIII2*(zin+1) lt 1.7 then print,'    OIII 2          @  0.5008 micron ->  ',strtrim(OIII2*(zin+1),2),' micron'
   if vb eq 1 and Halpha*(zin+1) gt 1.05 and Halpha*(zin+1) lt 1.7 then print,'    H alpha         @  0.6563 micron ->  ',strtrim(Halpha*(zin+1),2),' micron'
   if vb eq 1 and SII1*(zin+1) gt 1.05 and SII1*(zin+1) lt 1.7 then print,'    SII 1           @  0.6710 micron ->  ',strtrim(SII1*(zin+1),2),' micron'
   if vb eq 1 and SII2*(zin+1) gt 1.05 and SII2*(zin+1) lt 1.7 then print,'    SII 2           @  0.6733 micron ->  ',strtrim(SII2*(zin+1),2),' micron'
endif


Nw = 4
;=============================================================================================
; = = = lam vs z = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'G141lines_z'+trim(zin)+'.eps'
   if n_elements(IMGPATH) ne 0 then plot1 = trim(IMGPATH)+'G141lines_z'+trim(zin)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=800, ysize=700, title = 'lam vs z'
   thickall = 2
endelse
; setting plot range
XR = [1,5]
YR = [0.9,1.9]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,zrange,zrange, col=col.black    $
        , /NODATA $
        , xtitle ='(1+z)' $
        , ytitle =textoidl('\lambda (\mum)') $
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

oplot,XR,[1.05,1.05],linestyle=2,thick=thickall,col=col.gray
oplot,XR,[1.7,1.7],linestyle=2,thick=thickall,col=col.gray


oplot,zrange,OII*zrange,linestyle=0,thick=thickall,col=col.purple
oplot,zrange,A4break*zrange,linestyle=0,thick=thickall,col=col.blue
oplot,zrange,Hgamma*zrange,linestyle=0,thick=thickall,col=col.cyan
oplot,zrange,Hbeta*zrange,linestyle=0,thick=thickall,col=col.green
oplot,zrange,OIII1*zrange,linestyle=0,thick=thickall,col=col.yellow
oplot,zrange,OIII2*zrange,linestyle=0,thick=thickall,col=col.yellow
oplot,zrange,Halpha*zrange,linestyle=0,thick=thickall,col=col.orange
oplot,zrange,SII1*zrange,linestyle=0,thick=thickall,col=col.red
oplot,zrange,SII2*zrange,linestyle=0,thick=thickall,col=col.red



XYOUTS,XR[0]+0.04*DX,YR[0]+0.82*DY,'G141 range',col=col.gray,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.75*DY,'OII',col=col.purple,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.70*DY,textoidl('4000 \AA break'),col=col.blue,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.65*DY,textoidl('H_\gamma'),col=col.cyan,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.60*DY,textoidl('H_\beta'),col=col.green,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.55*DY,'OIII',col=col.yellow,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.50*DY,textoidl('H_\alpha'),col=col.orange,charthick=thickall,charsize=2.0
XYOUTS,XR[0]+0.04*DX,YR[0]+0.45*DY,'SII',col=col.red,charthick=thickall,charsize=2.0

if ZZ eq 1 then begin
   oplot,[zin+1,zin+1],YR,linestyle=2,thick=thickall,col=col.black
   XYOUTS,zin+1.2,YR[0]+0.9*DY,'z='+strtrim(zin,2),col=col.black,charthick=thickall,charsize=2.0
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================



if vb eq 1 then print,' '
if vb eq 1 then print,':: plotLinesVSz.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
