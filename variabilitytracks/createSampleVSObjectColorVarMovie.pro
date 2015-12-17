;+
;----------------------------
;   NAME
;----------------------------
; createSampleVSObjectColorVarMovie.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure taking the output for plot 19 in plotDavisVSstripe82VSshen.pro
; and turning it into a movie.
;----------------------------
;   COMMENTS
;----------------------------
; The program actually doesn't create the frames for a movie
; but rather enables to plot the individual parts of the plot so they
; can be animated in keynote.
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : The save file create in plot 19 of plotDavisVSstripe82VSshen.pro
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /EPS            : set /EPS to write plots to .eps files
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> createSampleVSObjectColorVarMovie,'dataforplot19movie111222.sav',/VERBOSE,/STP
; IDL> createSampleVSObjectColorVarMovie,'dataforplot19movie.sav',/VERBOSE,/STP
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-12-30  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
@ linearfitMCMC.pro
;----------------------------
;-
PRO createSampleVSObjectColorVarMovie,datafile,EPS=EPS,VERBOSE=VERBOSE,STP=STP

PS = n_elements(EPS)
VB = n_elements(VERBOSE)



restore,datafile  ;  restoring variables

; M and z range
MassMin = 8.6
MassMax = 9.0
Zmin    = 1.0
Zmax    = 1.75

; fit to sample <g> and <r>
RESULT    = [0.989236,0.986326,0.988782,0.0659401,0.0468968,0.128979,0.0819829,0.000377031,-0.00281466,-0.00148977,0.154477,0.108195,0.323332,0.185342]
resultEC  = [0.989236,0.989236,0.977737,0.0756036,0.0783952,0.0829890,0.0783952,0.000377031,0.000377031,0.0120618,0.00975794,0.0685961,0.0171945,0.0685961]


; Individual slopes fit
medBIN = 0.81921347
minSLOPE = 0.755460
maxSLOPE = 0.843329

fit = 'gr'

NW = 0
;=============================================================================================
Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/plot19movie.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'movie frame number '+trim(66)
   thickall = 2
endelse
; setting plot range
XR = [22,18]
YR = [22,18]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
XT = STRMID(fit, 0, 1)
YT = STRMID(fit, 1, 1)

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='<'+XT+'>' $
        , ytitle ='<'+YT+'>' $
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

   meansam_r    = fltarr(Cmassobj)
   meansam_g    = fltarr(Cmassobj)
   meansam_rerr = fltarr(Cmassobj)
   meansam_gerr = fltarr(Cmassobj)


for ii=0,Cmassobj-1 do begin         ; looping over selected objects
;for ii=0,5-1 do begin         ; looping over selected objects

   if ROUND(ii/10.) eq ii/10. then begin 

   Objent   = where(Qdata.headobjid eq IDS(Dmassent(ii)),Nent)

   gDAT     = Qdata(Objent).psfmag_g
   rDAT     = Qdata(Objent).psfmag_r
   gDATERR  = Qdata(Objent).psfmagerr_g
   rDATERR  = Qdata(Objent).psfmagerr_r

   meansam_r(ii)    = mean(rDAT)                     ; storing mean value
   meansam_g(ii)    = mean(gDAT)                     ; storing mean value
   meansam_rerr(ii) = sqrt(total(rDATERR^2.)/Nent)   ; propagated errors ; stdev(rDAT) ; setting error as standard dev.
   meansam_gerr(ii) = sqrt(total(gDATERR^2.)/Nent)   ; propagated errors ; stdev(gDAT) ; setting error as standard dev.

   aobj     = acorrected0(Dmassent(ii)) ; Qcat(Dmassent(ii)).abest
   bobj     = Qcat(Dmassent(ii)).bbest

      lines = 1
      if lines eq 1 then begin
         Llength  = 2.*stdev(gDAT)                         ; desired length of line
         Dxline   = Llength/(1+aobj)                    ; the size of xaxis projection
         Dyline   = Llength*aobj/(1+aobj)               ; the size of yaxis projection
         bprime   = meansam_r(ii)-aobj*meansam_g(ii)+bobj               ; b' after rewriting r-<r> = a*(g-<g>)+b  to  r = a*g + b'
         xvecline = [meansam_g(ii)-Dxline/2.,meansam_g(ii)+Dxline/2.]   ; x component of line
         yvecline = [meansam_r(ii)-Dyline/2.,meansam_r(ii)+Dyline/2.]   ; y component of lien
      endif

      if lines eq 1 then oplot,xvecline,yvecline,thick=thickall,col=col.red
      if lines eq 1 then ARROW,xvecline(1),yvecline(1),xvecline(0),yvecline(0),thick=thickall,col=col.red,/data,HSIZE=!D.X_SIZE / 64. / 1.1
      if lines eq 1 then ARROW,xvecline(0),yvecline(0),xvecline(1),yvecline(1),thick=thickall,col=col.red,/data,HSIZE=!D.X_SIZE / 64. / 1.1

      PLOTSYM,0,1.5,/fill
      oplot,fltarr(2)+meansam_g(ii),fltarr(2)+meansam_r(ii),thick=thickall,col=col.black,psym=8

      if ROUND(ii/300.) eq ii/300. and VB eq 1 then print,systime(0),' Object ',ii
   endif
endfor

col = getcolor(/load)

meanmeansam_g = 20.0768
meanmeansam_r = 19.7979

PL1 = 0
if PL1 eq 1 then begin
; plotting mean-sample fit
NX = 30
XX = findgen(NX)-15
     ;oplot,XX+mean(meansam_g(*)),result(0)*XX+mean(meansam_r(*))+result(7),linestyle=0,thick=thickall,col=col.black
oplot,XX+meanmeansam_g,result(0)*XX+meanmeansam_r+result(7),linestyle=0,thick=thickall,col=col.black

; overplotting 'error-lines' 
     ;oplot,XX(NX/2.:NX-1)+mean(meansam_g(*)),(RESULT(0)+RESULTec(4))*XX(NX/2.:NX-1)+mean(meansam_r(*))+(RESULT(7)+RESULTec(11)),linestyle=2,thick=thickall,col=col.black
     ;oplot,XX(0:NX/2.)   +mean(meansam_g(*)),(RESULT(0)+RESULTec(4))*XX(0:NX/2.) +mean(meansam_r(*))+(RESULT(7)-RESULTec(10)),linestyle=2,thick=thickall,col=col.black
     ;oplot,XX(0:NX/2.)   +mean(meansam_g(*)),(RESULT(0)-RESULTec(3))*XX(0:NX/2.) +mean(meansam_r(*))+(RESULT(7)+RESULTec(11)),linestyle=2,thick=thickall,col=col.black
     ;oplot,XX(NX/2.:NX-1)+mean(meansam_g(*)),(RESULT(0)-RESULTec(3))*XX(NX/2.:NX-1)+mean(meansam_r(*))+(RESULT(7)-RESULTec(10)),linestyle=2,thick=thickall,col=col.black
oplot,XX(NX/2.:NX-1)+meanmeansam_g,(RESULT(0)+RESULTec(4))*XX(NX/2.:NX-1)+meanmeansam_r+(RESULT(7)+RESULTec(11)),linestyle=2,thick=thickall,col=col.black
oplot,XX(0:NX/2.)   +meanmeansam_g,(RESULT(0)+RESULTec(4))*XX(0:NX/2.) +meanmeansam_r+(RESULT(7)-RESULTec(10)),linestyle=2,thick=thickall,col=col.black
oplot,XX(0:NX/2.)   +meanmeansam_g,(RESULT(0)-RESULTec(3))*XX(0:NX/2.) +meanmeansam_r+(RESULT(7)+RESULTec(11)),linestyle=2,thick=thickall,col=col.black
oplot,XX(NX/2.:NX-1)+meanmeansam_g,(RESULT(0)-RESULTec(3))*XX(NX/2.:NX-1)+meanmeansam_r+(RESULT(7)-RESULTec(10)),linestyle=2,thick=thickall,col=col.black

     ;XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'Sample MCMC fit: -0.01',col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
endif

PL2 = 1
if PL2 eq 1 then begin
; plotting individual sample fit
     ;XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'Mean of individual slopes: -0.18',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0

NX = 30
XX = findgen(NX)-15
     ;oplot,xx+mean(meansam_g(*)),medBIN  *xx+mean(meansam_r(*))+RESULT(10),thick=thickall,linestyle=0,col=col.red
     ;oplot,xx+mean(meansam_g(*)),minSLOPE*xx+mean(meansam_r(*))+RESULT(10),thick=thickall,linestyle=2,col=col.red
     ;oplot,xx+mean(meansam_g(*)),maxSLOPE*xx+mean(meansam_r(*))+RESULT(10),thick=thickall,linestyle=2,col=col.red
oplot,xx+meanmeansam_g,medBIN  *xx+meanmeansam_r+RESULT(10),thick=thickall,linestyle=0,col=col.red
oplot,xx+meanmeansam_g,minSLOPE*xx+meanmeansam_r+RESULT(10),thick=thickall,linestyle=2,col=col.red
oplot,xx+meanmeansam_g,maxSLOPE*xx+meanmeansam_r+RESULT(10),thick=thickall,linestyle=2,col=col.red

;By hand (33rd--66th percentile of masses and the 25th--75th percentile of redshifts):
;XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],textoidl('8.6 < log(M_{BH}) < 9.0'),col=col.black,charsize=2.5,charthick=thickall
;XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],textoidl('1.0 < z < 1.75'),col=col.black,charsize=2.5,charthick=thickall
;XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('N_{obj} = ')+trim(Cmassobj),col=col.black,charsize=2.5,charthick=thickall
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: createSampleVSObjectColorVarMovie.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
