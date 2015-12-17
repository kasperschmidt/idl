;+
;----------------------------
;   NAME
;----------------------------
; plotPDFs.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure plotting the probability distribution functions (PDFs)
; given as output from linearfitMCMC.pro
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; PDFfile         : string containing name and path of input PDF file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Objectdata      : An array containing the [[x],[y],[dx],[dy],[dxy],[dyx]] values
;                   for the data fitted where
;                     x    = x-components
;                     y    = y-components
;                     dx   = x-component uncertainties
;                     dy   = y-component uncertainties
;                     dxy  = the covariance between x and y (set to 0 if none)
;                     dxy  = the covariance between y and x (set to 0 if none)
; /OUTLIERS       : set /OUTLIERS if the file contains PDFs from pruning outliers
; /ALLINONE       : set /ALLINONE to create a mosaic of all plots
; /MEANSHIFT      : set /MEANSHIFT to subtract the mean values of the
;                   data (shift them to (0,0)) if the PDFs were created with the
;                   /INITGUESSALT keyword, i.e. the b-values are around 0
; EPS             : set EPS to a sting with the 'base name' to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; data=[[201,244,47,287,203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[592,401,583,402,495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]]
;COVAR=[-0.84,0.31,0.64,-0.27,-0.33,0.67,-0.02,-0.05,-0.84,-0.69,0.30,-0.46,-0.03,0.50,0.73,-0.52,0.90,0.40,-0.78,-0.56]*sqrt([9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5]*[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.])

; IDL> plotPDFs,'test.dat',objectdata=[[data],[covar],[covar]],/VERBOSE,/OUTLIERS   ,EPS='test'



; SEED=10 & NP=10 & mag1=RANDOMN(seed,NP)*0.3+18 & mag2=RANDOMN(seed,NP)*0.3+19 & dmag1=abs(RANDOMN(seed,NP)*0.2+0.0) & dmag2=abs(RANDOMN(seed,NP)*0.2+0) & datarray=[[mag1],[mag2],[dmag1],[dmag2]] 
; IDL> plotPDFs,'test.dat',objectdata=[[datarray],[fltarr(NP)],[fltarr(NP)]],/VERBOSE,/OUTLIERS,/ALLINONE   ,EPS='test'

;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-12-01  started by K. B. Schmidt (MPIA)
; 2010-12-06  /MEANSHIFT keyword added. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO  plotPDFs,PDFfile,OBJECTDATA=OBJECTDATA,OUTLIERS=OUTLIERS,EPS=EPS,VERBOSE=VERBOSE,ALLINONE=ALLINONE,MEANSHIFT=MEANSHIFT

OBJ = n_elements(OBJECTDATA)
OUT = n_elements(OUTLIERS)
AIO = n_elements(ALLINONE)
MS  = n_elements(MEANSHIFT)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)

if OUT eq 0 then begin
   FMT = ('f,f,f')
   readcol,format=FMT,PDFfile,bb,mm,KlnL,comment='#'
endif else begin
   FMT = ('f,f,f,f,f,f,f,f')
   readcol,format=FMT,PDFfile,bb,mm,KlnL,Pb,Xb,Yb,Vbx,Vby,corr,comment='#'   
endelse

Nlines     = n_elements(bb)  ; number of MCMC loops in file
Nlinesplot = 100.            ; Number of loops to plot

Nfake = 100.
Xvec  = findgen(Nfake)/100*1000-500   ; x-components to draw lines

if OBJ ne 0 then begin
   DATARRAY = OBJECTDATA
   Ndat = n_elements(DATARRAY(*,0))  ; the number of data points

   if MS eq 1 then begin
      ; Shifting data to (0,0)
      Mxcomp          = mean(DATARRAY(*,0))  ; mean value of x components
      Mycomp          = mean(DATARRAY(*,1))  ; mean value of x components   
      DATARRAY(*,0)   = DATARRAY(*,0) - Mxcomp
      DATARRAY(*,1)   = DATARRAY(*,1) - Mycomp
   endif 
endif

ent     = where(KlnL eq min(KlnL))
bestent = ent(0)  ; entry of fit with lowest 'chi^2'


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; calculating mean and median values plus confidence intervals
RESULT     = fltarr(14)
; entries for plus and minus confidence intervals
ent68m     = round(Nlines*0.16)
ent68p     = round(Nlines*0.84)
ent95m     = round(Nlines*0.025)
ent95p     = round(Nlines*0.975)

RESULT(0)  = mm(bestent)                          ; the value with lowest LnL
RESULT(1)  = median(mm)                           ; the median
RESULT(2)  = mean(mm)                             ; the mean
msort      = mm(sort(mm))                         ; sorting a values
RESULT(3)  = RESULT(1)-msort(ent68m)
RESULT(4)  = msort(ent68p)-RESULT(1)
RESULT(5)  = RESULT(1)-msort(ent95m)
RESULT(6)  = msort(ent95p)-RESULT(1)

RESULT(7)  = bb(bestent)                          ; the value with lowest LnL
RESULT(8)  = median(bb)                           ; the median
RESULT(9)  = mean(bb)                             ; the mean
bsort      = bb(sort(bb))                         ; sorting b values
RESULT(10) = RESULT(8)-bsort(ent68m)
RESULT(11) = bsort(ent68p)-RESULT(8)
RESULT(12) = RESULT(8)-bsort(ent95m)
RESULT(13) = bsort(ent95p)-RESULT(8)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Nw = 0
;=============================================================================================
;= = = Slope Histogram = = =
!p.multi = [0,0,0]
if PS ne 0 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'MCMCsteps_'+strtrim(EPS,2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'MCMCsteps'
   thickall = 2
endelse

; setting plot range
if OBJ eq 0 then begin
   XR    = [min(Xvec),max(Xvec)]
   YR    = [2*min(bb),2*max(bb)]
endif else begin
   DDATX = max(DATARRAY(*,0))-min(DATARRAY(*,0))
   DDATY = max(DATARRAY(*,1))-min(DATARRAY(*,1))
   XR    = [min(DATARRAY(*,0))-0.1*DDATX,max(DATARRAY(*,0))+0.1*DDATX]
   YR    = [min(DATARRAY(*,1))-0.1*DDATY,max(DATARRAY(*,1))+0.1*DDATY]

   XR = [XR[1],XR[0]]  ; flipping axes for magnitudes
   YR = [YR[1],YR[0]]  ; flipping axes for magnitudes
   XR = [0.5,-0.5]
   YR = [0.6,-0.5]
endelse

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='g-<g>' $
        , ytitle ='r-<r>' $
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

   for ii=1L,Nlines-1 do begin ; looping over objects and plotting Nlinesplot of the MCMC results
      NN = round(Nlines/Nlinesplot)+0.0
      if ii/NN eq ceil(ii/NN) then oplot,Xvec,mm(ii)*Xvec+bb(ii),linestyle=0,col=col.green,thick=thickall
   endfor

   ;oplot,Xvec,mm(0)*Xvec+bb(0),linestyle=2,col=col.blue,thick=thickall
   oplot,Xvec,mm(bestent)*Xvec+bb(bestent),linestyle=0,col=col.red,thick=thickall

   if OBJ ne 0 then begin ; if data of object is given
      PLOTSYM,0,1.5,/fill
      oplot,DATARRAY(*,0),DATARRAY(*,1),psym=8,thick=thickall,col=col.black
      for kk=0,Ndat-1 do begin ; drawing error ellipse around points
         cent  = [DATARRAY(kk,0),DATARRAY(kk,1)]
         arr   = [[DATARRAY(kk,2),DATARRAY(kk,5)],[DATARRAY(kk,4),DATARRAY(kk,3)]]   ; covariance matrix for kk'th object
         eval  = EIGENQL(arr,EIGENVECTORS=evec)  ; calculating the eigenvalues and eigenvectors of covariance matrix
         rad   = [DATARRAY(kk,2),DATARRAY(kk,3)]
;         rad   = [2*sqrt(eval(0)),2*sqrt(eval(1))]
         arot  = atan(evec(0,0)/evec(0,1))  ; rotation angle
         ell   = ELLIPSE(cent,rad,arot=arot)
         oplot,ell(0,*),ell(1,*),linestyle=0,thick=thickall,col=col.black
      endfor
   endif
if PS ne 0 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;= = = A vs B = = =
!p.multi = [0,0,0]
if PS ne 0 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'aVSb_PDF_'+strtrim(EPS,2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'MCMCsteps'
   thickall = 2
endelse

; setting plot range
XR = [min(bb),max(bb)]
YR = [min(mm),max(mm)]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='b in a*g+b' $
        , ytitle ='a in a*g+b' $
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

   PLOTSYM,0,0.5,/fill
   oplot,bb,mm,psym=8,col=col.black

   contourarray,bb,min(bb),max(bb),mm,min(mm),max(mm),40,40,4,contarr,levelbin,xrange,yrange
   contour,contarr,xrange,yrange,/overplot,levels=levelbin, C_COLOR=[col.green],thick=thickall

   xx = fltarr(2)
   PLOTSYM,0,1.5,/FILL
   oplot  ,xx+bb(0),xx+mm(1),psym=8,col=col.blue              ; overplotting intial guess
   oplot  ,xx+RESULT(7),xx+RESULT(0),psym=8,col=col.red       ; overplotting best fit
   oplot  ,xx+RESULT(9),xx+RESULT(2),psym=8,col=col.orange    ; overplotting mean fit
   oplot  ,xx+RESULT(8),xx+RESULT(1),psym=8,col=col.magenta   ; overplotting median fit
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(10),xx+RESULT(3),/LOBAR,errcol=col.magenta,errthick=thickall
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(11),xx+RESULT(4),/HIBAR,errcol=col.magenta,errthick=thickall
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(12),xx+RESULT(5),/LOBAR,errcol=col.magenta,errthick=thickall,errstyle=2
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(13),xx+RESULT(6),/HIBAR,errcol=col.magenta,errthick=thickall,errstyle=2

   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.95,"PDF median fit with 68% and 95% confidence intervals",col=col.magenta,charsize=1.5,charthick=thickall
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.90,"PDF mean fit",col=col.orange,charsize=1.5,charthick=thickall
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.85,"Best fit",col=col.red,charsize=1.5,charthick=thickall
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.80,"Initial guess",col=col.blue,charsize=1.5,charthick=thickall

   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.05,"N!Dpoints!N = "+strtrim(Nlines,2),col=col.black,charsize=1.5,charthick=thickall
stop
if PS ne 0 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

if out ne 0 then begin ; if file contains results for pruning of ouliers -- continue
;=============================================================================================
;= = = Pb PDF = = =
!p.multi = [0,0,0]
if PS ne 0 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'Pb_PDF_'+strtrim(EPS,2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'MCMCsteps'
   thickall = 2
endelse

histbin = 0.01
plothist,Pb,Xhist,Yhist,bin=histbin,/noplot
; setting plot range
XR    = [min(Xhist),max(Xhist)]
YR    = [0,max(yhist)+0.05*max(yhist)]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='Pb' $
        , ytitle ='#' $
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

plothist,Pb,Xhist,Yhist,bin=histbin,col=col.black,thick=thickall,/overplot

if PS ne 0 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; --- ALLINONE plot ---
if AIO eq 1 then begin

!p.multi = [0,2,2]
if PS ne 0 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'ALLINONE_'+strtrim(EPS,2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=45, ysize=45;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1000, ysize=800, title = 'ALLINONE'
   thickall = 2
endelse
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;= = = MCMC loops = = =
; setting plot range
if OBJ eq 0 then begin
   XR    = [min(Xvec),max(Xvec)]
   YR    = [2*min(bb),2*max(bb)]
endif else begin
   DDATX = max(DATARRAY(*,0))-min(DATARRAY(*,0))
   DDATY = max(DATARRAY(*,1))-min(DATARRAY(*,1))
   XR    = [min(DATARRAY(*,0))-0.1*DDATX,max(DATARRAY(*,0))+0.1*DDATX]
   YR    = [min(DATARRAY(*,1))-0.1*DDATY,max(DATARRAY(*,1))+0.1*DDATY]

   XR = [XR[1],XR[0]]
   YR = [YR[1],YR[0]]
endelse

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='g' $
        , ytitle ='r' $
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

   for ii=1L,Nlines-1 do begin ; looping over objects and plotting Nlinesplot of the MCMC results
      NN = round(Nlines/Nlinesplot)+0.0
      if ii/NN eq ceil(ii/NN) then oplot,Xvec,mm(ii)*Xvec+bb(ii),linestyle=0,col=col.green,thick=thickall
   endfor

   oplot,Xvec,mm(0)*Xvec+bb(0),linestyle=2,col=col.blue,thick=thickall
   oplot,Xvec,mm(bestent)*Xvec+bb(bestent),linestyle=0,col=col.red,thick=thickall

   if OBJ ne 0 then begin ; if data of object is given
      PLOTSYM,0,1.5,/fill
      oplot,DATARRAY(*,0),DATARRAY(*,1),psym=8,thick=thickall,col=col.black
      for kk=0,Ndat-1 do begin ; drawing error ellipse around points
         cent  = [DATARRAY(kk,0),DATARRAY(kk,1)]
         arr   = [[DATARRAY(kk,2),DATARRAY(kk,5)],[DATARRAY(kk,4),DATARRAY(kk,3)]]   ; covariance matrix for kk'th object
         eval  = EIGENQL(arr,EIGENVECTORS=evec)  ; calculating the eigenvalues and eigenvectors of covariance matrix
         rad   = [DATARRAY(kk,2),DATARRAY(kk,3)]
;         rad   = [2*sqrt(eval(0)),2*sqrt(eval(1))]
         arot  = atan(evec(0,0)/evec(0,1))  ; rotation angle
         ell   = ELLIPSE(cent,rad,arot=arot)
         oplot,ell(0,*),ell(1,*),linestyle=0,thick=thickall,col=col.black
      endfor
   endif

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;= = = AvsB plot = = =
; setting plot range
XR = [min(bb),max(bb)]
YR = [min(mm),max(mm)]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='b in a*g+b' $
        , ytitle ='a in a*g+b' $
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

   PLOTSYM,0,0.5,/fill
   oplot,bb,mm,psym=8,col=col.black

   contourarray,bb,min(bb),max(bb),mm,min(mm),max(mm),40,40,4,contarr,levelbin,xrange,yrange
   contour,contarr,xrange,yrange,/overplot,levels=levelbin, C_COLOR=[col.green],thick=thickall

   xx = fltarr(2)
   PLOTSYM,0,1.5,/FILL
   oplot  ,xx+bb(0),xx+mm(1),psym=8,col=col.blue              ; overplotting intial guess
   oplot  ,xx+RESULT(7),xx+RESULT(0),psym=8,col=col.red       ; overplotting best fit
   oplot  ,xx+RESULT(9),xx+RESULT(2),psym=8,col=col.orange    ; overplotting mean fit
   oplot  ,xx+RESULT(8),xx+RESULT(1),psym=8,col=col.magenta   ; overplotting median fit
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(10),xx+RESULT(3),/LOBAR,errcol=col.magenta,errthick=thickall
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(11),xx+RESULT(4),/HIBAR,errcol=col.magenta,errthick=thickall
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(12),xx+RESULT(5),/LOBAR,errcol=col.magenta,errthick=thickall,errstyle=2
   oploterror,xx+RESULT(8),xx+RESULT(1),xx+RESULT(13),xx+RESULT(6),/HIBAR,errcol=col.magenta,errthick=thickall,errstyle=2

   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.95,"PDF median fit with 68% and 95% confidence",col=col.magenta,charsize=1.5,charthick=thickall
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.90,"PDF mean fit",col=col.orange,charsize=1.5,charthick=thickall
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.85,"Best fit",col=col.red,charsize=1.5,charthick=thickall
   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.80,"Initial guess",col=col.blue,charsize=1.5,charthick=thickall

   XYOUTS,XR[0]+DX*0.05,YR[0]+DY*0.05,"N!Dpoints!N = "+strtrim(Nlines,2),col=col.black,charsize=1.5,charthick=thickall

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;= = = Pb PDF = = =
histbin = 0.01
plothist,Pb,Xhist,Yhist,bin=histbin,/noplot
; setting plot range
XR    = [min(Xhist),max(Xhist)]
YR    = [0,max(yhist)+0.05*max(yhist)]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='Pb' $
        , ytitle ='#' $
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

plothist,Pb,Xhist,Yhist,bin=histbin,col=col.black,thick=thickall,/overplot
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 


;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;= = = a&b PDF w error bars = = =
histbin = 0.001
plothist,bb,Xhist,Yhist,bin=histbin,/noplot
; setting plot range
XR    = [min(Xhist),max(Xhist)]
YR    = [0,max(yhist)+0.05*max(yhist)]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , xtitle ='b in a*g+b' $
        , ytitle ='#' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white $
        , position = [0.57,0.07,0.98,0.25]

oplot,[bb(0),bb(0)],[0.0,1e5],col=col.blue,thick=thickall,linestyle=0            ; initial guess
oplot,[RESULT(7),RESULT(7)],[0.0,1e5],col=col.red,thick=thickall,linestyle=0     ; best fit
oplot,[RESULT(9),RESULT(9)],[0.0,1e5],col=col.orange,thick=thickall,linestyle=0  ; mean fit

oplot,[RESULT(8),RESULT(8)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=0
oplot,[RESULT(8)-RESULT(10),RESULT(8)-RESULT(10)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=0
oplot,[RESULT(8)+RESULT(11),RESULT(8)+RESULT(11)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=0
oplot,[RESULT(8)-RESULT(12),RESULT(8)-RESULT(12)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=2
oplot,[RESULT(8)+RESULT(13),RESULT(8)+RESULT(13)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=2

plothist,bb,Xhist,Yhist,bin=histbin,col=col.black,thick=thickall,/overplot


histbin = 0.001
plothist,mm,Xhist,Yhist,bin=histbin,/noplot
; setting plot range
XR    = [min(Xhist),max(Xhist)]
YR    = [0,max(yhist)+0.05*max(yhist)]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,bb,mm, col=col.black    $
        , /NODATA $
        , /NOERASE $
        , xtitle ='a in a*g+b' $
        , ytitle ='#' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white $
        , position = [0.57,0.32,0.98,0.5]

oplot,[mm(0),mm(0)],[0.0,1e5],col=col.blue,thick=thickall,linestyle=0            ; initial guess
oplot,[RESULT(0),RESULT(0)],[0.0,1e5],col=col.red,thick=thickall,linestyle=0     ; best fit
oplot,[RESULT(2),RESULT(2)],[0.0,1e5],col=col.orange,thick=thickall,linestyle=0  ; mean fit

oplot,[RESULT(1),RESULT(1)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=0
oplot,[RESULT(1)-RESULT(3),RESULT(1)-RESULT(3)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=0
oplot,[RESULT(1)+RESULT(4),RESULT(1)+RESULT(4)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=0
oplot,[RESULT(1)-RESULT(5),RESULT(1)-RESULT(5)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=2
oplot,[RESULT(1)+RESULT(6),RESULT(1)+RESULT(6)],[0.0,1e5],col=col.magenta,thick=thickall,linestyle=2

plothist,mm,Xhist,Yhist,bin=histbin,col=col.black,thick=thickall,/overplot

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 




if PS ne 0 then begin
   device, /close
   set_plot, 'x'
endif
!p.multi = [0,0,0]
Nw = Nw+1  
endif ; end ALLINONE plot
;=============================================================================================


endif ; end section on pruning outliers results


if vb eq 1 then print,' '
if vb eq 1 then print,'::  plotPDFs.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
