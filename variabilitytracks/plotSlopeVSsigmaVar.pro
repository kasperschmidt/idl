;+
;----------------------------
;   NAME
;----------------------------
; plotSlopeVSsigmaVar.pro
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
; Dmags           : vector with the DMAG values for which files are to be plotted 
;                   (see slopeinvestigation.pro for the meaning of DMAG) 
; Nobj            : The number of objects in file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; gaussvardir     : path of directory with gaussvarfit files in (default is present directory)
; datvardir       : path of directory with datvarfit files in (default is present directory)
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotSlopeVSsigmaVar,(findgen(10)+1)/10.,50,datvardir='PDFs_FGstars_datvarfit/',gaussvardir='PDFs_FGstars_gaussvarfit/',/VERBOSE
;
; IDL> plotSlopeVSsigmaVar,(findgen(11))/10.,50,datvardir='PDFs_FGstars_datvarfit_1year/',gaussvardir='PDFs_FGstars_gaussvarfit/',/VERBOSE
;
; IDL> plotSlopeVSsigmaVar,findgen(11)/50.,50,datvardir='PDFs_FGstars_datvarfit3/',gaussvardir='PDFs_FGstars_gaussvarfit3/',/VERBOSE
;
; IDL> plotSlopeVSsigmaVar,[0.0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0],50,datvardir='PDFs_FGstars_datvarfit_1year_ui/',/VERBOSE
;
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-02-16  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotSlopeVSsigmaVar,DMAGs,Nobj,EPS=EPS,gaussvardir=gaussvardir,datvardir=datvardir,VERBOSE=VERBOSE

GS = n_elements(gaussvardir)
LI = n_elements(datvardir)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

if n_elements(gaussvardir) eq 0 then gaussvardir = './'  ; setting default directory to present dir
if n_elements(datvardir)   eq 0 then datvardir   = './'  ; setting default directory to present dir

Nfiles = n_elements(DMAGS)


Nheaderlines = 4                                      ; number of header lines
Ncol         = 17                                     ; number of columns in file
Nrows        = Nobj+Nheaderlines                      ; number of rows in file
GAUSSdat     = fltarr(Nfiles,Ncol,Nobj)               ; array to read files into
LINEARdat    = fltarr(Nfiles,Ncol,Nobj)               ; array to read files into

for ii=0,Nfiles-1 do begin

   if LI eq 1 then begin
      ;linearname = strtrim(datvardir,2)+'slopeinvestigation_output_FGstars_datvarfit_wDMAG'+strtrim(DMAGs(ii),2)+'.txt'
      ;linearname = strtrim(datvardir,2)+'slopeinvestigation_output_FGstars_datvarfit_wDMAG'+strtrim(DMAGs(ii),2)+'_1year.txt'
      linearname = strtrim(datvardir,2)+'slopeinvestigation_output_FGstars_datvarfit_ui_wDMAG'+strtrim(DMAGs(ii),2)+'.txt'
      if vb eq 1 then print,':: plotSlopeVSsigmaVar.pro :: Reading the file ',strtrim(linearname)
      ; --- Reading repick ASCII file ---
      openr,lun,linearname, /GET_LUN                   ; open file for reading     
      header = STRARR(Nheaderlines)                    ; string array for header
      DUMMYARR       = fltarr(Ncol,Nrows-Nheaderlines) ; array to read file into
      readf,lun,header                                 ; reading header into string array
      readf,lun,DUMMYARR                               ; reading data into array
      LINEARdat(ii,*,*) = DUMMYARR
      free_lun,lun  
      ; ----------------------------------
   endif

   if GS eq 1 then begin
      gaussname = strtrim(gaussvardir,2)+'slopeinvestigation_output_FGstars_gaussvarfit_wDMAG'+strtrim(DMAGs(ii),2)+'.txt';'_1year.txt'
      if vb eq 1 then print,':: plotSlopeVSsigmaVar.pro :: Reading the file ',strtrim(gaussname)
      ; --- Reading repick ASCII file ---
      openr,lun,gaussname, /GET_LUN                    ; open file for reading     
      header = STRARR(Nheaderlines)                    ; string array for header
      DUMMYARR       = fltarr(Ncol,Nrows-Nheaderlines) ; array to read file into
      readf,lun,header                                 ; reading header into string array
      readf,lun,DUMMYARR                               ; reading data into array
      GAUSSdat(ii,*,*) = DUMMYARR
      free_lun,lun  
      ; ----------------------------------
   endif
endfor

;objid   abest   amed   amean   aplus68   aminus68   aplus95   aminus95   bbest   bmed   bmean   bplus68   bminus68   bplus95   bminus95   z      season
meanagauss  = fltarr(Nfiles)
meanalinear = fltarr(Nfiles)

Nw=0
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopesVSsigmaadded.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs DMAGs added to objects'
   thickall = 2
endelse
; setting plot range
DmagShift = (Dmags(1)-Dmags(0))/10.
;XR = [min(Dmags)-DmagShift*5,max(Dmags)+DmagShift*5]
XR = [0.01,1.1]
YR = [-0.5,1.2]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\sigma_{var}')+' (the DMAG added)' $
        , ytitle ='a (fitted slopes)' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , /xlog $
        , yminor = 2 $
        , background = col.white

PLOTSYM,0,0.8,/fill,thick=thickall
for jj=0,Nfiles-1 do begin
   if LI eq 1 then meanalinear(jj) = mean(LINEARdat(jj,1,*)) 
   if GS eq 1 then meanagauss(jj)  = mean(GAUSSdat(jj,1,*))

   if LI eq 1 then oplot,fltarr(50)+Dmags(jj)-DmagShift,LINEARdat(jj,1,*),psym=8,col=col.red,thick=thickall
   if GS eq 1 then oplot,fltarr(50)+Dmags(jj)+DmagShift,GAUSSdat(jj,1,*),psym=8,col=col.blue
endfor

PLOTSYM,0,2.0,/FILL,thick=thickall
if LI eq 1 then oplot,Dmags-DmagShift,meanalinear,psym=8,col=col.red,thick=thickall
if GS eq 1 then oplot,Dmags+DmagShift,meanagauss,psym=8,col=col.blue,thick=thickall
PLOTSYM,0,2.0,thick=thickall
if LI eq 1 then oplot,Dmags-DmagShift,meanalinear,psym=8,col=col.black,thick=thickall
if GS eq 1 then oplot,Dmags+DmagShift,meanagauss,psym=8,col=col.black,thick=thickall

if LI eq 1 then begin
   XYOUTS,DX*0.90+XR[0],DY*0.20+YR[0],'linear var a ',col=col.black,charsize=1.5,charthick=thickall,alignment=1.0
   PLOTSYM,0,1.5,/FILL,thick=thickall
   oplot,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.21+YR[0],psym=8,col=col.red,thick=thickall

   XYOUTS,DX*0.90+XR[0],DY*0.1+YR[0],'mean linear var a ',col=col.black,charsize=1.5,charthick=thickall,alignment=1.0
   PLOTSYM,0,2.0,/FILL,thick=thickall
   oplot,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.11+YR[0],psym=8,col=col.red,thick=thickall
   PLOTSYM,0,2.0,thick=thickall
   oplot,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.11+YR[0],psym=8,col=col.black,thick=thickall
endif

if GS eq 1 then begin
   XYOUTS,DX*0.90+XR[0],DY*0.15+YR[0],'gauss var a ',col=col.black,charsize=1.5,charthick=thickall,alignment=1.0
   PLOTSYM,0,1.5,/FILL,thick=thickall
   oplot,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.16+YR[0],psym=8,col=col.blue,thick=thickall

   XYOUTS,DX*0.90+XR[0],DY*0.05+YR[0],'mean gauss var a ',col=col.black,charsize=1.5,charthick=thickall,alignment=1.0
   PLOTSYM,0,2.0,/FILL,thick=thickall
   oplot,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.06+YR[0],psym=8,col=col.blue,thick=thickall
   PLOTSYM,0,2.0,thick=thickall
   oplot,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.06+YR[0],psym=8,col=col.black,thick=thickall
endif

if vb eq 1 then print,':: plotSlopeVSsigmaVar.pro :: The values ...'
if vb eq 1 then print,'                              Dmags:'
if vb eq 1 then print,dmags
if vb eq 1 and GS eq 1 then print,'                              Gauss:'
if vb eq 1 and GS eq 1 then print,meanagauss
if vb eq 1 and LI eq 1 then print,'                              Linear:'
if vb eq 1 and LI eq 1 then print,meanalinear

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: plotSlopeVSsigmaVar.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
