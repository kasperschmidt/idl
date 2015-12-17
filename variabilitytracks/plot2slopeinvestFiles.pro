;+
;----------------------------
;   NAME
;----------------------------
; plot2slopeinvestFiles.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure comparing the output of two slopeinvestigation output files
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; fileONE         : string containing name and path of first slopeinvestigation file
; fileTWO         : string containing name and path of second slopeinvestigation file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; AGAMMA          : The string conaining name and path of a file containing
;                   the A and gamma values from Schmidt et al 2010 of the objects
;                   Assumed format (no header):
;                        col 1:  ID
;                        col 2:  z
;                        col 3:  A
;                        col 4:  Gamma
; Subranges       : four component vector indicating the entries to compare
;                   [minONE,maxONE,minTWO,maxTWO]. Default is comparing all entries
; /ERRBAR         : plot 68% confidence error bars on slopes and offsets ; NB! make
;                    sure that the errorbars have been corrected via correctMCMCerrorsInCat.pro 
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : New Fits file containing SDSS objects, same format
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plot2slopeinvestFiles,'slopeinvestigation_output_shenmatchALL_grfit110129.txt','PDFs_S82QSOs_shenmatchALL_grfit11015_outliers9999/slopeinvestigation_output_shenmatchALL_grfit110215_outliers9999.txt',Subranges=[0,499,0,499],AGAMMA='/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/S82objid_z_Agamma.dat'
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-15  started by K. B. Schmidt (MPIA)
; 2011-04-12  AGAMMA keyword added. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plot2slopeinvestFiles,fileONE,fileTWO,Subranges=Subranges,ERRBAR=ERRBAR,EPS=EPS,VERBOSE=VERBOSE,AGAMMA=AGAMMA
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

AG = n_elements(AGAMMA)
EB = n_elements(ERRBAR)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

Nheaderlines = 4                                      ; number of header lines
Ncol         = 17                                     ; number of columns in file

; --- Reading ASCII file fileONE ---
NrowsONE     = File_lines(fileONE)                    ; number of rows in file
arrONE       = fltarr(Ncol,NrowsONE-Nheaderlines)     ; array to read file into
openr,lun,fileONE, /GET_LUN                           ; open file for reading     
headerONE = STRARR(Nheaderlines)                      ; string array for header
readf,lun,headerONE                                   ; reading header into string array
readf,lun,arrONE                                      ; reading data into array
free_lun,lun  
; ----------------------------------

; --- Reading ASCII file fileTWO ---
NrowsTWO     = File_lines(fileTWO)                    ; number of rows in file
arrTWO       = fltarr(Ncol,NrowsTWO-Nheaderlines)     ; array to read file into
openr,lun,fileTWO, /GET_LUN                           ; open file for reading     
headerTWO = STRARR(Nheaderlines)                      ; string array for header
readf,lun,headerTWO                                   ; reading header into string array
readf,lun,arrTWO                                      ; reading data into array
free_lun,lun  
; ----------------------------------

if AG eq 1 then begin                                 ; if A and gamma values are to be loaded (and later on plotted)
   ; --- Reading A and gamma file ---
   NrowsAG     = File_lines(AGAMMA)                   ; number of rows in file
   arrAG       = fltarr(4,NrowsAG)                    ; array to read file into
   openr,lun,AGAMMA, /GET_LUN                         ; open file for reading     
   readf,lun,arrAG                                    ; reading data into array
   free_lun,lun  
   ; ----------------------------------
   if n_elements(Subranges) ne 0 then begin
      arrAG = arrAG(*,Subranges(0):Subranges(1))      ; selecting sub range
   endif
endif


if n_elements(Subranges) ne 0 then begin         ; if specific sub range is to be compared select these
   arrONE = arrONE(*,Subranges(0):Subranges(1)) 
   arrTWO = arrTWO(*,Subranges(2):Subranges(3))
endif

NobjONE = n_elements(arrONE(0,*))
NobjTWO = n_elements(arrTWO(0,*))
if NobjONE ne NobjTWO then begin
   print,':: plot2slopeinvestFiles.pro :: There is not the same number of object in the two files!'
   print,'                                There are '+strtrim(NobjONE,2)+' objects in fileONE and '+strtrim(NobjTWO,2)+' objects in fileTWO'
   if n_elements(Subranges) eq 0 then print,'                                Try using the subranges keyword.'
   if n_elements(Subranges) ne 0 then print,'                                Check that the given subranges have the same size.'
   print,'                                    -> ABORTING'
   stop
endif



Nw = 0
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopeVSslope.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs Slope'
   thickall = 2
endelse

; setting plot range
;XR = [min(arrONE(1,*))-0.1,max(arrONE(1,*))+0.1]
;YR = [min(arrTWO(1,*))-0.1,max(arrTWO(1,*))+0.1]
XR = [-0.1,1.5]
YR = [-0.1,1.5]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='slope fileONE' $
        , ytitle ='slope fileTWO' $
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

oplot,findgen(10)-5,findgen(10)-5,col=col.black,thick=thickall

PLOTSYM,0,1.,/FILL,thick=thickall
oplot,arrONE(1,*),arrTWO(1,*),col=col.red,thick=thickall,psym=8

if EB eq 1 then begin
   ;overplotting error bar
   oploterror,arrONE(1,*),arrTWO(1,*),arrONE(4,*),arrTWO(4,*),/HIBAR,errcol=col.red,errthick=thickall,psym=3,col=col.red ; 68% confidence
   oploterror,arrONE(1,*),arrTWO(1,*),arrONE(5,*),arrTWO(5,*),/LOBAR,errcol=col.red,errthick=thickall,psym=3,col=col.red ; 68% confidence
   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'Error bars: 68% confidence',col=col.black,charsize=1.5,charthick=thickall
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'offsetVSoffset.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'offset VS offset'
   thickall = 2
endelse

; setting plot range
;XR = [min(arrONE(8,*))-0.03,max(arrONE(8,*))+0.03]
;YR = [min(arrTWO(8,*))-0.03,max(arrTWO(8,*))+0.03]
XR = [-0.2,0.25]
;YR = [-0.2,0.25]
YR = [-0.05,0.05]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='offset fileONE' $
        , ytitle ='offset fileTWO' $
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

oplot,findgen(10)-5,findgen(10)-5,col=col.black,thick=thickall

PLOTSYM,0,1.,/FILL,thick=thickall
oplot,arrONE(8,*),arrTWO(8,*),col=col.blue,thick=thickall,psym=8

if EB eq 1 then begin
   ;overplotting error bar
   oploterror,arrONE(8,*),arrTWO(8,*),arrONE(11,*),arrTWO(12,*),/HIBAR,errcol=col.blue,errthick=thickall,psym=3,col=col.blue ; 68% confidence
   oploterror,arrONE(8,*),arrTWO(8,*),arrONE(11,*),arrTWO(12,*),/LOBAR,errcol=col.blue,errthick=thickall,psym=3,col=col.blue ; 68% confidence
   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'Error bars: 68% confidence',col=col.black,charsize=1.5,charthick=thickall
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'zVSz.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'z VS z'
   thickall = 2
endelse

; setting plot range
XR = [0,6]
YR = [0,6]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='z fileONE' $
        , ytitle ='z fileTWO' $
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

oplot,findgen(10),findgen(10),col=col.black,thick=thickall

PLOTSYM,0,1.,/FILL,thick=thickall
oplot,arrONE(15,*),arrTWO(15,*),col=col.green,thick=thickall,psym=8

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================


if AG eq 1 then begin

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSAprime_compare_zcorrected.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=45, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1200, ysize=500, title = 'slope VS Aprime zcorrected'
   thickall = 2
endelse
; setting plot range
XR = [alog10(0.018),alog10(1.)]
;XR = [alog10(0.018),alog10(0.1)]
YR = [0.1,1.4]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ="A'" $
        , ytitle =textoidl('a_{best,gr}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xtickname = xticks  $
;        , /xlog $
        , background = col.white


Aprime = arrAG(2,*)*(1+arrAG(1,*))^arrAG(3,*)
Aprime = alog10(Aprime)

LOADCT,39
device,decomposed=0
BYTCOL   = BYTSCL(FINDGEN(NobjONE))  ; array used to color datasets
for ii=0,NobjONE-1 do begin
   PLOTSYM,0,1.5,/FILL
   oplot,fltarr(2)+Aprime(ii),fltarr(2)+arrONE(1,ii),col=BYTCOL(ii),psym=8,thick=thickall 
   PLOTSYM,3,1.5,/FILL
   oplot,fltarr(2)+Aprime(ii),fltarr(2)+arrTWO(1,ii),col=BYTCOL(ii),psym=8,thick=thickall 
   oplot,[Aprime(ii),Aprime(ii)],[arrONE(1,ii),arrTWO(1,ii)],col=BYTCOL(ii),thick=thickall 
endfor
col = getcolor(/load)

XYOUTS,DX*0.9+XR[0],DY*0.10+YR[0],'file ONE (full data set)',col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.9+XR[0],DY*0.05+YR[0],'file TWO (pre-cleaned)',col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
   PLOTSYM,0,2.5,/FILL
   oplot,fltarr(2)+DX*0.94+XR[0],fltarr(2)+DY*0.12+YR[0],col=col.black,psym=8,thick=thickall 
   PLOTSYM,3,2.5,/FILL
   oplot,fltarr(2)+DX*0.94+XR[0],fltarr(2)+DY*0.07+YR[0],col=col.black,psym=8,thick=thickall 


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


endif  ; end of AG plotting



if vb eq 1 then print,' '
if vb eq 1 then print,':: plot2slopeinvestFiles.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
