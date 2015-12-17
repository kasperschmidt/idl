;+
;----------------------------
;   NAME
;----------------------------
; plotRepickVSinitial.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This program compares two files of fitted slopes (linearfitMCMC.pro). 
; The purpose is to see what difference 're-picking' the data via
; repickdata.pro does to the fitted slopes.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; initialfit      : File containing the linear MCMC fit performed on the intial data 
; repickfit       : File containing the linear MCMC fit performed on the intial're-picked' data 
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
; IDL> plotRepickVSinitial,'PDFs_FILELISTtestfilelist_FILE0_grfit/slopeinvestigationMULTIPLE_output_FILELISTtestfilelist_FILE0_grfit.txt','PDFs_FILELISTtestfilelist_FILE0_grfit_RP01/slopeinvestigationMULTIPLE_output_FILELISTtestfilelist_FILE0_grfit_RP01.txt',/VERBOSE,ERR=95

; IDL> plotRepickVSinitial,'slopeinvestigation_output_z1p4to1p6_grfit.txt','slopeinvestigation_output_z1p4to1p6_grfit_RP01.txt',/VERBOSE,ERR=95
; IDL> plotRepickVSinitial,'slopeinvestigation_output_z1p4to1p6_ugfit.txt','slopeinvestigation_output_z1p4to1p6_ugfit_RP01.txt',/VERBOSE,ERR=95

; IDL> plotRepickVSinitial,'slopeinvestigation_output_z1p4to1p6_grfit.txt','slopeinvestigation_output_z1p4to1p6_grfit_RP02.txt',/VERBOSE,ERR=95
; IDL> plotRepickVSinitial,'slopeinvestigation_output_z1p4to1p6_ugfit.txt','slopeinvestigation_output_z1p4to1p6_ugfit_RP02.txt',/VERBOSE,ERR=95

;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-07  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotRepickVSinitial,initialfit,repickfit,ERR=ERR,EPS=EPS,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

; --- Reading initial ASCII file ---
Nrows        = File_lines(initialfit)            ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
INITIAL      = dblarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,initialfit, /GET_LUN                   ; open file for reading
headerINITIAL = STRARR(Nheaderlines)             ; string array for header
readf,lun,headerINITIAL                          ; reading header into string array
readf,lun,INITIAL                                ; note that these parameters also include the KlnL value - hence PARAM[2] is not needed
free_lun,lun  
; ----------------------------------
if vb eq 1 then print,' '
if vb eq 1 then print,':: plotRepickVSinitial.pro :: The header of the initial file reads:'
if vb eq 1 then print,headerINITIAL

; --- Reading repick ASCII file ---
Nrows        = File_lines(repickfit)             ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
REPICK       = dblarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,repickfit, /GET_LUN                    ; open file for reading     
headerREPICK = STRARR(Nheaderlines)              ; string array for header
readf,lun,headerREPICK                           ; reading header into string array
readf,lun,REPICK                                 ; note that these parameters also include the KlnL value - hence PARAM[2] is not needed
free_lun,lun  
; ----------------------------------
if vb eq 1 then print,' '
if vb eq 1 then print,':: plotRepickVSinitial.pro :: The header of the re-pick file reads:'
if vb eq 1 then print,headerREPICK

if n_elements(ERR) ne 1 then ERR=0  ; setting ERR to 0 if not given on command line

Nw = 0
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'SlopeINvsRP.eps' ; name of eps file
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slopes'
   thickall = 2
endelse

; setting plot range
XR = [-0.5,2.5]
YR = [-0.5,2.5]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,INITIAL(0,*),REPICK(0,*), col=col.black    $
        , /NODATA $
        , xtitle ='a in a*x+b  INITIAL' $
        , ytitle ='a in a*x+b  RE-PICK' $
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

oplot,XR,YR,linestyle=1,thick=thickall,col=col.black


plotsym,0,1.5,/fill
oplot,INITIAL(1,*),REPICK(1,*),psym=8,col=col.black

if ERR eq 68 then begin ; plotting 68% confidence intervals if requested
   oploterror,INITIAL(1,*),REPICK(1,*),INITIAL(4,*),REPICK(4,*),/LOBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black
   oploterror,INITIAL(1,*),REPICK(1,*),INITIAL(5,*),REPICK(5,*),/HIBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black
endif

if ERR eq 95 then begin ; plotting 95% confidence intervals if requested
   oploterror,INITIAL(1,*),REPICK(1,*),INITIAL(6,*),REPICK(6,*),/LOBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black;,errstyle=2
   oploterror,INITIAL(1,*),REPICK(1,*),INITIAL(7,*),REPICK(7,*),/HIBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black;,errstyle=2
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
   plot1 = 'ScaleINvsRP.eps' ; name of eps file
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Scale'
   thickall = 2
endelse

; setting plot range
XR = [-0.5,0.5]
YR = [-0.5,0.5]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,INITIAL(0,*),REPICK(0,*), col=col.black    $
        , /NODATA $
        , xtitle ='b in a*x+b  INITIAL' $
        , ytitle ='b in a*x+b  RE-PICK' $
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

oplot,XR,YR,linestyle=1,thick=thickall,col=col.black


plotsym,0,1.5,/fill
oplot,INITIAL(8,*),REPICK(8,*),psym=8,col=col.black

if ERR eq 68 then begin ; plotting 68% confidence intervals if requested
   oploterror,INITIAL(8,*),REPICK(8,*),INITIAL(11,*),REPICK(11,*),/LOBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black
   oploterror,INITIAL(8,*),REPICK(8,*),INITIAL(12,*),REPICK(12,*),/HIBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black
endif

if ERR eq 95 then begin ; plotting 95% confidence intervals if requested
   oploterror,INITIAL(8,*),REPICK(8,*),INITIAL(13,*),REPICK(13,*),/LOBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black;,errstyle=2
   oploterror,INITIAL(8,*),REPICK(8,*),INITIAL(14,*),REPICK(14,*),/HIBAR,errcol=col.black,errthick=thickall,psym=8,col=col.black;,errstyle=2
endif

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

if vb eq 1 then print,' '
if vb eq 1 then print,':: plotRepickVSinitial.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
