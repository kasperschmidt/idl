;+
;----------------------------
;   NAME
;----------------------------
; plotShiftTest.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure plotting the files run for testing the shifting in the linearfitMCMC.pro
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
; /ERRBAR         : set /ERRBAR to overplot the errorbars
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotShiftTest,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-24  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotShiftTest,ERRBAR=ERRBAR,EPS=EPS,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

ERR = n_elements(ERRBAR)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

shift20 = 'PDFs_top10shift20_grfit/slopeinvestigation_output_top10_shift20_grfit.txt'
shift18 = 'PDFs_top10shift18_grfit/slopeinvestigation_output_top10_shift18_grfit.txt'
shift16 = 'PDFs_top10shift16_grfit/slopeinvestigation_output_top10_shift16_grfit.txt'
shift14 = 'PDFs_top10shift14_grfit/slopeinvestigation_output_top10_shift14_grfit.txt'
shift12 = 'PDFs_top10shift12_grfit/slopeinvestigation_output_top10_shift12_grfit.txt'

Nfiles  = 5.

;# objid   abest   amed   amean   aplus68   aminus68   aplus95 aminus95   bbest   bmed   bmean   bplus68   bminus68   bplus95   bminus95   z      season

; --- Reading fit result ASCII files ---
Nrows        = File_lines(shift20)               ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
result20     = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,shift20, /GET_LUN                      ; open file for reading     
header20 = STRARR(Nheaderlines)                  ; string array for header
readf,lun,header20                               ; reading header into string array
readf,lun,result20                               
free_lun,lun  
; --------------------------------------
Nrows        = File_lines(shift18)               ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
result18     = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,shift18, /GET_LUN                      ; open file for reading     
header18 = STRARR(Nheaderlines)                  ; string array for header
readf,lun,header18                               ; reading header into string array
readf,lun,result18                               
free_lun,lun  
; --------------------------------------
Nrows        = File_lines(shift16)               ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
result16     = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,shift16, /GET_LUN                      ; open file for reading     
header16 = STRARR(Nheaderlines)                  ; string array for header
readf,lun,header16                               ; reading header into string array
readf,lun,result16                               
free_lun,lun  
; --------------------------------------
Nrows        = File_lines(shift14)               ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
result14     = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,shift14, /GET_LUN                      ; open file for reading     
header14 = STRARR(Nheaderlines)                  ; string array for header
readf,lun,header14                               ; reading header into string array
readf,lun,result14                               
free_lun,lun  
; --------------------------------------
Nrows        = File_lines(shift12)               ; number of rows in file
Nheaderlines = 4                                 ; number of header lines
Ncol         = 17                                ; number of columns in file
result12     = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,shift12, /GET_LUN                      ; open file for reading     
header12 = STRARR(Nheaderlines)                  ; string array for header
readf,lun,header12                               ; reading header into string array
readf,lun,result12                               
free_lun,lun  
; --------------------------------------

if vb eq 1 then print,' --- The MCMC "best" slopes (shift12,shift14,shift16,shift18,shift20) --- '
for ii=0,Nrows-Nheaderlines-1 do begin
   if vb eq 1 then print,result12(1,ii),result14(1,ii),result16(1,ii),result18(1,ii),result20(1,ii)
endfor

if vb eq 1 then print,' --- The MCMC "best" offset (shift12,shift14,shift16,shift18,shift20) --- '
for ii=0,Nrows-Nheaderlines-1 do begin
   if vb eq 1 then print,result12(8,ii),result14(8,ii),result16(8,ii),result18(8,ii),result20(8,ii)
endfor

if vb eq 1 then print,' --- The MCMC "best" offset shifted back (shift12,shift14,shift16,shift18,shift20) --- '
for ii=0,Nrows-Nheaderlines-1 do begin
   if vb eq 1 then print,12-result12(1,ii)*12+result12(8,ii),14-result14(1,ii)*14+result14(8,ii),16-result16(1,ii)*16+result16(8,ii),18-result18(1,ii)*18+result18(8,ii),20-result20(1,ii)*20+result20(8,ii)
endfor



Nw = 0 ; resetting window numbering
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'ShiftTest_abestcompar.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'comparison of slopes'
   thickall = 2
endelse
; setting plot range
XR = [0,2*(Nfiles)+1]
YR = [.55,1.05]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
;        , xtitle ='object no.' $
        , ytitle ='fitted slopes' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , psym = 2 $
        , yminor = 2 $
        , pos = [0.15,0.25,0.95,0.95] $
        , background = col.white

colvec = [col.blue,col.red,col.green,col.orange,col.magenta,col.cyan,col.pink,col.black,col.charcoal,col.brown]
PLOTSYM,0,1.5,/fill
xx = fltarr(2)
for ii=0,Nrows-Nheaderlines-1 do begin
   oplot,xx+1+ii*0.1,xx+result12(1,ii),thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+1+ii*0.1,xx+result12(1,ii),xx+result12(4,ii),/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+1+ii*0.1,xx+result12(1,ii),xx+result12(5,ii),/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   ;if ERR eq 1 then oploterror,xx+1+ii*0.1,xx+result12(1,ii),xx+result12(6,ii),/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 95% err
   ;if ERR eq 1 then oploterror,xx+1+ii*0.1,xx+result12(1,ii),xx+result12(7,ii),/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 95% err

   oplot,xx+3+ii*0.1,xx+result14(1,ii),thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+3+ii*0.1,xx+result14(1,ii),xx+result14(4,ii),/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+3+ii*0.1,xx+result14(1,ii),xx+result14(5,ii),/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   oplot,xx+5+ii*0.1,xx+result16(1,ii),thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+5+ii*0.1,xx+result16(1,ii),xx+result16(4,ii),/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+5+ii*0.1,xx+result16(1,ii),xx+result16(5,ii),/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   oplot,xx+7+ii*0.1,xx+result18(1,ii),thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+7+ii*0.1,xx+result18(1,ii),xx+result18(4,ii),/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+7+ii*0.1,xx+result18(1,ii),xx+result18(5,ii),/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   oplot,xx+9+ii*0.1,xx+result20(1,ii),thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+9+ii*0.1,xx+result20(1,ii),xx+result20(4,ii),/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+9+ii*0.1,xx+result20(1,ii),xx+result20(5,ii),/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
endfor

XYOUTS,1.5,YR[0]-0.05*DY,'Shift by 12',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,3.5,YR[0]-0.05*DY,'Shift by 14',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,5.5,YR[0]-0.05*DY,'Shift by 16',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,7.5,YR[0]-0.05*DY,'Shift by 18',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,9.5,YR[0]-0.05*DY,'Shift by 20',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0


if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'ShiftTest_bbestcompar.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'comparison of slopes'
   thickall = 2
endelse
; setting plot range
XR = [0,2*(Nfiles)+1]
YR = [0,7]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
;        , xtitle ='object no.' $
        , ytitle ='fitted offsets' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , psym = 2 $
        , yminor = 2 $
        , pos = [0.15,0.25,0.95,0.95] $
        , background = col.white

colvec = [col.blue,col.red,col.green,col.orange,col.magenta,col.cyan,col.pink,col.black,col.charcoal,col.brown]
PLOTSYM,0,1.5,/fill
xx = fltarr(2)
for ii=0,Nrows-Nheaderlines-1 do begin

   VALUE   = 12-result12(1,ii)*12+result12(8,ii)
   ERRLOW  = sqrt( (12*result12(4,ii))^2 + result12(11,ii)^2 )
   ERRHIGH = sqrt( (12*result12(5,ii))^2 + result12(12,ii)^2 )
   oplot,xx+1+ii*0.1,xx+VALUE,thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+1+ii*0.1,xx+VALUE,xx+ERRLOW ,/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+1+ii*0.1,xx+VALUE,xx+ERRHIGH,/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   VALUE   = 14-result14(1,ii)*14+result14(8,ii)
   ERRLOW  = sqrt( (14*result14(4,ii))^2 + result14(11,ii)^2 )
   ERRHIGH = sqrt( (14*result14(5,ii))^2 + result14(12,ii)^2 )
   oplot,xx+3+ii*0.1,xx+VALUE,thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+3+ii*0.1,xx+VALUE,xx+ERRLOW ,/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+3+ii*0.1,xx+VALUE,xx+ERRHIGH,/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   VALUE   = 16-result16(1,ii)*16+result16(8,ii)
   ERRLOW  = sqrt( (16*result16(4,ii))^2 + result16(11,ii)^2 )
   ERRHIGH = sqrt( (16*result16(5,ii))^2 + result16(12,ii)^2 )
   oplot,xx+5+ii*0.1,xx+VALUE,thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+5+ii*0.1,xx+VALUE,xx+ERRLOW ,/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+5+ii*0.1,xx+VALUE,xx+ERRHIGH,/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   VALUE   = 18-result18(1,ii)*18+result18(8,ii)
   ERRLOW  = sqrt( (18*result18(4,ii))^2 + result18(11,ii)^2 )
   ERRHIGH = sqrt( (18*result18(5,ii))^2 + result18(12,ii)^2 )
   oplot,xx+7+ii*0.1,xx+VALUE,thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+7+ii*0.1,xx+VALUE,xx+ERRLOW ,/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+7+ii*0.1,xx+VALUE,xx+ERRHIGH,/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err

   VALUE   = 20-result20(1,ii)*20+result20(8,ii)
   ERRLOW  = sqrt( (20*result20(4,ii))^2 + result20(11,ii)^2 )
   ERRHIGH = sqrt( (20*result20(5,ii))^2 + result20(12,ii)^2 )
   oplot,xx+9+ii*0.1,xx+VALUE,thick=thickall     ,col=colvec(ii),psym=8
   if ERR eq 1 then oploterror,xx+9+ii*0.1,xx+VALUE,xx+ERRLOW ,/LOBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
   if ERR eq 1 then oploterror,xx+9+ii*0.1,xx+VALUE,xx+ERRHIGH,/HIBAR,errcol=colvec(ii),errthick=thickall,psym=8,col=colvec(ii)  ; 68% err
endfor

XYOUTS,1.5,YR[0]-0.05*DY,'Shift by 12',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,3.5,YR[0]-0.05*DY,'Shift by 14',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,5.5,YR[0]-0.05*DY,'Shift by 16',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,7.5,YR[0]-0.05*DY,'Shift by 18',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0
XYOUTS,9.5,YR[0]-0.05*DY,'Shift by 20',col=col.black,charsize=1.5,charthick=thickall,ORIENTATION=90,alignment=1.0


if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

if vb eq 1 then print,' '
if vb eq 1 then print,':: plotShiftTest.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
