;+
;----------------------------
;   NAME
;----------------------------
; plotModelSpecFiles.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure reading and plotting the output data files from compspec_mockobs.pro
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
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotModelSpecFiles,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-02-04  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotModelSpecFiles,EPS=EPS,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

Nfiles = 200                                           ; the number of data files to read
zs=findgen(Nfiles)/100.+0.4
Nheaderlines = 13                                      ; number of header lines
Ncol         = 19                                      ; number of columns in 
Nrows        = 49                                      ; number of rows in files

ALLDATA      = FLTARR(Ncol,Nrows-Nheaderlines,Nfiles)  ; array to read file into
header       = STRARR(Nheaderlines)                    ; string array for header

for ii=0,n_elements(zs)-1 do begin
   filename = 'compspecRUN/compspec_mockobs_3F0_x_3alpha_z'+strtrim(zs(ii),2)+'.dat'
   openr,lun,filename, /GET_LUN                        ; open file for reading     
   readf,lun,header                                    ; reading header into string array (overwriting)
   DATAARR = FLTARR(Ncol,Nrows-Nheaderlines)
   readf,lun,DATAARR                                    ; reading data into array
   ALLDATA(*,*,ii) = DATAARR
   free_lun,lun  
endfor

Fu_vdb  = ALLDATA(4,18,*)   ; the total flux of the Vanden Berk spectrum in the u band
Fg_vdb  = ALLDATA(5,18,*)   ; the total flux of the Vanden Berk spectrum in the g band
Fr_vdb  = ALLDATA(6,18,*)   ; the total flux of the Vanden Berk spectrum in the r band
Fi_vdb  = ALLDATA(7,18,*)   ; the total flux of the Vanden Berk spectrum in the i band
Fz_vdb  = ALLDATA(8,18,*)   ; the total flux of the Vanden Berk spectrum in the z band

Fu_cont = ALLDATA(9,18,*)   ; the continuum flux of Vanden Berk spectrum in the u band
Fg_cont = ALLDATA(10,18,*)  ; the continuum flux of Vanden Berk spectrum in the g band
Fr_cont = ALLDATA(11,18,*)  ; the continuum flux of Vanden Berk spectrum in the r band
Fi_cont = ALLDATA(12,18,*)  ; the continuum flux of Vanden Berk spectrum in the i band
Fz_cont = ALLDATA(13,18,*)  ; the continuum flux of Vanden Berk spectrum in the z band

Fu_line = Fu_vdb-Fu_cont    ; the emission line flux of Vanden Berk spectrum in the u band
Fg_line = Fg_vdb-Fg_cont    ; the emission line flux of Vanden Berk spectrum in the g band
Fr_line = Fr_vdb-Fr_cont    ; the emission line flux of Vanden Berk spectrum in the r band
Fi_line = Fi_vdb-Fi_cont    ; the emission line flux of Vanden Berk spectrum in the i band
Fz_line = Fz_vdb-Fz_cont    ; the emission line flux of Vanden Berk spectrum in the z band

Nw = 0
;=============================================================================================
; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'fluxVSz.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Vanden Berk flux vs z'
   thickall = 2
endelse
; setting plot range
XR = [min(zs)-0.1*(max(zs)-min(zs)),max(zs)+0.1*(max(zs)-min(zs))]
YR = [0,1000]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , ytitle =textoidl('F_{Vanden Berk}') $
        , xtitle ='z' $
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

oplot,zs,Fu_vdb,thick=thickall,col=col.blue    
oplot,zs,Fg_vdb,thick=thickall,col=col.darkgreen
oplot,zs,Fr_vdb,thick=thickall,col=col.gold
oplot,zs,Fi_vdb,thick=thickall,col=col.orange
oplot,zs,Fz_vdb,thick=thickall,col=col.red

XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'u band',col=col.blue   ,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'g band',col=col.darkgreen  ,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'r band',col=col.gold ,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],'i band',col=col.orange ,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.75+YR[0],'z band',col=col.red    ,charsize=2.5,charthick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/fluxfracVSz.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Vanden Berk continuuum flux/line flux vs z'
   thickall = 2
endelse
; setting plot range
;XR = [min(zs)-0.1*(max(zs)-min(zs)),max(zs)+0.1*(max(zs)-min(zs))]
XR = [-0.42057002,5.5886702]
YR = [0.1,0.8]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , ytitle =textoidl('log(F_{lines}/F_{cont})') $
        , xtitle ='z' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yticks = 5 $
        , /ylog $
        , yminor = 2 $
        , background = col.white

oplot,zs,Fu_line/Fu_cont,thick=thickall,col=col.blue,linestyle=2    
oplot,zs,Fi_line/Fi_cont,thick=thickall,col=col.orange,linestyle=2
oplot,zs,Fz_line/Fz_cont,thick=thickall,col=col.red,linestyle=2

oplot,zs,Fg_line/Fg_cont,thick=thickall+4,col=col.darkgreen;,linestyle=2
oplot,zs,Fr_line/Fr_cont,thick=thickall+4,col=col.gold;,linestyle=2

XYOUTS,DX*0.90+XR[0],DY*0.29+YR[0],'SDSS z band',col=col.red    ,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.90+XR[0],DY*0.20+YR[0],'SDSS g band',col=col.darkgreen  ,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.90+XR[0],DY*0.13+YR[0],'SDSS r band',col=col.gold ,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.90+XR[0],DY*0.07+YR[0],'SDSS i band',col=col.orange ,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.90+XR[0],DY*0.03+YR[0],'SDSS u band',col=col.blue   ,charsize=2.5,charthick=thickall,alignment=1.0

;XYOUTS,DX*0.95+XR[0],DY*0.90+YR[0],'u band',col=col.blue   ,charsize=2.5,charthick=thickall,alignment=1.0
;XYOUTS,DX*0.95+XR[0],DY*0.80+YR[0],'g band',col=col.darkgreen  ,charsize=2.5,charthick=thickall,alignment=1.0
;XYOUTS,DX*0.95+XR[0],DY*0.70+YR[0],'r band',col=col.gold ,charsize=2.5,charthick=thickall,alignment=1.0
;XYOUTS,DX*0.95+XR[0],DY*0.62+YR[0],'i band',col=col.orange ,charsize=2.5,charthick=thickall,alignment=1.0
;XYOUTS,DX*0.95+XR[0],DY*0.55+YR[0],'z band',col=col.red    ,charsize=2.5,charthick=thickall,alignment=1.0

oplot,[0.95,0.95],[0.0001,0.33],thick=thickall,col=col.black
oplot,[1.85,1.85],[0.0001,0.33],thick=thickall,col=col.black
XYOUTS,1.0,0.34,'MgII from g to r',col=col.black    ,charsize=2.5,charthick=thickall,orientation=90
XYOUTS,1.9,0.34,'CIII] from g to r',col=col.black    ,charsize=2.5,charthick=thickall,orientation=90


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


if vb eq 1 then print,' '
if vb eq 1 then print,':: plotModelSpecFiles.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
