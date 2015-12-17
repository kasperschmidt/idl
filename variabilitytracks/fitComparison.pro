;+
;----------------------------
;   NAME
;----------------------------
; fitComparison.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Routine comparing the difference between fitting QSOs in g VS r and
; in g VS g-r (for just plotting data use plotCOLMAG.pro)
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; subfits           : string containing name and path of fits file
;                     with data to read and plot
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /SEASONS        : set /SEASONS to color code the objects with their seasons
; /ERRBAR         : set this keyword to overplot errorbars and to
;                   estimate the slope of each season. This slope will
;                   be written to an ascii file together with the
;                   objid, redshift and slope of all data points irrespective of season.
;                   Furthermore Kendall's tau is estimated for each season
;                   and added to the file.
; OBJIDS          : string vector containing IDs of obects to plot. If set to 
;                   only one element this is taken as the number of objects to plot
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
;top19
; IDL> fitComparison,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_11_09:58:09_2010.fits',/VERBOSE,/SEASONS

; IDL> fitComparison,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',/VERBOSE,/SEASONS,OBJECTS = ['587731185133027418','587731187279135109','587731513155125454','587731186729353409','587731512075485356','587731187274940621','587731511545758032','587731185115005391','587731185119986070']

; IDL> fitComparison,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',/VERBOSE,/SEASONS,OBJECTS = ['587731511545758032','587731185115005391']

;
;
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-04-12  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ LINEARFITMCMC.pro
;----------------------------
;-
PRO fitComparison,subfits,SEASONS=SEASONS,ERRBAR=ERRBAR,EPS=EPS,VERBOSE=VERBOSE,OBJECTS=OBJECTS


SS = n_elements(SEASONS)
EB = n_elements(ERRBAR)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

namedate,subfits,path,name,extension,date,dateus
s = mrdfits(subfits,1)                                ; reading input data
uniqent = uniq(s.headobjid)                           ; entries of objects
IDs     = s(uniqent).headobjid                        ; getting uniq objids
zs      = s(uniqent).z                                ; the corresponding redshifts

mjd_u = s.MJD_U
mjd_g = s.MJD_G
mjd_r = s.MJD_R
mjd_i = s.MJD_I
mjd_z = s.MJD_Z
seasons = [5.1,5.2,5.24,5.28,5.32,5.35,5.38,5.42,5.5]*10^4. ; the mjd limits of the seasons 
MJD99   = 51330   ; MJD for June 1st 1999
seasons = [MJD99,MJD99+365,MJD99+2*365,MJD99+3*365,MJD99+4*365,MJD99+5*365,MJD99+6*365,MJD99+7*365,MJD99+8*365,MJD99+9*365]
Nseasons = n_elements(seasons)-1
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
if n_elements(OBJECTS) eq 0 then Nplot = 1
if n_elements(OBJECTS) gt 1 then Nplot = n_elements(OBJECTS)
if n_elements(OBJECTS) eq 1 then Nplot = OBJECTS

for ii=0,Nplot-1 do begin  ; looping over objects to inspect
   IDobj    = IDs(ii)
   entOBJ   = where(s.headobjid eq IDobj,Nepochs)
   if n_elements(OBJECTS) gt 1 then begin
     IDobj    = OBJECTS(ii)
     entOBJ   = where(strtrim(s.headobjid,2) eq OBJECTS(ii),Nepochs)
   endif
   mjdOBJ   = s(entOBJ).mjd_r

   gOBJ     = s(entOBJ).PSFMAG_g
   gerrOBJ  = s(entOBJ).PSFMAGERR_g
   rOBJ     = s(entOBJ).PSFMAG_r
   rerrOBJ  = s(entOBJ).PSFMAGERR_r
   grOBJ    = s(entOBJ).PSFMAG_g-s(entOBJ).PSFMAG_r
   grerrOBJ = sqrt(gerrOBJ^2.+rerrOBJ^2.)


   ; fitting data in g vs r
   DATARRAYgr      = fltarr(Nepochs,4)   
   DATARRAYgr(*,0) = gOBJ
   DATARRAYgr(*,1) = rOBJ
   DATARRAYgr(*,2) = gerrOBJ
   DATARRAYgr(*,3) = rerrOBJ

   MCMCloops = 10000
   linearfitMCMC,DATARRAYgr,RESULTgr,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT
   ;resultgr = [0.907266,0.899004,0.902067,0.0760102,0.0539336,0.140053,0.0920597,-0.118347,-0.123490,-0.123372,0.151887,0.100884,0.302301,0.174501]

   ; fitting data in g vs gr (xaxis)
   DATARRAY        = fltarr(Nepochs,4)
   DATARRAY(*,0)   = grOBJ
   DATARRAY(*,1)   = gOBJ
   DATARRAY(*,2)   = grerrOBJ
   DATARRAY(*,3)   = gerrOBJ

   MCMCloops = 10000
   linearfitMCMC,DATARRAY,RESULT,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT
   ;result = [6.05506,6.35437,7.41498,4.83458,2.33219,5.76235,4.88641,-0.767166,-0.637893,-0.965335,1.08544,0.458790,2.75601,0.67689]

   ; fitting data in g vs gr (xaxis)
   DATARRAY        = fltarr(Nepochs,4)
   DATARRAY(*,0)   = gOBJ
   DATARRAY(*,1)   = grOBJ
   DATARRAY(*,2)   = gerrOBJ
   DATARRAY(*,3)   = grerrOBJ

   MCMCloops = 10000
   linearfitMCMC,DATARRAY,RESULT2,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT
   ;result2 = [0.143108,0.140972,0.145923,0.0459759,0.0342669,0.0895541,0.0640767,0.128534,0.126096,0.126839,0.113510,0.0764739,0.227593,0.132983]


   Nw = 0
   ;========================================= xaxis g-r yaxis g ====================================================
   Xdat = grOBJ
   Ydat = gOBJ
   Xdaterr = grerrOBJ
   Ydaterr = gerrOBJ

   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/fitcomp_gVSgr_object'+strtrim(ii,2)+'.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
      !P.FONT = 0                        ; allowing to change font of device
      device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot      
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'gVSgr'
      thickall = 2
   endelse

   ; setting plot range
   XR = [min(Xdat)-0.1,max(Xdat)+0.1]
   YR = [max(Ydat)+0.1,min(Ydat)-0.1]
   ;XR = [0,1]
   ;YR = [22,19.0]
   ;XR = [-0.1,0.65] ; cutout of object 1
   ;YR = [20.4,19.5] ; cutout of object 1


   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='g' $
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

   plotsym,0,1.5,/fill

   if SS eq 1 then begin
      LOADCT,39
      device,decomposed=0
      BYTCOL   = BYTSCL(FINDGEN(Nseasons+1))  ; array used to color datasets

      for kk = 0,Nseasons-1 do begin            ; looping over seasons
         Sent = where(mjdOBJ gt seasons(kk) AND mjdOBJ lt seasons(kk+1),Nsent)

         if Sent ne [-1] and Nsent gt 1 then begin    ; checking that there are obs in season
            oplot,Xdat(sent),Ydat(sent),col=BYTCOL(kk),psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  
               oploterror,Xdat(sent),Ydat(sent),Xdaterr(sent),Ydaterr(sent),psym=3,col=BYTCOL(kk),ERRCOLOR=BYTCOL(kk),ERRTHICK=thickall
            endif
         endif

         if Nsent eq 1 AND Sent ne [-1] then begin
            oplot,fltarr(2)+Xdat(sent),fltarr(2)+Ydat(sent),col=BYTCOL(kk),psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  
               oploterror,fltarr(2)+Xdat(sent),fltarr(2)+Ydat(sent),fltarr(2)+Xdaterr(sent),fltarr(2)+Ydaterr(sent),psym=3,col=BYTCOL(kk),ERRCOLOR=BYTCOL(kk),ERRTHICK=thickall
            endif
         endif
            XYOUTS,DX*0.95+XR[0],DY*0.06*(kk+1)+YR[0],"year "+strtrim(kk,2),col=BYTCOL(kk),charsize=2.5,charthick=thickall,alignment=1.0
      endfor
      col = getcolor(/load)
   endif else begin
      oplot,Xdat,Ydat,col=col.black,psym=8
      if EB eq 1 then oploterror,Xdat,Ydat,Xdaterr,Ydaterr,psym=3,col=col.black,ERRCOLOR=col.black,ERRTHICK=thickall
   endelse

   ;XYOUTS,DX*0.05+XR[0],DY*0.92+YR[0],"ID: "+strtrim(IDobj,2),col=col.black,charsize=2.5,charthick=thickall


   ; --- overplotting fits ---
   xx = findgen(30)-15.
   oplot,xx+mean(Xdat),xx/result2(0)+mean(Ydat)-result2(7)/result2(0),linestyle=2,thick=thickall,col=col.blue
   oplot,xx+mean(Xdat),result(0)*xx+mean(Ydat)+result(7),linestyle=0,thick=thickall,col=col.black
   oplot,xx+mean(Xdat),1/(1-resultgr(0))*XX+mean(Ydat)+resultgr(7)/(1-resultgr(0)),linestyle=2,thick=thickall,col=col.red


   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif
   Nw = Nw+1                    ; incremeting window number by 1
   ;=============================================================================================

   ;========================================== xaxis g yaxis g-r ===================================================
   Xdat = gOBJ
   Ydat = grOBJ
   Xdaterr = gerrOBJ
   Ydaterr = grerrOBJ

   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/fitcomp_grVSg_object'+strtrim(ii,2)+'.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
      !P.FONT = 0                        ; allowing to change font of device
      device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot      
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'gVSgr'
      thickall = 2
   endelse

   ; setting plot range
   XR = [max(Xdat)+0.1,min(Xdat)-0.1]
   YR = [min(Ydat)-0.1,max(Ydat)+0.1]
   ;XR = [22,19.0]
   ;YR = [0,1]
   ;XR = [20.4,19.5] ; cutout of object 1
   ;YR = [-0.1,0.65] ; cutout of object 1

   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='g' $
        , ytitle ='g-r' $
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

   plotsym,0,1.5,/fill

   if SS eq 1 then begin
      LOADCT,39
      device,decomposed=0
      BYTCOL   = BYTSCL(FINDGEN(Nseasons+1))  ; array used to color datasets

      for kk = 0,Nseasons-1 do begin            ; looping over seasons
         Sent = where(mjdOBJ gt seasons(kk) AND mjdOBJ lt seasons(kk+1),Nsent)

         if Sent ne [-1] and Nsent gt 1 then begin    ; checking that there are obs in season
            oplot,Xdat(sent),Ydat(sent),col=BYTCOL(kk),psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  
               oploterror,Xdat(sent),Ydat(sent),Xdaterr(sent),Ydaterr(sent),psym=3,col=BYTCOL(kk),ERRCOLOR=BYTCOL(kk),ERRTHICK=thickall
            endif
         endif

         if Nsent eq 1 AND Sent ne [-1] then begin
            oplot,fltarr(2)+Xdat(sent),fltarr(2)+Ydat(sent),col=BYTCOL(kk),psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  
               oploterror,fltarr(2)+Xdat(sent),fltarr(2)+Ydat(sent),fltarr(2)+Xdaterr(sent),fltarr(2)+Ydaterr(sent),psym=3,col=BYTCOL(kk),ERRCOLOR=BYTCOL(kk),ERRTHICK=thickall
            endif
         endif
            XYOUTS,DX*0.95+XR[0],DY*0.06*(kk+1)+YR[0],"year "+strtrim(kk,2),col=BYTCOL(kk),charsize=2.5,charthick=thickall,alignment=1.0
      endfor
      col = getcolor(/load)
   endif else begin
      oplot,Xdat,Ydat,col=col.black,psym=8
      if EB eq 1 then oploterror,Xdat,Ydat,Xdaterr,Ydaterr,psym=3,col=col.black,ERRCOLOR=col.black,ERRTHICK=thickall
   endelse

   ;XYOUTS,DX*0.05+XR[0],DY*0.92+YR[0],"ID: "+strtrim(IDobj,2),col=col.black,charsize=2.5,charthick=thickall

   ; --- overplotting fits ---
   xx = findgen(30)-15.
   oplot,xx+mean(Xdat),result2(0)*xx+mean(Ydat)+result2(7),linestyle=0,thick=thickall,col=col.blue
   oplot,xx+mean(Xdat),xx/result(0)+mean(Ydat)-result(7)/result(0),linestyle=2,thick=thickall,col=col.black
   oplot,xx+mean(Xdat),(1-resultgr(0))*XX+mean(Ydat)-resultgr(7),linestyle=2,thick=thickall,col=col.red

   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif
   Nw = Nw+1                    ; incremeting window number by 1
   ;=============================================================================================

   ;========================================= xaxis g yaxis r ====================================================
   Xdat = gOBJ
   Ydat = rOBJ
   Xdaterr = gerrOBJ
   Ydaterr = rerrOBJ

   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/fitcomp_rVSg_object'+strtrim(ii,2)+'.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
      !P.FONT = 0                        ; allowing to change font of device
      device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot      
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'rVSg'
      thickall = 2
   endelse

   ; setting plot range
   XR = [max(Xdat)+0.1,min(Xdat)-0.1]
   YR = [max(Ydat)+0.1,min(Ydat)-0.1]
   ;XR = [22,19.0]
   ;YR = [22,19.0]
   ;XR = [20.4,19.5] ; cutout of object 1
   ;YR = [20.35,19.3] ; cutout of object 1

   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='g' $
        , ytitle ='r' $
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

   plotsym,0,1.5,/fill

   if SS eq 1 then begin
      LOADCT,39
      device,decomposed=0
      BYTCOL   = BYTSCL(FINDGEN(Nseasons+1))  ; array used to color datasets

      for kk = 0,Nseasons-1 do begin            ; looping over seasons
         Sent = where(mjdOBJ gt seasons(kk) AND mjdOBJ lt seasons(kk+1),Nsent)

         if Sent ne [-1] and Nsent gt 1 then begin    ; checking that there are obs in season
            oplot,Xdat(sent),Ydat(sent),col=BYTCOL(kk),psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  
               oploterror,Xdat(sent),Ydat(sent),Xdaterr(sent),Ydaterr(sent),psym=3,col=BYTCOL(kk),ERRCOLOR=BYTCOL(kk),ERRTHICK=thickall
            endif
         endif

         if Nsent eq 1 AND Sent ne [-1] then begin
            oplot,fltarr(2)+Xdat(sent),fltarr(2)+Ydat(sent),col=BYTCOL(kk),psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  
               oploterror,fltarr(2)+Xdat(sent),fltarr(2)+Ydat(sent),fltarr(2)+Xdaterr(sent),fltarr(2)+Ydaterr(sent),psym=3,col=BYTCOL(kk),ERRCOLOR=BYTCOL(kk),ERRTHICK=thickall
            endif
         endif
            XYOUTS,DX*0.95+XR[0],DY*0.06*(kk+1)+YR[0],"year "+strtrim(kk,2),col=BYTCOL(kk),charsize=2.5,charthick=thickall,alignment=1.0
      endfor
      col = getcolor(/load)
   endif else begin
      oplot,Xdat,Ydat,col=col.black,psym=8
      if EB eq 1 then oploterror,Xdat,Ydat,Xdaterr,Ydaterr,psym=3,col=col.black,ERRCOLOR=col.black,ERRTHICK=thickall
   endelse

   ;XYOUTS,DX*0.05+XR[0],DY*0.92+YR[0],"ID: "+strtrim(IDobj,2),col=col.black,charsize=2.5,charthick=thickall

   ; --- overplotting fits ---
   xx = findgen(30)-15.
   oplot,XX+mean(Xdat),(1-result2(0))*XX+mean(Ydat)-result2(7),linestyle=2,thick=thickall,col=col.blue
   oplot,xx+mean(Xdat),(1-1/result(0))*xx+mean(Ydat)+result(7)/result(0),linestyle=2,thick=thickall,col=col.black
   oplot,xx+mean(Xdat),resultgr(0)*XX+mean(Ydat)+resultgr(7),linestyle=0,thick=thickall,col=col.red

   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif
   Nw = Nw+1                    ; incremeting window number by 1
   ;=============================================================================================
;wait,2.
endfor ; end of inspection/plotting of objects (ii)

if vb eq 1 then print,' '
if vb eq 1 then print,':: fitComparison.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END


