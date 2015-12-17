;+
;----------------------------
;   NAME
;----------------------------
; plotcolorcolorspace.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure plotting objects from a fits file in SDSS color space
;----------------------------
;   COMMENTS
;----------------------------
; images are put in a folder called moviefigs - make sure it is present
; in the directory
;----------------------------
;   INPUTS:
;----------------------------
; fitsfile        : string containing name and path of fitsfile with input data
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /ALLINONE       : set this keyword to get all objects plotted in the
;                   same window. Deafult is set up for creating eps
;                   'movie frames' by replotting the window for each object
; /SEASONS        : set /SEASONS to color code the objects with their seasons
; /ERRBAR         : set this keyword to overplot errorbars
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotcolorcolorspace,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_11_09:58:09_2010.fits',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-08-11  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotcolorcolorspace,fitsfile,ALLINONE=ALLINONE,SEASONS=SEASONS,ERRBAR=ERRBAR,EPS=EPS,VERBOSE=VERBOSE

AI = n_elements(ALLINONE)
SS = n_elements(SEASONS)
EB = n_elements(ERRBAR)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

s = mrdfits(fitsfile,1)                               ; reading input data
uniqent = uniq(s.headobjid)                           ; entries of objects
IDs     = s(uniqent).headobjid                        ; getting uniq objids
zs      = s(uniqent).z                                ; the corresponding redshifts

; calculating colors
ug         = s.PSFMAG_U-s.PSFMAG_G
gr         = s.PSFMAG_G-s.PSFMAG_R
ri         = s.PSFMAG_R-s.PSFMAG_I
iz         = s.PSFMAG_I-s.PSFMAG_Z
gi         = s.PSFMAG_G-s.PSFMAG_I

ug_dered   = s.DEREDPSFMAG_U-s.DEREDPSFMAG_G
gr_dered   = s.DEREDPSFMAG_G-s.DEREDPSFMAG_R
ri_dered   = s.DEREDPSFMAG_R-s.DEREDPSFMAG_I
iz_dered   = s.DEREDPSFMAG_I-s.DEREDPSFMAG_Z
gi_dered   = s.DEREDPSFMAG_G-s.DEREDPSFMAG_I

; caculating estimated errors on de-reddened magnitudes like in Richards et al 2002
errfrac = 0.15 ; the fraction of the exctinction estimated to be its error acc. to Schlegel et al. (1998)

PSFerr_u  = s.PSFMAGERR_u                                                ; error on PSF mag
ext_u     = s.PSFMAG_u - s.DEREDPSFMAG_u                                 ; extinction
DRerr_u   = sqrt( PSFerr_u*PSFerr_u + (errfrac*ext_u)*(errfrac*ext_u) )  ; error on extinction corrected mag

PSFerr_g  = s.PSFMAGERR_G                                                ; error on PSF mag
ext_g     = s.PSFMAG_G - s.DEREDPSFMAG_G                                 ; extinction
DRerr_g   = sqrt( PSFerr_g*PSFerr_g + (errfrac*ext_g)*(errfrac*ext_g) )  ; error on extinction corrected mag
            
PSFerr_r  = s.PSFMAGERR_r                                                ; error on PSF mag
ext_r     = s.PSFMAG_r - s.DEREDPSFMAG_r                                 ; extinction
DRerr_r   = sqrt( PSFerr_r*PSFerr_r + (errfrac*ext_r)*(errfrac*ext_r) )  ; error on extinction corrected mag

PSFerr_i  = s.PSFMAGERR_i                                                ; error on PSF mag
ext_i     = s.PSFMAG_i - s.DEREDPSFMAG_i                                 ; extinction
DRerr_i   = sqrt( PSFerr_i*PSFerr_i + (errfrac*ext_i)*(errfrac*ext_i) )  ; error on extinction corrected mag

PSFerr_z  = s.PSFMAGERR_z                                                ; error on PSF mag
ext_z     = s.PSFMAG_z - s.DEREDPSFMAG_z                                 ; extinction
DRerr_z   = sqrt( PSFerr_z*PSFerr_z + (errfrac*ext_z)*(errfrac*ext_z) )  ; error on extinction corrected mag

DRerr_ug  = sqrt(DRerr_u^2. + DRerr_g^2)                                 ; the color propagated error
DRerr_gr  = sqrt(DRerr_g^2. + DRerr_r^2)                                 ; the color propagated error
DRerr_ri  = sqrt(DRerr_r^2. + DRerr_i^2)                                 ; the color propagated error
DRerr_iz  = sqrt(DRerr_i^2. + DRerr_z^2)                                 ; the color propagated error

; reading MJD and defining seasons
mjd_u = s.MJD_U
mjd_g = s.MJD_G
mjd_r = s.MJD_R
mjd_i = s.MJD_I
mjd_z = s.MJD_Z
seasons = [5.1,5.2,5.24,5.28,5.32,5.35,5.38,5.42,5.5]*10^4. ; the mjd limits of the seasons 
MJD99   = 51330   ; MJD for June 1st 1999
seasons = [MJD99,MJD99+365,MJD99+2*365,MJD99+3*365,MJD99+4*365,MJD99+5*365,MJD99+6*365,MJD99+7*365,MJD99+8*365,MJD99+9*365]

; the colors of the seasons
Scol    = strarr(9)
Scol(0) = 'magenta'
Scol(1) = 'pink'
Scol(2) = 'red'
Scol(3) = 'orange'
Scol(4) = 'yellow'
Scol(5) = 'green'
Scol(6) = 'sky'
Scol(7) = 'blue'
Scol(8) = 'navy'

Nw = 0
;=============================================================================================
;= = = ug vs gr = = =
!p.multi = [0,0,0]
for ii=0,n_elements(uniqent)-1 do begin   ; looping over objects
ent=where(s.headobjid eq IDs(ii) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'moviefigs/ugVSgr'+strtrim(zs(ii),2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'ug VS gr'
   thickall = 2
endelse
; setting plot range
XR = [min(gr_dered(ent))-(max(gr_dered(ent))-min(gr_dered(ent)))*0.1,max(gr_dered(ent))+(max(gr_dered(ent))-min(gr_dered(ent)))*0.1]
YR = [min(ug_dered(ent))-(max(ug_dered(ent))-min(ug_dered(ent)))*0.1,max(ug_dered(ent))+(max(ug_dered(ent))-min(ug_dered(ent)))*0.1]
;XR = [median(gr_dered(ent))-1.0,median(gr_dered(ent))+1.0]
;YR = [median(ug_dered(ent))-1.0,median(ug_dered(ent))+1.0]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

print,'ug',IDs(ii),min(ug_dered(ent)),max(ug_dered(ent))
print,'gr',IDs(ii),min(gr_dered(ent)),max(gr_dered(ent))

;=== PLOTTING LIGHT CURVE(S) ===
plot,ug,gr, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='u-g' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(ii) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_U ne -9999 AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_u(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            c1_sort = ug_dered(Sent)
            c1_sort = c1_sort(entsort)
            c2_sort  = gr_dered(Sent)
            c2_sort  = c2_sort(entsort)
            c1_err   = DRerr_ug(Sent)
            c1_err   = c1_err(entsort)
            c2_err   = DRerr_ug(Sent)
            c2_err   = c2_err(entsort)

            PLOTSYM,0,2.3,/FILL
            oplot,c2_sort(1:n_elements(Sent)-1),c1_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,c2_sort(Sent)*0.+c2_sort(0),c1_sort(Sent)*0.+c1_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; overplotting errorbars
               oploterror,c2_sort,c1_sort,c2_err,c1_err,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
            ; ====================================
            for tt=0,n_elements(Sent)-1 do begin ; printing 'observation number' on point
               CCC2 = GETCOLOR('black', 100)
               if kk eq n_elements(seasons)-2 then CCC2 = GETCOLOR('white', 100)
               XYOUTS,c2_sort(tt)-0.008*DX,c1_sort(tt)-0.008*DY,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            endfor
         endif

         if n_elements(Sent) eq 1 AND Sent ne [-1] then begin
            PLOTSYM,3,2.3,/FILL
            oplot,findgen(4)*0.0+ug_dered(Sent),findgen(4)*0.0+gr_dered(Sent),col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin ; overplotting errorbars
               oploterror,ug_dered(Sent),gr_dered(Sent),DRerr_ug,DRerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

         XYOUTS,DX*0.70+XR[0],DY*0.95+YR[0],"June'99-June'00",col=col.magenta,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.90+YR[0],"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.85+YR[0],"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.80+YR[0],"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.75+YR[0],"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.70+YR[0],"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.65+YR[0],"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.60+YR[0],"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.55+YR[0],"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

         XYOUTS,DX*0.05+XR[0],DY*0.10+YR[0],'spec z: '+STRTRIM(zs(ii),2) ,col=col.black,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.05+XR[0],DY*0.05+YR[0],'DR7 ID: '+STRTRIM(IDs(ii),2),col=col.black,charsize=1.5,charthick=thickall
   endif else begin
      PLOTSYM,0,1.5,/FILL
      oplot,findgen(4)*0.0+ug_dered(ent),findgen(4)*0.0+gr_dered(ent),col=col.black,psym=8,thick=thickall
      if EB eq 1 then oploterror,ug_dered(ent),gr_dered(ent),DRerr_ug,DRerr_gr,psym=3,col=col.black,ERRCOLOR=col.black,ERRTHICK=thickall
   endelse

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;= = = gr vs ri = = =
!p.multi = [0,0,0]
for ii=0,n_elements(uniqent)-1 do begin   ; looping over objects
ent=where(s.headobjid eq IDs(ii) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'moviefigs/grVSri'+strtrim(zs(ii),2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'gr VS ri'
   thickall = 2
endelse
; setting plot range
XR = [min(ri_dered(ent))-(max(ri_dered(ent))-min(ri_dered(ent)))*0.1,max(ri_dered(ent))+(max(ri_dered(ent))-min(ri_dered(ent)))*0.1]
YR = [min(gr_dered(ent))-(max(gr_dered(ent))-min(gr_dered(ent)))*0.1,max(gr_dered(ent))+(max(gr_dered(ent))-min(gr_dered(ent)))*0.1]
;XR = [median(ri_dered(ent))-1.0,median(ri_dered(ent))+1.0]
;YR = [median(gr_dered(ent))-1.0,median(gr_dered(ent))+1.0]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

print,'ri',IDs(ii),min(ri_dered(ent)),max(ri_dered(ent))
print,'gr',IDs(ii),min(gr_dered(ent)),max(gr_dered(ent))

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,ri, col=col.black    $
        , /NODATA $
        , xtitle ='r-i' $
        , ytitle ='g-r' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(ii) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999 AND s.PSFMAG_I ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_u(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            c1_sort = gr_dered(Sent)
            c1_sort = c1_sort(entsort)
            c2_sort  = ri_dered(Sent)
            c2_sort  = c2_sort(entsort)
            c1_err   = DRerr_gr(Sent)
            c1_err   = c1_err(entsort)
            c2_err   = DRerr_gr(Sent)
            c2_err   = c2_err(entsort)

            PLOTSYM,0,2.3,/FILL
            oplot,c2_sort(1:n_elements(Sent)-1),c1_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,c2_sort(Sent)*0.+c2_sort(0),c1_sort(Sent)*0.+c1_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; overplotting errorbars
               oploterror,c2_sort,c1_sort,c2_err,c1_err,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
            ; ====================================
            for tt=0,n_elements(Sent)-1 do begin ; printing 'observation number' on point
               CCC2 = GETCOLOR('black', 100)
               if kk eq n_elements(seasons)-2 then CCC2 = GETCOLOR('white', 100)
               XYOUTS,c2_sort(tt)-0.008*DX,c1_sort(tt)-0.008*DY,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            endfor
         endif

         if n_elements(Sent) eq 1 AND Sent ne [-1] then begin
            PLOTSYM,3,2.3,/FILL
            oplot,findgen(4)*0.0+gr_dered(Sent),findgen(4)*0.0+ri_dered(Sent),col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin ; overplotting errorbars
               oploterror,gr_dered(Sent),ri_dered(Sent),DRerr_gr,DRerr_ri,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

         XYOUTS,DX*0.70+XR[0],DY*0.95+YR[0],"June'99-June'00",col=col.magenta,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.90+YR[0],"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.85+YR[0],"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.80+YR[0],"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.75+YR[0],"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.70+YR[0],"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.65+YR[0],"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.60+YR[0],"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.70+XR[0],DY*0.55+YR[0],"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

         XYOUTS,DX*0.05+XR[0],DY*0.10+YR[0],'spec z: '+STRTRIM(zs(ii),2) ,col=col.black,charsize=1.5,charthick=thickall
         XYOUTS,DX*0.05+XR[0],DY*0.05+YR[0],'DR7 ID: '+STRTRIM(IDs(ii),2),col=col.black,charsize=1.5,charthick=thickall
   endif else begin
      PLOTSYM,0,1.5,/FILL
      oplot,findgen(4)*0.0+gr_dered(ent),findgen(4)*0.0+ri_dered(ent),col=col.black,psym=8,thick=thickall
      if EB eq 1 then oploterror,gr_dered(ent),ri_dered(ent),DRerr_gr,DRerr_ri,psym=3,col=col.black,ERRCOLOR=col.black,ERRTHICK=thickall
   endelse

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

if AI eq 1 then print,'ALLINONE keyword not enabled...'


if vb eq 1 then print,' '
if vb eq 1 then print,':: plotcolorcolorspace.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
