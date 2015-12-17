;+
;----------------------------
;   NAME
;----------------------------
; plotCOLMAG.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Plotting color-magnitude and magnitude-magnitude diagrams of the
; ('sub') fits files extracted with extractFitsSubarray.pro
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; subfits           : string containing name and path of sub fits file to read
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /ONEBYONE       : Some plots has a one-by-one plotting scheme
;                   enabled. Thus if /ONEBYONE is set these plot will
;                   plot the individual objects onebyone (erasing the
;                   prior objects. WARNING it waits 1 second between
;                   each plot so large data sets are not preferable
;                   with this keyword.
; /SEASONS        : set /SEASONS to color code the objects with their seasons
; /NOTQSO         : set this keyword to plot objects which are not QSO and 
;                   hence don't have matches in the Shen catalog. The only
;                   thing this keyword does is to skip plots with Shen data
; /NODR           : set /NODR if no dereddened PSF magnitudes are available
;                   in fitsfile (as for the Sesar RRL). All calculations
;                   involving dereddened PSF mags will be done with PSF mags instead.
; /ERRBAR         : set this keyword to overplot errorbars and to
;                   estimate the slope of each season. This slope will
;                   be written to an ascii file together with the
;                   objid, redshift and slope of all data points irrespective of season.
;                   Furthermore Kendall's tau is estimated for each season
;                   and added to the file.
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; ====== 0.2< z <4.0   0.15< A <2.0   0.0< gamma <5.0  - outliers marked ====== 
; IDL> plotCOLMAG,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/eps

; ====== 1.4 -- 1.6 	& 0.00 -- 2.00	& 0.00 -- 5.00
; IDL> plotCOLMAG,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Fri_Aug_20_07:56:48_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 1.6 -- 1.8 	& 0.15 -- 2.00 	& 0.00 -- 5.00
; IDL> plotCOLMAG,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p60000z1p80000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_14:47:04_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 1.8 -- 2.0 	& 0.15 -- 2.00	& 0.00 -- 5.00
; IDL> plotCOLMAG,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p80000z2p00000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:16_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 2.0 -- 2.3 	& 0.15 -- 2.00	& 0.00 -- 5.00
; IDL> plotCOLMAG,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_2p00000z2p30000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:35_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

;top19
; IDL> plotCOLMAG,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_11_09:58:09_2010.fits',/VERBOSE,/SEASONS
;
;
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-18  started by K. B. Schmidt (MPIA) (taken from plot_subFITSfile)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ properdist.pro
@ mag2flux.pro
@ namedate.pro
;----------------------------
;-
PRO plotCOLMAG,subfits,ONEBYONE=ONEBYONE,SEASONS=SEASONS,NOTQSO=NOTQSO,NODR=NODR,ERRBAR=ERRBAR,EPS=EPS,VERBOSE=VERBOSE

WT = n_elements(ONEBYONE)
SS = n_elements(SEASONS)
ND = n_elements(NODR)
EB = n_elements(ERRBAR)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

namedate,subfits,path,name,extension,date,dateus
s = mrdfits(subfits,1)                                ; reading input data
uniqent = uniq(s.headobjid)                           ; entries of objects
IDs     = s(uniqent).headobjid                        ; getting uniq objids
zs      = s(uniqent).z                                ; the corresponding redshifts


readcol,'S82objid_z_Agamma.dat',idall,zall,Aall,gammaall    ; reading redshift, A and gamma for all S82 QSOs for sample comparison plots

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

; calculating colors
ug         = s.PSFMAG_U-s.PSFMAG_G
gr         = s.PSFMAG_G-s.PSFMAG_R
ri         = s.PSFMAG_R-s.PSFMAG_I
iz         = s.PSFMAG_I-s.PSFMAG_Z
gi         = s.PSFMAG_G-s.PSFMAG_I

PSFmag_u         = s.PSFMAG_U
PSFmag_g         = s.PSFMAG_G
PSFmag_r         = s.PSFMAG_R
PSFmag_i         = s.PSFMAG_I
PSFmag_z         = s.PSFMAG_G


Objectnumber = 1


Nw = 0
;=============================================================================================
; = = = Color mag variation = = =
slopes1 = fltarr(n_elements(IDs))
slopes2 = fltarr(n_elements(IDs))

!p.multi = [0,0,0]

;for i=0,n_elements(IDs)-1 do begin  ; loop over ids for movie frames
for i=0,Objectnumber-1 do begin  ; loop over ids for movie frames
   ent=where(s.headobjid eq IDs(i) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers
   XVAL  = gr(ent)
   YVAL  = PSFMAG_G(ent)

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'moviefigs/colmag_gVSgr_Mbh'+strtrim(zs(i),2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Color magnitude plot with Mbh color'
   thickall = 2
endelse
; setting plot range
;XR = [-0.5,1.4]
;YR = [23,17]
;XR = [median(XVAL)-0.5,median(XVAL)+0.5]
;YR = [median(YVAL)+1.0,median(YVAL)-1.0]
XR = [min(XVAL)-0.1,max(XVAL)+0.1]
YR = [max(YVAL)+0.1,min(YVAL)-0.1]



;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='g' $
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


;for i=0,n_elements(IDs)-1 do begin  ; loop over ids    uncomment for movie
   xx = findgen(300)/300*(XR(1)-XR(0))+XR(0)

   gerrpsf = s(ent).PSFMAGERR_G
   YERR    = gerrpsf
   START   = [1,20]                    ; initial guess for fit
   result  = MPFITFUN('LINE',XVAL,YVAL,YERR, start,/QUIET)
   slopes1(i) = result(1)

   XVAL2   = PSFMAG_G(ent)
   YVAL2   = gr(ent)
   START2  = [-12,0.5]                    ; initial guess for fit
   result2  = MPFITFUN('LINE',XVAL2,YVAL2,YERR2, start2,/QUIET)
   slopes2(i) = result2(1)

   oplot,xx,result(0)+xx*result(1),col=col.black,linestyle=0,thick=thickall
   plotsym,0,1.5,/fill
   if SS ne 1 then oplot,gr(ent),PSFMAG_G(ent),col=col.black,psym=8;,symsize=4*Mbh(i)/Max(Mbh)


   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(i) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_r(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            gr_sort = gr(Sent)
            gr_sort = gr_sort(entsort)
            g_sort  = PSFMAG_G(Sent)
            g_sort  = g_sort(entsort)

            PLOTSYM,0,2.3,/FILL
            ; oplot,gr(Sent),PSFMAG_G(Sent),col=CCC,psym=8,thick=thickall 
            oplot,gr_sort(1:n_elements(Sent)-1),g_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,gr(Sent)*0.+gr_sort(0),PSFMAG_G(Sent)*0.+g_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                               ; the g-r color propagated error

               oploterror,gr_sort,g_sort,PSFerr_gr,PSFerr_g,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall

               XVAL_s    = gr_sort 
               YVAL_s    = g_sort
               YERR_s    = PSFerr_g
               START_s   = [1,20]                    ; initial guess for fit
               result_s  = MPFITFUN('LINE',XVAL_s,YVAL_s,YERR_s,start_s,/QUIET)
               ; estimating slopes with g as x variable instead of g-r
               XVAL_s2   = g_sort
               YVAL_s2   = gr_sort 
               YERR_s2   = PSFerr_gr
               START_s2  = [-12,0.5]                    ; initial guess for fit
               result_s2 = MPFITFUN('LINE',XVAL_s2,YVAL_s2,YERR_s2,start_s2,/QUIET)

               ;oplot,xx,result_s(0)+xx*result_s(1),col=CCC,linestyle=0,thick=thickall ; overplotting the season slopes
            endif
            ; ====================================
            for tt=0,n_elements(Sent)-1 do begin ; printing 'observation number' on point
               CCC2 = GETCOLOR('black', 100)
               if kk eq n_elements(seasons)-2 then CCC2 = GETCOLOR('white', 100)
               XYOUTS,gr_sort(tt)-0.008,g_sort(tt)+0.015,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall
            endfor
         endif

     
         if n_elements(Sent) eq 1 AND Sent ne [-1] then begin
            PLOTSYM,3,2.3,/FILL
            oplot,findgen(4)*0.0+gr(Sent),findgen(4)*0.0+PSFMAG_G(Sent),col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                                 ; the g-r color propagated error
               oploterror,gr(Sent),PSFMAG_G(Sent),PSFerr_gr,PSFerr_g,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.95+YR[0],"Sep-Dec '99",col=col.magenta,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.90+YR[0],"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.85+YR[0],"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.80+YR[0],"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.75+YR[0],"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.70+YR[0],"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.65+YR[0],"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.60+YR[0],"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.55+YR[0],"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.10+YR[0],'spec z: '+STRTRIM(zs(I),2) ,col=col.black,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.05+YR[0],'DR7 ID: '+STRTRIM(IDs(I),2),col=col.black,charsize=1.5,charthick=thickall
   endif

   if WT eq 1 then begin
      wait,1.0
      oplot,gr(ent),PSFMAG_G(ent),psym=2,col=col.white ; waiting and then 'erasing' data
      oplot,xx,result(0)+xx*result(1),col=col.white,linestyle=0,thick=thickall
   endif
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames

close,22
Nw = Nw+1                                            ; incremeting window number by 1

;=============================================================================================

;=============================================================================================
; = = = Color mag variation = = =
slopes1 = fltarr(n_elements(IDs))
slopes2 = fltarr(n_elements(IDs))

!p.multi = [0,0,0]

;for i=0,n_elements(IDs)-1 do begin  ; loop over ids for movie frames
for i=0,Objectnumber-1 do begin  ; loop over ids for movie frames
   ent=where(s.headobjid eq IDs(i) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers
   XVAL  = PSFMAG_G(ent)
   YVAL  = gr(ent)


if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'moviefigs/colmag_grVSg_Mbh'+strtrim(zs(i),2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Color magnitude plot with Mbh color'
   thickall = 2
endelse
; setting plot range
XR = [max(XVAL)+0.1,min(XVAL)-0.1]
YR = [min(YVAL)-0.1,max(YVAL)+0.1]



;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='g' $
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

   plotsym,0,1.5,/fill
   if SS ne 1 then oplot,PSFMAG_G(ent),gr(ent),col=col.black,psym=8;,symsize=4*Mbh(i)/Max(Mbh)


   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(i) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_r(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            gr_sort = gr(Sent)
            gr_sort = gr_sort(entsort)
            g_sort  = PSFMAG_G(Sent)
            g_sort  = g_sort(entsort)

            PLOTSYM,0,2.3,/FILL
            ; oplot,gr(Sent),PSFMAG_G(Sent),col=CCC,psym=8,thick=thickall 
            oplot,g_sort(1:n_elements(Sent)-1),gr_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,PSFMAG_G(Sent)*0.+g_sort(0),gr(Sent)*0.+gr_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                               ; the g-r color propagated error

               oploterror,g_sort,gr_sort,PSFerr_g,PSFerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall

               XVAL_s    = gr_sort 
               YVAL_s    = g_sort
               YERR_s    = PSFerr_g
               START_s   = [1,20]                    ; initial guess for fit
               result_s  = MPFITFUN('LINE',XVAL_s,YVAL_s,YERR_s,start_s,/QUIET)
               ; estimating slopes with g as x variable instead of g-r
               XVAL_s2   = g_sort
               YVAL_s2   = gr_sort 
               YERR_s2   = PSFerr_gr
               START_s2  = [-12,0.5]                    ; initial guess for fit
               result_s2 = MPFITFUN('LINE',XVAL_s2,YVAL_s2,YERR_s2,start_s2,/QUIET)

               ;oplot,xx,result_s(0)+xx*result_s(1),col=CCC,linestyle=0,thick=thickall ; overplotting the season slopes
            endif
            ; ====================================
            for tt=0,n_elements(Sent)-1 do begin ; printing 'observation number' on point
               CCC2 = GETCOLOR('black', 100)
               if kk eq n_elements(seasons)-2 then CCC2 = GETCOLOR('white', 100)
               XYOUTS,gr_sort(tt)-0.008,g_sort(tt)+0.015,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall
            endfor
         endif

     
         if n_elements(Sent) eq 1 AND Sent ne [-1] then begin
            PLOTSYM,3,2.3,/FILL
            oplot,findgen(4)*0.0+PSFMAG_G(Sent),findgen(4)*0.0+gr(Sent),col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                                 ; the g-r color propagated error
               oploterror,PSFMAG_G(Sent),gr(Sent),PSFerr_g,PSFerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.95+YR[0],"Sep-Dec '99",col=col.magenta,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.90+YR[0],"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.85+YR[0],"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.80+YR[0],"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.75+YR[0],"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.70+YR[0],"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.65+YR[0],"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.60+YR[0],"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.55+YR[0],"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.10+YR[0],'spec z: '+STRTRIM(zs(I),2) ,col=col.black,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.05+YR[0],'DR7 ID: '+STRTRIM(IDs(I),2),col=col.black,charsize=1.5,charthick=thickall
   endif

   if WT eq 1 then begin
      wait,1.0
      oplot,gr(ent),PSFMAG_G(ent),psym=2,col=col.white ; waiting and then 'erasing' data
      oplot,xx,result(0)+xx*result(1),col=col.white,linestyle=0,thick=thickall
   endif
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames

close,22
Nw = Nw+1                                            ; incremeting window number by 1
stop
;=============================================================================================

;=============================================================================================
; = = = Color magmag variation = = =
slopes1 = fltarr(n_elements(IDs))
slopes2 = fltarr(n_elements(IDs))

!p.multi = [0,0,0]

;for i=0,n_elements(IDs)-1 do begin  ; loop over ids for movie frames
for i=0,Objectnumber-1 do begin  ; loop over ids for movie frames
   ent=where(s.headobjid eq IDs(i) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers
   XVAL  = gr(ent)
   YVAL  = (PSFMAG_G(ent)+PSFMAG_R(ent))/2.

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'moviefigs/colmagmagVSgr_Mbh'+strtrim(zs(i),2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Color magnitude plot with Mbh color'
   thickall = 2
endelse
; setting plot range
;XR = [-0.5,1.4]
;YR = [23,17]
;XR = [median(XVAL)-0.5,median(XVAL)+0.5]
;YR = [median(YVAL)+1.0,median(YVAL)-1.0]
XR = [min(XVAL)-0.1,max(XVAL)+0.1]
YR = [max(YVAL)+0.1,min(YVAL)-0.1]



;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle ='(g+r)/2' $
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
;for i=0,n_elements(IDs)-1 do begin  ; loop over ids    uncomment for movie
   xx = findgen(300)/300*(XR(1)-XR(0))+XR(0)

   if SS ne 1 then oplot,gr(ent),(PSFMAG_G(ent)+PSFMAG_R(ent))/2,col=col.black,psym=2;,symsize=4*Mbh(i)/Max(Mbh)

   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(i) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_r(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            gr_sort = gr(Sent)
            gr_sort = gr_sort(entsort)
            g_sort  = (PSFMAG_G(Sent)+PSFMAG_R(Sent))/2
            g_sort  = g_sort(entsort)

            PLOTSYM,0,2.3,/FILL
            ; oplot,gr(Sent),PSFMAG_G(Sent),col=CCC,psym=8,thick=thickall 
            oplot,gr_sort(1:n_elements(Sent)-1),g_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,gr(Sent)*0.+gr_sort(0),PSFMAG_G(Sent)*0.+g_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                               ; the g-r color propagated error

               oploterror,gr_sort,g_sort,PSFerr_gr,PSFerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall

               XVAL_s    = gr_sort 
               YVAL_s    = g_sort
               YERR_s    = PSFerr_g
               START_s   = [1,20]                    ; initial guess for fit
               result_s  = MPFITFUN('LINE',XVAL_s,YVAL_s,YERR_s,start_s,/QUIET)
               ; estimating slopes with g as x variable instead of g-r
               XVAL_s2   = g_sort
               YVAL_s2   = gr_sort 
               YERR_s2   = PSFerr_gr
               START_s2  = [-12,0.5]                    ; initial guess for fit
               result_s2 = MPFITFUN('LINE',XVAL_s2,YVAL_s2,YERR_s2,start_s2,/QUIET)

               ;oplot,xx,result_s(0)+xx*result_s(1),col=CCC,linestyle=0,thick=thickall ; overplotting the season slopes
            endif
            ; ====================================
            for tt=0,n_elements(Sent)-1 do begin ; printing 'observation number' on point
               CCC2 = GETCOLOR('black', 100)
               if kk eq n_elements(seasons)-2 then CCC2 = GETCOLOR('white', 100)
               XYOUTS,gr_sort(tt)-0.008,g_sort(tt)+0.015,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall
            endfor
         endif

     
         if n_elements(Sent) eq 1 AND Sent ne [-1] then begin
            PLOTSYM,3,2.3,/FILL
            oplot,findgen(4)*0.0+gr(Sent),findgen(4)*0.0+(PSFMAG_G(Sent)+PSFMAG_R(Sent))/2,col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                                 ; the g-r color propagated error
               oploterror,gr(Sent),(PSFMAG_G(Sent)+PSFMAG_R(Sent))/2,PSFerr_gr,PSFerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.95+YR[0],"Sep-Dec '99",col=col.magenta,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.90+YR[0],"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.85+YR[0],"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.80+YR[0],"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.75+YR[0],"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.70+YR[0],"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.65+YR[0],"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.60+YR[0],"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.6+XR[0],(YR[1]-YR[0])*0.55+YR[0],"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.10+YR[0],'spec z: '+STRTRIM(zs(I),2) ,col=col.black,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.05+YR[0],'DR7 ID: '+STRTRIM(IDs(I),2),col=col.black,charsize=1.5,charthick=thickall
   endif

   if WT eq 1 then begin
      wait,1.0
      oplot,gr(ent),(PSFMAG_G(ent)+PSFMAG_R(ent))/2,psym=2,col=col.white ; waiting and then 'erasing' data
      oplot,xx,result(0)+xx*result(1),col=col.white,linestyle=0,thick=thickall
   endif
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames

close,22
Nw = Nw+1                                            ; incremeting window number by 1

;=============================================================================================


;=============================================================================================
; = = = mag - mag variation = = =
slopes1 = fltarr(n_elements(IDs))
slopes2 = fltarr(n_elements(IDs))

!p.multi = [0,0,0]

;for i=0,n_elements(IDs)-1 do begin  ; loop over ids for movie frames
for i=0,Objectnumber-1 do begin  ; loop over ids for movie frames
   ent=where(s.headobjid eq IDs(i) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers
   XVAL  = PSFMAG_G(ent)
   YVAL  = PSFMAG_R(ent)

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'moviefigs/magmag_gVSr'+strtrim(zs(i),2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'magnitude-magnitude plot'
   thickall = 2
endelse
; setting plot range
;XR = [-0.5,1.4]
;YR = [23,17]
;XR = [median(XVAL)-0.5,median(XVAL)+0.5]
;YR = [median(YVAL)+1.0,median(YVAL)-1.0]
XR = [max(XVAL)+0.1,min(XVAL)-0.1]
YR = [max(YVAL)+0.1,min(YVAL)-0.1]



;=== PLOTTING LIGHT CURVE(S) ===
plot,s.PSFMAG_G,s.PSFMAG_G, col=col.black    $
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
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white


;for i=0,n_elements(IDs)-1 do begin  ; loop over ids    uncomment for movie

   if SS ne 1 then oplot,PSFMAG_G(ent),PSFMAG_R(ent),col=col.black,psym=2;,symsize=4*Mbh(i)/Max(Mbh)


   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(i) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_r(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            r_sort  = PSFmag_r(Sent)
            r_sort  = r_sort(entsort)
            g_sort  = PSFMAG_G(Sent)
            g_sort  = g_sort(entsort)

            PLOTSYM,0,2.3,/FILL
            oplot,g_sort(1:n_elements(Sent)-1),r_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,PSFMAG_G(Sent)*0.+g_sort(0),r_sort(Sent)*0.+r_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag

               oploterror,g_sort,r_sort,PSFerr_g,PSFerr_r,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
            ; ====================================
            for tt=0,n_elements(Sent)-1 do begin ; printing 'observation number' on point
               CCC2 = GETCOLOR('black', 100)
               if kk eq n_elements(seasons)-2 then CCC2 = GETCOLOR('white', 100)
               if tt+1 le 9 then XYOUTS,g_sort(tt)-0.006,r_sort(tt)-0.007,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall
               if tt+1 gt 9 then XYOUTS,g_sort(tt)-0.013,r_sort(tt)-0.007,strtrim(tt+1,2),col=CCC2,charsize=1.,charthick=thickall
            endfor
         endif

     
         if n_elements(Sent) eq 1 AND Sent ne [-1] then begin
            PLOTSYM,3,2.3,/FILL
            oplot,findgen(4)*0.0+PSFMAG_G(Sent),findgen(4)*0.0+PSFMAG_R(Sent),col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               oploterror,PSFMAG_G(Sent),PSFMAG_R(Sent),PSFerr_g,PSFerr_r,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.95+YR[0],"Sep-Dec '99",col=col.magenta,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.90+YR[0],"Sep-Dec '00",col=col.pink,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.85+YR[0],"Sep-Dec '01",col=col.red,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.80+YR[0],"Sep-Dec '02",col=col.orange,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.75+YR[0],"Sep-Dec '03",col=col.yellow,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.70+YR[0],"Sep-Dec '04",col=col.green,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.65+YR[0],"Sep-Dec '05",col=col.skyblue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.60+YR[0],"Sep-Dec '06",col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.55+YR[0],"Sep-Dec '07",col=col.navy,charsize=1.5,charthick=thickall

      XYOUTS,(XR[1]-XR[0])*0.4+XR[0],(YR[1]-YR[0])*0.10+YR[0],'spec z: '+STRTRIM(zs(I),2) ,col=col.black,charsize=1.5,charthick=thickall
      XYOUTS,(XR[1]-XR[0])*0.4+XR[0],(YR[1]-YR[0])*0.05+YR[0],'DR7 ID: '+STRTRIM(IDs(I),2),col=col.black,charsize=1.5,charthick=thickall
   endif

   if WT eq 1 then begin
      wait,1.0
      oplot,PSFMAG_G(ent),PSFMAG_R(ent),psym=2,col=col.white ; waiting and then 'erasing' data
   endif
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames

close,22
Nw = Nw+1                                            ; incremeting window number by 1

;=============================================================================================

if vb eq 1 then print,' '
if vb eq 1 then print,':: plotCOLMAG.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END


FUNCTION LINE, X, P
  RETURN, P[0] + P[1]*X
END
