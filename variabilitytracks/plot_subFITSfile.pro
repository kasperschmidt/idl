;+
;----------------------------
;   NAME
;----------------------------
; plot_subFITSfile.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Plotting various plots of the 'sub' fits files extracted with extractFitsSubarray.pro
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
;
; ====== reading Sesar RRL sample ======
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/RRL_sesar_etal2009/sesar09RRL_sorted.fits',/VERBOSE,/SEASONS,/NOTQSO,/ONEBYONE;,/ERRBAR,/SEASONS 
; ====== reading F/G stars ======
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_outliers9999.fit',/VERBOSE,/SEASONS,/NOTQSO,/ONEBYONE,/NODR;,/ERRBAR

;
; ====== 1.4< z <1.6   0.15< A <2.0   0.0< gamma <5.0  - outliers marked ====== 
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Sat_Aug_7_16:09:47_2010.fits',/VERBOSE,/SEASONS
;
; ====== 0.5< z <3.0   0.15< A <2.0   0.0< gamma <5.0  - outliers marked ====== 
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p500000z3p00000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_14:43:12_2010.fits',/VERBOSE,/SEASONS,/ERRBAR
;
; ====== top 19 from 1.4< z <1.6   0.15< A <2.0   0.0< gamma <5.0 ======
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_11_09:58:09_2010.fits',/VERBOSE,/SEASONS
;
;
;
;

; ====== 1.4 -- 1.6 	& 0.00 -- 2.00	& 0.00 -- 5.00
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Fri_Aug_20_07:56:48_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 1.6 -- 1.8 	& 0.15 -- 2.00 	& 0.00 -- 5.00
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p60000z1p80000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_14:47:04_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 1.8 -- 2.0 	& 0.15 -- 2.00	& 0.00 -- 5.00
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p80000z2p00000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:16_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 2.0 -- 2.3 	& 0.15 -- 2.00	& 0.00 -- 5.00
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_2p00000z2p30000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:35_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/EPS

; ====== 0.2< z <4.0   0.15< A <2.0   0.0< gamma <5.0  - outliers marked ====== 
; IDL> plot_subFITSfile,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010.fits',/VERBOSE,/SEASONS,/ERRBAR,/eps

;
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-07-09  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ properdist.pro
@ mag2flux.pro
@ namedate.pro
;----------------------------
;-
PRO plot_subFITSfile,subfits,ONEBYONE=ONEBYONE,SEASONS=SEASONS,NOTQSO=NOTQSO,NODR=NODR,ERRBAR=ERRBAR,EPS=EPS,VERBOSE=VERBOSE

WT = n_elements(ONEBYONE)
SS = n_elements(SEASONS)
NQ = n_elements(NOTQSO)
ND = n_elements(NODR)
EB = n_elements(ERRBAR)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

namedate,subfits,path,name,extension,date,dateus
s = mrdfits(subfits,1)                                ; reading input data
uniqent = uniq(s.headobjid)                           ; entries of objects
IDs     = s(uniqent).headobjid                        ; getting uniq objids
zs      = s(uniqent).z                                ; the corresponding redshifts
if NQ ne 1 then Mbh     = s(uniqent).logbh_mgII_md04                  ; getting black hole masses

readcol,'S82objid_z_Agamma.dat',idall,zall,Aall,gammaall    ; reading redshift, A and gamma for all S82 QSOs for sample comparison plots

if ND eq 1 then begin
   DEREDPSFMAG_U = s.PSFMAG_U
   DEREDPSFMAG_G = s.PSFMAG_G
   DEREDPSFMAG_R = s.PSFMAG_R
   DEREDPSFMAG_I = s.PSFMAG_I
   DEREDPSFMAG_Z = s.PSFMAG_Z
endif else begin
   DEREDPSFMAG_U = s.DEREDPSFMAG_U
   DEREDPSFMAG_G = s.DEREDPSFMAG_G
   DEREDPSFMAG_R = s.DEREDPSFMAG_R
   DEREDPSFMAG_I = s.DEREDPSFMAG_I
   DEREDPSFMAG_Z = s.DEREDPSFMAG_Z
endelse

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

ug_dered   = DEREDPSFMAG_U-DEREDPSFMAG_G
gr_dered   = DEREDPSFMAG_G-DEREDPSFMAG_R
ri_dered   = DEREDPSFMAG_R-DEREDPSFMAG_I
iz_dered   = DEREDPSFMAG_I-DEREDPSFMAG_Z
gi_dered   = DEREDPSFMAG_G-DEREDPSFMAG_I

; estimating the flux corresponding to measured magnitudes (assumed perfect AB)
lam_u    = 3543 ; Angstrom
lam_g    = 4770 ; Angstrom
lam_r    = 6231 ; Angstrom
lam_i    = 7625 ; Angstrom
lam_z    = 9134 ; Angstrom

lam_uRF  = lam_u/(1+s.z)
lam_gRF  = lam_g/(1+s.z)
lam_rRF  = lam_r/(1+s.z)
lam_iRF  = lam_i/(1+s.z)
lam_zRF  = lam_z/(1+s.z)

if vb eq 1 then print,' '
if vb eq 1 then print,':: plot_subFITSfile.pro.pro :: The redshift range of the data is [',strtrim(min(s.z),2),',',strtrim(max(s.z),2),']'
if vb eq 1 then print,'                               In this range the ugriz restframe wavelengths are in the intervals:'
if vb eq 1 then print,'                               u: [ ',strtrim(min(lam_uRF),2),' , ',strtrim(max(lam_uRF),2),' ]'
if vb eq 1 then print,'                               g: [ ',strtrim(min(lam_gRF),2),' , ',strtrim(max(lam_gRF),2),' ]'
if vb eq 1 then print,'                               r: [ ',strtrim(min(lam_rRF),2),' , ',strtrim(max(lam_rRF),2),' ]'
if vb eq 1 then print,'                               i: [ ',strtrim(min(lam_iRF),2),' , ',strtrim(max(lam_iRF),2),' ]'
if vb eq 1 then print,'                               z: [ ',strtrim(min(lam_zRF),2),' , ',strtrim(max(lam_zRF),2),' ]'
if vb eq 1 then print,' '

Nw = 0 & goto,DMJDplotting ;& print,"damn I'm here"

;calculating absolut Mags from apparent mags (of quasars only)
if NQ ne 1 then begin   ; checking if input is quasars
   properdist,s.z,Dp,Dl,Da;,/VERBOSE
   ABS_u   = s.PSFMAG_U + 5. - 5.*alog10(Dl*10^6.)
   ABS_g   = s.PSFMAG_G + 5. - 5.*alog10(Dl*10^6.)
   ABS_r   = s.PSFMAG_R + 5. - 5.*alog10(Dl*10^6.)
   ABS_i   = s.PSFMAG_I + 5. - 5.*alog10(Dl*10^6.)
   ABS_z   = s.PSFMAG_Z + 5. - 5.*alog10(Dl*10^6.)

   ABS_uDR = DEREDPSFMAG_U + 5. - 5.*alog10(Dl*10^6.)
   ABS_gDR = DEREDPSFMAG_G + 5. - 5.*alog10(Dl*10^6.)
   ABS_rDR = DEREDPSFMAG_R + 5. - 5.*alog10(Dl*10^6.)
   ABS_iDR = DEREDPSFMAG_I + 5. - 5.*alog10(Dl*10^6.)
   ABS_zDR = DEREDPSFMAG_Z + 5. - 5.*alog10(Dl*10^6.)

   ABS_grDR = ABS_gDR - ABS_rDR

   ; calculating the absolut fluxes
   Flux_u   = mag2flux(ABS_U,ABwave=lam_uRF)
   Flux_g   = mag2flux(ABS_G,ABwave=lam_gRF)
   Flux_r   = mag2flux(ABS_R,ABwave=lam_rRF)
   Flux_i   = mag2flux(ABS_I,ABwave=lam_iRF)
   Flux_z   = mag2flux(ABS_Z,ABwave=lam_zRF)

   Flux_uDR = mag2flux(ABS_uDR,ABwave=lam_uRF)
   Flux_gDR = mag2flux(ABS_gDR,ABwave=lam_gRF)
   Flux_rDR = mag2flux(ABS_rDR,ABwave=lam_rRF)
   Flux_iDR = mag2flux(ABS_iDR,ABwave=lam_iRF)
   Flux_zDR = mag2flux(ABS_zDR,ABwave=lam_zRF)

   ;F_Kcorr = F_lambda/(1+z)^2  ; according to the notes http://kbs.wiki-site.com/index.php/Monochromatic_K-correction

   ; calculating continuum slopes via eq. 2 for alpha in Davis et al 2007
   alpha_ugDR = -( 2 + (alog10(Flux_uDR) - alog10(Flux_gDR)) / (alog10(lam_uRF)-alog10(lam_gRF))  ) 
   alpha_grDR = -( 2 + (alog10(Flux_gDR) - alog10(Flux_rDR)) / (alog10(lam_gRF)-alog10(lam_rRF))  ) 
   alpha_riDR = -( 2 + (alog10(Flux_rDR) - alog10(Flux_iDR)) / (alog10(lam_rRF)-alog10(lam_iRF))  ) 
   alpha_izDR = -( 2 + (alog10(Flux_iDR) - alog10(Flux_zDR)) / (alog10(lam_iRF)-alog10(lam_zRF))  ) 

   alpha_urDR = -( 2 + (alog10(Flux_uDR) - alog10(Flux_rDR)) / (alog10(lam_uRF)-alog10(lam_rRF))  ) 
   alpha_giDR = -( 2 + (alog10(Flux_gDR) - alog10(Flux_iDR)) / (alog10(lam_gRF)-alog10(lam_iRF))  ) 
   alpha_izDR = -( 2 + (alog10(Flux_rDR) - alog10(Flux_zDR)) / (alog10(lam_rRF)-alog10(lam_zRF))  ) 

   alpha_uiDR = -( 2 + (alog10(Flux_uDR) - alog10(Flux_iDR)) / (alog10(lam_uRF)-alog10(lam_iRF))  ) 
   alpha_gzDR = -( 2 + (alog10(Flux_gDR) - alog10(Flux_zDR)) / (alog10(lam_gRF)-alog10(lam_zRF))  ) 

   alpha_uzDR = -( 2 + (alog10(Flux_uDR) - alog10(Flux_zDR)) / (alog10(lam_uRF)-alog10(lam_zRF))  ) 

   ;calculating average aplha for each object
   AvAlpha_grDR = fltarr(n_elements(IDs))
   for ll=0,n_elements(IDs)-1 do begin  ; loop over ids
      ent=where(s.headobjid eq IDs(ll))
      AvAlpha_grDR(ll) = mean(alpha_grDR(ent))
   endfor

   ;calculating estimate for log(L/Ledd) = log(L)-log(Ledd) ; L/Ledd \propto 10^(-mag/2.5)/Mbh
   logLbolLedd  = s.LOGLBOL  - s.LOGBH_MGII_MD04
   logL1350Ledd = s.LOGL1350 - s.LOGBH_MGII_MD04
   logL3000Ledd = s.LOGL3000 - s.LOGBH_MGII_MD04
   logL5100Ledd = s.LOGL5100 - s.LOGBH_MGII_MD04

   logLrDRLedd  = -ABS_r/2.5 - s.LOGBH_MGII_MD04
endif

;NW = 0 & goto, MBHplot


NW = 0   ; window number reset
if NQ ne 1 then begin  ; cheking if input is quasars                       - skipping plots #1
;=============================================================================================
; = = = Comparison of A gamma values = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'Agamma_compare.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Comparison of A gamma values to whole sample'
   thickall = 2
endelse
; setting plot range
XR = [0.01,1]
YR = [-0.1,1.2]

;=== PLOTTING LIGHT CURVE(S) ===
plot,Aall,gammaall, col=col.black    $
        , /NODATA $
        , xtitle ='A' $
        , ytitle =textoidl('\gamma') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

PLOTSYM,0,0.5,/fill
oplot,Aall,gammaall, col=col.black,psym=8,thick=thickall
oplot,s(uniq(s.A)).A,s(uniq(s.A)).gamma,psym=4,col=col.red,thick=thickall

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
goto, skip1
;=============================================================================================
; = = = g vs g-i = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'colmag_gVSgi.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Color magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [-0.5,1.4]
YR = [23,17]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gi,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='g-i' $
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


for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   oplot,gi(ent),s(ent).PSFMAG_G, col=col.black,psym=2
   oplot,gi_dered(ent),DEREDPSFMAG_G(ent), col=col.red,psym=2
;   oplot,gi(ent),s(ent).PSFMAG_G-mean(s(ent).PSFMAG_G), col=col.black,psym=2
;   oplot,gi_dered(ent),DEREDPSFMAG_G(ent)-mean(DEREDPSFMAG_G(ent)), col=col.red,psym=2
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
skip1:
goto, skip2
;=============================================================================================
; = = = Mg vs Mg-Mr = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'colmag_ABSgVSgr.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Absolute color magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [-0.5,1.4]
YR = [-22,-28]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='M!Dg!N-M!Dr!N' $
        , ytitle ='M!Dg!N' $
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

for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))

   xx = findgen(300)/300*(XR(1)-XR(0))+XR(0)
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ;XVAL  = gr(ent)
   ;YVAL  = s(ent).PSFMAG_G
   ;YERR  = s(ent).PSFMAGERR_G
   ;START = [1,20]                    ; initial guess for fit
   ;result = MPFITFUN('LINE',XVAL,YVAL,YERR, start,/QUIET)
   ;oplot,xx,result(0)+xx*result(1),col=col.black,linestyle=0

   ;oplot,gr(ent),s(ent).PSFMAG_G, col=col.black,psym=2
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 
   XVAL  = ABS_grDR(ent)
   YVAL  = ABS_gDR(ent)
   YERR  = s(ent).PSFMAGERR_G
   START = [1,20]                    ; initial guess for fit
   result = MPFITFUN('LINE',XVAL,YVAL,YERR, start,/QUIET)
   oplot,xx,result(0)+xx*result(1),col=col.black,linestyle=0

   oplot,ABS_grDR(ent),ABS_gDR(ent), col=col.black,psym=2
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
endif                                                                ; end - skipping plots #1
;=============================================================================================
skip2:

;=============================================================================================
; = = = Color mag variation = = =
slopes1 = fltarr(n_elements(IDs))
slopes2 = fltarr(n_elements(IDs))
if EB eq 1 then begin
   seasonslopes = '/'+path+'/'+name+'_seasonslopes.dat'
   openw,22,seasonslopes,width=400
   printf,22,'# columns are:'
   printf,22,'# ID   beta_l_gVSgr    z    season    epochs    beta_s_gVSgr    Ktau    beta_l_grVSg    beta_s_grVSg'
   if vb eq 1 then print,':: plot_subFITSfile.pro :: The season slopes will be written to: ',strtrim(seasonslopes,2)
endif
!p.multi = [0,0,0]

for i=0,n_elements(IDs)-1 do begin  ; loop over ids for movie frames
   ent=where(s.headobjid eq IDs(i) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers
   XVAL  = gr_dered(ent)
   YVAL  = DEREDPSFMAG_G(ent)

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
;   plot1 = 'colmag_gVSgr_Mbh.eps'
;   plot1 = 'moviefigs/colmag_gVSgr_Mbh'+strtrim(i,2)+'.eps' ; name for movie frames
   plot1 = 'moviefigs/colmag_gVSgr_Mbh'+strtrim(zs(i),2)+'.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Color magnitude plot with Mbh color'
;   window, 0, xsize=600, ysize=500, title = 'Color magnitude plot with Mbh color'
   thickall = 2
endelse
; setting plot range
;XR = [-0.5,1.4]
;YR = [23,17]
XR = [median(XVAL)-0.5,median(XVAL)+0.5]
YR = [median(YVAL)+1.0,median(YVAL)-1.0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='(g-r)!Ddered!N' $
        , ytitle ='g!Ddered!N' $
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
   errfrac = 0.15 ; the fraction of the exctinction estimated to be its error acc. to Schlegel et al. (1998)
   ; caculating estimated errors on de-reddened magnitudes like in Richards et al 2002
   ext_g   = s(ent).PSFMAG_G-DEREDPSFMAG_G(ent)
   gerrpsf = s(ent).PSFMAGERR_G

   ;YERR    = sqrt( gerrpsf*gerrpsf + (errfrac*ext_g)*(errfrac*ext_g) )   ; misleading - not 'real' error on mag
   YERR    = gerrpsf
   START   = [1,20]                    ; initial guess for fit
   result  = MPFITFUN('LINE',XVAL,YVAL,YERR, start,/QUIET)
   slopes1(i) = result(1)

   XVAL2   = DEREDPSFMAG_G(ent)
   YVAL2   = gr_dered(ent)
   START2  = [-12,0.5]                    ; initial guess for fit
   result2  = MPFITFUN('LINE',XVAL2,YVAL2,YERR2, start2,/QUIET)
   slopes2(i) = result2(1)


   if NQ ne 1 then begin  ; cheking if input is quasars
      colset = (Max(Mbh)-Mbh(i))/( Max(Mbh)-Min(Mbh) ) * 255    ; setting the color according to Mbh
;      oplot,xx,result(0)+xx*result(1),COLOR=COLOR24([colset,0,0]),linestyle=0,thick=thickall
      oplot,xx,result(0)+xx*result(1),col=col.black,linestyle=0,thick=thickall+2*(Max(Mbh)-Mbh(i))/( Max(Mbh)-Min(Mbh) )
   endif
   if NQ eq 1 then oplot,xx,result(0)+xx*result(1),col=col.black,linestyle=0,thick=thickall
   if SS ne 1 then oplot,gr_dered(ent),DEREDPSFMAG_G(ent),col=col.black,psym=2;,symsize=4*Mbh(i)/Max(Mbh)


   if SS eq 1 then begin
      for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
         CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch
         Sent = where(s.headobjid eq IDs(i) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_G ne -9999 AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

         if Sent ne [-1] and n_elements(Sent) gt 1 then begin    ; checking that there are obs in season
            ; sorting for indicating 'time-sequence' of intra-season points
            entsort = sort(mjd_r(sent))
            mjdsort = mjd_r(Sent)
            mjdsort = mjdsort(entsort)
            gr_sort = gr_dered(Sent)
            gr_sort = gr_sort(entsort)
            g_sort  = DEREDPSFMAG_G(Sent)
            g_sort  = g_sort(entsort)

            PLOTSYM,0,2.3,/FILL
            ; oplot,gr_dered(Sent),DEREDPSFMAG_G(Sent),col=CCC,psym=8,thick=thickall 
            oplot,gr_sort(1:n_elements(Sent)-1),g_sort(1:n_elements(Sent)-1),col=CCC,psym=8,thick=thickall 
            ; === plotting first point as star ===
            PLOTSYM,3,2.3,/FILL
            oplot,gr_dered(Sent)*0.+gr_sort(0),DEREDPSFMAG_G(Sent)*0.+g_sort(0),col=CCC,psym=8,thick=thickall 
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               ext_g     = s(Sent).PSFMAG_G - DEREDPSFMAG_G(Sent)                       ; extinction
               DRerr_g   = sqrt( PSFerr_g*PSFerr_g + (errfrac*ext_g)*(errfrac*ext_g) )  ; error on extinction corrected mag
               DRerr_g   = DRerr_g(entsort)                                             ; sorting the season errors

               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               ext_r     = s(Sent).PSFMAG_G - DEREDPSFMAG_R(Sent)                       ; extinction
               DRerr_r   = sqrt( PSFerr_r*PSFerr_r + (errfrac*ext_r)*(errfrac*ext_r) )  ; error on extinction corrected mag
               DRerr_r   = DRerr_r(entsort)                                             ; sorting the season errors

               DRerr_gr  = sqrt(DRerr_r^2. + DRerr_g^2)                                 ; the g-r color propagated error
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                                 ; the g-r color propagated error
               ;oploterror,gr_sort,g_sort,DRerr_g,DRerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
               oploterror,gr_sort,g_sort,PSFerr_g,PSFerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall

               XVAL_s    = gr_sort 
               YVAL_s    = g_sort
               ;YERR_s    = DRerr_g
               YERR_s    = PSFerr_g
               START_s   = [1,20]                    ; initial guess for fit
               result_s  = MPFITFUN('LINE',XVAL_s,YVAL_s,YERR_s,start_s,/QUIET)
               ; estimating slopes with g as x variable instead of g-r
               XVAL_s2   = g_sort
               YVAL_s2   = gr_sort 
               YERR_s2   = PSFerr_gr
               START_s2  = [-12,0.5]                    ; initial guess for fit
               result_s2 = MPFITFUN('LINE',XVAL_s2,YVAL_s2,YERR_s2,start_s2,/QUIET)

               ; estimating Kendall's tau for the season
               kendalltau,gr_sort,g_sort,Ktau

               ; storing slopes and Ktau in file
               printf,22,FORMAT='(A25,f15.5,f15.5,i5,i5,f15.5,f15.5,f15.5,f15.5)',IDs(i),slopes1(i),zs(i),kk,n_elements(Sent),result_s(1),Ktau,slopes2(i),result_s2(1)
               oplot,xx,result_s(0)+xx*result_s(1),col=CCC,linestyle=0,thick=thickall
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
            oplot,findgen(4)*0.0+gr_dered(Sent),findgen(4)*0.0+DEREDPSFMAG_G(Sent),col=CCC,psym=8,thick=thickall,CLIP=[XR[0],YR[0],XR[1],YR[1]]
            ; === overplotting errors ===
            if EB eq 1 and Sent ne [-1] then begin  ; getting/calculating errorbars on dereddened observations
               PSFerr_g  = s(Sent).PSFMAGERR_G                                          ; error on PSF mag
               ext_g     = s(Sent).PSFMAG_G - DEREDPSFMAG_G(Sent)                       ; extinction
               DRerr_g   = sqrt( PSFerr_g*PSFerr_g + (errfrac*ext_g)*(errfrac*ext_g) )  ; error on extinction corrected mag

               PSFerr_r  = s(Sent).PSFMAGERR_R                                          ; error on PSF mag
               ext_r     = s(Sent).PSFMAG_G - DEREDPSFMAG_R(Sent)                       ; extinction
               DRerr_r   = sqrt( PSFerr_r*PSFerr_r + (errfrac*ext_r)*(errfrac*ext_r) )  ; error on extinction corrected mag

               DRerr_gr  = sqrt(DRerr_r^2. + DRerr_g^2)                                 ; the g-r color propagated error
               PSFerr_gr = sqrt(PSFerr_r^2. + PSFerr_g^2)                                 ; the g-r color propagated error
               ;oploterror,gr_dered(Sent),DEREDPSFMAG_G(Sent),DRerr_g,DRerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
               oploterror,gr_dered(Sent),DEREDPSFMAG_G(Sent),PSFerr_g,PSFerr_gr,psym=3, col=CCC, ERRCOLOR=CCC,ERRTHICK=thickall
            endif
         endif
      endfor

      XYOUTS,XR[1]-0.27,YR[1]+0.1,"June'99-June'00",col=col.magenta,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.2,"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.3,"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.4,"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.5,"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.6,"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.7,"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.8,"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
      XYOUTS,XR[1]-0.27,YR[1]+0.9,"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

      XYOUTS,XR[0]+0.05,YR[0]-0.2,'spec z: '+STRTRIM(zs(I),2) ,col=col.black,charsize=1.5,charthick=thickall
      XYOUTS,XR[0]+0.05,YR[0]-0.1,'DR7 ID: '+STRTRIM(IDs(I),2),col=col.black,charsize=1.5,charthick=thickall
   endif

   if WT eq 1 and NQ ne 1 then begin
      wait,1.0
      oplot,gr_dered(ent),DEREDPSFMAG_G(ent),psym=2,col=col.white ; waiting and then 'erasing' data
      oplot,xx,result(0)+xx*result(1),col=col.white,linestyle=0,thick=thickall+2*(Max(Mbh)-Mbh(i))/( Max(Mbh)-Min(Mbh) )
   endif
   ;- - - - - - - - - - - - - - - - - - - - - - - - - - - 
;endfor      ; uncomment for movie

;slope of ensemble
XVAL  = gr_dered
YVAL  = DEREDPSFMAG_G
errfrac = 0.15 ; the fraction of the exctinction estimated to be its error acc. to Schlegel et al. (1998)
; caculating estimated errors on de-reddened magnitudes like in Richards et al 2002
ext_g   = s.PSFMAG_G-DEREDPSFMAG_G
gerrpsf = s.PSFMAGERR_G

;YERR    = sqrt( gerrpsf*gerrpsf + (errfrac*ext_g)*(errfrac*ext_g) )
YERR    = gerrpsf
START   = [1,20]                    ; initial guess for fit
resultENS  = MPFITFUN('LINE',XVAL,YVAL,YERR, start,/QUIET,PERROR=errorENS)

oplot,xx,resultENS(0)+xx*resultENS(1),col=col.blue,linestyle=2,thick=thickall
;errors
;oplot,xx,resultENS(0)-3*errorENS(0)+xx*resultENS(1),col=col.blue,linestyle=2,thick=thickall
;oplot,xx,resultENS(0)-3*errorENS(0)+xx*resultENS(1),col=col.blue,linestyle=2,thick=thickall

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

endfor ; endfor for movie frames

close,22
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
; = = = Histogram of col-mag slopes = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'Hist_colmag_gVSgr_slopes.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Histogram slopes Color magnitude'
   thickall = 2
endelse
; setting plot range
XR = [min(slopes1)-1,max(slopes1)+1]
YR = [0,n_elements(slopes1)/5]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='g!Ddered!N vs (g-r)!Ddered!N linear fit slopes' $
;        , ytitle =' ' $
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

plothist,slopes1,xh1,yh1,bin=(max(slopes1)-min(slopes1))/20.,/overplot,col=col.black,thick=thickall

oplot,[resultENS(1),resultENS(1)],YR,col=col.blue,thick=thickall,linestyle=0
XYOUTS,resultENS(1)+0.2,YR(1)/2,'Ensemble slope',col=col.blue,charsize=1.5
;errors
;oplot,[resultENS(1)-3*errorENS(1),resultENS(1)-3*errorENS(1)],YR,col=col.blue,thick=thickall,linestyle=2
;oplot,[resultENS(1)+3*errorENS(1),resultENS(1)+3*errorENS(1)],YR,col=col.blue,thick=thickall,linestyle=2

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

if NQ ne 1 then begin  ; cheking if input is quasars                       - skipping plots #2
goto, skip3
;=============================================================================================
; = = = mean r-band Absolute magnitude vs z of sample = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'MabsVSz_rband.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Mabs vs z'
   thickall = 2
endelse
; setting plot range
XR = [min(s.z)-0.5,max(s.z)+0.5]
YR = [max(ABS_r)+0.5,min(ABS_r)-0.5]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle ='<M!Dr!N> ' $
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

for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   oplot,s(ent).z*0.0+mean(s(ent).z),ABS_r*0.0+mean(ABS_r(ent)),psym=2,col=col.black
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
skip3:

;=============================================================================================
; = = = L/Ledd approximation VS color = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'LLeddVSgr.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'L/Leddington VS color'
   thickall = 2
endelse
; setting plot range
XR = [-0.5,1.4]
YR = [min(logLbolLedd)-1,max(logLbolLedd)+1]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='<(g-r)!Ddered!N>' $
        , ytitle =textoidl('log(L/M_{BHvir MGII}) \sim log(L/L_{edd})') $
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

for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   oplot,gr_dered(ent)*0.0+mean(gr_dered(ent)),logLbolLedd(ent)*0.0+mean(logLbolLedd(ent)),psym=2,col=col.black  
   oplot,gr_dered(ent)*0.0+mean(gr_dered(ent)),logL1350Ledd(ent)*0.0+mean(logL1350Ledd(ent)),psym=2,col=col.blue  
   oplot,gr_dered(ent)*0.0+mean(gr_dered(ent)),logL3000Ledd(ent)*0.0+mean(logL3000Ledd(ent)),psym=2,col=col.red  
   oplot,gr_dered(ent)*0.0+mean(gr_dered(ent)),logL5100Ledd(ent)*0.0+mean(logL5100Ledd(ent)),psym=2,col=col.green  
endfor

xyouts,0.5,38.0,'L!Dbol  Shen et al 2010!N',col=col.black,charsize=2
xyouts,0.5,37.5,'L!D1350 Shen et al 2010!N',col=col.blue,charsize=2
xyouts,0.5,37.,'L!D3000 Shen et al 2010!N',col=col.red,charsize=2
;xyouts,0.5,5.4,'L!D5100 Shen et al 2010!N',col=col.green,charsize=2

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = LLedd vs gr color = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'LLedd_rbandVSgr.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Log(L/Ledd) VS color'
;   window, 0, xsize=600, ysize=500, title = 'Log(L/Ledd) VS color'
   thickall = 2
endelse
; setting plot range
XR = [-0.5,1.4]
YR = [min(logLrDRLedd),max(logLrDRLedd)]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='(g-r)!Ddered!N' $
        , ytitle =textoidl('log(-M_r/2.5)-log(M_{BHvir MGII}) \sim log(L/L_{edd})') $
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

for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   oplot,gr_dered(ent),logLrDRLedd(ent),psym=2,col=col.black

   if WT eq 1 then begin
      wait,1.0
      oplot,gr_dered(ent),logLrDRLedd(ent),psym=2,col=col.white ; waiting and then 'erasing' data
   endif
 
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

goto, skip4
;=============================================================================================
; = = = alpha VS color = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'alphagrVSgr.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'alpha VS color'
   thickall = 2
endelse
; setting plot range
XR = [-0.5,1.4]
YR = [-3.0,2.0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='(g-r)!Ddered!N' $
        , ytitle =textoidl('\alpha_{g-r}') $
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

for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   oplot,gr_dered(ent),alpha_grDR(ent),psym=2,col=col.black

   if WT eq 1 then begin
      wait,1.0
      oplot,gr_dered(ent),alpha_grDR(ent),psym=2,col=col.white ; waiting and then 'erasing' data
   endif
 
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
skip4:

;=============================================================================================
; = = = Histogram of alpha values = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'Hist_alpha.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=30, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=800, ysize=500, title = 'Histogram of alpha values'
   thickall = 2
endelse
; setting plot range
XR = [-2.5,1.5]
;XR = [-1.5,0.5]  ; range from Davis et al 2007 fig 2
YR = [0,n_elements(alpha_grDR)/5]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\alpha') $
;        , ytitle =' ' $
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

plothist,alpha_grDR,xh1,yh1,bin=(max(alpha_grDR)-min(alpha_grDR))/500.,/overplot,col=col.black,thick=thickall
oplot,[-1.5,-1.5],[0,max(YR)],linestyle=2,col=col.blue,thick=thickall
oplot,[0.5,0.5],[0,max(YR)],linestyle=2,col=col.blue,thick=thickall
Xyouts,-1.3,max(YR)/3.,'Range in Davis et al 2007 fig. 2',charsize=1.5,charthick=thickall,col=col.blue

plothist,AvAlpha_grDR,xh2peak,yh2peak,bin=(max(alpha_grDR)-min(alpha_grDR))/500.,/overplot,col=col.black,thick=thickall,linestyle=2,peak=max(yh1)/2
plothist,AvAlpha_grDR,xh2,yh2,bin=(max(alpha_grDR)-min(alpha_grDR))/500.,/noplot,col=col.black,thick=thickall,linestyle=2
if vb eq 1 then print,':: plot_subFITSfile.pro.pro :: Scale factor for mean alphas (dashed histogram) in histogram plot :',max(yh2peak)/max(yh2)


if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
MBHplot:
; = = = Mbh VS color = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'Mbh_VSgr.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'M_BH VS color'
   thickall = 2
endelse
; setting plot range
XR = [min(Mbh(where(Mbh ne 0)))-0.2,max(Mbh)+0.2]
YR = [-0.5,1.4]


;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(M_{BHvir MGII})') $
        , ytitle ='<g-r>' $
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

meangr = fltarr(n_elements(IDs))

for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   meangr(i) = mean(gr_dered(ent))

;   oplot,Mbh(i)+fltarr(n_elements(ent)),gr_dered(ent),psym=3,col=col.black

   PLOTSYM,0,1.0,/fill
   oplot,gr_dered(ent)*0.0+mean(Mbh(i)),gr_dered(ent)*0.0+meangr(i),psym=8,col=col.red,thick=thickall

   ;oplot,ug_dered(ent)*0.0+mean(Mbh(i)),ug_dered(ent)*0.0+mean(ug_dered(ent)),psym=2,col=col.blue
   ;oplot,ri_dered(ent)*0.0+mean(Mbh(i)),ri_dered(ent)*0.0+mean(ri_dered(ent)),psym=2,col=col.yellow   
   ;oplot,iz_dered(ent)*0.0+mean(Mbh(i)),iz_dered(ent)*0.0+mean(iz_dered(ent)),psym=2,col=col.red
endfor


; divide Mbh into bins and overplot mean
bins = findgen(35)/10.+7.
for jj=0,n_elements(bins)-2 do begin
   binent = where(Mbh gt bins(jj) and Mbh lt bins(jj+1))
   if binent ne [-1] then begin
      meanM = mean(Mbh(binent))
      PLOTSYM,8,2.0,/fill
      oplot,fltarr(2)+meanM,fltarr(2)+mean(meangr(binent)),psym=8,col=col.black

      ; standard deviation around the means
      StndM = total( 1./n_elements(Mbh(binent)) * sqrt((Mbh(binent)-meanM)^2) )
      Stndgr = total( 1./n_elements(meangr(binent)) * sqrt((meangr(binent)-mean(meangr(binent)))^2) )
      oploterror,fltarr(2)+meanM,fltarr(2)+mean(meangr(binent)),StndM,Stndgr,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall
   endif
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif

Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
; = = = mean Color mag variation = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'colmag_gVSgr_mean.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Mean color magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [-0.5,1.4]
YR = [23,17]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='<(g-r)!Ddered!N>' $
        , ytitle ='<g!Ddered!N>' $
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

meanG    = fltarr(n_elements(IDs))
meanGR   = fltarr(n_elements(IDs))
meanGerr = fltarr(n_elements(IDs))
for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))

   oplot,gr_dered(ent)*0.0+mean(gr_dered(ent)),DEREDPSFMAG_G(ent)*0.0+mean(DEREDPSFMAG_G(ent)),col=col.black,psym=2
   meanG(i)    = mean(DEREDPSFMAG_G(ent))
   meanGR(i)   = mean(gr_dered(ent))
   meanGerr(i) = mean(s(ent).PSFMAGERR_G)
endfor

;slope of mean ensemble
XVAL      = meanGR
YVAL      = meanG
YERR      = meanGerr
START     = [1,20]                    ; initial guess for fit
resultEM  = MPFITFUN('LINE',XVAL,YVAL,YERR, start,/QUIET,PERROR=errorENS)

oplot,xx,resultEM(0)+xx*resultEM(1),col=col.blue,linestyle=0,thick=thickall
if vb eq 1 then print,':: plot_subFITSfile.pro.pro :: The result from fitting to the ensemble full data: ',resultENS
if vb eq 1 then print,'                               The result from fitting to the ensemble mean data: ',resultEM

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = LLedd vs alpha = = =
!p.multi = [0,0,0]

if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'alphaVSLLedd.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Log(L/Ledd) VS alpha'
   thickall = 2
endelse
; setting plot range
XR = [35.5,38.5]
YR = [-2.0,1.0]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L/M_{BHvir MGII}) \sim log(L/L_{edd})') $
        , ytitle =textoidl('<\alpha>') $
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


for i=0,n_elements(IDs)-1 do begin  ; loop over ids
   ent=where(s.headobjid eq IDs(i))
   PLOTSYM,0,2.0,/fill
   oplot,fltarr(2)*0.0+logLbolLedd(ent(0)),fltarr(2)*0.0+mean(alpha_grDR(ent)),psym=8,col=col.black,thick=thickall
   oplot,fltarr(2)*0.0+logL1350Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_grDR(ent)),psym=8,col=col.blue,thick=thickall
   oplot,fltarr(2)*0.0+logL3000Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_grDR(ent)),psym=8,col=col.red,thick=thickall
   oplot,fltarr(2)*0.0+logL5100Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_grDR(ent)),psym=8,col=col.green,thick=thickall

   PLOTSYM,3,1.0,/fill
   oplot,fltarr(2)*0.0+logLbolLedd(ent(0)),fltarr(2)*0.0+mean(alpha_riDR(ent)),psym=8,col=col.black,thick=thickall
   oplot,fltarr(2)*0.0+logL1350Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_riDR(ent)),psym=8,col=col.blue,thick=thickall
   oplot,fltarr(2)*0.0+logL3000Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_riDR(ent)),psym=8,col=col.red,thick=thickall
   oplot,fltarr(2)*0.0+logL5100Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_riDR(ent)),psym=8,col=col.green,thick=thickall

   PLOTSYM,4,1.0,/fill
   oplot,fltarr(2)*0.0+logLbolLedd(ent(0)),fltarr(2)*0.0+mean(alpha_giDR(ent)),psym=8,col=col.black,thick=thickall
   oplot,fltarr(2)*0.0+logL1350Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_giDR(ent)),psym=8,col=col.blue,thick=thickall
   oplot,fltarr(2)*0.0+logL3000Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_giDR(ent)),psym=8,col=col.red,thick=thickall
   oplot,fltarr(2)*0.0+logL5100Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_giDR(ent)),psym=8,col=col.green,thick=thickall

   PLOTSYM,5,1.0,/fill
   oplot,fltarr(2)*0.0+logLbolLedd(ent(0)),fltarr(2)*0.0+mean(alpha_izDR(ent)),psym=8,col=col.black,thick=thickall
   oplot,fltarr(2)*0.0+logL1350Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_izDR(ent)),psym=8,col=col.blue,thick=thickall
   oplot,fltarr(2)*0.0+logL3000Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_izDR(ent)),psym=8,col=col.red,thick=thickall
   oplot,fltarr(2)*0.0+logL5100Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_izDR(ent)),psym=8,col=col.green,thick=thickall

   PLOTSYM,8,1.0,/fill
   oplot,fltarr(2)*0.0+logLbolLedd(ent(0)),fltarr(2)*0.0+mean(alpha_uzDR(ent)),psym=8,col=col.black,thick=thickall
   oplot,fltarr(2)*0.0+logL1350Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_uzDR(ent)),psym=8,col=col.blue,thick=thickall
   oplot,fltarr(2)*0.0+logL3000Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_uzDR(ent)),psym=8,col=col.red,thick=thickall
   oplot,fltarr(2)*0.0+logL5100Ledd(ent(0)),fltarr(2)*0.0+mean(alpha_uzDR(ent)),psym=8,col=col.green,thick=thickall

; available alphas...
;   alpha_ugDR    alpha_grDR    alpha_riDR    alpha_izDR 
;   alpha_urDR    alpha_giDR    alpha_izDR 
;   alpha_uiDR    alpha_gzDR 
;   alpha_uzDR 

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],"Lbol",col=col.black,charsize=1.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],"L1350",col=col.blue,charsize=1.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],"L3000",col=col.red,charsize=1.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],"L5100",col=col.green,charsize=1.5,charthick=thickall

   PLOTSYM,0,2.0,/fill
   OPLOT,fltarr(2)+DX*0.25+XR[0],fltarr(2)+DY*0.96+YR[0],psym=8,col=col.black,thick=thickall
   XYOUTS,DX*0.27+XR[0],DY*0.95+YR[0],textoidl('\alpha_{gr}'),col=col.black,charsize=1.5,charthick=thickall

   PLOTSYM,3,2.0,/fill
   OPLOT,fltarr(2)+DX*0.25+XR[0],fltarr(2)+DY*0.91+YR[0],psym=8,col=col.black,thick=thickall
   XYOUTS,DX*0.27+XR[0],DY*0.90+YR[0],textoidl('\alpha_{ri}'),col=col.black,charsize=1.5,charthick=thickall

   PLOTSYM,4,2.0,/fill
   OPLOT,fltarr(2)+DX*0.25+XR[0],fltarr(2)+DY*0.86+YR[0],psym=8,col=col.black,thick=thickall
   XYOUTS,DX*0.27+XR[0],DY*0.85+YR[0],textoidl('\alpha_{gi}'),col=col.black,charsize=1.5,charthick=thickall

   PLOTSYM,5,2.0,/fill
   OPLOT,fltarr(2)+DX*0.25+XR[0],fltarr(2)+DY*0.81+YR[0],psym=8,col=col.black,thick=thickall
   XYOUTS,DX*0.27+XR[0],DY*0.80+YR[0],textoidl('\alpha_{iz}'),col=col.black,charsize=1.5,charthick=thickall

   PLOTSYM,8,2.0,/fill
   OPLOT,fltarr(2)+DX*0.25+XR[0],fltarr(2)+DY*0.76+YR[0],psym=8,col=col.black,thick=thickall
   XYOUTS,DX*0.27+XR[0],DY*0.75+YR[0],textoidl('\alpha_{uz}'),col=col.black,charsize=1.5,charthick=thickall

   ;XYOUTS,DX*0.05+XR[0],DY*0.2+YR[0],'spec z: '+STRTRIM(zs(i),2) ,col=col.black,charsize=1.5,charthick=thickall
   ;XYOUTS,DX*0.05+XR[0],DY*0.1+YR[0],'DR7 ID: '+STRTRIM(IDs(i),2),col=col.black,charsize=1.5,charthick=thickall

endfor
stop
if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif



Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif                                                                ; end - skipping plots #2

DMJDplotting:
print,"I'm here"
;=============================================================================================
; = = = MJD hist = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'DMJDhist.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Mean color magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [-5,60]
;YR = [0,n_elements(mjd_r)/10.]
YR = [0,1.25*10^4.]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\Delta MJD [days]') $
        , ytitle ='N' $
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

for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
   Dmjd1 = -999
   CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch

   for ii = 0L,n_elements(IDs)-1 do begin
      MJDent = where(s.headobjid eq IDs(ii) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

      if n_elements(MJDent) gt 1 then begin    ; checking that there are obs in season
         MJDin = MJD_r(MJDent)                 ; the MJDs
         sMJD  = MJDin(sort(MJDin))            ; sorting the MJDs
         for jj = 0,n_elements(sMJD)-2 do begin
            Dmjd_jj = sMJD(jj+1)-sMJD(jj)      ; calculating difference between MJDs
            if Dmjd1 eq [-999] then begin
               Dmjd1 = Dmjd_jj           ; starting vector
            endif else begin
               Dmjd1 = [Dmjd1,Dmjd_jj]   ; appending value
            endelse
         endfor
      endif
   endfor

   if n_elements(Dmjd1) gt 1 then plothist,Dmjd1,xh,yh,BIN=1.,/overplot,col=CCC,thick=thickall

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

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = MJD hist = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'DMJDhistALL.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Mean color magnitude plot'
   thickall = 2
endelse
; setting plot range
XR = [-10,180]
YR = [0,n_elements(IDs)/2.]

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gr,s.PSFMAG_G, col=col.black    $
        , /NODATA $
        , xtitle ='Season length [days]' $
        , ytitle ='N' $
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

for kk = 0,n_elements(seasons)-2 do begin            ; looping over seasons
   DMJDsh = -999
   CCC = GETCOLOR(Scol(kk), 100)  ; setting color of epoch

   for ii = 0,n_elements(IDs)-1 do begin
      MJDent = where(s.headobjid eq IDs(ii) AND mjd_r gt seasons(kk) AND mjd_r lt seasons(kk+1) AND s.PSFMAG_R ne -9999) ; -9999 removes outliers

      if n_elements(MJDent) gt 1 then begin    ; checking that there are obs in season
         MJDin = MJD_r(MJDent)                 ; the MJDs
         DMJDseason = max(MJDin)-min(MJDin)
         if DMJDsh eq [-999] then begin
            DMJDsh = DMJDseason
         endif else begin
            DMJDsh = [DMJDsh,DMJDseason]
         endelse
      endif
   endfor
   if n_elements(Dmjdsh) gt 1 then plothist,DMJDsh,xh,yh,BIN=1.,/overplot,col=CCC,thick=thickall
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

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================






if vb eq 1 then print,' '
if vb eq 1 then print,':: plot_subFITSfile.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END


FUNCTION LINE, X, P
  RETURN, P[0] + P[1]*X
END
