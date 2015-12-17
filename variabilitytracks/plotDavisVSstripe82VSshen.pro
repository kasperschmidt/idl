;+
;----------------------------
;   NAME
;----------------------------
; plotDavisVSstripe82VSshen.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure plotting the various QSO information (Shen et al 2010 catalog 
; combined with the g-r slope fits created via create_slopeANDshencat.pro)
; together with the Davis et al 2007 models and the interpretations of
; those presented in Knecht 2010 (bachelor thesis @ Uni Heidelberg).
; see list of plots under PLOTSELECT keyword
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; QSOcat        : QSO catalog to plot
; QSOfits       : datafile used to estimate the g,r slopes in QSOcat
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; PDFdir          : to estimate the outlier probability for each object, set this
;                   keyword to the path of directory containing the PDF files.
; NBIN            : the number of bins to bin data in when plotting.
;                   Default is Nbin=10
; Npointsmin      : the minimum number of points in bins to be plotted, 
;                   i.e. only bins with more points are shown
;                   Default is Npointsmin=25
; SUBSAM          : 4-component vector defining the mass and redshift subsample to 
;                   plot in figures 1,2,17,18 and 19. The input should be on the form: 
;                   SUBSAM = [MassMin,MassMax,Zmin,Zmax]
; PLOTSELECT      : Vector with the numbers of the plots to create. The choices are:
;                       1      : g-r     VS  logL/Ledd       M and z subsample
;                       2*     : g-r     VS  logL_g/Ledd     M and z subsample (uses def. from plot 1)
;                       3*!#   : r       VS  g               w. models (one plot per object)
;                       4*!    : g       VS  g-r             w. models (one plot per object)
;                       5*!    : g-r     VS  g               w. models (one plot per object)
;                       6&     : <r>     VS  <g>
;                       7      : slope   VS  z
;                       8      : slope   VS  logLLedd
;                       9+     : slope   VS  logLLedd        z-corrected 
;                       10     : slope   VS  logMMsun
;                       11+    : slope   VS  logMMsun        z-corrected 
;                       12     : slope   VS  logL5100Ledd
;                       13     : slope   VS  logL3000Ledd
;                       14     : slope   VS  logL1350Ledd
;                       15^    : LLedd   VS  M space         2D histogram w. slope as color coding
;                       16^+   : LLedd   VS  M space         2D histogram w. slope as color coding z-corrected (uses logLLedd from plot 15)
;                       17*&   : slope   VS  chi2 g-r
;                       18*&   : chi2 r  VS  chi2 g
;                       19+    : g-r     VS  g               M and z subsample; each object as small lines
;                       20*    : chi g   VS chi2 g
;                       21*    : chi r   VS chi2 r 
;                       22*    : chi g-r VS chi2 g-r 
;                       23$&%  : slope   VS A'
;                       24$&   : slope   VS gamma
;                       25$&+% : slope   VS A'               z-corrected 
;                       26$&+  : slope   VS gamma            z-corrected 
;                       27^%   : A'      VS gamma            2D histogram w. slope as color coding
;                       28^+%  : A'      VS gamma            2D histogram w. slope as color coding z-corrected
;                       29!    : r       VS  g               w. errbars w/o models (one plot per object) - NB! hardcoded limits on A' and gamma
;                       30+%   : slope   VS A'               modified zoom-in version of 25 (need info from 25) 
;                       31+    : slope   VS gamma            modified zoom-in version of 26 (need info from 26)  
;                       32+    : A'      VS A'_schw
;                       33*&   : Dslope  VS  chi g-r         combining plot 17 and 22
;                   Figures marked with ___'*'___ include davis model information and is therfore only plotted for 'gr' input.
;                   Figures marked with ___'+'___ use the acorrected information from plot 7 (which therefore needs to be plotted as well)
;                   Default is plotting all plots, i.e. PLOTSELECT=findgen(100)
; SMOOTH          : 2 component vector with FWHM of 2D gaussian kernel
;                   to smooth the 2D histograms in plots marked with ___'^'___
;                   NB! misleading since it smoothes the edges to the 'background' value/color
; /RUNCHI2        : set this keyword to run chi2 caluclations of input. If not set a
;                   standard/default hard coded file wil be loaded instead corresponding 
;                   to the redshift selected in SUBSAM. If such a file doesn't excist
;                   this keywrod is needed to create it.
; FIT             : FIT indicates what lables to use by given the space in which the input was
;                   performed. The default is FIT='gr'
; Nobjplot        : number of objects to plot in the plots marked with '!' above.
;                   Default is Nobjplot=1  ; NQSOs - NB! takes a lot of time for many objects
; MAGCUT          : This keyword is used to remove faint objects from the sample.
;                   Set it to a two component vector with the range in r-band (median of epochs)
;                   magnitude to have in the plotted sample.
;                      NB! Takes a bit of time for large input samples (~40 min for 9000 objects).
;                      NB! this cut disables the use of the PDF information
;                   By making the input a 4 or 6 component vector (e.g. [rmin,rmax,Amin,Amax,Gmin,Gmax]=[0.0,19.5,0.2,1.2,1.0,3.0]) 
;                   one can also put cuts on the Schmidt et al. 2010 parameters A and gamma
; /Fstars         : set /Fstars to overplot the Fstar results on plots marked with ___'$'___ above
; /RRL            : set /RRL to overplot the RRL results on plots marked with ___'$'___ above
; /GSLOPE         : set /GSLOPE to substitute fitted slopes with random slopes drawn
;                   from a gaussian distribution around the mean of the fitted slopes
; /CONTOUR        : set /CONTOUR to plot contours in plots marked with ___'&'___ above
; /ERRELLIPSE     : set /ERRELLIPSE to overplot error ellipses (and no models) on figures marked with ___'#'___ above
; /SLOPEM1        : set /SLOPEM1 to plot the obtained slopes mines 1 instead of just the slopes
; /SCHWARZA       : set /SCHWARZA to scale the variability amplitudes
;                   from Schmidt et al 2010 to the orbital time at the
;                   Schwartzhild radius instead of 1 year. This affects plots with ___'%'___
; /BOVYAandG      : set this keyword to match the objects with the A and gammas from Jo Bovy and plot those instead.
;                   NB! since the matching is done the keyword only works with plots 23-28,30-32 (and 7) 
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotDavisVSstripe82VSshen,'slopeANDshencatTue_Dec_14_152013_2010_season0.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL/',/VERBOSE,NBIN=40,Npointsmin=30,PLOTSELECT=[1,3,4,5,17,18],SUBSAM=[9.0,9.1,2.0,2.2]

; IDL> plotDavisVSstripe82VSshen,'slopeANDshencatThu_Feb_3_163401_2011_season0.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL_grfit110129/',/VERBOSE,NBIN=40,Npointsmin=30,PLOTSELECT=[7,8,9,10,11,12,13,14,15,16],SUBSAM=[9.0,9.1,2.0,2.2],FIT='gr'  ; MAGCUT=[0.0,19.5,0.2,1.2,1.0,3.0])

; IDL> plotDavisVSstripe82VSshen,'slopeANDshencatThu_Feb_3_163447_2011_season0.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL_uifit110129/',/VERBOSE,NBIN=40,Npointsmin=30,PLOTSELECT=[7,8,9,10,11,12,13,14,15,16],SUBSAM=[9.0,9.1,2.0,2.2],FIT='ui'


; IDL> plotDavisVSstripe82VSshen,'slopeANDshencatMon_Feb_7_132648_2011_season0_TOP10rg.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits',/VERBOSE,PLOTSELECT=[29]


; --------------- paper commands from notesLT110619 -------------------------
;IDL> plotDavisVSstripe82VSshen,'slopeANDshencatThu_Feb_3_163401_2011_season0_ERRcorrected.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL_grfit110129/',/VERBOSE,NBIN=40,Npointsmin=5,PLOTSELECT=[7,15,16,25,26,28,30,31],FIT='gr',/Fstars,/RRL,/slopeM1,/BOVYAandG,smooth=[6,5]       ,SUBSAM=[8.6,9.0,1.0,1.75],/eps

;IDL> plotDavisVSstripe82VSshen,'slopeANDshencatThu_Feb_3_163401_2011_season0_ERRcorrected.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL_grfit110129/',/VERBOSE,NBIN=40,Npointsmin=5,PLOTSELECT=[7,15,16,28],FIT='gr',/Fstars,/RRL,/slopeM1,/BOVYAandG,smooth=[2,2],/eps 

;IDL> plotDavisVSstripe82VSshen,'slopeANDshencatThu_Feb_3_163447_2011_season0_ERRcorrected.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL_uifit110129/',/VERBOSE,NBIN=40,Npointsmin=5,PLOTSELECT=[7,28],FIT='ui',/Fstars,/RRL,/slopeM1,/BOVYAandG,smooth=[2,2],/eps 



;IDL> plotDavisVSstripe82VSshen,'slopeANDshencatThu_Feb_3_163401_2011_season0_ERRcorrected.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',PDFdir='PDFs_S82QSOs_shenmatchALL_grfit110129/',/VERBOSE,NBIN=40,Npointsmin=5,PLOTSELECT=[19],FIT='gr',SUBSAM=[8.6,9.0,1.0,1.75]


;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-12-14  started by K. B. Schmidt (MPIA)
; 2011-01-19  keyword RUNCHI2 added. K. B. Schmidt (MPIA)
; 2011-01-24  keyword SUBSAM added. K. B. Schmidt (MPIA)
; 2011-02-08  keyword GSLOPE and MAGCUT added. K. B. Schmidt (MPIA)
; 2011-03-09  keyword CONTOUR added. K. B. Schmidt (MPIA)
; 2011-03-09  keyword ERRELLIPSE added. K. B. Schmidt (MPIA)
; 2011-04-09  keyword slopeM1 added. K. B. Schmidt (MPIA)
; 2011-04-11  keyword SCHWARZA added. K. B. Schmidt (MPIA)
; 2011-05-30  keyword BOVYAandG added. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ calcBadprob.pro
@ chisquareDavismodels.pro
@ contourarray.pro
@ properdist.pro
@ linearfitMCMC.pro
@ imgsmoothing2D.pro
@ imgsmoothingGWEIGHT.pro
@ matchRADEC.pro
@ sauron_colormap.pro
;----------------------------
;-
PRO plotDavisVSstripe82VSshen,QSOcat,QSOfits,PDFdir=PDFdir,NBIN=NBIN,Npointsmin=Npointsmin,EPS=EPS,VERBOSE=VERBOSE,RUNCHI2=RUNCHI2,PLOTSELECT=PLOTSELECT,SUBSAM=SUBSAM,SMOOTH=SMOOTH,FIT=FIT,Nobjplot=Nobjplot,GSLOPE=GSLOPE,MAGCUT=MAGCUT,Fstars=Fstars,RRL=RRL,CONTOUR=CONTOUR,ERRELLIPSE=ERRELLIPSE,slopeM1=slopeM1,SCHWARZA=SCHWARZA,BOVYAandG=BOVYAandG
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

BOAG = n_elements(BOVYAandG)
SCHW = n_elements(SCHWARZA)
SM1 = n_elements(slopeM1)
EE  = n_elements(ERRELLIPSE)
CONT = n_elements(CONTOUR)
PDF = n_elements(PDFdir)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)
NB  = n_elements(NBIN)
NPM = n_elements(Npointsmin)
SS  = n_elements(SUBSAM)
SMO = n_elements(SMOOTH)

if n_elements(PLOTSELECT) eq 0 then PLOTSELECT=findgen(100)
if n_elements(FIT)        eq 0 then fit = 'gr'                ; default lables set to gr


; ---- reading davis models ---- 
;if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Reading Davis models'
;FMT = ('f,f,f,f,d')  ; columns are:  log(M/Msun)   log(L/Ledd)   cosi   alpha   nuLnu2200
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/slopes/slope1450_2200_a0.0.out'    ,FORMAT=FMT,logM_D1,logL_D1,cosi_D1,alpha_D1,L2200_D1
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/slopes/slope1450_2200_a0.0_f1.out' ,FORMAT=FMT,logM_D2,logL_D2,cosi_D2,alpha_D2,L2200_D2
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/slopes/slope1450_2200_a0.9.out'    ,FORMAT=FMT,logM_D3,logL_D3,cosi_D3,alpha_D3,L2200_D3
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/slopes/slope2200_4000_a0.0.out'    ,FORMAT=FMT,logM_03,logL_03,cosi_03,alpha_03,L2200_03
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/slopes/slope2200_4000_a0.0_f1.out' ,FORMAT=FMT,logM_03,logL_03,cosi_03,alpha_03,L2200_03
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/slopes/slope2200_4000_a0.9.out'    ,FORMAT=FMT,logM_03,logL_03,cosi_03,alpha_03,L2200_03

; ---- reading Knecht interpretations of Davis models ---- 
if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Reading Knecht version of Davis models'
;FMT = ('f,f,f,d,f,f')  ; columns are:  log(M/Msun)   log(L/Ledd)   alpha   L2200   gr   gmag (Mass, alpha and Ls are mean over the 5 different cosi in Davis files)
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davis_data_converted/davis_models_a0.0_to_SDSS.dat'    ,FORMAT=FMT,logM_M1,logL_M1,alpha_M1,L2200_M1,gr_M1,gmag_M1
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davis_data_converted/davis_models_a0.0_f1_to_SDSS.dat' ,FORMAT=FMT,logM_M2,logL_M2,alpha_M2,L2200_M2,gr_M2,gmag_M2
;readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davis_data_converted/davis_models_a0.9_to_SDSS.dat'    ,FORMAT=FMT,logM_M3,logL_M3,alpha_M3,L2200_M3,gr_M3,gmag_M3

FMT = ('f,f,f,f,d,f,f')  ; columns are: z log(M/Msun) log(L/Ledd) alpha L2200 gr gmag (Mass, alpha and Ls are mean over the 5 different cosi in Davis files)
readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davismodels_allZandM/davis_models_a0.0_to_SDSS.dat',FORMAT=FMT,zbin1,logM_M1,logL_M1,alpha_M1,L2200_M1,gr_M1,gmag_M1
readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davismodels_allZandM/davis_models_a0.0_f1_to_SDSS.dat',FORMAT=FMT,zbin2,logM_M2,logL_M2,alpha_M2,L2200_M2,gr_M2,gmag_M2
readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davismodels_allZandM/davis_models_a0.9_to_SDSS.dat',FORMAT=FMT,zbin3,logM_M3,logL_M3,alpha_M3,L2200_M3,gr_M3,gmag_M3

; ----------------------------------------
ZBINuniq = Zbin1(uniq(Zbin1))   ; the different redshift bins of the models
; selecting model values for given z bin.
if SS ne 0 then begin
   deltaZ   = SUBSAM(3)-SUBSAM(2)
   Dzbin    = abs(ZBINuniq-(SUBSAM(2)+deltaZ/2.))             ; difference between z bins and center of chosen z range 
   Bestent  = where(Dzbin eq min(Dzbin))                      ; entry of z bin closest to center of chosen z range
   ent1     = where(Zbin1 eq ZBINuniq(bestent(0)))            ; model entries to use (taking the smallest if multiple bins)
   ent2     = where(Zbin2 eq ZBINuniq(bestent(0)))            ; model entries to use (taking the smallest if multiple bins)
   ent3     = where(Zbin3 eq ZBINuniq(bestent(0)))            ; model entries to use (taking the smallest if multiple bins)
   if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Model for redshift '+strtrim(ZBINuniq(bestent(0)),2)+' chosen for plots.'
   if vb eq 1 then print,'                                    This resulted in '+strtrim(n_elements(ent1),2)+', '+strtrim(n_elements(ent2),2)+' and '+strtrim(n_elements(ent3),2)+' entries.'
   zstring  =  STRJOIN(STRSPLIT(ZBINuniq(bestent(0)),'.',/extract),'p')  ; string for chi2 filename
endif else begin
   ent1     = where(Zbin1 eq 1.5)
   ent2     = where(Zbin2 eq 1.5)
   ent3     = where(Zbin3 eq 1.5)
   zstring  = '1p50000'
endelse

;chi2restorefile = 'chisquareDavismodels'+strtrim(zstring,2)+'_for_0z7AND0p0A2p0AND0p0gamma5p0_Wed_Dec_8_12:54:21_2010.idlsave'  ; restores C2_gmag,C2_rmag,C2_gr,C2_slope,slopes

chi2restorefile = 'chisquareDavismodels'+strtrim(zstring,2)+'_for_0z7AND0p0A2p0AND0p0gamma5p0_Wed_Dec_8_12:54:21_2010_FEB3.idlsave'
;chi2restorefile = 'chisquareDavismodels_for_0z7AND0p0A2p0AND0p0gamma5p0_Wed_Dec_8_12:54:21_2010_no2.idlsave'  ; original w. C2_gmag,C2_rmag,C2_gr,C2_slope

logM_M1  = logM_M1(ent1)
logL_M1  = logL_M1(ent1)
alpha_M1 = alpha_M1(ent1)
L2200_M1 = L2200_M1(ent1)
gr_M1    = gr_M1(ent1)
gmag_M1  = gmag_M1(ent1)

logM_M2  = logM_M2(ent2)
logL_M2  = logL_M2(ent2)
alpha_M2 = alpha_M2(ent2)
L2200_M2 = L2200_M2(ent2)
gr_M2    = gr_M2(ent2)
gmag_M2  = gmag_M2(ent2)

logM_M3  = logM_M3(ent3)
logL_M3  = logL_M3(ent3)
alpha_M3 = alpha_M3(ent3)
L2200_M3 = L2200_M3(ent3)
gr_M3    = gr_M3(ent3)
gmag_M3  = gmag_M3(ent3)
; ----------------------------------------

rmag_M1 = gmag_M1-gr_M1     ; r-band magnitudes
rmag_M2 = gmag_M2-gr_M2     ; r-band magnitudes
rmag_M3 = gmag_M3-gr_M3     ; r-band magnitudes

MMent  = uniq(logM_M1)      ; netries of unique masses
Nmass  = n_elements(MMent)  ; number of mass bins

; ---- reading QSO data ---- 
if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Reading QSO data files'
Qcat  = mrdfits(QSOcat,1)
Qdata = mrdfits(QSOfits,1)
NQSOs = n_elements(Qcat.headobjid)  ; number of QSOs with shen matches
IDS   = Qdata(uniq(Qdata.headobjid)).headobjid
; ---- cahnging slopes ----
if SM1 eq 1 then begin 
   Qcat.abest = Qcat.abest-1
   YLIM = 0. ; upper limit for polyfills
endif else begin
   YLIM = 1. ; upper limit for polyfills
endelse
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if n_elements(MAGCUT) ne 0 then begin                           ; removing entries with too dimm magnitudes (among other things)

   CUTTYPE = n_elements(MAGCUT)                              ; checking what cuts to make. 
   case CUTTYPE of
      ; +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
      2: begin                                               ; make cut in mag only
         if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Cutting in r-band mag only'
         for mm=0,NQSOs-1 do begin                                    ; looping over QSOs
            if vb eq 1 and mm/1000. eq round(mm/1000.) then print,'                                          Checking object '+strtrim(mm,2)+' @ '+strtrim(systime(),2)
            Objent = where(Qdata.headobjid eq IDs(mm))                ; getting entries of mm'th object
            magobj = median(Qdata(Objent).psfmag_r)
            if magobj gt MAGCUT(0) and magobj lt MAGCUT(1) then begin ; checking if mean r magnitude is within magnitude cut
               if n_elements(IDselect) eq 0 then begin                ; checking if it is the first object
                  IDselect = IDs(mm)                                  ; saving ID
                  MCent    = Objent                                   ; saving object entries
                  MCent2   = mm                                       ; saving object number
               endif else begin                                       ; if and object has already been saved then...
                  IDselect = [IDselect,IDs(mm)]                       ; append ID
                  MCent    = [MCent,Objent]                           ; append object entries
                  MCent2   = [MCent2,mm]                              ; append object number
               endelse
            endif
         endfor
      end
      ; +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
      4: begin                                               ; make cut in mag and A only
         if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Cutting in r-band mag and A only'
         for mm=0,NQSOs-1 do begin                                    ; looping over QSOs
            if vb eq 1 and mm/1000. eq round(mm/1000.) then print,'                                          Checking object '+strtrim(mm,2)+' @ '+strtrim(systime(),2)
            Objent = where(Qdata.headobjid eq IDs(mm))                ; getting entries of mm'th object
            magobj = median(Qdata(Objent).psfmag_r)
            Aobj   = Qcat(mm).A
            if magobj gt MAGCUT(0) and magobj lt MAGCUT(1) and Aobj gt MAGCUT(2) and Aobj lt MAGCUT(3) then begin  ; checking if r, and A values are within specified cut
               if n_elements(IDselect) eq 0 then begin                ; checking if it is the first object
                  IDselect = IDs(mm)                                  ; saving ID
                  MCent    = Objent                                   ; saving object entries
                  MCent2   = mm                                       ; saving object number
               endif else begin                                       ; if and object has already been saved then...
                  IDselect = [IDselect,IDs(mm)]                       ; append ID
                  MCent    = [MCent,Objent]                           ; append object entries
                  MCent2   = [MCent2,mm]                              ; append object number
               endelse
            endif
         endfor
      end
      ; +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
      6: begin                                               ; make cut in mag, A and gamma
         if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Cutting in r-band mag, A and gamma'
         for mm=0,NQSOs-1 do begin                                    ; looping over QSOs
            if vb eq 1 and mm/1000. eq round(mm/1000.) then print,'                                          Checking object '+strtrim(mm,2)+' @ '+strtrim(systime(),2)
            Objent = where(Qdata.headobjid eq IDs(mm))                ; getting entries of mm'th object
            magobj = median(Qdata(Objent).psfmag_r)
            Aobj   = Qcat(mm).A
            Gobj   = Qcat(mm).gamma
            if magobj gt MAGCUT(0) and magobj lt MAGCUT(1) and Aobj gt MAGCUT(2) and Aobj lt MAGCUT(3) and Gobj gt MAGCUT(4) and Gobj lt MAGCUT(5) then begin ; checking if r, A and gamma values are within specified cut
               if n_elements(IDselect) eq 0 then begin                ; checking if it is the first object
                  IDselect = IDs(mm)                                  ; saving ID
                  MCent    = Objent                                   ; saving object entries
                  MCent2   = mm                                       ; saving object number
               endif else begin                                       ; if and object has already been saved then...
                  IDselect = [IDselect,IDs(mm)]                       ; append ID
                  MCent    = [MCent,Objent]                           ; append object entries
                  MCent2   = [MCent2,mm]                              ; append object number
               endelse
            endif
         endfor
      end
      ; +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
      else: begin
         if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Wrong dimension of MAGCUT input  -> aborting'
         stop
      end
   endcase
   if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: The selected magnitude cut removed ',strtrim(NQSOs-n_elements(IDselect),2)+' out of the '+strtrim(NQSOs,2)+' intial objects ('+strtrim(n_elements(IDselect),2)+' objects left)'
   Qcat  = Qcat(MCent2)                                        ; extracting selected entries
   Qdata = Qdata(MCent)                                        ; extracting selected entries
   NQSOs = n_elements(IDselect)                                ; counting selected QSOs
   IDs   = IDselect                                            ; saving new list of IDs
endif
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if SCHW eq 1 then begin
; getting A in units of Schwarzschild orbital times (proportional to BHmass)
NschwRadii = 60
Nschw  = NschwRadii^(3./2.)
;Nschw  = 100. ; number of Schwarzchild orbital times (correspond to Nschw^2/3 schw radii) to scale with 
             ; (so the scaling is comparable to the original 1yr scaling) 
Qcat.A = Qcat.A*(2.77*10^(-3.)*10^(Qcat.LOGBH_MGII_MD04-9.)*Nschw)^Qcat.gamma  ; 3 is really 2.77
Qcat(where(Qcat.LOGBH_MGII_MD04 eq 0)).A = 9999  ; setting amplitudes to 0 if no BH mass estimate excists
Aschw = Qcat.A
save,Aschw,filename='Aprime_Schw_Nschw.sav'
Nschw = 10.
;
endif
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
aglobal = median(Qcat.abest)
if N_elements(Nobjplot) eq 0 then Nobjplot = 1 ; NQSOs  ; the number of objects to plot.
if NB eq 0  then NBIN = 10        ; setting default number of bins for plots with binning
if NPM eq 0 then Npointsmin = 25  ; setting default number of minimum points in bins to be plotted
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; subsample ranges used in plots, 1,2 and 19
if SS eq 0 then begin   ; if no subsample is defined on the command line define it here
   MassMin  = 8.5
   MassMax  = 8.6
   Zmin     = 1.4
   Zmax     = 1.6
endif else begin
   MassMin  = SUBSAM(0)
   MassMax  = SUBSAM(1) 
   Zmin     = SUBSAM(2)
   Zmax     = SUBSAM(3)
endelse
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if n_elements(GSLOPE) ne 0 then begin                  ; creating 'fake' slopes from a gaussian distribution around the mean slope 
   if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Creating "fake" gaussian distributed slopes'
   AbestORIGINAL  = Qcat(*).abest                      ; saving original slopes in vector
   GSIGMA         = 0.1                                ; sigma of gaussian distribution (new errorbar)
   GMEAN          = mean(AbestORIGINAL)                ; mean of gaussian distribution
   GAUSSVAL       = RANDOMN(seed,NQSOs)                ; random gaussian points drawn from [0,1] distribution
   Qcat.abest     = GAUSSVAL * GSIGMA + GMEAN          ; new gaussian distributed slopes
endif
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if where(PLOTSELECT eq 17) ne [-1] or where(PLOTSELECT eq 18) ne [-1] or where(PLOTSELECT eq 20) ne [-1] or where(PLOTSELECT eq 21) ne [-1] or where(PLOTSELECT eq 22) ne [-1] or where(PLOTSELECT eq 33) ne [-1] then begin ; only restoring chi2 when needed
   ; restoring the chi2 data arrays for plotting (and potentially calculating them... however for many objects that 
   ; takes a long time!) The restoring returns the arrays: C2_gmag,C2_rmag,C2_gr,C2_slopem,slopes for further maniulation.
   ; see chisquareDavismodels.pro for more information on the arrays.
   if n_elements(RUNCHI2) eq 1 then begin
      chisquareDavismodels,QSOcat,QSOfits,chi2output,/VERBOSE                                              ; running the chi2 calculations
   endif else begin
      chi2output = chi2restorefile    ; loading save file of chi2 results
;      chi2output = 'chisquareDavismodelsTOP100.idlsave'  ; loading save file of chi2 results
   endelse
   if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: Restoring the chi2 file '+strtrim(chi2output,2)
   restore,chi2output
   C2_gmag0  = C2_gmag
   C2_rmag0  = C2_rmag
   C2_gr0    = C2_gr
   C2_slope0 = C2_slope
endif
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


if PDF eq 1 and n_elements(MAGCUT) eq 0 then begin
   ; structure containing percentages
   PbadObj = fltarr(n_elements(Qdata.headobjid))  ; vector with the size of data array for probabilities
   PARAM = fltarr(8)
;   for ll=0,NQSOs-1 do begin
   for ll=0,Nobjplot-1 do begin
      ; Reading PDF parameters for ll'th object
      PDFnumber  = where(strtrim(IDS,2) eq strtrim(Qcat(ll).headobjid,2))
;      PDFfileobj = strtrim(PDFdir,2)+'PDFs_object'+strtrim(PDFnumber,2)+'.dat'
      if fit eq 'ui' then PDFfileobj = strtrim(PDFdir,2)+'PDFs_object'+strtrim(PDFnumber,2)+'_uifit.dat'
      if fit eq 'gr' then PDFfileobj = strtrim(PDFdir,2)+'PDFs_object'+strtrim(PDFnumber,2)+'_grfit.dat'

      ; --- Reading ASCII PDF file ---
      Nrows        = File_lines(PDFfileobj(0))   ; number of rows in file
      Nheaderlines = 6                           ; number of header lines
      Ncol         = 9                           ; number of columns in file
      PARAM0 = fltarr(Ncol,Nrows-Nheaderlines)   ; data to read file into
      OPENR,lun,PDFfileobj(0), /GET_LUN      
      header = STRARR(6)
      READF,lun,header   ; reading header into string array
      READF,lun,PARAM0   ; note that these parameters also include the KlnL value - hence PARAM[2] is not needed
      free_lun,lun  
      ; -----------------------------

      Bent = where(PARAM0(2,*) eq min(PARAM0(2,*)))  ; entry of the fit value with highest probability
      
      PARAM = [PARAM0(0,Bent)*cos(atan(PARAM0(1,Bent))),atan(PARAM0(1,Bent)),PARAM0(3,Bent),PARAM0(4,Bent),PARAM0(5,Bent),PARAM0(6,Bent),PARAM0(7,Bent),PARAM0(8,Bent)]  ; the parameters   bperp, theta, Pb, Xb, Yb, Vxb, Vyb, corr

      ; getting data for ll'th object
      Pent = where(Qdata.headobjid eq Qcat(ll).headobjid,Cobj)
      DATARRAY = fltarr(Cobj,4)
      DATARRAY(*,0) = Qdata(Pent).psfmag_g    
      DATARRAY(*,1) = Qdata(Pent).psfmag_r    
      DATARRAY(*,2) = Qdata(Pent).psfmagERR_g 
      DATARRAY(*,3) = Qdata(Pent).psfmagERR_r 

      ; of setting data (assumes that the fitting results in PDFfileobj were calculated with that keyword)
      Mxcomp          = mean(DATARRAY(*,0))  ; mean value of x components
      Mycomp          = mean(DATARRAY(*,1))  ; mean value of x components
      DATARRAY(*,0)   = DATARRAY(*,0) - Mxcomp
      DATARRAY(*,1)   = DATARRAY(*,1) - Mycomp

      calcBadprob,PARAM,DATARRAY,PbadObj0;,/VERBOSE

      PbadObj(Pent) = PbadObj0  ; filling vector with probability of points (epochs) beeing bad
;if ll eq 1 then stop
   endfor
   PbadMIN = 0.5  ; all points with Pbad > PbadMIN are plotted as open symbols 
endif

; --- TIP ---  to get statistics on the outlier frequency/fraction run the code for Nobjplot=XX and do:
outstat = 0
if outstat eq 1 then begin
   filledentries = where(PbadObj ne 0.00,Cfill) 
   badentries    = where(PbadObj gt PbadMIN,Cbad) 
   badfrac = (Cbad+0.0)/(Cfill+0.0)
   if vb eq 1 then print,' '
   if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: The fraction of epochs removed averaged over ',trim(Nobjplot),' objects'
   if vb eq 1 then print,'                                    with an outlier rejection of Pbad>',trim(Pbadmin),' is:  ',trim(badfrac)
   stop
endif


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; calculating correaltions of data (spearman's rank, pearson rank and kendall's tau)
correlationcalc = 0
if correlationcalc eq 1 then begin
   spearman = fltarr(NQSOs,2)
   kendall  = fltarr(NQSOs,2)
   pearson  = fltarr(NQSOs)

   Nobjloop = NQSOs
   for ii=0,Nobjloop-1 do begin    ; looping over objects with shen data
      Objent = where(Qdata.headobjid eq Qcat(ii).headobjid,Cepochs)   ; entries for given object
      Xdat = Qdata(objent).psfmag_g
      Ydat = Qdata(objent).psfmag_r

      spearman(ii,*) = R_CORRELATE(Xdat, Ydat) 
      kendall(ii,*)  = R_CORRELATE(Xdat, Ydat,/KENDALL) 
      pearson(ii)    = CORRELATE(Xdat, Ydat) 
   endfor

   if vb eq 1 then print,' '
   if vb eq 1 then print,":: plotDavisVSstripe82VSshen.pro :: The average Spearman's rank of the data is       ",trim(total(abs(spearman(*,0)))/Nobjloop)
   if vb eq 1 then print,":: plotDavisVSstripe82VSshen.pro :: The average Kendall's tau of the data is         ",trim(total(kendall(*,0))/Nobjloop)
   if vb eq 1 then print,":: plotDavisVSstripe82VSshen.pro :: The average Pearson's correlation of the data is ",trim(total(abs(pearson(*)))/Nobjloop)
   if vb eq 1 then print,' '
   stop
endif



; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; matching objects with Bovy's catalog
if BOAG eq 1 then begin
   bovy      = mrdfits('/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/powerlawSF_constmean/powerlawSF_constmean_r.fits',1) 

   Narcsec = 3 ; the number of arc seconds to match objects within
   matchRADEC,Qcat.ra,Qcat.dec,bovy.ra,bovy.dec,Narcsec*0.0002777778,MATCH,/VERBOSE

   Qcat(match(0,*)).A = Qcat(match(0,*)).A*0+1e-8        ; setting all A values to 1e-8
   Qcat(match(0,*)).A = exp(bovy(match(1,*)).loga/2.)    ; filling entries with bovy values

   Qcat(match(0,*)).gamma = Qcat(match(0,*)).gamma*0-10  ; setting all gamma values to -10
   Qcat(match(0,*)).gamma = bovy(match(1,*)).gamma       ; filling entries with bovy values
endif
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


Nw = 0 ; resetting window numbering
if where(PLOTSELECT eq 1) ne [-1] then begin
;=============================================================================================
;Dmassent = where(Qcat.LOGBH_MGII_MD04 gt 0, Cmassobj)   ; selecting all objects with mass estimate
Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
Ledd     = alog10(1.3)+38+Qcat(Dmassent).LOGBH_MGII_MD04
logLLedd = Qcat(Dmassent).LOGLBOL-Ledd  ; log(L_bol/L_edd) in mass range

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/'+strtrim(FIT,2)+'VSLLedd_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'color vs L/Ledd'
   thickall = 2
endelse
; setting plot range
XR = [-2.0,0]
YR = [-0.5,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,logLLedd,logLLedd, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{bol}/L_{edd})') $
        , ytitle =strmid(fit,0,1)+'-'+strmid(fit,1,1) $
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

LOADCT,39
device,decomposed=0
BYTCOL   = BYTSCL(FINDGEN(Cmassobj))  ; array used to color datasets
for ii=0,Cmassobj-1 do begin
;for ii=0,1 do begin
   Objent = where(Qdata.headobjid eq IDS(Dmassent(ii)))
   grDM     = Qdata(Objent).psfmag_g-Qdata(Objent).psfmag_r   ; gr for object
   PLOTSYM,0,0.5,/FILL   
   oplot,fltarr(n_elements(grDM))+logLLedd(ii),grDM,col=BYTCOL(ii),psym=8,thick=thickall 
   PLOTSYM,0,1.1,/FILL   
   oplot,fltarr(2)+logLLedd(ii),fltarr(2)+mean(grDM),col=BYTCOL(ii),psym=8,thick=thickall 
endfor
col = getcolor(/load)

XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin,'(F10,1)')+':'+trim(MassMax,'(F10,1)')+']',col=col.black,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin,'(F10,1)')+':'+trim(Zmax,'(F10,1)')+']',col=col.black,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'epoch color  : ',col=col.black,charsize=2.5,charthick=thickall
   PLOTSYM,0,0.5,/FILL   
   oplot,fltarr(2)+DX*0.35+XR[0],fltarr(2)+DY*0.86+YR[0],col=col.black,psym=8,thick=thickall 
XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],'mean color  : ',col=col.black,charsize=2.5,charthick=thickall
   PLOTSYM,0,1.0,/FILL   
   oplot,fltarr(2)+DX*0.35+XR[0],fltarr(2)+DY*0.81+YR[0],col=col.black,psym=8,thick=thickall 


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 2) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax, Cmassobj)  ; selecting mass range
Ledd     = alog10(1.3)+38+Qcat(Dmassent).LOGBH_MGII_MD04

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/'+strtrim(FIT,2)+'VSLLedd_'+strmid(fit,0,1)+'_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'color vs L/Ledd'
   thickall = 2
endelse
; setting plot range
XR = [-4.0,0]
;XR = [44,46]  ; xrange for logLg plotting
YR = [-0.5,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{g}/L_{edd})') $
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

LOADCT,39
device,decomposed=0
BYTCOL   = BYTSCL(FINDGEN(Cmassobj))  ; array used to color datasets
for ii=0,Cmassobj-1 do begin
;for ii=0,1 do begin
   Objent = where(Qdata.headobjid eq IDS(Dmassent(ii)))

   CC        = 3e8                                                   ; m/s
   Lam_g     = 4770e-10                                              ; m
   Fluxg     = 10^(-Qdata(Objent).psfmag_g/2.5)*3631*1e-23*CC/Lam_g  ; Fb from Ivezic et al 2007 times c/lambda
   properdist,Qcat(Dmassent(ii)).z,Dp,Dl,Da;,/VERBOSE                ; calculating luminosoty distance
   L_g48     = 4*!pi*Fluxg*(DL*3.068)^2                              ; L_g/1e48
   logLg     = alog10(L_g48)+48
   logLgLedd = logLg-Ledd                                            ; log(L_bol/L_edd) in mass range
   grDM      = Qdata(Objent).psfmag_g-Qdata(Objent).psfmag_r         ; gr for object

   PLOTSYM,0,0.5,/FILL   
   oplot,logLgLedd,grDM,col=BYTCOL(ii),psym=8,thick=thickall 
;   oplot,logLg,grDM,col=BYTCOL(ii),psym=8,thick=thickall 
endfor
col = getcolor(/load)

XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin,'(F10,1)')+':'+trim(MassMax,'(F10,1)')+']',col=col.black,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin,'(F10,1)')+':'+trim(Zmax,'(F10,1)')+']',col=col.black,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'indiv. epoch: ',col=col.black,charsize=2.5,charthick=thickall
   PLOTSYM,0,0.5,/FILL   
   oplot,fltarr(2)+DX*0.35+XR[0],fltarr(2)+DY*0.86+YR[0],col=col.black,psym=8,thick=thickall 

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 3) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
for ii=0,Nobjplot-1 do begin    ; looping over objects with shen data
   Objent = where(Qdata.headobjid eq Qcat(ii).headobjid,Cepochs)   ; entries for given object
   Xdat = Qdata(objent).psfmag_g
   Ydat = Qdata(objent).psfmag_r
   Xdaterr = Qdata(objent).psfmagERR_g
   Ydaterr = Qdata(objent).psfmagERR_r

   ;= = = Models vs Object plot = = =
   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/modVSobj'+strtrim(ii,2)+'.eps' ; name of eps file
      if EE eq 1 then plot1 = 'epsplots/modVSobj'+strtrim(ii,2)+'NoModel.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
      !P.FONT = 0                        ; allowing to change font of device
      device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot      
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'Model vs object'
      thickall = 2
   endelse

   ; setting plot range
   XR = [median(Xdat)+1,median(Xdat)-1]
   YR = [median(Xdat)+1,median(Xdat)-1]
   ;XR = [max(Xdat)+0.1,min(Xdat)-0.1]
   ;YR = [max(Ydat)+0.1,min(Ydat)-0.1]
   XR = [20.4,19.5] ; cutout of object 1
   YR = [20.35,19.3] ; cutout of object 1
   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]
   XT = STRMID(fit, 0, 1)
   YT = STRMID(fit, 1, 1)

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,gmag_M1,rmag_M1, col=col.black    $
        , /NODATA $
        , xtitle =XT $
        , ytitle =YT $
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
   ; ============ MODELS ============
   for jj=0,Nmass-1 do begin   ; looping over the individual masses
      entries = where(logM_M3 eq logM_M3(MMent(jj)))
;      oplot,gmag_M1(entries),rmag_M1(entries),linestyle=1,thick=thickall,col=col.green
;      oplot,gmag_M2(entries),rmag_M2(entries),linestyle=1,thick=thickall,col=col.red
;      oplot,gmag_M3(entries),rmag_M3(entries),linestyle=1,thick=thickall,col=col.orange
      ;XYOUTS,min(gmag_M3(entries)),min(rmag_M3(entries)),textoidl('  log(M/M_\odot)=')+strtrim(logM_M3(MMent(jj)),2),col=col.black,charsize=1.0,charthick=thickall
   endfor

   if EE eq 0 then begin  ; plotting models if no error ellipses plotted
      ; over-plotting model with mass closest to Shen estimate in thick solid
      QSOmass = Qcat(ii).LOGBH_MGII_MD04
      if Qcat(ii).LOGBH_MGII_MD04 eq 0 then QSOmass = 8.5  ; if mass not estimated in shen catalog then set to 8.5
      Mdiff     = abs(logM_M1(MMent) - QSOmass)
      Mmodelent = where(Mdiff eq min(Mdiff)) ;  the entry of the model closest in mass to object
      for kk=0,2 do begin ; drawing best mass model and two adjecent models
         Plotent = Mmodelent+kk-1
         if Plotent ge 0 and Plotent le n_elements(MMent)-1 then begin ; making sure there are models for the given mass
            modelmass = logM_M3(MMent(Plotent))  ;  mass of model (closest to object in mass)
            entries = where(logM_M3 eq modelmass(0))  ; entries for model mass closest to object in mass

            oplot,gmag_M1(entries),rmag_M1(entries),linestyle=0,thick=thickall+2,col=col.green
            oplot,gmag_M2(entries),rmag_M2(entries),linestyle=0,thick=thickall+2,col=col.red 
            oplot,gmag_M3(entries),rmag_M3(entries),linestyle=0,thick=thickall+2,col=col.orange 

            testmass = 1
            if testmass eq 1 then begin
               if kk eq 0 then begin
                  colstr = 'col.blue'
                  oplot,gmag_M1(entries),rmag_M1(entries),linestyle=0,thick=thickall+2,col=col.blue
                  oplot,gmag_M2(entries),rmag_M2(entries),linestyle=0,thick=thickall+2,col=col.blue
                  oplot,gmag_M3(entries),rmag_M3(entries),linestyle=0,thick=thickall+2,col=col.blue
               endif
               if kk eq 1 then begin
                  colstr = 'col.cyan'
                  oplot,gmag_M1(entries),rmag_M1(entries),linestyle=0,thick=thickall+2,col=col.cyan
                  oplot,gmag_M2(entries),rmag_M2(entries),linestyle=0,thick=thickall+2,col=col.cyan
                  oplot,gmag_M3(entries),rmag_M3(entries),linestyle=0,thick=thickall+2,col=col.cyan
               endif
               if kk eq 2 then begin
                  colstr = 'col.magenta'
                  oplot,gmag_M1(entries),rmag_M1(entries),linestyle=0,thick=thickall+2,col=col.magenta
                  oplot,gmag_M2(entries),rmag_M2(entries),linestyle=0,thick=thickall+2,col=col.magenta
                  oplot,gmag_M3(entries),rmag_M3(entries),linestyle=0,thick=thickall+2,col=col.magenta
               endif

               print,'------------------------- Modelmass ',trim(modelmass),' = ',trim(colstr)

            endif





            ;XYOUTS,DX*0.95+XR[0],DY*(0.05+kk*0.05)+YR[0],'Solid Models: '+textoidl('log(M_{Model}/M_\odot)=')+trim(modelmass,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
         endif
      endfor
   endif

   ;by hand
   XYOUTS,DX*0.95+XR[0],DY*0.15+YR[0],textoidl('log(M_{Model}/M_{\odot})=9.0\pm0.1'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0

   ; ================================

   NX = 30
   XX = findgen(NX)-15
   oplot,XX+mean(Xdat),Qcat(ii).Abest*XX+mean(Ydat)+Qcat(ii).Bbest,linestyle=0,thick=thickall,col=col.blue
   print,mean(Xdat),Qcat(ii).Abest,mean(Ydat)+Qcat(ii).Bbest
   ; the correct errors on the first object!
   ;Qcat(0).Bminus68 = 0.0113063     
   ;Qcat(0).Bplus68  = 0.00682601 
   ;Qcat(0).Aminus68 = 0.0542855
   ;Qcat(0).Aplus68  = 0.0516941 

   ; overplotting 'error-lines' 
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(Qcat(ii).Abest+Qcat(ii).Aplus68)*XX(NX/2.:NX-1)+mean(Ydat)+(Qcat(ii).Bbest+Qcat(ii).Bplus68),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(Qcat(ii).Abest+Qcat(ii).Aplus68)*XX(0:NX/2.)   +mean(Ydat)+(Qcat(ii).Bbest-Qcat(ii).Bminus68),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(Qcat(ii).Abest-Qcat(ii).Aminus68)*XX(0:NX/2.)   +mean(Ydat)+(Qcat(ii).Bbest+Qcat(ii).Bplus68),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(Qcat(ii).Abest-Qcat(ii).Aminus68)*XX(NX/2.:NX-1)+mean(Ydat)+(Qcat(ii).Bbest-Qcat(ii).Bminus68),linestyle=2,thick=thickall,col=col.blue

   ;oplot,XX(NX/2.:NX-1)+mean(Xdat),(Qcat(ii).Abest+Qcat(ii).Aplus95)*XX(NX/2.:NX-1)+mean(Ydat)+(Qcat(ii).Bbest+Qcat(ii).Bplus95),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(0:NX/2.)   +mean(Xdat),(Qcat(ii).Abest+Qcat(ii).Aplus95)*XX(0:NX/2.)   +mean(Ydat)+(Qcat(ii).Bbest-Qcat(ii).Bminus95),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(0:NX/2.)   +mean(Xdat),(Qcat(ii).Abest-Qcat(ii).Aminus95)*XX(0:NX/2.)   +mean(Ydat)+(Qcat(ii).Bbest+Qcat(ii).Bplus95),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(NX/2.:NX-1)+mean(Xdat),(Qcat(ii).Abest-Qcat(ii).Aminus95)*XX(NX/2.:NX-1)+mean(Ydat)+(Qcat(ii).Bbest-Qcat(ii).Bminus95),linestyle=3,thick=thickall,col=col.blue

   plotsym,0,1.5,/fill
   oplot,Xdat,Ydat,psym=8,col=col.black

   if PDF eq 1 then begin
      Pdat = PbadObj(objent) 
      OLs  = where(Pdat gt PbadMIN)   ; entries of probable outliers
      if OLs ne [-1] then begin
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.white   ; 'erasing' objects
         plotsym,0,1.5
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.black
      endif
   endif

   if EE eq 1 then begin  ; plotting error ellipses if requested
      for nn=0,Cepochs-1 do begin
        ell = ELLIPSE([Xdat(nn),Ydat(nn)],[Xdaterr(nn),Ydaterr(nn)])
         if PDF eq 1 then begin
            if Pdat(nn) gt Pbadmin then begin
               oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black,linestyle=2  ; plotting dashed ellipse
            endif else begin
               oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black
            endelse
         endif else begin
            oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black
         endelse
      endfor
   endif

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'SDSS J'+strtrim(Qcat(ii).sdssname,2),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],textoidl('log(M_{BH}/M_\odot)=')+trim(Qcat(ii).LOGBH_MGII_MD04,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('log(L_{bol}/(erg/s))=')+trim(Qcat(ii).LOGLBOL,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],textoidl('z=')+trim(Qcat(ii).z,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.75+YR[0],"MCMC 'best' fit",col=col.blue,charsize=2.5,charthick=thickall
;   XYOUTS,DX*0.05+XR[0],DY*0.70+YR[0],'DR7 '+strtrim(Qcat(ii).headobjid,2),col=col.black,charsize=2.5,charthick=thickall

   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif
endfor
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 4) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
for ii=0,Nobjplot-1 do begin    ; looping over objects with shen data
   Objent = where(Qdata.headobjid eq Qcat(ii).headobjid)   ; entries for given object
   Xdat = Qdata(objent).psfmag_g-Qdata(objent).psfmag_r
   Ydat = Qdata(objent).psfmag_g

   ;= = = Models vs Object plot = = =
   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/modVSobj_gr'+strtrim(ii,2)+'.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'Model vs object'
      thickall = 2
   endelse

   ; setting plot range
   XR = [median(Xdat)-1,median(Xdat)+1]
   YR = [median(Ydat)+1,median(Ydat)-1]
   ;XR = [min(Xdat)-0.1,max(Xdat)+0.1]
   ;YR = [max(Ydat)+0.1,min(Ydat)-0.1]
   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]
   YT = STRMID(fit, 0, 1)

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,gmag_M1,rmag_M1, col=col.black    $
        , /NODATA $
        , xtitle ='g-r' $
        , ytitle =YT $
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
   ; ============ MODELS ============
   for jj=0,Nmass-1 do begin   ; looping over the individual masses
      entries = where(logM_M3 eq logM_M3(MMent(jj)))
;      oplot,gr_M1(entries),gmag_M1(entries),linestyle=1,thick=thickall,col=col.green
;      oplot,gr_M2(entries),gmag_M2(entries),linestyle=1,thick=thickall,col=col.red
;      oplot,gr_M3(entries),gmag_M3(entries),linestyle=1,thick=thickall,col=col.orange
      ;XYOUTS,min(gmag_M3(entries)),min(rmag_M3(entries)),textoidl('  log(M/M_\odot)=')+strtrim(logM_M3(MMent(jj)),2),col=col.black,charsize=1.0,charthick=thickall
   endfor

   ; over-plotting model with mass closest to Shen estimate in thick solid
   QSOmass = Qcat(ii).LOGBH_MGII_MD04
   if Qcat(ii).LOGBH_MGII_MD04 eq 0 then QSOmass = 8.5  ; if mass not estimated in shen catalog then set to 8.5
   Mdiff     = abs(logM_M1(MMent) - QSOmass)
   Mmodelent = where(Mdiff eq min(Mdiff)) ;  the entry of the model closest in mass to object
   for kk=0,2 do begin ; drawing best mass model and two adjecent models
      Plotent = Mmodelent+kk-1
      if Plotent ge 0 and Plotent le n_elements(MMent)-1 then begin ; making sure there are models for the given mass
         modelmass = logM_M3(MMent(Plotent))  ;  mass of model (closest to object in mass)
         entries = where(logM_M3 eq modelmass(0))  ; entries for model mass closest to object in mass

         oplot,gr_M1(entries),gmag_M1(entries),linestyle=0,thick=thickall+2,col=col.green
         oplot,gr_M2(entries),gmag_M2(entries),linestyle=0,thick=thickall+2,col=col.red 
         oplot,gr_M3(entries),gmag_M3(entries),linestyle=0,thick=thickall+2,col=col.orange 

         ;XYOUTS,DX*0.95+XR[0],DY*(0.05+kk*0.05)+YR[0],'Solid Models: '+textoidl('log(M_{Model}/M_\odot)=')+trim(modelmass,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
      endif
   endfor

   ;by hand
   XYOUTS,DX*0.95+XR[0],DY*0.07+YR[0],textoidl('log(M_{Model}/M_{\odot})=9.0\pm0.1'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
   ; ================================


   NX = 30
   XX = findgen(NX)-15

   slope  = 1/(1-Qcat(ii).Abest)
   offset = Qcat(ii).Bbest/(1-Qcat(ii).Abest) 

   oplot,XX+mean(Xdat),slope*XX+mean(Ydat)+offset,linestyle=0,thick=thickall,col=col.blue

   ; the correct errors on the first object!
   ;Qcat(0).Bminus68 = 0.0113063     
   ;Qcat(0).Bplus68  = 0.00682601 
   ;Qcat(0).Aminus68 = 0.0542855
   ;Qcat(0).Aplus68  = 0.0516941 

   ;propagating errors
   Bplus68prop  = sqrt( slope^2.*Qcat(ii).Bplus68^2. + (1-Qcat(ii).Abest)*Qcat(ii).Bbest^2.*Qcat(ii).Aplus68^2. )
   Bminus68prop = sqrt( slope^2.*Qcat(ii).Bminus68^2.+ (1-Qcat(ii).Abest)*Qcat(ii).Bbest^2.*Qcat(ii).Aminus68^2. )
   Bplus95prop  = sqrt( slope^2.*Qcat(ii).Bplus95^2. + (1-Qcat(ii).Abest)*Qcat(ii).Bbest^2.*Qcat(ii).Aplus95^2. )
   Bminus95prop = sqrt( slope^2.*Qcat(ii).Bminus95^2.+ (1-Qcat(ii).Abest)*Qcat(ii).Bbest^2.*Qcat(ii).Aminus95^2. )
   Aplus68prop  = slope^2.*Qcat(ii).Aplus68
   Aminus68prop = slope^2.*Qcat(ii).Aminus68
   Aplus95prop  = slope^2.*Qcat(ii).Aplus95
   Aminus95prop = slope^2.*Qcat(ii).Aminus95

   ; overplotting 'error-lines'
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope+Aplus68prop) *XX(NX/2.:NX-1)+mean(Ydat)+(offset+Bplus68prop ),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(slope+Aplus68prop) *XX(0:NX/2.)   +mean(Ydat)+(offset-Bminus68prop),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(slope-Aminus68prop)*XX(0:NX/2.)   +mean(Ydat)+(offset+Bplus68prop ),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope-Aminus68prop)*XX(NX/2.:NX-1)+mean(Ydat)+(offset-Bminus68prop),linestyle=2,thick=thickall,col=col.blue

   ;oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope+Aplus95prop) *XX(NX/2.:NX-1)+mean(Ydat)+(offset+Bplus95prop ),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(0:NX/2.)   +mean(Xdat),(slope+Aplus95prop) *XX(0:NX/2.)   +mean(Ydat)+(offset-Bminus95prop),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(0:NX/2.)   +mean(Xdat),(slope-Aminus95prop)*XX(0:NX/2.)   +mean(Ydat)+(offset+Bplus95prop ),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope-Aminus95prop)*XX(NX/2.:NX-1)+mean(Ydat)+(offset-Bminus95prop),linestyle=3,thick=thickall,col=col.blue

   plotsym,0,1.5,/fill
   oplot,Xdat,Ydat,psym=8,col=col.red

   if PDF eq 1 then begin
      Pdat = PbadObj(objent) 
      OLs  = where(Pdat gt PbadMIN)   ; entries of probable outliers
      if OLs ne [-1] then begin
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.white   ; 'erasing' objects
         plotsym,0,1.5
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.red
      endif
   endif

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'SDSS J'+strtrim(Qcat(ii).sdssname,2),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'DR7 '+strtrim(Qcat(ii).headobjid,2),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('log(M_{BH}/M_\odot)=')+trim(Qcat(ii).LOGBH_MGII_MD04,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],textoidl('log(L_{bol}/(erg/s))=')+trim(Qcat(ii).LOGLBOL,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.75+YR[0],textoidl('z=')+trim(Qcat(ii).z,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.70+YR[0],"MCMC 'best' fit",col=col.blue,charsize=2.5,charthick=thickall

   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif
endfor
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 5) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
for ii=0,Nobjplot-1 do begin    ; looping over objects with shen data
   Objent = where(Qdata.headobjid eq Qcat(ii).headobjid)   ; entries for given object
   Xdat = Qdata(objent).psfmag_g
   Ydat = Qdata(objent).psfmag_g-Qdata(objent).psfmag_r

   ;= = = Models vs Object plot = = =
   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/modVSobj_g'+strtrim(ii,2)+'.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'Model vs object'
      thickall = 2
   endelse

   ; setting plot range
   XR = [median(Xdat)+1,median(Xdat)-1]
   YR = [median(Ydat)-1,median(Ydat)+1]
   ;XR = [max(Xdat)+0.1,min(Xdat)-0.1]
   ;YR = [min(Ydat)-0.1,max(Ydat)+0.1]
   XR = [20.4,19.5] ; cutout of object 1
   YR = [-0.1,0.65] ; cutout of object 1
   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]
   XT = STRMID(fit, 0, 1)

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,gmag_M1,rmag_M1, col=col.black    $
        , /NODATA $
        , xtitle =XT $
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
   ; ============ MODELS ============
   for jj=0,Nmass-1 do begin   ; looping over the individual masses
      entries = where(logM_M3 eq logM_M3(MMent(jj)))
;      oplot,gmag_M1(entries),gr_M1(entries),linestyle=1,thick=thickall,col=col.green
;      oplot,gmag_M2(entries),gr_M2(entries),linestyle=1,thick=thickall,col=col.red
;      oplot,gmag_M3(entries),gr_M3(entries),linestyle=1,thick=thickall,col=col.orange
      ;XYOUTS,min(gmag_M3(entries)),min(rmag_M3(entries)),textoidl('  log(M/M_\odot)=')+strtrim(logM_M3(MMent(jj)),2),col=col.black,charsize=1.0,charthick=thickall
   endfor

   ; over-plotting model with mass closest to Shen estimate in thick solid
   QSOmass = Qcat(ii).LOGBH_MGII_MD04
   if Qcat(ii).LOGBH_MGII_MD04 eq 0 then QSOmass = 8.5  ; if mass not estimated in shen catalog then set to 8.5
   Mdiff     = abs(logM_M1(MMent) - QSOmass)
   Mmodelent = where(Mdiff eq min(Mdiff)) ;  the entry of the model closest in mass to object
   for kk=0,2 do begin ; drawing best mass model and two adjecent models
      Plotent = Mmodelent+kk-1
      if Plotent ge 0 and Plotent le n_elements(MMent)-1 then begin ; making sure there are models for the given mass
         modelmass = logM_M3(MMent(Plotent))  ;  mass of model (closest to object in mass)
         entries = where(logM_M3 eq modelmass(0))  ; entries for model mass closest to object in mass

         oplot,gmag_M1(entries),gr_M1(entries),linestyle=0,thick=thickall+2,col=col.green
         oplot,gmag_M2(entries),gr_M2(entries),linestyle=0,thick=thickall+2,col=col.red 
         oplot,gmag_M3(entries),gr_M3(entries),linestyle=0,thick=thickall+2,col=col.orange 

            testmass = 1
            if testmass eq 1 then begin
               if kk eq 0 then begin
                  colstr = 'col.blue'
                  oplot,gmag_M1(entries),gr_M1(entries),linestyle=0,thick=thickall+2,col=col.blue
                  oplot,gmag_M2(entries),gr_M2(entries),linestyle=0,thick=thickall+2,col=col.blue
                  oplot,gmag_M3(entries),gr_M3(entries),linestyle=0,thick=thickall+2,col=col.blue
               endif
               if kk eq 1 then begin
                  colstr = 'col.cyan'
                  oplot,gmag_M1(entries),gr_M1(entries),linestyle=0,thick=thickall+2,col=col.cyan
                  oplot,gmag_M2(entries),gr_M2(entries),linestyle=0,thick=thickall+2,col=col.cyan
                  oplot,gmag_M3(entries),gr_M3(entries),linestyle=0,thick=thickall+2,col=col.cyan
               endif
               if kk eq 2 then begin
                  colstr = 'col.magenta'
                  oplot,gmag_M1(entries),gr_M1(entries),linestyle=0,thick=thickall+2,col=col.magenta
                  oplot,gmag_M2(entries),gr_M2(entries),linestyle=0,thick=thickall+2,col=col.magenta
                  oplot,gmag_M3(entries),gr_M3(entries),linestyle=0,thick=thickall+2,col=col.magenta
               endif

               print,'------------------------- Modelmass ',trim(modelmass),' = ',trim(colstr)

            endif


         ;XYOUTS,DX*0.95+XR[0],DY*(0.05+kk*0.05)+YR[0],'Solid Models: '+textoidl('log(M_{Model}/M_\odot)=')+trim(modelmass,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
      endif
   endfor

   ;by hand
   XYOUTS,DX*0.95+XR[0],DY*0.07+YR[0],textoidl('log(M_{Model}/M_{\odot})=9.0\pm0.1'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
   ; ================================

   XX = findgen(30)-15

   slope  = (1-Qcat(ii).Abest)
   offset = -Qcat(ii).Bbest

   oplot,XX+mean(Xdat),slope*XX+mean(Ydat)+offset,linestyle=0,thick=thickall,col=col.blue
   print,mean(Xdat),slope,mean(Ydat)+offset

   ; the correct errors on the first object!
   ;Qcat(0).Bminus68 = 0.0113063     
   ;Qcat(0).Bplus68  = 0.00682601 
   ;Qcat(0).Aminus68 = 0.0542855
   ;Qcat(0).Aplus68  = 0.0516941 

   ;propagating errors
   Bplus68prop  = Qcat(ii).Bplus68
   Bminus68prop = Qcat(ii).Bminus68
   Bplus95prop  = Qcat(ii).Bplus95
   Bminus95prop = Qcat(ii).Bminus95
   Aplus68prop  = Qcat(ii).Aplus68
   Aminus68prop = Qcat(ii).Aminus68
   Aplus95prop  = Qcat(ii).Aplus95
   Aminus95prop = Qcat(ii).Aminus95

   ; overplotting 'error-lines'
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope+Aplus68prop) *XX(NX/2.:NX-1)+mean(Ydat)+(offset+Bplus68prop ),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(slope+Aplus68prop) *XX(0:NX/2.)   +mean(Ydat)+(offset-Bminus68prop),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(slope-Aminus68prop)*XX(0:NX/2.)   +mean(Ydat)+(offset+Bplus68prop ),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope-Aminus68prop)*XX(NX/2.:NX-1)+mean(Ydat)+(offset-Bminus68prop),linestyle=2,thick=thickall,col=col.blue

   ;oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope+Aplus95prop) *XX(NX/2.:NX-1)+mean(Ydat)+(offset+Bplus95prop ),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(0:NX/2.)   +mean(Xdat),(slope+Aplus95prop) *XX(0:NX/2.)   +mean(Ydat)+(offset-Bminus95prop),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(0:NX/2.)   +mean(Xdat),(slope-Aminus95prop)*XX(0:NX/2.)   +mean(Ydat)+(offset+Bplus95prop ),linestyle=3,thick=thickall,col=col.blue
   ;oplot,XX(NX/2.:NX-1)+mean(Xdat),(slope-Aminus95prop)*XX(NX/2.:NX-1)+mean(Ydat)+(offset-Bminus95prop),linestyle=3,thick=thickall,col=col.blue

   plotsym,0,1.5,/fill
   oplot,Xdat,Ydat,psym=8,col=col.black

   if PDF eq 1 then begin
      Pdat = PbadObj(objent) 
      OLs  = where(Pdat gt PbadMIN)   ; entries of probable outliers
      if OLs ne [-1] then begin
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.white   ; 'erasing' objects
         plotsym,0,1.5
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.black
      endif
   endif

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'SDSS J'+strtrim(Qcat(ii).sdssname,2),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],textoidl('log(M_{BH}/M_\odot)=')+trim(Qcat(ii).LOGBH_MGII_MD04,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('log(L_{bol}/(erg/s))=')+trim(Qcat(ii).LOGLBOL,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],textoidl('z=')+trim(Qcat(ii).z,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.75+YR[0],"MCMC 'best' fit",col=col.blue,charsize=2.5,charthick=thickall
;   XYOUTS,DX*0.05+XR[0],DY*0.70+YR[0],'DR7 '+strtrim(Qcat(ii).headobjid,2),col=col.black,charsize=2.5,charthick=thickall

   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif
endfor
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 6) ne [-1] then begin
;=============================================================================================
mgALL = mean(Qdata.psfmag_g)
mrALL = mean(Qdata.psfmag_r)

;= = = ALL slopes = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/SlopedistMean'+strtrim(fit2)+'.eps' ; name of eps file
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Model vs object'
   thickall = 2
endelse

; setting plot range
XR = [mgALL+2,mgALL-2]
YR = [mrALL+3,mrALL-3]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,gmag_M1,rmag_M1, col=col.black    $
        , /NODATA $
        , xtitle ='<'+strmid(fit,0,1)+'>' $
        , ytitle ='<'+strmid(fit,1,1)+'>' $
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

;XX = findgen(100)/50-1
XX = findgen(3)-1  ; for fast running...
plotsym,0,0.5,/fill
for kk=0,NQSOs-1 do begin ; looping over objects with shen matches
;for kk=0,3-1 do begin
   ;oplot,XX+mgALL,(Qcat(kk).Abest*(XX)+Qcat(kk).Bbest)+(XX+mrall),psym=8,col=col.black
   oplot,XX+mgALL,(Qcat(kk).Abest*(XX)+Qcat(kk).Bbest)+(XX+mrall),col=col.gray,thick=thickall-3
   if kk eq 0 then begin
      scatterX = XX+mgALL
      scatterY = (Qcat(kk).Abest*(XX)+Qcat(kk).Bbest)+(XX+mrall)
   endif else begin
      scatterX = [scatterX,XX+mgALL]
      scatterY = [scatterY,(Qcat(kk).Abest*(XX)+Qcat(kk).Bbest)+(XX+mrall)]
   endelse
endfor

if CONT eq 1 then begin
   ;-- plotting contours --
   contourarray,scatterX,XR[1],XR[0],scatterY,YR[1],YR[0],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=[5.,50.,500.,5000.] $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif

oplot,XR,[mrALL,mrALL],thick=thickall,col=col.blue
oplot,[mgALL,mgALL],YR,thick=thickall,col=col.blue
oplot,findgen(50),findgen(50),thick=thickall,col=col.green

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 7) ne [-1] then begin
;=============================================================================================
zs = Qcat.z

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSz_CAT'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device  
   thickall = 6
endif else begin
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs redshift'
   thickall = 2
endelse
; setting plot range
XR = [min(zs)-0.1*(max(zs)-min(zs)),max(zs)+0.1*(max(zs)-min(zs))]
print,'redshift range used in plot 7: ',XR
;YR = [0,2]
YR = [-0.1,1.4]
if SM1 eq 1 then YR = YR - 1
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]


NNmin = 5

plot,zs,zs, col=col.black    $
        , /NODATA $
;        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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


POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,zs,zs, col=col.black    $
        , /NODATA,/noerase $
;        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}   \equiv   \deltam_r/\deltam_g - 1') $
        , ytitle =textoidl('Obs. quasar color variability  (\deltam_r/\deltam_g - 1)') $
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
; ------------------------------------------------------------------------------

XYOUTS,DX*0.95+XR[0],DY*0.30+YR[0],'Brighter = Bluer',col=col.darkgray,charsize=2.5,charthick=thickall,orientation=90

PLOTSYM,0,0.7,/FILL
oplot,zs,Qcat.abest,col=col.darkgray,psym=8,thick=thickall 
oplot,zs(sort(zs)),zs*0.0+aglobal,thick=thickall,col=col.black,linestyle=2

; making array with redshift bins
zarr = min(zs)+(findgen(NBIN+1)/(NBIN)*(max(zs)-min(zs)))
MeanBins = fltarr(2,NBIN)   ; array with mean centers and mean values

for kk=0,n_elements(zarr)-2 do begin ; looping over redshift bins
   zent = where(zs ge zarr(kk) AND zs lt zarr(kk+1),Czs)
   BinC = zarr(kk)+(zarr(kk+1)-zarr(kk))/2  ; center of bin
   MeanBins(0,kk) = BinC

   if zent ne [-1] and Czs gt Npointsmin then begin
      slopezbin = Qcat(zent).abest
      if Czs eq 1 then slopezbin = fltarr(1,1)+slopezbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopezsorted = slopezbin(sort(slopezbin))
      Mom = moment(slopezbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopezbin)
      ; 68% confidence level
      Slow68  = slopezsorted[0.16*Czs]
      Shigh68 = slopezsorted[0.84*Czs]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopezsorted[0.025*Czs]
      Shigh95 = slopezsorted[0.975*Czs]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopezbin)
      ; 68% confidence level
      Slow68_mean  = slopezsorted[0.16*Czs]
      Shigh68_mean = slopezsorted[0.84*Czs]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopezsorted[0.025*Czs]
      Shigh95_mean = slopezsorted[0.975*Czs]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      ;oploterror,xx+BinC,xx+slopemed,xx+sqrt(MOM(1)),psym=2, col=col.black, ERRCOLOR=col.red, thick=thickall  ; overplotting standard deviation of mean as error bar
      ;oplot,xx+BinC,xx+slopemed,psym=8,col=col.red  ; over plotting the data

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 
      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(Czs),psym=3, col=col.green, ERRCOLOR=col.green, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68m_mean,/LOBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean
      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68p_mean,/HIBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(Czs)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(Czs)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black

      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data
      ; saving mean values in vector
      MeanBins(1,kk) = Slopemean
   endif else begin
      MeanBins(1,kk) = aglobal
   endelse
endfor

slopemeanz = SPLINE(Meanbins(0,*),MeanBins(1,*),zs(sort(zs)))

zsSorted    = zs(sort(zs))
zminset     = 0.2
zmaxset     = 3.7
Zrangeent   = where(zsSorted gt zminset and zsSorted lt zmaxset)
SmeanZrange = slopemeanz(Zrangeent)
Zrange      = zsSorted(Zrangeent)
oplot,Zrange,SmeanZrange,col=col.red,thick=thickall

Dslope = aglobal-slopemeanz               ; slope redshift-correction term

acorrected0 = Qcat(sort(zs)).abest+Dslope  ; slopes corrected for z-wiggles/trend

save,acorrected0,filename='acorrect_'+trim(fit)+'.sav'  ; saving the redshift corrected slopes 

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 8) ne [-1] then begin
;=============================================================================================
Ledd  = alog10(1.3)+38+Qcat.LOGBH_MGII_MD04
logLLedd = Qcat.LOGLBOL-Ledd  ; log(L_bol/L_edd)

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogLLedd_CAT'+strtrim(fit,2)+'.eps'
   if SS ne 0 then plot1 = 'epsplots/slopeVSlogLLedd_CAT'+strtrim(fit,2)+'_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax,2)+'zmin'+trim(Zmin,2)+'zmax'+trim(zmax,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs logLLedd'
   thickall = 2
endelse
; setting plot range
;XR = [min(logLLedd)-0.1*(max(logLLedd)-min(logLLedd)),max(logLLedd)+0.1*(max(logLLedd)-min(logLLedd))]
XR = [-2.0,0]
;YR = [0,2]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,logLLedd,logLLedd, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{bol}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,logLLedd,logLLedd, col=col.black    $
        , /NODATA, /noerase $
        , xtitle =textoidl('log(L_{bol}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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
; ------------------------------------------------------------------------------

if SS ne 0 then begin ; if a subsample is selected
   SUBent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range

   subAs    = Qcat(SUBent).abest
   logLLedd = logLLedd(SUBent)

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'=> Nobj     : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin
   subAs = Qcat.abest
endelse

PLOTSYM,0,0.70,/FILL
;oplot,logLLedd,Qcat.abest,col=col.gray,psym=8,thick=thickall 
oplot,logLLedd,subAs,col=col.darkgray,psym=8,thick=thickall 

; making array with redshift bins
;logLLeddarr = min(logLLedd)+(findgen(NBIN+1)/(NBIN)*(max(logLLedd)-min(logLLedd)))
logLLeddarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(logLLeddarr)-2 do begin ; looping over redshift bins
   logLLeddent = where(logLLedd ge logLLeddarr(kk) AND logLLedd lt logLLeddarr(kk+1),ClogLLedds)
   if logLLeddent ne [-1] and ClogLLedds gt Npointsmin then begin
      slopelogLLeddbin = Qcat(logLLeddent).abest
      if ClogLLedds eq 1 then slopelogLLeddbin = fltarr(1,1)+slopelogLLeddbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopelogLLeddsorted = slopelogLLeddbin(sort(slopelogLLeddbin))
      Mom = moment(slopelogLLeddbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopelogLLeddbin)
      ; 68% confidence level
      Slow68  = slopelogLLeddsorted[0.16*ClogLLedds]
      Shigh68 = slopelogLLeddsorted[0.84*ClogLLedds]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopelogLLeddsorted[0.025*ClogLLedds]
      Shigh95 = slopelogLLeddsorted[0.975*ClogLLedds]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopelogLLeddbin)
      ; 68% confidence level
      Slow68_mean  = slopelogLLeddsorted[0.16*ClogLLedds]
      Shigh68_mean = slopelogLLeddsorted[0.84*ClogLLedds]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopelogLLeddsorted[0.025*ClogLLedds]
      Shigh95_mean = slopelogLLeddsorted[0.975*ClogLLedds]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      BinC = logLLeddarr(kk)+(logLLeddarr(kk+1)-logLLeddarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      ;oploterror,xx+BinC,xx+slopemed,xx+sqrt(MOM(1)),psym=2, col=col.black, ERRCOLOR=col.red, thick=thickall  ; overplotting standard deviation of mean as error bar
      ;oplot,xx+BinC,xx+slopemed,psym=8,col=col.red  ; over plotting the data

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 
      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(ClogLLedds),psym=3, col=col.green, ERRCOLOR=col.green, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68m_mean,/LOBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean
      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68p_mean,/HIBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(ClogLLedds)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(ClogLLedds)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black

      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 9) ne [-1] then begin
;=============================================================================================
Ledd  = alog10(1.3)+38+Qcat.LOGBH_MGII_MD04
logLLedd = Qcat.LOGLBOL-Ledd  ; log(L_bol/L_edd)
LLsort = logLLedd(sort(zs))

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogLLedd_CAT'+strtrim(fit,2)+'zcorrected.eps'
   if SS ne 0 then plot1 = 'epsplots/slopeVSlogLLedd_CAT'+strtrim(fit,2)+'zcorrected_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs LL corrected for z-dependence'
   thickall = 2
endelse
; setting plot range
;XR = [min(logLLedd)-0.1*(max(logLLedd)-min(logLLedd)),max(logLLedd)+0.1*(max(logLLedd)-min(logLLedd))]
XR = [-2.0,0]
;YR = [0,2]
YR = [0.25,1.25]
if SM1 eq 1 then YR = YR - 1
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,LLsort,LLsort, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{bol}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,LLsort,LLsort, col=col.black    $
        , /NODATA,/noerase $
        , xtitle =textoidl('log(L_{bol}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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
; ------------------------------------------------------------------------------


if SS ne 0 then begin ; if a subsample is selected
   SUBent = where(Qcat(sort(zs)).LOGBH_MGII_MD04 gt MassMin and Qcat(sort(zs)).LOGBH_MGII_MD04 lt MassMax and zs(sort(zs)) lt Zmax and zs(sort(zs)) gt Zmin, Cmassobj)  ; selecting mass range

   acorrected    = acorrected0(SUBent)
   LLsort        = LLsort(SUBent)

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'=> Nobj     : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin
   acorrected    = acorrected0
endelse


PLOTSYM,0,0.7,/FILL
oplot,LLsort,acorrected,col=col.darkgray,psym=8,thick=thickall 

; making array with redshift bins
;LLsortarr = min(LLsort)+(findgen(NBIN+1)/(NBIN)*(max(LLsort)-min(LLsort)))
LLsortarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(LLsortarr)-2 do begin ; looping over redshift bins
   LLsortent = where(LLsort ge LLsortarr(kk) AND LLsort lt LLsortarr(kk+1),CLLsorts)
   if LLsortent ne [-1] and CLLsorts gt Npointsmin then begin
      slopeLLsortbin = acorrected(LLsortent)
      if CLLsorts eq 1 then slopeLLsortbin = fltarr(1,1)+slopeLLsortbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopeLLsortsorted = slopeLLsortbin(sort(slopeLLsortbin))
      Mom = moment(slopeLLsortbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopeLLsortbin)
      ; 68% confidence level
      Slow68  = slopeLLsortsorted[0.16*CLLsorts]
      Shigh68 = slopeLLsortsorted[0.84*CLLsorts]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopeLLsortsorted[0.025*CLLsorts]
      Shigh95 = slopeLLsortsorted[0.975*CLLsorts]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopeLLsortbin)
      ; 68% confidence level
      Slow68_mean  = slopeLLsortsorted[0.16*CLLsorts]
      Shigh68_mean = slopeLLsortsorted[0.84*CLLsorts]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopeLLsortsorted[0.025*CLLsorts]
      Shigh95_mean = slopeLLsortsorted[0.975*CLLsorts]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      BinC = LLsortarr(kk)+(LLsortarr(kk+1)-LLsortarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      ;oploterror,xx+BinC,xx+slopemed,xx+sqrt(MOM(1)),psym=2, col=col.black, ERRCOLOR=col.red, thick=thickall  ; overplotting standard deviation of mean as error bar
      ;oplot,xx+BinC,xx+slopemed,psym=8,col=col.red  ; over plotting the data

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 
      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(CLLsorts),psym=3, col=col.green, ERRCOLOR=col.green, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68m_mean,/LOBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean
      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68p_mean,/HIBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(CLLsorts)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(CLLsorts)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black

      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 10) ne [-1] then begin
;=============================================================================================
logMMsun = Qcat.LOGBH_MGII_MD04

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogMMsun_CAT'+strtrim(fit,2)+'.eps'
   if SS ne 0 then plot1 = 'epsplots/slopeVSlogMMsun_CAT'+strtrim(fit,2)+'_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs logMMsun'
   thickall = 2
endelse
; setting plot range
;XR = [min(logMMsun)-0.1*(max(logMMsun)-min(logMMsun)),max(logMMsun)+0.1*(max(logMMsun)-min(logMMsun))]
XR = [6.6,11.0]
;YR = [0,2]
YR = [0.25,1.25]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,logMMsun,logMMsun, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(M_{BH}/M_\odot)') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,logMMsun,logMMsun, col=col.black    $
        , /NODATA, /noerase $
        , xtitle =textoidl('log(M_{BH}/M_\odot)') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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
; ------------------------------------------------------------------------------


if SS ne 0 then begin ; if a subsample is selected
   SUBent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range

   subAs         = Qcat(SUBent).abest
   logMMsun      = logMMsun(SUBent)

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'=> Nobj     : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin
   subAs = Qcat.abest
endelse

PLOTSYM,0,0.7,/FILL
oplot,logMMsun,subAs,col=col.darkgray,psym=8,thick=thickall 

; making array with redshift bins
;logMMsunarr = min(logMMsun)+(findgen(NBIN+1)/(NBIN)*(max(logMMsun)-min(logMMsun)))
logMMsunarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(logMMsunarr)-2 do begin ; looping over redshift bins
   logMMsunent = where(logMMsun ge logMMsunarr(kk) AND logMMsun lt logMMsunarr(kk+1),ClogMMsuns)
   if logMMsunent ne [-1] and ClogMMsuns gt Npointsmin then begin
      slopelogMMsunbin = Qcat(logMMsunent).abest
      if ClogMMsuns eq 1 then slopelogMMsunbin = fltarr(1,1)+slopelogMMsunbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopelogMMsunsorted = slopelogMMsunbin(sort(slopelogMMsunbin))
      Mom = moment(slopelogMMsunbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopelogMMsunbin)
      ; 68% confidence level
      Slow68  = slopelogMMsunsorted[0.16*ClogMMsuns]
      Shigh68 = slopelogMMsunsorted[0.84*ClogMMsuns]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopelogMMsunsorted[0.025*ClogMMsuns]
      Shigh95 = slopelogMMsunsorted[0.975*ClogMMsuns]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopelogMMsunbin)
      ; 68% confidence level
      Slow68_mean  = slopelogMMsunsorted[0.16*ClogMMsuns]
      Shigh68_mean = slopelogMMsunsorted[0.84*ClogMMsuns]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopelogMMsunsorted[0.025*ClogMMsuns]
      Shigh95_mean = slopelogMMsunsorted[0.975*ClogMMsuns]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      BinC = logMMsunarr(kk)+(logMMsunarr(kk+1)-logMMsunarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      ;oploterror,xx+BinC,xx+slopemed,xx+sqrt(MOM(1)),psym=2, col=col.black, ERRCOLOR=col.red, thick=thickall  ; overplotting standard deviation of mean as error bar
      ;oplot,xx+BinC,xx+slopemed,psym=8,col=col.red  ; over plotting the data

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 
      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(ClogMMsuns),psym=3, col=col.green, ERRCOLOR=col.green, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68m_mean,/LOBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean
      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68p_mean,/HIBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(ClogMMsuns)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(ClogMMsuns)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black

      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 11) ne [-1] then begin
;=============================================================================================
logMMsun = Qcat.LOGBH_MGII_MD04
MMsort = logMMsun(sort(zs))

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogMMsun_CAT'+strtrim(fit,2)+'zcorrected.eps'
   if SS ne 0 then plot1 = 'epsplots/slopeVSlogMMsun_CAT'+strtrim(fit,2)+'zcorrected_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs Mass corrected for z-dependence'
   thickall = 2
endelse
; setting plot range
;XR = [min(MMsort)-0.1*(max(MMsort)-min(MMsort)),max(MMsort)+0.1*(max(MMsort)-min(MMsort))]
XR = [6.6,11.0]
;YR = [0,2]
YR = [0.25,1.25]
if SM1 eq 1 then YR = YR - 1
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,MMsort,MMsort, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(M_{BH}/M_\odot)') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,MMsort,MMsort, col=col.black    $
        , /NODATA,/noerase $
        , xtitle =textoidl('log(M_{BH}/M_\odot)') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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
; ------------------------------------------------------------------------------


if SS ne 0 then begin ; if a subsample is selected
   SUBent = where(MMSort gt MassMin and MMSort lt MassMax and zs(sort(zs)) lt Zmax and zs(sort(zs)) gt Zmin, Cmassobj)  ; selecting mass range

   acorrected    = acorrected0(SUBent)
   MMsort        = MMsort(SUBent)

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'=> Nobj     : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin
   acorrected    = acorrected0
endelse

PLOTSYM,0,0.7,/FILL
oplot,MMsort,acorrected,col=col.darkgray,psym=8,thick=thickall 

; making array with redshift bins
;MMsortarr = min(MMsort)+(findgen(NBIN+1)/(NBIN)*(max(MMsort)-min(MMsort)))
MMsortarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(MMsortarr)-2 do begin ; looping over redshift bins
   MMsortent = where(MMsort ge MMsortarr(kk) AND MMsort lt MMsortarr(kk+1),CMMsorts)
   if MMsortent ne [-1] and CMMsorts gt Npointsmin then begin
      slopeMMsortbin = acorrected(MMsortent)
      if CMMsorts eq 1 then slopeMMsortbin = fltarr(1,1)+slopeMMsortbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopeMMsortsorted = slopeMMsortbin(sort(slopeMMsortbin))
      Mom = moment(slopeMMsortbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopeMMsortbin)
      ; 68% confidence level
      Slow68  = slopeMMsortsorted[0.16*CMMsorts]
      Shigh68 = slopeMMsortsorted[0.84*CMMsorts]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopeMMsortsorted[0.025*CMMsorts]
      Shigh95 = slopeMMsortsorted[0.975*CMMsorts]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopeMMsortbin)
      ; 68% confidence level
      Slow68_mean  = slopeMMsortsorted[0.16*CMMsorts]
      Shigh68_mean = slopeMMsortsorted[0.84*CMMsorts]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopeMMsortsorted[0.025*CMMsorts]
      Shigh95_mean = slopeMMsortsorted[0.975*CMMsorts]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      BinC = MMsortarr(kk)+(MMsortarr(kk+1)-MMsortarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      ;oploterror,xx+BinC,xx+slopemed,xx+sqrt(MOM(1)),psym=2, col=col.black, ERRCOLOR=col.red, thick=thickall  ; overplotting standard deviation of mean as error bar
      ;oplot,xx+BinC,xx+slopemed,psym=8,col=col.red  ; over plotting the data

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 
      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(CMMsorts),psym=3, col=col.green, ERRCOLOR=col.green, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68m_mean,/LOBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean
      ;oploterror,xx+BinC,xx+slopemean,xx+Serr68p_mean,/HIBAR,errcol=col.blue,errthick=thickall,psym=2,col=col.blue,HATLENGTH=2*!D.X_VSIZE / 100 ; 68% confidence on mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(CMMsorts)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(CMMsorts)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black

      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 12) ne [-1] then begin
;=============================================================================================
Ledd  = alog10(1.3)+38+Qcat.LOGBH_MGII_MD04
logL5100Ledd = Qcat.LOGL5100-Ledd  ; log(L_bol/L_edd)

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogL5100Ledd_CAT'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs logL5100'
   thickall = 2
endelse
; setting plot range
;XR = [min(logL5100Ledd)-0.1*(max(logL5100Ledd)-min(logL5100Ledd)),max(logL5100Ledd)+0.1*(max(logL5100Ledd)-min(logL5100Ledd))]
XR = [-4.0,-0.5]
YR = [0,2]

NNmin = 5

plot,logL5100Ledd,logL5100Ledd, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{5100}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

PLOTSYM,0,1.0,/FILL
oplot,logL5100Ledd,Qcat.abest,col=col.gray,psym=8,thick=thickall 

; making array with redshift bins
;logL5100Leddarr = min(logL5100Ledd)+(findgen(NBIN+1)/(NBIN)*(max(logL5100Ledd)-min(logL5100Ledd)))
logL5100Leddarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(logL5100Leddarr)-2 do begin ; looping over redshift bins
   logL5100Leddent = where(logL5100Ledd ge logL5100Leddarr(kk) AND logL5100Ledd lt logL5100Leddarr(kk+1),ClogL5100Ledd)
   if logL5100Leddent ne [-1] then begin
      slopelogL5100Leddbin = Qcat(logL5100Leddent).abest
      if ClogL5100Ledd eq 1 then slopelogL5100Leddbin = fltarr(1,1)+slopelogL5100Leddbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopelogL5100Leddsorted = slopelogL5100Leddbin(sort(slopelogL5100Leddbin))
      slopemed = median(slopelogL5100Leddbin)
      ; 68% confidence level
      Slow68  = slopelogL5100Leddsorted[0.16*ClogL5100Ledd]
      Shigh68 = slopelogL5100Leddsorted[0.84*ClogL5100Ledd]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopelogL5100Leddsorted[0.025*ClogL5100Ledd]
      Shigh95 = slopelogL5100Leddsorted[0.975*ClogL5100Ledd]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95
      ; standard deviation of mean
      Sstnd   = total( 1./n_elements(slopelogL5100Leddbin) * sqrt((slopelogL5100Leddbin-slopemed)^2) )

      BinC = logL5100Leddarr(kk)+(logL5100Leddarr(kk+1)-logL5100Leddarr(kk))/2

      PLOTSYM,8,2.,/FILL
      oplot,logL5100Ledd(logL5100Leddent)*0+BinC,Qcat.abest*0+slopemed,psym=8,col=col.black

      oploterror,logL5100Ledd(logL5100Leddent)*0+BinC,Qcat.abest*0+slopemed,slopelogL5100Leddsorted*0+Sstnd,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 13) ne [-1] then begin
;=============================================================================================
Ledd  = alog10(1.3)+38+Qcat.LOGBH_MGII_MD04
logL3000Ledd = Qcat.LOGL3000-Ledd  ; log(L_bol/L_edd)

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogL3000Ledd_CAT'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs logL3000'
   thickall = 2
endelse
; setting plot range
;XR = [min(logL3000Ledd)-0.1*(max(logL3000Ledd)-min(logL3000Ledd)),max(logL3000Ledd)+0.1*(max(logL3000Ledd)-min(logL3000Ledd))]
XR = [-4.0,0]
YR = [0,2]

NNmin = 5

plot,logL3000Ledd,logL3000Ledd, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{3000}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

PLOTSYM,0,1.0,/FILL
oplot,logL3000Ledd,Qcat.abest,col=col.gray,psym=8,thick=thickall 

; making array with redshift bins
;logL3000Leddarr = min(logL3000Ledd)+(findgen(NBIN+1)/(NBIN)*(max(logL3000Ledd)-min(logL3000Ledd)))
logL3000Leddarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(logL3000Leddarr)-2 do begin ; looping over redshift bins
   logL3000Leddent = where(logL3000Ledd ge logL3000Leddarr(kk) AND logL3000Ledd lt logL3000Leddarr(kk+1),ClogL3000Ledd)
   if logL3000Leddent ne [-1] then begin
      slopelogL3000Leddbin = Qcat(logL3000Leddent).abest
      if ClogL3000Ledd eq 1 then slopelogL3000Leddbin = fltarr(1,1)+slopelogL3000Leddbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopelogL3000Leddsorted = slopelogL3000Leddbin(sort(slopelogL3000Leddbin))
      slopemed = median(slopelogL3000Leddbin)
      ; 68% confidence level
      Slow68  = slopelogL3000Leddsorted[0.16*ClogL3000Ledd]
      Shigh68 = slopelogL3000Leddsorted[0.84*ClogL3000Ledd]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopelogL3000Leddsorted[0.025*ClogL3000Ledd]
      Shigh95 = slopelogL3000Leddsorted[0.975*ClogL3000Ledd]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95
      ; standard deviation of mean
      Sstnd   = total( 1./n_elements(slopelogL3000Leddbin) * sqrt((slopelogL3000Leddbin-slopemed)^2) )

      BinC = logL3000Leddarr(kk)+(logL3000Leddarr(kk+1)-logL3000Leddarr(kk))/2

      PLOTSYM,8,2.,/FILL
      oplot,logL3000Ledd(logL3000Leddent)*0+BinC,Qcat.abest*0+slopemed,psym=8,col=col.black

      oploterror,logL3000Ledd(logL3000Leddent)*0+BinC,Qcat.abest*0+slopemed,slopelogL3000Leddsorted*0+Sstnd,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 14) ne [-1] then begin
;=============================================================================================
Ledd  = alog10(1.3)+38+Qcat.LOGBH_MGII_MD04
logL1350Ledd = Qcat.LOGL1350-Ledd  ; log(L_bol/L_edd)

; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSlogL1350Ledd_CAT'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs logL1350'
   thickall = 2
endelse
; setting plot range
;XR = [min(logL1350Ledd)-0.1*(max(logL1350Ledd)-min(logL1350Ledd)),max(logL1350Ledd)+0.1*(max(logL1350Ledd)-min(logL1350Ledd))]
XR = [-3.0,0]
YR = [0,2]

NNmin = 5

plot,logL1350Ledd,logL1350Ledd, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('log(L_{1350}/L_{edd})') $
        , ytitle =textoidl('s_{'+strtrim(FIT,2)+'}') $
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

PLOTSYM,0,1.0,/FILL
oplot,logL1350Ledd,Qcat.abest,col=col.gray,psym=8,thick=thickall 

; making array with redshift bins
;logL1350Leddarr = min(logL1350Ledd)+(findgen(NBIN+1)/(NBIN)*(max(logL1350Ledd)-min(logL1350Ledd)))
logL1350Leddarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

for kk=0,n_elements(logL1350Leddarr)-2 do begin ; looping over redshift bins
   logL1350Leddent = where(logL1350Ledd ge logL1350Leddarr(kk) AND logL1350Ledd lt logL1350Leddarr(kk+1),ClogL1350Ledd)
   if logL1350Leddent ne [-1] then begin
      slopelogL1350Leddbin = Qcat(logL1350Leddent).abest
      if ClogL1350Ledd eq 1 then slopelogL1350Leddbin = fltarr(1,1)+slopelogL1350Leddbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopelogL1350Leddsorted = slopelogL1350Leddbin(sort(slopelogL1350Leddbin))
      slopemed = median(slopelogL1350Leddbin)
      ; 68% confidence level
      Slow68  = slopelogL1350Leddsorted[0.16*ClogL1350Ledd]
      Shigh68 = slopelogL1350Leddsorted[0.84*ClogL1350Ledd]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopelogL1350Leddsorted[0.025*ClogL1350Ledd]
      Shigh95 = slopelogL1350Leddsorted[0.975*ClogL1350Ledd]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95
      ; standard deviation of mean
      Sstnd   = total( 1./n_elements(slopelogL1350Leddbin) * sqrt((slopelogL1350Leddbin-slopemed)^2) )

      BinC = logL1350Leddarr(kk)+(logL1350Leddarr(kk+1)-logL1350Leddarr(kk))/2

      PLOTSYM,8,2.,/FILL
      oplot,logL1350Ledd(logL1350Leddent)*0+BinC,Qcat.abest*0+slopemed,psym=8,col=col.black

      oploterror,logL1350Ledd(logL1350Leddent)*0+BinC,Qcat.abest*0+slopemed,slopelogL1350Leddsorted*0+Sstnd,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall
   endif
endfor

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 15) ne [-1] then begin
;=============================================================================================
Ledd     = alog10(1.3)+38+Qcat.LOGBH_MGII_MD04
logLLedd = Qcat.LOGLBOL-Ledd  ; log(L_bol/L_edd)
logMMsun = Qcat.LOGBH_MGII_MD04

; = = = LLedd VS M space = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/LvsMspace'+strtrim(fit,2)+'.eps'
   Xw = 25 & Yw = 25
   device,  file=plot1 ,/color , /encapsulated, xsize=Xw, ysize=Yw, BITS=8,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   Xw = 600 & Yw = 600
   window, Nw, xsize=Xw, ysize=Yw, title = 'L vs M'
   thickall = 2
endelse
; setting plot range
XR = [7.5,9.8]
YR = [-1.8,0.05]
CHSZall = 2.5

; position of TVimage array
Xminpos = 0.15
Yminpos = 0.12
Xmaxpos = 0.95
Ymaxpos = 0.83
TVP = [Xminpos,Yminpos,Xmaxpos,Ymaxpos]
POSPLOT = TVP

Lbins = Nbin
Mbins = Nbin
DMsun = abs(XR[1]-XR[0])/Mbins
DLbol = abs(YR[1]-YR[0])/Lbins
Aarr0 = FLTARR(Mbins,Lbins)     ; array for mean a values for colors
Carr  = FLTARR(Mbins,Lbins)     ; array for number of objects in each bin

for ii=0,Lbins-1 do begin
   for jj=0,Mbins-1 do begin
      aent = where(logLLedd gt YR[0]+ii*DLbol and logLLedd lt YR[0]+(ii+1)*DLbol and logMMsun gt XR[0]+jj*DMsun and logMMsun lt XR[0]+(jj+1)*DMsun, Counta)
      ; drawing all points
      ;if Counta eq 0 then Aarr0(ii,jj) = 9999.
      ;if Counta eq 1 then Aarr0(ii,jj) = Qcat(Aent).abest
      ;if Counta gt 1 then Aarr0(ii,jj) = median(Qcat(Aent).abest)

      if Counta le Npointsmin then Aarr0(jj,ii) = 9999.
      if Counta gt Npointsmin then begin 
         Aarr0(jj,ii) = median(Qcat(Aent).abest) 
         Carr(jj,ii)  = Counta                    ;    storing number of objects in bin
      endif
   endfor
endfor

Showmin  = 0.1  ; the minimum value to show in plot -> to get sensible color bar
Showmax  = 2.0  ; the maximum value to show in plot -> to get sensible color bar

if SM1 eq 1 then Showmin = Showmin - 1
if SM1 eq 1 then Showmax = Showmax - 1

maxval = max(Aarr0(where(Aarr0 ne 9999 and Aarr0 lt showmax)))   ; finding the maximum value of array (making sure that pixels with median gt 2 are ignored as well)
minval = min(Aarr0(where(Aarr0 ne 0    and Aarr0 gt showmin)))   ; finding the maximum value of array
Abyte  = BYTSCL(Aarr0,MIN=minVal,MAX=MaxVal)   ; image turned into bytscale for plotting

; --- saving parameters ---
saveent = where(logMMsun gt XR[0] and logMMsun lt XR[1] and logLLedd gt YR[0] and logLLedd lt YR[1])
xvec = logMMsun(saveent)
yvec = logLLedd(saveent)
save,xvec,yvec,Aarr0,filename='array.sav'
;restore,'array.sav'  restoring variables
; -------------------------

plot,logLLedd,logMMsun, /NODATA, background=col.white,col=col.white  ; creating all white window

device, decomposed=0     ; tip before loading color scheme
sauron_colormap         ; loading the sauron color map   - can also use LOADCT,39

; --- Making the last color white in previously loaded color scheme ---
TVLCT, Rvec, Gvec, Bvec, /GET   ; getting red, green and blue color vectors
Rvec[255] = 255                 ; Changing last red color
Gvec[255] = 255                 ; Changing last green color
Bvec[255] = 255                 ; Changing last blue color
TVLCT, Rvec, Gvec, Bvec         ; Setting the edited vectors as colors
; ---------------------------------------------------------------------

if SMO ne 0 then begin
   goodent = where(Abyte lt max(Abyte))              ; entries with actual values
   WHTARR  = Carr/max(Carr) ; array with weights
   ;WHTARR(where(Carr ne 0))  = 1 
   ;imgsmoothing2D,Abyte,SMOOTH,ABYTEsmooth,/VERBOSE,WEIGHTARR=WHTARR
   imgsmoothingGWEIGHT,Abyte,SMOOTH,WHTARR,ABYTEsmooth,/VERBOSE
   Abyte(goodent) = AbyteSmooth(goodent)             ; only using smoothing from good values
endif

tvimage,Abyte,POSITION=TVP,/nointerpolation;,/overplot,/keep_aspect_ratio
CLEGEND = textoidl('s_{'+strtrim(FIT,2)+'}')
colorbar,position=[Xminpos,Ymaxpos+0.06,Xmaxpos,Ymaxpos+0.12] $
    ,title=CLEGEND $
    , range=[Minval,Maxval] $
    , format='(F5.2)' $
    , charsize=CHSZall $
    , thick=thickall $
    , xthick = thickall $
    , ythick = thickall $
    , charthick=thickall $
    , color='BLACK'

device, decomposed=0     ; tip before loading color scheme
col=getcolor(/load)
plot,logMMsun,logLLedd, col=col.black  $
        , /NODATA $ 
        , /noerase $
        , xtitle =textoidl('log(M_{BH}/M_{\odot})') $
        , ytitle =textoidl('log(L_{bol}/L_{edd})') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CHSZall $
        , charthick = thickall $
        , psym = 3 $
        , yminor = 2 $   
        , position=POSPLOT $
        , background = col.white

;XYOUTS,DX*0.15+XR[0],DY*0.85+YR[0],textoidl('<s_{'+strtrim(FIT,2)+'}>'),col=col.black,charsize=4,charthick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 16) ne [-1] then begin
;=============================================================================================
LLsort = logLLedd(sort(zs))
MMsort = logMMsun(sort(zs))

; = = = LLedd VS M space = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/LvsMspace'+strtrim(fit,2)+'_zcorrected.eps'
   Xw = 25 & Yw = 25
   device,  file=plot1 ,/color , /encapsulated, xsize=Xw, ysize=Yw, BITS=8,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   Xw = 600 & Yw = 600
   window, Nw, xsize=Xw, ysize=Yw, title = 'L vs M z-corrected'
   thickall = 2
endelse
; setting plot range
XR = [7.5,9.8]
YR = [-1.8,0.05]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CHSZall = 2.5

; position of TVimage array
Xminpos = 0.15
Yminpos = 0.12
Xmaxpos = 0.95
Ymaxpos = 0.83
TVP = [Xminpos,Yminpos,Xmaxpos,Ymaxpos]
POSPLOT = TVP

Lbins = Nbin
Mbins = Nbin
DMsun = abs(XR[1]-XR[0])/Mbins
DLbol = abs(YR[1]-YR[0])/Lbins
Aarr0 = FLTARR(Mbins,Lbins)     ; array for mean a values for colors
Carr  = FLTARR(Mbins,Lbins)     ; array for number of objects in each bin

for ii=0,Lbins-1 do begin
   for jj=0,Mbins-1 do begin
      aent = where(LLsort gt YR[0]+ii*DLbol and LLsort lt YR[0]+(ii+1)*DLbol and MMsort gt XR[0]+jj*DMsun and MMsort lt XR[0]+(jj+1)*DMsun, Counta)
      if Counta le Npointsmin then Aarr0(jj,ii) = 9999.
      ;if Counta eq 1 then Aarr0(jj,ii) = Qcat(Aent).abest
      if Counta gt Npointsmin then begin 
         Aarr0(jj,ii) = median(acorrected0(Aent)) ;    median(Qcat(Aent).abest)
         Carr(jj,ii)  = Counta                    ;    storing number of objects in bin
      endif
   endfor
endfor

Showmin  = 0.1  ; the minimum value to show in plot -> to get sensible color bar
Showmax  = 2.0  ; the maximum value to show in plot -> to get sensible color bar
if SM1 eq 1 then Showmin = Showmin - 1
if SM1 eq 1 then Showmax = Showmax - 1

maxvalsort = max(Aarr0(where(Aarr0 ne 9999 and Aarr0 lt showmax)))   ; finding the maximum value of array (making sure that pixels with median gt 2 are ignored as well)
minvalsort = min(Aarr0(where(Aarr0 ne 0    and Aarr0 gt showmin)))   ; finding the maximum value of array
Abyte  = BYTSCL(Aarr0,MIN=minVal,MAX=MaxVal)   ; image turned into bytscale for plotting ; using same min and max vals as non-corrected version

; --- saving parameters ---
saveent = where(MMsort gt XR[0] and MMsort lt XR[1] and LLsort gt YR[0] and LLsort lt YR[1])
xvec = MMsort(saveent)
yvec = LLsort(saveent)
save,xvec,yvec,Aarr0,filename='array_zcorr.sav'
;restore,'array.sav'  restoring variables
; -------------------------

plot,LLsort,MMsort, /NODATA, background=col.white,col=col.white  ; creating all white window

device, decomposed=0     ; tip before loading color scheme
sauron_colormap         ; loading the sauron color map   - can also use LOADCT,39

; --- Making the last color white in previously loaded color scheme ---
TVLCT, Rvec, Gvec, Bvec, /GET   ; getting red, green and blue color vectors
Rvec[255] = 255                 ; Changing last red color
Gvec[255] = 255                 ; Changing last green color
Bvec[255] = 255                 ; Changing last blue color
TVLCT, Rvec, Gvec, Bvec         ; Setting the edited vectors as colors
; ---------------------------------------------------------------------

if SMO ne 0 then begin

   print,'The M bin size = ',trim(DMsun)
   print,'The L bin size = ',trim(DLbol)

   goodent = where(Abyte lt max(Abyte))              ; entries with actual values
   WHTARR  = Carr/max(Carr) ; array with weights
   ;WHTARR(where(Carr ne 0))  = 1 
   ;imgsmoothing2D,Abyte,SMOOTH,ABYTEsmooth,/VERBOSE,WEIGHTARR=WHTARR
   imgsmoothingGWEIGHT,Abyte,SMOOTH,WHTARR,ABYTEsmooth,/VERBOSE
   Abyte(goodent) = AbyteSmooth(goodent)             ; only using smoothing from good values

   ell = ELLIPSE([7.9,-1.55],SMOOTH/2.*[DMsun,DLbol],Npoints=50)  ; creating ellipse to represent smoothing kernel
endif

tvimage,Abyte,POSITION=TVP,/nointerpolation;,/overplot,/keep_aspect_ratio
CLEGEND = textoidl('s_{'+strtrim(FIT,2)+'}')
colorbar,position=[Xminpos,Ymaxpos+0.06,Xmaxpos,Ymaxpos+0.12] $
    ;,title=CLEGEND $
    , range=[Minval,Maxval] $
    , format='(F5.2)' $
    , charsize=CHSZall $
    , thick=thickall $
    , xthick = thickall $
    , ythick = thickall $
    , charthick=thickall $
    , color='BLACK'

device, decomposed=0     ; tip before loading color scheme
col=getcolor(/load)
plot,MMsort,LLsort, col=col.black  $
        , /NODATA,/noerase $
        , xtitle =textoidl('log(M_{BH}/M_{\odot})') $
        , ytitle =textoidl('log(L_{bol}/L_{edd})') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CHSZall $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $   
        , position=POSPLOT $
        , background = col.white

if SMO ne 0 then oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black  ; overplotting ellipse representing smoothing kernel
XYOUTS,DX*0.1+XR[0],DY*0.85+YR[0],textoidl('<s_{'+strtrim(FIT,2)+'}>'),col=col.black,charsize=4,charthick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 17) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/chi2_slopeVSgr.eps'
   if SS ne 0 then plot1 = 'epsplots/chi2_slopeVSgr_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'chi2 slope vs g-r'
   thickall = 2
endelse
; setting plot range
XR = [1e0,8e4]
YR = [-1.0,1.0]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

; --- plotting data in main window ---
plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\chi^2_{g-r}') $
        , ytitle =textoidl('\Delta')+textoidl('s_{gr}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize =cs $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , pos = [0.15,0.15,0.8,0.8] $
        , /xlog $
;        ,/ylog  $
        , background = col.white

if SS ne 0 then begin ; if a subsample is selected
   Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
   C2_gr    = C2_gr0(Dmassent,*)
   C2_slope = C2_slope0(Dmassent,*)
   MASSobj  = Qcat(Dmassent).LOGBH_MGII_MD04

   XYOUTS,DX*0.000005+XR[0],DY*0.92+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.84+YR[0],'z range        : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.76+YR[0],'Nobj           : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin                     ; if not use all values
   C2_gr    = C2_gr0
   C2_slope = C2_slope0
   MASSobj  = Qcat.LOGBH_MGII_MD04
endelse

;plotting data with mass as well as gr smaller than the model values on average (circle)
PLOTSYM,0,0.5,/fill
goodent = where(MASSobj ne 0 and C2_gr(*,1) lt 0)
if goodent ne [-1] then oplot,C2_gr(goodent,0),C2_slope(goodent,0),psym=8,thick=thickall,col=col.green
goodent = where(MASSobj ne 0 and C2_gr(*,3) lt 0)
if goodent ne [-1] then oplot,C2_gr(goodent,2),C2_slope(goodent,1),psym=8,thick=thickall,col=col.red
goodent = where(MASSobj ne 0 and C2_gr(*,5) lt 0)
if goodent ne [-1] then oplot,C2_gr(goodent,4),C2_slope(goodent,2),psym=8,thick=thickall,col=col.orange

;plotting data with mass as well as gr larger than the model values on average (star)
;PLOTSYM,3,0.5,/fill
goodent = where(MASSobj ne 0 and C2_gr(*,1) gt 0)
if goodent ne [-1] then oplot,C2_gr(goodent,0),C2_slope(goodent,0),psym=8,thick=thickall,col=col.green
goodent = where(MASSobj ne 0 and C2_gr(*,3) gt 0)
if goodent ne [-1] then oplot,C2_gr(goodent,2),C2_slope(goodent,1),psym=8,thick=thickall,col=col.red
goodent = where(MASSobj ne 0 and C2_gr(*,5) gt 0)
if goodent ne [-1] then oplot,C2_gr(goodent,4),C2_slope(goodent,2),psym=8,thick=thickall,col=col.orange

if CONT eq 1 then begin
   levall = [5.,25.,50.,1000.]
   ;-- plotting contours --
   scatterX = C2_gr(*,0)
   scatterY = C2_slope(*,0)
   contourarray,scatterX,XR[0],XR[1],scatterY,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=levall $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
   scatterX = C2_gr(*,2)
   scatterY = C2_slope(*,1)
   contourarray,scatterX,XR[0],XR[1],scatterY,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=levall $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
   scatterX = C2_gr(*,4)
   scatterY = C2_slope(*,2)
   contourarray,scatterX,XR[0],XR[1],scatterY,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=levall $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif

oplot,[0.0001,XR[1]],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

; --- X axis hist ---
binS = 0.1
grvals = C2_gr(where(C2_gr(*,0) gt XR[0] and C2_gr(*,0) lt XR[1]),0)
Lgr    = alog10(grvals)                                                     ; to get log bins easy
plothist,Lgr,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,Lgr,bin=binS,col=col.green, thick=thickall $
        , axiscolor=col.black $
        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , xrange=[alog10(XR[0]),alog10(XR[1])] , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , yticks=2 $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

grvals = C2_gr(where(C2_gr(*,2) gt XR[0] and C2_gr(*,2) lt XR[1]),2)
Lgr    = alog10(grvals)
plothist,Lgr,bin=binS,col=col.red,/overplot,thick=thickall

grvals2 = C2_gr(where(C2_gr(*,4) gt XR[0] and C2_gr(*,4) lt XR[1]),4)
Lgr2    = alog10(grvals2)
plothist,Lgr2,bin=binS,col=col.orange,/overplot,thick=thickall

; --- Y axis hist ---
binS = 0.05
slopes = C2_slope(where(C2_slope(*,0) gt YR[0] and C2_slope(*,0) lt YR[1]),0)
plothist,slopes,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,slopes,bin=binS,col=col.green, thick=thickall $
        , /rotate  $
        , axiscolor=col.black $
        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange=YR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , xticks=2 $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the y ticks
        , pos=[0.8,0.15,0.97,0.8]

slopes = C2_slope(where(C2_slope(*,1) gt -10 and C2_slope(*,1) lt 10),1)
plothist,slopes,bin=binS,col=col.red,/overplot,/rotate,thick=thickall

slopes = C2_slope(where(C2_slope(*,2) gt -10 and C2_slope(*,2) lt 10),2)
plothist,slopes,bin=binS,col=col.orange,/overplot,/rotate,thick=thickall

oplot,[0.0,max(yhist)+0.05*max(yhist)],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 18) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/chi2_gVSr.eps'
   if SS ne 0 then plot1 = 'epsplots/chi2_gVSr_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'chi 2 g vs r'
   thickall = 2
endelse
; setting plot range
XR = [10,8e5]
YR = [10,8e5]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

NNmin = 5

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\chi^2_{g}') $
        , ytitle =textoidl('\chi^2_{r}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog,/ylog $
        , pos = [0.15,0.15,0.8,0.8] $
        , background = col.white


if SS ne 0 then begin ; if a subsample is selected
   Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
   C2_gmag = C2_gmag0(Dmassent,*)
   C2_rmag = C2_rmag0(Dmassent,*)
   MASSobj = Qcat(Dmassent).LOGBH_MGII_MD04

   XYOUTS,DX*0.000005+XR[0],DY*0.50+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.25+YR[0],'z range      : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.12+YR[0],'=> Nobj     : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin                     ; if not use all values
   C2_gmag = C2_gmag0
   C2_rmag = C2_rmag0
   MASSobj = Qcat.LOGBH_MGII_MD04
endelse

;plotting data with mass as well as g and r smaller than the model values on average (circle)
PLOTSYM,0,0.5,/fill
goodent = where(MASSobj ne 0 and C2_gmag(*,1) lt 0 and C2_rmag(*,1) lt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,0),C2_rmag(goodent,0),psym=8,thick=thickall,col=col.green
goodent = where(MASSobj ne 0 and C2_gmag(*,3) lt 0 and C2_rmag(*,3) lt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,2),C2_rmag(goodent,2),psym=8,thick=thickall,col=col.red
goodent = where(MASSobj ne 0 and C2_gmag(*,5) lt 0 and C2_rmag(*,5) lt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,4),C2_rmag(goodent,4),psym=8,thick=thickall,col=col.orange

;plotting data with mass as well as g and r larger than the model values on average (star)
PLOTSYM,3,0.5,/fill
goodent = where(MASSobj ne 0 and C2_gmag(*,1) gt 0 and C2_rmag(*,1) gt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,0),C2_rmag(goodent,0),psym=8,thick=thickall,col=col.green
goodent = where(MASSobj ne 0 and C2_gmag(*,3) gt 0 and C2_rmag(*,3) gt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,2),C2_rmag(goodent,2),psym=8,thick=thickall,col=col.red
goodent = where(MASSobj ne 0 and C2_gmag(*,5) gt 0 and C2_rmag(*,5) gt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,4),C2_rmag(goodent,4),psym=8,thick=thickall,col=col.orange

;plotting data with mass as well as g larger and r smaller than the model values on average (triangle)
PLOTSYM,4,0.5,/fill
goodent = where(MASSobj ne 0 and C2_gmag(*,1) gt 0 and C2_rmag(*,1) lt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,0),C2_rmag(goodent,0),psym=8,thick=thickall,col=col.green
goodent = where(MASSobj ne 0 and C2_gmag(*,3) gt 0 and C2_rmag(*,3) lt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,2),C2_rmag(goodent,2),psym=8,thick=thickall,col=col.red
goodent = where(MASSobj ne 0 and C2_gmag(*,5) gt 0 and C2_rmag(*,5) lt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,4),C2_rmag(goodent,4),psym=8,thick=thickall,col=col.orange

;plotting data with mass as well as g smaller and r larger than the model values on average (square)
PLOTSYM,8,0.5,/fill
goodent = where(MASSobj ne 0 and C2_gmag(*,1) lt 0 and C2_rmag(*,1) gt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,0),C2_rmag(goodent,0),psym=8,thick=thickall,col=col.green
goodent = where(MASSobj ne 0 and C2_gmag(*,3) lt 0 and C2_rmag(*,3) gt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,2),C2_rmag(goodent,2),psym=8,thick=thickall,col=col.red
goodent = where(MASSobj ne 0 and C2_gmag(*,5) lt 0 and C2_rmag(*,5) gt 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,4),C2_rmag(goodent,4),psym=8,thick=thickall,col=col.orange


if CONT eq 1 then begin
   levall = [5.,25.,50.,100.]
   ;-- plotting contours --
   scatterX = C2_gmag(*,0)
   scatterY = C2_rmag(*,0)
   contourarray,scatterX,XR[0],XR[1],scatterY,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=levall $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
   scatterX = C2_gmag(*,2)
   scatterY = C2_rmag(*,2)
   contourarray,scatterX,XR[0],XR[1],scatterY,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=levall $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
   scatterX = C2_gmag(*,4)
   scatterY = C2_rmag(*,4)
   contourarray,scatterX,XR[0],XR[1],scatterY,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=levall $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif

; --- X axis hist ---
binS = 0.1
gmagvals = C2_gmag(where(C2_gmag(*,0) gt XR[0] and C2_gmag(*,0) lt XR[1]),0)
Lgmag    = alog10(gmagvals)                                                     ; to get log bins easy
plothist,Lgmag,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,Lgmag,bin=binS,col=col.green, thick=thickall $
        , axiscolor=col.black $
        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /ystyle $
        , xrange=[alog10(XR[0]),alog10(XR[1])] , /xstyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , yticks=2 $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

gmagvals = C2_gmag(where(C2_gmag(*,2) gt XR[0] and C2_gmag(*,2) lt XR[1]),2)
Lgmag    = alog10(gmagvals)
plothist,Lgmag,bin=binS,col=col.red,/overplot,thick=thickall

gmagvals = C2_gmag(where(C2_gmag(*,4) gt XR[0] and C2_gmag(*,4) lt XR[1]),4)
Lgmag    = alog10(gmagvals)
plothist,Lgmag,bin=binS,col=col.orange,/overplot,thick=thickall

; --- Y axis hist ---
binS = 0.1
rmagvals = C2_rmag(where(C2_rmag(*,0) gt YR[0] and C2_rmag(*,0) lt YR[1]),0)
Lrmag    = alog10(rmagvals)                                                     ; to get log bins easy
plothist,Lrmag,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,Lrmag,bin=binS,col=col.green, thick=thickall $
        , /rotate $
        , axiscolor=col.black $
        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange=[alog10(YR[0]),alog10(YR[1])] , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , xticks=2 $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.8,0.15,0.97,0.8]

rmagvals = C2_rmag(where(C2_rmag(*,2) gt YR[0] and C2_rmag(*,2) lt YR[1]),2)
Lrmag    = alog10(rmagvals)
plothist,Lrmag,bin=binS,col=col.red,/overplot,thick=thickall,/rotate

rmagvals = C2_rmag(where(C2_rmag(*,4) gt YR[0] and C2_rmag(*,4) lt YR[1]),4)
Lrmag    = alog10(rmagvals)
plothist,Lrmag,bin=binS,col=col.orange,/overplot,thick=thickall,/rotate


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif
if where(PLOTSELECT eq 19) ne [-1] then begin

savefile = 'dataforplot19movie.sav'
;QdataALL = QDATA(ObjentALL)
save,Qdata,Qcat,DATARRAY,acorrected0,IDS,filename=savefile
;restore,savefile  ;  restoring variables
stop
;=============================================================================================
Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/'+strmid(fit,1,1)+'VS'+strmid(fit,0,1)+'_lines_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = strmid(fit,1,1)+' vs '+strmid(fit,0,1)+' with lines'
   thickall = 2
endelse
; setting plot range
XR = [22,18]
YR = [22,18]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
XT = STRMID(fit, 0, 1)
YT = STRMID(fit, 1, 1)

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='<'+XT+'>' $
        , ytitle ='<'+YT+'>' $
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

   meansam_r    = fltarr(Cmassobj)
   meansam_g    = fltarr(Cmassobj)
   meansam_rerr = fltarr(Cmassobj)
   meansam_gerr = fltarr(Cmassobj)

;LOADCT,39
;device,decomposed=0
;BYTCOL   = BYTSCL(FINDGEN(Cmassobj))  ; array used to color datasets

for ii=0,Cmassobj-1 do begin         ; looping over selected objects
   Objent   = where(Qdata.headobjid eq IDS(Dmassent(ii)),Nent)

   if fit eq 'gr' then begin
      gDAT     = Qdata(Objent).psfmag_g
      rDAT     = Qdata(Objent).psfmag_r
      gDATERR  = Qdata(Objent).psfmagerr_g
      rDATERR  = Qdata(Objent).psfmagerr_r
   endif else if fit eq 'ui' then begin
      gDAT     = Qdata(Objent).psfmag_u
      rDAT     = Qdata(Objent).psfmag_i
      gDATERR  = Qdata(Objent).psfmagerr_u
      rDATERR  = Qdata(Objent).psfmagerr_i
   endif

   meansam_r(ii)    = mean(rDAT)                     ; storing mean value
   meansam_g(ii)    = mean(gDAT)                     ; storing mean value
   meansam_rerr(ii) = sqrt(total(rDATERR^2.)/Nent)   ; propagated errors ; stdev(rDAT) ; setting error as standard dev.
   meansam_gerr(ii) = sqrt(total(gDATERR^2.)/Nent)   ; propagated errors ; stdev(gDAT) ; setting error as standard dev.

   aobj     = acorrected0(Dmassent(ii)) ; Qcat(Dmassent(ii)).abest
   bobj     = Qcat(Dmassent(ii)).bbest

   Llength  = stdev(gDAT)                         ; desired length of line
   Dxline   = Llength/(1+aobj)                    ; the size of xaxis projection
   Dyline   = Llength*aobj/(1+aobj)               ; the size of yaxis projection
   bprime   = meansam_r(ii)-aobj*meansam_g(ii)+bobj               ; b' after rewriting r-<r> = a*(g-<g>)+b  to  r = a*g + b'
   xvecline = [meansam_g(ii)-Dxline/2.,meansam_g(ii)+Dxline/2.]   ; x component of line
   yvecline = [meansam_r(ii)-Dyline/2.,meansam_r(ii)+Dyline/2.]   ; y component of lien

   if ROUND(ii/10.) eq ii/10. then begin 
      oplot,xvecline,yvecline,thick=thickall,col=col.gray;,col=BYTCOL(ii)
      if ROUND(ii/300.) eq ii/300. and VB eq 1 then print,systime(0),' Object ',ii
   endif

   ; creating vector with all object entries used above
   if n_elements(ObjentALL) ne 0 then ObjentALL = [ObjentALL,Objent]  ; appending new objent values
   if n_elements(ObjentALL) eq 0 then ObjentALL = Objent  ; appending new objent values

endfor
col = getcolor(/load)

Xcross  = 20.3
GRscale = 0.3
;oplot,findgen(30),findgen(30)-GRscale,thick=thickall,linestyle=0,col=col.green
;XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],strmid(fit,1,1)+'='+strmid(fit,0,1)+'-'+trim(GRscale),col=col.green,charsize=2.5,charthick=thickall;,alignment=1.0

SAMa = mean(Qcat.Abest)
B1   = (Xcross-GRscale)-(SAMa*Xcross)   ; 5.1
;oplot,findgen(30),SAMa*findgen(30)+B1,thick=thickall,linestyle=0,col=col.blue
;XYOUTS,DX*0.95+XR[0],DY*0.15+YR[0],'Total sample (mean) line: '+strmid(fit,1,1)+'='+strtrim(SAMa,2)+'*'+strmid(fit,0,1)+'+'+strtrim(B1,2),col=col.blue,charsize=2.5,charthick=thickall,alignment=1.0

; bootstrapping to get errors on mean slope
Nbstraps = 1000
MeanBS   = fltarr(Nbstraps)
Avalsel  = acorrected0(Dmassent)  ; the Cmassobj values of A used
for pp=0,Nbstraps-1 do begin
   entrand    =  Round( RandomU(seed,Cmassobj)*Cmassobj ) ; Cmassobj entries randomly chosen (each object can be chosen multiple times) 
   MeanBS(pp) =  mean(Avalsel(entrand))
endfor
minSLOPE = min(meanBS)
maxSLOPE = max(meanBS)

medBIN = mean(acorrected0(Dmassent)) ; mean(Qcat(Dmassent).Abest)   ; median slope of selected mass and z bin
B2     = (Xcross-GRscale)-(medBIN*Xcross)  ; 3.1
;oplot,findgen(30),medBIN*findgen(30)+B2,thick=thickall,linestyle=0,col=col.red

LFITSAM = 1
if LFITSAM eq 1 then begin ; fitting a line to the (<r> vs. <g>) sample of selected QSOs 
   DATARRAY      = fltarr(Cmassobj,4)
   DATARRAY(*,0) = meansam_g(*)
   DATARRAY(*,1) = meansam_r(*)
   DATARRAY(*,2) = meansam_gerr(*)
   DATARRAY(*,3) = meansam_rerr(*)
   MCMCloops = 10000
   RESULTec = fltarr(14)

   if VB eq 1 then print,systime(0),' Running linearfitMCMC'
   linearfitMCMC,DATARRAY,RESULT,Nmcmc=MCMCloops,OUTPUT='PDFsampleMCMC.dat',/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT,ERRCORRECT=RESULTec
   if VB eq 1 then print,systime(0),' linearfitMCMC done'

   B3     = mean(meansam_r(*))-RESULT(0)*mean(meansam_g(*))+RESULT(7)
   ;oplot,findgen(30),RESULT(0)*findgen(30)+B3,thick=thickall,linestyle=0,col=col.black

   NX = 30
   XX = findgen(NX)-15
   oplot,XX+mean(meansam_g(*)),result(0)*XX+mean(meansam_r(*))+result(7),linestyle=0,thick=thickall,col=col.black

   ; overplotting 'error-lines' 
   oplot,XX(NX/2.:NX-1)+mean(meansam_g(*)),(RESULT(0)+RESULTec(4))*XX(NX/2.:NX-1)+mean(meansam_r(*))+(RESULT(7)+RESULTec(11)),linestyle=2,thick=thickall,col=col.black
   oplot,XX(0:NX/2.)   +mean(meansam_g(*)),(RESULT(0)+RESULTec(4))*XX(0:NX/2.) +mean(meansam_r(*))+(RESULT(7)-RESULTec(10)),linestyle=2,thick=thickall,col=col.black
   oplot,XX(0:NX/2.)   +mean(meansam_g(*)),(RESULT(0)-RESULTec(3))*XX(0:NX/2.) +mean(meansam_r(*))+(RESULT(7)+RESULTec(11)),linestyle=2,thick=thickall,col=col.black
   oplot,XX(NX/2.:NX-1)+mean(meansam_g(*)),(RESULT(0)-RESULTec(3))*XX(NX/2.:NX-1)+mean(meansam_r(*))+(RESULT(7)-RESULTec(10)),linestyle=2,thick=thickall,col=col.black

   ;XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'Sample MCMC fit: '+strmid(fit,1,1)+'='+trim(RESULT(0),'(F10.2)')+'*'+strmid(fit,0,1)+'+'+trim(B3,'(F10.2)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
   ;by hand (gr):
   XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'Sample MCMC fit: -0.01',col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
   ;by hand (ui):
   ;XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'Sample MCMC fit: -0.08',col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
endif

Astr = trim(medBIN,'(F10.2)') ; creating strings from result
Bstr = trim(B2,'(F10.2)') ; creating strings from result
;XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'Sample (mean) line: '+strmid(fit,1,1)+'='+Astr+'*'+strmid(fit,0,1)+'+'+Bstr,col=col.red,charsize=2.5,charthick=thickall,alignment=1.0


;XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'Mean of individual slopes: '+strmid(fit,1,1)+'='+Astr+'*'+strmid(fit,0,1)+'+C',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0
;by hand (gr):
XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'Mean of individual slopes: -0.18',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0
;by hand (ui):
;XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'Mean of individual slopes: -0.48',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0



NX = 30
XX = findgen(NX)-15
oplot,xx+mean(meansam_g(*)),medBIN  *xx+mean(meansam_r(*))+RESULT(10),thick=thickall,linestyle=0,col=col.red
oplot,xx+mean(meansam_g(*)),minSLOPE*xx+mean(meansam_r(*))+RESULT(10),thick=thickall,linestyle=2,col=col.red
oplot,xx+mean(meansam_g(*)),maxSLOPE*xx+mean(meansam_r(*))+RESULT(10),thick=thickall,linestyle=2,col=col.red

;XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'mass range  : ['+trim(MassMin)+':'+trim(MassMax)+']',col=col.black,charsize=2.5,charthick=thickall
;XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'z range      : ['+trim(Zmin)+':'+trim(Zmax)+']',col=col.black,charsize=2.5,charthick=thickall
;XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'=> Nobj     : '+trim(Cmassobj),col=col.black,charsize=2.5,charthick=thickall

;By hand (5th--95th percentile of all masses and redshifts):
;XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],textoidl('8.0 < log(M_{BH} < 9.5'),col=col.black,charsize=2.5,charthick=thickall
;XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],textoidl('0.5 < z < 2.1'),col=col.black,charsize=2.5,charthick=thickall
;XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('N_{obj} = ')+trim(Cmassobj),col=col.black,charsize=2.5,charthick=thickall

;By hand (33rd--66th percentile of masses and the 25th--75th percentile of redshifts):
XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],textoidl('8.6 < log(M_{BH}) < 9.0'),col=col.black,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],textoidl('1.0 < z < 1.75'),col=col.black,charsize=2.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('N_{obj} = ')+trim(Cmassobj),col=col.black,charsize=2.5,charthick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 20) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/chi_gVSchi2_g.eps'
   if SS ne 0 then plot1 = 'epsplots/chi_gVSchi2_g_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'chi g vs chi2 g'
   thickall = 2
endelse
; setting plot range
XR = [10,8e5]
YR = [-1.,1.]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\chi^2_{g}') $
        , ytitle ='D'+textoidl('_{g}') $
;        , ytitle ='!15D!3'+textoidl('_{g}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CS $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , pos = [0.15,0.15,0.8,0.8] $
        , background = col.white

if SS ne 0 then begin ; if a subsample is selected
   Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
   C2_gmag = C2_gmag0(Dmassent,*)
   MASSobj = Qcat(Dmassent).LOGBH_MGII_MD04

   XYOUTS,DX*0.000005+XR[0],DY*0.92+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.84+YR[0],'z range        : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.76+YR[0],'Nobj           : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin                     ; if not use all values
   C2_gmag = C2_gmag0
   MASSobj = Qcat.LOGBH_MGII_MD04
endelse

PLOTSYM,0,0.5,/fill
goodent = where(MASSobj ne 0)
if goodent ne [-1] then oplot,C2_gmag(goodent,0),C2_gmag(goodent,1),psym=8,thick=thickall,col=col.green
if goodent ne [-1] then oplot,C2_gmag(goodent,2),C2_gmag(goodent,3),psym=8,thick=thickall,col=col.red
if goodent ne [-1] then oplot,C2_gmag(goodent,4),C2_gmag(goodent,5),psym=8,thick=thickall,col=col.orange

oplot,[0.0001,XR[1]],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

; --- X axis hist ---
binS = 0.1
gmagvals = C2_gmag(where(C2_gmag(*,0) gt XR[0] and C2_gmag(*,0) lt XR[1]),0)
Lgmag    = alog10(gmagvals)                                                     ; to get log bins easy
plothist,Lgmag,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,Lgmag,bin=binS,col=col.green, thick=thickall $
        , axiscolor=col.black $
        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /ystyle $
        , xrange=[alog10(XR[0]),alog10(XR[1])] , /xstyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , yticks=2 $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

gmagvals = C2_gmag(where(C2_gmag(*,2) gt XR[0] and C2_gmag(*,2) lt XR[1]),2)
Lgmag    = alog10(gmagvals)
plothist,Lgmag,bin=binS,col=col.red,/overplot,thick=thickall

gmagvals = C2_gmag(where(C2_gmag(*,4) gt XR[0] and C2_gmag(*,4) lt XR[1]),4)
Lgmag    = alog10(gmagvals)
plothist,Lgmag,bin=binS,col=col.orange,/overplot,thick=thickall

; --- Y axis hist ---
binS = 0.05
chigvals = C2_gmag(where(C2_gmag(*,1) gt YR[0] and C2_gmag(*,1) lt XR[1]),1)
plothist,chigvals,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,chigvals,bin=binS,col=col.green, thick=thickall $
        , /rotate $
        , axiscolor=col.black $
        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange=YR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , xticks=2 $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.8,0.15,0.97,0.8]


chigvals = C2_gmag(where(C2_gmag(*,3) gt YR[0] and C2_gmag(*,3) lt XR[1]),3)
plothist,chigvals,bin=binS,col=col.red,/overplot,thick=thickall,/rotate

chigvals = C2_gmag(where(C2_gmag(*,5) gt YR[0] and C2_gmag(*,5) lt XR[1]),5)
plothist,chigvals,bin=binS,col=col.orange,/overplot,thick=thickall,/rotate

oplot,[0.0,max(yhist)+0.05*max(yhist)],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 21) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/chi_rVSchi2_r.eps'
   if SS ne 0 then plot1 = 'epsplots/chi_rVSchi2_r_Mmin'+trim(MassMin,2)+'Mmax'+trim(MassMax,2)+'zmin'+trim(Zmin,2)+'zmax'+trim(zmax,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'chi r vs chi2 r'
   thickall = 2
endelse
; setting plot range
XR = [10,8e5]
YR = [-1,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\chi^2_{r}') $
        , ytitle ='D'+textoidl('_{r}') $
;        , ytitle ='!15D!3'+textoidl('_{r}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CS $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , pos = [0.15,0.15,0.8,0.8] $
        , background = col.white

if SS ne 0 then begin ; if a subsample is selected
   Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
   C2_rmag = C2_rmag0(Dmassent,*)
   MASSobj = Qcat(Dmassent).LOGBH_MGII_MD04

   XYOUTS,DX*0.000005+XR[0],DY*0.92+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.84+YR[0],'z range        : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.76+YR[0],'Nobj           : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin                     ; if not use all values
   C2_rmag = C2_rmag0
   MASSobj = Qcat.LOGBH_MGII_MD04
endelse

PLOTSYM,0,0.5,/fill
goodent = where(MASSobj ne 0)
if goodent ne [-1] then oplot,C2_rmag(goodent,0),C2_rmag(goodent,1),psym=8,thick=thickall,col=col.green
if goodent ne [-1] then oplot,C2_rmag(goodent,2),C2_rmag(goodent,3),psym=8,thick=thickall,col=col.red
if goodent ne [-1] then oplot,C2_rmag(goodent,4),C2_rmag(goodent,5),psym=8,thick=thickall,col=col.orange

oplot,[0.0001,XR[1]],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

; --- X axis hist ---
binS = 0.1
rmagvals = C2_rmag(where(C2_rmag(*,0) gt XR[0] and C2_rmag(*,0) lt XR[1]),0)
Lrmag    = alog10(rmagvals)                                                     ; to get log bins easy
plothist,Lrmag,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,Lrmag,bin=binS,col=col.green, thick=thickall $
        , axiscolor=col.black $
        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /ystyle $
        , xrange=[alog10(XR[0]),alog10(XR[1])] , /xstyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , yticks=2 $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

rmagvals = C2_rmag(where(C2_rmag(*,2) gt XR[0] and C2_rmag(*,2) lt XR[1]),2)
Lrmag    = alog10(rmagvals)
plothist,Lrmag,bin=binS,col=col.red,/overplot,thick=thickall

rmagvals = C2_rmag(where(C2_rmag(*,4) gt XR[0] and C2_rmag(*,4) lt XR[1]),4)
Lrmag    = alog10(rmagvals)
plothist,Lrmag,bin=binS,col=col.orange,/overplot,thick=thickall

; --- Y axis hist ---
binS = 0.05
chirvals = C2_rmag(where(C2_rmag(*,1) gt YR[0] and C2_rmag(*,1) lt XR[1]),1)
plothist,chirvals,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,chirvals,bin=binS,col=col.green, thick=thickall $
        , /rotate $
        , axiscolor=col.black $
        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange=YR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , xticks=2 $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.8,0.15,0.97,0.8]


chirvals = C2_rmag(where(C2_rmag(*,3) gt YR[0] and C2_rmag(*,3) lt XR[1]),3)
plothist,chirvals,bin=binS,col=col.red,/overplot,thick=thickall,/rotate

chirvals = C2_rmag(where(C2_rmag(*,5) gt YR[0] and C2_rmag(*,5) lt XR[1]),5)
plothist,chirvals,bin=binS,col=col.orange,/overplot,thick=thickall,/rotate

oplot,[0.0,max(yhist)+0.05*max(yhist)],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 22) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/chi_grVSchi2_gr.eps'
   if SS ne 0 then plot1 = 'epsplots/chi_grVSchi2_gr_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'chi gr vs chi2 gr'
   thickall = 2
endelse
; setting plot range
XR = [10,8e5]
YR = [-1,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\chi^2_{g-r}') $
        , ytitle ='D'+textoidl('_{g-r}') $
;        , ytitle ='!15D!3'+textoidl('_{g-r}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CS $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , pos = [0.15,0.15,0.8,0.8] $
        , background = col.white

if SS ne 0 then begin ; if a subsample is selected
   Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
   C2_gr = C2_gr0(Dmassent,*)
   MASSobj = Qcat(Dmassent).LOGBH_MGII_MD04

   XYOUTS,DX*0.000005+XR[0],DY*0.92+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.84+YR[0],'z range        : ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.000005+XR[0],DY*0.76+YR[0],'Nobj           : '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
endif else begin                     ; if not use all values
   C2_gr = C2_gr0
   MASSobj = Qcat.LOGBH_MGII_MD04
endelse

PLOTSYM,0,0.5,/fill
goodent = where(MASSobj ne 0)
if goodent ne [-1] then oplot,C2_gr(goodent,0),C2_gr(goodent,1),psym=8,thick=thickall,col=col.green
if goodent ne [-1] then oplot,C2_gr(goodent,2),C2_gr(goodent,3),psym=8,thick=thickall,col=col.red
if goodent ne [-1] then oplot,C2_gr(goodent,4),C2_gr(goodent,5),psym=8,thick=thickall,col=col.orange

oplot,[0.0001,XR[1]],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

; --- X axis hist ---
binS = 0.1
grvals = C2_gr(where(C2_gr(*,0) gt XR[0] and C2_gr(*,0) lt XR[1]),0)
Lgr    = alog10(grvals)                                                     ; to get log bins easy
plothist,Lgr,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,Lgr,bin=binS,col=col.green, thick=thickall $
        , axiscolor=col.black $
        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /ystyle $
        , xrange=[alog10(XR[0]),alog10(XR[1])] , /xstyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , yticks=2 $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

grvals = C2_gr(where(C2_gr(*,2) gt XR[0] and C2_gr(*,2) lt XR[1]),2)
Lgr    = alog10(grvals)
plothist,Lgr,bin=binS,col=col.red,/overplot,thick=thickall

grvals = C2_gr(where(C2_gr(*,4) gt XR[0] and C2_gr(*,4) lt XR[1]),4)
Lgr    = alog10(grvals)
plothist,Lgr,bin=binS,col=col.orange,/overplot,thick=thickall

; --- Y axis hist ---
binS = 0.05
chigrvals = C2_gr(where(C2_gr(*,1) gt YR[0] and C2_gr(*,1) lt XR[1]),1)
plothist,chigrvals,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,chigrvals,bin=binS,col=col.green, thick=thickall $
        , /rotate $
        , axiscolor=col.black $
        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange=YR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , xticks=2 $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.8,0.15,0.97,0.8]

chigrvals = C2_gr(where(C2_gr(*,3) gt YR[0] and C2_gr(*,3) lt XR[1]),3)
plothist,chigrvals,bin=binS,col=col.red,/overplot,thick=thickall,/rotate

chigrvals = C2_gr(where(C2_gr(*,5) gt YR[0] and C2_gr(*,5) lt XR[1]),5)
plothist,chigrvals,bin=binS,col=col.orange,/overplot,thick=thickall,/rotate

oplot,[0.0,max(yhist)+0.05*max(yhist)],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 23) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSAprime'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS Aprime'
   thickall = 2
endelse
; setting plot range
XR = [0.018,1.]
YR = [0.1,1.4]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ="A'" $
;        , ytitle ='a' $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , background = col.white

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

;oplot,XR,fltarr(2)+1,thick=thickall,linestyle=2,col=col.black

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA,/noerase $
        , xtitle ="A'" $
;        , ytitle ='a' $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , /xlog $
        , background = col.white
; ------------------------------------------------------------------------------


Aprime = fltarr(NQSOs)
for ll=0,NQSOs-1 do begin
   Aprime(ll) = Qcat(ll).A*(1+Qcat(ll).z)^Qcat(ll).gamma
endfor

PLOTSYM,0,0.5,/FILL
oplot,Aprime,Qcat.abest,col=col.charcoal,psym=8,thick=thickall

;oplot,Qcat.A,Qcat.abest,col=col.red,psym=8,thick=thickall

if CONT eq 1 then begin
   ;-- plotting contours --
   contourarray,Aprime,XR[0],XR[1],Qcat.abest,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=[5.,15.,30.,70.] $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif

if n_elements(Fstars) eq 1 then begin  ; overplotting FGstars if requested
   readcol,'Fstarsobjid_z_Agamma.dat',FORMAT=('i,f,f,f'),FSobjid,FSz,FSa,FSgamma   ; reading the A and gamma of all 5000 FGstars
   readcol,'slopeinvestigation_output_FGstars_grfit110207.txt',FORMAT=(''),FSobjid2,FSabest,FSamed,FSamean,FSaplus68,FSaminus68,FSaplus95,FSaminus95,FSbbest,FSbmed,FSbmean,FSbplus68,FSbminus68,FSbplus95,FSbminus95,FSz,FSseason

   if SM1 eq 1 then FSabest = FSabest -1

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,FSa(0:499),FSabest,col=col.blue,psym=8,thick=thickall
   XYOUTS,DX*0.85+XR[0],DY*0.15+YR[0],'FG stars',col=col.blue,charsize=2.5,charthick=thickall,alignment=1.0
endif

if n_elements(RRL) eq 1 then begin  ; overplotting FGstars if requested
   readcol,'RRLsSesarobjid_z_Agamma.dat',FORMAT=('i,f,f,f'),RRLobjid,RRLz,RRLa,RRLgamma   ; reading the A and gamma of all 5000 FGstars
   readcol,'PDFs_RRL_grfit110215/slopeinvestigation_output_RRL_grfit110215.txt',FORMAT=(''),RRLobjid2,RRLabest,RRLamed,RRLamean,RRLaplus68,RRLaminus68,RRLaplus95,RRLaminus95,RRLbbest,RRLbmed,RRLbmean,RRLbplus68,RRLbminus68,RRLbplus95,RRLbminus95,RRLz,RRLseason

   if SM1 eq 1 then RRLabest = RRLabest -1

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,RRLa,RRLabest,col=col.red,psym=8,thick=thickall
   XYOUTS,DX*0.85+XR[0],DY*0.10+YR[0],'RR Lyrae',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0
endif

varmark = 1
if varmark eq 1 then begin
   Dmags       = [0.0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
   meanalinear = [0.451592,0.422081,0.757427,0.823645,0.883770,0.892704,0.991097,0.991593,0.963111,0.960074,0.936306,0.992285,1.00808,1.00537,1.01425,1.00451,1.00437,1.00327,0.982635]

   if SM1 eq 1 then meanalinear = meanalinear -1

   PLOTSYM,0,2.0,/FILL,thick=thickall
   oplot,Dmags,meanalinear,psym=8,col=col.green,thick=thickall
   PLOTSYM,0,2.0,thick=thickall
   oplot,Dmags,meanalinear,psym=8,col=col.black,thick=thickall

   XYOUTS,DX*0.85+XR[0],DY*0.05+YR[0],textoidl('synthetic data with s_{gr(in)} \equiv 0'),col=col.green,charsize=2.5,charthick=thickall,alignment=1.0
endif


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif


if where(PLOTSELECT eq 24) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSgamma'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS gamma'
   thickall = 2
endelse
; setting plot range
XR = [-0.05,1.25]
YR = [0.1,1.4]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
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

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

;oplot,XR,fltarr(2)+1,thick=thickall,linestyle=2,col=col.black

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA,/noerase $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
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
; ------------------------------------------------------------------------------


PLOTSYM,0,0.5,/FILL
oplot,Qcat.gamma,Qcat.abest,col=col.charcoal,psym=8,thick=thickall

if CONT eq 1 then begin
   ;-- plotting contours --
   contourarray,Qcat.gamma,XR[0],XR[1],Qcat.abest,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=[5.,15.,30.,70.] $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif

if n_elements(Fstars) eq 1 then begin  ; overplotting FGstars if requested
   readcol,'Fstarsobjid_z_Agamma.dat',FORMAT=('i,f,f,f'),FSobjid,FSz,FSa,FSgamma   ; reading the A and gamma of all 5000 FGstars
   readcol,'slopeinvestigation_output_FGstars_grfit110207.txt',FORMAT=(''),FSobjid2,FSabest,FSamed,FSamean,FSaplus68,FSaminus68,FSaplus95,FSaminus95,FSbbest,FSbmed,FSbmean,FSbplus68,FSbminus68,FSbplus95,FSbminus95,FSz,FSseason

   if SM1 eq 1 then FSabest = FSabest -1

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,FSgamma(0:499),FSabest,col=col.blue,psym=8,thick=thickall
   XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'FG stars',col=col.blue,charsize=2.5,charthick=thickall,alignment=1.0
endif

if n_elements(RRL) eq 1 then begin  ; overplotting FGstars if requested
   readcol,'RRLsSesarobjid_z_Agamma.dat',FORMAT=('i,f,f,f'),RRLobjid,RRLz,RRLa,RRLgamma   ; reading the A and gamma of all 5000 FGstars
   readcol,'PDFs_RRL_grfit110215/slopeinvestigation_output_RRL_grfit110215.txt',FORMAT=(''),RRLobjid2,RRLabest,RRLamed,RRLamean,RRLaplus68,RRLaminus68,RRLaplus95,RRLaminus95,RRLbbest,RRLbmed,RRLbmean,RRLbplus68,RRLbminus68,RRLbplus95,RRLbminus95,RRLz,RRLseason

   if SM1 eq 1 then RRLabest = RRLabest -1

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,RRLgamma,RRLabest,col=col.red,psym=8,thick=thickall
   XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'RR Lyrae',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 25) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSAprime'+strtrim(fit,2)+'_zcorrected.eps'
   if SCHW eq 1 then plot1 = 'epsplots/slopeVSAprime'+strtrim(fit,2)+'_zcorrected_schw.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS Aprime zcorrected'
   thickall = 2
endelse
; setting plot range
XR = [alog10(0.018),alog10(1.)]
YR = [0.1,1.4]
if FIT eq 'ui' then YR = [-0.1,1.2]
if SM1 eq 1 then YR = YR - 1
;if SCHW eq 1 then XR = [alog10(0.002*Nschw),alog10(0.2*Nschw)]

xticks = ['0.03','0.1','0.3','1.0']
;if SCHW eq 1 then xticks = trim(['0.003','0.01','0.03','0.1']*Nschw)

XTIT = "A'"
if SCHW eq 1 then XTIT = textoidl("A'_{schw}")

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =XTIT $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , xtickname = xticks  $
;        , /xlog $
        , background = col.white

Sent = sort(qcat.z)

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

;oplot,XR,fltarr(2)+1,thick=thickall,linestyle=2,col=col.black

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA,/noerase $
        , xtitle =XTIT $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , xtickname = xticks $
;        , /xlog $
        , background = col.white
; ------------------------------------------------------------------------------


Aprime = fltarr(NQSOs)
for ll=0,NQSOs-1 do begin
   Aprime(ll) = Qcat(ll).A*(1+Qcat(ll).z)^Qcat(ll).gamma
endfor
Aprime = alog10(Aprime)

;PLOTSYM,0,0.5,/FILL
PLOTSYM,0,0.7,/FILL
oplot,Aprime(Sent),acorrected0,col=col.darkgray,psym=8,thick=thickall

;oplot,Qcat.A,Qcat.abest,col=col.red,psym=8,thick=thickall

if CONT eq 1 then begin
   ;-- plotting contours --
   contourarray,Aprime(Sent),XR[0],XR[1],acorrected0,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=[5.,15.,30.,70.] $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif


if n_elements(Fstars) eq 1 then begin  ; overplotting FGstars if requested

   if BOAG eq 0 then begin
      readcol,'Fstarsobjid_z_Agamma.dat',FORMAT=('O,f,f,f'),FSobjid,FSz,FSa,FSgamma   ; reading the A and gamma of all 5000 FGstars
      readcol,'slopeinvestigation_output_FGstars_grfit110207.txt',FORMAT=(''),FSobjid2,FSabest,FSamed,FSamean,FSaplus68,FSaminus68,FSaplus95,FSaminus95,FSbbest,FSbmed,FSbmean,FSbplus68,FSbminus68,FSbplus95,FSbminus95,FSz,FSseason
   endif else begin
      bovyFGS = mrdfits('joBstuff/star_powerlawSF_constmean/star_powerlawSF_constmean_r_SORTkey.fits',1) ; reading the A and gamma of all bovy's FGstars
      readcol,'joBstuff/bovyFGstars_grfit110608_top500.txt',FORMAT=(''),FSobjid2,FSabest,FSamed,FSamean,FSaplus68,FSaminus68,FSaplus95,FSaminus95,FSbbest,FSbmed,FSbmean,FSbplus68,FSbminus68,FSbplus95,FSbminus95,FSz,FSseason
      FSa = exp(bovyFGS(FSobjid2-1).logA/2)
   endelse

   if SM1 eq 1 then FSabest = FSabest -1

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,alog10(FSa(0:499)),FSabest,col=col.blue,psym=8,thick=thickall
   XYOUTS,DX*0.96+XR[0],DY*0.10+YR[0],'FG stars',col=col.blue,charsize=2.5,charthick=thickall,alignment=1.0
endif

if n_elements(RRL) eq 1 then begin  ; overplotting FGstars if requested

   readcol,'PDFs_RRL_grfit110215/slopeinvestigation_output_RRL_grfit110215.txt',FORMAT=(''),RRLobjid2,RRLabest,RRLamed,RRLamean,RRLaplus68,RRLaminus68,RRLaplus95,RRLaminus95,RRLbbest,RRLbmed,RRLbmean,RRLbplus68,RRLbminus68,RRLbplus95,RRLbminus95,RRLz,RRLseason
   if SM1 eq 1 then RRLabest = RRLabest -1

   if BOAG eq 0 then begin
      readcol,'RRLsSesarobjid_z_Agamma.dat',FORMAT=('O,f,f,f'),RRLobjid,RRLz,RRLa,RRLgamma   ; reading the A and gamma of all 5000 FGstars
   endif else begin
      bovyRRL = mrdfits('joBstuff/rrlyrae_powerlawSF_constmean/rrlyrae_powerlawSF_constmean_r_SORTid.fits',1)
      RRLobjid = bovyRRL.ID

      RRLa = fltarr(n_elements(bovyRRL.ID))
      for mm=0,n_elements(bovyRRL.ID)-1 do begin  ; filling the RRL array
         fillent = where(RRLobjid2 eq RRLobjid(mm))  ; entry to fill
         RRLa(mm) = exp(bovyRRL(mm).loga/2)
      endfor
   endelse

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,alog10(RRLa),RRLabest,col=col.red,psym=8,thick=thickall
   XYOUTS,DX*0.96+XR[0],DY*0.05+YR[0],'RR Lyrae',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0
endif

; making array with Aprime bins
Aprime        = Aprime(Sent)
acorrected    = acorrected0(sort(APRIME))
APRIMEsort    = APRIME(sort(APRIME))
APRIMEsortarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

APRIMEavg         = fltarr(3,Nbin)  ; array for bincenter, mean value and errors on mean
Pfillrect_Aprime  = fltarr(4,Nbin)  ; array for poly fill levels

for kk=0,n_elements(APRIMEsortarr)-2 do begin ; looping over redshift bins
   APRIMEsortent = where(APRIMEsort ge APRIMEsortarr(kk) AND APRIMEsort lt APRIMEsortarr(kk+1),CAPRIMEsorts)
   if APRIMEsortent ne [-1] and CAPRIMEsorts gt Npointsmin then begin
      slopeAPRIMEsortbin = acorrected(APRIMEsortent)
      if CAPRIMEsorts eq 1 then slopeAPRIMEsortbin = fltarr(1,1)+slopeAPRIMEsortbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopeAPRIMEsortsorted = slopeAPRIMEsortbin(sort(slopeAPRIMEsortbin))
      Mom = moment(slopeAPRIMEsortbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopeAPRIMEsortsorted)
      ; 68% confidence level
      Slow68  = slopeAPRIMEsortsorted[0.16*CAPRIMEsorts]
      Shigh68 = slopeAPRIMEsortsorted[0.84*CAPRIMEsorts]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopeAPRIMEsortsorted[0.025*CAPRIMEsorts]
      Shigh95 = slopeAPRIMEsortsorted[0.975*CAPRIMEsorts]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopeAPRIMEsortsorted)
      ; 68% confidence level
      Slow68_mean  = slopeAPRIMEsortsorted[0.16*CAPRIMEsorts]
      Shigh68_mean = slopeAPRIMEsortsorted[0.84*CAPRIMEsorts]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopeAPRIMEsortsorted[0.025*CAPRIMEsorts]
      Shigh95_mean = slopeAPRIMEsortsorted[0.975*CAPRIMEsorts]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      BinC = APRIMEsortarr(kk)+(APRIMEsortarr(kk+1)-APRIMEsortarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 
      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(CAPRIMEsorts),psym=3, col=col.green, ERRCOLOR=col.green, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(CAPRIMEsorts)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(CAPRIMEsorts)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black,NOCLIP=0,clip=[XR[0], YR[0], XR[1], YR[1]]
      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data

      ; filling array (used in zoom version)
      APRIMEavg(0,kk)  = BinC
      APRIMEavg(1,kk)  = slopemean
      APRIMEavg(2,kk)  = sqrt(MOM(1))/sqrt(CAPRIMEsorts)
      Pfillrect_Aprime(0:3,kk) = [Xlow,Xhigh,Ylow,Yhigh]
   endif
endfor

varmark = 1
if varmark eq 1 and SCHW eq 0 then begin
   Dmags       = [0.0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
   if FIT eq 'gr' then meanalinear = [0.451592,0.422081,0.757427,0.823645,0.883770,0.892704,0.991097,0.991593,0.963111,0.960074,0.936306,0.992285,1.00808,1.00537,1.01425,1.00451,1.00437,1.00327,0.982635]
   if FIT eq 'ui' then meanalinear = [-0.0203185,0.106838,0.00781821,-0.175671,-0.0536402,-0.232877,-0.0852944,-0.0275363,-0.0344860,0.169736,0.108563,0.956882,1.02226,1.04859,1.06864,1.05970,1.04920,1.09544,1.04465]   

   if SM1 eq 1 then meanalinear = meanalinear -1

   PLOTSYM,0,2.0,/FILL,thick=thickall
   oplot,alog10(Dmags),meanalinear,psym=8,col=col.darkgreen,thick=thickall
   PLOTSYM,0,2.0,thick=thickall
   oplot,alog10(Dmags),meanalinear,psym=8,col=col.black,thick=thickall

   XYOUTS,DX*0.05+XR[0],DY*0.92+YR[0],textoidl('synthetic data with s_{gr(in)} \equiv 0'),col=col.darkgreen,charsize=2.5,charthick=thickall;,alignment=1.0
endif


if where(PLOTSELECT eq 30) ne [-1] then begin ; if the zoom version is to be plotted, overplot dashed box of zoom region
   if fit eq 'gr' then begin 
      XRzoomA = [alog10(0.1),alog10(0.7)]
      YRzoomA = [0.6,1.0]
      if SM1 eq 1 then YRzoomA = YRzoomA - 1
   endif
   if fit eq 'ui' then begin 
      XRzoomA = [alog10(0.21),alog10(0.8)]
      YRzoomA = [0.5,0.8]
      if SM1 eq 1 then YRzoomA = YRzoomA - 1
   endif
   oplot,[XRzoomA(0),XRzoomA(0)],[YRzoomA(0),YRzoomA(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRzoomA(1),XRzoomA(1)],[YRzoomA(0),YRzoomA(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRzoomA(0),XRzoomA(1)],[YRzoomA(0),YRzoomA(0)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRzoomA(0),XRzoomA(1)],[YRzoomA(1),YRzoomA(1)],col=col.black,linestyle=2,thick=thickall
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif


if where(PLOTSELECT eq 26) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSgamma'+strtrim(fit,2)+'_zcorrected.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS gamma zcorrected'
   thickall = 2
endelse
; setting plot range
XR = [-0.05,1.25]
YR = [0.1,1.4]
if FIT eq 'ui' then YR = [-0.1,1.2]
if SM1 eq 1 then YR = YR - 1

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
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

Sent = sort(Qcat.z)

POLYFILL,[XR[0],XR[1],XR[1],XR[0]],[YR[0],YR[0],YLIM,YLIM],col=col.lightgray

;oplot,XR,fltarr(2)+1,thick=thickall,linestyle=2,col=col.black

; ------------------ Overplotting axis on Polyfill -----------------------------
plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA,/noerase $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('s_{'+strtrim(fit,2)+'}') $
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
; ------------------------------------------------------------------------------

PLOTSYM,0,0.7,/FILL
oplot,Qcat(Sent).gamma,acorrected0,col=col.darkgray,psym=8,thick=thickall

if fit eq 'gr' then GoodA = where(Qcat(sort(Qcat.z)).A gt 0.1)
if fit eq 'ui' then GoodA = where(Qcat(sort(Qcat.z)).A gt 0.25)

oplot,Qcat(GoodA).gamma,acorrected0(GoodA),col=col.green,psym=8,thick=thickall
Sent = GoodA
acorrected0 = acorrected0(GoodA)

if CONT eq 1 then begin
   ;-- plotting contours --
   contourarray,Qcat(Sent).gamma,XR[0],XR[1],acorrected0,YR[0],YR[1],40,40,4,contarr2,levelbin,xrange,yrange
   contour,contarr2,xrange,yrange,/overplot $
	, levels=[5.,15.,30.,70.] $
	, C_COLOR=col.black $
;	, C_LABELS = levelbin , charsize=2.5 $ 
        , thick=thickall
   ;------------------------
endif

if n_elements(Fstars) eq 1 then begin  ; overplotting FGstars if requested

   if BOAG eq 0 then begin
      readcol,'Fstarsobjid_z_Agamma.dat',FORMAT=('O,f,f,f'),FSobjid,FSz,FSa,FSgamma   ; reading the A and gamma of all 5000 FGstars
      readcol,'slopeinvestigation_output_FGstars_grfit110207.txt',FORMAT=(''),FSobjid2,FSabest,FSamed,FSamean,FSaplus68,FSaminus68,FSaplus95,FSaminus95,FSbbest,FSbmed,FSbmean,FSbplus68,FSbminus68,FSbplus95,FSbminus95,FSz,FSseason
   endif else begin
      bovyFGS = mrdfits('joBstuff/star_powerlawSF_constmean/star_powerlawSF_constmean_r_SORTkey.fits',1) ; reading the A and gamma of all bovy's FGstars
      readcol,'joBstuff/bovyFGstars_grfit110608_top500.txt',FORMAT=(''),FSobjid2,FSabest,FSamed,FSamean,FSaplus68,FSaminus68,FSaplus95,FSaminus95,FSbbest,FSbmed,FSbmean,FSbplus68,FSbminus68,FSbplus95,FSbminus95,FSz,FSseason
      FSgamma = bovyFGS(FSobjid2-1).gamma
   endelse

   if SM1 eq 1 then FSabest = FSabest -1

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,FSgamma(0:499),FSabest,col=col.blue,psym=8,thick=thickall
   XYOUTS,DX*0.95+XR[0],DY*0.10+YR[0],'FG stars',col=col.blue,charsize=2.5,charthick=thickall,alignment=1.0
endif

if n_elements(RRL) eq 1 then begin  ; overplotting FGstars if requested
   readcol,'PDFs_RRL_grfit110215/slopeinvestigation_output_RRL_grfit110215.txt',FORMAT=(''),RRLobjid2,RRLabest,RRLamed,RRLamean,RRLaplus68,RRLaminus68,RRLaplus95,RRLaminus95,RRLbbest,RRLbmed,RRLbmean,RRLbplus68,RRLbminus68,RRLbplus95,RRLbminus95,RRLz,RRLseason
   if SM1 eq 1 then RRLabest = RRLabest -1

   if BOAG eq 0 then begin
      readcol,'RRLsSesarobjid_z_Agamma.dat',FORMAT=('O,f,f,f'),RRLobjid,RRLz,RRLa,RRLgamma   ; reading the A and gamma of all 5000 FGstars
   endif else begin
      bovyRRL = mrdfits('joBstuff/rrlyrae_powerlawSF_constmean/rrlyrae_powerlawSF_constmean_r_SORTid.fits',1)
      RRLobjid = bovyRRL.ID

      RRLgamma = fltarr(n_elements(bovyRRL.ID))
      for mm=0,n_elements(bovyRRL.ID)-1 do begin  ; filling the RRL array
         fillent = where(RRLobjid2 eq RRLobjid(mm))  ; entry to fill
         RRLgamma(mm) = bovyRRL(mm).gamma
      endfor
   endelse

   PLOTSYM,0,0.4,thick=thickall,/fill
   oplot,RRLgamma,RRLabest,col=col.red,psym=8,thick=thickall
   XYOUTS,DX*0.95+XR[0],DY*0.05+YR[0],'RR Lyrae',col=col.red,charsize=2.5,charthick=thickall,alignment=1.0
endif

; making array with redshift bins
gamma        = Qcat(sent).gamma
acorrected   = acorrected0(sort(gamma))
gammasort    = gamma(sort(gamma))
gammasortarr = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

GAMMAavg         = fltarr(3,Nbin)  ; array for bincenter, mean value and errors on mean
Pfillrect_gamma  = fltarr(4,Nbin)  ; array for poly fill levels

for kk=0,n_elements(gammasortarr)-2 do begin ; looping over redshift bins
   gammasortent = where(gammasort ge gammasortarr(kk) AND gammasort lt gammasortarr(kk+1),Cgammasorts)
   if gammasortent ne [-1] and Cgammasorts gt Npointsmin then begin
      slopegammasortbin = acorrected(gammasortent)
      if Cgammasorts eq 1 then slopegammasortbin = fltarr(1,1)+slopegammasortbin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      slopegammasortsorted = slopegammasortbin(sort(slopegammasortbin))
      Mom = moment(slopegammasortbin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using median for bins ----
      slopemed  = median(slopegammasortsorted)
      ; 68% confidence level
      Slow68  = slopegammasortsorted[0.16*Cgammasorts]
      Shigh68 = slopegammasortsorted[0.84*Cgammasorts]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopegammasortsorted[0.025*Cgammasorts]
      Shigh95 = slopegammasortsorted[0.975*Cgammasorts]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95

      ; ---- using mean for bins ----
      slopemean = mean(slopegammasortsorted)
      ; 68% confidence level
      Slow68_mean  = slopegammasortsorted[0.16*Cgammasorts]
      Shigh68_mean = slopegammasortsorted[0.84*Cgammasorts]
      Serr68p_mean = Shigh68 - slopemean
      Serr68m_mean = slopemean - Slow68
      ; 95% confidence level (2 sigma)
      Slow95_mean  = slopegammasortsorted[0.025*Cgammasorts]
      Shigh95_mean = slopegammasortsorted[0.975*Cgammasorts]
      Serr95p_mean = Shigh95 - slopemean
      Serr95m_mean = slopemean - Slow95

      ; ---- plotting bins and error bars ----
      BinC = gammasortarr(kk)+(gammasortarr(kk+1)-gammasortarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL

      oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 

      ;oploterror,xx+BinC,xx+slopemean,xx+sqrt(MOM(1))/sqrt(Cgammasorts),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation of mean

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = slopemean-sqrt(MOM(1))/sqrt(Cgammasorts)
      Yhigh = slopemean+sqrt(MOM(1))/sqrt(Cgammasorts)
      if Ylow lt YR[0] then ylow = YR[0]
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black,NOCLIP=0,clip=[XR[0], YR[0], XR[1], YR[1]]

      oplot,xx+BinC,xx+slopemean,psym=3,col=col.black  ; over plotting the data

      ; filling array (used in zoom version)
      GAMMAavg(0,kk)  = BinC
      GAMMAavg(1,kk)  = slopemean
      GAMMAavg(2,kk)  = sqrt(MOM(1))/sqrt(CGAMMAsorts)
      Pfillrect_gamma(0:3,kk) = [Xlow,Xhigh,Ylow,Yhigh]
   endif
endfor

if where(PLOTSELECT eq 31) ne [-1] then begin ; if the zoom version is to be plotted, overplot dashed box of zoom region
   if fit eq 'gr' then begin 
      XRzoomG = [0.02,0.65]
      YRzoomG = [0.6,1.0]
      if SM1 eq 1 then YRzoomG = YRzoomG - 1
   endif
   if fit eq 'ui' then begin 
      XRzoomG = [0.05,0.65]
;      YRzoomG = [0.3,0.7]
      YRzoomG = [0.4,0.9]
      if SM1 eq 1 then YRzoomG = YRzoomG - 1
   endif
   oplot,[XRzoomG(0),XRzoomG(0)],[YRzoomG(0),YRzoomG(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRzoomG(1),XRzoomG(1)],[YRzoomG(0),YRzoomG(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRzoomG(0),XRzoomG(1)],[YRzoomG(0),YRzoomG(0)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRzoomG(0),XRzoomG(1)],[YRzoomG(1),YRzoomG(1)],col=col.black,linestyle=2,thick=thickall
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 27) ne [-1] then begin
;=============================================================================================
Aprime = fltarr(NQSOs)
for ll=0,NQSOs-1 do begin
   Aprime(ll) = Qcat(ll).A*(1+Qcat(ll).z)^Qcat(ll).gamma
endfor
logAprime = alog10(Aprime)
gamma     = Qcat.gamma

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/gammaVSAprime'+strtrim(fit,2)+'.eps'
   Xw = 25 & Yw = 25
   device,  file=plot1 ,/color , /encapsulated, xsize=Xw, ysize=Yw, BITS=8,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   Xw = 600 & Yw = 600
   window, Nw, xsize=Xw, ysize=Yw, title = 'Gamma VS A'
   thickall = 2
endelse
; setting plot range
XR = alog10([0.01,1.])
YR = [-0.1,1.25]
CHSZall = 2.5

; position of TVimage array
Xminpos = 0.15
Yminpos = 0.12
Xmaxpos = 0.95
Ymaxpos = 0.83
TVP = [Xminpos,Yminpos,Xmaxpos,Ymaxpos]
POSPLOT = TVP

Gbins   = Nbin
Abins   = Nbin
DAprime = abs(XR[1]-XR[0])/Abins
DGamma  = abs(YR[1]-YR[0])/Gbins
Aarr0   = FLTARR(Abins,Gbins)     ; array for mean a values for colors
Carr    = FLTARR(Abins,Gbins)     ; array for number of objects in each bin

for ii=0,Gbins-1 do begin
   for jj=0,Abins-1 do begin
      aent = where(gamma gt YR[0]+ii*Dgamma and gamma lt YR[0]+(ii+1)*Dgamma and logAprime gt XR[0]+jj*DAprime and logAprime lt XR[0]+(jj+1)*DAprime,Counta) ;and Qcat.z lt 1.0

      if Counta le Npointsmin then Aarr0(jj,ii) = 9999.
      if Counta gt Npointsmin then begin 
         Aarr0(jj,ii) = median(Qcat(Aent).abest) 
         Carr(jj,ii)  = Counta                    ;    storing number of objects in bin
      endif
   endfor
endfor

Showmin  = 0.1  ; the minimum value to show in plot -> to get sensible color bar
Showmax  = 2.0  ; the maximum value to show in plot -> to get sensible color bar

maxval = max(Aarr0(where(Aarr0 ne 9999 and Aarr0 lt showmax)))   ; finding the maximum value of array (making sure that pixels with median gt 2 are ignored as well)
minval = min(Aarr0(where(Aarr0 ne 0    and Aarr0 gt showmin)))   ; finding the maximum value of array
Abyte  = BYTSCL(Aarr0,MIN=minVal,MAX=MaxVal)   ; image turned into bytscale for plotting

plot,fltarr(2),fltarr(2), /NODATA, background=col.white,col=col.white  ; creating all white window

device, decomposed=0     ; tip before loading color scheme
sauron_colormap         ; loading the sauron color map   - can also use LOADCT,39

; --- Making the last color white in previously loaded color scheme ---
TVLCT, Rvec, Gvec, Bvec, /GET   ; getting red, green and blue color vectors
Rvec[255] = 255                 ; Changing last red color
Gvec[255] = 255                 ; Changing last green color
Bvec[255] = 255                 ; Changing last blue color
TVLCT, Rvec, Gvec, Bvec         ; Setting the edited vectors as colors
; ---------------------------------------------------------------------

if SMO ne 0 then begin
   goodent = where(Abyte lt max(Abyte))              ; entries with actual values
   WHTARR  = Carr/max(Carr) ; array with weights
   ;WHTARR(where(Carr ne 0))  = 1 
   ;imgsmoothing2D,Abyte,SMOOTH,ABYTEsmooth,/VERBOSE,WEIGHTARR=WHTARR
   imgsmoothingGWEIGHT,Abyte,SMOOTH,WHTARR,ABYTEsmooth,/VERBOSE
   Abyte(goodent) = AbyteSmooth(goodent)             ; only using smoothing from good values
endif

tvimage,Abyte,POSITION=TVP,/nointerpolation;,/overplot,/keep_aspect_ratio
CLEGEND = textoidl('s_{'+strtrim(FIT,2)+'}')
colorbar,position=[Xminpos,Ymaxpos+0.06,Xmaxpos,Ymaxpos+0.12] $
    ;,title=CLEGEND $
    , range=[Minval,Maxval] $
    , format='(F5.2)' $
    , charsize=CHSZall $
    , thick=thickall $
    , xthick = thickall $
    , ythick = thickall $
    , charthick=thickall $
    , color='BLACK'

device, decomposed=0     ; tip before loading color scheme
col=getcolor(/load)
plot,logAprime,gamma, col=col.black  $
        , /NODATA $
        , /noerase $
        , xtitle ="log(A')" $
        , ytitle =textoidl('\gamma') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CHSZall $
        , charthick = thickall $
        , psym = 3 $
        , yminor = 2 $   
        , position=POSPLOT $
        , background = col.white

;XYOUTS,DX*0.15+XR[0],DY*0.85+YR[0],textoidl('<s_{'+strtrim(FIT,2)+'}>'),col=col.black,charsize=4,charthick=thickall

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 28) ne [-1] then begin
;=============================================================================================
Aprime = fltarr(NQSOs)
for ll=0,NQSOs-1 do begin
   Aprime(ll) = Qcat(ll).A*(1+Qcat(ll).z)^Qcat(ll).gamma
endfor
Sent      = sort(Qcat.z)
logAprime = alog10(Aprime(Sent))
gamma     = Qcat(Sent).gamma

!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/gammaVSAprime'+strtrim(fit,2)+'_zcorrected.eps'
   if SCHW eq 1 then plot1 = 'epsplots/gammaVSAprime'+strtrim(fit,2)+'_zcorrected_schw.eps'
   Xw = 25 & Yw = 25
   device,  file=plot1 ,/color , /encapsulated, xsize=Xw, ysize=Yw, BITS=8,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   Xw = 600 & Yw = 600
   window, Nw, xsize=Xw, ysize=Yw, title = 'Gamma VS A zcorrected'
   thickall = 2
endelse
; setting plot range
XR = alog10([0.03,1.]) ; [0.01,1.]
YR = [-0.1,0.8]
if BOAG eq 1 then YR = [-0.1,1.0]
CHSZall = 2.5
;if SCHW eq 1 then XR = [alog10(0.002*Nschw),alog10(0.16*Nschw)]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

xticks = ['0.04','0.06','0.10','0.16','0.25','0.40','0.63','1.00']  ; for xrange alog10([0.03,1.]) 
;xticks = ['0.01','0.03','0.1','0.3','1.0'] $  ; for xrange alog10([0.01,1.]) 
;if SCHW eq 1 then xticks = trim(['0.003','0.01','0.03','0.1']*Nschw)

XTIT = "A'"
if SCHW eq 1 then XTIT = textoidl("A'_{schw}")

; position of TVimage array
Xminpos = 0.15
Yminpos = 0.12
Xmaxpos = 0.95
Ymaxpos = 0.83
TVP = [Xminpos,Yminpos,Xmaxpos,Ymaxpos]
POSPLOT = TVP

Gbins   = Nbin
Abins   = Nbin
DAprime = abs(XR[1]-XR[0])/Abins
DGamma  = abs(YR[1]-YR[0])/Gbins
Aarr0   = FLTARR(Abins,Gbins)     ; array for mean a values for colors
Carr    = FLTARR(Abins,Gbins)     ; array for number of objects in each bin

for ii=0,Gbins-1 do begin
   for jj=0,Abins-1 do begin
      aent = where(gamma gt YR[0]+ii*Dgamma and gamma lt YR[0]+(ii+1)*Dgamma and logAprime gt XR[0]+jj*DAprime and logAprime lt XR[0]+(jj+1)*DAprime, Counta)
      if Counta le Npointsmin then Aarr0(jj,ii) = 9999. 
      if Counta gt Npointsmin then begin 
         Aarr0(jj,ii) = median(acorrected0(Aent))
         Carr(jj,ii)  = Counta                    ;    storing number of objects in bin
      endif
   endfor
endfor

Showmin  = 0.1  ; the minimum value to show in plot -> to get sensible color bar
Showmax  = 1.3  ; the maximum value to show in plot -> to get sensible color bar
if SM1 eq 1 then Showmin = Showmin - 1
if SM1 eq 1 then Showmax = Showmax - 1

maxval = max(Aarr0(where(Aarr0 ne 9999 and Aarr0 lt showmax)))   ; finding the maximum value of array (making sure that pixels with median gt 2 are ignored as well)
minval = min(Aarr0(where(Aarr0 ne 0    and Aarr0 gt showmin)))   ; finding the maximum value of array
Abyte  = BYTSCL(Aarr0,MIN=minVal,MAX=MaxVal)   ; image turned into bytscale for plotting

plot,fltarr(2),fltarr(2), /NODATA, background=col.white,col=col.white  ; creating all white window

device, decomposed=0     ; tip before loading color scheme
sauron_colormap         ; loading the sauron color map   - can also use LOADCT,39

; --- Making the last color white in previously loaded color scheme ---
TVLCT, Rvec, Gvec, Bvec, /GET   ; getting red, green and blue color vectors
Rvec[255] = 255                 ; Changing last red color
Gvec[255] = 255                 ; Changing last green color
Bvec[255] = 255                 ; Changing last blue color
TVLCT, Rvec, Gvec, Bvec         ; Setting the edited vectors as colors
; ---------------------------------------------------------------------

if SMO ne 0 then begin

   print,"The A' bin size = ",trim(Daprime)
   print,'The gamma bin size = ',trim(Dgamma)

   goodent = where(Abyte lt max(Abyte))              ; entries with actual values
   WHTARR  = Carr/max(Carr) ; array with weights
   ;WHTARR(where(Carr ne 0))  = 1 
   ;imgsmoothing2D,Abyte,SMOOTH,ABYTEsmooth,/VERBOSE,WEIGHTARR=WHTARR
   imgsmoothingGWEIGHT,Abyte,SMOOTH,WHTARR,ABYTEsmooth,/VERBOSE
   Abyte(goodent) = AbyteSmooth(goodent)             ; only using smoothing from good values

   ell = ELLIPSE([alog10(0.06),0.65],SMOOTH/2.*[Daprime,Dgamma],Npoints=50)  ; creating ellipse to represent smoothing kernel
endif

tvimage,Abyte,POSITION=TVP,/nointerpolation;,/overplot,/keep_aspect_ratio
CLEGEND = textoidl('s_{'+strtrim(FIT,2)+'}')
colorbar,position=[Xminpos,Ymaxpos+0.06,Xmaxpos,Ymaxpos+0.12] $
    ;,title=CLEGEND $
    , range=[Minval,Maxval] $
    , format='(F5.2)' $
    , charsize=CHSZall $
    , thick=thickall $
    , xthick = thickall $
    , ythick = thickall $
    , charthick=thickall $
    , color='BLACK'

device, decomposed=0     ; tip before loading color scheme
col=getcolor(/load)
plot,fltarr(2),fltarr(2), col=col.black  $
        , /NODATA,/noerase $
        , xtitle =XTIT $
        , xtickname = xticks $
        , ytitle =textoidl('\gamma') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CHSZall $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $   
        , position=POSPLOT $
        , background = col.white

if SMO ne 0 then oplot,ell(0,*),ell(1,*),thick=thickall,col=col.black  ; overplotting ellipse representing smoothing kernel
XYOUTS,DX*0.10+XR[0],DY*0.85+YR[0],textoidl('<s_{'+strtrim(FIT,2)+'}>'),col=col.black,charsize=4,charthick=thickall

; overplotting boxes with good values
if FIT eq 'gr' then begin
   XRbox = alog10([0.1,1.0])
   YRbox = [0.0,1.0]

   oplot,[XRbox(0),XRbox(0)],[YRbox(0),YRbox(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRbox(1),XRbox(1)],[YRbox(0),YRbox(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRbox(0),XRbox(1)],[YRbox(0),YRbox(0)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRbox(0),XRbox(1)],[YRbox(1),YRbox(1)],col=col.black,linestyle=2,thick=thickall
endif

if FIT eq 'ui' then begin
   XRbox = alog10([0.25,1.0])
   YRbox = [0.0,1.0]

   oplot,[XRbox(0),XRbox(0)],[YRbox(0),YRbox(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRbox(1),XRbox(1)],[YRbox(0),YRbox(1)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRbox(0),XRbox(1)],[YRbox(0),YRbox(0)],col=col.black,linestyle=2,thick=thickall
   oplot,[XRbox(0),XRbox(1)],[YRbox(1),YRbox(1)],col=col.black,linestyle=2,thick=thickall
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 29) ne [-1] then begin
;=============================================================================================
Aprime = fltarr(NQSOs)
for ll=0,NQSOs-1 do begin
   Aprime(ll) = Qcat(ll).A*(1+Qcat(ll).z)^Qcat(ll).gamma
endfor

;defining Aprime and gamma sub sample
Gmin = 0.0
Gmax = 0.2
Amin = 10^(-1.3)
Amax = 10^(-1.0)
xx = where(Aprime(0:Nobjplot-1) gt Amin and Aprime(0:Nobjplot-1) lt Amax and Qcat(0:Nobjplot-1).gamma gt Gmin and Qcat(0:Nobjplot-1).gamma lt Gmax,CC)
if VB eq 1 then print,":: plotDavisVSstripe82VSshen.pro :: Objects to plot in selected A'-gamma cut:",cc

for ii=0,Nobjplot-1 do begin    ; looping over objects with shen data

   ; putting restictions on A and gamma for object before plotting
   if Aprime(ii) gt Amin and Aprime(ii) lt Amax and Qcat(ii).gamma gt Gmin and Qcat(ii).gamma lt Gmax then begin  
   ;if Qcat(ii).abest lt 0.1 and Qcat(ii).abest gt -0.1 then begin   ; only very shallow slopes

   Objent = where(Qdata.headobjid eq Qcat(ii).headobjid)   ; entries for given object
   if fit eq 'gr' then begin
      Xdat = Qdata(objent).psfmag_g
      Ydat = Qdata(objent).psfmag_r
      Xerr = Qdata(objent).psfmagERR_g
      Yerr = Qdata(objent).psfmagERR_r
   endif
   if fit eq 'ui' then begin
      Xdat = Qdata(objent).psfmag_u
      Ydat = Qdata(objent).psfmag_i
      Xerr = Qdata(objent).psfmagERR_u
      Yerr = Qdata(objent).psfmagERR_i
   endif
   
   ;= = = mag vs mag plot = = =
   !p.multi = [0,0,0]
   if PS eq 1 then begin
      set_plot, 'ps'
      col=getcolor(/load)     ; get color table for plot
      plot1 = 'epsplots/'+strtrim(fit,2)+'space_obj'+strtrim(ii,2)+'.eps' ; name of eps file
      device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
      thickall = 6
   endif else begin
      set_plot, 'x'
      col=getcolor(/load)     ; get color table for plot
      device, retain=2        ; ensuring that plotting windows 'regenerate'
      window, Nw, xsize=600, ysize=500, title = 'mag-mag space'
      thickall = 2
   endelse

   ; setting plot range
   XR = [median(Xdat)+1,median(Xdat)-1]
   YR = [median(Ydat)+1,median(Ydat)-1]
   DX = XR[1]-XR[0]
   DY = YR[1]-YR[0]
   XT = STRMID(fit, 0, 1)
   YT = STRMID(fit, 1, 1)

   ;=== PLOTTING LIGHT CURVE(S) ===
   plot,gmag_M1,rmag_M1, col=col.black    $
        , /NODATA $
        , xtitle =XT $
        , ytitle =YT $
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

   NX = 30
   XX = findgen(NX)-15
   oplot,XX+mean(Xdat),Qcat(ii).Abest*XX+mean(Ydat)+Qcat(ii).Bbest,linestyle=0,thick=thickall,col=col.blue

   ; overplotting 'error-lines'
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(Qcat(ii).Abest+Qcat(ii).Aplus68)*XX(NX/2.:NX-1)+mean(Ydat)+(Qcat(ii).Bbest+Qcat(ii).Bplus68),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(Qcat(ii).Abest+Qcat(ii).Aplus68)*XX(0:NX/2.)   +mean(Ydat)+(Qcat(ii).Bbest-Qcat(ii).Bminus68),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(0:NX/2.)   +mean(Xdat),(Qcat(ii).Abest-Qcat(ii).Aminus68)*XX(0:NX/2.)   +mean(Ydat)+(Qcat(ii).Bbest+Qcat(ii).Bplus68),linestyle=2,thick=thickall,col=col.blue
   oplot,XX(NX/2.:NX-1)+mean(Xdat),(Qcat(ii).Abest-Qcat(ii).Aminus68)*XX(NX/2.:NX-1)+mean(Ydat)+(Qcat(ii).Bbest-Qcat(ii).Bminus68),linestyle=2,thick=thickall,col=col.blue

   plotsym,0,1.5,/fill
   oplot,Xdat,Ydat,psym=8,col=col.red
   oploterror,Xdat,Ydat,Xerr,Yerr,psym=2,col=col.red,ERRCOLOR=col.red,thick=thickall

   if PDF eq 1 then begin
      Pdat = PbadObj(objent) 
      OLs  = where(Pdat gt PbadMIN)   ; entries of probable outliers
      if OLs ne [-1] then begin
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.white   ; 'erasing' objects
         oploterror,Xdat(OLs),Ydat(OLs),Xerr(OLs),Yerr(OLs),psym=2,col=col.white,ERRCOLOR=col.white,thick=thickall,errthick=thickall ; 'erasing' error bar
         plotsym,0,1.5
         oplot,Xdat(OLs),Ydat(OLs),psym=8,col=col.red
      endif
   endif

   plotsym,0,1.5
   oplot,Xdat,Ydat,psym=8,col=col.black,thick=thickall

   XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'SDSS J'+strtrim(Qcat(ii).sdssname,2),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'DR7 '+strtrim(Qcat(ii).headobjid,2),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],textoidl('log(M_{BH}/M_\odot)=')+trim(Qcat(ii).LOGBH_MGII_MD04,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.80+YR[0],textoidl('log(L_{bol}/(erg/s))=')+trim(Qcat(ii).LOGLBOL,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.75+YR[0],textoidl('z=')+trim(Qcat(ii).z,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall
   XYOUTS,DX*0.05+XR[0],DY*0.70+YR[0],"MCMC 'best' fit",col=col.blue,charsize=2.5,charthick=thickall

   if PS eq 1 then begin
      device, /close
      !P.FONT = -1    ; setting default font (needed if PS font has been changed)
      set_plot, 'x'
   endif

   endif  ; end of A and gamma restrionctions

endfor
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================
endif




if where(PLOTSELECT eq 30) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSAprime'+strtrim(fit,2)+'_zcorrectedZOOM.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS Aprime zcorrected ZOOM'
   thickall = 2
endelse
; setting plot range
if fit eq 'gr' then begin 
   XR = XRzoomA
   YR = YRzoomA
   Xts = ['0.1','0.16','0.25','0.40','0.63']
endif
if fit eq 'ui' then begin 
   XR = XRzoomA
   YR = YRzoomA
   Xts = ['0.25','0.32','0.40','0.50','0.63','0.79']
endif

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ="A'" $
;        , ytitle ='a' $
        , ytitle =textoidl('<s_{'+strtrim(fit,2)+'}>') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , xtickname = Xts $  
;        , /xlog $
        , background = col.white

;oploterror,APRIMEavg(0,*),APRIMEavg(1,*),APRIMEavg(2,*),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation

Nrows = n_elements(Pfillrect_Aprime(0,*))
for ll=0,Nrows-1 do begin
   POLYFILL,[Pfillrect_Aprime(0,ll),Pfillrect_Aprime(1,ll),Pfillrect_Aprime(1,ll),Pfillrect_Aprime(0,ll)],[Pfillrect_Aprime(2,ll),Pfillrect_Aprime(2,ll),Pfillrect_Aprime(3,ll),Pfillrect_Aprime(3,ll)],col=col.black,NOCLIP=0,clip=[XR[0], YR[0], XR[1], YR[1]]
endfor

save,APRIMEavg,filename='AprimeAvg_plot30.sav'

varmark = 0
if varmark eq 1 and SCHW eq 0 then begin
   Dmags       = [0.0,0.02,0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
   if FIT eq 'gr' then meanalinear = [0.451592,0.422081,0.757427,0.823645,0.883770,0.892704,0.991097,0.991593,0.963111,0.960074,0.936306,0.992285,1.00808,1.00537,1.01425,1.00451,1.00437,1.00327,0.982635]
   if FIT eq 'ui' then meanalinear = [-0.0203185,0.106838,0.00781821,-0.175671,-0.0536402,-0.232877,-0.0852944,-0.0275363,-0.0344860,0.169736,0.108563,0.956882,1.02226,1.04859,1.06864,1.05970,1.04920,1.09544,1.04465]   

   if SM1 eq 1 then meanalinear = meanalinear -1

   PLOTSYM,0,2.0,/FILL,thick=thickall
   oplot,alog10(Dmags),meanalinear,psym=8,col=col.darkgreen,thick=thickall
   PLOTSYM,0,2.0,thick=thickall
   oplot,alog10(Dmags),meanalinear,psym=8,col=col.black,thick=thickall

   XYOUTS,DX*0.05+XR[0],DY*0.09+YR[0],textoidl('synthetic data with s_{gr(in)} \equiv 0'),col=col.darkgreen,charsize=2.5,charthick=thickall;,alignment=1.0
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif


if where(PLOTSELECT eq 31) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/slopeVSgamma'+strtrim(fit,2)+'_zcorrectedZOOM.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'slope VS gamma zcorrected ZOOM'
   thickall = 2
endelse
; setting plot range
if fit eq 'gr' then begin 
   XR = XRzoomG
   YR = YRzoomG
endif
if fit eq 'ui' then begin 
   XR = XRzoomG
   YR = YRzoomG
endif

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\gamma') $
;        , ytitle ='a' $
        , ytitle =textoidl('<s_{'+strtrim(fit,2)+'}>') $
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

;oploterror,GAMMAavg(0,*),GAMMAavg(1,*),GAMMAavg(2,*),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=2*!D.X_VSIZE/100 ; standard deviation

Nrows = n_elements(Pfillrect_gamma(0,*))
for ll=0,Nrows-1 do begin
   POLYFILL,[Pfillrect_gamma(0,ll),Pfillrect_gamma(1,ll),Pfillrect_gamma(1,ll),Pfillrect_gamma(0,ll)],[Pfillrect_gamma(2,ll),Pfillrect_gamma(2,ll),Pfillrect_gamma(3,ll),Pfillrect_gamma(3,ll)],col=col.black,NOCLIP=0,clip=[XR[0], YR[0], XR[1], YR[1]]
endfor


save,GAMMAavg,filename='GammaAvg_plot31.sav'

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                   ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 32) ne [-1] then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/AprimeVSAprimeSchw_'+strtrim(fit,2)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'A vs A'
   thickall = 2
endelse
; setting plot range
XR = [alog10(0.018),alog10(1.)]
YR = [alog10(0.018),alog10(1.)]
;YR = [alog10(0.008),alog10(0.33)]

xticks = ['0.03','0.1','0.3','1.0']
yticks = ['0.03','0.1','0.3','1.0']
;yticks = ['0.01','0.03','0.1','0.3']

DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , ytitle =textoidl("A'_{schw}") $
        , xtitle =textoidl("A'") $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , xtickname = xticks  $
        , ytickname = yticks  $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

restore,'Aprime_Schw_Nschw.sav'  ; restoring AprimeSchw for gt fit

Aprime     = fltarr(NQSOs)
AprimeSchw = fltarr(NQSOs)
for ll=0,NQSOs-1 do begin
   Aprime(ll)     = Qcat(ll).A*(1+Qcat(ll).z)^Qcat(ll).gamma
   AprimeSchw(ll) = Aschw(ll) *(1+Qcat(ll).z)^Qcat(ll).gamma
endfor

goodent = where(Aschw ne 9999)
Ap     = alog10(Aprime(goodent))
Aps    = alog10(AprimeSchw(goodent))

PLOTSYM,0,0.7,/FILL
oplot,Ap,Aps,col=col.darkgray,psym=8,thick=thickall

oplot,alog10(findgen(100)/100.),alog10(findgen(100)/100.),col=col.black,thick=thickall

; making array with Aprime bins
Ap_sort       = Ap(sort(Ap))
Aps_sort      = Aps(sort(Ap))
Ap_sortarr    = XR[0]+(findgen(NBIN+1)/(NBIN)*(XR[1]-XR[0]))

APavg     = fltarr(3,Nbin)  ; array for bincenter, mean value and errors on mean

for kk=0,n_elements(Ap_sortarr)-2 do begin ; looping over redshift bins
   Ap_sortent = where(Ap_sort ge Ap_sortarr(kk) AND Ap_sort lt Ap_sortarr(kk+1),Cap)
print,cap,Ap_sortarr(kk),Ap_sortarr(kk+1)
   if Ap_sortent ne [-1] and Cap gt Npointsmin then begin
      Aps_bin = Aps_sort(Ap_sortent)
      if Cap eq 1 then Aps_bin = fltarr(1,1)+Aps_bin  ; making sure that also bins with 1 entry are drawn (median can only be taken of arrays)
      Aps_bin = Aps_bin(sort(Aps_bin))
      Mom = moment(Aps_bin)  ; the moments of the bin vector; MOM(0)=mean, MOM(1)=variance, MOM(2)=skewness, MOM(3)=kurtosis
      
      ; ---- using mean for bins ----
      Apsmean = mean(Aps_bin)
      ; 68% confidence level
      Slow68_mean  = Aps_bin[0.16*Cap]
      Shigh68_mean = Aps_bin[0.84*Cap]
      Serr68p_mean = Shigh68 - Apsmean
      Serr68m_mean = Apsmean - Slow68

      ; ---- plotting bins and error bars ----
      BinC = Ap_sortarr(kk)+(Ap_sortarr(kk+1)-Ap_sortarr(kk))/2
      xx = fltarr(2)
      PLOTSYM,8,2.,/FILL
      oploterror,xx+BinC,xx+Apsmean,xx+sqrt(MOM(1)),psym=3, col=col.black, ERRCOLOR=col.black, thick=thickall,HATLENGTH=0*2*!D.X_VSIZE/100 ; standard deviation 

      ; --- plotting small errorbars as rectangles ---
      Xlow  = BinC-0.012*DX
      Xhigh = BinC+0.012*DX
      Ylow  = Apsmean-sqrt(MOM(1))/sqrt(Cap)
      Yhigh = Apsmean+sqrt(MOM(1))/sqrt(Cap)
      POLYFILL,[Xlow,Xhigh,Xhigh,Xlow],[Ylow,Ylow,Yhigh,Yhigh],col=col.black,/clip

      oplot,xx+BinC,xx+Apsmean,psym=3,col=col.black  ; over plotting the data

      ; filling array (used in zoom version)
      APavg(0,kk)  = BinC
      APavg(1,kk)  = Apsmean
      APavg(2,kk)  = sqrt(MOM(1))/sqrt(Cap)
   endif
endfor





if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                   ; incremeting window number by 1
;=============================================================================================
endif

if where(PLOTSELECT eq 33) ne [-1] and fit eq 'gr' then begin
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'epsplots/chi_grVSslopes.eps'
   if SS ne 0 then plot1 = 'epsplots/chi_grVSslopes_Mmin'+trim(MassMin)+'Mmax'+trim(MassMax)+'zmin'+trim(Zmin)+'zmax'+trim(zmax)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                        ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'chi gr vs slopes'
   thickall = 2
endelse
; setting plot range
XR = [-1.0,1.0]
YR = [-1,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]
CS = 2.5   ; the charsize

plot,findgen(2),findgen(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\Delta')+textoidl('s_{gr}') $
        , ytitle ='D'+textoidl('_{g-r}') $
;        , ytitle ='!15D!3'+textoidl('_{g-r}') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = CS $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , xticks = 4 , xtickname = ['-1.0','-0.5','0.0','0.5',' '] $      ; removing the x ticks
        , pos = [0.15,0.15,0.8,0.8] $
        , background = col.white

if SS ne 0 then begin ; if a subsample is selected
   Dmassent = where(Qcat.LOGBH_MGII_MD04 gt MassMin and Qcat.LOGBH_MGII_MD04 lt MassMax and Qcat.z lt Zmax and Qcat.z gt Zmin, Cmassobj)  ; selecting mass range
   C2_gr = C2_gr0(Dmassent,*)
   C2_slope = C2_slope0(Dmassent,*)
   MASSobj = Qcat(Dmassent).LOGBH_MGII_MD04

   ;XYOUTS,DX*0.95+XR[0],DY*0.18+YR[0],'mass range  : ['+trim(MassMin,'(F10.1)')+':'+trim(MassMax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall,alignment=1
   ;XYOUTS,DX*0.95+XR[0],DY*0.11+YR[0],'z range        :   ['+trim(Zmin,'(F10.1)')+':'+trim(Zmax,'(F10.1)')+']',col=col.black,charsize=2.5,charthick=thickall,alignment=1
   ;XYOUTS,DX*0.95+XR[0],DY*0.04+YR[0],'Nobj           :      '+trim(Cmassobj,'(F10.1)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1
   ;by hand
   XYOUTS,DX*0.95+XR[0],DY*0.04+YR[0],textoidl('7.0 < log(M_{BH}) < 10.0'),col=col.black,charsize=2.5,charthick=thickall,alignment=1
   XYOUTS,DX*0.95+XR[0],DY*0.11+YR[0],textoidl('1.4 < z < 1.6'),col=col.black,charsize=2.5,charthick=thickall,alignment=1
   XYOUTS,DX*0.95+XR[0],DY*0.18+YR[0],textoidl('N_{obj} = ')+trim(Cmassobj),col=col.black,charsize=2.5,charthick=thickall,alignment=1
endif else begin                     ; if not use all values
   C2_gr = C2_gr0
   C2_slope = C2_slope0
   MASSobj = Qcat.LOGBH_MGII_MD04
endelse

PLOTSYM,0,0.5,/fill
goodent = where(MASSobj ne 0)
if goodent ne [-1] then oplot,C2_slope(goodent,0),C2_gr(goodent,1),psym=8,thick=thickall,col=col.green
if goodent ne [-1] then oplot,C2_slope(goodent,1),C2_gr(goodent,3),psym=8,thick=thickall,col=col.red
if goodent ne [-1] then oplot,C2_slope(goodent,2),C2_gr(goodent,5),psym=8,thick=thickall,col=col.orange

oplot,[XR[0],XR[1]],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0
oplot,[0.0,0.0],[YR[0],YR[1]],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0


; --- X axis hist ---
binS = 0.05
SLvals = C2_slope(where(C2_slope(*,0) gt XR[0] and C2_slope(*,0) lt XR[1]),0)
plothist,SLvals,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,SLvals,bin=binS,col=col.green, thick=thickall $
        , axiscolor=col.black $
        , yrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , xrange=XR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , yticks=2 $
;        , /xlog $
        , xticks = 1 , xtickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.15,0.8,0.8,0.98]

SLvals = C2_slope(where(C2_slope(*,1) gt XR[0] and C2_slope(*,1) lt XR[1]),1)
plothist,SLvals,bin=binS,col=col.red,/overplot,thick=thickall

SLvals = C2_slope(where(C2_slope(*,2) gt XR[0] and C2_slope(*,2) lt XR[1]),2)
plothist,SLvals,bin=binS,col=col.orange,/overplot,thick=thickall

oplot,[0.0,0.0],[0.0,max(yhist)+0.05*max(yhist)],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

; --- Y axis hist ---
binS = 0.05
chigrvals = C2_gr(where(C2_gr(*,1) gt YR[0] and C2_gr(*,1) lt XR[1]),1)
plothist,chigrvals,bin=binS,xhist,yhist,/noplot                                     ; getting histogram vectors for ranges
plothist,chigrvals,bin=binS,col=col.green, thick=thickall $
        , /rotate $
        , axiscolor=col.black $
        , xrange = [0.0,max(yhist)+0.05*max(yhist)], /xstyle $
        , yrange=YR , /ystyle $
;        , /fill $
;        ,/fline  $
        , charthick = thickall $
	, xthick = thickall $
	, ythick = thickall $
        , fcolor=col.green $
	, charsize =cs $
        , /noerase $
        , xticks=2 $
;        , /xlog $
        , yticks = 1 , ytickname = [' ',' '] $      ; removing the x ticks
        , pos=[0.8,0.15,0.97,0.8]

chigrvals = C2_gr(where(C2_gr(*,3) gt YR[0] and C2_gr(*,3) lt XR[1]),3)
plothist,chigrvals,bin=binS,col=col.red,/overplot,thick=thickall,/rotate

chigrvals = C2_gr(where(C2_gr(*,5) gt YR[0] and C2_gr(*,5) lt XR[1]),5)
plothist,chigrvals,bin=binS,col=col.orange,/overplot,thick=thickall,/rotate

oplot,[0.0,max(yhist)+0.05*max(yhist)],[0.0,0.0],linestyle=2,thick=thickall,col=col.black  ; overplotting line at 0

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif












if vb eq 1 then print,' '
if vb eq 1 then print,':: plotDavisVSstripe82VSshen.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
