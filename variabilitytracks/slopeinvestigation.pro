;+
;----------------------------
;   NAME
;----------------------------
; slopeinvestigation.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure reads in a Stripe 82 data file and uses the Hogg et
; al. MCMC linear fitting to estimate the bluer-brighter slopes in
; mag-mag diagrams (for inter and inter seasons). The slopes as are
; written to an output file for further investigation
;----------------------------
;   COMMENTS
;----------------------------
; The output file can be plotted independently with slopeinvestigationPLOT.pro
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : string containing name and path of input data file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /PLOT           : set /PLOT to plot the result
; MCMCloops       : use this keyword to indicate the number of MCMC samplings
;                   to use when fitting
; PDFdir          : directory where the file with the PDFs will be put
;                   (end with slash) - if not set they will be put in ./
;                   and overwrite any files already here.
; /SEASONS        : set /SEASONS to estimate the linear fit of the individual
;                   seasons of the (stripe 82) data as well.
; Nepochmin       : When fitting individual seasons this keyword gives the 
;                   minimum number of epochs in seasons to fit (default is 5)           
; COLFIT          : set COLFIT to a string indicating what magnitude
;                   space to perform the fit in. Default is in the gr space.
;                   possible spaces are (assuming the data is available)
;                      COLFIT='ug'
;                      COLFIT='gr'
;                      COLFIT='ri'
;                      COLFIT='gi'
;                     *COLFIT='rg'
;                     *COLFIT='gauss'    ; drawing epochs from gaussian cloud on median g and r magnitudes
;                     *COLFIT='datvar'   ; linear variability (Dg(t)=Dr(t)=c*t) added to (non-varying?) gr data
;                     *COLFIT='datvarui' ; linear variability (Dg(t)=Dr(t)=c*t) added to (non-varying?) ui data
;                     *COLFIT='gaussvar' ; same as 'datvar' but with variability drawn from gaussian distribution 
;                   Choices with * are not enabled for the SEASONS 
; REPICK          : This keyword will redraw the data from gaussian distributions around 
;                   each data point with standard deviations REPICK*Xerr_i and REPICK*Yerr_i
; DMAG            : the amount of variability to put in when running with COLFIT='datvar'
;                   or COLFIT='gaussvar'  (default is 0.5 mag/1 years)
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
; /SKIPALLFIT     : set /SKIPALLFIT to only fit the seasons 
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : New file containing the fitted slopes
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; top 19
; IDL>  slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_11_09:58:09_2010.fits','top19.txt',/VERBOSE,PDFdir='PDFs_top19_outliers9999/',MCMCloops=1000,/SEASONS,Nepoch=8;,/SKIPALLFIT

;top10 test
; IDL>  slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','top10QSO.txt',PDFdir='PDFs_top10QSO/',MCMCloops=10000,/SEASONS,Nepoch=8,/VERBOSE;,/SKIPALLFIT

; IDL>  slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','top10QSO_N2.txt',PDFdir='PDFs_top10QSO_N2/',MCMCloops=10000,/SEASONS,Nepoch=8,/VERBOSE;,/SKIPALLFIT


; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','top10QSO_rgfit.txt',PDFdir='PDFs_top10QSO_rg/',MCMCloops=10000,/SEASONS,Nepoch=8,/VERBOSE,COLFIT='rg'


; ====== 1.4 -- 1.6     & 0.00 -- 2.00  & 0.00 -- 5.00
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Fri_Aug_20_07:56:48_2010.fits','slopeinvestigation_output_z1p4to1p6_outliers9999.txt',PDFdir='PDFs_z1p4to1p6_outliers9999/',MCMCloops=100000  ;,/SEASONS,Nepoch=8,/SKIPALLFIT,COLFIT='gr'

; ====== 1.6 -- 1.8     & 0.15 -- 2.00  & 0.00 -- 5.00
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p60000z1p80000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_14:47:04_2010.fits','slopeinvestigation_output_z1p6to1p8_outliers9999.txt',PDFdir='PDFs_z1p6to1p8_outliers9999/',MCMCloops=100000  ;,/SEASONS,Nepoch=8,/SKIPALLFIT

; ====== 1.8 -- 2.0     & 0.15 -- 2.00  & 0.00 -- 5.00
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p80000z2p00000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:16_2010.fits','slopeinvestigation_output_z1p8to2p0_outliers9999.txt',PDFdir='PDFs_z1p8to2p0_outliers9999/',MCMCloops=100000  ;,/SEASONS,Nepoch=8,/SKIPALLFIT

; ====== 2.0 -- 2.3     & 0.15 -- 2.00  & 0.00 -- 5.00
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_2p00000z2p30000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:35_2010.fits','slopeinvestigation_output_z2p0to2p3_outliers9999.txt',PDFdir='PDFs_z2p0to2p3_outliers9999/',MCMCloops=100000  ;,/SEASONS,Nepoch=8,/SKIPALLFIT

; ====== 0.2< z <4.0   0.15< A <2.0   0.0< gamma <5.0  - outliers marked ====== 
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010.fits','slopeinvestigation_output_z0p2to4p0_outliers9999.txt',PDFdir='PDFs_z0p2to4p0_outliers9999/',MCMCloops=100000  ;,/SEASONS,Nepoch=8,/SKIPALLFIT


;============= no outlier precleaning... =============

; ====== 1.4 -- 1.6     & 0.00 -- 2.00  & 0.00 -- 5.00
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_grfit.txt',PDFdir='PDFs_z1p4to1p6_grfit/',MCMCloops=10000,COLFIT='gr'

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_ugfit.txt',PDFdir='PDFs_z1p4to1p6_ugfit/',MCMCloops=10000,COLFIT='ug' 

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_rifit.txt',PDFdir='PDFs_z1p4to1p6_rifit/',MCMCloops=10000,COLFIT='ri' 

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_gifit.txt',PDFdir='PDFs_z1p4to1p6_gifit/',MCMCloops=10000,COLFIT='gi' 

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_uifit.txt',PDFdir='PDFs_z1p4to1p6_uifit/',MCMCloops=10000,COLFIT='ui' 

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_grfit_RP01.txt',PDFdir='PDFs_z1p4to1p6_grfit_RP01/',MCMCloops=10000,COLFIT='gr',REPICK=3.0

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_ugfit_RP01.txt',PDFdir='PDFs_z1p4to1p6_ugfit_RP01/',MCMCloops=10000,COLFIT='ug',REPICK=3.0

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_grfit_RP02.txt',PDFdir='PDFs_z1p4to1p6_grfit_RP02/',MCMCloops=10000,COLFIT='gr',REPICK=3.0

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Jan_5_14:34:32_2011.fits','slopeinvestigation_output_z1p4to1p6_ugfit_RP02.txt',PDFdir='PDFs_z1p4to1p6_ugfit_RP02/',MCMCloops=10000,COLFIT='ug',REPICK=3.0

;
; XXX=== running all objects ===XXX
; == 0.0 -- 7.0     & 0.00 -- 2.00  & 0.00 -- 5.00
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits','slopeinvestigation_output_shenmatchALL_grfit110129.txt',PDFdir='PDFs_S82QSOs_shenmatchALL_grfit110129/',MCMCloops=10000,COLFIT='gr'
;
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits','slopeinvestigation_output_shenmatchALL_uifit110129.txt',PDFdir='PDFs_S82QSOs_shenmatchALL_uifit110129/',MCMCloops=10000,COLFIT='ui'
;
; == pre-cleaned 0.0 - 7.0 & 0.0 -- 2.0 & 0.0 -- 5.0
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0z7AND0A2AND0gamma5_Tue_Feb_15_09:19:29_2011.fits','slopeinvestigation_output_shenmatchALL_grfit110215_outliers9999.txt',PDFdir='PDFs_S82QSOs_shenmatchALL_grfit11015_outliers9999/',MCMCloops=10000,COLFIT='gr'

;
; === running all FG stars ===
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_grfit110207.txt',PDFdir='PDFs_FGstars_grfit110207/',MCMCloops=10000,COLFIT='gr',/VERBOSE
;
; === running on perfect FG stars ===
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_grfit110209gauss.txt',PDFdir='PDFs_FGstars_grfit110209gauss/',MCMCloops=10000,COLFIT='gauss',/VERBOSE

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_grfitgaussSIG1.txt',PDFdir='PDFs_FGstars_grfitgaussSIG1/',MCMCloops=10000,COLFIT='gauss',/verbose

; === running 500 FG stars with var put in ===
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_datvarfit110215.txt',PDFdir='PDFs_FGstars_datvarfit110215/',MCMCloops=10000,COLFIT='datvar',DMAG=1.0,/VERBOSE

; --- looping over DMAG ---
; IDL> for ii=0,10 do begin & DMin = ii/10. & slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_datvarfit_wDMAG'+strtrim(DMin,2)+'.txt',PDFdir='PDFs_FGstars_datvarfit2/',MCMCloops=10000,COLFIT='datvar',DMAG=DMin,/VERBOSE & endfor

; --- looping over DMAG ---
; IDL> for ii=0,10 do begin & DMin = ii/10. & slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_gaussvarfit_wDMAG'+strtrim(DMin,2)+'.txt',PDFdir='PDFs_FGstars_gaussvarfit2/',MCMCloops=10000,COLFIT='gaussvar',DMAG=DMin,/VERBOSE & endfor


; IDL> for ii=0,10 do begin & DMin = ii/50. & slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_datvarfit_wDMAG'+strtrim(DMin,2)+'_1year.txt',PDFdir='PDFs_FGstars_datvarfit3/',MCMCloops=10000,COLFIT='datvar',DMAG=DMin,/VERBOSE & endfor


; IDL> for ii=0,10 do begin & DMin = ii/50. & slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','slopeinvestigation_output_FGstars_gaussvarfit_wDMAG'+strtrim(DMin,2)+'_1year.txt',PDFdir='PDFs_FGstars_gaussvarfit3/',MCMCloops=10000,COLFIT='gaussvar',DMAG=DMin,/VERBOSE & endfor





; === running all RRL ===
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/RRL_sesar_etal2009/sesar09RRL_sorted.fits','slopeinvestigation_output_RRL_grfit110215.txt',PDFdir='PDFs_RRL_grfit110215/',MCMCloops=10000,COLFIT='gr',/VERBOSE

; testing shifting
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','slopeinvestigation_output_top10_shift20_grfit.txt',PDFdir='PDFs_top10shift20_grfit/',MCMCloops=10000,COLFIT='gr'
;


; === running Jo Bovy's FG stars ===
; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/bovyFGstars.fits','bovyFGstars_grfit110608.txt',PDFdir='PDFs_bovyFGstars_grfit110608/',MCMCloops=10000,COLFIT='gr',/VERBOSE

; IDL> slopeinvestigation,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/bovyFGstars.fits','bovyFGstars_grfit110608_top500.txt',PDFdir='PDFs_bovyFGstars_grfit110608_top500/',MCMCloops=10000,COLFIT='gr',/VERBOSE

;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-25  started by K. B. Schmidt (MPIA)
; 2010-11-29  seasons keyword added. K. B. Schmidt (MPIA)
; 2011-01-05  COLFIT & REPICK keywords added. K. B. Schmidt (MPIA)
; 2011-02-09  Added 'gauss' COLFIT option. K. B. Schmidt (MPIA)
; 2011-02-15  Added 'datvar' COLFIT option. K. B. Schmidt (MPIA)
; 2011-03-23  Added 'datvarui' COLFIT option. K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ linearfitMCMC.pro
@ slopeinvestigationPLOT.pro  ; if /plot keyword set
@ repickdata.pro
@ namedate.pro
;----------------------------
;-
PRO slopeinvestigation,datafile,outputfile,EPS=EPS,VERBOSE=VERBOSE,MCMCloops=MCMCloops,PLOT=PLOT,PDFdir=PDFdir,SEASONS=SEASONS,SKIPALLFIT=SKIPALLFIT,Nepochmin=Nepochmin,COLFIT=COLFIT,REPICK=REPICK,DMAG=DMAG
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

print,':: slopeinvestigation.pro ::  START   ',systime( )

Ofile = n_elements(OUTPUTFILE)
PD    = n_elements(PDFdir)
PL    = n_elements(PLOT)
SEA   = n_elements(SEASONS)
SA    = n_elements(SKIPALLFIT)
NEP   = n_elements(Nepochmin)
PS    = n_elements(EPS)
RP    = n_elements(REPICK)
VB    = n_elements(VERBOSE)
MC    = n_elements(MCMCloops)

if MC eq 0 then MCMCloops = 1000

data    = mrdfits(datafile,1)
;data    = data(0:679)                  ; only selecting part of the
;data (top 10 of FGstars)
;data    = data(0:3088)                 ; only selecting part of the data (top 50 of FGstars)
;data    = data(0:6192)                 ; only selecting part of the data (top 100 of FGstars)
;data    = data(0:28805)                ; only selecting part of the data (top 500 of (pre-cleaned) QSO sample)
data    = data(0:27683)                ; only selecting part of the data (top 500 of bovy's FGstars - where objects with Nepoch<10 is removed)


Uent    = uniq(data.headobjid)
HEADIDs = data(Uent).headobjid
zz      = data(Uent).z
Nobj    = n_elements(HEADIDs)

if vb eq 1 then print,':: slopeinvestigation.pro :: Number of objects to run for ',strtrim(Nobj,2)

if colfit eq 'gauss' or colfit eq 'datvar' or colfit eq 'datvarui' or colfit eq 'gaussvar' then dataNEW = data  ; creating new array to be filled with gauss data

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
if SA eq 0 then begin ; only do fit of full data set if SKIPALLFIT is not set
   if Ofile eq 0 then begin 
      outputfile='slopeinvestigation_output.txt'  ; setting default name if none given
      if vb eq 1 then print,':: slopeinvestigation.pro :: No output name given - using default name: ',strtrim(outputfile,2)
   endif else begin
      if vb eq 1 then print,':: slopeinvestigation.pro :: Output will be written to: ',strtrim(outputfile,2)
   endelse

   openw,33,outputfile,width=500
   printf,33,'# File containing the output from slopeinvestigation.pro created on ',strtrim(systime( ),2)
   printf,33,'# Each linear fit parameters were obtained from ',strtrim(MCMCloops,2),' linear MCMC fits.'
   printf,33,'# The columns are (see linearfitMCMC.pro for more detail):'
   printf,33,'# objid   abest   amed   amean   aplus68   aminus68   aplus95   aminus95   bbest   bmed   bmean   bplus68   bminus68   bplus95   bminus95   z      season'

;   for ii=8385,Nobj-1 do begin
   for ii=0,Nobj-1 do begin
      objent   = where(data.headobjid eq HEADIDs(ii) and data.PSFmag_g ne -9999 and data.PSFmag_r ne -9999)  ; entries for object
      if PD eq 0 then PDFdir = './'
      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      CASE COLFIT OF   ; determining which magnitude space to fit in
         'ug': BEGIN
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in ug magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_ugfit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_u],[data(objent).PSFmag_g],[data(objent).PSFmagerr_u],[data(objent).PSFmagerr_g]] ; create input array
         END
         'gr': BEGIN
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in gr magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_grfit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_g],[data(objent).PSFmag_r],[data(objent).PSFmagerr_g],[data(objent).PSFmagerr_r]] ; create input array
         END
         'ri': BEGIN
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in ri magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_rifit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_r],[data(objent).PSFmag_i],[data(objent).PSFmagerr_r],[data(objent).PSFmagerr_i]] ; create input array
         END
         'gi': BEGIN
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in gi magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_gifit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_g],[data(objent).PSFmag_i],[data(objent).PSFmagerr_g],[data(objent).PSFmagerr_i]] ; create input array
         END
         'ui': BEGIN
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in ui magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_uifit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_u],[data(objent).PSFmag_i],[data(objent).PSFmagerr_u],[data(objent).PSFmagerr_i]] ; create input array
         END
         'rg': BEGIN  ; testing oppsite axis give same (inverse) slopes
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in rg magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_rgfit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_r],[data(objent).PSFmag_g],[data(objent).PSFmagerr_r],[data(objent).PSFmagerr_g]] ; create input array
         END
         'gauss': BEGIN  ; "perfectly non-varying data" drawn from gaussina distribution
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit for gaussian drawn magnitudes for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_gaussfit.dat' ; setting path to PDFs
            Nepochs = n_elements(objent)                                           ; the number of points to draw
            ; X-component
            GSIGMAX                          = median(data(objent).PSFmagerr_g)    ; sigma of gaussian distribution
            MEANX                            = median(data(objent).PSFmag_g)       ; mean of gaussian distribution
            GAUSSX                           = RANDOMN(seed,Nepochs)               ; random gaussian point drawn from gauss w. mean 0 and sigma 1
            dataNEW(objent).PSFmag_g         = GAUSSX * GSIGMAX + MEANX            ; new x components
            dataNEW(objent).PSFmagerr_g      = fltarr(Nepochs)+GSIGMAX             ; new error bars
            ; Y-component
            GSIGMAY                          = median(data(objent).PSFmagerr_r)    ; sigma of gaussian distribution
            MEANY                            = median(data(objent).PSFmag_r)       ; mean of gaussian distribution
            GAUSSY                           = RANDOMN(seed,Nepochs)               ; random gaussian point drawn from gauss w. mean 0 and sigma 1 
            dataNEW(objent).PSFmag_r         = GAUSSY * GSIGMAY + MEANY            ; new y components
            dataNEW(objent).PSFmagerr_r      = fltarr(Nepochs)+GSIGMAY             ; new error bars

            DATARRAY = [[dataNEW(objent).PSFmag_g],[dataNEW(objent).PSFmag_r],[dataNEW(objent).PSFmagerr_g],[dataNEW(objent).PSFmagerr_r]] ; create input array
         END
         'datvar': BEGIN  ; data (preferable non-varying) with linear variability added
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit for magnitudes with linear variability added for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_datvarfit_wDMAG'+strtrim(strjoin(strsplit(DMAG,'.',/extract),'p'),2)+'.dat'  ; setting path to PDFs
            Mindate = min(data(objent).MJD_g)                                                                ; MJD of first observation
            if n_elements(Dmag) eq 0 then Dmag = 0.5                                                         ; default amount of variability to put in (mag/year)
            dataNEW(objent).psfmag_g = data(objent).psfmag_g + Dmag*(data(objent).MJD_g-Mindate)/(1*365.25)  ; adding variability (on a 1 year timescale)
            dataNEW(objent).psfmag_r = data(objent).psfmag_r + Dmag*(data(objent).MJD_g-Mindate)/(1*365.25)  ; adding variability (on a 1 year timescale)

            DATARRAY = [[dataNEW(objent).PSFmag_g],[dataNEW(objent).PSFmag_r],[dataNEW(objent).PSFmagerr_g],[dataNEW(objent).PSFmagerr_r]] ; create input array
         END
         'datvarui': BEGIN  ; data (preferable non-varying) with linear variability added (ui space)
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit for magnitudes with linear variability added for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_datvarfitui_wDMAG'+strtrim(strjoin(strsplit(DMAG,'.',/extract),'p'),2)+'.dat'  ; setting path to PDFs
            Mindate = min(data(objent).MJD_u)                                                                ; MJD of first observation
            if n_elements(Dmag) eq 0 then Dmag = 0.5                                                         ; default amount of variability to put in (mag/year)
            dataNEW(objent).psfmag_u = data(objent).psfmag_u + Dmag*(data(objent).MJD_u-Mindate)/(1*365.25)  ; adding variability (on a 1 year timescale)
            dataNEW(objent).psfmag_i = data(objent).psfmag_i + Dmag*(data(objent).MJD_u-Mindate)/(1*365.25)  ; adding variability (on a 1 year timescale)

            DATARRAY = [[dataNEW(objent).PSFmag_u],[dataNEW(objent).PSFmag_i],[dataNEW(objent).PSFmagerr_u],[dataNEW(objent).PSFmagerr_i]] ; create input array
         END
         'gaussvar': BEGIN  ; data (preferable non-varying) with gaussian white noise variability added
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit for magnitudes with gaussian variability added for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_gaussvarfit_wDMAG'+strtrim(strjoin(strsplit(DMAG,'.',/extract),'p'),2)+'.dat'  ; setting path to PDFs
            Mindate = min(data(objent).MJD_g)                                        ; MJD of first observation
            if n_elements(Dmag) eq 0 then Dmag = 0.5                                 ; default amount of variability to put in (sigma of gauss dist.)
            Nepochs = n_elements(objent)                                             ; the number of points to draw
            GAUSSg                          = RANDOMN(seed,Nepochs)                  ; random gaussian point drawn from gauss w. mean 0 and sigma 1
            ;GAUSSr                          = RANDOMN(seed,Nepochs)                  ; random gaussian point drawn from gauss w. mean 0 and sigma 1
            dataNEW(objent).psfmag_g = data(objent).psfmag_g + GAUSSg * Dmag         ; adding variability (gaussian/white noise)
            dataNEW(objent).psfmag_r = data(objent).psfmag_r + GAUSSg * Dmag         ; adding variability (gaussian/white noise)

            DATARRAY = [[dataNEW(objent).PSFmag_g],[dataNEW(objent).PSFmag_r],[dataNEW(objent).PSFmagerr_g],[dataNEW(objent).PSFmagerr_r]] ; create input array
         END
         ELSE: BEGIN
            if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in default (gr) magnitude space for object ',strtrim(ii,2)
            outPDF = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_grfit.dat'  ; setting path to PDFs
            DATARRAY = [[data(objent).PSFmag_g],[data(objent).PSFmag_r],[data(objent).PSFmagerr_g],[data(objent).PSFmagerr_r]] ; create input array
         END
      ENDCASE
      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      if RP eq 1 then begin  ; re-picking the data from gaussian distributions with sigma=REPICK
         if VB eq 1 then print,':: slopeinvestigation.pro ::  Re-picking data from gaussian distribution(s) with sigma=',strtrim(REPICK,2)
         repickdata,datarray,REPICK,datarrayOUT;,/VERBOSE
         DATARRAY = DATARRAYOUT
      endif
      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      linearfitMCMC,DATARRAY,RESULT,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT  ; the one used!
      ;linearfitMCMC,DATARRAY,RESULT,Nmcmc=MCMCloops,OUTPUT=outPDF,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,1.0,1.0,0.1,0.1,0.005],/CLIP,/SHIFT,/VERBOSE;,/SIGMAADJUST;,/INITGUESSALT;,/SIGMAADJUST,/PLOT;xxxx
      seasonNO = 0. ; the season's number - only relevant for seasonal fits below
      printf,33,format=FMT,HEADIDs(ii),RESULT(0),RESULT(1),RESULT(2),RESULT(3),RESULT(4),RESULT(5),RESULT(6),RESULT(7),RESULT(8),RESULT(9),RESULT(10),RESULT(11),RESULT(12),RESULT(13),zz(ii),seasonNO   ; writing the result from the linear fit to file
   endfor

   close,33
   if PL eq 1 then begin 
      slopeinvestigationPLOT,outputfile,/VERBOSE;,/EPS
   endif
endif
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

if SEA eq 1 then begin   ; performing the fit for the individual seasons


   if Ofile eq 0 then begin 
      outputfileSEA='slopeinvestigation_output_seasons.txt'  ; setting default name if none given
      if vb eq 1 then print,':: slopeinvestigation.pro :: No output name given - using default name: ',strtrim(outputfile,2)
   endif else begin
      base          = strsplit(outputfile,'.',/extract)
      if n_elements(base) gt 2 then print,':: slopeinvestigation.pro :: Dots in the given output name should be avoided...'
      outputfileSEA = strtrim(base(0),2)+'_seasons.'+strtrim(base(1),2)
      if vb eq 1 then print,':: slopeinvestigation.pro :: Season output will be written to: ',strtrim(outputfileSEA,2)
   endelse

   ; the seasons to divide data into:
   MJD99 = 51330   ; MJD for June 1st 1999
   SBIN  = [MJD99,MJD99+365,MJD99+2*365,MJD99+3*365,MJD99+4*365,MJD99+5*365,MJD99+6*365,MJD99+7*365,MJD99+8*365,MJD99+9*365]
   Nseasons = N_elements(SBIN)-1
 
   if NEP eq 0 then Nepochmin = 5 ; setting the minimum number of points in seasons to fit

   openw,44,outputfileSEA,width=500
   printf,44,'# File containing the output from slopeinvestigation.pro created on ',strtrim(systime( ),2)
   printf,44,'# Each linear fit parameters were obtained from ',strtrim(MCMCloops,2),' linear MCMC fits.'
   printf,44,'# The seasons were only fit if they contained at least ',strtrim(Nepochmin,2),' epochs.'
   printf,44,'# The columns are (see linearfitMCMC.pro for more detail):'
   printf,44,'# objid   abest   amed   amean   aplus68   aminus68   aplus95   aminus95   bbest   bmed   bmean   bplus68   bminus68   bplus95   bminus95   z   season'

;   for ii=8385,Nobj-1 do begin
   for ii=0,Nobj-1 do begin
      for jj=0,Nseasons-1 do begin   ; looping over seasons
         objentS = where(data.headobjid eq HEADIDs(ii) and data.PSFmag_g ne -9999 and data.PSFmag_r ne -9999 and data.mjd_g gt SBIN(jj) and data.mjd_g lt SBIN(jj+1))  ; entries for season
         if n_elements(objentS) ge Nepochmin then begin  ; cheking that there is enough epochs to fit to
            if PD eq 0 then PDFdir = './'
            ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            CASE COLFIT OF   ; determining which magnitude space to fit in
               'ug': BEGIN
                  if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in ug magnitude space for object ',strtrim(ii,2),' season ',strtrim(jj,2)
                  outPDFs = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'season'+strtrim(jj,2)+'_ugfit.dat'  ; setting path to PDFs
                  DATARRAYs = [[data(objentS).PSFmag_u],[data(objentS).PSFmag_g],[data(objentS).PSFmagerr_u],[data(objentS).PSFmagerr_g]] ; create input array
               END
               'gr': BEGIN
                  if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in gr magnitude space for object ',strtrim(ii,2),' season ',strtrim(jj,2)
                  outPDFs = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'season'+strtrim(jj,2)+'_grfit.dat'  ; setting path to PDFs
                  DATARRAYs = [[data(objentS).PSFmag_g],[data(objentS).PSFmag_r],[data(objentS).PSFmagerr_g],[data(objentS).PSFmagerr_r]] ; create input array
               END
               'ri': BEGIN
                  if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in ri magnitude space for object ',strtrim(ii,2),' season ',strtrim(jj,2)
                  outPDFs = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'season'+strtrim(jj,2)+'_rifit.dat'  ; setting path to PDFs
                  DATARRAYs = [[data(objentS).PSFmag_r],[data(objentS).PSFmag_i],[data(objentS).PSFmagerr_r],[data(objentS).PSFmagerr_i]] ; create input array
               END
               'gi': BEGIN
                  if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in gi magnitude space for object ',strtrim(ii,2),' season ',strtrim(jj,2)
                  outPDFs = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'season'+strtrim(jj,2)+'_gifit.dat'  ; setting path to PDFs
                  DATARRAYs = [[data(objentS).PSFmag_g],[data(objentS).PSFmag_i],[data(objentS).PSFmagerr_g],[data(objentS).PSFmagerr_i]] ; create input array
               END
               ELSE: BEGIN
                  if VB eq 1 then print,':: slopeinvestigation.pro :: Runnning fit in default (gr) magnitude space for object ',strtrim(ii,2),' season ',strtrim(jj,2)
                  outPDFs = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'season'+strtrim(jj,2)+'_grfit.dat'  ; setting path to PDFs
                  DATARRAYs = [[data(objentS).PSFmag_g],[data(objentS).PSFmag_r],[data(objentS).PSFmagerr_g],[data(objentS).PSFmagerr_r]] ; create input array
               END
            ENDCASE
            ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            if RP eq 1 then begin  ; re-picking the data from gaussian distributions with sigma=REPICK
               if VB eq 1 then print,':: slopeinvestigation.pro ::  Re-picking data from gaussian distribution(s) with sigma=',strtrim(REPICK,2)
               repickdata,datarrays,REPICK,datarrayOUTs;,/VERBOSE
               DATARRAYs = DATARRAYOUTs
            endif
            ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            linearfitMCMC,DATARRAYs,RESULT,Nmcmc=MCMCloops,OUTPUT=outPDFs,/OUTLIERS,SIGMAS=[0.2,!pi/50.,0.05,0.5,1.0,0.05,0.1,0.005],/CLIP,/SHIFT,/SIGMAADJUST,/VERBOSE;,/INITGUESSALT,/PLOT
            seasonNO = jj+1. ; the season's number
            printf,44,format=FMT,HEADIDs(ii),RESULT(0),RESULT(1),RESULT(2),RESULT(3),RESULT(4),RESULT(5),RESULT(6),RESULT(7),RESULT(8),RESULT(9),RESULT(10),RESULT(11),RESULT(12),RESULT(13),zz(ii),seasonNO   ; writing the result from the linear fit to file
         endif
      endfor
   endfor

   close,44

   if PL eq 1 then begin 
      slopeinvestigationPLOT,outputfile,/VERBOSE;,/EPS
   endif

endif
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

if colfit eq 'gauss' or colfit eq 'datvar' or colfit eq 'gaussvar' then begin  ; create file name to save gauss data to
   basename  = strsplit(datafile,'.',/extract)
   namedate,datafile,path,name,extension,date,dateus,/local  
   if colfit eq 'datvar'   then   gaussfile = basename(0)+'_datvardata_wDMAG'+strtrim(DMAG,2)+'_'+strtrim(dateus,2)+'.fits'
   if colfit eq 'gaussvar' then   gaussfile = basename(0)+'_gaussvardata_wDMAG'+strtrim(DMAG,2)+'_'+strtrim(dateus,2)+'.fits'
   if colfit eq 'gauss'    then   gaussfile = basename(0)+'_gaussdata_'+strtrim(dateus,2)+'.fits'
   if VB eq 1 then print,':: slopeinvestigation.pro :: Writing the gaussian picked data to ',strtrim(gaussfile,2)
   mwrfits, dataNEW,gaussfile, /create 
endif

if vb eq 1 then print,' '
if vb eq 1 then print,':: slopeinvestigation.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
print,':: slopeinvestigation.pro ::   END    ',systime( )
;stop
END
