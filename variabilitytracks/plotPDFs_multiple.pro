;+
;----------------------------
;   NAME
;----------------------------
; plotPDFs_multiple.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure for running the plotPDFs.pro for mutiple objects
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; Objectdata      : string containing name and path of file with object data
; PDFdir          : Directory where the PDF files are located
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
; --- TOP 19 ---
; IDL> plotPDFs_multiple,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_11_09:58:09_2010.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_top19_outliers9999/',/VERBOSE,/SEASONS


; IDL> plotPDFs_multiple,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_top10QSO/',/VERBOSE,/SEASONS

; IDL> plotPDFs_multiple,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_top10QSO_N2/',/VERBOSE,/SEASONS

; IDL> plotPDFs_multiple,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_S82QSOs_shenmatchALL_grfit110129/',/VERBOSE

; --- perfect FGstars ---
; IDL> plotPDFs_multiple,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_FGstars_grfit110207/',/VERBOSE

; IDL> plotPDFs_multiple,'DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits','PDFs_S82QSOs_shenmatchALL_grfit110129/',/VERBOSE

;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-12-01  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ plotPDFs.pro
;----------------------------
;-
PRO plotPDFs_multiple,objectdata,PDFdir,EPS=EPS,VERBOSE=VERBOSE,SEASONS=SEASONS

SEA = n_elements(SEASONS)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)

str  = mrdfits(objectdata,1)   ; reding data
Uent = uniq(str.headobjid)
Nobj = n_elements(Uent)
HIDs = str(Uent).headobjid


;for ii=0L,Nobj-1 do begin   ; looping over objects
for ii=0L,0 do begin   ; looping over objects
   ;=====================================================================================================
   ent = where(str.headobjid eq HIDs(ii) and str.PSFMAG_G ne -9999 and str.PSFmag_R ne -9999,Nobs)   ; the entries for the object (-9999 values are already taken into account)

   Xvar  = str(ent).PSFMAG_G
   Yvar  = str(ent).PSFMAG_R
   DXvar = str(ent).PSFMAGERR_G
   DYvar = str(ent).PSFMAGERR_R
   COVAR = fltarr(Nobs) ; Co variance entries
   dataarr = [[Xvar],[Yvar],[DXvar],[DYvar],[covar],[covar]]
   PDFfile = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_grfit.dat'

   if PS eq 0 then begin
      plotPDFs,PDFfile,objectdata=dataarr,/OUTLIERS,/ALLINONE,/MEANSHIFT;,/VERBOSE
      wait,0.5
   endif else begin
      epsbase = 'object'+strtrim(ii,2)
      plotPDFs,PDFfile,objectdata=dataarr,/OUTLIERS,EPS=strtrim(epsbase,2),/ALLINONE,/MEANSHIFT;,/VERBOSE
   endelse

   ;=====================================================================================================
   if SEA eq 1 then begin
      ; the seasons to divide data into:
      MJD99 = 51330   ; MJD for June 1st 1999
      SBIN  = [MJD99,MJD99+365,MJD99+2*365,MJD99+3*365,MJD99+4*365,MJD99+5*365,MJD99+6*365,MJD99+7*365,MJD99+8*365,MJD99+9*365]
      Nseasons = N_elements(SBIN)-1

      SPstring = 'ls '+strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'season* | grep -v "\_ALL*" '
      spawn,SPstring,seasonfiles
      Nsea = n_elements(seasonfiles)

      for jj=0L,Nsea-1 do begin  ; looping over seasons
         PDFfile = strtrim(seasonfiles(jj),2)
         dotpos  = STRPOS(PDFfile,'.')
         jjS     = STRMID(PDFfile, dotpos-1, 1)  ; number of the season
         ent     = where(str.headobjid eq HIDs(ii) and str.PSFmag_g ne -9999 and str.PSFmag_r ne -9999 and str.mjd_g gt SBIN(jjS) and str.mjd_g lt SBIN(jjS+1),NobsS)  ; entries for season

         Xvar     = str(ent).PSFMAG_G
         Yvar     = str(ent).PSFMAG_R
         DXvar    = str(ent).PSFMAGERR_G
         DYvar    = str(ent).PSFMAGERR_R
         COVAR    = fltarr(NobsS) ; Co variance entries
         dataarr  = [[Xvar],[Yvar],[DXvar],[DYvar],[covar],[covar]]

         if PS eq 0 then begin
            plotPDFs,PDFfile,objectdata=dataarr,/OUTLIERS,/ALLINONE,/MEANSHIFT;,/VERBOSE
            wait,0.5
         endif else begin
            epsbase = 'object'+strtrim(ii,2)+'season'+strtrim(jjs,2)
            plotPDFs,PDFfile,objectdata=dataarr,/OUTLIERS,EPS=strtrim(epsbase,2),/ALLINONE,/MEANSHIFT;,/VERBOSE
         endelse
      endfor
;   if  ii eq 3 and jj eq 7 then stop ; stopping at particular object
   endif
   ;=====================================================================================================
endfor    ; end of object loop


if vb eq 1 then print,' '
if vb eq 1 then print,':: plotPDFs_multiple.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
