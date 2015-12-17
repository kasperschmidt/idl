;+
;----------------------------
;   NAME
;----------------------------
; correctMCMCerrorsInCat.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure correcting the false error estimates from the
; linearfitMCMC.pro code in the catalogs created by
; create_slopeANDshencat.pro via the directory containing the
; individual PDF files.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; shencat         : catalog in which the errors need to be corrected
; PDFdir          : directory with PDFs for the objects in the catalog
; FIT             : string with the fit the input catalog and PDFdir correspond
;                   to, e.g. 'gr' or 'ui'
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; newcat          : New Fits file containing the shencat but with MCMC
;                   parameter errors corrected according to the PDFs
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> correctMCMCerrorsInCat,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/slopeANDshencatThu_Feb_3_163401_2011_season0.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_S82QSOs_shenmatchALL_grfit110129/','gr',newcat,/VERBOSE

; IDL> correctMCMCerrorsInCat,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/slopeANDshencatThu_Feb_3_163447_2011_season0.fits','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/PDFs_S82QSOs_shenmatchALL_uifit110129/','ui',newcat,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-04-05  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ getPDFforOBJID.pro
@ namedate.pro
;----------------------------
;-
PRO correctMCMCerrorsInCat,shencat,PDFdir,fit,newcat,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)

Incat = mrdfits(shencat,1)
Nobj  = n_elements(Incat.HEADOBJID)


for ii=0,Nobj-1 do begin  ; looping over objects
   IDii    = Incat(ii).HEADOBJID
   PDFfile = strtrim(PDFdir,2)+'PDFs_object'+strtrim(ii,2)+'_'+trim(FIT)+'fit.dat'

   ; --- Reading repick ASCII file ---
   Nrows        = File_lines(PDFfile)               ; number of rows in file
   Nheaderlines = 6                                 ; number of header lines
   Ncol         = 9                                 ; number of columns in file
   PDFarr       = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
   openr,lun,PDFfile, /GET_LUN                      ; open file for reading     
   header = STRARR(Nheaderlines)                    ; string array for header
   readf,lun,header                                 ; reading header into string array
   readf,lun,PDFarr                                 ; reading data into array
   free_lun,lun  
   ; ----------------------------------

   Nlines = Nrows-Nheaderlines
   bb     = PDFarr(0,*)
   mm     = PDFarr(1,*) 
   KlnL   = PDFarr(2,*)

   ent     = where(KlnL eq min(KlnL))
   bestent = ent(0)  ; entry of fit with lowest 'chi^2'

   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   ; calculating mean and median values plus confidence intervals
   RESULT     = fltarr(14)
   ; entries for plus and minus confidence intervals
   ent68m     = round(Nlines*0.16)
   ent68p     = round(Nlines*0.84)
   ent95m     = round(Nlines*0.025)
   ent95p     = round(Nlines*0.975)

   if ent95p eq Nlines then ent95p = Nlines-1

   RESULT(0)  = mm(bestent)                          ; the value with lowest LnL
   RESULT(1)  = median(mm)                           ; the median
   RESULT(2)  = mean(mm)                             ; the mean
   msort      = mm(sort(mm))                         ; sorting a values
   RESULT(3)  = RESULT(1)-msort(ent68m)
   RESULT(4)  = msort(ent68p)-RESULT(1)
   RESULT(5)  = RESULT(1)-msort(ent95m)
   RESULT(6)  = msort(ent95p)-RESULT(1)

   RESULT(7)  = bb(bestent)                          ; the value with lowest LnL
   RESULT(8)  = median(bb)                           ; the median
   RESULT(9)  = mean(bb)                             ; the mean
   bsort      = bb(sort(bb))                         ; sorting b values
   RESULT(10) = RESULT(8)-bsort(ent68m)
   RESULT(11) = bsort(ent68p)-RESULT(8)
   RESULT(12) = RESULT(8)-bsort(ent95m)
   RESULT(13) = bsort(ent95p)-RESULT(8)
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

   Dabest = abs(RESULT(0)-Incat(ii).Abest)
   if abs(RESULT(0)) lt 1 then aDiff = 1e-5
   if abs(RESULT(0)) gt 1 and abs(RESULT(0)) lt 10 then aDiff = 1e-4
   if abs(RESULT(0)) gt 10 and abs(RESULT(0)) lt 100 then aDiff = 1e-3

   if Dabest gt aDiff then begin  ; checking that slope values agree
      print,':: correctMCMCerrorsInCat.pro :: The slope of object number ',trim(ii),' (',strtrim(IDii,2),') does not agree with the PDF one'
      print,'                                     value in input cat  : ',Incat(ii).Abest
      print,'                                     value in from PDF   : ',RESULT(0)
      stop
   endif

   Dbbest = abs(RESULT(7)-Incat(ii).Bbest)
   if abs(RESULT(7)) lt 1 then bDiff = 1e-5
   if abs(RESULT(7)) gt 1 and abs(RESULT(7)) lt 10 then bDiff = 1e-4
   if abs(RESULT(7)) gt 10 and abs(RESULT(7)) lt 100 then bDiff = 1e-3
   if abs(RESULT(7)) gt 100 and abs(RESULT(7)) lt 1000 then bDiff = 1e-2

   if Dbbest gt bDiff then begin  ; checking that the offset values agrees
      print,':: correctMCMCerrorsInCat.pro :: The offset of object number ',trim(ii),' (',strtrim(IDii,2),') does not agree with the PDF one' 
      print,'                                     value in input cat  : ',Incat(ii).Bbest
      print,'                                     value in from PDF   : ',RESULT(7)
      stop
   endif

   ; overwriting errors in catalog
   Incat(ii).Aminus68 = RESULT(3)
   Incat(ii).Aplus68  = RESULT(4)
   Incat(ii).Aminus95 = RESULT(5)
   Incat(ii).Aplus95  = RESULT(6)

   Incat(ii).Bminus68 = RESULT(10)
   Incat(ii).Bplus68  = RESULT(11)
   Incat(ii).Bminus95 = RESULT(12)
   Incat(ii).Bplus95  = RESULT(13)

endfor

; writing new catalog with corrected errors
namedate,shencat,path,name,extension,date,dateus,/local
newcat = '/'+path+'/'+name+'_ERRcorrected.'+extension  ; creating name of new catalog
mwrfits, Incat, newcat, /create
if vb eq 1 then print,':: correctMCMCerrorsInCat.pro :: Wrote corrected catalog to: ',strtrim(newcat,2)


if vb eq 1 then print,' '
if vb eq 1 then print,':: correctMCMCerrorsInCat.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
