;+
;----------------------------
;   NAME
;----------------------------
; SDSSepochsfits2averagefits.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure turning a SDSS fits file with information on objects and
; their individual Stripe 82 epochs into a fits file the the average
; magnitudes etc.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; epochfile        : string containing name and path of input data file
;                    File must contain (at least) the columns:
;                       HEADOBJID*
;                       INDEX
;                       OBJID
;                       PRIORITYID
;                       RA
;                       DEC
;                       DATE_OBSERVER
;                       PSFMAG_U
;                       PSFMAG_G
;                       PSFMAG_R
;                       PSFMAG_I
;                       PSFMAG_Z
;                       PSFMAGERR_U
;                       PSFMAGERR_G
;                       PSFMAGERR_R
;                       PSFMAGERR_I
;                       PSFMAGERR_Z
;                       MODELMAG_U
;                       MODELMAG_G
;                       MODELMAG_R
;                       MODELMAG_I
;                       MODELMAG_Z
;                       MODELMAGERR_U
;                       MODELMAGERR_G
;                       MODELMAGERR_R
;                       MODELMAGERR_I
;                       MODELMAGERR_Z
;                       DEREDPSFMAG_U
;                       DEREDPSFMAG_G
;                       DEREDPSFMAG_R
;                       DEREDPSFMAG_I
;                       DEREDPSFMAG_Z
;                    *column not in output file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : New Fits file containing SDSS objects, same format
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> SDSSepochsfits2averagefits,'/Users/kasperborelloschmidt/work/observing/101210_GRONDP86/prepHSTprop120223/top25obslenscand_fullepochmatch.fits',/VERBOSE,/STP
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2012-02-23  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
;@ xxx.pro
;----------------------------
;-
PRO SDSSepochsfits2averagefits,epochfile,outputfile,EPS=EPS,VERBOSE=VERBOSE,STP=STP

VB = n_elements(VERBOSE)

DE = mrdfits(epochfile,1)

Uent = uniq(DE.headobjid)
Nobj = n_elements(Uent)
if vb eq 1 then print,':: SDSSepochsfits2averagefits.pro :: Found ',trim(Nobj),' unique objects in the input file.'

; creating output structure
OUTstr={INDEX:0,OBJID:' ',PRIORITYID:' ',RA:0.0d,DEC:0.0d,DATE_OBSERVER:' ',Z:0.0d,PSFMAG_U:0.0d,PSFMAG_G:0.0d,PSFMAG_R:0.0d,PSFMAG_I:0.0d,PSFMAG_Z:0.0d,PSFMAGERR_U:0.0d,PSFMAGERR_G:0.0d,PSFMAGERR_R:0.0d,PSFMAGERR_I:0.0d,PSFMAGERR_Z:0.0d,MODELMAG_U:0.0d,MODELMAG_G:0.0d,MODELMAG_R:0.0d,MODELMAG_I:0.0d,MODELMAG_Z:0.0d,MODELMAGERR_U:0.0d,MODELMAGERR_G:0.0d,MODELMAGERR_R:0.0d,MODELMAGERR_I:0.0d,MODELMAGERR_Z:0.0d,DEREDPSFMAG_U:0.0d,DEREDPSFMAG_G:0.0d,DEREDPSFMAG_R:0.0d,DEREDPSFMAG_I:0.0d,DEREDPSFMAG_Z:0.0d}
OUTstr=replicate(OUTstr,Nobj)

for ii=0,Nobj-1 do begin    ; looping over objects and filling output structure
   objENT  = where(DE.objid eq DE(Uent(ii)).HEADOBJID,Nobjent)  ; entries for ii'th object
   DEsub = DE(objENT)                                 ; sub array only containing objects entries
   goodENT = where(DEsub.psfmag_u ne -9999 and DEsub.psfmag_g ne -9999 and DEsub.psfmag_r ne -9999 and DEsub.psfmag_i ne -9999 and DEsub.psfmag_z ne -9999, Ngood)
   if Ngood eq [-1] then begin
      print,':: SDSSepochsfits2averagefits.pro :: Object ',trim(DE(ii).headobjid),"'s epoch ",trim(DE(ii).objid),' had no good entries (all outliers in min 1 band)'
   endif else begin
      outstr(ii).INDEX =            DEsub(ii).INDEX
      outstr(ii).OBJID =            DEsub(ii).OBJID
      outstr(ii).PRIORITYID =       DEsub(ii).PRIORITYID
      outstr(ii).RA =               DEsub(ii).RA
      outstr(ii).DEC =              DEsub(ii).DEC
      outstr(ii).DATE_OBSERVER =    DEsub(ii).DATE_OBSERVER
      zent = where(DEsub.z ne -1)
      if zent eq [-1] then zent = ii
      outstr(ii).Z =                DEsub(zent).Z

      outstr(ii).PSFMAG_U =         mean(DEsub(goodent).PSFMAG_U)
      outstr(ii).PSFMAG_G =         mean(DEsub(goodent).PSFMAG_G)
      outstr(ii).PSFMAG_R =         mean(DEsub(goodent).PSFMAG_R)
      outstr(ii).PSFMAG_I =         mean(DEsub(goodent).PSFMAG_I)
      outstr(ii).PSFMAG_Z =         mean(DEsub(goodent).PSFMAG_Z)
      outstr(ii).PSFMAGERR_U =      mean(DEsub(goodent).PSFMAGERR_U)
      outstr(ii).PSFMAGERR_G =      mean(DEsub(goodent).PSFMAGERR_G)
      outstr(ii).PSFMAGERR_R =      mean(DEsub(goodent).PSFMAGERR_R)
      outstr(ii).PSFMAGERR_I =      mean(DEsub(goodent).PSFMAGERR_I)
      outstr(ii).PSFMAGERR_Z =      mean(DEsub(goodent).PSFMAGERR_Z)
      outstr(ii).MODELMAG_U =       mean(DEsub(goodent).MODELMAG_U)
      outstr(ii).MODELMAG_G =       mean(DEsub(goodent).MODELMAG_G)
      outstr(ii).MODELMAG_R =       mean(DEsub(goodent).MODELMAG_R)
      outstr(ii).MODELMAG_I =       mean(DEsub(goodent).MODELMAG_I)
      outstr(ii).MODELMAG_Z =       mean(DEsub(goodent).MODELMAG_Z)
      outstr(ii).MODELMAGERR_U =    mean(DEsub(goodent).MODELMAGERR_U)
      outstr(ii).MODELMAGERR_G =    mean(DEsub(goodent).MODELMAGERR_G)
      outstr(ii).MODELMAGERR_R =    mean(DEsub(goodent).MODELMAGERR_R)
      outstr(ii).MODELMAGERR_I =    mean(DEsub(goodent).MODELMAGERR_I)
      outstr(ii).MODELMAGERR_Z =    mean(DEsub(goodent).MODELMAGERR_Z)
      outstr(ii).DEREDPSFMAG_U =    mean(DEsub(goodent).DEREDPSFMAG_U)
      outstr(ii).DEREDPSFMAG_G =    mean(DEsub(goodent).DEREDPSFMAG_G)
      outstr(ii).DEREDPSFMAG_R =    mean(DEsub(goodent).DEREDPSFMAG_R)
      outstr(ii).DEREDPSFMAG_I =    mean(DEsub(goodent).DEREDPSFMAG_I)
      outstr(ii).DEREDPSFMAG_Z =    mean(DEsub(goodent).DEREDPSFMAG_Z)
   endelse
endfor

NameBase   = strsplit(epochfile,'.',/extract)
outputfile = NameBase(0)+'_epochs2averageOUT.fits'

;---WRITING THE STRUCTURE TO A FITSFILE---
mwrfits, OUTstr, outputfile, /create

if vb eq 1 then print,':: SDSSepochsfits2averagefits.pro :: The output has been put in:'
if vb eq 1 then print,'                                 ',trim(outputfile)


if vb eq 1 then print,' '
if vb eq 1 then print,':: SDSSepochsfits2averagefits.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
