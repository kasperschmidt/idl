;+
;----------------------------
;   NAME
;----------------------------
; create_slopeANDshencat.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure combining the slopeinvestigation.pro (linearfitMCMC.pro)
; output with the catalog values of Shen et al 2010.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : string containing name and path of fits data file
;                   which was used to create the fitoutput file
; fitoutput       : the output file with slopes from slopeinvestigation.pro
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; SEASON          : set SEASON to the number of the seson to write to
;                   output file. Default is SEASON=0, i.e. all seasons
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : New Fits file containing SDSS objects, same format
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> create_slopeANDshencat,'DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits','S82QSOs_shenmatchALL.txt',outputfile,/VERBOSE

; IDL> create_slopeANDshencat,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','top10QSO_rgfit.txt',outputfile,/VERBOSE

; IDL> create_slopeANDshencat,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/top10QSOs_test_1p4z1p6_Agt0p15.fits','top10QSO.txt',outputfile,/VERBOSE


;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-12-10  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO create_slopeANDshencat,datafile,fitoutput,outputfile,SWRITE=SWRITE,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

VB = n_elements(VERBOSE)

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;reading Shen et al 2010 catalog and defining quantities
Shencat  = '/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/dr7_bh_June_2010.fits'
Sshen    = mrdfits(Shencat,1)
RAshen   = Sshen.RA
Decshen  = Sshen.Dec
SDSSname = Sshen.SDSS_NAME
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;reading original (Shen matched) data fits file
Sdat     = mrdfits(datafile,1)
Uent     = uniq(Sdat.headobjid)
IDs      = Sdat(uent).headobjid
RAdat    = Sdat(uent).RA
Decdat   = Sdat(uent).Dec
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;reading slopeinvestigation output file
readcol,fitoutput,format=('A,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f'),objid,abest,amed,amean,aplus68,aminus68,aplus95,aminus95,bbest,bmed,bmean,bplus68,bminus68,bplus95,bminus95,zz,seas;,/silent
if n_elements(SWRITE) eq 0 then SWRITE = 0  ; setting default to all season result
entseason = where(seas eq SWRITE)           ; getting entries of seasons
Nfits     = n_elements(entseason)           ; the number of fits for the given season in file
; selecting data for only the given season 
objid    = objid(entseason)
abest    = abest(entseason)
amed     = amed(entseason)
amean    = amean(entseason)
aplus68  = aplus68(entseason)
aminus68 = aminus68(entseason)
aplus95  = aplus95(entseason)
aminus95 = aminus95(entseason)
bbest    = bbest(entseason)
bmed     = bmed(entseason)
bmean    = bmean(entseason)
bplus68  = bplus68(entseason)
bminus68 = bminus68(entseason)
bplus95  = bplus95(entseason)
bminus95 = bminus95(entseason)
zz       = zz(entseason)
seas     = seas(entseason)

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; reading file with matches (SDSS names) from the Shen et al 2010 catalog
readcol,FORMAT='(A,F,F,A,F,F,I)','mathcQSOs_Thu_Jul_8_132203_2010.dat',ID2,RA2,Dec2,SDSSnameMATCH,RAmatch,Decmatch,NMa;,/silent
UniqMatch     = where(Nma eq 1.,NN)               ; counting unique matches
Nma           = Nma(UniqMatch)                    ; only used selected matches
ID2           = ID2(UniqMatch)                    ; IDs of unique matches
SDSSnameMATCH = SDSSnameMATCH(UniqMatch)          ; SDSSnames of unique matches
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; defining the output structure
   str={                               $
         ; -------- Data catalog values --------
         OBJID:'',                     $
         HEADOBJID:'',                 $
         RA:0.0d,                      $
         DEC:0.0d,                     $
         FLAGS:'',                     $
         TYPE:0,                       $
         PSFMAG_U:0.0,                 $
         PSFMAG_G:0.0,                 $
         PSFMAG_R:0.0,                 $
         PSFMAG_I:0.0,                 $
         PSFMAG_Z:0.0,                 $
         PSFMAGERR_U:0.0,              $
         PSFMAGERR_G:0.0,              $
         PSFMAGERR_R:0.0,              $
         PSFMAGERR_I:0.0,              $
         PSFMAGERR_Z:0.0,              $
         MJD_U:0.0d,                   $
         MJD_G:0.0d,                   $
         MJD_R:0.0d,                   $
         MJD_I:0.0d,                   $
         MJD_Z:0.0d,                   $
         DEREDPSFMAG_U:0.0,            $
         DEREDPSFMAG_G:0.0,            $
         DEREDPSFMAG_R:0.0,            $
         DEREDPSFMAG_I:0.0,            $
         DEREDPSFMAG_Z:0.0,            $
         z:0.0d,                       $
         A:0.0,                        $
         gamma:0.0,                    $
         ; -------- Shen catalog values --------
         SDSSname:'',                  $
         BALflag:0.0,                  $
         FIRSTflag:0.0,                $
         logBH_MGII_MD04:0.0,          $
         logBH_MGII_MD04_ERR:0.0,      $
         logLbol:0.0,                  $
         logLbol_err:0.0,              $
         logL5100:0.0,                 $
         logL5100_err:0.0,             $
         logL3000:0.0,                 $
         logL3000_err:0.0,             $
         logL1350:0.0,                 $
         logL1350_err:0.0,             $
         ; -------- Fit catalog values --------
         abest:0.0,                    $
         amed:0.0,                     $
         amean:0.0,                    $
         aplus68:0.0,                  $
         aminus68:0.0,                 $
         aplus95:0.0,                  $
         aminus95:0.0,                 $
         bbest:0.0,                    $
         bmed:0.0,                     $
         bmean:0.0,                    $
         bplus68:0.0,                  $
         bminus68:0.0,                 $
         bplus95:0.0,                  $
         bminus95:0.0,                 $
         zz:0.0,                       $
         season:0.0  }
   str = replicate(str,Nfits)
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
countM = 0                                                           ; restting counter (used to fill structure)
for ii=0L,NN-1 do begin                                              ; looping over (unique) matches and filling output structure
   entSHEN   = where(SDSSname eq SDSSnameMATCH(ii))                  ; selecting unique matches and finding them in Shen catalog
   entDat    = where(strtrim(IDs,2) eq ID2(ii))                      ; selecting unique matches and finding them in Data catalog
   entFit    = where(objid eq ID2(ii))                               ; selecting unique matches and finding them in Data catalog
   if entSHEN ne [-1] and entDat ne [-1] and entFit ne [-1] then begin  ; making sure there are matches
      ;writing data to output structure
      ; ---------------- Data catalog values ----------------
      str(countM).OBJID                       = Sdat(Uent(entDat)).OBJID
      str(countM).HEADOBJID                   = Sdat(Uent(entDat)).HEADOBJID
      str(countM).RA                          = Sdat(Uent(entDat)).RA
      str(countM).DEC                         = Sdat(Uent(entDat)).DEC
      str(countM).FLAGS                       = Sdat(Uent(entDat)).FLAGS
      str(countM).TYPE                        = Sdat(Uent(entDat)).TYPE
      str(countM).PSFMAG_U                    = Sdat(Uent(entDat)).PSFMAG_U
      str(countM).PSFMAG_G                    = Sdat(Uent(entDat)).PSFMAG_G
      str(countM).PSFMAG_R                    = Sdat(Uent(entDat)).PSFMAG_R
      str(countM).PSFMAG_I                    = Sdat(Uent(entDat)).PSFMAG_I
      str(countM).PSFMAG_Z                    = Sdat(Uent(entDat)).PSFMAG_Z
      str(countM).PSFMAGERR_U                 = Sdat(Uent(entDat)).PSFMAGERR_U
      str(countM).PSFMAGERR_G                 = Sdat(Uent(entDat)).PSFMAGERR_G
      str(countM).PSFMAGERR_R                 = Sdat(Uent(entDat)).PSFMAGERR_R
      str(countM).PSFMAGERR_I                 = Sdat(Uent(entDat)).PSFMAGERR_I
      str(countM).PSFMAGERR_Z                 = Sdat(Uent(entDat)).PSFMAGERR_Z
      str(countM).MJD_U                       = Sdat(Uent(entDat)).MJD_U
      str(countM).MJD_G                       = Sdat(Uent(entDat)).MJD_G
      str(countM).MJD_R                       = Sdat(Uent(entDat)).MJD_R
      str(countM).MJD_I                       = Sdat(Uent(entDat)).MJD_I
      str(countM).MJD_Z                       = Sdat(Uent(entDat)).MJD_Z
      str(countM).DEREDPSFMAG_U               = Sdat(Uent(entDat)).DEREDPSFMAG_U
      str(countM).DEREDPSFMAG_G               = Sdat(Uent(entDat)).DEREDPSFMAG_G
      str(countM).DEREDPSFMAG_R               = Sdat(Uent(entDat)).DEREDPSFMAG_R
      str(countM).DEREDPSFMAG_I               = Sdat(Uent(entDat)).DEREDPSFMAG_I
      str(countM).DEREDPSFMAG_Z               = Sdat(Uent(entDat)).DEREDPSFMAG_Z
      str(countM).z                           = Sdat(Uent(entDat)).z
      str(countM).A                           = Sdat(Uent(entDat)).A
      str(countM).gamma                       = Sdat(Uent(entDat)).gamma
      ; ---------------- Shen catalog values ----------------
      str(countM).SDSSname                    = Sshen(entShen).SDSS_name
      str(countM).BALflag                     = Sshen(entShen).BAL_FLAG
      str(countM).FIRSTflag                   = Sshen(entShen).FIRST_FR_TYPE 
      str(countM).logBH_MGII_MD04             = Sshen(entSHEN).logBH_MGII_MD04
      str(countM).logBH_MGII_MD04_ERR         = Sshen(entSHEN).logBH_MGII_MD04_ERR 
      str(countM).logLbol                     = Sshen(entSHEN).logLbol 
      str(countM).logLbol_err                 = Sshen(entSHEN).logLbol_err
      str(countM).logL5100                    = Sshen(entSHEN).logL5100
      str(countM).logL5100_err                = Sshen(entSHEN).logL5100_err
      str(countM).logL3000                    = Sshen(entSHEN).logL3000
      str(countM).logL3000_err                = Sshen(entSHEN).logL3000_err
      str(countM).logL1350                    = Sshen(entSHEN).logL1350
      str(countM).logL1350_err                = Sshen(entSHEN).logL1350_err
      ; ---------------- Fit catalog values ----------------
      str(countM).abest                       = abest(entFit)
      str(countM).amed                        = amed(entFit)
      str(countM).amean                       = amean(entFit)
      str(countM).aplus68                     = aplus68(entFit)
      str(countM).aminus68                    = aminus68(entFit)
      str(countM).aplus95                     = aplus95(entFit)
      str(countM).aminus95                    = aminus95(entFit)
      str(countM).bbest                       = bbest(entFit)
      str(countM).bmed                        = bmed(entFit)
      str(countM).bmean                       = bmean(entFit)
      str(countM).bplus68                     = bplus68(entFit)
      str(countM).bminus68                    = bminus68(entFit)
      str(countM).bplus95                     = bplus95(entFit)
      str(countM).bminus95                    = bminus95(entFit)
      str(countM).zz                          = zz(entFit)
      str(countM).season                      = seas(entFit)

      countM = countM + 1
   endif
endfor


;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; Setting up output structure for output file

; creating fits name 
date       = systime( )
date       = STRJOIN(STRSPLIT(STRTRIM(date,2),' ',/extract),'_')
date       = STRJOIN(STRSPLIT(STRTRIM(date,2),':',/extract))
outputfile = 'slopeANDshencat'+strtrim(date,2)+'_season'+strtrim(SWRITE,2)+'.fits'

; writing structure to output fits file
mwrfits, str, outputfile, /create
if vb eq 1 then print,':: create_slopeANDshencat.pro :: Wrote data for ',strtrim(countM,2),' objects to: ',strtrim(outputfile,2)
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 


if vb eq 1 then print,' '
if vb eq 1 then print,':: create_slopeANDshencat.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END

