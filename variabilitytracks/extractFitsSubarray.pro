;+
;----------------------------
;   NAME
;----------------------------
; extractFitsSubarray.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure reads the Stripe 82 fits file, the file of matches
; with the Shen et al 2010 catalog, and a file with the (r-band) A and
; gamma values from the variability power-law fit from Schmidt et al
; 2010. Then it extracts the data from the fits file which satisfies
; criteria on redshift, A and gamma provided by the user on the
; command line.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; Zlim            : Vector with the redshift limits to apply; [Zmin,Zmax]
; Alim            : Vector with the A limits to apply; [Amin,Amax]
; gammalim        : Vector with the gamma limits to apply; [gammamin,gammamax]
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; newfits         : Name of fits file with extracted data.
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> extractFitsSubarray,[1.4,1.6],[0.15,2.0],[0.0,5.0],newfits,/VERBOSE

; IDL> extractFitsSubarray,[0,7],[0,2],[0,5],newfits,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-07-08  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ namedate.pro
;----------------------------
;-
PRO extractFitsSubarray,Zlim,Alim,gammalim,newfits,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)

fitsfile = '/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/QSOs/DR5qcat_s82_photo_FieldModelANDDered_sorted.fit'
;fitsfile = '/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/QSOs/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999.fit'

namedate,fitsfile,path,name,extension,date,dateus

if vb eq 1 then print,' '
if vb eq 1 then print,':: extractFitsSubarray.pro :: -- START OF PROGRAM -- '
if vb eq 1 then print,'                              ',date
if vb eq 1 then print,' '
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; reading the (r-band) A and gamma values of the QSOs
readcol,FORMAT='(A,F,F,F)','S82objid_z_Agamma.dat',ID1,z,Aval,Gammaval
;readcol,FORMAT='(A,F,F,F)','S82objid_z_Agamma_test4obj.dat',ID1,z,Aval,Gammaval

; reading file with matches (SDSS names) from the Shen et al 2010 catalog
readcol,FORMAT='(A,F,F,A,F,F,I)','mathcQSOs_Thu_Jul_8_132203_2010.dat',ID2,RA2,Dec2,SDSSname,RAmatch,Decmatch,Nmatch

; reading the fits file to extract the data from
s = mrdfits(fitsfile,1) 

; reading the Shen et al 2010 catalog
shen = mrdfits('dr7_bh_June_2010.fits',1) 

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
Nextract = 0.0                                    ; restting counter
for ii=0,n_elements(ID1)-1 do begin               ; looping over objects with A and gamma calculated
   E1 = where(ID2 eq ID1(ii), C1)                 ; finding matches in Shen et al 2010 match file
   E2 = where(s.headobjid eq ID1(ii), C2)         ; finding matches in fits file
   SDSSii  = SDSSname(E1)

   if C1 ne 0 and C2 ne 0 and SDSSii(0) ne 0.0 then begin ; making sure that there are matches in both files
      ; getting the values for the ii'th object
      zii     = z(ii)
      Aii     = Aval(ii)
      gammaii = gammaval(ii)
      RAii    = RAmatch(E1)
      Decii   = Decmatch(E1)

      E3 = where(shen.SDSS_name eq SDSSii(0), C3)    ; the match in the Shen catalog
      if C3 eq 0 then print,':: extractFitsSubarray.pro :: WARNING! The Shen et al data could not be found for ',strtrim(SDSSii(0),2),', i.e. ',strtrim(ID1(ii),2),' -> ABORTING'
      if C3 eq 0 then stop

      ; checking that criteria provided on the command line is fulfilled
      if zii gt zlim[0] AND zii lt zlim[1] AND Aii gt Alim[0] AND Aii lt Alim[1] AND gammaii gt gammalim[0] AND gammaii lt gammalim[1] then begin

      ;alternative if statement for extracting specific objects as well (make sure they are in the A,gamm,z range chosen...):
;      if zii gt zlim[0] AND zii lt zlim[1] AND Aii gt Alim[0] AND Aii lt Alim[1] AND gammaii gt gammalim[0] AND gammaii lt gammalim[1] AND (ID1(ii) eq 587731185133027418  OR ID1(ii) eq 587731187279135109  OR ID1(ii) eq 587731513155125454 OR ID1(ii) eq 587731186729353409  OR ID1(ii) eq 587731512075485356 OR ID1(ii) eq 587731187274940621 OR ID1(ii) eq 587731511545758032 OR ID1(ii) eq 587731185115005391 OR ID1(ii) eq 587731185126736014 OR ID1(ii) eq 587731185119986070 OR ID1(ii) eq 587731185129422879 OR ID1(ii) eq 587731185667145777 OR ID1(ii) eq 587731186209849443 OR ID1(ii) eq 587731187279135109 OR ID1(ii) eq 587731513154207926 OR ID1(ii) eq 587731514214842538 OR ID1(ii) eq 588015509288976546 OR ID1(ii) eq 588015508752892062 OR ID1(ii) eq 588015508748042423 OR ID1(ii) eq 588015508745683127 ) then begin

         if Nextract eq 0 then begin ; arrays defined by the first match
            sOBJID           = s(E2).OBJID
            sHEADOBJID       = s(E2).HEADOBJID
            sRA              = s(E2).RA
            sDEC             = s(E2).DEC
            sFLAGS           = s(E2).FLAGS
            sTYPE            = s(E2).TYPE
            sPSFMAG_U        = s(E2).PSFMAG_U
            sPSFMAG_G        = s(E2).PSFMAG_G
            sPSFMAG_R        = s(E2).PSFMAG_R
            sPSFMAG_I        = s(E2).PSFMAG_I
            sPSFMAG_Z        = s(E2).PSFMAG_Z
            sPSFMAGERR_U     = s(E2).PSFMAGERR_U
            sPSFMAGERR_G     = s(E2).PSFMAGERR_G
            sPSFMAGERR_R     = s(E2).PSFMAGERR_R
            sPSFMAGERR_I     = s(E2).PSFMAGERR_I
            sPSFMAGERR_Z     = s(E2).PSFMAGERR_Z
            sMJD_U           = s(E2).MJD_U
            sMJD_G           = s(E2).MJD_G
            sMJD_R           = s(E2).MJD_R
            sMJD_I           = s(E2).MJD_I
            sMJD_Z           = s(E2).MJD_Z
            sDEREDPSFMAG_U   = s(E2).DEREDPSFMAG_U
            sDEREDPSFMAG_G   = s(E2).DEREDPSFMAG_G
            sDEREDPSFMAG_R   = s(E2).DEREDPSFMAG_R
            sDEREDPSFMAG_I   = s(E2).DEREDPSFMAG_I
            sDEREDPSFMAG_Z   = s(E2).DEREDPSFMAG_Z

            sSDSSname        = strarr(C2) + SDSSii(0)
            sRAmatch         = fltarr(C2) * 0.0 + RAii(0)
            sDECmatch        = fltarr(C2) * 0.0 + Decii(0)
            sZ               = fltarr(C2) * 0.0 + zii(0)
            sA               = fltarr(C2) * 0.0 + Aii(0)
            sGAMMA           = fltarr(C2) * 0.0 + gammaii(0)

            sMbhVir_MGIIMD04 = fltarr(C2) * 0.0 + shen(E3).logBH_MGII_MD04
            sMbhVir_MGIIMD04_err = fltarr(C2) * 0.0 + shen(E3).logBH_MGII_MD04_err
            slogLbol         = fltarr(C2) * 0.0 + shen(E3).logLbol
            slogLbol_err     = fltarr(C2) * 0.0 + shen(E3).logLbol_err
            slogL5100        = fltarr(C2) * 0.0 + shen(E3).logL5100
            slogL5100_err    = fltarr(C2) * 0.0 + shen(E3).logL5100_err
            slogL3000        = fltarr(C2) * 0.0 + shen(E3).logL3000
            slogL3000_err    = fltarr(C2) * 0.0 + shen(E3).logL3000_err
            slogL1350        = fltarr(C2) * 0.0 + shen(E3).logL1350
            slogL1350_err    = fltarr(C2) * 0.0 + shen(E3).logL1350_err


         endif else begin      ; appending values to arrays
            sOBJID           = [sOBJID,s(E2).OBJID]
            sHEADOBJID       = [sHEADOBJID,s(E2).HEADOBJID]
            sRA              = [sRA,s(E2).RA]
            sDEC             = [sDEC,s(E2).DEC]
            sFLAGS           = [sFLAGS,s(E2).FLAGS]
            sTYPE            = [sTYPE,s(E2).TYPE]
            sPSFMAG_U        = [sPSFMAG_U,s(E2).PSFMAG_U]
            sPSFMAG_G        = [sPSFMAG_G,s(E2).PSFMAG_G]
            sPSFMAG_R        = [sPSFMAG_R,s(E2).PSFMAG_R]
            sPSFMAG_I        = [sPSFMAG_I,s(E2).PSFMAG_I]
            sPSFMAG_Z        = [sPSFMAG_Z,s(E2).PSFMAG_Z]
            sPSFMAGERR_U     = [sPSFMAGERR_U,s(E2).PSFMAGERR_U]
            sPSFMAGERR_G     = [sPSFMAGERR_G,s(E2).PSFMAGERR_G]
            sPSFMAGERR_R     = [sPSFMAGERR_R,s(E2).PSFMAGERR_R]
            sPSFMAGERR_I     = [sPSFMAGERR_I,s(E2).PSFMAGERR_I]
            sPSFMAGERR_Z     = [sPSFMAGERR_Z,s(E2).PSFMAGERR_Z]
            sMJD_U           = [sMJD_U,s(E2).MJD_U]
            sMJD_G           = [sMJD_G,s(E2).MJD_G]
            sMJD_R           = [sMJD_R,s(E2).MJD_R]
            sMJD_I           = [sMJD_I,s(E2).MJD_I]
            sMJD_Z           = [sMJD_Z,s(E2).MJD_Z]
            sDEREDPSFMAG_U   = [sDEREDPSFMAG_U,s(E2).DEREDPSFMAG_U]
            sDEREDPSFMAG_G   = [sDEREDPSFMAG_G,s(E2).DEREDPSFMAG_G]
            sDEREDPSFMAG_R   = [sDEREDPSFMAG_R,s(E2).DEREDPSFMAG_R]
            sDEREDPSFMAG_I   = [sDEREDPSFMAG_I,s(E2).DEREDPSFMAG_I]
            sDEREDPSFMAG_Z   = [sDEREDPSFMAG_Z,s(E2).DEREDPSFMAG_Z]

            sSDSSname        = [sSDSSname,strarr(C2) + SDSSii(0)]
            sRAmatch         = [sRAmatch,dblarr(C2) * 0.0 + RAii(0)]
            sDECmatch        = [sDECmatch,dblarr(C2) * 0.0 + Decii(0)]
            sZ               = [sZ,fltarr(C2) * 0.0 + zii(0)]
            sA               = [sA,fltarr(C2) * 0.0 + Aii(0)]
            sGAMMA           = [sGAMMA,fltarr(C2) * 0.0 + gammaii(0)]  

            sMbhVir_MGIIMD04 = [sMbhVir_MGIIMD04,fltarr(C2) * 0.0 + shen(E3).logBH_MGII_MD04]
            sMbhVir_MGIIMD04_err = [sMbhVir_MGIIMD04_err,fltarr(C2) * 0.0 + shen(E3).logBH_MGII_MD04_err]
            slogLbol         = [slogLbol,fltarr(C2) * 0.0 + shen(E3).logLbol]
            slogLbol_err     = [slogLbol_err,fltarr(C2) * 0.0 + shen(E3).logLbol_err]
            slogL5100        = [slogL5100,fltarr(C2) * 0.0 + shen(E3).logL5100]
            slogL5100_err    = [slogL5100_err,fltarr(C2) * 0.0 + shen(E3).logL5100_err]
            slogL3000        = [slogL3000,fltarr(C2) * 0.0 + shen(E3).logL3000]
            slogL3000_err    = [slogL3000_err,fltarr(C2) * 0.0 + shen(E3).logL3000_err]
            slogL1350        = [slogL1350,fltarr(C2) * 0.0 + shen(E3).logL1350]
            slogL1350_err    = [slogL1350_err,fltarr(C2) * 0.0 + shen(E3).logL1350_err]

         endelse 
         Nextract = Nextract + 1.0   ; counting the umber of extractions done, i.e. how many object satisfy z,A and gamma limits
      endif
   endif
endfor

if vb eq 1 then print,':: extractFitsSubarray.pro :: Extracted data for ',strtrim(Nextract,2),' objects'

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; defining the output structure and filling it
NN = n_elements(sOBJID)   ; the size of the structure
   str={OBJID:'',HEADOBJID:'',RA:0.0d,DEC:0.0d,FLAGS:'',TYPE:0,PSFMAG_U:0.0,PSFMAG_G:0.0,PSFMAG_R:0.0,PSFMAG_I:0.0,PSFMAG_Z:0.0,PSFMAGERR_U:0.0,PSFMAGERR_G:0.0,PSFMAGERR_R:0.0,PSFMAGERR_I:0.0,PSFMAGERR_Z:0.0,MJD_U:0.0d,MJD_G:0.0d,MJD_R:0.0d,MJD_I:0.0d,MJD_Z:0.0d,DEREDPSFMAG_U:0.0,DEREDPSFMAG_G:0.0,DEREDPSFMAG_R:0.0,DEREDPSFMAG_I:0.0,DEREDPSFMAG_Z:0.0,SDSSname:'',RAmatch:0.0d,DECmatch:0.0d,z:0.0d,A:0.0,gamma:0.0,logBH_MGII_MD04:0.0,logBH_MGII_MD04_ERR:0.0,logLbol:0.0,logLbol_err:0.0,logL5100:0.0,logL5100_err:0.0,logL3000:0.0,logL3000_err:0.0,logL1350:0.0,logL1350_err:0.0}
   str = replicate(str,NN)

str.OBJID           = sOBJID(*)
str.HEADOBJID       = sHEADOBJID(*)
str.RA              = sRA(*)
str.DEC             = sDEC(*)
str.FLAGS           = sFLAGS(*)
str.TYPE            = sTYPE(*)
str.PSFMAG_U        = sPSFMAG_U(*)
str.PSFMAG_G        = sPSFMAG_G(*)
str.PSFMAG_R        = sPSFMAG_R(*)
str.PSFMAG_I        = sPSFMAG_I(*)
str.PSFMAG_Z        = sPSFMAG_Z(*)
str.PSFMAGERR_U     = sPSFMAGERR_U(*)
str.PSFMAGERR_G     = sPSFMAGERR_G(*)
str.PSFMAGERR_R     = sPSFMAGERR_R(*)
str.PSFMAGERR_I     = sPSFMAGERR_I(*)
str.PSFMAGERR_Z     = sPSFMAGERR_Z(*)
str.MJD_U           = sMJD_U(*)
str.MJD_G           = sMJD_G(*)
str.MJD_R           = sMJD_R(*)
str.MJD_I           = sMJD_I(*)
str.MJD_Z           = sMJD_Z(*)
str.DEREDPSFMAG_U   = sDEREDPSFMAG_U(*)
str.DEREDPSFMAG_G   = sDEREDPSFMAG_G(*)
str.DEREDPSFMAG_R   = sDEREDPSFMAG_R(*)
str.DEREDPSFMAG_I   = sDEREDPSFMAG_I(*)
str.DEREDPSFMAG_Z   = sDEREDPSFMAG_Z(*)

str.SDSSname        = sSDSSname(*)
str.RAmatch         = sRAmatch(*)
str.DECmatch        = sDECmatch(*)
str.Z               = sZ(*)
str.A               = sA(*)
str.GAMMA           = sGAMMA(*)

str.logBH_MGII_MD04 = sMbhVir_MGIIMD04(*)
str.logBH_MGII_MD04_err = sMbhVir_MGIIMD04_err(*)
str.logLbol        = slogLbol(*)
str.logLbol_err    = slogLbol_err(*)
str.logL5100       = slogL5100(*)
str.logL5100_err   = slogL5100_err(*)
str.logL3000       = slogL3000(*)
str.logL3000_err   = slogL3000_err(*)
str.logL1350       = slogL1350(*)
str.logL1350_err   = slogL1350_err(*)


; creating fits name 
newfits0 = name+'_SUB_'+strtrim(zlim[0],2)+'z'+strtrim(zlim[1],2)+'AND'+strtrim(Alim[0],2)+'A'+strtrim(Alim[1],2)+'AND'+strtrim(gammalim[0],2)+'gamma'+strtrim(gammalim[1],2)
newfits1 = STRSPLIT(newfits0, /EXTRACT,'.')        ; splitting name around dots
newfits2 = STRJOIN(newfits1,'p')                   ; joining name back together with ps instead of dots
newfits  = newfits2+'_'+dateus+'.fits'             ; adding date and extension

; writing structure to output fits file
mwrfits, str, newfits, /create
if vb eq 1 then print,':: extractFitsSubarray.pro :: Wrote the extracted data to: ',strtrim(newfits,2)
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
if vb eq 1 then print,' '
if vb eq 1 then print,':: extractFitsSubarray.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
