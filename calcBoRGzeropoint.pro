;+
;----------------------------
;   NAME
;----------------------------
; calcBoRGzeropoint.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; calculating the BoRG zeropoints using the Schlegel et al. (1998)
; dust maps and the Cardelli et al., 1989 reddening curve with RV = 3.1
;----------------------------
;   COMMENTS
;----------------------------
; Has to be run one level above the *drz*.fits files where the
; (CRVAL1, CRVAL2) = (RA, DEC) will be extracted from the fits header
;----------------------------
;   INPUTS:
;----------------------------
; dirlist         : list of BoRG fields (directories) to estimate the zeropoints for
; bands           : list of strings with bands to estimate zeropoints for. 
;                   Possibilities: ['f606w','f600lp','f098m','f105w','f125w','f160w']
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ZPkind          : The kind of HST zeropoints to use (characterizing the aperture in which
;                   the zeropoints were estimated). Possibilities are 'infinite' and 'arcsec0p4'.
;                   Default value is 'infinite' like in Bradley et al. 2012
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; zeropoints      : File containing the field names and the estimated zeropoints.
;                   This file can be used to create BoRG catalogs by running
;                   > for i in `cat dirlist.txt` ; do ./process_borg_field.sh ; done 
;                   File will be saved in same directory as dirlist.txt
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; BASH> ls -p | grep '/' | sed 's/\///g' > dirlist.txt 
; IDL> calcBoRGzeropoint,'/Users/kasperborelloschmidt/work/BoRG/borgdata/cycle19_new_nov14_cat/dirlist.txt',['f606w','f098m','f125w','f160w'],/VERBOSE

; IDL> calcBoRGzeropoint,'/Users/kasperborelloschmidt/work/BoRG/borgdata/version_2p0/dirlist606band.txt',['f606w','f098m','f125w','f160w'],/VERBOSE

; IDL> calcBoRGzeropoint,'/Users/kasperborelloschmidt/work/BoRG/borgdata/version_2p0/dirlist606band.txt',['f600lp','f098m','f125w','f160w'],/VERBOSE

;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2012-12-30  started by K. B. Schmidt (UCSB)
; 2013-04-16  added ZPkind keyword. K. B. Schmidt (UCSB)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
@ dust_getval.pro
;----------------------------
;-
PRO calcBoRGzeropoint,dirlist,bands,EPS=EPS,VERBOSE=VERBOSE,STP=STP,ZPkind=ZPkind
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

ZP = n_elements(ZPkind)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

if vb eq 1 then print,' '
if vb eq 1 then print,':: calcBoRGzeropoint.pro :: -- START OF PROGRAM -- '
if vb eq 1 then print,' '
;----------------------------------------------------------------------------------------------------
readcol,dirlist,FORMAT=('A'),field,comment='#'
Nfields = n_elements(field)
Nbands  = n_elements(bands)
if vb eq 1 then print,':: calcBoRGzeropoint.pro :: Found ',trim(Nfields),' fields with ',trim(Nbands),' bands to estimate zeropoints for '
;----------------------------------------------------------------------------------------------------
outfile = repstr(dirlist,'.txt','_deredzeropoints.txt')
if vb eq 1 then print,':: calcBoRGzeropoint.pro :: Output will be written to ',outfile
openw,22,outfile
printf,22,'# File containing estimated de-reddened zeropoints of '
printf,22,'# ',bands
printf,22,'# from BoRG fields listed in:'
printf,22,'#     '+trim(dirlist)
printf,22,'# Using the Schlegel et al (1998) dust maps.'
printf,22,'# Created with calcBoRGzeropoint.pro'
printf,22,'# Columns are:'
printf,22,'# '
printf,22,'# fieldname ',bands
;----------------------------------------------------------------------------------------------------
AlamEBV_f606w  = 3.01882984135
AlamEBV_f600lp = 2.24159324026
AlamEBV_f098m  = 1.29502816006
AlamEBV_f105w  = 1.18148250758
AlamEBV_f125w  = 0.893036743585
AlamEBV_f160w  = 0.633710427959

ZPuse = 'infinite' ; Default kind of zeropoints
if ZP eq 1 then ZPuse = ZPkind ; overwriting zero-point if given on command line

if (ZPuse eq 'infinite') then begin ; using ~infenite apertures zero-points as in Larry's paper
   if vb eq 1 then print,':: calcBoRGzeropoint.pro :: Use HST zero-points of ~infinite apertures (as in Bradley et al. 2012)
   if vb eq 1 then print,'                            From http://www.stsci.edu/hst/wfc3/phot_zp_lbn (pre 130306)'
   ZP_f606w  = 26.0814041237
   ZP_f600lp = 25.8814691531
   ZP_f098m  = 25.68102452
   ZP_f105w  = 26.27 ; taken form website as opposed to other values which are provide by Larry
   ZP_f125w  = 26.2473632068 ;
   ZP_f160w  = 25.9558992372 ;
endif else if (ZPuse eq 'arcsec0p4') then begin ; using 0.4 arcsec aperture zero-points from http://www.stsci.edu/hst/wfc3/phot_zp_lbn
   if vb eq 1 then print,':: calcBoRGzeropoint.pro :: Use HST zero-points of updated ~infinite apertures (post 130306)' 
   if vb eq 1 then print,'                            From http://www.stsci.edu/hst/wfc3/phot_zp_lbn '
   ZP_f606w  = 26.0691
   ZP_f600lp = 25.8746
   ZP_f098m  = 25.6674
   ZP_f105w  = 26.2687
   ZP_f125w  = 26.2303
   ZP_f160w  = 25.9463
endif else if (ZPuse eq 'arcsec0p4') then begin ; using 0.4 arcsec aperture zero-points from http://www.stsci.edu/hst/wfc3/phot_zp_lbn
   if vb eq 1 then print,':: calcBoRGzeropoint.pro :: Use HST zero-points of 0.4 arcsec apertures (post 130306)'
   if vb eq 1 then print,'                            From http://www.stsci.edu/hst/wfc3/phot_zp_lbn '
   ZP_f606w  = 25.9668
   ZP_f600lp = 25.7681
   ZP_f098m  = 25.5041
   ZP_f105w  = 26.0974
   ZP_f125w  = 26.0449
   ZP_f160w  = 25.7551
endif else begin
   if vb eq 1 then print,':: calcBoRGzeropoint.pro :: The selected zero point "'+ZPkind+'" is not valid --> ABORTING'
   close,/all
   stop
endelse



;----------------------------------------------------------------------------------------------------
for ii=0,Nfields-1 do begin ; looping over fields
    if vb eq 1 then print,':: calcBoRGzeropoint.pro :: Calculated zero-points for '+field(ii)+':'
    drzfile = field(ii)+'/'+field(ii)+'_*'+bands(0)+'*drz.fits'
    drzdat = mrdfits(drzfile,0,hdr,/SILENT)
    RA  = fxpar(hdr,'CRVAL1')
    Dec = fxpar(hdr,'CRVAL2')

    spawn,'skycoor -g '+trim(RA)+' '+trim(Dec),spawnout
    skytrim = strsplit(spawnout,' ',/EXTRACT)
    gallong = skytrim(0)
    gallat  = skytrim(1)

    EBV = dust_getval(gallong,gallat,/interp,ipath='~/idl711mac/itt/idl71/lib/schlegelmapsIDL/SFD_4096/')
    if vb eq 1 then print,'          Using Schlegel et al (1998) redening of E(B-V) = '+trim(EBV)

    ; -- estimating corrected zeropoints --
    ZPout = fltarr(Nbands)

    test = where(bands eq 'f606w')
    if test ne [-1] then begin
        LBcorr_f606w = AlamEBV_f606w*EBV
        ZPcorr_f606w = ZP_f606w-LBcorr_f606w
        if vb eq 1 then print,'          f606w : '+trim(ZPcorr_f606w)
        ZPout(test) = ZPcorr_f606w
    endif

    test = where(bands eq 'f600lp')
    if test ne [-1] then begin
        LBcorr_f600lp = AlamEBV_f600lp*EBV
        ZPcorr_f600lp = ZP_f600lp-LBcorr_f600lp
        if vb eq 1 then print,'          f600lp : '+trim(ZPcorr_f600lp)
        ZPout(test) = ZPcorr_f600lp
    endif

    test = where(bands eq 'f098m')
    if test ne [-1] then begin
        LBcorr_f098m = AlamEBV_f098m*EBV
        ZPcorr_f098m = ZP_f098m-LBcorr_f098m
        if vb eq 1 then print,'          f098m : '+trim(ZPcorr_f098m)
        ZPout(test) = ZPcorr_f098m
    endif

    test = where(bands eq 'f105w')
    if test ne [-1] then begin
        LBcorr_f105w = AlamEBV_f105w*EBV
        ZPcorr_f105w = ZP_f105w-LBcorr_f105w
        if vb eq 1 then print,'          f105w : '+trim(ZPcorr_f105w)
        ZPout(test) = ZPcorr_f105w
    endif

    test = where(bands eq 'f125w')
    if test ne [-1] then begin
        LBcorr_f125w = AlamEBV_f125w*EBV
        ZPcorr_f125w = ZP_f125w-LBcorr_f125w
        if vb eq 1 then print,'          f125w : '+trim(ZPcorr_f125w)
        ZPout(test) = ZPcorr_f125w
    endif

    test = where(bands eq 'f160w')
    if test ne [-1] then begin
        LBcorr_f160w = AlamEBV_f160w*EBV
        ZPcorr_f160w = ZP_f160w-LBcorr_f160w
        if vb eq 1 then print,'          f160w : '+trim(ZPcorr_f160w)
        ZPout(test) = ZPcorr_f160w
    endif

    printf,22,field(ii),ZPout
endfor
;----------------------------------------------------------------------------------------------------
close,22
;----------------------------------------------------------------------------------------------------
if vb eq 1 then print,' '
if vb eq 1 then print,':: calcBoRGzeropoint.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
