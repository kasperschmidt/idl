;+
;----------------------------
;   NAME
;----------------------------
; runAperPhot_multiple.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Running the aper.pro (performing aperture photometry) for multiple objects
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; objectlist      : Ascii file (extension .txt) with info on objects to run aper.pro for
;                   Should contain the columns:
;                      col1: Name
;                      col2: RA
;                      col3: Dec
;                      col4: Aperlist
;                            A list on the format [a,b,c,...] of the
;                            sizes of the apertures to use given in arcsec
;                      col5: image
;                            The path and name to fits image to perform photometry on 
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : Ascii file containing the results of the aperture photometr
;                   Contains the columns:
;                      col1: Name
;                      col2: RA
;                      col3: Dec
;                      col5: aperflux
;                      col6: aperfluxerr
;                      col7: apersky
;                      col8: aperskyerr
;                      col4: image
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> runAperPhot_multiple,'',/VERBOSE,/STP
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2013-02-04  started by K. B. Schmidt (UCSB)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
;@ xxx.pro
;----------------------------
;-
PRO runAperPhot_multiple,objectlist,outputfile,VERBOSE=VERBOSE,STP=STP
PS = n_elements(EPS)
VB = n_elements(VERBOSE)
;-----------------------------------------------------------------------------------------
; loading object list
readcol,objectlist,format=['A','D','D','A','A'],name,ra,dec,aperlist,image
Ncalc = n_elements(name)
;-----------------------------------------------------------------------------------------
; opening output file
outname = StrJoin( StrSplit(objectlist, '.txt', /Regex, /Extract, /Preserve_Null), '_aperphot.txt')
print, outname
openw,22,outname,width=500 
printf,22,'# '
printf,22,'# Results from performing aperture photometry on objects in '+trim(objectlist)
printf,22,'# Created with runAperPhot_multiple.pro '
printf,22,'# '
printf,22,'# Name   RA   Dec  aperflux   aperfluxerr   apersky   aperskyerr   image'

;-----------------------------------------------------------------------------------------
; performing aperture photometry

for ii=0,Ncalc-1 do begin

    FILE   = image[ii]
    IMARR  = mrdfits(FILE,0,hdr)

    Xref   = FXPAR(hdr,'CRPIX1')    ; X reference pixel
    Yref   = FXPAR(hdr,'CRPIX2')    ; Y reference pixel
    RAref  = FXPAR(hdr,'CRVAL1')    ; RA of reference pixel 
    DECref = FXPAR(hdr,'CRVAL2')    ; DEC of reference pixel
    Nxpix  = FXPAR(hdr,'NAXIS1')    ; number of pixels in RA
    Nypix  = FXPAR(hdr,'NAXIS2')    ; number of pixels in DEC

    XCOOR   = [38,38]-1                                            ; x-coordinates of aperture centers
    YCOOR   = [37,37]-1                                            ; y-coordinates of aperture centers
    PIXCOM  = 5                                                    ; the pixel radius where the fluxes are compared
    APER,IMARR,XCOOR,YCOOR,flux,fluxerr,sky,skyerr,1,DEFAPER,DEFBCK,DEFBAD,/FLUX,/SILENT

endfor

close,22

if vb eq 1 then print,' '
if vb eq 1 then print,':: SDSSepochsfits2averagefits.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
