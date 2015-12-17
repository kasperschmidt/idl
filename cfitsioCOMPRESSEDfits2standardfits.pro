;+
;----------------------------
;   NAME
;----------------------------
; cfitsioCOMPRESSEDfits2standardfits.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure taking and input fits file compressed via the cfitsio
; package, extracting/decrompressing a set of specificed extensions and
; putting them together to a new standard fits file.
;----------------------------
;   COMMENTS
;----------------------------
; This code was used for the GxGx simulations from T.J. Cox and P. Jonsson
;----------------------------
;   INPUTS:
;----------------------------
; fitscompress        : string containing name and path of compressed fits file
; extensions          : string vector containing the names of the extensions 
;                       to extract/decompress and combine into a new fits file
; fmtvec              : string vector containing the fits format of the extensions to be 
;                       written to the outputfits, e.g. BINTABLE, IMAGE etc.
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /CLEAN          : set /CLEAN to delete the files where each
;                   extension is extracted into to save space
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfits      : the name of the (non-excisting) file the de-compressed
;                   extensions will be written to
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> cfitsioCOMPRESSEDfits2standardfits,'/disk3/kschmidt/GRISMSIMdir/simsJanFeb2012/G3G2-u4/mcrx_0048.fits',['lambda','CAMERA0','CAMERA1','CAMERA4'],['BINTABLE','IMAGE','IMAGE','IMAGE'],'/disk3/kschmidt/GRISMSIMdir/simsJanFeb2012/G3G2-u4/IMCOPYmcrx_0048.fits',/VERBOSE,/STP;,/CLEAN

; IDL> cfitsioCOMPRESSEDfits2standardfits,'/disk3/kschmidt/GRISMSIMdir/simsJanFeb2012/G3G2-u4/mcrx_0048.fits',['lambda','CAMERA0-NONSCATTER','CAMERA1-NONSCATTER','CAMERA4-NONSCATTER'],['BINTABLE','IMAGE','IMAGE','IMAGE'],'/disk3/kschmidt/GRISMSIMdir/simsJanFeb2012/G3G2-u4/IMCOPYNOSCTmcrx_0048.fits',/VERBOSE,/STP,/CLEAN
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2012-03-02  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
; imcopy     Uses the imcopy routine of the cfitsio package to
;            decompress, hence that needs to be installed
@ trim.pro
;----------------------------
;-
PRO cfitsioCOMPRESSEDfits2standardfits,fitscompress,extensions,fmtvec,outputfits,VERBOSE=VERBOSE,STP=STP,CLEAN=CLEAN

CL = n_elements(CLEAN)
VB = n_elements(VERBOSE)

Next = n_elements(extensions)  ; the number of extension to extract
nameDECOM = strarr(Next)       ; array for names of decompressed extensions
nameSPLIT = strsplit(fitscompress,'.fi',/regex,/extract)

; testing if output file exists
spawnSTR = 'find '+outputfits
spawn,spawnSTR,findRES
   splitRES = strsplit(findRES,' ',/extract,/regex)
if findRES ne '' then begin
   print,':: ELmaps_superwrapperV1p0 :: The chosen outputfile already excists...'
   print,'                                                                              ---> Aborting'
   stop
end


for ii=0,Next-1 do begin  ; looping over extensions
   nameDECOM(ii) = namesplit(0)+'-'+extensions(ii)+'.fi'+namesplit(1)
   if vb eq 1 then print,':: cfitsioCOMPRESSEDfits2standardfits :: Extension ',trim(extensions(ii)),' decompressed into ',trim(nameDECOM(ii))
   HDU_NO = find_HDU(fitscompress,extensions(ii))             
   spawnSTR  = 'imcopy '+trim(fitscompress)+'['+trim(HDU_NO)+']'+' '+trim(nameDECOM(ii))
   ;print,'spawning --> ',spawnSTR
   spawn,spawnSTR

   HDU_NO  = find_HDU(nameDECOM(ii),extensions(ii))             
   fitsext = READFITS(nameDECOM(ii),hdr,EXTEN_NO=HDU_NO)

   hdrNEW = hdr
   if FXPAR(hdrNEW,'XTENSION') eq 0 then begin
      hdrNEW(0) = "XTENSION= '"+FMTVEC(ii)+"'           / binary table extension  "  ; overwriting first line of header
   endif
   writefits, outputfits, fitsext, hdrNEW, /Append   
   if vb eq 1 then print,':: cfitsioCOMPRESSEDfits2standardfits :: writing '+trim(extensions(ii))+' extension to '+trim(outputfits)   
endfor

if CL eq 1 then begin
   if vb eq 1 then print,':: cfitsioCOMPRESSEDfits2standardfits :: Cleaning directoriy '
   for jj=0,Next-1 do begin
      spawnSTR = 'rm '+nameDECOM(jj)
      spawn,spawnSTR
   endfor
endif


if vb eq 1 then print,' '
if vb eq 1 then print,':: cfitsioCOMPRESSEDfits2standardfits :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
