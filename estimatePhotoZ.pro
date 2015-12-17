;+
;----------------------------
;   NAME
;----------------------------
; estimatePhotoZ.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Reading a fits file with the specified columns, creates a structure
; and calculates/estimates the photometric redshift via J. F. Hennawi's
; photo-z code. See http://adsabs.harvard.edu/abs/2009arXiv0908.3907H
; for a short description.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; data           : name and path of fits binary table with QSO data. The fits
;                  file should contain a column with magnitudes, magnitude 
;                  errors and extinction (e.g. taken from the CasJobs server) 
;                  corresponding to the magnitude given for the mag-keyword.
; mag            : String indicating the magnitudes to use. Choices are:
;                  mag='PSFsdss'          ; Used for point sdss sources. Fits columns 
;                                           should be named psfMag_x and psfMagErr_x 
;                                           where x indicates the sdss band.
;                  mag='FIBERsdss'        ; Used for extended sources. Fits columns 
;                                           should be named FiberMag_x and FiberMagErr_x 
;                                           where x indicates the band.
;                  mag='FIBERab'          ; Used for extended sources. Fits columns 
;                                           should be named FiberMagAB_x and FiberMagErrAB_x 
;                                           where x indicates the band.
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; strOUT          : structure containing the results of ADD_X2_TAGS.pro
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> estimatePhotoZ,'/Users/kasperborelloschmidt/work/casjobs_SDSS/lenscandidatesearch/topXX/mergeSDSSandUKIDSS_output100927_460lenscand_wEXT.fits',strout,mag='FIBERsdss',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-10-05  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ ADD_X2_TAGS.pro
;----------------------------
;-
PRO estimatePhotoZ,data,strOUT,MAG=MAG,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

str = mrdfits(data,1)

; vector with softening parameters from http://www.sdss.org/dr7/algorithms/fluxcal.html#counts2mag
bb         = [1.4,0.9,1.2,1.8,7.4]  ; wihtout 1e-10

case MAG of
   'PSFsdss': begin
      if vb eq 1 then print,':: estimatePhotoZ.pro :: SDSS asinh PSF magnitudes chosen'
      Nobj = n_elements(str.PSFmag_r)  ; number of objects in fits file
      FLUX       = fltarr(Nobj,5)      ; define flux array
      FLUX_IVAR  = fltarr(Nobj,5)      ; define flux error array
      EXTINCTION = fltarr(Nobj,5)      ; define extinction array

      FLUX(*,0)      = sdss_mags2flux(str.PSFmag_u,bb(0))                     ; converting u band magnitude to flux
      FLUX(*,1)      = sdss_mags2flux(str.PSFmag_g,bb(1))                     ; converting g band magnitude to flux
      FLUX(*,2)      = sdss_mags2flux(str.PSFmag_r,bb(2))                     ; converting r band magnitude to flux
      FLUX(*,3)      = sdss_mags2flux(str.PSFmag_i,bb(3))                     ; converting i band magnitude to flux
      FLUX(*,4)      = sdss_mags2flux(str.PSFmag_z,bb(4))                     ; converting z band magnitude to flux

      FLUX_IVAR(*,0) = sdss_magerr2ivar(str.PSFmagerr_u,str.PSFmag_u,bb(0))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,1) = sdss_magerr2ivar(str.PSFmagerr_g,str.PSFmag_g,bb(1))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,2) = sdss_magerr2ivar(str.PSFmagerr_r,str.PSFmag_r,bb(2))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,3) = sdss_magerr2ivar(str.PSFmagerr_i,str.PSFmag_i,bb(3))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,4) = sdss_magerr2ivar(str.PSFmagerr_z,str.PSFmag_z,bb(4))   ; converting magnitude error inverse variance

      EXTINCTION(*,0)= str.EXTINCTION_u
      EXTINCTION(*,1)= str.EXTINCTION_g
      EXTINCTION(*,2)= str.EXTINCTION_r
      EXTINCTION(*,3)= str.EXTINCTION_i
      EXTINCTION(*,4)= str.EXTINCTION_z

   end

   'FIBERsdss': begin
      if vb eq 1 then print,':: estimatePhotoZ.pro :: SDSS asinh Fiber magnitudes chosen'
      Nobj = n_elements(str.FIBERmag_r)  ; number of objects in fits file
      FLUX       = fltarr(Nobj,5)        ; define flux array
      FLUX_IVAR  = fltarr(Nobj,5)        ; define flux error array
      EXTINCTION = fltarr(Nobj,5)        ; define extinction array

      FLUX(*,0)      = sdss_mags2flux(str.FIBERmag_u,bb(0))                     ; converting u band magnitude to flux
      FLUX(*,1)      = sdss_mags2flux(str.FIBERmag_g,bb(1))                     ; converting g band magnitude to flux
      FLUX(*,2)      = sdss_mags2flux(str.FIBERmag_r,bb(2))                     ; converting r band magnitude to flux
      FLUX(*,3)      = sdss_mags2flux(str.FIBERmag_i,bb(3))                     ; converting i band magnitude to flux
      FLUX(*,4)      = sdss_mags2flux(str.FIBERmag_z,bb(4))                     ; converting z band magnitude to flux

      FLUX_IVAR(*,0) = sdss_magerr2ivar(str.FIBERmagerr_u,str.FIBERmag_u,bb(0))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,1) = sdss_magerr2ivar(str.FIBERmagerr_g,str.FIBERmag_g,bb(1))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,2) = sdss_magerr2ivar(str.FIBERmagerr_r,str.FIBERmag_r,bb(2))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,3) = sdss_magerr2ivar(str.FIBERmagerr_i,str.FIBERmag_i,bb(3))   ; converting magnitude error inverse variance
      FLUX_IVAR(*,4) = sdss_magerr2ivar(str.FIBERmagerr_z,str.FIBERmag_z,bb(4))   ; converting magnitude error inverse variance

      EXTINCTION(*,0)= str.EXTINCTION_u
      EXTINCTION(*,1)= str.EXTINCTION_g
      EXTINCTION(*,2)= str.EXTINCTION_r
      EXTINCTION(*,3)= str.EXTINCTION_i
      EXTINCTION(*,4)= str.EXTINCTION_z
   end

   'FIBERab': begin
      if vb eq 1 then print,':: estimatePhotoZ.pro :: AB Fiber magnitudes chosen'
      Nobj = n_elements(str.FIBERmag_r)  ; number of objects in fits file
      FLUX       = fltarr(Nobj,5)        ; define flux array
      FLUX_IVAR  = fltarr(Nobj,5)        ; define flux error array
      EXTINCTION = fltarr(Nobj,5)        ; define extinction array

      ; AB = 31.43 − 2.5 log(Fnu [nJy]) =>                ; AB mag def. gives...
      Fnu_nJy_u = 10^( (str.FIBERmag_u - 31.42)/(-2.5) )  ; flux in nano Jansky 
      Fnu_nJy_g = 10^( (str.FIBERmag_g - 31.42)/(-2.5) )  
      Fnu_nJy_r = 10^( (str.FIBERmag_r - 31.42)/(-2.5) )  
      Fnu_nJy_i = 10^( (str.FIBERmag_i - 31.42)/(-2.5) )  
      Fnu_nJy_z = 10^( (str.FIBERmag_z - 31.42)/(-2.5) )  

      FLUX(*,0) = ( Fnu_nJy_u / 3631. )                   ; flux in "nano maggie", 1 nMgy in flux is equivalent to 22.5 mag (AB system)
      FLUX(*,1) = ( Fnu_nJy_g / 3631. )                   
      FLUX(*,2) = ( Fnu_nJy_r / 3631. )                   
      FLUX(*,3) = ( Fnu_nJy_i / 3631. )                   
      FLUX(*,4) = ( Fnu_nJy_z / 3631. )                   

;      FLUX_IVAR(*,0) =
;      FLUX_IVAR(*,1) =
;      FLUX_IVAR(*,2) =
;      FLUX_IVAR(*,3) =
;      FLUX_IVAR(*,4) =

      EXTINCTION(*,0)= str.EXTINCTION_u
      EXTINCTION(*,1)= str.EXTINCTION_g
      EXTINCTION(*,2)= str.EXTINCTION_r
      EXTINCTION(*,3)= str.EXTINCTION_i
      EXTINCTION(*,4)= str.EXTINCTION_z
      Print,':: estimatePhotoZ.pro :: AB fiber magnitudes not enabled  --> ABORTING '
      stop
   end

   ELSE: begin
      Print,':: estimatePhotoZ.pro :: mag-keyword not valied  --> ABORTING '
      stop
   end
endcase

; creating structure for photo-z input
create_struct, strIN, 'fluxstructure',['PSFFLUX','PSFFLUX_IVAR','EXTINCTION'], 'F(5),F(5),F(5)'
strIN = replicate(strIN, Nobj)

; Filling structure with data
for ii=0,4 do begin
   strIN(*).PSFFLUX(ii)       = FLUX(*,ii)
   strIN(*).PSFFLUX_IVAR(ii)  = FLUX_IVAR(*,ii)
   strIN(*).EXTINCTION(ii)    = EXTINCTION(*,ii)
endfor

if vb eq 1 then print,':: estimatePhotoZ.pro :: Created and filled the structure "fluxstructure"'
if vb eq 1 then help,strIN,/str

; running the photo-z code on data
strOUT = ADD_X2_TAGS(strIN)

if vb eq 1 then print,':: estimatePhotoZ.pro :: Estimated the phot-z and returned the structure:'
if vb eq 1 then help,strout,/str

if vb eq 1 then print,' '
if vb eq 1 then print,':: estimatePhotoZ.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
