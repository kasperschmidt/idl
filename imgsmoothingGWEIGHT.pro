;+
;----------------------------
;   NAME
;----------------------------
; imgsmoothingGWEIGHT.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure taking an input image array and smoothing it by looping
; over its pixels and assigning the mean 2D gaussian of specified
; width (+ weight mask) weighted value to each pixel. This is an
; alternative to the procedure imgsmoothing2D.pro which cannot take a 
; weight map, i.e. ignore pixels because it is a convolution and not a
; looping over pixels like it is done here.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; imgarray        : Input image array to be smoothed
; width           : Vector with FWHM of 2D gaussian in pixels [width x,widht y]
; weigth          : array with the weights of each pixel. Weight 0 is ignored
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; imgsmooth       : The image array after smoothing with the gaussian kernel
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> imgsmoothingGWEIGHT,imgarray,[2,3],weightarr,imgsmooth,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-04-08  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO imgsmoothingGWEIGHT,imgarray,width,weightarr,imgsmooth,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)

Sarr = size(imgarray)
Nx = Sarr(1)
Ny = Sarr(2)

imgsmooth = imgarray

for ii=0,Nx-1 do begin                                                           ; looping over x-dimension
   for jj=0,Ny-1 do begin                                                        ; looping over y-dimension
      if weightarr(ii,jj) ne 0 then begin                                        ; checking that the pixel is not ignored
         GAUSSarr = PSF_GAUSSIAN(Npixel=[Nx,Ny],FWHM=width,CENTROID=[ii,jj])     ; creating gaussian weighting function on pixel (ii,jj)
         goodent = where(weightarr ne 0)                                         ; entries with weights - 0s ignored
         VALTOT  = total(imgarray(goodent)*weightarr(goodent)*GAUSSarr(goodent)) ; calculating sum over all pixels with weights
         NORMTOT = total(weightarr(goodent)*GAUSSarr(goodent))                   ; normalising by weights
         imgsmooth(ii,jj) = VALTOT/NORMTOT
      endif
   endfor
endfor

if vb eq 1 then print,' '
if vb eq 1 then print,':: imgsmoothingGWEIGHT.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
