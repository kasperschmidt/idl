;+
;----------------------------
;   NAME
;----------------------------
; imgsmoothing2D.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure taking an input image array and smoothing it with a 2D
; gaussian of specified width, i.e., convolving image with 2D gaussian PSF
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; imgarray        : Input image array to be smoothed
; width           : Vector with FWHM of 2D gaussian in pixels
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; WEIGHTARR       : set WEIGHTS equal to an array of the same dimentions
;                   as the imagarray with weight factors for each pixels to 
;                   be used in the smoothing. - NOT WORKING PROPERLY - 
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; imgsmooth       : The image array after smoothing with the gaussian kernel
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> imgsmoothing2D,imgarray,[2,3],imgsmooth,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
; - It seems that only FWHM of integer pixels are allowed/makes a
;   (significant) difference, which can become an issue for arrays
;   with few pixels
; - For images which are only has values in some pixels (2D histograms
;   for instance) the smoothing will smooth the edges towards the
;   background value/color creating and artificial 'rim/edge'. To
;   avoid this one can make a weghting... which is not working or use
;   the procedure imgsmoothingGWEIGHT.pro
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-02-03  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO imgsmoothing2D,imgarray,width,imgsmooth,VERBOSE=VERBOSE,WEIGHTARR=WEIGHTARR

VB = n_elements(VERBOSE)

Sarr = size(imgarray)

Nx = Sarr(1)
Ny = Sarr(2)
IMGCENTER = [round(Nx/2.),round(Ny/2.)]  ; center of image array (rounded if image size is even)

;creating 2D kernel
FWHM_KERNEL = width   ; full width half maximum in the x and y direction
CENT_lowerleft  = [0,0]          ; position for gaussians
CENT_lowerright = [Nx-1,0]       ;
CENT_upperleft  = [0,Ny-1]       ;
CENT_upperright = [Nx-1,Ny-1]    ;
PSF_ll = PSF_GAUSSIAN(NPIXEL=[Nx,Ny],FWHM=FWHM_KERNEL,CENTROID=CENT_lowerleft ,NDIMEN=2,/NORMALIZE)
PSF_lr = PSF_GAUSSIAN(NPIXEL=[Nx,Ny],FWHM=FWHM_KERNEL,CENTROID=CENT_lowerright,NDIMEN=2,/NORMALIZE)
PSF_ul = PSF_GAUSSIAN(NPIXEL=[Nx,Ny],FWHM=FWHM_KERNEL,CENTROID=CENT_upperleft ,NDIMEN=2,/NORMALIZE)
PSF_ur = PSF_GAUSSIAN(NPIXEL=[Nx,Ny],FWHM=FWHM_KERNEL,CENTROID=CENT_upperright,NDIMEN=2,/NORMALIZE)

kernel = fltarr(Nx,Ny)                            ; creating 0-value array to be the kernel
kernel = (kernel+PSF_ll+PSF_lr+PSF_ul+PSF_ur)/4.  ; adding PSF 'corners' to kernel and normalizing
;if n_elements(WEIGHTARR) gt 0 then kernel = WEIGHTARR*kernel

FFT_kernel = FFT(kernel,1)           ; FFT'ing the kernel (using the ,1 command to conserve normalisation,
                                     ; alternatively one could multiply with #pixels instead of ,1)
FFT_img    = FFT(imgarray)           ; Fourier transforming flux image 

if n_elements(WEIGHTARR) eq 0 then begin
   CONV_F     = FFT_img * FFT_kernel              ; calculating convolution in Fourier space
endif else begin
   FFT_wht    = FFT(WEIGHTARR)
   CONV_F     = (FFT_img -FFT_wht) * FFT_kernel   ; calculating convolution in Fourier space
endelse
 
CONV       = FFT(CONV_F,/inverse)    ; getting convolution in real space by inverse Fourier trans.
IMGSMOOTH  = real_part(CONV)  ; filling cube with convolved images

print,total(imgarray),total(imgsmooth)
;print,total(conv_f), total(conv)

if vb eq 1 then print,' '
if vb eq 1 then print,':: imgsmoothing2D.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
