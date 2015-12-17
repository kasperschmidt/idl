;+
; NAME:
;       TVRD_24
;
; PURPOSE:
;       This function reads a 24-bit image from a 24-bit or 8-bit
;       display device, returning a pixel-interleaved byte array 
;       of type BYTARR(3, xsize, ysize), where xsize and ysize are
;       the dimensions of the current graphics device.
;
; CATEGORY:
;       Graphics.
;
; CALLING SEQUENCE:
;       image = TVRD_24()
;
; INPUTS:
;       None
;
; KEYWORD PARAMETERS:
;       None. 
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;       image = tvrd_24()
;       
;
; MODIFICATION HISTORY:
;       Part of Struan's Surface Tutorial: 
;            http://www.sljus.lu.se/stm/IDL/Surf_Tips/
;       Written by:	Struan Gray, Sljusfysik, Lunds Universitet, 970305.
;-


function tvrd_24

  if !d.n_colors lt 257 then begin
    scr_image = tvrd()
    img_dims = size(scr_image)
    tvlct, red, grn, blu, /get
    image = bytarr(3, img_dims(1), img_dims(2), /nozero)
    image(0,*,*) = red(scr_image)
    image(1,*,*) = grn(scr_image)
    image(2,*,*) = blu(scr_image)
  endif else begin
    image = tvrd(true=1)
  endelse

 return, image

end   ; function tvrd_24
