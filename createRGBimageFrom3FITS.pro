;+
;----------------------------
;   NAME
;----------------------------
; createRGBimageFrom3FITS.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure reading 3 fits images and turning into a color (eps) image.
; Via keywords specific regions of the input fits files can be cut out
; and used.
; NB! the images are given as RED (high wave/low freq), GREEN (mid
; wave/mid freq) and BLUE (low wave/high freq)
;----------------------------
;   COMMENTS
;----------------------------
;... Why not use bash> ds9 -rgb -red red.fits -green green.fits -blue blue.fits &
;----------------------------
;   INPUTS:
;----------------------------
; fitsR           : string containing name and path of RED fits file (high wave/low freq)
; fitsG           : string containing name and path of GREEN fits file (mid wave/mid freq)
; fitsB           : string containing name and path of BLUE fits file (low wave/high freq)
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; RADEC           : Two component vector with ra and dec pisition to
;                   cut around
; CUTRAD          : Size of sub fits to cut out around RADEC in arcsec
;                   Default value is 5''
; NAMEKEY         : String appended to the image output name(s)
; FITSEXT         : Three component vector containing the the extensions
;                   to read of the red, green and blue fits file. The
;                   default is reading the 0'th extensions, i.e. [0,0,0]
; REBINSCALE      : In case the pixel size of the fits images are
;                   different this keyword gives the images to which
;                   size the others are rebinned (expanded or contracted).
;                   The input is a string contaning, 'red', 'green' or 'blue'
;                   NB --- input case sensitive! ---
;                   As a default the pixel scales are assumed identical.
; /EPS            : set /EPS to write plots to .eps files
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; --- SPITZER CH3CH2CH1 ---
; IDL> createRGBimageFrom3FITS,'/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/spitzer/irac/irac_ch3_go2_sci_10.fits','/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/spitzer/irac/irac_ch2_go2_sci_10.fits','/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/spitzer/irac/irac_ch1_go2_sci_10.fits',/VERBOSE,RADEC=[150.15129396,2.35479737],CUTRAD=5.0,NAMEKEY='SPITZER_irac_CH3CH2CH1',/eps

; --- SDSS irg ---
; IDL> createRGBimageFrom3FITS,'/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/sdss/i/matched_psf/sdss_i_matched-psf_077_sci_20.fits','/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/sdss/r/matched_psf/sdss_r_matched-psf_077_sci_20.fits','/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/sdss/g/matched_psf/sdss_g_matched-psf_077_sci_20.fits',/VERBOSE,RADEC=[150.15129396,2.35479737],CUTRAD=5.0,NAMEKEY='SDSS_irg',/eps

; --- SDSS zru ---
; IDL> createRGBimageFrom3FITS,'/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/sdss/z/matched_psf/sdss_z_matched-psf_077_sci_20.fits','/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/sdss/r/matched_psf/sdss_r_matched-psf_077_sci_20.fits','/Users/kasperborelloschmidt/work/3dHST/ancillarydata/orient1_01414/irsa.ipac.caltech.edu/data/COSMOS/images/sdss/u/matched_psf/sdss_u_matched-psf_077_sci_20.fits',/VERBOSE,RADEC=[150.15129396,2.35479737],CUTRAD=5.0,NAMEKEY='SDSS_zru',/eps

; --- WFC3+ACS
; IDL> createRGBimageFrom3FITS,'irsa.ipac.caltech.edu/data/COSMOS/images/acs/I/acs_I_077_sci_13.fits','3DHSTwfc3/orient1_drz.fits','3DHSTwfc3/orient1_drz.fits',/VERBOSE,RADEC=[150.15129396,2.35479737],CUTRAD=5.0,NAMEKEY='HST_acsI_wfc3F140W_wfc3F140W',FITSEXT=[0,1,1],REBINSCALE='green',/eps

; IDL> createRGBimageFrom3FITS,'irsa.ipac.caltech.edu/data/COSMOS/images/acs/I/acs_I_077_sci_13.fits','3DHSTwfc3/orient1_drz.fits','3DHSTwfc3/orient1_drz.fits',/VERBOSE,RADEC=[150.15129396,2.35479737],CUTRAD=5.0,NAMEKEY='HST_acsI_wfc3F140W_wfc3F140W_acsscale',FITSEXT=[0,1,1],REBINSCALE='red',/eps

;IDL> createRGBimageFrom3FITS,'/Users/kasperborelloschmidt/work/applications/postagestamps/COSMOS-3-G141_00346/0001_150.08155000_2.31854000_acs_I_mosaic_30mas_sci.fits','/Users/kasperborelloschmidt/work/applications/postagestamps/COSMOS-3-G141_00346/COSMOS-3-G141_00346_thumb.fits','/Users/kasperborelloschmidt/work/applications/postagestamps/COSMOS-3-G141_00346/0001_150.08155000_2.31854000_COSMOS.Ks.original_psf.v5.fits',/VERBOSE,RADEC=[150.08154755,2.31854383],CUTRAD=2.0,NAMEKEY='GOODS-S-23-G141_00588_colorcomp_iHK',REBINSCALE='red',FITSEXT=[0,0,0]

;ds9 -rgb -red 0001_150.08155000_2.31854000_acs_I_mosaic_30mas_sci.fits -green COSMOS-3-G141_00346_thumb.fits -blue 0001_150.08155000_2.31854000_COSMOS.Ks.original_psf.v5.fits &


;IDL> createRGBimageFrom3FITS,'/Users/kasperborelloschmidt/work/applications/postagestamps/COSMOS-3-G141_00346/0001_150.08155000_2.31854000_COSMOS.gp.original_psf.v2.fits','/Users/kasperborelloschmidt/work/applications/postagestamps/COSMOS-3-G141_00346/0001_150.08155000_2.31854000_COSMOS.rp.original_psf.v2.fits','/Users/kasperborelloschmidt/work/applications/postagestamps/COSMOS-3-G141_00346/0001_150.08155000_2.31854000_COSMOS.ip.original_psf.v2.fits',/VERBOSE,RADEC=[150.08154755,2.31854383],CUTRAD=2.0,NAMEKEY='GOODS-S-23-G141_00588_colorcomp_SUBARUgri',REBINSCALE='red'

;ds9 -rgb -red 0001_150.08155000_2.31854000_COSMOS.gp.original_psf.v2.fits -green 0001_150.08155000_2.31854000_COSMOS.rp.original_psf.v2.fits -blue 0001_150.08155000_2.31854000_COSMOS.ip.original_psf.v2.fits &



;----------------------------
;   BUGS
;----------------------------
; - There is no way of rotating images, so they are assumed to be
;   oriented in the same whay when co-added to a color image
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-07-12  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ zscale.pro
@ namedate.pro
;----------------------------
;-
PRO createRGBimageFrom3FITS,fitsR,fitsG,fitsB,RADEC=RADEC,CUTRAD=CUTRAD,EPS=EPS,VERBOSE=VERBOSE,STP=STP,NAMEKEY=NAMEKEY,FITSEXT=FITSEXT,REBINSCALE=REBINSCALE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

KEY = n_elements(NAMEKEY)
CR  = n_elements(cutrad)
PS  = n_elements(EPS)
VB  = n_elements(VERBOSE)
; ------------------------------------------------------------------------------------------------------------------------
if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Reading fits images'
; reading input images
if n_elements(FITSEXT) eq 0 then FITSEXT = [0,0,0]
REDarr   = mrdfits(fitsR,FITSEXT[0],REDhdr,/SILENT)
GREENarr = mrdfits(fitsG,FITSEXT[1],GREENhdr,/SILENT)
BLUEarr  = mrdfits(fitsB,FITSEXT[2],BLUEhdr,/SILENT)

namedate,fitsR,path,redfile,extension,date,dateus,/local    ; getting the file name of the red file
namedate,fitsG,path,greenfile,extension,date,dateus,/local  ; getting the file name of the green file
namedate,fitsB,path,bluefile,extension,date,dateus,/local   ; getting the file name of the blue file
; ------------------------------------------------------------------------------------------------------------------------
if n_elements(RADEC) eq 2 then begin  ; cutting out sub-images if RADEC vector is given
   if CR eq 0 then begin
      cutrad = 5.  ; default value of 5 arcsec
      if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Setting cutout "radius" to default of 5 arcsec'
   endif

   if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Extracting astrometric information and cutting out sub-images'
   ; ---- RED ----
   EXTAST,REDhdr,REDast
   pixrad = abs(cutrad/(REDast.CD#REDast.Cdelt*3600.))  ; gettign # pixels to cut out around RADEC
   ADXY,REDhdr,RADEC[0],RADEC[1],xRED,yRED  ; getting the x and y coordinates for given RADEC
   hextract,REDarr,REDhdr,REDarr_sub,REDhdr_sub,xRED-pixrad[0],xRED+pixrad[0],yRED-pixrad[1],yRED+pixrad[1],/SILENT ; cutting out image
   REDarr   = REDarr_sub                    ; overwrite input image array
   REDhdr   = REDhdr_sub                    ; overwrite input image hdr
   ; ---- GREEN ----
   EXTAST,GREENhdr,GREENast
   pixrad = abs(cutrad/(GREENast.CD#GREENast.Cdelt*3600.))  ; gettign # pixels to cut out around RADEC
   ADXY,GREENhdr,RADEC[0],RADEC[1],xGREEN,yGREEN  ; getting the x and y coordinates for given RADEC
   hextract,GREENarr,GREENhdr,GREENarr_sub,GREENhdr_sub,xGREEN-pixrad[0],xGREEN+pixrad[0],yGREEN-pixrad[1],yGREEN+pixrad[1],/SILENT ; cutting out image
   GREENarr   = GREENarr_sub                ; overwrite input image array
   GREENhdr   = GREENhdr_sub                ; overwrite input image hdr
   ; ---- BLUE ----
   EXTAST,BLUEhdr,BLUEast
   pixrad = abs(cutrad/(BLUEast.CD#BLUEast.Cdelt*3600.))  ; gettign # pixels to cut out around RADEC
   ADXY,BLUEhdr,RADEC[0],RADEC[1],xBLUE,yBLUE  ; getting the x and y coordinates for given RADEC
   hextract,BLUEarr,BLUEhdr,BLUEarr_sub,BLUEhdr_sub,xBLUE-pixrad[0],xBLUE+pixrad[0],yBLUE-pixrad[1],yBLUE+pixrad[1],/SILENT ; cutting out image
   BLUEarr   = BLUEarr_sub                  ; overwrite input image array
   BLUEhdr   = BLUEhdr_sub                  ; overwrite input image hdr
endif

; ------------------------------------------------------------------------------------------------------------------------
if n_elements(REBINSCALE) ne 0 then begin                                   ; rebinning arrays if not of the same size
   if REBINSCALE eq 'red' then begin
      if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Scaling arrays to RED pixel size'
      Srebin = size(REDarr)                                                 ; getting size of GREEN array
      newx   = Srebin(1)
      newy   = Srebin(2)

      Srebin = size(GREENarr)                                               ; getting size of GREEN array
      greenx  = Srebin(1)
      greeny  = Srebin(2)
      if (greenx ne newx) or (greeny ne newy) then begin
         hrebin, GREENarr, GREENhdr, GREENarrnew, GREENhdrnew, newx, newy   ; rebinning array
         GREENarr = GREENarrnew                                             ; overwriting old array
         GREENhdr = GREENhdrnew                                             ; overwriting old header
      endif

      Srebin = size(BLUEarr)                                                ; getting size of BLUE array
      bluex  = Srebin(1)
      bluey  = Srebin(2)
      if (bluex ne newx) or (bluey ne newy) then begin
         hrebin, BLUEarr, BLUEhdr, BLUEarrnew, BLUEhdrnew, newx, newy       ; rebinning array
         BLUEarr = BLUEarrnew                                               ; overwriting old array
         BLUEhdr = BLUEhdrnew                                               ; overwriting old header
      endif
   endif

   if REBINSCALE eq 'green' then begin
      if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Scaling arrays to GREEN pixel size'
      Srebin = size(GREENarr)                                               ; getting size of GREEN array
      newx   = Srebin(1)
      newy   = Srebin(2)

      Srebin = size(REDarr)                                                 ; getting size of RED array
      redx   = Srebin(1)
      redy   = Srebin(2)
      if (redx ne newx) or (redy ne newy) then begin 
         hrebin, REDarr, REDhdr, REDarrnew, REDhdrnew, newx, newy           ; rebinning array
         REDarr = REDarrnew                                                 ; overwriting old array
         REDhdr = REDhdrnew                                                 ; overwriting old header
      endif

      Srebin = size(BLUEarr)                                                ; getting size of BLUE array
      bluex  = Srebin(1)
      bluey  = Srebin(2)
      if (bluex ne newx) or (bluey ne newy) then begin
         hrebin, BLUEarr, BLUEhdr, BLUEarrnew, BLUEhdrnew, newx, newy       ; rebinning array
         BLUEarr = BLUEarrnew                                               ; overwriting old array
         BLUEhdr = BLUEhdrnew                                               ; overwriting old header
      endif
   endif

   if REBINSCALE eq 'blue' then begin
      if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Scaling arrays to BLUE pixel size'
      Srebin = size(BLUEarr)                                                ; getting size of BLUE array
      newx   = Srebin(1)
      newy   = Srebin(2)

      Srebin = size(GREENarr)                                               ; getting size of GREEN array
      greenx  = Srebin(1)
      greeny  = Srebin(2)
      if (greenx ne newx) or (greeny ne newy) then begin
         hrebin, GREENarr, GREENhdr, GREENarrnew, GREENhdrnew, newx, newy   ; rebinning array
         GREENarr = GREENarrnew                                             ; overwriting old array
         GREENhdr = GREENhdrnew                                             ; overwriting old header
      endif

      Srebin = size(REDarr)                                                 ; getting size of RED array
      redx   = Srebin(1)
      redy   = Srebin(2)
      if (redx ne newx) or (redy ne newy) then begin
         hrebin, REDarr, REDhdr, REDarrnew, REDhdrnew, newx, newy           ; rebinning array
         REDarr = REDarrnew                                                 ; overwriting old array
         REDhdr = REDhdrnew                                                 ; overwriting old header
      endif
   endif
endif
; ------------------------------------------------------------------------------------------------------------------------
if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Scaling red, green and blue image arrays'
zscale,REDarr,z1,z2                               ; calculating IRAF(DS9) z-scale limits for IMGarr
REDbyte = BYTSCL(REDarr,MIN=z1,MAX=z2)           ; image turned into bytscale for plotting

zscale,GREENarr,z1,z2                               ; calculating IRAF(DS9) z-scale limits for IMGarr
GREENbyte = BYTSCL(GREENarr,MIN=z1,MAX=z2)           ; image turned into bytscale for plotting

zscale,BLUEarr,z1,z2                               ; calculating IRAF(DS9) z-scale limits for IMGarr
BLUEbyte = BYTSCL(BLUEarr,MIN=z1,MAX=z2)           ; image turned into bytscale for plotting

image = [[[REDbyte] ], [[GREENbyte] ], [[BLUEbyte] ]] ; creating color image array
; ------------------------------------------------------------------------------------------------------------------------
if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: Displaying image'
Nw = 0 ; resetting window numbering
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'createRGBimageFrom3FITS_output.eps'
   if KEY eq 1 then plot1 = 'createRGBimageFrom3FITS_'+trim(NAMEKEY)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=600, title = 'plotSEDand2DSpec'
   thickall = 2
endelse
; setting plot range
XR = [RADEC[0]-cutrad/3600.,RADEC[0]+cutrad/3600.]
YR = [RADEC[1]-cutrad/3600.,RADEC[1]+cutrad/3600.]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

; position of TVimage array
Xminpos = 0.20
Yminpos = 0.17
Xmaxpos = 0.92
Ymaxpos = 0.90
TVP = [Xminpos,Yminpos,Xmaxpos,Ymaxpos]
POSPLOT = TVP

plot,fltarr(2),fltarr(2), /NODATA, background=col.white,col=col.white  ; creating all white window

tvimage,image,POSITION=TVP,/nointerpolation;,/overplot,/keep_aspect_ratio

device, decomposed=0     ; tip before loading color scheme
col=getcolor(/load)

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , /NOERASE $
        , xtitle =textoidl('RA [deg]') $
        , ytitle =textoidl('DEC [deg]') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , XTICKFORMAT = '(F10.4)' $
        , YTICKFORMAT = '(F10.4)' $
        , XTICKS = 3 $
        , YTICKS = 7 $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , position=POSPLOT $
        , background = col.white


SCALEr = ' '
SCALEg = ' '
SCALEb = ' '
if n_elements(REBINSCALE) ne 0 then begin  
   if REBINSCALE eq 'red'   then SCALEr = 'scl: '  ; String to add filename to indicate scale used
   if REBINSCALE eq 'green' then SCALEg = 'scl: '  ; String to add filename to indicate scale used
   if REBINSCALE eq 'blue'  then SCALEb = 'scl: '  ; String to add filename to indicate scale used
endif

XYOUTS,DX*0.90+XR[0],DY*0.15+YR[0],trim(SCALEr+redfile),col=col.white,charsize=2.5,charthick=thickall,alignment=1.0
PLOTSYM,0,3.0,/fill
OPLOT,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.17+YR[0],psym=8,col=col.white
PLOTSYM,0,2.0,/fill
OPLOT,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.17+YR[0],psym=8,col=col.red

XYOUTS,DX*0.90+XR[0],DY*0.10+YR[0],trim(SCALEg+greenfile),col=col.white,charsize=2.5,charthick=thickall,alignment=1.0
PLOTSYM,0,3.0,/fill
OPLOT,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.12+YR[0],psym=8,col=col.white
PLOTSYM,0,2.0,/fill
OPLOT,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.12+YR[0],psym=8,col=col.green

XYOUTS,DX*0.90+XR[0],DY*0.05+YR[0],trim(SCALEb+bluefile),col=col.white,charsize=2.5,charthick=thickall,alignment=1.0
PLOTSYM,0,3.0,/fill
OPLOT,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.07+YR[0],psym=8,col=col.white
PLOTSYM,0,2.0,/fill
OPLOT,fltarr(2)+DX*0.95+XR[0],fltarr(2)+DY*0.07+YR[0],psym=8,col=col.blue

; printing size in uppe left corner
imgsize  = size(image)
EXTAST,REDhdr,REDast   ; extracting astrometric information from (potentially) new header
XYSIZE   = imgsize[1:2]*(REDast.CD#REDast.Cdelt*3600.)
XYOUTS,DX*0.05+XR[0],DY*0.92+YR[0],trim(abs(XYSIZE(0)),'(f8.2)')+'"x'+trim(abs(XYSIZE(1)),'(f8.2)')+'"',col=col.white,charsize=2.5,charthick=thickall,alignment=0.0



if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


write_jpeg, 'testimage.jpg', image, true = 3,qual=99


if vb eq 1 then print,' '
if vb eq 1 then print,':: createRGBimageFrom3FITS.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
