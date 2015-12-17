;+
;----------------------------
;   NAME
;----------------------------
; slopeinvestigationPLOT.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure for plotting the slopeinvestigation.pro output
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : string containing name and path of input data file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ZBIN            : Number of redshift bins to average over in plots
;                   (default is 10)
; fitsfile        : name and path of original fits file used to create the input data file
; FIT             : FIT indicates what labels to use by given the space in which the input was
;                   performed. The default is FIT='gr'
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output.txt',/VERBOSE

; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_z0p2to4p0_outliers9999.txt',/VERBOSE,ZBIN=25
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_z0p2to4p0_outliers9999_seasons.txt',/VERBOSE,ZBIN=25
; IDL> slopeinvestigationPLOT,'S82QSOs_shenmatchALL.txt',/VERBOSE,ZBIN=40
; IDL> slopeinvestigationPLOT,'S82QSOs_shenmatchALL_NOsigmaadjustALL.txt',/VERBOSE,ZBIN=40
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_FGstars_grfit110207.txt',/VERBOSE,ZBIN=40

; --- original FG stars ---
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_FGstars_grfit110207.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted.fits'

; --- perfect FG stars ---
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_FGstars_grfit110209gauss.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_gaussdata.fits'

; --- perfect FG stars - testing SIGMAS ---
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_FGstars_grfitgaussSIG1.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_gaussdata_Fri_Feb_11_12:03:02_2011.fits'

; --- FG stars - variability added ---
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_FGstars_datvarfit110215.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_datvardata_wDMAG0.500000_Tue_Feb_15_17:20:42_2011.fits'
; --------------------------------------------------------------------------------------------------------------------------------------------
; IDL> slopeinvestigationPLOT,'slopeinvestigation_output_FGstars_datvarfit_wDMAG0.00000.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_datvardata_wDMAG0.500000_Tue_Feb_15_17:20:42_2011.fits'

; IDL> slopeinvestigationPLOT,'PDFs_FGstars_datvarfit/slopeinvestigation_output_FGstars_datvarfit_wDMAG0.700000.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_datvardata_wDMAG0.700000_Wed_Feb_16_13:13:55_2011.fits'
; --------------------------------------------------------------------------------------------------------------------------------------------
; IDL> slopeinvestigationPLOT,'PDFs_FGstars_gaussvarfit/slopeinvestigation_output_FGstars_gaussvarfit_wDMAG0.700000.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_gaussvardata_wDMAG0.700000_Wed_Feb_16_13:14:18_2011.fits'

; IDL> slopeinvestigationPLOT,'PDFs_FGstars_gaussvarfit/slopeinvestigation_output_FGstars_gaussvarfit_wDMAG0.100000.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_gaussvardata_wDMAG0.100000_Wed_Feb_16_09:37:30_2011.fits'
; --------------------------------------------------------------------------------------------------------------------------------------------
; IDL> slopeinvestigationPLOT,'PDFs_FGstars_datvarfit_1year/slopeinvestigation_output_FGstars_datvarfit_wDMAG0.00000.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_datvardata_wDMAG0.00000_Wed_Feb_16_20:53:28_2011.fits'

; IDL> slopeinvestigationPLOT,'PDFs_FGstars_datvarfit_1year/slopeinvestigation_output_FGstars_datvarfit_wDMAG0.700000.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_TOP500extracted_datvardata_wDMAG0.700000_Thu_Feb_17_00:39:47_2011.fits'


; --- RRL ---
; IDL> slopeinvestigationPLOT,'PDFs_RRL_grfit110215/slopeinvestigation_output_RRL_grfit110215.txt',/VERBOSE,ZBIN=40,fitsfile='/Users/kasperborelloschmidt/work/casjobs_SDSS/RRL_sesar_etal2009/sesar09RRL_sorted.fits'


;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-26  started by K. B. Schmidt (MPIA)
; 2011-02-08  keyword DATAFILE, FIT added K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO slopeinvestigationPLOT,datafile,EPS=EPS,VERBOSE=VERBOSE,ZBIN=ZBIN,FITSFILE=FITSFILE,FIT=FIT

ZB = n_elements(ZBIN)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)


; reading result file just written for plotting 
readcol,FORMAT=('A,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f'),datafile,objid,abest,amed,amean,aplus68,aminus68,aplus95,aminus95,bbest,bmed,bmean,bplus68,bminus68,bplus95,bminus95,zs,season

Nobj = n_elements(objid)

; making array with redshift bins
if ZB eq 0 then ZBIN = 10
zarr = min(zs)+(findgen(ZBIN+1)/(ZBIN)*(max(zs)-min(zs)))

Yfrac = 1./30.  ; the fraction of Nobj to be shown on histograms y-axis

Nw = 4
;=============================================================================================
;= = = Slope Histogram = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopeinvestigationPLOTS/ahist.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope Hist'
   thickall = 2
endelse

dbin = 0.01 ; bin size
; setting plot range
XR = [min(abest)-dbin,max(abest)+dbin]
YR = [0,Nobj*Yfrac]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,abest,abest, col=col.black    $
        , /NODATA $
        , xtitle ='a for a*x+b' $
        , ytitle ='#' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

plothist,abest,bin=dbin,/overplot,col=col.red,thick=thickall,/fill,fcolor=col.red
plothist,amed,bin=dbin,/overplot,col=col.blue,thick=thickall
plothist,amean,bin=dbin,/overplot,col=col.green,thick=thickall

XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'best a',col=col.red,charsize=1.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'median a',col=col.blue,charsize=1.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'mean a',col=col.green,charsize=1.5,charthick=thickall

;overplotting average error bar
xx = fltarr(2)
oploterror,xx+DX*0.8+XR[0],xx+DY*0.95+YR[0],xx+mean(aminus68),xx,/LOBAR,errcol=col.black,errthick=thickall
oploterror,xx+DX*0.8+XR[0],xx+DY*0.95+YR[0],xx+mean(aplus68) ,xx,/HIBAR,errcol=col.black,errthick=thickall
oploterror,xx+DX*0.8+XR[0],xx+DY*0.90+YR[0],xx+mean(aminus95),xx,/LOBAR,errcol=col.black,errthick=thickall
oploterror,xx+DX*0.8+XR[0],xx+DY*0.90+YR[0],xx+mean(aplus95) ,xx,/HIBAR,errcol=col.black,errthick=thickall

XYOUTS,DX*0.3+XR[0],DY*0.95+YR[0],'<68% confidence>',col=col.black,charsize=1.5,charthick=thickall
XYOUTS,DX*0.3+XR[0],DY*0.90+YR[0],'<95% confidence>',col=col.black,charsize=1.5,charthick=thickall


if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
;= = = scale Histogram = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopeinvestigationPLOTS/bhist.eps' ; name for movie frames
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Norm Hist'
   thickall = 2
endelse

dbin = 0.001 ; bin size
; setting plot range
;XR = [min(bbest)-dbin,max(bbest)+dbin]
XR = [-0.3,0.3]
YR = [0,Nobj*Yfrac]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

;=== PLOTTING LIGHT CURVE(S) ===
plot,abest,abest, col=col.black    $
        , /NODATA $
        , xtitle ='b for a*x+b' $
        , ytitle ='#' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

plothist,bbest,bin=dbin,/overplot,col=col.red,thick=thickall,/fill,fcolor=col.red
plothist,bmed,bin=dbin,/overplot,col=col.blue,thick=thickall
plothist,bmean,bin=dbin,/overplot,col=col.green,thick=thickall

XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'best b',col=col.red,charsize=1.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],'median b',col=col.blue,charsize=1.5,charthick=thickall
XYOUTS,DX*0.05+XR[0],DY*0.85+YR[0],'mean b',col=col.green,charsize=1.5,charthick=thickall

;overplotting average error bar
xx = fltarr(2)
oploterror,xx+DX*0.8+XR[0],xx+DY*0.95+YR[0],xx+mean(bminus68),xx,/LOBAR,errcol=col.black,errthick=thickall
oploterror,xx+DX*0.8+XR[0],xx+DY*0.95+YR[0],xx+mean(bplus68) ,xx,/HIBAR,errcol=col.black,errthick=thickall
oploterror,xx+DX*0.8+XR[0],xx+DY*0.90+YR[0],xx+mean(bminus95),xx,/LOBAR,errcol=col.black,errthick=thickall
oploterror,xx+DX*0.8+XR[0],xx+DY*0.90+YR[0],xx+mean(bplus95) ,xx,/HIBAR,errcol=col.black,errthick=thickall

XYOUTS,DX*0.3+XR[0],DY*0.95+YR[0],'<68% confidence>',col=col.black,charsize=1.5,charthick=thickall
XYOUTS,DX*0.3+XR[0],DY*0.90+YR[0],'<95% confidence>',col=col.black,charsize=1.5,charthick=thickall


if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                    ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopeinvestigationPLOTS/slopeVSz.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Slope vs redshift'
   thickall = 2
endelse
; setting plot range
XR = [min(zs)-0.1*(max(zs)-min(zs)),max(zs)+0.1*(max(zs)-min(zs))]
YR = [0,2]

NNmin = 5

plot,zs,zs, col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle ='a for a*x+b' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

PLOTSYM,0,1.0,/FILL
oplot,zs,abest,col=col.gray,psym=8,thick=thickall 

for kk=0,n_elements(zarr)-2 do begin ; looping over redshift bins
   zent = where(zs ge zarr(kk) AND zs lt zarr(kk+1),Czs)
   if zent ne [-1] then begin
      slopezbin = abest(zent)
      slopezsorted = slopezbin(sort(slopezbin))
      slopemed = median(slopezbin)
      ; 68% confidence level
      Slow68  = slopezsorted[0.16*Czs]
      Shigh68 = slopezsorted[0.84*Czs]
      Serr68p = Shigh68 - slopemed
      Serr68m = slopemed - Slow68
      ; 95% confidence level (2 sigma)
      Slow95  = slopezsorted[0.025*Czs]
      Shigh95 = slopezsorted[0.975*Czs]
      Serr95p = Shigh95 - slopemed
      Serr95m = slopemed - Slow95
      ; standard deviation of mean
      Sstnd   = total( 1./n_elements(slopezbin) * sqrt((slopezbin-slopemed)^2) )

      BinC = zarr(kk)+(zarr(kk+1)-zarr(kk))/2

      PLOTSYM,8,2.,/FILL
      oplot,zs(zent)*0+BinC,abest*0+slopemed,psym=8,col=col.black

      oploterror,zs(zent)*0+BinC,abest*0+slopemed,slopezsorted*0+Sstnd,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall
      if kk gt 34 then print,kk,'===',slopezbin,size(slopezbin)
   endif
endfor

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopeinvestigationPLOTS/aVSb.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'offset vs slope'
   thickall = 2
endelse
; setting plot range
XR = [min(abest)-0.1*(max(abest)-min(abest)),max(abest)+0.1*(max(abest)-min(abest))]
YR = [min(bbest)-0.1*(max(bbest)-min(bbest)),max(bbest)+0.1*(max(bbest)-min(bbest))]
XR = [0,1]
YR = [-0.5,0.5]


plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle ='a' $
        , ytitle ='b' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

PLOTSYM,0,1.5,/FILL
oplot,abest,bbest,col=col.gray,psym=8,thick=thickall 

PLOTSYM,0,1.5
;oplot,abest,bbest,col=col.black,psym=8,thick=thickall 

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if n_elements(fitsfile) ne 0 then begin   ; if the original fits file is given
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   if n_elements(FIT) eq 0 then fit = 'gr'                ; default lables set to gr
   fits = mrdfits(fitsfile,1)
   Hids = fits(uniq(fits.headobjid)).headobjid
   
   Nobj = n_elements(Hids)
   Nobjplot = Nobj


   ;=============================================================================================
   for ii=0,Nobjplot-1 do begin    ; looping over objects with shen data

      Objent = where(fits.headobjid eq Hids(ii))   ; entries for given object
      if fit eq 'gr' then begin
         Xdat = fits(objent).psfmag_g
         Ydat = fits(objent).psfmag_r
         Xerr = fits(objent).psfmagERR_g
         Yerr = fits(objent).psfmagERR_r
      endif
      if fit eq 'ui' then begin
         Xdat = fits(objent).psfmag_u
         Ydat = fits(objent).psfmag_i
         Xerr = fits(objent).psfmagERR_u
         Yerr = fits(objent).psfmagERR_i
      endif
   
      ;= = = mag vs mag plot = = =
      !p.multi = [0,0,0]
      if PS eq 1 then begin
         set_plot, 'ps'
         col=getcolor(/load)     ; get color table for plot
         plot1 = 'slopeinvestigationPLOTS/'+strtrim(fit,2)+'space_obj'+strtrim(ii,2)+'_slopeinvPLOT.eps' ; name of eps file
         device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK;, xsize=7
         thickall = 6
      endif else begin
         set_plot, 'x'
         col=getcolor(/load)     ; get color table for plot
         device, retain=2        ; ensuring that plotting windows 'regenerate'
         window, Nw, xsize=600, ysize=500, title = 'mag-mag space'
         thickall = 2
      endelse

      ; setting plot range
      XR = [median(Xdat)+1,median(Xdat)-1]
      YR = [median(Ydat)+1,median(Ydat)-1]
;      XR = [median(Xdat)+0.25,median(Xdat)-0.25]
;      YR = [median(Ydat)+0.25,median(Ydat)-0.25]
      DX = XR[1]-XR[0]
      DY = YR[1]-YR[0]
      XT = STRMID(fit, 0, 1)
      YT = STRMID(fit, 1, 1)

      ;=== PLOTTING LIGHT CURVE(S) ===
      plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =XT $
        , ytitle =YT $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

      NX = 50
      XX = findgen(NX)-NX/2. + mean(Xdat)
      oplot,XX,Abest(ii)*XX+(mean(Ydat)-Abest(ii)*mean(Xdat)+Bbest(ii)),linestyle=0,thick=thickall,col=col.blue
 
      ; overplotting 'error-lines'
      Xlow  = XX(0:NX/2.)
      Xhigh = XX(NX/2.:NX-1)
      oplot,Xhigh,(Abest(ii)+Aplus68(ii)) *Xhigh+(mean(Ydat)-(Abest(ii)+Aplus68(ii))*mean(Xdat)+(Bbest(ii)+Bplus68(ii)) ) ,linestyle=2,thick=thickall,col=col.blue
      oplot,Xlow ,(Abest(ii)+Aplus68(ii)) *Xlow +(mean(Ydat)-(Abest(ii)+Aplus68(ii))*mean(Xdat)+(Bbest(ii)-Bminus68(ii))) ,linestyle=2,thick=thickall,col=col.blue
      oplot,Xlow ,(Abest(ii)-Aminus68(ii))*Xlow +(mean(Ydat)-(Abest(ii)-Aminus68(ii))*mean(Xdat)+(Bbest(ii)+Bplus68(ii)) ),linestyle=2,thick=thickall,col=col.blue
      oplot,Xhigh,(Abest(ii)-Aminus68(ii))*Xhigh+(mean(Ydat)-(Abest(ii)-Aminus68(ii))*mean(Xdat)+(Bbest(ii)-Bminus68(ii))),linestyle=2,thick=thickall,col=col.blue

      oplot,Xhigh,(Abest(ii)+Aplus95(ii)) *Xhigh+(mean(Ydat)-(Abest(ii)+Aplus95(ii))*mean(Xdat)+(Bbest(ii)+Bplus95(ii)) ) ,linestyle=3,thick=thickall,col=col.blue
      oplot,Xlow ,(Abest(ii)+Aplus95(ii)) *Xlow +(mean(Ydat)-(Abest(ii)+Aplus95(ii))*mean(Xdat)+(Bbest(ii)-Bminus95(ii))) ,linestyle=3,thick=thickall,col=col.blue
      oplot,Xlow ,(Abest(ii)-Aminus95(ii))*Xlow +(mean(Ydat)-(Abest(ii)-Aminus95(ii))*mean(Xdat)+(Bbest(ii)+Bplus95(ii)) ),linestyle=3,thick=thickall,col=col.blue
      oplot,Xhigh,(Abest(ii)-Aminus95(ii))*Xhigh+(mean(Ydat)-(Abest(ii)-Aminus95(ii))*mean(Xdat)+(Bbest(ii)-Bminus95(ii))),linestyle=3,thick=thickall,col=col.blue

      plotsym,0,1.5,/fill
      oplot,Xdat,Ydat,psym=8,col=col.red
      oploterror,Xdat,Ydat,Xerr,Yerr,psym=2,col=col.red,ERRCOLOR=col.red,thick=thickall

      plotsym,0,1.5
      oplot,Xdat,Ydat,psym=8,col=col.black,thick=thickall

      XYOUTS,DX*0.05+XR[0],DY*0.95+YR[0],'DR7 '+strtrim(Hids(ii),2),col=col.black,charsize=1.5,charthick=thickall
      XYOUTS,DX*0.05+XR[0],DY*0.90+YR[0],"MCMC 'best' fit",col=col.blue,charsize=1.5,charthick=thickall

      if PS eq 1 then begin
         device, /close
         set_plot, 'x'
      endif else begin
         wait,0.5       ; waiting between each plot to screen
      endelse
   endfor
   Nw = Nw+1                    ; incremeting window number by 1
   ;=============================================================================================
   
   
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
endif  ; end of original fits file sequence
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


if vb eq 1 then print,' '
if vb eq 1 then print,':: slopeinvestigationPLOT.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
