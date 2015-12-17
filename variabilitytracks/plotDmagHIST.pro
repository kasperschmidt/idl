;+
;----------------------------
;   NAME
;----------------------------
; plotDmagHIST.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure plots a histogram of the r-magnitude differences on a
; given time scale for an input quasar files with S82 epochs
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : string containing name and path of input data file
; Dt              : the time interval/time lag to create histogram for [days]
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; 
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotDmagHIST,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0z7AND0A2AND0gamma5_Tue_Feb_15_09:19:29_2011.fits',365.25,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-05-23  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotDmagHIST,datafile,Dt,EPS=EPS,VERBOSE=VERBOSE

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

dat = mrdfits(datafile,1)
Nlines = n_elements(dat.headobjid)

HEADID = dat(uniq(dat.headobjid)).headobjid
Nobj   = n_elements(HEADID)
if vb eq 1 then print,':: plotDmagHIST.pro :: found ',trim(Nobj),' objects in inputfile, with an average of ',trim(Nlines/Nobj),' epochs per object'


;for ii=0,Nobj-1 do begin
for ii=0,500-1 do begin
   objent = where(dat.headobjid eq HEADID(ii) and dat.PSFMAG_r ne -9999, Nepoch)
   for jj=0,Nepoch-2 do begin
      errquad  = sqrt(dat(objent(jj)).PSFMAGERR_r^2.+dat(objent(jj+1:Nepoch-1)).PSFMAGERR_r^2.); errors in quadrature
      Dmag     = abs(dat(objent(jj)).PSFMAG_r - dat(objent(jj+1:Nepoch-1)).PSFMAG_r) - errquad   ; the difference in magnitudes
      Dtime    = dat(objent(jj)).MJD_r    - dat(objent(jj+1:Nepoch-1)).MJD_r                ; the difference in MJDs
      goodent  = where(abs(Dtime) lt Dt+0.01*Dt and abs(Dtime) gt Dt-0.01*Dt, Ngood)     ; finding Dtimes that are within Dt+/-5%
      ;print,Ngood,ii,jj,'[',jj+1,Nepoch-1,']'
      ;print,Dtime(goodent)
      if goodent ne [-1] then Dmaggood = Dmag(goodent)                                      ; getting Dmags for good Dtime
      if n_elements(magall) gt 0 and goodent ne [-1] then magall = [magall,Dmaggood]  ; appending values
      if n_elements(magall) eq 0 and goodent ne [-1] then magall = Dmaggood           ; creating Dmaggood
   endfor
endfor

Nw = 0 ; resetting window numbering
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'DmagHIST.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Dmag histogram'
   thickall = 2
endelse
; getting 68 percentile (1 sigma)
magallsort = magall(sort(magall))
Nma = n_elements(magallsort)
lowMA  = magallsort(0.16*Nma)
highMA = magallsort(0.84*Nma)

lowMA95  = magallsort(0.025*Nma)
highMA95 = magallsort(0.975*Nma)


; setting plot range
XR = [lowMA95,highMA95]
YR = [0,Nma/15]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\Delta r_{mag}') $
        , ytitle =textoidl('#') $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.5 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
        , background = col.white

Dbin = (highMA-lowMA)/20.  ; the bin size of histogram is set to 1.20 of the 68 percentile

plothist,magall,bin=Dbin,col=col.black,thick=thickall*2,/overplot

XYOUTS,DX*0.95+XR[0],DY*0.90+YR[0],'Mean : '+trim(mean(magall),'(f10.3)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0
XYOUTS,DX*0.95+XR[0],DY*0.83+YR[0],'Median : '+trim(median(magall),'(f10.3)'),col=col.black,charsize=2.5,charthick=thickall,alignment=1.0


if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================





if vb eq 1 then print,' '
if vb eq 1 then print,':: plotDmagHIST.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
