;+
;----------------------------
;   NAME
;----------------------------
;  plotseasonslopes.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Plotting the file containing the seasonal slopes of the (QSO) data 
; estimated with plot_subFITSfile.pro
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; slopesfile      : string with name and path of file containing slopes
;                   (created with plot_subFITSfile.pro)
; FIT             : set FIT equal to one of the following integers to determine
;                   which gVSgr slopes to plot, i.e. how they were fit
;                      FIT = 1      : MPFITFUN with g on x-axis
;                      FIT = 2      : MPFITFUN with gr on x-axis
;                      FIT = 3      : First principal component (PCA)
;                      FIT = else   : Default MPFITFUN with gr on x-axis
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ZBIN            : Numeber of redshift bins to average over in plots
; /EPS            : set /EPS to write plots to .eps files
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;  l_fileout      : If ZBIN is used the long term mean and median slope over 
;                   the redshift bins and their errors are stored in this file.
;  s_fileout      : If ZBIN is used the short term mean and median slope over 
;                   the redshift bins and their errors are stored in this file.
;----------------------------
;   EXAMPLES/USAGE
;----------------------------

; IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p500000z3p00000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_14:43:12_2010_seasonslopes.dat',/VERBOSE,FIT=1,ZBIN=20
; === various z ranges
; === All of A range for z~1.5
;IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p00000A2p00000AND0p00000gamma5p00000_Fri_Aug_20_07:56:48_2010_seasonslopes.dat',/VERBOSE,/eps,FIT=1

; paper files:
; IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p40000z1p60000AND0p150000A2p00000AND0p00000gamma5p00000_Sat_Aug_7_16:09:47_2010_seasonslopes.dat',/VERBOSE,FIT=1,ZBIN=20
;IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p60000z1p80000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_14:47:04_2010_seasonslopes.dat',/VERBOSE,/eps,FIT=1
;IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_1p80000z2p00000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:16_2010_seasonslopes.dat',/VERBOSE,/eps,FIT=1
;IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_2p00000z2p30000AND0p150000A2p00000AND0p00000gamma5p00000_Mon_Aug_9_15:16:35_2010_seasonslopes.dat',/VERBOSE,/eps,FIT=1
; IDL> plotseasonslopes,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999_SUB_0p200000z4p00000AND0p150000A2p00000AND0p00000gamma5p00000_Wed_Aug_25_07:56:13_2010_seasonslopes.dat',/VERBOSE,FIT=1,ZBIN=30

;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-08-09  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ namedate.pro
;----------------------------
;-
PRO plotseasonslopes,slopesfile,FIT=FIT,ZBIN=ZBIN,optional=optional,EPS=EPS,VERBOSE=VERBOSE

ZB = n_elements(ZBIN)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

;readcol,FORMAT='(o,f,f,i,i,f,f)',slopesfile,ID,Bsample,zs,season,NN,Bseason,Ktau
readcol,FORMAT='(o,f,f,i,i,f,f,f)',slopesfile,ID,Bsample,zs,season,NN,Bseason,Ktau,Bsample2,Bseason2   ; if slopes are also calculated with g VS gr AND gr VS g

namedate,slopesfile,path,name,extension,date,dateus

case FIT of
  1: begin
        Bsam = Bsample
        Bsea = Bseason
        stit = textoidl('\beta_{s,gr} (short term)')
        ltit = textoidl('\beta_{l,gr} (long term/secular)')
        if ZB eq 1 then begin
           l_fileout = '/'+path+'/meanLONGtermslopes_grXax_zbin'+strtrim(ZBIN,2)+name+'.dat'
           openw,11,l_fileout,width=300
           printf,11,'# This file contains the long term QSO slopes calculated with plot_subFITSfile.pro '
           printf,11,'# averaged over '+strtrim(ZBIN,2)+' equally spaced redshift for z = ['+strtrim(min(zs),2)+','+strtrim(max(zs),2)+']'
           printf,11,'# The original slopes were estimated with MFTIFUN and g-r on the x-axis'
           printf,11,'# The mean values were written to the file with plotseasonslopes.pro. '
           printf,11,'# The columns are: '
           printf,11,'# 0: center of redshift bin'
           printf,11,'# 1: mean slope in redshift bin'
           printf,11,'# 2: standard deviation of the mean'
           printf,11,'# 3: median slope in redshift bin'
           printf,11,'# 4: plus 68% scatter around median'
           printf,11,'# 5: minus 68% scatter around median'
           printf,11,'# 6: plus 95% scatter around median'
           printf,11,'# 7: minus 95% scatter around median'

           s_fileout = '/'+path+'/meanSHORTtermslopes_grXax_zbin'+strtrim(ZBIN,2)+name+'.dat'
           openw,22,s_fileout,width=300
           printf,22,'# This file contains the short term QSO slopes calculated with plot_subFITSfile.pro '
           printf,22,'# averaged over '+strtrim(ZBIN,2)+' equally spaced redshift for z = ['+strtrim(min(zs),2)+','+strtrim(max(zs),2)+']'
           printf,22,'# The original slopes were estimated with MFTIFUN and g-r on the x-axis'
           printf,22,'# The mean values were written to the file with plotseasonslopes.pro. '
           printf,22,'# The columns are: '
           printf,11,'# 0: center of redshift bin'
           printf,22,'# 1: mean slope in redshift bin'
           printf,22,'# 2: standard deviation of the mean'
           printf,22,'# 3: median slope in redshift bin'
           printf,22,'# 4: plus 68% scatter around median'
           printf,22,'# 5: minus 68% scatter around median'
           printf,22,'# 6: plus 95% scatter around median'
           printf,22,'# 7: minus 95% scatter around median'
        endif
     end
  2: begin
        Bsam = Bsample2
        Bsea = Bseason2
        stit = textoidl('\beta_{s,g} (short term)')
        ltit = textoidl('\beta_{l,g} (long term/secular)')
        if ZB eq 1 then begin
           l_fileout = '/'+path+'/meanLONGtermslopes_gXax_zbin'+strtrim(ZBIN,2)+name+'.dat'
           openw,11,l_fileout,width=300
           printf,11,'# This file contains the long term QSO slopes calculated with plot_subFITSfile.pro '
           printf,11,'# averaged over '+strtrim(ZBIN,2)+' equally spaced redshift for z = ['+strtrim(min(zs),2)+','+strtrim(max(zs),2)+']'
           printf,11,'# The original slopes were estimated with MFTIFUN and g on the x-axis'
           printf,11,'# The mean values were written to the file with plotseasonslopes.pro. '
           printf,11,'# The columns are: '
           printf,11,'# 0: center of redshift bin'
           printf,11,'# 1: mean slope in redshift bin'
           printf,11,'# 2: standard deviation of the mean'
           printf,11,'# 3: median slope in redshift bin'
           printf,11,'# 4: plus 68% scatter around median'
           printf,11,'# 5: minus 68% scatter around median'
           printf,11,'# 6: plus 95% scatter around median'
           printf,11,'# 7: minus 95% scatter around median'

           s_fileout = '/'+path+'/meanSHORTtermslopes_gXax_zbin'+strtrim(ZBIN,2)+name+'.dat'
           openw,22,s_fileout,width=300
           printf,22,'# This file contains the short term QSO slopes calculated with plot_subFITSfile.pro '
           printf,22,'# averaged over '+strtrim(ZBIN,2)+' equally spaced redshift for z = ['+strtrim(min(zs),2)+','+strtrim(max(zs),2)+']'
           printf,22,'# The original slopes were estimated with MFTIFUN and g on the x-axis'
           printf,22,'# The mean values were written to the file with plotseasonslopes.pro. '
           printf,22,'# The columns are: '
           printf,11,'# 0: center of redshift bin'
           printf,22,'# 1: mean slope in redshift bin'
           printf,22,'# 2: standard deviation of the mean'
           printf,22,'# 3: median slope in redshift bin'
           printf,22,'# 4: plus 68% scatter around median'
           printf,22,'# 5: minus 68% scatter around median'
           printf,22,'# 6: plus 95% scatter around median'
           printf,22,'# 7: minus 95% scatter around median'
        endif
     end
  3: begin
        if vb eq 1 then print,':: plotseasonslopes.pro :: The PCA slopes are not in datafile  -> aborting '
        stop
     end
  else: begin
        if vb eq 1 then print,':: plotseasonslopes.pro :: Fit specified not valid. Default fit for slopes chosen - see porgram header. '
        Bsam = Bsample
        Bsea = Bseason
        stit = textoidl('\beta_{s,gr} (short term)')
        ltit = textoidl('\beta_{l,gr} (long term/secular)')
        if ZB eq 1 then begin
           l_fileout = '/'+path+'/meanLONGtermslopes_grXax_zbin'+strtrim(ZBIN,2)+name+'.dat'
           openw,11,l_fileout,width=300
           printf,11,'# This file contains the long term QSO slopes calculated with plot_subFITSfile.pro '
           printf,11,'# averaged over '+strtrim(ZBIN,2)+' equally spaced redshift for z = ['+strtrim(min(zs),2)+','+strtrim(max(zs),2)+']'
           printf,11,'# The original slopes were estimated with MFTIFUN and g-r on the x-axis'
           printf,11,'# The mean values were written to the file with plotseasonslopes.pro. '
           printf,11,'# The columns are: '
           printf,11,'# 0: center of redshift bin'
           printf,11,'# 1: mean slope in redshift bin'
           printf,11,'# 2: standard deviation of the mean'
           printf,11,'# 3: median slope in redshift bin'
           printf,11,'# 4: plus 68% scatter around median'
           printf,11,'# 5: minus 68% scatter around median'
           printf,11,'# 6: plus 95% scatter around median'
           printf,11,'# 7: minus 95% scatter around median'

           s_fileout = '/'+path+'/meanSHORTtermslopes_grXax_zbin'+strtrim(ZBIN,2)+name+'.dat'
           openw,22,s_fileout,width=300
           printf,22,'# This file contains the short term QSO slopes calculated with plot_subFITSfile.pro '
           printf,22,'# averaged over '+strtrim(ZBIN,2)+' equally spaced redshift for z = ['+strtrim(min(zs),2)+','+strtrim(max(zs),2)+']'
           printf,22,'# The original slopes were estimated with MFTIFUN and g-r on the x-axis'
           printf,22,'# The mean values were written to the file with plotseasonslopes.pro. '
           printf,22,'# The columns are: '
           printf,11,'# 0: center of redshift bin'
           printf,22,'# 1: mean slope in redshift bin'
           printf,22,'# 2: standard deviation of the mean'
           printf,22,'# 3: median slope in redshift bin'
           printf,22,'# 4: plus 68% scatter around median'
           printf,22,'# 5: minus 68% scatter around median'
           printf,22,'# 6: plus 95% scatter around median'
           printf,22,'# 7: minus 95% scatter around median'
        endif
     end
endcase

Uent    = uniq(ID)
IDuniq  = ID(Uent)
UBsamp  = Bsam(Uent)

; the colors of the seasons
Scol    = strarr(9)
Scol(0) = 'magenta'
Scol(1) = 'pink'
Scol(2) = 'red'
Scol(3) = 'orange'
Scol(4) = 'yellow'
Scol(5) = 'green'
Scol(6) = 'sky'
Scol(7) = 'blue'
Scol(8) = 'navy'

; making array with redshift bins
if ZB eq 1 then zarr = min(zs)+(findgen(ZBIN+1)/(ZBIN)*(max(zs)-min(zs)))

Nw=0
;=============================================================================================
; = = = Seasonal VS sample slopes = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'seasonVSsample.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Seasonla vs Sample slopes'
   thickall = 2
endelse
; setting plot range
;XR = [min(Bsea),max(Bsea)]
;YR = [min(Bsam),max(Bsam)]
XR = [-1,4]
YR = [-1,4]

plot,ID,ID, col=col.black    $
        , /NODATA $
        , xtitle =stit $
        , ytitle =ltit $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

PLOTSYM,0,1.0
oplot,Bsea,Bsam,col=col.black,psym=8,thick=thickall 

NNmin = 10
ent20 = where(NN gt NNmin)
PLOTSYM,0,1.0,/FILL
oplot,Bsea(ent20),Bsam(ent20),col=col.green,psym=8,thick=thickall 

xx = findgen(1000)/1000 * (XR[1]-XR[0]) + XR[0]
oplot,xx,xx,linestyle=2,thick=thickall,col=col.black

XYOUTS,(XR[1]-XR[0])*0.55+XR[0],(YR[1]-YR[0])*0.05+YR[0],'Fit based on >'+strtrim(NNmin,2)+' points',col=col.green,charsize=1.5,charthick=thickall

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = Seasonal slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'seasonVSz.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Seasonal vs redshift'
   thickall = 2
endelse
; setting plot range
XR = [min(zs)-0.1*(max(zs)-min(zs)),max(zs)+0.1*(max(zs)-min(zs))]
;YR = [min(Bsea),max(Bsea)]
YR = [-1,4]

NNmin = 5

plot,ID,ID, col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =stit $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

PLOTSYM,0,1.0,/FILL

for ii=0,n_elements(Scol)-1 do begin  ; looping over seasons
   xx = n_elements(Scol)-ii-1    ; reversing ii so latest season is plotted first   
   CCC = GETCOLOR(Scol(xx), 100) ; getting color of season
   Sent = where(season eq xx AND NN gt NNmin)
   if ZB eq 1 then PLOTSYM,0,0.5,/FILL
   if n_elements(Sent) gt 1 then oplot,zs(Sent),Bsea(Sent),col=CCC,psym=8,thick=thickall 

   ; calculating the median values and percentiles for overplotting
   if ZB eq 1 then begin
      for jj=0,ZBIN-1 do begin
         zent = where(season eq xx AND NN gt NNmin AND zs ge zarr(jj) AND zs lt zarr(jj+1),Czs)
         if zent ne [-1] then begin
            slopezbin = Bsea(zent)
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

            BinC = zarr(jj)+(zarr(jj+1)-zarr(jj))/2

            if ZB eq 1 then PLOTSYM,8,1.5,/FILL
;            oplot,zs(zent)*0+BinC,Bsea*0+slopemed,psym=8,col=CCC
            ; plotting percentile errorbar if more than 5 points in redshift bin
;            if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsea*0+slopemed,slopezsorted*0+Serr68p,psym=2, col=CCC, ERRCOLOR=CCC, /HIBAR, thick=thickall
;            if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsea*0+slopemed,slopezsorted*0+Serr68m,psym=2, col=CCC, ERRCOLOR=CCC, /LOBAR, thick=thickall

         endif
      endfor
   endif
endfor

if ZB eq 1 then begin ; calculating the median values and percentiles for overplotting for ALL slopes
   for kk=0,ZBIN-1 do begin
      zent = where(zs ge zarr(kk) AND zs lt zarr(kk+1),Czs)
      if zent ne [-1] then begin
         slopezbin = Bsea(zent)
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
;         Sstnd   = total( 1./n_elements(slopezbin) * sqrt((slopezbin-mean(slopezbin))^2) )
         Sstnd   = total( 1./n_elements(slopezbin) * sqrt((slopezbin-slopemed)^2) )

         BinC = zarr(kk)+(zarr(kk+1)-zarr(kk))/2

         if ZB eq 1 then PLOTSYM,8,2.,/FILL
         oplot,zs(zent)*0+BinC,Bsea*0+slopemed,psym=8,col=col.black
;         oplot,zs(zent)*0+BinC,Bsam*0+mean(slopezbin),psym=8,col=col.black
         ; plotting percentile errorbar if more than 5 points in redshift bin
;         if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsea*0+slopemed,slopezsorted*0+Serr68p,psym=2, col=col.black, ERRCOLOR=col.black, /HIBAR, thick=thickall
;         if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsea*0+slopemed,slopezsorted*0+Serr68m,psym=2, col=col.black, ERRCOLOR=col.black, /LOBAR, thick=thickall
         ; plotting standard deviation errorbar if more than 5 points in redshift bin
         if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsam*0+slopemed,slopezsorted*0+Sstnd,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall

         printf,22,binc,mean(slopezbin),Sstnd,slopemed,Serr68p,Serr68m,Serr95p,Serr95m
      endif
   endfor
endif
close,22

XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.95+YR[0],"June'99-June'00",col=col.magenta,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.90+YR[0],"June'00-June'01",col=col.pink,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.85+YR[0],"June'01-June'02",col=col.red,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.80+YR[0],"June'02-June'03",col=col.orange,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.75+YR[0],"June'03-June'04",col=col.yellow,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.70+YR[0],"June'04-June'05",col=col.green,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.65+YR[0],"June'05-June'06",col=col.skyblue,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.60+YR[0],"June'06-June'07",col=col.blue,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.65+XR[0],(YR[1]-YR[0])*0.55+YR[0],"June'07-June'08",col=col.navy,charsize=1.5,charthick=thickall

XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.05+YR[0],'slopes estimated on >'+strtrim(NNmin,2)+' points',col=col.black,charsize=1.5,charthick=thickall

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = sample slopes VS redshift = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'sampleVSz.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = 'Sample vs redshift'
   thickall = 2
endelse
; setting plot range
XR = [min(zs)-0.1*(max(zs)-min(zs)),max(zs)+0.1*(max(zs)-min(zs))]
;YR = [min(Bsea),max(Bsea)]
YR = [-1,4]

NNmin = 5

plot,ID,ID, col=col.black    $
        , /NODATA $
        , xtitle ='z' $
        , ytitle =ltit $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

PLOTSYM,0,1.0,/FILL
if ZB eq 1 then PLOTSYM,0,0.5,/FILL
oplot,zs,Bsam,col=col.gray,psym=8,thick=thickall 


if ZB eq 1 then begin ; calculating the median values and percentiles for overplotting for ALL slopes
   for kk=0,ZBIN-1 do begin
      zent = where(zs ge zarr(kk) AND zs lt zarr(kk+1),Czs)
      if zent ne [-1] then begin
         slopezbin = Bsam(zent)
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
;         Sstnd   = total( 1./n_elements(slopezbin) * sqrt((slopezbin-mean(slopezbin))^2) )
         Sstnd   = total( 1./n_elements(slopezbin) * sqrt((slopezbin-slopemed)^2) )

         BinC = zarr(kk)+(zarr(kk+1)-zarr(kk))/2

         if ZB eq 1 then PLOTSYM,8,2.,/FILL
         oplot,zs(zent)*0+BinC,Bsam*0+slopemed,psym=8,col=col.black
;         oplot,zs(zent)*0+BinC,Bsam*0+mean(slopezbin),psym=8,col=col.black
         ; plotting percentile errorbar if more than 5 points in redshift bin
;         if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsam*0+slopemed,slopezsorted*0+Serr68p,psym=2, col=col.black, ERRCOLOR=col.black, /HIBAR, thick=thickall
;         if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsam*0+slopemed,slopezsorted*0+Serr68m,psym=2, col=col.black, ERRCOLOR=col.black, /LOBAR, thick=thickall
         ; plotting standard deviation errorbar if more than 5 points in redshift bin
         if Czs gt 5 then oploterror,zs(zent)*0+BinC,Bsam*0+slopemed,slopezsorted*0+Sstnd,psym=2, col=col.black, ERRCOLOR=col.black, thick=thickall

         printf,11,binc,mean(slopezbin),Sstnd,slopemed,Serr68p,Serr68m,Serr95p,Serr95m

      endif
   endfor
endif
close,11

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
; = = = Slope histograms = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'slopehist.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = ' Histograms of slopes'
   thickall = 2
endelse
; setting plot range
XR = [-1,4]
YR = [0,n_elements(Bsea)/3.]

plot,ID,ID, col=col.black    $
        , /NODATA $
        , xtitle ='slope' $
        , ytitle ='N' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

plothist,Bsam(uniq(ID)),xh1,yh1,bin=0.2,/overplot,col=col.black,thick=thickall;,peak=300
plothist,Bsea,xh2,yh2,bin=0.2,/overplot,col=col.red,thick=thickall

XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.90+YR[0],ltit,col=col.black,charsize=1.5,charthick=thickall
XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.85+YR[0],stit,col=col.red,charsize=1.5,charthick=thickall

if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================


;=============================================================================================
; = = = Kendall's tau = = =
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   plot1 = 'ktauhist.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=25, ysize=25;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=600, ysize=500, title = ' Histograms of slopes'
   thickall = 2
endelse
; setting plot range
XR = [-1.1,1.1]
YR = [0,n_elements(Ktau)/5.]

plot,ID,ID, col=col.black    $
        , /NODATA $
        , xtitle =textoidl('Kendall \tau') $
        , ytitle ='N' $
        , thick = thickall $
        , xthick = thickall $
        , ythick = thickall $
        , xrange = XR , /xstyle $
        , yrange = YR , /ystyle $
        , charsize = 2.0 $
        , charthick = thickall $
        , psym = 2 $
        , yminor = 2 $
;        , xticks = 3 $;, xtickname = [' ',' '] $      ; removing the x ticks
;        , pos=[0.16,0.3,0.97,0.95] $
        , background = col.white

plothist,Ktau,xh3,yh3,bin=0.05,/overplot,col=col.black,thick=thickall

NNmin = 5
entNN = where(NN gt NNmin)
plothist,Ktau(entNN),xh4,yh4,bin=0.05,/overplot,col=col.green,thick=thickall

XYOUTS,(XR[1]-XR[0])*0.05+XR[0],(YR[1]-YR[0])*0.90+YR[0],"Kendall's "+textoidl('\tau')+' estimate based on >'+strtrim(NNmin,2)+' points',col=col.green,charsize=1.5,charthick=thickall


if PS eq 1 then begin
   device, /close
   set_plot, 'x'
endif
Nw = Nw+1                                            ; incremeting window number by 1
;=============================================================================================

if vb eq 1 then print,' '
if vb eq 1 then print,':: plotseasonslopes.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
