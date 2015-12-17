;+
;----------------------------
;   NAME
;----------------------------
; specWatmosphereANDskylinesOH.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure reading a spectrum and plotting it with atmosphere transmission 
; curves and OH airglow skylines overplotted to assess observability
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; specfile        : file containing spectrum. Format of file is
;                      col 1:  lambda [A]
;                      col 2:  flux   [erg/s/A/cm2] - units not nesecssary since it is normalized
;                      col 3:  error  [erg/s/A/cm2]
;                      col 3:  contam 
;                   Comments marked with #
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ZOOMRANGE       : vector with range to plot/zoom into given in Angstrom
; ZEST            : An estimated redshift for which a set of emission lines are overplotted
; basenameEPS     : Name appended to the EPS figures
; /EPS            : set /EPS to write plots to .eps files
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : New Fits file containing SDSS objects, same format
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/COSMOS-1-F140Wselection/COSMOS-1-G141_01186.ascii',/VERBOSE,/STP,ZOOMRANGE=[14000,16000],zest=1.266

; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/GOODS-S-23-F140Wselection/GOODS-S-23-G141_00353.ascii',/VERBOSE,/STP,zest=2.2600,basenameEPS='GOODS-S-23-G141_00353',ZOOMRANGE=[14000,16000]


; new candidates (110927)
; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/COSMOS-3-F140Wselection/COSMOS-3-G141_01159.ascii',/VERBOSE,/STP,zest=2.09,basenameEPS='COSMOS-3-G141_01159'

; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/COSMOS-4-F140Wselection/COSMOS-4-G141_00512.ascii',/VERBOSE,/STP,zest=2.1,basenameEPS='COSMOS-4-G141_00512'


; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/COSMOS-4-F140Wselection/COSMOS-4-G141_00347.ascii',/VERBOSE,/STP,zest=1.746,basenameEPS='COSMOS-4-G141_00347'
; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/COSMOS-6-F140Wselection/COSMOS-6-G141_00791.ascii',/VERBOSE,/STP,zest=1.8383,basenameEPS='COSMOS-6-G141_00791'
; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/COSMOS-18-F140Wselection/COSMOS-18-G141_01007.ascii',/VERBOSE,/STP,zest=1.519,basenameEPS='COSMOS-18-G141_01007'
; IDL> specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/GOODS-S-23-F140Wselection/GOODS-S-23-G141_00187.ascii',/VERBOSE,/STP,zest=1.6098,basenameEPS='GOODS-S-23-G141_00187'



; ------------------- running list of files -------------------
; IDL> .com specWatmosphereANDskylinesOH.pro

; IDL> files = ['GOODS-S-23-F140Wselection/GOODS-S-23-G141_00588','GOODS-S-23-F140Wselection/GOODS-S-23-G141_00513','GOODS-S-23-F140Wselection/GOODS-S-23-G141_00353','GOODS-S-23-F140Wselection/GOODS-S-23-G141_00256','GOODS-S-23-F140Wselection/GOODS-S-23-G141_00187','GOODS-S-24-F140Wselection/GOODS-S-24-G141_00396','GOODS-S-24-F140Wselection/GOODS-S-24-G141_00810','GOODS-S-24-F140Wselection/GOODS-S-24-G141_00470','COSMOS-1-F140Wselection/COSMOS-1-G141_00550','COSMOS-1-F140Wselection/COSMOS-1-G141_01186','COSMOS-3-F140Wselection/COSMOS-3-G141_00346','COSMOS-3-F140Wselection/COSMOS-3-G141_01164','COSMOS-11-F140Wselection/COSMOS-11-G141_00173','COSMOS-11-F140Wselection/COSMOS-11-G141_00254','COSMOS-16-F140Wselection/COSMOS-16-G141_00321','COSMOS-18-F140Wselection/COSMOS-18-G141_00556','COSMOS-18-F140Wselection/COSMOS-18-G141_00778','COSMOS-18-F140Wselection/COSMOS-18-G141_01007','COSMOS-20-F140Wselection/COSMOS-20-G141_01095']

; IDL> name = ['GOODS-S-23-G141_00588','GOODS-S-23-G141_00513','GOODS-S-23-G141_00353','GOODS-S-23-G141_00256','GOODS-S-23-G141_00187','GOODS-S-24-G141_00396','GOODS-S-24-G141_00810','GOODS-S-24-G141_00470','COSMOS-1-G141_00550','COSMOS-1-G141_01186','COSMOS-3-G141_00346','COSMOS-3-G141_01164','COSMOS-11-G141_00173','COSMOS-11-G141_00254','COSMOS-16-G141_00321','COSMOS-18-G141_00556','COSMOS-18-G141_00778','COSMOS-18-G141_01007','COSMOS-20-G141_01095']

; IDL> redshifts = [2.0799,1.7855,2.2600,1.3334,1.6098,2.0315,1.8910,1.0753,1.8470,1.2663,1.9657,1.7724,1.1758,1.8251,2.0667,1.9481,1.0518,1.1054,1.9173]

; IDL> Nfiles = n_elements(files) & for ii=0,Nfiles-1 do begin & specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/'+trim(files(ii))+'.ascii',/VERBOSE,zest=redshifts(ii),basenameEPS=name(ii) & endfor

; IDL> print,'/Users/kasperborelloschmidt/work/3dHST/mergersearch/SFRselection/pointingCATandFITS/'+files+'_lineimgoverlay_byhand.png' 


; ------------------- running all 94 mergers -------------------
; IDL> .com specWatmosphereANDskylinesOH.pro
; IDL> readcol,FORMAT=('A,f,f'),'the94mergers.dat',files,zphot,IDphot,comment='#' & Nfiles = n_elements(files)
; IDL> epsname = strarr(Nfiles,2) & for jj=0,Nfiles-1 do begin & epsname(jj,*) = strsplit(files(jj),'/',/extract) & endfor
; IDL> readcol,FORMAT=('f,f,f,f,f,f,f,f,f,f'),'the94mergers_RETUNRINFOredshifts.dat',zgrism,zerr1,zerr2,zerr3,zerr4,zerr5,zerr6,zerr7,IMGCUT,zerr,comment='#' & NfilesZ = n_elements(files)
; IDL> for ii=0,Nfiles-1 do begin & specWatmosphereANDskylinesOH,'SFRselection/pointingCATandFITS/'+trim(files(ii))+'.ascii',/VERBOSE,zest=zgrism(ii),basenameEPS=trim(epsname(ii,1)),/eps & endfor
; IDL> plot,zphot,zgrism,xtitle='zphot',ytitle='zgrism',psym=2,xrange=[0.5,2.5],yrange=[0.5,2.5],charsize=3.0



; ------------------- running all 61 mergers -------------------
; IDL> .com specWatmosphereANDskylinesOH.pro
; IDL> dat = mrdfits('/Users/kasperborelloschmidt/work/3dHST/mergersearch/120224brightANDgoodmatch/selectOBJfromGRISMcats_v1p7catsOUTPUT_0p7z2p3_-1HaEW1E+18_19mF140w23p5_0FCONTAM1000_0p75FCOVER1_0RMATCH0p3_1SFR5_-9.5sSFR-5_9Mstar12_vissel120301_SFcategory_zELMAP_Ncomp.fits',1)
; IDL> dat = dat(where(dat.vissel120301 eq 1))
; IDL> for ii=0,n_elements(dat.ID)-1 do begin & specWatmosphereANDskylinesOH,'/Users/kasperborelloschmidt/work/3dHST/mergersearch/120224brightANDgoodmatch/data/'+trim(dat(ii).ID)+'.ascii',/VERBOSE,zest=dat(ii).z_grism,basenameEPS=trim(dat(ii).ID),/eps & endfor





;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-09-13  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO specWatmosphereANDskylinesOH,specfile,EPS=EPS,VERBOSE=VERBOSE,STP=STP,ZOOMRANGE=ZOOMRANGE,ZEST=ZEST,basenameEPS=basenameEPS

EB = n_elements(basenameEPS)
ZE = n_elements(ZEST)
ZR = n_elements(ZOOMRANGE)
PS = n_elements(EPS)
VB = n_elements(VERBOSE)

; ----- data files taken from http://www.gemini.edu/?q=node/10789 -----
Afile = '/Users/kasperborelloschmidt/work/observing/atmospheretransmissionDAT/cptrans_zm_100_20.dat'
Astring = 'airmass 2.0 + water vapor column 10.0 mm'

;Afile = '/Users/kasperborelloschmidt/work/observing/atmospheretransmissionDAT/cptrans_zm_23_10.dat'
;Astring = 'airmass 1.0 ; water vapor cplumn 2.3 mm'

;Afile = '/Users/kasperborelloschmidt/work/observing/atmospheretransmissionDAT/cptrans_zm_23_20.dat'
;Astring = 'airmass 1.0 ; water vapor cplumn 2.3 mm'

; --- Reading atmosphere file ---
Nrows        = File_lines(Afile)                 ; number of rows in file
Nheaderlines = 0                                 ; number of header lines
Ncol         = 2                                 ; number of columns in file
ATMARR       = dblarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,Afile, /GET_LUN                        ; open file for reading     
readf,lun,ATMARR                                 ; reading data into array
free_lun,lun  
; ----------------------------------

ATMARR(0,*) = ATMARR(0,*)*1e4  ; turning micron into A


; ----- data file taken from http://www.gemini.edu/?q=node/10166 -----
Sfile = '/Users/kasperborelloschmidt/work/observing/OHairglowDAT/rousselot2000.dat.txt'
Sstring = 'OH airglow from Rousselot at al. 2000'
; --- Reading skylines file ---
Nrows        = File_lines(Sfile)                 ; number of rows in file
Nheaderlines = 28                                ; number of header lines
Ncol         = 2                                 ; number of columns in file
SKYARR       = fltarr(Ncol,Nrows-Nheaderlines)   ; array to read file into
openr,lun,Sfile, /GET_LUN                        ; open file for reading     
headerS = STRARR(Nheaderlines)                   ; string array for header
readf,lun,headerS                                ; reading header into string array
readf,lun,SKYARR                                 ; reading data into array
free_lun,lun  
; ----------------------------------

; --- Reading grond filter curves ---
readcol,'~/work/GRONDfilterset/GROND_filterecurves.txt',lamGROND,gGROND,rGROND,iGROND,zGROND,JGROND,HGROND,KGROND,comment='#'
lamGROND = lamGROND*10.
xval_g = 4500  ; lamGROND(where(gGROND eq max(gGROND)))
xval_r = 6200  ; lamGROND(where(rGROND eq max(rGROND)))
xval_i = 7500  ; lamGROND(where(iGROND eq max(iGROND)))
xval_z = 8900  ; lamGROND(where(zGROND eq max(zGROND)))
xval_J = 12500  ; lamGROND(where(jGROND eq max(jGROND)))
xval_H = 16300  ; lamGROND(where(hGROND eq max(hGROND)))
xval_K = 21500  ; lamGROND(where(kGROND eq max(kGROND)))

; --- Reading SINFONI filter curves ---
readcol,'~/work/SINFONIfilerset/effSINFONI_Jband.txt',lamSINFONIj,effSINFONIj,comment='#'
readcol,'~/work/SINFONIfilerset/effSINFONI_Hband.txt',lamSINFONIh,effSINFONIh,comment='#'
readcol,'~/work/SINFONIfilerset/effSINFONI_Kband.txt',lamSINFONIk,effSINFONIk,comment='#'
effSINFONIj = effSINFONIj/100.  ; changing percentage to fraction
effSINFONIh = effSINFONIh/100.
effSINFONIk = effSINFONIk/100.
lamSINFONIj = lamSINFONIj/1e3
lamSINFONIh = lamSINFONIh/1e3
lamSINFONIk = lamSINFONIk/1e3

G141range = [11500,17000]

; --- Reading spectrum file ---
readcol,specfile,Lspec,Fspec,ErrSpec,ContSpec,COMMENT='#'

; set of emission lines plotted if ZEST is given
LinesL   = [3727,4342,4863,5008,6563] ; rest wavelength of lines
LineName = ['OII','H\gamma','H\beta','OIII','H\alpha']  ; will be put in textoidl()
Nlines   = n_elements(LinesL)



Nw = 0 ; resetting window numbering
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   if EB eq 0 then plot1 = 'specAatmosphereAskylines.eps'
   if EB eq 1 then plot1 = 'specAatmosphereAskylines_'+trim(basenameEPS)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=50, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1200, ysize=500, title = 'Spec + Sky + Atm'
   thickall = 2
endelse
; setting plot range
Lrange = [min([min(ATMARR(0,*)),min(SKYARR(0,*))]),max([max(ATMARR(0,*)),max(SKYARR(0,*))])]
XR = Lrange
YR = [0,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [A]') $
        , ytitle =textoidl('Transmission') $
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

oplot,ATMARR(0,*),ATMARR(1,*),thick=thickall,linestyle=0,col=col.gray
XYOUTS,DX*0.98+XR[0],DY*0.88+YR[0],'Atmosphere transmission: '+trim(Astring),col=col.darkgray,charsize=2.0,charthick=thickall,alignment=1.0

oplot,SKYARR(0,*),SKYARR(1,*)/max(SKYARR(1,*)),thick=thickall,linestyle=0,col=col.blue
XYOUTS,DX*0.98+XR[0],DY*0.82+YR[0],'Skylines: '+trim(Sstring),col=col.blue,charsize=2.0,charthick=thickall,alignment=1.0

; ---------------GROND FILTERS------------------
oplot,lamGROND,gGROND,thick=thickall,linestyle=2,col=col.black  ;purple
oplot,lamGROND,rGROND,thick=thickall,linestyle=2,col=col.black  ;magenta
oplot,lamGROND,iGROND,thick=thickall,linestyle=2,col=col.black  ;blue
oplot,lamGROND,zGROND,thick=thickall,linestyle=2,col=col.black  ;green
oplot,lamGROND,jGROND,thick=thickall,linestyle=2,col=col.black  ;yellow
oplot,lamGROND,hGROND,thick=thickall,linestyle=2,col=col.black  ;orange
oplot,lamGROND,kGROND,thick=thickall,linestyle=2,col=col.black  ;red

if xval_g gt XR[0] and xval_g lt XR[1] then XYOUTS,xval_g,DY*0.05+max(gGROND),'GROND g',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_r gt XR[0] and xval_r lt XR[1] then XYOUTS,xval_r,DY*0.05+max(rGROND),'GROND r',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_i gt XR[0] and xval_i lt XR[1] then XYOUTS,xval_i,DY*0.05+max(iGROND),'GROND i',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_z gt XR[0] and xval_z lt XR[1] then XYOUTS,xval_z,DY*0.05+max(zGROND),'GROND z',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_J gt XR[0] and xval_J lt XR[1] then XYOUTS,xval_J,DY*0.05+max(jGROND),'GROND J',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_H gt XR[0] and xval_H lt XR[1] then XYOUTS,xval_H,DY*0.05+max(hGROND),'GROND H',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_K gt XR[0] and xval_K lt XR[1] then XYOUTS,xval_K,DY*0.05+max(kGROND),'GROND K',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
; -----------------------------------------------

oplot,Lspec,Fspec/max(Fspec),thick=thickall,linestyle=0,col=col.black
XYOUTS,DX*0.98+XR[0],DY*0.94+YR[0],trim(specfile),col=col.black,charsize=2.0,charthick=thickall,alignment=1.0

oplot,fltarr(2)+G141range[0],YR,thick=thickall,linestyle=3,col=col.black
oplot,fltarr(2)+G141range[1],YR,thick=thickall,linestyle=3,col=col.black

if ze eq 1 then begin  ; overplotting emission lines if zest is given
   for ii=0,Nlines-1 do begin  ; looping over lines to plot
      if LinesL(ii)*(1+zest) gt XR[0] and LinesL(ii)*(1+zest) lt XR[1] then begin
         oplot,fltarr(2)+LinesL(ii)*(1+zest),YR,thick=thickall,linestyle=0,col=col.red
         XYOUTS,LinesL(ii)*(1+zest)+DX/100.,DY*ii/15.+0.3+YR[0],trim(textoidl(Linename(ii))),col=col.red,charsize=2.5,charthick=thickall
      endif
   endfor
   XYOUTS,DX*0.98+XR[0],DY*0.76+YR[0],'z = '+trim(zest),col=col.red,charsize=2.0,charthick=thickall,alignment=1.0
endif

if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   if EB eq 0 then plot1 = 'specAatmosphereAskylinesZOOMG141.eps'
   if EB eq 1 then plot1 = 'specAatmosphereAskylinesZOOMG141_'+trim(basenameEPS)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=50, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1200, ysize=500, title = 'Spec + Sky + Atm ZOOM G141'
   thickall = 2
endelse
; setting plot range
XR = G141range
YR = [0,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [A]') $
        , ytitle =textoidl('Transmission') $
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

zooment = where(ATMARR(0,*) gt XR(0) and ATMARR(0,*) lt XR(1))
oplot,ATMARR(0,zooment),ATMARR(1,zooment),thick=thickall,linestyle=0,col=col.gray
XYOUTS,DX*0.98+XR[0],DY*0.88+YR[0],'Atmosphere transmission: '+trim(Astring),col=col.darkgray,charsize=2.0,charthick=thickall,alignment=1.0

zooment = where(SKYARR(0,*) gt XR(0) and SKYARR(0,*) lt XR(1))
oplot,SKYARR(0,zooment),SKYARR(1,zooment)/max(SKYARR(1,zooment)),thick=thickall,linestyle=0,col=col.blue
XYOUTS,DX*0.98+XR[0],DY*0.82+YR[0],'Skylines: '+trim(Sstring),col=col.blue,charsize=2.0,charthick=thickall,alignment=1.0

; ---------------GROND FILTERS------------------
oplot,lamGROND,gGROND,thick=thickall,linestyle=2,col=col.black  ;purple
oplot,lamGROND,rGROND,thick=thickall,linestyle=2,col=col.black  ;magenta
oplot,lamGROND,iGROND,thick=thickall,linestyle=2,col=col.black  ;blue
oplot,lamGROND,zGROND,thick=thickall,linestyle=2,col=col.black  ;green
oplot,lamGROND,jGROND,thick=thickall,linestyle=2,col=col.black  ;yellow
oplot,lamGROND,hGROND,thick=thickall,linestyle=2,col=col.black  ;orange
oplot,lamGROND,kGROND,thick=thickall,linestyle=2,col=col.black  ;red

if xval_g gt XR[0] and xval_g lt XR[1] then XYOUTS,xval_g,DY*0.05+max(gGROND),'GROND g',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_r gt XR[0] and xval_r lt XR[1] then XYOUTS,xval_r,DY*0.05+max(rGROND),'GROND r',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_i gt XR[0] and xval_i lt XR[1] then XYOUTS,xval_i,DY*0.05+max(iGROND),'GROND i',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_z gt XR[0] and xval_z lt XR[1] then XYOUTS,xval_z,DY*0.05+max(zGROND),'GROND z',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_J gt XR[0] and xval_J lt XR[1] then XYOUTS,xval_J,DY*0.05+max(jGROND),'GROND J',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_H gt XR[0] and xval_H lt XR[1] then XYOUTS,xval_H,DY*0.05+max(hGROND),'GROND H',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_K gt XR[0] and xval_K lt XR[1] then XYOUTS,xval_K,DY*0.05+max(kGROND),'GROND K',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
; -----------------------------------------------

zooment = where(Lspec gt XR(0) and Lspec lt XR(1))
oplot,Lspec(zooment),Fspec(zooment)/max(Fspec(zooment)),thick=thickall,linestyle=0,col=col.black
XYOUTS,DX*0.98+XR[0],DY*0.94+YR[0],trim(specfile),col=col.black,charsize=2.0,charthick=thickall,alignment=1.0

oplot,fltarr(2)+G141range[0],YR,thick=thickall,linestyle=3,col=col.black
oplot,fltarr(2)+G141range[1],YR,thick=thickall,linestyle=3,col=col.black

if ZR eq 2 then begin  ; if a zoom range is given
   oplot,fltarr(2)+zoomrange[0],YR,thick=thickall,linestyle=1,col=col.black
   oplot,fltarr(2)+zoomrange[1],YR,thick=thickall,linestyle=1,col=col.black
endif

if ze eq 1 then begin  ; overplotting emission lines if zest is given
   for ii=0,Nlines-1 do begin  ; looping over lines to plot
      if LinesL(ii)*(1+zest) gt XR[0] and LinesL(ii)*(1+zest) lt XR[1] then begin
         oplot,fltarr(2)+LinesL(ii)*(1+zest),YR,thick=thickall,linestyle=0,col=col.red
         XYOUTS,LinesL(ii)*(1+zest)+DX/100.,DY*ii/15.+0.3+YR[0],trim(textoidl(Linename(ii))),col=col.red,charsize=2.5,charthick=thickall
      endif
   endfor
   XYOUTS,DX*0.98+XR[0],DY*0.76+YR[0],'z = '+trim(zest),col=col.red,charsize=2.0,charthick=thickall,alignment=1.0
endif



if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   if EB eq 0 then plot1 = 'specAatmosphereAskylinesZOOMJHK.eps'
   if EB eq 1 then plot1 = 'specAatmosphereAskylinesZOOMJHK_'+trim(basenameEPS)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=50, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1200, ysize=500, title = 'Spec + Sky + Atm ZOOM JHK'
   thickall = 2
endelse
; setting plot range
XR = [10500,25000]/1e4   ; JHK range
;XR = [3000,10000]   ; griz range
YR = [0,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [\mum] (JHK range)') $
        , ytitle =textoidl('Transmission') $
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

zooment = where(ATMARR(0,*)/1e4 gt XR(0) and ATMARR(0,*)/1e4 lt XR(1))
oplot,ATMARR(0,zooment)/1e4,ATMARR(1,zooment),thick=thickall,linestyle=0,col=col.gray
XYOUTS,DX*0.98+XR[0],DY*0.88+YR[0],'Atmosphere transmission: '+trim(Astring),col=col.darkgray,charsize=2.0,charthick=thickall,alignment=1.0

zooment = where(SKYARR(0,*)/1e4 gt XR(0) and SKYARR(0,*)/1e4 lt XR(1))
oplot,SKYARR(0,zooment)/1e4,SKYARR(1,zooment)/max(SKYARR(1,zooment)),thick=thickall,linestyle=0,col=col.blue
XYOUTS,DX*0.98+XR[0],DY*0.82+YR[0],'Skylines: '+trim(Sstring),col=col.blue,charsize=2.0,charthick=thickall,alignment=1.0

; ---------------GROND FILTERS------------------
oplot,lamGROND/1e4,gGROND,thick=thickall,linestyle=2,col=col.black  ;purple
oplot,lamGROND/1e4,rGROND,thick=thickall,linestyle=2,col=col.black  ;magenta
oplot,lamGROND/1e4,iGROND,thick=thickall,linestyle=2,col=col.black  ;blue
oplot,lamGROND/1e4,zGROND,thick=thickall,linestyle=2,col=col.black  ;green
oplot,lamGROND/1e4,jGROND,thick=thickall,linestyle=2,col=col.black  ;yellow
oplot,lamGROND/1e4,hGROND,thick=thickall,linestyle=2,col=col.black  ;orange
oplot,lamGROND/1e4,kGROND,thick=thickall,linestyle=2,col=col.black  ;red

if xval_g/1e4 gt XR[0] and xval_g/1e4 lt XR[1] then XYOUTS,xval_g/1e4,DY*0.05+max(gGROND),'GROND g',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_r/1e4 gt XR[0] and xval_r/1e4 lt XR[1] then XYOUTS,xval_r/1e4,DY*0.05+max(rGROND),'GROND r',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_i/1e4 gt XR[0] and xval_i/1e4 lt XR[1] then XYOUTS,xval_i/1e4,DY*0.05+max(iGROND),'GROND i',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_z/1e4 gt XR[0] and xval_z/1e4 lt XR[1] then XYOUTS,xval_z/1e4,DY*0.05+max(zGROND),'GROND z',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_J/1e4 gt XR[0] and xval_J/1e4 lt XR[1] then XYOUTS,xval_J/1e4,DY*0.05+max(jGROND),'GROND J',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_H/1e4 gt XR[0] and xval_H/1e4 lt XR[1] then XYOUTS,xval_H/1e4,DY*0.05+max(hGROND),'GROND H',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_K/1e4 gt XR[0] and xval_K/1e4 lt XR[1] then XYOUTS,xval_K/1e4,DY*0.05+max(kGROND),'GROND K',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
; -----------------------------------------------


zooment = where(Lspec gt G141range(0) and Lspec lt G141range(1))
;zooment = where(Lspec gt XR(0) and Lspec lt XR(1))
oplot,Lspec(zooment)/1e4,Fspec(zooment)/max(Fspec(zooment)),thick=thickall,linestyle=0,col=col.black
XYOUTS,DX*0.98+XR[0],DY*0.94+YR[0],trim(specfile),col=col.black,charsize=2.0,charthick=thickall,alignment=1.0

oplot,fltarr(2)+G141range[0]/1e4,YR,thick=thickall,linestyle=3,col=col.black
oplot,fltarr(2)+G141range[1]/1e4,YR,thick=thickall,linestyle=3,col=col.black

if ZR eq 2 then begin  ; if a zoom range is given
   oplot,fltarr(2)+zoomrange[0]/1e4,YR,thick=thickall,linestyle=1,col=col.black
   oplot,fltarr(2)+zoomrange[1]/1e4,YR,thick=thickall,linestyle=1,col=col.black
endif

if ze eq 1 then begin  ; overplotting emission lines if zest is given
   for ii=0,Nlines-1 do begin  ; looping over lines to plot
      if LinesL(ii)/1e4*(1+zest) gt XR[0] and LinesL(ii)/1e4*(1+zest) lt XR[1] then begin
         oplot,fltarr(2)+LinesL(ii)/1e4*(1+zest),YR,thick=thickall,linestyle=0,col=col.red
         XYOUTS,LinesL(ii)/1e4*(1+zest)+DX/100.,DY*ii/15.+0.3+YR[0],trim(textoidl(Linename(ii))),col=col.red,charsize=2.5,charthick=thickall
      endif
   endfor
   XYOUTS,DX*0.98+XR[0],DY*0.76+YR[0],'z = '+trim(zest),col=col.red,charsize=2.0,charthick=thickall,alignment=1.0
endif



if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================

;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   if EB eq 0 then plot1 = 'specAatmosphereAskylinesZOOMJHKclean.eps'
   if EB eq 1 then plot1 = 'specAatmosphereAskylinesZOOMJHKclean_'+trim(basenameEPS)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=50, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1200, ysize=500, title = 'Spec + Sky + Atm ZOOM JHK clean'
   thickall = 2
endelse
; setting plot range
XR = [10500,25000]/1e4   ; JHK range
;XR = [3000,10000]   ; griz range
YR = [0,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [\mum] (JHK range)') $
        , ytitle =textoidl('Transmission') $
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

; ---------------GROND FILTERS------------------
;oplot,lamGROND/1e4,gGROND,thick=thickall,linestyle=0,col=col.gray  ;purple
;oplot,lamGROND/1e4,rGROND,thick=thickall,linestyle=0,col=col.gray  ;magenta
;oplot,lamGROND/1e4,iGROND,thick=thickall,linestyle=0,col=col.gray  ;blue
;oplot,lamGROND/1e4,zGROND,thick=thickall,linestyle=0,col=col.gray  ;green
;oplot,lamGROND/1e4,jGROND,thick=thickall,linestyle=0,col=col.gray  ;yellow
;oplot,lamGROND/1e4,hGROND,thick=thickall,linestyle=0,col=col.gray  ;orange
;oplot,lamGROND/1e4,kGROND,thick=thickall,linestyle=0,col=col.gray  ;red

oplot,lamSINFONIj,effSINFONIj,thick=thickall,linestyle=0,col=col.darkgray  
oplot,lamSINFONIh,effSINFONIh,thick=thickall,linestyle=0,col=col.darkgray
oplot,lamSINFONIk,effSINFONIk,thick=thickall,linestyle=0,col=col.darkgray  

;if xval_g/1e4 gt XR[0] and xval_g/1e4 lt XR[1] then XYOUTS,xval_g/1e4,DY*0.05+max(gGROND),'GROND g',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
;if xval_r/1e4 gt XR[0] and xval_r/1e4 lt XR[1] then XYOUTS,xval_r/1e4,DY*0.05+max(rGROND),'GROND r',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
;if xval_i/1e4 gt XR[0] and xval_i/1e4 lt XR[1] then XYOUTS,xval_i/1e4,DY*0.05+max(iGROND),'GROND i',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
;if xval_z/1e4 gt XR[0] and xval_z/1e4 lt XR[1] then XYOUTS,xval_z/1e4,DY*0.05+max(zGROND),'GROND z',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
;if xval_J/1e4 gt XR[0] and xval_J/1e4 lt XR[1] then XYOUTS,xval_J/1e4,DY*0.05+max(jGROND),'GROND J',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
;if xval_H/1e4 gt XR[0] and xval_H/1e4 lt XR[1] then XYOUTS,xval_H/1e4,DY*0.05+max(hGROND),'GROND H',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
;if xval_K/1e4 gt XR[0] and xval_K/1e4 lt XR[1] then XYOUTS,xval_K/1e4,DY*0.05+max(kGROND),'GROND K',col=col.gray,charsize=2.0,charthick=thickall,alignment=0.5
if xval_J/1e4 gt XR[0] and xval_J/1e4 lt XR[1] then XYOUTS,xval_J/1e4,DY*0.05+max(effSINFONIj),'SINFONI J',col=col.darkgray,charsize=2.0,charthick=thickall,alignment=0.5
if xval_H/1e4 gt XR[0] and xval_H/1e4 lt XR[1] then XYOUTS,xval_H/1e4,DY*0.05+max(effSINFONIh),'SINFONI H',col=col.darkgray,charsize=2.0,charthick=thickall,alignment=0.5
if xval_K/1e4 gt XR[0] and xval_K/1e4 lt XR[1] then XYOUTS,xval_K/1e4,DY*0.05+max(effSINFONIk),'SINFONI K',col=col.darkgray,charsize=2.0,charthick=thickall,alignment=0.5
; -----------------------------------------------


; ---------------------POLY FILL SINFONI CURVES --------------------------
low   = min(lamSINFONIj)
high  = max(lamSINFONIj)
lowY  = 0
highY = 0
xpoly = [         low,  low, lamSINFONIj,  high,         high]
ypoly = [effSINFONIj[0], lowY, effSINFONIj, highY, effSINFONIj[0]]
PolyFill, xpoly, ypoly, Color=col.gray

low   = min(lamSINFONIh)
high  = max(lamSINFONIh)
lowY  = 0
highY = 0
xpoly = [         low,  low, lamSINFONIh,  high,         high]
ypoly = [0, lowY, effSINFONIh, highY, 0]
PolyFill, xpoly, ypoly, Color=col.gray

low   = min(lamSINFONIk)
high  = max(lamSINFONIk)
lowY  = 0
highY = 0
xpoly = [         low,  low, lamSINFONIk,  high,         high]
ypoly = [effSINFONIk[0], lowY, effSINFONIk, highY, effSINFONIk[0]]
PolyFill, xpoly, ypoly, Color=col.gray
; --------------------------------------------------------------------------




zooment = where(Lspec gt G141range(0) and Lspec lt G141range(1))
oplot,Lspec(zooment)/1e4,Fspec(zooment)/max(Fspec(zooment)),thick=thickall,linestyle=0,col=col.black
if EB eq 0 then XYOUTS,DX*0.98+XR[0],DY*0.075+YR[0],'3D-HST 1D spectrum',col=col.black,charsize=2.0,charthick=thickall,alignment=1.0
if EB eq 1 then XYOUTS,DX*0.98+XR[0],DY*0.075+YR[0],'3D-HST 1D spectrum of '+trim(basenameEPS),col=col.black,charsize=2.0,charthick=thickall,alignment=1.0

oplot,fltarr(2)+G141range[0]/1e4,YR,thick=thickall,linestyle=3,col=col.black
oplot,fltarr(2)+G141range[1]/1e4,YR,thick=thickall,linestyle=3,col=col.black

if ZR eq 2 then begin  ; if a zoom range is given
   oplot,fltarr(2)+zoomrange[0]/1e4,YR,thick=thickall,linestyle=2,col=col.black
   oplot,fltarr(2)+zoomrange[1]/1e4,YR,thick=thickall,linestyle=2,col=col.black
endif


if ze eq 1 then begin  ; overplotting emission lines if zest is given
   sign = 1  ; defining sign to control XYOUTs
   for ii=0,Nlines-1 do begin  ; looping over lines to plot
      if LinesL(ii)/1e4*(1+zest) gt XR[0] and LinesL(ii)/1e4*(1+zest) lt XR[1] then begin
         oplot,fltarr(2)+LinesL(ii)/1e4*(1+zest),YR,thick=thickall,linestyle=0,col=col.red
         XYOUTS,LinesL(ii)/1e4*(1+zest)+DX/100.,0.85+sign*0.05+YR[0],trim(textoidl(Linename(ii))),col=col.red,charsize=2.5,charthick=thickall
      endif
      sign = sign*(-1.)
   endfor
   XYOUTS,DX*0.98+XR[0],DY*0.15+YR[0],'z = '+trim(zest,'(F8.4)'),col=col.red,charsize=2.0,charthick=thickall,alignment=1.0
endif



if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================


if ZR eq 2 then begin  ; if a zoom range is given
;=============================================================================================
!p.multi = [0,0,0]
if PS eq 1 then begin
   set_plot, 'ps'
   col=getcolor(/load)     ; get color table for plot
   if EB eq 0 then plot1 = 'specAatmosphereAskylinesZOOMrange.eps'
   if EB eq 1 then plot1 = 'specAatmosphereAskylinesZOOMrange_'+trim(basenameEPS)+'.eps'
   device,  file=plot1 ,/color , /encapsulated, xsize=50, ysize=25,/CMYK  
   !P.FONT = 0                         ; allowing to change font of device
   device, set_font='Times-Roman'      ; setting font of device;, xsize=7
   thickall = 6
endif else begin
   set_plot, 'x'
   col=getcolor(/load)     ; get color table for plot
   device, retain=2        ; ensuring that plotting windows 'regenerate'
   window, Nw, xsize=1200, ysize=500, title = 'Spec + Sky + Atm ZOOMrange'
   thickall = 2
endelse
; setting plot range
XR = ZOOMRANGE
YR = [0,1]
DX = XR[1]-XR[0]
DY = YR[1]-YR[0]

plot,fltarr(2),fltarr(2), col=col.black    $
        , /NODATA $
        , xtitle =textoidl('\lambda [A]') $
        , ytitle =textoidl('Transmission') $
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

zooment = where(ATMARR(0,*) gt XR(0) and ATMARR(0,*) lt XR(1))
oplot,ATMARR(0,zooment),ATMARR(1,zooment),thick=thickall,linestyle=0,col=col.gray
XYOUTS,DX*0.98+XR[0],DY*0.88+YR[0],'Atmosphere transmission: '+trim(Astring),col=col.darkgray,charsize=2.0,charthick=thickall,alignment=1.0

zooment = where(SKYARR(0,*) gt XR(0) and SKYARR(0,*) lt XR(1))
oplot,SKYARR(0,zooment),SKYARR(1,zooment)/max(SKYARR(1,zooment)),thick=thickall,linestyle=0,col=col.blue
XYOUTS,DX*0.98+XR[0],DY*0.82+YR[0],'Skylines: '+trim(Sstring),col=col.blue,charsize=2.0,charthick=thickall,alignment=1.0

; ---------------GROND FILTERS------------------
oplot,lamGROND,gGROND,thick=thickall,linestyle=2,col=col.black  ;purple
oplot,lamGROND,rGROND,thick=thickall,linestyle=2,col=col.black  ;magenta
oplot,lamGROND,iGROND,thick=thickall,linestyle=2,col=col.black  ;blue
oplot,lamGROND,zGROND,thick=thickall,linestyle=2,col=col.black  ;green
oplot,lamGROND,jGROND,thick=thickall,linestyle=2,col=col.black  ;yellow
oplot,lamGROND,hGROND,thick=thickall,linestyle=2,col=col.black  ;orange
oplot,lamGROND,kGROND,thick=thickall,linestyle=2,col=col.black  ;red

if xval_g gt XR[0] and xval_g lt XR[1] then XYOUTS,xval_g,DY*0.05+max(gGROND),'GROND g',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_r gt XR[0] and xval_r lt XR[1] then XYOUTS,xval_r,DY*0.05+max(rGROND),'GROND r',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_i gt XR[0] and xval_i lt XR[1] then XYOUTS,xval_i,DY*0.05+max(iGROND),'GROND i',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_z gt XR[0] and xval_z lt XR[1] then XYOUTS,xval_z,DY*0.05+max(zGROND),'GROND z',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_J gt XR[0] and xval_J lt XR[1] then XYOUTS,xval_J,DY*0.05+max(jGROND),'GROND J',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_H gt XR[0] and xval_H lt XR[1] then XYOUTS,xval_H,DY*0.05+max(hGROND),'GROND H',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
if xval_K gt XR[0] and xval_K lt XR[1] then XYOUTS,xval_K,DY*0.05+max(kGROND),'GROND K',col=col.black,charsize=2.0,charthick=thickall,alignment=0.5
; -----------------------------------------------

zooment = where(Lspec gt XR(0) and Lspec lt XR(1))
oplot,Lspec(zooment),Fspec(zooment)/max(Fspec(zooment)),thick=thickall,linestyle=0,col=col.black
XYOUTS,DX*0.98+XR[0],DY*0.94+YR[0],trim(specfile),col=col.black,charsize=2.0,charthick=thickall,alignment=1.0

if ze eq 1 then begin  ; overplotting emission lines if zest is given
   for ii=0,Nlines-1 do begin  ; looping over lines to plot
      if LinesL(ii)*(1+zest) gt XR[0] and LinesL(ii)*(1+zest) lt XR[1] then begin
         oplot,fltarr(2)+LinesL(ii)*(1+zest),YR,thick=thickall,linestyle=0,col=col.red
         XYOUTS,LinesL(ii)*(1+zest)+DX/100.,DY*ii/15.+0.3+YR[0],trim(textoidl(Linename(ii))),col=col.red,charsize=2.5,charthick=thickall
      endif
   endfor
   XYOUTS,DX*0.98+XR[0],DY*0.76+YR[0],'z = '+trim(zest),col=col.red,charsize=2.0,charthick=thickall,alignment=1.0
endif



if PS eq 1 then begin
   device, /close
   !P.FONT = -1    ; setting default font (needed if PS font has been changed)
   set_plot, 'x'
endif
Nw = Nw+1                       ; incremeting window number by 1
;=============================================================================================
endif

if vb eq 1 then print,' '
if vb eq 1 then print,':: specWatmosphereANDskylinesOH.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
