;+
;----------------------------
;   NAME
;----------------------------
; makeSDSSfindingchart.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure creates and downloads jpg finding charts of a list of
; SDSS objects
;----------------------------
;   COMMENTS
;----------------------------
; Uses wget to download the image (saved as NAME.jpg if names are given
;----------------------------
;   INPUTS:
;----------------------------
; RA              : list of R.A. of object
; DEC             : list of Dec of object
; SAVEDIR         : path to directory where the finding charts will be saved 
;                   (end with slash)
; PIX             : the pixel size of the image on the form [xpix,ypix]
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; SCALE           : scale of images (default is 0.396127 ''/pix)
; NAMES           : List of names (e.g. objids) to use as name of finding charts
;                   If not given the names will be ra and dec in sexigesimal (w/o colon) 
; OPTION          : Drawing options as given on http://cas.sdss.org/astro/en/tools/chart/chart.asp
;                   the default is GLI: Grid, Label and Inverted 
; SEX             : set /SEX to write ra and dec as sexagesimal numbers on finding charts
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> makeSDSSfindingchart,[320.67922,320.67],[-0.44823529,-50.44823529],'/Users/kasperborelloschmidt/work/observing/101210_GRONDP86/findingcharts/',[758,758],NAMES=['name1','name2'],OPTION='GLI',/VERBOSE,/SEX
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-10  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
; wget to download images
; skycoor to create default names
;----------------------------
;-
PRO makeSDSSfindingchart,RA,DEC,SAVEDIR,PIX,SCALE=SCALE,NAMES=NAMES,OPTION=OPTION,SEX=SEX,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

SC = n_elements(SCALE)
NM = n_elements(NAMES)
OP = n_elements(OPTION)
SX = n_elements(SEX)
VB = n_elements(VERBOSE)

if SC eq 0 then SCALE = 0.396127
if OP eq 0 then OPTION = 'GLI'

for ii=0,n_elements(RA)-1 do begin
   if NM eq 0 then begin
      spawnstring='skycoor '+strtrim(ra(ii),2)+'  '+strtrim(dec(ii),2)
      spawn,spawnstring,spawnout
      sexcoor = strsplit(spawnout,' ',/extract)
      RAsex   = sexcoor(0)
      DECsex  = sexcoor(1)
      term1   = strjoin(strsplit(RAsex,':',/extract))
      term2   = strjoin(strsplit(DECsex,':',/extract))
      
      NAME = strtrim(term1,2)+strtrim(term2,2)+'.jpg'
   endif else begin
      NAME = NAMES(ii)+'.jpg'
   endelse

   command = " wget -O "+strtrim(SAVEDIR,2)+strtrim(NAME,2)+" 'http://casjobs.sdss.org/ImgCutoutDR7/getjpeg.aspx?ra="+strtrim(RA(ii),2)+"&dec="+strtrim(DEC(ii),2)+"&scale="+strtrim(SCALE,2)+"&width="+strtrim(PIX(0),1)+"&height="+strtrim(PIX(1),2)+"&opt="+strtrim(OPTION,2)+"' "
   spawn, command
   if vb eq 1 then print,':: makeSDSSfindingchart.pro :: Saved finding chart as ',strtrim(NAME,2)
   if vb eq 1 then print,' ' 

   if SX eq 1 then begin
      skycoorcmd =  'skycoor '+strtrim(RA(ii),2)+' '+strtrim(DEC(ii),2)
      spawn,skycoorcmd,sex0 ; turning deg into sexagesimal
      sex = strsplit(sex0,/extract)  ; vector with sexagesimal ra and dec
      
      pos = [pix(0)*0.05,pix(1)*0.95] ; position of where to write ra and dec
      convertcmd = 'convert '+strtrim(SAVEDIR,2)+strtrim(NAME,2)+' -pointsize 15 -draw'+" ' "+' text '+strtrim(pos(0),2)+', '+strtrim(pos(1),2)+' " ra: '+strtrim(sex[0],2)+'  dec: '+strtrim(sex[1],2)+'  ID: '+strtrim(NAME,2)+' " '+" ' "+strtrim(SAVEDIR,2)+strtrim(NAME,2) ; writing ra dec on image in sexagesimal
      spawn,convertcmd
   endif
endfor

if vb eq 1 then print,' '
if vb eq 1 then print,':: makeSDSSfindingchart.pro :: Downloaded ',strtrim(n_elements(RA),2),' finding charts to ',strtrim(SAVEDIR,2)
if vb eq 1 then print,' '
if vb eq 1 then print,':: makeSDSSfindingchart.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
