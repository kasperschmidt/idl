;+
;----------------------------
;   NAME
;----------------------------
; slopeinvestigationMULTIPLE.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure runs slopeinvestigation.pro multiple times for
; several setups
;----------------------------
;   COMMENTS
;----------------------------
; NB! code has to be run from IDL shell in /Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks
;----------------------------
;   INPUTS:
;----------------------------
; FILELIST        : ascii file with the list of datafiles to run slopeinvestigation.pro on
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; REPICK          : This keyword will redraw the data from gaussian distributions around 
;                   each data point with standard deviations REPICK*Xerr_i and REPICK*Yerr_i
;                   for each datafile in FILELIST after having run the intial data.
; /ALLCOL         : set /ALLCOL to run fit in all magnitude planes (for all colors)
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> slopeinvestigationMULTIPLE,'testfilelist.txt',REPICK=3.0,/ALLCOL,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-06  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ slopeinvestigation.pro
;----------------------------
;-
PRO slopeinvestigationMULTIPLE,FILELIST,VERBOSE=VERBOSE,REPICK=REPICK,ALLCOL=ALLCOL
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

VB = n_elements(VERBOSE)
RP = n_elements(REPICK)
AC = n_elements(ALLCOL)

readcol,format='A',filelist,file
Nfiles = n_elements(file)

listname = strsplit(filelist,'.',/extract)
if n_elements(listname) gt 2 then begin
   print,':: slopeinvestigationMULTIPLE :: Only one dot (before extension) allowed in filelist name  -> aborting'
   stop
endif

for ii=0,Nfiles-1 do begin                                    ; looping over files
   magspace = ['gr','ug','ri','gi']                           ; define vector with COLFIT possibilities
   if AC eq 1 then COL = 4                                    ; check if all colors are suppose to be used
   if AC eq 0 then COL = 1                                    ; ony running fit in gr space
   for jj=0,COL-1 do begin                                     ; running slopeinvestigation
      directory   = 'PDFs_FILELIST'+strtrim(listname(0),2)+'_FILE'+strtrim(ii,2)+'_'+strtrim(magspace(jj),2)+'fit/'
      outputascii = strtrim(directory,2)+'slopeinvestigationMULTIPLE_output_FILELIST'+strtrim(listname(0),2)+'_FILE'+strtrim(ii,2)+'_'+strtrim(magspace(jj),2)+'fit.txt'
      spawnstring = 'mkdir '+strtrim(directory,2)                ; string to pass through spawn
      spawn,spawnstring,SPresult                                 ; creating PDF directory
      if VB eq 1 then print,SPresult
      slopeinvestigation,file(ii),outputascii,PDFdir=directory,MCMCloops=10000,COLFIT=magspace(jj)
   endfor

   if RP eq 1 then begin
      SIG = REPICK                                            ; setting standard deviation to use when re-picking
      for jj=0,COL-1 do begin                                  ; running slopeinvestigation with re-picking data
         directoryRP   = 'PDFs_FILELIST'+strtrim(listname(0),2)+'_FILE'+strtrim(ii,2)+'_'+strtrim(magspace(jj),2)+'fit_RP02/'
         outputasciiRP = strtrim(directoryRP,2)+'slopeinvestigationMULTIPLE_output_FILELIST'+strtrim(listname(0),2)+'_FILE'+strtrim(ii,2)+'_'+strtrim(magspace(jj),2)+'fit_RP02.txt'   
         spawnstring = 'mkdir '+strtrim(directoryRP,2)           ; string to pass through spawn
         spawn,spawnstring,SPresult                              ; creating PDF directory
         if VB eq 1 then print,SPresult
         slopeinvestigation,file(ii),outputasciiRP,PDFdir=directoryRP,MCMCloops=10000,COLFIT=magspace(jj),REPICK=SIG
      endfor
   endif
endfor 

if vb eq 1 then print,' '
if vb eq 1 then print,':: slopeinvestigationMULTIPLE.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
