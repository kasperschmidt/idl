;+
;----------------------------
;   NAME
;----------------------------
; getPDFforOBJID.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Routine getting the PDF number for a DR7 object ID.
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; datafile        : string containing name and path of the data file
;                   used as input to slopeinvestigation.pro
; DR7id           : string with the ID you want to find the PDF number for
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /PLOT           : runs plotPDFs.pro on the indetified file (file dir is hardcoded)
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; PDFnumber       : The number of the PDF file created with slopeinvestigation.pro
;                   i.e., on the form PDFs_objectPDFnumber*.dat
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> getPDFforOBJID,'DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits','587731173842944689',PDFnumber,/VERBOSE

; it can then be plotted with the command
; IDL> plotPDFs,'PDFs_S82QSOs_shenmatchALL/PDFs_object672.dat',/VERBOSE,/OUTLIERS,/ALLINONE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-12-10  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ plotPDFs.pro
;----------------------------
;-
PRO getPDFforOBJID,datafile,DR7id,PDFnumber,VERBOSE=VERBOSE,PLOT=PLOT
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

PL = n_elements(PLOT)
VB = n_elements(VERBOSE)

str = mrdfits(datafile,1)

IDS = str(uniq(str.headobjid)).headobjid

PDFnumber = where(strtrim(IDS,2) eq strtrim(DR7id,2))

if vb eq 1 then print,':: getPDFforOBJID.pro :: The PDF number of object ',strtrim(DR7id,2),' is ',strtrim(PDFnumber,2)
if vb eq 1 then print,'                         Hence the PDFs will be in PDFs_object',strtrim(PDFnumber(0),2),'.dat (among others)'

if PL eq 1 then begin
   plotPDFs,'PDFs_S82QSOs_shenmatchALL/PDFs_object'+strtrim(PDFnumber(0),2)+'.dat',/OUTLIERS,/ALLINONE;,/VERBOSE
   ;plotPDFs,'PDFs_S82QSOs_shenmatchALL_NOsigmaadjust/PDFs_object'+strtrim(PDFnumber(0),2)+'.dat',/OUTLIERS,/ALLINONE;,/VERBOSE
endif

END
