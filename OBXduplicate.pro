;+
;----------------------------
;   NAME
;----------------------------
; OBXduplicate.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure for duplicating .obx files
; (NB! has to be manually adjusted for individual master OBs!)
;----------------------------
;   COMMENTS
;----------------------------
; The matching of finding charts is done based on the ID. I.e. to
; properbly match the finding charts to the files the name of the
; findingcharts should be ID(i).jpg for the i'th object
;----------------------------
;   INPUTS:
;----------------------------
; ID              : unique ID of objects
; ra              : ra to substitute  (degrees - will be turned into sexegesimal) 
; dec             : dec to substitute (degrees - will be turned into sexegesimal)
; priority        : Priority number (from 1 to N_objects)
; outputdir       : path of the directory in which the .obx files will
;                   be put (end with slash).
; findingcharts   : directory (relative to output dir) of the finding
;                   charts, e.g. '../findingcharts/' if outputdir is in 
;                   the same directory as the finding chart directory (end with slash).
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
; /SEX            : set this keyword if the ra and dec vectors are
;                   string arrays with ra and dec in sexigesimal units 
;                   (e.g. 03:32:48.504 -00:21:55.33)
;----------------------------
;   OUTPUTS:
;----------------------------
; .onx files and a list of the created .obx and their priority
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> OBXduplicate,ID,ra,dec,priority,'/Users/kasperborelloschmidt/work/observing/101210_GRONDP86/OBs/','../findingcharts/',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-25  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
; skycoor    used to turn RA DEC degrees into sexagesimal
;----------------------------
;-
PRO OBXduplicate,ID,ra,dec,priority,outputdir,findingcharts,VERBOSE=VERBOSE,SEX=SEX
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

SX = n_elements(SEX)
VB = n_elements(VERBOSE)

Nobx = n_elements(ID)  ; number of OBs to create
if vb eq 1 then print,' '
if vb eq 1 then print,':: OBXduplicate.pro :: Detected ',strtrim(Nobx,2),' objects to make an .obx file for.'
if vb eq 1 then print,' '

; opening file
obxLIST = strtrim(outputdir,2)+'obxpriorities.txt'
openw,33,obxLIST,width=300


for ii=0,Nobx-1 do begin
   ; getting values to substitute in master.obx (written by hand below)
   obxID   = ID(ii)
   obxP    = priority(ii)
   obxRA   = ra(ii)
   obxDEC  = dec(ii)

   if SX eq 0 then begin  ; if ra dec given in degress - turn them into sexagesimal
      spawnstring='skycoor '+strtrim(obxra,2)+'  '+strtrim(obxdec,2)
      spawn,spawnstring,spawnout
      sexcoor = strsplit(spawnout,' ',/extract)
      obxRA   = sexcoor(0)
      obxDEC  = sexcoor(1)
   endif

   ; creating SDSS name from ra and dec
   term1   = strjoin(strsplit(obxRA,':',/extract))
   term2   = strjoin(strsplit(obxDEC,':',/extract))
   obxNAME = 'SDSS J'+strtrim(term1,2)+strtrim(term2,2)

   ; opening file
   FILE0   = strtrim(term1,2)+strtrim(term2,2)+'.obx'
   obxFILE = strtrim(outputdir,2)+FILE0
   openw,22,obxFILE,width=300

   ; writing name and priority to file
   printf,33,FILE0,'  ',obxP,'  ',obxID

   ; --- substituting into and writing .obx file ---
   printf,22,'IMPEX.VERSION "2.0"'
   printf,22,'type                    "O"'
   printf,22,'STTimeIntervals         ""'
   printf,22,'calibrationReq          ""'
   printf,22,'InstrumentComments      ""'
   printf,22,'userComments            "DR7 ID: ',strtrim(obxID,2),'"'
   printf,22,'userPriority            "',strtrim(obxP,2),'"'
   printf,22,'LineNumber              "0"'
   printf,22,'name                    "',strtrim(term1,2)+strtrim(term2,2),'"'     ;',strtrim(obxID,2),'"'
   printf,22,'finding_chart_list "',strtrim(findingcharts,2),strtrim(term1,2)+strtrim(term2,2),'.jpg "'
   printf,22,' '
   printf,22,'comments                ""'
   printf,22,'objectClass             " Unknown            "'
   printf,22,'ra                      " ',strtrim(obxRA,2),'"'
   printf,22,'dec                     " ',strtrim(obxDEC,2),'"'
   printf,22,'epoch                   "2000.0"'
   printf,22,'equinox                 "2000"'
   printf,22,'propDec                 "0.000000"'
   printf,22,'propRA                  "0.000000"'
   printf,22,'diffRA                  "0.000000"'
   printf,22,'diffDec                 "0.000000"'
   printf,22,'LineNumber              "0"'
   printf,22,'TARGET.NAME             "',strtrim(obxNAME,2),'"'
   printf,22,' '
   printf,22,'air_mass                      "5.0"'
   printf,22,'fractional_lunar_illumination "1.0"'
   printf,22,'sky_transparency              "Photometric"'
   printf,22,'moon_angular_distance         "30"'
   printf,22,'seeing                        "1.0"'
   printf,22,'StrehlRatio                   "0.0"'
   printf,22,'CONSTRAINT.SET.NAME           "No Name"'
   printf,22,' '
   printf,22,'longDescription              ""'
   printf,22,'IPVersion                    "102.0"'
   printf,22,'instrument                   "GROND"'
   printf,22,'LineNumber                   "0"'
   printf,22,'OBSERVATION.DESCRIPTION.NAME "8m4td"'
   printf,22,' '
   printf,22,'ACQUISITION.TEMPLATE.NAME "GROND_img_acq"'
   printf,22,'TEL.TARG.FOCOFFSET "0"'
   printf,22,'TEL.AG.START       "T"'
   printf,22,'TEL.GS1.ALPHA      " ',strtrim(obxRA,2),'"'
   printf,22,'TEL.GS1.DELTA      " ',strtrim(obxDEC,2),'"'
   printf,22,'TEL.GS1.MAG        "14"'
   printf,22,'TEL.PRESET.NEW     "T"'
   printf,22,' '
   printf,22,'TEMPLATE.NAME "GROND_img_obs_exp"'
   printf,22,'DET1.DIT            "10"'
   printf,22,'DET1.NDIT           "1"'
   printf,22,'DET1.IMODE          "Double"'
   printf,22,'DET1.NINT           "2"'
   printf,22,'DET2.NGR            "1"'
   printf,22,'DET2.NIZ            "1"'
   printf,22,'DET2.OMODE          "slow"'
   printf,22,'DET2.UITGR          "114.9"'
   printf,22,'DET2.UITIZ          "114.9"'
   printf,22,'TEL.TARG.TYPE       "science"'
   printf,22,'TEL.TARG.FOCOFFSET  "0"'
   printf,22,'TEL.TARG.NTD        "4"'
   printf,22,'TEL.TARG.NTP        "4"'
   printf,22,'TEL.TARG.OBSEQNUM   "1"'
   printf,22,'TEL.TARG.OBSRUNID   "1"'
   printf,22,'TEL.TARG.OBTYPEID   "8m4td"'
   printf,22,'TEL.TARG.TARGETID   "template"'
   printf,22,'TEL.COMBINED.OFFSET "T"'
   printf,22,'TEL.PRESET.AUTO     "F"'
   printf,22,'INS.TARG.NMD        "6"'
   printf,22,'INS.TARG.NMP        "6"'
   printf,22,' '
   close,22
endfor

close,33

if vb eq 1 then print,' '
if vb eq 1 then print,':: OBXduplicate.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END

