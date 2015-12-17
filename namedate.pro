;+
;----------------------------
;   NAME
;----------------------------
; namedate.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure returning the input string split into a path and a name together with 
; the UTC date and time as a string (with and w/o underscores as dividers).
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; input       : string containing the path and name of some file
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /local      : giving the (local) system time instead of UTC
;----------------------------
;   OUTPUTS:
;----------------------------
; path        : string containing the path to the file
; name        : string containing the name of the file (without extension)
; extension   : string containing the extension of the file
; date        : string containing the utc date and time
; dateus      : string containing the utc date and time seperated by underscores
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; input contains: the/path/of/an/inputfile/file.txt
; IDL> namedate,input,path,name,extension,date,dateus,/local
; returns the strings:
; path      = the/path/of/a/inputfile
; name      = file
; extension = txt
; date      = Wed Dec  2 10:41:43 2009
; dateus    = Wed_Dec_2_10:41:43_2009
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2009-12-02  started by K. B. Schmidt (MPIA)
; 2010-10-13  /local keyword added - K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;
;----------------------------
;-
PRO namedate,input,path,name,extension,date,dateus,local=local

; creating a string of the utc date and time
date   = systime(/UTC)
if n_elements(local) eq 1 then date   = systime( )
dateus = STRJOIN(STRSPLIT(date, /EXTRACT), '_')                      ; replace spaces wiht under scores
pathcut   = STRSPLIT(input, /EXTRACT,'/')                            ; splitting the path and filename
path      = STRJOIN(pathcut(0:n_elements(pathcut)-2),'/')            ; joining path back together
nametot   = STRSPLIT(pathcut(n_elements(pathcut)-1), /EXTRACT,'.')   ; splitting name and extension
name      = nametot(0)
extension = nametot(1)

END
