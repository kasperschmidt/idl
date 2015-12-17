;+
;----------------------------
;   NAME
;----------------------------
; runDisplay_v2_multiplefields.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Running the Display_v2 scripts for visually inspecting objects for
; multiple fields
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; filelist         : List of files (e.g. borg_name/final_dropouts.cat)
;                    to containing objects to inspect. Expect a format similar 
;                    to final_dropouts.cat (or rather ID in first column and 
;                    and x and y pixel position in image in column 9 and 10
; zeropoints       : the zero points of the fields in the filelist.
;                    Can be generated with calcBoRGzeropoint.pro
; inspectionscheme : File containing the inspection cheme to use (i.e. what 
;                    buttons to generate). Contains first column with value
;                    and a short description after a tab-delimiter
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; outputfile      : Name of file to write output to. Default is runDisplay_v2_multiplefields_OUTPUT.txt
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> runDisplay_v2_multiplefields,'/Users/kasperborelloschmidt/work/BoRG/borgdata/cycle19_new_nov14_cat/all_final_dropouts_run130118.txt','dirlist_deredzeropoints.txt','inspectionscheme.txt',outputfile='all_final_dropouts_cats_DisplayInspectionDATE.txt',/VERBOSE,/STP
;
; -- version_2p0 objects --
; IDL> runDisplay_v2_multiplefields,'/Users/kasperborelloschmidt/work/BoRG/borgdata/version_2p0/final_dropouts_ALL_606band.txt','dirlist606band_deredzeropoints.txt','../cycle19_new_nov14_cat/inspectionscheme.txt',outputfile='all_final_dropouts_cats_DisplayInspection130413.txt',/VERBOSE,/STP
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2013-01-18  started by K. B. Schmidt (UCSB)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
@  display_images_cat.pro
@ /Users/kasperborelloschmidt/idl711mac/itt/idl71/lib/astron/pro/readcol.pro ; to enable tab-delimiter
;----------------------------
;-
PRO runDisplay_v2_multiplefields,filelist,zeropoints,inspectionscheme,outputfile=outputfile,VERBOSE=VERBOSE,STP=STP

VB = n_elements(VERBOSE)

READCOL,filelist,format='A',catalogs,comment='#'
Ncats = n_elements(catalogs)

READCOL,zeropoints,format='A,f,f,f,f',zpfield,Vzp,Yzp,Jzp,Hzp,comment='#'

if vb eq 1 then print,':: runDisplay_v2_multiplefields :: Found '+trim(Ncats)+' catalogs to inspect'

if n_elements(outputfile) eq 1 then begin
    outfile = outputfile
endif else begin
    outfile = 'runDisplay_v2_multiplefields_OUTPUT.txt'
endelse
openw,22,outfile,width=300


for ii = 0,Ncats-1 do begin
    catname = strsplit(catalogs(ii),'/',/extract)
    catname = catname(0)
    if vb eq 1 then print,':: runDisplay_v2_multiplefields :: Attempting to read '+catalogs(ii) 
    readcol,catalogs(ii),format = 'A,F,F',ID,SN_ISO125,AUTOM125,D_AUTOM125,dropmeasured,error_dropmeasured,measured125160,error_measured125160,x_det,y_det,flag_det,stellar_det,SN_ISO606,SN_ISO098,SN_ISO160,ones
    Nobj = n_elements(ID)
    if Nobj eq 0 then begin
         print,':: runDisplay_v2_multiplefields :: '+catalogs(ii)+' is empty; advancing to next catalog'
         goto, next_catalog
    endif
    spawnstr = 'ls '+catname+'/*_drz.fits'                     ; assuming images are in *_drz.fits files
    spawn,spawnstr,spawnout
    if n_elements(spawnout) ne 4 then begin
        print,':: runDisplay_v2_multiplefields :: ERROR :: More than 4 fits file for (not enabled): '+catname
        stop
    endif
    filters = ['V', 'Y', 'J', 'H']
    imfiles = [spawnout(3),spawnout(0),spawnout(1),spawnout(2)]

    zprow = where(catname eq zpfield)     ; finding row in zeropoints file corresponding to for field
    if zprow eq [-1] then begin
        print,':: runDisplay_v2_multiplefields :: ERROR :: No zero points found for '+catname
        stop
    endif
    zp = [Vzp(zprow),Yzp(zprow),Jzp(zprow),Hzp(zprow)]

    info = display_images_cat(imfiles, x_det, y_det, ID, filtvect=filters, zero_point=zp, source_file = inspectionscheme)

    ;help,info,/str
    ;print, info.tv

    for jj = 0,Nobj-1 do begin
        printstr=catname+'_'+trim(ID(jj),'(I04)')+'    '+trim(info.tv(jj))
        printf,22,printstr
    endfor

    next_catalog:
endfor
close,22

if vb eq 1 then print,' '
if vb eq 1 then print,':: runDisplay_v2_multiplefields :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
