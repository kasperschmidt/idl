;+
;----------------------------
;   NAME
;----------------------------
; ds9region_objects.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Creating a ds9 region file which indicates object detections on a
; fits image. For instance an SExtractor output catalog can be
; overplotted the image from which is was created
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; xpos            : list of the object's x-coordinates
; ypos            : list of the object's y-coordinates
; rad             : list of radii of the circles to be drawn
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ID              : A list of object IDs/numbers which will be written
;                   on the objects with the circles
; IMAGEFILE       : Provide text string of image file which the regions fit
;                   to and this will be written in the generated region file's 
;                   header and file name.
; COOR            : units of xpos and ypos (default is pixels, i.e. physical)
;                   The choices are:
;                          COOR = 'PIX'
;                          COOR = 'RADEC'
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; regname         : Name of the generated region file
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> ds9region_objects,xpos,ypos,rad,regname,ID=no,COOR='pix',IMAGEFILE='F140W_drz.fits',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-12  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO ds9region_objects,xpos,ypos,rad,regname,ID=ID,COOR=COOR,IMAGEFILE=IMAGEFILE,VERBOSE=VERBOSE

IMG = n_elements(IMAGEFILE)
CO  = n_elements(COOR)
VB  = n_elements(VERBOSE)

time0 = systime( )
time  = StrJoin(StrSplit(time0,' ',/Extract),'_')

if IMG eq 0 then begin
   regname = 'ds9region_objects_OUT'+time+'.reg'
endif else begin
   IMAGE0 = StrSplit(IMAGEFILE,'.',/Extract)
   if n_elements(IMAGE0) eq 2 then IMAGE  = IMAGE0(0)
   if n_elements(IMAGE0) gt 2 then IMAGE  = StrJoin(IMAGE0(0:n_elements(IMAGE0)-2),'_')
   regname = strtrim(IMAGE,2)+'_ds9region_objects_'+time+'.reg'
endelse

openw,55,regname,width=200
; ------------------- writing region file -------------------

printf,55,'# Region file for ds9'
printf,55,'# created on ',time0,' with ds9region_objects.pro'
if IMG ne 0 then printf,55,'# Made for ',IMAGEFILE
printf,55,'global color=green dashlist=8 3 width=1 font="helvetica 10 normal" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1'
printf,55,'  '
case COOR of
   'PIX'    : printf,55,'physical'
   'RADEC'  : printf,55,'fk5'
   else     : printf,55,'physical'
endcase

for ii=0,n_elements(xpos)-1 do begin  ; loop over all objects
   printf,55,'  '
   printf,55,'circle(',strtrim(xpos(ii),2),',',strtrim(ypos(ii),2),',',strtrim(rad(ii),2),') # color=red width=2 font="helvetica 9 normal" '
   if n_elements(ID) ne 0 then printf,55,'# text(',strtrim(xpos(ii),2),',',strtrim(ypos(ii),2),') color=red width=2 font="helvetica 9 normal" text={',strtrim(ID(ii),2),'} '
endfor

; -----------------------------------------------------------
close,55

if vb eq 1 then print,':: ds9region_objects.pro :: Wrote region file ',strtrim(regname,2)
if vb eq 1 and IMG ne 0 then print,'                            for ',strtrim(IMAGEFILE,2)
END
