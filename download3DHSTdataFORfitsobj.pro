;+
;----------------------------
;   NAME
;----------------------------
; download3DHSTdataFORfitsobj.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure reading a fits file and downloading the 3D-HST data
; products for the objects in the file. By default only the 2D spec is
; downloaded for each object but keywords enable further downloads.
;----------------------------
;   COMMENTS
;----------------------------
;
; NB! it is preferable to run this code in an empty directory since
;     the data is stored in the working directory!
;
;----------------------------
;   INPUTS:
;----------------------------
; fitsfile        : string containing name and path of input fits table with objects.
;                   The fits table needs to contain the column ID for each object
;                   containing the objects' "ID" given on the form:
;                      FIELD-POINTING#-G141-OBJ#   e.g.   COSMOS-1-G141-00202
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /thumb          : download the thumbnail fits if available for object
; /spec1D         : download the 1D spectrum png if available for object
; /SEDfit         : download the SEDfit png if available for object
; /STP            : Stop at the end of the program
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> download3DHSTdataFORfitsobj,'/Users/kasperborelloschmidt/work/3dHST/mergersearch/selectOBJfromGRISMandSED_Dzoutliers110208ALLCOLS.fits',/VERBOSE,/STP,/THUMB,/SPEC1D,/SEDFIT
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2012-02-08  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;Forward_Function runcode
;@ xxx.pro
;----------------------------
;-
PRO download3DHSTdataFORfitsobj,fitsfile,THUMB=THUMB,SPEC1D=SPEC1D,SEDFIT=SEDFIT,VERBOSE=VERBOSE,STP=STP

TH  = n_elements(THUMB)
S1D = n_elements(SPEC1D)
SED = n_elements(SEDFIT)
VB  = n_elements(VERBOSE)

dat = mrdfits(fitsfile,1)

Nobj = N_elements(dat.ID)

for ii=0,Nobj-1 do begin
   ; - - - - - - - - - - - - - - - - - - - - - - - - DOWNLOADING 2D SPEC - - - - - - - - - - - - - - - - - - - - - - - -
   if vb eq 1 then print,'-------------------Downloading 2D spectrum for ',trim(dat(ii).ID),' -> OBJ ',trim(ii+1),' of ',trim(Nobj),'-------------------'
   spawnSTR = "wget 'http://unicorn.astro.yale.edu/P/GRISM/images/"+trim(dat(ii).ID)+"_2D.fits.gz' --user=3dhst --password=getspecs"
   spawn,spawnSTR

   ; - - - - - - - - - - - - - - - - - - - - - - - - DOWNLOADING THUMBNAIL - - - - - - - - - - - - - - - - - - - - - - - -
   if TH eq 1 then begin
      if vb eq 1 then print,'-------------------Downloading F140W thumbnail for ',trim(dat(ii).ID),' -> OBJ ',trim(ii+1),' of ',trim(Nobj),'-------------------'
      spawnSTR = "wget 'http://unicorn.astro.yale.edu/P/GRISM/images/"+trim(dat(ii).ID)+"_thumb.fits.gz' --user=3dhst --password=getspecs"   
      spawn,spawnSTR
   endif

   ; - - - - - - - - - - - - - - - - - - - - - - - - DOWNLOADING 1D SPEC - - - - - - - - - - - - - - - - - - - - - - - -
   if S1D eq 1 then begin
      if vb eq 1 then print,'-------------------Downloading 1D spectrum png for ',trim(dat(ii).ID),' -> OBJ ',trim(ii+1),' of ',trim(Nobj),'-------------------'
      spawnSTR = "wget 'http://unicorn.astro.yale.edu/P/GRISM/images/"+trim(dat(ii).ID)+"_1D.png' --user=3dhst --password=getspecs"
      spawn,spawnSTR
   endif

   ; - - - - - - - - - - - - - - - - - - - - - - - - DOWNLOADING SEDFIT - - - - - - - - - - - - - - - - - - - - - - - -
   if SED eq 1 then begin
      if vb eq 1 then print,'-------------------Downloading SED fit png for ',trim(dat(ii).ID),' -> OBJ ',trim(ii+1),' of ',trim(Nobj),'-------------------'
      spawnSTR = "wget 'http://unicorn.astro.yale.edu/P/GRISM/SED/"+trim(dat(ii).ID)+"_SED.png' --user=3dhst --password=getspecs"
      spawn,spawnSTR
   endif
endfor

if vb eq 1 then print,':: download3DHSTdataFORfitsobj.pro :: Un-zipping downloaded files '
spawn,'gunzip *.gz'


if vb eq 1 then print,' '
if vb eq 1 then print,':: download3DHSTdataFORfitsobj.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
if n_elements(STP) eq 1 then stop
END
