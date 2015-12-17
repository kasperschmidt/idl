;+
;----------------------------
;   NAME
;----------------------------
; create_bovyFGstarsfits.pro
;
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Routine reading in a file with the names of the sesar et al 2009 stripe82 RR Lyrae
; reading the data from each file and converting it to a fits file which has the right
; format for beeing fed to the variability algorithm
;----------------------------
;   COMMENTS
;----------------------------
; 
;----------------------------
;   INPUTS:
;----------------------------
; filenames       : string containing the path of the file with the names of the 
;                   RR Lyrae datafiles (should be in the same directory as the files)
; fitsname        : String containing the name of the fits file to be created
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> create_bovyFGstarsfits,'/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/star/starfiles.txt','/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/bovyFGstars.fits',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-06-08  started by K. B. Schmidt (MPIA) - based on ~/idl711mac/itt/idl71/lib/myprogs/create_sesarRRLfits.pro
;
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO create_bovyFGstarsfits,filenames,fitsname,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)
Nlines = 0.0    ;resetting counter
enttot = 0.0    ;resetting counter

FMT = 'A'
readcol,filenames,FORMAT=FMT,fname            ; reading the filenames
Nfiles = n_elements(fname)                    ; counting the number of files

pathsplit = STRSPLIT(filenames,'/',/EXTRACT)   ; seperating path
Npath = n_elements(pathsplit)                  ; counting 'steps' in path
; putting path back together (without name of file)
path = '/'+pathsplit(0)+'/'
for k=1,Npath-2 do begin
   path = path+pathsplit(k)+'/'
endfor


Epochmin = 10  ; minimum number of epochs

for l = 0,Nfiles-1 do begin                 ; countin number of lines to put in output table
   FGSl = mrdfits(path+fname(l),1,/silent)  ; reading data for l'th FGstar
   coaddENT = where(FGSl.mjd_u ne 0)        ; getting entries of real epochs (no coadds)
   FGSl = FGSl(coaddent)                    ; only selecting epochs which are real (no coadds)

   if n_elements(FGSl.ra) ge Epochmin then Nlines = Nlines+n_elements(FGSl.ra)
endfor

;creating structure to write to fitsfile
   str={objid:long64(1),headobjid:long64(1),z:0.0d,ra:0.0d,dec:0.0d, mjd_u:0.0d,mjd_g:0.0d,mjd_r:0.0d,mjd_i:0.0d,mjd_z:0.0d, psfMag_u:0.0d,psfMag_g:0.0d,psfMag_r:0.0d,psfMag_i:0.0d,psfMag_z:0.0d, psfMagerr_u:0.0d,psfMagerr_g:0.0d,psfMagerr_r:0.0d,psfMagerr_i:0.0d,psfMagerr_z:0.0d, psfSigma1_u:0.0d,psfSigma1_g:0.0d,psfSigma1_r:0.0d,psfSigma1_i:0.0d,psfSigma1_z:0.0d}

str = replicate(str,Nlines)


for i = 0,Nfiles-1 do begin
   FGSl = mrdfits(path+fname(i),1,/silent)        ; reading data for l'th FGstar
   coaddENT = where(FGSl.mjd_u ne 0)        ; getting entries of real epochs (no coadds)
   FGSl = FGSl(coaddent)                    ; only selecting epochs which are real (no coadds)

   if n_elements(FGSl.ra) ge Epochmin then begin  ; only adding object to structure if Nepochs is greater than or equal Epochmin 
      headobjid = strtrim(i+1,2)                  ; assigning object number as headobjid

      for j=0,n_elements(FGSl.ra)-1 do begin
         ;---filling the structure with data
         ;----------------------------------------------------------------------
         ;unique object IDs on the form hhhhhhhhhjjjj, where h is the headobjid 
         ;and j gives the number of the entry
         if j lt 10 then stringID                 = headobjid+'000'+strtrim(j,2)
         if j ge 10 and j lt 100 then stringID    = headobjid+'00'+strtrim(j,2)
         if j ge 100 and j lt 1000 then stringID  = headobjid+'0'+strtrim(j,2)
         str(enttot+j).objid                      = stringID
         ;----------------------------------------------------------------------
         str(enttot+j).headobjid       = headobjid
         str(enttot+j).z               = -9999.
         str(enttot+j).ra              = FGSl(j).ra
         str(enttot+j).dec             = FGSl(j).dec
         str(enttot+j).mjd_u           = FGSl(j).mjd_u
         str(enttot+j).mjd_g           = FGSl(j).mjd_g
         str(enttot+j).mjd_r           = FGSl(j).mjd_r
         str(enttot+j).mjd_i           = FGSl(j).mjd_i
         str(enttot+j).mjd_z           = FGSl(j).mjd_z
         str(enttot+j).psfMag_u        = FGSl(j).u
         str(enttot+j).psfMag_g        = FGSl(j).g
         str(enttot+j).psfMag_r        = FGSl(j).r
         str(enttot+j).psfMag_i        = FGSl(j).i
         str(enttot+j).psfMag_z        = FGSl(j).z
         str(enttot+j).psfMagerr_u     = FGSl(j).err_u
         str(enttot+j).psfMagerr_g     = FGSl(j).err_g
         str(enttot+j).psfMagerr_r     = FGSl(j).err_r
         str(enttot+j).psfMagerr_i     = FGSl(j).err_i
         str(enttot+j).psfMagerr_z     = FGSl(j).err_z
         str(enttot+j).psfSigma1_u     = -9999
         str(enttot+j).psfSigma1_g     = -9999
         str(enttot+j).psfSigma1_r     = -9999
         str(enttot+j).psfSigma1_i     = -9999
         str(enttot+j).psfSigma1_z     = -9999
      endfor
      enttot = enttot+n_elements(FGSl.ra)
   endif
endfor
;---WRITING THE STRUCTURE TO A FITSFILE---
mwrfits, str, fitsname, /create
;---PRINTING EACH ENTRY IN THE STRUCTURE---
help,str,/struc
if VB eq 1 then print,':: create_bovyFGstarsfits.pro :: Combined the FG stars data in the file: ',fitsname
END
