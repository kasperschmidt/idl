;+
;----------------------------
;   NAME
;----------------------------
; create_sesarRRLfits.pro
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
; IDL> create_sesarRRLfits,'/disk3/kschmidt/structurefunctioncalcs/RRL_sesar09/table1/filenames.txt','/disk3/kschmidt/structurefunctioncalcs/RRL_sesar09/sesar09RRL.fits',/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2009-10-26  started by K. B. Schmidt (MPIA)
;
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
; input/output:              i        i        opt. i
PRO create_sesarRRLfits,filenames,fitsname,VERBOSE=VERBOSE

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

FMT2 = 'D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D'
for l = 0,Nfiles-1 do begin
   readcol,path+fname(l),FORMAT=FMT2,ra,dec,umjd,upsf,uerr,gmjd,gpsf,gerr,rmjd,rpsf,rerr,imjd,ipsf,ierr,zmjd,zpsf,zerr
   ;counting number of lines to put in structure
   Nlines = Nlines+n_elements(ra)
endfor

;creating structure to write to fitsfile
   str={objid:long64(1),headobjid:long64(1),z:0.0d,ra:0.0d,dec:0.0d, mjd_u:0.0d,mjd_g:0.0d,mjd_r:0.0d,mjd_i:0.0d,mjd_z:0.0d, psfMag_u:0.0d,psfMag_g:0.0d,psfMag_r:0.0d,psfMag_i:0.0d,psfMag_z:0.0d, psfMagerr_u:0.0d,psfMagerr_g:0.0d,psfMagerr_r:0.0d,psfMagerr_i:0.0d,psfMagerr_z:0.0d, psfSigma1_u:0.0d,psfSigma1_g:0.0d,psfSigma1_r:0.0d,psfSigma1_i:0.0d,psfSigma1_z:0.0d}

str = replicate(str,Nlines)


for i = 0,Nfiles-1 do begin
   readcol,path+fname(i),FORMAT=FMT2,ra,dec,umjd,upsf,uerr,gmjd,gpsf,gerr,rmjd,rpsf,rerr,imjd,ipsf,ierr,zmjd,zpsf,zerr
   IDsplit   = STRSPLIT(fname(i),'.',/EXTRACT)   ; splitting filename
   headobjid = IDsplit(0)                        ; getting the id of the RRL

   for j=0,n_elements(ra)-1 do begin
      ;---filling the structure with data
      ;----------------------------------------------------------------------
      ;unique object IDs on the form hhhhhhhhhjjjj, where h is the headobjid 
      ;and j gives the number of the entry
      if j lt 10 then stringID                 = headobjid+'000'+strtrim(j,1)
      if j ge 10 and j lt 100 then stringID    = headobjid+'00'+strtrim(j,1)
      if j ge 100 and j lt 1000 then stringID  = headobjid+'0'+strtrim(j,1)
      str(enttot+j).objid                      = stringID
      ;----------------------------------------------------------------------
      str(enttot+j).headobjid       = headobjid
      str(enttot+j).z               = -9999.
      str(enttot+j).ra              = ra(j)
      str(enttot+j).dec             = dec(j)
      str(enttot+j).mjd_u           = umjd(j)
      str(enttot+j).mjd_g           = gmjd(j)
      str(enttot+j).mjd_r           = rmjd(j)
      str(enttot+j).mjd_i           = imjd(j)
      str(enttot+j).mjd_z           = zmjd(j)
      str(enttot+j).psfMag_u        = upsf(j)
      str(enttot+j).psfMag_g        = gpsf(j)
      str(enttot+j).psfMag_r        = rpsf(j)
      str(enttot+j).psfMag_i        = ipsf(j)
      str(enttot+j).psfMag_z        = zpsf(j)
      str(enttot+j).psfMagerr_u     = uerr(j)
      str(enttot+j).psfMagerr_g     = gerr(j)
      str(enttot+j).psfMagerr_r     = rerr(j)
      str(enttot+j).psfMagerr_i     = ierr(j)
      str(enttot+j).psfMagerr_z     = zerr(j)
      str(enttot+j).psfSigma1_u     = -9999
      str(enttot+j).psfSigma1_g     = -9999
      str(enttot+j).psfSigma1_r     = -9999
      str(enttot+j).psfSigma1_i     = -9999
      str(enttot+j).psfSigma1_z     = -9999
   endfor
   enttot = enttot+n_elements(ra)
endfor
;---WRITING THE STRUCTURE TO A FITSFILE---
mwrfits, str, fitsname, /create
;---PRINTING EACH ENTRY IN THE STRUCTURE---
help,str,/struc
if VB eq 1 then print,':: create_sesarRRLfits.pro :: Combined the RRL data in the file: ',fitsname
END
