;+
;----------------------------
;   NAME
;----------------------------
; cropfits2fitANDjpg.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Creating fit and jpg postage stams (cut-out) from large fits files by specifying
; a pixel region around ra and dec values.
;----------------------------
;   COMMENTS
;----------------------------
; If the selected cropping area is outside fits image the code stops
; and waits for manual input of the reagion to crop
;----------------------------
;   INPUTS:
;----------------------------
; URLs             : string containing the name (and pathe) of the file with
;                    the URLs, field, filedID, S82objID, ra, dec and DR7objID
; fitsdir          : path to directory where the fits to be cropped are (also 
;                    the directory where the postage stamps will be placed in
;                    subdirectories called jpgstamps and fitstamps respectively)
; pixrad           : the pixel radius to be cut our around the ra dec position
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /NOJPG          : set /NOJPG to disable the creation of the JPG postage stamps
; /NOFIT          : set /NOFIT to disable the creation of the FIT postage stamps
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> cropfits2fitANDjpg,'/disk3/kschmidt/structurefunctioncalcs/extended_lenssearch/ext_17i21_UVX_030RA060_candidates_epochURLsTEST_kschmidt.csv','/disk3/kschmidt/structurefunctioncalcs/extended_lenssearch/singleepochsTEST/',20,/VERBOSE
;
; 460 lens candidates
; IDL> cropfits2fitANDjpg,'/Users/kasperborelloschmidt/work/casjobs_SDSS/lenscandidatesearch/topXX/S82bestseeingfits/bestseeing460cand_uband_fitsurl_kschmidt.csv','/Users/kasperborelloschmidt/work/casjobs_SDSS/lenscandidatesearch/topXX/S82bestseeingfits/',37,/VERBOSE,/NOJPG
;
; 348 spec QSOs
; IDL> cropfits2fitANDjpg,'/Users/kasperborelloschmidt/work/casjobs_SDSS/lenscandidatesearch/plotSeeingAndTypeStatSelected/S82bestseeingfits/bestseeing_348specQSOs_uband_fitsurl.csv','/Users/kasperborelloschmidt/work/casjobs_SDSS/lenscandidatesearch/plotSeeingAndTypeStatSelected/S82bestseeingfits/',37,/VERBOSE,/NOJPG
;----------------------------
;   BUGS
;----------------------------
; If an image cannot be found and error will be printed, but the image
; will be created with the fits image prior to it on the input
; list. The images not found has to be deleted by hand.
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-01-26  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ namedate.pro
;----------------------------
;-
; input/output:          i      i      i     opt. i ...
PRO cropfits2fitANDjpg,URLs,fitsdir,pixrad,NOJPG=NOJPG,NOFIT=NOFIT,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)
NJ = n_elements(NOJPG)
NF = n_elements(NOFIT)

FMT = 'A,I,O,O,D,D,O'
READCOL,FORMAT=FMT,URLs,URL,field,fieldID,epochID,ra,dec,headID

ent  = uniq(headID)       ; entries of unique object ids
Nobj = n_elements(ent)    ; getting the number of unique objects in file

spix = pixrad*2+1         ; # pixel on a side of the postage stamp

; making jpgstamps and fitstamps directories
fitstampdir = fitsdir+'fitstamps' 
jpgstampdir = fitsdir+'jpgstamps'
; checking if the directories already excists
spawn, 'ls '+fitstampdir, ls_fit,err_fit
spawn, 'ls '+jpgstampdir, ls_jpg,err_jpg
; creating directories if they don't excist
if size(err_fit,/dimensions) eq 1 and NF eq 0 then begin
   spawn, 'mkdir '+fitstampdir
   if vb eq 1 then print,':: cropfits2fitANDjpg.pro :: Created the non-excisting output directory:'
   if vb eq 1 then print,'                             ',fitstampdir
endif
if size(err_jpg,/dimensions) eq 1 and NJ eq 0 then begin
   spawn, 'mkdir '+jpgstampdir
   if vb eq 1 then print,':: cropfits2fitANDjpg.pro :: Created the non-excisting output directory:'
   if vb eq 1 then print,'                             ',jpgstampdir
endif

for i=0,Nobj-1 do begin                                        ; looping over objects
   epochURL = URL(where(headID eq headID(ent(i)),cepochs))     ; URL for epochs
   epochra  = ra(where(headID eq headID(ent(i)),cepochs))      ; ra for epochs
   epochdec = dec(where(headID eq headID(ent(i)),cepochs))     ; dec for epochs
   epID     = epochID(where(headID eq headID(ent(i)),cepochs)) ; ID for epochs
   if vb eq 1 then print,':: cropfits2fitANDjpg.pro :: Object ',strtrim(headID(ent(i)),1)
   if vb eq 1 then print,'                             has    ',strtrim(n_elements(epID),1),' epoch(s)'

      for j=0,n_elements(epID)-1 do begin                      ; looping over epochs of the i'th object
         namedate,epochURL(j),path,name,extension,date,dateus  ; extracting name and extension of image
         fitsimg = name+'.'+extension                          ; creating image name
         fitsdata = MRDFITS(fitsdir+fitsimg,0,HDR,/silent)     ; reading fits image and its header
         adxy, HDR,epochra(j),epochdec(j),x,y                  ; turning ra and dec into pixel positions (changed from i-index to j on 101008)
         ; extracting the subimage in a specified pixel region
         hextract, fitsdata, hdr, fit_new, hdr_new, x-pixrad, x+pixrad, y-pixrad, y+pixrad,/SILENT

         ; creating name for postage stamps
         stampname = 'DR7_'+strtrim(headID(ent(i)),1)+'_stamp_'+strtrim(spix,1)+'x'+strtrim(spix,1)+'pix_no'+strtrim(j,1)+'_s82epoch_'+strtrim(epID(j),1)+'_'+name

         ; writing fits postage stamp
         if NF eq 0 then mwrfits,fit_new,fitstampdir+'/'+stampname+'.fit',hdr_new
         ; writing jpg postage stamp
         if NJ eq 0 then write_jpeg,jpgstampdir+'/'+stampname+'.jpg',fit_new,QUALITY=100
      endfor

endfor

if vb eq 1 then print,' '
if vb eq 1 then print,':: cropfits2fitANDjpg.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
