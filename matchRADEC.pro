;+
;----------------------------
;   NAME
;----------------------------
; matchRADEC.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; This procedure takes a list of RA and DEC and matches them with
; another set of RA and DEC for a given match radius and returns the
; entries of the matches
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; RA1        : RA of first list of objects
; DEC1       : DEC corresponding to RA1
; RA2        : RA of second list of objects to be matched with RA1
; DEC2       : DEC corresponding to DEC2
; MATCHRAD   : radius to match objects within (in the same units as
;              RA1, DEC1, RA2 and DEC2 e.g. degrees)
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE   : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; MATCH      : Array with matches. Each row correspond to a single
;              match and each columns corresponds to:
;                 match(0,*): RA1 entry matched
;                 match(1,*): RA2 entry matched
;                 match(2,*): match radius of match
;              Hence the number of unique matches is given by Nmatch = uniq(match(0,*))
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> bovy = mrdfits('/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/joBstuff/powerlawSF_constmean/powerlawSF_constmean_r.fits',1) & schmidt = mrdfits('/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/slopeANDshencatThu_Feb_3_163401_2011_season0_ERRcorrected.fits',1) & matchRADEC,schmidt.ra,schmidt.dec,bovy.ra,bovy.dec,1*0.0002777778,MATCH,/VERBOSE


; IDL>  matchRADEC,RA1,DEC1,RA2,DEC2,0.0002777778,MATCH,/VERBOSE  ; 1 arcsec = 0.0002777777 deg
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-05-23  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;
;----------------------------
;-
PRO matchRADEC,RA1,DEC1,RA2,DEC2,MATCHRAD,MATCH,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)

MATCH = 0                                                                                    ; resetting MATCH array
N1 = n_elements(RA1)                                                                         ; number of objects in RA1 vector

for ii=0,N1-1 do begin                                                                       ; looping over objects to match
   dist     = sqrt((RA2-RA1(ii))^2.+(DEC2-DEC1(ii))^2.)                                      ; distance to objects in RA2 and DEC2
   entmatch = where(dist lt matchrad,Cmatch)

   for jj=0,Cmatch-1 do begin                                                                ; looping over found matches
      if n_elements(MATCH) gt 1 then MATCH = [[MATCH],[ii,entmatch(jj),dist(entmatch(jj))]]  ; appending matches
      if n_elements(MATCH) eq 1 then MATCH = [ii,entmatch(jj),dist(entmatch(jj))]            ; creating MATCHES array
   endfor
if ii eq 3 and Cmatch gt 1 then stop
endfor 

Nmatch       = n_elements(MATCH(0,*))                                                        ; counting all matches
Nmatchunique = n_elements(uniq(MATCH(0,*)))                                                  ; counting unque matches

if vb eq 1 then print,':: matchRADEC.pro :: Found ',trim(Nmatch),' matches '
if vb eq 1 then print,':: matchRADEC.pro :: Found ',trim(Nmatchunique),' unique matches '

if vb eq 1 then print,' '
if vb eq 1 then print,':: matchRADEC.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
