;+
;----------------------------
;   NAME
;----------------------------
; priorityID.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Creating a priority IDs for a set of objects. The ids are created
; by turning values into strings and combining them to an IDstring
; which can afterwards be sorted to give a prioritized list of the 
; objects. The largest ID is given to the object with highest
; priority, i.e. max(ID) is the top priority
;----------------------------
;   COMMENTS
;----------------------------
; The ID will have length of TOTAL(DATACHAR(0,*)) characters which
; can be problematic to handle (unless used as a string) for Nprop large.
;----------------------------
;   INPUTS:
;----------------------------
; VALARR           : Integer array containing the characteristica for the objects
;                    whcih will be used used to prioritize them by.
;                    The array should be on the following form with 0 beeing
;                    the most important and N beeing the least important
;                    property of the object, i.e. 0 will be first in the ID
;                    and N will be last (Nobj = the number of objects to sort)
;                          VALARR(Nobj,0)    e.g. redshift
;                          VALARR(Nobj,1)    e.g. magnitude
;                          .
;                          .
;                          .
;                          VALARR(Nobj,N)    e.g. color
; DATACHAR          : Array of dimension (2,Nprop) indicatating the size of string 
;                     to make from value and whether it should be sorted in a ascending
;                     or descending manor, i.e.:
;                          DATACHAR(Nprop,0)    size of string
;                          DATACHAR(Nprop,1)    0 for descending  (large = best)
;                                               1 for ascending   (small = best)
;                     in the case of ascending sorting max(property)-property is used
;                     such that large ID always means high priority
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; WRITEFITS         : Set WRITEFITS to object IDs and a fits file with the VALARR,
;                     object IDs and the created priority ID will be created.
;                     NB! the structure is created to fit the sorting in 
;                         createHTMLcandinspect.pro
; /VERBOSE          : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; ID                : Vector containing the priority IDs of the Nobj input objects
;                     Large ID = High priority
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; array with 3 properties for 10 objects
; IDL> VALARRin=[[1,2,3,4,2,2,2,2,2,2],[18,20,22,25,26,22.2345253,8.999999,17.6,18.9,22.23493],[999,99,9,9,9,99,999,99,999,9]]
; vector with data description
; IDL> DATACHARin = [[1,5,3],[0,1,0]]
; creating IDs
; IDL> priorityID,VALARRin,DATACHARin,ID,/VERBOSE
;
;----------------------------
;   BUGS
;----------------------------
; Can't deal with properties containing both positive and negative 
; values. This can be fixed easily to add the minimum value of
; the property to the property for all objects.
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-07  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
@ NUMBER_FORMATTER.pro  ; by David W. Fanning
;----------------------------
;-
PRO priorityID,VALARRin,DATACHARin,ID,WRITEFITS=WRITEFITS,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

WF = n_elements(WRITEFITS)
VB = n_elements(VERBOSE)

; defining arrays for calculations not to return modified input arrays
VALARR   = VALARRin
DATACHAR = DATACHARin

Sarr = size(VALARR)  ; size of array

if Sarr(0) ne 2 then begin 
   print,':: priorityID.pro :: ERROR Input array has wrong dimensions, it should have the form (Nproperties,Nobjects) '
   print,'                                 --> Aborting'
   stop
endif

Nobj  = Sarr(1)   ; the number of objects to sort
Nprop = sarr(2)   ; the number of properties given per object
if vb eq 1 then print,':: priorityID.pro :: ',strtrim(Nprop,2),' properties for ',strtrim(Nobj,1),' objects found in input array'

sizeID = TOTAL(DATACHAR(*,0)) ; the number of characters in the IDs
if vb eq 1 then print,':: priorityID.pro :: Each ID will contain ',strtrim(sizeID,2),' characters'

ID    = strarr(Nobj)

; creating new values for ascending rpoperties (small best)
ascent = where(DATACHAR(*,1) eq 1,Nasc) ; properties with small best
for ff=0,Nasc-1 do begin  ; looping over selected properties
   VALARR(*,ascent(ff)) = max(VALARR(*,ascent(ff))) - VALARR(*,ascent(ff))  ; subtracting values from max value
endfor


for ii=0,Nobj-1 do begin                                          ; looping over the objects

   IDobj = strarr(1)                                              ; resetting IDobj

   for jj=0,Nprop-1 do begin

      SP = STRPOS(VALARR(ii,jj),'.')                              ; search for dot position if any decimals
      if SP ne -1 then begin
         Ndec = DATACHAR(jj,0) - STRLEN(number_formatter( ROUND(max(VALARR(*,jj)))))  ; the number of decimals
         if Ndec lt 0 then begin
            print,':: priorityID.pro :: ERROR: The string size is smaller than Ncharacters for property number ',strtrim(jj,2)
            print,'                                 --> Aborting'
            stop
         endif
         propjj = number_formatter(VALARR(ii,jj), Decimals=Ndec)  ; tunring number into string with Ndec decimals
         propjj = strjoin(strsplit(propjj,'.',/extract))          ; removing dot and join to string
      endif else begin
         propjj = number_formatter(VALARR(ii,jj))                 ; turning number into string
      endelse

      length = strlen(propjj)                                     ; length of property string
      N0s    = DATACHAR(jj,0) - length                            ; zeros to add in front of string

      if strlen(IDobj) eq 0 then begin                            ; IDobj for first property
         if N0s gt 0 then begin
            zeroes = strjoin(strarr(N0s)+'0'); 0s to add in front in front to keep track of size (10 is better than 1)
            IDobj  = zeroes+propjj                                ; saving string for properti in IDobj
         endif else begin
            IDobj  = propjj                                       ; saving string for properti in IDobj
         endelse
      endif else begin                                            ; IDobj for the remaining properties
         if N0s gt 0 then begin
            zeroes = strjoin(strarr(N0s)+'0')  ; 0s to add in front in front to keep track of size (10 is better than 1)
            IDobj  = IDobj+zeroes+propjj                          ; appending IDobj to existing IDobj
         endif else begin
            IDobj  = IDobj+propjj                                 ; appending IDobj to existing IDobj
         endelse
      endelse
   endfor

   ID(ii) = IDobj                                                 ; the final ID for object ii to return
endfor

if WF ne 0 then begin   ; writing VALARR to fits file
   spawn,'pwd',workdir
   fitsname = workdir+'/priorityIDoutput.fits'
   str = {objid:'',zspecGT1:0.0,UKIDSS_Hmag:0.0d,chi2ratio:0.0d,zphot:0.0d,galVSstar:0.0d,priorityID:''}
   str = replicate(str,Nobj)

   str.objid       = WRITEFITS(*)
   str.zspecGT1    = VALARRin(*,0)
   str.UKIDSS_Hmag = VALARRin(*,1)
   str.chi2ratio   = VALARRin(*,2)
   str.zphot       = VALARRin(*,3)
   str.galVSstar   = VALARRin(*,4)
   str.galVSstar   = VALARRin(*,4)
   str.priorityID  = ID(*)

   mwrfits, str, fitsname, /create
   help,str,/struc
   if VB eq 1 then print,':: priorityID.pro :: Wrote data and pririty IDs to ',fitsname
endif


if vb eq 1 then print,' '
if vb eq 1 then print,':: priorityID.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
