;+
;----------------------------
;   NAME
;----------------------------
; matchQSOwithShenetal.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure mathing a list of (QSO) ra and decs with the catalog of
; SDSS DR7 quasar properties from Shen et al 2010
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; IDs             : Vector containing IDs/Names of objects to match
; RAin            : Vector containing the RAs of the QSOs to match
; Decin           : Vector containing the Decs of the QSOs to match
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; RMATCH          : set RMATCH to the radius (arc minutes) to match RA and 
;                   Decs within. Default is 0.5 arc minute = 0.008333 degrees
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; matches         : Name of file containing the matches.
;                   The format of the file is:
;                   column 1: ID input
;                   column 2: RA input
;                   column 3: Dec input
;                   column 4: SDSS name
;                   column 5: RA Shen catalog
;                   column 6: Dec Shen catalog
;                   column 7: The total number of matches for that object (ID)
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> matchQSOwithShenetal,IDs,RAin,Decin,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-07-07  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO matchQSOwithShenetal,IDs,RAin,Decin,RMATCH=RMATCH,VERBOSE=VERBOSE

RM = n_elements(RMATCH)
VB = n_elements(VERBOSE)

date   = systime(/UTC)
dateus = STRJOIN(STRSPLIT(date, /EXTRACT), '_')    ; replace spaces wiht under scores

if vb eq 1 then print,' '
if vb eq 1 then print,':: matchQSOwithShenetal.pro :: -- START OF PROGRAM -- '
if vb eq 1 then print,'                              ',date
if vb eq 1 then print,' '

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;reading Shen et al 2010 catalog and defining quantities
Shencat = '/Users/kasperborelloschmidt/work/casjobs_SDSS/variabilitytracks/dr7_bh_June_2010.fits'
Sshen = mrdfits(Shencat,1)
RAshen  = Sshen.RA
Decshen = Sshen.Dec
SDSSname = Sshen.SDSS_NAME

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; preparing output file
if RM eq 0 then RMATCH = 0.008333   ; default match radius in degrees set if none given
FMT='(A,2f20.7,A25,2f20.7,I)'       ;format for output (string,2 floats on 20 digits w. 7 decimals)
openw,55,'mathcQSOs_'+dateus+'.dat', WIDTH=600   ; open output file
; == writing header ==
printf,55,'# Output from matchQSOwithShenetal.pro run on ',strtrim(date,2)
printf,55,'# using a match radius of ',strtrim(Rmatch,2),' degrees'
printf,55,'# The columns of the file correspond to:'
printf,55,'#   ID       RA       Dec       SDSSname       RA match       Dec match       No of matches'

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
;matching objects
for ii=0L,n_elements(RAin)-1 do begin      ; loop over RA and Decs to match (input)
   dist = fltarr(n_elements(RAshen))*0.0   ; defining vector of 0s to contain distances

   for jj=0L,n_elements(RAshen)-1 do begin  ; loop over objects in Shen et al 2010s catalog
      dist(jj) = sqrt( (RAin(ii)-RAshen(jj))^2 + (Decin(ii)-Decshen(jj))^2 )  ; calc distance to Shen objects 
   endfor

   Ment = where( dist lt RMATCH , count)   ; matched entry(ies) for object ii

   CASE count OF
     0:    begin
              if vb eq 1 then print,':: matchQSOwithShenetal.pro :: Found no match for object ',strtrim(IDs(ii),2)
              printf,FORMAT=FMT,55,IDs(ii),RAin(ii),Decin(ii),0.0,0.0,0.0,count
           end
     1:    begin
              if vb eq 1 then print,':: matchQSOwithShenetal.pro :: Found a unique match for object ',strtrim(IDs(ii),2)
              printf,FORMAT=FMT,55,IDs(ii),RAin(ii),Decin(ii),SDSSname(Ment),RAshen(Ment),Decshen(Ment),count
           end
     else: begin
              if vb eq 1 then print,':: matchQSOwithShenetal.pro :: Found ',strtrim(count,2),' matches for object ',strtrim(IDs(ii),2)
              for kk=0,count-1 do begin
                 printf,FORMAT=FMT,55,IDs(ii),RAin(ii),Decin(ii),SDSSname(Ment(kk)),RAshen(Ment(kk)),Decshen(Ment(kk)),count
              endfor
           end
   ENDCASE
endfor
close,55
;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
; printing conclusive numbers:

;= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
if vb eq 1 then print,' '
if vb eq 1 then print,':: matchQSOwithShenetal.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
