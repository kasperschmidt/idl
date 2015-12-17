;+
;----------------------------
;   NAME
;----------------------------
; chisquareDavismodels.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure calculating the chi2 values (goodness of "fit") for the
; davis models compared to the QSO data in plotDavisVSstripe82VSshen.pro
; the output is written to a IDLsavefile for use/plotting in
; plotDavisVSstripe82VSshen.pro
;
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; QSOcat        : QSO catalog to plot
; QSOfits       : datafile used to estimate the g,r slopes in QSOcat
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; ZBIN            : vector containing the redshift bins of models to run over. The possibilities are:
;                   0.1, 0.3, 0.5 ... 1.3, 1.5 (default), 1.7, ... 5.7 and 5.9
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
; outputfile      : Name of file with the saved output (idl save file
;                   which can be restored with IDL> restore,outputfile
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> chisquareDavismodels,'slopeANDshencatTue_Dec_14_152013_2010_season0.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',outputfile,/VERBOSE,zbin=[0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7,2.9,3.1,3.3,3.5,3.7,3.9,4.1,4.3,4.5,4.7,4.9,5.1,5.3,5.5,5.7,5.9]

; GR run from end of January
;  IDL> chisquareDavismodels,'slopeANDshencatThu_Feb_3_163401_2011_season0.fits','DR5qcat_s82_photo_FieldModelANDDered_sorted_SUB_0z7AND0p00000A2p00000AND0p00000gamma5p00000_Wed_Dec_8_12:54:21_2010.fits',outputfile,/VERBOSE,zbin=[0.1,0.3,0.5,0.7,0.9,1.1,1.3,1.5,1.7,1.9,2.1,2.3,2.5,2.7,2.9,3.1,3.3,3.5,3.7,3.9,4.1,4.3,4.5,4.7,4.9,5.1,5.3,5.5,5.7,5.9]
;
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-19  started by K. B. Schmidt (MPIA)
; 2011-01-27  zbin keyword K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
;--------------------------------------------------------------------------------------------------------------------
FUNCTION CHI2,DATA,DATAERR,MODEL
   ; Function calculating the chi squared value. It returns both the 
   ; chi squared value and the normalized sum of the individual terms 
   ; in the chi2 so one can determine to which 'side' of the model a 
   ; given data set lies.
   fac     = (DATA-MODEL)/DATAERR             ; the differences between data and model normalized with the error
   sum     = total(fac)                       ; the sume over differences 
   sumERR  = total(1/DATAERR)                 ; sum of invers errors for normalization
   sumNORM = sum/sumERR                       ; normalized sum
   c2      = total(fac^2.)                    ; the chi^2 value
   RETURN,[c2,sumNORM]
END
;--------------------------------------------------------------------------------------------------------------------
PRO chisquareDavismodels,QSOcat,QSOfits,outputfile,ZBIN=ZBIN,EPS=EPS,VERBOSE=VERBOSE
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

PS = n_elements(EPS)
VB = n_elements(VERBOSE)

; ---- reading Knecht interpretations of Davis models ---- 
FMT = ('f,f,f,f,d,f,f')  ; columns are: z log(M/Msun) log(L/Ledd) alpha L2200 gr gmag (Mass, alpha and Ls are mean over the 5 different cosi in Davis files)
readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davismodels_allZandM/davis_models_a0.0_to_SDSS.dat',FORMAT=FMT,zbin1_0,logM_M1_0,logL_M1_0,alpha_M1_0,L2200_M1_0,gr_M1_0,gmag_M1_0
readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davismodels_allZandM/davis_models_a0.0_f1_to_SDSS.dat',FORMAT=FMT,zbin2_0,logM_M2_0,logL_M2_0,alpha_M2_0,L2200_M2_0,gr_M2_0,gmag_M2_0
readcol,'/Users/kasperborelloschmidt/work/teaching/matthias/davismodels_allZandM/davis_models_a0.9_to_SDSS.dat',FORMAT=FMT,zbin3_0,logM_M3_0,logL_M3_0,alpha_M3_0,L2200_M3_0,gr_M3_0,gmag_M3_0

; ---- reading QSO data ---- 
Qcat  = mrdfits(QSOcat,1)
Qdata = mrdfits(QSOfits,1)

NQSOs = n_elements(Qcat.headobjid)  ; number of QSOs with shen matches
IDS   = Qdata(uniq(Qdata.headobjid)).headobjid


if n_elements(Zbin) eq 0 then zbin = [1.5]        ; setting default zbin if none chosen

for ll=0,n_elements(zbin)-1 do begin              ; looping over chosen z values

   ent1     = where(Zbin1_0 eq zbin(ll))            ; model entries to use
   ent2     = where(Zbin2_0 eq zbin(ll))            ; model entries to use
   ent3     = where(Zbin3_0 eq zbin(ll))            ; model entries to use
   if vb eq 1 then print,':: chisquareDavismodels.pro :: Model for redshift '+strtrim(zbin(ll),2)+' chosen for comparison.'

   logM_M1  = logM_M1_0(ent1)
   logL_M1  = logL_M1_0(ent1)
   alpha_M1 = alpha_M1_0(ent1)
   L2200_M1 = L2200_M1_0(ent1)
   gr_M1    = gr_M1_0(ent1)
   gmag_M1  = gmag_M1_0(ent1)

   logM_M2  = logM_M2_0(ent2)
   logL_M2  = logL_M2_0(ent2)
   alpha_M2 = alpha_M2_0(ent2)
   L2200_M2 = L2200_M2_0(ent2)
   gr_M2    = gr_M2_0(ent2)
   gmag_M2  = gmag_M2_0(ent2)

   logM_M3  = logM_M3_0(ent3)
   logL_M3  = logL_M3_0(ent3)
   alpha_M3 = alpha_M3_0(ent3)
   L2200_M3 = L2200_M3_0(ent3)
   gr_M3    = gr_M3_0(ent3)
   gmag_M3  = gmag_M3_0(ent3)
   ; ----------------------------------------

   rmag_M1 = gmag_M1-gr_M1    ; r-band magnitudes
   rmag_M2 = gmag_M2-gr_M2    ; r-band magnitudes
   rmag_M3 = gmag_M3-gr_M3    ; r-band magnitudes

   MMent  = uniq(logM_M1)     ; entries of unique masses
   Nmass  = n_elements(MMent) ; number of mass bins
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;                          ESTIMATING CHI2 VALUES MODEL vs. DATA
   C2_gmag  = fltarr(NQSOs,6)
   C2_rmag  = fltarr(NQSOs,6)
   C2_gr    = fltarr(NQSOs,6)
   C2_slope = fltarr(NQSOs,3)
   slopes   = fltarr(NQSOs,3)

   for jj=0,NQSOs-1 do begin   ; looping over objects
   ;for jj=0,5 do begin   ; looping over objects
      Masscheck = 1         ; value checking if mass is known
      Objent    = where( Qdata.headobjid eq IDS(jj), Cpoints )
      QSOmass   = Qcat(jj).LOGBH_MGII_MD04

      if QSOmass eq 0 then Masscheck = 0  ; if mass not estimated then masscheck changed
      Dmass     = abs(logM_M1 - QSOmass)

      if masscheck eq 1 then begin  ; if mass of object is known
         gsortent        = Objent(sort(Qdata(Objent).PSFmag_g))
         rsortent        = Objent(sort(Qdata(Objent).PSFmag_r))
         gdatsort        = Qdata(gsortent).PSFmag_g
         rdatsort        = Qdata(rsortent).PSFmag_r
         gdatERRsort     = Qdata(gsortent).PSFmagERR_g
         rdatERRsort     = Qdata(rsortent).PSFmagERR_r
         grdata          = Qdata(Objent).PSFmag_g-Qdata(Objent).PSFmag_r                         ; calculating g-r for data
         grdataERR       = sqrt(Qdata(Objent).PSFmagERR_g^2+Qdata(Objent).PSFmagERR_r^2)         ; propagating g-r errors
         grsort          = grdata(gsortent)                                                      ; sorting corresponding to g
         grERRsort       = grdataERR(gsortent)                                                   ; sorting corresponding to g

         ; --- model 1 ---
         modelent        = where(Dmass eq min(Dmass))
         M1rint          = SPLINE(gmag_M1(Modelent),rmag_M1(Modelent),gdatsort)                  ; interpolating r model to g data grid
         M1gint          = SPLINE(rmag_M1(Modelent),gmag_M1(Modelent),rdatsort)                  ; interpolating g model to r data grid
         M1grint         = SPLINE(gmag_M1(Modelent),gr_M1(Modelent),gdatsort)                    ; interpolating g-r model to g data grid
         C2_gmag(jj,0:1) = CHI2(gdatsort,gdatERRsort,M1gint)                                     ; calculating chi2
         C2_rmag(jj,0:1) = CHI2(rdatsort,rdatERRsort,M1rint)                                     ; calculating chi2
         C2_gr(jj,0:1)   = CHI2(grsort,grERRsort,M1grint)                                        ; calculating chi2
         slopes(jj,0)    = (M1rint(0)-M1rint(Cpoints-1))/(gdatsort(0)-gdatsort(Cpoints-1))       ; estimating slope
         C2_slope(jj,0)  = Qcat(jj).Abest-slopes(jj,0)                                           ; the difference in slopes
         ; --- model 2 ---
         modelent        = where(Dmass eq min(Dmass))
         M2rint          = SPLINE(gmag_M2(Modelent),rmag_M2(Modelent),gdatsort)                  ; interpolating r model to g data grid
         M2gint          = SPLINE(rmag_M2(Modelent),gmag_M2(Modelent),rdatsort)                  ; interpolating g model to r data grid
         M2grint         = SPLINE(gmag_M2(Modelent),gr_M2(Modelent),gdatsort)                    ; interpolating g-r model to g data grid
         C2_gmag(jj,2:3) = CHI2(gdatsort,gdatERRsort,M2gint)                                     ; calculating chi2
         C2_rmag(jj,2:3) = CHI2(rdatsort,rdatERRsort,M2rint)                                     ; calculating chi2
         C2_gr(jj,2:3)   = CHI2(grsort,grERRsort,M2grint)                                        ; calculating chi2
         slopes(jj,1)    = (M2rint(0)-M2rint(Cpoints-1))/(gdatsort(0)-gdatsort(Cpoints-1))       ; estimating slope
         C2_slope(jj,1)  = Qcat(jj).Abest-slopes(jj,1)   
         ; --- model 3 ---
         modelent        = where(Dmass eq min(Dmass))
         M3rint          = SPLINE(gmag_M3(Modelent),rmag_M3(Modelent),gdatsort)                  ; interpolating r model to g data grid
         M3gint          = SPLINE(rmag_M3(Modelent),gmag_M3(Modelent),rdatsort)                  ; interpolating g model to r data grid
         M3grint         = SPLINE(gmag_M3(Modelent),gr_M3(Modelent),gdatsort)                    ; interpolating g-r model to g data grid
         C2_gmag(jj,4:5) = CHI2(gdatsort,gdatERRsort,M3gint)                                     ; calculating chi2
         C2_rmag(jj,4:5) = CHI2(rdatsort,rdatERRsort,M3rint)                                     ; calculating chi2
         C2_gr(jj,4:5)   = CHI2(grsort,grERRsort,M3grint)                                        ; calculating chi2
         slopes(jj,2)    = (M3rint(0)-M3rint(Cpoints-1))/(gdatsort(0)-gdatsort(Cpoints-1))       ; estimating slope
         C2_slope(jj,2)  = Qcat(jj).Abest-slopes(jj,2)   
      endif else begin              ; if mass of object is not known
         C2_gmag(jj,0:5) = -9999 
         C2_rmag(jj,0:5) = -9999 
         C2_gr(jj,0:5)   = -9999
         C2_slope(jj,0:2)= -9999
         slopes(jj,0:2)  = -9999
   endelse
   endfor
   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   zbinstring = STRJOIN(STRSPLIT(zbin(ll),'.',/extract),'p')
   outputfile = 'chisquareDavismodels'+strtrim(zbinstring,2)+'.idlsave'
   save,C2_gmag,C2_rmag,C2_gr,C2_slope,slopes,filename=outputfile,DESCRIPTION="Variables saved: C2_gmag,C2_rmag,C2_gr,C2_slope,slopes"
   if vb eq 1 then print,':: chisquareDavismodels.pro :: Wrote output to '+strtrim(outputfile,2)
endfor
 

if vb eq 1 then print,' '
if vb eq 1 then print,':: chisquareDavismodels.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
stop
END
