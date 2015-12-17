;+
;----------------------------
;   NAME
;----------------------------
; plotfitsLCwuick.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; small code to quickly plot some lightcurves etc
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; fitsfile           : name of fitsfile to plot
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
;
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> plotfitsLCquick,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted_outliers9999.fit'
; IDL> plotfitsLCquick,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/stars/stripe82calibStars_v2p6_top5k_wFieldinfo_wModelMag_sorted.fit'
; IDL> plotfitsLCquick,'/Users/kasperborelloschmidt/work/casjobs_SDSS/fits/QSOs/DR5qcat_s82_photo_FieldModelANDDered_sorted_outliers9999.fit'
; IDL> plotfitsLCquick,'/Users/kasperborelloschmidt/work/casjobs_SDSS/RRL_sesar_etal2009/sesar09RRL_sorted.fits'
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-08-18  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO plotfitsLCquick,fitsfile

s = mrdfits(fitsfile,1)
HEADID = s(uniq(s.headobjid)).headobjid  

!p.multi = [0,2,2]
for ii=0,n_elements(HEADID) do begin
   ent1 = where(s.headobjid eq headid(ii))
   ent  = ent1(sort(s(ent1).MJD_g))
   plot,findgen(n_elements(ent)),s(ent).PSFMAG_g,psym=2,YRANGE=[22,18],xrange=[-1,120],ytitle='mag'

   oplot,findgen(n_elements(ent)),s(ent).PSFMAG_r,psym=1

   plot,s(ent).PSFMAG_g,s(ent).PSFMAG_r,XRANGE=[22,18],YRANGE=[22,18],psym=2,xtitle='g',ytitle='r'

   plot,s(ent).PSFMAG_g-s(ent).PSFMAG_r,s(ent).PSFMAG_g,psym=2,YRANGE=[22,18],xrange=[-0.4,1],xtitle='g-r',ytitle='g'

   plot,s(ent).PSFMAG_g-s(ent).PSFMAG_r,s(ent).PSFMAG_g,psym=2,YRANGE=[22,18],xrange=[-0.4,1],xtitle='g-r',ytitle='g'

   wait,1.0 
endfor
!p.multi = [0,0,0]

if vb eq 1 then print,' '
if vb eq 1 then print,':: plotfitsLCwuick.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
