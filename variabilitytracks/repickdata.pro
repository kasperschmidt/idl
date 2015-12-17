;+
;----------------------------
;   NAME
;----------------------------
; repickdata.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Procedure reading an array of data (x,y,dx,dy) and 'repicking' it
; from guassian distributed 'clouds' of a certain width (error)
; centered on the old datapoints
;----------------------------
;   COMMENTS
;----------------------------
;
;----------------------------
;   INPUTS:
;----------------------------
; datarray        : Array containing data to 're-pick'. Array should be on the form
;                      DATARRAY = [[X(N)],[Y(N)],[dX(N)],[dY(N)]
;                   where N is the number of data points
; Derr            : the change of the errors (size of gaussian clouds, to pick
;                   new data from. Given as a percentage of existing errors
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE        : set /VERBOSE to get info/messages printed to the screen 
; /PLOT           : set /PLOT to illustrate/show the new data in a simple scatter plot 
;----------------------------
;   OUTPUTS:
;----------------------------
; datarrayOUT     : Array containing 're-picked' data on the form
;                      DATARRAY = [[Xnew(N)],[Ynew(N)],[dXnew(N)],[dYnew(N)]
;                   where N is the number of data points        
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> datarray=[[201,244,47,287,203,58,210,202,198,158,165,201,157,131,166,160,186,125,218,146],[592,401,583,402,495,173,479,504,510,416,393,442,317,311,400,337,423,334,533,344],[9,4,11,7,5,9,4,4,11,7,5,5,5,6,6,5,9,8,6,5],[61,25,38,15,21,15,27,14,30,16,14,25,52,16,34,31,42,26,16,22.]] & repickdata,datarray,3.0,datarrayOUT,/VERBOSE,/PLOT

; IDL> repickdata,datarray,3.0,datarrayOUT,/VERBOSE
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-01-05  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
PRO repickdata,datarray,Derr,datarrayOUT,VERBOSE=VERBOSE,PLOT=PLOT
profiler,/system & profiler ; profiler so statistics can be called with profiler,/report after execution

VB = n_elements(VERBOSE)
PL = n_elements(PLOT)

DATARRAYOUT = DATARRAY * 0.0       ; creating output array with same size as DATARRAY

Ndat = n_elements(DATARRAY(*,0))

; X-component
DATARRAYOUT(*,2)       = DATARRAY(*,2)*Derr                 ; sigma of gaussian distribution (new errorbar)
MEANX                  = DATARRAY(*,0)                      ; mean of gaussian distribution
GAUSSX                 = RANDOMN(seed,Ndat)                 ; random gaussian point drawn from [0,1] distribution
DATARRAYOUT(*,0)       = GAUSSX * DATARRAYOUT(*,2) + MEANX  ; new x component
; Y-component
DATARRAYOUT(*,3)       = DATARRAY(*,3)*Derr                 ; sigma of gaussian distribution (new errorbar)
MEANY                  = DATARRAY(*,1)                      ; mean of gaussian distribution
GAUSSY                 = RANDOMN(seed,Ndat)                 ; random gaussian point drawn from [0,1] distribution
DATARRAYOUT(*,1)       = GAUSSY * DATARRAYOUT(*,3) + MEANY  ; new y component

if PL eq 1 then begin
   col=getcolor(/load)
   ; determining plot range
   Xmin = min([min(datarray(*,0)),min(datarrayOUT(*,0))])
   Xmax = max([max(datarray(*,0)),max(datarrayOUT(*,0))])
   Ymin = min([min(datarray(*,1)),min(datarrayOUT(*,1))])
   Ymax = max([max(datarray(*,1)),max(datarrayOUT(*,1))])
   ; plotting original data
   plot,datarray(*,0),datarray(*,1),psym=4,xrange=[Xmin,Xmax],yrange=[Ymin,Ymax]
   oploterror,datarray(*,0),datarray(*,1),datarrayOUT(*,2),datarrayOUT(*,3),psym=3,errstyle=2  
   oploterror,datarray(*,0),datarray(*,1),datarray(*,2),datarray(*,3),psym=3,errthick=3
   ; plotting re-picked data
   oplot,datarrayOUT(*,0),datarrayOUT(*,1),psym=2,col=col.red   
   oploterror,datarrayOUT(*,0),datarrayOUT(*,1),datarrayOUT(*,2),datarrayOUT(*,3),psym=3,col=col.red,errcolor=col.red,errthick=3
endif



if vb eq 1 then print,' '
if vb eq 1 then print,':: repickdata.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
;stop
END
