;+
;----------------------------
;   NAME
;----------------------------
; fct_2Dgauss.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Function returning a (rotated) 2D gaussian
; Can be used as input for mpfit2Dfun.pro
;----------------------------
;   COMMENTS
;----------------------------
; It is the 2D gaussian described here:
; http://en.wikipedia.org/wiki/Gaussian_function#Two-dimensional_Gaussian_function
; which has been coded up
;----------------------------
;   INPUTS:
;----------------------------
; X        : Array of x-values on the form (Nx X Ny):
;               | 1  2  3  4  5 |      ; X positions of all pixels
;               | 1  2  3  4  5 |
;               | 1  2  3  4  5 |
;           This can be obtained by 
;               X = XR # (YC*0 + 1)
;            Where XR and YC are the x (row) and y (col) _vectors_ of the array
; Y        : Array of y-values on the form (Nx X Ny):
;               | 15  15  15  15 |      ; Y positions of all pixels
;               | 16  16  16  16 |
;               | 17  17  17  17 |
;           This can be obtained by 
;               Y = (XR*0 + 1) # YC
;            Where XR and YC are the x (row) and y (col) _vectors_ of the array
; P        : The parameters for the gaussian. The content of the
;            6-component vector is:
;              P[0] = Amplitude/peak value
;              P[1] = center in x-direction
;              P[2] = center in y-direction
;              P[3] = Standard deviation in x-direction
;              P[4] = Standard deviation in y-direction
;              P[5] = Clock-wise rotation in radians; angle with y-axis
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; /VERBOSE : set /VERBOSE to get info/messages printed to the screen
;----------------------------
;   OUTPUTS:
;----------------------------
;
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
;
; IDL> XR      = findgen(30)-15
; IDL> YC      = findgen(30)-15
; IDL> X       = XR # (YC*0 + 1)
; IDL> Y       = (XR*0 + 1) # YC
; IDL> P       = [2,0,5,2,4,!pi/4]
; IDL> Gauss2D = FCT_2Dgauss(X,Y,P,/verbose) 
; IDL> Surface,Gauss2D,charsize=2,zrange=[0,P[0]*2]
;
; all-in-one
; IDL> XR=findgen(30)-15 & YC=findgen(30)-15 & X=XR#(YC*0+1) & Y=(XR*0+1)#YC & P=[2,0,5,2,4,!pi/8] & Gauss2D=FCT_2Dgauss(X,Y,P,/verbose) & Surface,Gauss2D,charsize=2,zrange=[0,P[0]*2]
;
; all-in-one rotating
;ang=[0,1*!pi/10,2*!pi/10,3*!pi/10,4*!pi/10,5*!pi/10,6*!pi/10,7*!pi/10,8*!pi/10,9*!pi/10,!pi] & for ii=0,9 do begin & XR=findgen(30)-15 & YC=findgen(30)-15 & X=XR#(YC*0+1) & Y=(XR*0+1)#YC & P=[2,0,5,2,4,ang(ii)] & Gauss2D=FCT_2Dgauss(X,Y,P) & Surface,Gauss2D,charsize=2,zrange=[0,P[0]*2]  & wait,1 & endfor
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2011-06-24  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;@ xxx.pro
;----------------------------
;-
FUNCTION FCT_2Dgauss, X, Y, P,VERBOSE=VERBOSE

VB = n_elements(VERBOSE)

if vb eq 1 then print,':: fct_2Dgauss.pro :: Creating gaussin with the parameters:'
if vb eq 1 then print,'                      P[0] = ',trim(P[0],'(f15.4)'),'   : Amplitude'
if vb eq 1 then print,'                      P[1] = ',trim(P[1],'(f15.4)'),'   : center in x-direction'
if vb eq 1 then print,'                      P[2] = ',trim(P[2],'(f15.4)'),'   : center in y-direction'
if vb eq 1 then print,'                      P[3] = ',trim(P[3],'(f15.4)'),'   : Standard deviation in x-direction'
if vb eq 1 then print,'                      P[4] = ',trim(P[4],'(f15.4)'),'   : Standard deviation in y-direction'
if vb eq 1 then print,'                      P[5] = ',trim(P[5],'(f15.4)'),'   : Clock-wise rotation in radians'

aa = cos(P[5])^2./(2.*P[3]^2) + sin(P[5])^2./(2.*P[4]^2)

bb = -sin(2.*P[5])/(4.*P[3]^2) + sin(2.*P[5])/(4.*P[4]^2)

cc = sin(P[5])^2./(2.*P[3]^2) + cos(P[5])^2./(2.*P[4]^2)

RETURN, P[0]*exp(-(aa*(X-P[1])^2.+2*bb*(X-P[1])*(Y-P[2])+cc*(Y-P[2])^2.))

if vb eq 1 then print,' '
if vb eq 1 then print,':: fct_2Dgauss.pro :: -- END OF PROGRAM -- '
if vb eq 1 then print,' '
END
