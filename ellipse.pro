;+
;----------------------------
;   NAME
;----------------------------
; ellipse.pro
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
; Function creating an array with the coordinates for an ellipse which
; can then be (over)plotted
;----------------------------
;   COMMENTS
;----------------------------
; 
;----------------------------
;   INPUTS:
;----------------------------
; center          : the coordinates of the center of the ellipse
; radii           : the radii of the ellipse
;----------------------------
;   OPTIONAL INPUTS:
;----------------------------
; Arot            : (counterclockwise) rotaion angle in radians (default 0)
; Npoints         : Number of points in ellipse (default 100)
;----------------------------
;   OUTPUTS:
;----------------------------
; Array with ellipse
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; IDL> ell = ELLIPSE([5,5],[1,2],arot=!pi/4.,Npoints=20)
; IDL> plot,ell(0,*),ell(1,*)
;----------------------------
;   BUGS
;----------------------------
;
;----------------------------
;   REVISION HISTORY
;----------------------------
; 2010-11-30  started by K. B. Schmidt (MPIA)
;----------------------------
;   DEPENDENCIES
;----------------------------
;
;----------------------------
;-

FUNCTION ellipse, center, radii, Arot=Arot, Npoints=Npoints
   if n_elements(Npoints) eq 0 then Npoints = 100           ; Number of points in ellipse
   points = (2 * !PI / (Npoints-1.0)) * FINDGEN(Npoints)
   x = radii[0] * COS(points)                               ; x components of ellipse at [0,0]
   y = radii[1] * SIN(points)                               ; y components of ellipse at [0,0]
   vec = transpose([[x],[y]])
   if n_elements(Arot) eq 0 then Arot = 0                   ; rotation angle in radians
   rotmat = [[cos(Arot),-sin(Arot)],[sin(Arot),cos(Arot)]]  ; rotation matrix
   vec  = (rotmat#vec)                                      ; rotating ellipse
   Crep = transpose([[replicate(center[0],Npoints)],[replicate(center[1],Npoints)]]) ; replicating center to be added to ellipse
   vec  = vec + Crep                                        ; move ellipse to center position
   RETURN, vec
END
