;+
;----------------------------
;   NAME
;----------------------------
;   contourarray
;
;----------------------------
;   PURPOSE/DESCRIPTION
;----------------------------
;   Creates an array which can be used as input for the contour plotting
;   keyword 'contour' in idl to create a density contour map of a scatter
;   plot containing many points, from two input vectors of data. It returns
;   suitable plotting ranges and contour levels to make the plot look accceptable
;----------------------------
;   COMMENTS
;----------------------------
;   none
;----------------------------
;   INPUTS:
;----------------------------
; x             : The x component of the data points
; xmin          : The minimum value of the x-range (the range you want to plot, not
;                 nescessarily the range of the data vector)
; xmax          : The maximum value of the x-range (the range you want to plot, not
;                 nescessarily the range of the data vector)
; y             : The y component of the data points
; ymin          : The minimum value of the y-range (the range you want to plot, not
;                 nescessarily the range of the data vector)
; ymax          : The maximum value of the y-range (the range you want to plot, not
;                 nescessarily the range of the data vector)
; xarrsize      : The x-size of the contour array created
;                 (start with 10 and see how it goes)
; yarrsize      : The y-size of the contour array created
;                 (start with 10 and see how it goes)
; levels        : The number of contour levels you want to have in your plot.
;                 These levels are automatically spaced but can be set manually and
;                 you are probably better off doing that...
;----------------------------  
;   OPTIONAL INPUTS:
;----------------------------
; none
;----------------------------
;   OUTPUTS:
;----------------------------
; contarr       : The array containing the 'density map' of the x and y data.
;                 basically a 2D histogram where the binsizes are determinde by
;                 the inputs xarrsize and yarrsize
; levelbin      : A vectore containing suitable levels for the contours
; xrange        : A vector containing the x range for plotting the contours
; yrange        : A vector containing the y range for plotting the contours
;----------------------------
;   EXAMPLES/USAGE
;----------------------------
; if you have set of scatter data points in the two vectors c7(crs),c11(crs) you can
; call the program by:

;contourarray,c7(crs),0.0,1.0,c11(crs),-0.1,1.2,10,10,10,contarr2,levelbin,xrange,yrange

; and afterwards plot the contours (after calling the colors) :
;col=getcolor(/load) 
;contour,contarr2,xrange,yrange $     ; you can use the command /overplot if needed
;	, levels=levelbin $
;	, C_COLOR=[col.blue,col.red,col.green,col.yellow,col.magenta] $
;	, C_LABELS = levelbin
;----------------------------
;   BUGS
;----------------------------
;
;
;----------------------------
;   REVISION HISTORY
;----------------------------
;   2009-07-09  started by K. B. Schmidt (MPIA)
;
;----------------------------
; dependecies,to be compiled as well:
; none
; input/output:  i   i    i  i   i    i      i         i      i     o         o      o      o
PRO contourarray,x,xmin,xmax,y,ymin,ymax,xarrsize,yarrsize,levels,contarr,levelbin,xrange,yrange

;defining arrays for output
contarr = fltarr(xarrsize,yarrsize)
xrange = fltarr(xarrsize)
yrange = fltarr(yarrsize)

;looping over the x columns in output array
for j=0,xarrsize-1 do begin
   ;calculating each entry of the x range vectore as the central point of each column 
   xrange(j) = (xmin+(j)*((xmax-xmin)/xarrsize))+(xmax-xmin)/xarrsize/2.
   ;looping over the y columns in output array
   for k=0,yarrsize-1 do begin
      ;calculating each entry of the y range vectore as the central point of each column
      yrange(k) = (ymin+(k)*((ymax-ymin)/yarrsize))+(ymax-ymin)/yarrsize/2.
      ;calculating the amount of points falling in a given 2D-box in the output array 
      box = where(x lt (xmin+(j+1)*((xmax-xmin)/xarrsize)) AND x gt (xmin+j*((xmax-xmin)/xarrsize)) AND y lt (ymin+(k+1)*((ymax-ymin)/yarrsize)) AND y gt (ymin+k*((ymax-ymin)/yarrsize)) ,cbox)
      ;filling the output array with the counts so it makes up a '2D histogram'
      contarr(j,k) = cbox
   endfor
endfor

;creating contour levels spaced equally in the contarr range for plotting
Clevelmin = min(contarr)
Clevelmax = max(contarr)
levelbin  = fltarr(levels)
for h=0,levels-1 do begin
   levelbin(h) = Clevelmin+h*(Clevelmax-Clevelmin)/levels
endfor

END