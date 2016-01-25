pro griddata4
; Create a dataset of N points.\.
n = 100 ;# of scattered points
seed = -121147L ;For consistency
x = RANDOMU(seed, n)
y = RANDOMU(seed, n)
; Create a dependent variable in the form a function of (x,y)
; with peaks & valleys.
f =3 *EXP(-((9*x-2)^2+(7-9*y)^2)/4)+$
3 * EXP(-((9*x+1)^2)/49 - (1-0.9*y)) + $
2 * EXP(-((9*x-7)^2 + (6-9*y)^2)/4) - $
EXP(-(9*x-4)^2 - (2-9*y)^2)
; Note: the inverse distance, kriging, polynomial regression, and
; radial basis function methods are, by default, global methods in
; which each input node affects each output node. With these
; methods, large datasets can require a prohibitively long time to
; compute unless the scope of the interpolation is limited to a
; local area around each interpolate by specifying search rules.
; In fact, the radial basis function requires time proportional to
; the cube of the number of input points.
; For example, with 2,000 input points, a typical workstation
; required 500 seconds to interpolate a 10,000 point grid using
; radial basis functions. By limiting the size of the fit to the
; 20 closest points to each interpolate, via the MIN_POINTS
; keyword, the time required dropped to less than a second.
; Initialize display.
WINDOW,0,XSIZE =512,YSIZE =512,$
TITLE = 'Radial Basis Function'
!P.MULTI = [0, 1, 2, 0, 0]
; Slow way:
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
/RADIAL_BASIS_FUNCTION)
SURFACE, grid, CHARSIZE = 3, TITLE = 'All Points'
; The following example requires triangulation.
TRIANGULATE, x, y, tr
; Faster way:
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
/RADIAL_BASIS_FUNCTION, MIN_POINTS = 15, TRIANGLES = tr)
SURFACE, grid, CHARSIZE = 3, TITLE = 'Nearest 15 Points'
; Set system variable back to default value.
!P.MULTI = 0
end