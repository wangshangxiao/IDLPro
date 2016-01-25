pro griddata2
; Create a dataset of N points.
n = 100 ;# of scattered points
seed = -121147L ;For consistency
x = RANDOMU(seed, n)
y = RANDOMU(seed, n)
; Create a dependent variable in the form of a function of (x,y)
; with peaks & valleys.
f =3 *EXP(-((9*x-2)^2+(7-9*y)^2)/4)+$
3 * EXP(-((9*x+1)^2)/49 - (1-0.9*y)) + $
2 * EXP(-((9*x-7)^2 + (6-9*y)^2)/4) - $
EXP(-(9*x-4)^2 - (2-9*y)^2)
; Initialize display.
WINDOW,0,XSIZE =512,YSIZE =512,$
TITLE = 'Different Methods of Gridding'
!P.MULTI = [0, 1, 2, 0, 0]
; Use radial basis function with multilog basis function.
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
/RADIAL_BASIS_FUNCTION, FUNCTION_TYPE = 1)
SURFACE, grid, CHARSIZE = 3, TITLE = 'Radial Basis Function'
; The following example requires triangulation.
TRIANGULATE, x, y, tr
; Use Modified Shepard's method.
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
TRIANGLES = tr, /SHEPARDS)
SURFACE, grid, CHARSIZE = 3, TITLE = "Modified Shepard's Method"
; Set system variable back to default value.
!P.MULTI = 0
end