pro griddata3
; Create a dataset of N points.
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
; Initialize display.
WINDOW,0,XSIZE =512,YSIZE =768,$
TITLE = 'Polynomial Regression'
!P.MULTI = [0, 1, 3, 0, 0]
; The following examples require the triangulation.
TRIANGULATE, x, y, tr
; Fit with a 2nd degree polynomial in x and y. This fit considers
; all points when fitting the surface, obliterating the individual
; peaks.
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
TRIANGLES = tr, /POLYNOMIAL_REGRESSION)
SURFACE, grid, CHARSIZE = 3, TITLE = 'Global Degree 2 Polynomial'
; Fit with a 2nd degree polynomial in x and y, but this time use
; only the 10 closest nodes to each interpolant. This provides a
; relatively smooth surface, but still shows the individual peaks.
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
TRIANGLES = tr, /POLYNOMIAL_REGRESSION, MAX_PER_SECTOR = 10)
SURFACE, grid, CHARSIZE = 3, TITLE = 'Local Polynomial, 10 Point'
; As above, but use only the nodes within a distance of 0.4 when
; fitting each interpolant.
grid = GRIDDATA(x, y, f, START = 0, DELTA = 0.02, DIMENSION = 51, $
TRIANGLES = tr, /POLYNOMIAL_REGRESSION, SEARCH_ELLIPSE = 0.4)
SURFACE, grid, CHARSIZE = 3, $
TITLE = 'Local Polynomial, Radius = 0.4'
; Set system variable back to default value.
!P.MULTI = 0
end