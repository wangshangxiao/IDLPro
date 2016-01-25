PRO GriddingIrregularIntervals
; Make 100 normal x, y points:
x = RANDOMN(seed, 100)
y = RANDOMN(seed, 100)
PRINT, MIN(x), MAX(x)
PRINT, MIN(y), MAX(y)
; Make a Gaussian surface:
z = EXP(-(x^2 + y^2))
; Obtain triangulation:
TRIANGULATE, x, y, triangles, boundary
; Create random x values. These values will be used to
; form the x locations of the resulting grid.
gridX = RANDOMN(seed, 30)
; Sort x values. Sorted values are required for the XOUT
; keyword.
sortX = UNIQ(gridX, SORT(gridX))
gridX = gridX[sortX]
;Outputsorted x values to be used with theXOUT
; keyword.
PRINT, 'gridX:'
PRINT, gridX
; Create random y values. These values will be used to
; form the y locations of the resulting grid.
gridY = RANDOMN(seed, 30)
; Sort y values. Sorted values are required for the YOUT
; keyword.
sortY = UNIQ(gridY, SORT(gridY))
gridY = gridY[sortY]
;Outputsorted y values to be used with theYOUT
; keyword.
PRINT, 'gridY:'
PRINT, gridY
; Derive grid of initial values. The location of the
; resulting grid points are the inputs to the XOUT and
; YOUT keywords.
grid = TRIGRID(x, y, z, triangles, XOUT = gridX, $
YOUT = gridY, EXTRAPOLATE = boundary)
; Display resulting grid. The grid lines are not
; at regular intervals because of the randomness of the
; inputs to the XOUT and YOUT keywords.
SURFACE, grid, gridX, gridY, /XSTYLE, /YSTYLE
END