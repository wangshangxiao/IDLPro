pro griddata7
; Import the Data:
; Determine the path to the file.
file = FILEPATH('irreg_grid1.txt', $
SUBDIRECTORY = ['examples', 'data'])
; Import the data from the file into a structure.
dataStructure = READ_ASCII(file)
; Get the imported array from the first field of
; the structure.
dataArray = TRANSPOSE(dataStructure.field1)
; Initialize the variables of this example from
; the imported array.
x = dataArray[*, 0]
y = dataArray[*, 1]
data = dataArray[*, 2]
; Grid the Data and Display the Results:
; Preprocess and sort the data. GRID_INPUT will
; remove any duplicate locations.
GRID_INPUT, x, y, data, xSorted, ySorted, dataSorted
; Initialize the grid parameters.
gridSize = [51, 51]
; Use the equation of a straight line and the grid parameters to
; determine the x of the resulting grid.
slope = (MAX(xSorted) - MIN(xSorted))/(gridSize[0] - 1)
intercept = MIN(xSorted)
xGrid = (slope*FINDGEN(gridSize[0])) + intercept
; Use the equation of a straight line and the grid parameters to
; determine the y of the resulting grid.
slope = (MAX(ySorted) - MIN(ySorted))/(gridSize[1] - 1)
intercept = MIN(ySorted)
yGrid = (slope*FINDGEN(gridSize[1])) + intercept
; Initialize display.
DEVICE, DECOMPOSED = 0
LOADCT, 38
WINDOW,0,XSIZE =600,YSIZE =600,$
TITLE = 'The Resulting Grid from the Radial Basis Function '+ $
'Method with Faulting'
!P.MULTI = [0, 1, 2, 0, 0]
; Define a fault area, which contains data points.
faultVertices = [[2200, 4000], [2200, 3000], [2600, 2700], $
[2600, -50], [5050, -50], [5050, 4000], [2200, 4000]]
faultConnectivity = [7, 0, 1, 2, 3, 4, 5, 6, -1]
; Grid the data with faulting using the Radial Basis Function
; method.
grid = GRIDDATA(xSorted, ySorted, dataSorted, $
DIMENSION = gridSize, METHOD = 'RadialBasisFunction', $
FAULT_XY = faultVertices, FAULT_POLYGONS = faultConnectivity, $
MISSING = MIN(dataSorted))
; Display grid results.
CONTOUR, BYTSCL(grid), xGrid, YGrid, /XSTYLE, /YSTYLE, /FILL, $
LEVELS = BYTSCL(INDGEN(18)), C_COLORS = BYTSCL(INDGEN(18)), $
TITLE='FaultAreaContainsData ' +$
'(Fault Area in Dashed Lines)', XTITLE = 'x', YTITLE = 'y'
; Display outline of fault area.
PLOTS, faultVertices, /DATA, LINESTYLE = 2, THICK = 2
; Define a fault area, which does not contain data points.
faultVertices = [[2600, -50], [2800, -50], [2800, 2700], $
[2400, 3000], [2400, 4000], [2200, 4000], [2200, 3000], $
[2600, 2700], [2600, -50]]
faultConnectivity = [9, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1]
; Grid the data with faulting using the Radial Basis Function
; method.
grid = GRIDDATA(xSorted, ySorted, dataSorted, $
DIMENSION = gridSize, METHOD = 'RadialBasisFunction', $
FAULT_XY = faultVertices, FAULT_POLYGONS = faultConnectivity, $
MISSING = MIN(dataSorted))
; Display grid results.
CONTOUR, BYTSCL(grid), xGrid, YGrid, /XSTYLE, /YSTYLE, /FILL, $
LEVELS = BYTSCL(INDGEN(18)), C_COLORS = BYTSCL(INDGEN(18)), $
TITLE = 'Fault Area Does Not Contain Data '+ $
'(Fault Area in Dashed Lines)', XTITLE = 'x', YTITLE = 'y'
; Display outline of fault area.
PLOTS, faultVertices, /DATA, LINESTYLE = 2, THICK = 2
; Set system variable back to default value.
!P.MULTI = 0
end