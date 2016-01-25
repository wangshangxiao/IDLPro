pro griddata6
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
; Display the Data:
;Scalethe data to rangefrom 1 to 253soa colortable canbe
; applied. The values of 0, 254, and 255 are reserved as outliers.
scaled = BYTSCL(data, TOP = !D.TABLE_SIZE - 4) + 1B
; Load the color table. If you are on a TrueColor, set the
; DECOMPOSED keyword to the DEVICE command before running a
; color table related routine.
DEVICE, DECOMPOSED = 0
LOADCT, 38
; Open a display window and plot the data points.
WINDOW, 0
PLOT, x, y, /XSTYLE, /YSTYLE, LINESTYLE = 1, $
TITLE = 'Original Data, Scaled (1 to 253)', $
XTITLE = 'x', YTITLE = 'y'
; Now display the data values with respect to the color table.
FOR i =0L,(N_ELEMENTS(x)-1)DO PLOTS,x[i],y[i],PSYM =-1,$
SYMSIZE = 2., COLOR = scaled[i]
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
; Grid the data with the Radial Basis Function method.
grid = GRIDDATA(xSorted, ySorted, dataSorted, $
DIMENSION = gridSize, METHOD = 'RadialBasisFunction')
; Open a display window and contour the Radial Basis Function
; results.
WINDOW, 1
scaled = BYTSCL(grid, TOP = !D.TABLE_SIZE - 4) + 1B
CONTOUR, scaled, xGrid, YGrid, /XSTYLE, /YSTYLE, /FILL, $
LEVELS = BYTSCL(INDGEN(18), TOP = !D.TABLE_SIZE - 4) + 1B, $
C_COLORS = BYTSCL(INDGEN(18), TOP = !D.TABLE_SIZE - 4) + 1B, $
TITLE = 'The Resulting Grid with Radial Basis Function', $
XTITLE = 'x', YTITLE = 'y'
end