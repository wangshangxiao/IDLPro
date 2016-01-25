PRO DecimatingAMesh
;Determine path to data file.
elevbinFile = FILEPATH('elevbin.dat', $
SUBDIRECTORY = ['examples', 'data'])
; Initialize data parameters.
elevbinSize = [64, 64]
elevbinData = BYTARR(elevbinSize[0], elevbinSize[1])
; Open file, read in data, and close file.
OPENR, unit, elevbinFile, /GET_LUN
READU, unit, elevbinData
FREE_LUN, unit
; Convert data into a mesh, which is defined by
; vertice locations and their connectivity.
MESH_OBJ, 1, vertices, connectivity, elevbinData
; Initialize a model for display.
oModel = OBJ_NEW('IDLgrModel')
; Form a polygon from the mesh.
oPolygon = OBJ_NEW('IDLgrPolygon', vertices, $
POLYGONS = connectivity, SHADING = 1.5, $
COLOR = [0, 255, 0], STYLE = 1)
; Add polygon to model.
oModel -> Add, oPolygon
; Rotate model for better initial perspective.
oModel -> Scale, 1, 1, 0.25
oModel -> Rotate, [-1, 0, 1], 45.
; Display model in the XOBJVIEW utility.
XOBJVIEW, oModel, /BLOCK, SCALE = 1., $
TITLE = 'Original Mesh from Elevation Data'
; Derive a color table for the filled polygon.
oPalette = OBJ_NEW('IDLgrPalette')
oPalette -> LOADCT, 29
; Fill in the polygon mesh with the colors of the table
; (the colors correspond to the z-values of the polygon).
oPolygon -> SetProperty, STYLE = 2, $
VERT_COLORS = BYTSCL(vertices[2, *]), $
PALETTE = oPalette
; Display model in the XOBJVIEW utility.
XOBJVIEW, oModel, /BLOCK, SCALE = 1., $
TITLE = 'Filled Original Mesh'
; Decimate the mesh down to 20 percent of the original
; number of vertices.
numberVertices = MESH_DECIMATE(vertices, connectivity, $
decimatedConnectivity, VERTICES = decimatedVertices, $
PERCENT_VERTICES = 20)
; Update the polygon with the resulting decimated mesh.
oPolygon -> SetProperty, DATA = decimatedVertices, $
POLYGONS = decimatedConnectivity, STYLE = 1, $
VERT_COLORS = 0, COLOR = [0, 255, 0]
; Display updated model in the XOBJVIEW utility.
XOBJVIEW, oModel, /BLOCK, SCALE = 1., $
TITLE = 'Decimation Results (by 80%)'
; Fill in the updated polygon mesh with the colors of
; the table (the colors correspond to the z-values of
; the updated polygon).
oPolygon -> SetProperty, STYLE = 2, $
VERT_COLORS = BYTSCL(decimatedVertices[2, *]), $
PALETTE = oPalette
; Display model in the XOBJVIEW utility.
XOBJVIEW, oModel, /BLOCK, SCALE = 1., $
TITLE = 'Filled Decimation Results'
; Cleanup all the objects by destroying the model.
OBJ_DESTROY, [oModel, oPalette]
END