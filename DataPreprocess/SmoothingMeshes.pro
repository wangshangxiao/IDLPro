PRO SmoothingMeshes
; Initialize mesh size parameters.
nX =10
nY =5
; Initialize the x coordinates of the mesh's vertices.
xVertices = FINDGEN(nX) # REPLICATE(1., nY)
PRINT, 'xVertices: '
PRINT, xVertices, FORMAT = '(10F6.1)'
; Initialize the y coordinates of the mesh's vertices.
yVertices = REPLICATE(1., nX) # FINDGEN(nY)
PRINT, 'yVertices: '
PRINT, yVertices, FORMAT = '(10F6.1)'
; Derive the overall vertices of the mesh.
vertices = FLTARR(3, (nX*nY))
vertices[0, *] = xVertices
vertices[1, *] = yVertices
PRINT, 'vertices: '
PRINT, vertices, FORMAT = '(3F6.1)'
; Triangulate the mesh to establish connectivity.
TRIANGULATE, xVertices, yVertices, triangles
trianglesSize = SIZE(triangles, /DIMENSIONS)
polygons = LONARR(4, trianglesSize[1])
polygons[0, *] = 3
polygons[1, 0] = triangles
PRINT, 'polygons: '
PRINT, polygons, FORMAT = '(4I6)'
; Derive connectivity from the resulting triangulation.
connectivity = REFORM(polygons, N_ELEMENTS(polygons))
; Initialize a model for the display.
oModel = OBJ_NEW('IDLgrModel')
; Initialize a polygon object to contain the mesh.
oPolygon = OBJ_NEW('IDLgrPolygon', vertices, $
POLYGONS = connectivity, COLOR = [0, 128, 0], $
STYLE = 1)
; Add the polygon to the model.
oModel -> Add, oPolygon
; Display the model.
XOBJVIEW, oModel, /BLOCK, $
TITLE = 'Original Mesh'
; Introduce an irregular vertex by drastically changing
; a single y coordinate.
vertices[1, 45] = 10.
; Update polygon with new vertices.
oPolygon -> SetProperty, DATA = vertices
; Display change.
XOBJVIEW, oModel, /BLOCK, $
TITLE = 'Mesh with New Irregular Vertex'
; Smooth entire mesh to reduce the effect of the
; irregular vertex.
smoothedVertices = MESH_SMOOTH(vertices, connectivity)
; Update polygon and display results.
oPolygon -> SetProperty, DATA = smoothedVertices
XOBJVIEW, oModel, /BLOCK, $
TITLE = 'Smoothing with No Fixed Vertices'
; Determine which vertices should be fixed. Basically,
; all of the vertices should be fixed except for the
; irregular vertex.
fixed = LINDGEN((nX*nY) - 1)
fixed[45] = fixed[45:*] + 1
; Smooth mesh with resulting fixed vertices.
smoothedVertices = MESH_SMOOTH(vertices, connectivity, $
FIXED_VERTICES = fixed)
; Update polygon and display results.
oPolygon -> SetProperty, DATA = smoothedVertices
XOBJVIEW, oModel, /BLOCK, $
TITLE = 'Smoothing with Almost All Vertices Fixed'
; Cleanup object references.
OBJ_DESTROY, [oModel]
END