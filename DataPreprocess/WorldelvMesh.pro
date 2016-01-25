PRO WorldelvMesh
; Determine path to image file.
worldelvFile = FILEPATH('worldelv.dat', $
SUBDIRECTORY = ['examples', 'data'])
; Initialize image parameters.
worldelvSize = [360, 360]
worldelvImage = BYTARR(worldelvSize[0], worldelvSize[1])
; Open file, read in image, and close file.
OPENR, unit, worldelvFile, /GET_LUN
READU, unit, worldelvImage
FREE_LUN, unit
;Resizeimagetoobtaindatafor a 1 degreeintervalin
; both directions.
worldelvImage = CONGRID(worldelvImage, 360, 180, /INTERP)
; Initialize display objects.
oWindow = OBJ_NEW('IDLgrWindow', RETAIN = 2, $
DIMENSIONS = [worldelvSize[0], worldelvSize[1]/2], $
TITLE = 'Original Elevation Image')
oView = OBJ_NEW('IDLgrView', VIEWPLANE_RECT = [0., 0., $
worldelvSize[0], worldelvSize[1]/2])
oModel = OBJ_NEW('IDLgrModel')
; Initialize and set palette to the STD GAMMA-II color
; table.
oPalette = OBJ_NEW('IDLgrPalette')
oPalette -> LoadCT, 5
; Initialize image object.
oImage = OBJ_NEW('IDLgrImage', worldelvImage, $
PALETTE = oPalette)
; Add the image to the model, which is added to the
; view, and then the view is displayed in the window.
oModel -> Add, oImage
oView -> Add, oModel
oWindow -> Draw, oView
; Clean-up unused object references.
OBJ_DESTROY, [oView]
; Scale image values to the earth radius. Multiple
; scaling by 50 to exaggerate elevation.
worldelvImage = 50.*1.77*(worldelvImage/255.)
; Add the earth's radius to the image. The image only
; contains elevation information from the deepest parts
; of the oceans. The earth's radius is added to obtain
; a sphere with small changes in elevation on its
; surface.
radii = worldelvImage + REPLICATE(1275.6, 360, 180)
; Derive a mesh from the exaggerated image data and the
; radius of the earth.
MESH_OBJ, 4, vertices, connectivity, radii, /CLOSED
; Initialize a model to display.
oModel = OBJ_NEW('IDLgrModel')
; Determine the radius of each vertex to provide color
; at each vertex.
sphericalCoordinates = CV_COORD(FROM_RECT = vertices, $
/TO_SPHERE)
elevation = REFORM(sphericalCoordinates[2, *], $
N_ELEMENTS(sphericalCoordinates[2, *]))
; Initialize polygon to contain mesh.
oPolygon = OBJ_NEW('IDLgrPolygon', vertices, $
POLYGONS = connectivity, SHADING = 1, $
VERT_COLORS = BYTSCL(elevation), $
PALETTE = oPalette)
; Add polygon to model.
oModel -> Add, oPolygon
; Rotate model to place view at 0 degrees latitude.
oModel -> Rotate, [1., 0., 0.], -90.
; Display model.
XOBJVIEW, oModel, /BLOCK, SCALE = 1, $
TITLE = 'Exaggerated Earth Elevation'
; Clip earth polgyon along the equator.
planeCoefficients = [0., 0., 1., 0.]
numberVertices = MESH_CLIP(planeCoefficients, $
vertices, connectivity, $
clippedVertices, clippedConnectivity, $
CUT_VERTS = cutVerticesIndex)
; Determine the radius of each vertex to provide color
; at each vertex.
sphericalCoordinates = CV_COORD($
FROM_RECT = clippedVertices, /TO_SPHERE)
elevation = REFORM(sphericalCoordinates[2, *], $
N_ELEMENTS(sphericalCoordinates[2, *]))
; Update polygon with results from clipping.
oPolygon -> SetProperty, DATA = clippedVertices, $
POLYGONS = clippedConnectivity, $
VERT_COLORS = BYTSCL(elevation)
; Display updated model.
XOBJVIEW, oModel, /BLOCK, SCALE = 1, $
TITLE = 'Earth Clipped at the Equator'
; Determine clipped plane's vertices.
cutVertices = clippedVertices[*, cutVerticesIndex]
x = cutVertices[0, *]
y = cutVertices[1, *]
z = cutVertices[2, *]
; Compute the center vertex of the clipped plane.
centerX = TOTAL(x)/N_ELEMENTS(x)
centerY = TOTAL(y)/N_ELEMENTS(y)
centerZ = TOTAL(z)/N_ELEMENTS(z)
; Determine the inner radius of the earth polygon.
sphericalCoordinates = CV_COORD(FROM_RECT = cutVertices, $
/TO_SPHERE)
elevation = REFORM(sphericalCoordinates[2, *], $
N_ELEMENTS(sphericalCoordinates[2, *]))
innerRadius = MIN(elevation)
; Derive the corner vertices of the clipping plane.
planeVertices = $
[[centerX - innerRadius, 0, centerZ - innerRadius], $
[centerX + innerRadius, 0, centerZ - innerRadius], $
[centerX + innerRadius, 0, centerZ + innerRadius], $
[centerX - innerRadius, 0, centerZ + innerRadius]]
planeConnectivity = [4, 0, 1, 2, 3]
; Determine the path to the earth's mantle convection
; data file.
convecFile = FILEPATH('convec.dat', $
SUBDIRECTORY = ['examples', 'data'])
; Initialize convection image and parameters.
convecSize = [248, 248]
convecImage = BYTARR(convecSize[0], convecSize[1])
convecData = BYTARR(convecSize[0], convecSize[1], 4)
; Open file, read in image, and close file.
OPENR, unit, convecFile, /GET_LUN
READU, unit, convecImage
FREE_LUN, unit
; Create mask of image. Mask out the background (zero
; values of the image, and apply mask to the alpha
; channel.
mask = BYTSCL(convecImage GT 0)
convecData[*, *, 3] = mask
; Convert indexed image to RGB image.
DEVICE, DECOMPOSED = 0
LOADCT, 27
TVLCT, red, green, blue,/GET
convecData[*, *, 0] = red[convecImage]
convecData[*, *, 1] = green[convecImage]
convecData[*, *, 2] = blue[convecImage]
; Initialize an image object of the resulting RGB image
;to be used as a texturemap placed on theclipping
; plane.
oPlaneImage = OBJ_NEW('IDLgrImage', convecData, $
INTERLEAVE = 2, BLEND_FUNCTION = [3, 4])
; Initialize polygon of clipping plane, which contains
; the texture map of the image.
oPlanePolygon = OBJ_NEW('IDLgrPolygon', $
planeVertices, POLYGONS = planeConnectivity, $
SHADING = 0, COLOR = [255, 255, 255], $
TEXTURE_MAP = oPlaneImage, $
TEXTURE_COORD = [[0, 0], [1, 0], [1, 1], [0, 1]])
; Add the clipping plane's polygon to the model.
oModel -> Add, oPlanePolygon
; Display results.
XOBJVIEW, oModel, /BLOCK, SCALE = 1, $
TITLE = 'Earth Elevation and Mantle Convection'
; Decimate clipped earth polygon.
numberTriangles = MESH_DECIMATE(clippedVertices, $
clippedConnectivity, decimatedConnectivity, $
VERTICES = decimatedVertices, PERCENT_VERTICES = 10)
; Determine the radius of each vertex to provide color
; at each vertex.
sphericalCoordinates = CV_COORD($
FROM_RECT = decimatedVertices, /TO_SPHERE)
elevation = REFORM(sphericalCoordinates[2, *], $
N_ELEMENTS(sphericalCoordinates[2, *]))
; Update polygon with results from decimating.
oPolygon -> SetProperty, DATA = decimatedVertices, $
POLYGONS = decimatedConnectivity, $
VERT_COLORS = BYTSCL(elevation)
; Display decimation results.
XOBJVIEW, oModel, /BLOCK, SCALE = 1, $
TITLE = 'Decimated Earth and Mantle Convection'
; Cleanup the object references.
OBJ_DESTROY, [oModel, oPalette, oPlaneImage]
END