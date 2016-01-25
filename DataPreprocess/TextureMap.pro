PRO TextureMap
; State the path to image file.
image_file = FILEPATH('elev_t.jpg', $
SUBDIRECTORY=['examples', 'data'])
; Import image file.
READ_JPEG, image_file, image
;Statethe path to DEMdata file.
data_file = FILEPATH('elevbin.dat', $
SUBDIRECTORY=['examples', 'data'])
; Import elevation data.
dem_data = BYTARR(64, 64)
OPENR, unit, data_file, /GET_LUN
READU, unit, dem_data
FREE_LUN, unit
; Increase size of data for visibility.
dem_data = CONGRID(dem_data, 128,128, /INTERP)
; Initialize the model, surface and image objects.
oModel = OBJ_NEW('IDLgrModel')
oSurface = OBJ_NEW('IDLgrSurface', dem_data, STYLE = 2)
oImage = OBJ_NEW('IDLgrImage', image, $
INTERLEAVE = 0, /INTERPOLATE)
; Calculate normalized conversion factors and
; shift -.5 in every direction to center object
; in the window.
; Keep in mind that your view default coordinate
; system is [-1,-1], [1, 1]
oSurface -> GETPROPERTY, XRANGE = xr, $
YRANGE = yr, ZRANGE = zr
xs = NORM_COORD(xr)
xs[0] = xs[0] - 0.5
ys = NORM_COORD(yr)
ys[0] = ys[0] - 0.5
zs = NORM_COORD(zr)
zs[0] = zs[0] - 0.5
oSurface -> SETPROPERTY, XCOORD_CONV = xs, $
YCOORD_CONV = ys, ZCOORD = zs
; Apply the image to surface (texture mapping).
oSurface->SetProperty, TEXTURE_MAP = oImage, $
COLOR = [255, 255, 255]
; Add the surface to the model.
oModel -> Add, oSurface
; Rotate the model for better display of surface
; in the object window.
oModel -> ROTATE, [1, 0, 0], -90
oModel -> ROTATE, [0, 1, 0], 30
oModel -> ROTATE, [1, 0, 0], 30
; Display results in XOBJVIEW utility to provide
; rotation, zoom, and translation control.
XOBJVIEW, oModel, /BLOCK, SCALE = 1
; Cleanup object references.
OBJ_DESTROY, [oImage, oModel]
END