PRO FindingPowerLinesInRocklandME
;Determine path to file.
file = FILEPATH('rockland.png', $
SUBDIRECTORY = ['examples', 'data'])
; Import image from file into IDL.
image = READ_PNG(file)
;Determine size of image.
imageSize = SIZE(image, /DIMENSIONS)
; Display cropped image
DEVICE, DECOMPOSED = 1
WINDOW, 0, XSIZE = imageSize[1], YSIZE = imageSize[2], $
TITLE = 'Rockland, Maine'
TV, image, TRUE = 1
; Use layer from green channel as the intensity of the
; image.
intensity = REFORM(image[1, *, *])
; Determine size of intensity image.
intensitySize = SIZE(intensity, /DIMENSIONS)
; Mask intensity image to highlight power lines.
mask = intensity GT 240
; Transform mask.
transform = HOUGH(mask, RHO = rho, THETA = theta)
; Scale transform to obtain just the power lines.
transform = (TEMPORARY(transform) - 100) > 0
; Backproject to compare with original image.
backprojection = HOUGH(transform, /BACKPROJECT, $
RHO = rho, THETA = theta, $
NX = intensitySize[0], NY = intensitySize[1])
; Reverse color table to clarify lines. If you are on
; a TrueColor display, set the DECOMPOSED keyword to 0
; before using any color table related routines.
DEVICE, DECOMPOSED = 0
LOADCT, 0
TVLCT, red, green, blue, /GET
TVLCT,255 -red,255 -green,255 -blue
; Display results.
WINDOW, 1, XSIZE = intensitySize[0], $
YSIZE = intensitySize[1], $
TITLE = 'Resulting Power Lines'
TVSCL, backprojection
END