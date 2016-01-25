PRO RemovingNoiseFromAnImageWithFFT
; Determine the path to the file.
file = FILEPATH('abnorm.dat', $
SUBDIRECTORY = ['examples', 'data'])
; Initialize size parameter and image array.
imageSize = [64, 64]
image = BYTARR(imageSize[0], imageSize[1])
; Open file, read in image, and close file.
OPENR, unit, file, /GET_LUN
READU, unit, image
FREE_LUN, unit
; Initialize display parameters, including a color
; table. If you are on a TrueColor display, set
; the DECOMPOSED keyword to 0 before using any color
; table related routines.
displaySize = [128, 128]
DEVICE, DECOMPOSED = 0
LOADCT, 5
WINDOW, 0, XSIZE = 2*displaySize[0], $
YSIZE = displaySize[1], $
TITLE = 'Original Image : Transformation'
; Display original image.
TVSCL, CONGRID(image, displaySize[0], displaySize[1], $
/INTERP), 0
; Transform image.
transform = ALOG(SHIFT(FFT(image), (imageSize[0]/2), $
(imageSize[1]/2)))
; Display transformation.
TVSCL, CONGRID(transform, displaySize[0], $
displaySize[1], /INTERP), 1
; Scale transform make its minimum value equal to zero.
scaledTransform = transform - MIN(transform)
; Display results of scaling.
WINDOW, 1, TITLE = 'Transform Scaled to a Zero Minimum'
SURFACE, scaledTransform, /XSTYLE, /YSTYLE, $
TITLE = 'Transform Scaled to a Zero Minimum'
; Filter scaled transform to only include high
; frequency data.
mask = FLOAT(scaledTransform) GT 6.
filteredTransform = (scaledTransform*mask) + $
MIN(transform)
; Initialize display.
WINDOW, 2, XSIZE = 2*displaySize[0], $
YSIZE = displaySize[1], $
TITLE = 'Filtered Transformation : Results'
; Display filtered transform.
TVSCL, CONGRID(FLOAT(filteredTransform), displaySize[0], $
displaySize[1], /INTERP), 0
; Apply inverse transformation to filtered transform.
inverseTransform = ABS(FFT(EXP(filteredTransform), $
/INVERSE))
; Display results of inverse transformation.
TVSCL, CONGRID(inverseTransform, displaySize[0], $
displaySize[1], /INTERP), 1
END