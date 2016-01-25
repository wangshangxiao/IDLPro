PRO ContrastingCells
;Determine path to file.
file = FILEPATH('endocell.jpg', $
SUBDIRECTORY = ['examples', 'data'])
; Import image within file into IDL.
READ_JPEG, file, endocellImage
; Determine image's size, but divide it by 4 to reduce
; the image.
imageSize = SIZE(endocellImage, /DIMENSIONS)/4
; Resize image to quarter its original length and width.
endocellImage = CONGRID(endocellImage, $
imageSize[0], imageSize[1])
; If you are on a truecolor display, set the DECOMPOSED
; keyword to the DEVICE command to zero before using
; any color table related routines.
DEVICE, DECOMPOSED = 0
; Load in the STD GAMMA-II color table.
LOADCT, 5
; Initialize the display.
WINDOW, 0, XSIZE = 2*imageSize[0], YSIZE = imageSize[1], $
TITLE = 'Original (left) and Filtered (right)'
; Display original image.
TV, endocellImage, 0
; Filter original image to clarify the edges of the
; cells.
image = ROBERTS(endocellImage)
; Display filtered image.
TVSCL, image, 1
; Transform the filtered image.
transform = RADON(image, RHO = rho, THETA = theta)
; Display transforms of the image.
transformSize = SIZE(transform, /DIMENSIONS)
WINDOW,1,TITLE ='Original Transform(top)and ' +$
'Scaled Transform (bottom)', $
XSIZE = transformSize[0], YSIZE = 2*transformSize[1]
TVSCL, transform, 0
; Scale the transform to include only the density
; values above the mean of the transform.
scaledTransform = transform > MEAN(transform)
; Display scaled transform.
TVSCL, scaledTransform, 1
; Backproject the scaled transform.
backprojection = RADON(scaledTransform, /BACKPROJECT, $
RHO = rho, THETA=theta, NX = imageSize[0], $
NY = imageSize[1])
; Initialize another display.
WINDOW, 2, XSIZE = 2*imageSize[0], YSIZE = imageSize[1], $
TITLE = 'Backproject (left) and Final Result (right)'
; Display backprojection.
TVSCL, backprojection, 0
; Use the backprojection as a mask to provide
; a color density contrast of the original image.
constrastingImage = endocellImage*backprojection
; Display resulting contrast image.
TVSCL, endocellImage*backprojection, 1
END