pro polyfill_test
; Define pattern array as 10 by 10:

PAT = BYTARR(10,10)

; Set center pixel to bright:
PAT[5,5] = 255

; Fill the rectangle defined by the four corners of the window with
; the pattern:
;POLYFILL, !X.WINDOW([0,1,1,0]), $
;          !Y.WINDOW([0,0,1,1]), /NORM, PAT = PAT
; Create the vectors of X and Y values:

X = [30, 100, 100, 30] & Y = [30, 30, 100, 100]

; Fill the polygon with color index 175:
POLYFILL, X, Y, COLOR = 175, /DEVICE,pat=pat

end