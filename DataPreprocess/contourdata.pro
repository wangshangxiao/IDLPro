PRO contourdata

;+
; RSI TRAINING FILE HEADER
; FILE: contourdata.pro
; ROUTINE: contourdata
; PURPOSE: contour example with 8-bit and 24-bit color
; TOPICS: contour, color indexing in 8 and 24-bit
; CLASS: Intro/Intermediate
; REFERENCE: Color chapter Intro to IDL
; LAST MODIFIED: 09/30/99 - BL
; USAGE: contourdata
;-

device, get_decompose = oldDC
device, get_visual_depth = vd
if (vd eq 8) then begin
	loadct, 5
	colors = [10, 50, 90, 140, 180]
endif else begin
	device, decompose = 1
	colors = ['ff0000'x, '00ff00'x, '0000ff'x, '7f7f7f'x, 'ffff00'x]
endelse

restore, filepath('marbells.dat', subdir = ['examples','data'])
contour, elev, NLEVELS = 15, C_COLORS = colors, $
	MIN_VALUE = 3000, /FOLLOW, XSTYLE = 1, YSTYLE = 1
device, decompose = oldDC

END
