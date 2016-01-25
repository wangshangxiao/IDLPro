;+
; RSI TRAINING FILE HEADER
; FILE: multisine.pro
; ROUTINE: multisine
; PURPOSE: oplot with color, /3d
; TOPICS: plot, oplot, color tables
; CLASS: Intermediate / Intro
; REFERENCE:
; LAST MODIFIED: 09/30/99 - BL
; USAGE: multisine
; NOTES:
;-

PRO multisine, threed = threed

device, get_visual_depth = vd
if (vd gt 8) then begin
	device, get_decompose = oldDC
	if (keyword_set(threed)) then begin
		device, decompose = 1
		erase, 'ff3300'xl
		x = [0.15, 0.85, 0.85, 0.15, 0.15]
		y = [0.15, 0.15, 0.85, 0.85, 0.15]
		polyfill, x, y, COLOR = '7f7f7f'xl, /NORMAL
		x = [0.85, 0.9, 0.9, 0.85, 0.85]
		y = [0.15, 0.20, 0.90, 0.85, 0.15]
		polyfill, x, y, COLOR = '7f7f7f'xl, /NORMAL
		plots, x, y, COLOR = 'ffffff'xl, /NORMAL, THICK = 2
		x = [0.15, 0.85, 0.9, 0.2, 0.15]
		y = [0.85, 0.85, 0.9, 0.9, 0.85]
		polyfill, x, y, COLOR = '7f7f7f'xl, /NORMAL
		plots, x, y, COLOR = 'ffffff'xl, /NORMAL, THICK = 2
	endif
	device, decompose = 0
endif

data1 = sin(findgen(360)*!DTOR)
data2 = cos(findgen(360)*!DTOR)

!P.POSITION = [0.15, 0.15, 0.85, 0.85]
plot, data1, /NODATA, XSTYLE = 1, /NOERASE
!p.position = 0
loadct, 31
for i = 1.0, 0.1, -0.1 do oplot, data1*i, color = i*200
loadct, 12
for i = 1.0, 0.1, -0.1 do oplot, data2*i, color = i*200


if (vd gt 8) then begin
	device, decompose = oldDC
endif

END

