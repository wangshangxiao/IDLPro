pro oprintex

; Create a view object
oView1 = Obj_New('IDLgrView', $
	VIEWPLANE_RECT = [0,0,400,400], $
	ZCLIP = [1,-1])
; Create a model object
oModel1 = Obj_New('IDLgrModel')

; Create an Image object
oImage = Obj_New('IDLgrImage', dist(400))

; Create the hierarchy
oModel1->Add, oImage
oView1->Add, oModel1

; Create the printer object and set some properties
oPrinter = Obj_New('IDLgrPrinter', $
	/LANDSCAPE, $
	N_COPIES = 2, $
	PRINT_QUALITY = 3)

; Create a palette that will be used for the printer
; object and load a color table into the palette
oPalette = Obj_New('IDLgrPalette')
oPalette->LoadCt, 5
oPrinter->SetProperty, Palette = oPalette

; Draw the hierarchy to the printer and close
; the document
;oPrinter->Draw, oView1
;oPrinter->NewDocument
a = dialog_printersetup(oPrinter)
	if (a) then begin
		oPrinter->Draw, state.oView
		oPrinter->NewDocument
	endif
	obj_destroy, oPrinter
end
