PRO FirstObject, data

if (n_params() ne 1) then data = dist(40)

; Create the Scene
oScene  = obj_new('IDLgrScene')

; ImageView
oImageView = obj_new('IDLgrView', UNITS = 3, $
	LOCATION = [0.0, 0.5], DIMENSION = [0.5,0.5])

; ContourView
oContourView = obj_new('IDLgrView', UNITS = 3, $
	LOCATION = [0.5, 0.5], DIMENSION = [0.5,0.5])

; SurfaceView
oSurfaceView = obj_new('IDLgrView', UNITS = 3, $
	LOCATION = [0.0, 0.0], DIMENSION = [1.0,0.5])

; Models
oImageModel = obj_new('IDLgrModel')
oContourModel = obj_new('IDLgrModel')
oSurfaceModel = obj_new('IDLgrModel')

; Graphical Atoms
oPalette = obj_new('IDLgrPalette')
oPalette->LoadCT, 5
oImage = obj_new('IDLgrImage', data, PALETTE = oPalette)
oContour = obj_new('IDLgrContour', data, N_LEVELS = 15)
oSurface = obj_new('IDLgrSurface', data, COLOR = [0,0,255])

; Build the tree
oSurfaceModel->Add, oSurface
oContourModel->Add, oContour
oImageModel->Add, oImage

oSurfaceView->Add, oSurfaceModel
oContourView->Add, oContourModel
oImageView->Add, oImageModel

oScene->Add, oSurfaceView
oScene->Add, oContourView
oScene->Add, oImageView

; Calculate coordinate conversion factors
SetCC, [oSurface, oContour, oImage]

; Rotate Surface
oSurfaceModel->Rotate, [1,0,0], -90
oSurfaceModel->Rotate, [0,1,0], 45
oSurfaceModel->Rotate, [1,0,0], 40

; Destination object
oWindow = obj_new('IDLgrWindow', DIM = [400,400], TITLE = 'First Object')

; Render
oWindow->Draw, oScene

END





