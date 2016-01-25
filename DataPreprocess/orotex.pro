zData = BESELJ(SHIFT(DIST(40),20,20)/2,0)

oView = Obj_New('IDLgrView', $
                VIEWPLANE_RECT = [-1, -1, 2.0, 2.0], $
                ZCLIP = [4,-4], $
                COLOR = 255)

oModel = Obj_New('IDLgrModel')
oContourModel = obj_new('idlgrmodel')

oSurface = Obj_New('IDLgrSurface', zData, $
                   XCOORD_CONV = [-0.5, 1.0 / (float(n_elements(zData[*,0]))-1.0)], $
                   YCOORD_CONV = [-0.5, 1.0 / (float(n_elements(zData[0,*]))-1.0)], $
                   ZCOORD_CONV = norm_coord([min(zData), max(zData)]), $
                   COLOR = [0,255,128])
oContour = obj_new('IDLgrContour', zData, $
                   N_LEVELS = 20, $
                   XCOORD_CONV = [-0.5, 1.0 / (float(n_elements(zData[*,0]))-1.0)], $
                   YCOORD_CONV = [-0.5, 1.0 / (float(n_elements(zData[0,*]))-1.0)], $
                   ZCOORD_CONV = norm_coord([min(zData), max(zData)]), $
                   /PLANAR, $
                   /FILL, $
                   GEOMZ = (max(zData) - min(zData)) / 2.0, $
                   C_COLOR = [[255,0,0], [0,255,0], [0,0,255], [255,255,255], [0,64,255]])
ocontourModel->add, ocontour
oModel->add, ocontourmodel

oTextModel = obj_new('IDLgrModel')
oText = obj_new('IDLgrText', LOCATION = [-0.9, -0.9, 0.0])
oTextModel->Add, oText

oModel->Add, oSurface
oView->Add, oModel
oView->Add, oTextModel

oPalette = obj_new('IDLgrPalette')
oPalette->LoadCT, 0

oWindow = obj_new('IDLgrWindow', $
                  DIM = [400,400], $
                  COLOR_MODEL = 0, $
                  PALETTE = oPalette, $
                  TITLE = 'Rotation Example')

for i = 0,19 do begin

    oModel->Rotate, [0,1,0], i * 10
    oText->SetProperty, $
      STRINGS = 'ROTATION STEP: '+strtrim(i+1,2)
    oWindow->Draw, oView
endfor

for i = 0,19 do begin

    oContourModel->Rotate, [0,0,1], 12
    oText->SetProperty, $
      STRINGS = 'ROTATION STEP: '+strtrim(i+1,2)
    oWindow->Draw, oView
endfor
end
