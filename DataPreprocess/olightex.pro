oView = obj_new('IDLgrView', $
                PROJECTION = 2)

oSphereModel = obj_new('IDLgrModel')
oSphere = obj_new('Orb', $
                  RADIUS = 0.5, $
                  POSITION = [0.0,0.0,0.0], $
                  COLOR = [128,128,128], $
                  STYLE = 2, $
                  DENSITY = 1.25)
oSphereModel->Add, oSphere
;oSphereModel->Scale, 1.0, 0.75, 1.0
oSphereModel->Translate, -0.3, 0.1, 0.0

oLightModel = obj_new('IDLgrModel')
oLight1 = obj_new('IDLgrLight', $
                  TYPE = 2, $
                  LOCATION = [-0.5,-0.5, 1], $
                  COLOR = [255,0,0])
oLight2 = obj_new('IDLgrLight', $
                  TYPE = 2, $
                  LOCATION = [0.0,0.0, 1], $
                  COLOR = [0,255,0])
oLight3 = obj_new('IDLgrLight', $
                  TYPE = 2, $
                  LOCATION = [0.5,0.5, 1], $
                  COLOR = [0,0,255])
oLightModel->Add, oLight1
oLightModel->Add, oLight2
oLightModel->Add, oLight3

oView->Add, oSphereModel
;oSphereModel->Add, oLightModel
oView->Add, oLightModel

oWindow = obj_new('IDLgrWindow', $
                  DIMENSIONS = [400,400])

for i = 0, 179 do begin
    oSphereModel->Rotate, [1,1,0], 2
    oWindow->Draw, oView
endfor

end
