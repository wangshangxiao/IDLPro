; create a view object
oView = obj_new('IDLgrView', $
                ZCLIP = [4,-4], $
                EYE = 5)

; create 4 models, one for the fixed light
; and three individual models for the
; spheres
oLightModel = obj_new('IDLgrModel')
oModel1 = obj_new('IDLgrModel')
oModel2 = obj_new('IDLgrModel')
oModel3 = obj_new('IDLgrModel')

; create the sphere objects of different
; positions and radius
oSphere1 = obj_new('Orb', $
                   RADIUS = .2, $
                   POS = [-.1, -.1, 0.0], $
                   COLOR = [255,128,0])
oSphere2 = obj_new('Orb', $
                   RADIUS = .3, $
                   POS = [.2, .2, 0.0], $
                   COLOR = [0,128,255])
oSphere3 = obj_new('Orb', $
                   RADIUS = .4, $
                   POS = [.6, .6, 0.0], $
                   COLOR = [255,128,255])

; a light object for shading
oLight = obj_new('IDLgrLight', $
                 TYPE = 2, $
                 LOCATION = [-.5, -.5, 1])

; create a hierarchy of models
; within models
oLightModel->Add, oLight
oModel1->Add, oSphere1
oModel1->Add, oModel2
oModel2->Add, oSphere2
oModel2->Add, oModel3
oModel3->Add, oSphere3

oView->Add, oLightModel
oView->Add, oModel1

oWindow = obj_new('IDLgrWindow')
oWindow->Draw, oView

; create a buffer to store the
; frames for the MPEG
oBuffer = obj_new('IDLgrBuffer')

; create the MPEG object
oMpeg = obj_new('IDLgrMpeg', $
                DIMENSIONS = [250,250], $
                FILE = 'Spheres.mpg')

; perform some rotation and write
; MPEG frames
for i = 0, 4 do begin
    oModel1->Rotate, [1,0,0], 10
    oBuffer->Draw, oView
    oWindow->Draw, oView
    oMpeg->put, oBuffer->Read()
    oModel2->Rotate, [0,1,0], 10
    oBuffer->Draw, oView
    oWindow->Draw, oView
    oMpeg->put, oBuffer->Read()
    oModel3->Rotate, [0,0,1], 10
    oBuffer->Draw, oView
    oWindow->Draw, oView
    oMpeg->put, oBuffer->Read()
endfor

oText = obj_new('IDLgrText', $
                'WRITING MPEG FILE .... ', $
                LOCATION = [-0.9,-0.9,0.0] )
oLightModel->Add, oText
oWindow->Draw, oView

; write the mpeg to a file
oMpeg->Save

oText->SetProperty, STRINGS = 'WRITING MPEG FILE .... Done'
oWindow->Draw, oView

END
