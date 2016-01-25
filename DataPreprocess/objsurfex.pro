pro objsurfex

size = 40
peak = Shift(Dist(size),size/2+5, size/2-5)
peak = Exp(-(peak/15)^2)

oView = OBJ_NEW('IDLgrView', $
                color=255, EYE = 3, $
                ZCLIP = [1,-1])
oModel = OBJ_NEW('IDLgrModel' )
oView->Add, oModel
oSurface = OBJ_NEW('IDLgrSurface', peak, $
                   color=0, $
                   STYLE = 2, $
                   SHADING = 1)
oModel->Add, oSurface
oSurface->GetProperty, $
  XRANGE=xrange, $
  YRANGE=yrange, $
  ZRANGE=zrange
xs = [-0.5, 1/(xrange[1]-xrange[0])]
ys = [-0.5, 1/(yrange[1]-yrange[0])]
zs = [-0.5, 1/(zrange[1]-zrange[0])]
oSurface->SetProperty, $
  XCOORD_CONV=xs, $
  YCOORD_CONV=ys, $
  ZCOORD_CONV=zs
oLightModel = obj_new('IDLgrModel')
oLight = obj_new('IDLgrLight', $
                 TYPE = 2, $
                 LOCATION = [-1,2,2])
oLightModel->Add, oLight
oView->Add, oLightModel
oModel->Rotate,[1,0,0], -90
oModel->Rotate,[0,1,0], 30
oModel->Rotate,[1,0,0], 30
oXaxis = obj_new('IDLgrAxis', 0, $
                 RANGE = xrange, $
                 XCOORD_CONV = xs, $
                 YCOORD_CONV = ys, $
                 ZCOORD_CONV = zs, $
                 LOCATION = [0,0,0])
oYaxis = obj_new('IDLgrAxis', 1, $
    RANGE = yrange, $
    XCOORD_CONV = xs, $
    YCOORD_CONV = ys, $
    ZCOORD_CONV = zs, $
    LOCATION = [0,0,0])
oZaxis = obj_new('IDLgrAxis', 2, $
                 RANGE = zrange, $
                 XCOORD_CONV = xs, $
                 YCOORD_CONV = ys, $
                 ZCOORD_CONV = zs, $
                 LOCATION = [0,yrange[1],0])
oModel->Add, oXaxis
oModel->Add, oYaxis
oModel->Add, oZaxis
oWindow = OBJ_NEW('IDLgrWindow', COLOR_MODEL = 1)
oWindow->Draw, oView

;wait,3
;omodel->rotate,[1,0,0],90
;omodel->rotate,[0,0,1],120
;owindow->draw,oView
end
