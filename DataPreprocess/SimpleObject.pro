PRO SimpleObject

oView = obj_new('IDLgrView', VIEWPLANE_RECT = [-10,-2,380,4])
oModel = obj_new('IDLgrModel')
oPlot = obj_new('IDLgrPlot', sin(findgen(360)*!DtoR), THICK = 3)

oView->Add, oModel
oModel->Add, oPlot

oWindow = obj_new('IDLgrWindow', TITLE = 'The Simplest OG Plot')
oWindow->Draw, oView

END