PRO TireView_Event, event

widget_control, event.top, get_uvalue = state
widget_control, state.draw, XSIZE = event.x, YSIZE = event.y
print,event.x, event.y
state.oWindow->Draw, state.oView

END

PRO TireViewCleanup, tlb

widget_control, tlb, get_uvalue = state
for i = 0, n_tags(state)-1 do $
	if (obj_valid(state.(i)[0])) then obj_destroy, state.(i)

END


PRO TireViewDraw, Event

widget_control, event.top, get_uvalue = state
h = state.oTrack->Update(event, TRANSFORM = new)

if (h) then begin
	state.oTireModel->GetProperty, TRANSFORM = old
	state.oTireModel->SetProperty, TRANSFORM = old # new
	state.oWindow->Draw, state.oView
endif

if (event.type eq 0 and event.clicks eq 2) then begin
	oPrinter = obj_new('IDLgrPrinter')
	a = dialog_printersetup(oPrinter)
	if (a) then begin
		oPrinter->Draw, state.oView
		oPrinter->NewDocument
	endif
	obj_destroy, oPrinter
endif


END

PRO TireView

; widgets
tlb = widget_base(TITLE = 'Tire Example', /COLUMN)
draw = widget_draw(tlb, XSIZE = 400, YSIZE = 400, $
	GRAPHICS_LEVEL = 2, /BUTTON_EVENTS, /MOTION_EVENTS, $
	EVENT_PRO = 'TireViewDraw')
widget_control, tlb, /REALIZE
widget_control, draw, GET_VALUE = oWindow

; objects
oView = obj_new('IDLgrView', COLOR = [0,127,127], $
	VIEWPLANE_RECT = [-3,-3,6,6], ZCLIP = [3,-3])
oTireModel = obj_new('IDLgrModel')
oContourModel = obj_new('IDLgrModel')

BuildTire, points, conn, ddist

oTire = obj_new('IDLgrPolygon', points, POLYGONS = conn, $
	SHADING = 1, COLOR = [255,255,255])
oLight = obj_new('IDLgrLight', LOCATION = [-2,-2,2], TYPE = 2)

oTireContour = OBJ_NEW('IDLgrContour',$
    GEOMZ=points, $
    POLYGONS=conn, $
    C_COLOR = [[255,0,0],[0,255,0],[0,0,255],[255,255,255],[0,0,0]], $
    N_LEVELS = 15)
oContourModel->Scale, 1.02, 1.02, 1.02

;oTextModel = obj_new('IDLgrModel')
;oText = obj_new('IDLgrText', 'Time to Make the Donuts!!', LOCATION = [0.0, 2.5, 0.0], $
;	ALIGN = 0.5)
;oTextModel->Add, oText
;oView->Add, oTextModel

oTireModel->Add, oTire
oTireModel->Add, oLight
oTireModel->Add, oContourModel
oContourModel->Add, oTireContour
oView->Add, oTireModel

oWindow->Draw, oView

oTrack = obj_new('Trackball', [200,200],200)

state = {oWindow:oWindow, oView:oView, $
	oTireModel:oTireModel, oContourModel:oContourModel, $
	oTire:oTire, oTireContour:oTireContour, $
	oTrack:oTrack, oLight:oLight, $
	draw:draw}

widget_control, tlb, set_uvalue = state

Xmanager, 'TireView', tlb,$
	/NO_BLOCK, CLEANUP = 'TireViewCleanup'

END