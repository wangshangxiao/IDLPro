PRO widzoom_event, event


WIDGET_CONTROL, event.id, GET_UVALUE=uvalue
CASE uvalue OF
  'ZOOM': HELP, /STRUCT, event
  'DONE': WIDGET_CONTROL, event.top, /DESTROY
ENDCASE

END

PRO widzoom

device,retain=1,decomposed=0

;sele_name=dialog_pickfile(FILTER="*.tif")
;image = TIFF_READ(sele_name, R, G, B,planarconfig=2)

;sele_name=dialog_pickfile(FILTER="*.gif")
;READ_GIF,sele_name,image, R, G, B
;tvlct,R,G,B
;tv,image
sele_name=dialog_pickfile(FILTER="*.jpg")
;READ_JPEG, sele_name, image,ctable, COLORS=!D.N_COLORS-1
;TVLCT, ctable
READ_JPEG, sele_name, image,True=3

;TVLCT, r, g, b, /Get
tv,image,True=3
sz = SIZE(image)
help,image
print,sz

base=WIDGET_BASE(/COLUMN)
zoom=CW_ZOOM(base, XSIZE=sz[1], YSIZE=sz[2], UVALUE='ZOOM')
done=WIDGET_BUTTON(base, VALUE='exit', UVALUE='DONE')
WIDGET_CONTROL, base, /REALIZE

;TVLCT, ctable
;TV, image
;help,image
;a=dist(300)
;loadct,5
WIDGET_CONTROL, zoom, SET_VALUE=image
XMANAGER, 'widzoom', base

END