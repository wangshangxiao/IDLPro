;return value: success=1,failed=0
function attachshape, oShapeModal, oShape, oSymbol, oFont
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return, 0
	ENDIF

;	sometimes the event handler has no option but to perform an operation that is slow.
;	In such a case, it is a good idea to give the user feedback that the system is busy.
	WIDGET_CONTROL, /HOURGLASS

	if obj_valid(oShapeModal) eq 0 or obj_valid(oShape) eq 0 then return, 0

	oLabelModal = oShapeModal->GetByName('oLabelModal')

	oShape->GetProperty, N_ENTITIES=num_ent, ENTITY_TYPE=ShapeType, ATTRIBUTE_NAMES=aname
	for i=0, num_ent-1 do begin
		ent = oShape->GetEntity(i, /ATTRIBUTES)

		text = ''
		;find the string tags.
		if size((*ent.ATTRIBUTES), /type) eq 8 then begin
			for tag_i=0, n_tags(*ent.ATTRIBUTES)-1 do begin
				if size((*ent.ATTRIBUTES).(tag_i), /type) eq 7 then begin
					text = (*ent.ATTRIBUTES).(tag_i)
				endif
			endfor
		endif

		if ent.N_PARTS eq 0 then begin
			case ShapeType of
				1:begin	;point目前无法显示，原因待查
					oPlots = OBJ_NEW('IDLgrPlot',[ent.BOUNDS[0],ent.BOUNDS[0]],[ent.BOUNDS[1],ent.BOUNDS[1]],color=[0,0,0],SYMBOL=oSymbol, uvalue={text:text})
					oPlots->SetProperty, uvalue={text:text,oAtomic:oPlots}
					oText = OBJ_NEW('IDLgrText',text, LOCATIONS=ent.BOUNDS[0:1],FONT=oFont)
					oLabelModal->Add,oPlots
					oLabelModal->Add,oText
				end
				else:begin	;暂时不做处理
				end
			endcase
		endif

		for j=0, ent.N_PARTS-1 do begin
			if(j ne ent.N_PARTS-1) then begin
				xlist = reform((*ent.vertices)[0,(*ent.PARTS)[j]:(*ent.PARTS)[j+1]-1]);[0, *])
				ylist = reform((*ent.vertices)[1,(*ent.PARTS)[j]:(*ent.PARTS)[j+1]-1]);[1, *])
			endif else begin
				xlist = reform((*ent.vertices)[0,(*ent.PARTS)[j]:*]);[0, *])
				ylist = reform((*ent.vertices)[1,(*ent.PARTS)[j]:*]);[1, *])
			endelse

			case ShapeType of
				1:begin	;point
					oPlots = OBJ_NEW('IDLgrPlot',xlist,ylist,color=[0,0,0],thick=1,LINESTYLE=6,uvalue={text:text})
					oShapeModal->Add,oPlots
				end
				3:begin	;PolyLine
					oPolyline = OBJ_NEW('IDLgrPolyline',xlist,ylist,color=[0,0,0],thick=1,LINESTYLE=0,uvalue={text:text})
					oShapeModal->Add,oPolyline
				end
				5:begin	;Polygon
					if size((*ent.ATTRIBUTES).(0), /type) eq 3 then begin
						county_code = strtrim((*ent.ATTRIBUTES).(0),2)
					endif else begin
						county_code = '0'
					endelse

					oPolyline = OBJ_NEW('IDLgrPolygon',xlist,ylist,STYLE=1,color=[0,0,0],THICK=1,LINESTYLE=0,name=county_code)
					oShapeModal->Add,oPolyline

					if text ne '' then begin
						oTess = OBJ_NEW('IDLgrTessellator')
						oTess->AddPolygon,xlist, ylist
						;其作用是将凹多形转成凸多边形.x,y须是一行值.
						result = oTess->Tessellate(v, c)
						OBJ_DESTROY,oTess

						if(result eq 1) then begin
							oPolygon = OBJ_NEW('IDLgrPolygon',v,POLYGONS=c,STYLE=1,LINESTYLE=6,ALPHA_CHANNEL=0.0,uvalue={text:text,oPolyline:oPolyline})
							oShapeModal->Add,oPolygon
						endif
					endif	;if text ne '' then begin
				end
				8:begin	;MultiPoint
					oPlots = OBJ_NEW('IDLgrPlot',xlist,ylist,color=[0,0,0],thick=1,LINESTYLE=6,uvalue={text:text})
					oShapeModal->Add,oPlots
				end
				else:begin	;暂时不做处理
				end
			endcase	;case ShapeType of
		endfor	;for j=0, ent.N_PARTS-1 do begin
		oShape->DestroyEntity, ent
	endfor	;for i=0, num_ent-1 do begin

    OBJ_DESTROY, oShape
    return, 1
end

pro display, pstaff

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	;获取图像显示的区域范围
	base_id = (*pstaff).base_id
	GEOMETRY = WIDGET_INFO(base_id, /GEOMETRY)
	cmdbase_height = 43
	xsize = GEOMETRY.SCR_XSIZE
	ysize = GEOMETRY.SCR_YSIZE - cmdbase_height

	draw_id = widget_draw(base_id, uname='draw', XSIZE=xsize, YSIZE=ysize, $
		GRAPHICS_LEVEL=2, RETAIN=2, $
		/BUTTON_EVENTS, /motion_events, event_pro='dealwithMotion',KILL_NOTIFY='display_cleanup')
	value_text = widget_label(base_id,value='', SCR_XSIZE=xsize, SCR_YSIZE=cmdbase_height/2.5, uname='value_text')

	cmd_base = widget_base(base_id, uname='cmd_base', SCR_XSIZE=xsize, SCR_YSIZE=cmdbase_height/2, /row, /BASE_ALIGN_CENTER,/ALIGN_CENTER, sensitive=0,XPAD=100)

	ratio_base = widget_base(cmd_base,uname='ratio_base',/row,/Exclusive)
	roam_btn = widget_button(ratio_base, TOOLTIP='漫游', uname='roam_btn', EVENT_PRO='change_cursor', /BITMAP, VALUE='.\Image\hand.bmp')
	zin_btn = widget_button(ratio_base, TOOLTIP='放大', uname='zin_btn', EVENT_PRO='change_cursor', /BITMAP, VALUE='.\Image\zoom_in.bmp')
	zout_btn = widget_button(ratio_base, TOOLTIP='缩小', uname='zout_btn', EVENT_PRO='change_cursor', /BITMAP, VALUE='.\Image\zoom_out.bmp')
	widget_control,roam_btn,/SET_BUTTON

	reset_btn = widget_button(cmd_base, TOOLTIP='复位', uname='reset_btn', EVENT_PRO='zoom_image', /BITMAP, VALUE='.\Image\globe_7.bmp')
	zoomin_btn = widget_button(cmd_base, TOOLTIP='固定放大', uname='zoomin_btn', EVENT_PRO='zoom_image', /BITMAP, VALUE='.\Image\zoom_in_page_3.bmp')
	zoomout_btn = widget_button(cmd_base, TOOLTIP='固定缩小', uname='zoomout_btn', EVENT_PRO='zoom_image', /BITMAP, VALUE='.\Image\zoom_out_page_3.bmp')
	add_btn = widget_button(cmd_base, TOOLTIP='添加矢量', uname='add_btn', EVENT_PRO='add_shape', /BITMAP, VALUE='.\Image\layer_with_arrow_1.bmp')
	save_btn = widget_button(cmd_base, TOOLTIP='保存', uname='save_btn', EVENT_PRO='save_image', /BITMAP, VALUE='.\Image\save.bmp')

	WIDGET_CONTROL, base_id, /REALIZE

	register_display_cursor, 'Hand', '.\Image\hand1.bmp'
	register_display_cursor, 'ZoomIn', '.\Image\zoom_in1.bmp'
	register_display_cursor, 'ZoomOut', '.\Image\zoom_out1.bmp'

	refresh, pstaff

end

pro refresh, pstaff
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	base_id = (*pstaff).base_id
	class_tag=(*pstaff).class
	level_num=n_elements((*pstaff).palette)/3

	image = (*pstaff).image
	palette = (*pstaff).palette
	shapefile = (*pstaff).shapefile
	legend = (*pstaff).legend
	title = (*pstaff).title

	draw_id = widget_info(base_id, FIND_BY_UNAME='draw')
	cmd_base = widget_info(base_id, FIND_BY_UNAME='cmd_base')

	GEOMETRY = WIDGET_INFO(draw_id, /GEOMETRY)
	xsize = GEOMETRY.SCR_XSIZE
	ysize = GEOMETRY.SCR_YSIZE

	widget_control, draw_id, get_uvalue=obj_list
	if size(obj_list,/TYPE) eq 8 then begin
		if obj_valid(obj_list.oView) eq 1 then begin
			OBJ_DESTROY, [obj_list.oView, obj_list.oPalette, obj_list.oSymbol, obj_list.oFont]
		endif
	endif

	widget_control, draw_id, get_value = oWindow

	oView = obj_new('IDLgrview', VIEWPLANE_RECT=[0.0,0.0,xsize,ysize])

	oImageModal = obj_new('IDLgrModel',name='oImageModal', SELECT_TARGET=1)
	oShapeModal = obj_new('IDLgrModel',name='oShapeModal', SELECT_TARGET=0)
	oLabelModal = obj_new('IDLgrModel',name='oLabelModal', SELECT_TARGET=1)
	oBarModal = obj_new('IDLgrModel',name='oBarModal', SELECT_TARGET=1)
	oTextModal = obj_new('IDLgrModel',name='oTextModal', SELECT_TARGET=1)
	oRectModal = obj_new('IDLgrModel',name='oRectModal', SELECT_TARGET=1)

	oView->Add, oImageModal
	oView->Add, oShapeModal
	oShapeModal->Add, oLabelModal
	oView->Add, oBarModal
	oView->Add, oTextModal
	oView->Add, oRectModal

	oSymbol = OBJ_NEW('IDLgrSymbol',1,color=[0,0,0],size=3000,thick=2)
	oFont = OBJ_NEW('IDLgrFont', '宋体', size=9)
	oPalette = OBJ_NEW('IDLgrPalette')
	case size(palette, /type) of
		2: begin
			oPalette -> LoadCT, palette
			oPalette -> SetRGB, 0, 255B, 255B, 255B
		end
		1: begin
			oPalette -> SetProperty, RED_VALUES = (transpose(palette))[*,0],GREEN_VALUES = (transpose(palette))[*,1], BLUE_VALUES = (transpose(palette))[*,2]
		end
		else:begin
		end
	endcase

	if size(*image,/type) ne 0 then begin
		image_data = *image
		image_size = size(image_data,/DIMENSIONS)
		;以widget_draw短的一边为标准，缩放图像至适应widget_draw大小，同时保持图像高宽比不变
		wh_ratio_image = image_size[0]*1.0/image_size[1]
		wh_ratio_draw = xsize * 1.0 / ysize
		if(wh_ratio_image gt wh_ratio_draw) then begin
			scale_v = xsize * 1.0 / image_size[0]
		endif else begin
			scale_v = ysize * 1.0 / image_size[1]
		endelse
		xoffset = (xsize - image_size[0]*scale_v) / 2.0
		yoffset = (ysize - image_size[1]*scale_v) / 2.0
		scalebar=scale_v
		oImage = obj_new('IDLgrimage',image_data, palette=oPalette, /ORDER, name='oImage')
		oImageModal->Add, oImage
		oImageModal->reset
		oImageModal->scale, scale_v, scale_v, 1
		oImageModal->Translate, xoffset, yoffset, 0

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		oShape = obj_new('IDLffShape', shapefile)
		if attachshape(oShapeModal, oShape, oSymbol, oFont) eq 1 then begin
	      if(wh_ratio_image gt wh_ratio_draw) then begin
				scale_v = xsize / ((*pstaff).xsize * (*pstaff).pixelsize)
			endif else begin
				scale_v = ysize / ((*pstaff).ysize * (*pstaff).pixelsize)
			endelse
	        oShapeModal->reset
			oShapeModal->Translate, -(*pstaff).startx, -((*pstaff).starty-((*pstaff).ysize * (*pstaff).pixelsize)), 0
			oShapeModal->scale, scale_v, scale_v, 1
			oShapeModal->Translate, xoffset, yoffset, 0
		endif

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		oScaleline1 = OBJ_NEW('IDLgrPolyline',[225,325],[30,30], $
			color=[0,0,0],THICK=1,LINESTYLE=0,name='oScaleline1',HIDE=0)

		oScaleText1=obj_new('idlgrtext',strtrim(string(0),2),$
				locations=[222,15],name='oScaleText1')
		oScaleline2 = OBJ_NEW('IDLgrPolyline',[225,225],[30,40], $
			color=[0,0,0],THICK=1,LINESTYLE=0,name='oScaleline2',HIDE=0)

		oScaleline3 = OBJ_NEW('IDLgrPolyline',[250,250],[30,37], $
			color=[0,0,0],THICK=1,LINESTYLE=0,name='oScaleline3',HIDE=0)

		oScaleText2=obj_new('idlgrtext',strtrim(string(51/scalebar,format='(F7.1)'),2),$
				locations=[260,15],name='oScaleText2')
		oScaleline4 = OBJ_NEW('IDLgrPolyline',[275,275],[30,40], $
			color=[0,0,0],THICK=1,LINESTYLE=0,name='oScaleline4',HIDE=0)

		oScaleline5 = OBJ_NEW('IDLgrPolyline',[300,300],[30,37], $
			color=[0,0,0],THICK=1,LINESTYLE=0,name='oScaleline5',HIDE=0)

		oScaleText3=obj_new('idlgrtext',strtrim(string(102/scalebar,format='(F7.1)'),2),$
				locations=[302,15],name='oScaleText3')
		oScaleline6 = OBJ_NEW('IDLgrPolyline',[325,325],[30,40], $
			color=[0,0,0],THICK=1,LINESTYLE=0,name='oScaleline6',HIDE=0)

		oScaleText6=obj_new('idlgrtext','km',$
				locations=[338,15])

		oRectModal -> add,oScaleline1
		oRectModal -> add,oScaleline2
		oRectModal -> add,oScaleline3
		oRectModal -> add,oScaleline4
		oRectModal -> add,oScaleline5
		oRectModal -> add,oScaleline6

		oTextModal->Add, oScaleText1
		oTextModal->Add, oScaleText2
		oTextModal->Add, oScaleText3
		oTextModal->Add, oScaleText6

		;	指北针
		north_arrow1=obj_new('idlgrpolygon',[7,7,5]*xsize/60,[55,60,53]*ysize/66,color=[0,0,0])
		north_arrow2=obj_new('idlgrpolygon',[9,7,7]*xsize/60,[53,60,55]*ysize/66,color=[0,0,0],style=1)

		afont=obj_new('idlgrfont','times*bold',size=xsize/30)
		aText=obj_new('idlgrtext','N',location=[7*xsize/60,60*ysize/66],alignment=0.5,font=afont)

		oRectModal->add,north_arrow1
		oRectModal->add,north_arrow2
		oRectModal->add,aText

		if class_tag eq 1 then begin
			barDims = [xsize/40, ysize/3]
			oColorbar = OBJ_NEW('IDLgrColorbar', palette=oPalette,$
			   DIMENSIONS=barDims, /SHOW_OUTLINE,SUBTICKLEN=0,ticklen=0)
			oBarModal->Add, oColorbar
			oBarModal->Translate, xsize/40*38, ysize/40, 0

			inc=(ysize/3)/5
			oText=objarr(5)
			for i=0,level_num-1,1 do begin
				oText[i]=obj_new('idlgrtext',strtrim(string(i),2),palette=oPalette,$
				locations=[(xsize/40*38-15),(ysize/40+0.6*inc/2+i*inc)])
			endfor
			oTextModal->Add, oText
		endif else begin
			barDims = [xsize/40, ysize/3]
			oColorbar = OBJ_NEW('IDLgrColorbar', palette=oPalette,$
			   DIMENSIONS=barDims, /SHOW_AXIS, /SHOW_OUTLINE)
			oBarModal->Add, oColorbar
			oBarModal->Translate, xsize/40*38, ysize/40, 0
		endelse
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		oRectline = OBJ_NEW('IDLgrPolygon',[0,0,0,0],[0,0,0,0],STYLE=1,color=[0,0,0],THICK=1,LINESTYLE=0,name='oRectline',HIDE=1)
		oRectModal->Add,oRectline

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		widget_control, cmd_base, sensitive=1
	endif else begin	;if size(*image,/type) ne 0 then begin
		widget_control, cmd_base, sensitive=0
	endelse

	oWindow->setproperty, GRAPHICS_TREE = oView
	oWindow->Draw, oView

	obj_list = {oView:oView, oPalette:oPalette, oSymbol:oSymbol, oFont:oFont, pstaff:pstaff, moveable:0, lx:0, ly:0, cursor_state:0}
	;cursor_state 0:roam,1:zoomin,2:zoomout
	widget_control, draw_id, set_uvalue=obj_list

	roam_btn = widget_info(cmd_base, FIND_BY_UNAME='roam_btn')
	widget_control,roam_btn,/SET_BUTTON
	oWindow->SetCurrentCursor,'CROSSHAIR'
end

pro dealwithMotion, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	draw_id = WIDGET_INFO(event.top ,FIND_BY_UNAME='draw')

	widget_control, draw_id, get_value = oWindow
	widget_control, draw_id, get_uvalue = obj_list
	oImage = obj_list.oView->GetByName('oImageModal/oImage')
	if obj_valid(oImage) eq 0 then begin
		return
	endif

	if obj_list.cursor_state eq 0 then begin
		case event.type of
		0:begin	;button press
			if event.press eq 1 then begin	;left button press
				oWindow->SetCurrentCursor, 'Hand'
				obj_list.moveable = 1
				obj_list.lx = event.x
				obj_list.ly = event.y
				widget_control, draw_id, set_uvalue = obj_list
			endif
			if event.press eq 1 and event.clicks eq 2 then begin	;left button double click
				select = oWindow->Select(obj_list.oView, [event.x, event.y],ORDER=1)
				if obj_valid(select[0]) eq 1 then begin
					ncounts = n_elements(select)
					for i=0, ncounts-1 do begin
						select[i]->getproperty, uvalue = uv
						if size(uv,/type) ne 0 then begin
							if size(uv.oPolyline, /type) eq 11 then begin
								uv.oPolyline->GetProperty,color=v_color,THICK=v_thick
								if array_equal(v_color, [0,0,0]) then begin
									v_color = [0,255,255]
									v_thick = 3
									uv.oPolyline->GetProperty,name=county_code
									select_table, event, county_code
								endif else begin
									v_color = [0,0,0]
									v_thick = 1
								endelse
								uv.oPolyline->SetProperty,color=v_color,THICK=v_thick
							endif	;if size(uv.oPolyline, /type) eq 11 then begin
						endif	;if size(uv,/type) ne 0 then begin
					endfor	;for i=0, ncounts-1 do begin
					oWindow->Draw, obj_list.oView
				endif	;if obj_valid(select[0]) eq 1 then begin
			endif	;if event.press eq 1 and event.clicks eq 2 then begin
		end	;0:begin
		1:begin	;button release
			if event.release eq 1 then begin	;left button release
				oWindow->SetCurrentCursor, 'CROSSHAIR'
				obj_list.moveable = 0
				obj_list.lx = 0
				obj_list.ly = 0
				widget_control, draw_id, set_uvalue = obj_list
			endif
		end	;1:begin
		2:begin	;motion
			if obj_list.moveable eq 0 then begin	;moveable为0是漫游，为1是拖动
				oShapeModal = obj_list.oView->GetByName('oShapeModal')
				oShapeModal->GetProperty, TRANSFORM=matrix
				xlabel = (event.x - matrix[3,0]) / matrix[0,0]
				ylabel = (event.y - matrix[3,1]) / matrix[1,1]

				startx = (*(obj_list.pstaff)).startx
				starty = (*(obj_list.pstaff)).starty
				pixelsize = (*(obj_list.pstaff)).pixelsize

				image_x_cor = (xlabel - startx) / pixelsize
				image_y_cor = (starty - ylabel) / pixelsize
				xsize  = FIX((*(obj_list.pstaff)).xsize)
				ysize  = FIX((*(obj_list.pstaff)).ysize)

				oImage->GetProperty, DATA=image_data
				value_text = WIDGET_INFO(event.top ,FIND_BY_UNAME='value_text')
				if image_x_cor ge 0 and image_x_cor lt xsize and image_y_cor ge 0 and image_y_cor lt ysize then begin
					widget_control, value_text, set_value = ' X:'+strtrim(xlabel,2)+',Y:'+strtrim(ylabel,2)+',Value:'+strtrim(string(image_data[image_x_cor,image_y_cor],/PRINT),2)
				endif else begin
					widget_control, value_text, set_value = ' X:'+strtrim(xlabel,2)+',Y:'+strtrim(ylabel,2)+',Value:非图像区域'
				endelse

				select = oWindow->Select(obj_list.oView, [event.x, event.y],ORDER=1)
				if obj_valid(select[0]) eq 1 then begin
					ncounts = n_elements(select)
					for i=0, ncounts-1 do begin
						select[i]->getproperty, uvalue = uv
						if size(uv, /type) ne 0 then begin
							widget_control, value_text, get_value = text
							widget_control, value_text, set_value = text + '	' +uv.text
						endif
						;help, uv, /struc
					endfor
				endif
			endif else begin	;if obj_list.moveable eq 0 then begin
				xoffset = event.x - obj_list.lx
				yoffset	= event.y - obj_list.ly
				widget_control, draw_id, get_value = oWindow
				oImageModal = obj_list.oView->GetByName('oImageModal')
				oShapeModal = obj_list.oView->GetByName('oShapeModal')
				oImageModal->Translate, xoffset, yoffset, 0
				oShapeModal->Translate, xoffset, yoffset, 0
				oWindow->Draw, obj_list.oView
				obj_list.lx = event.x
				obj_list.ly = event.y
				widget_control, draw_id, set_uvalue = obj_list
			endelse	;endif else begin
		end	;2:begin
		else:begin
		end
		endcase
	endif else begin	;if obj_list.cursor_state eq 0 then begin
	oRectline = obj_list.oView->GetByName('oRectModal/oRectline')
	case event.type of
		0:begin	;button press
			if event.press eq 1 then begin	;left button press
				obj_list.moveable = 1
				oRectline -> SetProperty,hide=0
				rect=[[0,0,0,0], [0,0,0,0]]
				rect = transpose(rect)
				oRectline -> SetProperty, DATA=rect
				obj_list.lx = event.x
				obj_list.ly = event.y
				widget_control, draw_id, set_uvalue = obj_list
			endif
			if event.press eq 1 and event.clicks eq 2 then begin	;left button double click
				oTextModal = obj_list.oView->GetByName('oTextModal')
				oScaleText2 = oTextModal->GetByName('oScaleText2')
				oScaleText3 = oTextModal->GetByName('oScaleText3')

				oScaleText2 -> GetProperty,strings=scale2

				if obj_list.cursor_state eq 1 then begin
					if scale2 gt 0.5 then begin
						scale_v = 1.15
					endif else begin
						scale_v = 1
					endelse
				endif else begin
					if scale2 lt 500 then begin
						scale_v = 0.85
					endif else begin
						scale_v = 1
					endelse
				endelse
				oImageModal = obj_list.oView->GetByName('oImageModal')
				oShapeModal = obj_list.oView->GetByName('oShapeModal')
				oImageModal->scale, scale_v, scale_v, 1
				oShapeModal->scale, scale_v, scale_v, 1
				xoffset = -event.x * (scale_v-1)
				yoffset = -event.y * (scale_v-1)
				oImageModal->Translate, xoffset, yoffset, 0
				oShapeModal->Translate, xoffset, yoffset, 0

				oTextModal = obj_list.oView->GetByName('oTextModal')
				oScaleText2 = oTextModal->GetByName('oScaleText2')
				oScaleText3 = oTextModal->GetByName('oScaleText3')

				oScaleText2 -> GetProperty,strings=scale2
				scale_bar2=float(scale2)/scale_v
				oScaleText2 -> SetProperty,strings=strtrim(string(scale_bar2,format='(F7.1)'),2)

				oScaleText3 -> GetProperty,strings=scale3
				scale_bar3=scale_bar2*2
				oScaleText3 -> SetProperty,strings=strtrim(string(scale_bar3,format='(F7.1)'),2)
			endif	;if event.press eq 1 and event.clicks eq 2 then begin

			oWindow->Draw, obj_list.oView
		end	;0:begin
		1:begin	;button release
			if event.release eq 1 then begin	;left button release
				obj_list.moveable = 0
				oRectline -> SetProperty,hide=1
				obj_list.lx = 0
				obj_list.ly = 0
				widget_control, draw_id, set_uvalue = obj_list
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				oRectline -> GetProperty, DATA=rect
				xsize=abs(rect[0,2]-rect[0,0])
				ysize=abs(rect[1,2]-rect[1,0])
				if xsize eq 0 or ysize eq 0 then return
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				oImageModal = obj_list.oView->GetByName('oImageModal')
				oShapeModal = obj_list.oView->GetByName('oShapeModal')
				oImage = oImageModal->GetByName('oImage')
				oImage -> GetProperty, DIMENSIONS=image_size
				oImageModal->GetProperty, TRANSFORM=image_matrix_old

				GEOMETRY = WIDGET_INFO(draw_id, /GEOMETRY)
				xsize_d = GEOMETRY.SCR_XSIZE
				ysize_d = GEOMETRY.SCR_YSIZE

				wh_ratio_rect = xsize * 1.0 / ysize
				wh_ratio_draw = xsize_d * 1.0 / ysize_d

				oTextModal = obj_list.oView->GetByName('oTextModal')
				oScaleText2 = oTextModal->GetByName('oScaleText2')
				oScaleText3 = oTextModal->GetByName('oScaleText3')

				oScaleText2 -> GetProperty,strings=scale2

				if(wh_ratio_rect gt wh_ratio_draw) then begin
					scale_v = xsize_d * 1.0 / xsize
				endif else begin
					scale_v = ysize_d * 1.0 / ysize
				endelse

				if obj_list.cursor_state eq 2 then begin
					scale_v = 1.0 / (-1.0 / scale_v + 2.0)
					; x=1,		y=1
					; x->无穷,	y->1/2
					; y=1 / (-1/x + 2)
				endif

				if (scale_v gt 1) and (scale2 lt 0.5) then begin
					scale_v = 1
				endif

				if (scale_v lt 1) and (scale2 gt 500) then begin
					scale_v = 1
				endif

				scalebar=scale_v

				image_matrix_new = image_matrix_old
				image_matrix_new[0:1,0:1] *= scale_v
				xoffset = min(rect[0,*])
				yoffset = min(rect[1,*])

				if obj_list.cursor_state eq 1 then begin
					image_matrix_new[3,0:1] -= [xoffset,yoffset]
					image_matrix_new[3,0:1] *= scale_v
				endif else if obj_list.cursor_state eq 2 then begin
					image_matrix_new[3,0:1] += [xoffset,yoffset]
					image_matrix_new[3,0:1] *= scale_v
				endif

				oImageModal->SetProperty, TRANSFORM=image_matrix_new
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				pstaff = obj_list.pstaff
				palette = (*pstaff).palette
				xsize_o = image_size[0]*image_matrix_new[0,0]
				ysize_o = image_size[1]*image_matrix_new[1,1]
				if(wh_ratio_rect gt wh_ratio_draw) then begin
					scale_v = xsize_o / ((*pstaff).xsize * (*pstaff).pixelsize)
				endif else begin
					scale_v = ysize_o / ((*pstaff).ysize * (*pstaff).pixelsize)
				endelse

				if (scale_v gt 1) and (scale2 lt 0.5) then begin
					scale_v = 1
				endif

				if (scale_v lt 1) and (scale2 gt 500) then begin
					scale_v = 1
				endif

				oShapeModal->reset
				oShapeModal->Translate, -(*pstaff).startx, -((*pstaff).starty-((*pstaff).ysize * (*pstaff).pixelsize)), 0
				oShapeModal->scale, scale_v, scale_v, 1
				oShapeModal->Translate, image_matrix_new[3,0], image_matrix_new[3,1], 0
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

				scale_bar2=float(scale2)/(scalebar)
				oScaleText2 -> SetProperty,strings=strtrim(string(scale_bar2,format='(F7.1)'),2)

				oScaleText3 -> GetProperty,strings=scale3
				scale_bar3=scale_bar2*2
				oScaleText3 -> SetProperty,strings=strtrim(string(scale_bar3,format='(F7.1)'),2)


				oWindow->Draw, obj_list.oView
			endif
		end	;1:begin
		2:begin	;motion
			if obj_list.moveable eq 1 then begin	;moveable在缩放状态下含义与漫游状态下不同，0是不显示矩形，1是显示矩形
				rect = [[obj_list.lx, obj_list.lx, event.x, event.x], $
						[obj_list.ly, event.y, event.y, obj_list.ly]]
				rect = transpose(rect)
				oRectline -> SetProperty, DATA=rect

				oWindow->Draw, obj_list.oView
			endif

			oShapeModal = obj_list.oView->GetByName('oShapeModal')
			oShapeModal->GetProperty, TRANSFORM=matrix
			xlabel = (event.x - matrix[3,0]) / matrix[0,0]
			ylabel = (event.y - matrix[3,1]) / matrix[1,1]

			startx = (*(obj_list.pstaff)).startx
			starty = (*(obj_list.pstaff)).starty
			pixelsize = (*(obj_list.pstaff)).pixelsize

			image_x_cor = (xlabel - startx) / pixelsize
			image_y_cor = (starty - ylabel) / pixelsize
			xsize  = FIX((*(obj_list.pstaff)).xsize)
			ysize  = FIX((*(obj_list.pstaff)).ysize)

			oImage->GetProperty, DATA=image_data
			value_text = WIDGET_INFO(event.top ,FIND_BY_UNAME='value_text')
			if image_x_cor ge 0 and image_x_cor lt xsize and image_y_cor ge 0 and image_y_cor lt ysize then begin
				widget_control, value_text, set_value = ' X:'+strtrim(xlabel,2)+',Y:'+strtrim(ylabel,2)+',Value:'+strtrim(string(image_data[image_x_cor,image_y_cor],/PRINT),2)
			endif else begin
				widget_control, value_text, set_value = ' X:'+strtrim(xlabel,2)+',Y:'+strtrim(ylabel,2)+',Value:未知'
			endelse

			select = oWindow->Select(obj_list.oView, [event.x, event.y],ORDER=1)
			if obj_valid(select[0]) eq 1 then begin
				ncounts = n_elements(select)
				for i=0, ncounts-1 do begin
					select[i]->getproperty, uvalue = uv
					if size(uv, /type) ne 0 then begin
						widget_control, value_text, get_value = text
						widget_control, value_text, set_value = text + '	' +uv.text
					endif
					;help, uv, /struc
				endfor
			endif
		end	;2:begin
		else:begin
		end
	endcase
endelse

end

pro change_cursor, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	draw_id = WIDGET_INFO(event.top ,FIND_BY_UNAME='draw')
	widget_control, draw_id, get_value = oWindow
	widget_control, draw_id, get_uvalue = obj_list

	case event.id of
		WIDGET_INFO(event.top ,FIND_BY_UNAME='roam_btn'):begin
			oWindow->SetCurrentCursor, 'CROSSHAIR'
			obj_list.cursor_state = 0
		end
		WIDGET_INFO(event.top ,FIND_BY_UNAME='zin_btn'):begin
			oWindow->SetCurrentCursor, 'ZoomIn'
			obj_list.cursor_state = 1
		end
		WIDGET_INFO(event.top ,FIND_BY_UNAME='zout_btn'):begin
			oWindow->SetCurrentCursor, 'ZoomOut'
			obj_list.cursor_state = 2
		end
		else:begin
			return
		end
	endcase

	widget_control, draw_id, set_uvalue = obj_list
end

pro zoom_image, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	WIDGET_CONTROL, /HOURGLASS

	draw_id = WIDGET_INFO(event.top ,FIND_BY_UNAME='draw')
	widget_control, draw_id, get_value = oWindow
	widget_control, draw_id, get_uvalue = obj_list

	oTextModal = obj_list.oView->GetByName('oTextModal')
	oScaleText2 = oTextModal->GetByName('oScaleText2')
	oScaleText3 = oTextModal->GetByName('oScaleText3')

	oScaleText2 -> GetProperty,strings=scale2
	oScaleText3 -> GetProperty,strings=scale3

	case event.id of
		WIDGET_INFO(event.top ,FIND_BY_UNAME='reset_btn'):begin
			refresh, obj_list.pstaff
			return
		end
		WIDGET_INFO(event.top ,FIND_BY_UNAME='zoomin_btn'):begin
			if scale2 gt 0.5 then begin
				scale_v = 1.2
			endif else begin
				scale_v = 1
			endelse
		end
		WIDGET_INFO(event.top ,FIND_BY_UNAME='zoomout_btn'):begin
			if scale2 lt 500 then begin
				scale_v = 0.8
			endif else begin
				scale_v = 1
			endelse
		end
		else:begin
			return
		end
	endcase

	oImageModal = obj_list.oView->GetByName('oImageModal')
	oShapeModal = obj_list.oView->GetByName('oShapeModal')

	oImageModal->scale, scale_v, scale_v, 1
	oShapeModal->scale, scale_v, scale_v, 1

	GEOMETRY = WIDGET_INFO(draw_id, /GEOMETRY)
	xsize = GEOMETRY.SCR_XSIZE
	ysize = GEOMETRY.SCR_YSIZE


	scale_bar2=float(scale2)/scale_v
;	print,scale_bar2,scale2,scale_v
	oScaleText2 -> SetProperty,strings=strtrim(string(scale_bar2,format='(F7.1)'),2)

	scale_bar3=scale_bar2*2
	oScaleText3 -> SetProperty,strings=strtrim(string(scale_bar3,format='(F7.1)'),2)

	xoffset = -xsize/2 * (scale_v-1)
	yoffset = -ysize/2 * (scale_v-1)
	oImageModal->Translate, xoffset, yoffset, 0
	oShapeModal->Translate, xoffset, yoffset, 0

	oWindow->Draw, obj_list.oView
end

pro add_shape, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	draw_id = WIDGET_INFO(event.top ,FIND_BY_UNAME='draw')
	widget_control, draw_id, get_value = oWindow
	widget_control, draw_id, get_uvalue = obj_list

	add_shapefile = dialog_pickfile(DIALOG_PARENT=draw_id, $
						DEFAULT_EXTENSION='.shp', filter = '*.shp', $
						TITLE='选择Shape格式矢量文件', /MUST_EXIST)
	if add_shapefile eq '' then return

	oShape = obj_new('IDLffShape', add_shapefile)
	if obj_valid(oShape) eq 0 then begin
		OBJ_DESTROY, oShape
		info = DIALOG_MESSAGE('无法打开该矢量文件。')
		return
	endif

	oShapeModal = obj_list.oView->GetByName('oShapeModal')

    if attachshape(oShapeModal, oShape, obj_list.oSymbol, obj_list.oFont) eq 1 then begin
		oWindow->Draw, obj_list.oView
    endif
end

pro save_image, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	forward_function file_extname

	if event.select ne 1 then return

	filename = dialog_pickfile(dialog_parent=event.top, /write, /overwrite_prompt, $
	filter=[['*.jpg', '*.bmp'],['JPG图片(*.jpg)', '24位位图(*.bmp)']], $
		default_extension='jpg', title='选择文件')
	if filename eq '' then return

	draw_id = WIDGET_INFO(event.top ,FIND_BY_UNAME='draw')
	widget_control, draw_id, get_value = oWindow
	result = oWindow->Read()
	result->GetProperty, data=image
	OBJ_DESTROY, result

	;保存图片
	extname = strlowcase(file_extname(filename))
	case extname of
		'bmp' : write_bmp, filename, image, /RGB
		'jpg' : write_jpeg, filename, image, TRUE=1, QUALITY=100
	endcase
end

pro display_cleanup, draw_id
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control, draw_id, get_uvalue=obj_list
	if size(obj_list,/TYPE) eq 8 then begin
		if obj_valid(obj_list.oView) eq 1 then begin
			OBJ_DESTROY, [obj_list.oView, obj_list.oPalette, obj_list.oSymbol, obj_list.oFont]
		endif
	endif
end

pro highlight, pstaff, all_code, county_code
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	base_id = (*pstaff).base_id
	draw_id = widget_info(base_id, FIND_BY_UNAME='draw')

	widget_control, draw_id, get_value = oWindow
	widget_control, draw_id, get_uvalue = obj_list
	oImage = obj_list.oView->GetByName('oImageModal/oImage')
	if obj_valid(oImage) eq 0 then begin
		return
	endif

	oShapeModal = obj_list.oView->GetByName('oShapeModal')
	for i=0, n_elements(all_code)-1 do begin
		oPolygon = oShapeModal->GetByName(all_code[i])
		if obj_valid(oPolygon) then begin
			oPolygon->setproperty,color=[0,0,0],THICK=1
		endif
	endfor

	oPolygon = oShapeModal->GetByName(county_code)
	if obj_valid(oPolygon) then begin
		oPolygon->setproperty,color=[0,255,255],THICK=3
	endif

	oWindow->Draw, obj_list.oView
end

pro highlight_table,event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	if event.type ne 4 then return	;Cell Selection

	widget_control,event.top,get_uvalue=pstate
	table = widget_info(event.id, FIND_BY_UNAME='highlight_table')

	widget_control,table,get_value=array_text
	IF ARRAY_EQUAL(array_text,'') THEN RETURN

	selpt = widget_info(table, /TABLE_SELECT)
	if (selpt[3] - selpt[1]) ne 0 then return

	arr_size = size(array_text, /DIMENSIONS)
	if (selpt[2] - selpt[0]) ne arr_size[0]-1 then return

	highlight, (*pstate).pstaff_display, transpose(array_text[1,*]), array_text[1,selpt[1]]
end

pro select_table, event, county_code
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	table = widget_info(event.top, FIND_BY_UNAME='highlight_table')
	if table eq 0 then return

	widget_control,table,get_value=array_text
	IF ARRAY_EQUAL(array_text,'') THEN RETURN

	index = where(array_text[1,*] eq county_code, ncount)
	if ncount eq 0 then return

	arr_size = size(array_text, /DIMENSIONS)
	widget_control, table, SET_TABLE_SELECT=[0,index,arr_size[0]-1,index]
end

pro register_display_cursor, cursor_name, bitmap_file
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	bitmap=READ_BMP(bitmap_file)
	blank_index = where(bitmap eq 4)
	fill_index = where(bitmap eq 15)
	hot_index = where(bitmap eq 9)
	txt = bytarr(16*16)
	txt[*] = byte(' ')
	txt[blank_index]=byte('#')
	txt[fill_index]=byte('.')
	txt[hot_index]=byte('$')
	txt = reform(txt,16,16)
	strArray = string(REVERSE(txt,2))
	cursor_image = CREATE_CURSOR(strArray, HOTSPOT=hotspot, MASK=mask)
	REGISTER_CURSOR, cursor_name, cursor_image, HOTSPOT=hotspot, MASK=mask, /OVERWRITE
end
