;影像地形纠正模块，可进行C纠正和Minnaert纠正
;钮立明
;2010年3月

FUNCTION TOPO_C,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).C_INPUT_FIELD,get_value=image_file
	Widget_Control,(*PSTATE).C_ZEN_FIELD,get_value=zen_file
	Widget_Control,(*PSTATE).C_IRR_FIELD,get_value=irr_file
	Widget_Control,(*PSTATE).C_OUTPUT_FIELD,get_value=outputfile

	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	case 0 of
		file_test(image_file)	:	begin
			CAUTION = DIALOG_MESSAGE('所选择的影像文件不存在！',TITLE='提示',/INFORMATION)
			return,0
		end

		file_test(zen_file)	:	begin
			CAUTION = DIALOG_MESSAGE('所选择的天顶角文件不存在！',TITLE='提示',/INFORMATION)
			return,0
		end

		file_test(irr_file)	:	begin
			CAUTION = DIALOG_MESSAGE('所选择的入射角文件不存在！',TITLE='提示',/INFORMATION)
			return,0
		end
		else	:
	endcase

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START

	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF
;获取影像信息
	queryStatus = query_image(image_file,imageinfo)
		imageSize = imageInfo.dimensions
		imageChannel = imageInfo.channels

	in_image = PTR_NEW((read_tiff(image_file, geotiff = geotiff)),/NO_COPY)

	irr_data = PTR_NEW(read_tiff(irr_file, geotiff = geotiff),/NO_COPY)

	zen=read_tiff(zen_file, geotiff = geotiff)

	irr=PTR_NEW(cos(temporary((*irr_data))*!DTOR),/no_copy)

	PTR_FREE,irr_data

	index=where((*in_image) ge 0.0)

	x1=PTR_NEW((*irr)[index],/NO_COPY)

	output_image=fltarr(imageChannel,imageSize[0],imageSize[1],/nozero)

	PROGRESSTIMER->UPDATE, 10

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	for i=0,imageChannel-1,1 do begin

		image = PTR_NEW(temporary((*in_image)[i,*,*]),/NO_COPY)

		y1=PTR_NEW((*image)[index],/NO_COPY)
		index2=where((*x1 le 1000) and (*x1 ge -1000) and (*y1 le 1000) and (*y1 ge -1000) )

		M = REGRESS((*x1)[index2], ((*y1)[index2]), CONST = B)

		PTR_FREE,y1
		C=B[0]/M[0]
		print,C

		REF=PTR_NEW(((*image)*((cos(zen*!DTOR)+C)/((*irr)+C))),/NO_COPY)

		PTR_FREE,image

		output_image[i,*,*]=*REF

		PTR_FREE,REF

		PROGRESSTIMER->UPDATE, 10+(i*80.0/(imageChannel-1))
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了操作')
			PROGRESSTIMER->DESTROY ;结束进度条
			RETURN,0
		ENDIF

	endfor

	PROGRESSTIMER->UPDATE, 95

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	write_tiff, outputfile, output_image, GEOTIFF=GEOTIFF,/float

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	PTR_FREE,in_image,x1,irr

	HEAP_GC,/VERBOSE

	RETURN,1
END

FUNCTION TOPO_M,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).M_INPUT_FIELD,get_value=image_file
	Widget_Control,(*PSTATE).M_SLOPE_FIELD,get_value=slo_file
	Widget_Control,(*PSTATE).M_IRR_FIELD,get_value=irr_file
	Widget_Control,(*PSTATE).M_OUTPUT_FIELD,get_value=outputfile

	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	case 0 of
		file_test(image_file)	:	begin
			CAUTION = DIALOG_MESSAGE('所选择的影像文件不存在！',TITLE='提示',/INFORMATION)
			return,0
		end

		file_test(slo_file)	:	begin
			CAUTION = DIALOG_MESSAGE('所选择的坡度文件不存在！',TITLE='提示',/INFORMATION)
			return,0
		end

		file_test(irr_file)	:	begin
			CAUTION = DIALOG_MESSAGE('所选择的入射角文件不存在！',TITLE='提示',/INFORMATION)
			return,0
		end
		else	:
	endcase

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START

	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	queryStatus = query_image(image_file,imageinfo)
		imageSize = imageInfo.dimensions
		imageChannel = imageInfo.channels

	in_image = read_tiff(image_file, geotiff = geotiff)
	in_image=PTR_NEW(in_image,/NO_COPY)


;	ndvi=ptr_new(((*in_image)[NIR,*,*]-(*in_image)[R,*,*])/((*in_image)[NIR,*,*]+ $
;		(*in_image)[R,*,*]))

	data = read_tiff(irr_file, geotiff = geotiff)
	data = PTR_NEW(data,/NO_COPY)

	irr=cos(temporary((*data))*!DTOR)
	PTR_FREE,data
	irr=PTR_NEW(irr,/NO_COPY)


 	slo=read_tiff(slo_file, geotiff = geotiff)
	slo=PTR_NEW(slo,/NO_COPY)

	slope=cos((*slo)*!DTOR)
	PTR_FREE,slo
	slope=PTR_NEW(slope,/NO_COPY)

	index1=PTR_NEW((where(((*in_image) gt 0.0) )),/NO_COPY)

	x=PTR_NEW(alog10((*irr)*(*slope)))
	x1=PTR_NEW((*x)[*index1],/NO_COPY)

	output_image=fltarr(imageChannel,imageSize[0],imageSize[1],/nozero)

	for i=0,imageChannel-1,1 do begin
		image = PTR_NEW((*in_image)[i,*,*])

		y=PTR_NEW(alog10((*image)*(*slope)))

		y1=PTR_NEW((*y)[*index1],/NO_COPY)

		;保证值在正确的范围内，避免回归失败
		index2=PTR_NEW((where((*slope le 70) and (*x1 le 1000) and (*x1 ge -1000) and (*y1 le 1000) $
			and (*y1 ge -1000) )),/NO_COPY)

		M = REGRESS((*x1)[*index2],(*y1)[*index2], CONST = B)

		PTR_FREE,y,y1,index2

		k=M[0]
		print,k

		REF = PTR_NEW((((*image)*(*slope))/(((*irr)^k)*((*slope)^k))),/NO_COPY)

		PTR_FREE,image

		output_image[i,*,*]=*REF

		PTR_FREE,REF

		PROGRESSTIMER->UPDATE, 10+(i*95.0/(imageChannel-1))

		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了操作')
			PROGRESSTIMER->DESTROY ;结束进度条
			RETURN,0
		ENDIF

	endfor

	PROGRESSTIMER->UPDATE, 95

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	write_tiff, outputfile, output_image, GEOTIFF=GEOTIFF,/float
	PTR_FREE,x,x1,slope,irr,index1,in_image

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	HEAP_GC,/VERBOSE

	RETURN,1
END

FUNCTION TOPO_CORRECT,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	if (*PSTATE).CORRECTION EQ 0 then begin

		Widget_Control,(*PSTATE).C_OUTPUT_FIELD,get_value=FILE
		if file_test(FILE) eq 1 then begin
			CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
		   if CAUTION EQ 'No' THEN BEGIN
			   return,0
			endif
		endif

		common_log,'进行C 纠正'
		STATUS = TOPO_C(EVENT)
	endif else begin

		Widget_Control,(*PSTATE).M_OUTPUT_FIELD,get_value=FILE
		if file_test(FILE) eq 1 then begin
			CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
		   if CAUTION EQ 'No' THEN BEGIN
			   return,0
			endif
		endif

		common_log,'进行Minnaert 纠正'
		STATUS = TOPO_M(EVENT)
	endelse

	return,STATUS
END

PRO TOPOGRAPHIC_CORRECTION_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	(*PSTATE).CORRECTION=widget_info((*PSTATE).TOPO_TAB,/TAB_CURRENT )

	Widget_Control,Event.top,set_uvalue=PSTATE

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='C_INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输入影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='M_INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输入影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='C_OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'设置输出影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='M_OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'设置输出影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='C_IRR_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开太阳入射角影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='M_IRR_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开太阳入射角影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='C_ZEN_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开太阳天顶角影像'
		end
		Widget_Info(wWidget, FIND_BY_UNAME='M_SLOPE_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开坡度影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $

      		STATUS=TOPO_CORRECT(event)

				if STATUS eq 1 then $
					CAL_INFO=dialog_message('计算完成！',/information)
      		common_log,'进行地形纠正'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '地形纠正', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'退出地形纠正'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出地形纠正'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO TOPOGRAPHIC_CORRECTION, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'打开地形纠正程序'
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

	TOPO_TLB = Widget_Base( GROUP_LEADER=wGroup,/COLUMN,UNAME='TOPO_TLB' ,SPACE=3$
       ,TITLE='地形纠正',XPAD=1,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,TLB_FRAME_ATTR=1)

	TOPO_TAB = Widget_Tab(TOPO_TLB)

;C纠正模块
	C_BASE = Widget_Base(TOPO_TAB,TITLE='C 纠正',/COLUMN,UNAME='C_BASE')

	C_FILE_BASE = Widget_Base(C_BASE,/COLUMN,UNAME='C_FILE_BASE',/FRAME)

		C_INPUT_BASE = Widget_Base(C_FILE_BASE,/ROW,UNAME='C_INPUT_BASE',/align_right)
			C_INPUT_FIELD = CW_FIELD(C_INPUT_BASE,UNAME='C_INPUT_FIELD',TITLE='输入影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			C_INPUT_BUTTON = Widget_Button(C_INPUT_BASE,UNAME='C_INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:C_INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
		widget_control,C_INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

		C_ZENITH_BASE = Widget_Base(C_FILE_BASE,/ROW,UNAME='C_ZENITH_BASE',/align_right)
			C_ZEN_FIELD = CW_FIELD(C_ZENITH_BASE,UNAME='C_ZEN_FIELD',TITLE='太阳天顶角',XSIZE=33 ,YSIZE=1,NOEDIT=1)
			C_ZEN_BUTTON = Widget_Button(C_ZENITH_BASE,UNAME='C_ZEN_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:C_ZEN_FIELD, filter:'*.tif', title:'太阳天顶角文件'}
		widget_control,C_ZEN_BUTTON, set_uvalue=pfinfo,/NO_COPY

		C_IRRADIANCE_BASE = Widget_Base(C_FILE_BASE,/ROW,UNAME='C_IRRADIANCE_BASE',/align_right)
			C_IRR_FIELD = CW_FIELD(C_IRRADIANCE_BASE,UNAME='C_IRR_FIELD',TITLE='太阳入射角',XSIZE=33 ,YSIZE=1,NOEDIT=1)
			C_IRR_BUTTON = Widget_Button(C_IRRADIANCE_BASE,UNAME='C_IRR_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:C_IRR_FIELD, filter:'*.tif', title:'太阳入射角文件'}
		widget_control,C_IRR_BUTTON, set_uvalue=pfinfo,/NO_COPY

		C_OUTPUT_BASE = Widget_Base(C_FILE_BASE,/ROW,UNAME='C_OUTPUT_BASE',/align_right)
			C_OUTPUT_FIELD = CW_FIELD(C_OUTPUT_BASE,UNAME='C_OUTPUT_FIELD',TITLE='输出影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			C_OUTPUT_BUTTON = Widget_Button(C_OUTPUT_BASE,UNAME='C_OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:C_OUTPUT_FIELD, filter:'*.tif', title:'输出影像'}
		widget_control,C_OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY


;Minnaert模块
	M_BASE = Widget_Base(TOPO_TAB,TITLE='Minnaert 纠正',/COLUMN,UNAME='M_BASE')

	M_FILE_BASE = Widget_Base(M_BASE,/COLUMN,UNAME='M_FILE_BASE',/FRAME)

		M_INPUT_BASE = Widget_Base(M_FILE_BASE,/ROW,UNAME='M_INPUT_BASE',/align_right)
			M_INPUT_FIELD = CW_FIELD(M_INPUT_BASE,UNAME='M_INPUT_FIELD',TITLE='输入影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			M_INPUT_BUTTON = Widget_Button(M_INPUT_BASE,UNAME='M_INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:M_INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
		widget_control,M_INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

		M_IRRADIANCE_BASE = Widget_Base(M_FILE_BASE,/ROW,UNAME='M_IRRADIANCE_BASE',/align_right)
			M_IRR_FIELD = CW_FIELD(M_IRRADIANCE_BASE,UNAME='M_IRR_FIELD',TITLE='太阳入射角',XSIZE=33 ,YSIZE=1,NOEDIT=1)
			M_IRR_BUTTON = Widget_Button(M_IRRADIANCE_BASE,UNAME='M_IRR_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:M_IRR_FIELD, filter:'*.tif', title:'太阳入射角文件'}
		widget_control,M_IRR_BUTTON, set_uvalue=pfinfo,/NO_COPY

		M_SLOPE_BASE = Widget_Base(M_FILE_BASE,/ROW,UNAME='M_SLOPE_BASE',/align_right)
			M_SLOPE_FIELD = CW_FIELD(M_SLOPE_BASE,UNAME='M_SLOPE_FIELD',TITLE='坡度文件',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			M_SLOPE_BUTTON = Widget_Button(M_SLOPE_BASE,UNAME='M_SLOPE_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:M_SLOPE_FIELD, filter:'*.tif', title:'坡度文件'}
		widget_control,M_SLOPE_BUTTON, set_uvalue=pfinfo,/NO_COPY

		M_OUTPUT_BASE = Widget_Base(M_FILE_BASE,/ROW,UNAME='M_OUTPUT_BASE',/align_right)
			M_OUTPUT_FIELD = CW_FIELD(M_OUTPUT_BASE,UNAME='M_OUTPUT_FIELD',TITLE='输出影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			M_OUTPUT_BUTTON = Widget_Button(M_OUTPUT_BASE,UNAME='M_OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:M_OUTPUT_FIELD, filter:'*.tif', title:'输出影像'}
		widget_control,M_OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY


	CMD_BASE = Widget_Base(TOPO_TLB,UNAME='CMD_BASE',/BASE_align_center,/COLUMN,XPAD=4,YPAD=0)
	CONTROL_BASE = Widget_Base(CMD_BASE,UNAME='CONTROL_BASE',/FRAME,SCR_XSIZE=328,SCR_YSIZE=32,/row,space=53,XPAD=15)
		CAL_BUTTON = Widget_Button(CONTROL_BASE,VALUE='计算',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CONTROL_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CONTROL_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	STATE = {	TOPO_TAB	:	TOPO_TAB	, $
					CORRECTION	:	0	, $
					C_INPUT_FIELD	:	C_INPUT_FIELD	, $
					C_OUTPUT_FIELD	:	C_OUTPUT_FIELD	, $
					C_ZEN_FIELD	:	C_ZEN_FIELD	, $
					C_IRR_FIELD	:	C_IRR_FIELD	, $
					M_INPUT_FIELD	:	M_INPUT_FIELD	, $
					M_OUTPUT_FIELD	:	M_OUTPUT_FIELD	, $
					M_IRR_FIELD	:	M_IRR_FIELD	, $
					M_SLOPE_FIELD	:	M_SLOPE_FIELD $
				}

	PSTATE = PTR_NEW(STATE,/NO_COPY)

	Widget_Control,TOPO_TLB,/REALIZE

	Widget_Control,TOPO_TLB,set_uvalue=PSTATE

	Xmanager,'TOPOGRAPHIC_CORRECTION',TOPO_TLB,/NO_BLOCK

END