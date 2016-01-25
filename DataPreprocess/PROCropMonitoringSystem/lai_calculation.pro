FUNCTION LAI_CAL,event

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		if obj_valid(progressTimer) ne 0 then $
			OBJ_DESTROY,progressTimer ;销毁进度条
		help, /last_message, output=errtext
		common_log,'计算出错' + errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return,0
	ENDIF

	Widget_Control,Event.top,get_uvalue=PSTATE

;获得输入输出路径
	Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile
	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	;检测输入、输出文件
	if file_test(inputfile) eq 0 then begin
		CAUTION = dialog_message('输入的文件不存在!',title='警告')
		return,0
	endif

	if strcompress(outputfile,/remove_all) eq '' then begin
		CAUTION = dialog_message('输出的文件未设置!',title='警告')
		return,0
	endif

;确保输出文件类型的正确性
	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='LAI计算')
	progressTimer->START
	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	progressTimer->UPDATE, 10
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	inputimage = read_tiff(inputfile, geotiff = geotiff)

	progressTimer->UPDATE, 30
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	LAI=2.967*alog(FLOAT(1+inputimage))-1.201

	progressTimer->UPDATE, 70
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	Write_tiff,outputfile,LAI,geotiff=geotiff,/float

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	return,1
END

PRO LAI_CALCULATION_EVENT,EVENT

	on_error,2

	forward_function LAI_CAL

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		;打开输入文件的控件
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输入影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'设置输出影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				;运行程序
				Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=FILE
				if file_test(FILE) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
				   if CAUTION EQ 'No' THEN BEGIN
					   return
					endif
				endif

      		STATUS=LAI_CAL(event)

				if STATUS eq 1 then $
					CAL_INFO=dialog_message('计算完成！',/information)
      		common_log,'进行LAI运算'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '指数计算', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
;				common_log,'退出LAI计算程序'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出LAI计算程序'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO LAI_CALCULATION, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

;设置程序初始位置
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;设置主框架
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset $
      ,TITLE='LAI计算' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 ,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)

		;设置子框架
			FILE_BASE = Widget_Base(TLB_BASE,/COLUMN,UNAME='FILE_BASE',/FRAME)

		;选择文件组件
			INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
				INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='输入RVI数据',XSIZE=32 ,YSIZE=1,NOEDIT=1)
				INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
						,SCR_XSIZE=36 ,SCR_YSIZE=15)

			pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
			widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

			OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE',/align_right)
				OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='输出LAI文件',XSIZE=32 ,YSIZE=1,NOEDIT=1)
				OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
						,SCR_XSIZE=36 ,SCR_YSIZE=15)

			pfinfo = {field_id:OUTPUT_FIELD, filter:'*.tif', title:'输出影像文件'}
			widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

		;指令控制区域
			CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,SCR_XSIZE=328,SCR_YSIZE=32,/row,space=56,XPAD=15)
				CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='计算',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
				HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
				EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

			Widget_Control,TLB_BASE,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD	, $
					OUTPUT_FIELD	:	OUTPUT_FIELD	 $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=PSTATE

	Xmanager,'LAI_CALCULATION',TLB_BASE,/NO_BLOCK

END