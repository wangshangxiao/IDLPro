;非监督分类模块(ISODATA法)
;钮立明　2010年4月27日

FUNCTION ISODATA,EVENT

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		common_log,'分类出错' + errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return,0
	ENDIF

	Widget_Control,Event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile

	if file_test(inputfile) eq 0 then begin
		CAUTION = dialog_message('输入文件不存在!',title='警告')
	   return,0
	endif

	if query_tiff(inputfile,tiffinfo) eq 0 then begin
		CAUTION = dialog_message('输入文件不是正确的TIFF文件!',title='警告')
	   return,0
	endif

;	if tiffinfo.channels lt 2 then begin
;		CAUTION = dialog_message('输入文件少于2个波段!',title='警告')
;	   return,0
;	endif

	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	if strcompress(outputfile,/remove_all) eq '' then begin
		CAUTION = dialog_message('输出的文件未设置!',title='警告')
		return,0
	endif

	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	Widget_Control,(*PSTATE).CLS_MIN_NUM,get_value=minclsnum

	case 1 of
		minclsnum le 0 : begin
			minclsnum=1
			Widget_Control,(*PSTATE).CLS_MIN_NUM,set_value=1
		end
		minclsnum ge 1000 : begin
			minclsnum=1000
			Widget_Control,(*PSTATE).CLS_MIN_NUM,set_value=1000
		end
		else	: begin
			minclsnum = fix(minclsnum)
			Widget_Control,(*PSTATE).CLS_MIN_NUM,set_value=minclsnum
		end
	endcase

	Widget_Control,(*PSTATE).CLS_MAX_NUM,get_value=maxclsnum

	case 1 of
		maxclsnum le 0 : begin
			maxclsnum=1
			Widget_Control,(*PSTATE).CLS_MAX_NUM,set_value=1
		end
		maxclsnum ge 1000 : begin
			maxclsnum=1000
			Widget_Control,(*PSTATE).CLS_MAX_NUM,set_value=1000
		end
		maxclsnum lt minclsnum	:	begin
			maxclsnum=minclsnum
			Widget_Control,(*PSTATE).CLS_MAX_NUM,set_value=maxclsnum
		end
		else	: begin
			maxclsnum = fix(maxclsnum)
			Widget_Control,(*PSTATE).CLS_MAX_NUM,set_value=maxclsnum
		end
	endcase

	Widget_Control,(*PSTATE).CLS_ITERATION_NUM,get_value=iternum
	case 1 of
		iternum le 0 : begin
			iternum=1
			Widget_Control,(*PSTATE).CLS_ITERATION_NUM,set_value=1
		end
		iternum ge 1000 : begin
			iternum=1000
			Widget_Control,(*PSTATE).CLS_ITERATION_NUM,set_value=1000
		end
		else	: begin
			iternum = fix(iternum)
			Widget_Control,(*PSTATE).CLS_ITERATION_NUM,set_value=iternum
		end
	endcase

	Widget_Control,(*PSTATE).CLS_CHANGE_THRESHOLD,get_value=threshold

	threshold=float(threshold)

	case 1 of
		threshold lt 0.0 : begin
			threshold=0.0
			Widget_Control,(*PSTATE).CLS_CHANGE_THRESHOLD,set_value='0.00'
		end
		threshold ge 100.0 : begin
			threshold=100.0
			Widget_Control,(*PSTATE).CLS_CHANGE_THRESHOLD,set_value='100.0'
			threshold=1.0
		end
		else	: begin
			threshold /= 100.0
		end
	endcase

	class_names = strtrim(string(indgen(maxclsnum)),2)

	compile_opt strictarr

	envi, /restore_base_save_files
	envi_batch_init

	temp=PTR_NEW(READ_TIFF(inputfile,geotiff=geoinfo))
	PTR_FREE,temp

	envi_open_data_file,inputfile,r_fid=fid,/TIFF

	ENVI_FILE_QUERY, fid, ns=ns, nl=nl,nb=nb,dims=dims

	opos=indgen(nb)
;	dims = [-1, 0, ns-1, 0, nl-1]

 	lookup = bytarr(3, maxclsnum[0])
	if (fid eq -1) then begin
    	CAUTION = dialog_message('影像文件存在错误!',title='警告')
    	return,0
	endif

	ENVI_DOIT, 'CLASS_DOIT', CLASS_NAMES=class_names,pos=opos,dims=dims,fid=fid,/in_memory,lookup=lookup,method=4,r_fid=o_fid, $
		CHANGE_THRESH=threshold[0],MIN_CLASSES=minclsnum[0],NUM_CLASSES=maxclsnum[0], ITERATIONS=iternum[0] , $
		ISO_MERGE_DIST=5.0, ISO_MERGE_PAIRS=fix(2), ISO_MIN_PIXELS=fix(1), ISO_SPLIT_SMULT=1.0, ISO_SPLIT_STD=1.0

	if o_fid eq -1 then begin
		CAUTION = dialog_message('用户中上了操作或分类影像有问题!',title='警告')
		return,0
	endif

	result = ENVI_GET_DATA(fid=o_fid, dims=dims, pos=0)
	write_tiff,outputfile,result,geotiff=geoinfo

;	ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=o_fid,pos=[0],out_name=outputfile,/tiff
;out_name=outputfile[0]
	envi_file_mng, id=fid, /remove
	envi_file_mng, id=o_fid, /remove

	return,1

END

PRO UNSUPERVISED_CLASSIFICATION_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输入影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输出影像'
		end

		;指令控制区域的响应
		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				;运行程序
				Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=FILE
				tempfile=strsplit(FILE,'.',/extract)
				outputfile=tempfile[0]+'.tif'
				if file_test(outputfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
				   if CAUTION EQ 'No' THEN BEGIN
					   return
					endif
				endif

      		STATUS=ISODATA(event)

				if STATUS eq 1 then $
					CAL_INFO=dialog_message('计算完成！',/information)
      		common_log,'进行运算'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '分类模块', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'退出程序'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出程序'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF
END

PRO UNSUPERVISED_CLASSIFICATION,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'进行非监督分类'

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

	UC_TLB = Widget_Base(GROUP_LEADER=wGroup,/COLUMN,UNAME='UC_TLB' ,SPACE=2,TITLE='非监督分类', $
		XPAD=2,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,TLB_FRAME_ATTR=1)

	FILE_BASE = Widget_Base(UC_TLB,/COLUMN,/FRAME,UNAME='FILE_BASE',SPACE=2,XPAD=1,YPAD=1,/BASE_ALIGN_CENTER)

	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)

		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',XSIZE=30 ,YSIZE=1,TITLE='输入影像',NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

		pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
		widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/COLUMN,UNAME='OUTPUT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)

	OUTPUT_FILE_BASE = Widget_Base(OUTPUT_BASE,/ROW,UNAME='OUTPUT_FILE_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_FILE_BASE,UNAME='OUTPUT_FIELD',XSIZE=30 ,YSIZE=1,TITLE='输出影像',NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_FILE_BASE,UNAME='OUTPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

		pfinfo = {field_id:OUTPUT_FIELD, filter:'*.tif', title:'输出影像文件'}
		widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	CLS_PARA_BASE	= Widget_Base(UC_TLB,/COLUMN,UNAME='CLS_PARA_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_LEFT,/FRAME)

	CLS_NUM_BASE = Widget_Base(CLS_PARA_BASE,UNAME='CLS_NUM_BASE',/ROW,XPAD=5)
		CLS_NUM_LABEL = Widget_Label(CLS_NUM_BASE,UNAME='CLS_NUM_LABEL',VALUE='分类数: ')
		CLS_MIN_NUM = CW_FIELD(CLS_NUM_BASE,UNAME='CLS_MIN_NUM',XSIZE=5,YSIZE=1,TITLE='最小分类数',VALUE=25)
		CLS_MAX_NUM = CW_FIELD(CLS_NUM_BASE,UNAME='CLS_MAX_NUM',XSIZE=5,YSIZE=1,TITLE='最大分类数',VALUE=30)

	CLS_ITERATION_BASE = Widget_Base(CLS_PARA_BASE,UNAME='CLS_ITERATION_BASE',/COLUMN,XPAD=2,/BASE_ALIGN_LEFT)
		CLS_ITERATION_NUM = CW_FIELD(CLS_ITERATION_BASE,UNAME='CLS_ITERATION_NUM',XSIZE=5,YSIZE=1, $
				TITLE='最大迭代次数：',VALUE=20)
		CLS_CHANGE_THRESHOLD = CW_FIELD(CLS_ITERATION_BASE,UNAME='CLS_CHANGE_THRESHOLD',XSIZE=5,YSIZE=1, $
				TITLE='变化阈值 %(0-100)：',VALUE='5.00')

	CMD_BASE = Widget_Base(UC_TLB,/ROW,UNAME='CMD_BASE',XPAD=9,SPACE=40,/FRAME,/BASE_ALIGN_CENTER)

	CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='运行',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
	HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
	EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,UC_TLB,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					CLS_MIN_NUM	:	CLS_MIN_NUM ,	$
					CLS_MAX_NUM	:	CLS_MAX_NUM ,	$
					CLS_ITERATION_NUM	:	CLS_ITERATION_NUM ,	$
					CLS_CHANGE_THRESHOLD	:	CLS_CHANGE_THRESHOLD $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,UC_TLB,SET_UVALUE=PSTATE

	XManager, 'UNSUPERVISED_CLASSIFICATION',UC_TLB,/NO_BLOCK
END