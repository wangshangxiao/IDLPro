;分类后重编码模块
;钮立明　2010年5月5日

FUNCTION CRITERIA_EXP,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).TABLE,get_value=table

	if (*PSTATE).BUTT_SYM ne 3 then begin
		CAUTION = DIALOG_MESSAGE('语法错误!',/information)
		return,0
	endif
	;      		index=WHERE(table[(*PSTATE).COLUMN_INDEX,*]+(*PSTATE).OP_SYMBOL[(*PSTATE).OP_INDEX]+(*PSTATE).VALUE_TB[(*PSTATE).VALUE_INDEX])

	Widget_Control,(*PSTATE).EXPRESSION_TEXT,get_value=EXPRESSION

	TEMP1 = STRSPLIT(EXPRESSION[0],'AND|OR|NOT',/EXTRACT,COUNT=N_TEMP1,/REGEX)

	OP_TEMP = STRSPLIT(EXPRESSION[0],' ',/EXTRACT,COUNT=N_TEMP_OP,/REGEX)
	OP_POS = WHERE((OP_TEMP EQ 'AND') OR (OP_TEMP EQ 'OR') OR (OP_TEMP EQ 'NOT'))

	if OP_POS[0] ne -1 then begin
		OP_NUM = n_elements(OP_POS)
		OP_ARR = OP_TEMP[OP_POS]
	endif

	for i =0,N_TEMP1-1,1 do begin
		TEMP2=strsplit(TEMP1[i],' ',/extract)

		if TEMP2[0] EQ "'旧编码'" then begin
			c_index=0
		endif else begin
			c_index=1
		endelse

		case TEMP2[1] of
			'GT' : condition = table[c_index,*] gt TEMP2[2]
			'GE' : condition = table[c_index,*] ge TEMP2[2]
			'EQ' : condition = table[c_index,*] eq TEMP2[2]
			'LE' : condition = table[c_index,*] le TEMP2[2]
			'LT' : condition = table[c_index,*] lt TEMP2[2]
			'NE' : condition = table[c_index,*] ne TEMP2[2]
			else :
		endcase

		if i eq 0 then begin
			TEMP3 = condition
		endif else begin
			case OP_ARR[i-1] of
				'AND' : TEMP3 and= condition
				'OR'	: TEMP3 or= condition
				'NOT'	: TEMP3 and= not condition
			endcase
		endelse
	endfor

	index = where(TEMP3)

	if index[0] ne -1 then begin
		select_index = intarr(2,n_elements(index)*3)
		select_index[1,0:n_elements(index)-1] = index
		select_index[0,n_elements(index):2*n_elements(index)-1]=1
		select_index[1,n_elements(index):2*n_elements(index)-1]=index
		select_index[0,2*n_elements(index):3*n_elements(index)-1]=2
		select_index[1,2*n_elements(index):3*n_elements(index)-1]=index

		Widget_Control,(*PSTATE).TABLE,set_table_select=select_index
	endif else begin
		CAUTION = DIALOG_MESSAGE('查询语句逻辑错误!',/information)
		return,0
	endelse

	return,1
END

PRO CRITERIA_EVENT,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		;打开输入文件的控件
		Widget_Info(wWidget, FIND_BY_UNAME='COLUMN_LIST'): begin

			if ((*PSTATE).BUTT_SYM ne 0) and ((*PSTATE).BUTT_SYM ne 4) then begin
				CAUTION = DIALOG_MESSAGE('语法错误!',/information)
				return
			endif

			Widget_Control,(*PSTATE).TABLE,get_value=table

      	COLUMN_INDEX = Widget_Info((*PSTATE).COLUMN_LIST,/LIST_SELECT)
      	Widget_Control,(*PSTATE).EXPRESSION_TEXT,/APPEND,/NO_NEWLINE,SET_VALUE="'"+strtrim(string((*PSTATE).COLUMN_NAME[COLUMN_INDEX]),2)+"'"


			(*PSTATE).COLUMN_INDEX = COLUMN_INDEX

			VALUE_TB=strtrim(string(table[COLUMN_INDEX,*]),2)
			Widget_Control,(*PSTATE).VALUE_LIST,set_value=VALUE_TB
			(*PSTATE).VALUE_TB=VALUE_TB

			(*PSTATE).BUTT_SYM=1

			Widget_Control,event.top,set_uvalue=PSTATE
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='OPERATOR_LIST'): begin

			if (*PSTATE).BUTT_SYM ne 1 then begin
				CAUTION = DIALOG_MESSAGE('语法错误!',/information)
				return
			endif

      	OPERATOR_INDEX = Widget_Info((*PSTATE).OPERATOR_LIST,/LIST_SELECT)

			(*PSTATE).OP_INDEX = OPERATOR_INDEX

      	Widget_Control,(*PSTATE).EXPRESSION_TEXT,/APPEND,/NO_NEWLINE,SET_VALUE=' '+strtrim(string((*PSTATE).OP_SYMBOL[OPERATOR_INDEX]),2)+' '

			(*PSTATE).BUTT_SYM=2

			Widget_Control,event.top,set_uvalue=PSTATE
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='VALUE_LIST'): begin

			if (*PSTATE).BUTT_SYM ne 2 then begin
				CAUTION = DIALOG_MESSAGE('语法错误!',/information)
				return
			endif

      	VALUE_INDEX = Widget_Info((*PSTATE).VALUE_LIST,/LIST_SELECT)

			(*PSTATE).VALUE_INDEX = VALUE_INDEX

			Widget_Control,(*PSTATE).EXPRESSION_TEXT,/APPEND,/NO_NEWLINE,SET_VALUE=strtrim(string((*PSTATE).VALUE_TB[VALUE_INDEX]),2)

			(*PSTATE).BUTT_SYM=3

			Widget_Control,event.top,set_uvalue=PSTATE
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='AND_BUTT'): begin

			if (*PSTATE).BUTT_SYM ne 3 then begin
				CAUTION = DIALOG_MESSAGE('语法错误!',/information)
				return
			endif

			Widget_Control,(*PSTATE).EXPRESSION_TEXT,/APPEND,/NO_NEWLINE,SET_VALUE= ' AND '

			(*PSTATE).BUTT_SYM=4
			Widget_Control,event.top,set_uvalue=PSTATE
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='OR_BUTT'): begin

			if (*PSTATE).BUTT_SYM ne 3 then begin
				CAUTION = DIALOG_MESSAGE('语法错误!',/information)
				return
			endif

			Widget_Control,(*PSTATE).EXPRESSION_TEXT,/APPEND,/NO_NEWLINE,SET_VALUE= ' OR '

			(*PSTATE).BUTT_SYM=4
			Widget_Control,event.top,set_uvalue=PSTATE
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='NOT_BUTT'): begin

			if (*PSTATE).BUTT_SYM ne 3 then begin
				CAUTION = DIALOG_MESSAGE('语法错误!',/information)
				return
			endif

			Widget_Control,(*PSTATE).EXPRESSION_TEXT,/APPEND,/NO_NEWLINE,SET_VALUE= ' NOT '

			(*PSTATE).BUTT_SYM=4
			Widget_Control,event.top,set_uvalue=PSTATE
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $

				CRITERIA_STATUS = CRITERIA_EXP(EVENT)

		end

		Widget_Info(wWidget, FIND_BY_UNAME='CLEAR_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $

      		Widget_Control,(*PSTATE).EXPRESSION_TEXT,set_value='',/NO_NEWLINE

				(*PSTATE).BUTT_SYM=0
				Widget_Control,event.top,set_uvalue=PSTATE

      		common_log,'清空选择表达式'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '帮助主题', BOOK='HELP\HELP.chm', /FULL_PATH
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

PRO CRITERIA,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]+200)/2
	w_yoffset=(scr_dims[1]-250)/2

	CR_TLB = Widget_Base(GROUP_LEADER=(*PSTATE).RC_TLB,/COLUMN,Title='选择分类值' ,TLB_FRAME_ATTR=1 $
			,XPAD=2,YPAD=2,XOFFSET=w_xoffset,YOFFSET=w_yoffset,/TLB_KILL_REQUEST_EVENTS,/TLB_SIZE_EVENTS )

	(*PSTATE).CRITERIA_ID = CR_TLB
	Widget_Control,event.top,set_uvalue=PSTATE

	SELECT_BASE = Widget_Base(CR_TLB,/ROW,/FRAME,XPAD=2,SPACE=5)

	Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,get_value=table

	COLUMN_NAME = ['旧编码','新编码']

	COLUMN_BASE = Widget_Base(SELECT_BASE,/COLUMN,/BASE_ALIGN_CENTER,XPAD=0)
	COLUMN_LABEL = Widget_Label(COLUMN_BASE,VALUE='列名')
	COLUMN_LIST = Widget_List(COLUMN_BASE,UNAME='COLUMN_LIST',VALUE=COLUMN_NAME,XSIZE=11,YSIZE=9)

	OP_SYMBOL = ['GT','LT','EQ','LE','GE','NE']

	OPERATOR_BASE = Widget_Base(SELECT_BASE,/COLUMN,/BASE_ALIGN_CENTER,XPAD=0)
	OPERATOR_LABEL = Widget_Label(OPERATOR_BASE,VALUE='操作符')
	OPERATOR_LIST = Widget_List(OPERATOR_BASE,UNAME='OPERATOR_LIST',VALUE=OP_SYMBOL,XSIZE=5,YSIZE=9)

	Widget_Control,COLUMN_LIST,set_list_select=0

	INDEX=Widget_Info(COLUMN_LIST,/LIST_SELECT)
	VALUE_TB=strtrim(string(table[INDEX,*]),2)

	VALUE_BASE = Widget_Base(SELECT_BASE,/COLUMN,/BASE_ALIGN_CENTER,XPAD=0)
	VALUE_LABEL = Widget_Label(VALUE_BASE,VALUE='类值')
	VALUE_LIST = Widget_List(VALUE_BASE,UNAME='VALUE_LIST',VALUE=VALUE_TB,XSIZE=11,YSIZE=9)

	RELATION_BASE = Widget_Base(SELECT_BASE,/COLUMN,/BASE_ALIGN_CENTER,XPAD=0,YPAD=30,SPACE=5)
	AND_BUTT = Widget_Button(RELATION_BASE,UNAME='AND_BUTT',XSIZE=28,YSIZE=20,VALUE='AND')
	OR_BUTT = Widget_Button(RELATION_BASE,UNAME='OR_BUTT',XSIZE=28,YSIZE=20,VALUE='OR')
	NOT_BUTT = Widget_Button(RELATION_BASE,UNAME='NOT_BUTT',XSIZE=28,YSIZE=20,VALUE='NOT')

	EXPRESSION_BASE = Widget_Base(CR_TLB,/COLUMN,/FRAME,XPAD=2,SPACE=0)
	EXPRESSION_TEXT = Widget_Text(EXPRESSION_BASE,UNAME='EXPRESSION_TEXT',XSIZE=46,YSIZE=5,/WRAP)

	CMD_BASE = Widget_Base(CR_TLB,UNAME='CMD_BASE',/FRAME,SCR_XSIZE=200,SCR_YSIZE=32,/row,space=10,XPAD=7)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='确定',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		CLEAR_BUTTON = Widget_Button(CMD_BASE,VALUE='清空',UNAME='CLEAR_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,CR_TLB,/REALIZE

	winfo = {	EXPRESSION_TEXT	:	EXPRESSION_TEXT	, $
					TABLE	:	(*PSTATE).ATTRIBUTE_TABLE	, $
					COLUMN_NAME	:	COLUMN_NAME	, $
					COLUMN_LIST	:	COLUMN_LIST	, $
					COLUMN_INDEX	:	-1 , $
					OP_SYMBOL	:	OP_SYMBOL , $
					OPERATOR_LIST	:	OPERATOR_LIST	, $
					OP_INDEX	:	-1 , $
					VALUE_TB	:	VALUE_TB , $
					VALUE_LIST	:	VALUE_LIST , $
					VALUE_INDEX	:	-1 , $
					BUTT_SYM	:	0 $
				}

	QSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,CR_TLB,set_uvalue=QSTATE

	XManager, 'CRITERIA', CR_TLB, /NO_BLOCK,EVENT_HANDLER='CRITERIA_EVENT'

END

FUNCTION RECODE_CLS,EVENT

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		if obj_valid(progressTimer) ne 0 then $
			OBJ_DESTROY,progressTimer ;销毁进度条
		help, /last_message, output=errtext
		common_log,'重编码出错' + errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return,0
	ENDIF

	Widget_Control,Event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	if strcompress(outputfile,/remove_all) eq '' then begin
		CAUTION = dialog_message('输出的文件未设置!',title='警告')
		return,0
	endif

	tempfile = strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,GET_VALUE=table_value

	dims=size(table_value,/dimensions)

	image=(*PSTATE).IMAGE
	geoinfo=(*PSTATE).GEOINFO

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START

	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	for i=0,dims[1]-1,1 do begin

		index=where(*image eq table_value[0,i])

		if index[0] eq -1 then continue

		(*image)[index]=table_value[1,i]

		PROGRESSTIMER->UPDATE, (i*90.0/(dims[1]-1))
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了操作')
			PROGRESSTIMER->DESTROY ;结束进度条
			RETURN,0
		ENDIF
	endfor

	write_tiff,outputfile,*image,geotiff=(*geoinfo)

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	return,1
END

PRO RECODE_EVENT,EVENT

	on_error,2

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

				Widget_Control,(*PSTATE).INPUT_FIELD,get_value=FILE

				if FILE EQ '' then return

				if file_test(FILE) eq 0 then begin
					CAUTION = dialog_message('输入文件不存在!',title='警告')
				   return
				endif

				if query_tiff(FILE) eq 0 then begin
					CAUTION = dialog_message('输入文件不是正确的TIFF文件!',title='警告')
				   return
				endif

				image=PTR_NEW(read_tiff(FILE,GEOTIFF=geoinfo),/NO_COPY)

				status = query_image(FILE,imageinfo)
				dims=imageinfo.dimensions

				hist=histogram(*image)
				index = transpose(where(hist ne 0))
				row = n_elements(index)

				table_value=[index,index,hist[index]]

				Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,TABLE_YSIZE=row
				Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,SET_VALUE=table_value

				(*PSTATE).IMAGE=image
				(*PSTATE).GEOINFO=PTR_NEW(geoinfo,/NO_COPY)
				Widget_Control,event.top,set_uvalue=PSTATE

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
				if file_test(FILE) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
				   if CAUTION EQ 'No' THEN BEGIN
					   return
					endif
				endif

      		STATUS=RECODE_CLS(event)

				if STATUS eq 1 then $
					CAL_INFO=dialog_message('计算完成！',/information)
      		common_log,'进行运算'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='ATTRIBUTE_TABLE'): begin
			if( n_tags(event) eq 7 )then begin
				contextBase=widget_info(event.top,find_by_uname='CONTEXT_BASE')
				widget_displaycontextmenu,event.id,event.x,event.y,contextBase
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='SELECTION'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $

				Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,get_value=table

				if Widget_Info((*PSTATE).CRITERIA_ID,/valid_id) eq 0 then begin
					if table[0,0] ne '' then begin
		      		CRITERIA,EVENT
		      	endif else begin
		      		CAUTION = DIALOG_MESSAGE('无有效数据！',/information)
		      	endelse
				endif

;				if STATUS eq 1 then $
;					CAL_INFO=dialog_message('计算完成！',/information)
      		common_log,'选择数据'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='VALUE_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				Widget_Control,(*PSTATE).VALUE_FIELD,get_value=new_value

				new_value = fix(new_value[0])
				Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,get_value=change_table
				CHANGE_INDEX=Widget_Info((*PSTATE).ATTRIBUTE_TABLE,/TABLE_SELECT)
				change_table[1,CHANGE_INDEX[1,*]]=new_value

				Widget_Control,(*PSTATE).ATTRIBUTE_TABLE,set_value=change_table
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

PRO RECODE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'进行分类后重编码'

;设置程序初始位置
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2


	RC_TLB = Widget_Base(UNAME='RC_TLB',XOFFSET=w_xoffset ,YOFFSET=w_yoffset,/COLUMN,TITLE='分类后重编码', $
			/TLB_KILL_REQUEST_EVENTS,/TLB_SIZE_EVENTS ,TLB_FRAME_ATTR=1)

	;选择文件组件
	FILE_BASE = Widget_Base(RC_TLB,/COLUMN,UNAME='FILE_BASE',/FRAME)

	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='输入文件',XSIZE=27 ,YSIZE=1,NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
		widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE',/align_right)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='输出文件',XSIZE=27 ,YSIZE=1,NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:OUTPUT_FIELD, filter:'*.tif', title:'输出影像文件'}
		widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	TABLE_BASE = Widget_Base(RC_TLB,UNAME='TABLE_BASE',/COLUMN,SPACE=0,XPAD=0,YPAD=0,/FRAME)

	COLUMN_NAME = ['旧编码','新编码','像元数']

	ATTRIBUTE_TABLE = Widget_Table(TABLE_BASE,UNAME='ATTRIBUTE_TABLE',COLUMN_LABELS=COLUMN_NAME, $
		XSIZE=3,YSIZE=row,/EDITABLE,/SCROLL,X_SCROLL_SIZE=3,Y_SCROLL_SIZE=10,/CONTEXT_EVENTS,/DISJOINT_SELECTION)

		CONTEXT_BASE = widget_base(ATTRIBUTE_TABLE,/context_menu,uname='CONTEXT_BASE')
		SELECTION = widget_button(CONTEXT_BASE,value='选择记录',uname='SELECTION')

	CHANGE_VALUE_BASE = Widget_Base(RC_TLB,UNAME='CHANGE_VALUE_BASE',/ROW,SPACE=3,YPAD=1,/FRAME)
		VALUE_FIELD = CW_FIELD(CHANGE_VALUE_BASE,UNAME='VALUE_FIELD',TITLE='新编码 ')
		VALUE_BUTTON = Widget_Button(CHANGE_VALUE_BASE,UNAME='VALUE_BUTTON',VALUE='更新',XSIZE=40,YSIZE=10)
;指令控制区域
	CMD_BASE = Widget_Base(RC_TLB,UNAME='CMD_BASE',/FRAME,SCR_XSIZE=200,SCR_YSIZE=32,/row,space=40,XPAD=6)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='确定',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,RC_TLB,/REALIZE

	winfo = {	RC_TLB	:	RC_TLB , $
					INPUT_FIELD	:	INPUT_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					VALUE_FIELD	:	VALUE_FIELD , $
					IMAGE	:	PTR_NEW(/NO_COPY) , $
					GEOINFO	:	PTR_NEW(/NO_COPY) , $
					COLUMN_NAME	:	COLUMN_NAME , $
					ATTRIBUTE_TABLE	:	ATTRIBUTE_TABLE , $
					CRITERIA_ID	:	0 $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,RC_TLB,set_uvalue=PSTATE

	XMANAGER,'RECODE',RC_TLB,/NO_BLOCK

END