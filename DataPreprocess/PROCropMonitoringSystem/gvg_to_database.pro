;钮立明,2010年4月，用于实现GVG数据入库

;获取数据库指定表格中的值，输入项为数据库对象和所查询表格的名称，输出为表格中的值
FUNCTION GET_DB_TABLE,DBobj,TABLE_NAME,SHOWNUM

	on_error,2

	SQL = 'select top '+strtrim(string(SHOWNUM),2)+' * from '+ TABLE_NAME

	RSobj=obj_new('idldbrecordset',DBobj,sql=SQL,N_BUFFERS=SHOWNUM)

	if OBJ_VALID(RSobj) eq 0 then begin
		CAUTION = Dialog_Message('所查询的表格不存在！',title='警告')
		return,strarr(1,SHOWNUM)
	endif

	COLNUM = RSobj -> nfields()
	ROWNUM = SHOWNUM

	DATA_TABLE = strarr(COLNUM,ROWNUM)

	if (RSobj -> MOVECURSOR(/FIRST) EQ 1) then begin

		count=0

		repeat begin
			for i=0,COLNUM-1,1 do begin
				DATA_TABLE[i,count] = RSobj -> GetField(i)
			endfor
			count++
		endrep until RSobj -> movecursor(/next) eq 0
	endif

	Obj_destroy,RSobj

	return,STRTRIM(DATA_TABLE,2)
END

PRO EXPORT_GVG_HEAD,EVENT

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		if obj_valid(progressTimer) ne 0 then $
			OBJ_DESTROY,progressTimer ;销毁进度条
		help, /last_message, output=errtext
		common_log,'导入出错' + errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	Widget_Control,Event.top,get_uvalue=PSTATE
	TABLE_HEAD = (*PSTATE).NAME_TABLE

	csvfile= dialog_pickfile( DIALOG_PARENT=EVENT.TOP,title='导出表头', filter=['*.csv'],FILE='GVG采样数据.csv',/write)

	if csvfile eq '' then begin
		return
	end

	tempfile=strsplit(csvfile,'.',/extract)
	outputfile=tempfile[0]+'.csv'


	if file_test(outputfile) eq 1 then begin
		CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
	   if CAUTION EQ 'No' THEN BEGIN
		   return
		endif
	endif

	context = strjoin(TABLE_HEAD,',',/single)

	OPENW,lun,outputfile,/GET_LUN
	printf,lun,context
	FREE_LUN,lun

	CAUTION=DIALOG_MESSAGE('导出表头完成!',title='提示' )

END

PRO IMPORT_GVG_DATA,EVENT

;	CATCH, Error_status
;	IF Error_status NE 0 THEN BEGIN
;		PRINT, 'Error index: ', Error_status
;		PRINT, 'Error message: ', !ERROR_STATE.MSG
;		if obj_valid(progressTimer) ne 0 then $
;			OBJ_DESTROY,progressTimer ;销毁进度条
;		help, /last_message, output=errtext
;		common_log,'导入出错' + errtext
;		Result = DIALOG_MESSAGE(errtext, /CENTER)
;		CATCH, /CANCEL
;		return
;	ENDIF

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	csvfile= dialog_pickfile( DIALOG_PARENT=EVENT.TOP,title='导入数据', filter=['*.csv'],/MUST_EXIST)

	if csvfile eq '' then return

	if file_test(csvfile) eq 0 then begin
		CAUTION=dialog_message('选择导入的文件不存在',title='警告')
		return
	endif

	progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='GVG数据入库')
	progressTimer->START
	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	progressTimer->UPDATE, 10
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN
	ENDIF

	n_record = FILE_LINES(csvfile)

	if n_record le 1 then begin
		OBJ_DESTROY,progressTimer
		infors=DIALOG_MESSAGE('导入CSV文件中有效数据为空',title='警告')
		return
	endif

	progressTimer->UPDATE, 20
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN
	ENDIF

	inputdata=strarr(n_record)
	OPENR,lun,csvfile,/get_lun
	readf,lun,inputdata
	free_lun,lun

	null_index = where(inputdata eq '')

	if null_index[0] ne -1 then begin
		OBJ_DESTROY,progressTimer
		CAUTION=DIALOG_MESSAGE('必须保证不能有空行!',title='提示' )
    	return
	endif

	FIRST_LINE = strsplit(inputdata[0],',',COUNT = N_COL,/EXTRACT,/PRESERVE_NULL)

	if ARRAY_EQUAL(FIRST_LINE,(*PSTATE).NAME_TABLE) ne 1 then begin
		OBJ_DESTROY,progressTimer ;销毁进度条
		infors=DIALOG_MESSAGE('必须保证源文件的第一行为数据表的列名!',title='提示' )
		return
	endif

	RSobj=obj_new('idldbrecordset',(*PSTATE).DBobj,TABLE=(*PSTATE).TABLE_NAME)

	RSobj -> GetProperty,Field_Info = finfo

	for j=1,n_record-1,1 do begin

;		count=0
		data_temp = strsplit(inputdata[j],',',/EXTRACT,/PRESERVE_NULL)

		null_index=where(data_temp eq '')
		if null_index[0] ne -1 then $
			data_temp[null_index]=0
;		import_name = strarr(N_COL)
		for i=0,N_COL-1,1 do begin
			case finfo[i].TYPE_NAME of
				'VARCHAR'	:	begin
					data_temp[i]=strtrim(string(data_temp[i]),2)
				end
				'INTEGER'	:	begin
					data_temp[i]=strtrim(string(fix(data_temp[i])),2)
				end
				'REAL'	:	begin
					data_temp[i]=strtrim(string(float(data_temp[i])),2)
				end
				else	:
			endcase
		endfor

		inputdata[j]=strjoin(data_temp,',')

		;查询原表中是否有相同记录
		query_SQL = 'SELECT * FROM '+(*PSTATE).TABLE_NAME+' WHERE '+ (finfo.field_name)[0]+  $
				"='"+data_temp[0]+"' and "+(finfo.field_name)[1] +'='+ data_temp[1]+';'

		EXobj=obj_new('idldbrecordset',(*PSTATE).DBobj,sql=query_SQL)

		if OBJ_VALID(EXobj) eq 1 then begin

			RecordNum = EXobj -> MoveCursor(/FIRST)
			if RecordNum eq 1 then begin
				;覆盖表中相同的记录
				QUESTION = Dialog_Message('数据库中存在相同的记录，是否覆盖?',title='警告',/QUESTION)

				if QUESTION eq 'Yes' then begin

					EXobj -> DeleteRecord

				endif else begin
					continue
				endelse
			endif
		endif

		insert_SQL = 'INSERT INTO '+ (*PSTATE).TABLE_NAME +' VALUES ('+inputdata[j]+')'

		(*PSTATE).DBobj -> ExecuteSQL,insert_SQL

		progressTimer->UPDATE, 20+(j*95.0/(n_record-1))
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了操作')
			PROGRESSTIMER->DESTROY ;结束进度条
			RETURN
		ENDIF
	endfor

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN
	ENDIF

	PROGRESSTIMER->DESTROY

	Obj_destroy,EXobj

	CAUTION = DIALOG_MESSAGE('GVG数据入库成功！',title='提示')

	common_log,'GVG数据入库成功'
	log, '种植结构-数据入库', 1
END

PRO GVG_TO_DATABASE_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE
	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='EXPORT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		EXPORT_GVG_HEAD,EVENT
      		common_log,'导出表头'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='IMPORT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		IMPORT_GVG_DATA,EVENT
      		common_log,'导入GVG数据'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, 'GVG数据入库', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				Widget_Control,event.top,/DESTROY
				common_log,'退出GVG数据导入'
		end

		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出GVG数据导入'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF
END

PRO GVG_TO_DATABASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'启动GVG数据入库'

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

;设置程序初始位置
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-600)/2
	w_yoffset=(scr_dims[1]-350)/2

;设置主框架
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='GVG数据入库' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 ,MBAR=TLB_BASE_MBAR,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)

;设置表格的列名
	NAME_TABLE = [	'县代码','年份','冬小麦',$
                  '春小麦','早稻','中稻',$
                  '晚稻','春玉米','夏玉米',$
                  '大豆','谷子','高粱',$
                  '马铃薯','红薯','花生',$
                  '油菜','向日葵','棉花',$
                  '麻类','糖料','烟叶',$
                  '蔬菜瓜果','其它作物','GVG图象数量',$
                  '生长季'    ]

;列数
	n_column=n_elements(NAME_TABLE)

;显示行数
	shownum=12

;调用数据库连接函数连接数据库
;	DBobj = CONNECT_DATABASE('crop')

	if obj_valid(DBobj) eq 0 then return

;设定查询表名
	TABLE_NAME='CROP_TYPES_PROPORTION_ORI'

;调用数据表查询函数获取数据表中的内容
	DATA_TABLE = GET_DB_TABLE(DBobj,TABLE_NAME,shownum)

;表格显示
	TABLE_BASE = WIDGET_BASE(TLB_BASE,UNAME='TABLE_BASE',/ROW,/FRAME,XPAD=0,YPAD=0)
	GVG_TABLE = WIDGET_TABLE(TABLE_BASE,UNAME='GVG_TABLE',COLUMN_WIDTHS=60,SCR_Xsize=610,SCR_Ysize=230, $
	    	DISJOINT_SELECTION=1,/RESIZEABLE_COLUMNS, $
	    	xsize=n_column,ysize=shownum,column_LABELS=NAME_TABLE,/SCROLL,VALUE=DATA_TABLE)

;命令控制区域
	CMD_BASE = WIDGET_BASE(TLB_BASE,UNAME='CMD_BASE',/ROW,SPACE=100,XPAD=30,/BASE_ALIGN_CENTER,/FRAME)

	EXPORT_BUTTON = WIDGET_BUTTON(CMD_BASE,UNAME='EXPORT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25,VALUE='导出表头')
	IMPORT_BUTTON = WIDGET_BUTTON(CMD_BASE,UNAME='IMPORT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25,VALUE='导入数据')
	HELP_BUTTON = WIDGET_BUTTON(CMD_BASE,UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25,VALUE='帮助')
	EXIT_BUTTON = WIDGET_BUTTON(CMD_BASE,UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25,VALUE='退出')

	winfo = {	NAME_TABLE	:	NAME_TABLE	, $
					n_column	:	n_column	, $
					DBobj	:	DBobj	, $
					TABLE_NAME	:	TABLE_NAME	}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=PSTATE

	Widget_Control,TLB_BASE,/REALIZE

	XMANAGER,'GVG_TO_DATABASE',TLB_BASE,/NO_BLOCK

END