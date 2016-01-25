;数据分割
Function HJ_DATA_SPLIT,data,col,row,inc,block,split_temp,split_name

	on_error,2

	x=floor(col/block)
	if (col mod block) ne 0 then x+=1
	y=floor(row/block)
	if (row mod block) ne 0 then y+=1

	for i=0,x-1,1 do begin
		if col gt block then begin
			case i of
				0 : 	begin
							temp_col=PTR_NEW((*data)[0:(block+inc-1),*],/NO_COPY)
;							data=PTR_NEW((*data)[block-inc:*,*],/NO_COPY)
					 	end
				x-1 : begin
							temp_col=PTR_NEW((*data)[i*block-inc:*,*],/NO_COPY)
						end
				else : begin
							temp_col=PTR_NEW((*data)[i*block-inc:((i+1)*block+inc-1),*],/NO_COPY)
;							*data=(*data)[block:*,*]
						 end
			endcase
		endif else begin
			temp_col=PTR_NEW((*data),/NO_COPY)
		endelse

		for j=0,y-1,1 do begin
			if row gt block then begin
				case j of
					0	: begin
							temp=PTR_NEW((*temp_col)[*,0:(block+inc-1)],/NO_COPY)
							*temp_col=(*temp_col)[*,(block-inc):*]
						end
					y-1 : begin
							temp=PTR_NEW((*temp_col)[*,0:*],/NO_COPY)
						end
					else : begin
							temp=PTR_NEW((*temp_col)[*,0:(block+2*inc-1)],/NO_COPY)
							*temp_col=(*temp_col)[*,block:*]
						end
				endcase
			endif else begin
				temp=PTR_NEW((*temp_col),/NO_COPY)
			endelse

;			print,size(*temp,/dimensions)
			temp_name=split_temp+split_name+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'

			SAVE,temp,FILENAME=temp_name
			GC_STR={temp:temp}
			HEAP_FREE,GC_STR
		endfor
		PTR_FREE,temp_col
	endfor

	PTR_FREE,data
;	help,data,/structure

	return,{x:x,y:y,col:col,row:row}

END

Function HJ_DATA_MERGE,x,y,col,row,inc,block,merge_temp,merge_name,index

	on_error,2

	new_data=fltarr(col,row,/NOZERO)
	temp_size=0
	c=0
	for i=0,x-1,1 do begin
		r=0
		for j=0,y-1,1 do begin
			temp_name=merge_temp+merge_name+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			RESTORE,temp_name
			FILE_DELETE,temp_name,/NOEXPAND_PATH
			if block lt col then begin
				if i eq 0 then *tempdata=(*tempdata)[0:(block-1),*]
				if i eq x-1 then *tempdata=(*tempdata)[inc:*,*]
				if (i ne 0)and(i ne x-1) then *tempdata=(*tempdata)[inc:(block+inc-1),*]
			endif

			if block lt row then begin
				if j eq 0 then *tempdata=(*tempdata)[*,0:(block-1)]
				if j eq y-1 then *tempdata=(*tempdata)[*,inc:*]
				if (j ne 0)and(j ne y-1) then *tempdata=(*tempdata)[*,inc:(block+inc-1)]
			endif

			temp_size=size(*tempdata,/dimensions)
			new_data[c:(c+temp_size[0]-1),r:r+temp_size[1]-1]=*tempdata
			PTR_FREE,tempdata
			r+=temp_size[1]
		endfor
		c=c+temp_size[0]
	endfor

	return,new_data
END

FUNCTION CCD_CALIBRATION,event

	CATCH, Error_status
;	This statement begins the error handler:
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

	FORWARD_FUNCTION DATA_SPLIT,DATA_MERGE

	Widget_Control,Event.top,get_uvalue=PSTATE

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

	progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='环境星CCD数据定标')
	progressTimer->START
	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	progressTimer->UPDATE, 5
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

;确保输出文件类型的正确性
	tempfile=strsplit(outputfile,'.',/extract)


	queryStatus = query_image(inputfile,imageinfo)
		imageSize = imageInfo.dimensions
		imageChannel = imageInfo.channels

	blue_band=widget_info((*PSTATE).BAND_BLUE,/DROPLIST_SELECT)
	green_band=widget_info((*PSTATE).BAND_GREEN,/DROPLIST_SELECT)
	red_band=widget_info((*PSTATE).BAND_RED,/DROPLIST_SELECT)
	nir_band=widget_info((*PSTATE).BAND_NIR,/DROPLIST_SELECT)

	index = [blue_band,green_band,red_band,nir_band]

;根据选择的传感器类型设置定标系数
	type = Widget_Info((*PSTATE).SENSOR_SELECT,/DROPLIST_SELECT)

	A=fltarr(4)
	L=fltarr(4)

	CASE type of
		0	:	begin
			A[0] = 0.4259
			L[0] = 9.3184
			A[1] = 0.4213
			L[1] = 9.1758
			A[2] = 0.5881
			L[2] = 7.5072
			A[3] = 0.6981
			L[3] = 4.1484
		end
		1	:	begin
			A[0] = 0.9230
			L[0] = 4.6344
			A[1] = 0.9399
			L[1] = 4.0982
			A[2] = 1.3093
			L[2] = 3.7360
			A[3] = 1.3178
			L[3] = 0.7385
		end
		2	:	begin
			A[0] = 0.4817
			L[0] = 1.6146
			A[1] = 0.4728
			L[1] = 4.0052
			A[2] = 0.6262
			L[2] = 6.2193
			A[3] = 0.7007
			L[3] = 2.8302
		end
		3	:	begin
			A[0] = 0.8934
			L[0] = 2.2219
			A[1] = 0.9006
			L[1] = 4.0683
			A[2] = 1.2461
			L[2] = 5.2537
			A[3] = 1.1261
			L[3] = 6.3497
		end
		else :
	endcase

	inc=3
	block=1000
	temp_path='temp\'
	split_temp=temp_path+'split_temp_'
	merge_temp=temp_path+'merge_temp_'

	progressTimer->UPDATE, 10
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	for n=0,3,1 do begin

		outputfile=tempfile[0]+strtrim(string(index[n]),2)+'.tif'

		inputimage = PTR_NEW(float(read_tiff(inputfile, geotiff = geotiff,channel=[index[n]])),/NO_COPY)

		COL=imageSize[0]
		ROW=imageSize[1]
		SPLIT_NAME='data'
		data_info = HJ_DATA_SPLIT(inputimage,COL,ROW,inc,block,split_temp,SPLIT_NAME)

		PTR_FREE,inputimage

		for i=0,DATA_INFO.x-1,1 do begin
			for j=0,DATA_INFO.y-1,1 do begin

				temp_name=split_temp+'data'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
				RESTORE,temp_name
				FILE_DELETE,temp_name,/NOEXPAND_PATH
				tempdata=PTR_NEW(((*temp)),/NO_COPY)
				PTR_FREE,temp

				(*tempdata)=float(temporary(*tempdata))/A[n]+L[n]

				out_name=merge_temp+'out_temp'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
				SAVE,tempdata,FILENAME=out_name
				GC_STR={tempdata:tempdata}
				HEAP_FREE,GC_STR

			endfor

			progressTimer->UPDATE, 10+20*n+(i*20.0/(DATA_INFO.x-1))
			IF CANCELLED THEN BEGIN
				OK = DIALOG_MESSAGE('用户终止了操作')
				PROGRESSTIMER->DESTROY ;结束进度条
				RETURN,0
			ENDIF

		endfor

		outputimage=PTR_NEW((HJ_DATA_MERGE(DATA_INFO.x,DATA_INFO.y,DATA_INFO.col, $
				DATA_INFO.row,inc,block,merge_temp,'out_temp',0)),/NO_COPY)

		WRITE_TIFF,outputfile,*outputimage,geotiff=geotiff,/float

		PTR_FREE,outputimage

	endfor

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	return,1
END

PRO HJ_CALIBRATION_EVENT,EVENT

	on_error,2

	FORWARD_FUNCTION CCD_CALIBRATION
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
      		widget_control, (*PSTATE).INPUT_FIELD, get_value = inputfile
       		if file_test(inputfile) ne 0 then begin

					result=query_image(inputfile,channels=Bandnum)
					Bandlist=strtrim(string(indgen(Bandnum)+1),2)

					widget_control,(*PSTATE).band_blue,set_value=Bandlist
					widget_control,(*PSTATE).band_green,set_value=Bandlist
					widget_control,(*PSTATE).band_red,set_value=Bandlist
					widget_control,(*PSTATE).band_nir,set_value=Bandlist
				endif
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
      		STATUS = CCD_CALIBRATION(event)

				if STATUS eq 1 then $
					CAL_INFO=dialog_message('计算完成！',/information)
      		common_log,'进行运算'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '辐射定标', BOOK='HELP\HELP.chm', /FULL_PATH
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

PRO HJ_CALIBRATION,GROUP_LEADER=wGroup,_EXTRA=_VWBExtra

	common_log,'开始进行环境星CCD数据辐射定标'

;设置程序初始位置
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;设置主框架
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset  $
      ,TITLE='环境星CCD数据定标' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 ,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)

;设置子框架
	FILE_BASE = Widget_Base(TLB_BASE,/COLUMN,UNAME='FILE_BASE',/FRAME)

;选择文件组件
	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='输入数据',XSIZE=32 ,YSIZE=1,NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
	widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE',/align_right)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='输出数据',XSIZE=32 ,YSIZE=1,NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:OUTPUT_FIELD, filter:'*.tif', title:'输出影像文件'}
	widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	SENSOR_SELECT_BASE = Widget_Base(TLB_BASE,UNAME='SENSOR_SELECT_BASE',/FRAME,/COLUMN, $
			SPACE=2,XPAD=3,YPAD=1,/base_align_left)
		SENSOR_TYPE = ['HJ-1A-CCD1','HJ-1A-CCD2','HJ-1B-CCD1','HJ-1B-CCD2']
		SENSOR_SELECT = Widget_Droplist(SENSOR_SELECT_BASE,TITLE='传感器类型' $
				,UNAME='SENSOR_SELECT',VALUE=SENSOR_TYPE)
		BAND_LABEL_BASE = Widget_Base(SENSOR_SELECT_BASE,/COLUMN)
		BAND_LABEL = Widget_Label(BAND_LABEL_BASE,value='波段设置',FONT='宋体*bold*12')
		BAND_SELECT_BASE = Widget_Base(SENSOR_SELECT_BASE,UNAME='BAND_SELECT_BASE',/ROW, $
			SPACE=0,XPAD=0,YPAD=0,/BASE_ALIGN_RIGHT)
		BAND_LIST = ['1','2','3','4']
		BAND1_BASE = Widget_Base(BAND_SELECT_BASE,/COLUMN,/BASE_ALIGN_RIGHT,XPAD=0)
		BAND2_BASE = Widget_Base(BAND_SELECT_BASE,/COLUMN,/BASE_ALIGN_RIGHT)
		BAND_BLUE = Widget_Droplist(BAND1_BASE,TITLE='蓝光波段',UNAME='BAND_BLUE',value=BAND_LIST)
		BAND_GREEN = Widget_Droplist(BAND1_BASE,TITLE='绿光波段',UNAME='BAND_GREEN',value=BAND_LIST)
		BAND_RED = Widget_Droplist(BAND2_BASE,TITLE='红光波段',UNAME='BAND_RED',value=BAND_LIST)
		BAND_NIR = Widget_Droplist(BAND2_BASE,TITLE='近红外波段',UNAME='BAND_NIR',value=BAND_LIST)

;指令控制区域
	CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,/row,space=45,XPAD=15)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='计算',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,TLB_BASE,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD	, $
					OUTPUT_FIELD	:	OUTPUT_FIELD	, $
					SENSOR_SELECT	:	SENSOR_SELECT	, $
					BAND_BLUE	:	BAND_BLUE	, $
					BAND_GREEN	:	BAND_GREEN	, $
					BAND_RED	:	BAND_RED	, $
					BAND_NIR	:	BAND_NIR	, $
					BAND_LIST	:	BAND_LIST	$
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=PSTATE

	Xmanager,'HJ_CALIBRATION',TLB_BASE,/NO_BLOCK

END