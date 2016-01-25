
;PRO PICK_FILE_PATH,event
;
;	CATCH, Error_status
;	;This statement begins the error handler:
;	IF Error_status NE 0 THEN BEGIN
;		PRINT, 'Error index: ', Error_status
;		PRINT, 'Error message: ', !ERROR_STATE.MSG
;		;Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
;		help, /last_message, output=errtext
;		Result = DIALOG_MESSAGE(errtext, /CENTER)
;		CATCH, /CANCEL
;		return
;	ENDIF
;
;	widget_control,event.id,get_uvalue=pfinfo
;	field_id = pfinfo.field_id
;	title = pfinfo.title
;	filepath = DIALOG_PICKFILE(title=title,/DIRECTORY);envi_pickfile
;
;	if (filepath eq '') then return else begin
;		widget_control,field_id,set_value=filepath
;	end
;END

PRO Swath_Path_Setting_event,event

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		;打开输入文件的控件
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE_PATH,event
      		common_log,'设置MRTSwath路径'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='INPUT2_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE_PATH,event
      		common_log,'设置MRTSwathData路径'
		end

		;指令控制区域的响应
		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				;运行程序
      		Widget_Control,((*PSTATE).event).top,get_uvalue=QSTATE
      		Widget_Control,(*PSTATE).INPUT_FIELD,get_value=MRTSwath
      		(*QSTATE).MRTSwath=MRTSwath
				Widget_Control,(*PSTATE).INPUT2_FIELD,get_value=MRTSwathData
				(*QSTATE).MRTSwathData=MRTSwathData
				Widget_Control,((*PSTATE).event).top,set_uvalue=QSTATE
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

PRO Swath_Path_Setting,event

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

;设置程序初始位置
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;设置主框架
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=(*PSTATE).WID_BASE_TLB, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='设定MRTSwath相关路径' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 $ ;设定字组件按列或行排列
      ,/TLB_KILL_REQUEST_EVENTS,/modal) ;设置关闭组件事件

;设置子框架
	FILE_BASE = Widget_Base(TLB_BASE,/COLUMN,UNAME='FILE_BASE',/FRAME)

;-----------------------------------------------------------------------------------------------------
;选择文件组件
	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='MRTSwath',XSIZE=25 ,YSIZE=1, $
			value=(*PSTATE).MRTSWATH)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT_FIELD, title:'MRTSwath'}
	widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	INPUT2_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT2_BASE',/align_right)
	INPUT2_FIELD = CW_FIELD(INPUT2_BASE,UNAME='INPUT2_FIELD',TITLE='MRTSwathData',XSIZE=25 ,YSIZE=1, $
			value=(*PSTATE).MRTSwathData)
	INPUT2_BUTTON = Widget_Button(INPUT2_BASE,UNAME='INPUT2_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
			,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT2_FIELD, title:'MRTSwathData'}
	widget_control,INPUT2_BUTTON, set_uvalue=pfinfo,/NO_COPY
;-----------------------------------------------------------------------------------------------------
;指令控制区域
	CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,SCR_YSIZE=32,/row,space=85,XPAD=35)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='确定',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
;-----------------------------------------------------------------------------------------------------

	Widget_Control,TLB_BASE,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD , $
					INPUT2_FIELD	:	INPUT2_FIELD , $
					event	: event $
				}

	QSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=QSTATE

	Xmanager,'Swath_Path_Setting',TLB_BASE,/NO_BLOCK
END

FUNCTION DATA_TIME_Y_M_D

	on_error,2

	INITIAL_YEAR=2000
	CURRENT_TIME=SYSTIME()
	CURRENT_YEAR=STRSPLIT(CURRENT_TIME,' ',/EXTRACT)

	YEAR_NUM=fix(CURRENT_YEAR[4])-INITIAL_YEAR+1

	YEAR_LIST=strtrim(string(indgen(YEAR_NUM)+INITIAL_YEAR),2)

	MONTH_LIST=strtrim(string(indgen(12)+1),2)

	DAY_LIST=strtrim(string(indgen(31)+1),2)
	return,{YEAR_LIST:YEAR_LIST,MONTH_LIST:MONTH_LIST,DAY_LIST:DAY_LIST}

END

FUNCTION DATA_TIME_D,YEAR,MONTH

	on_error,2

	DAY_NUM=FIX(julday(fix(MONTH)+1,1,fix(YEAR))-julday(fix(MONTH),1,fix(YEAR)))
	DAY_LIST=strtrim(string(indgen(DAY_NUM)+1),2)
	return,DAY_LIST
END

FUNCTION JDATE_CAL,YEAR,MONTH,DAY

	on_error,2

	JULD=julday(fix(MONTH),fix(DAY),fix(YEAR))-julday(12,31,fix(YEAR)-1)
	JULD=STRING(JULD,FORMAT='(I03)')

	DATE=LONG(YEAR+JULD)

	return,DATE
END

FUNCTION DATE_CAL,JUL,GMT
;这边的JUL形如A2009183

	on_error,2

	JYEAR=long(strmid(JUL,1,4))
	JDAY=long(strmid(JUL,5))
	JUL=julday(12,31,JYEAR-1)+JDAY

	CALDAT,JUL,MONTH,DAY,YEAR

	HR=strmid(GMT,0,2)
	MI=strmid(GMT,2,2)

	DATE=strtrim(string(YEAR),2)+'-'+strtrim(string(MONTH),2)+'-'+strtrim(string(DAY),2)+ $
		' '+HR+':'+MI
	MD=strtrim(string(YEAR),2)+strtrim(string(MONTH,format='(I02)'),2)+strtrim(string(DAY,format='(I02)'),2)

	return,{DATE:DATE,MD:MD}
END

FUNCTION NEWDATE_CAL,JUL,HR,MI
;这边的JUL形如20090102

	on_error,2

	YEAR=long(strmid(JUL,0,4))
	MONTH=long(strmid(JUL,4,2))
	DAY=long(strmid(JUL,6,2))
;	JUL=julday(12,31,JYEAR-1)+JDAY

;	CALDAT,JUL,MONTH,DAY,YEAR
;
;	HR=strmid(GMT,0,2)
;	MI=strmid(GMT,2,2)

	DATE=strtrim(string(YEAR),2)+'-'+strtrim(string(MONTH),2)+'-'+strtrim(string(DAY),2)+ $
		' '+HR+':'+MI
	MD=strtrim(string(YEAR),2)+strtrim(string(MONTH,format='(I02)'),2)+strtrim(string(DAY,format='(I02)'),2)

	return,{DATE:DATE,MD:MD}
END

FUNCTION TRANSDATA,DATA_LIST

	on_error,2

	DATE_INDEX=uniq(DATA_LIST[0,*],sort(DATA_LIST[0,*]))
	DATE_LIST=DATA_LIST[0,DATE_INDEX]

	ptrDAY=PTRARR(n_elements(DATE_LIST))

	for i=0,n_elements(DATE_LIST)-1,1 do begin

		DATA_INDEX=where((DATA_LIST[0,*] eq DATE_LIST[i]),count)

		DAY1B = { date : DATE_LIST[i], $
			modis_type:0, $
			count : count, $
			list_1km : DATA_LIST[1,DATA_INDEX], $
			list_hkm : DATA_LIST[2,DATA_INDEX], $
			list_qkm : DATA_LIST[3,DATA_INDEX], $
			list_geo : DATA_LIST[4,DATA_INDEX] $
		}

		ptrDAY[i]=PTR_NEW(DAY1B)
	endfor
	return,ptrDAY
END

PRO FILE_QUERY_TABLE,Event

	on_error,2

	widget_control,event.top,get_uvalue=PSTATE
	widget_control,(*PSTATE).input,get_value=PATH

    SY_INDEX=widget_info((*PSTATE).WID_DROPLIST_SYEAR,/DROPLIST_SELECT)
    SM_INDEX=widget_info((*PSTATE).WID_DROPLIST_SMONTH,/DROPLIST_SELECT)
    SD_INDEX=widget_info((*PSTATE).WID_DROPLIST_SDAY,/DROPLIST_SELECT)
    SYEAR=(*PSTATE).year[SY_INDEX]
    SMONTH=(*PSTATE).month[SM_INDEX]
    SDAY=(*PSTATE).day[SD_INDEX]

    EY_INDEX=widget_info((*PSTATE).WID_DROPLIST_EYEAR,/DROPLIST_SELECT)
    EM_INDEX=widget_info((*PSTATE).WID_DROPLIST_EMONTH,/DROPLIST_SELECT)
    ED_INDEX=widget_info((*PSTATE).WID_DROPLIST_EDAY,/DROPLIST_SELECT)
    EYEAR=(*PSTATE).year[EY_INDEX]
    EMONTH=(*PSTATE).month[EM_INDEX]
    EDAY=(*PSTATE).day[ED_INDEX]

;	START_DATE=JDATE_CAL(SYEAR,SMONTH,SDAY)
;	END_DATE=JDATE_CAL(EYEAR,EMONTH,EDAY)

    START_DATE=TransDateToString(SYEAR,SMONTH,SDAY)
	END_DATE=TransDateToString(EYEAR,EMONTH,EDAY)
    START_DATE=long(START_DATE)
    END_DATE=long(END_DATE)
;    print, START_DATE
;    print, END_DATE
	if file_test(PATH) eq 0 then begin
		Result = DIALOG_MESSAGE('数据所在文件夹未指定!', /CENTER)
		return
	endif

	FILES=file_search(PATH,'*.HDF')

	if FILES[0] eq '' then begin
		Result = DIALOG_MESSAGE('所指定的文件夹中无数据!', /CENTER)
		return
	endif

    print,files
	FILE_NUM=n_elements(FILES)
	FILE_TABLE=STRARR(5,FILE_NUM)
	DATA_LIST=STRARR(5,FILE_NUM)
	FILE_TABLE[*,*]='N'

	row=0
	pos=1

	for i=0,FILE_NUM-1,1 do begin
		name_temp=strsplit(FILES[i],'.',/EXTRACT)
		FILE_NAME=strsplit(FILES[i],'\',/EXTRACT)
		NUM=n_elements(FILE_NAME)-1
;        print,name_temp

;		FILE_DATE=long(strmid(name_temp[1],1))
;      	print,'aaaaaaaaaaaaaaaaaaaa'
;		print,FILE_DATE
;----------------------------------------------------
        year = strmid(file_basename(files[i]), pos+5, 4)
		month = strmid(file_basename(files[i]), pos+10, 2)
		day = strmid(file_basename(files[i]), pos+13, 2)
		hour = strmid(file_basename(files[i]), pos+16, 2)
		minute = strmid(file_basename(files[i]), pos+19, 2)
		site = strmid(file_basename(files[i]), pos+22, 2)
	   if fix(hour) lt 7 or fix(hour) gt 18 then continue
		;geofile = [geofile, files[i]]
		file_dates=TransDateToString(year, month, day)
		FILE_DATE=long(file_dates)
;		print,file_date
;        hm=[hour,minute]
;        print,hm
;----------------------------------------------------

		if (FILE_DATE gt END_DATE)or(FILE_DATE lt START_DATE) then $
			continue

		CASE 1 of
			strpos(name_temp[1],'1KM') ge 0 : col=1
			strpos(name_temp[1],'HKM') ge 0 : col=2
			strpos(name_temp[1],'QKM') ge 0 : col=3
			ELSE : col=4
		ENDCASE

;		StrDATE=DATE_CAL(name_temp[1],name_temp[2])
;		FDATE=StrDATE.DATE
;		MDATE=StrDATE.MD
        StrDATE=NEWDATE_CAL(file_dates,hour,minute)
        FDATE=StrDATE.DATE
		MDATE=StrDATE.MD
;        print,fdate
;        print,mdate
;        print, 'aaaaaaaaaaaaaaaaaa'
		row_temp=where(strmatch(FILE_TABLE[0,*],FDATE) eq 1)
		if row_temp ne -1 then begin
			row=row_temp
		endif else begin
			row_temp=where(FILE_TABLE[0,*] eq 'N')
			row=row_temp[0]
			FILE_TABLE[0,row]=FDATE
			DATA_LIST[0,row]=MDATE
		endelse

		FILE_TABLE[col,row]='Y'
		DATA_LIST[col,row]=FILES[i]
;		FILE_TABLE[col,row]=FILE_NAME[NUM]
	endfor

	row_temp=where(FILE_TABLE[0,*] eq 'N')
	if row_temp[0] gt 0 then begin
		endrow=row_temp[0]-1
	endif else begin
		endrow=0
	endelse
	FILE_TABLE=FILE_TABLE[*,0:endrow]

;需要传回带路径的文件名
	DATA_LIST=DATA_LIST[*,0:endrow]
    print,data_list
	(*PSTATE).ptrDATA=PTR_NEW(TRANSDATA(DATA_LIST))

	widget_control,(*PSTATE).WID_TABLE_DATA,set_value=FILE_TABLE
;	widget_control,(*PSTATE).ptrDATA,set_value=DATASTR
	widget_control,event.top,set_uvalue=PSTATE

END

;预处理程序接口
PRO MODIS1B_PRE,Event

	on_error,2

	widget_control,event.top,get_uvalue=PSTATE
	widget_control,(*PSTATE).output,get_value=OUTPUT
	MODIS1B=(*PSTATE).ptrDATA
	AC_STATUS=(*PSTATE).AC_STATUS
	print,MODIS1B
	print,AC_STATUS

	print,(*PSTATE).MRTSwath
	print,(*PSTATE).MRTSwathData
	CAUTION = DIALOG_MESSAGE('计算需要花费较长时间，确认要进行计算？',TITLE='提示',/QUESTION)

	if CAUTION EQ 'No' then return

;调用预处理程序传递两个参数 *MODIS1B为一个指针数组，其中每个元素指向一个结构体;
;C_STATUS为大气纠正状态
	RSDroughtModisPrepRun,(*MODIS1B),AC_STATUS,OUTPUT,(*PSTATE).MRTSwath,(*PSTATE).MRTSwathData

	CAL_INFO=dialog_message('处理完成！',/information)
	common_log,'进行处理'

END

pro WID_BASE_event, Event

	on_error,2

  widget_control,event.top,get_uvalue=PSTATE

  SY_INDEX=widget_info((*PSTATE).WID_DROPLIST_SYEAR,/DROPLIST_SELECT)
  SM_INDEX=widget_info((*PSTATE).WID_DROPLIST_SMONTH,/DROPLIST_SELECT)
  SYEAR=(*PSTATE).year[SY_INDEX]
  SMONTH=(*PSTATE).month[SM_INDEX]

  EY_INDEX=widget_info((*PSTATE).WID_DROPLIST_EYEAR,/DROPLIST_SELECT)
  EM_INDEX=widget_info((*PSTATE).WID_DROPLIST_EMONTH,/DROPLIST_SELECT)
  EYEAR=(*PSTATE).year[EY_INDEX]
  EMONTH=(*PSTATE).month[EM_INDEX]

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_INPUT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE_PATH,event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_OUTPUT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE_PATH,event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_SYEAR'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
        SDAY_LIST=DATA_TIME_D(SYEAR,SMONTH)
        widget_control,(*PSTATE).WID_DROPLIST_SDAY,set_value=SDAY_LIST
        FILE_QUERY_TABLE,Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_SMONTH'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
		  SDAY_LIST=DATA_TIME_D(SYEAR,SMONTH)
        widget_control,(*PSTATE).WID_DROPLIST_SDAY,set_value=SDAY_LIST
		  FILE_QUERY_TABLE,Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_EYEAR'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
		EDAY_LIST=DATA_TIME_D(EYEAR,EMONTH)
        widget_control,(*PSTATE).WID_DROPLIST_EDAY,set_value=EDAY_LIST
        FILE_QUERY_TABLE,Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_EMONTH'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
        EDAY_LIST=DATA_TIME_D(EYEAR,EMONTH)
        widget_control,(*PSTATE).WID_DROPLIST_EDAY,set_value=EDAY_LIST
        FILE_QUERY_TABLE,Event
    end

	 Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_SDAY'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
        FILE_QUERY_TABLE,Event
    end

	 Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_EDAY'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST' )then $
        FILE_QUERY_TABLE,Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_AC'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
		(*PSTATE).AC_STATUS=Event.select
        widget_control,event.top,set_uvalue=PSTATE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_Swath'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
			widget_control,event.top,get_uvalue=PSTATE

			Swath_Path_Setting,event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_RUN'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control,(*PSTATE).WID_TABLE_DATA,get_value=PATH_TABLE
	    widget_control,(*PSTATE).input,get_value=INPATH
		widget_control,(*PSTATE).output,get_value=OUTPATH

		DAY1B=(*PSTATE).ptrDATA

		if ((INPATH eq '') or (OUTPATH eq '')) then begin
				Result = DIALOG_MESSAGE('请指定输入和输出路径!', /CENTER)
		  		return
		endif

        index=where((PATH_TABLE eq 'N') or (PATH_TABLE eq ''))
        if index[0] ne -1 then begin
        		Result = DIALOG_MESSAGE('数据有缺失，请补齐后再执行!', /CENTER)
		  		return
		endif

		MODIS1B_PRE,Event
    end

	Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_HELP'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	common_log,'打开帮助文档'
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '特定数据处理', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
  	end

	Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXIT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
        HEAP_GC, /VERBOSE
    end

    else:
  endcase

end

pro WID_BASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

;设置程序初始位置
  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  WID_BASE_TLB = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='WID_BASE_TLB' ,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,SCR_XSIZE=361  $
      ,SCR_YSIZE=420 ,TITLE='MODIS 1B数据预处理' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1)


  WID_BASE_DATA = Widget_Base(WID_BASE_TLB, UNAME='WID_BASE_DATA'  $
      ,XOFFSET=3 ,YOFFSET=2 ,SCR_XSIZE=354 ,SCR_YSIZE=324  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_TEXT_INPUT = Widget_Text(WID_BASE_DATA, UNAME='WID_TEXT_INPUT'  $
      ,XOFFSET=117 ,YOFFSET=7 ,SCR_XSIZE=175 ,SCR_YSIZE=24   $
      ,XSIZE=20 ,YSIZE=1)


  WID_BUTTON_INPUT = Widget_Button(WID_BASE_DATA,  $
      UNAME='WID_BUTTON_INPUT' ,XOFFSET=303 ,YOFFSET=7 ,SCR_XSIZE=35  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_input, title:'输入影像文件夹'}
  widget_control,WID_BUTTON_input, set_uvalue=pfinfo,/NO_COPY

  WID_LABEL_INPUT = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_INPUT' ,XOFFSET=8 ,YOFFSET=13 ,SCR_XSIZE=108  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='MODIS 1b数据路径：')


  WID_LABEL_TABLE = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_TABLE' ,XOFFSET=8 ,YOFFSET=142 ,SCR_XSIZE=108  $
      ,SCR_YSIZE=19 ,/ALIGN_LEFT ,VALUE='输出数据列表')


  WID_LABEL_START = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_START' ,XOFFSET=9 ,YOFFSET=86 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=19 ,/ALIGN_LEFT ,VALUE='开始时间')

  dtime=DATA_TIME_Y_M_D()
  year=dtime.YEAR_LIST
  month=dtime.MONTH_LIST
  day=dtime.DAY_LIST

  WID_DROPLIST_SYEAR = Widget_Droplist(WID_BASE_DATA,  $
      UNAME='WID_DROPLIST_SYEAR' ,XOFFSET=69 ,YOFFSET=82  $
      ,SCR_XSIZE=50 ,SCR_YSIZE=23,value=year)


  WID_TABLE_DATA = Widget_Table(WID_BASE_DATA, UNAME='WID_TABLE_DATA'  $
      ,FRAME=1 ,XOFFSET=8 ,YOFFSET=163 ,SCR_XSIZE=330 ,SCR_YSIZE=156  $
      ,COLUMN_LABELS=[ '时间(GMT)','1KM', 'HKM', 'QKM', 'GEO' ] ,XSIZE=6  $
      ,COLUMN_WIDTHS=[110,30,30,30,30],YSIZE=6,/SCROLL,ALIGNMENT=1)


  WID_DROPLIST_SMONTH = Widget_Droplist(WID_BASE_DATA,  $
      UNAME='WID_DROPLIST_SMONTH' ,XOFFSET=140 ,YOFFSET=82  $
      ,SCR_XSIZE=46 ,SCR_YSIZE=22,value=month)


  WID_DROPLIST_SDAY = Widget_Droplist(WID_BASE_DATA,  $
      UNAME='WID_DROPLIST_SDAY' ,XOFFSET=210 ,YOFFSET=82  $
      ,SCR_XSIZE=46 ,SCR_YSIZE=20,VALUE=day)


  WID_LABEL_SYEAR = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_SYEAR' ,XOFFSET=122 ,YOFFSET=86 ,SCR_XSIZE=18  $
      ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='年')


  WID_LABEL_SMONTH = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_SMONTH' ,XOFFSET=190 ,YOFFSET=86 ,SCR_XSIZE=19  $
      ,SCR_YSIZE=22 ,/ALIGN_LEFT ,VALUE='月')


  WID_LABEL_SDAY = Widget_Label(WID_BASE_DATA, UNAME='WID_LABEL_SDAY'  $
      ,XOFFSET=258 ,YOFFSET=86 ,SCR_XSIZE=19 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='日')


  WID_LABEL_END = Widget_Label(WID_BASE_DATA, UNAME='WID_LABEL_END'  $
      ,XOFFSET=9 ,YOFFSET=114 ,SCR_XSIZE=60 ,SCR_YSIZE=19  $
      ,/ALIGN_LEFT ,VALUE='结束时间')


  WID_DROPLIST_EYEAR = Widget_Droplist(WID_BASE_DATA,  $
      UNAME='WID_DROPLIST_EYEAR' ,XOFFSET=69 ,YOFFSET=111  $
      ,SCR_XSIZE=50 ,SCR_YSIZE=23,VALUE=year)


  WID_LABEL_EYEAR = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_EYEAR' ,XOFFSET=122 ,YOFFSET=114 ,SCR_XSIZE=18  $
      ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='年')


  WID_DROPLIST_EMONTH = Widget_Droplist(WID_BASE_DATA,  $
      UNAME='WID_DROPLIST_EMONTH' ,XOFFSET=140 ,YOFFSET=111  $
      ,SCR_XSIZE=46 ,SCR_YSIZE=22,value=month)


  WID_LABEL_EMONTH = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_EMONTH' ,XOFFSET=190 ,YOFFSET=114  $
      ,SCR_XSIZE=19 ,SCR_YSIZE=22 ,/ALIGN_LEFT ,VALUE='月')


  WID_DROPLIST_EDAY = Widget_Droplist(WID_BASE_DATA,  $
      UNAME='WID_DROPLIST_EDAY' ,XOFFSET=210 ,YOFFSET=111  $
      ,SCR_XSIZE=46 ,SCR_YSIZE=20,VALUE=day)


  WID_LABEL_EDAY = Widget_Label(WID_BASE_DATA, UNAME='WID_LABEL_EDAY'  $
      ,XOFFSET=258 ,YOFFSET=114 ,SCR_XSIZE=18 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='日')


  WID_LABEL_OUTPUT = Widget_Label(WID_BASE_DATA,  $
      UNAME='WID_LABEL_OUTPUT' ,XOFFSET=8 ,YOFFSET=51 ,SCR_XSIZE=108  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='输出数据路径：')


  WID_BUTTON_OUTPUT = Widget_Button(WID_BASE_DATA,  $
      UNAME='WID_BUTTON_OUTPUT' ,XOFFSET=303 ,YOFFSET=45  $
      ,SCR_XSIZE=35 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  WID_TEXT_OUTPUT = Widget_Text(WID_BASE_DATA,  $
      UNAME='WID_TEXT_OUTPUT' ,XOFFSET=117 ,YOFFSET=45 ,SCR_XSIZE=175  $
      ,SCR_YSIZE=24  ,XSIZE=20 ,YSIZE=1)

  pfinfo = {field_id:WID_TEXT_output,title:'输出影像文件夹'}
  widget_control,WID_BUTTON_output, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_AC = Widget_Base(WID_BASE_TLB, UNAME='WID_BASE_AC'  $
      ,XOFFSET=11 ,YOFFSET=329 ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE,SPACE=0,YPAD=0)


  WID_BUTTON_AC = Widget_Button(WID_BASE_AC, UNAME='WID_BUTTON_AC'  $
      ,/ALIGN_LEFT ,VALUE='执行大气纠正')

  WID_BUTTON_Swath = Widget_Button(WID_BASE_TLB, UNAME='WID_BUTTON_Swath'  $
      ,XOFFSET=205 ,YOFFSET=326 ,SCR_XSIZE=80 ,SCR_YSIZE=24  $
      ,/ALIGN_CENTER ,VALUE='设定路径')

  WID_BASE_CMD = Widget_Base(WID_BASE_TLB, UNAME='WID_BASE_CMD',/ROW,/FRAME,XOFFSET=11 ,YOFFSET=355, $
  			SPACE=40,XPAD=30,SCR_XSIZE=330)
	  WID_BUTTON_RUN = Widget_Button(WID_BASE_CMD, UNAME='WID_BUTTON_RUN'  $
	      ,XOFFSET=205 ,YOFFSET=330 ,SCR_XSIZE=60 ,SCR_YSIZE=26  $
	      ,/ALIGN_CENTER ,VALUE='执行')

	  WID_BUTTON_HELP = Widget_Button(WID_BASE_CMD, UNAME='WID_BUTTON_HELP'  $
	      ,XOFFSET=205 ,YOFFSET=330 ,SCR_XSIZE=60 ,SCR_YSIZE=26  $
	      ,/ALIGN_CENTER ,VALUE='帮助')

	  WID_BUTTON_EXIT = Widget_Button(WID_BASE_CMD,  $
	      UNAME='WID_BUTTON_EXIT' ,XOFFSET=285 ,YOFFSET=330 ,SCR_XSIZE=60  $
	      ,SCR_YSIZE=26 ,/ALIGN_CENTER ,VALUE='退出')


  ptrDATA=PTR_NEW()

  Widget_Control, /REALIZE, WID_BASE_TLB

  winfo = {		input		:	WID_TEXT_INPUT, $
  				output		:	WID_TEXT_OUTPUT, $
  				year		:	year, $
  				month		:	month, $
  				day		:	day, $
				WID_DROPLIST_SYEAR	:	WID_DROPLIST_SYEAR, $
				WID_DROPLIST_SMONTH	:	WID_DROPLIST_SMONTH, $
				WID_DROPLIST_SDAY		:	WID_DROPLIST_SDAY, $
				WID_DROPLIST_EYEAR	:	WID_DROPLIST_EYEAR, $
				WID_DROPLIST_EMONTH	:	WID_DROPLIST_EMONTH, $
				WID_DROPLIST_EDAY		:	WID_DROPLIST_EDAY, $
				WID_TABLE_DATA			:	WID_TABLE_DATA, $
				ptrDATA	:	ptrDATA, $
				AC_STATUS	:	0 , $
				WID_BASE_TLB	:	WID_BASE_TLB , $
				MRTSwath	:	'C:\MRTSWATH\bin\' , $
				MRTSwathData	:	'C:\MRTSWATH\data\' $
				}
  PSTATE=PTR_NEW(winfo,/no_copy)
  widget_control, WID_BASE_TLB, set_uvalue=PSTATE,/NO_COPY

  XManager, 'WID_BASE', WID_BASE_TLB, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro DROUGHT_PRE_GUI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WID_BASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

end