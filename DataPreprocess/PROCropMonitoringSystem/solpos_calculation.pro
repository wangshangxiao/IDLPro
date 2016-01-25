;¼ÆËãÌ«ÑôÎ»ÖÃ²ÎÊý³ÌÐòµÄ½çÃæÓëÊÂ¼þ´¦Àí´úÂë
;Å¥Á¢Ã÷
PRO SOLPOS_CALCULATION_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE
	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
		widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	Widget_Control,(*PSTATE).HOUR_TEXT,get_value=hour
	hour=fix(hour)
   if (hour ge 24) or (hour lt 0) then hour=0
   Widget_Control,(*PSTATE).HOUR_TEXT,set_value=strtrim(string(hour),2)

   Widget_Control,(*PSTATE).MIN_TEXT,get_value=minute
	hour=fix(minute)
   if (minute ge 60) or (minute lt 0) then minute=0
   Widget_Control,(*PSTATE).MIN_TEXT,set_value=strtrim(string(minute),2)

   Widget_Control,(*PSTATE).SEC_TEXT,get_value=sec
	hour=float(sec)
   if (sec ge 60) or (sec lt 0) then sec=0
   Widget_Control,(*PSTATE).SEC_TEXT,set_value=strtrim(string(sec),2)

	Widget_Control,(*PSTATE).DEM_PIXEL,get_value=pixel
	pixel=fix(pixel)
	if (pixel le 0) then pixel=1
	Widget_Control,(*PSTATE).DEM_PIXEL,set_value=pixel

 	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'´ò¿ªÊäÈëÓ°Ïñ'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE_PATH,event
      		common_log,'Éè¶¨Êä³öÓ°ÏñÎÄ¼þ¼Ð'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='DEM_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'´ò¿ªDEMÓ°Ïñ'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='DEM_CHECK_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		Widget_Control,(*PSTATE).DEM_BASE,sensitive=Event.select
      		common_log,'Ñ¡ÔñÊäÈëDEMÊý¾Ý'
      		if Event.select eq 0 then begin
      			Widget_Control,(*PSTATE).DEM_FIELD,set_value=''
				endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='TEMP_CHECK_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		Widget_Control,(*PSTATE).TEMP_BASE,sensitive=Event.select
      		common_log,'Ñ¡ÔñÊäÈëÎÂ¶ÈÊý¾Ý'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='PRES_CHECK_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		Widget_Control,(*PSTATE).PRES_BASE,sensitive=Event.select
      		common_log,'Ñ¡ÔñÊäÈëÆøÑ¹Êý¾Ý'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin

				Widget_Control,(*PSTATE).DEM_FIELD,get_value=dem_file
				if dem_file eq '' then begin
					CAUTION = dialog_message('Î´ÊäÈëDEMÎÄ¼þ,½á¹ûÖÐµÄÌ«ÑôÈëÉä½ÇºÍÆÂ¶ÈÎÄ¼þ½«²»×¼È·£¬ÊÇ·ñ¼ÌÐøÇ?',title='ÌáÊ¾',/question)
				   if CAUTION EQ 'No' THEN BEGIN
					   return
					endif
				endif

				Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=FILE
				Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile
				outname=file_basename(inputfile,'.tif',/fold_case)
				FILE += outname
				file_arr=[FILE+'_zen.tif',FILE+'_azi.tif',FILE+'_ia.tif',FILE+'_slope.tif']
				if (file_test(file_arr[0]) eq 1) or (file_test(file_arr[1]) eq 1) or (file_test(file_arr[2]) eq 1) or (file_test(file_arr[3]) eq 1) then begin
					CAUTION = dialog_message('Êä³öÎÄ¼þÒÑ´æÔÚ£¬ÊÇ·ñ¸²¸Ç?',title='¾¯¸æ',/question)
				   if CAUTION EQ 'No' THEN BEGIN
					   return
					endif
				endif

				SOLAR_POSITION_CALCULATION,event
      		common_log,'¼ÆËãÌ«ÑôÎ»ÖÃ²ÎÊý'
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
      		common_log,'´ò¿ª°ïÖúÎÄµµ'
      		if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '·øÉä¶¨±ê', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
				info_help=dialog_message('ÕÒ²»µ½°ïÖúÎÄµµ',title='¾¯¸æ')
				endelse
			endif
		end
		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
      		Widget_Control,event.top,/destroy
      		common_log,'ÍË³öÌ«ÑôÎ»ÖÃ²ÎÊý¼ÆËã³ÌÐò'
      		return
		end

		else :
	endcase

  	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
		common_log,'ÍË³öÌ«ÑôÎ»ÖÃ²ÎÊý¼ÆËã³ÌÐò'
	ENDIF

END

PRO SOLPOS_CALCULATION,GROUP_LEADER=wGroup,_EXTRA=_VWBExtra_

	common_log,'½øÐÐÌ«ÑôÎ»ÖÃ²ÎÊý¼ÆËã'

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

	TLB = Widget_Base(GROUP_LEADER=wGroup,/COLUMN,UNAME='TLB',TITLE='Ì«ÑôÎ»ÖÃ²ÎÊý¼ÆËã', $
			XPAD=1,YPAD=1,XOFFSET=w_xoffset,YOFFSET=w_yoffset,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)

	FILE_BASE = Widget_Base(TLB,/COLUMN,UNAME='FILE_BASE',/FRAME)
		INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE')
			INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='ÊäÈëÓ°Ïñ',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'ÊäÈëÓ°ÏñÎÄ¼þ'}
		widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

		OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE')
			OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='Êä³öÓ°Ïñ',XSIZE=35 ,YSIZE=1,NOEDIT=1)
			OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,SCR_YSIZE=15)

		pfinfo = {field_id:OUTPUT_FIELD, title:'Êä³öÓ°ÏñÎÄ¼þ¼Ð'}
		widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	CURRENT_TIME = BIN_DATE(SYSTIME())
	YEAR=CURRENT_TIME[0]
	MONTH=CURRENT_TIME[1]
	DAY=CURRENT_TIME[2]
	CT=strtrim(string(YEAR),2)+'-'+strtrim(string(MONTH),2)+'-'+strtrim(string(DAY),2)

	TIME_BASE = Widget_Base(TLB,/COLUMN,UNAME='TIME_BASE',/FRAME)
		YMD_BASE = Widget_Base(TIME_BASE,/ROW,UNAME='YMD_BASE',XPAD=7)
			YMD_FIELD = CW_FIELD(YMD_BASE,UNAME='YMD_FIELD',TITLE='ÄêÔÂÈÕ(YYYY-MM-DD)',XSIZE=25 ,YSIZE=1,/NOEDIT,VALUE=CT)
			YMD_BUTTON = Widget_Button(YMD_BASE,UNAME='YMD_BUTTON',VALUE='.\Image\Calendar.bmp',/BITMAP $
			,EVENT_PRO='ActiveXCal', uvalue={text_id:YMD_FIELD, pointer:PTR_NEW(), detail:'YMD_BUTTON'} $
			,SCR_XSIZE=30 ,SCR_YSIZE=20)

		HMS_BASE = Widget_Base(TIME_BASE,/ROW,UNAME='HMS_BASE',XPAD=10)
			HMS_LABEL = Widget_Label(HMS_BASE,VALUE='ÊÀ½çÊ±(UT) ',/ALIGN_LEFT)
			HOUR_TEXT = Widget_Text(HMS_BASE,UNAME='HOUR_TEXT',SCR_XSIZE=50 ,SCR_YSIZE=23,VALUE='0',/EDITABLE)
			HOUR_LABEL = Widget_Label(HMS_BASE,UNAME='HOUR_LABEL',VALUE='Ê± ')
			MIN_TEXT = Widget_Text(HMS_BASE,UNAME='MIN_TEXT',SCR_XSIZE=50 ,SCR_YSIZE=23,VALUE='0',/EDITABLE)
			MIN_LABEL = Widget_Label(HMS_BASE,UNAME='MIN_LABEL',VALUE='·Ö ')
			SEC_TEXT = Widget_Text(HMS_BASE,UNAME='SEC_TEXT',SCR_XSIZE=50 ,SCR_YSIZE=23,VALUE='0',/EDITABLE)
			SEC_LABEL = Widget_Label(HMS_BASE,UNAME='SEC_LABEL',VALUE='Ãë ')

	PARA_BASE = Widget_Base(TLB,/COLUMN,UNAME='PARA_BASE',/FRAME,SPACE=0)
		PARA_LABEL = Widget_Label(PARA_BASE,VALUE='¸¨Öú²ÎÊýÉèÖÃ',/ALIGN_LEFT)
		CHECK_BASE = Widget_Base(PARA_BASE,UNAME='CHECK_BASE',/ROW,/NONEXCLUSIVE)
			DEM_CHECK_BUTTON = Widget_Button(CHECK_BASE,UNAME='DEM_CHECK_BUTTON',VALUE='Ê¹ÓÃDEM')
			TEMP_CHECK_BUTTON = Widget_Button(CHECK_BASE,UNAME='TEMP_CHECK_BUTTON',VALUE='Ê¹ÓÃÎÂ¶ÈÊý¾Ý')
			PRES_CHECK_BUTTON = Widget_Button(CHECK_BASE,UNAME='PRES_CHECK_BUTTON',VALUE='Ê¹ÓÃÆøÑ¹Êý¾Ý')

		DEM_BASE = Widget_Base(PARA_BASE,/COLUMN,UNAME='DEM_BASE',SENSITIVE =0,SPACE=0)
			DEM_INPUT_BASE = Widget_Base(DEM_BASE,/ROW,UNAME='DEM_INPUT_BASE')
				DEM_FIELD = CW_FIELD(DEM_INPUT_BASE,UNAME='DEM_FIELD',TITLE='ÊäÈë¸ß³Ì',XSIZE=35 ,YSIZE=1,NOEDIT=1)
				DEM_BUTTON = Widget_Button(DEM_INPUT_BASE,UNAME='DEM_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
						,SCR_XSIZE=36 ,SCR_YSIZE=15)
				pfinfo = {field_id:DEM_FIELD, filter:'*.tif', title:'ÊäÈëDEMÎÄ¼þ'}
				widget_control,DEM_BUTTON, set_uvalue=pfinfo,/NO_COPY

			DEM_PIXEL_BASE = Widget_Base(DEM_BASE,/ROW,UNAME='DEM_PIXEL_BASE')
				DEM_PIXEL = CW_FIELD(DEM_PIXEL_BASE,UNAME='DEM_PIXEL',TITLE='ÊäÈëDEM·Ö±æÂÊ(Ã×)',XSIZE=10 ,YSIZE=1,VALUE='30')

		METE_BASE = Widget_Base(PARA_BASE,/ROW,UNAME='METE_BASE',/ALIGN_LEFT)
			TEMP_BASE =	Widget_Base(METE_BASE,/ROW,UNAME='TEMP_BASE',/ALIGN_LEFT,SENSITIVE =0)
			TEMP_FIELD = CW_FIELD(TEMP_BASE,UNAME='TEMP_FIELD',TITLE='ÊäÈëÎÂ¶È(¡ãC)',XSIZE=7 ,YSIZE=1,VALUE='0.00')
			PRES_BASE = Widget_Base(METE_BASE,/ROW,UNAME='PRES_BASE',SENSITIVE =0)
			PRES_FIELD = CW_FIELD(PRES_BASE,UNAME='PRES_FIELD',TITLE='ÊäÈëÆøÑ¹(ºÁ°Í)',XSIZE=7,YSIZE=1,VALUE='1000.0')

	CONTROL_BASE = Widget_Base(TLB,UNAME='CONTROL_BASE',/FRAME,SCR_XSIZE=200,SCR_YSIZE=32,/BASE_align_center,/row,space=56,XPAD=15)
		CAL_BUTTON = Widget_Button(CONTROL_BASE,VALUE='¼ÆËã',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CONTROL_BASE,VALUE='°ïÖú',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CONTROL_BASE,VALUE='ÍË³ö',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	STATE = {	INPUT_FIELD	:	INPUT_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					YEAR	:	YEAR , $
					MONTH	:	MONTH , $
					DAY	:	DAY , $
					YMD_FIELD	:	YMD_FIELD , $
					HOUR_TEXT	:	HOUR_TEXT , $
					MIN_TEXT	:	MIN_TEXT , $
					SEC_TEXT	:	SEC_TEXT , $
					DEM_BASE	:	DEM_BASE , $
					DEM_PIXEL	:	DEM_PIXEL , $
					DEM_FIELD	:	DEM_FIELD , $
					TEMP_BASE	:	TEMP_BASE , $
					PRES_BASE	:	PRES_BASE , $
					TEMP_FIELD	:	TEMP_FIELD , $
					PRES_FIELD	:	PRES_FIELD $
				}

	PSTATE = PTR_NEW(STATE,/NO_COPY)
	WIDGET_CONTROL, YMD_BUTTON, get_uvalue=staff
	staff.pointer = PSTATE
	WIDGET_CONTROL, YMD_BUTTON, set_uvalue=staff

	Widget_Control,TLB,set_uvalue=PSTATE
	Widget_Control, /REALIZE, TLB

	XManager, 'SOLPOS_CALCULATION',TLB, /NO_BLOCK
END