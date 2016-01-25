;高光谱数据预处理
;钮立明

pro WID_BASE_preHSI_event, Event

	on_error,2

  WIDGET_CONTROL, event.top, GET_UVALUE=PSTATE
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_input1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        img_browse, Event
        common_log,'设置输入文件名'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_input2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        pick_file_path, Event
        common_log,'设置输入文件名'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_output1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        img_browse, Event
        common_log,'设置输出文件名'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_output2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        pick_file_path, Event
        common_log,'设置输出文件名'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_calculate1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
			Widget_Control,(PSTATE).output1,get_value=FILE
			if file_test(FILE) eq 1 then begin
				CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   if CAUTION EQ 'No' THEN BEGIN
				   return
				endif
			endif

        calculate_hsi, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_calculate2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      	CAUTION = DIALOG_MESSAGE('计算需要花费较长时间，确认要进行计算？',TITLE='提示',/QUESTION)

			if CAUTION EQ 'No' then return

			calculate_hyp, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_quit1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EXIT_PROGRAM, Event
        common_log,'退出高光谱数据处理程序'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_GST'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_SLIDER' )then $
        widget_control, event.id, GET_VALUE=vv,/NO_COPY
		(PSTATE).WID_SLIDER_GST = vv
        WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_BFNP'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_SLIDER' )then $
        widget_control, event.id, GET_VALUE=vv,/NO_COPY
		(PSTATE).WID_SLIDER_BFNP = vv
        WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_HELP1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      	if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '特定数据处理', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('找不到帮助文档',title='警告')
		endelse
        common_log,'打开帮助文档'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_help2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      	if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '特定数据处理', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('找不到帮助文档',title='警告')
		endelse
        common_log,'打开帮助文档'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_quit2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EXIT_PROGRAM, Event
        common_log,'退出高光谱数据处理程序'
    end
    else:
  endcase

end
;
;程序主界面
;
pro WID_BASE_preHSI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  Resolve_Routine, 'preHSI_base_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  WID_TLB = Widget_Base(GROUP_LEADER=wGroup,TITLE='高光谱数据预处理',TLB_FRAME_ATTR=1,/NO_COPY $
  			,XOFFSET=w_xoffset,YOFFSET=w_yoffset)

  WID_TAB_HSI = Widget_Tab(WID_TLB)

;************************************HJ星HSI数据处理**************************************

  WID_BASE_HSI = Widget_Base( WID_TAB_HSI,  $
      UNAME='WID_BASE_preHSI' ,title='HJ星HSI',SPACE=3 ,XPAD=3 ,YPAD=3,/COLUMN)

  WID_BASE_HSI_PARA = Widget_Base(WID_BASE_HSI,/COLUMN,/FRAME)

  WID_BASE_HSI_INPUT = Widget_Base(WID_BASE_HSI_PARA,/ROW,/BASE_ALIGN_CENTER )

  WID_TEXT_input1 = CW_FIELD(WID_BASE_HSI_INPUT,  TITLE='输入数据'$
      ,UNAME='WID_TEXT_input1' ,XSIZE=30 ,YSIZE=1,NOEDIT=1)

  WID_BASE_HSI_OUTPUT = Widget_Base(WID_BASE_HSI_PARA,/ROW,/BASE_ALIGN_CENTER)

  WID_TEXT_output1 = CW_FIELD(WID_BASE_HSI_OUTPUT,  TITLE='输出数据'$
      ,UNAME='WID_TEXT_output1' ,XSIZE=30 ,YSIZE=1,NOEDIT=1)

  WID_BUTTON_input1 = Widget_Button(WID_BASE_HSI_INPUT,  $
      UNAME='WID_BUTTON_input1' ,XOFFSET=272 ,YOFFSET=12 ,SCR_XSIZE=44  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_input1, filter:'*.tif', title:'输入原始影像(.TIFF)'}
  widget_control,WID_BUTTON_input1, set_uvalue=pfinfo,/NO_COPY

  WID_BUTTON_output1 = Widget_Button(WID_BASE_HSI_OUTPUT,  $
      UNAME='WID_BUTTON_output1' ,XOFFSET=272 ,YOFFSET=48  $
      ,SCR_XSIZE=44 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_output1, filter:'*.tif', title:'输出影像(.TIFF)'}
  widget_control,WID_BUTTON_output1, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_HSI_CMD = Widget_Base(WID_BASE_HSI,/FRAME,/ROW,/BASE_ALIGN_CENTER,XPAD=5,SPACE=37)

  WID_BUTTON_calculate1 = Widget_Button(WID_BASE_HSI_CMD,  $
      UNAME='WID_BUTTON_calculate1',XSIZE=70,YSIZE=25 $
       ,VALUE='计算')

  WID_BUTTON_HELP1 = Widget_Button(WID_BASE_HSI_CMD,  $
      UNAME='WID_BUTTON_HELP1',XSIZE=70,YSIZE=25 $
       ,VALUE='帮助')

  WID_BUTTON_quit1 = Widget_Button(WID_BASE_HSI_CMD,  $
      UNAME='WID_BUTTON_quit1',XSIZE=70,YSIZE=25  $
      ,VALUE='退出')

  WID_TEXT_INFO=Widget_text(WID_BASE_HSI, $
  	  UNAME='WID_TEXT_INFO',XOFFSET=10,YOFFSET=120,SCR_XSIZE=305,SCR_YSIZE=85 $
  	  ,VALUE='自动进行HJ星数据的预处理，得到辐亮度产品')
;************************************EO-1 Hyperion数据处理**************************************

  WID_BASE_Hyp = Widget_Base( WID_TAB_HSI,  $
      UNAME='WID_BASE_preHSI' ,title='EO-1 Hyperion',SPACE=3 ,XPAD=3 ,YPAD=3,/COLUMN)

  WID_BASE_HYP_PARA = Widget_Base(WID_BASE_Hyp,/COLUMN,/FRAME)
;  WID_LABEL_input = Widget_Label(WID_BASE_Hyp,  $
;      UNAME='WID_LABEL_input' ,XOFFSET=10 ,YOFFSET=17 ,SCR_XSIZE=56  $
;      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='输入数据')

  WID_BASE_HYP_INPUT = Widget_Base(WID_BASE_HYP_PARA,/ROW,/BASE_ALIGN_CENTER )

  WID_BASE_HYP_OUTPUT = Widget_Base(WID_BASE_HYP_PARA,/ROW,/BASE_ALIGN_CENTER )
  WID_TEXT_input2 = CW_FIELD(WID_BASE_HYP_INPUT,  $
      UNAME='WID_TEXT_input2'  ,TITLE='输入数据'  $
      ,XSIZE=30 ,YSIZE=1,NOEDIT=1)


  WID_TEXT_output2 = CW_FIELD(WID_BASE_HYP_OUTPUT,  $
      UNAME='WID_TEXT_output2'  ,TITLE='输出数据'$
      ,XSIZE=30 ,YSIZE=1,NOEDIT=1)


;  WID_LABEL_output = Widget_Label(WID_BASE_HYP_PARA,  $
;      UNAME='WID_LABEL_output' ,XOFFSET=10 ,YOFFSET=53 ,SCR_XSIZE=56  $
;      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='输出数据')


  WID_BUTTON_input2 = Widget_Button(WID_BASE_HYP_INPUT,  $
      UNAME='WID_BUTTON_input2' ,SCR_XSIZE=44  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_input2, title:'输入影像文件夹'}
  widget_control,WID_BUTTON_input2, set_uvalue=pfinfo,/NO_COPY

  WID_BUTTON_output2 = Widget_Button(WID_BASE_HYP_OUTPUT,  $
      UNAME='WID_BUTTON_output2' ,XOFFSET=272 ,YOFFSET=48  $
      ,SCR_XSIZE=44 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_output2, filter:'*.tif', title:'输出影像文件夹'}
  widget_control,WID_BUTTON_output2, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_PARA=Widget_base(WID_BASE_Hyp,xoffset=10,yoffset=82,/FRAME,TITLE='去坏线参数' $
  	  ,xsize=230,ysize=90,COLUMN=1,YPAD=0,SPACE=0)

  WID_BASE_PARA_LABEL=Widget_base(WID_BASE_PARA,ROW=1,XPAD=5,YPAD=1)
    WID_LABEL_PARA=Widget_LABEL(WID_BASE_PARA_LABEL,UNAME='WID_LABEL_PARA' $
       ,VALUE='去坏线参数',FONT='DRAFT*13',/ALIGN_LEFT)

  WID_BASE_PARA_CTL=Widget_base(WID_BASE_PARA,COLUMN=1,YPAD=0)
	WID_BASE_GST=Widget_BASE(WID_BASE_PARA_CTL,/BASE_ALIGN_CENTER,ROW=1,YPAD=0)
	WID_BASE_PARA_LABEL_GST=Widget_base(WID_BASE_GST,COLUMN=1,XSIZE=45,/ALIGN_BOTTOM)
	WID_LABEL_GST=Widget_LABEL(WID_BASE_PARA_LABEL_GST,UNAME='WID_LABEL_GST' $
	  ,VALUE='GST值:',/ALIGN_LEFT)
	WID_BASE_PARA_SLIDER_GST=Widget_base(WID_BASE_GST,COLUMN=1,YPAD=0)
  	WID_SLIDER_GST=Widget_SLIDER(WID_BASE_PARA_SLIDER_GST,/DRAG,MAXIMUM=35,MINIMUM=15 $
	  ,VALUE=21,XSIZE=110,UNAME='WID_SLIDER_GST')

	WID_BASE_BFNP=Widget_BASE(WID_BASE_PARA_CTL,/BASE_ALIGN_CENTER,ROW=1,YPAD=0)
	WID_BASE_PARA_LABEL_BFNP=Widget_base(WID_BASE_BFNP,COLUMN=1,XSIZE=45,/ALIGN_BOTTOM)
	WID_LABEL_BFNP=Widget_LABEL(WID_BASE_PARA_LABEL_BFNP,UNAME='WID_LABEL_BFNP' $
	  ,VALUE='BFNP值:',/ALIGN_LEFT)
	WID_BASE_PARA_SLIDER_BFNP=Widget_base(WID_BASE_BFNP,COLUMN=1,YPAD=0)
  	WID_SLIDER_BFNP=Widget_SLIDER(WID_BASE_PARA_SLIDER_BFNP,/DRAG,MAXIMUM=60,MINIMUM=20 $
	  ,VALUE=25,XSIZE=110,UNAME='WID_SLIDER_BFNP')

  WID_BASE_HYP_CMD = Widget_Base(WID_BASE_HYP,/FRAME,/ROW,/BASE_ALIGN_CENTER,XPAD=5,SPACE=37)

  WID_BUTTON_calculate2 = Widget_Button(WID_BASE_HYP_CMD,  $
      UNAME='WID_BUTTON_calculate2' ,XOFFSET=250 ,YOFFSET=85  $
      ,SCR_XSIZE=70 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='计算')

  WID_BUTTON_calculate2 = Widget_Button(WID_BASE_HYP_CMD,  $
      UNAME='WID_BUTTON_help2' ,XOFFSET=250 ,YOFFSET=115  $
      ,SCR_XSIZE=70 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='帮助')

  WID_BUTTON_quit2 = Widget_Button(WID_BASE_HYP_CMD,  $
      UNAME='WID_BUTTON_quit2' ,XOFFSET=250 ,YOFFSET=145 ,SCR_XSIZE=70  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='退出')

;**************************************************************************

  Widget_Control, /REALIZE, WID_TLB

  winfo = {	input1	:	WID_TEXT_input1, $
  		   	output1	:	WID_TEXT_output1, $
  		   	input2	:	WID_TEXT_input2, $
  		   	output2	:	WID_TEXT_output2, $
  		   	WID_SLIDER_GST	:	21, $
  		   	WID_SLIDER_BFNP	:	25}

  widget_control, WID_TLB, set_uvalue=winfo,/NO_COPY

  XManager, 'WID_BASE_preHSI', WID_TLB, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro preHSI_base, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  common_log,'打开高光谱数据预处理程序'
  WID_BASE_preHSI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end