;�߹�������Ԥ����
;ť����

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
        common_log,'���������ļ���'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_input2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        pick_file_path, Event
        common_log,'���������ļ���'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_output1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        img_browse, Event
        common_log,'��������ļ���'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_output2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        pick_file_path, Event
        common_log,'��������ļ���'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_calculate1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
			Widget_Control,(PSTATE).output1,get_value=FILE
			if file_test(FILE) eq 1 then begin
				CAUTION = dialog_message('����ļ��Ѵ��ڣ��Ƿ񸲸�?',title='����',/question)
			   if CAUTION EQ 'No' THEN BEGIN
				   return
				endif
			endif

        calculate_hsi, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_calculate2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      	CAUTION = DIALOG_MESSAGE('������Ҫ���ѽϳ�ʱ�䣬ȷ��Ҫ���м��㣿',TITLE='��ʾ',/QUESTION)

			if CAUTION EQ 'No' then return

			calculate_hyp, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_quit1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EXIT_PROGRAM, Event
        common_log,'�˳��߹������ݴ������'
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
			ONLINE_HELP, '�ض����ݴ���', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
        common_log,'�򿪰����ĵ�'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_help2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      	if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '�ض����ݴ���', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
        common_log,'�򿪰����ĵ�'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_quit2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EXIT_PROGRAM, Event
        common_log,'�˳��߹������ݴ������'
    end
    else:
  endcase

end
;
;����������
;
pro WID_BASE_preHSI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  Resolve_Routine, 'preHSI_base_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  WID_TLB = Widget_Base(GROUP_LEADER=wGroup,TITLE='�߹�������Ԥ����',TLB_FRAME_ATTR=1,/NO_COPY $
  			,XOFFSET=w_xoffset,YOFFSET=w_yoffset)

  WID_TAB_HSI = Widget_Tab(WID_TLB)

;************************************HJ��HSI���ݴ���**************************************

  WID_BASE_HSI = Widget_Base( WID_TAB_HSI,  $
      UNAME='WID_BASE_preHSI' ,title='HJ��HSI',SPACE=3 ,XPAD=3 ,YPAD=3,/COLUMN)

  WID_BASE_HSI_PARA = Widget_Base(WID_BASE_HSI,/COLUMN,/FRAME)

  WID_BASE_HSI_INPUT = Widget_Base(WID_BASE_HSI_PARA,/ROW,/BASE_ALIGN_CENTER )

  WID_TEXT_input1 = CW_FIELD(WID_BASE_HSI_INPUT,  TITLE='��������'$
      ,UNAME='WID_TEXT_input1' ,XSIZE=30 ,YSIZE=1,NOEDIT=1)

  WID_BASE_HSI_OUTPUT = Widget_Base(WID_BASE_HSI_PARA,/ROW,/BASE_ALIGN_CENTER)

  WID_TEXT_output1 = CW_FIELD(WID_BASE_HSI_OUTPUT,  TITLE='�������'$
      ,UNAME='WID_TEXT_output1' ,XSIZE=30 ,YSIZE=1,NOEDIT=1)

  WID_BUTTON_input1 = Widget_Button(WID_BASE_HSI_INPUT,  $
      UNAME='WID_BUTTON_input1' ,XOFFSET=272 ,YOFFSET=12 ,SCR_XSIZE=44  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_input1, filter:'*.tif', title:'����ԭʼӰ��(.TIFF)'}
  widget_control,WID_BUTTON_input1, set_uvalue=pfinfo,/NO_COPY

  WID_BUTTON_output1 = Widget_Button(WID_BASE_HSI_OUTPUT,  $
      UNAME='WID_BUTTON_output1' ,XOFFSET=272 ,YOFFSET=48  $
      ,SCR_XSIZE=44 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_output1, filter:'*.tif', title:'���Ӱ��(.TIFF)'}
  widget_control,WID_BUTTON_output1, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_HSI_CMD = Widget_Base(WID_BASE_HSI,/FRAME,/ROW,/BASE_ALIGN_CENTER,XPAD=5,SPACE=37)

  WID_BUTTON_calculate1 = Widget_Button(WID_BASE_HSI_CMD,  $
      UNAME='WID_BUTTON_calculate1',XSIZE=70,YSIZE=25 $
       ,VALUE='����')

  WID_BUTTON_HELP1 = Widget_Button(WID_BASE_HSI_CMD,  $
      UNAME='WID_BUTTON_HELP1',XSIZE=70,YSIZE=25 $
       ,VALUE='����')

  WID_BUTTON_quit1 = Widget_Button(WID_BASE_HSI_CMD,  $
      UNAME='WID_BUTTON_quit1',XSIZE=70,YSIZE=25  $
      ,VALUE='�˳�')

  WID_TEXT_INFO=Widget_text(WID_BASE_HSI, $
  	  UNAME='WID_TEXT_INFO',XOFFSET=10,YOFFSET=120,SCR_XSIZE=305,SCR_YSIZE=85 $
  	  ,VALUE='�Զ�����HJ�����ݵ�Ԥ�����õ������Ȳ�Ʒ')
;************************************EO-1 Hyperion���ݴ���**************************************

  WID_BASE_Hyp = Widget_Base( WID_TAB_HSI,  $
      UNAME='WID_BASE_preHSI' ,title='EO-1 Hyperion',SPACE=3 ,XPAD=3 ,YPAD=3,/COLUMN)

  WID_BASE_HYP_PARA = Widget_Base(WID_BASE_Hyp,/COLUMN,/FRAME)
;  WID_LABEL_input = Widget_Label(WID_BASE_Hyp,  $
;      UNAME='WID_LABEL_input' ,XOFFSET=10 ,YOFFSET=17 ,SCR_XSIZE=56  $
;      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��������')

  WID_BASE_HYP_INPUT = Widget_Base(WID_BASE_HYP_PARA,/ROW,/BASE_ALIGN_CENTER )

  WID_BASE_HYP_OUTPUT = Widget_Base(WID_BASE_HYP_PARA,/ROW,/BASE_ALIGN_CENTER )
  WID_TEXT_input2 = CW_FIELD(WID_BASE_HYP_INPUT,  $
      UNAME='WID_TEXT_input2'  ,TITLE='��������'  $
      ,XSIZE=30 ,YSIZE=1,NOEDIT=1)


  WID_TEXT_output2 = CW_FIELD(WID_BASE_HYP_OUTPUT,  $
      UNAME='WID_TEXT_output2'  ,TITLE='�������'$
      ,XSIZE=30 ,YSIZE=1,NOEDIT=1)


;  WID_LABEL_output = Widget_Label(WID_BASE_HYP_PARA,  $
;      UNAME='WID_LABEL_output' ,XOFFSET=10 ,YOFFSET=53 ,SCR_XSIZE=56  $
;      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������')


  WID_BUTTON_input2 = Widget_Button(WID_BASE_HYP_INPUT,  $
      UNAME='WID_BUTTON_input2' ,SCR_XSIZE=44  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_input2, title:'����Ӱ���ļ���'}
  widget_control,WID_BUTTON_input2, set_uvalue=pfinfo,/NO_COPY

  WID_BUTTON_output2 = Widget_Button(WID_BASE_HYP_OUTPUT,  $
      UNAME='WID_BUTTON_output2' ,XOFFSET=272 ,YOFFSET=48  $
      ,SCR_XSIZE=44 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_output2, filter:'*.tif', title:'���Ӱ���ļ���'}
  widget_control,WID_BUTTON_output2, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_PARA=Widget_base(WID_BASE_Hyp,xoffset=10,yoffset=82,/FRAME,TITLE='ȥ���߲���' $
  	  ,xsize=230,ysize=90,COLUMN=1,YPAD=0,SPACE=0)

  WID_BASE_PARA_LABEL=Widget_base(WID_BASE_PARA,ROW=1,XPAD=5,YPAD=1)
    WID_LABEL_PARA=Widget_LABEL(WID_BASE_PARA_LABEL,UNAME='WID_LABEL_PARA' $
       ,VALUE='ȥ���߲���',FONT='DRAFT*13',/ALIGN_LEFT)

  WID_BASE_PARA_CTL=Widget_base(WID_BASE_PARA,COLUMN=1,YPAD=0)
	WID_BASE_GST=Widget_BASE(WID_BASE_PARA_CTL,/BASE_ALIGN_CENTER,ROW=1,YPAD=0)
	WID_BASE_PARA_LABEL_GST=Widget_base(WID_BASE_GST,COLUMN=1,XSIZE=45,/ALIGN_BOTTOM)
	WID_LABEL_GST=Widget_LABEL(WID_BASE_PARA_LABEL_GST,UNAME='WID_LABEL_GST' $
	  ,VALUE='GSTֵ:',/ALIGN_LEFT)
	WID_BASE_PARA_SLIDER_GST=Widget_base(WID_BASE_GST,COLUMN=1,YPAD=0)
  	WID_SLIDER_GST=Widget_SLIDER(WID_BASE_PARA_SLIDER_GST,/DRAG,MAXIMUM=35,MINIMUM=15 $
	  ,VALUE=21,XSIZE=110,UNAME='WID_SLIDER_GST')

	WID_BASE_BFNP=Widget_BASE(WID_BASE_PARA_CTL,/BASE_ALIGN_CENTER,ROW=1,YPAD=0)
	WID_BASE_PARA_LABEL_BFNP=Widget_base(WID_BASE_BFNP,COLUMN=1,XSIZE=45,/ALIGN_BOTTOM)
	WID_LABEL_BFNP=Widget_LABEL(WID_BASE_PARA_LABEL_BFNP,UNAME='WID_LABEL_BFNP' $
	  ,VALUE='BFNPֵ:',/ALIGN_LEFT)
	WID_BASE_PARA_SLIDER_BFNP=Widget_base(WID_BASE_BFNP,COLUMN=1,YPAD=0)
  	WID_SLIDER_BFNP=Widget_SLIDER(WID_BASE_PARA_SLIDER_BFNP,/DRAG,MAXIMUM=60,MINIMUM=20 $
	  ,VALUE=25,XSIZE=110,UNAME='WID_SLIDER_BFNP')

  WID_BASE_HYP_CMD = Widget_Base(WID_BASE_HYP,/FRAME,/ROW,/BASE_ALIGN_CENTER,XPAD=5,SPACE=37)

  WID_BUTTON_calculate2 = Widget_Button(WID_BASE_HYP_CMD,  $
      UNAME='WID_BUTTON_calculate2' ,XOFFSET=250 ,YOFFSET=85  $
      ,SCR_XSIZE=70 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='����')

  WID_BUTTON_calculate2 = Widget_Button(WID_BASE_HYP_CMD,  $
      UNAME='WID_BUTTON_help2' ,XOFFSET=250 ,YOFFSET=115  $
      ,SCR_XSIZE=70 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='����')

  WID_BUTTON_quit2 = Widget_Button(WID_BASE_HYP_CMD,  $
      UNAME='WID_BUTTON_quit2' ,XOFFSET=250 ,YOFFSET=145 ,SCR_XSIZE=70  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='�˳�')

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
  common_log,'�򿪸߹�������Ԥ�������'
  WID_BASE_preHSI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end