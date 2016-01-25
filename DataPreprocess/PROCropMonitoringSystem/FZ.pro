


PRO FZ_event,Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ? widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top
  widget_control,wWidget,GET_UVALUE=groupleader
  widget_control,wTarget,/INPUT_FOCUS

;  help,Event.id,/str
  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='IndexCal'): FZ_Calculate,GROUP_LEADER=groupleader
    Widget_Info(wWidget, FIND_BY_UNAME='IndexStatistic'): FZ_Statistic,GROUP_LEADER=groupleader
    Widget_Info(wWidget, FIND_BY_UNAME='close_bu'):widget_control,Event.top,/DESTROY
    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu') :begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '����ָ�����ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;    ONLINE_HELP, BOOK='help\help.chm',/FULL_PATH,'����ָ�����ģ��'
    ELSE:
  ENDCASE

END
;------------------------------------------------------------------
;*******************************"������-����"ģ�����*********************************
PRO FZ,GROUP_LEADER=groupleader

	   IF ( XREGISTERED('FZ') NE 0 ) THEN RETURN   ;����������ҿ�,�����ٵ����´���.

;--------------------------------------------------------------------------
  X_TEMP=140+596
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  FZ_TLB = Widget_Base(GROUP_LEADER=groupleader,UNAME='FZ_TLB'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='����ָ������' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  SubTLB = Widget_Base(FZ_TLB,XOFFSET=3,/FRAME,/column,UNAME='SubTLB' )

;;����ָ������
  IndexCal = Widget_Button(SubTLB, UNAME='IndexCal',SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='����ָ������')

;; ����ָ��ͳ��
  IndexStatistic=Widget_Button(SubTLB, UNAME='IndexStatistic',SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='����ָ��ͳ��')

  BASE_BTN_PATH_SETUP = Widget_BASE(SubTLB, UNAME='BASE_BTN_PATH_SETUP'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  Close_bu=Widget_Button(BASE_BTN_PATH_SETUP,UNAME='close_bu',VALUE='�ر�',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  Help_bu=Widget_Button(BASE_BTN_PATH_SETUP,UNAME='Help_bu',VALUE='����',SCR_YSIZE=button_ysize,SCR_xSIZE=temp,ACCELERATOR = 'F1')
  ;***************************************************************

	 widget_control,FZ_TLB,/REALIZE

	 widget_control,Close_bu,/INPUT_FOCUS    ;ʹ�ùؼ���INPUT_FOCUS,���������ʵ��֮�������Ч

	 widget_control,FZ_TLB,SET_UVALUE=groupleader
	 XManager, 'FZ', FZ_TLB, /NO_BLOCK

END
