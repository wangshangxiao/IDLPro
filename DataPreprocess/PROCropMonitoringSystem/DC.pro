
;******************"��������"ģ��������¼�**********************************************
PRO DC_event,Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ? widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top
  widget_control,wWidget,GET_UVALUE=groupleader
  widget_control,wTarget,/INPUT_FOCUS

;  help,Event.id,/str
  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='TrendYieldBU'): DC_TrendYield,GROUP_LEADER=groupleader

    Widget_Info(wWidget, FIND_BY_UNAME='FloatBU'): BEGIN
  	     SubTLB = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'SubTLB')
	     contextBase = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'DB_Button_contextBase')
	     WIDGET_DISPLAYCONTEXTMENU,SubTLB, 124,31, contextBase
    END

    Widget_Info(wWidget, FIND_BY_UNAME='ParaCal'): DC_FloatYield,GROUP_LEADER=groupleader
    Widget_Info(wWidget, FIND_BY_UNAME='SpatialInt'): DC_SpatialExtrapolate,GROUP_LEADER=groupleader

    Widget_Info(wWidget, FIND_BY_UNAME='BioYieldBU'): BEGIN
  	     SubTLB = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'SubTLB')
	     BIO_contextBase = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'BIO_contextBase')
	     WIDGET_DISPLAYCONTEXTMENU,SubTLB, 124,56, BIO_contextBase
    END

    Widget_Info(wWidget, FIND_BY_UNAME='RatioCal'): DC_Bio_Ratio,GROUP_LEADER=groupleader
    Widget_Info(wWidget, FIND_BY_UNAME='HICal'): DC_Bio_HI,GROUP_LEADER=groupleader

    Widget_Info(wWidget, FIND_BY_UNAME='YieldIncorporationBU'): DC_YieldMerge,GROUP_LEADER=groupleader

    Widget_Info(wWidget, FIND_BY_UNAME='ResultYieldBU'): DC_SynthesisOutput,GROUP_LEADER=groupleader

    Widget_Info(wWidget, FIND_BY_UNAME='close_bu'):widget_control,Event.top,/DESTROY
    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu') :begin
    	if file_test('help\help.chm') then begin
    		ONLINE_HELP, BOOK='help\help.chm',/FULL_PATH,'����Ԥ�����ģ��'
    	endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
    end
    ELSE:
  ENDCASE

END
;------------------------------------------------------------------
;*******************************"����Ԥ�����"ģ�����*********************************
PRO DC,GROUP_LEADER=groupleader

	  ; IstoDB = C_CONN_DATABASE()   ;���������Ƚ�������ʱӦ��ȥ��,ͬʱ��Ҫ����"C_CONN_DATABASE"
	  ; IF NOT IstoDB THEN RETURN

  IF ( XREGISTERED('DC') NE 0 ) THEN RETURN   ;����������ҿ�,�����ٵ����´���.



  X_TEMP=140+265
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  DCYieldTLB = Widget_Base(GROUP_LEADER=groupleader,UNAME='DCYieldTLB'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='����Ԥ�����' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  SubTLB = Widget_Base(DCYieldTLB,XOFFSET=3,/FRAME,SPACE=1,/column,UNAME='SubTLB',/BASE_ALIGN_LEFT )

;;���Ʋ�������
  TrendYieldBU = Widget_Button(SubTLB, UNAME='TrendYieldBU',SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='���Ʋ�������')

;; ������������
  FloatBU=Widget_Button(SubTLB, UNAME='FloatBU'  ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='������������')
 ;; ����������������ģ��
	  DB_Button_contextBase = WIDGET_BASE(DCYieldTLB, /CONTEXT_MENU, UNAME  = 'DB_Button_contextBase')
	  ParaCal=Widget_Button(DB_Button_contextBase,UNAME='ParaCal',VALUE='������������')
	  SpatialInt=Widget_Button(DB_Button_contextBase,UNAME='SpatialInt',VALUE='�ռ��ֵ����')

  ;������-��������

;  BioYieldBU = Widget_Button(SubTLB, UNAME='BioYieldBU'   ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
;      ,/ALIGN_CENTER ,VALUE='������-��������')

 ;; ������-������������ģ��
	  BIO_contextBase = WIDGET_BASE(DCYieldTLB, /CONTEXT_MENU, UNAME  = 'BIO_contextBase')
	  RatioCal=Widget_Button(BIO_contextBase,UNAME='RatioCal',VALUE='�򵥱�ֵ������')
	  HICal=Widget_Button(BIO_contextBase,UNAME='HICal',VALUE='�ջ�ָ��������')


  YieldIncorporationBU = Widget_Button(SubTLB, UNAME='YieldIncorporationBU',SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='�����ںϷ���')

  ResultYieldBU = Widget_Button(SubTLB, UNAME='ResultYieldBU',SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='�����ʾ���')

  BASE_BTN_PATH_SETUP = Widget_BASE(SubTLB, UNAME='BASE_BTN_PATH_SETUP'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  Close_bu=Widget_Button(BASE_BTN_PATH_SETUP,UNAME='close_bu',VALUE='�ر�',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  Help_bu=Widget_Button(BASE_BTN_PATH_SETUP,UNAME='Help_bu',VALUE='����',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

 widget_control,DCYieldTLB,/REALIZE

 widget_control,Close_bu,/INPUT_FOCUS    ;ʹ�ùؼ���INPUT_FOCUS,���������ʵ��֮�������Ч

 widget_control,DCYieldTLB,SET_UVALUE=groupleader

 XManager, 'DC', DCYieldTLB, /NO_BLOCK

END