
;**************************************************************************
;���¸�,�ɼ̻�
;ϵͳ�趨ģ��Ĳ˵�,�����˲˵��ϵĲ��ֹ���
;2006.07.26
;**************************************************************************




pro CMD_HELP_NQ_EVENT,event
	 PRINT,'�����쳣���,����'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, "'ũҵ�������ģ��'", BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('�Ҳ��������ĵ�',title='����')
	endelse

;	 ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,"'ũҵ�������ģ��'"
	 ;temp=dialog_message('ϵͳ��û�а���')
end

PRO CMD_NQ_INTERPOLATE_EVENT,EVENT
;�ó���ʵ�����Ҳ�˵��ĳ���
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_NQ, FIND_BY_UNAME = 'ZS_Button_contextBase_RT')
;     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_NQ, 124,3, contextBase

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

    NQ_INTERPOLATE_intermain,GROUP_LEADER=BASE_TOP
    PRINT,'�����ֵ'
    RETURN
END

PRO CMD_NQ_Classify_EVENT,EVENT
;�ó���ʵ�����Ҳ�˵��ĳ���
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_NQ, FIND_BY_UNAME = 'ZS_Button_contextBase_PRO')
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
     COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
     NQ_classify_classifyagri,GROUP_LEADER=BASE_TOP
PRINT,'�Աȷ���'
     RETURN
END

PRO CMD_CLOSE_NQ_EVENT,EVENT
     PRINT,'ũ������,�ر�'
     CLOSE,/all
	 WIDGET_CONTROL, event.top, /destroy
     RETURN
END
pro NQ_eventcb,event


end

;;;;;;;;;;;;;��������-------------------
;---------------------------------------------------------------------------
pro NQ_interfaces,GROUP_LEADER=wGroup

	IF ( XREGISTERED('BASE_TOP_NQ') NE 0 ) THEN RETURN

  X_TEMP=140+16+166+332
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_NQ = Widget_Base(GROUP_LEADER=wGroup,UNAME='BASE_TOP_NQ'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='ũҵ�������' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  BASE_FRAME_NQ = Widget_Base( BASE_TOP_NQ,XOFFSET=3,/FRAME,/column )

;;�����ֵ
  CMD_NQ_INTERPOLATE=Widget_Button(BASE_FRAME_NQ, UValue='CMD_DB_CONNECTION' ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='�����ֵ',event_pro='CMD_NQ_INTERPOLATE_EVENT')
;;�Աȷ���
  CMD_ZS_PRO=Widget_Button(BASE_FRAME_NQ, UValue='CMD_ZS_PRO' ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='�Աȷ���',event_pro='CMD_NQ_Classify_EVENT')
 ;�û��Զ�������

  BASE_BTN_ZS = Widget_BASE(BASE_FRAME_NQ, UValue='BASE_BTN_ZS'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  CMD_CLOSE_NQ=Widget_Button(BASE_BTN_ZS,UValue='CMD_CLOSE_NQ',VALUE='�ر�',event_pro='CMD_CLOSE_NQ_EVENT',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  CMD_HELP_NQ=Widget_Button(BASE_BTN_ZS,UValue='CMD_HELP_NQ',VALUE='����',SCR_YSIZE=button_ysize,SCR_xSIZE=temp,event_pro='CMD_HELP_NQ_EVENT')

  state = { $
        BASE_FRAME_NQ  	: BASE_FRAME_NQ	$
        }
  PSTATE = PTR_NEW(STATE, /no_copy)
  Widget_Control, BASE_TOP_NQ, set_uvalue=PSTATE
  Widget_Control, /REALIZE, BASE_TOP_NQ
  WIDGET_CONTROL,CMD_CLOSE_NQ,/INPUT_FOCUS


  XManager, 'BASE_TOP_NQ',Event_handle='NQ_eventcb', BASE_TOP_NQ, /NO_BLOCK, cleanup='common_cleaner'

end

pro NQ_Interface,GROUP_LEADER=wGroup

    NQ_interfaces,GROUP_LEADER=wGroup

end
