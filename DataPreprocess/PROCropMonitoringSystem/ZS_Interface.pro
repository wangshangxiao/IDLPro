
;**************************************************************************
;���¸�,�ɼ̻�
;ϵͳ�趨ģ��Ĳ˵�,�����˲˵��ϵĲ��ֹ���
;2006.07.26
;**************************************************************************
PRO CMD_ZS_RT_EVENT,EVENT
;�ó���ʵ�����Ҳ�˵��ĳ���
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_ZS, FIND_BY_UNAME = 'ZS_Button_contextBase_RT')
     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_ZS, 124,3, contextBase
     RETURN
END

PRO CMD_ZS_PRO_EVENT,EVENT
;�ó���ʵ�����Ҳ�˵��ĳ���
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_ZS, FIND_BY_UNAME = 'ZS_Button_contextBase_PRO')
     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_ZS, 124,34, contextBase
     RETURN
END

pro ZS_eventcb,event

    ;��ȡ�û�ֵ
    WIDGET_CONTROL, Event.id, GET_UVALUE=uval
    widget_control, Event.top,Get_Uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

    CASE uval OF
     'CMD_ZS_RT_CLASSIFY': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         ZS_classify, GROUP_LEADER=wGroup
                         PRINT,'ʵʱ���,��ֵ�ּ�'
                         end
     'CMD_ZS_RT_STA': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'ʵʱ���,ͳ��'
                         ZS_RT_STA, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
                         end

	 'CMD_ZS_RT_SUM': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'ʵʱ���,����'
                         ZS_STA_RT_ABOVE_COUNTY,GROUP_LEADER=WGROUP
                         end

	 'CMD_ZS_PRO_STA': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'���̼��,ͳ��'
                         ZS_PRO_STA, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
                         end

	 'CMD_ZS_PRO_HANTS':begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'���̼��,�ع�'
                         ZS_HANTS,GROUP_LEADER=WGROUP
                         end

     'CMD_ZS_PRO_SUM': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'���̼��,����'
                         ZS_STA_PRO_ABOVE_COUNTY,GROUP_LEADER=WGROUP
                         end

     'CMD_ZS_RESULT':begin
                         PRINT,'���Ƽ��,�������'
						 ZS_JPG, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
                         end

    'CMD_CLOSE_ZS':begin
                         PRINT,'���Ƽ��,�ر�'
                         CLOSE,/all
     					 WIDGET_CONTROL, event.top, /destroy
                         end

	'CMD_HELP_ZS':begin

     					 PRINT,'���Ƽ��,����'
     					 TXT_TEMP ='���Ƽ��'

						if file_test('HELP\HELP.chm') then begin
							ONLINE_HELP,  "'���Ƽ��ģ��'", BOOK='HELP\HELP.chm', /FULL_PATH
						endif else begin
							info_help=dialog_message('�Ҳ��������ĵ�',title='����')
						endelse

;     					 ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH, "'���Ƽ��ģ��'"

                         end
      else :break
    Endcase

end

;;;;;;;;;;;;;��������-------------------
;---------------------------------------------------------------------------
pro ZS,GROUP_LEADER=wGroup

  X_TEMP=140+16+166
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_ZS = Widget_Base(  GROUP_LEADER=wGroup,UNAME='BASE_TOP_ZS'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='���ﳤ�Ƽ��' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  BASE_FRAME_ZS = Widget_Base( BASE_TOP_ZS,XOFFSET=3,/FRAME,/column )

;;ʵʱ���
  CMD_ZS_RT=Widget_Button(BASE_FRAME_ZS, UValue='CMD_DB_CONNECTION' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='ʵʱ���',event_pro='CMD_ZS_RT_EVENT')
  ZS_Button_contextBase_RT = WIDGET_BASE(BASE_FRAME_ZS, /CONTEXT_MENU, UNAME  = 'ZS_Button_contextBase_RT')
  CMD_ZS_RT_CLASSIFY=Widget_Button(ZS_Button_contextBase_RT,UValue='CMD_ZS_RT_CLASSIFY',VALUE='��ֵ�ּ�')
  CMD_ZS_RT_STA=Widget_Button(ZS_Button_contextBase_RT,UValue='CMD_ZS_RT_STA',VALUE='ͳ�Ƶ���')
  CMD_ZS_RT_SUM=Widget_Button(ZS_Button_contextBase_RT,UValue='CMD_ZS_RT_SUM',VALUE='����')


;;���̼��
  CMD_ZS_PRO=Widget_Button(BASE_FRAME_ZS, UValue='CMD_ZS_PRO' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='���̼��',event_pro='CMD_ZS_PRO_EVENT')
  ZS_Button_contextBase_PRO = WIDGET_BASE(BASE_FRAME_ZS, /CONTEXT_MENU, UNAME  = 'ZS_Button_contextBase_PRO')
  CMD_ZS_PRO_STA=Widget_Button(ZS_Button_contextBase_PRO,UValue='CMD_ZS_PRO_STA',VALUE='ͳ�Ƶ���')
  CMD_ZS_PRO_HANTS=Widget_Button(ZS_Button_contextBase_PRO,UValue='CMD_ZS_PRO_HANTS',VALUE='�����ع�')
  CMD_ZS_PRO_SUM=Widget_Button(ZS_Button_contextBase_PRO,UValue='CMD_ZS_PRO_SUM',VALUE='����')




  ;�û��Զ�������
  CMD_ZS_RESULT = Widget_Button(BASE_FRAME_ZS, UValue='CMD_ZS_RESULT'  ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='�������ͼ')

  BASE_BTN_ZS = Widget_BASE(BASE_FRAME_ZS, UValue='BASE_BTN_ZS'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  CMD_CLOSE_ZS=Widget_Button(BASE_BTN_ZS,UValue='CMD_CLOSE_ZS',VALUE='�ر�',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  CMD_HELP_ZS=Widget_Button(BASE_BTN_ZS,UValue='CMD_HELP_ZS',VALUE='����',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

  state = { $
        BASE_FRAME_ZS  	: BASE_FRAME_ZS	$
        }
  PSTATE = PTR_NEW(STATE, /no_copy)
  Widget_Control, BASE_TOP_ZS, set_uvalue=PSTATE
  Widget_Control, /REALIZE, BASE_TOP_ZS
  WIDGET_CONTROL,CMD_CLOSE_ZS,/INPUT_FOCUS


  XManager, 'BASE_TOP_ZS',Event_handle='ZS_eventcb', BASE_TOP_ZS, /NO_BLOCK, cleanup='common_cleaner'

end

pro ZS_INTERFACE,GROUP_LEADER=wGroup

  IF ( XREGISTERED('BASE_TOP_ZS') NE 0 ) THEN RETURN

  ZS,GROUP_LEADER=wGroup

end
