
;**************************************************************************
;���¸�,�ɼ̻�
;ϵͳ�趨ģ��Ĳ˵�,�����˲˵��ϵĲ��ֹ���
;2006.07.26
;**************************************************************************
;PRO DB_CON_EVENT,EVENT
;;��
;	PRINT,'AAA'
;    widget_control,event.top,get_uvalue = pstate
;     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_PATH_SETUP, FIND_BY_UNAME = 'DB_Button_contextBase')
;     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_PATH_SETUP, 124,31, contextBase
;     RETURN
;END

pro SD_SETUP_eventcb,event

    ;��ȡ�û�ֵ
    WIDGET_CONTROL, Event.id, GET_UVALUE=uval
    widget_control, Event.top,Get_Uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

    CASE uval OF
     'CMD_PATH_SETUP': 	begin
                         SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         end
     'CMD_DB_Connect':	begin
     					 IF(yesORno EQ 1) THEN BEGIN
							TEMP=DIALOG_MESSAGE('���ݿ��Ѿ�����,Ҫ���½�������ô?',/QUESTION,title='�Ƿ����½�������')
							print,temp
							IF(TEMP EQ 'No') THEN RETURN
	 					 ENDIF

                         SD_DB_CONNECT, GROUP_LEADER=wGroup
                         end
;     'CMD_DB_DISConnect':begin
;                         WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;						 IF(yesORno EQ 1) THEN BEGIN
;							DBobj=OBJ_NEW('IDLdbDatabase')
;							yesORno=0
;							;ʹ�����˵�������
;				            COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
;				            WIDGET_CONTROL,MENU_OPERATION	,SENSITIVE=0
;				            WIDGET_CONTROL,MENU_MANAGE		,SENSITIVE=0
;							TEMP=DIALOG_MESSAGE('�ѶϿ����ݿ�',TITLE='����',/INFORMATION)
;						 ENDIF ELSE BEGIN
;							TEMP=DIALOG_MESSAGE('���ݿⲢû������',TITLE='����',/INFORMATION)
;						 ENDELSE;INTERFACE_MONITOR_RT,PSTATE, GROUP_LEADER=(*pstate).BASE_TOP_GROWTH_MONITOR
;                         end

     'CMD_ROI_DEFINE':begin
                         PRINT,'�û��Զ�������'
                         IF yesORno EQ 0 THEN BEGIN
					      	no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
					     ENDIF ELSE BEGIN
					      	PRINT,'������س���'            ;���ó������
					      	SD_ROI_MANAGE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
					     ENDELSE

                         end

     'CMD_CLOSE_PATH_SETUP':begin
                         CLOSE,/all
     					 WIDGET_CONTROL, event.top, /destroy

                         end



	'CMD_HELP_PATH_SETUP':begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, "'ϵͳ�趨ģ��'", BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;     					 ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,"'ϵͳ�趨ģ��'"
;                         end
      else :break
    Endcase

end

;;;;;;;;;;;;;��������-------------------
;---------------------------------------------------------------------------
pro SD_SETUP,GROUP_LEADER=wGroup

  X_TEMP=140+16
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  HELP,X_OFFSET
  HELP,Y_OFFSET
  BASE_TOP_PATH_SETUP = Widget_Base(  GROUP_LEADER=wGroup,UNAME='BASE_TOP_PATH_SETUP'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='ϵͳ�趨' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  BASE_FRAME_PATH_SETUP = Widget_Base( BASE_TOP_PATH_SETUP,XOFFSET=3,/FRAME,/column )

;;·������
  CMD_PATH_SETUP = Widget_Button(BASE_FRAME_PATH_SETUP, UValue='CMD_PATH_SETUP'  ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='·������')

;; ���ݿ����ӹ���
  CMD_DB_Connect=Widget_Button(BASE_FRAME_PATH_SETUP, UValue='CMD_DB_Connect' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='���ݿ�����')
;  DB_Button_contextBase = WIDGET_BASE(BASE_FRAME_PATH_SETUP, /CONTEXT_MENU, UNAME  = 'DB_Button_contextBase')
;  CMD_DB_Connect=Widget_Button(DB_Button_contextBase,UValue='CMD_DB_Connect',VALUE='�������ݿ�')
;  CMD_DB_DISConnect=Widget_Button(DB_Button_contextBase,UValue='CMD_DB_DISConnect',VALUE='�Ͽ����ݿ�')

  ;�û��Զ�������
  CMD_ROI_DEFINE = Widget_Button(BASE_FRAME_PATH_SETUP, UValue='CMD_ROI_DEFINE'  ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='�Զ�������')

  BASE_BTN_PATH_SETUP = Widget_BASE(BASE_FRAME_PATH_SETUP, UValue='BASE_BTN_PATH_SETUP'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  CMD_CLOSE_PATH_SETUP=Widget_Button(BASE_BTN_PATH_SETUP,UValue='CMD_CLOSE_PATH_SETUP',VALUE='�ر�',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  CMD_HELP_PATH_SETUP=Widget_Button(BASE_BTN_PATH_SETUP,UValue='CMD_HELP_PATH_SETUP',VALUE='����',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

  state = { $
        BASE_FRAME_PATH_SETUP  	: BASE_FRAME_PATH_SETUP	$
        }
  PSTATE = PTR_NEW(STATE, /no_copy)
  Widget_Control, BASE_TOP_PATH_SETUP, set_uvalue=PSTATE
  Widget_Control, /REALIZE, BASE_TOP_PATH_SETUP
  WIDGET_CONTROL,CMD_CLOSE_PATH_SETUP,/INPUT_FOCUS


  XManager, 'BASE_TOP_PATH_SETUP',Event_handle='SD_SETUP_eventcb', BASE_TOP_PATH_SETUP, /NO_BLOCK, cleanup='common_cleaner'

end

pro SD_INTERFACE,GROUP_LEADER=wGroup

  IF ( XREGISTERED('BASE_TOP_PATH_SETUP') NE 0 ) THEN RETURN

  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  SD_SETUP,GROUP_LEADER=BASE_TOP

end

pro common_cleaner, id
	WIDGET_CONTROL,id,get_uvalue=pstate
	heap_free,pstate
end

