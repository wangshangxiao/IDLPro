
;**************************************************************************
;徐新刚,蒙继华
;系统设定模块的菜单,包括了菜单上的部分功能
;2006.07.26
;**************************************************************************
;PRO DB_CON_EVENT,EVENT
;;该
;	PRINT,'AAA'
;    widget_control,event.top,get_uvalue = pstate
;     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_PATH_SETUP, FIND_BY_UNAME = 'DB_Button_contextBase')
;     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_PATH_SETUP, 124,31, contextBase
;     RETURN
;END

pro SD_SETUP_eventcb,event

    ;获取用户值
    WIDGET_CONTROL, Event.id, GET_UVALUE=uval
    widget_control, Event.top,Get_Uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

    CASE uval OF
     'CMD_PATH_SETUP': 	begin
                         SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         end
     'CMD_DB_Connect':	begin
     					 IF(yesORno EQ 1) THEN BEGIN
							TEMP=DIALOG_MESSAGE('数据库已经连接,要重新进行连接么?',/QUESTION,title='是否重新进行链接')
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
;							;使操作菜单不可用
;				            COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
;				            WIDGET_CONTROL,MENU_OPERATION	,SENSITIVE=0
;				            WIDGET_CONTROL,MENU_MANAGE		,SENSITIVE=0
;							TEMP=DIALOG_MESSAGE('已断开数据库',TITLE='警告',/INFORMATION)
;						 ENDIF ELSE BEGIN
;							TEMP=DIALOG_MESSAGE('数据库并没有连接',TITLE='警告',/INFORMATION)
;						 ENDELSE;INTERFACE_MONITOR_RT,PSTATE, GROUP_LEADER=(*pstate).BASE_TOP_GROWTH_MONITOR
;                         end

     'CMD_ROI_DEFINE':begin
                         PRINT,'用户自定义监测区'
                         IF yesORno EQ 0 THEN BEGIN
					      	no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
					     ENDIF ELSE BEGIN
					      	PRINT,'调用相关程序'            ;调用程序界面
					      	SD_ROI_MANAGE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
					     ENDELSE

                         end

     'CMD_CLOSE_PATH_SETUP':begin
                         CLOSE,/all
     					 WIDGET_CONTROL, event.top, /destroy

                         end



	'CMD_HELP_PATH_SETUP':begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, "'系统设定模块'", BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;     					 ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,"'系统设定模块'"
;                         end
      else :break
    Endcase

end

;;;;;;;;;;;;;界面生成-------------------
;---------------------------------------------------------------------------
pro SD_SETUP,GROUP_LEADER=wGroup

  X_TEMP=140+16
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  HELP,X_OFFSET
  HELP,Y_OFFSET
  BASE_TOP_PATH_SETUP = Widget_Base(  GROUP_LEADER=wGroup,UNAME='BASE_TOP_PATH_SETUP'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='系统设定' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  BASE_FRAME_PATH_SETUP = Widget_Base( BASE_TOP_PATH_SETUP,XOFFSET=3,/FRAME,/column )

;;路径设置
  CMD_PATH_SETUP = Widget_Button(BASE_FRAME_PATH_SETUP, UValue='CMD_PATH_SETUP'  ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='路径设置')

;; 数据库链接管理
  CMD_DB_Connect=Widget_Button(BASE_FRAME_PATH_SETUP, UValue='CMD_DB_Connect' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='数据库链接')
;  DB_Button_contextBase = WIDGET_BASE(BASE_FRAME_PATH_SETUP, /CONTEXT_MENU, UNAME  = 'DB_Button_contextBase')
;  CMD_DB_Connect=Widget_Button(DB_Button_contextBase,UValue='CMD_DB_Connect',VALUE='连接数据库')
;  CMD_DB_DISConnect=Widget_Button(DB_Button_contextBase,UValue='CMD_DB_DISConnect',VALUE='断开数据库')

  ;用户自定义监测区
  CMD_ROI_DEFINE = Widget_Button(BASE_FRAME_PATH_SETUP, UValue='CMD_ROI_DEFINE'  ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='自定义监测区')

  BASE_BTN_PATH_SETUP = Widget_BASE(BASE_FRAME_PATH_SETUP, UValue='BASE_BTN_PATH_SETUP'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  CMD_CLOSE_PATH_SETUP=Widget_Button(BASE_BTN_PATH_SETUP,UValue='CMD_CLOSE_PATH_SETUP',VALUE='关闭',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  CMD_HELP_PATH_SETUP=Widget_Button(BASE_BTN_PATH_SETUP,UValue='CMD_HELP_PATH_SETUP',VALUE='帮助',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

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

