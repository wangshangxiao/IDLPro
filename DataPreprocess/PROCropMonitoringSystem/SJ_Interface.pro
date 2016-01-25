;**************************************************************************
;徐新刚,蒙继华
;系统设定模块的菜单,包括了菜单上的部分功能
;2006.07.26
;**************************************************************************
PRO CMD_SJ_finddata_EVENT,EVENT
;该程序实现了右侧菜单的出现
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_SJ, FIND_BY_UNAME = 'SJ_Button_contextBase_RT')

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
    SJ_abnormalcheck,GROUP_LEADER=event.top
    PRINT,'数据检测'
    RETURN
END

PRO CMD_SJ_datamaintenance_EVENT,EVENT
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_SJ, FIND_BY_UNAME = 'SJ_Button_contextBase_PRO')
     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_SJ, 124,3, contextBase
     RETURN
END


PRO CMD_CLOSE_SJ_EVENT,EVENT
     PRINT,'数据管理,关闭'
     CLOSE,/all
	 WIDGET_CONTROL, event.top, /destroy
     RETURN
END


pro SJ_eventcb,event

;    wWidget =  Event.top
;    widget_control,wWidget,GET_UVALUE=groupleader

    WIDGET_CONTROL, Event.id, GET_UVALUE=uval
    widget_control, Event.top,Get_Uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

    CASE uval OF
     'CMD_NQ_datatodb': 	begin
                         SJ_todatabase_interface,GROUP_LEADER=event.top
                         PRINT,'数据入库'
                         end

    'CMD_NQ_datamaintance': 	begin
					 	 SJ_datamaintance_interface,GROUP_LEADER=event.top
                         PRINT,'数据维护'
                         end

    'CMD_CLOSE_SJ':begin
                         PRINT,'数据管理,关闭'
                         CLOSE,/all
     					 WIDGET_CONTROL, event.top, /destroy
                         end

	'CMD_HELP_SJ':begin

     					 ;ONLINE_HELP,  BOOK='帮助\HELP.chm', /FULL_PATH
     					 PRINT,'数据管理,帮助'

						if file_test('HELP\HELP.chm') then begin
							ONLINE_HELP, '数据管理模块', BOOK='HELP\HELP.chm', /FULL_PATH
						endif else begin
							info_help=dialog_message('找不到帮助文档',title='警告')
						endelse

;     					 ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,'数据管理模块'
     					 ;temp=dialog_message('系统暂没有帮助')

                         end
      else :break
    Endcase

end

;;;;;;;;;;;;;界面生成-------------------
;---------------------------------------------------------------------------
pro SJ,GROUP_LEADER=wGroup

  X_TEMP=140+16+166-83
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_SJ = Widget_Base(  GROUP_LEADER=wGroup,UNAME='BASE_TOP_SJ'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $,,SCR_YSIZE=155  $
      ,TITLE='数据管理' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  BASE_FRAME_SJ = Widget_Base( BASE_TOP_SJ,XOFFSET=3,/FRAME,/column )

;;数据检测
  CMD_SJ_finddata=Widget_Button(BASE_FRAME_SJ, UValue='CMD_DB_CONNECTION' ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='数据检测',event_pro='CMD_SJ_finddata_EVENT')
;;数据维护
;  CMD_SJ_PRO=Widget_Button(BASE_FRAME_SJ, UValue='CMD_SJ_PRO' ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
;      ,/ALIGN_CENTER ,VALUE='数据入库',event_pro='CMD_SJ_datamaintenance_EVENT')
   CMD_NQ_datatodb=Widget_Button(BASE_FRAME_SJ, UValue='CMD_NQ_datatodb' ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='数据入库',event_pro='')
;用户自定义监测区
   CMD_NQ_datamaintance=Widget_Button(BASE_FRAME_SJ, UValue='CMD_NQ_datamaintance' ,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='数据维护',event_pro='')


  BASE_BTN_SJ = Widget_BASE(BASE_FRAME_SJ, UValue='BASE_BTN_SJ'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  CMD_CLOSE_SJ=Widget_Button(BASE_BTN_SJ,UValue='CMD_CLOSE_SJ',VALUE='关闭',event_pro='CMD_CLOSE_SJ_EVENT',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  CMD_HELP_SJ=Widget_Button(BASE_BTN_SJ,UValue='CMD_HELP_SJ',VALUE='帮助',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

  state = { $
        BASE_FRAME_SJ  	: BASE_FRAME_SJ	$
        }
  PSTATE = PTR_NEW(STATE, /no_copy)
  Widget_Control, BASE_TOP_SJ, set_uvalue=PSTATE
  Widget_Control, /REALIZE, BASE_TOP_SJ
  WIDGET_CONTROL,CMD_CLOSE_SJ,/INPUT_FOCUS


  XManager, 'BASE_TOP_SJ',Event_handle='SJ_eventcb', BASE_TOP_SJ, /NO_BLOCK, cleanup='common_cleaner'

end

pro SJ_Interface,GROUP_LEADER=wGroup

	IF ( XREGISTERED('BASE_TOP_SJ') NE 0 ) THEN RETURN

    SJ,GROUP_LEADER=wGroup

end