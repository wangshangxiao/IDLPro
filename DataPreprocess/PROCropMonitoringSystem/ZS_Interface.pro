
;**************************************************************************
;徐新刚,蒙继华
;系统设定模块的菜单,包括了菜单上的部分功能
;2006.07.26
;**************************************************************************
PRO CMD_ZS_RT_EVENT,EVENT
;该程序实现了右侧菜单的出现
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_ZS, FIND_BY_UNAME = 'ZS_Button_contextBase_RT')
     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_ZS, 124,3, contextBase
     RETURN
END

PRO CMD_ZS_PRO_EVENT,EVENT
;该程序实现了右侧菜单的出现
     widget_control,event.top,get_uvalue = pstate
     contextBase = WIDGET_INFO((*pstate).BASE_FRAME_ZS, FIND_BY_UNAME = 'ZS_Button_contextBase_PRO')
     WIDGET_DISPLAYCONTEXTMENU, (*pstate).BASE_FRAME_ZS, 124,34, contextBase
     RETURN
END

pro ZS_eventcb,event

    ;获取用户值
    WIDGET_CONTROL, Event.id, GET_UVALUE=uval
    widget_control, Event.top,Get_Uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

    CASE uval OF
     'CMD_ZS_RT_CLASSIFY': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         ZS_classify, GROUP_LEADER=wGroup
                         PRINT,'实时监测,差值分级'
                         end
     'CMD_ZS_RT_STA': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'实时监测,统计'
                         ZS_RT_STA, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
                         end

	 'CMD_ZS_RT_SUM': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'实时监测,汇总'
                         ZS_STA_RT_ABOVE_COUNTY,GROUP_LEADER=WGROUP
                         end

	 'CMD_ZS_PRO_STA': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'过程监测,统计'
                         ZS_PRO_STA, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
                         end

	 'CMD_ZS_PRO_HANTS':begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'过程监测,重构'
                         ZS_HANTS,GROUP_LEADER=WGROUP
                         end

     'CMD_ZS_PRO_SUM': 	begin
                         ;SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
                         PRINT,'过程监测,汇总'
                         ZS_STA_PRO_ABOVE_COUNTY,GROUP_LEADER=WGROUP
                         end

     'CMD_ZS_RESULT':begin
                         PRINT,'长势监测,结果分析'
						 ZS_JPG, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
                         end

    'CMD_CLOSE_ZS':begin
                         PRINT,'长势监测,关闭'
                         CLOSE,/all
     					 WIDGET_CONTROL, event.top, /destroy
                         end

	'CMD_HELP_ZS':begin

     					 PRINT,'长势监测,帮助'
     					 TXT_TEMP ='长势监测'

						if file_test('HELP\HELP.chm') then begin
							ONLINE_HELP,  "'长势监测模块'", BOOK='HELP\HELP.chm', /FULL_PATH
						endif else begin
							info_help=dialog_message('找不到帮助文档',title='警告')
						endelse

;     					 ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH, "'长势监测模块'"

                         end
      else :break
    Endcase

end

;;;;;;;;;;;;;界面生成-------------------
;---------------------------------------------------------------------------
pro ZS,GROUP_LEADER=wGroup

  X_TEMP=140+16+166
  Y_TEMP=0+114
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_ZS = Widget_Base(  GROUP_LEADER=wGroup,UNAME='BASE_TOP_ZS'  $
      ,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET ,SCR_XSIZE=141 $;,SCR_YSIZE=90  $
      ,TITLE='作物长势监测' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)
  button_xsize=120
  button_ysize=24

  BASE_FRAME_ZS = Widget_Base( BASE_TOP_ZS,XOFFSET=3,/FRAME,/column )

;;实时监测
  CMD_ZS_RT=Widget_Button(BASE_FRAME_ZS, UValue='CMD_DB_CONNECTION' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='实时监测',event_pro='CMD_ZS_RT_EVENT')
  ZS_Button_contextBase_RT = WIDGET_BASE(BASE_FRAME_ZS, /CONTEXT_MENU, UNAME  = 'ZS_Button_contextBase_RT')
  CMD_ZS_RT_CLASSIFY=Widget_Button(ZS_Button_contextBase_RT,UValue='CMD_ZS_RT_CLASSIFY',VALUE='差值分级')
  CMD_ZS_RT_STA=Widget_Button(ZS_Button_contextBase_RT,UValue='CMD_ZS_RT_STA',VALUE='统计到县')
  CMD_ZS_RT_SUM=Widget_Button(ZS_Button_contextBase_RT,UValue='CMD_ZS_RT_SUM',VALUE='汇总')


;;过程监测
  CMD_ZS_PRO=Widget_Button(BASE_FRAME_ZS, UValue='CMD_ZS_PRO' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='过程监测',event_pro='CMD_ZS_PRO_EVENT')
  ZS_Button_contextBase_PRO = WIDGET_BASE(BASE_FRAME_ZS, /CONTEXT_MENU, UNAME  = 'ZS_Button_contextBase_PRO')
  CMD_ZS_PRO_STA=Widget_Button(ZS_Button_contextBase_PRO,UValue='CMD_ZS_PRO_STA',VALUE='统计到县')
  CMD_ZS_PRO_HANTS=Widget_Button(ZS_Button_contextBase_PRO,UValue='CMD_ZS_PRO_HANTS',VALUE='曲线重构')
  CMD_ZS_PRO_SUM=Widget_Button(ZS_Button_contextBase_PRO,UValue='CMD_ZS_PRO_SUM',VALUE='汇总')




  ;用户自定义监测区
  CMD_ZS_RESULT = Widget_Button(BASE_FRAME_ZS, UValue='CMD_ZS_RESULT'  ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='监测结果成图')

  BASE_BTN_ZS = Widget_BASE(BASE_FRAME_ZS, UValue='BASE_BTN_ZS'  ,FRAME=1,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize+6  $
      ,/ALIGN_CENTER ,/ROW)
  temp=55
  CMD_CLOSE_ZS=Widget_Button(BASE_BTN_ZS,UValue='CMD_CLOSE_ZS',VALUE='关闭',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)
  CMD_HELP_ZS=Widget_Button(BASE_BTN_ZS,UValue='CMD_HELP_ZS',VALUE='帮助',SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

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
