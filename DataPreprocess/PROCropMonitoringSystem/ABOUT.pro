;
; IDL Event Callback Procedures
; ABOUT_eventcb
;
; Generated on:	11/08/2004 10:42.27
;
;-----------------------------------------------------------------


;-----------------------------------------------------------------
PRO CMD_ABOUT_OK, Event

     CLOSE,/all
     WIDGET_CONTROL, event.top, /destroy
   ;  return, Event ; By Default, return the event.

end
;
; Empty stub procedure used for autoloading.
;
pro ABOUT_eventcb
end

;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	11/08/2004 10:42.27
;
pro BASE_ABOUT_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_ABOUT_OK'): begin
    end
    else:
  endcase

end

pro BASE_ABOUT, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_

	IF ( XREGISTERED('BASE_ABOUT') NE 0 ) THEN RETURN

  Resolve_Routine, 'ABOUT_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  ;定义一个变量来放按纽的边界宽度
	width=1

;  BASE_ABOUT = Widget_Base( GROUP_LEADER=wGroup, UNAME='BASE_ABOUT'  $
;      ,XOFFSET=350 ,YOFFSET=200 ,SCR_XSIZE=219 ,SCR_YSIZE=158  $
;      ,TITLE='省级农情遥感监测系统' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
;      ,TLB_FRAME_ATTR=1);,/modal)
;
;
;  CMD_ABOUT_AE = Widget_Button(BASE_ABOUT, UNAME='CMD_ABOUT_AE'  $
;      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=80 ,SCR_YSIZE=80  $
;      ,/ALIGN_CENTER ,/bitmap,VALUE='image\AE_ABOUT.bmp',FRAME=width)
;
;  LBL_ABOUT_5 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_5'  $
;      ,XOFFSET=92 ,YOFFSET=53 ,SCR_XSIZE=115 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='与生态环境课题组')
;
;  LBL_ABOUT_4 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_4'  $
;      ,XOFFSET=92 ,YOFFSET=38 ,SCR_XSIZE=115 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='研究所应用中心农业')
;
;  LBL_ABOUT_3 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_3'  $
;      ,XOFFSET=92 ,YOFFSET=23 ,SCR_XSIZE=115 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='中国科学院遥感应用')
;
;
;  LBL_ABOUT_1 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_1'  $
;      ,XOFFSET=93 ,YOFFSET=8 ,SCR_XSIZE=115 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='版权所有：')
;
;
;  LBL_ABOUT_2 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_2'  $
;      ,XOFFSET=92 ,YOFFSET=73 ,SCR_XSIZE=114 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='版本信息：Version 1.1')
COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
BASE_ABOUT = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='BASE_ABOUT'  $
      ,XOFFSET=380 ,YOFFSET=200 ,SCR_XSIZE=250 ,SCR_YSIZE=158  $
      ,TITLE='省级农情遥感监测系统' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
      ,TLB_FRAME_ATTR=1);,/modal)


  CMD_ABOUT_AE = Widget_Button(BASE_ABOUT, UNAME='CMD_ABOUT_AE'  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=80 ,SCR_YSIZE=80  $
      ,/ALIGN_CENTER ,/bitmap,VALUE='image\AE_ABOUT.bmp',FRAME=width)

  LBL_ABOUT_5 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_5'  $
      ,XOFFSET=92 ,YOFFSET=53 ,SCR_XSIZE=115 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='')

  LBL_ABOUT_4 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_4'  $
      ,XOFFSET=92 ,YOFFSET=38 ,SCR_XSIZE=170 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='农业与生态遥感研究室')

  LBL_ABOUT_3 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_3'  $
      ,XOFFSET=92 ,YOFFSET=23 ,SCR_XSIZE=170 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='中国科学院遥感应用研究所')


  LBL_ABOUT_1 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_1'  $
      ,XOFFSET=93 ,YOFFSET=8 ,SCR_XSIZE=115 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='版权所有：')


  LBL_ABOUT_2 = Widget_Label(BASE_ABOUT, UNAME='LBL_ABOUT_2'  $
      ,XOFFSET=92 ,YOFFSET=73 ,SCR_XSIZE=170 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='版本信息：Version 2.1')


  CMD_ABOUT_OK = Widget_Button(BASE_ABOUT, UNAME='CMD_ABOUT_OK'  $
      ,XOFFSET=79 ,YOFFSET=96 ,SCR_XSIZE=80 ,SCR_YSIZE=24  $
      ,EVENT_PRO='CMD_ABOUT_OK' ,/ALIGN_CENTER ,VALUE='确定',FRAME=width);)


  Widget_Control, /REALIZE, BASE_ABOUT
  WIDGET_CONTROL,CMD_ABOUT_OK,/INPUT_FOCUS

  XManager, 'BASE_ABOUT', BASE_ABOUT, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro ABOUT, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
	common_log,'关于系统'
  BASE_ABOUT, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
end
