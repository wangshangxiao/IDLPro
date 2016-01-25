;
; IDL Event Callback Procedures
; CONTACT_eventcb
;
; Generated on:	11/08/2004 10:42.27
;
;-----------------------------------------------------------------


;-----------------------------------------------------------------
PRO CMD_CONTACT_OK, Event

		common_log,'关闭联系'
     CLOSE,/all
     WIDGET_CONTROL, event.top, /destroy
   ;  return, Event ; By Default, return the event.

end
;
; Empty stub procedure used for autoloading.
;
pro CONTACT_eventcb
end

;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	11/08/2004 10:42.27
;
pro BASE_CONTACT_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CONTACT_OK'): begin
    end
    else:
  endcase

end

pro BASE_CONTACT, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_

	IF ( XREGISTERED('BASE_CONTACT') NE 0 ) THEN RETURN

  Resolve_Routine, 'CONTACT_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  ;定义一个变量来放按纽的边界宽度
	width=1


COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
BASE_CONTACT = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='BASE_CONTACT'  $
      ,XOFFSET=350 ,YOFFSET=200 ,SCR_XSIZE=310 ,SCR_YSIZE=158  $
      ,TITLE='省级农情遥感监测系统' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
      ,TLB_FRAME_ATTR=1);,/modal)


  CMD_CONTACT_AE = Widget_Button(BASE_CONTACT, UNAME='CMD_CONTACT_AE'  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=80 ,SCR_YSIZE=80  $
      ,/ALIGN_CENTER ,/bitmap,VALUE='image\AE_ABOUT.bmp',FRAME=width)

  LBL_CONTACT_5 = Widget_Label(BASE_CONTACT, UNAME='LBL_CONTACT_5'  $
      ,XOFFSET=92 ,YOFFSET=53 ,SCR_XSIZE=220 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='北京市朝阳区安定门外大屯路9718信箱')

  LBL_CONTACT_4 = Widget_Label(BASE_CONTACT, UNAME='LBL_CONTACT_4'  $
      ,XOFFSET=92 ,YOFFSET=38 ,SCR_XSIZE=200 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='E-mail:environ@irsa.ac.cn')

  LBL_CONTACT_3 = Widget_Label(BASE_CONTACT, UNAME='LBL_CONTACT_3'  $
      ,XOFFSET=92 ,YOFFSET=23 ,SCR_XSIZE=170 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='电话:010-64842375(6)')


  LBL_CONTACT_1 = Widget_Label(BASE_CONTACT, UNAME='LBL_CONTACT_1'  $
      ,XOFFSET=93 ,YOFFSET=8 ,SCR_XSIZE=145 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='中国科学院遥感应用研究所')


  LBL_CONTACT_2 = Widget_Label(BASE_CONTACT, UNAME='LBL_CONTACT_2'  $
      ,XOFFSET=92 ,YOFFSET=73 ,SCR_XSIZE=170 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='邮编:100101')


  CMD_CONTACT_OK = Widget_Button(BASE_CONTACT, UNAME='CMD_CONTACT_OK'  $
      ,XOFFSET=112 ,YOFFSET=96 ,SCR_XSIZE=80 ,SCR_YSIZE=24  $
      ,EVENT_PRO='CMD_CONTACT_OK' ,/ALIGN_CENTER ,VALUE='确定',FRAME=width)


  Widget_Control, /REALIZE, BASE_CONTACT
  WIDGET_CONTROL,CMD_CONTACT_OK,/INPUT_FOCUS

  XManager, 'BASE_CONTACT', BASE_CONTACT, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro CONTACT, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
	common_log,'启动联系'
  BASE_CONTACT, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
end