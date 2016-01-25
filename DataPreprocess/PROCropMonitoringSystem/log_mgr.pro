;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	02/24/2010 15:31.33
;
pro WID_BASE_LOG_event, Event

	on_error,2

  WIDGET_CONTROL, event.top, GET_UVALUE=PSTATE

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of
	 Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_CLEAR'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
			clearfile=(*PSTATE).logfile

			openw,lun,clearfile,/GET_LUN
			printf,lun,''
			FREE_LUN,lun

			widget_control,(*PSTATE).WID_TEXT_0,SET_value=''

    end

	 Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_QUIT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
;			common_log,'关闭日志管理'
			widget_control, event.top, /destroy
    end

    else:
  endcase

end

pro WID_BASE_LOG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

;  common_log,'启动日志管理'

  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB'  $
      ,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,SCR_XSIZE=500 ,SCR_YSIZE=310  $
      ,TITLE='日志管理' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1)


  WID_BASE_1 = Widget_Base(TLB, UNAME='WID_BASE_1' ,XOFFSET=2  $
      ,YOFFSET=2 ,SCR_XSIZE=495 ,SCR_YSIZE=244 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

  logfile='text\common_log.txt'
  openr,lun,logfile,/get_lun

  temp=fstat(lun)
  log_temp=bytarr(temp.SIZE)
  readu,lun,log_temp
  free_lun,lun

  log_text=string(log_temp)

  WID_TEXT_0 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_0'  $
      ,SCR_XSIZE=488 ,SCR_YSIZE=243 ,XSIZE=20 ,YSIZE=1,/SCROLL,value=log_text)


  WID_BASE_2 = Widget_Base(TLB, UNAME='WID_BASE_2' ,XOFFSET=2  $
      ,YOFFSET=245 ,SCR_XSIZE=471 ,SCR_YSIZE=37 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

  WID_BUTTON_CLEAR = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_CLEAR'  $
      ,XOFFSET=105 ,YOFFSET=4 ,SCR_XSIZE=71 ,SCR_YSIZE=27  $
      ,/ALIGN_CENTER ,VALUE='清空日志')

  WID_BUTTON_QUIT = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_QUIT'  $
      ,XOFFSET=305 ,YOFFSET=4 ,SCR_XSIZE=71 ,SCR_YSIZE=27  $
      ,/ALIGN_CENTER ,VALUE='退出')

  winfo = { logfile : logfile , $
  				WID_TEXT_0	:	WID_TEXT_0 $
  			  }

  PSTATE=PTR_NEW(winfo,/no_copy)
  widget_control,TLB, set_uvalue=PSTATE,/NO_COPY

  Widget_Control, /REALIZE, TLB

  XManager, 'WID_BASE_LOG', TLB, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro log_mgr, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WID_BASE_LOG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end