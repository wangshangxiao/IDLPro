
;创建时间：2006.08.17
;创建人员：杨绍锷
;业务管理模块界面设计

;
;PRO jindu_butt_event,EVENT
;
;;	PRINT,'AAA'
;    widget_control,event.top,get_uvalue = pstate
;     contextBase = WIDGET_INFO((*pstate).buttbase, FIND_BY_UNAME = 'jindu_base')
;     WIDGET_DISPLAYCONTEXTMENU, (*pstate).buttbase, 124,31, contextBase
;     PRINT,'AAA'
;     RETURN
;END



pro GL_PRO_event,event

   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
              widget_info(Event.id, /tree_root) : event.id)

   wWidget =  Event.top
   ;widget_control,wWidget,GET_UVALUE=GLleader

   CASE wTarget OF

       Widget_Info(wWidget, FIND_BY_UNAME='xianshi'):GL_xianshi;,group_leader=GLleader
       Widget_Info(wWidget, FIND_BY_UNAME='diaodu'):GL_diaodu;,group_leader=GLleader
;       widget_info(wWidget,FIND_BY_UNAME='period_misssion'):GL_jindu_period_mission
;       widget_info(wWidget,FIND_BY_UNAME='custom_misssion'):GL_jindu_custom_mission
;       Widget_Info(wWidget, FIND_BY_UNAME='jindu'):begin
;;            GL_jindu;,group_leader=GLleader
;           temp=dialog_message('开发中！',title='信息')
;       end

		Widget_Info(wWidget, FIND_BY_UNAME='close'):begin
			widget_control,event.top,/destroy
		end

		widget_info(wWidget, FIND_BY_UNAME='help'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '业务管理模块', BOOK='HELP\HELP.chm', /FULL_PATH
	;            temp=dialog_message('系统暂没有帮助')
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end

      ELSE:

   ENDCASE


end




;*******************************************************************
pro GL_PRO,GROUP_LEADER=wGroup

   IF ( XREGISTERED('GL_PRO') NE 0 ) THEN RETURN

   X_TEMP=770
   Y_TEMP=0+114

   ;业务管理模块界面
   COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
   base=widget_base(GROUP_LEADER=BASE_TOP,title='业务管理',$
                    XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET,SCR_XSIZE=141,$
                    SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)


   buttbase=widget_base(base,column=1,frame=1,xoffset=3,/align_center)
   button_xsize=120
   button_ysize=24

   ;任务查询
   xianshi_butt=widget_button(buttbase,value='任务查询',uname='xianshi',frame=0,$
            SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)

   ;任务调度
   diaodu_butt=widget_button(buttbase,value='任务调度',uname='diaodu',frame=0,$
            SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)

   ;进度管理, 该模块思路尚未清晰，开发尚未进行，2006-12-05
;   jindu_butt=widget_button(buttbase,value='进度管理',uname='jindu',frame=0,$
;            SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/menu,/align_center)
;;            event_pro='jindu_butt_event')
;
;
;		;周期性任务
;		period_misssion_butt=widget_button(jindu_butt,value='周期性任务',$
;		                                   uname='period_misssion')
;		;自定义任务
;		custom_misssion_butt=widget_button(jindu_butt,value='自定义任务',$
;		                                   uname='custom_misssion')

;
;   ;进度管理
;
;  jindu_base=widget_base(buttbase,/CONTEXT_MENU,uname='jindu_base')
;
;   		;周期性任务
;		period_misssion_butt=widget_button(jindu_base,value='周期性任务',$
;		                                   uname='period_misssion')
;		;自定义任务
;		custom_misssion_butt=widget_button(jindu_base,value='自定义任务',$
;		                                   uname='custom_misssion')


   ;"关闭"与"帮助"按键的base
   elsebase=Widget_BASE(buttbase,FRAME=1,SCR_XSIZE=button_xsize,$
            SCR_YSIZE=button_ysize+6,/ALIGN_CENTER,/ROW)
   temp=55

   ;关闭
   close_butt=Widget_Button(elsebase,VALUE='关闭',uname='close',$
                           SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

   ;帮助
   help_butt=Widget_Button(elsebase,VALUE='帮助',uname='help',$
                          SCR_YSIZE=button_ysize,SCR_xSIZE=temp)


;  state = { $
;        buttbase  	: buttbase	$
;        }
;  PSTATE = PTR_NEW(STATE, /no_copy)

   widget_control,base,/realize
   WIDGET_CONTROL,close_butt,/INPUT_FOCUS
;
;COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
;  GL_PRO,GROUP_LEADER=BASE_TOP


   xmanager,'GL_PRO',base,/no_block

end