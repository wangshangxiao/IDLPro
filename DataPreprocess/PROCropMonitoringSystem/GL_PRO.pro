
;����ʱ�䣺2006.08.17
;������Ա��������
;ҵ�����ģ��������

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
;           temp=dialog_message('�����У�',title='��Ϣ')
;       end

		Widget_Info(wWidget, FIND_BY_UNAME='close'):begin
			widget_control,event.top,/destroy
		end

		widget_info(wWidget, FIND_BY_UNAME='help'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, 'ҵ�����ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
	;            temp=dialog_message('ϵͳ��û�а���')
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
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

   ;ҵ�����ģ�����
   COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
   base=widget_base(GROUP_LEADER=BASE_TOP,title='ҵ�����',$
                    XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET,SCR_XSIZE=141,$
                    SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)


   buttbase=widget_base(base,column=1,frame=1,xoffset=3,/align_center)
   button_xsize=120
   button_ysize=24

   ;�����ѯ
   xianshi_butt=widget_button(buttbase,value='�����ѯ',uname='xianshi',frame=0,$
            SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)

   ;�������
   diaodu_butt=widget_button(buttbase,value='�������',uname='diaodu',frame=0,$
            SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)

   ;���ȹ���, ��ģ��˼·��δ������������δ���У�2006-12-05
;   jindu_butt=widget_button(buttbase,value='���ȹ���',uname='jindu',frame=0,$
;            SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/menu,/align_center)
;;            event_pro='jindu_butt_event')
;
;
;		;����������
;		period_misssion_butt=widget_button(jindu_butt,value='����������',$
;		                                   uname='period_misssion')
;		;�Զ�������
;		custom_misssion_butt=widget_button(jindu_butt,value='�Զ�������',$
;		                                   uname='custom_misssion')

;
;   ;���ȹ���
;
;  jindu_base=widget_base(buttbase,/CONTEXT_MENU,uname='jindu_base')
;
;   		;����������
;		period_misssion_butt=widget_button(jindu_base,value='����������',$
;		                                   uname='period_misssion')
;		;�Զ�������
;		custom_misssion_butt=widget_button(jindu_base,value='�Զ�������',$
;		                                   uname='custom_misssion')


   ;"�ر�"��"����"������base
   elsebase=Widget_BASE(buttbase,FRAME=1,SCR_XSIZE=button_xsize,$
            SCR_YSIZE=button_ysize+6,/ALIGN_CENTER,/ROW)
   temp=55

   ;�ر�
   close_butt=Widget_Button(elsebase,VALUE='�ر�',uname='close',$
                           SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

   ;����
   help_butt=Widget_Button(elsebase,VALUE='����',uname='help',$
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