


pro MJ_Interface_event,event

   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
              widget_info(Event.id, /tree_root) : event.id)

   wWidget =  Event.top
   ;widget_control,wWidget,GET_UVALUE=GLleader

   CASE wTarget OF

;       widget_Info(wWidget, FIND_BY_UNAME='butt_Unsupervise_Classify'):begin   ;�Ǽල����
;;            MJ_Unsupervise_Classify
;           ;temp=dialog_message('�����У�',title='��Ϣ')
;           MJ_class, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;          end

;       widget_Info(wWidget, FIND_BY_UNAME='butt_ClassifyID'):begin      ;�����ʶ
;;            MJ_ClassifyID
;           ;temp=dialog_message('�����У�',title='��Ϣ')
;           MJ_recode, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;          end

       widget_Info(wWidget, FIND_BY_UNAME='butt_STA'):begin           ;ͳ��
;            MJ_STA
          ; temp=dialog_message('�����У�',title='��Ϣ')
           ;MJ_stat, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
           MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
          end


	   Widget_Info(wWidget, FIND_BY_UNAME='butt_Extrapolate'): BEGIN              ;����
		  	     buttbase = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'buttbase')
			     contextBase = WIDGET_INFO(buttbase,FIND_BY_UNAME = 'butt_Extrapolate_contextBase')
			     WIDGET_DISPLAYCONTEXTMENU,buttbase, 124,29, contextBase
		  END

	    	widget_Info(wWidget, FIND_BY_UNAME='butt_Extrapolate_GVG'):begin           ;GVG����
			;            MJ_STA
			          ; temp=dialog_message('�����У�',title='��Ϣ')
			           MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
			          end

			widget_Info(wWidget, FIND_BY_UNAME='butt_Extrapolate_proportion'):begin           ;��ֲ��������
			;            MJ_STA
			          ; temp=dialog_message('�����У�',title='��Ϣ')
			           MJ_Extrapolate_Proportion, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
			          end





       widget_Info(wWidget, FIND_BY_UNAME='butt_Area_Calculate'):begin      ;�������
;            MJ_Area_Calculate
          ; temp=dialog_message('�����У�',title='��Ϣ')
           MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
          end

	    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;�ر�
			widget_control,event.top,/destroy
	      end

	    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;����

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '�������ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;			ONLINE_HELP,'�������ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
;
;		  end

      ELSE:

   ENDCASE


end




;*******************************************************************
pro MJ_Interface,GROUP_LEADER=wGroup

   IF ( XREGISTERED('base') NE 0 ) THEN RETURN

   X_TEMP=460+28
   Y_TEMP=0+114

   ;�������ģ�����
   COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
   base=widget_base(GROUP_LEADER=BASE_TOP, title='�������',$
                    XOFFSET=X_TEMP+X_OFFSET-140,YOFFSET=Y_TEMP+Y_OFFSET,SCR_XSIZE=141,$
                    SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)


   buttbase=widget_base(base,column=1,frame=1,xoffset=3,/align_center,UNAME='buttbase')
   button_xsize=120
   button_ysize=24

;   ;�Ǽල����
;   butt_Unsupervise_Classify=widget_button(buttbase,value='�Ǽල����'   $
;            ,uname='butt_Unsupervise_Classify',frame=0$
;            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)
;
;   ;�����ʶ
;   butt_ClassifyID=widget_button(buttbase,value='�����ʶ'  $
;            ,uname='butt_ClassifyID',frame=0   $
;            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)

   ;ͳ��
   butt_STA=widget_button(buttbase,value='��ֲ��������'    $
            ,uname='butt_STA',frame=0    $
            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)



;;����
  butt_Extrapolate=Widget_Button(buttbase, UNAME='butt_Extrapolate' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='��������')
  butt_Extrapolate_contextBase = WIDGET_BASE(buttbase,/CONTEXT_MENU, UNAME  = 'butt_Extrapolate_contextBase')
  butt_Extrapolate_GVG=Widget_Button(butt_Extrapolate_contextBase,UNAME='butt_Extrapolate_GVG',VALUE='�����������')
  butt_Extrapolate_proportion=Widget_Button(butt_Extrapolate_contextBase,UNAME='butt_Extrapolate_proportion',VALUE='��ֲ��������')


   ;�������
   butt_Area_Calculate=widget_button(buttbase,value='�������'    $
            ,uname='butt_Area_Calculate',frame=0    $
            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)



   ;"�ر�"��"����"������base
   elsebase=Widget_BASE(buttbase,FRAME=1,SCR_XSIZE=button_xsize,$
            SCR_YSIZE=button_ysize+6,/ALIGN_CENTER,/ROW)
   temp=55

   ;�ر�
   butt_Close=Widget_Button(elsebase,VALUE='�ر�',uname='butt_Close',$
                           SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

   ;����
   butt_Help=Widget_Button(elsebase,VALUE='����',uname='butt_Help',$
                          SCR_YSIZE=button_ysize,SCR_xSIZE=temp)


  Widget_Control, /REALIZE, base
  WIDGET_CONTROL,butt_Close,/INPUT_FOCUS


  XManager, 'base',Event_handle='MJ_Interface_event', base, /NO_BLOCK


end