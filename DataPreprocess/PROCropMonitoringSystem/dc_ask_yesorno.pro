
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	03/18/2006 21:52.24
;
PRO DC_Ask_YesOrNo_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  CASE wTarget OF
	Widget_Info(wWidget, FIND_BY_UNAME='No_BU'): BEGIN
		WIDGET_CONTROL,Event.top,GET_UVALUE = ReplyID
		IsAlway = Widget_Info(Widget_Info(wWidget, FIND_BY_UNAME='Alway_BU'),/BUTTON_SET)
		IF IsAlway EQ 1 THEN BEGIN *ReplyID = 4    ;����"��"
		ENDIF ELSE *ReplyID = 3					   ;"��"
	    WIDGET_CONTROL,Event.top,SET_UVALUE = ReplyID
	    WIDGET_CONTROL,Event.top,/DESTROY

;	    print,*ReplyID
;	    help,IsAlway
	END

	Widget_Info(wWidget, FIND_BY_UNAME='Yes_BU'): BEGIN
		WIDGET_CONTROL,Event.top,GET_UVALUE = ReplyID
		IsAlway = Widget_Info(Widget_Info(wWidget, FIND_BY_UNAME='Alway_BU'),/BUTTON_SET)
		IF IsAlway EQ 1 THEN BEGIN *ReplyID = 2    ;����"��"
		ENDIF ELSE *ReplyID = 1					   ;"��"
	    WIDGET_CONTROL,Event.top,SET_UVALUE = ReplyID
	    WIDGET_CONTROL,Event.top,/DESTROY

;	    print,*ReplyID
;	    help,IsAlway
	END

	Widget_Info(wWidget, FIND_BY_UNAME='Cancle_BU'): BEGIN
		WIDGET_CONTROL,Event.top,GET_UVALUE = ReplyID
		*ReplyID = 5					   ;"ȡ��"
	    WIDGET_CONTROL,Event.top,SET_UVALUE = ReplyID
	    WIDGET_CONTROL,Event.top,/DESTROY

;	    print,*ReplyID
	END

    ELSE:
  ENDCASE

END
;_______________________________________________________________
;---------------���������--------------------------------------
pro DC_Ask_YesOrNo, Prompt_text $          ;��ʾ�ı�.
			      , GROUP_LEADER $
			      , ReplyID =ReplyID  $    ;���ص�ѯ��ֵ.ָ������.����ֵΪ1-5֮���ֵ.
			      , NoAlways = NoAlways    ;�����Ƿ���ʾ"��������ͬ�Ļش�"���ϰ�ť����,��ѡ

;������ʽ: DC_Ask_YesOrNo, Prompt_text,GROUP_LEADER,ReplyID =ReplyId,[/NoAlways]
;ReplyId�ķ��ؽ��:
;;*ReplyID = 1		��
;;*ReplyID = 2		����
;;*ReplyID = 3		��
;;*ReplyID = 4		���Ƿ�
;;*ReplyID = 5		ȡ��


  TLB_BASE = Widget_Base( GROUP_LEADER=GROUP_LEADER, UNAME='TLB_BASE'  $
      ,XOFFSET=420 ,YOFFSET=370 ,SCR_XSIZE=272 ,SCR_YSIZE=160 ,TITLE='ѯ��'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=9,/MODAL,/TAB_MODE)

  Info_LABEL = Widget_Text(TLB_BASE, UNAME='Info_LABEL'  $
      ,XOFFSET=5 ,YOFFSET=3 ,SCR_XSIZE=254 ,SCR_YSIZE=57,YSIZE=10   $
      ,VALUE='��ʾ��Ϣ',/WRAP)
;-----------------------------------------
  Yes_BU = Widget_Button(TLB_BASE, UNAME='Yes_BU' ,XOFFSET=10  $
      ,YOFFSET=72 ,SCR_XSIZE=55 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='��')

  No_BU = Widget_Button(TLB_BASE, UNAME='No_BU' ,XOFFSET=105  $
      ,YOFFSET=72 ,SCR_XSIZE=55 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='��')

  Cancle_BU = Widget_Button(TLB_BASE, UNAME='Cancle_BU' ,XOFFSET=200  $
      ,YOFFSET=72 ,SCR_XSIZE=55 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='ȡ��')
  ;--------------------------------
  Always_BASE = Widget_Base(TLB_BASE, UNAME='Always_BASE' ,XOFFSET=15  $
      ,YOFFSET=104 ,SCR_XSIZE=165 ,SCR_YSIZE=22 ,TITLE='IDL' ,COLUMN=1  $
      ,/NONEXCLUSIVE)

  Alway_BU = Widget_Button(Always_BASE, UNAME='Alway_BU' ,/ALIGN_LEFT  $
      ,VALUE='������ͬ�Ļش�')
 ;------------------------------------------------
  Widget_Control,Info_LABEL,SET_VALUE=Prompt_text

  IF KEYWORD_SET(NoAlways) THEN Widget_Control,Always_BASE,MAP=0

  Widget_Control, /REALIZE, TLB_BASE
  Widget_Control,Yes_BU,/INPUT_FOCUS

  Ini_ReplyID = 1    ;��ʼ��ѯ��ֵΪ'��'��ûѡ"��������ͬ�Ļش�"

  ReplyID = ptr_new(Ini_ReplyID,/no_copy)

  Widget_Control, TLB_BASE, SET_UVALUE = ReplyID

  XManager, 'DC_Ask_YesOrNo',TLB_BASE , /NO_BLOCK

end
