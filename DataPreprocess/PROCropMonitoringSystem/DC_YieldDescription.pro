

PRO DC_YieldDescription_EVENT,EVENT
  IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
;  	Widget_Control, EVENT.TOP,GET_UVALUE=Para
;  	WIDGET_CONTROL,*Para,SET_BUTTON=0
;  	PTR_FREE,Para
  	WIDGET_CONTROL,EVENT.TOP,/DESTROY
  ENDIF

END

;---------------------------------------------------
PRO DC_YieldDescription,GROUP_LEADER=groupleader,ITTLB=tlb

     IF ( XREGISTERED('DC_YieldDescription') NE 0 ) THEN RETURN

    Tlb_BASE = Widget_Base(GROUP_LEADER=groupleader,UNAME='Tlb_BASE'   $
      ,XOFFSET=490 ,YOFFSET=200 ,SCR_XSIZE=360  ,SCR_YSIZE=140,TITLE='��������������' $
      ,SPACE=3,XPAD=1 ,YPAD=1,TLB_FRAME_ATTR=1,/TLB_KILL_REQUEST_EVENTS)

    Descroption = Widget_Text(Tlb_BASE, UNAME='Descroption' ,FRAME=1  $
       ,SCR_XSIZE=350,SCR_YSIZE=103,YSIZE=6,XOFFSET=3,YOFFSET=3,/EDIT,/WRAP)

  ModelDescription=['������������:' $
  					,' Yield1�����������ݼ���õ��ĵ���.'  $
  					,' Yield2��վ�������������Ƶõ��ĵ���.' $
  					,' Yield3����ң��ָ������õ��ĵ���.' $
  					,' Yield4��վ��ң��ָ�����Ƶõ��ĵ���.'  $
  					,' Yield5������������ֵ���õ��ĵ���.' $
  					,' Yield6���������ջ�ָ�����������������õ��ĵ���.' $
  					]
  Widget_Control, Descroption,SET_VALUE=ModelDescription
  Widget_Control,Tlb_BASE,/REALIZE

  IF ARG_PRESENT(tlb) THEN tlb = Tlb_BASE   ;���ݶ�BASE


;;  PA=PTR_NEW(DisplayDes,/NO_COPY)
;;
;;  Widget_Control, Tlb_BASE,SET_UVALUE=PA

  XManager, 'DC_YieldDescription', Tlb_BASE, /NO_BLOCK

END