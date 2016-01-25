

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
      ,XOFFSET=490 ,YOFFSET=200 ,SCR_XSIZE=360  ,SCR_YSIZE=140,TITLE='各单产类型描述' $
      ,SPACE=3,XPAD=1 ,YPAD=1,TLB_FRAME_ATTR=1,/TLB_KILL_REQUEST_EVENTS)

    Descroption = Widget_Text(Tlb_BASE, UNAME='Descroption' ,FRAME=1  $
       ,SCR_XSIZE=350,SCR_YSIZE=103,YSIZE=6,XOFFSET=3,YOFFSET=3,/EDIT,/WRAP)

  ModelDescription=['单产类型描述:' $
  					,' Yield1：县气象数据计算得到的单产.'  $
  					,' Yield2：站点气象数据外推得到的单产.' $
  					,' Yield3：县遥感指数计算得到的单产.' $
  					,' Yield4：站点遥感指数外推得到的单产.'  $
  					,' Yield5：县生物量比值法得到的单产.' $
  					,' Yield6：由外推收获指数结合县生物量计算得到的单产.' $
  					]
  Widget_Control, Descroption,SET_VALUE=ModelDescription
  Widget_Control,Tlb_BASE,/REALIZE

  IF ARG_PRESENT(tlb) THEN tlb = Tlb_BASE   ;传递顶BASE


;;  PA=PTR_NEW(DisplayDes,/NO_COPY)
;;
;;  Widget_Control, Tlb_BASE,SET_UVALUE=PA

  XManager, 'DC_YieldDescription', Tlb_BASE, /NO_BLOCK

END