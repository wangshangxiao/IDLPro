
; IDL Event Callback Procedures
; REGION_COUNTY_ADD_eventcb
;
; Generated on:	07/05/2006 09:07.18
;
;
; Empty stub procedure used for autoloading.
;
pro REGION_COUNTY_ADD_eventcb
end

function read_counties_record,pstate

	;首先检测数据库的链接是否成功,如不成功则不进行读取
	if ((*pstate).dbco_id eq 0) then begin
     	text=dialog_message('请先设置数据库链接!',/information)
     	close,/all
     	RETURN,0
    endif
;    od = Obj_New('IDLdbDatabase')
    od=(*((*pstate).f)).DBCO

	;定义一个数组,用来存储从数据库中读取出来的数据
;	arr_county = ''
;	;定义一个变量用来存储有效记录的个数
;	COUNT=0

	;首先把县名称的数据集读取出来
	SQL = 'SELECT NAME FROM COUNTY_CODE'
	ORS = Obj_New('IDLdbRecordset', od, SQL=SQL)


	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
;		arr_county[COUNT] = ORS -> GETFIELD(0)
;		COUNT = COUNT+1
		arr_county = ORS -> GETFIELD(0)
		COUNT = 1
		WHILE (ORS->MOVECURSOR(/NEXT)) DO BEGIN
			temp = ORS -> GETFIELD(0)
			arr_county = [arr_county,temp]
			COUNT = COUNT+1L
		ENDWHILE
	ENDIF
	Obj_Destroy,ORS

	return,arr_county

end
;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	07/05/2006 09:07.18
;
pro WID_BASE_COUNTIES_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_REGIONCOUNTY_READ'):begin
		Widget_Control,event.top,get_uvalue = pstate
		arr_county = read_counties_record(pstate)

		ListCounty = Widget_Info(event.top,FIND_BY_UNAME='WID_LIST_COUNTY')
		Widget_Control,ListCounty,SET_VALUE=arr_county

		print,arr_county
	;	arr_county = 0
	end
	Widget_Info(wWidget,FIND_BY_UNAME='CMD_REGIONCOUNTY_ADD'):begin
	end
	Widget_Info(wWidget,FIND_BY_UNAME='CMD_CANCEL'):begin
		close,/all
    	widget_control, event.top, /destroy
	end

    else:
  endcase

end

pro WID_BASE_COUNTIES,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  Resolve_Routine, 'REGION_COUNTY_ADD_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WID_BASE_COUNTIES = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_BASE_COUNTIES'  $
      ,XOFFSET=300 ,YOFFSET=200 ,SCR_XSIZE=311 ,SCR_YSIZE=371  $
      ,TITLE='县的添加' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_COUNTY = Widget_Base(WID_BASE_COUNTIES, UNAME='WID_BASE_COUNTY'  $
      ,FRAME=1 ,XOFFSET=10 ,YOFFSET=15 ,SCR_XSIZE=283 ,SCR_YSIZE=313  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LIST_COUNTY = Widget_List(WID_BASE_COUNTY,  $
      UNAME='WID_LIST_COUNTY' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=26  $
      ,SCR_XSIZE=193 ,SCR_YSIZE=280 ,/MULTIPLE ,XSIZE=11 ,YSIZE=2)


  WID_LABEL_COUNTY = Widget_Label(WID_BASE_COUNTY,  $
      UNAME='WID_LABEL_COUNTY' ,XOFFSET=7 ,YOFFSET=8 ,SCR_XSIZE=88  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='县名：')

  WID_BUTTON_OK = Widget_Button(WID_BASE_COUNTY, UNAME='WID_BUTTON_OK'  $
      ,XOFFSET=210 ,YOFFSET=38 ,SCR_XSIZE=62 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='读取数据')


  WID_BUTTON_ADD = Widget_Button(WID_BASE_COUNTY,  $
      UNAME='WID_BUTTON_ADD' ,XOFFSET=210 ,YOFFSET=85 ,SCR_XSIZE=62  $
      ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='添加')


  WID_BUTTON_CANCEL = Widget_Button(WID_BASE_COUNTY,  $
      UNAME='WID_BUTTON_CANCEL' ,XOFFSET=210 ,YOFFSET=132  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='取消')

	state = { $
        WID_BASE_COUNTIES			:	WID_BASE_COUNTIES  $
        }

	pstate = ptr_new(state,/no_copy)
	Widget_Control,WID_BASE_COUNTIES,set_uvalue = pstate

  Widget_Control, /REALIZE, WID_BASE_COUNTIES

  XManager, 'WID_BASE_COUNTIES', WID_BASE_COUNTIES, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro REGION_COUNTY_ADD,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WID_BASE_COUNTIES,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
