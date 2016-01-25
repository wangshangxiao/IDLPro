
;
;*************************************************************************************
;�����½��û��Զ�����������д�����ݿ�
;2009.01.13
;*************************************************************************************


function SD_read_county, arr_county

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	;���ȼ�����ݿ�������Ƿ�ɹ�,�粻�ɹ��򲻽��ж�ȡ
	if (yesORno eq 0) then begin
     	text=dialog_message('�����������ݿ�����!',TITLE='��ʾ',/information)
     	close,/all
     	RETURN,0
    endif
    od=DBobj

	;���Ȱ������Ƶ����ݼ���ȡ����
	PROVINCE_CODE=PROVINCE_CODE
	SQL = 'SELECT CODE,NAME FROM COUNTY_CODE WHERE PROVINCE_CODE='+"'"+PROVINCE_CODE+"'"
	ORS = Obj_New('IDLdbRecordset', od, SQL=SQL)

	;��ȡ����ѯ����¼��������
;	TEMP=ORS->MOVECURSOR(/LAST)
	;TOTAL_COUNTY=ORS-> CurrentRecord()+1
	;ԭ���Ĵ��벻֪��ʲôԭ��,����ĳ���,���Ҳ�������
	;�����������ʹ�������ķ�������ȡ��¼������
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	Obj_Destroy,RecordNumOBJ
	;
	COUNTY1= {	COUNTY,  $
		CODE	:	''	,$
		NAME	:	''	,$
		inROI   :   0 $
		}
	arr_county = replicate({COUNTY},RecordNum)

   	COUNT=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
		arr_county[0].CODE = ORS -> GETFIELD(0)
		arr_county[0].NAME = ORS -> GETFIELD(1)
		arr_county[0].inROI = 0
		WHILE (ORS->MOVECURSOR(/NEXT)) DO BEGIN
			COUNT = COUNT+1
			arr_county[COUNT].CODE = ORS -> GETFIELD(0)
			arr_county[COUNT].NAME = ORS -> GETFIELD(1)
			arr_county[COUNT].inROI = 0
		ENDWHILE
	ENDIF
	Obj_Destroy,ORS
	return, 1
end

pro WID_BASE_USERREGION_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top
  WIDGET_CONTROL,wWidget,GET_UVALUE = arr_county

  case wTarget of
    ;����������
	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_READ'):begin
		PRINT,'����������'
		result = SD_READ_COUNTY(arr_county)
		IF(result eq 0) THEN BEGIN
			CLOSE,/ALL
			RETURN
		ENDIF
		WIDGET_CONTROL, wWidget, SET_UVALUE = arr_county
		LISTSOURCE = WIDGET_INFO(wWidget, FIND_BY_UNAME='WID_LIST_SOURCE')
		WIDGET_CONTROL,LISTSOURCE,SET_VALUE=arr_county[*].NAME
		WIDGET_CONTROL,LISTSOURCE,SET_UVALUE=arr_county[*].NAME
	end

	;����������
	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_RIGHTALL'):begin
		PRINT,'�������е���'
		ListSource = Widget_Info(wWidget,FIND_BY_UNAME='WID_LIST_SOURCE')
		widget_control, ListSource, get_uvalue = arr_county_source
		ListTarget = Widget_Info(wWidget,FIND_BY_UNAME = 'WID_LIST_TARGET')
		Widget_control,ListTarget,GET_UVALUE = arr_county_target
		if strtrim(arr_county_source[0],2)  eq '' then begin
			PROMPT = DIALOG_MESSAGE('���Ƚ������ݵĶ�ȡ',TITLE='����')
			RETURN
		endif else begin
			arr_county_target = arr_county_source
			Widget_control,ListTarget,SET_VALUE = arr_county_target
			Widget_control,ListTarget,SET_UVALUE = arr_county_target
			arr_county[*].inROI = 1
			WIDGET_CONTROL, wWidget, SET_UVALUE = arr_county
			RETURN
		endelse
	end

	;ɾ��������
	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_LEFTALL'):begin
		PRINT,'ɾ�����е���'
		ListSource = Widget_Info(wWidget,FIND_BY_UNAME='WID_LIST_SOURCE')
		widget_control, ListSource, get_uvalue = arr_county_source
		ListTarget = Widget_Info(wWidget,FIND_BY_UNAME = 'WID_LIST_TARGET')
		Widget_control, ListTarget, GET_UVALUE = arr_county_target
		if strtrim(arr_county_source[0],2) eq '' then begin
			PROMPT = DIALOG_MESSAGE('���Ƚ������ݵĶ�ȡ',TITLE='����')
			RETURN
		endif else begin
			Widget_control,ListTarget,SET_VALUE = ''
			Widget_control,ListTarget,SET_UVALUE = ''
			arr_county[*].inROI = 0
			WIDGET_CONTROL, wWidget, SET_UVALUE = arr_county
			return
		end
	end

	;������
	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_RIGHT'):begin
		PRINT,'�����µ���'
		ListSource = Widget_Info(wWidget,FIND_BY_UNAME='WID_LIST_SOURCE')
		widget_control, ListSource, get_uvalue = arr_county_source
		ListTarget = Widget_Info(wWidget,FIND_BY_UNAME = 'WID_LIST_TARGET')
		Widget_control, ListTarget, GET_UVALUE = arr_county_target
		if strtrim(arr_county_source[0],2) eq '' then begin
			PROMPT = DIALOG_MESSAGE('���Ƚ������ݵĶ�ȡ',TITLE='����')
			RETURN
		endif
		ARR_INDEX_SELECTED = WIDGET_INFO(ListSource, /LIST_SELECT);ѡ�е��б�
		IF(ARR_INDEX_SELECTED[0] EQ -1) THEN BEGIN
			PROMPT = DIALOG_MESSAGE('��ѡ����Ҫ��ӵ���',TITLE='����')
			RETURN
		ENDIF ELSE BEGIN
			arr_county[ARR_INDEX_SELECTED].inROI = 1
			index = where(arr_county[*].inROI eq 1)
			Widget_control,ListTarget,SET_VALUE = arr_county[index].NAME
			Widget_control,ListTarget,SET_UVALUE = arr_county[index].NAME
			WIDGET_CONTROL, wWidget, SET_UVALUE = arr_county
			RETURN
		ENDELSE

	end

	;ɾ����
	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_LEFT'):begin
		PRINT,'ɾ����'
		ListSource = Widget_Info(wWidget,FIND_BY_UNAME='WID_LIST_SOURCE')
		widget_control, ListSource, get_uvalue = arr_county_source
		ListTarget = Widget_Info(wWidget,FIND_BY_UNAME = 'WID_LIST_TARGET')
		Widget_control, ListTarget, GET_UVALUE = arr_county_target
		if strtrim(arr_county_source[0],2) eq '' then begin
			PROMPT = DIALOG_MESSAGE('���Ƚ������ݵĶ�ȡ',TITLE='����')
			RETURN
		endif
		ARR_INDEX_SELECTED = WIDGET_INFO(ListTarget, /LIST_SELECT);ѡ�е��б�
		IF(ARR_INDEX_SELECTED[0] EQ -1) or strtrim(arr_county_target[0],2) eq '' THEN BEGIN
			PROMPT = DIALOG_MESSAGE('��ѡ����Ҫɾ������',TITLE='����')
			RETURN
		ENDIF ELSE BEGIN
			ROI_index = where(arr_county[*].inROI eq 1, ncount)
			if(ncount eq 0) then begin
				Widget_control,ListTarget,SET_VALUE = ''
				Widget_control,ListTarget,SET_UVALUE = ''
			endif else begin
				sub_arr = make_array(ncount, /INTEGER, value=1)
				sub_arr[ARR_INDEX_SELECTED] = 0
				arr_county[ROI_index].inROI = sub_arr;
				index = where(arr_county[*].inROI eq 1, ncount)
				if(ncount eq 0) then begin
					Widget_control,ListTarget,SET_VALUE = ''
					Widget_control,ListTarget,SET_UVALUE = ''
				endif else begin
					Widget_control,ListTarget,SET_VALUE = arr_county[index].NAME
					Widget_control,ListTarget,SET_UVALUE = arr_county[index].NAME
				endelse
			endelse
			WIDGET_CONTROL, wWidget, SET_UVALUE = arr_county
			RETURN
		ENDELSE
	end

	;������õļ����д�����ݿ�
	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_SAVE'):begin
		ListSource = Widget_Info(wWidget,FIND_BY_UNAME='WID_LIST_SOURCE')
		widget_control, ListSource, get_uvalue = arr_county_source
		ListTarget = Widget_Info(wWidget,FIND_BY_UNAME = 'WID_LIST_TARGET')
		Widget_control, ListTarget, GET_UVALUE = arr_county_target
		if strtrim(arr_county_source[0],2) eq '' then begin
			PROMPT = DIALOG_MESSAGE('���Ƚ������ݵĶ�ȡ',TITLE='����')
			RETURN
		endif

		WID_TEXT_NAME = Widget_Info(event.top, FIND_BY_UNAME='WID_TEXT_NAME')
		WIDGET_CONTROL,WID_TEXT_NAME,GET_VALUE=ROI_NAME
		ROI_NAME=STRTRIM(ROI_NAME[0],2)
		ROI_CODE='0'
		IF (ROI_NAME EQ '') THEN BEGIN
			PROMPT = DIALOG_MESSAGE('����������������',TITLE='����')
			RETURN
		ENDIF

		;�����ݿ��в�ѯ�Ƿ��Ѿ�����һ��ͬ����¼
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
		OD=DBobj

		SQL = 'SELECT NAME,CODE FROM ROI_CODE WHERE NAME='+"'"+ROI_NAME+"'"
		ORS = Obj_New('IDLdbRecordset', od, SQL=SQL)
		IF(ORS->MOVECURSOR(/FIRST) EQ 1) THEN BEGIN
			TEMP = DIALOG_MESSAGE('�����м��������,��������������',TITLE='����')
		ENDIF ELSE BEGIN
			index = where(arr_county[*].inROI eq 1, ncount)
			;����Ҫ��һ����,���Ҳ���б���,����,���˳�
			IF (ncount EQ 0) THEN BEGIN
				PROMPT = DIALOG_MESSAGE('�����������������',TITLE='����')
				RETURN
			ENDIF

			SQL = 'SELECT CODE FROM ROI_CODE ORDER BY CODE'		;ԭ����
			;��CODE���򣬵�CODEΪ�ı���������clng(CODE)ת�����ֽ�������
			SQL = 'SELECT CODE FROM ROI_CODE ORDER BY clng(CODE)'	;�������޸ģ�20070906
			ORS = Obj_New('IDLdbRecordset', od, SQL=SQL)
			;Ϊ��������һ������
			IF(ORS->MOVECURSOR(/FIRST) NE 1) THEN BEGIN
				ROI_CODE='1'
			ENDIF ELSE BEGIN
				;��ü�¼����
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				ARR_CODE=STRARR(RecordNum)
				;������������������
				COUNT=0
				ARR_CODE[COUNT]=ORS -> GETFIELD(0)
				WHILE (ORS->MOVECURSOR(/NEXT)) DO BEGIN
					COUNT = COUNT+1
					ARR_CODE[COUNT] = ORS -> GETFIELD(0)
				ENDWHILE
				;�ҳ����е���С����
				FLAG_CODE_FOUND=0	;һ���Ƿ��ҵ���С���д����
				FOR I=0,RecordNum-1 DO BEGIN
					PRINT,'I',I
					IF (STRTRIM(I+1,2) NE STRTRIM(ARR_CODE[I],2)) THEN BEGIN
						ROI_CODE=STRTRIM(I+1,2)
						FLAG_CODE_FOUND=1
						BREAK
					ENDIF
				ENDFOR
				;��������ǿ������еĴ�����������
				PRINT,'HERE1',ROI_CODE
				IF (FLAG_CODE_FOUND EQ 0) THEN BEGIN
					ROI_CODE=STRTRIM(RecordNum+1,2)
				ENDIF
				PRINT,'HERE2',ROI_CODE
			ENDELSE

			;�����¼,����������Ҫ��������
	   		;һ����COUNTY_TO_ROI,��һ����ROI_CODE
	   		;(1)�����Զ�����������
	   		;SQL='INSERT INTO "CROP"."ROI_CODE"'
	   		SQL='INSERT INTO ROI_CODE'
			SQL=SQL+'("CODE" ,"NAME") '
			SQL=SQL+'VALUES ('
			SQL=SQL+"'"+STRTRIM(STRING(ROI_CODE),2)	+"'"+','
			SQL=SQL+"'"+STRTRIM(STRING(ROI_NAME),2)	+"'"+')'
			PRINT,SQL
			OD->EXECUTESQL,SQL
			;(2)����COUNTY_TO_ROI
			FOR I=0,ncount-1 DO BEGIN
				;SQL='INSERT INTO "CROP_PROVINCE"."COUNTY_TO_ROI"'
				SQL='INSERT INTO COUNTY_TO_ROI'
				SQL=SQL+'("COUNTY_CODE" ,"ROI_CODE") '
				SQL=SQL+'VALUES ('
				SQL=SQL+"'"+STRTRIM(arr_county[index[I]].CODE, 2)	+"'"+','
				SQL=SQL+"'"+STRTRIM(STRING(ROI_CODE),2)	+"'"+')'
				;PRINT,SQL
				OD->EXECUTESQL,SQL
			ENDFOR
			PROMPT = DIALOG_MESSAGE('�����Ѿ�����',TITLE='���',/Information )
		ENDELSE
		OBJ_DESTROY,ORS
	END

	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_CANCEL'):begin
		close,/all
    	widget_control, event.top, /destroy
	end

	Widget_Info(wWidget,FIND_BY_UNAME='BUTTON_HELP'):begin

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '������Զ���', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
	end

    else:
  endcase
end

pro BASE_ROI_CREATE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

IF ( XREGISTERED('WID_BASE_USERREGION') NE 0 ) THEN RETURN

;  Resolve_Routine, 'USER_CUSTOM_CREATE_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

   WID_BASE_USERREGION = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='WID_BASE_USERREGION' ,XOFFSET=250 ,YOFFSET=200  $
      ,SCR_XSIZE=487 ,SCR_YSIZE=418 ,TITLE='�Զ�����������'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1)


  WID_BASE_REGION = Widget_Base(WID_BASE_USERREGION,  $
      UNAME='WID_BASE_REGION' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=6  $
      ,SCR_XSIZE=469 ,SCR_YSIZE=332 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  WID_BASE_SOURCE = Widget_Base(WID_BASE_REGION,  $
      UNAME='WID_BASE_SOURCE' ,FRAME=1 ,XOFFSET=8 ,YOFFSET=25  $
      ,SCR_XSIZE=195 ,SCR_YSIZE=294 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  WID_LIST_SOURCE = Widget_List(WID_BASE_SOURCE,  $
      UNAME='WID_LIST_SOURCE' ,FRAME=1 ,XOFFSET=4 ,YOFFSET=6  $
      ,SCR_XSIZE=184 ,SCR_YSIZE=281 ,XSIZE=11 ,YSIZE=2,/MULTIPLE, value='', uvalue='')


  WID_BASE_TARGET = Widget_Base(WID_BASE_REGION,  $
      UNAME='WID_BASE_TARGET' ,FRAME=1 ,XOFFSET=264 ,YOFFSET=70  $
      ,SCR_XSIZE=195 ,SCR_YSIZE=248 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  WID_LIST_TARGET = Widget_List(WID_BASE_TARGET,  $
      UNAME='WID_LIST_TARGET' ,FRAME=1 ,XOFFSET=5 ,YOFFSET=6  $
      ,SCR_XSIZE=184 ,SCR_YSIZE=235 ,XSIZE=11 ,YSIZE=2,/MULTIPLE, value='', uvalue='')


  WID_BUTTON_RIGHT = Widget_Button(WID_BASE_REGION,  $
      UNAME='BUTTON_RIGHT' ,XOFFSET=210 ,YOFFSET=73 ,SCR_XSIZE=48  $
      ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='->')


  WID_BUTTON_LEFT = Widget_Button(WID_BASE_REGION,  $
      UNAME='BUTTON_LEFT' ,XOFFSET=210 ,YOFFSET=124 ,SCR_XSIZE=48  $
      ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='<-')


  WID_BUTTON_RIGHTALL = Widget_Button(WID_BASE_REGION,  $
      UNAME='BUTTON_RIGHTALL' ,XOFFSET=210 ,YOFFSET=176  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='>>')


  WID_BUTTON_LEFTALL = Widget_Button(WID_BASE_REGION,  $
      UNAME='BUTTON_LEFTALL' ,XOFFSET=210 ,YOFFSET=227  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='<<')


  WID_LABEL_SOURCE = Widget_Label(WID_BASE_REGION,  $
      UNAME='WID_LABEL_SOURCE' ,XOFFSET=7 ,YOFFSET=10 ,SCR_XSIZE=88  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='������')


  WID_LABEL_TARGET = Widget_Label(WID_BASE_REGION,  $
      UNAME='WID_LABEL_TARGET' ,XOFFSET=264 ,YOFFSET=52 ,SCR_XSIZE=88  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='����������أ�')


  WID_BASE_4 = Widget_Base(WID_BASE_USERREGION, UNAME='WID_BASE_4'  $
      ,FRAME=1 ,XOFFSET=6 ,YOFFSET=345 ,SCR_XSIZE=469 ,SCR_YSIZE=37  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BUTTON_READ = Widget_Button(WID_BASE_4, UNAME='BUTTON_READ'  $
      ,XOFFSET=16 ,YOFFSET=6 ,SCR_XSIZE=65 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='��ȡ����')


  WID_BUTTON_SAVE = Widget_Button(WID_BASE_4, UNAME='BUTTON_SAVE'  $
      ,XOFFSET=140 ,YOFFSET=6 ,SCR_XSIZE=65 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='��������')


  WID_BUTTON_HELP = Widget_Button(WID_BASE_4,  $
      UNAME='BUTTON_HELP' ,XOFFSET=265 ,YOFFSET=6 ,SCR_XSIZE=65  $
      ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='����')

  WID_BUTTON_CANCEL = Widget_Button(WID_BASE_4,  $
      UNAME='BUTTON_CANCEL' ,XOFFSET=386 ,YOFFSET=6 ,SCR_XSIZE=65  $
      ,SCR_YSIZE=23 ,/ALIGN_CENTER ,VALUE='�ر�')


  WID_TEXT_NAME = Widget_Text(WID_BASE_REGION,  $
      UNAME='WID_TEXT_NAME' ,XOFFSET=264 ,YOFFSET=25 ,SCR_XSIZE=195  $
      ,SCR_YSIZE=23 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  WID_LABEL_NAME = Widget_Label(WID_BASE_REGION,  $
      UNAME='WID_LABEL_NAME' ,XOFFSET=264 ,YOFFSET=10 ,SCR_XSIZE=80  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='���������  ��')


	COUNTY1= {	COUNTY,  $
		CODE	:	''	,$
		NAME	:	''	,$
		inROI   :   0 $
		}
	arr_county = replicate({COUNTY}, 1)

	Widget_Control,WID_BASE_USERREGION,set_uvalue = arr_county

  Widget_Control, /REALIZE, WID_BASE_USERREGION
  WIDGET_CONTROL,WID_BUTTON_CANCEL,/INPUT_FOCUS

  XManager, 'WID_BASE_USERREGION', WID_BASE_USERREGION, /NO_BLOCK

end

pro SD_ROI_Creat, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_ROI_CREATE, GROUP_LEADER=BASE_TOP, _EXTRA=_VWBExtra_
end



