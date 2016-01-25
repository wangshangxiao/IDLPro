;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.
; Empty stub procedure used for autoloading.
;
;**************************************************************************
;�ɼ̻�
;�û��Զ��������Ķ���\�޸�\����\ɾ��
;2006.07.26
;2006.07.28,��ģ��Ľ�������˵���,����ʼ�˴���ı�д
;���ȿ�ʼ�������ݶ�ȡ����
;**************************************************************************

pro CUSTOM_REGION_MANAGE_eventcb
end
;
; Generated on:	07/05/2006 08:39.47
FUNCTION READ_ROI,PSTATE

	COMMON COMMON_BLOCK,YESORNO,DBOBJ,FILE_PATH,YEAR,DSN,USER_NAME,PWD,PROVINCE_CODE
	;���ȼ�����ݿ�������Ƿ�ɹ�,�粻�ɹ��򲻽��ж�ȡ
	IF (YESORNO EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('�����������ݿ�����!',TITLE='��ʾ',/INFORMATION)
     	CLOSE,/ALL
     	RETURN,0
    ENDIF
    OD=DBOBJ

	;���в�ѯ
	SQL = 'SELECT CODE,NAME FROM ROI_CODE ORDER BY clng(CODE)'	;ORDER BY clng(CODE)�����������ӣ�20070906
	ORS = Obj_New('IDLdbRecordset', od, SQL=SQL)
	;��ȡ��¼������
	SQL_1='select count(*) from ('+Sql+')'
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=SQL_1)
	RecordNum = RecordNumOBJ->GETFIELD(0)
	TOTAL_COUNTY=RecordNum
	Obj_Destroy,RecordNumOBJ
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
		ROI_COUNT = 0
		;�޸�����ǰ�������ݶ�ȡ�ĳ���,��WHILE��Ϊ��REPEAT
		REPEAT BEGIN
   			(*PSTATE).ARR_ROI[ROI_COUNT].CODE = ORS -> GETFIELD(0)
		   	(*PSTATE).ARR_ROI[ROI_COUNT].NAME = ORS -> GETFIELD(1)
		   	PRINT,ORS -> GETFIELD(1)
		   	SQL = 'SELECT A.COUNTY_CODE,B.NAME FROM '
			SQL = SQL+'COUNTY_TO_ROI A,COUNTY_CODE B '
			SQL = SQL+'WHERE ( A.COUNTY_CODE=B.CODE AND '
			SQL = SQL+'("A.ROI_CODE" = '+"'"+STRTRIM((*PSTATE).ARR_ROI[ROI_COUNT].CODE,2)+"' "+')'
			SQL = SQL+')'
			ORS_1 = Obj_New('IDLdbRecordset', od, SQL=SQL)
			COUNTY_COUNT = 0

			IF(ORS_1->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
				REPEAT BEGIN
	 				(*PSTATE).ARR_ROI[ROI_COUNT].ARR_COUNTY_CODE[COUNTY_COUNT] = ORS_1 -> GETFIELD(0)
					(*PSTATE).ARR_ROI[ROI_COUNT].ARR_COUNTY_NAME[COUNTY_COUNT] = ORS_1 -> GETFIELD(1)
					PRINT,'   ',ROI_COUNT,COUNTY_COUNT,ORS_1 -> GETFIELD(1)
					COUNTY_COUNT=COUNTY_COUNT+1
				ENDREP UNTIL (ORS_1->MOVECURSOR(/NEXT) NE 1)
			ENDIF
			Obj_Destroy,ORS_1

			(*PSTATE).ARR_ROI[ROI_COUNT].NUM_COUNTY=COUNTY_COUNT
			ROI_COUNT = ROI_COUNT+1
		ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		(*PSTATE).NUM_ROI_ALL=TOTAL_COUNTY
		Obj_Destroy,ORS
		RETURN,1
	ENDIF ELSE BEGIN
     	close,/all
     	Obj_Destroy,ORS
     	RETURN,0
	ENDELSE
END

;
PRO WID_BASE_CUSTOM_EVENT, EVENT

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  COMMON COMMON_BLOCK,YESORNO,DBOBJ,FILE_PATH,YEAR,DSN,USER_NAME,PWD,PROVINCE_CODE
  Widget_Control,event.top,get_uvalue = pstate
  case wTarget of


	Widget_Info(wWidget,FIND_BY_UNAME='WID_LIST_SOURCE'):begin
		;���һ���Զ�������ʱ,��ʾ�����ڵ�������
		LISTSOURCE = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_SOURCE')
		INDEX_SEL = WIDGET_INFO(ListSource, /LIST_SELECT)
		LISTTARGET = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_TARGET')
		WIDGET_CONTROL,LISTTARGET,SET_VALUE=$
			(*PSTATE).ARR_ROI[INDEX_SEL].ARR_COUNTY_NAME[0:(*PSTATE).ARR_ROI[INDEX_SEL].NUM_COUNTY-1]
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_REGION_DELETE'):begin
		LISTSOURCE = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_SOURCE')
		INDEX_SEL = WIDGET_INFO(ListSource, /LIST_SELECT)
		INDEX_SEL =	INDEX_SEL[0]
		IF INDEX_SEL EQ -1 THEN BEGIN
			TEMP=DIALOG_MESSAGE('��ѡ��Ҫɾ�����Զ�������!',TITLE='��ʾ')
			RETURN
		ENDIF

		;���û�ȷ�ϼ������ɾ��
		TEMP= DIALOG_MESSAGE('ȷ��Ҫɾ���ü����ô?',TITLE='��ʾ',/QUESTION)
		IF TEMP EQ 'No' THEN RETURN
		PRINT,'ȷ��ɾ��'
		ROI_CODE=(*PSTATE).ARR_ROI[INDEX_SEL].CODE
		PRINT,ROI_CODE

		;ѡɾ��COUNTY_TO_ROI�е���Ӧ����
		OD=DBOBJ
		SQL='DELETE FROM COUNTY_TO_ROI WHERE ( "ROI_CODE" = '+"'"+ROI_CODE+"' "+')'
  		OD->EXECUTESQL,SQL
  		;��ɾ��ROI_CODE�е���Ӧ����
		SQL='DELETE FROM ROI_CODE WHERE ( "CODE" = '+"'"+ROI_CODE+"' "+')'
  		OD->EXECUTESQL,SQL

		;���¶�ȡ����
		TEMP = READ_ROI(pstate)
		LISTSOURCE = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_SOURCE')
		LISTTARGET = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_TARGET')
		WIDGET_CONTROL,LISTSOURCE,SET_VALUE=''
		WIDGET_CONTROL,LISTTARGET,SET_VALUE=''
		IF TEMP EQ 0 THEN BEGIN
			TEXT=DIALOG_MESSAGE('���ݿ���û���Զ�������!',TITLE='��ʾ',/INFORMATION)
			WIDGET_CONTROL,LISTSOURCE,SET_VALUE=''
			WIDGET_CONTROL,LISTTARGET,SET_VALUE=''
			(*PSTATE).NUM_ROI_ALL=0
			RETURN
		ENDIF
		WIDGET_CONTROL,LISTSOURCE,SET_VALUE=(*PSTATE).ARR_ROI[0:(*PSTATE).NUM_ROI_ALL-1].NAME
		RETURN
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_COUNTY_DELETE'):begin
		TEMP=DIALOG_MESSAGE('������!',TITLE='��ʾ')
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_REGION_NEW'):begin
		SD_ROI_Creat, GROUP_LEADER=CUSTOM_REGION_MANAGE,_EXTRA=_VWBExtra_
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_COUNTY_ADD'):begin
		;Widget_Control,event.top,get_uvalue=pstate
		;REGION_COUNTY_ADD,(*pstate).F, GROUP_LEADER=CUSTOM_REGION_MANAGE
		TEMP=DIALOG_MESSAGE('������!',TITLE='��ʾ')
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_CANCEL'):begin
		CLOSE,/ALL
    	WIDGET_CONTROL, EVENT.TOP, /DESTROY
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_HELP'):begin

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '������Զ���', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse

;		ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,'������Զ���'
	end

	Widget_Info(wWidget,FIND_BY_UNAME='CMD_READ'):begin
		TEMP = READ_ROI(pstate)
		LISTSOURCE = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_SOURCE')
		LISTTARGET = WIDGET_INFO(EVENT.TOP,FIND_BY_UNAME='WID_LIST_TARGET')
		WIDGET_CONTROL,LISTSOURCE,SET_VALUE=''
		WIDGET_CONTROL,LISTTARGET,SET_VALUE=''
		IF TEMP EQ 0 THEN BEGIN
			TEXT=DIALOG_MESSAGE('���ݿ���û���Զ�������!',TITLE='��ʾ',/INFORMATION)
			WIDGET_CONTROL,LISTSOURCE,SET_VALUE=''
			WIDGET_CONTROL,LISTTARGET,SET_VALUE=''
			(*PSTATE).NUM_ROI_ALL=0
			RETURN
		ENDIF
		WIDGET_CONTROL,LISTSOURCE,SET_VALUE=(*PSTATE).ARR_ROI[0:(*PSTATE).NUM_ROI_ALL-1].NAME
	end

    else:
  endcase

end

pro WID_BASE_CUSTOM,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

IF ( XREGISTERED('WID_BASE_CUSTOM') NE 0 ) THEN RETURN

  Resolve_Routine, 'CUSTOM_REGION_MANAGE_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WID_BASE_CUSTOM = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_BASE_CUSTOM'  $
      ,XOFFSET=250 ,YOFFSET=200 ,SCR_XSIZE=460 ,SCR_YSIZE=394  $
      ,TITLE='�Զ�����������' ,SPACE=3 ,XPAD=3 ,YPAD=3, TLB_FRAME_ATTR=1)

  WID_BASE_1 = Widget_Base(WID_BASE_CUSTOM, UNAME='WID_BASE_1' ,FRAME=1  $
      ,XOFFSET=5 ,YOFFSET=6 ,SCR_XSIZE=213 ,SCR_YSIZE=303  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_LABEL_0 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_0'  $
      ,XOFFSET=6 ,YOFFSET=8 ,SCR_XSIZE=104 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='�Զ��������б�:')


  WID_BUTTON_REGIONDELETE = Widget_Button(WID_BASE_1, UNAME='CMD_REGION_DELETE'  $
      ,XOFFSET=10 ,YOFFSET=268 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='ɾ��')


  WID_BUTTON_ADD = Widget_Button(WID_BASE_1, UNAME='CMD_REGION_NEW'  $
      ,XOFFSET=143 ,YOFFSET=268 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_LIST_SOURCE = Widget_List(WID_BASE_1, UNAME='WID_LIST_SOURCE' ,FRAME=1  $
      ,XOFFSET=5 ,YOFFSET=30 ,SCR_XSIZE=201 ,SCR_YSIZE=230 ,XSIZE=11  $
      ,YSIZE=2)


  WID_BASE_3 = Widget_Base(WID_BASE_CUSTOM, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=235 ,YOFFSET=6 ,SCR_XSIZE=213 ,SCR_YSIZE=303  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_LABEL_1 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_1'  $
      ,XOFFSET=6 ,YOFFSET=8 ,SCR_XSIZE=104 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='������������б�:')

  ;***********************************************************************************
  ;�޸���2006.12.4,���ڿ���ʱ���,��ȥ���˶��Զ������������޸ĵĹ���
  ;ɾ�����������༭��ť
  ;�ɼ̻�
;  WID_BUTTON_COUNTYDELETE = Widget_Button(WID_BASE_3, UNAME='CMD_COUNTY_DELETE'  $
;      ,XOFFSET=10 ,YOFFSET=268 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
;      ,/ALIGN_CENTER ,VALUE='ɾ��')
;
;
;  WID_BUTTON_COUNTYADD = Widget_Button(WID_BASE_3, UNAME='CMD_COUNTY_ADD'  $
;      ,XOFFSET=143 ,YOFFSET=268 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
;      ,/ALIGN_CENTER ,VALUE='����')
  ;***********************************************************************************


  WID_LIST_TARGET = Widget_List(WID_BASE_3, UNAME='WID_LIST_TARGET' ,FRAME=1  $
      ,XOFFSET=5 ,YOFFSET=30 ,SCR_XSIZE=201 ,SCR_YSIZE=230+35 ,XSIZE=11  $
      ,YSIZE=2,/MULTIPLE)


  WID_BASE_2 = Widget_Base(WID_BASE_CUSTOM, UNAME='WID_BASE_2' ,FRAME=1  $
      ,XOFFSET=5 ,YOFFSET=318 ,SCR_XSIZE=443 ,SCR_YSIZE=39  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BUTTON_CANCEL = Widget_Button(WID_BASE_2, UNAME='CMD_CANCEL'  $
      ,XOFFSET=350 ,YOFFSET=7 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�ر�')


  WID_BUTTON_HELP = Widget_Button(WID_BASE_2, UNAME='CMD_HELP'  $
      ,XOFFSET= 189,YOFFSET=8 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_BUTTON_OK = Widget_Button(WID_BASE_2, UNAME='CMD_READ'  $
      ,XOFFSET=31 ,YOFFSET=8 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='��ȡ����')

   	ONE_ROI = { $
  		NAME	:	''	,$	;ROI NAME
  		CODE	:	''	,$	;ROI CODE
  		NUM_COUNTY	:	0	,$	;THE NUMBER OF COUNTIES IN A ROI
  		ARR_COUNTY_CODE	:	STRARR(100)	,$	;COUNTY CODE OF ALL COUNTIES IN THE ROI
  		ARR_COUNTY_NAME	:	STRARR(100)	$	;COUNTY NAME OF ALL COUNTIES IN THE ROI
        }


	STATE = { $

        ARR_ROI			:    REPLICATE(ONE_ROI, 50),$
        NUM_ROI_ALL		:	0,$
        NUM_ROI_SEL		:	0,$
        WID_BASE_CUSTOM	:	WID_BASE_CUSTOM $
        }

	pstate = ptr_new(state,/no_copy)
	Widget_Control,WID_BASE_CUSTOM,set_uvalue = pstate



  Widget_Control, /REALIZE, WID_BASE_CUSTOM
  WIDGET_CONTROL,WID_BUTTON_CANCEL,/INPUT_FOCUS

  XManager, 'WID_BASE_CUSTOM', WID_BASE_CUSTOM, /NO_BLOCK, cleanup='SD_Connect_cleanup'

end
;
; Empty stub procedure used for autoloading.
;
pro SD_ROI_MANAGE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_CUSTOM, GROUP_LEADER=BASE_TOP, _EXTRA=_VWBExtra_
end
