;�������ݿ⣬������ΪODBC�е�����Դ���ƣ����Ϊ���ݿ����
;FUNCTION CONNECT_DATABASE,sourcename
;
;	Status = DB_EXISTS()
;
;	if Status ne 1 then begin
;
;		CAUTION = Dialog_Message('���ݿ�����ܲ����ã�',title='����')
;		return,0
;	endif
;
;	DBobj = OBJ_NEW('IDLdbDatabase')
;	DBobj -> IDLdbDatabase::Connect,DATASOURCE = sourcename
;
;	return,DBobj
;END

FUNCTION CREATE_NEW_USER,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).USER_NAME,get_value=USERNAME

	DBobj = (*PSTATE).DBobj

	DBobj ->GetProperty, USER_NAME = variable
aa
;	CREATE_SQL = "CREATE USER "+USERNAME[0]+" "+(*PSTATE).password +" k234"
;	CREATE_SQL = 'select top 3 * from agro_meteo_data_day'
;	CREATE_SQL = 'CREATE USER nlm22 "1234" k123'
;	DBobj -> ExecuteSQL,CREATE_SQL

END

PRO CREATE_USER_EVENT,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).PWD_FIELD,get_value=PWD
	Widget_Control,(*PSTATE).CPWD_FIELD,get_value=CPWD

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='PWD_FIELD'): begin
			common_log,'��������'

			length1=STRLEN(PWD[0])

			if length1 eq 0 then begin
				(*PSTATE).password=''
	   		widget_control,(*PSTATE).PWD_FIELD,SET_VALUE=''
			endif

			if event.type eq 0 then begin                    ;���뵥���ַ�
			   temppass=string(event.ch)
			   (*PSTATE).password += temppass
			   temppass=bytarr(length1)
			   temppass[*]=byte('*')
			   widget_control,(*PSTATE).PWD_FIELD,SET_VALUE=string(temppass),SET_TEXT_SELECT=STRLEN(temppass)
			endif

			if event.type eq 2 and length1 gt 0 then begin    ;ɾ���ַ�
			   temppass = bytarr(length1)
			   temppass[*] = byte('*')
			   (*PSTATE).password = strmid((*PSTATE).password,0,0 > length1)
			   widget_control,(*PSTATE).PWD_FIELD,SET_VALUE=string(temppass), $
			   	SET_TEXT_SELECT=STRLEN(temppass)
			endif

			Widget_Control,event.top,set_uvalue=PSTATE

		end

		Widget_Info(wWidget, FIND_BY_UNAME='CPWD_FIELD'): begin
			common_log,'ȷ������'

			length2=STRLEN(CPWD[0])

			if length2 eq 0 then begin
				(*PSTATE).cpassword=''
	   		widget_control,(*PSTATE).CPWD_FIELD,SET_VALUE=''
			endif

			if event.type eq 0 then begin                    ;���뵥���ַ�
			   temppass=string(event.ch)
			   (*PSTATE).cpassword += temppass
			   temppass=bytarr(length2)
			   temppass[*]=byte('*')
			   widget_control,(*PSTATE).CPWD_FIELD,SET_VALUE=string(temppass), $
			   	SET_TEXT_SELECT=STRLEN(temppass)
			endif

			if event.type eq 2 and length2 gt 0 then begin    ;ɾ���ַ�
			   temppass = bytarr(length2)
			   temppass[*] = byte('*')
			   (*PSTATE).cpassword = strmid((*PSTATE).cpassword,0,0 > length2)
			   widget_control,(*PSTATE).CPWD_FIELD,SET_VALUE=string(temppass), $
			   	SET_TEXT_SELECT=STRLEN(temppass)
			endif

			Widget_Control,event.top,set_uvalue=PSTATE

		end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'�����û�'

				if strlowcase((*PSTATE).password) eq strlowcase((*PSTATE).cpassword) then begin

					CREATE_STATUS = CREATE_NEW_USER(EVENT)

				endif else begin
					CAUTION=dialog_message('������������벻һ�£�',title='����')
					widget_control,(*PSTATE).PWD_FIELD,SET_VALUE=''
					widget_control,(*PSTATE).CPWD_FIELD,SET_VALUE=''
					return
				endelse

				if CREATE_STATUS eq 1 then $
					CAUTION=dialog_message('�����û��ɹ���',title='��ʾ')
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'�˳�����'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'�˳�����'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO CREATE_USER,EVENT

	on_error,2

	Widget_Control,event.top,get_uvalue=PSTATE
	DBobj=(*PSTATE).DBobj

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-210)/2
	w_yoffset=(scr_dims[1]-350)/2

	CU_TLB = WIDGET_BASE(GROUP_LEADER=event.top, UNAME='CU_TLB' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='�����û�' ,SPACE=3 ,XPAD=1 ,YPAD=1  $
      ,COLUMN=1 ,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)

	USER_BASE = WIDGET_BASE(CU_TLB,UNAME='USER_BASE',/COLUMN,/FRAME,SPACE=3,XPAD=1,/BASE_ALIGN_RIGHT)
		USRNAM_BASE = Widget_Base(USER_BASE,/ROW,XPAD=0,YPAD=0,SPACE=3)
			USRNAM_LABEL = Widget_Label(USRNAM_BASE,UNAME='USRNAM_LABEL',VALUE='�û���')
			USER_NAME = Widget_Text(USRNAM_BASE,UNAME='USER_NAME',XSIZE=10,/editable)

		PWD_BASE = Widget_Base(USER_BASE,/ROW,XPAD=0,YPAD=0,SPACE=3)
			PWD_LABEL = Widget_Label(PWD_BASE,UNAME='PWD_LABEL',VALUE='��������')
			PWD_FIELD = Widget_Text(PWD_BASE,UNAME='PWD_FIELD',XSIZE=10,/editable,/ALL_EVENTS)

		CPWD_BASE = Widget_Base(USER_BASE,/ROW,XPAD=0,YPAD=0,SPACE=3)
			CPWD_LABEL = Widget_Label(CPWD_BASE,UNAME='CPWD_LABEL',VALUE='ȷ������')
			CPWD_FIELD = Widget_Text(CPWD_BASE,UNAME='CPWD_FIELD',XSIZE=10,/editable,/ALL_EVENTS)

	CMD_BASE = Widget_Base(CU_TLB,UNAME='CMD_BASE',/FRAME,/row,space=3,XPAD=2)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='ȷ��',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='ȡ��',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,CU_TLB,/REALIZE

	winfo = {	USER_NAME	:	USER_NAME , $
					PWD_FIELD	:	PWD_FIELD , $
					CPWD_FIELD	:	CPWD_FIELD , $
					password	:	'' , $
					cpassword	:	'' , $
					DBobj	:	DBobj $
				}

	QSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,CU_TLB,set_uvalue=QSTATE

	Xmanager,'CREATE_USER',CU_TLB,/NO_BLOCK

END

PRO USER_MANAGEMENT_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='CREATE_BUTT'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'�����û�'
      		CREATE_USER,EVENT

			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'�򿪰����ĵ�'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '��������', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('�Ҳ��������ĵ�',title='����')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'�˳�����'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'�˳�����'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO USER_MANAGEMENT,GROUP_LEADER=wGroup,_EXTRA=_VWBExtra_

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

;���ó����ʼλ��
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;���������
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='�û�����' ,SPACE=3 ,XPAD=2 ,YPAD=1  $
      ,COLUMN=1 ,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)

	if obj_valid(DBobj) eq 0 then return


;�����
	USER_BASE = WIDGET_BASE(TLB_BASE,UNAME='USER_BASE',/ROW,/FRAME,SPACE=3)

		USER_LIST_BASE = WIDGET_BASE(USER_BASE,UNAME='USER_LIST_BASE',/COLUMN)
			USER_LABEL = WIDGET_LABEL(USER_LIST_BASE,UNAME='USER_LABEL',VALUE=' �û�����',/ALIGN_LEFT)
			USER_LIST = WIDGET_LIST(USER_LIST_BASE,UNAME='USER_LIST',value='',/FRAME $
					,XSIZE=20,YSIZE=10)

		GRANT_BASE =WIDGET_BASE(USER_BASE,UNAME='GRANT_BASE',/COLUMN,YPAD=0,SPACE=1)

			OBJ_TYPE=['��','��ͼ']
			OBJ_TYPE_SELECT = WIDGET_DROPLIST(GRANT_BASE,UNAME='OBJ_TYPE_SELECT',VALUE=OBJ_TYPE, $
					TITLE='��������',/ALIGN_LEFT)

			GRANT_OPTION_BASE = WIDGET_BASE(GRANT_BASE,UNAME='GRANT_OPTION_BASE',/COLUMN,/FRAME)
				GRANT_LABEL = WIDGET_LABEL(GRANT_OPTION_BASE,UNAME='GRANT_LABEL',VALUE='Ȩ��',/ALIGN_LEFT)

				GRANT_SELECT_BASE = WIDGET_BASE(GRANT_OPTION_BASE,UNAME='GRANT_SELECT_BASE',/COLUMN,/NONEXCLUSIVE,/ALIGN_LEFT,YPAD=0)
					READ_DA_BUTT = WIDGET_BUTTON(GRANT_SELECT_BASE,UNAME='READ_DA_BUTT',VALUE='��ȡ����')
					UPDATE_DA_BUTT = WIDGET_BUTTON(GRANT_SELECT_BASE,UNAME='UPDATE_DA_BUTT',VALUE='��������')
					INSERT_DA_BUTT = WIDGET_BUTTON(GRANT_SELECT_BASE,UNAME='INSERT_DA_BUTT',VALUE='��������')
					DELETE_DA_BUTT = WIDGET_BUTTON(GRANT_SELECT_BASE,UNAME='DELETE_DA_BUTT',VALUE='ɾ������')

					Widget_Control,READ_DA_BUTT,/SET_BUTTON
					Widget_Control,UPDATE_DA_BUTT,/SET_BUTTON
					Widget_Control,INSERT_DA_BUTT,/SET_BUTTON
					Widget_Control,DELETE_DA_BUTT,/SET_BUTTON

	USER_MNG_BASE = WIDGET_BASE(TLB_BASE,UNAME='USER_MNG_BASE',/ROW,/BASE_ALIGN_CENTER $
			,SPACE=28,XPAD=5,/FRAME)
		CREATE_BUTT = WIDGET_BUTTON(USER_MNG_BASE,UNAME='CREATE_BUTT',VALUE='�����û�',XSIZE=65,YSIZE=25)
		DELETE_BUTT = WIDGET_BUTTON(USER_MNG_BASE,UNAME='DELETE_BUTT',VALUE='ɾ���û�',XSIZE=65,YSIZE=25)
		MODIFY_BUTT = WIDGET_BUTTON(USER_MNG_BASE,UNAME='MODIFY_BUTT',VALUE='�޸��û�',XSIZE=65,YSIZE=25)



;ָ���������
	CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,/row,space=25,XPAD=15)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='����',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='����',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='�˳�',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,TLB_BASE,/REALIZE

	winfo = {	DBobj	:	DBobj , $
					SQL_CMD	:	'' $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=PSTATE

	Xmanager,'USER_MANAGEMENT',TLB_BASE,/NO_BLOCK

END