
;**************ʵ����*�ַ�����ʾ����Ĳ���************************
pro CONN_passwordtext,event        ;ע���ڸ�����Ĺؼ����б������/ALL_EVENTS�¼�.

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

;	widget_control,event.top,GET_UVALUE=pstate
	widget_control,event.id,GET_VALUE=passw   ;ע���ʱ��passw��ֻ��һ��Ԫ�ص��ַ�������,��password[0]

	;help,event,/STRU
	temppass=''
	length=STRLEN(passw[0])


	if event.type eq 0 then begin                    ;���뵥���ַ�
	   temppass=string(event.ch)
	   PWD=PWD+temppass ;����ָ��Ҳ�ǿ��Ե�
	   temppass=bytarr(length)
	   temppass[*]=byte('*')
	   widget_control,event.id,SET_VALUE=string(temppass),SET_TEXT_SELECT=STRLEN(temppass)
	endif

	if event.type eq 2 and length gt 0 then begin    ;ɾ���ַ�
	   temppass = bytarr(length)
	   temppass[*] = byte('*')
	   PWD = strmid(PWD,0,0 > length)
	   widget_control,event.id,SET_VALUE=string(temppass),SET_TEXT_SELECT=STRLEN(temppass)
	endif

	if length eq 0 then begin
	   PWD =''
	   widget_control,event.id,SET_VALUE=''
	endif

;;	widget_control,event.top,SET_UVALUE=pstate


end
    ;**********************������"����ʵ��***********************
PRO CONN_REALIZE,EVENT

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
      WIDGET_CONTROL, /HOURGLASS        ;���¼�������æ��״̬ʱ,ָ���Ϊ�ȴ�״̬
                                     ;����"DEVICE, CURSOR_STANDARD = 30 "ָ��仯

      WIDGET_CONTROL,EVENT.TOP,GET_UVALUE=PSTATE

	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
	WIDGET_CONTROL,BASE_TOP,GET_UVALUE=diaoduID
	;WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=1

      ;Ϊ�˲��빫���������ı�������,����Щ����������_1
      WIDGET_CONTROL,(*PSTATE).TEXT_USERNAME,GET_VALUE=USER_NAME_1
      USER_NAME_1	=	STRTRIM(USER_NAME_1[0],2)
      print,"USER_NAME_1     ",USER_NAME_1	;���������������

;      WIDGET_CONTROL,(*PSTATE).TEXT_DSN,GET_VALUE=DSN_1
      DSN_1 = widget_info((*PSTATE).TEXT_DSN,/COMBOBOX_GETTEXT)
      DSN_1		=	STRTRIM(DSN_1[0],2)
		print,"DSN_1    ",DSN_1     ;���������������

;	  WIDGET_CONTROL,(*PSTATE).TEXT_PWD,GET_VALUE=PWD_1
;      PWD_1		=	STRTRIM(PWD_1[0],2)
		PWD_1		=	PWD
		print,"PWD_1    ",PWD_1     ;���������������

;      OD = OBJ_NEW('IDLDBDATABASE')
;      OD->SETPROPERTY,/USE_CURSOR_LIB,/VERBOSE

	  ;���Գ���ʹ�õ�����
;;     DBobj=OBJ_NEW('IDLdbDatabase')
	   CATCH, Error_status
	   IF Error_status NE 0 THEN BEGIN
	;              PRINT, 'Error index: ', Error_status
	;              PRINT, 'Error message: ', !ERROR_STATE.MSG
	      widget_control,(*PSTATE).TEXT_PWD,SET_VALUE=''
	      COMMON COMMON_BASE,MENU_OPERATION,X_START,Y_START,BASE_TOP,MENU_MANAGE
      		WIDGET_CONTROL,MENU_OPERATION,SENSITIVE=0
           WIDGET_CONTROL,MENU_MANAGE	,SENSITIVE=0
	      infomation=DIALOG_MESSAGE('���ݿ�����ʧ��,�������롢���ݿ����û����������Ƿ���ȷ!',TITLE='����',/ERROR)
		  WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=0
	      CATCH, /CANCEL
	      RETURN                    ;������������,������ִ����������,�Ի���ִ���.
	    ENDIF
;	  IF yesORno EQ 1 THEN BEGIN	;�öα�������עʾ��
;	  	WIDGET_CONTROL,EVENT.TOP,/DESTROY
;	  	PROMPT=DIALOG_MESSAGE('���ݿ����ӳɹ�-------',TITLE='���ӳɹ�',/INFORMATION)
;	  	RETURN
;	  ENDIF

      yesORno=0
    OBJ_DESTROY,DBobj
	DBobj=OBJ_NEW('IDLdbDatabase')  ;�ȶϿ����ݿ⣨�����ɣ�

      DBobj->CONNECT,DATASOURCE=DSN_1,USER_ID=USER_NAME_1,PASSWORD=PWD_1
      DBobj->GETPROPERTY,IS_CONNECTED=CONN_YESORNO

      IF CONN_YESORNO EQ 1 THEN BEGIN

           WIDGET_CONTROL,EVENT.TOP,/DESTROY
           PROMPT=DIALOG_MESSAGE('���ݿ����ӳɹ�',TITLE='���ӳɹ�',/INFORMATION)
;           COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
           yesORno	=	1
           ;ʹ�����˵�����
           COMMON COMMON_BASE,MENU_OPERATION,X_START,Y_START,BASE_TOP,MENU_MANAGE
           PRINT,'A'
           HELP,MENU_OPERATION
           PRINT,'B'
           HELP,MENU_MANAGE
           WIDGET_CONTROL,MENU_OPERATION,SENSITIVE=1
           WIDGET_CONTROL,MENU_MANAGE	,SENSITIVE=1
           WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=1
;           DBobj	=	OD

      ENDIF ELSE BEGIN
      		COMMON COMMON_BASE,MENU_OPERATION,X_START,Y_START,BASE_TOP,MENU_MANAGE
      		WIDGET_CONTROL,MENU_OPERATION,SENSITIVE=0
           WIDGET_CONTROL,MENU_MANAGE	,SENSITIVE=0
           print,"USER_NAME_1     ",USER_NAME_1	;���������������
			print,"DSN_1    ",DSN_1     ;���������������
			print,"PWD_1    ",PWD_1     ;���������������
			print,'=============================================='

           WIDGET_CONTROL,(*PSTATE).TEXT_PWD,SET_VALUE=''
           INFOMATION=DIALOG_MESSAGE('���ݿ�����ʧ��,�������ݿ���Ӧ�����Ƿ���ȷ!_______',TITLE='����',/ERROR)
           WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=0
      ENDELSE


    END
    ;**************������"ȡ������"***************************************


    PRO CONN_CANCEL,EVENT
		common_log,'�˳����ݿ�����'
    	WIDGET_CONTROL,EVENT.TOP,/DESTROY

    END

	pro SD_Connect_cleanup,id
		WIDGET_CONTROL,id,GET_UVALUE=PSTATE
		heap_free, PSTATE
	end

PRO SD_DB_Connect_EVENT,EVENT

END

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;�������ݿ�
;-----------------------------------------------------------------
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
PRO SD_DB_Connect,GROUP_LEADER=wGroup   ;F������BASE��PSATE������

	common_log,'�������ݿ�����'

	if (xregistered('CONN_INTERFACE') ne 0) then return

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	USER_NAME_1=USER_NAME
	PWD_1=PWD
	DSN_1=DSN
	PRINT,USER_NAME,PWD,DSN
 	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
    CONN=WIDGET_BASE(TITLE='�������ݿ�',TLB_FRAME_ATTR=1,$
    				GROUP_LEADER=BASE_TOP,XOFFSET=400,YOFFSET=200,/COLUMN)
    CONN_INTERFACE=WIDGET_BASE(CONN,ROW=3)

    USER=WIDGET_BASE(CONN_INTERFACE,/ROW)        ;�����û���BASE
    USERLABEL=WIDGET_LABEL(USER,XSIZE=65,VALUE='�û���:')
    TEXT_USERNAME=WIDGET_TEXT(USER,XSIZE=15,VALUE=USER_NAME_1,/EDIT)

    PASSWORD=WIDGET_BASE(CONN_INTERFACE,/ROW)    ;��������BASE
    PASSLABEL=WIDGET_LABEL(PASSWORD,XSIZE=65,VALUE='��  ��:')
    TEXT_PWD=WIDGET_TEXT(PASSWORD,XSIZE=15,VALUE='******',/EDIT ,/ALL_EVENTS $
    					,EVENT_PRO='CONN_passwordtext')

    DATASOURCE=WIDGET_BASE(CONN_INTERFACE,/ROW)   ;�������ݿ���
    SOURCELABEL=WIDGET_LABEL(DATASOURCE,XSIZE=65,VALUE='����Դ����:')
    tdbobj=obj_new('idldbdatabase')
    DSN_TEMP = tdbobj->getdatasources()
    DSN_LIST = DSN_TEMP[*].(0)
    obj_destroy,tdbobj
    TEXT_DSN=WIDGET_COMBOBOX(DATASOURCE,XSIZE=100,VALUE=DSN_LIST)
;   TEXT_DSN=WIDGET_TEXT(DATASOURCE,XSIZE=15,VALUE=DSN_1,/EDIT)		;���������������µ�����Դ���ƽ��������ã������ɣ�

    CONN_OPERATE=WIDGET_BASE(CONN,SPACE=40,/ALIGN_CENTER,/ROW)
    CONN_RELIZE=WIDGET_BUTTON(CONN_OPERATE,VALUE='����',SCR_xSIZE=60,EVENT_PRO='CONN_REALIZE')
    CONN_CANCEL=WIDGET_BUTTON(CONN_OPERATE,VALUE='ȡ��',SCR_xSIZE=60,EVENT_PRO='CONN_CANCEL')

    WIDGET_CONTROL,CONN,/REALIZE
    WIDGET_CONTROL,CONN_CANCEL,/INPUT_FOCUS

    REALPASS=''   ; �����CONN_PARAMETER�ṹ���е�PASSWORD���ڼ�¼��ʵ�����롣

    CONN_PARAMETER={$
    	TEXT_USERNAME	:	TEXT_USERNAME,$
    	TEXT_DSN	:	TEXT_DSN,$
    	TEXT_PWD	:	TEXT_PWD}
    PSTATE=PTR_NEW(CONN_PARAMETER,/NO_COPY)
    WIDGET_CONTROL,CONN,SET_UVALUE=PSTATE
    XMANAGER, 'SD_DB_Connect', CONN, cleanup='SD_Connect_cleanup'
END