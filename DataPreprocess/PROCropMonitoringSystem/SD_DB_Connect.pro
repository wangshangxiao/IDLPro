
;**************实现以*字符来显示密码的操作************************
pro CONN_passwordtext,event        ;注意在该组件的关键字中必须加上/ALL_EVENTS事件.

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

;	widget_control,event.top,GET_UVALUE=pstate
	widget_control,event.id,GET_VALUE=passw   ;注意此时的passw是只有一个元素的字符串数组,即password[0]

	;help,event,/STRU
	temppass=''
	length=STRLEN(passw[0])


	if event.type eq 0 then begin                    ;插入单个字符
	   temppass=string(event.ch)
	   PWD=PWD+temppass ;不用指针也是可以的
	   temppass=bytarr(length)
	   temppass[*]=byte('*')
	   widget_control,event.id,SET_VALUE=string(temppass),SET_TEXT_SELECT=STRLEN(temppass)
	endif

	if event.type eq 2 and length gt 0 then begin    ;删除字符
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
    ;**********************下面是"连接实现***********************
PRO CONN_REALIZE,EVENT

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
      WIDGET_CONTROL, /HOURGLASS        ;当事件处理处于忙的状态时,指针变为等待状态
                                     ;区别"DEVICE, CURSOR_STANDARD = 30 "指针变化

      WIDGET_CONTROL,EVENT.TOP,GET_UVALUE=PSTATE

	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
	WIDGET_CONTROL,BASE_TOP,GET_UVALUE=diaoduID
	;WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=1

      ;为了不与公共变量区的变量重名,给这些变量都加了_1
      WIDGET_CONTROL,(*PSTATE).TEXT_USERNAME,GET_VALUE=USER_NAME_1
      USER_NAME_1	=	STRTRIM(USER_NAME_1[0],2)
      print,"USER_NAME_1     ",USER_NAME_1	;这句由杨绍锷所加

;      WIDGET_CONTROL,(*PSTATE).TEXT_DSN,GET_VALUE=DSN_1
      DSN_1 = widget_info((*PSTATE).TEXT_DSN,/COMBOBOX_GETTEXT)
      DSN_1		=	STRTRIM(DSN_1[0],2)
		print,"DSN_1    ",DSN_1     ;这句由杨绍锷所加

;	  WIDGET_CONTROL,(*PSTATE).TEXT_PWD,GET_VALUE=PWD_1
;      PWD_1		=	STRTRIM(PWD_1[0],2)
		PWD_1		=	PWD
		print,"PWD_1    ",PWD_1     ;这句由杨绍锷所加

;      OD = OBJ_NEW('IDLDBDATABASE')
;      OD->SETPROPERTY,/USE_CURSOR_LIB,/VERBOSE

	  ;调试程序使用的内容
;;     DBobj=OBJ_NEW('IDLdbDatabase')
	   CATCH, Error_status
	   IF Error_status NE 0 THEN BEGIN
	;              PRINT, 'Error index: ', Error_status
	;              PRINT, 'Error message: ', !ERROR_STATE.MSG
	      widget_control,(*PSTATE).TEXT_PWD,SET_VALUE=''
	      COMMON COMMON_BASE,MENU_OPERATION,X_START,Y_START,BASE_TOP,MENU_MANAGE
      		WIDGET_CONTROL,MENU_OPERATION,SENSITIVE=0
           WIDGET_CONTROL,MENU_MANAGE	,SENSITIVE=0
	      infomation=DIALOG_MESSAGE('数据库链接失败,请检查密码、数据库设置或网络设置是否正确!',TITLE='错误',/ERROR)
		  WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=0
	      CATCH, /CANCEL
	      RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
	    ENDIF
;	  IF yesORno EQ 1 THEN BEGIN	;该段被杨绍锷注示掉
;	  	WIDGET_CONTROL,EVENT.TOP,/DESTROY
;	  	PROMPT=DIALOG_MESSAGE('数据库链接成功-------',TITLE='链接成功',/INFORMATION)
;	  	RETURN
;	  ENDIF

      yesORno=0
    OBJ_DESTROY,DBobj
	DBobj=OBJ_NEW('IDLdbDatabase')  ;先断开数据库（杨绍锷）

      DBobj->CONNECT,DATASOURCE=DSN_1,USER_ID=USER_NAME_1,PASSWORD=PWD_1
      DBobj->GETPROPERTY,IS_CONNECTED=CONN_YESORNO

      IF CONN_YESORNO EQ 1 THEN BEGIN

           WIDGET_CONTROL,EVENT.TOP,/DESTROY
           PROMPT=DIALOG_MESSAGE('数据库链接成功',TITLE='链接成功',/INFORMATION)
;           COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
           yesORno	=	1
           ;使操作菜单可用
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
           print,"USER_NAME_1     ",USER_NAME_1	;这句由杨绍锷所加
			print,"DSN_1    ",DSN_1     ;这句由杨绍锷所加
			print,"PWD_1    ",PWD_1     ;这句由杨绍锷所加
			print,'=============================================='

           WIDGET_CONTROL,(*PSTATE).TEXT_PWD,SET_VALUE=''
           INFOMATION=DIALOG_MESSAGE('数据库链接失败,请检查数据库相应设置是否正确!_______',TITLE='错误',/ERROR)
           WIDGET_CONTROL,diaoduID.diaodu_Modal_BASE,SENSITIVE=0
      ENDELSE


    END
    ;**************下面是"取消连接"***************************************


    PRO CONN_CANCEL,EVENT
		common_log,'退出数据库连接'
    	WIDGET_CONTROL,EVENT.TOP,/DESTROY

    END

	pro SD_Connect_cleanup,id
		WIDGET_CONTROL,id,GET_UVALUE=PSTATE
		heap_free, PSTATE
	end

PRO SD_DB_Connect_EVENT,EVENT

END

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;链接数据库
;-----------------------------------------------------------------
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
PRO SD_DB_Connect,GROUP_LEADER=wGroup   ;F把主父BASE的PSATE传过来

	common_log,'启动数据库连接'

	if (xregistered('CONN_INTERFACE') ne 0) then return

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	USER_NAME_1=USER_NAME
	PWD_1=PWD
	DSN_1=DSN
	PRINT,USER_NAME,PWD,DSN
 	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
    CONN=WIDGET_BASE(TITLE='链接数据库',TLB_FRAME_ATTR=1,$
    				GROUP_LEADER=BASE_TOP,XOFFSET=400,YOFFSET=200,/COLUMN)
    CONN_INTERFACE=WIDGET_BASE(CONN,ROW=3)

    USER=WIDGET_BASE(CONN_INTERFACE,/ROW)        ;输入用户名BASE
    USERLABEL=WIDGET_LABEL(USER,XSIZE=65,VALUE='用户名:')
    TEXT_USERNAME=WIDGET_TEXT(USER,XSIZE=15,VALUE=USER_NAME_1,/EDIT)

    PASSWORD=WIDGET_BASE(CONN_INTERFACE,/ROW)    ;输入密码BASE
    PASSLABEL=WIDGET_LABEL(PASSWORD,XSIZE=65,VALUE='密  码:')
    TEXT_PWD=WIDGET_TEXT(PASSWORD,XSIZE=15,VALUE='******',/EDIT ,/ALL_EVENTS $
    					,EVENT_PRO='CONN_passwordtext')

    DATASOURCE=WIDGET_BASE(CONN_INTERFACE,/ROW)   ;输入数据库名
    SOURCELABEL=WIDGET_LABEL(DATASOURCE,XSIZE=65,VALUE='数据源名称:')
    tdbobj=obj_new('idldbdatabase')
    DSN_TEMP = tdbobj->getdatasources()
    DSN_LIST = DSN_TEMP[*].(0)
    obj_destroy,tdbobj
    TEXT_DSN=WIDGET_COMBOBOX(DATASOURCE,XSIZE=100,VALUE=DSN_LIST)
;   TEXT_DSN=WIDGET_TEXT(DATASOURCE,XSIZE=15,VALUE=DSN_1,/EDIT)		;这句有误，如果输入新的数据源名称将不起作用（杨绍锷）

    CONN_OPERATE=WIDGET_BASE(CONN,SPACE=40,/ALIGN_CENTER,/ROW)
    CONN_RELIZE=WIDGET_BUTTON(CONN_OPERATE,VALUE='链接',SCR_xSIZE=60,EVENT_PRO='CONN_REALIZE')
    CONN_CANCEL=WIDGET_BUTTON(CONN_OPERATE,VALUE='取消',SCR_xSIZE=60,EVENT_PRO='CONN_CANCEL')

    WIDGET_CONTROL,CONN,/REALIZE
    WIDGET_CONTROL,CONN_CANCEL,/INPUT_FOCUS

    REALPASS=''   ; 下面的CONN_PARAMETER结构体中的PASSWORD用于记录真实的密码。

    CONN_PARAMETER={$
    	TEXT_USERNAME	:	TEXT_USERNAME,$
    	TEXT_DSN	:	TEXT_DSN,$
    	TEXT_PWD	:	TEXT_PWD}
    PSTATE=PTR_NEW(CONN_PARAMETER,/NO_COPY)
    WIDGET_CONTROL,CONN,SET_UVALUE=PSTATE
    XMANAGER, 'SD_DB_Connect', CONN, cleanup='SD_Connect_cleanup'
END