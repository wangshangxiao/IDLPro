;数据导入模块
;钮立明于2009年3月27日完成修改
pro SJ_todatabase_CleanAllHeap,tlb
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2
    WIDGET_CONTROL,tlb,GET_UVALUE=PA
    HEAP_FREE,PA
END

;-------------界面帮助----------------------------
PRO SJ_todatabase_help_event,event
	 PRINT,'数据入库,帮助'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '数据入库', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('找不到帮助文档',title='警告')
	endelse

;     ONLINE_HELP,  BOOK='HELP\HELP.chm','数据入库'
END

;-------------界面关闭----------------------------
PRO SJ_todatabase_close_event,event
;	 CLOSE,/all
	common_log,'关闭数据入库'
     WIDGET_CONTROL, EVENT.TOP, /destroy
END

FUNCTION sj_todb_opencsv_event, event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

     WIDGET_CONTROL,Event.top,get_UVALUE = pstate

     csvfile= dialog_pickfile(dialog_parent=event.top, title='选择csv文件', filter=['*.csv'],path=FILE_PATH,/MUST_EXIST)

	 (*pstate).TXT_sourcefile=csvfile

	 IF (csvfile NE '') THEN BEGIN
     	SJ_importdata, Event
     ENDIF else begin
     	RETURN, Event ; By Default, return the event.
     endelse
END

;选择表格类型
PRO SJ_todatabase_tabletype,event

	widget_control, event.top, get_uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

	DBCO=DBobj

	table_type=widget_info((*pstate).drop_table1,/COMBOBOX_GETTEXT)

	case table_type of
		'农气数据':SQL="select C_NAME from TABLE_INDEX where TABLE_TYPE='农气数据'"
		'历年产量':SQL="select C_NAME from TABLE_INDEX where TABLE_TYPE='历年产量'"
		'作物面积':SQL="select C_NAME from TABLE_INDEX where TABLE_TYPE='作物面积'"
		else:
	endcase

	;判断是否有相应类型的表，若没有则返回空表
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM=RecordNum

	Obj_Destroy,RecordNumOBJ

    IF NUM LE 0 then begin
        temp=dialog_message('未发现符合条件的表格',/information,title='提示')
        temp[*,*]=''
        widget_control,(*PSTATE).data_table,set_value=temp[*,*]
        return
    endif

	stru_data =NQ_SJ_GetdataFromDB_Str_gai(SQL)
	table_value=transpose(stru_data)

	;PRINT,TABLE_VALUE

	widget_control,(*pstate).drop_table2,set_value=table_value

	id_0=Widget_Info(event.top,FIND_BY_UNAME=('drop_tabe2'))
	SJ_selectdata,{id:id_0,top:event.top,handler:id_0,index:0,STR:table_value[0]}
;	Tables = DBobj -> GetTables()
;
;	a=Tables.type
;    b=Tables.name
;	table=b[where(a eq 'TABLE')]

END

;选择数据表
PRO SJ_selectdata,event
    ;-------首先判断选择的是哪个数据表，因为现在给要入库的数据表都起了中文名字-----
    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	widget_control, event.top, get_uvalue = pstate

    drop_tablename = widget_info((*pstate).drop_table2, /COMBOBOX_GETTEXT)

	SQL="SELECT TABLE_NAME FROM TABLE_INDEX WHERE C_NAME ="+"'"+ drop_tablename+"'"
	TABLE_NAME=NQ_SJ_GetdataFromDB_Str_gai(SQL)

	drop_tablename=TRANSPOSE(TABLE_NAME)

	(*pstate).drop_tablename=drop_tablename
    ;=======20070720,以下由杨绍锷添加，用于判断是否选择了‘粘贴错误’===============
    if drop_tablename eq '粘贴错误' then begin
		info_yang=dialog_message('请选择有效表名',title='警告')
		return
    endif
    ;====================================================================

    SQL='SELECT top 12 * FROM '+ drop_tablename     ;  SQL='SELECT CTEST.USERID,CTEST.USERNAME,CTEST.PWD from CTEST'
    (*pstate).sql=SQL
	datatable = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,N_columns = m)
    (*pstate).table_column=m  ;把现在的列数传出去

	SQL1=SQL+'_CN'
	NAME_TABLE = NQ_SJ_GetdataFromDB_Str(SQL1)
	(*pstate).NAME_TABLE=PTR_NEW(NAME_TABLE)

    Widget_Control, (*pstate).data_table,TABLE_YSIZE=12,TABLE_XSIZE=m $
         ,COLUMN_LABELS = NAME_TABLE $
         ,SET_VALUE=datatable[*,*] $
         ,ALIGNMENT=0

END
;-------------------------------------------------------------------------------
PRO SJ_exporthead,event

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		if size(progressTimer) ne 0 then $
			OBJ_DESTROY,progressTimer ;销毁进度条
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control,/hourglass

	widget_control, event.top, get_uvalue = pstate
	drop_tablename=(*pstate).drop_tablename

	;将输出文件名定为中文表名
	SQL="SELECT C_NAME FROM TABLE_INDEX WHERE TABLE_NAME ="+"'"+ drop_tablename+"'"
	TABLE_NAME=NQ_SJ_GetdataFromDB_Str_gai(SQL)

	csvfile= dialog_pickfile(dialog_parent=event.top, title='导出表头', filter=['*.csv'],FILE=TABLE_NAME+'.csv')

	if csvfile eq '' then begin
		return
	end

;	sqlcolumnname='select top 1 * from '+drop_tablename
;   columnname = NQ_SJ_GetdataFromDB_Str_gai(sqlcolumnname,Column_Name=temp1_temp,N_columns = columnnum_temp)
	temp=(*(*pstate).NAME_TABLE)
	context = strjoin(temp,',',/single)

	OPENW,lun,csvfile,/GET_LUN
	printf,lun,context
		close,lun
	FREE_LUN,lun

	infor=DIALOG_MESSAGE('导出表头完成!',title='提示' )
END
;-------------------------------------------------------------------------------

PRO SJ_importdata,event

	;This statement begins the error handler:
	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		if size(progressTimer) ne 0 then $
			OBJ_DESTROY,progressTimer ;销毁进度条
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

    widget_control,/hourglass
    widget_control, event.top, get_uvalue = pstate

	NAME_TABLE=*(*pstate).NAME_TABLE
    ;WIDGET_CONTROL,(*pstate).TXT_sourcefile,get_value=TXT_sourcefile

	;首先读取csv中数据
    TXT_sourcefile = (*pstate).TXT_sourcefile
    drop_tablename=(*pstate).drop_tablename

	if file_test(TXT_sourcefile) eq 0 then begin
		info_yang=dialog_message('选择导入的文件不存在',title='警告')
		return
	endif

    ;=======20070720,以下由杨绍锷添加，用于判断是否选择了‘粘贴错误’===============
    if drop_tablename eq '粘贴错误' then begin
		info_yang=dialog_message('请选择有效表名',title='警告')
		return
    endif
    ;====================================================================
;---------------------
   	WIDGET_CONTROL, event.top, GET_UVALUE=pstate

    sqlcolumnname='select top 1 * from '+drop_tablename+''
;   print,sqlcolumnname
    columnname = NQ_SJ_GetdataFromDB_Str_gai(sqlcolumnname,Column_Name=temp1_temp,N_columns = columnnum_temp)
;---------------------
    progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='数据入库') ;新建进度条对象
	progressTimer->START
	progressTimer->UPDATE, (0.2 * 100.0)  ;更新进度条


    SQL=(*pstate).sql
    ;首先读取当前表格的信息
        (*pstate).drop_tablename=drop_tablename
;		 oD = DBobj
         oRS = OBJ_NEW('IDLdbRecordset',DBobj, SQL=SQL)
         m=oRS->NFields()
;	读取csv文件
	RecordLines = FILE_LINES(TXT_sourcefile)   ;lines of csv
	TimeRecord = STRARR(RecordLines)

	if RecordLines le 1 then begin
		OBJ_DESTROY,progressTimer ;销毁进度条
		infors=DIALOG_MESSAGE('导入CSV文件中有效数据为空',title='警告')
		return
	endif

	progressTimer->UPDATE, (0.4 * 100.0)  ;更新进度条
	OPENR,lun,TXT_sourcefile,/GET_LUN
	READF,lun,TimeRecord
	FREE_LUN,lun

;	for i=0,RecordLines[0]-1 do begin	;原代码
	for i=0.0,RecordLines[0]-1 do begin	;杨绍锷修改，20070717
	    if TimeRecord[i] eq '' then begin
		    OBJ_DESTROY,progressTimer ;销毁进度条
		    infors=DIALOG_MESSAGE('你必须保证不能有空行!',title='提示' )

	    	return
	    endif
	endfor

	num1=strsplit(TimeRecord(0),',',count=mm,/EXTRACT,/PRESERVE_NULL)
	num2=strsplit(TimeRecord(RecordLines[0]-1),',',count=mm2,/EXTRACT,/PRESERVE_NULL)

	;判断列名是否一致
    if ARRAY_EQUAL(num1,NAME_TABLE) ne 1 then begin
       OBJ_DESTROY,progressTimer ;销毁进度条
	   infors=DIALOG_MESSAGE('你必须保证源文件的第一行为数据表的列名!',title='提示' )
	   return
    endif

	if (mm eq m) and (m eq mm2) then begin               ;如果第1行的列或者最后一行的个数小于表格的列数，则不能导入

    	;countofcomma=strsplit(TimeRecord(2),',',count=mm,/EXTRACT)
;    	print,'STRSPLIT(TimeRecord[1],ESCAPE=',', /EXTRACT)'
    	column=(*pstate).table_column
    ; 下面是对读取进来的数据进行较为复杂的处理,将所有为空的列全赋值为null
        num2=STRSPLIT(TimeRecord[0],',', /EXTRACT,/PRESERVE_NULL)
        Alldata = STRARR(m,1)
;   	 FOR I=0,RecordLines[0]-1 DO BEGIN	;原代码
;		num_test_1=0.0	;杨绍锷修改，20070717
;		num_test_2=0.0
   	 FOR I=0,RecordLines[0]-1 DO BEGIN	;杨绍锷修改，20070717
   	    c=STRSPLIT(TimeRecord[I],',', /EXTRACT,/PRESERVE_NULL)
;   	    if 逗号数目 eq 列数-1 then begin
;===========原代码=====================================
;	        for j=0,m-1 do begin
;	            if c(j) eq '' then begin
;	            c(j)='null'
;	            endif
;	        endfor
;		Alldata = [[Alldata],[c]]
;===========原代码=====================================
;====以下为杨绍锷修改的代码，20070717==================
		num_c=n_elements(c)

		case 1 of

			num_c eq m : begin

				for j=0,m-1 do begin
		            if c(j) eq '' then begin
		            c(j)='null'
		            endif
		        endfor
		        Alldata = [[Alldata],[c]]
;		        num_test_1=num_test_1+1
;		        print,'num_test_1=',num_test_1
	        end

		    num_c lt m : begin
		    	temp=STRARR(m,1)
		    	for temp_flag=0,num_c-1 do begin
					temp[temp_flag]=c[temp_flag]
		    	endfor
		    	for j=0,num_c-1 do begin
		            if temp(j) eq '' then begin
		            temp(j)='null'
		            endif
		        endfor
		        for flag_new=num_c , m-1 do begin
					temp(flag_new)='null'
		        endfor
;		        c=temp
				Alldata = [[Alldata],[temp]]
;				num_test_2=num_test_2+1
;		        print,'num_test_2=',num_test_2
;    aa
		    end

			num_c gt m : begin
				info=dialog_message('表中数据有误，请检查',title='警告')
				Alldata=''
				return
			end

	    	else:
	    endcase
;======================================================

	 ENDFOR
  	  Alldata = Alldata[*,2:*]
      oRS->getproperty,FIELD_INFO =a
         datatable =STRARR(m,1)
	     c=strarr(1,1)
	     b=''
	     SY='''
	     t1='''

	progressTimer->UPDATE, (0.6 * 100.0)  ;更新进度条
	for j=0,RecordLines[0]-2 do begin
    	for i=0,m-1 do begin
			if i ne m-1 then begin
				if a(i).TYPE_NAME ne 'VARCHAR' then begin      ;如果为数字型

                	if Alldata[i,j] eq 'null' then begin
	                	b=b+Alldata(i,j)+','
			          	b=strtrim(b)
	                endif else begin
					        	Alldata[i,j]=FLOAT(Alldata[i,j])
					          	b=b+Alldata(i,j)+','
					          	b=strtrim(b)
					        endelse
				     	endif else begin                               ;如果为文本型
				     	    if Alldata[i,j] eq 'null' then begin
;						     	    Alldata[i,j]=SY+ Alldata[i,j]+SY
						        	b=b+Alldata(i,j)+','
						          	b=strtrim(b)
						    endif else begin
;						            Alldata[i,j]=FLOAT(Alldata[i,j])
                                    Alldata[i,j]=SY+ Alldata[i,j]+SY
				          		    b=b+Alldata(i,j)+','
				          		    b=strtrim(b)
						    endelse
						endelse
			   endif else begin                                        ;如果为最后一个字段了
			   		    if a(i).TYPE_NAME ne 'VARCHAR' then begin      ;如果为数字型

                          if Alldata[i,j] eq 'null' then begin         ;如果为数字型 ,但是为null
				          	b=b+Alldata(i,j)
				          	b=strtrim(b)
				          endif else begin                             ;如果为数字型 ,但是不为null
						    Alldata[i,j]=FLOAT(Alldata[i,j])
				          	b=b+Alldata(i,j)
				          	b=strtrim(b)
				          endelse
				     	endif else begin                                ;如果为文本型
                            if Alldata[i,j] eq 'null' then begin        ;如果为文本且为null,则
				        		b=b+Alldata(i,j)
				          		b=strtrim(b)
				          	endif else begin
						          Alldata[i,j]=SY+ Alldata[i,j]+SY
						           ; Alldata[i,j]=FLOAT(Alldata[i,j])    ;如果为文本且不为null,则
 				          		   ; b=SY+ Alldata[i,j]+SY
 	                                b=b+Alldata[i,j]
				          		    b=strtrim(b)
						    endelse
				        endelse
			  endelse

     endfor
        c=[[c],[b]]
        b=''
 endfor

 Alldata=''	;杨绍锷添加，清空数组Alldata，20070717

        c=c[*,1:RecordLines[0]-1]
 	progressTimer->UPDATE, (0.8 * 100.0)  ;更新进度条
;         print,c(0,0)
         for i=0,RecordLines[0]-2 do begin
;            print,i
      		CATCH, Error_status               ;截取错误.
    		 IF Error_status NE 0 THEN BEGIN
       			 CATCH, /CANCEL
        		 Goto,next
;             	 goto,GoOnOther
             ENDIF

        	 SQL1='INSERT INTO '+ drop_tablename +' VALUES ('+c(0,i)+')'

             print,SQL1
             DBobj->ExecuteSQL,SQL1
             next:
         endfor
   ;读取数据库中的各个表分别的字段

   ;导入数据库，然后不能导入的也不报错，但是记录下
  	progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
	OBJ_DESTROY,progressTimer ;销毁进度条
 infor=DIALOG_MESSAGE('完成导入!',title='提示' )

endif else begin
    OBJ_DESTROY,progressTimer ;销毁进度条
    infor=DIALOG_MESSAGE('您必须保证csv文件的第一行和最后一行为各个字段的名称，列数为当前表格的字段数!',title='提示' )

    return
endelse
		obj_destroy,oRS

	SQL='SELECT top 12 * FROM '+ drop_tablename

	datatable = NQ_SJ_GetdataFromDB_Str(SQL)

	Widget_Control, (*pstate).data_table,TABLE_YSIZE=12,TABLE_XSIZE=m $
         ,COLUMN_LABELS = NAME_TABLE $
         ,SET_VALUE=datatable[*,*] $
         ,ALIGNMENT=0
end

;____________________________________________________________________________

pro SJ_todatabase_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    else:
  endcase

end

pro SJ_todatabase,event
end

;--------------------------------------------------------------------------------

PRO SJ_todatabase, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'启动数据入库'

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

	IF ( XREGISTERED('SJ_todatabase') NE 0 ) THEN RETURN

 	TLB = Widget_Base(GROUP_LEADER=BASE_TOP,UNAME='TLB' ,XOFFSET=220  $
      	,YOFFSET=200 ,SCR_XSIZE=630 ,SCR_YSIZE=345  $
      	,TITLE='数据入库' ,SPACE=3 ,XPAD=3 ,YPAD=3,tlb_frame_attr=1,/column)

	;--------------右边的界面--------------------------
    leftbase= Widget_Base(TLB, UNAME='leftbase' $
      ,SPACE=3 ,XPAD=3 ,YPAD=3,/FRAME)
    ;----------------------------------------------------------------
    ;选出前面十几条数据作为实例
    sql='select top 12 * from AGRO_METEO_DATA_DAY'
  	datatable = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,Column_Name=tablename,N_columns = m)

	sql1=sql+'_CN'
	NAME_TABLE=NQ_SJ_GetdataFromDB_Str(SQL1)

	;显示数据表
    data_table = Widget_Table(leftbase, UNAME='data_table'  $
	    	,COLUMN_WIDTHS=60,SCR_Xsize=610,SCR_Ysize=230, $
	    	DISJOINT_SELECTION=1,/TRACKING_EVENTS,/RESIZEABLE_COLUMNS,$
	    	xsize=m,ysize=12,column_LABELS=NAME_TABLE,/SCROLL, $
	    	/ALL_EVENTS,VALUE=datatable[*,*])

    drop_tablename='AGRO_METEO_DATA_DAY'
    ;sql='select top 12 * from AGRO_METEO_DATA_DAY'

    WID_table = Widget_base(TLB, UNAME='WID_table' ,FRAME=1  $
    	,/ROW, SPACE=3 ,XPAD=3 ,YPAD=3,SCR_XSIZE=600,SCR_YSIZE=73)

	;选择入库的数据表
		WID_TABLE1=widget_base(WID_table,UNAME='WID_TABLE1',COLUMN=1,SCR_XSIZE=330,/ALIGN_CENTER)

			WID_TABLE11=widget_base(WID_table1,UNAME='WID_TABLE11',ROW=1,SCR_XSIZE=320,/ALIGN_LEFT)

			table_type=['农气数据','历年产量','作物面积']

			label_table1=widget_label(WID_TABLE11,UNAME='LABEL_TABLE1',VALUE='数据表类型')
			drop_table1=widget_combobox(WID_TABLE11,uname='drop_table1',scr_xsize=250 $
		    	,VALUE=table_type,Event_pro='SJ_todatabase_tabletype')

			WID_TABLE12=widget_base(WID_table1,UNAME='WID_TABLE12',ROW=1,SCR_XSIZE=320,/ALIGN_LEFT)
		    label_table2=widget_label(WID_TABLE12,uname='label_table2',value='目标数据表')

;		    Tables = DBobj -> GetTables()
;		    a=Tables.type
;		    b=Tables.name
;		    table=b[where(a eq 'TABLE')]

			SQL_TABLE="select C_NAME,TABLE_NAME from TABLE_INDEX where TABLE_TYPE='农气数据'"
	    	stru_data =NQ_SJ_GetdataFromDB_Str_gai(SQL_TABLE)
	    	table=TRANSPOSE(stru_data)

		    drop_table2=widget_combobox(WID_TABLE12,uname='drop_table2',scr_xsize=250 $
		    	,VALUE=table[*,0],Event_pro='SJ_selectdata')


;		space_base=widget_base(WID_table,XSIZE=2)
;		label = widget_label(WID_table,value='   ')

;	按键显示
		WID_TABLE2=widget_base(WID_table,UNAME='WID_TABLE2',row=1,/ALIGN_CENTER)
			headexport=widget_button(WID_table2,uname='headexport',value='导出表头' $
		    	,xsize=60,ysize=25, event_pro='SJ_exporthead')

;			space_base=widget_base(WID_table,XSIZE=10)

		    dataimport=widget_button(WID_table2,uname='dataimport',value='导入' $
		    	,xsize=60,ysize=25,event_func='sj_todb_opencsv_event',TOOLTIP='提示：导入前请查看目标表格内容，使其保证与数据文件的内容相对应，并保证记录唯一。');SJ_importdata

;			space_base=widget_base(WID_table,XSIZE=10)

		    SJ_todatabase_close=widget_button(WID_table2,uname='SJ_todatabase_close',$
		    	xsize=60,ysize=25,value='帮助',event_pro='SJ_todatabase_help_event')

;			space_base=widget_base(WID_table,XSIZE=10)

		    SJ_todatabase_help=widget_button(WID_table2,uname='SJ_todatabase_help',$
		    	xsize=60,ysize=25,value='关闭',event_pro='SJ_todatabase_close_event')


    state = { data_table : data_table,  $
              drop_table1 : drop_table1, $
              drop_table2 : drop_table2, $
              TXT_sourcefile : '', $
              drop_tablename : drop_tablename,$
              sql : sql,$
              NAME_TABLE:PTR_NEW(NAME_TABLE), $ ;用于传递中文表头
              table_column : 10 $              ;table的列数，可以动态发生改变
            }
  	pstate = PTR_NEW(state, /no_copy)

	widget_control, tlb, set_uvalue=pstate
    widget_control,tlb,/realize

    WIDGET_CONTROL,SJ_todatabase_help,/INPUT_FOCUS
	XMANAGER,'SJ_todatabase',tlb, CLEANUP='SJ_todatabase_CleanAllHeap',/NO_BLOCK

;    HEAP_GC, /VERBOSE

END

PRO SJ_todatabase_interface,GROUP_LEADER=wGroup
    SJ_todatabase,GROUP_LEADER=wGroup
END













































