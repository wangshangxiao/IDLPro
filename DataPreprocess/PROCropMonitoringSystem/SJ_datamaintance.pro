;����ά��ģ��
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

; Generated on:	08/23/2006 17:00.23
;ť�����޸���2009��3��27��

PRO SJ_datamaintance_CleanAllHeap,WID_BASE_0
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2
    WIDGET_CONTROL,WID_BASE_0,GET_UVALUE=PA
    HEAP_FREE,PA
END

PRO SJ_datamaintance_help,event

;	PRINT,'����ά��,����'

	IF FILE_TEST('HELP\HELP.chm') THEN BEGIN
		ONLINE_HELP, '����ά��', BOOK='HELP\HELP.chm', /FULL_PATH
	ENDIF ELSE BEGIN
		info_help=DIALOG_MESSAGE('�Ҳ��������ĵ�',TITLE='����')
	ENDELSE

;	ONLINE_HELP,  BOOK='HELP\HELP.chm','����ά��'
;	temp=dialog_message('ϵͳ��û�а���')
END

;����ά�����
PRO SJ_maintance_save,event

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		if obj_valid(progressTimer) ne 0 then $
			OBJ_DESTROY,progressTimer ;���ٽ�����
		help, /last_message, output=errtext
		common_log,'���д���' + errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

	WIDGET_CONTROL, event.top, GET_UVALUE=pstate
    ;--------------------------���ȵõ�������ʾ��widget_table�е�����---------------
    warning=DIALOG_MESSAGE('��ȷ��Ҫ�������в�����',TITLE='����', /QUESTION)

    IF warning EQ 'Yes' THEN BEGIN
		;��ȡҪ��������ݱ�����
    	;tablename=(*pstate).drop_tablename[0]

		;��ȡ��Ӧ���ݱ�����
        ;sql='select top 9 * from '+tablename+''

        sql=(*pstate).sql
        mm = NQ_SJ_GetdataFromDB_Str_gai(SQL,N_RECORDS = rownum_temp,Column_Name=temp1_temp,N_columns = columnnum_temp)

        IF array_equal(*((*pstate).columnname_EN),temp1_temp) EQ 0 THEN BEGIN
        	warning=DIALOG_MESSAGE('���ڵı�ṹ��������Ľṹ��һ�£��޷����б������(��μ��û��ֲ�)',title='����')
        	return
        ENDIF

        WIDGET_CONTROL,/HOURGLASS

        progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='���ݴ�����,���Ժ�!',TITLE='����ά��') ;�½�����������
        progressTimer->START
        progressTimer->UPDATE, (0.1 * 100.0)  ;���½�����

;	    drop_tablename=(*pstate).drop_tablename[0]
;	    SQL='SELECT top 9 * FROM '+ drop_tablename +''
;	    (*pstate).sql=SQL

       	oRS = OBJ_NEW('IDLdbRecordset',DBobj, SQL=SQL)
       	oRS->getproperty,FIELD_INFO =a

		drop_tablename=a[0].table_name

		;���ԭ���в�ֻһ������,��Ҫɾ������
     	IF rownum_temp NE 0 THEN BEGIN

	   		column=(*pstate).table_column                ;��ǰ���м���
			;row=(*pstate).table_row
	    	row=N_elements(*((*pstate).tablecontext))/ column       ;��ǰ���м���

	    	B=strarr(column,row)
;	    	for j=0,row-1 do begin
;	        	for i=0,column-1 do begin
;	            	B(i,j)=(*((*pstate).tablecontext))(i,j)
;	        	endfor
;	    	endfor
			B=(*((*pstate).tablecontext));��ȡ���ݱ�����

		    m=oRS->NFields()    ;m:�����ֶ���
		    datatable =STRARR(m,1)
		    temp1=strarr(1,1)

		    for i=0,m-1 do begin
		    	temp1=[temp1,string(a(i).field_name)]
		    endfor

		    temp1=temp1[1:*];��������

	        progressTimer->UPDATE, (0.3 * 100.0)  ;���½�����

	    ;--------------------------Ȼ�������ݿ���ɾ��������ʾ��widget_table�е�����-----

		    c=strarr(1,1)
		    d=''
		    SY='''
		    l=' '
		    nonzero_num=0          ;���� �ĸ���
	;--------------------------------------------------------------------------
	  		for j=0,row-1 do begin
	  			nonzero_num=0
	     		for i=0,column-1 do begin
			    	if i ne column-1 then begin
						if a(i).TYPE_NAME ne 'VARCHAR' then begin      ;���Ϊ������
	                    	if B[i,j] eq '' then begin
								d=strtrim(d)
	                        endif else begin
	                        	if nonzero_num eq 0 then begin

							    	B[i,j]=DOUBLE(B[i,j])
							        d=d+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
							        nonzero_num=nonzero_num+1
;		                                d=d+' and  '
                                endif else begin
;                                       d=d
								    B[i,j]=DOUBLE(B[i,j])
							       	d=d+' and  '+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
							       	nonzero_num=nonzero_num+1
                                endelse
						    endelse
					    endif else begin                               ;���Ϊ�ı���
					    	if B[i,j] eq '' then begin
								d=strtrim(d)
							endif else begin
	                            if nonzero_num eq 0 then begin
                                    B[i,j]=SY+ B[i,j]+SY
								    d=d+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
				          		    d=strtrim(d)
				          		    nonzero_num=nonzero_num+1
					          	endif else begin
				         		    B[i,j]=SY+ B[i,j]+SY
				         		   	d=d+' and  '+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
				          		    d=strtrim(d)
				          		    nonzero_num=nonzero_num+1
					          	endelse
							endelse
						endelse
				    endif else begin                                        ;���Ϊ���һ���ֶ���
				   		if a(i).TYPE_NAME ne 'VARCHAR' then begin      ;���Ϊ������
	                    	if B[i,j] eq '' then begin         ;���Ϊ������ ,����Ϊnull
					        	d=strtrim(d)
					        endif else begin                             ;���Ϊ������ ,���ǲ�Ϊnull
						        if nonzero_num eq 0 then begin
						        	B[i,j]=DOUBLE(B[i,j])
							        d=d+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
							        nonzero_num=nonzero_num+1
                                endif else begin
;							          	d=' and  '+d
						          	B[i,j]=DOUBLE(B[i,j])
							        d=d+' and  '+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
							        nonzero_num=nonzero_num+1
							    endelse
					        endelse
					    endif else begin                                ;���Ϊ�ı���
	                        if B[i,j] eq ' ' then begin        ;���Ϊ�ı���Ϊnull,��
					       		d=strtrim(d)
					        endif else begin;���Ϊ�ı��Ҳ�Ϊnull,��
					            if nonzero_num eq 0 then begin
				            	    B[i,j]=SY+ B[i,j]+SY
								    d=d+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
					          	    d=strtrim(d)
                                    nonzero_num=nonzero_num+1
					          	endif else begin
 				                    B[i,j]=SY+ B[i,j]+SY
								    d=d+' and  '+l+''+temp1[i]+' = '+STRTRIM(B[i,j],2)+''
					          	    d=strtrim(d)
                                    nonzero_num=nonzero_num+1
                                endelse
							endelse
					    endelse
				  	endelse
	     		endfor
	        	c=[[c],[d]]
	        	d=''
	 		endfor
;*******************************
	  		c=c[*,1:row]
	        progressTimer->UPDATE, (0.6 * 100.0)  ;���½�����


		         for i=0,row-1 do begin
		      		CATCH, Error_status               ;��ȡ����.
		    		 IF Error_status NE 0 THEN BEGIN
		       			 CATCH, /CANCEL
		        		 Goto,next
		             ENDIF
		        	 SQL1='delete from  '+ drop_tablename +' where '+c(0,i)+' '
		             DBobj->ExecuteSQL,SQL1
		             next:
		         endfor
  		endif                ;�ж�ԭ�����Ƿ�ֻ��һ�����ݽ���

		    ;--------------------------Ȼ��õ�������ʾ��widget_table�е�����---------------

		WIDGET_CONTROL, (*pstate).data_table, GET_VALUE=str_valuet   ;value:������ʾ��widget_table�е�����

		str_value=str_valuet(*,1:*)
		if SIZE(str_value,/TYPE) eq 8 Then BEGIN

	    	columnnow=(*pstate).table_column
		    row =size(str_value,/N_ELEMENTS)
		    Value=strarr(columnnow,row)

			for i=0,columnnow-1 do begin
				IF SIZE(str_value.(i),/TYPE) EQ 1 THEN BEGIN
					Value[i,*]=strtrim(FIX(str_value.(i)),2)
				ENDIF ELSE Value[i,*]=strtrim(str_value.(i),2)
			endfor

	    ENDIF else begin
        	Value=str_value
        endelse

		flag=array_equal(value,'')           ;flag�����ж����ڱ���������Ƿ�ȫΪ��

        if flag ne 1 then begin
			columnnow=(*pstate).table_column

		    rownow=N_elements(value)/ columnnow
;--------------------------Ȼ��������ʾ��widget_table�е������������ݿ�-------
			c=strarr(1,1)
			b=''
			SY='''

			for j=0,rownow-1 do begin
				b=''
			    for i=0,columnnow-1 do begin
					if i ne columnnow-1 then begin
						if a(i).TYPE_NAME ne 'VARCHAR' then begin      ;���Ϊ������
			            	if value[i,j] eq '' then begin
			                	b=b+'null'+','
					          	b=strtrim(b)
                            endif else begin
					        	value[i,j]=FLOAT(value[i,j])
					          	b=b+value(i,j)+','
					          	b=strtrim(b)
					        endelse
				     	endif else begin                               ;���Ϊ�ı���
				     		if value[i,j] eq '' then begin
;						     	Alldata[i,j]=SY+ Alldata[i,j]+SY
					        	b=b+'null'+','
					          	b=strtrim(b)
						    endif else begin
;						        Alldata[i,j]=FLOAT(Alldata[i,j])
                                value[i,j]=SY+ value[i,j]+SY
				          		b=b+value(i,j)+','
				          		b=strtrim(b)
						    endelse
						endelse
				    endif else begin                                        ;���Ϊ���һ���ֶ���
			   		    if a(i).TYPE_NAME ne 'VARCHAR' then begin      ;���Ϊ������
                          	if value[i,j] eq '' then begin         ;���Ϊ������ ,����Ϊnull
				          		b=b+'null'
				          		b=strtrim(b)
				          	endif else begin                             ;���Ϊ������ ,���ǲ�Ϊnull
						    	value[i,j]=FLOAT(value[i,j])
				          		b=b+value(i,j)
				          		b=strtrim(b)
				          	endelse
				     	endif else begin                                ;���Ϊ�ı���
                            if value[i,j] eq '' then begin        ;���Ϊ�ı���Ϊnull,��
				        		b=b+'null'
				          		b=strtrim(b)
				          	endif else begin
                                value[i,j]=SY+ value[i,j]+SY    ;���Ϊ�ı��Ҳ�Ϊnull,��
 				          		b=b+value[i,j]
				          		b=strtrim(b)
						    endelse
				        endelse
			  		endelse

			    endfor
			    c=[[c],[b]]
			endfor
			c=c[*,1:rownow]
		    progressTimer->UPDATE, (0.85 * 100.0)  ;���½�����
			;(*pstate).table_row=rownow
			;(*pstate).table_column=columnnow
			for i=0,rownow-1 do begin
				CATCH, Error_status               ;��ȡ����.
					IF Error_status NE 0 THEN BEGIN
				    	CATCH, /CANCEL
				        Goto,nexts
				    ENDIF
				    	SQL2='INSERT INTO '+ drop_tablename +' VALUES ('+c(0,i)+')'
				        DBobj->ExecuteSQL,SQL2
				        nexts:
			endfor

        endif        ;�жϴ�ʱ���������Ƿ�ȫΪ�ս���

		progressTimer->UPDATE, (1 * 100.0)  ;���½�����
		OBJ_DESTROY,progressTimer         ;���ַ����������ͷ������ڣ���Ҫ��progressTimer->DESTROY

		infor=DIALOG_MESSAGE('��ɱ���!',title='��ʾ' )

	endif else begin;------------------------------------NO:����Ҫ�������в���

	endelse
;		obj_destroy,oRS
END
;--------------------------------------------------------------------------------------

;���һ�м�¼
PRO SJ_maintance_add,event
	WIDGET_CONTROL, event.top, GET_UVALUE=pstate
    WIDGET_CONTROL, (*pstate).data_table, INSERT_ROWS=1
    infor=DIALOG_MESSAGE('�㽫�ڱ�����һ�к���������!',title='��ʾ' )
END
;--------------------------------------------------------------------------------------

PRO SJ_datamaintance,EVENT

END
;-----------------------------------------------------------------------
;�ر�����ά���Ի���
PRO SJ_maintance_close,event
;	 CLOSE,/all
     common_log,'�ر�����ά��'
     WIDGET_CONTROL, EVENT.TOP, /destroy

END
;--------------------------------------------------------------------------

;ɾ����
PRO SJ_maintance_delete,event

	WIDGET_CONTROL,EVENT.TOP,GET_UVALUE=PSTATE

	WIDGET_CONTROL,(*pstate).data_table,/DELETE_ROWS,/USE_TABLE_SELECT

	widget_control,(*pstate).data_table,get_value=test
	record_num=size(test)
	if record_num[0] eq 1 then begin

		WIDGET_CONTROL, (*pstate).data_table, INSERT_ROWS=1
		infor=DIALOG_MESSAGE('����ֻ��һ�������ˣ�ɾ����ϵͳ���Զ�����һ�п���!',title='��ʾ' )

	endif

	column=(*pstate).table_column                 ;�õ����ڱ���ֶ���Ŀ��Ҳ�������ڱ������
	row=n_elements(value)/column                  ;�õ�ѡ���˼���Ҫɾ�������ݵ�����

END
;---------------------------------------------------------------------------------

PRO SJ_maintance_sqlselect,event
    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

	widget_control, event.top, get_uvalue = pstate
    widget_control,(*pstate).Text_sql,get_value=SQL

    if n_elements(SQL) gt 1 then begin
    	SQL = strjoin(SQL, ' ', /SINGLE)
    endif

 	DB=DBobj
 	(*pstate).SQL=SQL
	CATCH, Error_status               ;��ȡ����.
	IF Error_status NE 0 THEN BEGIN
		CATCH, /CANCEL
		information=dialog_message('���sql�����������!',title='��ʾ' )
		return
	ENDIF

	str_datatable =NQ_SJ_GetdataFromDB_gai(SQL,N_RECORDS = rownum,Column_Name=temp1,N_columns = columnnum)

	postion1=strpos(strlowcase(SQL),'from ')
	postion2=strpos(SQL,' ',postion1+5)

	IF(postion2 NE -1) THEN BEGIN
		SQL1=STRMID(SQL,0,postion2)+'_CN'
	ENDIF ELSE BEGIN
		SQL1=SQL+'_CN'
	ENDELSE

	fieldName_test = NQ_SJ_GetdataFromDB_Str_gai(SQL1)

       if rownum eq 0 then begin

    	    SaveData=strarr(columnnum)
    	    data=[[temp1],[SaveData]]

	    	Widget_Control, (*pstate).data_table,TABLE_YSIZE=1,TABLE_XSIZE=columnnum $
            ,COLUMN_LABELS = fieldName_test $
            ,SET_VALUE=data $
            ,ALIGNMENT=0
            information=dialog_message('û�в�ѯ����Ӧ����',title='��ʾ', /INFORMATION)
       endif else begin

           	SaveData=strarr(columnnum,rownum)

	        for i=0,columnnum-1 do begin
	        	IF SIZE(str_datatable.(i),/TYPE) EQ 1 THEN BEGIN
	            	SaveData[i,*]=strtrim(fix(str_datatable.(i)),2)
	            ENDIF ELSE SaveData[i,*]=strtrim(str_datatable.(i),2)
	        endfor

			data=[[temp1],[SaveData]]

		    Widget_Control, (*pstate).data_table,TABLE_YSIZE=rownum+1,TABLE_XSIZE=columnnum $
            ,COLUMN_LABELS = fieldName_test $
            ,SET_VALUE=data $
            ,ALIGNMENT=0
       endelse
    ptr_free,(*pstate).tablecontext
    ptr_free,(*pstate).columnname_CN
    ptr_free,(*pstate).columnname_EN

    (*pstate).tablecontext=ptr_new(SaveData)
    (*pstate).table_column=columnnum
    (*pstate).table_row=rownum
    (*pstate).columnname_CN=ptr_new(fieldName_test)
    (*pstate).columnname_EN=ptr_new(temp1)

END
;--------------------------------------------------------------------------------------

;ѡ��������
PRO SJ_maintance_tabletype,event

	widget_control, event.top, get_uvalue = pstate

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

	DBCO=DBobj

	table_type=widget_info((*pstate).select_table_type,/COMBOBOX_GETTEXT)

	case table_type of
		'ũ������':SQL="select C_NAME from TABLE_INDEX where TABLE_TYPE='ũ������'"
		'�������':SQL="select C_NAME from TABLE_INDEX where TABLE_TYPE='�������'"
		'�������':SQL="select C_NAME from TABLE_INDEX where TABLE_TYPE='�������'"
		else:
	endcase

	;�ж��Ƿ�����Ӧ���͵ı���û���򷵻ؿձ�
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM=RecordNum

	Obj_Destroy,RecordNumOBJ

    IF NUM LE 0 then begin
        temp=dialog_message('δ���ַ��������ı��',/information,title='��ʾ')
        temp[*,*]=''
        widget_control,(*PSTATE).data_table,set_value=temp[*,*]
        return
    endif

	stru_data =NQ_SJ_GetdataFromDB_Str_gai(SQL)
	table_value=transpose(stru_data)

	;PRINT,TABLE_VALUE

	widget_control,(*pstate).drop_table,set_value=table_value

	id_0=Widget_Info(event.top,FIND_BY_UNAME=('drop_tabe'))
	SJ_maintance_selectdata,{id:id_0,top:event.top,handler:id_0,index:0,STR:table_value[0]}
;	Tables = DBobj -> GetTables()
;
;	a=Tables.type
;    b=Tables.name
;	table=b[where(a eq 'TABLE')]

END

PRO SJ_maintance_selectdata,event

	widget_control, event.top, get_uvalue = pstate

	TABLE_CNAME = widget_info((*pstate).drop_table, /COMBOBOX_GETTEXT);��ȡ�����˵����ı���

	;�������ı�����ѯӢ�ı���
	SQL="SELECT TABLE_NAME FROM TABLE_INDEX WHERE C_NAME ="+"'"+ TABLE_CNAME+"'"
	TABLE_NAME=NQ_SJ_GetdataFromDB_Str_gai(SQL)

;	drop_tablename=TRANSPOSE(TABLE_NAME)

	WIDGET_CONTROL,(*pstate).TABLE_TEXT,set_value=TABLE_NAME

    (*pstate).drop_tablename=TABLE_NAME

    SQL='SELECT top 9 * FROM '+ TABLE_NAME
    (*pstate).sql=SQL

    stru_data =NQ_SJ_GetdataFromDB_gai(SQL,N_RECORDS = rownum,Column_Name=temp1,N_columns = columnnum)

	SQL1=SQL+'_CN'

	fieldName_test = NQ_SJ_GetdataFromDB_Str_gai(SQL1)

    if rownum eq 0 then begin
    	data_table=strarr(columnnum,9)
 		data=[[temp1],[data_table]]
       	Widget_Control, (*pstate).data_table,TABLE_YSIZE=9,TABLE_XSIZE=columnnum $
        	,COLUMN_LABELS = fieldName_test $
        	,SET_VALUE=data $
        	,ALIGNMENT=0
    endif else begin
	    data_table=strarr(columnnum,rownum)
		for i=0,columnnum-1 do begin
			IF SIZE(stru_data.(i),/TYPE) EQ 1 THEN BEGIN
		    	data_table[i,*]=strtrim(fix(stru_data.(i)),2)
		    ENDIF ELSE data_table[i,*]=strtrim(stru_data.(i),2)
		endfor
		data=[[temp1],[data_table]]
	    Widget_control,(*pstate).data_table,SET_TABLE_SELECT=[-1,-1]
	    Widget_Control,(*pstate).data_table,TABLE_YSIZE=rownum+1,TABLE_XSIZE=columnnum $
	    	,COLUMN_LABELS = fieldName_test $
	        ,SET_VALUE=data $
	        ,ALIGNMENT=0

    endelse

    (*pstate).table_column=columnnum  ;�����ڵ���������ȥ

    ptr_free,(*pstate).tablecontext
    ptr_free,(*pstate).columnname_CN
    ptr_free,(*pstate).columnname_EN

    (*pstate).tablecontext=ptr_new(data_table)
    (*pstate).columnname_CN=ptr_new(fieldName_test)
    (*pstate).columnname_EN=ptr_new(temp1)

END


;-------------------------------------------------------------------
PRO SJ_datamaintance_event, Event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of

	else:
	endcase

END
;----------------------------------------------------------------------

pro SJ_datamaintance, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'��������ά��'
	IF (xregistered('SJ_datamaintance') ne 0) then return

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

	;����base���
	WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
	      ,XOFFSET=240 ,YOFFSET=200 ,SCR_XSIZE=600 ,SCR_YSIZE=360  $
	      ,TITLE=' ����ά��' ,SPACE=3 ,XPAD=3 ,YPAD=3,tlb_frame_attr=1)

		;���ݱ��SQL��������ʾ��
		WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'   $
		      ,XOFFSET=6 ,YOFFSET=6 ,SCR_XSIZE=590 ,SCR_YSIZE=290  $
		      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

		;��������
		win_base_button=widget_base(WID_BASE_0,uname='win_base_button',frame=1 $
			,XOFFSET=6,	YOFFSET=300,SCR_XSIZE=582 ,SCR_YSIZE=30)


		;���ݱ���ʾ��
		WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,FRAME=1 $
		      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=307 ,SCR_YSIZE=120  $
		      ,TITLE='IDL' ,SPACE=3,ROW=2 );,XPAD=3 ,YPAD=3,ROW=2)

		;���ݱ�ѡ��
		;��ȡ����

		SQL_TABLE="select C_NAME,TABLE_NAME from TABLE_INDEX where TABLE_TYPE='ũ������'"
	    stru_data =NQ_SJ_GetdataFromDB_Str_gai(SQL_TABLE)
	    table=TRANSPOSE(stru_data)

;		SQL_TABLE="select TABLE_NAME from TABLE_INDEX where TABLE_TYPE='ũ������'"
;	    stru_data =NQ_SJ_GetdataFromDB_Str_gai(SQL_TABLE)
;	    table2=TRANSPOSE(stru_data)

		table_type=['ũ������','�������','�������']

		;������ѡ�����
		WID_BASE_2T=WIDGET_BASE(WID_BASE_2,UNAME='WID_BASE_2T',column=1)

		WID_BASE_21=widget_base(WID_BASE_2T,UNAME='WID_BASE_21',ROW=1)
		WID_LABEL_21=WIDGET_LABEL(WID_BASE_21,UNAME='WID_LABEL_21' $
				,VALUE='������:',xoffset=12,yoffset=19)
		select_table_type = WIDGET_COMBOBOX(WID_BASE_21,UNAME='select_table_type'$
				,SCR_XSIZE=241 ,xoffset=66,yoffset=19,value=table_type,$
				event_pro='SJ_maintance_tabletype')

	    ;���������б����
	    WID_BASE_22=WIDGET_BASE(WID_BASE_2T,UNAME='WID_BASE_22',ROW=1)
	    WID_LABEL_22 = Widget_Label(WID_BASE_22, UNAME='WID_LABEL_22'  $
			,/ALIGN_LEFT ,VALUE='���ݱ�:',SCR_XSIZE=42 ,XOFFSET=12,YOFFSET=37)
	   	drop_table = Widget_combobox(WID_BASE_22, UNAME='drop_table'  $
				,value=table[*,0],Event_pro='SJ_maintance_selectdata',SCR_XSIZE=241 ,XOFFSET=66 ,YOFFSET=35)

		WID_BASE_23=WIDGET_BASE(WID_BASE_2T,UNAME='WID_BASE_23',ROW=1)
		WID_LABEL_23=Widget_Label(WID_BASE_23, UNAME='WID_LABEL_23'  $
				,SCR_XSIZE=42   $
				,/ALIGN_LEFT ,VALUE='��  ��:')
		TABLE_TEXT=WIDGET_TEXT(WID_BASE_23,UNAME='TABLE_TEXT' $
				,SCR_XSIZE=241 ,value=table[0,1])


		;���ݱ���ʾ��
	  	WID_BASE_4 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_4'   $
	      	,XOFFSET=0,YOFFSET=125,SCR_XSIZE=582 ,SCR_YSIZE=164  $
	      	,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3,/align_left,frame=1)

	    sql='select top 9 * from AGRO_METEO_DATA_DAY'

	    str_datatable = NQ_SJ_GetdataFromDB_gai(SQL,N_RECORDS=rownum,Column_Name=tablename,N_columns=columnnum)

		columnnames_EN=tablename
	    datatable=strarr(columnnum,rownum)

	    for i=0,columnnum-1 do begin
	        IF SIZE(str_datatable.(i),/TYPE) EQ 1 THEN BEGIN
	    	   	datatable[i,*]=strtrim(fix(str_datatable.(i)),2)
	        ENDIF ELSE datatable[i,*]=strtrim(str_datatable.(i),2)
	    endfor

		SQL1=SQL+'_CN'

		columnnames_CN = NQ_SJ_GetdataFromDB_Str_gai(SQL1)
;		columnnames_CN=['̨վ��','��','��','��','Ѯ','�������','ƽ������','�������','������','�������ʪ��']

		testtable=[[columnnames_EN],[datatable]]

;		test=[[tablename],[str_datatable]]
		;��ʾ��
	    data_table = Widget_Table(WID_BASE_4, UNAME='WID_TABLE_0',ALIGNMENT=0 $
	    	,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=582 ,SCR_YSIZE=165,XSIZE=10  $
	     	,YSIZE=9,VALUE=testtable,column_LABELS=columnnames_CN $
	      	,DISJOINT_SELECTION=1,/SCROLL  $
	      	,/ALL_EVENTS,/editable,column_width=80,/RESIZEABLE_COLUMNS )


		;��ť��ʾ
	  	WID_BUTTON_3 = Widget_Button(win_base_button, UNAME='WID_BUTTON_3'  $
	    	,XOFFSET=41 ,YOFFSET=4 ,SCR_XSIZE=60   $
	    	,/ALIGN_CENTER ,VALUE='���',event_pro='SJ_maintance_add')

	  	WID_BUTTON_4 = Widget_Button(win_base_button, UNAME='WID_BUTTON_4'  $
	    	,XOFFSET=148 ,YOFFSET=4 ,SCR_XSIZE=60   $
	    	,/ALIGN_CENTER ,VALUE='ɾ��',event_pro='SJ_maintance_delete')

	  	WID_BUTTON_6 = Widget_Button(win_base_button, UNAME='WID_BUTTON_6'  $
	    	,XOFFSET=255 ,YOFFSET=4 ,SCR_XSIZE=60   $
	    	,/ALIGN_CENTER ,VALUE='����',event_pro='SJ_maintance_save')


		;SQL����ѯģ��
	  	WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3'   $
	    	,XOFFSET=313 ,YOFFSET=0 ,SCR_XSIZE=273 ,SCR_YSIZE=120  $
	    	,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

	  	WID_BUTTON_add = Widget_Base(WID_BASE_3, UNAME='WID_BUTTON_add'  $
	    	,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=269 ,SCR_YSIZE=120,FRAME=1 )

	  	Text_sql = Widget_Text(WID_BUTTON_add, UNAME='Text_sql' ,XOFFSET=7  $
	    	,YOFFSET=28 ,SCR_XSIZE=253 ,SCR_YSIZE=60 ,/EDITABLE ,XSIZE=20  $
	    	,YSIZE=40, /SCROLL,/wrap,value='select top 4 * from agro_meteo_data_day')


	  	WID_BUTTON_0 = Widget_Button(WID_BUTTON_add, UNAME='WID_BUTTON_0'  $
	    	,XOFFSET=75 ,YOFFSET=91 ,SCR_XSIZE=125 ,SCR_YSIZE=23  $
	    	,/ALIGN_CENTER ,VALUE='��sql�����в�ѯ',event_pro='SJ_maintance_sqlselect')


	  	WID_LABEL_1 = Widget_Label(WID_BUTTON_add, UNAME='WID_LABEL_1'  $
	    	,XOFFSET=12 ,YOFFSET=10 ,SCR_XSIZE=250 ,SCR_YSIZE=16  $
	    	,/ALIGN_LEFT ,VALUE='����SQL����ѯ')

;		ԭ����SQL���Բ�ѯ�ⲿ��ģ�鲻֧��ɾ������Ӻͱ��棬$
;		�����޸��ˣ�����ȥ�����ⲿ��ע��(ť����)

;	  	WID_LABEL_1 = Widget_Label(WID_BUTTON_add, UNAME='WID_LABEL_1'  $
;	    	,XOFFSET=12 ,YOFFSET=26 ,SCR_XSIZE=250 ,SCR_YSIZE=16  $
;	    	,/ALIGN_LEFT ,VALUE='ɾ������ӻ򱣴�)')


		;�رպͰ�������
	  	WID_BUTTON_8 = Widget_Button(win_base_button, UNAME='WID_BUTTON_8'  $
	    	,XOFFSET=466 ,YOFFSET=4 ,SCR_XSIZE=60   $
	    	,/ALIGN_CENTER ,VALUE='�ر�',event_pro='SJ_maintance_close')


	  	WID_BUTTON_9 = Widget_Button(win_base_button, UNAME='WID_BUTTON_9'  $
	    	,XOFFSET=363,YOFFSET=4 ,SCR_XSIZE=60   $
	    	,/ALIGN_CENTER ,VALUE='����',event_pro='SJ_datamaintance_help')

	  	drop_tablename=table[*,1]
		;columnnames=['̨վ��','��','��','��','Ѯ','�������','ƽ������','�������','������','�������ʪ��']
	  	;columnnames=['STATION_ID','YEAR','MONTH','DAY','TENDAY','TEMP_MAX','TEMP_AVE','TEMP_MIN','PPT','HU_RA']
	  	sql='select top 9 * from AGRO_METEO_DATA_DAY'

	Widget_Control,WID_BASE_0,/REALIZE
	WIDGET_CONTROL,WID_BUTTON_8,/INPUT_FOCUS

	state = { tablecontext:ptr_new(datatable),$
			  table : table, $
              data_table : data_table,  $
              select_table_type : select_table_type, $
              drop_table : drop_table, $��;ѡ������������˵������
              drop_tablename : drop_tablename,$ ;�������������̴�������ѡ��ı���
              TABLE_TEXT : TABLE_TEXT, $
              sql : sql,$
              Text_sql : Text_sql, $
              columnname_CN : ptr_new(columnnames_CN), $
              columnname_EN : ptr_new(columnnames_EN), $
              table_row : 9,$
              table_column : 10 $   ;table�����������Զ�̬�����ı�
            }
	pstate = PTR_NEW(state, /no_copy)
	widget_control, WID_BASE_0, set_uvalue=pstate
	XManager, 'SJ_datamaintance', WID_BASE_0,CLEANUP='SJ_datamaintance_CleanAllHeap',/NO_BLOCK

END


pro SJ_datamaintance_interface,GROUP_LEADER=wGroup

	;	if (xregistered('SJ_datamaintance') ne 0) then return

	    SJ_datamaintance,GROUP_LEADER=wGroup

end