pro selectdata
    ;-------�����ж�ѡ������ĸ����ݱ���Ϊ���ڸ�Ҫ�������ݱ�������������-----



     drop_table = widget_info((*pstate).drop_table, /COMBOBOX_GETTEXT)


     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE=PSTATE
         SQL='SELECT * FROM CTEST'     ;  SQL='SELECT CTEST.USERID,CTEST.USERNAME,CTEST.PWD from CTEST'
 		 print,SQL


;datatable = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,Column_Name=temp1,N_columns = m)
;if totals eq 0 then begin
;		TEXT=DIALOG_MESSAGE('���ݿ�����ʱû����Ҫ�����ݡ�',/information)
;		return
;endif

		 oRS = OBJ_NEW('IDLdbRecordset',(*PSTATE).DB, SQL=SQL)
		 total=LONG(0)
   	     FLAG=oRS -> MoveCursor(/FIRST)
     IF FLAG EQ 1 THEN BEGIN
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			 REPEAT BEGIN
	            total = total+1
             ENDREP UNTIL(ORS->MoveCursor(/NEXT) EQ 0)
            print,"here is total",total
		Endif else begin
       	   TEXT=DIALOG_MESSAGE('���ݿ�����ʱû����Ҫ�����ݡ�',title='��ʾ')
		Endelse

	ENDIF ELSE BEGIN
	ENDELSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	m=oRS->NFields() & datatable =STRARR(m,1)
		         IF FLAG EQ 1 THEN BEGIN
				       IF (oRS->MoveCursor(/FIRST) EQ 1) THEN BEGIN
			              REPEAT BEGIN
				            Data = oRS->GetRecord()
				            Temp=STRARR(m,1)
				              FOR j=0,m-1 DO BEGIN
				                Temp[j,0]=Data.(j)
				              ENDFOR
				            datatable=[[datatable],[Temp]]
			              ENDREP UNTIL(oRS->MoveCursor(/NEXT) EQ 0)
				       ENDIF ELSE BEGIN
       					TEXT=DIALOG_MESSAGE('���ݿ�����ʱû����Ҫ�����ݡ�',title='��ʾ')
					   Endelse
			    ENDIF ELSE BEGIN
	 ENDELSE

     datatable=datatable[*,1:*]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(*pstate).TABLEROWNUM=total
   	(*pstate).TABLECOLUMN=oRS->NFields()
	PRINT,(*PSTATE).TABLEROWNUM
	PRINT,(*pstate).TABLECOLUMN
	PRINT,DATATABLE[0,0]
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Column_lab = ['�û���','�û���','����']

    Widget_Control, (*pstate).data_table,TABLE_YSIZE=(*pstate).TABLEROWNUM,TABLE_XSIZE=(*pstate).TABLECOLUMN $
         ,COLUMN_LABELS = Column_lab $
         ,SET_VALUE=datatable[*,*] $
;         ,SET_TABLE_VIEW = [0,0] $
         ,ALIGNMENT=0




end
;-------------------------------------------------------------------------------
pro importdata
   ;���ȶ�ȡcsv������


   ;��ȡ���ݿ��еĸ�����ֱ���ֶ�


   ;�������ݿ⣬Ȼ���ܵ����Ҳ���������Ǽ�¼��






end
;--------------------------------------------------------------------------------
pro SJ_todatabase

	IF ( XREGISTERED('SJ_todatabase') NE 0 ) THEN RETURN
 	TLB = Widget_Base(UNAME='TLB' ,XOFFSET=240  $
      ,YOFFSET=200 ,SCR_XSIZE=700 ,SCR_YSIZE=333  $
      ,TITLE='�������' ,SPACE=7 ,XPAD=7 ,YPAD=7)

    main = Widget_Base(TLB, UNAME='main' ,FRAME=1 ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=257 ,SCR_YSIZE=286 ,TITLE='IDL' ,SPACE=7  $
      ,XPAD=7 ,YPAD=7)

    WID_table = Widget_base(main, UNAME='WID_table' ,FRAME=1  $
     ,xoffset=7,yoffset=7,SCR_XSIZE=241 ,SCR_YSIZE=61,SPACE=7 ,XPAD=7  $
      ,YPAD=7)

    label_table1=widget_label(WID_table,uname='label_table1',scr_xsize=85 $
    ,scr_ysize=18,xoffset=7,yoffset=7,value='ѡ��Ŀ�����ݱ�')

;    label_table2=widget_label(WID_table,uname='label_table1',scr_xsize=45 $
;    ,scr_ysize=23,xoffset=7,yoffset=33,value='Ŀ�ı�')
    table=['AGRO_METEO_DATA_DAY','AGRO_METEO_STATION_INFO','AGRO_METEO_SUNSHINE_TIME_TENDAY']
    drop_table=widget_combobox(WID_table,uname='drop_table',scr_ysize=23 $
    ,xoffset=7,yoffset=32,VALUE=table,Event_pro='selectdata')

    down_base=widget_base(main,uname='down_base',frame=1,xoffset=7,yoffset=75,scr_xsize=241,scr_ysize=155)

    label_file=widget_label(down_base,uname='lable_file',xoffset=7,yoffset=7,value='ѡ����Ҫ������ļ�')

    lable_sourcefile=widget_label(down_base,xoffset=7,yoffset=33,scr_xsize=100,ysize=20,value='Դ�ļ�(.csv)')

    CMD_open = Widget_Button(down_base,  $
      UNAME='CMD_open' ,XOFFSET=204 ,YOFFSET=27  $
      ,SCR_XSIZE=28 ,SCR_YSIZE=23  $
      ,EVENT_FUNC='' ,/ALIGN_CENTER  $
      ,value='image\open.bmp',/bitmap)

    TXT_sourcefile = Widget_Text(down_base,  $
      UNAME='TXT_sourcefile' ,XOFFSET=7 ,YOFFSET=60  $
      ,SCR_XSIZE=225 ,SCR_YSIZE=23 ,/EDITABLE ,XSIZE=20 ,YSIZE=1 $
      ,value='D:\test\test1.csv')

    label_tip=widget_label(down_base,$
    uname='label_tip',xoffset=50,yoffset=95,scr_xsize=168,scr_ysize=16,value='��ʾ������ǰ��鿴Ŀ�������ݣ�ʹ�䱣֤ �������ļ����������Ӧ������֤��¼Ψһ')

    label_tip2=widget_label(down_base,$
    uname='label_tip2',xoffset=50,yoffset=114,scr_xsize=168,scr_ysize=18,value='�ݣ�ʹ�䱣֤�������ļ�������')
    label_tip3=widget_label(down_base,$
    uname='label_tip3',xoffset=50,yoffset=133,scr_xsize=168,scr_ysize=18,value='���Ӧ������֤��¼Ψһ��')

    bottombase=widget_base(main,uname='bottombase',xoffset=7,yoffset=237,frame=1,scr_xsize=241,scr_ysize=40)
    dataimport=widget_button(bottombase,uname='dataimport',xoffset=7 $
    ,yoffset=7,scr_xsize=80,scr_ysize=23,value='����' $
    ,event_pro='importdata')
    cancel=widget_button(bottombase,uname='cancel',xoffset=152,yoffset=7,scr_xsize=80,scr_ysize=23,value='ȡ��')

    ;--------------�ұߵĽ���--------------------------
    leftbase= Widget_Base(TLB, UNAME='leftbase' ,FRAME=1 ,XOFFSET=270  $
      ,YOFFSET=7 ,SCR_XSIZE=416 ,SCR_YSIZE=286 ,TITLE='IDL' ,SPACE=7  $
      ,XPAD=7 ,YPAD=7)
    data_table = Widget_Table(leftbase, UNAME='data_table'  $
      ,XOFFSET=7 ,YOFFSET=7 ,SCR_XSIZE=400,SCR_YSIZE=270 $
      ,/FRAME,COLUMN_WIDTHS=45,COLUMN_LABELS='',ROW_LABELS='',DISJOINT_SELECTION=1 $
      ,/TRACKING_EVENTS,/RESIZEABLE_COLUMNS, /ALL_EVENTS,/editable)

    oD = obj_new('IDLdbDatabase')
    oD->SetProperty,/Use_Cursor_Lib
    oD->Connect,DATASOURCE='CROP_PROVINCE',USER_ID='crop',PASSWORD='crop'

    widget_control,tlb,/realize
	XMANAGER,'SJ_todatabase',tlb,/NO_BLOCK


end
