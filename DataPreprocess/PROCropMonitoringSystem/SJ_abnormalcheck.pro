;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;曹文静写
; Generated on:	08/29/2006 08:13.23
;
;-------------------------------------
;------------------关闭界面时释放指针-----------------------------

pro SJ_abnormaldatacheck_CleanAllHeap,WID_BASE_1
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2
    WIDGET_CONTROL,WID_BASE_1,GET_UVALUE=PA
    HEAP_FREE,PA
end



;-----------------------------------------------------------------
pro SJ_abnormalcheck_close,event                ; 关闭界面
	common_log,'关闭异常检测'
	 CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy

end
;-------------------------------------
pro SJ_abnormalcheck_selectcombo,event          ; 改变下拉框触发的事件

    widget_control, event.top, get_uvalue = pstate
    combo_tablename = widget_info((*pstate).combo_tablename, /COMBOBOX_GETTEXT)

    CASE combo_tablename OF
		'气象数据':  begin
		 drop_tablename='METEO_DATA_DAY'
		 widget_control,(*pstate).WID_combo_month,SENSITIVE=1
		 widget_control,(*pstate).WID_LABEL_month,SENSITIVE=1
		 (*pstate).tablename='METEO_DATA_DAY'
		 temp1=['站点号','年','月','日','旬','最高气温',$
                 '平均气温','最低气温','降水量','空气相对温度']
         widtharray=[50,50,25,25,20,60,60,60,60,80]
		 			 end
		'面积数据':  begin
		 drop_tablename='CROP_AREA_COUNTY'
		 widget_control,(*pstate).WID_combo_month,SENSITIVE=0
		 widget_control,(*pstate).WID_LABEL_month,SENSITIVE=0
		 (*pstate).tablename='CROP_AREA_COUNTY'
		temp1=['县代码','数据年','冬小麦作物面积','春小麦作物面积',$
		'早稻作物面积','中稻作物面积','晚稻作物面积','春玉米面积','夏玉米面积','大豆作物面积']
		widtharray=[50,50,80,80,80,80,80,80,80,80]
		             end
		'产量数据':  begin
		 drop_tablename='CROP_PRODUCTION_county'
		 widget_control,(*pstate).WID_combo_month,SENSITIVE=0
		 widget_control,(*pstate).WID_LABEL_month,SENSITIVE=0
		 (*pstate).tablename='CROP_PRODUCTION_county'
		 temp1=['县代码','数据年','作物代码','产量']
		 widtharray=[50,50,80,80]
		             end
    else:
	ENDCASE

    SQL='SELECT top 9 * FROM '+ drop_tablename +''
	datatable = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,N_columns = m)



    datatable=0B
    temporarys=strarr(9,m)
    Widget_Control, (*pstate).data_table,TABLE_YSIZE=9,TABLE_XSIZE=m $
         ,SET_VALUE=temporarys[*,*] $
         ,COLUMN_LABELS = temp1 $
         ,COLUMN_WIDTH = widtharray $
         ,ALIGNMENT=0

end
;-------------------------------------
pro SJ_abnormalcheck_event, Event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of

    else:
  endcase

end
;-------------数据检测----------------------------
pro SJ_abnormal_datacheck,event

    widget_control,/hourglass
    widget_control, event.top, get_uvalue = pstate
    tablename=(*pstate).tablename                                                    ;得到现在的表名
    ;----------------------首先判断数据库是否有所选时间的数据-------------------
    CASE tablename OF
	  	 'METEO_DATA_DAY':  begin
         progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='数据异常检测') ;新建进度条对象
	     progressTimer->START
	     progressTimer->UPDATE, (0.3 * 100.0)  ;更新进度条
	  	 month = widget_info((*pstate).WID_combo_month, /COMBOBOX_GETTEXT)       ;得到月份
         year = widget_info((*pstate).WID_combo_year, /COMBOBOX_GETTEXT)        ;得到年份
	  	 SQL='select * from METEO_DATA_DAY where year='+ STRTRIM(year,2) +' and month='+ STRTRIM(month,2) +''
		 datatable = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,Column_Name=temp1,N_columns = m)
		 temp1=['站点号','年','月','日','旬','最高气温',$
                 '平均气温','最低气温','降水量','空气相对温度']

    IF TOTALS EQ 0 THEN BEGIN
	     TEXT=DIALOG_MESSAGE('数据库中暂时没有您要的数据。',TITLE='提示')
	     OBJ_DESTROY,progressTimer;销毁进度条
	     RETURN
	ENDIF
;	     print,'数据连接正常'
	     progressTimer->UPDATE, (0.4 * 100.0)  ;更新进度条
         abnormal_datatable =STRARR(m,1)
    total=0
     tmp_max=400
     tmp_min=-400
     ppt_max=500
     ppt_min=0
     hu_ra_max=100
     hu_ra_min=0
     progressTimer->UPDATE, (0.45 * 100.0)  ;更新进度条
     for i=0,totals-1 do begin
			 CASE 1 OF
             (float(datatable[5,i]) gt tmp_max) or (float(datatable[5,i]) lt tmp_min) :begin
                abnormal_datatable=[[abnormal_datatable],[datatable[*,i]]]
                total=total+1
             end

             (float(datatable[6,i]) gt tmp_max) or (float(datatable[6,i]) lt tmp_min):begin
                abnormal_datatable=[[abnormal_datatable],[datatable[*,i]]]
                total=total+1
             end

             (float(datatable[7,i]) gt tmp_max) or (float(datatable[7,i]) lt tmp_min):begin
                abnormal_datatable=[[abnormal_datatable],[datatable[*,i]]]
                total=total+1
             end

             (float(datatable[8,i]) gt ppt_max) or (float(datatable[8,i]) lt ppt_min) : begin
				abnormal_datatable=[[abnormal_datatable],[datatable[*,i]]]
                total=total+1
             end

             (float(datatable[9,i]) gt hu_ra_max) or (float(datatable[9,i]) lt hu_ra_min): begin
				 abnormal_datatable=[[abnormal_datatable],[datatable[*,i]]]
                total=total+1
             end
			ELSE:
			ENDCASE

     endfor
   	    progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条
	     IF TOTAL EQ 0 THEN BEGIN
	         OBJ_DESTROY,progressTimer;销毁进度条
		     TEXT=DIALOG_MESSAGE('数据库中暂时没有异常的数据。',TITLE='提示')
		     RETURN
		 ENDIF

       	abnormal_datatable=abnormal_datatable[*,1:*]
     	ptr_free,(*pstate).abnormaldata

      	(*pstate).abnormaldata=ptr_new(abnormal_datatable)

	end
		'CROP_AREA_COUNTY':  begin                            ;面积数据

    progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='数据异常检测') ;新建进度条对象
	progressTimer->START
	progressTimer->UPDATE, (0.3 * 100.0)  ;更新进度条
         year = widget_info((*pstate).WID_combo_year, /COMBOBOX_GETTEXT)        ;得到年份
	     SQL='select * from CROP_AREA_COUNTY where year='+ STRTRIM(year,2) +''
	;-----------------------------------------------------------------
	;首先计算数据的总数
		datatable = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,Column_Name=temp1,N_columns = m)
		temp1=['县代码','数据年','冬小麦作物面积','春小麦作物面积','早稻作物面积',$
		'中稻作物面积','晚稻作物面积','春玉米面积','夏玉米面积','大豆作物面积']
		progressTimer->UPDATE, (0.4 * 100.0)  ;更新进度条
    IF TOTALS EQ 0 THEN BEGIN
         OBJ_DESTROY,progressTimer;销毁进度条
	     TEXT=DIALOG_MESSAGE('数据库中暂时没有您要的数据。',TITLE='提示')
	     RETURN
	ENDIF
  	progressTimer->UPDATE, (0.5 * 100.0)  ;更新进度条


        nianji=0.5
		niannei=0.5
        sql5='SELECT CROP_AREA_COUNTY.COUNTY_CODE,CROP_AREA_COUNTY.YEAR,CROP_AREA_COUNTY.WINTER_WHEAT,CROP_AREA_COUNTY.SPRING_WHEAT,' $
           +' CROP_AREA_COUNTY.EARLY_RICE,CROP_AREA_COUNTY.SEMILATE_RICE,CROP_AREA_COUNTY.LATE_RICE,CROP_AREA_COUNTY.SPRING_CORN,' $
           +' CROP_AREA_COUNTY.SUMMER_CORN,CROP_AREA_COUNTY.SOYBEAN FROM CROP_AREA_COUNTY, [select county_code,avg(semilate_rice) as avgrice,avg(SPRING_CORN)' $
           +' as avgcorn,avg(SUMMER_CORN) as avgsummercorn,avg(soybean) as avgsoybean,avg(semilate_rice+SPRING_CORN+SUMMER_CORN+soybean) as county_zongarea from' $
           +' CROP_AREA_COUNTY where semilate_rice <> 0 and SPRING_CORN <> 0 and summer_corn <> 0 and soybean <> 0 group by county_code]. AS avg_table WHERE' $
           +' CROP_AREA_COUNTY.COUNTY_CODE=avg_table.county_code' $
           +' and (abs(semilate_rice/avg_table.avgrice-1) >'+strtrim(nianji,2)+'' $
           +' or abs(SPRING_CORN/avg_table.avgcorn-1)>'+strtrim(nianji,2)+' or abs(summer_CORN/avg_table.avgsummercorn-1)>'+strtrim(nianji,2)+' or abs(soybean/avg_table.avgsoybean-1)>'+strtrim(nianji,2)+'' $
           +' or abs((semilate_rice+SPRING_CORN+summer_corn+soybean)/avg_table.county_zongarea-1)>'+strtrim(niannei,2)+') and year='+strtrim(year,2) +''
      	abnormal_datatable = NQ_SJ_GetdataFromDB_Str(SQL5,N_RECORDS = TOTAL,Column_Name=temp1,N_columns = m)
	    progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条
    IF TOTAL EQ 0 THEN BEGIN
;         progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
         OBJ_DESTROY,progressTimer ;销毁进度条
	     TEXT=DIALOG_MESSAGE('数据库中暂时没有异常的数据。',TITLE='提示')
	     RETURN
	ENDIF
     ptr_free,(*pstate).abnormaldata
     (*pstate).abnormaldata=ptr_new(abnormal_datatable)

	 end
	 ;-----------------------------------产量表-----------------------------
		'CROP_PRODUCTION_county':  begin
        progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='数据检测') ;新建进度条对象
    	progressTimer->START
	    progressTimer->UPDATE, (0.3 * 100.0)  ;更新进度条
         year = widget_info((*pstate).WID_combo_year, /COMBOBOX_GETTEXT)        ;得到年份
	     SQL='select * from CROP_PRODUCTION_county where year='+ STRTRIM(year,2) +''
	;首先计算数据的总数
		data = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,Column_Name=temp1,N_columns = m)
		 temp1=['县代码','数据年','作物代码','产量']
			    progressTimer->UPDATE, (0.4 * 100.0)  ;更新进度条
        IF TOTALS EQ 0 THEN BEGIN
  	       OBJ_DESTROY,progressTimer;销毁进度条
	       TEXT=DIALOG_MESSAGE('数据库中暂时没有您要的数据。',TITLE='提示')
	       RETURN
	    ENDIF
	    data=0B

	    progressTimer->UPDATE, (0.5 * 100.0)  ;更新进度条
     	nianji2=0.2
		niannei2=0.08

       sql2='SELECT county_code, year,crop_code, production FROM' $
       +' [select ABS(a.production/avg_production-1) as AbsTest,a.county_code,a.crop_code,a.year,a.production from' $
       +' crop_production_county a,(select avg(production) AS avg_production,county_code,crop_code' $
       +'  from crop_production_county group by county_code,crop_code) b where a.county_code=b.county_code' $
       +' and a.crop_code=b.crop_code]. AS mm WHERE AbsTest>'+strtrim(nianji2,2)+' and year='+strtrim(year,2)+'' $
       +' union' $
       +' select crop_production_county.county_code,crop_production_county.year,crop_production_county.crop_code,' $
       +' crop_production_county.production from crop_production_county,(SELECT a1.county_code as code2, a1.year, a1.pro' $
       +' FROM (select county_code,year,sum(production) as pro from crop_production_county group by county_code,year) as a1,' $
       +' (select county_code,avg(pro2) as pro1 from (select county_code,year,sum(production)' $
       +' as pro2 from crop_production_county group by county_code,year) group by county_code) as a2' $'
       +' WHERE (abs(a1.pro/a2.pro1-1)>'+strtrim(niannei2,2)+') and a1.county_code=a2.county_code and a1.year='+strtrim(year,2)+')' $
       +'  as table5 where crop_production_county.county_code=table5.code2 and crop_production_county.year='+strtrim(year,2)+''
;       sql2='SELECT county_code, year,crop_code, production FROM' $
;       +' [select ABS(a.production/avg_production-1) as AbsTest,a.county_code,a.crop_code,a.year,a.production from' $
;       +' crop_production_county a,(select avg(production) AS avg_production,county_code,crop_code' $
;       +'  from crop_production_county group by county_code,crop_code) b where a.county_code=b.county_code' $
;       +' and a.crop_code=b.crop_code]. AS mm WHERE AbsTest>'+strtrim(nianji2,2)+' and year='+strtrim(2004,2)+'' $
;       +' union' $
;       +' select crop_production_county.county_code,crop_production_county.year,crop_production_county.crop_code,' $
;       +' crop_production_county.production from crop_production_county,(SELECT a1.county_code as code2, a1.year, a1.pro' $
;       +' FROM (select county_code,year,sum(production) as pro from crop_production_county group by county_code,year) as a1,' $
;       +' (select county_code,avg(pro2) as pro1 from (select county_code,year,sum(production)' $
;       +' as pro2 from crop_production_county group by county_code,year) group by county_code) as a2' $
;       +' WHERE (abs(a1.pro/a2.pro1-1)>'+strtrim(niannei2,2)+') and a1.county_code=a2.county_code and a1.year=2004)' $
;       +'  as table5 where crop_production_county.county_code=table5.code2 and crop_production_county.year=2004'


		abnormal_datatable = NQ_SJ_GetdataFromDB_Str(sql2,N_RECORDS = TOTAL,Column_Name=temp1,N_columns = m)

	    progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条
    IF   TOTAL EQ 0 THEN BEGIN
         OBJ_DESTROY,progressTimer ;销毁进度条
	     TEXT=DIALOG_MESSAGE('数据库中暂时没有异常的数据。',TITLE='提示')
	     RETURN
	ENDIF
         ptr_free,(*pstate).abnormaldata
         (*pstate).abnormaldata=ptr_new(abnormal_datatable)
	end
    else:
	ENDCASE
    ;----------------------如果有，判断是否为异常数据，如果是，将异常数据显示在widget_table表中----
       Widget_Control, (*pstate).data_table,TABLE_YSIZE=TOTAL,TABLE_XSIZE=m $
         ,COLUMN_LABELS = temp1 $
         ,SET_VALUE=abnormal_datatable[*,*] $
         ,ALIGNMENT=0
      (*pstate).table_column=m
      (*pstate).table_row=total
      (*pstate).Sum=total
	progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
	OBJ_DESTROY,progressTimer ;销毁进度条

    infor=DIALOG_MESSAGE('完成检测，异常数据已经显示在表中!',TITLE='提示')
end
;-------------数据标识----------------------------
pro SJ_abnormalcheck_mark,event                      ;主要用于把异常数据入库
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
    widget_control,/hourglass
    widget_control, event.top, get_uvalue = pstate

      validpointer=PTR_VALID((*pstate).abnormaldata)
      if validpointer eq 0 then begin
	        infor=DIALOG_MESSAGE('你必须首先检测到异常值，才能将其入库!',TITLE='提示')
	        return
      endif
	  b=fltarr(1,1)

	  if (*(*pstate).abnormaldata) eq b then begin
	        infor=DIALOG_MESSAGE('你必须首先检测到异常值，才能将其入库!',TITLE='提示')
	        return
	  endif
	  if (*pstate).Sum eq 0 then begin
	        infor=DIALOG_MESSAGE('没有异常数据将不入库!',TITLE='提示')
	        return
	  endif
      warning=DIALOG_MESSAGE('你确认要将找到的异常记录行入库进行标识吗？',title='警告', /QUESTION)



     if warning eq 'Yes' then begin
		     progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='异常数据标定') ;新建进度条对象
			 progressTimer->START
			 progressTimer->UPDATE, (0.2 * 100.0)  ;更新进度条

		     drop_tablename=(*pstate).tablename
		     table_row=(*pstate).table_row
		     case drop_tablename of
		        'METEO_DATA_DAY':begin
;		        oD=(*pstate).oD
			 	data_mark=(*(*pstate).abnormaldata)             ;data_mark:需要标识的数据
			 	progressTimer->UPDATE, (0.6 * 100.0)  ;更新进度条
						         for i=0,table_row-1 do begin
							      		 CATCH, Error_status               ;截取错误.
							    		 IF Error_status NE 0 THEN BEGIN
							       			 CATCH, /CANCEL
							        		 Goto,nexts
							             ENDIF
							        	 SQL2="INSERT INTO meteo_abnormaldata VALUES ('"+data_mark[0,I]+"','"+data_mark[1,i]+"','" $
							        	 +data_mark[2,i]+"','"+data_mark[3,i]+"','"+data_mark[4,i]+"','"+data_mark[5,i]+"','"+data_mark[6,i]+"','"+data_mark[7,i]+"','"+data_mark[8,i]+"','"+data_mark[9,i]+"')"
							             DBobj->ExecuteSQL,SQL2
							             nexts:
					             endfor
				progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
				OBJ_DESTROY,progressTimer ;销毁进度条
				infor=DIALOG_MESSAGE('完成保存!',TITLE='提示' )
              end
;-----------------面积数据------------------------------------------------------------------
              'CROP_AREA_COUNTY':begin
;		        oD=(*pstate).oD
			 	data_mark=(*(*pstate).abnormaldata)             ;data_mark:需要标识的数据
			 	progressTimer->UPDATE, (0.6 * 100.0)  ;更新进度条
						         for i=0,table_row-1 do begin
							      		 CATCH, Error_status               ;截取错误.
							    		 IF Error_status NE 0 THEN BEGIN
							       			 CATCH, /CANCEL
							        		 Goto,nextss
							             ENDIF
							        	 SQL2="INSERT INTO CROP_ABNORMALAREA_COUNTY VALUES ('"+data_mark[0,I]+"','"+data_mark[1,i]+"','" $
							        	 +data_mark[2,i]+"','"+data_mark[3,i]+"','"+data_mark[4,i]+"','"+data_mark[5,i]+"','"+data_mark[6,i]+"','"+data_mark[7,i]+"','"+data_mark[8,i]+"')"
							            DBobj->ExecuteSQL,SQL2
							             nextss:
					             endfor
				progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
				OBJ_DESTROY,progressTimer ;销毁进度条
				infor=DIALOG_MESSAGE('完成保存!',TITLE='提示' )

                 end
;---------------产量数据----------------------------------------------------------------------
              'CROP_PRODUCTION_county':begin
;		        oD=(*pstate).oD
			 	data_mark=(*(*pstate).abnormaldata)             ;data_mark:需要标识的数据
			 	progressTimer->UPDATE, (0.6 * 100.0)  ;更新进度条
						         for i=0,table_row-1 do begin
							      		 CATCH, Error_status               ;截取错误.
							    		 IF Error_status NE 0 THEN BEGIN
							       			 CATCH, /CANCEL
							        		 Goto,nextsy
							             ENDIF
							        	 SQL2="INSERT INTO crop_abnormalproduct_county VALUES ('"+data_mark[0,I]+"','"+data_mark[1,i]+"','" $
							        	 +data_mark[2,i]+"','"+data_mark[3,i]+"')"
							             DBobj->ExecuteSQL,SQL2
							             nextsy:
					             endfor
					progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
			    	OBJ_DESTROY,progressTimer ;销毁进度条
					infor=DIALOG_MESSAGE('完成保存!',TITLE='提示' )
                 end
            else:
            endcase

     endif else begin
     endelse

end
;-------------界面帮助----------------------------
pro SJ_abnormalcheck_help,event
	 PRINT,'数据异常检测,帮助'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '数据检测', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('找不到帮助文档',title='警告')
	endelse

;	 ONLINE_HELP,  BOOK='HELP\HELP.chm', '数据检测'
	 ;temp=dialog_message('系统暂没有帮助')
end
;--------------主界面-----------------------------
pro SJ_abnormalcheck, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'启动异常检测'

;  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
  IF ( XREGISTERED('SJ_abnormalcheck') NE 0 ) THEN RETURN
;  WID_BASE_0 = Widget_Base(GROUP_LEADER=wGroup, UNAME='WID_BASE_0'  $
;      ,XOFFSET=350 ,YOFFSET=150 ,SCR_XSIZE=368 ,SCR_YSIZE=398  $
;      ,TITLE='数据检测' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1)
COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
	WID_BASE_1 = Widget_Base(GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_1'  $
      ,XOFFSET=250 ,YOFFSET=200 ,SCR_XSIZE=600 $
      ,TITLE='数据检测' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1,/column)


  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=7 ,SCR_XSIZE=580 ,SCR_YSIZE=35 ,TITLE='IDL'  $
      ,SPACE=5 ,XPAD=5 ,YPAD=3, /row)

	WID_LABEL_0 = Widget_Label(WID_BASE_2, UNAME='WID_LABEL_0'  $
	  ,/ALIGN_LEFT ,VALUE='    数据类型:')

  combo_tablename = Widget_combobox(WID_BASE_2, UNAME='combo_tablename',  $
      value=['气象数据','面积数据','产量数据'],$
      event_pro='SJ_abnormalcheck_selectcombo')

  WID_LABEL_2 = Widget_Label(WID_BASE_2, UNAME='WID_LABEL_2'  $
      ,/ALIGN_LEFT ,VALUE='             年:')

;  list_year=['1980','1981','1982','1983','1984','1985','1986',$
;  '1987','1988','1989','1990','1991','1992','1993','1994',$
;  '1995','1996','1997','1998','1999','2000','2001','2002',$
;  '2003','2004','2005','2006','2007','2008','2009','2010',$
;  '2011','2012','2013','2014','2015']
  time=bin_date()
  year_num = time[0]-1980+1
  list_year=strtrim(string(indgen(year_num)+1980),2)

  WID_combo_year = Widget_combobox(WID_BASE_2, UNAME='WID_combo_year'  $
      ,value=list_year)
	;==杨绍锷修改，20070906=====================================================
  temp=(bin_date())[0]-1980
  widget_control,WID_combo_year, SET_COMBOBOX_SELECT=temp
  ;===========================================================

  WID_LABEL_month = Widget_Label(WID_BASE_2, UNAME='WID_LABEL_month'  $
      ,/ALIGN_LEFT ,VALUE='月:')

  list_month=['1','2','3','4','5','6','7','8','9','10','11','12']

  WID_combo_month = Widget_combobox(WID_BASE_2, UNAME='WID_combo_month'  $
      ,value=list_month)

  WID_BASE_6 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_6' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=89 ,SCR_XSIZE=580 ,SCR_YSIZE=210  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  columnlabel=['站点号','年','月','日','旬','最高气温',$
  '平均气温','最低气温','降水量','空气相对温度']
  widtharray=[50,50,25,25,20,60,60,60,60,80]
  data_table = Widget_Table(WID_BASE_6, UNAME='data_table'  $
      ,XOFFSET=0 ,YOFFSET=5 ,SCR_XSIZE=580 ,SCR_YSIZE=199 ,XSIZE=10  $
      ,YSIZE=9,COLUMN_LABELS=columnlabel,COLUMN_WIDTHS=widtharray)
;  oD=DBobj

	WID_BASE_4 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=306 ,SCR_XSIZE=580 ,SCR_YSIZE=39  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BUTTON_0 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_0'  $
      ,XOFFSET=107 ,YOFFSET=7 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='检测',event_pro='SJ_abnormal_datacheck')


  WID_BUTTON_1 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_1'  $
      ,XOFFSET=192 ,YOFFSET=7 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='标识',event_pro='SJ_abnormalcheck_mark')


  WID_BUTTON_2 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_2'  $
      ,XOFFSET=278 ,YOFFSET=7 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='帮助',event_pro='SJ_abnormalcheck_help')


  WID_BUTTON_3 = Widget_Button(WID_BASE_4, UNAME='WID_BUTTON_3'  $
      ,XOFFSET=363 ,YOFFSET=7 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='关闭',event_pro='SJ_abnormalcheck_close')


  Widget_Control, /REALIZE, WID_BASE_1
  WIDGET_CONTROL,WID_BUTTON_3,/INPUT_FOCUS

  state = { $
           ;   oD:oD,$
              data_table:data_table,$
              table_column:6,$
              table_row:9,$
              Sum:0,$
              WID_LABEL_month:WID_LABEL_month,$
              WID_combo_month:WID_combo_month,$
              WID_combo_year:WID_combo_year,$
              tablename:'METEO_DATA_DAY',$
              abnormaldata:ptr_new(),$
              combo_tablename:combo_tablename $              ;table的列数，可以动态发生改变
                  }
  	pstate = PTR_NEW(state, /no_copy)
	widget_control, WID_BASE_1, set_uvalue=pstate
    XManager, 'SJ_abnormalcheck', WID_BASE_1, CLEANUP='SJ_abnormaldatacheck_CleanAllHeap',/NO_BLOCK
end


