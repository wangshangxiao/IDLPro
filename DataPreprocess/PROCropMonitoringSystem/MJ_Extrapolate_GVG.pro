pro	fillthetable, PSTATE
	;;===========================================================================
	CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['出现以下错误：',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF
	;;===========================================================================

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProID=STRMID(PROVINCE_CODE,0,2)		;======加上不同省份的选择===

  	DBCO=DBobj

	;用于显示是否有有效数据的查询SQL
	 SQL='select NZ_CODE.NZ_Code,CROP_TYPES_PROPORTION_ORI.*'
	 SQL=SQL+' from NZ_CODE, CROP_TYPES_PROPORTION_ORI'
	 SQL=SQL+' where'
    SQL=SQL+' CROP_TYPES_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
    SQL=SQL+' AND CROP_TYPES_PROPORTION_ORI.County_code = NZ_CODE.County_code'
    SQL=SQL+' AND CROP_TYPES_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
    SQL=SQL+' AND CROP_TYPES_PROPORTION_ORI.NUM_OF_GVG >='+ STRTRIM((*PSTATE).GVG_NUM,2)
    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;不同省份的选择

  	;获取有效数据记录的个数
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	(*PSTATE).NUM=RecordNum

	Obj_Destroy,RecordNumOBJ

	IF (*PSTATE).NUM LE 0 then begin
        temp=dialog_message('未发现符合条件的数据',/information,title='提示')
        temp[*,*]=''
        (*PSTATE).NUM_SHOW=0
        widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
        return
    endif
;***************************************************************************
	;用于列表显示的查询SQL_SHOW;
	SQL='SELECT b.nz_code, b.County_name, b.County_Code,c.year, '
	SQL=SQL+' c.WINTER_WHEAT,c.SPRING_WHEAT,'
  	SQL=SQL+' c.EARLY_RICE,c.SEMILATE_RICE,c.LATE_RICE,'
	SQL=SQL+' c.SPRING_CORN,c.SUMMER_CORN,'
	SQL=SQL+' c.SOYBEAN,c.millet,'
    SQL=SQL+' c.broomcorn,c.potato,c.sweet_potato,c.peanut,c.cole,'
    SQL=SQL+' c.helianthus,c.cotton,c.hemp,c.sugar,'
	SQL=SQL+' c.tobacco,c.vegetable_fruit,c.other,c.NUM_OF_GVG,c.SU_OR_AU,'
	SQL=SQL+' b.PLOWLAND'	;获得县耕地面积
	SQL=SQL+' FROM (select NZ_CODE.*,PLOWLAND_AREA_COUNTY.PLOWLAND from NZ_CODE, PLOWLAND_AREA_COUNTY where PLOWLAND_AREA_COUNTY.COUNTY_CODE = NZ_CODE.County_code) b
	SQL=SQL+' LEFT JOIN (select CROP_TYPES_PROPORTION_ORI.* from CROP_TYPES_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
    SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋粮
    SQL=SQL+' AND NUM_OF_GVG >='+ STRTRIM((*PSTATE).GVG_NUM,2)
    SQL=SQL+' AND LEFT(CROP_TYPES_PROPORTION_ORI.County_code,2)='+ProID     ;不同省份的选择    20070406添加
	SQL_SHOW=SQL+') c ON b.County_code = c.County_code order by b.County_Code'   ;修改，20070823

;************************************************************************************
  	;获取数据长度
  	;获取表中数据记录的个数
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	(*PSTATE).NUM_SHOW=RecordNum

	Obj_Destroy,RecordNumOBJ

	IF (*PSTATE).NUM_SHOW LE 0 then begin
        temp=dialog_message('未发现符合条件的数据',/information,title='提示')
        temp[*,*]=''
        widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
        return
    endif

	;***************************************************************************

	arr_data_show=strarr(28,(*PSTATE).NUM_SHOW)
	ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
	count=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
		REPEAT BEGIN

			for i=0,27 do begin
				arr_data_show[i,count]=ORS -> GETFIELD(i)
			endfor

			COUNT=COUNT+1
		ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
	ENDIF

	Obj_Destroy,ORS

	;把未调查到的县显示为空
	for j=0,(*PSTATE).NUM_SHOW-1 do begin
		if arr_data_show[3,j] eq 0 then begin
			arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
			for m=4,26 do arr_data_show[m,j]=''
		endif
	endfor

	if ptr_valid((*PSTATE).Point_arr_show) eq 1 then begin
		ptr_free, (*PSTATE).Point_arr_show
	end
	(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
	(*PSTATE).Input_Flag=1	;表示已经进行数据导入
	(*PSTATE).Ext_Flag=0	;表示尚未进行外推

  	;(3)结果显示
  	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

  	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
end

pro MJ_Extrapolate_GVG_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
;;===========================================================================
	CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['出现以下错误：',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF
;;===========================================================================

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

ProID=STRMID(PROVINCE_CODE,0,2)		;======加上不同省份的选择===
;;;===========================================================================
case wTarget of
;;===========================================================================
	Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_year'):	BEGIN   ;选择外推的年份
		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0

		fillthetable, PSTATE
    END
;;;===========================================================================
    Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_GVG_NUM'):	BEGIN   ;GVG图像数量阀值
		(*PSTATE).GVG_NUM =(*PSTATE).ARR_GVG_NUM[event.index]
		(*PSTATE).Input_Flag=0

		fillthetable, PSTATE
    END
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='DROPLIST_SU_OR_AU'):begin
		(*PSTATE).SU_OR_AU =event.index           ;选择夏粮或秋粮
		(*PSTATE).Input_Flag=0

		fillthetable, PSTATE
    end
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='Droplist_Ext_Type'):begin
		(*PSTATE).Ext_Type =event.index           ;选择外推方式
    end
;===========================================================================
;+++++++++导入数据+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Widget_Info(wWidget,FIND_BY_UNAME='butt_ImportData'):	BEGIN          ;导入数据
      	fillthetable, PSTATE
    END
;;===========================================================================
;++++++外推+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Widget_Info(wWidget,FIND_BY_UNAME='butt_Ext'):	BEGIN     ;外推

		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('请先导入数据',/information,title='提示')
            return
        endif

		IF (*PSTATE).Input_Flag ne 1 then begin
            temp=dialog_message('请先导入数据',/information,title='提示')
            return
        endif

		arr_data_show=*((*PSTATE).Point_arr_show)

;----------------------------------------------------------------------------
		null_id = where(arr_data_show[4,*] eq '', null_count)

		while null_count gt 0 do begin
;----------按农业区划外推-------------------------------------------------------------
			if null_count gt 0 then begin
			    ;按农业区划外推
				NZ_code_col = arr_data_show[0,*]
				NZ_code = NZ_code_col[UNIQ(NZ_code_col, SORT(NZ_code_col))]
			    for i=0, n_elements(NZ_code)-1 do begin
			    	same_NZ_id = where(arr_data_show[0,*] eq NZ_code[i], count)
			    	same_NZ_full_id = where(arr_data_show[4,same_NZ_id] ne '', count_full, COMPLEMENT=same_NZ_null_id, NCOMPLEMENT=count_null)
			    	if count_null gt 0 and count_full gt 0 then begin
			    		sum_area = total(float((arr_data_show[27,same_NZ_id])[same_NZ_full_id]))
						for j=4, 24 do begin
			    			avg_ratio = total(float((arr_data_show[j,same_NZ_id])[same_NZ_full_id]) * float((arr_data_show[27,same_NZ_id])[same_NZ_full_id])) / sum_area
			    			arr_data_show[j,same_NZ_id[same_NZ_null_id]] = strtrim(avg_ratio, 2)
			    		endfor
			    	endif
			    endfor
			endif
			null_id = where(arr_data_show[4,*] eq '', null_count)
;-------按行政区划外推-----------------------------------------------------------------
			if null_count gt 0 then begin
			    ;按行政区划外推
				XZ_code_col = strmid(arr_data_show[2,*], 0, 4)
				XZ_code = XZ_code_col[UNIQ(XZ_code_col, SORT(XZ_code_col))]
			    for i=0, n_elements(XZ_code)-1 do begin
			    	same_XZ_id = where(strmid(arr_data_show[2,*], 0, 4) eq XZ_code[i], count)
			    	same_XZ_full_id = where(arr_data_show[4,same_XZ_id] ne '', count_full, COMPLEMENT=same_XZ_null_id, NCOMPLEMENT=count_null)
			    	if count_null gt 0 and count_full gt 0 then begin
			    		sum_area = total(float((arr_data_show[27,same_XZ_id])[same_XZ_full_id]))
						for j=4, 24 do begin
			    			avg_ratio = total(float((arr_data_show[j,same_XZ_id])[same_XZ_full_id]) * float((arr_data_show[27,same_XZ_id])[same_XZ_full_id])) / sum_area
			    			arr_data_show[j,same_XZ_id[same_XZ_null_id]] = strtrim(avg_ratio, 2)
			    		endfor
			    	endif
			    endfor
			endif
			null_id = where(arr_data_show[4,*] eq '', null_count)
		endwhile

;====================================================================================
		(*PSTATE).Ext_Flag = 1		;表示进行外推完成
		(*PSTATE).Input_Success_County ='No'
		widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show
		if ptr_valid((*PSTATE).Point_arr_show) eq 1 then begin
			ptr_free, (*PSTATE).Point_arr_show
		end
		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)		;
;----------------------------------------------------------------------------
	end     ;外推结束
;;*******************************************************************************************

Widget_Info(wWidget,FIND_BY_UNAME='Stat_To_Province'):	BEGIN     ;汇总到省

	IF (*PSTATE).Input_Flag ne 1 then begin
        temp=dialog_message('请先导入数据',/information,title='提示')
        return
    endif

	arr_data_show=*((*PSTATE).Point_arr_show)

	Num_Valid=0
	for i=0,(*PSTATE).NUM_SHOW-1 do begin
		if arr_data_show[4,i] ne '' then begin		;条件应判断每个格是否为空
			Num_Valid=Num_Valid+1
		endif
	endfor

	if Num_Valid le 0 then begin
		temp=dialog_message('空记录不能汇总，请先导入数据',/information,title='提示')
        return
	endif

	IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0表示尚未进行外推
	            (*PSTATE).Sta_Yes_ON=dialog_message('尚未进行外推，是否确定要汇总？', /QUESTION,title='提示')
	        endif

	if (*PSTATE).Sta_Yes_ON eq 'Yes' then begin   ;确定是否要汇总

		Valid_CountyCode=strarr(Num_Valid)

		sign=0
		for i=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[4,i] ne '' then begin
				Valid_CountyCode[sign]=arr_data_show[2,i]
				sign=sign+1
			endif
		endfor

		DBCO=DBobj
		;(2)读取有效数据
		SQL='select COUNTY_CODE,PLOWLAND'   ;获得县耕地面积
		SQL=SQL+' from  PLOWLAND_AREA_COUNTY'
		SQL=SQL+' where LEFT(COUNTY_CODE,2)='+ProID     ;不同省份的选择    20070406添加
		SQL=SQL+' order by COUNTY_CODE'

		;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		;获取有效数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		Area_NUM=RecordNum

		Obj_Destroy,RecordNumOBJ

		IF Area_NUM LE 0 then begin
		    temp=dialog_message('未找到县耕地面积数据',/information,title='提示')
		    return
		endif

		;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		Arr_Valid_County_Area=fltarr(2,Area_NUM)
		ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL)
		count=0
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			REPEAT BEGIN
				for i=0,1 do begin
					Arr_Valid_County_Area[i,count]=ORS -> GETFIELD(i)
				endfor
				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF

		Obj_Destroy,ORS

		Valid_County_Area=fltarr(Num_Valid)
		mark=0
;		record=0	;20070823添加
		for i=0,Num_Valid-1 do begin
			for j=mark,Area_NUM-1 do begin
				if Arr_Valid_County_Area[0,j] eq Valid_CountyCode[i] then begin
					Valid_County_Area[i]=Arr_Valid_County_Area[1,j]
					mark=j+1
;					record=record+1	;20070823添加
					break
				endif
			endfor
		endfor

;;;20070823添加****************************************
;		if record ne Num_Valid then begin
;			temp=dialog_message('县级面积数据不全，请检查',/information,title='提示')
;            return
;		endif
;***********************************************************

		County_Area_Weight=Valid_County_Area/total(Valid_County_Area)

		Data_To_Province=fltarr(27,Num_Valid)
		mark=0
		for i=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[4,i] ne '' then begin		;条件应判断每个格是否为空
				for j=4,24 do begin
					Data_To_Province[j,mark]=arr_data_show[j,i]
				endfor
				mark=mark+1
			endif
		endfor


		Province_Ext_Data=fltarr(27)
		for i=0,Num_Valid-1 do begin
	;		for j=4,24 do begin
				Province_Ext_Data_temp=Data_To_Province[*,i]*County_Area_Weight[i]
	;		endfor
			Province_Ext_Data=Province_Ext_Data+Province_Ext_Data_temp
		endfor

		;=================================================================================
		;根据公共变量PROVINCE_CODE获得省名
		SQL='Select Name from PROVINCE_CODE where Code='+"'"+PROVINCE_CODE+"'"

		oRS = OBJ_NEW('IDLdbRecordset', DBobj, SQL=SQL)
		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN
		    record = oRS->GetRecord()
		;    HELP, record, /STRUCTURE
		ENDIF
		Province_name=record.name

		Obj_Destroy,oRS
		;=================================================================================

		arr_data_show_province=strarr(25)
		arr_data_show_province[0]=STRTRIM(Province_name,2)
		arr_data_show_province[1]=PROVINCE_CODE
		arr_data_show_province[2]=STRTRIM((*PSTATE).SELYEAR,2)
		arr_data_show_province[24]=(*PSTATE).SU_OR_AU
		for i=3,23 do begin
			arr_data_show_province[i]=Province_Ext_Data[i+1]
		endfor

		(*PSTATE).NUM_SHOW=1
		if ptr_valid((*PSTATE).Point_arr_show) eq 1 then begin
			ptr_free, (*PSTATE).Point_arr_show
		end
		(*PSTATE).Point_arr_show=ptr_new(arr_data_show_province)

		(*PSTATE).Sta_Yes_ON = 'Yes'

		;(3)结果显示
	  	widget_control,(*PSTATE).TABLE_data,table_ysize=1,COLUMN_LABELS=(*PSTATE).labels_province
	;  					['Province_name','Province_code','year','WINTER_WHEAT',$
	;                      'SPRING_WHEAT','EARLY_RICE','SEMILATE_RICE',$
	;                      'LATE_RICE','SPRING_CORN','SUMMER_CORN',$
	;                      'SOYBEAN','millet','broomcorn',$
	;                      'potato','sweet_potato','peanut',$
	;                      'cole','helianthus','cotton',$
	;                      'hemp','sugar','tobacco',$
	;                      'vegetable_fruit','other',$
	;                      'SU_OR_AU'    ]    ;共26列

		(*PSTATE).Input_Success_Province ='No'
		(*PSTATE).Input_Flag=0	;表示尚未进行数据导入，汇总后要从新导入数据
;		(*PSTATE).Ext_Flag=0   ;表示尚未进行了外推
	  	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show_province

	endif

	(*PSTATE).Sta_Yes_ON='Yes'		;默认的'Sta_Yes_ON'是'Yes'

end


;++++++数据入库+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_data_to_DB'):begin     ;数据入库

		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('空记录不能入库，请先导入数据',/information,title='提示')
            return
        endif

		if (*PSTATE).NUM_SHOW eq 1 then begin		;汇总到省的数据入库
			widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

;			arr_data_show=*((*PSTATE).Point_arr_show)	;获取表中的数据
			year_input=arr_data_show[2,0]

			DBCO=DBobj

			;(A)将数据库中原有的数据进行删除

			;删除数据库中与要插入的记录年份相同的记录
			SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT_PROVINCE WHERE '
			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
			SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
;			PRINT,SQL
			DBCO->EXECUTESQL,SQL


;			(B)插入新的数据

			if (STRTRIM((arr_data_show[1]),2) eq '') or  $
				(STRTRIM((arr_data_show[2]),2) eq '') then begin
				temp=dialog_message('请检查，省代码或年份不能为空',/information,title='提示')
        		return
        	endif else begin
        		value2=STRTRIM((arr_data_show[1]),2)
				value3=STRTRIM((arr_data_show[2]),2)
			endelse

			if STRTRIM((arr_data_show[3]),2) eq '' then begin
				value4='NULL'
        	endif else begin
				value4=STRTRIM((arr_data_show[3]),2)
			endelse

			if STRTRIM((arr_data_show[4]),2) eq '' then begin
				value5='NULL'
        	endif else begin
        		value5=STRTRIM((arr_data_show[4]),2)
			endelse

			if STRTRIM((arr_data_show[5]),2) eq '' then begin
				value6='NULL'
        	endif else begin
        		value6=STRTRIM((arr_data_show[5]),2)
			endelse

			if STRTRIM((arr_data_show[6]),2) eq '' then begin
				value7='NULL'
        	endif else begin
        		value7=STRTRIM((arr_data_show[6]),2)
			endelse

			if STRTRIM((arr_data_show[7]),2) eq '' then begin
				value8='NULL'
        	endif else begin
        		value8=STRTRIM((arr_data_show[7]),2)
			endelse

			if STRTRIM((arr_data_show[8]),2) eq '' then begin
				value9='NULL'
        	endif else begin
        		value9=STRTRIM((arr_data_show[8]),2)
			endelse

			if STRTRIM((arr_data_show[9]),2) eq '' then begin
				value10='NULL'
        	endif else begin
        		value10=STRTRIM((arr_data_show[9]),2)
			endelse

			if STRTRIM((arr_data_show[10]),2) eq '' then begin
				value11='NULL'
        	endif else begin
        		value11=STRTRIM((arr_data_show[10]),2)
			endelse

			if STRTRIM((arr_data_show[11]),2) eq '' then begin
				value12='NULL'
        	endif else begin
        		value12=STRTRIM((arr_data_show[11]),2)
			endelse

			if STRTRIM((arr_data_show[12]),2) eq '' then begin
				value13='NULL'
        	endif else begin
        		value13=STRTRIM((arr_data_show[12]),2)
			endelse

			if STRTRIM((arr_data_show[13]),2) eq '' then begin
				value14='NULL'
        	endif else begin
        		value14=STRTRIM((arr_data_show[13]),2)
			endelse

			if STRTRIM((arr_data_show[14]),2) eq '' then begin
				value15='NULL'
        	endif else begin
        		value15=STRTRIM((arr_data_show[14]),2)
			endelse

			if STRTRIM((arr_data_show[15]),2) eq '' then begin
				value16='NULL'
        	endif else begin
        		value16=STRTRIM((arr_data_show[15]),2)
			endelse

			if STRTRIM((arr_data_show[16]),2) eq '' then begin
				value17='NULL'
        	endif else begin
        		value17=STRTRIM((arr_data_show[16]),2)
			endelse

			if STRTRIM((arr_data_show[17]),2) eq '' then begin
				value18='NULL'
        	endif else begin
        		value18=STRTRIM((arr_data_show[17]),2)
			endelse

			if STRTRIM((arr_data_show[18]),2) eq '' then begin
				value19='NULL'
        	endif else begin
        		value19=STRTRIM((arr_data_show[18]),2)
			endelse

			if STRTRIM((arr_data_show[19]),2) eq '' then begin
				value20='NULL'
        	endif else begin
        		value20=STRTRIM((arr_data_show[19]),2)
			endelse

			if STRTRIM((arr_data_show[20]),2) eq '' then begin
				value21='NULL'
        	endif else begin
        		value21=STRTRIM((arr_data_show[20]),2)
			endelse

			if STRTRIM((arr_data_show[21]),2) eq '' then begin
				value22='NULL'
        	endif else begin
        		value22=STRTRIM((arr_data_show[21]),2)
			endelse

			if STRTRIM((arr_data_show[22]),2) eq '' then begin
				value23='NULL'
        	endif else begin
        		value23=STRTRIM((arr_data_show[22]),2)
			endelse

			if STRTRIM((arr_data_show[23]),2) eq '' then begin
				value24='NULL'
        	endif else begin
        		value24=STRTRIM((arr_data_show[23]),2)
			endelse

			if STRTRIM((arr_data_show[24]),2) eq '' then begin
				value25='NULL'
        	endif else begin
        		value25=STRTRIM((arr_data_show[24]),2)
			endelse

;			if STRTRIM((arr_data_show[25]),2) eq '' then begin
;				value26=STRTRIM((*PSTATE).SU_OR_AU,2)
;        	endif else begin
;        		value26=STRTRIM((arr_data_show[25]),2)
;			endelse

			SQL='INSERT INTO CROP_TYPES_PROPORTION_EXT_PROVINCE '
			SQL=SQL+'(Province_code ,year ,WINTER_WHEAT ,SPRING_WHEAT,'
			SQL=SQL+'EARLY_RICE,SEMILATE_RICE,LATE_RICE,'
			SQL=SQL+'SPRING_CORN,SUMMER_CORN,SOYBEAN,'
			SQL=SQL+'millet,broomcorn,'
			SQL=SQL+'potato,sweet_potato,peanut,'
			SQL=SQL+'cole,helianthus,cotton,'
			SQL=SQL+'hemp,sugar,tobacco,'
			SQL=SQL+'vegetable_fruit,other,SU_OR_AU ) '
			SQL=SQL+'VALUES ('
			SQL=SQL+ value2			+','
			SQL=SQL+ value3			+','
			SQL=SQL+ value4			+','
			SQL=SQL+ value5			+','
			SQL=SQL+ value6			+','
			SQL=SQL+ value7			+','
			SQL=SQL+ value8			+','
			SQL=SQL+ value9			+','
			SQL=SQL+ value10		+','
			SQL=SQL+ value11		+','
			SQL=SQL+ value12		+','
			SQL=SQL+ value13		+','
			SQL=SQL+ value14		+','
			SQL=SQL+ value15		+','
			SQL=SQL+ value16		+','
			SQL=SQL+ value17		+','
			SQL=SQL+ value18		+','
			SQL=SQL+ value19		+','
			SQL=SQL+ value20		+','
			SQL=SQL+ value21		+','
			SQL=SQL+ value22		+','
			SQL=SQL+ value23		+','
			SQL=SQL+ value24		+','
			SQL=SQL+ value25		+')'

;				PRINT,SQL
			DBCO->EXECUTESQL,SQL

;				print,'总共入库记录数',jj
			(*PSTATE).Input_Success_Province ='Yes'		;判断外推的数据是否已经入库
			temp=dialog_message('省级外推结果入库成功',/information,title='提示')
			log, '种植结构-比例外推', 1
		endif

;=====================================================================================
;===============县级外推结果入库======================================================
		if (*PSTATE).NUM_SHOW GT 1 then begin		;县级外推结果入库
			IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0表示尚未进行外推
	            (*PSTATE).Input_Yes_ON=dialog_message('尚未进行外推，是否确定要入库？', /QUESTION,title='提示')
	        endif

			if (*PSTATE).Input_Yes_ON eq 'Yes' then begin   ;确定是否要入库

				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

	;			arr_data_show=*((*PSTATE).Point_arr_show)	;获取表中的数据
				year_input=arr_data_show[3,0]

				DBCO=DBobj

				;(A)将数据库中原有的数据进行删除

				;删除数据库中与要插入的记录年份相同的记录
				SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT WHERE '
				SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
				SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
				SQL=SQL+' AND LEFT(COUNTY_CODE,2)='+ProID     ;不同省份的选择    20070406添加

	;			PRINT,SQL
				DBCO->EXECUTESQL,SQL


	;			(B)插入新的数据
				jj=0

				FOR I=0,(*PSTATE).NUM_SHOW-1 DO BEGIN

					if (STRTRIM((arr_data_show[2,I]),2) eq '') or  $
						(STRTRIM((arr_data_show[3,I]),2) eq '') then begin
						temp=dialog_message('请检查，县代码或年份不能为空',/information,title='提示')
	            		return
	            	endif else begin
	            		value2=STRTRIM((arr_data_show[2,I]),2)
						value3=STRTRIM((arr_data_show[3,I]),2)
					endelse

					if STRTRIM((arr_data_show[4,I]),2) eq '' then begin
						value4='NULL'
	            	endif else begin
						value4=STRTRIM((arr_data_show[4,I]),2)
					endelse

					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
						value5='NULL'
	            	endif else begin
	            		value5=STRTRIM((arr_data_show[5,I]),2)
					endelse

					if STRTRIM((arr_data_show[6,I]),2) eq '' then begin
						value6='NULL'
	            	endif else begin
	            		value6=STRTRIM((arr_data_show[6,I]),2)
					endelse

					if STRTRIM((arr_data_show[7,I]),2) eq '' then begin
						value7='NULL'
	            	endif else begin
	            		value7=STRTRIM((arr_data_show[7,I]),2)
					endelse

					if STRTRIM((arr_data_show[8,I]),2) eq '' then begin
						value8='NULL'
	            	endif else begin
	            		value8=STRTRIM((arr_data_show[8,I]),2)
					endelse

					if STRTRIM((arr_data_show[9,I]),2) eq '' then begin
						value9='NULL'
	            	endif else begin
	            		value9=STRTRIM((arr_data_show[9,I]),2)
					endelse

					if STRTRIM((arr_data_show[10,I]),2) eq '' then begin
						value10='NULL'
	            	endif else begin
	            		value10=STRTRIM((arr_data_show[10,I]),2)
					endelse

					if STRTRIM((arr_data_show[11,I]),2) eq '' then begin
						value11='NULL'
	            	endif else begin
	            		value11=STRTRIM((arr_data_show[11,I]),2)
					endelse

					if STRTRIM((arr_data_show[12,I]),2) eq '' then begin
						value12='NULL'
	            	endif else begin
	            		value12=STRTRIM((arr_data_show[12,I]),2)
					endelse

					if STRTRIM((arr_data_show[13,I]),2) eq '' then begin
						value13='NULL'
	            	endif else begin
	            		value13=STRTRIM((arr_data_show[13,I]),2)
					endelse

					if STRTRIM((arr_data_show[14,I]),2) eq '' then begin
						value14='NULL'
	            	endif else begin
	            		value14=STRTRIM((arr_data_show[14,I]),2)
					endelse

					if STRTRIM((arr_data_show[15,I]),2) eq '' then begin
						value15='NULL'
	            	endif else begin
	            		value15=STRTRIM((arr_data_show[15,I]),2)
					endelse

					if STRTRIM((arr_data_show[16,I]),2) eq '' then begin
						value16='NULL'
	            	endif else begin
	            		value16=STRTRIM((arr_data_show[16,I]),2)
					endelse

					if STRTRIM((arr_data_show[17,I]),2) eq '' then begin
						value17='NULL'
	            	endif else begin
	            		value17=STRTRIM((arr_data_show[17,I]),2)
					endelse

					if STRTRIM((arr_data_show[18,I]),2) eq '' then begin
						value18='NULL'
	            	endif else begin
	            		value18=STRTRIM((arr_data_show[18,I]),2)
					endelse

					if STRTRIM((arr_data_show[19,I]),2) eq '' then begin
						value19='NULL'
	            	endif else begin
	            		value19=STRTRIM((arr_data_show[19,I]),2)
					endelse

					if STRTRIM((arr_data_show[20,I]),2) eq '' then begin
						value20='NULL'
	            	endif else begin
	            		value20=STRTRIM((arr_data_show[20,I]),2)
					endelse

					if STRTRIM((arr_data_show[21,I]),2) eq '' then begin
						value21='NULL'
	            	endif else begin
	            		value21=STRTRIM((arr_data_show[21,I]),2)
					endelse

					if STRTRIM((arr_data_show[22,I]),2) eq '' then begin
						value22='NULL'
	            	endif else begin
	            		value22=STRTRIM((arr_data_show[22,I]),2)
					endelse

					if STRTRIM((arr_data_show[23,I]),2) eq '' then begin
						value23='NULL'
	            	endif else begin
	            		value23=STRTRIM((arr_data_show[23,I]),2)
					endelse

					if STRTRIM((arr_data_show[24,I]),2) eq '' then begin
						value24='NULL'
	            	endif else begin
	            		value24=STRTRIM((arr_data_show[24,I]),2)
					endelse

					if STRTRIM((arr_data_show[25,I]),2) eq '' then begin
						value25='NULL'
	            	endif else begin
	            		value25=STRTRIM((arr_data_show[25,I]),2)
					endelse

					if STRTRIM((arr_data_show[26,I]),2) eq '' then begin
						value26=STRTRIM((*PSTATE).SU_OR_AU,2)
	            	endif else begin
	            		value26=STRTRIM((arr_data_show[26,I]),2)
					endelse

					SQL='INSERT INTO CROP_TYPES_PROPORTION_EXT '
					SQL=SQL+'(County_code ,year ,WINTER_WHEAT ,SPRING_WHEAT,'
					SQL=SQL+'EARLY_RICE,SEMILATE_RICE,LATE_RICE,'
					SQL=SQL+'SPRING_CORN,SUMMER_CORN,SOYBEAN,'
					SQL=SQL+'millet,broomcorn,'
					SQL=SQL+'potato,sweet_potato,peanut,'
					SQL=SQL+'cole,helianthus,cotton,'
					SQL=SQL+'hemp,sugar,tobacco,'
					SQL=SQL+'vegetable_fruit,other,NUM_OF_GVG,SU_OR_AU ) '
					SQL=SQL+'VALUES ('
					SQL=SQL+ value2			+','
					SQL=SQL+ value3			+','
					SQL=SQL+ value4			+','
					SQL=SQL+ value5			+','
					SQL=SQL+ value6			+','
					SQL=SQL+ value7			+','
					SQL=SQL+ value8			+','
					SQL=SQL+ value9			+','
					SQL=SQL+ value10		+','
					SQL=SQL+ value11		+','
					SQL=SQL+ value12		+','
					SQL=SQL+ value13		+','
					SQL=SQL+ value14		+','
					SQL=SQL+ value15		+','
					SQL=SQL+ value16		+','
					SQL=SQL+ value17		+','
					SQL=SQL+ value18		+','
					SQL=SQL+ value19		+','
					SQL=SQL+ value20		+','
					SQL=SQL+ value21		+','
					SQL=SQL+ value22		+','
					SQL=SQL+ value23		+','
					SQL=SQL+ value24		+','
					SQL=SQL+ value25		+','
					SQL=SQL+ value26		+')'

	;				PRINT,SQL
					DBCO->EXECUTESQL,SQL

					jj=jj+1

				ENDFOR
				print,'总共入库记录数',jj
				(*PSTATE).Input_Success_County ='Yes'		;判断外推的数据是否已经入库
				temp=dialog_message('县级外推结果入库成功',/information,title='提示')
				log, '种植结构-比例外推', 1
			endif
		endif

		(*PSTATE).Input_Yes_ON = 'Yes'		;默认的'Input_Yes_ON'是'Yes'，直接输入
;		(*PSTATE).Input_Success_County ='Yes'		;判断外推的数据是否已经入库
	end


;;*******************************************************************************************
;;*******************************************************************************************

;;===========================================================================
    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;关闭
		common_log,'关闭GVG外推'
		if (*PSTATE).NUM_SHOW lt 1 then begin
			widget_control,event.top,/destroy
			return
		endif

		if (*PSTATE).NUM_SHOW eq 1 then begin		;省级数据

			Input_Province_Decide='No'

			IF (*PSTATE).Input_Success_Province ne 'Yes' then begin	;表示外推的数据尚未入库
		        Input_Province_Decide=dialog_message('省级外推结果尚未入库，是否要入库？',/QUESTION,/CANCEL,title='提示')
		        log, '种植结构-比例外推', 0
		    endif

			case Input_Province_Decide of

			'No': begin
				widget_control,event.top,/destroy
				end

			'Cancel': begin
				return
				end

			'Yes': begin
				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

	;			arr_data_show=*((*PSTATE).Point_arr_show)	;获取表中的数据
				year_input=arr_data_show[2,0]

				DBCO=DBobj

				;(A)将数据库中原有的数据进行删除

				;删除数据库中与要插入的记录年份相同的记录
				SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT_PROVINCE WHERE '
				SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
				SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
				SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
	;			PRINT,SQL
				DBCO->EXECUTESQL,SQL


	;			(B)插入新的数据

				if (STRTRIM((arr_data_show[1]),2) eq '') or  $
					(STRTRIM((arr_data_show[2]),2) eq '') then begin
					temp=dialog_message('请检查，省代码或年份不能为空',/information,title='提示')
	        		return
	        	endif else begin
	        		value2=STRTRIM((arr_data_show[1]),2)
					value3=STRTRIM((arr_data_show[2]),2)
				endelse

				if STRTRIM((arr_data_show[3]),2) eq '' then begin
					value4='NULL'
	        	endif else begin
					value4=STRTRIM((arr_data_show[3]),2)
				endelse

				if STRTRIM((arr_data_show[4]),2) eq '' then begin
					value5='NULL'
	        	endif else begin
	        		value5=STRTRIM((arr_data_show[4]),2)
				endelse

				if STRTRIM((arr_data_show[5]),2) eq '' then begin
					value6='NULL'
	        	endif else begin
	        		value6=STRTRIM((arr_data_show[5]),2)
				endelse

				if STRTRIM((arr_data_show[6]),2) eq '' then begin
					value7='NULL'
	        	endif else begin
	        		value7=STRTRIM((arr_data_show[6]),2)
				endelse

				if STRTRIM((arr_data_show[7]),2) eq '' then begin
					value8='NULL'
	        	endif else begin
	        		value8=STRTRIM((arr_data_show[7]),2)
				endelse

				if STRTRIM((arr_data_show[8]),2) eq '' then begin
					value9='NULL'
	        	endif else begin
	        		value9=STRTRIM((arr_data_show[8]),2)
				endelse

				if STRTRIM((arr_data_show[9]),2) eq '' then begin
					value10='NULL'
	        	endif else begin
	        		value10=STRTRIM((arr_data_show[9]),2)
				endelse

				if STRTRIM((arr_data_show[10]),2) eq '' then begin
					value11='NULL'
	        	endif else begin
	        		value11=STRTRIM((arr_data_show[10]),2)
				endelse

				if STRTRIM((arr_data_show[11]),2) eq '' then begin
					value12='NULL'
	        	endif else begin
	        		value12=STRTRIM((arr_data_show[11]),2)
				endelse

				if STRTRIM((arr_data_show[12]),2) eq '' then begin
					value13='NULL'
	        	endif else begin
	        		value13=STRTRIM((arr_data_show[12]),2)
				endelse

				if STRTRIM((arr_data_show[13]),2) eq '' then begin
					value14='NULL'
	        	endif else begin
	        		value14=STRTRIM((arr_data_show[13]),2)
				endelse

				if STRTRIM((arr_data_show[14]),2) eq '' then begin
					value15='NULL'
	        	endif else begin
	        		value15=STRTRIM((arr_data_show[14]),2)
				endelse

				if STRTRIM((arr_data_show[15]),2) eq '' then begin
					value16='NULL'
	        	endif else begin
	        		value16=STRTRIM((arr_data_show[15]),2)
				endelse

				if STRTRIM((arr_data_show[16]),2) eq '' then begin
					value17='NULL'
	        	endif else begin
	        		value17=STRTRIM((arr_data_show[16]),2)
				endelse

				if STRTRIM((arr_data_show[17]),2) eq '' then begin
					value18='NULL'
	        	endif else begin
	        		value18=STRTRIM((arr_data_show[17]),2)
				endelse

				if STRTRIM((arr_data_show[18]),2) eq '' then begin
					value19='NULL'
	        	endif else begin
	        		value19=STRTRIM((arr_data_show[18]),2)
				endelse

				if STRTRIM((arr_data_show[19]),2) eq '' then begin
					value20='NULL'
	        	endif else begin
	        		value20=STRTRIM((arr_data_show[19]),2)
				endelse

				if STRTRIM((arr_data_show[20]),2) eq '' then begin
					value21='NULL'
	        	endif else begin
	        		value21=STRTRIM((arr_data_show[20]),2)
				endelse

				if STRTRIM((arr_data_show[21]),2) eq '' then begin
					value22='NULL'
	        	endif else begin
	        		value22=STRTRIM((arr_data_show[21]),2)
				endelse

				if STRTRIM((arr_data_show[22]),2) eq '' then begin
					value23='NULL'
	        	endif else begin
	        		value23=STRTRIM((arr_data_show[22]),2)
				endelse

				if STRTRIM((arr_data_show[23]),2) eq '' then begin
					value24='NULL'
	        	endif else begin
	        		value24=STRTRIM((arr_data_show[23]),2)
				endelse

				if STRTRIM((arr_data_show[24]),2) eq '' then begin
					value25='NULL'
	        	endif else begin
	        		value25=STRTRIM((arr_data_show[24]),2)
				endelse

	;			if STRTRIM((arr_data_show[25]),2) eq '' then begin
	;				value26=STRTRIM((*PSTATE).SU_OR_AU,2)
	;        	endif else begin
	;        		value26=STRTRIM((arr_data_show[25]),2)
	;			endelse

				SQL='INSERT INTO CROP_TYPES_PROPORTION_EXT_PROVINCE '
				SQL=SQL+'(Province_code ,year ,WINTER_WHEAT ,SPRING_WHEAT,'
				SQL=SQL+'EARLY_RICE,SEMILATE_RICE,LATE_RICE,'
				SQL=SQL+'SPRING_CORN,SUMMER_CORN,SOYBEAN,'
				SQL=SQL+'millet,broomcorn,'
				SQL=SQL+'potato,sweet_potato,peanut,'
				SQL=SQL+'cole,helianthus,cotton,'
				SQL=SQL+'hemp,sugar,tobacco,'
				SQL=SQL+'vegetable_fruit,other,SU_OR_AU ) '
				SQL=SQL+'VALUES ('
				SQL=SQL+ value2			+','
				SQL=SQL+ value3			+','
				SQL=SQL+ value4			+','
				SQL=SQL+ value5			+','
				SQL=SQL+ value6			+','
				SQL=SQL+ value7			+','
				SQL=SQL+ value8			+','
				SQL=SQL+ value9			+','
				SQL=SQL+ value10		+','
				SQL=SQL+ value11		+','
				SQL=SQL+ value12		+','
				SQL=SQL+ value13		+','
				SQL=SQL+ value14		+','
				SQL=SQL+ value15		+','
				SQL=SQL+ value16		+','
				SQL=SQL+ value17		+','
				SQL=SQL+ value18		+','
				SQL=SQL+ value19		+','
				SQL=SQL+ value20		+','
				SQL=SQL+ value21		+','
				SQL=SQL+ value22		+','
				SQL=SQL+ value23		+','
				SQL=SQL+ value24		+','
				SQL=SQL+ value25		+')'

	;				PRINT,SQL
				DBCO->EXECUTESQL,SQL

	;				print,'总共入库记录数',jj
				(*PSTATE).Input_Success_Province ='Yes'		;判断外推的数据是否已经入库
				temp=dialog_message('省级外推结果入库成功',/information,title='提示')
				log, '种植结构-比例外推', 1
				end
			else:
			endcase
			return
		endif

;========================================================================================
		if (*PSTATE).NUM_SHOW gt 1 then begin		;县级数据

			Input_County_Decide='No'

			IF ((*PSTATE).Ext_Flag eq 1) && ((*PSTATE).Input_Success_County ne 'Yes') then begin	;表示外推的数据尚未入库
		        Input_County_Decide=dialog_message('外推到县级的数据结果尚未入库，是否要入库？',/QUESTION,/CANCEL,title='提示')
		        log, '种植结构-比例外推', 0
		    endif

			case Input_County_Decide of

			'No': begin
				widget_control,event.top,/destroy
				end

			'Cancel': begin
				return
				end

			'Yes': begin
				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

		;			arr_data_show=*((*PSTATE).Point_arr_show)	;获取表中的数据
				year_input=arr_data_show[3,0]

				DBCO=DBobj

				;(A)将数据库中原有的数据进行删除

				;删除数据库中与要插入的记录年份相同的记录
				SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT WHERE '
				SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
				SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
				SQL=SQL+' AND LEFT(COUNTY_CODE,2)='+ProID     ;不同省份的选择    20070406添加

		;			PRINT,SQL
				DBCO->EXECUTESQL,SQL

		;			(B)插入新的数据
				jj=0

				FOR I=0,(*PSTATE).NUM_SHOW-1 DO BEGIN

					if (STRTRIM((arr_data_show[2,I]),2) eq '') or  $
						(STRTRIM((arr_data_show[3,I]),2) eq '') then begin
						temp=dialog_message('请检查，县代码或年份不能为空',/information,title='提示')
		        		return
		        	endif else begin
		        		value2=STRTRIM((arr_data_show[2,I]),2)
						value3=STRTRIM((arr_data_show[3,I]),2)
					endelse

					if STRTRIM((arr_data_show[4,I]),2) eq '' then begin
						value4='NULL'
		        	endif else begin
						value4=STRTRIM((arr_data_show[4,I]),2)
					endelse

					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
						value5='NULL'
		        	endif else begin
		        		value5=STRTRIM((arr_data_show[5,I]),2)
					endelse

					if STRTRIM((arr_data_show[6,I]),2) eq '' then begin
						value6='NULL'
		        	endif else begin
		        		value6=STRTRIM((arr_data_show[6,I]),2)
					endelse

					if STRTRIM((arr_data_show[7,I]),2) eq '' then begin
						value7='NULL'
		        	endif else begin
		        		value7=STRTRIM((arr_data_show[7,I]),2)
					endelse

					if STRTRIM((arr_data_show[8,I]),2) eq '' then begin
						value8='NULL'
		        	endif else begin
		        		value8=STRTRIM((arr_data_show[8,I]),2)
					endelse

					if STRTRIM((arr_data_show[9,I]),2) eq '' then begin
						value9='NULL'
		        	endif else begin
		        		value9=STRTRIM((arr_data_show[9,I]),2)
					endelse

					if STRTRIM((arr_data_show[10,I]),2) eq '' then begin
						value10='NULL'
		        	endif else begin
		        		value10=STRTRIM((arr_data_show[10,I]),2)
					endelse

					if STRTRIM((arr_data_show[11,I]),2) eq '' then begin
						value11='NULL'
		        	endif else begin
		        		value11=STRTRIM((arr_data_show[11,I]),2)
					endelse

					if STRTRIM((arr_data_show[12,I]),2) eq '' then begin
						value12='NULL'
		        	endif else begin
		        		value12=STRTRIM((arr_data_show[12,I]),2)
					endelse

					if STRTRIM((arr_data_show[13,I]),2) eq '' then begin
						value13='NULL'
		        	endif else begin
		        		value13=STRTRIM((arr_data_show[13,I]),2)
					endelse

					if STRTRIM((arr_data_show[14,I]),2) eq '' then begin
						value14='NULL'
		        	endif else begin
		        		value14=STRTRIM((arr_data_show[14,I]),2)
					endelse

					if STRTRIM((arr_data_show[15,I]),2) eq '' then begin
						value15='NULL'
		        	endif else begin
		        		value15=STRTRIM((arr_data_show[15,I]),2)
					endelse

					if STRTRIM((arr_data_show[16,I]),2) eq '' then begin
						value16='NULL'
		        	endif else begin
		        		value16=STRTRIM((arr_data_show[16,I]),2)
					endelse

					if STRTRIM((arr_data_show[17,I]),2) eq '' then begin
						value17='NULL'
		        	endif else begin
		        		value17=STRTRIM((arr_data_show[17,I]),2)
					endelse

					if STRTRIM((arr_data_show[18,I]),2) eq '' then begin
						value18='NULL'
		        	endif else begin
		        		value18=STRTRIM((arr_data_show[18,I]),2)
					endelse

					if STRTRIM((arr_data_show[19,I]),2) eq '' then begin
						value19='NULL'
		        	endif else begin
		        		value19=STRTRIM((arr_data_show[19,I]),2)
					endelse

					if STRTRIM((arr_data_show[20,I]),2) eq '' then begin
						value20='NULL'
		        	endif else begin
		        		value20=STRTRIM((arr_data_show[20,I]),2)
					endelse

					if STRTRIM((arr_data_show[21,I]),2) eq '' then begin
						value21='NULL'
		        	endif else begin
		        		value21=STRTRIM((arr_data_show[21,I]),2)
					endelse

					if STRTRIM((arr_data_show[22,I]),2) eq '' then begin
						value22='NULL'
		        	endif else begin
		        		value22=STRTRIM((arr_data_show[22,I]),2)
					endelse

					if STRTRIM((arr_data_show[23,I]),2) eq '' then begin
						value23='NULL'
		        	endif else begin
		        		value23=STRTRIM((arr_data_show[23,I]),2)
					endelse

					if STRTRIM((arr_data_show[24,I]),2) eq '' then begin
						value24='NULL'
		        	endif else begin
		        		value24=STRTRIM((arr_data_show[24,I]),2)
					endelse

					if STRTRIM((arr_data_show[25,I]),2) eq '' then begin
						value25='NULL'
		        	endif else begin
		        		value25=STRTRIM((arr_data_show[25,I]),2)
					endelse

					if STRTRIM((arr_data_show[26,I]),2) eq '' then begin
						value26=STRTRIM((*PSTATE).SU_OR_AU,2)
		        	endif else begin
		        		value26=STRTRIM((arr_data_show[26,I]),2)
					endelse

					SQL='INSERT INTO CROP_TYPES_PROPORTION_EXT '
					SQL=SQL+'(County_code ,year ,WINTER_WHEAT ,SPRING_WHEAT,'
					SQL=SQL+'EARLY_RICE,SEMILATE_RICE,LATE_RICE,'
					SQL=SQL+'SPRING_CORN,SUMMER_CORN,SOYBEAN,'
					SQL=SQL+'millet,broomcorn,'
					SQL=SQL+'potato,sweet_potato,peanut,'
					SQL=SQL+'cole,helianthus,cotton,'
					SQL=SQL+'hemp,sugar,tobacco,'
					SQL=SQL+'vegetable_fruit,other,NUM_OF_GVG,SU_OR_AU ) '
					SQL=SQL+'VALUES ('
					SQL=SQL+ value2			+','
					SQL=SQL+ value3			+','
					SQL=SQL+ value4			+','
					SQL=SQL+ value5			+','
					SQL=SQL+ value6			+','
					SQL=SQL+ value7			+','
					SQL=SQL+ value8			+','
					SQL=SQL+ value9			+','
					SQL=SQL+ value10		+','
					SQL=SQL+ value11		+','
					SQL=SQL+ value12		+','
					SQL=SQL+ value13		+','
					SQL=SQL+ value14		+','
					SQL=SQL+ value15		+','
					SQL=SQL+ value16		+','
					SQL=SQL+ value17		+','
					SQL=SQL+ value18		+','
					SQL=SQL+ value19		+','
					SQL=SQL+ value20		+','
					SQL=SQL+ value21		+','
					SQL=SQL+ value22		+','
					SQL=SQL+ value23		+','
					SQL=SQL+ value24		+','
					SQL=SQL+ value25		+','
					SQL=SQL+ value26		+')'

		;				PRINT,SQL
					DBCO->EXECUTESQL,SQL

					jj=jj+1

				ENDFOR
				print,'总共入库记录数',jj
				temp=dialog_message('县级外推结果入库成功',/information,title='提示')
				log, '种植结构-比例外推', 1

				(*PSTATE).Input_Success_County ='Yes'		;判断外推的数据是否已经入库

				end
			else:
			endcase
			return
		endif
  end 		;关闭结束
;;*******************************************************************************************
;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;帮助

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '成数外推', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('找不到帮助文档',title='警告')
		endelse
	end
;;============================================================================
    else:
  endcase

end			; event 的结束

;;*******************************************************************************************
;;*******************************************************************************************


pro MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'启动GVG外推'
IF ( XREGISTERED('MJ_Extrapolate_GVG') NE 0 ) THEN RETURN

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=100 ,YOFFSET=200 ,SCR_XSIZE=796 ,SCR_YSIZE=475,TLB_FRAME_ATTR =1  $
      ,TITLE='分类成数外推' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'   $
      ,XOFFSET=6 ,YOFFSET=55 ,SCR_XSIZE=790 ,SCR_YSIZE=340  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  ROW_LABEL=strtrim(indgen(16),2)
;  labels_county=['NZ_Code','County_name','County_code','year','WINTER_WHEAT',$
;                      'SPRING_WHEAT','EARLY_RICE','SEMILATE_RICE',$
;                      'LATE_RICE','SPRING_CORN','SUMMER_CORN',$
;                      'SOYBEAN','millet','broomcorn',$
;                      'potato','sweet_potato','peanut',$
;                      'cole','helianthus','cotton',$
;                      'hemp','sugar','tobacco',$
;                      'vegetable_fruit','other','NUM_OF_GVG',$
;                      'SU_OR_AU'    ]    ;共27列
  labels_county=['农业区划代码','县名','县代码','年份','冬小麦',$
                      '春小麦','早稻','中稻',$
                      '晚稻','春玉米','夏玉米',$
                      '大豆','谷子','高粱',$
                      '马铃薯','红薯','花生',$
                      '油菜','向日葵','棉花',$
                      '麻类','糖料','烟叶',$
                      '蔬菜瓜果','其它作物','GVG图象数量',$
                      '生长季'    ]    ;共27列
;  labels_province=['Province_name','Province_code','year','WINTER_WHEAT',$
;                      'SPRING_WHEAT','EARLY_RICE','SEMILATE_RICE',$
;                      'LATE_RICE','SPRING_CORN','SUMMER_CORN',$
;                      'SOYBEAN','millet','broomcorn',$
;                      'potato','sweet_potato','peanut',$
;                      'cole','helianthus','cotton',$
;                      'hemp','sugar','tobacco',$
;                      'vegetable_fruit','other',$
;                      'SU_OR_AU'    ]    ;共25列
  labels_province=['省名','省代码','年份','冬小麦',$
                      '春小麦','早稻','中稻',$
                      '晚稻','春玉米','夏玉米',$
                      '大豆','谷子','高粱',$
                      '马铃薯','红薯','花生',$
                      '油菜','向日葵','棉花',$
                      '麻类','糖料','烟叶',$
                      '蔬菜瓜果','其它作物',$
                      '生长季'    ]    ;共25列
  TABLE_data = Widget_Table(WID_BASE_1, UNAME='TABLE_data' ,FRAME=1  $
      ,XOFFSET=1 ,YOFFSET=4 ,SCR_XSIZE=776 ,SCR_YSIZE=327,YSIZE=16 ,XSIZE=27  $
      ,/RESIZEABLE_COLUMNS,ALIGNMENT=0,ROW_LABELS=ROW_LABEL  $
      ,/align_center,y_scroll_size=16   $
      ,column_labels=labels_county)
	WIDGET_CONTROL,TABLE_data,SET_TABLE_SELECT=[-1,-1,-1,-1]   ;让界面表格初始时无阴影


  WID_BASE_3 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=402 ,SCR_XSIZE=776 ,SCR_YSIZE=34  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_BASE_4 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=7 ,SCR_XSIZE=776 ,SCR_YSIZE=36 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)

;Value_Ext_Type=['按农业区划','按行政区划']
;Droplist_Ext_Type=widget_droplist(WID_BASE_4,/no_copy,$
;                  uname='Droplist_Ext_Type' ,$
;                  value=Value_Ext_Type,title='外推方式',$
;                  scr_xsize=150,Xoffset=366,YOFFSET=10)


  WID_LABEL_0 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_0'  $
      ,XOFFSET=540 ,YOFFSET=15 ,SCR_XSIZE=100 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='GVG图像数量阈值：')

	ARR_GVG_NUM=['0','20','50','100','200','500','1000','2000']
	DROPLIST_GVG_NUM = Widget_DROPLIST(WID_BASE_4, UNAME='DROPLIST_GVG_NUM'  $
      ,XOFFSET=650 ,YOFFSET=10 ,SCR_XSIZE=52 ,SCR_YSIZE=20 ,XSIZE=20  $
      ,YSIZE=1,value=ARR_GVG_NUM)
    WIDGET_CONTROL,DROPLIST_GVG_NUM,SET_DROPLIST_SELECT=2
    GVG_NUM=50

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;  TEXT_GVG_NUM = Widget_Text(WID_BASE_4, UNAME='TEXT_GVG_NUM'  $
;      ,XOFFSET=655 ,YOFFSET=10 ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,XSIZE=20  $
;      ,YSIZE=1,/editable,value='50',/ALL_EVENTS)
;	widget_control,TEXT_GVG_NUM, GET_VALUE=GVG_NUM

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


WID_BUTTON_6 = Widget_Button(WID_BASE_3, UNAME='butt_Ext'  $
      ,XOFFSET=168 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='外推')

	Stat_To_Province = Widget_Button(WID_BASE_3, UNAME='Stat_To_Province'  $
      ,XOFFSET=288 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='汇总到省')


  WID_BUTTON_7 = Widget_Button(WID_BASE_3, UNAME='butt_Close'  $
      ,XOFFSET=688 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='关闭')


  WID_BUTTON_8 = Widget_Button(WID_BASE_3, UNAME='butt_Help'  $
      ,XOFFSET=568 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='帮助')


  WID_BUTTON_9 = Widget_Button(WID_BASE_3, UNAME='butt_ImportData'  $
      ,XOFFSET=28 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='导入数据')


  WID_BUTTON_10 = Widget_Button(WID_BASE_3, UNAME='butt_data_to_DB'  $
      ,XOFFSET=428 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='数据入库')

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  WID_LABEL_1 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_1'  $
      ,XOFFSET=75,YOFFSET=15 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='年份：')


  ARRYEAR=STRTRIM(INDGEN(36)+1980,2)
  DROPLIST_year = Widget_combobox(WID_BASE_4, UNAME='DROPLIST_year'  $
      ,XOFFSET=125 ,YOFFSET=10 ,SCR_XSIZE=50 ,SCR_YSIZE=23 ,VALUE=ARRYEAR)
  temp=(bin_date())[0]-1980+1
  widget_control,DROPLIST_year, SET_COMBOBOX_SELECT=temp-1


  ARR_SU_OR_AU=['夏粮(0)','秋粮(1)']
  DROPLIST_SU_OR_AU = Widget_Droplist(WID_BASE_4, UNAME='DROPLIST_SU_OR_AU'  $
      ,XOFFSET=345 ,YOFFSET=10 ,SCR_XSIZE=68 ,SCR_YSIZE=27 ,VALUE=ARR_SU_OR_AU)

	temp=(bin_date())[1]
	if temp ge 3 and temp le 8 then begin
		SU_OR_AU=0		;0表示为夏粮
	endif else begin
		SU_OR_AU=1		;1表示为秋粮
	endelse
   WIDGET_CONTROL,DROPLIST_SU_OR_AU,SET_DROPLIST_SELECT=SU_OR_AU


  WID_LABEL_2 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_2'  $
      ,XOFFSET=290 ,YOFFSET=15 ,SCR_YSIZE=11  $
      ,/ALIGN_LEFT ,VALUE='生长季：')

;TEMP=STRARR(27,16)
  Point_arr_show=ptr_new()

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_7,/INPUT_FOCUS


	STATE = { $
  		TABLE_data	:	TABLE_data , $
  		Point_arr_show	:	Point_arr_show, $	;用于获取界面表格中的数据
  		ARRYEAR		:   ARRYEAR ,$
  		NUM			:	0,  $       ;导入有效的GVG记录数目
  		NUM_SHOW	:	0,  $		;在界面表中显示的记录数目
  		Ext_Type	:	0,  $
  		Ext_Flag	:	0,  $
  		Input_Flag	:	0,  $		;标记是否已经导入数据
  		SU_OR_AU	:	SU_OR_AU,  $
  		ARR_GVG_NUM	:	ARR_GVG_NUM  ,$		;选择GVG图象阀值
  		GVG_NUM		:	GVG_NUM,  $
  		Sta_Yes_ON	:	'Yes'	,  $	;判断是否要汇总到省
  		Input_Yes_ON	:	'Yes'	,  $	;判断是否要入库
  		Input_Success_County	:	'Yes'	,  $	;标记外推到县级的数据是否已经入库
  		Input_Success_Province	:	'Yes'	,  $	;标记外汇总到省的数据是否已经入库
  		labels_county	:	labels_county   ,  $
  		labels_province	:	labels_province	,  $
  		SELYEAR		:	(bin_date())[0]   $
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=PSTATE

  XManager, 'MJ_Extrapolate_GVG', WID_BASE_0, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'

end

