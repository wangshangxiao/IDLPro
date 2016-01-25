pro	fillthetable, PSTATE
	;;===========================================================================
	CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['�������´���',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF
	;;===========================================================================

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProID=STRMID(PROVINCE_CODE,0,2)		;======���ϲ�ͬʡ�ݵ�ѡ��===

  	DBCO=DBobj

	;������ʾ�Ƿ�����Ч���ݵĲ�ѯSQL
	 SQL='select NZ_CODE.NZ_Code,CROP_TYPES_PROPORTION_ORI.*'
	 SQL=SQL+' from NZ_CODE, CROP_TYPES_PROPORTION_ORI'
	 SQL=SQL+' where'
    SQL=SQL+' CROP_TYPES_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
    SQL=SQL+' AND CROP_TYPES_PROPORTION_ORI.County_code = NZ_CODE.County_code'
    SQL=SQL+' AND CROP_TYPES_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
    SQL=SQL+' AND CROP_TYPES_PROPORTION_ORI.NUM_OF_GVG >='+ STRTRIM((*PSTATE).GVG_NUM,2)
    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��

  	;��ȡ��Ч���ݼ�¼�ĸ���
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	(*PSTATE).NUM=RecordNum

	Obj_Destroy,RecordNumOBJ

	IF (*PSTATE).NUM LE 0 then begin
        temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
        temp[*,*]=''
        (*PSTATE).NUM_SHOW=0
        widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
        return
    endif
;***************************************************************************
	;�����б���ʾ�Ĳ�ѯSQL_SHOW;
	SQL='SELECT b.nz_code, b.County_name, b.County_Code,c.year, '
	SQL=SQL+' c.WINTER_WHEAT,c.SPRING_WHEAT,'
  	SQL=SQL+' c.EARLY_RICE,c.SEMILATE_RICE,c.LATE_RICE,'
	SQL=SQL+' c.SPRING_CORN,c.SUMMER_CORN,'
	SQL=SQL+' c.SOYBEAN,c.millet,'
    SQL=SQL+' c.broomcorn,c.potato,c.sweet_potato,c.peanut,c.cole,'
    SQL=SQL+' c.helianthus,c.cotton,c.hemp,c.sugar,'
	SQL=SQL+' c.tobacco,c.vegetable_fruit,c.other,c.NUM_OF_GVG,c.SU_OR_AU,'
	SQL=SQL+' b.PLOWLAND'	;����ظ������
	SQL=SQL+' FROM (select NZ_CODE.*,PLOWLAND_AREA_COUNTY.PLOWLAND from NZ_CODE, PLOWLAND_AREA_COUNTY where PLOWLAND_AREA_COUNTY.COUNTY_CODE = NZ_CODE.County_code) b
	SQL=SQL+' LEFT JOIN (select CROP_TYPES_PROPORTION_ORI.* from CROP_TYPES_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
    SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
    SQL=SQL+' AND NUM_OF_GVG >='+ STRTRIM((*PSTATE).GVG_NUM,2)
    SQL=SQL+' AND LEFT(CROP_TYPES_PROPORTION_ORI.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
	SQL_SHOW=SQL+') c ON b.County_code = c.County_code order by b.County_Code'   ;�޸ģ�20070823

;************************************************************************************
  	;��ȡ���ݳ���
  	;��ȡ�������ݼ�¼�ĸ���
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	(*PSTATE).NUM_SHOW=RecordNum

	Obj_Destroy,RecordNumOBJ

	IF (*PSTATE).NUM_SHOW LE 0 then begin
        temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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

	;��δ���鵽������ʾΪ��
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
	(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
	(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

  	;(3)�����ʾ
  	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

  	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
end

pro MJ_Extrapolate_GVG_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
;;===========================================================================
	CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['�������´���',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF
;;===========================================================================

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

ProID=STRMID(PROVINCE_CODE,0,2)		;======���ϲ�ͬʡ�ݵ�ѡ��===
;;;===========================================================================
case wTarget of
;;===========================================================================
	Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_year'):	BEGIN   ;ѡ�����Ƶ����
		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0

		fillthetable, PSTATE
    END
;;;===========================================================================
    Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_GVG_NUM'):	BEGIN   ;GVGͼ��������ֵ
		(*PSTATE).GVG_NUM =(*PSTATE).ARR_GVG_NUM[event.index]
		(*PSTATE).Input_Flag=0

		fillthetable, PSTATE
    END
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='DROPLIST_SU_OR_AU'):begin
		(*PSTATE).SU_OR_AU =event.index           ;ѡ������������
		(*PSTATE).Input_Flag=0

		fillthetable, PSTATE
    end
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='Droplist_Ext_Type'):begin
		(*PSTATE).Ext_Type =event.index           ;ѡ�����Ʒ�ʽ
    end
;===========================================================================
;+++++++++��������+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Widget_Info(wWidget,FIND_BY_UNAME='butt_ImportData'):	BEGIN          ;��������
      	fillthetable, PSTATE
    END
;;===========================================================================
;++++++����+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Widget_Info(wWidget,FIND_BY_UNAME='butt_Ext'):	BEGIN     ;����

		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('���ȵ�������',/information,title='��ʾ')
            return
        endif

		IF (*PSTATE).Input_Flag ne 1 then begin
            temp=dialog_message('���ȵ�������',/information,title='��ʾ')
            return
        endif

		arr_data_show=*((*PSTATE).Point_arr_show)

;----------------------------------------------------------------------------
		null_id = where(arr_data_show[4,*] eq '', null_count)

		while null_count gt 0 do begin
;----------��ũҵ��������-------------------------------------------------------------
			if null_count gt 0 then begin
			    ;��ũҵ��������
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
;-------��������������-----------------------------------------------------------------
			if null_count gt 0 then begin
			    ;��������������
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
		(*PSTATE).Ext_Flag = 1		;��ʾ�����������
		(*PSTATE).Input_Success_County ='No'
		widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show
		if ptr_valid((*PSTATE).Point_arr_show) eq 1 then begin
			ptr_free, (*PSTATE).Point_arr_show
		end
		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)		;
;----------------------------------------------------------------------------
	end     ;���ƽ���
;;*******************************************************************************************

Widget_Info(wWidget,FIND_BY_UNAME='Stat_To_Province'):	BEGIN     ;���ܵ�ʡ

	IF (*PSTATE).Input_Flag ne 1 then begin
        temp=dialog_message('���ȵ�������',/information,title='��ʾ')
        return
    endif

	arr_data_show=*((*PSTATE).Point_arr_show)

	Num_Valid=0
	for i=0,(*PSTATE).NUM_SHOW-1 do begin
		if arr_data_show[4,i] ne '' then begin		;����Ӧ�ж�ÿ�����Ƿ�Ϊ��
			Num_Valid=Num_Valid+1
		endif
	endfor

	if Num_Valid le 0 then begin
		temp=dialog_message('�ռ�¼���ܻ��ܣ����ȵ�������',/information,title='��ʾ')
        return
	endif

	IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0��ʾ��δ��������
	            (*PSTATE).Sta_Yes_ON=dialog_message('��δ�������ƣ��Ƿ�ȷ��Ҫ���ܣ�', /QUESTION,title='��ʾ')
	        endif

	if (*PSTATE).Sta_Yes_ON eq 'Yes' then begin   ;ȷ���Ƿ�Ҫ����

		Valid_CountyCode=strarr(Num_Valid)

		sign=0
		for i=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[4,i] ne '' then begin
				Valid_CountyCode[sign]=arr_data_show[2,i]
				sign=sign+1
			endif
		endfor

		DBCO=DBobj
		;(2)��ȡ��Ч����
		SQL='select COUNTY_CODE,PLOWLAND'   ;����ظ������
		SQL=SQL+' from  PLOWLAND_AREA_COUNTY'
		SQL=SQL+' where LEFT(COUNTY_CODE,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
		SQL=SQL+' order by COUNTY_CODE'

		;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		;��ȡ��Ч���ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		Area_NUM=RecordNum

		Obj_Destroy,RecordNumOBJ

		IF Area_NUM LE 0 then begin
		    temp=dialog_message('δ�ҵ��ظ����������',/information,title='��ʾ')
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
;		record=0	;20070823���
		for i=0,Num_Valid-1 do begin
			for j=mark,Area_NUM-1 do begin
				if Arr_Valid_County_Area[0,j] eq Valid_CountyCode[i] then begin
					Valid_County_Area[i]=Arr_Valid_County_Area[1,j]
					mark=j+1
;					record=record+1	;20070823���
					break
				endif
			endfor
		endfor

;;;20070823���****************************************
;		if record ne Num_Valid then begin
;			temp=dialog_message('�ؼ�������ݲ�ȫ������',/information,title='��ʾ')
;            return
;		endif
;***********************************************************

		County_Area_Weight=Valid_County_Area/total(Valid_County_Area)

		Data_To_Province=fltarr(27,Num_Valid)
		mark=0
		for i=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[4,i] ne '' then begin		;����Ӧ�ж�ÿ�����Ƿ�Ϊ��
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
		;���ݹ�������PROVINCE_CODE���ʡ��
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

		;(3)�����ʾ
	  	widget_control,(*PSTATE).TABLE_data,table_ysize=1,COLUMN_LABELS=(*PSTATE).labels_province
	;  					['Province_name','Province_code','year','WINTER_WHEAT',$
	;                      'SPRING_WHEAT','EARLY_RICE','SEMILATE_RICE',$
	;                      'LATE_RICE','SPRING_CORN','SUMMER_CORN',$
	;                      'SOYBEAN','millet','broomcorn',$
	;                      'potato','sweet_potato','peanut',$
	;                      'cole','helianthus','cotton',$
	;                      'hemp','sugar','tobacco',$
	;                      'vegetable_fruit','other',$
	;                      'SU_OR_AU'    ]    ;��26��

		(*PSTATE).Input_Success_Province ='No'
		(*PSTATE).Input_Flag=0	;��ʾ��δ�������ݵ��룬���ܺ�Ҫ���µ�������
;		(*PSTATE).Ext_Flag=0   ;��ʾ��δ����������
	  	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show_province

	endif

	(*PSTATE).Sta_Yes_ON='Yes'		;Ĭ�ϵ�'Sta_Yes_ON'��'Yes'

end


;++++++�������+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_data_to_DB'):begin     ;�������

		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('�ռ�¼������⣬���ȵ�������',/information,title='��ʾ')
            return
        endif

		if (*PSTATE).NUM_SHOW eq 1 then begin		;���ܵ�ʡ���������
			widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

;			arr_data_show=*((*PSTATE).Point_arr_show)	;��ȡ���е�����
			year_input=arr_data_show[2,0]

			DBCO=DBobj

			;(A)�����ݿ���ԭ�е����ݽ���ɾ��

			;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
			SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT_PROVINCE WHERE '
			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
			SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
;			PRINT,SQL
			DBCO->EXECUTESQL,SQL


;			(B)�����µ�����

			if (STRTRIM((arr_data_show[1]),2) eq '') or  $
				(STRTRIM((arr_data_show[2]),2) eq '') then begin
				temp=dialog_message('���飬ʡ�������ݲ���Ϊ��',/information,title='��ʾ')
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

;				print,'�ܹ�����¼��',jj
			(*PSTATE).Input_Success_Province ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
			temp=dialog_message('ʡ�����ƽ�����ɹ�',/information,title='��ʾ')
			log, '��ֲ�ṹ-��������', 1
		endif

;=====================================================================================
;===============�ؼ����ƽ�����======================================================
		if (*PSTATE).NUM_SHOW GT 1 then begin		;�ؼ����ƽ�����
			IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0��ʾ��δ��������
	            (*PSTATE).Input_Yes_ON=dialog_message('��δ�������ƣ��Ƿ�ȷ��Ҫ��⣿', /QUESTION,title='��ʾ')
	        endif

			if (*PSTATE).Input_Yes_ON eq 'Yes' then begin   ;ȷ���Ƿ�Ҫ���

				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

	;			arr_data_show=*((*PSTATE).Point_arr_show)	;��ȡ���е�����
				year_input=arr_data_show[3,0]

				DBCO=DBobj

				;(A)�����ݿ���ԭ�е����ݽ���ɾ��

				;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
				SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT WHERE '
				SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
				SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
				SQL=SQL+' AND LEFT(COUNTY_CODE,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���

	;			PRINT,SQL
				DBCO->EXECUTESQL,SQL


	;			(B)�����µ�����
				jj=0

				FOR I=0,(*PSTATE).NUM_SHOW-1 DO BEGIN

					if (STRTRIM((arr_data_show[2,I]),2) eq '') or  $
						(STRTRIM((arr_data_show[3,I]),2) eq '') then begin
						temp=dialog_message('���飬�ش������ݲ���Ϊ��',/information,title='��ʾ')
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
				print,'�ܹ�����¼��',jj
				(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
				temp=dialog_message('�ؼ����ƽ�����ɹ�',/information,title='��ʾ')
				log, '��ֲ�ṹ-��������', 1
			endif
		endif

		(*PSTATE).Input_Yes_ON = 'Yes'		;Ĭ�ϵ�'Input_Yes_ON'��'Yes'��ֱ������
;		(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
	end


;;*******************************************************************************************
;;*******************************************************************************************

;;===========================================================================
    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;�ر�
		common_log,'�ر�GVG����'
		if (*PSTATE).NUM_SHOW lt 1 then begin
			widget_control,event.top,/destroy
			return
		endif

		if (*PSTATE).NUM_SHOW eq 1 then begin		;ʡ������

			Input_Province_Decide='No'

			IF (*PSTATE).Input_Success_Province ne 'Yes' then begin	;��ʾ���Ƶ�������δ���
		        Input_Province_Decide=dialog_message('ʡ�����ƽ����δ��⣬�Ƿ�Ҫ��⣿',/QUESTION,/CANCEL,title='��ʾ')
		        log, '��ֲ�ṹ-��������', 0
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

	;			arr_data_show=*((*PSTATE).Point_arr_show)	;��ȡ���е�����
				year_input=arr_data_show[2,0]

				DBCO=DBobj

				;(A)�����ݿ���ԭ�е����ݽ���ɾ��

				;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
				SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT_PROVINCE WHERE '
				SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
				SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
				SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
	;			PRINT,SQL
				DBCO->EXECUTESQL,SQL


	;			(B)�����µ�����

				if (STRTRIM((arr_data_show[1]),2) eq '') or  $
					(STRTRIM((arr_data_show[2]),2) eq '') then begin
					temp=dialog_message('���飬ʡ�������ݲ���Ϊ��',/information,title='��ʾ')
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

	;				print,'�ܹ�����¼��',jj
				(*PSTATE).Input_Success_Province ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
				temp=dialog_message('ʡ�����ƽ�����ɹ�',/information,title='��ʾ')
				log, '��ֲ�ṹ-��������', 1
				end
			else:
			endcase
			return
		endif

;========================================================================================
		if (*PSTATE).NUM_SHOW gt 1 then begin		;�ؼ�����

			Input_County_Decide='No'

			IF ((*PSTATE).Ext_Flag eq 1) && ((*PSTATE).Input_Success_County ne 'Yes') then begin	;��ʾ���Ƶ�������δ���
		        Input_County_Decide=dialog_message('���Ƶ��ؼ������ݽ����δ��⣬�Ƿ�Ҫ��⣿',/QUESTION,/CANCEL,title='��ʾ')
		        log, '��ֲ�ṹ-��������', 0
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

		;			arr_data_show=*((*PSTATE).Point_arr_show)	;��ȡ���е�����
				year_input=arr_data_show[3,0]

				DBCO=DBobj

				;(A)�����ݿ���ԭ�е����ݽ���ɾ��

				;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
				SQL='DELETE FROM CROP_TYPES_PROPORTION_EXT WHERE '
				SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
				SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
				SQL=SQL+' AND LEFT(COUNTY_CODE,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���

		;			PRINT,SQL
				DBCO->EXECUTESQL,SQL

		;			(B)�����µ�����
				jj=0

				FOR I=0,(*PSTATE).NUM_SHOW-1 DO BEGIN

					if (STRTRIM((arr_data_show[2,I]),2) eq '') or  $
						(STRTRIM((arr_data_show[3,I]),2) eq '') then begin
						temp=dialog_message('���飬�ش������ݲ���Ϊ��',/information,title='��ʾ')
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
				print,'�ܹ�����¼��',jj
				temp=dialog_message('�ؼ����ƽ�����ɹ�',/information,title='��ʾ')
				log, '��ֲ�ṹ-��������', 1

				(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����

				end
			else:
			endcase
			return
		endif
  end 		;�رս���
;;*******************************************************************************************
;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;����

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '��������', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
	end
;;============================================================================
    else:
  endcase

end			; event �Ľ���

;;*******************************************************************************************
;;*******************************************************************************************


pro MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'����GVG����'
IF ( XREGISTERED('MJ_Extrapolate_GVG') NE 0 ) THEN RETURN

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=100 ,YOFFSET=200 ,SCR_XSIZE=796 ,SCR_YSIZE=475,TLB_FRAME_ATTR =1  $
      ,TITLE='�����������' ,SPACE=3 ,XPAD=3 ,YPAD=3)


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
;                      'SU_OR_AU'    ]    ;��27��
  labels_county=['ũҵ��������','����','�ش���','���','��С��',$
                      '��С��','�絾','�е�',$
                      '��','������','������',$
                      '��','����','����',$
                      '������','����','����',$
                      '�Ͳ�','���տ�','�޻�',$
                      '����','����','��Ҷ',$
                      '�߲˹Ϲ�','��������','GVGͼ������',$
                      '������'    ]    ;��27��
;  labels_province=['Province_name','Province_code','year','WINTER_WHEAT',$
;                      'SPRING_WHEAT','EARLY_RICE','SEMILATE_RICE',$
;                      'LATE_RICE','SPRING_CORN','SUMMER_CORN',$
;                      'SOYBEAN','millet','broomcorn',$
;                      'potato','sweet_potato','peanut',$
;                      'cole','helianthus','cotton',$
;                      'hemp','sugar','tobacco',$
;                      'vegetable_fruit','other',$
;                      'SU_OR_AU'    ]    ;��25��
  labels_province=['ʡ��','ʡ����','���','��С��',$
                      '��С��','�絾','�е�',$
                      '��','������','������',$
                      '��','����','����',$
                      '������','����','����',$
                      '�Ͳ�','���տ�','�޻�',$
                      '����','����','��Ҷ',$
                      '�߲˹Ϲ�','��������',$
                      '������'    ]    ;��25��
  TABLE_data = Widget_Table(WID_BASE_1, UNAME='TABLE_data' ,FRAME=1  $
      ,XOFFSET=1 ,YOFFSET=4 ,SCR_XSIZE=776 ,SCR_YSIZE=327,YSIZE=16 ,XSIZE=27  $
      ,/RESIZEABLE_COLUMNS,ALIGNMENT=0,ROW_LABELS=ROW_LABEL  $
      ,/align_center,y_scroll_size=16   $
      ,column_labels=labels_county)
	WIDGET_CONTROL,TABLE_data,SET_TABLE_SELECT=[-1,-1,-1,-1]   ;�ý������ʼʱ����Ӱ


  WID_BASE_3 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=402 ,SCR_XSIZE=776 ,SCR_YSIZE=34  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_BASE_4 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=7 ,SCR_XSIZE=776 ,SCR_YSIZE=36 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)

;Value_Ext_Type=['��ũҵ����','����������']
;Droplist_Ext_Type=widget_droplist(WID_BASE_4,/no_copy,$
;                  uname='Droplist_Ext_Type' ,$
;                  value=Value_Ext_Type,title='���Ʒ�ʽ',$
;                  scr_xsize=150,Xoffset=366,YOFFSET=10)


  WID_LABEL_0 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_0'  $
      ,XOFFSET=540 ,YOFFSET=15 ,SCR_XSIZE=100 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='GVGͼ��������ֵ��')

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
      ,/ALIGN_CENTER ,VALUE='����')

	Stat_To_Province = Widget_Button(WID_BASE_3, UNAME='Stat_To_Province'  $
      ,XOFFSET=288 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='���ܵ�ʡ')


  WID_BUTTON_7 = Widget_Button(WID_BASE_3, UNAME='butt_Close'  $
      ,XOFFSET=688 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�ر�')


  WID_BUTTON_8 = Widget_Button(WID_BASE_3, UNAME='butt_Help'  $
      ,XOFFSET=568 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_BUTTON_9 = Widget_Button(WID_BASE_3, UNAME='butt_ImportData'  $
      ,XOFFSET=28 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='��������')


  WID_BUTTON_10 = Widget_Button(WID_BASE_3, UNAME='butt_data_to_DB'  $
      ,XOFFSET=428 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�������')

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  WID_LABEL_1 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_1'  $
      ,XOFFSET=75,YOFFSET=15 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='��ݣ�')


  ARRYEAR=STRTRIM(INDGEN(36)+1980,2)
  DROPLIST_year = Widget_combobox(WID_BASE_4, UNAME='DROPLIST_year'  $
      ,XOFFSET=125 ,YOFFSET=10 ,SCR_XSIZE=50 ,SCR_YSIZE=23 ,VALUE=ARRYEAR)
  temp=(bin_date())[0]-1980+1
  widget_control,DROPLIST_year, SET_COMBOBOX_SELECT=temp-1


  ARR_SU_OR_AU=['����(0)','����(1)']
  DROPLIST_SU_OR_AU = Widget_Droplist(WID_BASE_4, UNAME='DROPLIST_SU_OR_AU'  $
      ,XOFFSET=345 ,YOFFSET=10 ,SCR_XSIZE=68 ,SCR_YSIZE=27 ,VALUE=ARR_SU_OR_AU)

	temp=(bin_date())[1]
	if temp ge 3 and temp le 8 then begin
		SU_OR_AU=0		;0��ʾΪ����
	endif else begin
		SU_OR_AU=1		;1��ʾΪ����
	endelse
   WIDGET_CONTROL,DROPLIST_SU_OR_AU,SET_DROPLIST_SELECT=SU_OR_AU


  WID_LABEL_2 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_2'  $
      ,XOFFSET=290 ,YOFFSET=15 ,SCR_YSIZE=11  $
      ,/ALIGN_LEFT ,VALUE='��������')

;TEMP=STRARR(27,16)
  Point_arr_show=ptr_new()

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_7,/INPUT_FOCUS


	STATE = { $
  		TABLE_data	:	TABLE_data , $
  		Point_arr_show	:	Point_arr_show, $	;���ڻ�ȡ�������е�����
  		ARRYEAR		:   ARRYEAR ,$
  		NUM			:	0,  $       ;������Ч��GVG��¼��Ŀ
  		NUM_SHOW	:	0,  $		;�ڽ��������ʾ�ļ�¼��Ŀ
  		Ext_Type	:	0,  $
  		Ext_Flag	:	0,  $
  		Input_Flag	:	0,  $		;����Ƿ��Ѿ���������
  		SU_OR_AU	:	SU_OR_AU,  $
  		ARR_GVG_NUM	:	ARR_GVG_NUM  ,$		;ѡ��GVGͼ��ֵ
  		GVG_NUM		:	GVG_NUM,  $
  		Sta_Yes_ON	:	'Yes'	,  $	;�ж��Ƿ�Ҫ���ܵ�ʡ
  		Input_Yes_ON	:	'Yes'	,  $	;�ж��Ƿ�Ҫ���
  		Input_Success_County	:	'Yes'	,  $	;������Ƶ��ؼ��������Ƿ��Ѿ����
  		Input_Success_Province	:	'Yes'	,  $	;�������ܵ�ʡ�������Ƿ��Ѿ����
  		labels_county	:	labels_county   ,  $
  		labels_province	:	labels_province	,  $
  		SELYEAR		:	(bin_date())[0]   $
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=PSTATE

  XManager, 'MJ_Extrapolate_GVG', WID_BASE_0, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'

end

