pro fill_prot_table, PSTATE
	;;;===========================================================================
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
	SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
	SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
	SQL=SQL+' where'
    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
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

	SQL='SELECT b.nz_code, b.County_name, b.County_Code,c.year, c.proportion, c.SU_OR_AU'
	SQL=SQL+',b.PLOWLAND'	;����ظ������
	SQL=SQL+' FROM (select NZ_CODE.*,PLOWLAND_AREA_COUNTY.PLOWLAND from NZ_CODE, PLOWLAND_AREA_COUNTY where PLOWLAND_AREA_COUNTY.COUNTY_CODE = NZ_CODE.County_code) b
	SQL=SQL+' LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
	SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
	SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
	SQL_SHOW=SQL+') c ON b.County_code = c.County_code order by b.County_Code'     ;�޸ģ�20070822
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

	arr_data_show=strarr(7,(*PSTATE).NUM_SHOW)
	ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
	count=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
		REPEAT BEGIN

			for i=0,6 do begin
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
			for m=4,5 do arr_data_show[m,j]=''
		endif
	endfor

	if ptr_valid((*PSTATE).Point_arr_show) eq 1 then ptr_free,(*PSTATE).Point_arr_show
	(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
	(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
	(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

  	;(3)�����ʾ
  	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)
  	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
end

;;*******************************************************************************************
pro MJ_Extrapolate_PROPORTION_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
;;;===========================================================================
	CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['�������´���',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF
 ;;;===========================================================================

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	ProID=STRMID(PROVINCE_CODE,0,2)		;======���ϲ�ͬʡ�ݵ�ѡ��===

;;;===========================================================================

  case wTarget of
;
;;=====ѡ�����Ƶ����======================================================================
	Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_year'):	BEGIN   ;ѡ�����Ƶ����
		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0
		fill_prot_table, PSTATE
	END
;;;;===========================================================================
;;;*******************************************************************************************
;;;*****ѡ������������**************************************************************************************
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='DROPLIST_SU_OR_AU'):begin
		(*PSTATE).SU_OR_AU =event.index           ;ѡ������������
		(*PSTATE).Input_Flag=0
		fill_prot_table, PSTATE
    end
;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='Droplist_Ext_Type'):begin
		(*PSTATE).Ext_Type =event.index           ;ѡ�����Ʒ�ʽ
      end
;;+++++++++��������+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Widget_Info(wWidget,FIND_BY_UNAME='butt_ImportData'):	BEGIN          ;��������
      	fill_prot_table, PSTATE
    END

;;++++++����+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
			    		sum_area = total(float((arr_data_show[6,same_NZ_id])[same_NZ_full_id]))
						for j=4, 4 do begin
			    			avg_ratio = total(float((arr_data_show[j,same_NZ_id])[same_NZ_full_id]) * float((arr_data_show[6,same_NZ_id])[same_NZ_full_id])) / sum_area
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
			    		sum_area = total(float((arr_data_show[6,same_XZ_id])[same_XZ_full_id]))
						for j=4, 4 do begin
			    			avg_ratio = total(float((arr_data_show[j,same_XZ_id])[same_XZ_full_id]) * float((arr_data_show[6,same_XZ_id])[same_XZ_full_id])) / sum_area
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
		if ptr_valid((*PSTATE).Point_arr_show) eq 1 then ptr_free,(*PSTATE).Point_arr_show
		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)		;
;----------------------------------------------------------------------------
	end     ;���ƽ���
;;;*******************************************************************************************
;;;*******************************************************************************************

Widget_Info(wWidget,FIND_BY_UNAME='Stat_To_Province'):	BEGIN     ;���ܵ�ʡ

	IF (*PSTATE).Input_Flag ne 1 then begin
        temp=dialog_message('���ȵ�������',/information,title='��ʾ')
        return
    endif

	arr_data_show=*((*PSTATE).Point_arr_show)


	Num_Valid=0		;Num_Valid��¼���в�Ϊ�յ�����
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
		record=0	;��¼�ҵ����������
		for i=0,Num_Valid-1 do begin
			for j=mark,Area_NUM-1 do begin
				if Arr_Valid_County_Area[0,j] eq Valid_CountyCode[i] then begin
					Valid_County_Area[i]=Arr_Valid_County_Area[1,j]
					mark=j+1
					record=record+1
					break
				endif
			endfor
		endfor
;
;		if record ne Num_Valid then begin
;			temp=dialog_message('�ؼ�������ݲ�ȫ������',/information,title='��ʾ')
;            return
;		endif


		County_Area_Weight=Valid_County_Area/total(Valid_County_Area)

		Data_To_Province=fltarr(6,Num_Valid)
		mark=0
		for i=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[4,i] ne '' then begin		;����Ӧ�ж�ÿ�����Ƿ�Ϊ��
				for j=4,5 do begin
					Data_To_Province[j,mark]=arr_data_show[j,i]
				endfor
				mark=mark+1
			endif
		endfor


		Province_Ext_Data=fltarr(6)
		for i=0,Num_Valid-1 do begin
			Province_Ext_Data_temp=Data_To_Province[*,i]*County_Area_Weight[i]
			Province_Ext_Data=Province_Ext_Data+Province_Ext_Data_temp
		endfor

		;=================================================================================
		;���ݹ�������PROVINCE_CODE���ʡ��
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

		SQL='Select Name from PROVINCE_CODE where Code='+"'"+PROVINCE_CODE+"'"

		oRS = OBJ_NEW('IDLdbRecordset', DBobj, SQL=SQL)
		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN
		    record = oRS->GetRecord()
		;    HELP, record, /STRUCTURE
		ENDIF
		Province_name=record.name

		Obj_Destroy,oRS
		;=================================================================================

		arr_data_show_province=strarr(5)
		arr_data_show_province[0]=STRTRIM(Province_name,2)
		arr_data_show_province[1]=PROVINCE_CODE
		arr_data_show_province[2]=STRTRIM((*PSTATE).SELYEAR,2)
		arr_data_show_province[3]=Province_Ext_Data[4]
		arr_data_show_province[4]=(*PSTATE).SU_OR_AU


		(*PSTATE).NUM_SHOW=1
		if ptr_valid((*PSTATE).Point_arr_show) eq 1 then ptr_free,(*PSTATE).Point_arr_show
		(*PSTATE).Point_arr_show=ptr_new(arr_data_show_province)

		(*PSTATE).Sta_Yes_ON = 'Yes'

		;(3)�����ʾ
	  	widget_control,(*PSTATE).TABLE_data,table_ysize=1,COLUMN_LABELS=(*PSTATE).labels_province

		(*PSTATE).Input_Success_Province ='No'
		(*PSTATE).Input_Flag=0	;��ʾ��δ�������ݵ��룬���ܺ�Ҫ���µ�������
;		(*PSTATE).Ext_Flag=0   ;��ʾ��δ����������
	  	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show_province

	endif

	(*PSTATE).Sta_Yes_ON='Yes'		;Ĭ�ϵ�'Sta_Yes_ON'��'Yes'

end


;;++++++�������+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
			SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT_PROVINCE WHERE '
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


			SQL='INSERT INTO CROP_PLANT_PROPORTION_EXT_PROVINCE '
			SQL=SQL+'(Province_code ,year ,proportion,SU_OR_AU ) '
			SQL=SQL+'VALUES ('
			SQL=SQL+ value2			+','
			SQL=SQL+ value3			+','
			SQL=SQL+ value4			+','
			SQL=SQL+ value5			+')'

;				PRINT,SQL
			DBCO->EXECUTESQL,SQL

;		CATCH, Error_status
;
;	  	IF Error_status NE 0 THEN BEGIN
;	   	  temp=dialog_message('�޷���⣬��ȷ��������ȷ����?,/error,title='����')
;	      return
;	   	ENDIF

;				print,'�ܹ�����¼��',jj
			(*PSTATE).Input_Success_Province ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
			temp=dialog_message('ʡ�����ƽ�����ɹ�',/information,title='��ʾ')
			log, '��ֲ����-��������', 1
		endif

;=====================================================================================
;===============�ؼ����ƽ�����======================================================
		if (*PSTATE).NUM_SHOW GT 1 then begin		;�ؼ����ƽ�����
			IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0��ʾ��δ��������
	            (*PSTATE).Input_Yes_ON=dialog_message('��δ�������ƣ��Ƿ�ȷ��Ҫ��⣿', /QUESTION,title='��ʾ')
	        endif

			if (*PSTATE).Input_Yes_ON eq 'Yes' then begin   ;ȷ���Ƿ�Ҫ���

				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

				year_input=arr_data_show[3,0]

				DBCO=DBobj

				;(A)�����ݿ���ԭ�е����ݽ���ɾ��

				;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
				SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT WHERE '
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

;					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
;						value5='NULL'
;	            	endif else begin
;	            		value5=STRTRIM((arr_data_show[5,I]),2)
;					endelse

					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
						value5=STRTRIM((*PSTATE).SU_OR_AU,2)
		        	endif else begin
		        		value5=STRTRIM((arr_data_show[5,I]),2)
					endelse


					SQL='INSERT INTO CROP_PLANT_PROPORTION_EXT '
					SQL=SQL+'(County_code ,year ,proportion,SU_OR_AU ) '
					SQL=SQL+'VALUES ('
					SQL=SQL+ value2			+','
					SQL=SQL+ value3			+','
					SQL=SQL+ value4			+','
					SQL=SQL+ value5			+')'

	;				PRINT,SQL
					DBCO->EXECUTESQL,SQL
		CATCH, Error_status

	  	IF Error_status NE 0 THEN BEGIN
	   	  temp=dialog_message('�޷���⣬��ȷ��������ȷ����',/error,title='����')
	      return
	   	ENDIF
					jj=jj+1

				ENDFOR
				print,'�ܹ�����¼��',jj
				(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
				temp=dialog_message('�ؼ����ƽ�����ɹ�',/information,title='��ʾ')
				log, '��ֲ����-��������', 1
			endif
		endif


		(*PSTATE).Input_Yes_ON = 'Yes'		;Ĭ�ϵ�'Input_Yes_ON'��'Yes'��ֱ������

	end


;;;*******************************************************************************************
;;;*******************************************************************************************
;
;;;===========================================================================
    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;�ر�
		common_log,'�ر���ֲ��������'
		if (*PSTATE).NUM_SHOW lt 1 then begin
			widget_control,event.top,/destroy
			return
		endif

		if (*PSTATE).NUM_SHOW eq 1 then begin		;ʡ������

			Input_Province_Decide='No'

			IF (*PSTATE).Input_Success_Province ne 'Yes' then begin	;��ʾ���Ƶ�������δ���
		        Input_Province_Decide=dialog_message('ʡ�����ƽ����δ��⣬�Ƿ�Ҫ��⣿',/QUESTION,/CANCEL,title='��ʾ')
		        log, '��ֲ����-��������', 0
		    endif

			case Input_Province_Decide of

			'No': begin
				ptr_free,(*PSTATE).Point_arr_show
				widget_control,event.top,/destroy
				end

			'Cancel': begin
				return
				end

			'Yes': begin
				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show

				year_input=arr_data_show[2,0]

				DBCO=DBobj

				;(A)�����ݿ���ԭ�е����ݽ���ɾ��

				;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
				SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT_PROVINCE WHERE '
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


				SQL='INSERT INTO CROP_PLANT_PROPORTION_EXT_PROVINCE '
				SQL=SQL+'(Province_code ,year ,proportion,SU_OR_AU ) '
				SQL=SQL+'VALUES ('
				SQL=SQL+ value2			+','
				SQL=SQL+ value3			+','
				SQL=SQL+ value4			+','
				SQL=SQL+ value5			+')'

	;				PRINT,SQL
				DBCO->EXECUTESQL,SQL

	;				print,'�ܹ�����¼��',jj
				(*PSTATE).Input_Success_Province ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
				temp=dialog_message('ʡ�����ƽ�����ɹ�',/information,title='��ʾ')
				log, '��ֲ����-��������', 1
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
		        log, '��ֲ����-��������', 0
		    endif

			case Input_County_Decide of

			'No': begin
				ptr_free,(*PSTATE).Point_arr_show
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
				SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT WHERE '
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

;					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
;						value5='NULL'
;		        	endif else begin
;		        		value5=STRTRIM((arr_data_show[5,I]),2)
;					endelse

					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
						value5=STRTRIM((*PSTATE).SU_OR_AU,2)
		        	endif else begin
		        		value5=STRTRIM((arr_data_show[5,I]),2)
					endelse


					SQL='INSERT INTO CROP_PLANT_PROPORTION_EXT '
					SQL=SQL+'(County_code ,year ,proportion,SU_OR_AU ) '
					SQL=SQL+'VALUES ('
					SQL=SQL+ value2			+','
					SQL=SQL+ value3			+','
					SQL=SQL+ value4			+','
					SQL=SQL+ value5			+')'

		;				PRINT,SQL
					DBCO->EXECUTESQL,SQL

					jj=jj+1

				ENDFOR
				print,'�ܹ�����¼��',jj
				temp=dialog_message('�ؼ����ƽ�����ɹ�',/information,title='��ʾ')
				log, '��ֲ����-��������', 1

				(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����

				end
			else:
			endcase
			return
		endif
  end 		;�رս���
;;;*******************************************************************************************
;;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;����

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '��������', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
	end
;		ONLINE_HELP,'����ģ��',  BOOK='HELP\HELP.chm', /FULL_PATH
;;        temp=dialog_message('ϵͳ��û�а���')
;	  end
;;============================================================================
    else:
  endcase

end			; event �Ľ���

;;*******************************************************************************************
;;*******************************************************************************************
pro MJ_Extrapolate_PROPORTION, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'������ֲ��������'
IF ( XREGISTERED('MJ_Extrapolate_PROPORTION') NE 0 ) THEN RETURN

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=180 ,YOFFSET=200 ,SCR_XSIZE=624 ,SCR_YSIZE=475,TLB_FRAME_ATTR =1  $
      ,TITLE='��ֲ��������' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'   $
      ,XOFFSET=6 ,YOFFSET=55 ,SCR_XSIZE=622 ,SCR_YSIZE=340  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  ROW_LABEL=strtrim(indgen(16),2)
;  labels_county=['NZ_Code','County_name','County_code','Year','Proportion',$
;                      'SU_OR_AU'    ]   ;��6��
  labels_county=['ũҵ��������','����','�ش���','���','��ֲ����',$
                      '������'    ]   ;��6��
;  labels_province=['Province_name','Province_code','year','Proportion', $
;                      'SU_OR_AU'    ]    ;��5��
  labels_province=['ʡ��','ʡ����','���','��ֲ����', $
                      '������'    ]    ;��5��
  TABLE_data = Widget_Table(WID_BASE_1, UNAME='TABLE_data' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=4 ,SCR_XSIZE=606 ,SCR_YSIZE=327 ,XSIZE=6  $
      ,/RESIZEABLE_COLUMNS,ALIGNMENT=0,ROW_LABELS=ROW_LABEL  $
      ,/align_center,y_scroll_size=16   $
      ,column_labels=labels_county)
	WIDGET_CONTROL,TABLE_data,SET_TABLE_SELECT=[-1,-1,-1,-1]   ;�ý������ʼʱ����Ӱ


  WID_BASE_3 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=402 ,SCR_XSIZE=606 ,SCR_YSIZE=34  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_BASE_4 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=606 ,SCR_YSIZE=38 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


;Value_Ext_Type=['��ũҵ����','����������']
;Droplist_Ext_Type=widget_droplist(WID_BASE_4,/no_copy,$
;                  uname='Droplist_Ext_Type' ,$
;                  value=Value_Ext_Type,title='���Ʒ�ʽ',$
;                  scr_xsize=160,Xoffset=369,YOFFSET=10)


  WID_BUTTON_6 = Widget_Button(WID_BASE_3, UNAME='butt_Ext'  $
      ,XOFFSET=127 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_BUTTON_7 = Widget_Button(WID_BASE_3, UNAME='butt_Close'  $
      ,XOFFSET=527 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�ر�')


  WID_BUTTON_8 = Widget_Button(WID_BASE_3, UNAME='butt_Help'  $
      ,XOFFSET=437 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_BUTTON_9 = Widget_Button(WID_BASE_3, UNAME='butt_ImportData'  $
      ,XOFFSET=17 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='��������')


  WID_BUTTON_10 = Widget_Button(WID_BASE_3, UNAME='butt_data_to_DB'  $
      ,XOFFSET=327 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�������')


 Stat_To_Province = Widget_Button(WID_BASE_3, UNAME='Stat_To_Province'  $
      ,XOFFSET=217 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='���ܵ�ʡ')


  WID_LABEL_1 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_1'  $
      ,XOFFSET=100,YOFFSET=15 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='��ݣ�')


  ARRYEAR=STRTRIM(INDGEN(36)+1980,2)
  DROPLIST_year = Widget_combobox(WID_BASE_4, UNAME='DROPLIST_year'  $
      ,XOFFSET=150 ,YOFFSET=10 ,SCR_XSIZE=50 ,SCR_YSIZE=23 ,VALUE=ARRYEAR)
  temp=(bin_date())[0]-1980+1
  widget_control,DROPLIST_year, SET_COMBOBOX_SELECT=temp-1


  ARR_SU_OR_AU=['����(0)','����(1)']
  DROPLIST_SU_OR_AU = Widget_Droplist(WID_BASE_4, UNAME='DROPLIST_SU_OR_AU'  $
      ,XOFFSET=350 ,YOFFSET=10 ,SCR_XSIZE=68 ,SCR_YSIZE=27 ,VALUE=ARR_SU_OR_AU)

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


  Point_arr_show=ptr_new()

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_7,/INPUT_FOCUS


	STATE = { $
  		TABLE_data	:	TABLE_data , $
  		Point_arr_show	:	Point_arr_show, $	;���ڻ�ȡ�������е�����
  		ARRYEAR		:   ARRYEAR ,$
  		NUM			:	0,  $       ;��Ч��¼��Ŀ
  		NUM_SHOW	:	0,  $		;�ڽ��������ʾ�ļ�¼��Ŀ
  		Ext_Type	:	0,  $
  		Ext_Flag	:	0,  $
  		SU_OR_AU	:	SU_OR_AU,  $
  		Input_Flag	:	0,  $		;����Ƿ��Ѿ���������
  		Input_Yes_ON	:	'Yes'	,  $
		Sta_Yes_ON	:	'Yes'	,  $
		Input_Success_County	:	'Yes'   ,  $
		Input_Success_Province	:	'Yes'   ,  $
		labels_county	:	labels_county   ,  $
  		labels_province	:	labels_province	,  $
  		SELYEAR		:	(bin_date())[0] 	$
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=PSTATE

  XManager, 'MJ_Extrapolate_PROPORTION', WID_BASE_0, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'

end