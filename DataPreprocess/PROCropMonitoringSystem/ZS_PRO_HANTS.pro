;**********************************************************************************

;***********************************************************************************
;
; �ɼ̻�,2008.08.19
; ������ʡ��ϵͳ�ж��ص��������������߽����ع�
; ��ԭ��ȫ���Ƽ��ϵͳ��HANTS
;
;-----------------------------------------------------------------
;-----------------------------------------------------------------
FUNCTION HANTS_DO, PSTATE
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

     IF (yesORno EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('�����������ݿ�����!',TITLE='��ʾ',/information)
     	CLOSE,/ALL
     	RETURN,0
     ENDIF

     OD=DBobj

    ;********************************************************************************************
     ;����������������Լ���,
     ;�������Ĳ���������,�򲻽��м���,
     ;���ԶԻ������ʽ��ʾ�û���������

	;WIDGET_CONTROL,(*pstate).TXT_DOD_HANTS,get_value=DOD
	DOD = (*pstate).TXT_DOD_HANTS
     IF(DOD EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('������DOD(���������)!',TITLE='��ʾ',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     DOD=FIX(DOD[0])

	;WIDGET_CONTROL,(*pstate).TXT_TOLERANCE_HANTS,get_value=TOLERANCE
	TOLERANCE = (*pstate).TXT_TOLERANCE_HANTS
     IF(TOLERANCE EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('�����������ع�����!',TITLE='��ʾ',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     TOLERANCE=FIX(TOLERANCE[0])

	;WIDGET_CONTROL,(*pstate).TXT_FREQUENCY_HANTS,get_value=FREQUENCY
	FREQUENCY = (*pstate).TXT_FREQUENCY_HANTS
     IF(FREQUENCY EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('������Ƶ��!',TITLE='��ʾ',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     FREQUENCY=FIX(FREQUENCY[0])

	;WIDGET_CONTROL,(*pstate).TXT_MENXIAN_HANTS,get_value=MENXIAN
	MENXIAN = (*pstate).TXT_MENXIAN_HANTS
     IF(MENXIAN EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('����������!',TITLE='��ʾ',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     MENXIAN=FIX(MENXIAN[0])

	;��ÿ�ʼ�ͽ�����ʱ�����
	YEAR_START=(*pstate).YEAR_START
	YEAR_END=(*pstate).YEAR_END
	MONTH_START=(*pstate).MONTH_START
	MONTH_END=(*pstate).MONTH_END
	DAY_START=(*pstate).DAY_START
	DAY_END=(*pstate).DAY_END

	;���ɲ�ѯ��ʱ�䷶Χ����
	JULDAY_START=JULDAY(MONTH_START, DAY_START, YEAR_START, 0, 0, 0)
	JULDAY_END	=JULDAY(MONTH_END, DAY_END, YEAR_END, 0, 0, 0)

	;�����������ʱ���ʱ����һ����Ҫ��,������Ҫ��ʱ���Ƿ��������ж�
	IF(JULDAY_START GT JULDAY_END) THEN BEGIN
		TEMP=DIALOG_MESSAGE('��ʼʱ�����Ҫ���ڽ���ʱ��',TITLE='��������')
		CLOSE,/ALL
 		RETURN, 0
	ENDIF

	;********************************************************************************************
	;********************************************************************************************
	;���ݿ������������,����������������ݵĺ����Լ���
	;�������Ҫ��,�������ʾ
	;��HANTS�������еĲ������м��
	IF(DOD LE 0) THEN BEGIN
		msg=DIALOG_MESSAGE('DOD����С����!',TITLE='��ʾ',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	IF(MENXIAN GT 0) THEN BEGIN
		msg=DIALOG_MESSAGE('����ֵ����С����!',TITLE='��ʾ',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	RANGE			=STRTRIM((*PSTATE).RANGE,2)
	RANGE_CODE		=STRTRIM((*PSTATE).RANGE_CODE,2)
	;����ѡ�����������������ж�
	IF(RANGE EQ '��ѡ��') THEN BEGIN
		msg=DIALOG_MESSAGE('��ѡ������!',TITLE='��ʾ',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END


	PRINT,YEAR_START,MONTH_START,DAY_START
	PRINT,YEAR_END,MONTH_END,DAY_END


	period			=STRTRIM((*PSTATE).period,2)
	SENSOR_TYPE		=STRTRIM((*PSTATE).SENSOR_TYPE,2)
	SENSOR_CODE		=STRTRIM((*PSTATE).SENSOR_CODE,2)

	WIDGET_CONTROL,(*pstate).DRAW_HANTS,get_value=DRAW_HANTS

	;����һ������,�����洢�����ݿ��ж�ȡ����������
	NUM_MAX=80
	ARR_YEAR	=INTARR(NUM_MAX)
	ARR_MONTH	=INTARR(NUM_MAX)
	ARR_DAY		=INTARR(NUM_MAX)

	ARR_PLOWLAND	=FLTARR(NUM_MAX)
	ARR_PADDY_FIELD	=FLTARR(NUM_MAX)
	ARR_DRY_LAND	=FLTARR(NUM_MAX)

	;********************************************************************
	;���ݳ��ȵı�ʶ��Ҫ�����޸�
	;�������������ʶ���ݵĳ���(�Ǹ�������Ĳ������ɵĳ���)

	;Ŀǰʹ����һ���Ƚϼ򵥵ķ���,���Ƕ�ȡһ���������ص�����
	;(�ڴ������ص�ʱ��,��ȡһ���ص�����,�����ȡ�����ص�����)
	;�����������,����Ҫ�������ص����ݵĳ���һ��
	IF RANGE NE '������(������)' THEN BEGIN	;�������ص�����

		;��������ʱ���������в�ѯ
		NUM_START=YEAR_START*500.0+MONTH_START*40.0+DAY_START
		NUM_END=YEAR_END*500.0+MONTH_END*40.0+DAY_END
		SQL='SELECT YEAR,MONTH,DAY FROM PARAMETER_PROCESS_COUNTY WHERE '
		SQL=SQL+'(COUNTY_CODE='+"'"+RANGE_CODE+"'"+') AND '
		SQL=SQL+'(periods='+period+') AND '
		SQL=SQL+'(DATA_TYPE='+"'NDVI'"+') AND '
		SQL=SQL+'(SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') AND '
		SQL=SQL+'(YEAR*500+MONTH*40+DAY>='+STRTRIM(NUM_START,2)+') AND '
		SQL=SQL+'(YEAR*500+MONTH*40+DAY<='+STRTRIM(NUM_END,2)+')'
		SQL=SQL+' ORDER BY YEAR,MONTH,DAY'
		SQL1=SQL
		PRINT,SQL

		;ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
		;��ȡ���ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',OD,SQL='select count(*) from ('+Sql+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		NI_INPUT=RecordNum

		if NI_INPUT gt 36 then begin
			info=dialog_message('��ѡʱ�䷶Χ�ѳ���һ�꣬������ѡ��',title='��ʾ')
			return,0
		endif

		Obj_Destroy,RecordNumOBJ
		;Obj_Destroy,ORS
		IF NI_INPUT EQ 0 THEN BEGIN
			TEMP='û���ҵ���Ӧ������'
			msg=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF

		;��һ���Ƚ�������㷨��������Ӧʱ���ڵļ�¼�����Ƿ����
		TEMP=0;ABS(LONG((JULDAY_END-JULDAY_START)/period)-NI_INPUT)
		;aa
		IF TEMP GT 1 THEN BEGIN
			TEMP='��Ӧʱ���ڵ����ݼ�¼������,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'��¼��Ӧ��������������:'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'ABS((������-��ʼ��)/Ƶ��-���ݸ���)<1'
			msg=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF ELSE BEGIN
			NI_INPUT=NI_INPUT
			(*PSTATE).NI=NI_INPUT
		ENDELSE
		PRINT,NI_INPUT

		;������������ݶ�ȡ����
		ORS = OBJ_NEW('IDLDBRECORDSET',OD,SQL=Sql)
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			COUNT=0
			REPEAT BEGIN
				ARR_YEAR[COUNT]		=ORS->GETFIELD(0)
				ARR_MONTH[COUNT]	=ORS->GETFIELD(1)
				ARR_DAY[COUNT]		=ORS->GETFIELD(2)
				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF

		(*PSTATE).ARR_YEAR_DATA	=ARR_YEAR
		(*PSTATE).ARR_MONTH_DATA=ARR_MONTH
		(*PSTATE).ARR_DAY_DATA	=ARR_DAY
		PRINT,(*PSTATE).ARR_YEAR_DATA
		PRINT,(*PSTATE).ARR_MONTH_DATA
		PRINT,(*PSTATE).ARR_DAY_DATA

	ENDIF ELSE BEGIN
		PRINT,'����������'
		;�ӱ�'COUNTY_CODE_RASTER'���ж�ȡһ����¼,���л�ȡһ����Ч���ش���
		;�ڷ���"ABS((������-��ʼ��)/Ƶ��-���ݸ���)<1"
		;�����������,�����������ΪNI_INPUT
		;����ÿ��������Ӧʱ����ڵ����ݸ�����Ӧ������һ��.
		SQL='SELECT CODE FROM COUNTY_CODE_RASTER'
		;���������ѡȡ������Ϊ�˱�֤�����������Ӧ�Ŀռ�������֮���Ӧ
		ORS = OBJ_NEW('IDLDBRECORDSET',OD,SQL=SQL)
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			COUNTY_CODE = ORS->GETFIELD(0)
		ENDIF

		COUNTY_CODE=STRTRIM(COUNTY_CODE,2)
		NUM_START=YEAR_START*500.0+MONTH_START*40.0+DAY_START
		NUM_END=YEAR_END*500.0+MONTH_END*40.0+DAY_END
		SQL='SELECT YEAR,MONTH,DAY FROM PARAMETER_PROCESS_COUNTY WHERE '
		SQL=SQL+'(COUNTY_CODE='+"'"+COUNTY_CODE+"'"+') AND '
		SQL=SQL+'(periods='+period+') AND '
		SQL=SQL+'(DATA_TYPE='+"'NDVI'"+') AND '
		SQL=SQL+'(SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') AND '
		SQL=SQL+'(YEAR*500+MONTH*40+DAY>='+STRTRIM(NUM_START,2)+') AND'
		SQL=SQL+'(YEAR*500+MONTH*40+DAY<='+STRTRIM(NUM_END,2)+')'
		SQL=SQL+' ORDER BY YEAR,MONTH,DAY'

		PRINT,SQL

		;ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
		;��ȡ���ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',OD,SQL='select count(*) from ('+Sql+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		NI_INPUT=RecordNum

;		if NI_INPUT gt 36 then begin
;			info=dialog_message('��ѡʱ�䷶Χ�ѳ���һ�꣬������ѡ��',title='��ʾ')
;			return,0
;		endif

		Obj_Destroy,RecordNumOBJ
		;Obj_Destroy,ORS
		IF NI_INPUT EQ 0 THEN BEGIN
			TEMP='û���ҵ���Ӧ������'
			msg=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF

		;��һ���Ƚ�������㷨��������Ӧʱ���ڵļ�¼�����Ƿ����
		TEMP=ABS(LONG((JULDAY_END-JULDAY_START)/FIX(period))-NI_INPUT)
		TEMP=0

		IF TEMP GT 1 THEN BEGIN
			TEMP='��Ӧʱ���ڵ����ݼ�¼������,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'��¼��Ӧ��������������:'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'ABS((������-��ʼ��)/Ƶ��-���ݸ���)<1'
			msg=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF ELSE BEGIN
			NI_INPUT=NI_INPUT
			(*PSTATE).NI=NI_INPUT
		ENDELSE

		;������������ݶ�ȡ����
		ORS = OBJ_NEW('IDLDBRECORDSET',OD,SQL=Sql)
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			COUNT=0
			REPEAT BEGIN
				ARR_YEAR[COUNT]		=ORS->GETFIELD(0)
				ARR_MONTH[COUNT]	=ORS->GETFIELD(1)
				ARR_DAY[COUNT]		=ORS->GETFIELD(2)
				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF

		(*PSTATE).ARR_YEAR_DATA	=ARR_YEAR
		(*PSTATE).ARR_MONTH_DATA=ARR_MONTH
		(*PSTATE).ARR_DAY_DATA	=ARR_DAY
		PRINT,(*PSTATE).ARR_YEAR_DATA
		PRINT,(*PSTATE).ARR_MONTH_DATA
		PRINT,(*PSTATE).ARR_DAY_DATA

		PRINT,NI_INPUT
	ENDELSE
	;********************************************************************


	;********************************************************************
	;��ȡ�����ع��Ĳ���
	NI=NI_INPUT
	FET=TOLERANCE
	NF=FREQUENCY
	IDRT=MENXIAN
	SCALE=1
	PER=NI*10
	DOD=DOD
	HILO='LO'
	TS=FINDGEN(NI+1)*10
	PA=INDGEN(10)
	FOR I=1,NF DO BEGIN
		PA[i]=PER/I
	ENDFOR
	AMP=FLTARR((NF+1)+1)
	PHI=FLTARR((NF+1)+1)

	;********************************************************************************************
	IF(NI_INPUT LT NF*2+DOD+1) THEN BEGIN
		TEMP='���ݵ㲻��,�볢��:'
		TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
		TEMP=TEMP+'(1)���ӵ�ĸ���'
		TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
		TEMP=TEMP+'(2)����Ƶ��'
		TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
		TEMP=TEMP+'(3)����DOD��ĸ���'
		MSG=DIALOG_MESSAGE(TEMP,TITLE='���ݵ㲻��',/INFORMATION)
		CLOSE,/ALL
		RETURN,0
	ENDIF
	;********************************************************************************************



	IF(NI GT NUM_MAX) THEN BEGIN
		TEMP='����ϵ�г��ȳ�����ϵͳ������!'
		msg=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
		CLOSE,/all
		RETURN,0
	ENDIF
	;����ȡ80����Ϊ���ݵ���С��������Ϊ5,ֻ����һ��Ļ����ᳬ��80

	;����һ�����������洢��Ч��¼�ĸ���
	COUNT=0
	;�ϳ�ʱ������,��SQL���ʹ��


    ;**********************************************************************************;
    ;������ϵͳ�������ݴ���ĺ���
	;�������������������,һ����һ�δ���һ����,��һ����һ�δ������е���
	IF(RANGE EQ '������(������)') THEN BEGIN	;һ�δ���ȫ�������ص�����
		PRINT,'WHOLE COUNTRY'


		;**********************************************************************************;
		;���������ж����ݳ��Ƚ�����֤,
		;��֤�ķ�����ʹ��ʹ��ǰ���NI_INPUT����
		;�����ݿ��ж�ȡÿ���ص����ݳ���,���ĳ���ص����ݳ�����NI_INPUT��һ��,
		;�򱨴��˳�����
		NUM_START=YEAR_START*500.0+MONTH_START*40.0+DAY_START
		NUM_END=YEAR_END*500.0+MONTH_END*40.0+DAY_END
		SQL='SELECT CODE FROM COUNTY_CODE_RASTER'	;���������ѡȡ������Ϊ�˱�֤�����������Ӧ�Ŀռ�������֮���Ӧ
		ORS = OBJ_NEW('IDLDBRECORDSET',OD,SQL=SQL)
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			REPEAT BEGIN
				COUNTY_CODE = ORS->GETFIELD(0)
				COUNTY_CODE=STRTRIM(COUNTY_CODE,2)

				SQL='SELECT * FROM PARAMETER_PROCESS_COUNTY WHERE '
				SQL=SQL+'(COUNTY_CODE='+"'"+COUNTY_CODE+"'"+') AND '
				SQL=SQL+'(periods='+period+') AND '
				SQL=SQL+'(DATA_TYPE='+"'NDVI'"+') AND '
				SQL=SQL+'(SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') AND '
				SQL=SQL+'(YEAR*500+MONTH*40+DAY>='+STRTRIM(NUM_START,2)+') AND'
				SQL=SQL+'(YEAR*500+MONTH*40+DAY<='+STRTRIM(NUM_END,2)+')'

				;PRINT,SQL

				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',OD,SQL='select count(*) from ('+Sql+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				NI_INPUT_TEMP=RecordNum
				Obj_Destroy,RecordNumOBJ

				IF (NI_INPUT NE NI_INPUT_TEMP) THEN BEGIN
					TEMP='�����ص��������г����쳣'
					TEMP=TEMP+STRING(BYTE(10))+STRING(BYTE(13))
					TEMP=TEMP+'�ش���:'+STRTRIM(COUNTY_CODE)
					msg=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
					CLOSE,/all
		    		RETURN,0
				ENDIF
				PRINT,COUNTY_CODE,NI_INPUT_TEMP
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)

		ENDIF
		Obj_Destroy,ORS

		;**********************************************************************************;
		;������Ҫʹ�õ�����
		;ARR_SHOW=STRARR(2*NI_INPUT+3,3*2500)
		ARR_SHOW=STRARR(2*NI_INPUT+3,3*200)
		;������200����,���������صĸ�����,
		;ͨ��һ��ʡֻ��100-120����

		;���Ȱ��ش�������ݼ���ȡ����
		;**********************************************************************************;
		;������ԭ����SQL���,�����ݿ��ж�̬������Ƶ�ʵĶ�ȡ,
		;ʡ��ϵͳ��ʹ�ù̶���Ƶ��,�������������޸�
;		SQL='SELECT CODE,NAME,NF FROM '
;		SQL=SQL+'(select county_code.code,county_code.name,county_hants_parameter.nf'
;		SQL=SQL+' from county_code,county_hants_parameter'
;		SQL=SQL+' where county_code.code=county_hants_parameter.county_code)'
		;**********************************************************************************;
		SQL='select T1.CODE,T2.NAME from COUNTY_CODE_RASTER T1,COUNTY_CODE T2 WHERE T1.CODE=T2.CODE'
		ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)

		;modified 20070424 for bug
		;aa
        print,''
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',OD,SQL='select count(*) from ('+Sql+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		TOTAL_COUNTY=RecordNum
		Obj_Destroy,RecordNumOBJ
		PRINT,'TOTAL_COUNTY',TOTAL_COUNTY


		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

			;COUNT_COUNTY=0
			COUNT_COUNTY=0
			;****************************************************************
			progressTimer = Obj_New("ShowProgress", tlb,/CancelButton)
			progressTimer->start
			;****************************************************************
			REPEAT BEGIN
				;****************************************************************
				cancelled = progressTimer->CheckCancel()
				IF cancelled THEN BEGIN
					ok = Dialog_Message('�û���ֹ�˲���',TITLE='��ʾ')
					progressTimer->Destroy ;����������
					RETURN,0
				ENDIF
				progressTimer->Update, (FLOAT(COUNT_COUNTY)/TOTAL_COUNTY * 100.0) ;��������
				;****************************************************************

				;�����������ش���
		 		COUNTY_CODE	=ORS -> GETFIELD(0)
				COUNTY		=ORS -> GETFIELD(1)
				;NF_DB		=ORS -> GETFIELD(2)
				NF_DB		=FREQUENCY
				;************************************************************************************
;				SQL='SELECT AVG_PLOWLAND,AVG_PADDY_FIELD,AVG_DRY_LAND'
;				SQL=SQL+' FROM CROP.NDVI_DAY_PROCESS_COUNTY WHERE '
;				SQL=SQL+'( ("COUNTY_CODE" = '+"'"+COUNTY_CODE+"' "+')'
;				SQL=SQL+' AND (YEAR ='+STRTRIM(STRING(YEAR_START),2)+')'
;				SQL=SQL+' AND (((MONTH-1)*3+DAY) >='+STRTRIM(STRING(DAY_START_36),2)+')'
;				SQL=SQL+' AND (((MONTH-1)*3+DAY) <='+STRTRIM(STRING(DAY_END_36),2)+')'
;				SQL=SQL+')'
;				SQL=SQL+'ORDER BY YEAR,MONTH,DAY'

				SQL='SELECT avg_plowland,avg_paddy_field,avg_dry_land,'
				SQL=SQL+'year,month,day '
				SQL=SQL+'FROM PARAMETER_PROCESS_COUNTY WHERE '
				SQL=SQL+'(COUNTY_CODE='+"'"+COUNTY_CODE+"'"+') AND '
				SQL=SQL+'(periods='+period+') AND '
				SQL=SQL+'(DATA_TYPE='+"'NDVI'"+') AND '
				SQL=SQL+'(SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') AND '
				;modified 20070425
				SQL=SQL+'(YEAR*500+MONTH*40+DAY>='+STRTRIM(NUM_START,2)+') AND'
				SQL=SQL+'(YEAR*500+MONTH*40+DAY<='+STRTRIM(NUM_END,2)+')'
				SQL=SQL+'ORDER BY YEAR,MONTH,DAY'
				ORS_1 = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
				COUNT=0

				;��ȡһ���ص�����
				IF(ORS_1->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
					ARR_PLOWLAND[COUNT] = ORS_1 -> GETFIELD(0)
					ARR_PADDY_FIELD[COUNT] = ORS_1 -> GETFIELD(1)
					ARR_DRY_LAND[COUNT] = ORS_1 -> GETFIELD(2)
					COUNT=COUNT+1
					WHILE (ORS_1->MOVECURSOR(/NEXT)) DO BEGIN
		 				ARR_PLOWLAND[COUNT] = ORS_1 -> GETFIELD(0)
						ARR_PADDY_FIELD[COUNT] = ORS_1 -> GETFIELD(1)
						ARR_DRY_LAND[COUNT] = ORS_1 -> GETFIELD(2)
						COUNT=COUNT+1
					ENDWHILE
					PRINT,'HERE COUNT',COUNT
				ENDIF

				;�Ѷ�ȡ�����ݵ�RECORDSET�����ͷŵ�
				OBJ_DESTROY, ORS_1

				IF(1) THEN BEGIN ;ֻ����ʵ�ʵ����ݳ��Ⱥ�ѡ������ݳ�����ͬʱ�Ž��м���
					;Ϊ����Ӧ�㷨,������һ����һ������
					ARR_PLOWLAND_TEMP=FLTARR(COUNT+1)
					ARR_PLOWLAND_TEMP[1:COUNT]=ARR_PLOWLAND[0:COUNT-1]
					ARR_PADDY_FIELD_TEMP=FLTARR(COUNT+1)
					ARR_PADDY_FIELD_TEMP[1:COUNT]=ARR_PADDY_FIELD[0:COUNT-1]
					ARR_DRY_LAND_TEMP=FLTARR(COUNT+1)
					ARR_DRY_LAND_TEMP[1:COUNT]=ARR_DRY_LAND[0:COUNT-1]

					;�ֶ������������������Ŵ�����Ľ��
					ARR_PLOWLAND_HANTS=FLTARR(COUNT+1)
					ARR_PADDY_FIELD_HANTS=FLTARR(COUNT+1)
					ARR_DRY_LAND_HANTS=FLTARR(COUNT+1)



					;********************************************************************************************
					;�����������е�HANTS������Ҫʹ�õĲ���,
					NI=COUNT
					FET=TOLERANCE
					NF=FREQUENCY

					;�ڴ���һ����������ݵ�ʱ��
					;ʹ�����ݿ��и��ݸ���ָ���ƶ���HANTS���ز���
					IF(NI EQ 36 ) THEN BEGIN
						IF((NF_DB GE 1) AND (NF_DB LE 3)) THEN BEGIN;����Ч��Χ�ڲ�ʹ��
							NF=NF_DB
						ENDIF ELSE BEGIN
							NF=3
						ENDELSE
					ENDIF

					IDRT=MENXIAN
					SCALE=1
					PER=NI*10
					DOD=DOD
					HILO='LO'
					TS=FINDGEN(NI+1)*10
					PA=INDGEN(10)
					FOR I=1,NF DO BEGIN
						PA[I]=PER/I
					ENDFOR

					AMP=FLTARR((NF+1)+1)
					PHI=FLTARR((NF+1)+1)
					;Y=O_DATA*10
					;********************************************************************************************

					Y=ARR_PLOWLAND_TEMP*100+10
					ARR_PLOWLAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
					Y=ARR_PADDY_FIELD_TEMP*100+10
					ARR_PADDY_FIELD_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
					Y=ARR_DRY_LAND_TEMP*100+10
					ARR_DRY_LAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
					;********************************************************************************************

					;********************************************************************************************
					;����һ������������һ�������ļ�¼(��������,�ֱ��Ǹ���\ˮ��\����),�����˴�����˵����ݺ�ԭʼ����

					ARR_SHOW[0,0+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,0+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,0+COUNT_COUNTY*3]='����'
					ARR_SHOW[3:NI+2,0+COUNT_COUNTY*3]=ARR_PLOWLAND_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,0+COUNT_COUNTY*3]=ARR_PLOWLAND[0:NI-1]

					ARR_SHOW[0,1+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,1+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,1+COUNT_COUNTY*3]='ˮ��'
					ARR_SHOW[3:NI+2,1+COUNT_COUNTY*3]=ARR_PADDY_FIELD_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,1+COUNT_COUNTY*3]=ARR_PADDY_FIELD[0:NI-1]

					ARR_SHOW[0,2+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,2+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,2+COUNT_COUNTY*3]='����'
					ARR_SHOW[3:NI+2,2+COUNT_COUNTY*3]=ARR_DRY_LAND_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,2+COUNT_COUNTY*3]=ARR_DRY_LAND[0:NI-1]
					COUNT_COUNTY=COUNT_COUNTY+1

				ENDIF

				;************************************************************************************
				PRINT,COUNT

			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
			OBJ_DESTROY, ORS
			progressTimer->destroy ;

			ARR_SHOW_1=STRARR(2*NI_INPUT+3,3*COUNT_COUNTY);������2500����;����ʵ�����ݴ�С���������
			ARR_SHOW_1[*,0:3*COUNT_COUNTY-1]=ARR_SHOW[*,0:3*COUNT_COUNTY-1]

			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_XSIZE=2*NI_INPUT+3
			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_YSIZE=3*COUNT_COUNTY
			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_VALUE=ARR_SHOW_1

			;�ͷ�һЩ�Ƚϴ������
			ARR_SHOW	=0
			ARR_SHOW_1	=0
			ARR_PLOWLAND_TEMP=0
			ARR_PLOWLAND_HANTS=0
			ARR_PADDY_FIELD_TEMP=0
			ARR_PADDY_FIELD_HANTS=0
			ARR_DRY_LAND_TEMP=0
			ARR_DRY_LAND_HANTS=0


			(*PSTATE).HANTS_DONE=1
			(*PSTATE).NI=NI_INPUT

		ENDIF
		PRINT,'HERE A'

		;WIDGET_CONTROL,(*F_READ_DATA).BASE_READ_DATA,/DESTROY
		MSG='��ɼ���'+STRING(BYTE(10))+STRING(BYTE(13))
		MSG=MSG+'��������'+STRTRIM(STRING(COUNT_COUNTY),2)+'����'
		TEMP=DIALOG_MESSAGE(MSG,TITLE='�������',/INFORMATION)

	ENDIF ELSE BEGIN	;����һ���ص�����
		print,'����һ���ص�����'
		NUM_START=YEAR_START*500.0+MONTH_START*40.0+DAY_START
		NUM_END=YEAR_END*500.0+MONTH_END*40.0+DAY_END
		SQL='SELECT avg_plowland,avg_paddy_field,avg_dry_land,'
		SQL=SQL+'year,month,day '
		SQL=SQL+'FROM PARAMETER_PROCESS_COUNTY WHERE '
		SQL=SQL+'(COUNTY_CODE='+"'"+RANGE_CODE+"'"+') AND '
		SQL=SQL+'(periods='+period+') AND '
		SQL=SQL+'(DATA_TYPE='+"'NDVI'"+') AND '
		SQL=SQL+'(SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') AND '
		SQL=SQL+'(YEAR*500+MONTH*40+DAY>='+STRTRIM(NUM_START,2)+') AND'
		SQL=SQL+'(YEAR*500+MONTH*40+DAY<='+STRTRIM(NUM_END,2)+')'
		SQL=SQL+'ORDER BY YEAR,MONTH,DAY'

		PRINT,'SQL���:'
		PRINT,SQL

		oRS = OBJ_NEW('IDLdbRecordset', oD, SQL=SQL)
		PRINT,'AAA'
		COUNT=0

		REPEAT BEGIN
			ARR_PLOWLAND[COUNT] = ORS -> GetField(0)
			ARR_PADDY_FIELD[COUNT] = ORS -> GetField(1)
			ARR_DRY_LAND[COUNT] = ORS -> GetField(2)
			ARR_YEAR[COUNT] = ORS -> GetField(3)
			ARR_YEAR[COUNT] = ORS -> GetField(4)
			ARR_YEAR[COUNT] = ORS -> GetField(5)
			COUNT=COUNT+1
		ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		;modified 20070425
		;aa
		IF(COUNT EQ 0) THEN BEGIN
			MSG=DIALOG_MESSAGE('û���ҵ���Ӧ������!',TITLE='��ʾ',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF

		IF(COUNT LT NI_INPUT) THEN BEGIN
			TEMP='��ѡ���ʱ�䷶Χ�ڵ����ݲ�ȫ,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
			TEMP=TEMP+'������ѡ��ʱ�䷶Χ�ٽ�������'
			MSG=DIALOG_MESSAGE(TEMP,TITLE='��ʾ',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF
		;Ϊ����Ӧ�㷨,������һ����һ������
		ARR_PLOWLAND_TEMP=FLTARR(COUNT+1)
		ARR_PLOWLAND_TEMP[1:COUNT]=ARR_PLOWLAND[0:COUNT-1]

		ARR_PADDY_FIELD_TEMP=FLTARR(COUNT+1)
		ARR_PADDY_FIELD_TEMP[1:COUNT]=ARR_PADDY_FIELD[0:COUNT-1]

		ARR_DRY_LAND_TEMP=FLTARR(COUNT+1)
		ARR_DRY_LAND_TEMP[1:COUNT]=ARR_DRY_LAND[0:COUNT-1]


		;�ֶ������������������Ŵ�����Ľ��
		ARR_PLOWLAND_HANTS=FLTARR(COUNT+1)
		ARR_PADDY_FIELD_HANTS=FLTARR(COUNT+1)
		ARR_DRY_LAND_HANTS=FLTARR(COUNT+1)


		;�����ݽ���HANTS����
		;********************************************************************************************
		;�����������е�HANTS������Ҫʹ�õĲ���,
		NI=COUNT
		FET=TOLERANCE
		NF=FREQUENCY
		IDRT=MENXIAN
		SCALE=1
		PER=NI*10
		DOD=DOD
		HILO='LO'
		TS=FINDGEN(NI+1)*10
		PA=INDGEN(10)
		FOR I=1,NF DO BEGIN
			PA[i]=PER/I
		ENDFOR


		;�����ﶨ���Ҫ������,
		;�����ó���������ߵ��ع�,�����ع���������.
		AMP=FLTARR((NF+1)+1)
		PHI=FLTARR((NF+1)+1)
		;Y=O_DATA*10

		Y=ARR_PLOWLAND_TEMP*100+10
		;aa
		ARR_PLOWLAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
		Y=ARR_PADDY_FIELD_TEMP*100+10
		ARR_PADDY_FIELD_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
		print,'a'
		Y=ARR_DRY_LAND_TEMP*100+10
		ARR_DRY_LAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
		;********************************************************************************************

		;********************************************************************************************
		;����һ������������һ�������ļ�¼(��������,�ֱ��Ǹ���\ˮ��\����),�����˴�����˵����ݺ�ԭʼ����
		PRINT,ARR_PLOWLAND_HANTS
		PRINT,ARR_PADDY_FIELD_HANTS
		PRINT,ARR_DRY_LAND_HANTS

		ARR_SHOW=STRARR(2*NI_INPUT+3,3)

		ARR_SHOW[0,0]=RANGE
		ARR_SHOW[1,0]=RANGE_CODE
		ARR_SHOW[2,0]='����'
		ARR_SHOW[3:NI+2,0]=ARR_PLOWLAND_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,0]=ARR_PLOWLAND[0:NI-1]

		ARR_SHOW[0,1]=RANGE
		ARR_SHOW[1,1]=RANGE_CODE
		ARR_SHOW[2,1]='ˮ��'
		ARR_SHOW[3:NI+2,1]=ARR_PADDY_FIELD_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,1]=ARR_PADDY_FIELD[0:NI-1]

		ARR_SHOW[0,2]=RANGE
		ARR_SHOW[1,2]=RANGE_CODE
		ARR_SHOW[2,2]='����'
		ARR_SHOW[3:NI+2,2]=ARR_DRY_LAND_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,2]=ARR_DRY_LAND[0:NI-1]
		WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_XSIZE=2*NI_INPUT+3
		WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_YSIZE=3
		WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_VALUE=ARR_SHOW

		PRINT,'COUNT',COUNT
		(*PSTATE).NI=COUNT
		(*PSTATE).HANTS_DONE=1
	ENDELSE
	;**********************************************************************************;


	;���û���ҵ��㹻�ļ�¼
	;�򲻽��м���,������������ж�
    IF(COUNT EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE('û���ҵ���Ӧ������!',TITLE='��ʾ',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	ENDIF
    ;********************************************************************************************
    RETURN,1

END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
FUNCTION DB_WRITE_HANTS, PSTATE
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	PRINT,'��ʼ����������'
	;���ȼ�����ݿ�������Ƿ�ɹ�,�粻�ɹ��򲻽�������
	;WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
    IF (yesORno EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('�����������ݿ�����!',TITLE='��ʾ',/information)
     	CLOSE,/ALL
     	RETURN,0
    ENDIF

    ;2)��HANTS�������еĲ������м��
	IF((*PSTATE).HANTS_DONE EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE('���Ƚ���г���������ع�!',TITLE='��ʾ',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	;��ȡһЩ�ع�ʱ����ز���,�������ݿ������

    OD=DBobj


     WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,GET_VALUE=ARR_TABLE


	;����һ���ṹ��,��Ҫ��������д��һ���ṹ������
	RESULT = { $
    	COUNTY_CODE	:	''		, $
    	YEAR 		:  	0		, $
    	MONTH 		:  	0		, $
    	DAY  		:  	0		, $
    	PLOWLAND	:	0.0		, $
    	PADDY_FIELD	:	0.0		, $
    	DRY_LAND	:	0.0		, $

    	period		:	0		, $
    	SENSOR_CODE	:	''		 $
      		}

	;ARR_RESULT=REPLICATE(RESULT, NI)

	;������д��ṹ��

	;��ȡ���ݼ��Ĵ�С
	LINES=(SIZE(ARR_TABLE))[2];���LINES�ŵ�����ʾ�ı��е�����
	LINES=LINES/3
	PRINT,LINES
	;********************************************************
   	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START
	;********************************************************
	PRINT,'HERE I',(*PSTATE).NI
	;RETURN,0
	FOR J=0,LINES-1 DO BEGIN
		;********************************************************
		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('�û���ֹ�˲���',TITLE='��ʾ')
			PROGRESSTIMER->DESTROY ;����������
			RETURN,0
		ENDIF
		PROGRESSTIMER->UPDATE, (FLOAT(J)/LINES * 100.0) ;��������
		;********************************************************
		NI=(*PSTATE).NI
		FOR I=0,NI-1 DO BEGIN

			RESULT.COUNTY_CODE	=	STRTRIM(ARR_TABLE[1,J*3])
			RESULT.YEAR			=	(*PSTATE).ARR_YEAR_DATA[I]
			RESULT.MONTH		=	(*PSTATE).ARR_MONTH_DATA[I]
			RESULT.DAY			=	(*PSTATE).ARR_DAY_DATA[I]
			RESULT.PLOWLAND		=	ARR_TABLE[I+3,0+J*3]
			RESULT.PADDY_FIELD	=	ARR_TABLE[I+3,1+J*3]
			RESULT.DRY_LAND		=	ARR_TABLE[I+3,2+J*3]
			RESULT.SENSOR_CODE	=	STRTRIM((*PSTATE).SENSOR_CODE,2)
			RESULT.period		=	(*PSTATE).period
			;PRINT,RESULT

			;ɾ�����ݿ�����Ҫ����ļ�¼ʱ��������ͬ�ļ�¼
			SQL='DELETE FROM PARAMETER_PROCESS_COUNTY WHERE ( '
			SQL=SQL+'(COUNTY_CODE = '+"'"+RESULT.COUNTY_CODE+"' "+')'
  			SQL=SQL+' AND (YEAR = '+STRTRIM(STRING(RESULT.YEAR),2)+')'
  			SQL=SQL+' AND (MONTH = '+STRTRIM(STRING(RESULT.MONTH),2)+')'
  			SQL=SQL+' AND (DAY = '+STRTRIM(STRING(RESULT.DAY),2)+')'
  			SQL=SQL+' AND (DATA_TYPE = '+"'"+STRTRIM('NDVI_H',2)+"'"+')'
  			SQL=SQL+' AND (periods = '+STRTRIM(STRING(RESULT.period),2)+')'
  			SQL=SQL+' AND (SENSOR_CODE = '+"'"+RESULT.SENSOR_CODE+"'"+'))'
  			;PRINT,SQL
  			OD->EXECUTESQL,SQL

			;ɾ�����ٲ����µļ�¼
			SQL='INSERT INTO PARAMETER_PROCESS_COUNTY '
			SQL=SQL+'(COUNTY_CODE ,YEAR ,MONTH ,DAY,'
			SQL=SQL+'periods,DATA_TYPE,SENSOR_CODE,'
			SQL=SQL+'avg_plowland ,avg_paddy_field,avg_dry_land,'
			SQL=SQL+'AVG_SPRING_WHEAT,AVG_WINTER_WHEAT,'
			SQL=SQL+'AVG_EARLY_RICE,AVG_SEMILATE_RICE,AVG_LATE_RICE,'
			SQL=SQL+'AVG_MAIZE,AVG_SOYBEAN ) '
			SQL=SQL+'VALUES ('
			SQL=SQL+"'"+STRTRIM(STRING(RESULT.COUNTY_CODE),2)	+"'"+','
			SQL=SQL+STRTRIM(STRING(RESULT.YEAR),2)				+','
			SQL=SQL+STRTRIM(STRING(RESULT.MONTH),2)				+','
			SQL=SQL+STRTRIM(STRING(RESULT.DAY),2)				+','
			SQL=SQL+STRTRIM(STRING(RESULT.period),2)			+','
			SQL=SQL+"'"+STRTRIM('NDVI_H',2)			+"'"+','
			SQL=SQL+"'"+STRTRIM(STRING(RESULT.SENSOR_CODE),2)	+"'"+','
			SQL=SQL+STRTRIM(STRING(RESULT.PLOWLAND),2)			+','
			SQL=SQL+STRTRIM(STRING(RESULT.PADDY_FIELD),2)		+','
			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)			+','
			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)			+','
			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)			+','
			SQL=SQL+STRTRIM(STRING(RESULT.PADDY_FIELD),2)		+','
			SQL=SQL+STRTRIM(STRING(RESULT.PADDY_FIELD),2)		+','
			SQL=SQL+STRTRIM(STRING(RESULT.PADDY_FIELD),2)		+','
			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)			+','
			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)			+')'
;			SQL='INSERT INTO NDVI_TENDAY_COUNTY_HANTS'
;			SQL=SQL+'("COUNTY_CODE" ,"YEAR" ,"MONTH" ,"DAY"'
; 			SQL=SQL+',"PLOWLAND","PADDY_FIELD","DRY_LAND","SENSOR_TYPE" ) '
; 			SQL=SQL+'VALUES ('
; 			SQL=SQL+"'"+STRTRIM(STRING(RESULT.COUNTY_CODE),2)	+"'"+','
; 			SQL=SQL+STRTRIM(STRING(RESULT.YEAR),2)			+','
; 			SQL=SQL+STRTRIM(STRING(RESULT.MONTH),2)			+','
; 			SQL=SQL+STRTRIM(STRING(RESULT.XUN),2)			+','
; 			SQL=SQL+STRTRIM(STRING(RESULT.PLOWLAND),2)		+','
; 			SQL=SQL+STRTRIM(STRING(RESULT.PADDY_FIELD),2)	+','
; 			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)		+','
; 			SQL=SQL+"'"+STRTRIM(STRING(RESULT.SENSOR_TYPE),2)	+"'"+')'
 			;PRINT,SQL

 			OD->EXECUTESQL,SQL
		ENDFOR
	ENDFOR
	;********************************************************
	PROGRESSTIMER->DESTROY ;
	;********************************************************
	MSG=DIALOG_MESSAGE('����װ�����',TITLE='���',/INFORMATION)
 	RETURN,1

END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
PRO TABLE_RESULT_HANTS0, EVENT
	if event.type eq 7 then return
	;print,event
	IF (EVENT.SEL_LEFT EQ -1) THEN RETURN
	WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
	IF ((*PSTATE).HANTS_DONE EQ 0) THEN RETURN
	;HELP,EVENT,/STRUCTURE
	TOP=EVENT.SEL_TOP
	NI=(*PSTATE).NI
	PRINT,'NI',NI
	WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_TABLE_SELECT=[0, top,(*PSTATE).NI*2+3-1,TOP]
	WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,GET_VALUE=ARR_RESULT
	PRINT,ARR_RESULT[*,TOP]
	ARR_H=ARR_RESULT[3:NI+3-1,TOP]
	ARR_O=ARR_RESULT[3+NI:NI*2+3-1,TOP]
	DRAW_HANTS=(*PSTATE).DRAW_HANTS

	;********************************************************************************************
	;��ѡ��Ľ����ͼ

	WIDGET_CONTROL,(*PSTATE).DRAW_HANTS,GET_VALUE=DRAW_HANTS
	WSET, DRAW_HANTS
	white=!D.N_COLORS-1

	;***********************************************
	device, set_font='����', /TT_FONT

	;����ԭʼ������
	X=FINDGEN(NI)*10
	PLOT,X,ARR_H,BACKGROUND=WHITE,color=0,XMARGIN=[7,2],YMARGIN=[3,1],PSYM=1,SYMSIZE=0.6$
		,XTITLE='��λ:��',YRANGE=[0.1,0.9],FONT=2,charsize=1
	XYOUTS,6,150,'ֲ',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,120,'��',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,90	,'ָ',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,60	,'��',FONT=2,charsize=1,COLOR=0,/DEVICE

	OPLOT,X,ARR_O,COLOR=0
	;�����ع��������
	OPLOT,X,ARR_H,PSYM=2,SYMSIZE=0.6,COLOR=255
	OPLOT,X,ARR_H,COLOR=255

	;��ϵͳ�������������,�Ա���к��ֵ���ʾ
	;DEVICE, SET_FONT = 'kai'
	;��ԭʼ���ߵ�ͼ��
	PLOTS,30,270,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,40,270,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,50,270,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,60,270,COLOR=0, /DEVICE ,/CONTINUE,PSYM=1,SYMSIZE=0.6
	PLOTS,30,270,COLOR=0,/DEVICE
	PLOTS,60,270,COLOR=0, /DEVICE ,/CONTINUE
	XYOUTS,65,266,'Original Curve',/DEVICE,COLOR=0


	;���ع�������ߵ�ͼ��
	PLOTS,180,270,COLOR=255+256L*(0L+0*256L),PSYM=2,SYMSIZE=0.6,/DEVICE
	PLOTS,190,270,COLOR=255+256L*(0L+0*256L),PSYM=2,SYMSIZE=0.6,/DEVICE
	PLOTS,200,270,COLOR=255+256L*(0L+0*256L),PSYM=2,SYMSIZE=0.6,/DEVICE
	PLOTS,210,270,COLOR=255+256L*(0L+0*256L), /DEVICE ,/CONTINUE,PSYM=2,SYMSIZE=0.6
	PLOTS,180,270,COLOR=255+256L*(0L+0*256L),/DEVICE
	PLOTS,210,270,COLOR=255+256L*(0L+0*256L), /DEVICE ,/CONTINUE
	XYOUTS,215,266,'HANTS Curve',/DEVICE,COLOR=255+256L*(0L+0*256L)

END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO HANTS_EVENTCB
END
;
; IDL WIDGET INTERFACE PROCEDURES. THIS CODE IS AUTOMATICALLY
;     GENERATED AND SHOULD NOT BE MODIFIED.

;
; GENERATED ON:	12/13/2004 16:49.27
;
PRO BASE_TOP_HANTS_EVENT, EVENT

  WTARGET = (WIDGET_INFO(EVENT.ID,/NAME) EQ 'TREE' ?  $
      WIDGET_INFO(EVENT.ID, /TREE_ROOT) : EVENT.ID)

  WWIDGET =  EVENT.TOP
  WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
  CASE WTARGET OF

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_YEAR_START_HANTS'): BEGIN
     	(*PSTATE).YEAR_START=(*PSTATE).ARR_YEAR[EVENT.INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_MONTH_START_HANTS'): BEGIN
     	(*PSTATE).MONTH_START=(*PSTATE).ARR_MONTH[EVENT.INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_DAY_START_HANTS'): BEGIN
     	(*PSTATE).DAY_START=(*PSTATE).ARR_DAY[EVENT.INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_YEAR_END_HANTS'): BEGIN
     	(*PSTATE).YEAR_END=(*PSTATE).ARR_YEAR[EVENT.INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_MONTH_END_HANTS'): BEGIN
     	(*PSTATE).MONTH_END=(*PSTATE).ARR_MONTH[EVENT.INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_DAY_END_HANTS'): BEGIN
     	(*PSTATE).DAY_END=(*PSTATE).ARR_DAY[EVENT.INDEX]
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_RANGE_HANTS'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).RANGE=(*PSTATE).ARR_RANGE[INDEX]
	    (*PSTATE).RANGE_CODE=(*PSTATE).ARR_RANGE_CODE[INDEX]
	    PRINT,(*PSTATE).RANGE,(*PSTATE).RANGE_CODE
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_SENSOR_HANTS'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).SENSOR_TYPE=(*PSTATE).ARR_SENSOR_TYPE[INDEX]
	    (*PSTATE).SENSOR_CODE=(*PSTATE).ARR_SENSOR_CODE[INDEX]
	    PRINT,(*PSTATE).SENSOR_TYPE,(*PSTATE).SENSOR_CODE
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_period_HANTS'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).period=(*PSTATE).ARR_DAY[INDEX]
	    PRINT,(*PSTATE).period
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_CLOSE_HANTS'): BEGIN
    	common_log,'�رչ������ع�'
    	CLOSE,/ALL
     	WIDGET_CONTROL, EVENT.TOP, /DESTROY
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='TABLE_RESULT_HANTS'): BEGIN
      	IF( TAG_NAMES(EVENT, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' )THEN $
        	TABLE_RESULT_HANTS0, EVENT    ;���н������޸ģ�ԭ����Ϊ TABLE_RESULT_HANTS, EVENT�������ɣ�20070320
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_OK_HANTS'): BEGIN
      	PRINT,'A'
      	TEMP=HANTS_DO(PSTATE)
      	IF(TEMP EQ 1) THEN BEGIN
      		TEMP=DIALOG_MESSAGE('�����ع��ɹ�',TITLE='�ɹ�')
      		TABLE_RESULT_HANTS0, {ID:(*PSTATE).TABLE_RESULT_HANTS, TOP:event.top, TYPE:4, SEL_LEFT:0, SEL_TOP:0}
      		log, '���̼��-�����ع�', 0
      	ENDIF ELSE BEGIN
      		TEMP=DIALOG_MESSAGE('�����ع�ʧ��',TITLE='ʧ��')
      		log, '���̼��-�����ع�', -1
      	ENDELSE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_DB_WRITE_HANTS'): BEGIN
      	TEMP=DB_WRITE_HANTS(PSTATE)
      	IF(TEMP EQ 1) THEN BEGIN
      		TEMP=DIALOG_MESSAGE('�������ɹ�',TITLE='�ɹ�')
      		log, '���̼��-�����ع�', 1
      	ENDIF ELSE BEGIN
      		TEMP=DIALOG_MESSAGE('�������ʧ��',TITLE='ʧ��')
      		log, '���̼��-�����ع�', -1
      	ENDELSE
    END
    ELSE:
  ENDCASE

END

pro slider_event, event
	WIDGET_CONTROL, event.top, GET_UVALUE=PSTATE
	slider = widget_info(event.id, /UNAME)
	widget_control, event.id, GET_VALUE=vv

	case slider of
		'TOLERANCE':begin
			(*PSTATE).TXT_TOLERANCE_HANTS = vv
		end
		'DOD':begin
			(*PSTATE).TXT_DOD_HANTS = vv
		end
		'FREQUENCY':begin
			(*PSTATE).TXT_FREQUENCY_HANTS = vv
		end
		else:begin
		end
	endcase
end

PRO BASE_TOP_HANTS,GROUP_LEADER=WGROUP

  RESOLVE_ROUTINE, 'HANTS_EVENTCB',/COMPILE_FULL_FILE  ; LOAD EVENT CALLBACK ROUTINES

	common current_date, c_year, c_month, c_day

  BASE_TOP_HANTS = WIDGET_BASE( GROUP_LEADER=WGROUP,  $
      UNAME='BASE_TOP_HANTS' ,XOFFSET=200 ,YOFFSET=200  $
      ,TITLE='���̼��--�����ع�' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
      ,TLB_FRAME_ATTR=1,/row)


  BASE_INPUT_HANTS = WIDGET_BASE(BASE_TOP_HANTS,  $
      UNAME='BASE_INPUT_HANTS' ,FRAME=1 ,XOFFSET=8 ,YOFFSET=8  $
      ,SCR_XSIZE=231 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)


  BASE_DATE_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_DATE_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=7  $
      ,SCR_XSIZE=217 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3, /column)

	subbase = widget_base(BASE_DATE_HANTS,/row,xpad=3,space=3)

  LBL_DATE_START_HANTS = WIDGET_LABEL(subbase,  $
      UNAME='LBL_DATE_START_HANTS' ,XOFFSET=11 ,YOFFSET=8  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=16 ,/ALIGN_LEFT ,VALUE='��ʼʱ��:  ')

  	WID_LABEL_date_s = Widget_text(subbase,xsize=12, $
		VALUE=strtrim(c_year,2)+'-'+strtrim('1',2)+'-'+strtrim('1',2))
 	CMD_pick_date_s = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date_s, pointer:PTR_NEW(), detail:'CMD_pick_date_s'})

    subbase = widget_base(BASE_DATE_HANTS,/row,xpad=3,space=3)

   LBL_DATE_END_HANTS = WIDGET_LABEL(subbase,  $
      UNAME='LBL_DATE_END_HANTS' ,XOFFSET=11 ,YOFFSET=60  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=16 ,/ALIGN_LEFT ,VALUE='����ʱ��:')

  	WID_LABEL_date_e = Widget_text(subbase,xsize=12, $
		VALUE=strtrim(c_year,2)+'-'+strtrim('12',2)+'-'+strtrim('31',2))
 	CMD_pick_date_e = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date_e, pointer:PTR_NEW(), detail:'CMD_pick_date_e'} )


    BASE_PLACE_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_PLACE_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=129  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=119 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_CHOOSE_PARAMETER_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_CHOOSE_PARAMETER_HANTS' ,XOFFSET=8 ,YOFFSET=10 ,SCR_XSIZE=139  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��ѡ�����ݵĲ���:')


  LBL_RANGE_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_RANGE_HANTS' ,XOFFSET=8 ,YOFFSET=33+3 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='����  :')


  DST_RANGE_HANTS = WIDGET_COMBOBOX(BASE_PLACE_HANTS,  $
      UNAME='DST_RANGE_HANTS' ,XOFFSET=60 ,YOFFSET=33 ,SCR_XSIZE=140  $
      ,SCR_YSIZE=23 )

  ;**********************************************************************************
  TEMP=27
  LBL_SENSOR_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_SENSOR_HANTS' ,XOFFSET=8 ,YOFFSET=33+3+TEMP ,SCR_XSIZE=45  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='������:')


  DST_SENSOR_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_SENSOR_HANTS' ,XOFFSET=60 ,YOFFSET=33+TEMP ,SCR_XSIZE=140  $
      ,SCR_YSIZE=23 )

  LBL_period_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_period_HANTS' ,XOFFSET=8 ,YOFFSET=33+3+TEMP*2 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='����  :')


  DST_period_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_period_HANTS' ,XOFFSET=60 ,YOFFSET=33+TEMP*2 ,SCR_XSIZE=140  $
      ,SCR_YSIZE=23 )

BASE_PARAMETER_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_PARAMETER_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=255  $
      ,SCR_XSIZE=217 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)

  BASE_PARAMETER_HANTS_RX = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_TOLERANCE_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_RX,/ALIGN_LEFT ,VALUE='����:')
  TXT_TOLERANCE_HANTS = widget_slider(BASE_PARAMETER_HANTS_RX,VALUE=5,MAXIMUM=500, MINIMUM=0,XSIZE=155,event_pro='slider_event',uname='TOLERANCE')

  BASE_PARAMETER_HANTS_DOD = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_DOD_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_DOD,/ALIGN_LEFT ,VALUE='DOD :')
  TXT_DOD_HANTS = widget_slider(BASE_PARAMETER_HANTS_DOD,VALUE=2,MAXIMUM=100, MINIMUM=1,XSIZE=155,event_pro='slider_event',uname='DOD')

  BASE_PARAMETER_HANTS_PL = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_FREQUENCY_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_PL,/ALIGN_LEFT ,VALUE='Ƶ��:')
  TXT_FREQUENCY_HANTS = widget_slider(BASE_PARAMETER_HANTS_PL,VALUE=2,MAXIMUM=4, MINIMUM=1,XSIZE=155,event_pro='slider_event',uname='FREQUENCY')

  BASE_CMD_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_CMD_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=322  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=38 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  CMD_OK_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS, UNAME='CMD_OK_HANTS'  $
      ,XOFFSET=7 ,YOFFSET=6 ,SCR_XSIZE=60 ,SCR_YSIZE=24  $
      ,/ALIGN_CENTER ,VALUE='�����ع�')


  CMD_CLOSE_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS,  $
      UNAME='CMD_CLOSE_HANTS' ,XOFFSET=149 ,YOFFSET=6 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
      ,VALUE='�ر�')


  CMD_DB_WRITE_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS,  $
      UNAME='CMD_DB_WRITE_HANTS' ,XOFFSET=80 ,YOFFSET=6 ,SCR_XSIZE=56  $
      ,SCR_YSIZE=24  ,/ALIGN_CENTER  $
      ,VALUE='�������')

  ;**********************************************************************************


  BASE_DRAW_HANTS = WIDGET_BASE(BASE_TOP_HANTS,  $
      UNAME='BASE_DRAW_HANTS' ,FRAME=1  $
      ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/col)


  LBL_DRAW_HANTS = WIDGET_LABEL(BASE_DRAW_HANTS,  $
      UNAME='LBL_DRAW_HANTS' ,XOFFSET=117 ,YOFFSET=2 ,SCR_XSIZE=128  $
      ,SCR_YSIZE=14 ,/ALIGN_center ,VALUE='������Ͻ����ʾ')


  DRAW_HANTS = WIDGET_DRAW(BASE_DRAW_HANTS, UNAME='DRAW_HANTS'  $
      ,FRAME=1 ,YOFFSET=17 ,SCR_XSIZE=400 ,SCR_YSIZE=210)


  TABLE_RESULT_HANTS = WIDGET_TABLE(BASE_DRAW_HANTS,/RESIZEABLE_COLUMNS,  $
      UNAME='TABLE_RESULT_HANTS' ,SCR_XSIZE=400 ,SCR_YSIZE=145  $
       ,EVENT_PRO='TABLE_RESULT_HANTS0',XSIZE=6 ,YSIZE=6,/ALL_EVENTS)

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
  NUM_MAX=80

year_droplist=strtrim(string(indgen(36)+1980),2)

  STATE = { $


        TXT_TOLERANCE_HANTS	:	5	, $
        TXT_MENXIAN_HANTS	:	-5	, $
        TXT_DOD_HANTS		:	2	, $
        TXT_FREQUENCY_HANTS	:	2	, $

        ARR_RANGE			:	STRARR(150)		,$
        ARR_RANGE_CODE		:	STRARR(150)		,$

		RANGE				:	'��ѡ��'		,$
		RANGE_CODE			:	''		,$
		period				:	1		,$



		ARR_YEAR_DATA		:INTARR(NUM_MAX)	,$
		ARR_MONTH_DATA		:INTARR(NUM_MAX)	,$
		ARR_DAY_DATA		:INTARR(NUM_MAX)	,$

;        ARR_YEAR			:	[ '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998','1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006',$
;        						'2007', '2008', '2009', '2010', '2011', '2012', '2013'] ,$
		ARR_YEAR		:	year_droplist , $

		ARR_MONTH			:	[ '1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12' ],$
		ARR_DAY				:	['1', '2', '3','4', '5', '6', '7', '8', '9', '10', $
								'11', '12', '13','14', '15', '16', '17', '18', '19', '20',$
								'21', '22', '23','24', '25', '26', '27', '28', '29', '30', '31'] ,$

		ARR_SENSOR_TYPE		:	['AVHRR','MODIS','VGT'] ,$
    	ARR_SENSOR_CODE		:	[1,2,3] ,$

    	SENSOR_TYPE			:	'AVHRR',$
    	SENSOR_CODE			:	1,$

    	YEAR_START			:	1980	,$
		MONTH_START			:	1		,$
		DAY_START			:	1		,$
		YEAR_END			:	1980	,$
		MONTH_END			:	1		,$
		DAY_END				:	1		,$

        NUM_OF_RANGE		:	0		,$

		HANTS_DONE			:	0		, $

        NI					:	0		, $
        ARR_HANTS			:	FLTARR(36),	$

		BASE_TOP_HANTS		:	BASE_TOP_HANTS		,$
		DRAW_HANTS			:	DRAW_HANTS			,$
		TABLE_RESULT_HANTS	:	TABLE_RESULT_HANTS	,$
		DST_RANGE			:	DST_RANGE_HANTS		$

;		DBCO_ID				:	yesORno	, $
;       	DBCO				:	OBJ_NEW('IDLDBDATABASE')		$
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, BASE_TOP_HANTS, SET_UVALUE=PSTATE

	WIDGET_CONTROL, CMD_pick_date_s, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_s, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_e, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_e, set_uvalue=staff

   	;************************************************************************
  	;�������б�ֵ,�е�������Ҫ�����ݿ��ж�ȡ
;  	(*PSTATE).DBCO=DBobj
;  	WIDGET_CONTROL,DST_YEAR_END_HANTS	,SET_VALUE=(*PSTATE).ARR_YEAR
;  	WIDGET_CONTROL,DST_YEAR_START_HANTS	,SET_VALUE=(*PSTATE).ARR_YEAR
;  	WIDGET_CONTROL,DST_MONTH_END_HANTS	,SET_VALUE=(*PSTATE).ARR_MONTH
;  	WIDGET_CONTROL,DST_MONTH_START_HANTS,SET_VALUE=(*PSTATE).ARR_MONTH
;  	WIDGET_CONTROL,DST_DAY_END_HANTS	,SET_VALUE=(*PSTATE).ARR_DAY
;  	WIDGET_CONTROL,DST_DAY_START_HANTS	,SET_VALUE=(*PSTATE).ARR_DAY
  	WIDGET_CONTROL,DST_period_HANTS		,SET_VALUE=(*PSTATE).ARR_DAY
  	WIDGET_CONTROL,DST_SENSOR_HANTS		,SET_VALUE=(*PSTATE).ARR_SENSOR_TYPE

  	;************************************************************************
  	;�����ݿ��а��ص��б����ݶ�ȡ����,����������ʾ�������б���
  	;SQL='select CODE,NAME from COUNTY_CODE WHERE (PROVINCE_CODE = '+"'"+PROVINCE_code+"' "+')'
  	;��ԭ����SQL�������˵���,Ϊ�˱�֤����COUNTY_CODE�ڲ������ж�������,
  	;��COUNTY_CODE_RASTER�н��������ݵĶ�ȡ
  	SQL='select T1.CODE,T2.NAME from COUNTY_CODE_RASTER T1,COUNTY_CODE T2 WHERE T1.CODE=T2.CODE '
  	SQL=SQL+'ORDER BY T2.CODE'
    PRINT,SQL
    oRS = OBJ_NEW('IDLdbRecordset', DBobj, SQL=SQL)


	;��ȡ���ݼ�¼�ĸ���
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM_COUNTY=RecordNum
	Obj_Destroy,RecordNumOBJ
	PRINT,NUM_COUNTY

	;�����ʡ����ʼ�ĸ�����2,������0
	;������Ϊ��һ��ѡ����'��ѡ��',�ڶ���ѡ����'������'
	num_of_COUNTY=NUM_COUNTY+2

   	IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

		;Ȼ�󽫹��ҵ����ƶ�������
		ARR_COUNTY		=STRARR(num_of_COUNTY)
		ARR_COUNTY_CODE	=STRARR(num_of_COUNTY)
		ARR_COUNTY[0]='��ѡ��'
		ARR_COUNTY[1]='������(������)'
		TEMP=oRS->MoveCursor(/FIRST)
		COUNT=1
		REPEAT BEGIN
			COUNT=COUNT+1
			ARR_COUNTY[COUNT]		=ORS->GetField(1)
			ARR_COUNTY_CODE[COUNT]	=ORS->GetField(0)
		ENDREP UNTIL (oRS->MoveCursor(/next) NE 1)

  		WIDGET_CONTROL,(*pstate).DST_RANGE,SET_VALUE=ARR_COUNTY
  		(*PSTATE).ARR_RANGE=ARR_COUNTY
  		(*PSTATE).ARR_RANGE_CODE=ARR_COUNTY_CODE
  		(*PSTATE).RANGE=ARR_COUNTY[0]
  		(*PSTATE).RANGE_CODE=ARR_COUNTY_CODE[0]
  		(*PSTATE).NUM_OF_RANGE=num_of_COUNTY
  		PRINT,(*PSTATE).NUM_OF_RANGE
   	ENDIF
   	obj_destroy,oRS
  	;************************************************************************
  	WIDGET_CONTROL, /REALIZE, BASE_TOP_HANTS
  	WIDGET_CONTROL,CMD_CLOSE_HANTS,/INPUT_FOCUS

	;***********************************************************************
	  ;Ϊ��ϵͳ��ʾ�����ӵĴ���
	  TEMP=1
	  IF(TEMP EQ 1)THEN BEGIN

			time_current=bin_date()
	  		year_index=time_current[0]-1980
	  		month_index=time_current[1]-1
	  		day_index=time_current[2]-1

;		  	WIDGET_CONTROL,DST_YEAR_END_HANTS, SET_COMBOBOX_SELECT=year_index
;		  	WIDGET_CONTROL,DST_YEAR_START_HANTS, SET_COMBOBOX_SELECT=year_index
;
;		  	WIDGET_CONTROL,DST_MONTH_START_HANTS, SET_DROPLIST_SELECT=0
;		  	WIDGET_CONTROL,DST_MONTH_END_HANTS, SET_DROPLIST_SELECT=11
;
;		  	WIDGET_CONTROL,DST_DAY_START_HANTS, SET_DROPLIST_SELECT=0
;		  	WIDGET_CONTROL,DST_DAY_END_HANTS, SET_DROPLIST_SELECT=30

		  	WIDGET_CONTROL,DST_PERIOD_HANTS, SET_DROPLIST_SELECT=9
		  	WIDGET_CONTROL,DST_RANGE_HANTS, SET_COMBOBOX_SELECT=0

;		  	(*PSTATE).YEAR_START	=FIX(strtrim(time_current[0],2))
;			(*PSTATE).MONTH_START	=1
;			(*PSTATE).DAY_START		=1
;			(*PSTATE).YEAR_END		=FIX(strtrim(time_current[0],2))
;			(*PSTATE).MONTH_END		=12
;			(*PSTATE).DAY_END		=31

			(*PSTATE).YEAR_START =	strtrim(c_year,2)
		  	(*PSTATE).MONTH_START=	strtrim('1',2)
		  	(*PSTATE).DAY_START	 =	strtrim('1',2)
		  	(*PSTATE).YEAR_END =	strtrim(c_year,2)
		  	(*PSTATE).MONTH_END=	strtrim('12',2)
		  	(*PSTATE).DAY_END	 =	strtrim('31',2)

			(*PSTATE).PERIOD		=10
			(*PSTATE).RANGE=ARR_COUNTY[0]
			(*PSTATE).RANGE_CODE=ARR_COUNTY_CODE[0]

		  	PRINT,(*PSTATE).RANGE



	  ENDIF
  	;***********************************************************************
  	XMANAGER, 'BASE_TOP_HANTS', BASE_TOP_HANTS, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'
  	white=!D.N_COLORS-1
  	WIDGET_CONTROL,DRAW_HANTS,GET_VALUE=TMEP
	WSET,TMEP
	TEMP=INDGEN(2)
	PLOT,TEMP,BACKGROUND=WHITE
	;************************************************************************

END
;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO ZS_HANTS,GROUP_LEADER=WGROUP
	common_log,'�����������ع�'
  ;����ͬһ�������ظ�����
  IF ( XREGISTERED('BASE_TOP_HANTS') NE 0 ) THEN RETURN
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_HANTS,GROUP_LEADER=BASE_TOP
END
