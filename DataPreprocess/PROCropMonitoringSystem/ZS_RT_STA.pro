;
; �ɼ̻�
; ���������صĲ���ͳ��,ʵʱͳ��,ͳ�Ʋ�ͬ�ȼ���ռ�İٷֱ�
; 2006.08.29
; ����ԭ������ͳ�ƺ�ȫ��ͳ��ģ��Ļ����Ͻ��иĽ����ɵ�
; ���ؼ�������Ҳ�������_PRO,û�иĳ�_RT
;-----------------------------------------------------------------


;-----------------------------------------------------------------
;����ϵͳ��������
;�����ע���Ǻ����ĵ�����ʽ

FUNCTION STAT_RT_COUNTY	,FILE_IN,YEAR,MONTH,DAY	$
						,SENSOR_CODE,DATA_TYPE	$
						,PERIOD,DBCO
	PRINT,'***********************************************'
	PRINT,'��ʡͳ��,��������:'
	PRINT,'FILE_IN:',FILE_IN
	PRINT,'DATE:',YEAR,MONTH,DAY
	PRINT,'DATA,SENSOR TYPE:',SENSOR_CODE,DATA_TYPE
	PRINT,'PERIOD:',PERIOD
	PRINT,'***********************************************'

	FILE_PLOWLAND	=	'data_grid\PLOWLAND_ratio'
	FILE_DRY_LAND	=	'data_grid\DRY_LAND_ratio'
	FILE_PADDY_FI	=	'data_grid\PADDY_FIELD_ratio'
	FILE_COUNTY		=	'data_grid\county_raster'
	FILE_IN			=	FILE_IN




	;�������������ļ�ͼ����Ϣ

	FILE_PLOWLAND_INFO	=GET_IMAGE_INFO(FILE_PLOWLAND)
	FILE_DRY_LAND_INFO	=GET_IMAGE_INFO(FILE_DRY_LAND)
	FILE_PADDY_FI_INFO	=GET_IMAGE_INFO(FILE_PADDY_FI)

	FILE_COUNTY_INFO	=GET_IMAGE_INFO(FILE_COUNTY)
	FILE_IN_INFO		=GET_IMAGE_INFO(FILE_IN)


	;**************************************************************************************
	;Ȼ�������ͼ����쳣���������
	;ֻҪ�����ļ�����һ������Ч�ļ��򷵻�ͳ�Ʋ��ɹ�
	IF(	(FILE_PLOWLAND_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_DRY_LAND_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_PADDY_FI_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_COUNTY_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_IN_INFO.FILE_RIGHT NE 1)) THEN BEGIN
		PRINT,'��ȡͼ��ͷ��Ϣʱ���ִ���'
		RETURN,0
	ENDIF

	;��������ֱ���һ�µ�Ҫ��
	IF((FILE_IN_INFO.X_PIXELSIZE NE FILE_IN_INFO.Y_PIXELSIZE)) THEN BEGIN
		PRINT,'�������ݵ���������ֱ��ʲ�һ��'
		RETURN,0
	ENDIF

	;�ռ���Ҫ���ص������ж�
	IF((FILE_IN_INFO.STARTX+FILE_IN_INFO.XSIZE*FILE_IN_INFO.X_PIXELSIZE LT FILE_COUNTY_INFO.STARTX) OR $
	   (FILE_IN_INFO.STARTX GT FILE_COUNTY_INFO.STARTX+FILE_COUNTY_INFO.XSIZE*FILE_COUNTY_INFO.X_PIXELSIZE)) THEN BEGIN
		PRINT,'�������ݵ����ݺͻ��������ڿռ���û���ص���'
		RETURN,0
	ENDIF
	;**************************************************************************************


	;**************************************************************************************
	;ͨ�����ݵĿռ���Ϣ,����ص��������Ŀռ���Ϣ,���ڽ������ݵĶ�ȡ

	;**************************************************************************************

	X_START_READ=(FILE_IN_INFO.STARTX>FILE_COUNTY_INFO.STARTX)>FILE_PLOWLAND_INFO.STARTX

	X_END_READ	=(FILE_IN_INFO.STARTX+FILE_IN_INFO.XSIZE*FILE_IN_INFO.X_PIXELSIZE)	$
		<(FILE_COUNTY_INFO.STARTX+FILE_COUNTY_INFO.XSIZE*FILE_COUNTY_INFO.X_PIXELSIZE) $
		<(FILE_PLOWLAND_INFO.STARTX+FILE_PLOWLAND_INFO.XSIZE*FILE_PLOWLAND_INFO.X_PIXELSIZE)

	Y_START_READ=(FILE_IN_INFO.STARTY<FILE_COUNTY_INFO.STARTY)<FILE_PLOWLAND_INFO.STARTY

	;��γ�ȵ��������µ�����Խ��Խ��,��������ͺ������õķ����෴
	Y_END_READ	=(FILE_IN_INFO.STARTY-FILE_IN_INFO.YSIZE*FILE_IN_INFO.Y_PIXELSIZE)	$
		>(FILE_COUNTY_INFO.STARTY-FILE_COUNTY_INFO.YSIZE*FILE_COUNTY_INFO.Y_PIXELSIZE) $
		>(FILE_PLOWLAND_INFO.STARTY-FILE_PLOWLAND_INFO.YSIZE*FILE_PLOWLAND_INFO.Y_PIXELSIZE)

	;��ͼ��ķ�Χ�����ж�
	IF(X_START_READ GT X_END_READ)THEN BEGIN
		PRINT,'ͳ�Ƶ���������������û���ص��Ĳ���'
		RETURN,0
	ENDIF
	IF(Y_START_READ LT Y_END_READ)THEN BEGIN
		PRINT,'ͳ�Ƶ���������������û���ص��Ĳ���'
		RETURN,0
	ENDIF

	;********************************************************************************
	;2006.08.11
	;�������ԭ���ĳ���������޸�,��Ҫע�����,����Ҫ���ܽ��ղ�ͬ�ֱ��ʵ�����,
	;ԭ����Ҫ���ܶ��κηֱ��ʵ����ݽ��д���(ʵ�������ﲻ���ܵ�)
	;������Ϊ���л������ݵķֱ��ʶ���һ��,Ŀǰ��1000M,
	;���ʹ�ø߷ֱ��ʵ�����������û��ʲôʵ������,
	;ͬʱΪ�˷������ݴ��������,�ڽ����˹���������ݵĶ�ȡ��
	;������Ĳ����ļ�CONGRID����������ݷֱ�����ͬ������.
	;Ϊ�����ݷ���,Ҳ���Զ����е����ݶ�����CONGRID����,
	;����һ��,��ʹ�������ݵķֱ��ʲ�ͬ,ϵͳҲͬ�����Խ��д���
	;����ͳһ��һ����ͬ�ķֱ���,Ϊ1000M
	;********************************************************************************
	;��������������������ݵĴ�С�ͷֱ�����һ�´ǵ�,ֻѡ����һ�����ص�������Ϊ����
	COMMON_AREA=GET_COMMON_AREA_3(	FILE_IN,	$
									FILE_COUNTY,$
									FILE_PLOWLAND)
	PRINT,COMMON_AREA


	;��ȡϵͳ���صĸ���,ָҪ��դ��ֵ���Ӧ���Ǹ�����
	TABLE_NAME='COUNTY_CODE_RASTER'
	SQL='SELECT CODE,RASTER_VALUE FROM COUNTY_CODE_RASTER ORDER BY RASTER_VALUE'
	ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL)

	;��ȡ���ݼ�¼�ĸ���
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM_COUNTY=RecordNum
	Obj_Destroy,RecordNumOBJ
	PRINT,NUM_COUNTY


	;-------------------------------------------------------------------------------
	;�����д������ڽ���������ʾ
	temp_str='����ͳ��'+STRTRIM(DATA_TYPE,2)+'����'
	temp_str=temp_str+string(byte(13))+string(byte(10))
	temp_str=temp_str+'ʱ��:'+STRTRIM(YEAR,2)+'��'+STRTRIM(MONTH,2)+'��'+STRTRIM(DAY,2)+'��'
	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON,MESSAGE=temp_str,TITLE='ʵʱ���ͳ��')
	PROGRESSTIMER->START
	;-------------------------------------------------------------------------------

	;********************************************************************************
	;�������ڴ洢ͳ�ƽ���Ľṹ��
	RESULT= {	$
		COUNTY_CODE	:	''	,$
		RASTER_VAL	:	''	,$

		FLAG		:	0	,$;һ����ʶ��,������ʶ�����Ƿ�����Ч����

		ARR_POINTS	:	FLTARR(5)	,$;�����洢ÿ���ȼ����ۼ�
		ARR_PER		:	FLTARR(5)	,$;�����洢ÿ���ȼ����ۼ�
		TOTAL_POINTS:	0	$ ;�����洢���еȼ����ۼ�

		}


	ARR_PL	=	REPLICATE(RESULT,NUM_COUNTY)
	ARR_PF	=	REPLICATE(RESULT,NUM_COUNTY)
	ARR_DL	=	REPLICATE(RESULT,NUM_COUNTY)

	;����Щ�洢����Ľṹ�����ݽ��г�ʼ��
	COUNT=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
		REPEAT BEGIN
			ARR_PL[COUNT].COUNTY_CODE= ORS -> GETFIELD(0)
			ARR_PL[COUNT].RASTER_VAL= ORS -> GETFIELD(1)

			ARR_PF[COUNT].COUNTY_CODE= ORS -> GETFIELD(0)
			ARR_PF[COUNT].RASTER_VAL= ORS -> GETFIELD(1)

			ARR_DL[COUNT].COUNTY_CODE= ORS -> GETFIELD(0)
			ARR_DL[COUNT].RASTER_VAL= ORS -> GETFIELD(1)

			COUNT=COUNT+1
		ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
	ENDIF
	Obj_Destroy,ORS
	;PRINT,'   ',ARR_RESULT

	;********************************************************************************
	;�������ݵĶ�ȡ,��������ͳһ����ͬ�ķֱ�����,
	;����ȷ����ͳһ�ֱ�����1KM
	HEIGHT_COMMON	=COMMON_AREA.Y_TOP_COMMON-COMMON_AREA.Y_BOTTOM_COMMON
	WIDTH_COMMON	=COMMON_AREA.X_RIGHT_COMMON-COMMON_AREA.X_LEFT_COMMON


	TEMP=GET_LINE_SAMPLE(FILE_IN,COMMON_AREA)
	SAMPLE_START_FILE_IN=TEMP.SAMPLES_START
	LINE_START_FILE_IN=TEMP.LINES_START
	SAMPLES_FILE_IN=TEMP.SAMPLES_TOTAL
	LINES_FILE_IN=TEMP.LINES_TOTAL
	PRINT,'SAMPLE_START_FILE_IN',SAMPLE_START_FILE_IN
	PRINT,'LINE_START_FILE_IN',LINE_START_FILE_IN
	PRINT,'SAMPLES_FILE_IN',SAMPLES_FILE_IN
	PRINT,'LINES_FILE_IN',LINES_FILE_IN

	TEMP=GET_LINE_SAMPLE(FILE_COUNTY,COMMON_AREA)
	SAMPLE_START_FILE_COUNTY=TEMP.SAMPLES_START
	LINE_START_FILE_COUNTY=TEMP.LINES_START
	SAMPLES_FILE_COUNTY=TEMP.SAMPLES_TOTAL
	LINES_FILE_COUNTY=TEMP.LINES_TOTAL
	PRINT,'SAMPLE_START_FILE_COUNTY',SAMPLE_START_FILE_COUNTY
	PRINT,'LINE_START_FILE_COUNTY',LINE_START_FILE_COUNTY
	PRINT,'SAMPLES_FILE_COUNTY',SAMPLES_FILE_COUNTY
	PRINT,'LINES_FILE_COUNTY',LINES_FILE_COUNTY

	TEMP=GET_LINE_SAMPLE(FILE_PLOWLAND,COMMON_AREA)
	SAMPLE_START_FILE_PLOWLAND=TEMP.SAMPLES_START
	LINE_START_FILE_PLOWLAND=TEMP.LINES_START
	SAMPLES_FILE_PLOWLAND=TEMP.SAMPLES_TOTAL
	LINES_FILE_PLOWLAND=TEMP.LINES_TOTAL
	PRINT,'SAMPLE_START_FILE_PLOWLAND',SAMPLE_START_FILE_PLOWLAND
	PRINT,'LINE_START_FILE_PLOWLAND',LINE_START_FILE_PLOWLAND
	PRINT,'SAMPLES_FILE_PLOWLAND',SAMPLES_FILE_PLOWLAND
	PRINT,'LINES_FILE_PLOWLAND',LINES_FILE_PLOWLAND

	PRINT,'METHOD 2'
	SAMPLES_1KM	=SAMPLES_FILE_PLOWLAND
	LINES_1KM	=LINES_FILE_PLOWLAND

	ARR_FILE_IN_1=READ_IMAGE_PART(FILE_IN,$
								LINE_START_FILE_IN,$
								SAMPLE_START_FILE_IN,$
								LINES_FILE_IN,$
								SAMPLES_FILE_IN)
	ARR_FILE_IN=*(ARR_FILE_IN_1.P_DATA)
	PTR_FREE, ARR_FILE_IN_1.P_DATA
	ARR_FILE_IN=CONGRID(ARR_FILE_IN,SAMPLES_1KM,LINES_1KM)
;	WINDOW,1,XSIZE=800,YSIZE=600
;	TVSCL,CONGRID(ARR_FILE_IN,800,600),/ORDER
;	ARR_FILE_IN=0


	ARR_FILE_COUNTY_1=READ_IMAGE_PART(FILE_COUNTY,$
								LINE_START_FILE_COUNTY,$
								SAMPLE_START_FILE_COUNTY,$
								LINES_FILE_COUNTY,$
								SAMPLES_FILE_COUNTY)
	ARR_FILE_COUNTY=*(ARR_FILE_COUNTY_1.P_DATA)
	PTR_FREE, ARR_FILE_COUNTY_1.P_DATA
	ARR_FILE_COUNTY=CONGRID(ARR_FILE_COUNTY,SAMPLES_1KM,LINES_1KM)


;	WINDOW,2,XSIZE=800,YSIZE=600
;	TVSCL,CONGRID(ARR_FILE_COUNTY,800,600),/ORDER
;	ARR_FILE_IN=0


	;��Ϊ������Ϊ����/ˮ��/�ͺ����������ݵĴ�С�ͷֱ��ʼ��ռ����Եȶ�����ͬ��
	;��Ϊʹ�ø��ص���ز�����ˮ��ͺ��ص����ݽ����˶�ȡ
	ARR_FILE_PL_1=READ_IMAGE_PART(FILE_PLOWLAND,$
								LINE_START_FILE_PLOWLAND,$
								SAMPLE_START_FILE_PLOWLAND,$
								LINES_FILE_PLOWLAND,$
								SAMPLES_FILE_PLOWLAND)
	ARR_FILE_PL=*(ARR_FILE_PL_1.P_DATA)
	PTR_FREE, ARR_FILE_PL_1.P_DATA
	ARR_FILE_PL=CONGRID(ARR_FILE_PL,SAMPLES_1KM,LINES_1KM)
;	WINDOW,3,XSIZE=800,YSIZE=600
;	TVSCL,CONGRID(ARR_FILE_PL,800,600),/ORDER
;	ARR_FILE_IN=0


	ARR_FILE_PF_1=READ_IMAGE_PART(FILE_PADDY_FI,$
								LINE_START_FILE_PLOWLAND,$
								SAMPLE_START_FILE_PLOWLAND,$
								LINES_FILE_PLOWLAND,$
								SAMPLES_FILE_PLOWLAND)
	ARR_FILE_PF=*(ARR_FILE_PF_1.P_DATA)
	PTR_FREE, ARR_FILE_PF_1.P_DATA
	ARR_FILE_PF=CONGRID(ARR_FILE_PF,SAMPLES_1KM,LINES_1KM)
;	WINDOW,4,XSIZE=800,YSIZE=600
;	TVSCL,CONGRID(ARR_FILE_PF,800,600),/ORDER
;	ARR_FILE_IN=0


	ARR_FILE_DL_1=READ_IMAGE_PART(FILE_DRY_LAND,$
								LINE_START_FILE_PLOWLAND,$
								SAMPLE_START_FILE_PLOWLAND,$
								LINES_FILE_PLOWLAND,$
								SAMPLES_FILE_PLOWLAND)
	ARR_FILE_DL=*(ARR_FILE_DL_1.P_DATA)
	PTR_FREE, ARR_FILE_DL_1.P_DATA
	ARR_FILE_DL=CONGRID(ARR_FILE_DL,SAMPLES_1KM,LINES_1KM)
;	WINDOW,5,XSIZE=800,YSIZE=600
;	TVSCL,CONGRID(ARR_FILE_DL,800,600),/ORDER
;	ARR_FILE_IN=0

	LINES	=LINES_1KM
	SAMPLES	=SAMPLES_1KM


	;**************************************************************************

	;******************************************************************************************

	;**************************************************************************
	FOR I=LONG(0),LINES-1 DO BEGIN
		;-------------------------------------------------------------------------------
		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('�û���ֹ��ϵͳ�Ĵ���.',TITLE='��ʾ')
			CLOSE,/ALL
			PROGRESSTIMER->DESTROY ;����������
		RETURN,0
		ENDIF
		PROGRESSTIMER->UPDATE, (FLOAT(I)/LINES * 100.0) ;��������

		;******************************************************************************************
		;******************************************************************************************
;		writeu,lun_1,DATA_FILEIN
;		writeu,lun_2,DATA_COUNTY
;		writeu,lun_3,DATA_LANDUSE
		;******************************************************************************************

		;����Ϊѭ��,��ʼͳ��
		;PRINT,'A'
		FOR J=LONG(0),SAMPLES-1 DO BEGIN
			IF(ARR_FILE_COUNTY[J,I] NE 0) THEN BEGIN
			;�ֱ�ָ���/ˮ��/����д�������������
				IF(ARR_FILE_PL[J,I] GT 10) THEN BEGIN
					K=ARR_FILE_COUNTY[J,I]-1
					CASE 1 OF
						;20060829,�ɼ̻�
						;��ԭ���ĳ���������޸�
						((ARR_FILE_IN[J,I] GE 1) AND (ARR_FILE_IN[J,I] LE 2)):BEGIN
							ARR_PL[K].ARR_POINTS[0]	=ARR_PL[K].ARR_POINTS[0]+1
							ARR_PL[K].TOTAL_POINTS	=ARR_PL[K].TOTAL_POINTS+1

						END
						((ARR_FILE_IN[J,I] GE 3) AND (ARR_FILE_IN[J,I] LE 4)):BEGIN
							ARR_PL[K].ARR_POINTS[1]	=ARR_PL[K].ARR_POINTS[1]+1
							ARR_PL[K].TOTAL_POINTS	=ARR_PL[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] EQ 5)):BEGIN
							ARR_PL[K].ARR_POINTS[2]	=ARR_PL[K].ARR_POINTS[2]+1
							ARR_PL[K].TOTAL_POINTS	=ARR_PL[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] GE 6) AND (ARR_FILE_IN[J,I] LE 7)):BEGIN
							ARR_PL[K].ARR_POINTS[3]	=ARR_PL[K].ARR_POINTS[3]+1
							ARR_PL[K].TOTAL_POINTS	=ARR_PL[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] GE 8) AND (ARR_FILE_IN[J,I] LE 9)):BEGIN
							ARR_PL[K].ARR_POINTS[4]	=ARR_PL[K].ARR_POINTS[4]+1
							ARR_PL[K].TOTAL_POINTS	=ARR_PL[K].TOTAL_POINTS+1
						END
						ELSE:
					ENDCASE
				ENDIF

				;(2)ˮ��
				IF(ARR_FILE_PF[J,I] GT 10) THEN BEGIN
					K=ARR_FILE_COUNTY[J,I]-1
					CASE 1 OF
						;20060829,�ɼ̻�
						;��ԭ���ĳ���������޸�
						((ARR_FILE_IN[J,I] GE 1) AND (ARR_FILE_IN[J,I] LE 2)):BEGIN
							ARR_PF[K].ARR_POINTS[0]	=ARR_PF[K].ARR_POINTS[0]+1
							ARR_PF[K].TOTAL_POINTS	=ARR_PF[K].TOTAL_POINTS+1

						END
						((ARR_FILE_IN[J,I] GE 3) AND (ARR_FILE_IN[J,I] LE 4)):BEGIN
							ARR_PF[K].ARR_POINTS[1]	=ARR_PF[K].ARR_POINTS[1]+1
							ARR_PF[K].TOTAL_POINTS	=ARR_PF[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] EQ 5)):BEGIN
							ARR_PF[K].ARR_POINTS[2]	=ARR_PF[K].ARR_POINTS[2]+1
							ARR_PF[K].TOTAL_POINTS	=ARR_PF[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] GE 6) AND (ARR_FILE_IN[J,I] LE 7)):BEGIN
							ARR_PF[K].ARR_POINTS[3]	=ARR_PF[K].ARR_POINTS[3]+1
							ARR_PF[K].TOTAL_POINTS	=ARR_PF[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] GE 8) AND (ARR_FILE_IN[J,I] LE 9)):BEGIN
							ARR_PF[K].ARR_POINTS[4]	=ARR_PF[K].ARR_POINTS[4]+1
							ARR_PF[K].TOTAL_POINTS	=ARR_PF[K].TOTAL_POINTS+1
						END
						ELSE:
					ENDCASE
				ENDIF

				;(3)����
				IF(ARR_FILE_DL[J,I] GT 10) THEN BEGIN
					K=ARR_FILE_COUNTY[J,I]-1
					CASE 1 OF
						;20060829,�ɼ̻�
						;��ԭ���ĳ���������޸�
						((ARR_FILE_IN[J,I] GE 1) AND (ARR_FILE_IN[J,I] LE 2)):BEGIN
							ARR_DL[K].ARR_POINTS[0]	=ARR_DL[K].ARR_POINTS[0]+1
							ARR_DL[K].TOTAL_POINTS	=ARR_DL[K].TOTAL_POINTS+1

						END
						((ARR_FILE_IN[J,I] GE 3) AND (ARR_FILE_IN[J,I] LE 4)):BEGIN
							ARR_DL[K].ARR_POINTS[1]	=ARR_DL[K].ARR_POINTS[1]+1
							ARR_DL[K].TOTAL_POINTS	=ARR_DL[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] EQ 5)):BEGIN
							ARR_DL[K].ARR_POINTS[2]	=ARR_DL[K].ARR_POINTS[2]+1
							ARR_DL[K].TOTAL_POINTS	=ARR_DL[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] GE 6) AND (ARR_FILE_IN[J,I] LE 7)):BEGIN
							ARR_DL[K].ARR_POINTS[3]	=ARR_DL[K].ARR_POINTS[3]+1
							ARR_DL[K].TOTAL_POINTS	=ARR_DL[K].TOTAL_POINTS+1
						END
						((ARR_FILE_IN[J,I] GE 8) AND (ARR_FILE_IN[J,I] LE 9)):BEGIN
							ARR_DL[K].ARR_POINTS[4]	=ARR_DL[K].ARR_POINTS[4]+1
							ARR_DL[K].TOTAL_POINTS	=ARR_DL[K].TOTAL_POINTS+1
						END
						ELSE:
					ENDCASE
				ENDIF
			ENDIF
		ENDFOR
	ENDFOR
	;PRINT,'HERE 1 20060829'

	;******************************************************************************************

	;******************************************************************************************

	;����ƽ��ֵ
	FOR I=0,NUM_COUNTY-1 DO BEGIN
		IF(ARR_PL[I].TOTAL_POINTS NE 0) THEN BEGIN
			ARR_PL[I].ARR_PER=ARR_PL[I].ARR_POINTS/ARR_PL[I].TOTAL_POINTS
		ENDIF
		IF(ARR_PF[I].TOTAL_POINTS NE 0) THEN BEGIN
			ARR_PF[I].ARR_PER=ARR_PF[I].ARR_POINTS/ARR_PF[I].TOTAL_POINTS
		ENDIF
		IF(ARR_DL[I].TOTAL_POINTS NE 0) THEN BEGIN
			ARR_DL[I].ARR_PER=ARR_DL[I].ARR_POINTS/ARR_DL[I].TOTAL_POINTS
		ENDIF
	ENDFOR
	;-------------------------------------------------------------------------------
	PROGRESSTIMER->DESTROY ;
	;-------------------------------------------------------------------------------
	;**************************************************************************************
	;-------------------------------------------------------------------------------
	;�����д������ڽ���������ʾ
	temp_str='�������'+STRTRIM(DATA_TYPE,2)+'����'
	temp_str=temp_str+string(byte(13))+string(byte(10))
	temp_str=temp_str+'ʱ��:'+STRTRIM(YEAR,2)+'��'+STRTRIM(MONTH,2)+'��'+STRTRIM(DAY,2)+'��'
	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON,MESSAGE=temp_str,TITLE='ʵʱ���ͳ��')
	PROGRESSTIMER->START
	;-------------------------------------------------------------------------------
	;PRINT,'THE RESULT IS:',ARR_PL
	;********************************************************************************
	;���ݿ����
	TABLE='PARAMETER_RT_COUNTY'
	;(1)�����ݿ���ԭ�е����ݽ���ɾ��
	;ɾ�����ݿ�����Ҫ����ļ�¼ʱ��������ͬ�ļ�¼
	SQL='DELETE FROM PARAMETER_RT_COUNTY WHERE  '
	SQL=SQL+' (YEAR = '+STRTRIM(STRING(YEAR),2)		+')'
	SQL=SQL+' AND (MONTH = '+STRTRIM(STRING(MONTH),2)		+')'
	SQL=SQL+' AND (DAY = '+STRTRIM(STRING(DAY),2)	+')'
	SQL=SQL+' AND (SENSOR_CODE = '+"'"+STRTRIM(SENSOR_CODE,2)+"'"	+')'
	SQL=SQL+' AND (DATA_TYPE = '+"'"+STRTRIM(DATA_TYPE,2)+"'"	+')'
	SQL=SQL+' AND (PERIODS = '+STRTRIM(PERIOD,2)	+')'
	;PRINT,SQL
	DBCO->EXECUTESQL,SQL

	;(2)�����µ�����
	FOR I=0,NUM_COUNTY-1 DO BEGIN
		;-------------------------------------------------------------------------------
		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('�û���ֹ��ϵͳ�Ĵ���.',TITLE='��ʾ')
			CLOSE,/ALL
			PROGRESSTIMER->DESTROY ;����������
		RETURN,0
		ENDIF
		PROGRESSTIMER->UPDATE, (FLOAT(I)/NUM_COUNTY * 100.0) ;��������
		;(1)����
		SQL='INSERT INTO PARAMETER_RT_COUNTY '
		SQL=SQL+'(COUNTY_CODE ,YEAR ,MONTH ,DAY'
		SQL=SQL+',PIXEL_WORSE '
		SQL=SQL+',PIXEL_LITTLEWORSE '
		SQL=SQL+',PIXEL_KEEPBALANCE '
		SQL=SQL+',PIXEL_LITTLEBETTER '
		SQL=SQL+',PIXEL_BETTER '
		SQL=SQL+',PERCENT_WORSE '
		SQL=SQL+',PERCENT_LITTLEWORSE '
		SQL=SQL+',PERCENT_KEEPBALANCE '
		SQL=SQL+',PERCENT_LITTLEBETTER '
		SQL=SQL+',PERCENT_BETTER '
		SQL=SQL+',LAND_TYPE '
		SQL=SQL+',PERIODS '
		SQL=SQL+',DATA_TYPE '
		SQL=SQL+',SENSOR_CODE ) '
		SQL=SQL+'VALUES ('
		SQL=SQL+"'"+STRTRIM(ARR_PL[I].COUNTY_CODE,2)	+"'"+','
		SQL=SQL+STRTRIM(STRING(YEAR),2)				+','
		SQL=SQL+STRTRIM(STRING(MONTH),2)			+','
		SQL=SQL+STRTRIM(STRING(DAY),2)			+','

		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_POINTS[0]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_POINTS[1]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_POINTS[2]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_POINTS[3]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_POINTS[4]),2)		+','

		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_PER[0]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_PER[1]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_PER[2]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_PER[3]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PL[I].ARR_PER[4]),2)		+','
		SQL=SQL+"'"+'����'	+"'"+','
		SQL=SQL+STRTRIM(PERIOD,2)+','
		SQL=SQL+"'"+STRTRIM(DATA_TYPE,2)+"'"+','
		SQL=SQL+"'"+STRTRIM(STRING(SENSOR_CODE),2)	+"'"+')'
		;PRINT,SQL
		DBCO->EXECUTESQL,SQL

		;(2)����
		SQL='INSERT INTO PARAMETER_RT_COUNTY '
		SQL=SQL+'(COUNTY_CODE ,"YEAR" ,"MONTH" ,"DAY"'
		SQL=SQL+',PIXEL_WORSE '
		SQL=SQL+',PIXEL_LITTLEWORSE '
		SQL=SQL+',PIXEL_KEEPBALANCE '
		SQL=SQL+',PIXEL_LITTLEBETTER '
		SQL=SQL+',PIXEL_BETTER '
		SQL=SQL+',PERCENT_WORSE '
		SQL=SQL+',PERCENT_LITTLEWORSE '
		SQL=SQL+',PERCENT_KEEPBALANCE '
		SQL=SQL+',PERCENT_LITTLEBETTER '
		SQL=SQL+',PERCENT_BETTER '
		SQL=SQL+',LAND_TYPE '
		SQL=SQL+',PERIODS '
		SQL=SQL+',DATA_TYPE '
		SQL=SQL+',SENSOR_CODE ) '
		SQL=SQL+'VALUES ('
		SQL=SQL+"'"+STRTRIM(ARR_PL[I].COUNTY_CODE,2)	+"'"+','
		SQL=SQL+STRTRIM(STRING(YEAR),2)				+','
		SQL=SQL+STRTRIM(STRING(MONTH),2)			+','
		SQL=SQL+STRTRIM(STRING(DAY),2)			+','

		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_POINTS[0]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_POINTS[1]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_POINTS[2]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_POINTS[3]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_POINTS[4]),2)		+','

		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_PER[0]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_PER[1]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_PER[2]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_PER[3]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_PF[I].ARR_PER[4]),2)		+','
		SQL=SQL+"'"+'ˮ��'	+"'"+','
		SQL=SQL+STRTRIM(PERIOD,2)+','
		SQL=SQL+"'"+STRTRIM(DATA_TYPE,2)+"'"+','
		SQL=SQL+"'"+STRTRIM(STRING(SENSOR_CODE),2)	+"'"+')'
		;PRINT,SQL
		DBCO->EXECUTESQL,SQL

		;(3)����
		SQL='INSERT INTO PARAMETER_RT_COUNTY '
		SQL=SQL+'(COUNTY_CODE ,"YEAR" ,"MONTH" ,"DAY"'
		SQL=SQL+',PIXEL_WORSE '
		SQL=SQL+',PIXEL_LITTLEWORSE '
		SQL=SQL+',PIXEL_KEEPBALANCE '
		SQL=SQL+',PIXEL_LITTLEBETTER '
		SQL=SQL+',PIXEL_BETTER '
		SQL=SQL+',PERCENT_WORSE '
		SQL=SQL+',PERCENT_LITTLEWORSE '
		SQL=SQL+',PERCENT_KEEPBALANCE '
		SQL=SQL+',PERCENT_LITTLEBETTER '
		SQL=SQL+',PERCENT_BETTER '
		SQL=SQL+',LAND_TYPE '
		SQL=SQL+',PERIODS '
		SQL=SQL+',DATA_TYPE '
		SQL=SQL+',SENSOR_CODE ) '
		SQL=SQL+'VALUES ('
		SQL=SQL+"'"+STRTRIM(ARR_PL[I].COUNTY_CODE,2)	+"'"+','
		SQL=SQL+STRTRIM(STRING(YEAR),2)				+','
		SQL=SQL+STRTRIM(STRING(MONTH),2)			+','
		SQL=SQL+STRTRIM(STRING(DAY),2)			+','

		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_POINTS[0]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_POINTS[1]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_POINTS[2]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_POINTS[3]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_POINTS[4]),2)		+','

		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_PER[0]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_PER[1]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_PER[2]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_PER[3]),2)		+','
		SQL=SQL+STRTRIM(STRING(ARR_DL[I].ARR_PER[4]),2)		+','
		SQL=SQL+"'"+'����'	+"'"+','
		SQL=SQL+STRTRIM(PERIOD,2)+','
		SQL=SQL+"'"+STRTRIM(DATA_TYPE,2)+"'"+','
		SQL=SQL+"'"+STRTRIM(STRING(SENSOR_CODE),2)	+"'"+')'
		;PRINT,SQL
		DBCO->EXECUTESQL,SQL
	ENDFOR
	;-------------------------------------------------------------------------------
	PROGRESSTIMER->DESTROY ;
	;-------------------------------------------------------------------------------
	;�ͷ�һЩ����
	ARR_PL=0
	ARR_PF=0
	ARR_DL=0
	;**************************************************************************************
	;**************************************************************************************
	PRINT,'RESULT IS:',ARR_PL
	RETURN,1

END
;-----------------------------------------------------------------
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION CMD_OK_STA_RT_COUNTY, EVENT

    ;�������ȡ��������ϵĲ���
    ;���������,һ���ǵ�����������,һ��������������
    WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

    IF((*PSTATE).SINGLE_BATCH EQ 'SINGLE') THEN BEGIN ;��������

    	YEAR_1				=(*PSTATE).YEAR
    	MONTH				=(*PSTATE).MONTH
    	DAY					=(*PSTATE).DAY
    	SENSOR_TYPE			=(*PSTATE).SENSOR_TYPE
    	SENSOR_CODE			=(*PSTATE).SENSOR_CODE

    	DATA_TYPE			=(*PSTATE).DATA_TYPE
    	PERIOD				=(*PSTATE).PERIOD

    	DBCO 				=DBobj

    	WIDGET_CONTROL,(*PSTATE).TXT_FILE_IN,GET_VALUE=FILE_IN
   		FILE_IN=FILE_IN[0]
   		IF(FILE_IN EQ '') THEN BEGIN
         	MSG=DIALOG_MESSAGE('��ѡ�������ļ�!',/INFORMATION,TITLE='��ѡ�������ļ�')
         	CLOSE,/ALL
         	RETURN,0
     	ENDIF

		IF(DATA_TYPE NE 'NDVI') THEN BEGIN
			TEMP=DIALOG_MESSAGE('ϵͳĿǰֻ�ܴ���NDVI',TITLE='��������')
			CLOSE,/ALL
     		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
		ENDIF
		PRINT,'HERE 1'

		RESULT=STAT_RT_COUNTY(FILE_IN,YEAR_1,MONTH,DAY	$
				,SENSOR_CODE,DATA_TYPE	$
				,PERIOD,DBCO)

		IF(RESULT EQ 1) THEN BEGIN
			TEMP=DIALOG_MESSAGE('ͳ�Ƴɹ�!',TITLE='�ɹ�')
			log, 'ʵʱ���-�ռ�ͳ��', 1
		ENDIF ELSE BEGIN
			TEMP=DIALOG_MESSAGE('ͳ��ʧ��!',TITLE='ʧ��')
			log, 'ʵʱ���-�ռ�ͳ��', -1
		ENDELSE


		;����ͳ�ƵĴ������
    ENDIF ELSE BEGIN	;������


    	YEAR_START			=(*PSTATE).YEAR_START
    	MONTH_START			=(*PSTATE).MONTH_START
    	DAY_START			=(*PSTATE).DAY_START
    	YEAR_END			=(*PSTATE).YEAR_END
    	MONTH_END			=(*PSTATE).MONTH_END
    	DAY_END				=(*PSTATE).DAY_END

    	SENSOR_TYPE			=(*PSTATE).SENSOR_TYPE_BATCH
    	SENSOR_CODE			=(*PSTATE).SENSOR_CODE_BATCH
    	DATA_TYPE			=(*PSTATE).DATA_TYPE_BATCH

    	DATA_TYPE			=(*PSTATE).DATA_TYPE_BATCH
    	PERIOD				=(*PSTATE).PERIOD_BATCH
    	DBCO 				=DBobj

    	WIDGET_CONTROL,(*PSTATE).TXT_SUFFIX,GET_VALUE=SUFFIX
   		SUFFIX=STRTRIM(SUFFIX[0],2)
   		WIDGET_CONTROL,(*PSTATE).TXT_PREFIX,GET_VALUE=PREFIX
   		PREFIX=STRTRIM(PREFIX[0],2)
   		WIDGET_CONTROL,(*PSTATE).TXT_FILE_PATH,GET_VALUE=FILE_PATH
    	FILE_PATH=STRTRIM(FILE_PATH[0],2)


		IF(DATA_TYPE NE 'NDVI') THEN BEGIN
			TEMP=DIALOG_MESSAGE('ϵͳĿǰֻ�ܴ���NDVI',TITLE='��������')
			CLOSE,/ALL
     		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
		ENDIF

		;���㿪ʼ�ͽ���������
		JULDAY_START=JULDAY(MONTH_START, DAY_START, YEAR_START, 0, 0, 0)
		JULDAY_END	=JULDAY(MONTH_END, DAY_END, YEAR_END, 0, 0, 0)


		;�����������ʱ���ʱ����һ����Ҫ��,������Ҫ��ʱ���Ƿ��������ж�
		IF(JULDAY_START GT JULDAY_END) THEN BEGIN
			TEMP=DIALOG_MESSAGE('��ʼʱ�����Ҫ���ڽ���ʱ��',TITLE='��������')
			CLOSE,/ALL
     		RETURN, 0
		ENDIF

		;��ͳ�Ƶ�ȫ���ͷ��ݵ�ͳ��������
		COUNT_SUCCESS=0;һ����ʶ��,������¼�ɹ�ͳ�Ƶ����ݵ�����(��)
		FOR	I=JULDAY_START,JULDAY_END DO BEGIN

			CALDAT, I, MONTH, DAY, YEAR_1, H, M, S
			PRINT,YEAR_1,MONTH,DAY
			YEAR_1=STRTRIM(YEAR_1,2)
			;����µ�λ������
			IF(FIX(MONTH) LE 9) THEN BEGIN
				MONTH	=	'0'+STRTRIM(MONTH,2)
			ENDIF ELSE BEGIN
				MONTH	=	STRTRIM(MONTH,2)
			ENDELSE

			;����µ�λ������
			IF(FIX(DAY) LE 9) THEN BEGIN
				DAY	=	'0'+STRTRIM(DAY,2)
			ENDIF ELSE BEGIN
				DAY	=	STRTRIM(DAY,2)
			ENDELSE

			FILE_IN	=	FILE_PATH+PREFIX+YEAR_1+MONTH+DAY+SUFFIX
			RESULT=STAT_RT_COUNTY(FILE_IN,YEAR_1,MONTH,DAY	$
				,SENSOR_CODE,DATA_TYPE	$
				,PERIOD,DBCO)

			IF(RESULT EQ 1) THEN BEGIN
				PRINT,STRTRIM(YEAR_1,2)+'��'+STRTRIM(MONTH,2)+'��'+STRTRIM(DAY,2)+'�����ݴ���ɹ�'
				COUNT_SUCCESS=COUNT_SUCCESS+1
			ENDIF ELSE BEGIN
				PRINT,STRTRIM(YEAR_1,2)+'��'+STRTRIM(MONTH,2)+'��'+STRTRIM(DAY,2)+'�����ݴ���ʧ��'
			ENDELSE
		ENDFOR
		TEMP='���ɹ�ͳ����'+STRTRIM(COUNT_SUCCESS,2)+'������!'
		TEMP_1=DIALOG_MESSAGE(TEMP,TITLE='ͳ�ƽ��')

		if COUNT_SUCCESS eq 0 then begin
			log, 'ʵʱ���-�ռ�ͳ��', -1
		endif else if COUNT_SUCCESS eq FIX(JULDAY_END)-FIX(JULDAY_START) then begin
			log, 'ʵʱ���-�ռ�ͳ��', 1
		endif else begin
			log, 'ʵʱ���-�ռ�ͳ��', 0
		endelse
    ENDELSE
 	RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;
; Empty stub procedure used for autoloading.
;***************************************************************************
;***************************************************************************
;
pro STA_RT_COUNTY_eventcb
end
;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	03/11/2005 16:45.10
;
pro STAT_RT_COUNTY_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

  common common_setpath, ppath
  zs_out_path = (*ppath).zs_out_path

  wWidget =  Event.top
  WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
  ;******************************************************************************
  ;��һ���µķ���,�ڰ�����Ϊ�ؼ�����¼�ʱ,���ƾ�Ȼ�ǶԴ�Сд���е�,���Ǹ��µķ���
  ;�Ժ���Ҫע��������
  ;******************************************************************************
  case wTarget of
	;��ʾ�ļ���

	Widget_Info(wWidget, FIND_BY_UNAME='CMD_CHANGE_PATH'): begin
    	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
    	widget_control,wWidget,sensitive=0	;��������ӣ�20070803
 		FILE_PATH_NEW=DIALOG_PICKFILE( /DIRECTORY,path=zs_out_path,title='��ѡ��Ŀ���ļ�·��', DIALOG_PARENT=Event.id)
 		IF(FILE_PATH_NEW NE '') THEN BEGIN
 	 		WIDGET_CONTROL,(*pstate).TXT_FILE_PATH,Set_value=FILE_PATH_NEW
 		ENDIF
 		widget_control,wWidget,sensitive=1	;��������ӣ�20070803
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_period'): begin
		INDEX=EVENT.INDEX
		(*PSTATE).PERIOD=INDEX+1
		PRINT, (*PSTATE).PERIOD
    end

    Widget_Info(wWidget, FIND_BY_UNAME='DST_period_BATCH'): begin
		INDEX=EVENT.INDEX
		(*PSTATE).PERIOD_BATCH=INDEX+1
		PRINT, (*PSTATE).PERIOD_BATCH
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_SHOW_NAME_STA_PRO'): begin
    	WIDGET_CONTROL,(*PSTATE).TXT_FILE_PATH,GET_VALUE=FILE_PATH
    	FILE_PATH=STRTRIM(FILE_PATH,2)
    	WIDGET_CONTROL,(*PSTATE).TXT_SUFFIX,GET_VALUE=SUFFIX
    	SUFFIX=STRTRIM(SUFFIX,2)
    	WIDGET_CONTROL,(*PSTATE).TXT_PREFIX,GET_VALUE=PREFIX
    	PREFIX=STRTRIM(PREFIX,2)
    	TEXT_STR=FILE_PATH+PREFIX+STRTRIM((*PSTATE).YEAR_START,2)
    	;�����µ�λ������
    	IF ((*PSTATE).MONTH_START LE 9) THEN BEGIN
    		TEXT_STR=TEXT_STR+'0'+STRTRIM((*PSTATE).MONTH_START,2)
    	ENDIF ELSE BEGIN
    		TEXT_STR=TEXT_STR+STRTRIM((*PSTATE).MONTH_START,2)
    	ENDELSE
    	;�������λ������
    	IF ((*PSTATE).DAY_START LE 9) THEN BEGIN
    		TEXT_STR=TEXT_STR+'0'+STRTRIM((*PSTATE).DAY_START,2)
    	ENDIF ELSE BEGIN
    		TEXT_STR=TEXT_STR+STRTRIM((*PSTATE).DAY_START,2)
    	ENDELSE

    	TEXT_STR=TEXT_STR+SUFFIX
    	WIDGET_CONTROL,(*PSTATE).TXT_NAME_SHOW,SET_VALUE=TEXT_STR
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CMD_OK_STA_PRO_COUNTY'): begin

    end
    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CHOOSE_FILE'): begin

		widget_control,wWidget,sensitive=0	;��������ӣ�20070803
	    file=DIALOG_PICKFILE(filter='*.*',path=zs_out_path,title='��ѡ��һ���ļ�', DIALOG_PARENT=Event.id)
	    IF (file NE '') THEN BEGIN
			WIDGET_CONTROL,(*PSTATE).TXT_FILE_IN,SET_VALUE=FILE
	    ENDIF
	    widget_control,wWidget,sensitive=1	;��������ӣ�20070803
    end
    ;��
    Widget_Info(wWidget, FIND_BY_UNAME='DST_YEAR'): begin
		INDEX=EVENT.INDEX
		(*PSTATE).YEAR=((*PSTATE).ARR_YEAR)[INDEX]
		;PRINT, (*PSTATE).YEAR
    end

    Widget_Info(wWidget, FIND_BY_UNAME='DST_YEAR_START'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).YEAR_START=((*PSTATE).ARR_YEAR)[INDEX]
		;PRINT, (*PSTATE).YEAR_START
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_YEAR_END'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).YEAR_END=((*PSTATE).ARR_YEAR)[INDEX]
		;PRINT, (*PSTATE).YEAR_END
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_MONTH'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).MONTH=((*PSTATE).ARR_MONTH)[INDEX]
		;PRINT, (*PSTATE).MONTH
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_MONTH_START'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).MONTH_START=((*PSTATE).ARR_MONTH)[INDEX]
		;PRINT, (*PSTATE).MONTH_START
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_MONTH_END'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).MONTH_END=((*PSTATE).ARR_MONTH)[INDEX]
		;PRINT, (*PSTATE).MONTH_END
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_DAY'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).DAY=((*PSTATE).ARR_DAY)[INDEX]
		;PRINT, (*PSTATE).DAY
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_DAY_START'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).DAY_START=((*PSTATE).ARR_DAY)[INDEX]
		;PRINT, (*PSTATE).DAY_START
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_DAY_END'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).DAY_END=((*PSTATE).ARR_DAY)[INDEX]
		;PRINT, (*PSTATE).DAY_END
    end

    Widget_Info(wWidget, FIND_BY_UNAME='DST_DATA_TYPE'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).DATA_TYPE=((*PSTATE).ARR_DATA_TYPE)[INDEX]
		;PRINT, (*PSTATE).DATA_TYPE
		defaultnames_zsprosta,event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_DATA_TYPE_BATCH'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).DATA_TYPE_BATCH=((*PSTATE).ARR_DATA_TYPE)[INDEX]
		;PRINT, (*PSTATE).DATA_TYPE_BATCH
		defaultnames_zsprosta,event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_SENSOR_TYPE'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).SENSOR_TYPE=((*PSTATE).ARR_SENSOR_TYPE)[INDEX]
		(*PSTATE).SENSOR_CODE=INDEX+1
		PRINT, (*PSTATE).SENSOR_CODE
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_SENSOR_TYPE_BATCH'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).SENSOR_TYPE_BATCH=((*PSTATE).ARR_SENSOR_TYPE)[INDEX]
		(*PSTATE).SENSOR_CODE_BATCH=INDEX+1
		PRINT, (*PSTATE).SENSOR_CODE_BATCH
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLOSE_STA_PRO'): begin
    common_log,'�رճ���ʵʱͳ��'
    	CLOSE,/ALL
	     WIDGET_CONTROL, EVENT.TOP, /DESTROY
	     RETURN
    end
    Widget_Info(wWidget, FIND_BY_UNAME='TAB_STA_COUNTY'): begin
    	IF(EVENT.TAB EQ 0) THEN BEGIN
    		(*PSTATE).SINGLE_BATCH='SINGLE'
    		;PRINT,(*PSTATE).SINGLE_BATCH
    	ENDIF ELSE BEGIN
    		(*PSTATE).SINGLE_BATCH='BATCH'
    		;PRINT,(*PSTATE).SINGLE_BATCH
    	ENDELSE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_HELP_STA_PRO'): begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, 'ʵʱ���ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse

;    	ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,'ʵʱ���ģ��'
    end

    else:
  endcase
end

pro ZS_RT_STA, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_

	common_log,'����ʵʱ����ͳ��'
  ;����ͬһ�������ظ�����
  IF ( XREGISTERED('STAT_RT_COUNTY') NE 0 ) THEN RETURN

  Resolve_Routine, 'STA_PRO_COUNTY_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;Ҳ���Բ���ʼ��

COMMON COMMON_SETPATH,ppath;	$
;		zs_out_path  ,$
;		zs_out_path ,$
;		mj_in_path  ,$
;		dc_out_path ,$
;		cl_out_path ,$
;		nq_in_path  ,$
;		nq_out_path ,$
;		fz_in_path  ,$
;		fz_out_path

zs_out_path = (*ppath).zs_out_path

common current_date, c_year, c_month, c_day
  TEMP=1
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  STAT_RT_COUNTY = Widget_Base( GROUP_LEADER=BASE_TOP,  $
      UNAME='STAT_RT_COUNTY' ,XOFFSET=360 ,YOFFSET=200 ,SCR_XSIZE=326  $
      ,SCR_YSIZE=325 ,TITLE='ʵʱ���--ͳ�Ƶ���' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR =1)


  TAB_STA_COUNTY = Widget_Tab(STAT_RT_COUNTY, UNAME='TAB_STA_COUNTY'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=307 ,SCR_YSIZE=235)


  BASE_SINGLE_PRO = Widget_Base(TAB_STA_COUNTY,  $
      UNAME='BASE_SINGLE_PRO' ,SCR_XSIZE=299 ,SCR_YSIZE=210  $
      ,TITLE='��������' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_0'  $
      ,XOFFSET=7 ,YOFFSET=10 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='��������:')


  WID_LABEL_1 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_1'  $
      ,XOFFSET=7 ,YOFFSET=35 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='������  :')


  WID_LABEL_2 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_2'  $
      ,XOFFSET=7 ,YOFFSET=60 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='ʱ��    :')


  WID_LABEL_5 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_5'  $
      ,XOFFSET=7 ,YOFFSET=135 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='�����ļ�:')


  DST_DATA_TYPE = Widget_Droplist(BASE_SINGLE_PRO,  $
      UNAME='DST_DATA_TYPE' ,XOFFSET=75-TEMP ,YOFFSET=6 ,SCR_XSIZE=83  $
      ,SCR_YSIZE=20)


  DST_SENSOR_TYPE = Widget_Droplist(BASE_SINGLE_PRO,  $
      UNAME='DST_SENSOR_TYPE' ,XOFFSET=75-TEMP ,YOFFSET=31 ,SCR_XSIZE=83  $
      ,SCR_YSIZE=20)

  ;******************************************************************
  ;�������������ѡ��
  WID_LABEL_2 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_2'  $
      ,XOFFSET=7 ,YOFFSET=60+25 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='����    :')

  DST_period = Widget_Droplist(BASE_SINGLE_PRO, UNAME='DST_period'  $
      ,XOFFSET=75-TEMP ,YOFFSET=56+25 ,SCR_XSIZE=53 ,SCR_YSIZE=20)
  ;******************************************************************
  subbase = widget_base(BASE_SINGLE_PRO,XOFFSET=72-TEMP ,YOFFSET=53,/row)

  WID_LABEL_date = Widget_text(subbase,xsize=15, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))

 CMD_pick_date = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date', $
 VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_sta_rt'})

  ;DST_RANGE = Widget_Droplist(BASE_SINGLE_PRO, UNAME='DST_RANGE'  $
     ; ,XOFFSET=75 ,YOFFSET=81 ,SCR_XSIZE=90 ,SCR_YSIZE=20)
  ;****************************************************************************
    ;****************************************************************************

  TXT_FILE_IN = Widget_Text(BASE_SINGLE_PRO, UNAME='TXT_FILE_IN'  $
      ,XOFFSET=75-TEMP ,YOFFSET=132 ,SCR_XSIZE=166 ,SCR_YSIZE=20  $
      ,/EDITABLE ,XSIZE=20 ,YSIZE=1,$
      VALUE= zs_out_path )


  CMD_CHOOSE_FILE = Widget_Button(BASE_SINGLE_PRO,  $
      UNAME='CMD_CHOOSE_FILE' ,XOFFSET=257-TEMP ,YOFFSET=131 ,SCR_XSIZE=36  $
      ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='.\Image\open.bmp' ,/BITMAP)


  BASE_BATCH_PRO = Widget_Base(TAB_STA_COUNTY, UNAME='BASE_BATCH_PRO'  $
      ,SCR_XSIZE=299 ,SCR_YSIZE=210 ,TITLE='������' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


;  DST_RANGE_BATCH = Widget_Droplist(BASE_BATCH_PRO,  $
;      UNAME='DST_RANGE_BATCH' ,XOFFSET=73 ,YOFFSET=107  $
;      ,SCR_XSIZE=90 ,SCR_YSIZE=20)
  ;****************************************************************************


  ;****************************************************************************


;  DST_DAY_START = Widget_Droplist(BASE_BATCH_PRO, UNAME='DST_DAY_START'  $
;      ,XOFFSET=224 ,YOFFSET=56 ,SCR_XSIZE=38 ,SCR_YSIZE=20)
;
;
;  WID_LABEL_9 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_9'  $
;      ,XOFFSET=267 ,YOFFSET=60 ,SCR_XSIZE=19 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='��')
;
;
;  WID_LABEL_10 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_10'  $
;      ,XOFFSET=196 ,YOFFSET=60 ,SCR_XSIZE=18 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='��')
;
;
;  DST_MONTH_START = Widget_Droplist(BASE_BATCH_PRO,  $
;      UNAME='DST_MONTH_START' ,XOFFSET=154 ,YOFFSET=56 ,SCR_XSIZE=39  $
;      ,SCR_YSIZE=20)
;
;
;  WID_LABEL_11 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_11'  $
;      ,XOFFSET=132 ,YOFFSET=60 ,SCR_XSIZE=21 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='��')
;
;
;  DST_YEAR_START = Widget_Combobox(BASE_BATCH_PRO,  $
;      UNAME='DST_YEAR_START' ,XOFFSET=74 ,YOFFSET=56 ,SCR_XSIZE=53  $
;      ,SCR_YSIZE=20)


  DST_SENSOR_TYPE_BATCH = Widget_Droplist(BASE_BATCH_PRO,  $
      UNAME='DST_SENSOR_TYPE_BATCH' ,XOFFSET=74 ,YOFFSET=31  $
      ,SCR_XSIZE=83 ,SCR_YSIZE=20)


  DST_DATA_TYPE_BATCH = Widget_Droplist(BASE_BATCH_PRO,  $
      UNAME='DST_DATA_TYPE_BATCH' ,XOFFSET=74 ,YOFFSET=6  $
      ,SCR_XSIZE=83 ,SCR_YSIZE=20)




  WID_LABEL_15 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_15'  $
      ,XOFFSET=7 ,YOFFSET=60 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='��ʼʱ��:')

	subbase = widget_base(BASE_BATCH_PRO,XOFFSET=72-TEMP ,YOFFSET=53,/row)
  	WID_LABEL_date_s = Widget_text(subbase,xsize=15, $
		VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))
 	CMD_pick_date_s = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date_s, pointer:PTR_NEW(), detail:'CMD_pick_date_s'})



;  WID_LABEL_12 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_12'  $
;      ,XOFFSET=266 ,YOFFSET=86 ,SCR_XSIZE=19 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='��')
;
;
  WID_LABEL_18 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_18'  $
      ,XOFFSET=7 ,YOFFSET=86 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='����ʱ��:')

	subbase = widget_base(BASE_BATCH_PRO,XOFFSET=72-TEMP ,YOFFSET=80,/row)
  	WID_LABEL_date_e = Widget_text(subbase,xsize=15, $
		VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))
 	CMD_pick_date_e = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date_e, pointer:PTR_NEW(), detail:'CMD_pick_date_e'} )

;
;
;  DST_YEAR_END = Widget_Combobox(BASE_BATCH_PRO, UNAME='DST_YEAR_END'  $
;      ,XOFFSET=73+1 ,YOFFSET=82 ,SCR_XSIZE=53 ,SCR_YSIZE=20)
;
;
;  WID_LABEL_19 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_19'  $
;      ,XOFFSET=131+1 ,YOFFSET=86 ,SCR_XSIZE=21 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='��')
;
;
;  DST_MONTH_END = Widget_Droplist(BASE_BATCH_PRO,  $
;      UNAME='DST_MONTH_END' ,XOFFSET=153+1 ,YOFFSET=82 ,SCR_XSIZE=39  $
;      ,SCR_YSIZE=20)
;
;
;  WID_LABEL_20 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_20'  $
;      ,XOFFSET=196+1 ,YOFFSET=86 ,SCR_XSIZE=18 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='��')
;
;
;  DST_DAY_END = Widget_Droplist(BASE_BATCH_PRO, UNAME='DST_DAY_END'  $
;      ,XOFFSET=223+1 ,YOFFSET=82 ,SCR_XSIZE=38 ,SCR_YSIZE=20)


  WID_LABEL_16 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_16'  $
      ,XOFFSET=7 ,YOFFSET=35 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='������  :')


  WID_LABEL_17 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_17'  $
      ,XOFFSET=7 ,YOFFSET=10 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='��������:')
  WID_LABEL_21 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_21'  $
      ,XOFFSET=7 ,YOFFSET=161 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='ǰ׺    :')

  ;******************************************************************
  ;�������������ѡ��
  WID_LABEL_2 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_2'  $
      ,XOFFSET=7 ,YOFFSET=60+50 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='����    :')

  DST_period_BATCH = Widget_Droplist(BASE_BATCH_PRO, UNAME='DST_period_BATCH'  $
      ,XOFFSET=75-1 ,YOFFSET=56+50 ,SCR_XSIZE=53 ,SCR_YSIZE=20)
  ;******************************************************************

  TEXT_PREFIX = Widget_Text(BASE_BATCH_PRO, UNAME='TEXT_PREFIX'  $
      ,XOFFSET=74 ,YOFFSET=158 ,SCR_XSIZE=50 ,SCR_YSIZE=20 ,/EDITABLE  $
      ,XSIZE=20 ,YSIZE=1,VALUE='CLS')


  TEXT_SUFFIX = Widget_Text(BASE_BATCH_PRO, UNAME='TEXT_SUFFIX'  $
      ,XOFFSET=209 ,YOFFSET=158 ,SCR_XSIZE=50 ,SCR_YSIZE=20  $
      ,/EDITABLE ,XSIZE=20 ,YSIZE=1,VALUE='NDVI')


  WID_LABEL_22 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_22'  $
      ,XOFFSET=142 ,YOFFSET=161 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='��׺    :')


  TEXT_NAME_SHOW = Widget_Text(BASE_BATCH_PRO, UNAME='TEXT_NAME_SHOW'  $
      ,XOFFSET=74 ,YOFFSET=183 ,SCR_XSIZE=156 ,SCR_YSIZE=20 ,XSIZE=20  $
      ,YSIZE=1)


  WID_LABEL_23 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_23'  $
      ,XOFFSET=7 ,YOFFSET=186 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='�ļ�����:')


  CMD_SHOW_NAME_STA_PRO = Widget_Button(BASE_BATCH_PRO,  $
      UNAME='CMD_SHOW_NAME_STA_PRO' ,XOFFSET=246 ,YOFFSET=182  $
      ,SCR_XSIZE=46 ,SCR_YSIZE=22  ,/ALIGN_CENTER ,VALUE='��ʾ')

  ;***********************************************************************
  TEMP=50
  TXT_FILE_PATH = Widget_Text(BASE_BATCH_PRO, UNAME='TXT_FILE_PATH'  $
      ,XOFFSET=74 ,YOFFSET=183-TEMP ,SCR_XSIZE=156 ,SCR_YSIZE=20 ,XSIZE=20  $
      ,YSIZE=1,VALUE=zs_out_path)
      PRINT,FILE_PATH


  WID_LABEL_23 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_23'  $
      ,XOFFSET=7 ,YOFFSET=186-TEMP ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='·��    :')


  CMD_CHANGE_PATH = Widget_Button(BASE_BATCH_PRO,  $
      UNAME='CMD_CHANGE_PATH' ,XOFFSET=256 ,YOFFSET=181-TEMP  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=22  ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)
  ;***********************************************************************


  CMD_OK_STA_RT_COUNTY = Widget_Button(STAT_RT_COUNTY,  $
      UNAME='CMD_OK_STA_PRO_COUNTY' ,XOFFSET=12 ,YOFFSET=251  $
      ,SCR_XSIZE=79 ,SCR_YSIZE=22 ,EVENT_FUNC='CMD_OK_STA_RT_COUNTY'  $
      ,/ALIGN_CENTER ,VALUE='ͳ��')


  CMD_HELP_STA_PRO = Widget_Button(STAT_RT_COUNTY,  $
      UNAME='CMD_HELP_STA_PRO' ,XOFFSET=120 ,YOFFSET=251  $
      ,SCR_XSIZE=79 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='����')


  CMD_CLOSE_STA_PRO = Widget_Button(STAT_RT_COUNTY,  $
      UNAME='CMD_CLOSE_STA_PRO' ,XOFFSET=228 ,YOFFSET=251  $
      ,SCR_XSIZE=79 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='�ر�')

	year_droplist=strtrim(string(indgen(36)+1980),2)

  STATE = { $
		widget_top : STAT_RT_COUNTY,$
;        ARR_YEAR		:	[ '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998','1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006',$
;        						'2007', '2008', '2009', '2010', '2011', '2012', '2013'] ,$
;		ARR_YEAR		:	year_droplist , $
;		ARR_MONTH		:	[ '1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12' ],$
		ARR_DAY			:	['1', '2', '3','4', '5', '6', '7', '8', '9', '10', $
							'11', '12', '13','14', '15', '16', '17', '18', '19', '20',$
							'21', '22', '23','24', '25', '26', '27', '28', '29', '30', '31'] ,$
		ARR_DATA_TYPE	:	['NDVI','LAI','NPP'] ,$
    	ARR_SENSOR_TYPE	:	['AVHRR','MODIS','VGT'] ,$
    	ARR_SENSOR_CODE	:	[1,2,3] ,$

		ARR_RANGE			:	STRARR(70) ,$
		ARR_RANGE_CODE		:	STRARR(70) ,$
		ARR_RANGE_ABBR_NAME	:	STRARR(70) ,$

		ARR_RANGE_BATCH				:	STRARR(70) ,$
		ARR_RANGE_CODE_BATCH		:	STRARR(70) ,$
		ARR_RANGE_ABBR_NAME_BATCH	:	STRARR(70) ,$


		ARR_STATE_MONITOR		:	STRARR(70) ,$
		ARR_STATE_MONITOR_BATCH	:	STRARR(70) ,$

		;��¼�����ĸ��ߵ�����
		ARR_X_LEFT		:	FLTARR(70)	,$
		ARR_X_RIGHT		:	FLTARR(70)	,$
		ARR_Y_TOP		:	FLTARR(70)	,$
		ARR_Y_BOTTOM	:	FLTARR(70)	,$

		ARR_X_LEFT_BATCH	:	FLTARR(70)	,$
		ARR_X_RIGHT_BATCH	:	FLTARR(70)	,$
		ARR_Y_TOP_BATCH		:	FLTARR(70)	,$
		ARR_Y_BOTTOM_BATCH	:	FLTARR(70)	,$


		YEAR			:	1990	,$
		MONTH			:	1		,$
		DAY				:	1		,$
		YEAR_START		:	1990	,$
		MONTH_START		:	1		,$
		DAY_START		:	1		,$
		YEAR_END		:	1990	,$
		MONTH_END		:	1		,$
		DAY_END			:	1		,$

		X_LEFT			:	0.0		,$
		X_RIGHT			:	0.0		,$
		Y_TOP			:	0.0		,$
		Y_BOTTOM		:	0.0		,$

		X_LEFT_BATCH	:	0.0		,$
		X_RIGHT_BATCH	:	0.0		,$
		Y_TOP_BATCH		:	0.0		,$
		Y_BOTTOM_BATCH	:	0.0		,$

		DATA_TYPE	    	:	'NDVI'		,$
		DATA_TYPE_BATCH 	:	'NDVI'		,$

		SENSOR_TYPE	    	:	'AVHRR'		,$
		SENSOR_CODE			:	1			,$
		SENSOR_CODE_BATCH	:	1			,$
		SENSOR_TYPE_BATCH 	:	'AVHRR'		,$

		PERIOD				:	1			,$
		PERIOD_BATCH		:	1			,$

		RANGE			:	'��ѡ��'		,$
		RANGE_BATCH		:	'��ѡ��'		,$
		RANGE_CODE		:	''				,$
		RANGE_CODE_BATCH	:	''			,$
		RANGE_ABBR_NAME	:	''				,$
		RANGE_ABBR_NAME_BATCH	:	''				,$
		STATE_MONITOR		:	0					,$
		STATE_MONITOR_BATCH	:	0					,$

		TXT_FILE_IN			:	TXT_FILE_IN			,$
		TXT_SUFFIX			:	TEXT_SUFFIX			,$
		TXT_PREFIX			:	TEXT_PREFIX			,$
		TXT_NAME_SHOW		:	TEXT_NAME_SHOW		,$
		TXT_FILE_PATH		:	TXT_FILE_PATH		,$


		SINGLE_BATCH		:	'SINGLE'			,$


		NUM_OF_RANGE_BATCH	:	0					 	,$
		NUM_OF_RANGE		:	0					 	 $


        }



    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, STAT_RT_COUNTY, SET_UVALUE=PSTATE
    WIDGET_CONTROL, CMD_pick_date, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_s, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_s, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_e, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_e, set_uvalue=staff
    Widget_Control, /REALIZE, STAT_RT_COUNTY
    WIDGET_CONTROL,CMD_CLOSE_STA_PRO,/INPUT_FOCUS

  ;************************************************************************

  ;***************************************************************************
  WIDGET_CONTROL,DST_DATA_TYPE,SET_VALUE=(*PSTATE).ARR_DATA_TYPE
  WIDGET_CONTROL,DST_DATA_TYPE_BATCH,SET_VALUE=(*PSTATE).ARR_DATA_TYPE

  WIDGET_CONTROL,DST_SENSOR_TYPE,SET_VALUE=(*PSTATE).ARR_SENSOR_TYPE
  WIDGET_CONTROL,DST_SENSOR_TYPE_BATCH,SET_VALUE=(*PSTATE).ARR_SENSOR_TYPE

;  WIDGET_CONTROL,DST_YEAR,SET_VALUE=(*PSTATE).ARR_YEAR
;  WIDGET_CONTROL,DST_MONTH,SET_VALUE=(*PSTATE).ARR_MONTH
;  WIDGET_CONTROL,DST_DAY,SET_VALUE=(*PSTATE).ARR_DAY
;
;  WIDGET_CONTROL,DST_YEAR_START,SET_VALUE=(*PSTATE).ARR_YEAR
;  WIDGET_CONTROL,DST_MONTH_START,SET_VALUE=(*PSTATE).ARR_MONTH
;  WIDGET_CONTROL,DST_DAY_START,SET_VALUE=(*PSTATE).ARR_DAY
;
;  WIDGET_CONTROL,DST_YEAR_END,SET_VALUE=(*PSTATE).ARR_YEAR
;  WIDGET_CONTROL,DST_MONTH_END,SET_VALUE=(*PSTATE).ARR_MONTH
;  WIDGET_CONTROL,DST_DAY_END,SET_VALUE=(*PSTATE).ARR_DAY

  WIDGET_CONTROL,DST_PERIOD,SET_VALUE=(*PSTATE).ARR_DAY
  WIDGET_CONTROL,DST_PERIOD_BATCH,SET_VALUE=(*PSTATE).ARR_DAY
  ;************************************************************************


  ;***********************************************************************
  ;Ϊ��ϵͳ��ʾ�����ӵĴ���
  TEMP=1
  IF(TEMP EQ 1)THEN BEGIN

;		time_current=bin_date()
;  		year_index=time_current[0]-1980
;  		month_index=time_current[1]-1
;  		day_index=time_current[2]-1
;
;	  	WIDGET_CONTROL,DST_YEAR, SET_COMBOBOX_SELECT=year_index
;	  	WIDGET_CONTROL,DST_MONTH, SET_DROPLIST_SELECT=month_index
;	  	WIDGET_CONTROL,DST_DAY, SET_DROPLIST_SELECT=day_index
;
;	  	WIDGET_CONTROL,DST_YEAR_START, SET_COMBOBOX_SELECT=year_index
;	  	WIDGET_CONTROL,DST_MONTH_START, SET_DROPLIST_SELECT=month_index
;	  	WIDGET_CONTROL,DST_DAY_START, SET_DROPLIST_SELECT=day_index
;
;	  	WIDGET_CONTROL,DST_YEAR_END, SET_COMBOBOX_SELECT=year_index
;	  	WIDGET_CONTROL,DST_MONTH_END, SET_DROPLIST_SELECT=month_index
;	  	WIDGET_CONTROL,DST_DAY_END, SET_DROPLIST_SELECT=day_index
	  	WIDGET_CONTROL,DST_PERIOD,SET_DROPLIST_SELECT=9
	  	WIDGET_CONTROL,DST_PERIOD_BATCH,SET_DROPLIST_SELECT=9


	  	FILE_TEMP=zs_out_path
	  	WIDGET_CONTROL,TXT_FILE_IN, SET_VALUE=FILE_TEMP
	  	(*PSTATE).YEAR	=	strtrim(c_year,2)
	  	(*PSTATE).MONTH	=	strtrim(c_month,2)
	  	(*PSTATE).DAY	=	strtrim(c_day,2)
	  	(*PSTATE).PERIOD=	10

		(*PSTATE).YEAR_START =	strtrim(c_year,2)
	  	(*PSTATE).MONTH_START=	strtrim(c_month,2)
	  	(*PSTATE).DAY_START	 =	strtrim(c_day,2)
	  	(*PSTATE).YEAR_END =	strtrim(c_year,2)
	  	(*PSTATE).MONTH_END=	strtrim(c_month,2)
	  	(*PSTATE).DAY_END	 =	strtrim(c_day,2)
	  	(*PSTATE).PERIOD_BATCH = 10

  ENDIF
  ;***********************************************************************
	defaultnames_zsrtsta,{ID:(*PSTATE).widget_top, TOP:(*PSTATE).widget_top}

  XManager, 'STAT_RT_COUNTY', STAT_RT_COUNTY, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'

end
;

pro defaultnames_zsrtsta, event
	Widget_Control, event.top, get_uvalue=pstate

	COMMON COMMON_SETPATH,ppath

	v_zs_out_path  = (*ppath).zs_out_path

	clf_prefix=(*ppath).clf_prefix
	clf_suffix=(*ppath).clf_suffix

	year = strtrim((*PSTATE).YEAR,2)
	;�����µ�λ������
	IF ((*PSTATE).MONTH LE 9) THEN BEGIN
		month='0'+STRTRIM((*PSTATE).MONTH,2)
	ENDIF ELSE BEGIN
		month=STRTRIM((*PSTATE).MONTH,2)
	ENDELSE
	;�������λ������
	IF ((*PSTATE).DAY LE 9) THEN BEGIN
		day='0'+STRTRIM((*PSTATE).DAY,2)
	ENDIF ELSE BEGIN
		day=STRTRIM((*PSTATE).DAY,2)
	ENDELSE

	time = year + month + day

;	case (*PSTATE).DATA_TYPE of
;		'NDVI':begin
;			prefix = ndvi_prefix
;			suffix = ndvi_suffix
;		end
;		'LAI':begin
;			prefix = lai_prefix
;			suffix = lai_suffix
;		end
;		'NPP':begin
;			prefix = npp_prefix
;			suffix = npp_suffix
;		end
;		else:begin
;			prefix = ndvi_prefix
;			suffix = ndvi_suffix
;		end
;	endcase
	prefix = clf_prefix
	suffix = clf_suffix

  	WIDGET_CONTROL,(*PSTATE).TXT_FILE_IN, SET_VALUE= v_zs_out_path + prefix + time + suffix

;	case (*PSTATE).DATA_TYPE_BATCH of
;		'NDVI':begin
;			prefix = ndvi_prefix
;			suffix = ndvi_suffix
;		end
;		'LAI':begin
;			prefix = lai_prefix
;			suffix = lai_suffix
;		end
;		'NPP':begin
;			prefix = npp_prefix
;			suffix = npp_suffix
;		end
;		else:begin
;			prefix = ndvi_prefix
;			suffix = ndvi_suffix
;		end
;	endcase

	WIDGET_CONTROL,(*PSTATE).TXT_FILE_PATH, SET_VALUE= v_zs_out_path
	WIDGET_CONTROL,(*PSTATE).TXT_PREFIX, SET_VALUE = prefix
	WIDGET_CONTROL,(*PSTATE).TXT_SUFFIX, SET_VALUE = suffix
 end