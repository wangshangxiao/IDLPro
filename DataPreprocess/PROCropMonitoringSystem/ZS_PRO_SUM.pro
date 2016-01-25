;
; �ɼ̻�
; 20060824ʡ��ũ��ң�м��ϵͳ�еĻ���ģ��
; ��ԭ��ȫ������ң�м��ϵͳ�Ļ������޸Ķ���
;-----------------------------------------------------------------

FUNCTION ZS_COUNT_STA_PRO_ABOVE_COUNTY,RANGE_NAME,RANGE_CODE,YEAR,MONTH,DAY,DATA_TYPE,SCALE,$
                           SENSOR_CODE,PERIOD
    ;*************************************************************************************
    ;����ϵͳ����Ҫ����,���ݲ�ͬ�ĳ߶�ѡ���˲�ͬ�ı���в�ѯ
    ;*************************************************************************************
    on_error,2

    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year_1,DSN,USER_NAME,PWD,PROVINCE_CODE
    ;��ȡ���ݹ����Ĳ���
    RANGE_NAME=RANGE_NAME
    RANGE_CODE=RANGE_CODE
    YEAR=YEAR
    MONTH=MONTH
    DAY=DAY
    DATA_TYPE=STRTRIM(DATA_TYPE,2)
    OD=DBobj
    SCALE=SCALE
    IF(DATA_TYPE EQ 'NDVI') THEN DATA_TYPE='NDVI_H'

    ;�����������ʾ���ݿ������µ������������,Ŀǰʹ�õ���2004���
    YEAR_GVG='2004'

    AREA_PLOWLAND_TOTAL     =0.0
    AREA_PADDY_FIELD_TOTAL    =0.0
    AREA_DRY_LAND_TOTAL     =0.0
    AREA_SPRING_WHEAT_TOTAL   =0.0
    AREA_WINTER_WHEAT_TOTAL   =0.0
    AREA_EARLY_RICE_TOTAL =0.0
    AREA_SEMILATE_RICE_TOTAL=0.0
    AREA_LATE_RICE_TOTAL  =0.0
    AREA_CORN_TOTAL   =0.0
    AREA_SOYBEAN_TOTAL       =0.0

    ADD_UP_PLOWLAND       =0.0
    ADD_UP_PADDY_FIELD =0.0
    ADD_UP_DRY_LAND       =0.0

    ADD_UP_WINTER_WHEAT    =0.0
    ADD_UP_SPRING_WHEAT    =0.0

    ADD_UP_EARLY_RICE =0.0
    ADD_UP_SEMILATE_RICE=0.0
    ADD_UP_LATE_RICE  =0.0
    ADD_UP_CORN   =0.0
    ADD_UP_SOYBEAN       =0.0

    ;����һ��SQL���
    ;*******************************************************************************************

    ;*******************************************************************************
    ;modified 20070607
    ;PROVINCE_CODE='230000'
    IF(SCALE EQ 'ȫʡ') THEN BEGIN
		RANGE_CODE=PROVINCE_CODE
      ;**********************************************************************
      ;(1)�ȴ������\ˮ��\�ͺ��ص�,(���������м�)
       SQL='SELECT T1.PLOWLAND,T1.PADDY_FIELD,T1.DRY_LAND,'
       SQL=SQL+'T2.avg_plowland,T2.avg_paddy_field,T2.avg_dry_land '
       SQL=SQL+'FROM PLOWLAND_AREA_COUNTY T1,PARAMETER_PROCESS_COUNTY T2 '
      SQL=SQL+' WHERE (T2.COUNTY_CODE IN '
      SQL=SQL+'(SELECT CODE FROM COUNTY_CODE WHERE PROVINCE_CODE = '
      SQL=SQL+"'"+PROVINCE_CODE+"'"+')) AND '

       SQL=SQL+'(T1.COUNTY_CODE=T2.COUNTY_CODE) '
       SQL=SQL+'AND (T2.YEAR='+STRTRIM(YEAR,2)+') '
       SQL=SQL+'AND (T2.MONTH='+STRTRIM(MONTH,2)+') '
       SQL=SQL+'AND (T2.DAY='+STRTRIM(DAY,2)+')'

       SQL=SQL+' AND (T2.periods='+period+') '
       SQL=SQL+' AND (T2.DATA_TYPE='+"'"+STRTRIM(DATA_TYPE,2)+"'"+') '
       SQL=SQL+'AND (T2.SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') '
      PRINT,'SQL1',SQL

       ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
       NUM_OF_COUNTY=1
       IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

         REPEAT  BEGIN
          NUM_OF_COUNTY=NUM_OF_COUNTY+1
          AREA_PLOWLAND     =    FLOAT(ORS->GETFIELD(0))
          AREA_PADDY_FIELD    =  FLOAT(ORS->GETFIELD(1))
          AREA_DRY_LAND     =    FLOAT(ORS->GETFIELD(2))
          VALUE_PLOWLAND   =   FLOAT(ORS->GETFIELD(3))
          VALUE_PADDY_FIELD   = FLOAT(ORS->GETFIELD(4))
          VALUE_DRY_LAND   =   FLOAT(ORS->GETFIELD(5))

          ;PRINT,LAND_TYPE,AVG_VALUE,AREA
          IF(VALUE_PLOWLAND NE 0) THEN BEGIN
              AREA_PLOWLAND_TOTAL=AREA_PLOWLAND+AREA_PLOWLAND_TOTAL
              ADD_UP_PLOWLAND=ADD_UP_PLOWLAND+AREA_PLOWLAND*VALUE_PLOWLAND
          ENDIF
          IF(VALUE_PADDY_FIELD NE 0) THEN BEGIN
              AREA_PADDY_FIELD_TOTAL=AREA_PADDY_FIELD+AREA_PADDY_FIELD_TOTAL
              ADD_UP_PADDY_FIELD=ADD_UP_PADDY_FIELD+AREA_PADDY_FIELD*VALUE_PADDY_FIELD
          ENDIF
          IF(VALUE_DRY_LAND NE 0) THEN BEGIN
              AREA_DRY_LAND_TOTAL=AREA_DRY_LAND+AREA_DRY_LAND_TOTAL
              ADD_UP_DRY_LAND=ADD_UP_DRY_LAND+AREA_DRY_LAND*VALUE_DRY_LAND
          ENDIF
         ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
       ENDIF
       OBJ_DESTROY,ORS
      ;**********************************************************************
      ;**********************************************************************
      ;*******************************************************************************
       ;(2)�ٴ�����������ֵ(Ҳ����������֮��)
       ;Ҳ�Ƿ�NDVI�ͷ�NDVI�������,��ǰ�����\ˮ��\���صĴ�������ͬ
       SQL='SELECT '
       SQL=SQL+'T1.WINTER_WHEAT,'
       SQL=SQL+'T1.SPRING_WHEAT,'
       SQL=SQL+'T1.EARLY_RICE,'
       SQL=SQL+'T1.SEMILATE_RICE,'
       SQL=SQL+'T1.LATE_RICE,'
       ;SQL=SQL+'T1.CORN,'
       ;����ԭ�����������׷�Ϊ�������׺�������,
       ;�ʶ�ԭ����SQL���������޸�
       ;�޸���,2007.03.22,MJH
       SQL=SQL+'T1.SUMMER_CORN+SPRING_CORN,'
       SQL=SQL+'T1.SOYBEAN,'

       SQL=SQL+'T2.AVG_WINTER_WHEAT,'
       SQL=SQL+'T2.AVG_SPRING_WHEAT,'
       SQL=SQL+'T2.AVG_EARLY_RICE,'
       SQL=SQL+'T2.AVG_SEMILATE_RICE,'
       SQL=SQL+'T2.AVG_LATE_RICE,'
       SQL=SQL+'T2.AVG_MAIZE,'
       SQL=SQL+'T2.AVG_SOYBEAN '

       SQL=SQL+'FROM CROP_AREA_COUNTY T1,PARAMETER_PROCESS_COUNTY T2 '
       SQL=SQL+' WHERE (T2.COUNTY_CODE IN '
      SQL=SQL+'(SELECT CODE FROM COUNTY_CODE WHERE PROVINCE_CODE = '
      SQL=SQL+"'"+PROVINCE_CODE+"'"+')) AND '

      SQL=SQL+'(T1.COUNTY_CODE=T2.COUNTY_CODE) '
      SQL=SQL+'AND (T1.YEAR='+YEAR_GVG+') '
       SQL=SQL+'AND (T2.YEAR='+STRTRIM(YEAR,2)+') '
       SQL=SQL+'AND (T2.MONTH='+STRTRIM(MONTH,2)+') '
       SQL=SQL+'AND (T2.DAY='+STRTRIM(DAY,2)+')'

       SQL=SQL+' AND (T2.periods='+period+') '
       SQL=SQL+' AND (T2.DATA_TYPE='+"'"+STRTRIM(DATA_TYPE,2)+"'"+') '
       SQL=SQL+'AND (T2.SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') '
       ;SQL=SQL+'ORDER BY YEAR,MONTH,DAY'
      PRINT,'SQL2',SQL

      ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
       NUM_OF_COUNTY=1
       IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

         WHILE (ORS->MOVECURSOR(/NEXT) EQ 1) DO BEGIN
          NUM_OF_COUNTY=NUM_OF_COUNTY+1
          AREA_WINTER_WHEAT   = FLOAT(ORS->GETFIELD(0))
          AREA_SPRING_WHEAT   = FLOAT(ORS->GETFIELD(1))
          AREA_EARLY_RICE     =  FLOAT(ORS->GETFIELD(2))
          AREA_SEMILATE_RICE  =    FLOAT(ORS->GETFIELD(3))
          AREA_LATE_RICE   =   FLOAT(ORS->GETFIELD(4))
          AREA_CORN      =   FLOAT(ORS->GETFIELD(5))
          AREA_SOYBEAN       = FLOAT(ORS->GETFIELD(6))

          VALUE_WINTER_WHEAT  =    FLOAT(ORS->GETFIELD(7))
          VALUE_SPRING_WHEAT  =    FLOAT(ORS->GETFIELD(8))
          VALUE_EARLY_RICE    =  FLOAT(ORS->GETFIELD(9))
          VALUE_SEMILATE_RICE =   FLOAT(ORS->GETFIELD(10))
          VALUE_LATE_RICE     =  FLOAT(ORS->GETFIELD(11))
          VALUE_CORN       =  FLOAT(ORS->GETFIELD(12))
          VALUE_SOYBEAN     =    FLOAT(ORS->GETFIELD(13))

          IF(VALUE_WINTER_WHEAT NE 0) THEN BEGIN
              AREA_WINTER_WHEAT_TOTAL=AREA_WINTER_WHEAT+AREA_WINTER_WHEAT_TOTAL
              ADD_UP_WINTER_WHEAT=ADD_UP_WINTER_WHEAT+AREA_WINTER_WHEAT*VALUE_WINTER_WHEAT
          ENDIF
          IF(VALUE_SPRING_WHEAT NE 0) THEN BEGIN
              AREA_SPRING_WHEAT_TOTAL=AREA_SPRING_WHEAT+AREA_SPRING_WHEAT_TOTAL
              ADD_UP_SPRING_WHEAT=ADD_UP_SPRING_WHEAT+AREA_SPRING_WHEAT*VALUE_SPRING_WHEAT
          ENDIF
          IF(VALUE_EARLY_RICE NE 0) THEN BEGIN
              AREA_EARLY_RICE_TOTAL=AREA_EARLY_RICE+AREA_EARLY_RICE_TOTAL
              ADD_UP_EARLY_RICE=ADD_UP_EARLY_RICE+AREA_EARLY_RICE*VALUE_EARLY_RICE
          ENDIF
          IF(VALUE_SEMILATE_RICE NE 0) THEN BEGIN
              AREA_SEMILATE_RICE_TOTAL=AREA_SEMILATE_RICE+AREA_SEMILATE_RICE_TOTAL
              ADD_UP_SEMILATE_RICE=ADD_UP_SEMILATE_RICE+AREA_SEMILATE_RICE*VALUE_SEMILATE_RICE
          ENDIF
          IF(VALUE_LATE_RICE NE 0) THEN BEGIN
              AREA_LATE_RICE_TOTAL=AREA_LATE_RICE+AREA_LATE_RICE_TOTAL
              ADD_UP_LATE_RICE=ADD_UP_LATE_RICE+AREA_LATE_RICE*VALUE_LATE_RICE
          ENDIF
          IF(VALUE_CORN NE 0) THEN BEGIN
              AREA_CORN_TOTAL=AREA_CORN+AREA_CORN_TOTAL
              ADD_UP_CORN=ADD_UP_CORN+AREA_CORN*VALUE_CORN
          ENDIF

          IF(VALUE_SOYBEAN NE 0) THEN BEGIN
              AREA_SOYBEAN_TOTAL=AREA_SOYBEAN+AREA_SOYBEAN_TOTAL
              ADD_UP_SOYBEAN=ADD_UP_SOYBEAN+AREA_SOYBEAN*VALUE_SOYBEAN
          ENDIF
         ENDWHILE
       ENDIF

    ;**********************************************************************
    ;����ʡ�߶���������(�Զ�������)
    ;**********************************************************************
    ENDIF ELSE BEGIN

      ;(1)�ȴ������\ˮ��\�ͺ��ص�,(���������м�)
       SQL='SELECT T1.PLOWLAND,T1.PADDY_FIELD,T1.DRY_LAND,'
       SQL=SQL+'T2.avg_plowland,T2.avg_paddy_field,T2.avg_dry_land '
       SQL=SQL+'FROM PLOWLAND_AREA_COUNTY T1,PARAMETER_PROCESS_COUNTY T2 '
      SQL=SQL+' WHERE (T2.COUNTY_CODE IN '
      SQL=SQL+'(SELECT COUNTY_CODE FROM COUNTY_TO_ROI WHERE ROI_CODE = '
      SQL=SQL+"'"+RANGE_CODE+"'"+')) AND '

       SQL=SQL+'(T1.COUNTY_CODE=T2.COUNTY_CODE) '
       SQL=SQL+'AND (T2.YEAR='+STRTRIM(YEAR,2)+') '
       SQL=SQL+'AND (T2.MONTH='+STRTRIM(MONTH,2)+') '
       SQL=SQL+'AND (T2.DAY='+STRTRIM(DAY,2)+')'

       SQL=SQL+' AND (T2.periods='+period+') '
       SQL=SQL+' AND (T2.DATA_TYPE='+"'"+STRTRIM(DATA_TYPE,2)+"'"+') '
       SQL=SQL+'AND (T2.SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') '

      PRINT,'SQL20050128',SQL

      ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
       NUM_OF_COUNTY=1
       IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

         WHILE (ORS->MOVECURSOR(/NEXT) EQ 1) DO BEGIN
          NUM_OF_COUNTY=NUM_OF_COUNTY+1
          AREA_PLOWLAND     =    FLOAT(ORS->GETFIELD(0))
          AREA_PADDY_FIELD    =  FLOAT(ORS->GETFIELD(1))
          AREA_DRY_LAND     =    FLOAT(ORS->GETFIELD(2))
          VALUE_PLOWLAND   =   FLOAT(ORS->GETFIELD(3))
          VALUE_PADDY_FIELD   = FLOAT(ORS->GETFIELD(4))
          VALUE_DRY_LAND   =   FLOAT(ORS->GETFIELD(5))

          ;PRINT,LAND_TYPE,AVG_VALUE,AREA
          IF(VALUE_PLOWLAND NE 0) THEN BEGIN
              AREA_PLOWLAND_TOTAL=AREA_PLOWLAND+AREA_PLOWLAND_TOTAL
              ADD_UP_PLOWLAND=ADD_UP_PLOWLAND+AREA_PLOWLAND*VALUE_PLOWLAND
          ENDIF
          IF(VALUE_PADDY_FIELD NE 0) THEN BEGIN
              AREA_PADDY_FIELD_TOTAL=AREA_PADDY_FIELD+AREA_PADDY_FIELD_TOTAL
              ADD_UP_PADDY_FIELD=ADD_UP_PADDY_FIELD+AREA_PADDY_FIELD*VALUE_PADDY_FIELD
          ENDIF
          IF( VALUE_DRY_LAND NE 0) THEN BEGIN
              AREA_DRY_LAND_TOTAL=AREA_DRY_LAND+AREA_DRY_LAND_TOTAL
              ADD_UP_DRY_LAND=ADD_UP_DRY_LAND+AREA_DRY_LAND*VALUE_DRY_LAND
          ENDIF
         ENDWHILE
       ENDIF
       OBJ_DESTROY,ORS
       ;*******************************************************************************
       ;*******************************************************************************
       ;(2)�ٴ�����������ֵ(Ҳ����������֮��)
       ;Ҳ�Ƿ�NDVI�ͷ�NDVI�������,��ǰ�����\ˮ��\���صĴ�������ͬ
       SQL='SELECT '
       SQL=SQL+'T1.WINTER_WHEAT,'
       SQL=SQL+'T1.SPRING_WHEAT,'
       SQL=SQL+'T1.EARLY_RICE,'
       SQL=SQL+'T1.SEMILATE_RICE,'
       SQL=SQL+'T1.LATE_RICE,'
       ;SQL=SQL+'T1.CORN,'
       SQL=SQL+'T1.SPRING_CORN+T1.SUMMER_CORN,'
       SQL=SQL+'T1.SOYBEAN,'

       SQL=SQL+'T2.AVG_PLOWLAND,'
       SQL=SQL+'T2.AVG_PADDY_FIELD,'
       SQL=SQL+'T2.AVG_DRY_LAND '

       SQL=SQL+'FROM CROP_AREA_COUNTY T1,PARAMETER_PROCESS_COUNTY T2 '
       SQL=SQL+' WHERE (T2.COUNTY_CODE IN '
      SQL=SQL+'(SELECT COUNTY_CODE FROM COUNTY_TO_ROI WHERE ROI_CODE = '
      SQL=SQL+"'"+RANGE_CODE+"'"+')) AND '

      SQL=SQL+'(T1.COUNTY_CODE=T2.COUNTY_CODE) '
      SQL=SQL+'AND (T1.YEAR='+YEAR_GVG+') '
       SQL=SQL+'AND (T2.YEAR='+STRTRIM(YEAR,2)+') '
       SQL=SQL+'AND (T2.MONTH='+STRTRIM(MONTH,2)+') '
       SQL=SQL+'AND (T2.DAY='+STRTRIM(DAY,2)+')'

       SQL=SQL+' AND (T2.periods='+period+') '
       SQL=SQL+' AND (T2.DATA_TYPE='+"'"+STRTRIM(DATA_TYPE,2)+"'"+') '
       SQL=SQL+'AND (T2.SENSOR_CODE='+"'"+SENSOR_CODE+"'"+') '

      ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
       NUM_OF_COUNTY=1
       IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

         WHILE (ORS->MOVECURSOR(/NEXT) EQ 1) DO BEGIN
          NUM_OF_COUNTY=NUM_OF_COUNTY+1
          AREA_WINTER_WHEAT   = FLOAT(ORS->GETFIELD(0))
          AREA_SPRING_WHEAT   = FLOAT(ORS->GETFIELD(1))
          AREA_EARLY_RICE     =  FLOAT(ORS->GETFIELD(2))
          AREA_SEMILATE_RICE  =    FLOAT(ORS->GETFIELD(3))
          AREA_LATE_RICE   =   FLOAT(ORS->GETFIELD(4))
          AREA_CORN      =   FLOAT(ORS->GETFIELD(5))
          AREA_SOYBEAN       = FLOAT(ORS->GETFIELD(6))

          VALUE_PLOWLAND   =   FLOAT(ORS->GETFIELD(7))
          VALUE_PADDY_FIELD   = FLOAT(ORS->GETFIELD(8))
          VALUE_DRY_LAND   =   FLOAT(ORS->GETFIELD(9))

          IF(VALUE_PADDY_FIELD NE 0) THEN BEGIN
              AREA_EARLY_RICE_TOTAL=AREA_EARLY_RICE+AREA_EARLY_RICE_TOTAL
              ADD_UP_EARLY_RICE=ADD_UP_EARLY_RICE+AREA_EARLY_RICE*VALUE_PADDY_FIELD

              AREA_SEMILATE_RICE_TOTAL=AREA_SEMILATE_RICE+AREA_SEMILATE_RICE_TOTAL
              ADD_UP_SEMILATE_RICE=ADD_UP_SEMILATE_RICE+AREA_SEMILATE_RICE*VALUE_PADDY_FIELD

              AREA_LATE_RICE_TOTAL=AREA_LATE_RICE+AREA_LATE_RICE_TOTAL
              ADD_UP_LATE_RICE=ADD_UP_LATE_RICE+AREA_LATE_RICE*VALUE_PADDY_FIELD
          ENDIF
          IF(VALUE_DRY_LAND NE 0) THEN BEGIN

              AREA_WINTER_WHEAT_TOTAL=AREA_WINTER_WHEAT+AREA_WINTER_WHEAT_TOTAL
              ADD_UP_WINTER_WHEAT=ADD_UP_WINTER_WHEAT+AREA_WINTER_WHEAT*VALUE_DRY_LAND

              AREA_SPRING_WHEAT_TOTAL=AREA_SPRING_WHEAT+AREA_SPRING_WHEAT_TOTAL
              ADD_UP_SPRING_WHEAT=ADD_UP_SPRING_WHEAT+AREA_SPRING_WHEAT*VALUE_DRY_LAND

              AREA_SOYBEAN_TOTAL=AREA_SOYBEAN+AREA_SOYBEAN_TOTAL
              ADD_UP_SOYBEAN=ADD_UP_SOYBEAN+AREA_SOYBEAN*VALUE_DRY_LAND

              AREA_CORN_TOTAL=AREA_CORN+AREA_CORN_TOTAL
              ADD_UP_CORN=ADD_UP_CORN+AREA_CORN*VALUE_DRY_LAND

          ENDIF
         ENDWHILE
       ENDIF

    ENDELSE


    ;����һ���ṹ������Ų�ѯ�Ľ��
    RESULT={$
       RANGE_NAME       : RANGE_NAME  ,$
       RANGE_CODE       : RANGE_CODE  ,$

       VALUE_PLOWLAND       : 0.0 ,$
       VALUE_PADDY_FIELD :   0.0   ,$
       VALUE_DRY_LAND       : 0.0 ,$

       VALUE_WINTER_WHEAT    :  0.0  ,$
       VALUE_SPRING_WHEAT    :  0.0  ,$
       VALUE_EARLY_RICE  :    0.0    ,$
       VALUE_SEMILATE_RICE   : 0.0 ,$
       VALUE_LATE_RICE     :    0.0    ,$

       VALUE_CORN    :  0.0  ,$
       VALUE_SOYBEAN     :  0.0   $

       }

    ;�Խṹ��ı������з�ֵ

    IF(AREA_PLOWLAND_TOTAL NE 0) THEN RESULT.VALUE_PLOWLAND=ADD_UP_PLOWLAND/AREA_PLOWLAND_TOTAL
    IF(AREA_PADDY_FIELD_TOTAL NE 0) THEN RESULT.VALUE_PADDY_FIELD=ADD_UP_PADDY_FIELD/AREA_PADDY_FIELD_TOTAL
    IF(AREA_DRY_LAND_TOTAL NE 0) THEN RESULT.VALUE_DRY_LAND=ADD_UP_DRY_LAND/AREA_DRY_LAND_TOTAL

    IF(AREA_WINTER_WHEAT_TOTAL NE 0) THEN RESULT.VALUE_WINTER_WHEAT=ADD_UP_WINTER_WHEAT/AREA_WINTER_WHEAT_TOTAL
    IF(AREA_SPRING_WHEAT_TOTAL NE 0) THEN RESULT.VALUE_SPRING_WHEAT=ADD_UP_SPRING_WHEAT/AREA_SPRING_WHEAT_TOTAL

    IF(AREA_EARLY_RICE_TOTAL NE 0) THEN RESULT.VALUE_EARLY_RICE=ADD_UP_EARLY_RICE/AREA_EARLY_RICE_TOTAL
    IF(AREA_SEMILATE_RICE_TOTAL NE 0) THEN RESULT.VALUE_SEMILATE_RICE=ADD_UP_SEMILATE_RICE/AREA_SEMILATE_RICE_TOTAL
    IF(AREA_LATE_RICE_TOTAL NE 0) THEN RESULT.VALUE_LATE_RICE=ADD_UP_LATE_RICE/AREA_LATE_RICE_TOTAL

    IF(AREA_CORN_TOTAL NE 0) THEN RESULT.VALUE_CORN=ADD_UP_CORN/AREA_CORN_TOTAL
    IF(AREA_SOYBEAN_TOTAL NE 0) THEN RESULT.VALUE_SOYBEAN=ADD_UP_SOYBEAN/AREA_SOYBEAN_TOTAL


    OBJ_DESTROY, ORS
    ;HELP,RESULT,/STRUCTURE
    RETURN,RESULT

END




;-----------------------------------------------------------------
FUNCTION CMD_ZS_COUNT_STA_PRO_ABOVE_COUNTY, EVENT

    ;**************************************************************
    ;����ģ���������,����˻��ܵĹ���
    ;**************************************************************
    on_error,2

    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year_1,DSN,USER_NAME,PWD,PROVINCE_CODE

    WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
    SCALE=(*PSTATE).SCALE

    ;���û��ѡ��Χ���˳�ϵͳ
    IF(SCALE EQ '��ѡ��߶�') THEN BEGIN
         MSG=DIALOG_MESSAGE('����ѡ��߶�!',TITLE='��ʾ',/INFORMATION)
         CLOSE,/ALL
		 log, '���̼��-����', -1
         RETURN,0
    ENDIF

    ;����һ��״̬����ʾ���ڶ�ȡ����
    ;F_READ_DATA=READING_DATA( GROUP_LEADER=(*PSTATE).BASE_STA_PRO_ABOVE_COUNTY)

    ;���Ȼ��Ҫ��ѯʹ�õĲ���
    YEAR    =(*PSTATE).YEAR
    MONTH   =(*PSTATE).MONTH
    DAY     =(*PSTATE).DAY

    RANGE   =(*PSTATE).RANGE
    RANGE_NAME	=(*PSTATE).RANGE
    NUM_OF_RANGE    =  (*PSTATE).NUM_OF_RANGE
    RANGE_CODE  =(*PSTATE).RANGE_CODE
    PERIOD		=(*PSTATE).PERIOD
    SENSOR_CODE	=(*PSTATE).SENSOR_CODE

    DATA_TYPE   =STRTRIM((*PSTATE).DATA_TYPE,2)
    SENSOR_CODE=STRTRIM((*PSTATE).SENSOR_CODE,2)
    PERIOD=STRTRIM((*PSTATE).PERIOD,2)


    IF(RANGE EQ (*PSTATE).ARR_RANGE[0] AND (SCALE NE 'ȫʡ')) THEN BEGIN ;ͬʱ�����е�ʡ���л���

       (*PSTATE).ALL_RANGE=1 ;ͬʱ�����е�ʡ���л���
       RESULT=STRARR(13+3,NUM_OF_RANGE-1)

       ;***************************************************************
       progressTimer = Obj_New("ShowProgress", tlb,/CancelButton)
       progressTimer->start
       ;***************************************************************
       ;ͨ��FORѭ�������е�ʡ���в�ѯ
       FOR I=0,NUM_OF_RANGE-2 DO BEGIN
         ;***************************************************************
         CANCELLED = PROGRESSTIMER->CHECKCANCEL()
         IF CANCELLED THEN BEGIN
          OK = DIALOG_MESSAGE('�û���ֹ�˲���',TITLE='��ʾ')
          PROGRESSTIMER->DESTROY ;����������
          log, '���̼��-����', 0
          RETURN,0
         ENDIF
         PROGRESSTIMER->UPDATE, (FLOAT(I)/(NUM_OF_RANGE-1) * 100.0) ;��������
         ;***************************************************************
         WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,TABLE_YSIZE=NUM_OF_RANGE-1
			RANGE_NAME	=(*PSTATE).ARR_RANGE[I+1]
			RANGE_CODE	=(*PSTATE).ARR_RANGE_CODE[I+1]
			RESULT_TEMP=ZS_COUNT_STA_PRO_ABOVE_COUNTY(RANGE_NAME,RANGE_CODE,YEAR,MONTH,DAY,DATA_TYPE,SCALE,$
                           SENSOR_CODE,PERIOD)
         ;RETURN,0
         RESULT[0,I]=RESULT_TEMP.RANGE_NAME
         RESULT[1,I]=RESULT_TEMP.RANGE_CODE

         RESULT[2,I]=RESULT_TEMP.VALUE_PLOWLAND
         RESULT[3,I]=RESULT_TEMP.VALUE_PADDY_FIELD
         RESULT[4,I]=RESULT_TEMP.VALUE_DRY_LAND

         RESULT[5,I]=RESULT_TEMP.VALUE_SPRING_WHEAT
         RESULT[6,I]=RESULT_TEMP.VALUE_WINTER_WHEAT

         RESULT[7,I]=RESULT_TEMP.VALUE_EARLY_RICE
         RESULT[8,I]=RESULT_TEMP.VALUE_SEMILATE_RICE
         RESULT[9,I]=RESULT_TEMP.VALUE_LATE_RICE

         RESULT[10,I]=RESULT_TEMP.VALUE_CORN
         RESULT[11,I]=RESULT_TEMP.VALUE_SOYBEAN

         RESULT[12,I]=(*PSTATE).DATA_TYPE
         RESULT[13,I]=(*PSTATE).SENSOR_CODE
         RESULT[14,I]=(*PSTATE).PERIOD
       ENDFOR
       progressTimer->destroy ;

    ENDIF ELSE BEGIN   ;ֻ��һ��ʡ���л���

       (*PSTATE).ALL_RANGE=0 ;ֻ��һ��ʡ���л���
       WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,TABLE_YSIZE=1

       RESULT_TEMP   = ZS_COUNT_STA_PRO_ABOVE_COUNTY(RANGE,RANGE_CODE,YEAR,MONTH,DAY,DATA_TYPE,SCALE,$
                                  SENSOR_CODE,PERIOD)
       ;RETURN,0
       RESULT=STRARR(13+3)
       RESULT[0]=RESULT_TEMP.RANGE_NAME
       RESULT[1]=RESULT_TEMP.RANGE_CODE

       RESULT[2]=RESULT_TEMP.VALUE_PLOWLAND
       RESULT[3]=RESULT_TEMP.VALUE_PADDY_FIELD
       RESULT[4]=RESULT_TEMP.VALUE_DRY_LAND

       RESULT[5]=RESULT_TEMP.VALUE_SPRING_WHEAT
       RESULT[6]=RESULT_TEMP.VALUE_WINTER_WHEAT

       RESULT[7]=RESULT_TEMP.VALUE_EARLY_RICE
       RESULT[8]=RESULT_TEMP.VALUE_SEMILATE_RICE
       RESULT[9]=RESULT_TEMP.VALUE_LATE_RICE

       RESULT[10]=RESULT_TEMP.VALUE_CORN
       RESULT[11]=RESULT_TEMP.VALUE_SOYBEAN
       RESULT[12]=(*PSTATE).DATA_TYPE
       RESULT[13]=(*PSTATE).SENSOR_CODE
       RESULT[14]=(*PSTATE).PERIOD

    ENDELSE

    ;�ر����ڶ�ȡ���ݵĶԻ���
    ;WIDGET_CONTROL,(*F_READ_DATA).BASE_READ_DATA,/DESTROY

    ;��ֵд�뵽�����
    RESULT=STRTRIM(RESULT,2)
    ;PRINT,'RESULT',RESULT
    WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,SET_VALUE=RESULT

    ;�ı����ֵ,���������Ѿ����
    (*PSTATE).COUNT_DONE=1
    log, '���̼��-����', 0
    RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION CMD_WRITE_STA_PRO_ABOVE_COUNTY, EVENT

	on_error,2

    WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE

    ;��ɰѻ��ܵĽ��������⴦��Ĺ���
    ;modified 20070607
    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year_1,DSN,USER_NAME,PWD,PROVINCE_CODE
    ;PROVINCE_CODE='230000'
    ;1)���ȶ��Ƿ��Ѿ����й����ܽ�����֤,ֻ���ڻ��ܹ�������г���
    IF((*PSTATE).COUNT_DONE EQ 0) THEN BEGIN
        MSG=DIALOG_MESSAGE('���Ƚ��л���!',TITLE='��ʾ',/INFORMATION)
        CLOSE,/ALL
        RETURN,0
    ENDIF

    WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,GET_VALUE=ARR_RESULT

    ;2)��ȡ��Ӧ�Ĳ���
    YEAR   =STRTRIM(STRING((*PSTATE).YEAR)   ,2)
    MONTH   =STRTRIM(STRING((*PSTATE).MONTH),2)
    DAY     =STRTRIM(STRING((*PSTATE).DAY) ,2)
    RANGE   =(*PSTATE).RANGE
    RANGE_CODE  =(*PSTATE).RANGE_CODE
    SCALE   =(*PSTATE).SCALE

    DATA_TYPE     =(*PSTATE).DATA_TYPE

    NUM_OF_RANGE    =(*PSTATE).NUM_OF_RANGE
    SENSOR_CODE     =(*PSTATE).SENSOR_CODE
    PERIOD       =(*PSTATE).PERIOD

    CASE SCALE OF
        'ȫʡ' :BEGIN
         SCALE_SQL='PROVINCE'
         RANGE_CODE=PROVINCE_CODE
       END
       '�Զ�������':BEGIN
         SCALE_SQL='ROI'
       END
    ENDCASE

    ;3)������ж��Ƕ�һ��ʡ���Ƕ����е�ʡ�����˻���,���ܶ��ٸ�ʡ,�����ٸ�

    ;�����ݿ�����ȡ����
    OD=DBobj

    DIMENSION=(SIZE(ARR_RESULT))[0]
;   PRINT,DIMENSION
;   HELP,ARR_RESULT
    IF(DIMENSION EQ 1) THEN BEGIN
       ;A)�ȼ��ü�¼�ǲ����Ѿ�������,����Ѿ�����,ɾ��
       SQL='DELETE FROM PARAMETER_PROCESS_'+SCALE_SQL
       SQL=SQL+' WHERE  ('+SCALE_SQL+'_CODE = '+"'"+RANGE_CODE+"' "+')'
        SQL=SQL+'AND (YEAR = '+YEAR+')'
        SQL=SQL+'AND (MONTH = '+MONTH+') '
        SQL=SQL+'AND (DAY = '+DAY+') '

        SQL=SQL+'AND (SENSOR_CODE = '+"'"+STRTRIM(SENSOR_CODE,2)+"'"+')'
        SQL=SQL+'AND (DATA_TYPE = '+"'"+STRTRIM(DATA_TYPE,2)+"'"+')'
        SQL=SQL+'AND (PERIODS = '+STRTRIM(PERIOD,2)+')'
       PRINT,'SQL 20060825',SQL
        OD->EXECUTESQL,SQL

       ;B)�������������
       SQL='INSERT INTO PARAMETER_PROCESS_'+SCALE_SQL
       SQL=SQL+'('+SCALE_SQL+'_CODE ,YEAR ,MONTH ,DAY,'
       SQL=SQL+'AVG_PLOWLAND ,AVG_PADDY_FIELD,AVG_DRY_LAND,'
       SQL=SQL+'AVG_WINTER_WHEAT ,AVG_SPRING_WHEAT,'
       SQL=SQL+'AVG_EARLY_RICE ,AVG_SEMILATE_RICE,AVG_LATE_RICE,'
       SQL=SQL+'AVG_MAIZE,AVG_SOYBEAN,'
        SQL=SQL+'DATA_TYPE,SENSOR_CODE,PERIODS ) '
        SQL=SQL+'VALUES ('+"'"+RANGE_CODE+"'"+','
        SQL=SQL+STRTRIM(YEAR,2)     +','
        SQL=SQL+STRTRIM(MONTH,2)  +','
        SQL=SQL+STRTRIM(DAY,2)       +','
        SQL=SQL+STRTRIM(ARR_RESULT[2],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[3],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[4],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[5],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[6],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[7],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[8],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[9],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[10],2) +','
        SQL=SQL+STRTRIM(ARR_RESULT[11],2) +','
        SQL=SQL+"'"+STRTRIM(ARR_RESULT[12],2)+"'" +','    ;��������
        SQL=SQL+"'"+STRTRIM(SENSOR_CODE,2)+"'" +','     ;����������
        SQL=SQL+STRTRIM(PERIOD,2)+')'   ;����
        PRINT,SQL
        OD->EXECUTESQL,SQL
;     PRINT,ARR_RESULT
    ENDIF ELSE BEGIN
       progressTimer = Obj_New("ShowProgress", tlb,/CancelButton)
       progressTimer->start
       K=(SIZE(ARR_RESULT))[2]
       FOR I=0,(SIZE(ARR_RESULT))[2]-1 DO BEGIN
         ;************************************************************
         cancelled = progressTimer->CheckCancel()
         IF cancelled THEN BEGIN
          ok = Dialog_Message('�û���ֹ�˲���',TITLE='��ʾ')
          progressTimer->Destroy ;����������
          log, '���̼��-����', 0
         RETURN,0
         ENDIF
         progressTimer->Update, (i*1.0/K * 100.0) ;��������
         ;************************************************************
         ;A)�ȼ��ü�¼�ǲ����Ѿ�������,����Ѿ�����,ɾ��
       	SQL='DELETE FROM PARAMETER_PROCESS_'+SCALE_SQL
       	SQL=SQL+' WHERE  ('+SCALE_SQL+'_CODE = '+"'"+STRTRIM(ARR_RESULT[1,I],2)+"' "+')'
        SQL=SQL+'AND (YEAR = '+YEAR+')'
        SQL=SQL+'AND (MONTH = '+MONTH+') '
        SQL=SQL+'AND (DAY = '+DAY+') '
        SQL=SQL+'AND (SENSOR_CODE = '+"'"+STRTRIM(SENSOR_CODE,2)+"'"+')'
        SQL=SQL+'AND (DATA_TYPE = '+"'"+STRTRIM(DATA_TYPE,2)+"'"+')'
        SQL=SQL+'AND (PERIODS = '+STRTRIM(PERIOD,2)+')'
       	PRINT,'SQL 20060827',SQL

         OD->EXECUTESQL,SQL

         ;B)�������������
        SQL='INSERT INTO PARAMETER_PROCESS_'+SCALE_SQL
       	SQL=SQL+'('+SCALE_SQL+'_CODE ,YEAR ,MONTH ,DAY,'
       	SQL=SQL+'AVG_PLOWLAND ,AVG_PADDY_FIELD,AVG_DRY_LAND,'
       	SQL=SQL+'AVG_WINTER_WHEAT ,AVG_SPRING_WHEAT,'
       	SQL=SQL+'AVG_EARLY_RICE ,AVG_SEMILATE_RICE,AVG_LATE_RICE,'
       	SQL=SQL+'AVG_MAIZE,AVG_SOYBEAN,'
        SQL=SQL+'DATA_TYPE,SENSOR_CODE,PERIODS ) '
        SQL=SQL+'VALUES ('+"'"+STRTRIM(ARR_RESULT[1,I],2)+"'"+','
        SQL=SQL+STRTRIM(YEAR,2)     +','
        SQL=SQL+STRTRIM(MONTH,2)  +','
        SQL=SQL+STRTRIM(DAY,2)       +','
        SQL=SQL+STRTRIM(ARR_RESULT[2,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[3,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[4,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[5,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[6,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[7,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[8,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[9,I],2)  +','
        SQL=SQL+STRTRIM(ARR_RESULT[10,I],2) +','
        SQL=SQL+STRTRIM(ARR_RESULT[11,I],2) +','
        SQL=SQL+"'"+STRTRIM(ARR_RESULT[12,I],2)+"'" +','    ;��������
        SQL=SQL+"'"+STRTRIM(SENSOR_CODE,2)+"'" +','     ;����������
        SQL=SQL+STRTRIM(PERIOD,2)+')'   ;����
;           PRINT,SQL
        OD->EXECUTESQL,SQL
       ENDFOR
       progressTimer->destroy ;
    ENDELSE
    MSG=DIALOG_MESSAGE('����������!',TITLE='��ʾ',/INFORMATION)
    log, '���̼��-����', 1
    RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------




;-----------------------------------------------------------------
FUNCTION DST_SCALE_STA_PRO_ABOVE_COUNTY, EVENT

    WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE


END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO STA_PRO_ABOVE_COUNTY_EVENTCB
END
;
; IDL WIDGET INTERFACE PROCEDURES. THIS CODE IS AUTOMATICALLY
;     GENERATED AND SHOULD NOT BE MODIFIED.
;-----------------------------------------------------------------

;-----------------------------------------------------------------
;
; GENERATED ON: 11/17/2004 16:42.38
;
PRO BASE_STA_PRO_ABOVE_COUNTY_EVENT, EVENT

	on_error,2

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year_1,DSN,USER_NAME,PWD,PROVINCE_CODE

  WTARGET = (WIDGET_INFO(EVENT.ID,/NAME) EQ 'TREE' ?  $
      WIDGET_INFO(EVENT.ID, /TREE_ROOT) : EVENT.ID)


  WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
  WWIDGET =  EVENT.TOP

  CASE WTARGET OF

    ;��ѡ���Զ�������ʱ,Ҫ������������ƶ�ȡ����
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_SCALE_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       SCALE=(*PSTATE).ARR_SCALE[INDEX]
       (*PSTATE).SCALE=SCALE


       PRINT,SCALE
       CASE SCALE OF
         '��ѡ��߶�':BEGIN
          TEMP=BYTARR(12)
          TEMP=STRING(TEMP)
          WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,COLUMN_LABELS=TEMP
          TEMP=0
          WIDGET_CONTROL,(*PSTATE).DST_RANGE_STA_PRO_ABOVE_COUNTY,SET_VALUE=['']
          WIDGET_CONTROL,(*PSTATE).DST_RANGE_STA_PRO_ABOVE_COUNTY,SENSITIVE=0
         END

         'ȫʡ':BEGIN
         WIDGET_CONTROL,(*PSTATE).DST_RANGE_STA_PRO_ABOVE_COUNTY,SENSITIVE=1
          WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,COLUMN_LABELS=['ʡ', 'ʡ����',  $
                  '����','ˮ��','����','��С��', '��С��', '�絾', '�е�', '��', $
                  '����','��', '����','������', '����']
               WIDGET_CONTROL,(*PSTATE).DST_RANGE_STA_PRO_ABOVE_COUNTY,SET_VALUE=['ȫʡ']

               (*PSTATE).RANGE    ='ȫʡ'
               (*PSTATE).NUM_OF_RANGE=1
               (*PSTATE).ARR_RANGE_CODE=[' ']
               (*PSTATE).RANGE_CODE='1'
               (*PSTATE).ARR_RANGE       =['ȫʡ']


         END

         '�Զ�������':BEGIN
         WIDGET_CONTROL,(*PSTATE).DST_RANGE_STA_PRO_ABOVE_COUNTY,SENSITIVE=1
          WIDGET_CONTROL,(*PSTATE).TABLE_RESULT,COLUMN_LABELS=['����', '����',  $
                  '����','ˮ��','����','��С��', '��С��', '�絾', '�е�', '��', $
                  '����', '��', '����','������', '����']

          OD=DBobj
              ORS = OBJ_NEW('IDLDBRECORDSET', OD, TABLE='ROI_CODE')
             ;�����ʡ����ʼ�ĸ�����1,������0
             ;������Ϊ��һ��ѡ����'����ʡ'
             NUM_OF_RANGE=1
          IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

              ;���ҳ�ʡ�ĸ���
              WHILE (ORS->MOVECURSOR(/NEXT) EQ 1) DO BEGIN
                 NUM_OF_RANGE=NUM_OF_RANGE+1
              ENDWHILE

              ;Ȼ��ʡ�����ƶ�������
              ARR_RANGE=STRARR(NUM_OF_RANGE+1)
              ARR_RANGE_CODE=STRARR(NUM_OF_RANGE+1)
              TEMP=ORS->MOVECURSOR(/FIRST)
              COUNT=0
              ARR_RANGE[COUNT]='���м����'

              COUNT=COUNT+1
              ARR_RANGE[COUNT]=ORS->GETFIELD(1)
              ARR_RANGE_CODE[COUNT]=ORS->GETFIELD(0)
              WHILE (ORS->MOVECURSOR(/NEXT) EQ 1) DO BEGIN
                 COUNT=COUNT+1
                 ;�ڶ�ȡʡ����ʱ���ʡ�Ĵ���Ҳ��ȡ�˳���
                 ;��Ҫ�Ǽ��ٺ���Ĺ�����
                 ARR_RANGE[COUNT]=ORS->GETFIELD(1)
                 ARR_RANGE_CODE[COUNT]=ORS->GETFIELD(0)
              ENDWHILE

              WIDGET_CONTROL,(*PSTATE).DST_RANGE_STA_PRO_ABOVE_COUNTY,SET_VALUE=ARR_RANGE
              NUM_OF_RANGE=NUM_OF_RANGE + 1
              (*PSTATE).ARR_RANGE[0:NUM_OF_RANGE-1]=ARR_RANGE[0:NUM_OF_RANGE-1]

              (*PSTATE).ARR_RANGE_CODE[0:NUM_OF_RANGE-1] =ARR_RANGE_CODE[0:NUM_OF_RANGE-1]
              (*PSTATE).NUM_OF_RANGE =NUM_OF_RANGE
              (*PSTATE).RANGE    ='���м����'
          ENDIF
          OBJ_DESTROY,ORS
         END
         ELSE:;
       ENDCASE

       RETURN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_YEAR_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).YEAR=(*PSTATE).ARR_YEAR[INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_MONTH_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).MONTH=(*PSTATE).ARR_MONTH[INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_DAY_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).DAY=(*PSTATE).ARR_DAY[INDEX]
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_SENSOR_TYPE_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).SENSOR_CODE=(*PSTATE).ARR_SENSOR_CODE[INDEX]
       PRINT,(*PSTATE).SENSOR_CODE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_period_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).PERIOD=(*PSTATE).ARR_DAY[INDEX]
       PRINT,(*PSTATE).PERIOD
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_DATA_TYPE_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).DATA_TYPE=(*PSTATE).ARR_DATA_TYPE[INDEX]
       PRINT,(*PSTATE).DATA_TYPE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_RANGE_STA_PRO_ABOVE_COUNTY'): BEGIN
        INDEX=EVENT.INDEX
       (*PSTATE).RANGE=(*PSTATE).ARR_RANGE[INDEX]
       (*PSTATE).RANGE_CODE=(*PSTATE).ARR_RANGE_CODE[INDEX]
       PRINT,(*PSTATE).RANGE
       PRINT,(*PSTATE).RANGE_CODE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_ZS_COUNT_STA_PRO_ABOVE_COUNTY'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_WRITE_STA_PRO_ABOVE_COUNTY'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_CANCEL_STA_PRO_ABOVE_COUNTY'): BEGIN
        common_log,'�رճ��ƹ��̻���'
        CLOSE,/ALL
        WIDGET_CONTROL, EVENT.TOP, /DESTROY
        RETURN
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_HELP_STA_PRO_ABOVE_COUNTY'): BEGIN

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, "���̼��ģ��", BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse

;        ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,"'���̼��ģ��'"
    END
    ELSE:
  ENDCASE

END
;-----------------------------------------------------------------
pro ZS_PRO_SUM_cleanup, id
	Widget_Control, id, get_uvalue=pstate
	heap_free, pstate
end
;-----------------------------------------------------------------

PRO BASE_STA_PRO_ABOVE_COUNTY_P, GROUP_LEADER=WGROUP

	on_error,2

  ;����ͬһ�������ظ�����
  IF ( XREGISTERED('BASE_STA_PRO_ABOVE_COUNTY') NE 0 ) THEN RETURN

  RESOLVE_ROUTINE, 'STA_PRO_ABOVE_COUNTY_EVENTCB',/COMPILE_FULL_FILE  ; LOAD EVENT CALLBACK ROUTINES
  TEMP_1=43
  BASE_STA_PRO_ABOVE_COUNTY = WIDGET_BASE( GROUP_LEADER=WGROUP,  $
      UNAME='BASE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=320 ,YOFFSET=200  $
      ,SCR_XSIZE=462 ,SCR_YSIZE=346+TEMP_1 ,TITLE='���̼��--����'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1)


  BASE_TIME_STA_PRO_ABOVE_COUNTY = Widget_Base(BASE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='BASE_TIME_STA_PRO_ABOVE_COUNTY' ,FRAME=1 ,XOFFSET=6  $
      ,YOFFSET=6 ,SCR_XSIZE=443 ,SCR_YSIZE=31 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  LBL_TIME_STA_PRO_ABOVE_COUNTY =  $
      Widget_Label(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
      UNAME='LBL_TIME_STA_PRO_ABOVE_COUNTY' ,XOFFSET=6 ,YOFFSET=8  $
      ,SCR_XSIZE=100 ,SCR_YSIZE=19 ,/ALIGN_LEFT ,VALUE='��ѡ����ܵ�ʱ��:')

	common current_date, c_year, c_month, c_day
	subbase = widget_base(BASE_TIME_STA_PRO_ABOVE_COUNTY,/row,xpad=3,ypad=3,space=5,XOFFSET=68+60)
	WID_LABEL_date = Widget_text(subbase,xsize=15, $
		VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))
	CMD_pick_date = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date'})


;  LBL_YEAR_STA_PRO_ABOVE_COUNTY =  $
;      Widget_Label(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;      UNAME='LBL_YEAR_STA_PRO_ABOVE_COUNTY' ,XOFFSET=68+45 ,YOFFSET=8  $
;      ,SCR_XSIZE=18 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')
;
;
;  LBL_MONTH_STA_PRO_ABOVE_COUNTY =  $
;      Widget_Label(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;      UNAME='LBL_MONTH_STA_PRO_ABOVE_COUNTY' ,XOFFSET=150-37+75 ,YOFFSET=8  $
;      ,SCR_XSIZE=18 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')
;
;
;  LBL_DAY_STA_PRO_ABOVE_COUNTY = Widget_Label(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;      UNAME='LBL_DAY_STA_PRO_ABOVE_COUNTY' ,XOFFSET=225-46+75 ,YOFFSET=8  $
;      ,SCR_XSIZE=18 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')
;
;
;  DST_YEAR_STA_PRO_ABOVE_COUNTY =  $
;      Widget_Combobox(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;; 	Widget_Droplist(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;      UNAME='DST_YEAR_STA_PRO_ABOVE_COUNTY' ,XOFFSET=90-32+75 ,YOFFSET=4  $
;      ,SCR_XSIZE=49 ,SCR_YSIZE=22)
;
;
;  DST_MONTH_STA_PRO_ABOVE_COUNTY =  $
;      Widget_Droplist(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;      UNAME='DST_MONTH_STA_PRO_ABOVE_COUNTY' ,XOFFSET=170-38+75 ,YOFFSET=4  $
;      ,SCR_XSIZE=40 ,SCR_YSIZE=21)
;
;
;  DST_DAY_STA_PRO_ABOVE_COUNTY =  $
;      Widget_Droplist(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
;      UNAME='DST_DAY_STA_PRO_ABOVE_COUNTY' ,XOFFSET=245-47+75 ,YOFFSET=4  $
;      ,SCR_XSIZE=44 ,SCR_YSIZE=21)


  LBL_period_STA_PRO_ABOVE_COUNTY =  $
      Widget_Label(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
      UNAME='LBL_period_STA_PRO_ABOVE_COUNTY' ,XOFFSET=352 ,YOFFSET=8  $
      ,SCR_XSIZE=29 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='����:')

  DST_period_STA_PRO_ABOVE_COUNTY =  $
      Widget_Droplist(BASE_TIME_STA_PRO_ABOVE_COUNTY,  $
      UNAME='DST_period_STA_PRO_ABOVE_COUNTY' ,XOFFSET=382 ,YOFFSET=4  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=21 )

;***************************************************************************
;�޸ĺ������
BASE_DATA_PARAMETER = Widget_Base(BASE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='BASE_DATA_PARAMETER' ,FRAME=1 ,XOFFSET=6  $
      ,YOFFSET=3+TEMP_1 ,SCR_XSIZE=443 ,SCR_YSIZE=31 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  LBL_SENSOR_TYPE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Label(BASE_DATA_PARAMETER,  $
      UNAME='LBL_SENSOR_TYPE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=6 ,YOFFSET=8  $
      ,SCR_XSIZE=100 ,SCR_YSIZE=19 ,/ALIGN_LEFT ,VALUE='��ѡ�񴫸�������:')


  DST_SENSOR_TYPE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Droplist(BASE_DATA_PARAMETER,  $
      UNAME='DST_SENSOR_TYPE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=90-32+75 ,YOFFSET=4  $
      ,SCR_XSIZE=100 ,SCR_YSIZE=22)



  LBL_DATA_TYPE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Label(BASE_DATA_PARAMETER,  $
      UNAME='LBL_DATA_TYPE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=352 ,YOFFSET=8  $
      ,SCR_XSIZE=29 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='����:')

  DST_DATA_TYPE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Droplist(BASE_DATA_PARAMETER,  $
      UNAME='DST_DATA_TYPE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=382 ,YOFFSET=4  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=21 )

;***************************************************************************
  BASE_RANGE_STA_PRO_ABOVE_COUNTY = Widget_Base(BASE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='BASE_RANGE_STA_PRO_ABOVE_COUNTY' ,FRAME=1 ,XOFFSET=6  $
      ,YOFFSET=6+37+TEMP_1 ,SCR_XSIZE=443 ,SCR_YSIZE=31 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  LBL_TIME_STA_PRO_ABOVE_COUNTY =  $
      Widget_Label(BASE_RANGE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='LBL_TIME_STA_PRO_ABOVE_COUNTY' ,XOFFSET=6 ,YOFFSET=8  $
      ,SCR_XSIZE=100 ,SCR_YSIZE=19 ,/ALIGN_LEFT ,VALUE='��ѡ����ܵĳ߶�:')



  DST_SCALE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Droplist(BASE_RANGE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='DST_SCALE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=90-32+75 ,YOFFSET=4  $
      ,SCR_XSIZE=100 ,SCR_YSIZE=21)




  LBL_RANGE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Label(BASE_RANGE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='LBL_RANGE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=299-40 ,YOFFSET=8  $
      ,SCR_XSIZE=70 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='���ܵķ�Χ:')


  DST_RANGE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Droplist(BASE_RANGE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='DST_RANGE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=330 ,YOFFSET=4  $
      ,SCR_XSIZE=100 ,SCR_YSIZE=22 ,SENSITIVE=0)


;*********************************************************************************


  TABLE_RESULT_STA_PRO_ABOVE_COUNTY = Widget_Table(BASE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='TABLE_RESULT_STA_PRO_ABOVE_COUNTY' ,XOFFSET=7 ,YOFFSET=65+40+TEMP_1  $
      ,SCR_XSIZE=442 ,SCR_YSIZE=148 ,XSIZE=15 ,YSIZE=135,FRAME=1)


  LBL_RESULT_STA_PRO_ABOVE_COUNTY = Widget_Label(BASE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='LBL_RESULT_STA_PRO_ABOVE_COUNTY' ,XOFFSET=9 ,YOFFSET=47+40+TEMP_1  $
      ,SCR_XSIZE=111 ,SCR_YSIZE=19 ,/ALIGN_LEFT  $
      ,VALUE='���ܽ����ʾ����:')


  BASE_CMD_STA_PRO_ABOVE_COUNTY = Widget_Base(BASE_STA_PRO_ABOVE_COUNTY,  $
      UNAME='BASE_CMD_STA_PRO_ABOVE_COUNTY' ,FRAME=1 ,XOFFSET=7  $
      ,YOFFSET=233+40+TEMP_1 ,SCR_XSIZE=443 ,SCR_YSIZE=36 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  CMD_ZS_COUNT_STA_PRO_ABOVE_COUNTY =  $
      Widget_Button(BASE_CMD_STA_PRO_ABOVE_COUNTY,  $
      UNAME='CMD_ZS_COUNT_STA_PRO_ABOVE_COUNTY' ,XOFFSET=8 ,YOFFSET=6  $
      ,SCR_XSIZE=92 ,SCR_YSIZE=22  $
      ,EVENT_FUNC='CMD_ZS_COUNT_STA_PRO_ABOVE_COUNTY' ,/ALIGN_CENTER  $
      ,VALUE='����')


  CMD_WRITE_STA_PRO_ABOVE_COUNTY =  $
      Widget_Button(BASE_CMD_STA_PRO_ABOVE_COUNTY,  $
      UNAME='CMD_WRITE_STA_PRO_ABOVE_COUNTY' ,XOFFSET=119 ,YOFFSET=6  $
      ,SCR_XSIZE=92 ,SCR_YSIZE=22  $
      ,EVENT_FUNC='CMD_WRITE_STA_PRO_ABOVE_COUNTY' ,/ALIGN_CENTER  $
      ,VALUE='�������')


  CMD_CANCEL_STA_PRO_ABOVE_COUNTY =  $
      Widget_Button(BASE_CMD_STA_PRO_ABOVE_COUNTY,  $
      UNAME='CMD_CANCEL_STA_PRO_ABOVE_COUNTY' ,XOFFSET=339 ,YOFFSET=6  $
      ,SCR_XSIZE=92 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER  $
      ,VALUE='�ر�')


  CMD_HELP_STA_PRO_ABOVE_COUNTY =  $
      Widget_Button(BASE_CMD_STA_PRO_ABOVE_COUNTY,  $
      UNAME='CMD_HELP_STA_PRO_ABOVE_COUNTY' ,XOFFSET=229 ,YOFFSET=6  $
      ,SCR_XSIZE=92 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER  $
      ,VALUE='����')



  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

year_droplist=strtrim(string(indgen(36)+1980),2)
  STATE={$
		widget_top : BASE_STA_PRO_ABOVE_COUNTY,$
        BASE_STA_PRO_ABOVE_COUNTY   : BASE_STA_PRO_ABOVE_COUNTY   ,$
        DST_RANGE_STA_PRO_ABOVE_COUNTY  :DST_RANGE_STA_PRO_ABOVE_COUNTY  ,$
;       ARR_YEAR   :   [ '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998','1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006'] ,$
       ARR_YEAR		:	year_droplist , $
       ARR_MONTH     :  [ '1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12' ],$
       ARR_DAY      :   ['1', '2', '3','4', '5', '6', '7', '8', '9', '10', $
                   '11', '12', '13','14', '15', '16', '17', '18', '19', '20',$
                   '21', '22', '23','24', '25', '26', '27', '28', '29', '30', '31'] ,$

       ARR_DATA_TYPE :   ['NDVI','LAI','NPP'],$
       ARR_SCALE     :  ['��ѡ��߶�','ȫʡ','�Զ�������']     ,$

       ARR_RANGE     :  STRARR(150)  ,$
       ARR_RANGE_CODE    :  STRARR(150)  ,$

       YEAR       :  1990 ,$
       MONTH        : 1   ,$
       DAY          :  1    ,$
       SCALE        : '��ѡ��߶�',$
       RANGE        : ''  ,$
       RANGE_CODE       : ''  ,$
       DATA_TYPE     :  'NDVI'   ,$
       TABLE_RESULT  :    TABLE_RESULT_STA_PRO_ABOVE_COUNTY  ,$

       COUNT_DONE       : 0   ,$ ;��һ����������־�Ƿ�����˻�������
       ALL_RANGE     :  1    ,$

       ARR_SENSOR       : [ 'AVHRR', 'MODIS', 'VGT'] ,$
       ARR_SENSOR_CODE   : [ 1, 2, 3] ,$
       SENSOR_CODE     :    1       ,$
       period         :    1       ,$


       NUM_OF_RANGE  :    0  $
       ;����ʵ��ʡ�ĸ���,�����б��е�������,һ�������Ϊ32

;       yesORno      :   yesORno   , $
;        DBobj         :    DBobj  $


         }

  PSTATE=PTR_NEW(STATE,/NO_COPY)
  WIDGET_CONTROL,BASE_STA_PRO_ABOVE_COUNTY,SET_UVALUE=PSTATE

	WIDGET_CONTROL, CMD_pick_date, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date, set_uvalue=staff

  WIDGET_CONTROL, /REALIZE, BASE_STA_PRO_ABOVE_COUNTY
  WIDGET_CONTROL,CMD_CANCEL_STA_PRO_ABOVE_COUNTY,/INPUT_FOCUS

  ;************************************************************************
  ;�������б�ֵ,�е�������Ҫ�����ݿ��ж�ȡ
;  WIDGET_CONTROL,DST_YEAR_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_YEAR
;  WIDGET_CONTROL,DST_MONTH_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_MONTH
;  WIDGET_CONTROL,DST_DAY_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_DAY
  WIDGET_CONTROL,DST_DATA_TYPE_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_DATA_TYPE

  WIDGET_CONTROL,DST_PERIOD_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_DAY
  WIDGET_CONTROL,DST_SENSOR_TYPE_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_SENSOR

  WIDGET_CONTROL,DST_SCALE_STA_PRO_ABOVE_COUNTY,SET_VALUE=(*PSTATE).ARR_SCALE
  ;************************************************************************
  WIDGET_CONTROL,TABLE_RESULT_STA_PRO_ABOVE_COUNTY,COLUMN_WIDTHS=51

  ;***********************************************************************
  ;Ϊ��ϵͳ��ʾ�����ӵĴ���
  TEMP=1
  IF(TEMP EQ 1)THEN BEGIN
	  	WIDGET_CONTROL,DST_PERIOD_STA_PRO_ABOVE_COUNTY,SET_DROPLIST_SELECT=9

		(*PSTATE).YEAR	=	strtrim(c_year,2)
	  	(*PSTATE).MONTH	=	strtrim(c_month,2)
	  	(*PSTATE).DAY	=	strtrim(c_day,2)
	  	(*PSTATE).PERIOD=	10
  ENDIF
  ;***********************************************************************

  XMANAGER, 'BASE_STA_PRO_ABOVE_COUNTY', BASE_STA_PRO_ABOVE_COUNTY, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'

END

;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO ZS_STA_PRO_ABOVE_COUNTY,GROUP_LEADER=WGROUP
  common_log,'�������ƹ��̻���'
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_STA_PRO_ABOVE_COUNTY_P, GROUP_LEADER=BASE_TOP
END
