;**********************************************************************************

;***********************************************************************************
;
; 蒙继华,2008.08.19
; 用于在省级系统中对县的作物生长过程线进行重构
; 在原来全球长势监测系统的HANTS
;
;-----------------------------------------------------------------
;-----------------------------------------------------------------
FUNCTION HANTS_DO, PSTATE
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

     IF (yesORno EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('请先设置数据库链接!',TITLE='提示',/information)
     	CLOSE,/ALL
     	RETURN,0
     ENDIF

     OD=DBobj

    ;********************************************************************************************
     ;进行输入参数完整性检验,
     ;如果输入的参数不完整,则不进行计算,
     ;并以对话框的形式提示用户进行输入

	;WIDGET_CONTROL,(*pstate).TXT_DOD_HANTS,get_value=DOD
	DOD = (*pstate).TXT_DOD_HANTS
     IF(DOD EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('请输入DOD(残留点个数)!',TITLE='提示',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     DOD=FIX(DOD[0])

	;WIDGET_CONTROL,(*pstate).TXT_TOLERANCE_HANTS,get_value=TOLERANCE
	TOLERANCE = (*pstate).TXT_TOLERANCE_HANTS
     IF(TOLERANCE EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('请输入曲线重构容限!',TITLE='提示',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     TOLERANCE=FIX(TOLERANCE[0])

	;WIDGET_CONTROL,(*pstate).TXT_FREQUENCY_HANTS,get_value=FREQUENCY
	FREQUENCY = (*pstate).TXT_FREQUENCY_HANTS
     IF(FREQUENCY EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('请输入频率!',TITLE='提示',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     FREQUENCY=FIX(FREQUENCY[0])

	;WIDGET_CONTROL,(*pstate).TXT_MENXIAN_HANTS,get_value=MENXIAN
	MENXIAN = (*pstate).TXT_MENXIAN_HANTS
     IF(MENXIAN EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('请输入门限!',TITLE='提示',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     MENXIAN=FIX(MENXIAN[0])

	;获得开始和结束的时间参数
	YEAR_START=(*pstate).YEAR_START
	YEAR_END=(*pstate).YEAR_END
	MONTH_START=(*pstate).MONTH_START
	MONTH_END=(*pstate).MONTH_END
	DAY_START=(*pstate).DAY_START
	DAY_END=(*pstate).DAY_END

	;构成查询的时间范围数据
	JULDAY_START=JULDAY(MONTH_START, DAY_START, YEAR_START, 0, 0, 0)
	JULDAY_END	=JULDAY(MONTH_END, DAY_END, YEAR_END, 0, 0, 0)

	;进行批处理的时候对时间有一定的要求,在这里要对时间是否合理进行判断
	IF(JULDAY_START GT JULDAY_END) THEN BEGIN
		TEMP=DIALOG_MESSAGE('起始时间必须要早于结束时间',TITLE='条件错误')
		CLOSE,/ALL
 		RETURN, 0
	ENDIF

	;********************************************************************************************
	;********************************************************************************************
	;数据库正常的情况下,在这里进行输入数据的合理性检验
	;如果不合要求,则给出提示
	;对HANTS程序运行的参数进行检查
	IF(DOD LE 0) THEN BEGIN
		msg=DIALOG_MESSAGE('DOD不能小于零!',TITLE='提示',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	IF(MENXIAN GT 0) THEN BEGIN
		msg=DIALOG_MESSAGE('门限值必需小于零!',TITLE='提示',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	RANGE			=STRTRIM((*PSTATE).RANGE,2)
	RANGE_CODE		=STRTRIM((*PSTATE).RANGE_CODE,2)
	;对所选的区域和子区域进行判断
	IF(RANGE EQ '请选择') THEN BEGIN
		msg=DIALOG_MESSAGE('请选择区域!',TITLE='提示',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END


	PRINT,YEAR_START,MONTH_START,DAY_START
	PRINT,YEAR_END,MONTH_END,DAY_END


	period			=STRTRIM((*PSTATE).period,2)
	SENSOR_TYPE		=STRTRIM((*PSTATE).SENSOR_TYPE,2)
	SENSOR_CODE		=STRTRIM((*PSTATE).SENSOR_CODE,2)

	WIDGET_CONTROL,(*pstate).DRAW_HANTS,get_value=DRAW_HANTS

	;定义一个数组,用来存储从数据库中读取出来的数据
	NUM_MAX=80
	ARR_YEAR	=INTARR(NUM_MAX)
	ARR_MONTH	=INTARR(NUM_MAX)
	ARR_DAY		=INTARR(NUM_MAX)

	ARR_PLOWLAND	=FLTARR(NUM_MAX)
	ARR_PADDY_FIELD	=FLTARR(NUM_MAX)
	ARR_DRY_LAND	=FLTARR(NUM_MAX)

	;********************************************************************
	;数据长度的标识需要进行修改
	;这个变量用来标识数据的长度(是根据输入的参数生成的长度)

	;目前使用了一个比较简单的方法,就是读取一个或两个县的数据
	;(在处理单个县的时候,读取一个县的数据,否则读取两个县的数据)
	;如果是两个县,则需要这两个县的数据的长度一致
	IF RANGE NE '所有县(批处理)' THEN BEGIN	;处理单个县的数据

		;定义两个时间用来进行查询
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
		;获取数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',OD,SQL='select count(*) from ('+Sql+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		NI_INPUT=RecordNum

		if NI_INPUT gt 36 then begin
			info=dialog_message('所选时间范围已超出一年，请重新选择',title='提示')
			return,0
		endif

		Obj_Destroy,RecordNumOBJ
		;Obj_Destroy,ORS
		IF NI_INPUT EQ 0 THEN BEGIN
			TEMP='没有找到相应的数据'
			msg=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF

		;用一个比较特殊的算法来考察相应时间内的记录条数是否合理
		TEMP=0;ABS(LONG((JULDAY_END-JULDAY_START)/period)-NI_INPUT)
		;aa
		IF TEMP GT 1 THEN BEGIN
			TEMP='相应时间内的数据记录数不对,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'记录数应该满足如下条件:'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'ABS((结束天-起始天)/频率-数据个数)<1'
			msg=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF ELSE BEGIN
			NI_INPUT=NI_INPUT
			(*PSTATE).NI=NI_INPUT
		ENDELSE
		PRINT,NI_INPUT

		;把年月天的数据读取出来
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
		PRINT,'批处理数据'
		;从表'COUNTY_CODE_RASTER'表中读取一条记录,从中获取一个有效的县代码
		;在符合"ABS((结束天-起始天)/频率-数据个数)<1"
		;条件的情况下,把这个长度作为NI_INPUT
		;并且每个县在相应时间段内的数据个数都应该与其一致.
		SQL='SELECT CODE FROM COUNTY_CODE_RASTER'
		;从这个表中选取数据是为了保证这个代码有相应的空间数据与之相对应
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
		;获取数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',OD,SQL='select count(*) from ('+Sql+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		NI_INPUT=RecordNum

;		if NI_INPUT gt 36 then begin
;			info=dialog_message('所选时间范围已超出一年，请重新选择',title='提示')
;			return,0
;		endif

		Obj_Destroy,RecordNumOBJ
		;Obj_Destroy,ORS
		IF NI_INPUT EQ 0 THEN BEGIN
			TEMP='没有找到相应的数据'
			msg=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF

		;用一个比较特殊的算法来考察相应时间内的记录条数是否合理
		TEMP=ABS(LONG((JULDAY_END-JULDAY_START)/FIX(period))-NI_INPUT)
		TEMP=0

		IF TEMP GT 1 THEN BEGIN
			TEMP='相应时间内的数据记录数不对,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'记录数应该满足如下条件:'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))+'ABS((结束天-起始天)/频率-数据个数)<1'
			msg=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
			CLOSE,/all
    		RETURN,0
		ENDIF ELSE BEGIN
			NI_INPUT=NI_INPUT
			(*PSTATE).NI=NI_INPUT
		ENDELSE

		;把年月天的数据读取出来
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
	;获取曲线重构的参数
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
		TEMP='数据点不足,请尝试:'
		TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
		TEMP=TEMP+'(1)增加点的个数'
		TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
		TEMP=TEMP+'(2)减少频率'
		TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
		TEMP=TEMP+'(3)减少DOD点的个数'
		MSG=DIALOG_MESSAGE(TEMP,TITLE='数据点不足',/INFORMATION)
		CLOSE,/ALL
		RETURN,0
	ENDIF
	;********************************************************************************************



	IF(NI GT NUM_MAX) THEN BEGIN
		TEMP='数据系列长度超过了系统的容量!'
		msg=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
		CLOSE,/all
		RETURN,0
	ENDIF
	;这里取80是因为数据的最小假设周期为5,只处理一年的话不会超过80

	;定义一个变量用来存储有效记录的个数
	COUNT=0
	;合成时间数据,供SQL语句使用


    ;**********************************************************************************;
    ;这里是系统进行数据处理的核心
	;分两种情况来处理数据,一种是一次处理一个县,另一种是一次处理所有的县
	IF(RANGE EQ '所有县(批处理)') THEN BEGIN	;一次处理全国所有县的数据
		PRINT,'WHOLE COUNTRY'


		;**********************************************************************************;
		;在批处理中对数据长度进行验证,
		;验证的方法是使用使用前面的NI_INPUT数据
		;从数据库中读取每个县的数据长度,如果某个县的数据长度与NI_INPUT不一致,
		;则报错并退出程序
		NUM_START=YEAR_START*500.0+MONTH_START*40.0+DAY_START
		NUM_END=YEAR_END*500.0+MONTH_END*40.0+DAY_END
		SQL='SELECT CODE FROM COUNTY_CODE_RASTER'	;从这个表中选取数据是为了保证这个代码有相应的空间数据与之相对应
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
					TEMP='以下县的数据序列长度异常'
					TEMP=TEMP+STRING(BYTE(10))+STRING(BYTE(13))
					TEMP=TEMP+'县代码:'+STRTRIM(COUNTY_CODE)
					msg=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
					CLOSE,/all
		    		RETURN,0
				ENDIF
				PRINT,COUNTY_CODE,NI_INPUT_TEMP
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)

		ENDIF
		Obj_Destroy,ORS

		;**********************************************************************************;
		;定义所要使用的数据
		;ARR_SHOW=STRARR(2*NI_INPUT+3,3*2500)
		ARR_SHOW=STRARR(2*NI_INPUT+3,3*200)
		;假设有200个县,这是最多的县的个数了,
		;通常一个省只有100-120个县

		;首先把县代码的数据集读取出来
		;**********************************************************************************;
		;下面是原来的SQL语句,从数据库中动态进行了频率的读取,
		;省级系统中使用固定的频率,因而对其进行了修改
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
					ok = Dialog_Message('用户终止了操作',TITLE='提示')
					progressTimer->Destroy ;结束进度条
					RETURN,0
				ENDIF
				progressTimer->Update, (FLOAT(COUNT_COUNTY)/TOTAL_COUNTY * 100.0) ;继续进行
				;****************************************************************

				;读出县名和县代码
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

				;读取一个县的数据
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

				;把读取完数据的RECORDSET对象释放掉
				OBJ_DESTROY, ORS_1

				IF(1) THEN BEGIN ;只有在实际的数据长度和选择的数据长度相同时才进行计算
					;为了适应算法,定义了一个大一的数组
					ARR_PLOWLAND_TEMP=FLTARR(COUNT+1)
					ARR_PLOWLAND_TEMP[1:COUNT]=ARR_PLOWLAND[0:COUNT-1]
					ARR_PADDY_FIELD_TEMP=FLTARR(COUNT+1)
					ARR_PADDY_FIELD_TEMP[1:COUNT]=ARR_PADDY_FIELD[0:COUNT-1]
					ARR_DRY_LAND_TEMP=FLTARR(COUNT+1)
					ARR_DRY_LAND_TEMP[1:COUNT]=ARR_DRY_LAND[0:COUNT-1]

					;又定义了三个数组用来放处理过的结果
					ARR_PLOWLAND_HANTS=FLTARR(COUNT+1)
					ARR_PADDY_FIELD_HANTS=FLTARR(COUNT+1)
					ARR_DRY_LAND_HANTS=FLTARR(COUNT+1)



					;********************************************************************************************
					;在这里获得所有的HANTS程序所要使用的参数,
					NI=COUNT
					FET=TOLERANCE
					NF=FREQUENCY

					;在处理一年的完整数据的时候
					;使用数据库中根据复种指数制定的HANTS分县参数
					IF(NI EQ 36 ) THEN BEGIN
						IF((NF_DB GE 1) AND (NF_DB LE 3)) THEN BEGIN;在有效范围内才使用
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
					;定义一个数据用来放一条完整的记录(包括三行,分别是耕地\水田\旱地),包括了处理过了的数据和原始数据

					ARR_SHOW[0,0+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,0+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,0+COUNT_COUNTY*3]='耕地'
					ARR_SHOW[3:NI+2,0+COUNT_COUNTY*3]=ARR_PLOWLAND_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,0+COUNT_COUNTY*3]=ARR_PLOWLAND[0:NI-1]

					ARR_SHOW[0,1+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,1+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,1+COUNT_COUNTY*3]='水田'
					ARR_SHOW[3:NI+2,1+COUNT_COUNTY*3]=ARR_PADDY_FIELD_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,1+COUNT_COUNTY*3]=ARR_PADDY_FIELD[0:NI-1]

					ARR_SHOW[0,2+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,2+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,2+COUNT_COUNTY*3]='旱地'
					ARR_SHOW[3:NI+2,2+COUNT_COUNTY*3]=ARR_DRY_LAND_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,2+COUNT_COUNTY*3]=ARR_DRY_LAND[0:NI-1]
					COUNT_COUNTY=COUNT_COUNTY+1

				ENDIF

				;************************************************************************************
				PRINT,COUNT

			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
			OBJ_DESTROY, ORS
			progressTimer->destroy ;

			ARR_SHOW_1=STRARR(2*NI_INPUT+3,3*COUNT_COUNTY);假设有2500个县;按真实的数据大小定义的数组
			ARR_SHOW_1[*,0:3*COUNT_COUNTY-1]=ARR_SHOW[*,0:3*COUNT_COUNTY-1]

			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_XSIZE=2*NI_INPUT+3
			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_YSIZE=3*COUNT_COUNTY
			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_VALUE=ARR_SHOW_1

			;释放一些比较大的数组
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
		MSG='完成计算'+STRING(BYTE(10))+STRING(BYTE(13))
		MSG=MSG+'共计算了'+STRTRIM(STRING(COUNT_COUNTY),2)+'个县'
		TEMP=DIALOG_MESSAGE(MSG,TITLE='计算完成',/INFORMATION)

	ENDIF ELSE BEGIN	;处理一个县的数据
		print,'处理一个县的数据'
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

		PRINT,'SQL语句:'
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
			MSG=DIALOG_MESSAGE('没有找到相应的数据!',TITLE='提示',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF

		IF(COUNT LT NI_INPUT) THEN BEGIN
			TEMP='所选择的时间范围内的数据不全,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
			TEMP=TEMP+'请重新选定时间范围再进行运算'
			MSG=DIALOG_MESSAGE(TEMP,TITLE='提示',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF
		;为了适应算法,定义了一个大一的数组
		ARR_PLOWLAND_TEMP=FLTARR(COUNT+1)
		ARR_PLOWLAND_TEMP[1:COUNT]=ARR_PLOWLAND[0:COUNT-1]

		ARR_PADDY_FIELD_TEMP=FLTARR(COUNT+1)
		ARR_PADDY_FIELD_TEMP[1:COUNT]=ARR_PADDY_FIELD[0:COUNT-1]

		ARR_DRY_LAND_TEMP=FLTARR(COUNT+1)
		ARR_DRY_LAND_TEMP[1:COUNT]=ARR_DRY_LAND[0:COUNT-1]


		;又定义了三个数组用来放处理过的结果
		ARR_PLOWLAND_HANTS=FLTARR(COUNT+1)
		ARR_PADDY_FIELD_HANTS=FLTARR(COUNT+1)
		ARR_DRY_LAND_HANTS=FLTARR(COUNT+1)


		;对数据进行HANTS处理
		;********************************************************************************************
		;在这里获得所有的HANTS程序所要使用的参数,
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


		;在这里定义必要的数组,
		;并调用程序进行曲线的重构,返回重构过的曲线.
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
		;定义一个数据用来放一条完整的记录(包括三行,分别是耕地\水田\旱地),包括了处理过了的数据和原始数据
		PRINT,ARR_PLOWLAND_HANTS
		PRINT,ARR_PADDY_FIELD_HANTS
		PRINT,ARR_DRY_LAND_HANTS

		ARR_SHOW=STRARR(2*NI_INPUT+3,3)

		ARR_SHOW[0,0]=RANGE
		ARR_SHOW[1,0]=RANGE_CODE
		ARR_SHOW[2,0]='耕地'
		ARR_SHOW[3:NI+2,0]=ARR_PLOWLAND_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,0]=ARR_PLOWLAND[0:NI-1]

		ARR_SHOW[0,1]=RANGE
		ARR_SHOW[1,1]=RANGE_CODE
		ARR_SHOW[2,1]='水田'
		ARR_SHOW[3:NI+2,1]=ARR_PADDY_FIELD_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,1]=ARR_PADDY_FIELD[0:NI-1]

		ARR_SHOW[0,2]=RANGE
		ARR_SHOW[1,2]=RANGE_CODE
		ARR_SHOW[2,2]='旱地'
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


	;如果没有找到足够的记录
	;则不进行计算,在这里进行了判断
    IF(COUNT EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE('没有找到相应的数据!',TITLE='提示',/INFORMATION)
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

	PRINT,'开始数据入库操作'
	;首先检测数据库的链接是否成功,如不成功则不进行运算
	;WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
    IF (yesORno EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('请先设置数据库链接!',TITLE='提示',/information)
     	CLOSE,/ALL
     	RETURN,0
    ENDIF

    ;2)对HANTS程序运行的参数进行检查
	IF((*PSTATE).HANTS_DONE EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE('请先进行谐函数曲线重构!',TITLE='提示',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	;获取一些重构时的相关参数,和与数据库的链接

    OD=DBobj


     WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,GET_VALUE=ARR_TABLE


	;定义一个结构体,将要入库的数据写成一个结构体数组
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

	;将数据写入结构体

	;获取数据集的大小
	LINES=(SIZE(ARR_TABLE))[2];这个LINES放的是显示的表中的行数
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
			OK = DIALOG_MESSAGE('用户终止了操作',TITLE='提示')
			PROGRESSTIMER->DESTROY ;结束进度条
			RETURN,0
		ENDIF
		PROGRESSTIMER->UPDATE, (FLOAT(J)/LINES * 100.0) ;继续进行
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

			;删除数据库中与要插入的记录时空属性相同的记录
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

			;删除后再插入新的记录
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
	MSG=DIALOG_MESSAGE('数据装载完成',TITLE='完成',/INFORMATION)
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
	;将选择的结果成图

	WIDGET_CONTROL,(*PSTATE).DRAW_HANTS,GET_VALUE=DRAW_HANTS
	WSET, DRAW_HANTS
	white=!D.N_COLORS-1

	;***********************************************
	device, set_font='宋体', /TT_FONT

	;画出原始的曲线
	X=FINDGEN(NI)*10
	PLOT,X,ARR_H,BACKGROUND=WHITE,color=0,XMARGIN=[7,2],YMARGIN=[3,1],PSYM=1,SYMSIZE=0.6$
		,XTITLE='单位:天',YRANGE=[0.1,0.9],FONT=2,charsize=1
	XYOUTS,6,150,'植',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,120,'被',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,90	,'指',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,60	,'数',FONT=2,charsize=1,COLOR=0,/DEVICE

	OPLOT,X,ARR_O,COLOR=0
	;画出重构后的曲线
	OPLOT,X,ARR_H,PSYM=2,SYMSIZE=0.6,COLOR=255
	OPLOT,X,ARR_H,COLOR=255

	;对系统的字体进行设置,以便进行汉字的显示
	;DEVICE, SET_FONT = 'kai'
	;画原始曲线的图例
	PLOTS,30,270,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,40,270,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,50,270,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,60,270,COLOR=0, /DEVICE ,/CONTINUE,PSYM=1,SYMSIZE=0.6
	PLOTS,30,270,COLOR=0,/DEVICE
	PLOTS,60,270,COLOR=0, /DEVICE ,/CONTINUE
	XYOUTS,65,266,'Original Curve',/DEVICE,COLOR=0


	;画重构后的曲线的图例
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
    	common_log,'关闭过程线重构'
    	CLOSE,/ALL
     	WIDGET_CONTROL, EVENT.TOP, /DESTROY
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='TABLE_RESULT_HANTS'): BEGIN
      	IF( TAG_NAMES(EVENT, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' )THEN $
        	TABLE_RESULT_HANTS0, EVENT    ;此行进行了修改，原代码为 TABLE_RESULT_HANTS, EVENT。杨绍锷，20070320
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_OK_HANTS'): BEGIN
      	PRINT,'A'
      	TEMP=HANTS_DO(PSTATE)
      	IF(TEMP EQ 1) THEN BEGIN
      		TEMP=DIALOG_MESSAGE('曲线重构成功',TITLE='成功')
      		TABLE_RESULT_HANTS0, {ID:(*PSTATE).TABLE_RESULT_HANTS, TOP:event.top, TYPE:4, SEL_LEFT:0, SEL_TOP:0}
      		log, '过程监测-曲线重构', 0
      	ENDIF ELSE BEGIN
      		TEMP=DIALOG_MESSAGE('曲线重构失败',TITLE='失败')
      		log, '过程监测-曲线重构', -1
      	ENDELSE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_DB_WRITE_HANTS'): BEGIN
      	TEMP=DB_WRITE_HANTS(PSTATE)
      	IF(TEMP EQ 1) THEN BEGIN
      		TEMP=DIALOG_MESSAGE('数据入库成功',TITLE='成功')
      		log, '过程监测-曲线重构', 1
      	ENDIF ELSE BEGIN
      		TEMP=DIALOG_MESSAGE('数据入库失败',TITLE='失败')
      		log, '过程监测-曲线重构', -1
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
      ,TITLE='过程监测--曲线重构' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
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
      ,SCR_XSIZE=62 ,SCR_YSIZE=16 ,/ALIGN_LEFT ,VALUE='起始时间:  ')

  	WID_LABEL_date_s = Widget_text(subbase,xsize=12, $
		VALUE=strtrim(c_year,2)+'-'+strtrim('1',2)+'-'+strtrim('1',2))
 	CMD_pick_date_s = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date_s, pointer:PTR_NEW(), detail:'CMD_pick_date_s'})

    subbase = widget_base(BASE_DATE_HANTS,/row,xpad=3,space=3)

   LBL_DATE_END_HANTS = WIDGET_LABEL(subbase,  $
      UNAME='LBL_DATE_END_HANTS' ,XOFFSET=11 ,YOFFSET=60  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=16 ,/ALIGN_LEFT ,VALUE='结束时间:')

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
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='请选择数据的参数:')


  LBL_RANGE_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_RANGE_HANTS' ,XOFFSET=8 ,YOFFSET=33+3 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='县市  :')


  DST_RANGE_HANTS = WIDGET_COMBOBOX(BASE_PLACE_HANTS,  $
      UNAME='DST_RANGE_HANTS' ,XOFFSET=60 ,YOFFSET=33 ,SCR_XSIZE=140  $
      ,SCR_YSIZE=23 )

  ;**********************************************************************************
  TEMP=27
  LBL_SENSOR_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_SENSOR_HANTS' ,XOFFSET=8 ,YOFFSET=33+3+TEMP ,SCR_XSIZE=45  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='传感器:')


  DST_SENSOR_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_SENSOR_HANTS' ,XOFFSET=60 ,YOFFSET=33+TEMP ,SCR_XSIZE=140  $
      ,SCR_YSIZE=23 )

  LBL_period_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_period_HANTS' ,XOFFSET=8 ,YOFFSET=33+3+TEMP*2 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='周期  :')


  DST_period_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_period_HANTS' ,XOFFSET=60 ,YOFFSET=33+TEMP*2 ,SCR_XSIZE=140  $
      ,SCR_YSIZE=23 )

BASE_PARAMETER_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_PARAMETER_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=255  $
      ,SCR_XSIZE=217 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)

  BASE_PARAMETER_HANTS_RX = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_TOLERANCE_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_RX,/ALIGN_LEFT ,VALUE='容限:')
  TXT_TOLERANCE_HANTS = widget_slider(BASE_PARAMETER_HANTS_RX,VALUE=5,MAXIMUM=500, MINIMUM=0,XSIZE=155,event_pro='slider_event',uname='TOLERANCE')

  BASE_PARAMETER_HANTS_DOD = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_DOD_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_DOD,/ALIGN_LEFT ,VALUE='DOD :')
  TXT_DOD_HANTS = widget_slider(BASE_PARAMETER_HANTS_DOD,VALUE=2,MAXIMUM=100, MINIMUM=1,XSIZE=155,event_pro='slider_event',uname='DOD')

  BASE_PARAMETER_HANTS_PL = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_FREQUENCY_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_PL,/ALIGN_LEFT ,VALUE='频率:')
  TXT_FREQUENCY_HANTS = widget_slider(BASE_PARAMETER_HANTS_PL,VALUE=2,MAXIMUM=4, MINIMUM=1,XSIZE=155,event_pro='slider_event',uname='FREQUENCY')

  BASE_CMD_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_CMD_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=322  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=38 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  CMD_OK_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS, UNAME='CMD_OK_HANTS'  $
      ,XOFFSET=7 ,YOFFSET=6 ,SCR_XSIZE=60 ,SCR_YSIZE=24  $
      ,/ALIGN_CENTER ,VALUE='曲线重构')


  CMD_CLOSE_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS,  $
      UNAME='CMD_CLOSE_HANTS' ,XOFFSET=149 ,YOFFSET=6 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
      ,VALUE='关闭')


  CMD_DB_WRITE_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS,  $
      UNAME='CMD_DB_WRITE_HANTS' ,XOFFSET=80 ,YOFFSET=6 ,SCR_XSIZE=56  $
      ,SCR_YSIZE=24  ,/ALIGN_CENTER  $
      ,VALUE='数据入库')

  ;**********************************************************************************


  BASE_DRAW_HANTS = WIDGET_BASE(BASE_TOP_HANTS,  $
      UNAME='BASE_DRAW_HANTS' ,FRAME=1  $
      ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/col)


  LBL_DRAW_HANTS = WIDGET_LABEL(BASE_DRAW_HANTS,  $
      UNAME='LBL_DRAW_HANTS' ,XOFFSET=117 ,YOFFSET=2 ,SCR_XSIZE=128  $
      ,SCR_YSIZE=14 ,/ALIGN_center ,VALUE='曲线拟合结果显示')


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

		RANGE				:	'请选择'		,$
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
  	;给下拉列表赋值,有的数据需要从数据库中读取
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
  	;从数据库中把县的列表数据读取出来,并把数据显示在下拉列表中
  	;SQL='select CODE,NAME from COUNTY_CODE WHERE (PROVINCE_CODE = '+"'"+PROVINCE_code+"' "+')'
  	;对原来的SQL语句进行了调整,为了保证所有COUNTY_CODE在参数表中都有数据,
  	;从COUNTY_CODE_RASTER中进行了数据的读取
  	SQL='select T1.CODE,T2.NAME from COUNTY_CODE_RASTER T1,COUNTY_CODE T2 WHERE T1.CODE=T2.CODE '
  	SQL=SQL+'ORDER BY T2.CODE'
    PRINT,SQL
    oRS = OBJ_NEW('IDLdbRecordset', DBobj, SQL=SQL)


	;获取数据记录的个数
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM_COUNTY=RecordNum
	Obj_Destroy,RecordNumOBJ
	PRINT,NUM_COUNTY

	;这里给省的起始的个数是2,而不是0
	;这是因为第一个选项是'请选择',第二个选项是'所有县'
	num_of_COUNTY=NUM_COUNTY+2

   	IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

		;然后将国家的名称读入数组
		ARR_COUNTY		=STRARR(num_of_COUNTY)
		ARR_COUNTY_CODE	=STRARR(num_of_COUNTY)
		ARR_COUNTY[0]='请选择'
		ARR_COUNTY[1]='所有县(批处理)'
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
	  ;为了系统演示所增加的代码
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
	common_log,'启动过程线重构'
  ;避免同一个界面重复出现
  IF ( XREGISTERED('BASE_TOP_HANTS') NE 0 ) THEN RETURN
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_HANTS,GROUP_LEADER=BASE_TOP
END
