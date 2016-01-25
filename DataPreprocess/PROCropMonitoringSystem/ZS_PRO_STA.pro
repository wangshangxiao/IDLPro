;
; MJH
; 用来作分县的参数统计
;2006.08.10
;是在原来全球统计模块的基础上进行改进而成的
;-----------------------------------------------------------------


;-----------------------------------------------------------------
;这是系统的主函数
;下面的注释是函数的调用形式
;RESULT=STAT_PRO_STATE(FILE_IN,YEAR,MONTH,DAY	$
;						,SENSOR_TYPE,DATA_TYPE	$
;						,RANGE,RANGE_CODE,RANGE_ABBR_NAME,DBCO)
FUNCTION STAT_PRO_COUNTY	,FILE_IN,YEAR,MONTH,DAY	$
						,SENSOR_CODE,DATA_TYPE	$
						,PERIOD,DBCO
	PRINT,'***********************************************'
	PRINT,'分省统计,参数如下:'
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




	;在这里获得三个文件图像信息

	FILE_PLOWLAND_INFO	=GET_IMAGE_INFO(FILE_PLOWLAND)
	FILE_DRY_LAND_INFO	=GET_IMAGE_INFO(FILE_DRY_LAND)
	FILE_PADDY_FI_INFO	=GET_IMAGE_INFO(FILE_PADDY_FI)

	FILE_COUNTY_INFO	=GET_IMAGE_INFO(FILE_COUNTY)
	FILE_IN_INFO		=GET_IMAGE_INFO(FILE_IN)


	;**************************************************************************************
	;然后对输入图像的异常情况进处理
	;只要三个文件中有一个是无效文件则返回统计不成功
	IF(	(FILE_PLOWLAND_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_DRY_LAND_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_PADDY_FI_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_COUNTY_INFO.FILE_RIGHT NE 1) OR	$
		(FILE_IN_INFO.FILE_RIGHT NE 1)) THEN BEGIN
		PRINT,'获取图像头信息时出现错误'
		RETURN,0
	ENDIF

	;两个方向分辩率一致的要求
	IF((FILE_IN_INFO.X_PIXELSIZE NE FILE_IN_INFO.Y_PIXELSIZE)) THEN BEGIN
		PRINT,'输入数据的两个方向分辨率不一致'
		RETURN,0
	ENDIF

	;空间上要有重叠区的判断
	IF((FILE_IN_INFO.STARTX+FILE_IN_INFO.XSIZE*FILE_IN_INFO.X_PIXELSIZE LT FILE_COUNTY_INFO.STARTX) OR $
	   (FILE_IN_INFO.STARTX GT FILE_COUNTY_INFO.STARTX+FILE_COUNTY_INFO.XSIZE*FILE_COUNTY_INFO.X_PIXELSIZE)) THEN BEGIN
		PRINT,'输入数据的数据和基础数据在空间上没有重叠区'
		RETURN,0
	ENDIF
	;**************************************************************************************


	;**************************************************************************************
	;通过数据的空间信息,获得重叠数据区的空间信息,用于进行数据的读取

	;**************************************************************************************

	X_START_READ=(FILE_IN_INFO.STARTX>FILE_COUNTY_INFO.STARTX)>FILE_PLOWLAND_INFO.STARTX

	X_END_READ	=(FILE_IN_INFO.STARTX+FILE_IN_INFO.XSIZE*FILE_IN_INFO.X_PIXELSIZE)	$
		<(FILE_COUNTY_INFO.STARTX+FILE_COUNTY_INFO.XSIZE*FILE_COUNTY_INFO.X_PIXELSIZE) $
		<(FILE_PLOWLAND_INFO.STARTX+FILE_PLOWLAND_INFO.XSIZE*FILE_PLOWLAND_INFO.X_PIXELSIZE)

	Y_START_READ=(FILE_IN_INFO.STARTY<FILE_COUNTY_INFO.STARTY)<FILE_PLOWLAND_INFO.STARTY

	;经纬度的坐标由下到上是越来越大,所以纵向和横向所用的符号相反
	Y_END_READ	=(FILE_IN_INFO.STARTY-FILE_IN_INFO.YSIZE*FILE_IN_INFO.Y_PIXELSIZE)	$
		>(FILE_COUNTY_INFO.STARTY-FILE_COUNTY_INFO.YSIZE*FILE_COUNTY_INFO.Y_PIXELSIZE) $
		>(FILE_PLOWLAND_INFO.STARTY-FILE_PLOWLAND_INFO.YSIZE*FILE_PLOWLAND_INFO.Y_PIXELSIZE)

	;对图像的范围进行判断
	IF(X_START_READ GT X_END_READ)THEN BEGIN
		PRINT,'统计的区域与输入数据没有重叠的部分'
		RETURN,0
	ENDIF
	IF(Y_START_READ LT Y_END_READ)THEN BEGIN
		PRINT,'统计的区域与输入数据没有重叠的部分'
		RETURN,0
	ENDIF

	;********************************************************************************
	;2006.08.11
	;在这里对原来的程序进行了修改,需要注意的是,程序要求能接收不同分辩率的数据,
	;原则上要求能对任何分辩率的数据进行处理(实际上这里不可能的)
	;我们认为所有基础数据的分辩率都是一致,目前是1000M,
	;因而使用高分辩率的数据在这里没有什么实际意义,
	;同时为了方便数据处理的需求,在进行了公共块的数据的读取后
	;把输入的参数文件CONGRID成与基础数据分辩率相同的数据.
	;为了数据方便,也可以对所有的数据都进行CONGRID处理,
	;这样一来,即使基础数据的分辩率不同,系统也同样可以进行处理
	;这里统一了一个共同的分辨率,为1000M
	;********************************************************************************
	;在这里假设三个耕地数据的大小和分辩率是一致辞的,只选择了一个耕地的数据作为代表
	COMMON_AREA=GET_COMMON_AREA_3(	FILE_IN,	$
									FILE_COUNTY,$
									FILE_PLOWLAND)
	PRINT,COMMON_AREA


	;获取系统中县的个数,指要与栅格值相对应的那个部分
	TABLE_NAME='COUNTY_CODE_RASTER'
	SQL='SELECT CODE,RASTER_VALUE FROM COUNTY_CODE_RASTER ORDER BY RASTER_VALUE'
	ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL)

	;获取数据记录的个数
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM_COUNTY=RecordNum
	Obj_Destroy,RecordNumOBJ
	PRINT,NUM_COUNTY


	;-------------------------------------------------------------------------------
	;这两行代码用于进程条的显示
	temp_str='正在统计'+STRTRIM(DATA_TYPE,2)+'数据'
	temp_str=temp_str+string(byte(13))+string(byte(10))
	temp_str=temp_str+'时间:'+STRTRIM(YEAR,2)+'年'+STRTRIM(MONTH,2)+'月'+STRTRIM(DAY,2)+'日'
	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON,MESSAGE=temp_str,TITLE='过程监测统计')
	PROGRESSTIMER->START
	;-------------------------------------------------------------------------------

	;********************************************************************************
	;定义用于存储统计结果的结构体
	RESULT= {	$
		COUNTY_CODE	:	''	,$
		RASTER_VAL	:	''	,$

		FLAG		:	0	,$;一个标识量,用来标识该县是否有有效数据

		SUM_PL		:	0.0	,$;用来存储参数的累加
		W_PL		:	0.0	,$;用来存储权重的累加
		V_PL		:	0.0	,$;用来存储最后的加权平均值

		SUM_PF		:	0.0	,$;分耕地\水田\旱地三种结果
		W_PF		:	0.0	,$;PL,PLOWLAND,PF PADDY FIELD,DL DRY LAND
		V_PF		:	0.0	,$

		SUM_DL		:	0.0	,$
		W_DL		:	0.0	,$
		V_DL		:	0.0	 $
		}


	ARR_RESULT	=	REPLICATE(RESULT,NUM_COUNTY)
	;对这些存储结果的结构体数据进行初始化
	COUNT=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
		REPEAT BEGIN
			ARR_RESULT[COUNT].COUNTY_CODE= ORS -> GETFIELD(0)
			ARR_RESULT[COUNT].RASTER_VAL= ORS -> GETFIELD(1)

			COUNT=COUNT+1
		ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
	ENDIF
	Obj_Destroy,ORS
	;PRINT,'   ',ARR_RESULT

	;********************************************************************************
	;进行数据的读取,并把数据统一到相同的分辩率上,
	;初步确定的统一分辩率是1KM
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


	;因为我们认为耕地/水田/和旱地三个数据的大小和分辩率及空间属性等都是相同的
	;因为使用耕地的相关参数对水田和旱地的数据进行了读取
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

	;**************************************************************************
	FOR I=0,LINES-1 DO BEGIN
		;-------------------------------------------------------------------------------
		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了系统的处理.',TITLE='提示')
			CLOSE,/ALL
			PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
		ENDIF
		PROGRESSTIMER->UPDATE, (FLOAT(I)/LINES * 100.0) ;继续进行

		;以列为循环,开始统计
		;PRINT,'A'
		FOR J=LONG(0),SAMPLES-1 DO BEGIN
			IF((ARR_FILE_PL[J,I] GT 0) AND $
				(ARR_FILE_COUNTY[J,I] NE 0) AND (ARR_FILE_IN[J,I] GT 100)) THEN BEGIN
				;这是本模块的核心部分,使用加权的方法进行累加
				K=ARR_FILE_COUNTY[J,I]-1
				ARR_RESULT[K].SUM_PL=	FLOAT(ARR_RESULT[K].SUM_PL)	$
										+FLOAT(ARR_FILE_IN[J,I])*FLOAT(ARR_FILE_PL[J,I])
				ARR_RESULT[K].SUM_PF=	FLOAT(ARR_RESULT[K].SUM_PF)	$
										+FLOAT(ARR_FILE_IN[J,I])*FLOAT(ARR_FILE_PF[J,I])
				ARR_RESULT[K].SUM_DL=	FLOAT(ARR_RESULT[K].SUM_DL)	$
										+FLOAT(ARR_FILE_IN[J,I])*FLOAT(ARR_FILE_DL[J,I])
				;

				ARR_RESULT[K].W_PL=	ARR_RESULT[K].W_PL+ARR_FILE_PL[J,I]
				ARR_RESULT[K].W_PF=	ARR_RESULT[K].W_PF+ARR_FILE_PF[J,I]
				ARR_RESULT[K].W_DL=	ARR_RESULT[K].W_DL+ARR_FILE_DL[J,I]

			ENDIF
		ENDFOR
	ENDFOR


	;******************************************************************************************
	;在这里对结果进行最后的计算,并把结果写入数据库
	;(1)计算平均值
	FOR K=0,NUM_COUNTY-1 DO BEGIN
		IF(ARR_RESULT[K].W_PL NE 0) THEN BEGIN
			ARR_RESULT[K].V_PL=(ARR_RESULT[K].SUM_PL/ARR_RESULT[K].W_PL)/127-1
		ENDIF
		IF(ARR_RESULT[K].W_PF NE 0) THEN BEGIN
			ARR_RESULT[K].V_PF=(ARR_RESULT[K].SUM_PF/ARR_RESULT[K].W_PF)/127-1
		ENDIF
		IF(ARR_RESULT[K].W_DL NE 0) THEN BEGIN
			ARR_RESULT[K].V_DL=(ARR_RESULT[K].SUM_DL/ARR_RESULT[K].W_DL)/127-1
		ENDIF
	ENDFOR

	;(2)结果入库

	;数据库操作
	TABLE='PARAMETER_PROCESS_COUNTY'
	;(A)将数据库中原有的数据进行删除

	;删除数据库中与要插入的记录时空属性相同的记录
	SQL='DELETE FROM '+TABLE+' WHERE ('
	SQL=SQL+'(YEAR = '+STRTRIM(STRING(YEAR),2)		+')'
	SQL=SQL+' AND (MONTH = '+STRTRIM(STRING(MONTH),2)		+')'
	SQL=SQL+' AND (DAY = '+STRTRIM(STRING(DAY),2)	+')'
	SQL=SQL+' AND (DATA_TYPE = '+"'"+STRTRIM(STRING(DATA_TYPE),2)	+"'"+')'
	SQL=SQL+' AND (periods = '+STRTRIM(STRING(PERIOD),2)	+')'
	SQL=SQL+' AND (SENSOR_CODE = '+"'"+STRTRIM(STRING(SENSOR_CODE),2)	+"'"+'))'
	PRINT,SQL
	DBCO->EXECUTESQL,SQL

	;(B)插入新的数据
	;转化成真实的NDVI值

	FOR I=0,NUM_COUNTY-1 DO BEGIN
;		SQL='INSERT INTO '+TABLE+'"'
;		SQL=SQL+'("COUNTY_CODE" ,"YEAR" ,"MONTH" ,"DAY",'
;		SQL=SQL+'"periods" ,"DATA_TYPE" ,"SENSOR_CODE",'
;		SQL=SQL+'"avg_plowland" ,"avg_paddy_field" ,"avg_dry_land",'
;		SQL=SQL+'"AVG_SPRING_WHEAT" ,"AVG_WINTER_WHEAT" ,'
;		SQL=SQL+'"AVG_EARLY_RICE" ,"AVG_SEMILATE_RICE" ,"AVG_LATE_RICE",'
;		SQL=SQL+',"AVG_MAIZE",,"AVG_SOYBEAN" ) '
		SQL='INSERT INTO '+TABLE+' '
		SQL=SQL+'(COUNTY_CODE ,YEAR ,MONTH ,DAY,'
		SQL=SQL+'periods,DATA_TYPE,SENSOR_CODE,'
		SQL=SQL+'avg_plowland ,avg_paddy_field,avg_dry_land,'
		SQL=SQL+'AVG_SPRING_WHEAT,AVG_WINTER_WHEAT,'
		SQL=SQL+'AVG_EARLY_RICE,AVG_SEMILATE_RICE,AVG_LATE_RICE,'
		SQL=SQL+'AVG_MAIZE,AVG_SOYBEAN ) '
		SQL=SQL+'VALUES ('
		SQL=SQL+"'"+STRTRIM(STRING(ARR_RESULT[I].COUNTY_CODE),2)	+"'"+','
		SQL=SQL+STRTRIM(STRING(YEAR),2)				+','
		SQL=SQL+STRTRIM(STRING(MONTH),2)			+','
		SQL=SQL+STRTRIM(STRING(DAY),2)				+','
		SQL=SQL+STRTRIM(STRING(period),2)			+','
		SQL=SQL+"'"+STRTRIM(STRING(DATA_TYPE),2)			+"'"+','
		SQL=SQL+"'"+STRTRIM(STRING(SENSOR_CODE),2)			+"'"+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_PL),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_PF),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_DL),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_DL),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_DL),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_PF),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_PF),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_PF),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_DL),2)			+','
		SQL=SQL+STRTRIM(STRING(ARR_RESULT[I].V_DL),2)			+')'
		PRINT,SQL
		DBCO->EXECUTESQL,SQL
	ENDFOR
	;******************************************************************************************

	;******************************************************************************************
	ARR_FILE_IN=0
	ARR_FILE_COUNTY=0
	ARR_FILE_PL=0
	ARR_FILE_PF=0
	ARR_FILE_DL=0
	PRINT,'END OF THE PRO'
	;-------------------------------------------------------------------------------
	PROGRESSTIMER->DESTROY ;
	;-------------------------------------------------------------------------------

	;******************************************************************************************

	RETURN,1

END
;-----------------------------------------------------------------
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION CMD_OK_STA_PRO_COUNTY, EVENT

    ;在这里获取程序界面上的参数
    ;分两种情况,一种是单个处理的情况,一种是批处理的情况
    WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE


    IF((*PSTATE).SINGLE_BATCH EQ 'SINGLE') THEN BEGIN ;单个处理

    	YEAR				=(*PSTATE).YEAR
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
         	MSG=DIALOG_MESSAGE('请选择输入文件!',/INFORMATION,TITLE='请选择输入文件')
         	CLOSE,/ALL
         	RETURN,0
     	ENDIF

		IF(DATA_TYPE NE 'NDVI') THEN BEGIN
			TEMP=DIALOG_MESSAGE('系统目前只能处理NDVI',TITLE='条件错误')
			CLOSE,/ALL
     		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
		ENDIF
		PRINT,'HERE 1'

		RESULT=STAT_PRO_COUNTY(FILE_IN,YEAR,MONTH,DAY	$
				,SENSOR_CODE,DATA_TYPE	$
				,PERIOD,DBCO)

		IF(RESULT EQ 1) THEN BEGIN
			TEMP=DIALOG_MESSAGE('统计成功!',TITLE='成功')
			log, '过程监测-统计', 1
		ENDIF ELSE BEGIN
			TEMP=DIALOG_MESSAGE('统计失败!',TITLE='失败')
			log, '过程监测-统计', -1
		ENDELSE


		;单个统计的代码结束
    ENDIF ELSE BEGIN	;批处理


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
			TEMP=DIALOG_MESSAGE('系统目前只能处理NDVI',TITLE='条件错误')
			CLOSE,/ALL
     		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
		ENDIF

		;计算开始和结束的日数
		JULDAY_START=JULDAY(MONTH_START, DAY_START, YEAR_START, 0, 0, 0)
		JULDAY_END	=JULDAY(MONTH_END, DAY_END, YEAR_END, 0, 0, 0)


		;进行批处理的时候对时间有一定的要求,在这里要对时间是否合理进行判断
		IF(JULDAY_START GT JULDAY_END) THEN BEGIN
			TEMP=DIALOG_MESSAGE('起始时间必须要早于结束时间',TITLE='条件错误')
			CLOSE,/ALL
     		RETURN, 0
		ENDIF

		;分统计到全国和分州的统计来处理
		COUNT_SUCCESS=0;一个标识量,用来记录成功统计的数据的数量(期)
		FOR	I=JULDAY_START,JULDAY_END DO BEGIN

			CALDAT, I, MONTH, DAY, YEAR, H, M, S
			PRINT,YEAR,MONTH,DAY
			YEAR=STRTRIM(YEAR,2)
			;解决月的位数问题
			IF(FIX(MONTH) LE 9) THEN BEGIN
				MONTH	=	'0'+STRTRIM(MONTH,2)
			ENDIF ELSE BEGIN
				MONTH	=	STRTRIM(MONTH,2)
			ENDELSE

			;解决月的位数问题
			IF(FIX(DAY) LE 9) THEN BEGIN
				DAY	=	'0'+STRTRIM(DAY,2)
			ENDIF ELSE BEGIN
				DAY	=	STRTRIM(DAY,2)
			ENDELSE

			FILE_IN	=	FILE_PATH+PREFIX+YEAR+MONTH+DAY+SUFFIX
			RESULT=STAT_PRO_COUNTY(FILE_IN,YEAR,MONTH,DAY	$
				,SENSOR_CODE,DATA_TYPE	$
				,PERIOD,DBCO)

			IF(RESULT EQ 1) THEN BEGIN
				PRINT,STRTRIM(YEAR,2)+'年'+STRTRIM(MONTH,2)+'月'+STRTRIM(DAY,2)+'日数据处理成功'
				COUNT_SUCCESS=COUNT_SUCCESS+1
			ENDIF ELSE BEGIN
				PRINT,STRTRIM(YEAR,2)+'年'+STRTRIM(MONTH,2)+'月'+STRTRIM(DAY,2)+'日数据处理失败'
			ENDELSE
		ENDFOR
		TEMP='共成功统计了'+STRTRIM(COUNT_SUCCESS,2)+'期数据!'
		TEMP_1=DIALOG_MESSAGE(TEMP,TITLE='统计结果')

		if COUNT_SUCCESS eq 0 then begin
			log, '过程监测-统计', -1
		endif else if COUNT_SUCCESS eq (FIX(JULDAY_END)-FIX(JULDAY_START)) then begin
			log, '过程监测-统计', 1
		endif else begin
			log, '过程监测-统计', 0
		endelse

    ENDELSE
 	RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;
; Empty stub procedure used for autoloading.
;***************************************************************************
;***************************************************************************
;
pro STA_PRO_COUNTY_eventcb
end
;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	03/11/2005 16:45.10
;
pro STAT_PRO_COUNTY_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

  common common_setpath, ppath
  zs_in_path = (*ppath).zs_in_path

  wWidget =  Event.top
  WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
  ;******************************************************************************
  ;有一个新的发现,在按名称为控件添加事件时,名称居然是对大小写敏感的,这是个新的发现
  ;以后需要注意这个情况
  ;******************************************************************************
  case wTarget of
	;显示文件名

	Widget_Info(wWidget, FIND_BY_UNAME='CMD_CHANGE_PATH'): begin
    	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
 		FILE_PATH_NEW=DIALOG_PICKFILE( /DIRECTORY,path=zs_in_path,title='请选择目标文件路径', DIALOG_PARENT=Event.id)
 		IF(FILE_PATH_NEW NE '') THEN BEGIN
 	 		WIDGET_CONTROL,(*pstate).TXT_FILE_PATH,Set_value=FILE_PATH_NEW
 		ENDIF
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
    	;处理月的位数问题
    	IF ((*PSTATE).MONTH_START LE 9) THEN BEGIN
    		TEXT_STR=TEXT_STR+'0'+STRTRIM((*PSTATE).MONTH_START,2)
    	ENDIF ELSE BEGIN
    		TEXT_STR=TEXT_STR+STRTRIM((*PSTATE).MONTH_START,2)
    	ENDELSE
    	;处理天的位数问题
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
	    file=DIALOG_PICKFILE(filter='*.*',path=zs_in_path,title='请选择一个文件', DIALOG_PARENT=Event.id)
	    IF (file NE '') THEN BEGIN
			WIDGET_CONTROL,(*PSTATE).TXT_FILE_IN,SET_VALUE=FILE
	    ENDIF
    end
    ;年
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
		defaultnames_zsprosta, event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_DATA_TYPE_BATCH'): begin
    	INDEX=EVENT.INDEX
		(*PSTATE).DATA_TYPE_BATCH=((*PSTATE).ARR_DATA_TYPE)[INDEX]
		;PRINT, (*PSTATE).DATA_TYPE_BATCH
		defaultnames_zsprosta, event
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
				ONLINE_HELP, "过程监测模块", BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse

;    	ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH,"'过程监测模块'"
    end

    else:
  endcase
end

pro ZS_PRO_STA, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_

  ;避免同一个界面重复出现
  IF ( XREGISTERED('STAT_PRO_COUNTY') NE 0 ) THEN RETURN
  Resolve_Routine, 'STA_PRO_COUNTY_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;也可以不初始化

COMMON COMMON_SETPATH,ppath;	$
;		zs_in_path  ,$
;		zs_out_path ,$
;		mj_in_path  ,$
;		dc_out_path ,$
;		cl_out_path ,$
;		nq_in_path  ,$
;		nq_out_path ,$
;		fz_in_path  ,$
;		fz_out_path

zs_in_path = (*ppath).zs_in_path
zs_out_path = (*ppath).zs_out_path

common current_date, c_year, c_month, c_day

  TEMP=1
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  STAT_PRO_COUNTY = Widget_Base( GROUP_LEADER=BASE_TOP,  $
      UNAME='STAT_PRO_COUNTY' ,XOFFSET=360 ,YOFFSET=200 ,SCR_XSIZE=326  $
      ,SCR_YSIZE=325 ,TITLE='过程监测--统计到县' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR =1)


  TAB_STA_COUNTY = Widget_Tab(STAT_PRO_COUNTY, UNAME='TAB_STA_COUNTY'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=307 ,SCR_YSIZE=235)


  BASE_SINGLE_PRO = Widget_Base(TAB_STA_COUNTY,  $
      UNAME='BASE_SINGLE_PRO' ,SCR_XSIZE=299 ,SCR_YSIZE=210  $
      ,TITLE='单个处理' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_0'  $
      ,XOFFSET=7 ,YOFFSET=10 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='数据类型:')


  WID_LABEL_1 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_1'  $
      ,XOFFSET=7 ,YOFFSET=35 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='传感器  :')


  WID_LABEL_2 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_2'  $
      ,XOFFSET=7 ,YOFFSET=60 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='时间    :')


  WID_LABEL_5 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_5'  $
      ,XOFFSET=7 ,YOFFSET=135 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='输入文件:')


  DST_DATA_TYPE = Widget_Droplist(BASE_SINGLE_PRO,  $
      UNAME='DST_DATA_TYPE' ,XOFFSET=75-TEMP ,YOFFSET=6 ,SCR_XSIZE=83  $
      ,SCR_YSIZE=20)


  DST_SENSOR_TYPE = Widget_Droplist(BASE_SINGLE_PRO,  $
      UNAME='DST_SENSOR_TYPE' ,XOFFSET=75-TEMP ,YOFFSET=31 ,SCR_XSIZE=83  $
      ,SCR_YSIZE=20)

  ;******************************************************************
  ;加入了周期这个选项
  WID_LABEL_2 = Widget_Label(BASE_SINGLE_PRO, UNAME='WID_LABEL_2'  $
      ,XOFFSET=7 ,YOFFSET=60+25 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='周期    :')

  DST_period = Widget_Droplist(BASE_SINGLE_PRO, UNAME='DST_period'  $
      ,XOFFSET=75-TEMP ,YOFFSET=56+25 ,SCR_XSIZE=53 ,SCR_YSIZE=20)
  ;******************************************************************
  subbase = widget_base(BASE_SINGLE_PRO,XOFFSET=72-TEMP ,YOFFSET=53,/row)

  WID_LABEL_date = Widget_text(subbase,xsize=15, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))

 CMD_pick_date = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date', $
 VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_sta'})

  ;DST_RANGE = Widget_Droplist(BASE_SINGLE_PRO, UNAME='DST_RANGE'  $
     ; ,XOFFSET=75 ,YOFFSET=81 ,SCR_XSIZE=90 ,SCR_YSIZE=20)
  ;****************************************************************************
    ;****************************************************************************

  TXT_FILE_IN = Widget_Text(BASE_SINGLE_PRO, UNAME='TXT_FILE_IN'  $
      ,XOFFSET=75-TEMP ,YOFFSET=132 ,SCR_XSIZE=166 ,SCR_YSIZE=20  $
      ,/EDITABLE ,XSIZE=20 ,YSIZE=1,$
      VALUE= zs_in_path )


  CMD_CHOOSE_FILE = Widget_Button(BASE_SINGLE_PRO,  $
      UNAME='CMD_CHOOSE_FILE' ,XOFFSET=257-TEMP ,YOFFSET=131 ,SCR_XSIZE=36  $
      ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='.\Image\open.bmp' ,/BITMAP)


  BASE_BATCH_PRO = Widget_Base(TAB_STA_COUNTY, UNAME='BASE_BATCH_PRO'  $
      ,SCR_XSIZE=299 ,SCR_YSIZE=210 ,TITLE='批处理' ,SPACE=3 ,XPAD=3  $
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
;      ,/ALIGN_LEFT ,VALUE='日')
;
;
;  WID_LABEL_10 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_10'  $
;      ,XOFFSET=196 ,YOFFSET=60 ,SCR_XSIZE=18 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='月')
;
;
;  DST_MONTH_START = Widget_Droplist(BASE_BATCH_PRO,  $
;      UNAME='DST_MONTH_START' ,XOFFSET=154 ,YOFFSET=56 ,SCR_XSIZE=39  $
;      ,SCR_YSIZE=20)
;
;
;  WID_LABEL_11 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_11'  $
;      ,XOFFSET=132 ,YOFFSET=60 ,SCR_XSIZE=21 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='年')
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
      ,VALUE='起始时间:')

	subbase = widget_base(BASE_BATCH_PRO,XOFFSET=72-TEMP ,YOFFSET=53,/row)
  	WID_LABEL_date_s = Widget_text(subbase,xsize=15, $
		VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))
 	CMD_pick_date_s = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, $
		VALUE='.\Image\Calendar.bmp',/BITMAP,EVENT_PRO='ActiveXCal',uvalue={text_id:WID_LABEL_date_s, pointer:PTR_NEW(), detail:'CMD_pick_date_s'})


;  WID_LABEL_12 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_12'  $
;      ,XOFFSET=266 ,YOFFSET=86 ,SCR_XSIZE=19 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='日')
;
;
  WID_LABEL_18 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_18'  $
      ,XOFFSET=7 ,YOFFSET=86 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='结束时间:')

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
;      ,/ALIGN_LEFT ,VALUE='年')
;
;
;  DST_MONTH_END = Widget_Droplist(BASE_BATCH_PRO,  $
;      UNAME='DST_MONTH_END' ,XOFFSET=153+1 ,YOFFSET=82 ,SCR_XSIZE=39  $
;      ,SCR_YSIZE=20)
;
;
;  WID_LABEL_20 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_20'  $
;      ,XOFFSET=196+1 ,YOFFSET=86 ,SCR_XSIZE=18 ,SCR_YSIZE=16  $
;      ,/ALIGN_LEFT ,VALUE='月')
;
;
;  DST_DAY_END = Widget_Droplist(BASE_BATCH_PRO, UNAME='DST_DAY_END'  $
;      ,XOFFSET=223+1 ,YOFFSET=82 ,SCR_XSIZE=38 ,SCR_YSIZE=20)


  WID_LABEL_16 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_16'  $
      ,XOFFSET=7 ,YOFFSET=35 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='传感器  :')


  WID_LABEL_17 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_17'  $
      ,XOFFSET=7 ,YOFFSET=10 ,SCR_XSIZE=55 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='数据类型:')

  WID_LABEL_21 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_21'  $
      ,XOFFSET=7 ,YOFFSET=161 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='前缀    :')

  ;******************************************************************
  ;加入了周期这个选项
  WID_LABEL_2 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_2'  $
      ,XOFFSET=7 ,YOFFSET=60+50 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='周期    :')

  DST_period_BATCH = Widget_Droplist(BASE_BATCH_PRO, UNAME='DST_period_BATCH'  $
      ,XOFFSET=75-1 ,YOFFSET=56+50 ,SCR_XSIZE=53 ,SCR_YSIZE=20)
  ;******************************************************************

  TEXT_PREFIX = Widget_Text(BASE_BATCH_PRO, UNAME='TEXT_PREFIX'  $
      ,XOFFSET=74 ,YOFFSET=158 ,SCR_XSIZE=50 ,SCR_YSIZE=20 ,/EDITABLE  $
      ,XSIZE=20 ,YSIZE=1,VALUE='SYN')


  TEXT_SUFFIX = Widget_Text(BASE_BATCH_PRO, UNAME='TEXT_SUFFIX'  $
      ,XOFFSET=209 ,YOFFSET=158 ,SCR_XSIZE=50 ,SCR_YSIZE=20  $
      ,/EDITABLE ,XSIZE=20 ,YSIZE=1,VALUE='NDVI')


  WID_LABEL_22 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_22'  $
      ,XOFFSET=142 ,YOFFSET=161 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='后缀    :')


  TEXT_NAME_SHOW = Widget_Text(BASE_BATCH_PRO, UNAME='TEXT_NAME_SHOW'  $
      ,XOFFSET=74 ,YOFFSET=183 ,SCR_XSIZE=156 ,SCR_YSIZE=20 ,XSIZE=20  $
      ,YSIZE=1)


  WID_LABEL_23 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_23'  $
      ,XOFFSET=7 ,YOFFSET=186 ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='文件名称:')


  CMD_SHOW_NAME_STA_PRO = Widget_Button(BASE_BATCH_PRO,  $
      UNAME='CMD_SHOW_NAME_STA_PRO' ,XOFFSET=246 ,YOFFSET=182  $
      ,SCR_XSIZE=46 ,SCR_YSIZE=22  ,/ALIGN_CENTER ,VALUE='显示')

  ;***********************************************************************
  TEMP=50
  TXT_FILE_PATH = Widget_Text(BASE_BATCH_PRO, UNAME='TXT_FILE_PATH'  $
      ,XOFFSET=74 ,YOFFSET=183-TEMP ,SCR_XSIZE=156 ,SCR_YSIZE=20 ,XSIZE=20  $
      ,YSIZE=1,VALUE=zs_in_path)
      PRINT,FILE_PATH


  WID_LABEL_23 = Widget_Label(BASE_BATCH_PRO, UNAME='WID_LABEL_23'  $
      ,XOFFSET=7 ,YOFFSET=186-TEMP ,SCR_XSIZE=55 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='路径    :')


  CMD_CHANGE_PATH = Widget_Button(BASE_BATCH_PRO,  $
      UNAME='CMD_CHANGE_PATH' ,XOFFSET=256 ,YOFFSET=181-TEMP  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=22  ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)
  ;***********************************************************************


  CMD_OK_STA_PRO_COUNTY = Widget_Button(STAT_PRO_COUNTY,  $
      UNAME='CMD_OK_STA_PRO_COUNTY' ,XOFFSET=12 ,YOFFSET=251  $
      ,SCR_XSIZE=79 ,SCR_YSIZE=22 ,EVENT_FUNC='CMD_OK_STA_PRO_COUNTY'  $
      ,/ALIGN_CENTER ,VALUE='统计')


  CMD_HELP_STA_PRO = Widget_Button(STAT_PRO_COUNTY,  $
      UNAME='CMD_HELP_STA_PRO' ,XOFFSET=120 ,YOFFSET=251  $
      ,SCR_XSIZE=79 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='帮助')


  CMD_CLOSE_STA_PRO = Widget_Button(STAT_PRO_COUNTY,  $
      UNAME='CMD_CLOSE_STA_PRO' ,XOFFSET=228 ,YOFFSET=251  $
      ,SCR_XSIZE=79 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='关闭')

	year_droplist=strtrim(string(indgen(36)+1980),2)

  STATE = { $
		widget_top : STAT_PRO_COUNTY,$
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

		;记录国家四个边的坐标
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

		RANGE			:	'请选择'		,$
		RANGE_BATCH		:	'请选择'		,$
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
    WIDGET_CONTROL, STAT_PRO_COUNTY, SET_UVALUE=PSTATE

    WIDGET_CONTROL, CMD_pick_date, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_s, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_s, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_e, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_e, set_uvalue=staff

    Widget_Control, /REALIZE, STAT_PRO_COUNTY
    WIDGET_CONTROL,CMD_CLOSE_STA_PRO,/INPUT_FOCUS

  ;************************************************************************
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
  ;为了系统演示所增加的代码
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

	  	FILE_TEMP=zs_in_path
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

;***************************************************************************
	defaultnames_zsprosta,{ID:(*PSTATE).widget_top, TOP:(*PSTATE).widget_top}
;***********************************************************************
  XManager, 'STAT_PRO_COUNTY', STAT_PRO_COUNTY, /NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'

end

pro defaultnames_zsprosta, event
	Widget_Control, event.top, get_uvalue=pstate

	COMMON COMMON_SETPATH,ppath

	v_zs_in_path  = (*ppath).zs_in_path

	ndvi_prefix=(*ppath).ndvi_prefix
	ndvi_suffix=(*ppath).ndvi_suffix
	lai_prefix =(*ppath).lai_prefix
	lai_suffix =(*ppath).lai_suffix
	npp_prefix =(*ppath).npp_prefix
	npp_suffix =(*ppath).npp_suffix

	year = strtrim((*PSTATE).YEAR,2)
	;处理月的位数问题
	IF ((*PSTATE).MONTH LE 9) THEN BEGIN
		month='0'+STRTRIM((*PSTATE).MONTH,2)
	ENDIF ELSE BEGIN
		month=STRTRIM((*PSTATE).MONTH,2)
	ENDELSE
	;处理天的位数问题
	IF ((*PSTATE).DAY LE 9) THEN BEGIN
		day='0'+STRTRIM((*PSTATE).DAY,2)
	ENDIF ELSE BEGIN
		day=STRTRIM((*PSTATE).DAY,2)
	ENDELSE

	time = year + month + day

	case (*PSTATE).DATA_TYPE of
		'NDVI':begin
			prefix = ndvi_prefix
			suffix = ndvi_suffix
		end
		'LAI':begin
			prefix = lai_prefix
			suffix = lai_suffix
		end
		'NPP':begin
			prefix = npp_prefix
			suffix = npp_suffix
		end
		else:begin
			prefix = ndvi_prefix
			suffix = ndvi_suffix
		end
	endcase

  	WIDGET_CONTROL,(*PSTATE).TXT_FILE_IN, SET_VALUE= v_zs_in_path + prefix + time + suffix

	case (*PSTATE).DATA_TYPE_BATCH of
		'NDVI':begin
			prefix = ndvi_prefix
			suffix = ndvi_suffix
		end
		'LAI':begin
			prefix = lai_prefix
			suffix = lai_suffix
		end
		'NPP':begin
			prefix = npp_prefix
			suffix = npp_suffix
		end
		else:begin
			prefix = ndvi_prefix
			suffix = ndvi_suffix
		end
	endcase

	WIDGET_CONTROL,(*PSTATE).TXT_FILE_PATH, SET_VALUE= v_zs_in_path
	WIDGET_CONTROL,(*PSTATE).TXT_PREFIX, SET_VALUE = prefix
	WIDGET_CONTROL,(*PSTATE).TXT_SUFFIX, SET_VALUE = suffix
 end


