;
;用于长势系统给通报成图
;可以在县/自定监测区/省三个尺度进行进行长势监测结果的分析
;蒙继华
;最新修改时间:20060830
;在原来全球农作物长势遥感监测系统的结果成图模块的基础上进行开发
;modified mjh 20070428解决了横坐标的问题
;-----------------------------------------------------------------
;下面两个函数是系统的核心函数,分别完成了实时监测结果的成图和过程监测的成图
FUNCTION ZS_SHOW_JPG_RT, PSTATE
	;对异常条件进行判断
	PRINT,'开始实时监测成图'

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	if PTR_VALID((*PSTATE).P_IMAGE_RT) then ptr_free,(*PSTATE).P_IMAGE_RT

	IF((*PSTATE).SCALE_RT EQ '') THEN BEGIN
		TEMP=DIALOG_MESSAGE('请选择实时监测成图的尺度',TITLE='请选择尺度')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF

	IF((*PSTATE).RANGE_RT EQ '请选择') THEN BEGIN
		PRINT,(*PSTATE).RANGE_RT
		TEMP=DIALOG_MESSAGE('请选择实时监测成图的范围',TITLE='请选择范围')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF

	WIDGET_CONTROL,(*PSTATE).TXT_FILE_CLASSIFIED_RT_JPG,GET_VALUE=FILE_IN
	FILE_IN=FILE_IN[0]
	IF(FILE_IN EQ '') THEN BEGIN
     	msg=DIALOG_MESSAGE('请选择分级文件!',/INFORMATION,TITLE='提示')
     	CLOSE,/all
     	RETURN,0
 	ENDIF

	FILE_IN_INFO=GET_IMAGE_INFO(FILE_IN)
	IF (FILE_IN_INFO.FILE_RIGHT NE 1) THEN BEGIN
		msg=DIALOG_MESSAGE('输入的文件有误!',/INFORMATION,TITLE='提示')
     	CLOSE,/all
     	RETURN,0
	ENDIF

	CASE (*PSTATE).LAND_TYPE_RT OF
	   '耕地':BEGIN
	   		FILE_LANDUSE='data_grid\farm'
	   END
	   '水田':BEGIN
	   		FILE_LANDUSE='data_grid\farm_rice'
	   END
	   '旱地':BEGIN
	   		FILE_LANDUSE='data_grid\farm_drought'
	   END
	   ELSE:TEMP=DIALOG_MESSAGE('实时监测耕地类型有错误',TITLE='提示')
	ENDCASE
	PRINT,FILE_LANDUSE
	TEMP=1.0001
	FILE_LANDUSE_INFO=GET_IMAGE_INFO(FILE_LANDUSE)
	IF(	(FILE_IN_INFO.X_PIXELSIZE LT FILE_LANDUSE_INFO.X_PIXELSIZE/TEMP) OR	$
		(FILE_IN_INFO.X_PIXELSIZE GT FILE_LANDUSE_INFO.X_PIXELSIZE*TEMP)	$
			) THEN BEGIN
		PRINT,'输入数据的空间分辨率不一致!'
		RETURN,0
	ENDIF

	;在这里定义了进行实时监测图像显示所需要的参数
	FILE_RANGE	=	''	;用来进行范围参考的文件的栅格文件名
	RANGE_CODE	=	0	;用来进行范围参考的文件中相应的像素值
	X_MIN		=	0.0
	X_MAX		=	0.0
	Y_MIN		=	0.0
	Y_MAX		=	0.0

	SCALE_1		=1.01
	;这个变量用来对图像进行缩放,其目的是在图的周转留下一些边白
	;**************************************************************************************
	;在这里要获取四个边界和RASTER_VALUE
	;需要对原来从矢量上获取边界的代码进行修改

	;根据不同的尺度进行范围文件的选择
	CASE (*PSTATE).SCALE_RT OF
		'PROVINCE'	:BEGIN
			FILE_RANGE='data_grid\province_new'
			FILE_SHAPE='data_vector\province.shp'
			RANGE_CODE=(*PSTATE).RANGE_CODE_RT
		END
		'ROI'	:BEGIN
			;通过生成一个临时文件的方法来解决
			FILE_SHAPE='data_vector\county.shp'
			RANGE_CODE=(*PSTATE).RANGE_CODE_RT

			;**********************************************************************
			;在这里生成一个临时的ROI文件
			FILE_TEMP_1='data_grid\county_raster'
			FILE_TEMP_2='data_TEMP\ROI_TEMP'
			FILE_INFO_TEMP_1=GET_IMAGE_INFO(FILE_TEMP_1)
			LINES_1		=FILE_INFO_TEMP_1.YSIZE
			SAMPLES_1	=FILE_INFO_TEMP_1.XSIZE
			;=============================================================
			DATA_TEMP_1=BYTARR(SAMPLES_1,LINES_1)	;原代码
			;=============================================================

			;从数据库中读取县的栅格代码
			DATA_TEMP_1=(read_file(FILE_TEMP_1)).dataarr
			DATA_TEMP_1=BYTE(DATA_TEMP_1)
			SQL='select t2.raster_value from COUNTY_TO_ROI t1,county_code_raster t2 '
			SQL=SQL+'where (t1.county_code=t2.code) and (t1.roi_code='+"'"+RANGE_CODE+"')"
			PRINT,SQL

			ORS = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=Sql)
			IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
				COUNT=0
				REPEAT BEGIN
					DATA_TEMP_1(WHERE (DATA_TEMP_1 EQ ORS->GETFIELD(0)))=200 ;原代码
				ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
			ENDIF
			obj_destroy, ORS

			;生成一幅二值的ROI图像,用1表示有效区域,0表示无效区域
			DATA_TEMP_1(WHERE (DATA_TEMP_1 NE 200))=0
			DATA_TEMP_1(WHERE (DATA_TEMP_1 EQ 200))=1

			OPENW,Lun_FILE_TEMP_2	,FILE_TEMP_2	,/get_lun
			WRITEU,Lun_FILE_TEMP_2	 	,DATA_TEMP_1

			;=======杨绍锷添加，20070907=========================================
			HDR_INFO=GET_IMAGE_INFO(FILE_TEMP_1)
		    HDR_INFO.DATATYPE=1
		    WRITE_HDR_FILE,FILE_TEMP_2,HDR_INFO
			;====================================================================

			free_lun,Lun_FILE_TEMP_2
			DATA_TEMP_1=0

			FILE_RANGE='data_TEMP\ROI_TEMP'
			(*PSTATE).RASTER_VALUE=1
			;**********************************************************************

		END
		'COUNTY':BEGIN
			FILE_RANGE='data_grid\county_raster'
			FILE_SHAPE='data_vector\county.shp'
			RANGE_CODE=(*PSTATE).RANGE_CODE_RT
		END
		ELSE:
	ENDCASE

	PRINT,(*PSTATE).SCALE_RT

	;求边界的大小,要分两种情况,
	;(1)省和县,只涉及到一个多边形,
	;(2)自定义监测区,包括多个多边形
	IF ((*PSTATE).SCALE_RT NE 'ROI') THEN BEGIN
		MYSHAPE = OBJ_NEW('IDLFFSHAPE' ,FILE_SHAPE)
		MYSHAPE -> IDLFFSHAPE::GETPROPERTY, N_ENTITIES=NUM_ENT
		FOR X=0, (NUM_ENT-1) DO BEGIN
		   	ENT = MYSHAPE -> IDLFFSHAPE::GETENTITY(X)
		   	ATTR = MYSHAPE->GETATTRIBUTES(X)
		   	PRINT,'RANGE_CODE',RANGE_CODE
;		   	ATTR.ATTRIBUTE_0=strtrim(360000,2)		;杨绍锷临时添加
		   	PRINT,'ATTR.ATTRIBUTE_0',ATTR.ATTRIBUTE_0
		   	IF(ATTR.ATTRIBUTE_0 EQ RANGE_CODE) THEN BEGIN

		   		X_MIN	=ENT.BOUNDS[0]
		   		X_MAX	=ENT.BOUNDS[4]
		   		Y_MIN	=ENT.BOUNDS[1]
		   		Y_MAX	=ENT.BOUNDS[5]
		   	ENDIF
		   	MYSHAPE->DestroyEntity, ENT
		ENDFOR
		obj_destroy,MYSHAPE
	ENDIF ELSE BEGIN	;处理自定义监测区
		;把所属县的代码读入一个数组
		SQL='SELECT COUNTY_CODE FROM COUNTY_TO_ROI WHERE ROI_CODE='+"'"+RANGE_CODE+"'"
		PRINT,SQL
		;获取数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		PRINT,RecordNum
		Obj_Destroy,RecordNumOBJ

		;定义一个数组,并将自定义监测区中的县的代码读进去
		ARR_COUNTY_CODE=STRARR(RecordNum)
		ORS = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=Sql)
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			COUNT=0
			REPEAT BEGIN
				ARR_COUNTY_CODE[COUNT]		=ORS->GETFIELD(0)
				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF
		obj_destroy, ORS
		PRINT,ARR_COUNTY_CODE

		;用遍历的算法通过每个县的边界来找出自定义监测区的边界
		TEMP_FLAG=0
		MYSHAPE = OBJ_NEW('IDLFFSHAPE' ,FILE_SHAPE)
		MYSHAPE -> IDLFFSHAPE::GETPROPERTY, N_ENTITIES=NUM_ENT
		FOR X=0, (NUM_ENT-1) DO BEGIN
		   	ENT = MYSHAPE -> IDLFFSHAPE::GETENTITY(X)
		   	ATTR = MYSHAPE->GETATTRIBUTES(X)
		   	PRINT,'RANGE_CODE',RANGE_CODE
		   	PRINT,'ATTR.ATTRIBUTE_0',ATTR.ATTRIBUTE_0
			FOR Y=0,RecordNum-1 DO BEGIN
			   	IF(ATTR.ATTRIBUTE_0 EQ ARR_COUNTY_CODE[Y]) THEN BEGIN

			   		IF(TEMP_FLAG EQ 0)THEN BEGIN
				   		X_MIN	=ENT.BOUNDS[0]
				   		X_MAX	=ENT.BOUNDS[4]
				   		Y_MIN	=ENT.BOUNDS[1]
				   		Y_MAX	=ENT.BOUNDS[5]

				   		TEMP_FLAG=1
			   		ENDIF ELSE BEGIN
			   			X_MIN=X_MIN<ENT.BOUNDS[0]
			   			X_MAX=X_MAX>ENT.BOUNDS[4]
			   			Y_MIN=Y_MIN<ENT.BOUNDS[1]
			   			Y_MAX=Y_MAX>ENT.BOUNDS[5]
			   		ENDELSE

			   	ENDIF
			ENDFOR
			MYSHAPE->DestroyEntity, ENT
		ENDFOR
		OBJ_DESTROY, MYSHAPE
	ENDELSE


	PRINT,X_MIN,X_MAX
	PRINT,Y_MIN,Y_MAX


	PRINT,FILE_RANGE
	PRINT,FILE_SHAPE
	PRINT,RANGE_CODE
	PRINT,X_MIN,X_MAX,Y_MIN,Y_MAX
	;**************************************************************************************

	PRINT,FILE_RANGE

	PRINT,RANGE_CODE
	PRINT,X_MIN,X_MAX,Y_MIN,Y_MAX


	FILE_RANGE_INFO=GET_IMAGE_INFO(FILE_RANGE)
	;**************************************************************************************
	;进行有效数据范围的确定
	X_START_READ=(FILE_IN_INFO.STARTX>FILE_RANGE_INFO.STARTX)>FILE_LANDUSE_INFO.STARTX	$
		>X_MIN
	X_END_READ	=(FILE_IN_INFO.STARTX+FILE_IN_INFO.XSIZE*FILE_IN_INFO.X_PIXELSIZE)	$
		<(FILE_RANGE_INFO.STARTX+FILE_RANGE_INFO.XSIZE*FILE_RANGE_INFO.X_PIXELSIZE) $
		<(FILE_LANDUSE_INFO.STARTX+FILE_LANDUSE_INFO.XSIZE*FILE_LANDUSE_INFO.X_PIXELSIZE)	$
		<X_MAX

	Y_START_READ=(FILE_IN_INFO.STARTY<FILE_RANGE_INFO.STARTY)<FILE_LANDUSE_INFO.STARTY	$
		<Y_MAX
	;经纬度的坐标由下到上是越来越大,所以纵向和横向所用的符号相反
	Y_END_READ	=(FILE_IN_INFO.STARTY-FILE_IN_INFO.YSIZE*FILE_IN_INFO.Y_PIXELSIZE)	$
		>(FILE_RANGE_INFO.STARTY-FILE_RANGE_INFO.YSIZE*FILE_RANGE_INFO.Y_PIXELSIZE) $
		>(FILE_LANDUSE_INFO.STARTY-FILE_LANDUSE_INFO.YSIZE*FILE_LANDUSE_INFO.Y_PIXELSIZE)	$
		>Y_MIN

	;对图像的范围进行判断
	IF(X_START_READ GE X_END_READ)THEN BEGIN
		PRINT,'监测区域与输入数据没有重叠的部分'
		STR_TEMP='监测区域与输入数据没有重叠的部分'
		TEMP=DIALOG_MESSAGE(STR_TEMP,TITLE='数据输入有误')
		RETURN,0
	ENDIF
	IF(Y_START_READ LE Y_END_READ)THEN BEGIN
		PRINT,'监测区域与输入数据没有重叠的部分'
		STR_TEMP='监测区域与输入数据没有重叠的部分'
		TEMP=DIALOG_MESSAGE(STR_TEMP,TITLE='数据输入有误')
		RETURN,0
	ENDIF

	;计算公共数据的行数和列数
	SAMPLES	=LONG(ABS(X_END_READ-X_START_READ)/(FILE_IN_INFO.X_PIXELSIZE>FILE_RANGE_INFO.X_PIXELSIZE>FILE_LANDUSE_INFO.X_PIXELSIZE))
	LINES	=LONG(ABS(Y_END_READ-Y_START_READ)/(FILE_IN_INFO.Y_PIXELSIZE>FILE_RANGE_INFO.Y_PIXELSIZE>FILE_LANDUSE_INFO.Y_PIXELSIZE))
	HELP,SAMPLES
	HELP,LINES

	;计算每个数据读取的起始行数和列数
	SAMPLE_START_FILEIN=LONG(ABS(X_START_READ-FILE_IN_INFO.STARTX)/FILE_IN_INFO.X_PIXELSIZE+0.5)
	LINE_START_FILEIN=LONG(ABS(Y_START_READ-FILE_IN_INFO.STARTY)/FILE_IN_INFO.Y_PIXELSIZE+0.5)

	SAMPLE_START_RANGE=LONG(ABS(X_START_READ-FILE_RANGE_INFO.STARTX)/FILE_RANGE_INFO.X_PIXELSIZE+0.5)
	LINE_START_RANGE=LONG(ABS(Y_START_READ-FILE_RANGE_INFO.STARTY)/FILE_RANGE_INFO.Y_PIXELSIZE+0.5)

	SAMPLE_START_LANDUSE=LONG(ABS(X_START_READ-FILE_LANDUSE_INFO.STARTX)/FILE_LANDUSE_INFO.X_PIXELSIZE+0.5)
	LINE_START_LANDUSE=LONG(ABS(Y_START_READ-FILE_LANDUSE_INFO.STARTY)/FILE_LANDUSE_INFO.Y_PIXELSIZE+0.5)
	;***************************************************************************************************

	;***************************************************************************************************
	;计算最后显示的数据的大小
	;从输入的界面中获取输出图像的大小
	WIDGET_CONTROL,(*PSTATE).TXT_XSIZE_RT_JPG,GET_VALUE=X_SIZE_INPUT
	X_SIZE_INPUT=FLOAT(X_SIZE_INPUT[0])
	WIDGET_CONTROL,(*PSTATE).TXT_YSIZE_RT_JPG,GET_VALUE=Y_SIZE_INPUT
	Y_SIZE_INPUT=FLOAT(Y_SIZE_INPUT[0])

	IF (X_SIZE_INPUT GT 2000) or (Y_SIZE_INPUT GT 2000) or (X_SIZE_INPUT LT 400) or (Y_SIZE_INPUT LT 400) THEN BEGIN

		X_SIZE_INPUT=400
		Y_SIZE_INPUT=400

		TEMP=DIALOG_MESSAGE('实时监测图片尺寸输入范围为400-2000!',TITLE='提示')

		WIDGET_CONTROL,(*PSTATE).TXT_XSIZE_RT_JPG,SET_VALUE=strtrim(string(X_SIZE_INPUT),2)
		WIDGET_CONTROL,(*PSTATE).TXT_YSIZE_RT_JPG,SET_VALUE=strtrim(string(Y_SIZE_INPUT),2)

		return,0
	ENDIF

	X_TEMP=X_SIZE_INPUT
	Y_TEMP=Y_SIZE_INPUT
	WIDTH=ABS(X_END_READ-X_START_READ)
	HEIGHT=ABS(Y_START_READ-Y_END_READ)


	;限制最大的那个边,然后确定另外一个边.
	IF((FLOAT(SAMPLES)/FLOAT(LINES)) GT X_TEMP/Y_TEMP) THEN BEGIN
		Y_TEMP=X_TEMP/(FLOAT(SAMPLES)/FLOAT(LINES))
	ENDIF ELSE BEGIN
		X_TEMP=Y_TEMP*(FLOAT(SAMPLES)/FLOAT(LINES))
	ENDELSE

	;动态的确定方法
	Y_TEMP=SQRT(Y_SIZE_INPUT*X_SIZE_INPUT*HEIGHT/WIDTH)
	X_TEMP=Y_SIZE_INPUT*X_SIZE_INPUT/Y_TEMP
	PRINT,X_TEMP
	PRINT,Y_TEMP

	X_TEMP_1=FIX(X_TEMP/SCALE_1)	;这两个变量用来进行真实数据的CONGRID大小,
	Y_TEMP_1=FIX(Y_TEMP/SCALE_1)	;它要比真实显示的要小,这里确定了一个系数为1.01

	X_SIZE=X_TEMP
	Y_SIZE=Y_TEMP

	;***************************************************************************************************
	;进行数据的读取,读取后的数据直接进行了小范围的CONGRID
	;READ_IMAGE_PART,FILE_IN,ROW_START,COL_START,LINES,SAMPLES

	;(1)读取分级文件
	DATA_FILE=READ_IMAGE_PART(FILE_IN,LINE_START_FILEIN,SAMPLE_START_FILEIN	$
									,LINES,SAMPLES)
	DATA_TEMP=CONGRID(*(DATA_FILE.P_DATA),X_TEMP_1,Y_TEMP_1)

	DATA_ARR_FILE=BYTARR(X_TEMP,Y_TEMP)
	DATA_ARR_FILE[FIX(X_TEMP*(SCALE_1-1)/2):FIX(X_TEMP*(SCALE_1-1)/2)+X_TEMP_1-1,FIX(Y_TEMP*(SCALE_1-1)/2):FIX(Y_TEMP*(SCALE_1-1)/2)+Y_TEMP_1-1]	=	$
			DATA_TEMP[*,*]

	ptr_free, DATA_FILE.P_DATA ;*(DATA_FILE.P_DATA)=0	;把内存释放了
	PRINT,'BBBBB'
	;(2)读取土地利用文件
	DATA_FILE=READ_IMAGE_PART(FILE_LANDUSE,LINE_START_LANDUSE,SAMPLE_START_LANDUSE	$
									,LINES,SAMPLES)
	DATA_TEMP=CONGRID(*(DATA_FILE.P_DATA),X_TEMP_1,Y_TEMP_1)
	DATA_ARR_LANDUSE=BYTARR(X_TEMP,Y_TEMP)
	DATA_ARR_LANDUSE[FIX(X_TEMP*(SCALE_1-1)/2):FIX(X_TEMP*(SCALE_1-1)/2)+X_TEMP_1-1,FIX(Y_TEMP*(SCALE_1-1)/2):FIX(Y_TEMP*(SCALE_1-1)/2)+Y_TEMP_1-1]	=	$
			DATA_TEMP[*,*]
	ptr_free, DATA_FILE.P_DATA ;*(DATA_FILE.P_DATA)=0	;把内存释放了

	;(3)读取范围文件
	DATA_FILE=READ_IMAGE_PART(FILE_RANGE,LINE_START_RANGE,SAMPLE_START_RANGE	$
									,LINES,SAMPLES)
	DATA_TEMP=CONGRID(*(DATA_FILE.P_DATA),X_TEMP_1,Y_TEMP_1)
	DATA_ARR_RANGE=BYTARR(X_TEMP,Y_TEMP)
	DATA_ARR_RANGE[FIX(X_TEMP*(SCALE_1-1)/2):FIX(X_TEMP*(SCALE_1-1)/2)+X_TEMP_1-1,FIX(Y_TEMP*(SCALE_1-1)/2):FIX(Y_TEMP*(SCALE_1-1)/2)+Y_TEMP_1-1]	=	$
			DATA_TEMP[*,*]
	ptr_free, DATA_FILE.P_DATA ;*(DATA_FILE.P_DATA)=0	;把内存释放了
	;RETURN,1

	;******************************************************************************
	;开始对数据进行图像化的处理和显示

	;为了利用原来的代码,在这里对原来的变量进行了转换
	DATA_IMAGE		=	DATA_ARR_FILE
	DATA_ARR_FILE	=	0
	DATA_ROI		=	DATA_ARR_RANGE
	DATA_ARR_RANGE	=	0
	VALUE_ROI		=	(*PSTATE).RASTER_VALUE



	;这个数据用来画省的边界

	DATA_ROI_1=DATA_ROI

	;为了把区域内的0和区域外的0分开,给省内的所有象元加1
	DATA_IMAGE=DATA_IMAGE+BYTE(1)
	FOR I=0,X_SIZE-1 DO BEGIN
		FOR J=0,Y_SIZE-1 DO BEGIN
			IF(DATA_ROI[I,J] NE VALUE_ROI) THEN BEGIN
				DATA_IMAGE[I,J]=0
 				DATA_ROI_1[I,J]=0
			ENDIF ELSE BEGIN
				DATA_ROI_1[I,J]=1
				;如果这个象元是非耕地象元,就给1
				IF(DATA_ARR_LANDUSE[I,J] NE 1 ) THEN DATA_IMAGE[I,J]=1
			ENDELSE
		ENDFOR
	ENDFOR

	TEMP=(*PSTATE).RANGE_RT
	PRINT,(*PSTATE).SCALE_RT
	PRINT,TEMP
	IF	((*PSTATE).SCALE_RT EQ 'COUNTRY') AND ((TEMP EQ 'AUS') OR(TEMP EQ 'USA') OR	$
			(TEMP EQ 'IND') OR(TEMP EQ 'CAN') OR(TEMP EQ 'RUS') OR(TEMP EQ 'BRAZ')) THEN BEGIN
		PRINT,'不进行图的上下左右移动'
	ENDIF ELSE BEGIN

		DATA_ROI_2=DATA_ROI_1
		;1左移
		FOR I=0,X_SIZE-2 DO BEGIN
			FOR J=0,Y_SIZE-1 DO BEGIN
				IF (DATA_ROI_1[I+1,J] EQ 1 ) THEN DATA_ROI_2[I,J]=1
			ENDFOR
		ENDFOR
		;2右移
		FOR I=1,X_SIZE-1 DO BEGIN
			FOR J=0,Y_SIZE-1 DO BEGIN
				IF (DATA_ROI_1[I-1,J] EQ 1 ) THEN DATA_ROI_2[I,J]=1
			ENDFOR
		ENDFOR
		;3上移
		FOR I=0,X_SIZE-1 DO BEGIN
			FOR J=0,Y_SIZE-2 DO BEGIN
				IF (DATA_ROI_1[I,J+1] EQ 1 ) THEN DATA_ROI_2[I,J]=1
			ENDFOR
		ENDFOR

		;4下移
		FOR I=0,X_SIZE-1 DO BEGIN
			FOR J=1,Y_SIZE-1 DO BEGIN
				IF (DATA_ROI_1[I,J-1] EQ 1 ) THEN DATA_ROI_2[I,J]=1
			ENDFOR
		ENDFOR

		;5去中心部分
		FOR I=0,X_SIZE-1 DO BEGIN
			FOR J=0,Y_SIZE-1 DO BEGIN
				IF ((DATA_ROI_1[I,J] EQ 0) AND(DATA_ROI_2[I,J] EQ 1) ) THEN DATA_IMAGE[I,J]=12
			ENDFOR
		ENDFOR

		;释放前面使用过的一些变量
		DATA_ROI=0
		DATA_FARM=0
	ENDELSE


	;定义一个三维的数组进行图像的最后显示

	RGB_ARR=BYTARR(X_SIZE,Y_SIZE,3)
	FOR I=0,X_SIZE-1 DO BEGIN
		FOR J=0,Y_SIZE-1 DO BEGIN
			CASE DATA_IMAGE[I,J] OF
				;省内的NAN
				;这里使用的数据比分级模块出来的结果要大1,
				;这是因为为了要区分省内的数据和省外的数据,已经给所有省内的数据加了1
				1: BEGIN
   					RGB_ARR[I,J,0]=BYTE(255)
   					RGB_ARR[I,J,1]=BYTE(255)
   					RGB_ARR[I,J,2]=BYTE(255)
   					END
   				2: BEGIN
   					RGB_ARR[I,J,0]=BYTE(10)
   					RGB_ARR[I,J,1]=BYTE(0)
   					RGB_ARR[I,J,2]=BYTE(128)
   					END
   				3: BEGIN
   					RGB_ARR[I,J,0]=BYTE(0)
   					RGB_ARR[I,J,1]=BYTE(0)
   					RGB_ARR[I,J,2]=BYTE(255)
   					END
   				4: BEGIN
   					RGB_ARR[I,J,0]=BYTE(0)
   					RGB_ARR[I,J,1]=BYTE(255)
   					RGB_ARR[I,J,2]=BYTE(255)
   					END
   				5: BEGIN
   					RGB_ARR[I,J,0]=BYTE(0)
   					RGB_ARR[I,J,1]=BYTE(250)
   					RGB_ARR[I,J,2]=BYTE(157)
   					END
   				6: BEGIN
   					RGB_ARR[I,J,0]=BYTE(0)
   					RGB_ARR[I,J,1]=BYTE(255)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   				7: BEGIN
   					RGB_ARR[I,J,0]=BYTE(181)
   					RGB_ARR[I,J,1]=BYTE(255)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   				8: BEGIN
   					RGB_ARR[I,J,0]=BYTE(219)
   					RGB_ARR[I,J,1]=BYTE(201)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   				9: BEGIN
   					RGB_ARR[I,J,0]=BYTE(255)
   					RGB_ARR[I,J,1]=BYTE(173)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   				10: BEGIN
   					RGB_ARR[I,J,0]=BYTE(255)
   					RGB_ARR[I,J,1]=BYTE(0)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   				;云
   				11: BEGIN
   					RGB_ARR[I,J,0]=BYTE(100)
   					RGB_ARR[I,J,1]=BYTE(100)
   					RGB_ARR[I,J,2]=BYTE(100)
   					END
   				;边界
   				12: BEGIN
   					RGB_ARR[I,J,0]=BYTE(0)
   					RGB_ARR[I,J,1]=BYTE(0)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   				;背景
   				0: BEGIN
   					RGB_ARR[I,J,0]=BYTE(255)
   					RGB_ARR[I,J,1]=BYTE(255)
   					RGB_ARR[I,J,2]=BYTE(255)
   					END
   				ELSE:BEGIN
   					RGB_ARR[I,J,0]=BYTE(255)
   					RGB_ARR[I,J,1]=BYTE(0)
   					RGB_ARR[I,J,2]=BYTE(0)
   					END
   			ENDCASE
		ENDFOR
	ENDFOR
	;把前面使用的数组释放了.
	DATA_IMAGE=0

	;再画数据
	white=!D.N_COLORS-1

	;进行矢量图形的显示
	TEMP=(*PSTATE).RANGE_RT

	IF	((*PSTATE).SCALE_RT EQ 'COUNTRY') AND ((TEMP EQ 'AUS') OR(TEMP EQ 'USA') OR	$
			(TEMP EQ 'IND') OR(TEMP EQ 'CAN') OR(TEMP EQ 'RUS') OR(TEMP EQ 'BRAZ')) THEN BEGIN


		!Y.STYLE = 1
		!X.STYLE = 1
		;以上两句话用来精确的定义坐标,
		;否则IDL的PLOT在成图时会自动的对坐标取整,
		;这扩大了坐标的范围

		SHAPE_LINE = '基础数据\各国矢量数据\'+TEMP+'_LINE.SHP'
		SHAPE_POINT= '基础数据\各国矢量数据\'+TEMP+'_POINT.SHP'

		window,2,xsize=X_SIZE,ysize = Y_SIZE
		WSET,2
		ERASE
		X_MIN=X_START_READ-(X_END_READ-X_START_READ)*(SCALE_1-1)/2
		X_MAX=X_END_READ+(X_END_READ-X_START_READ)*(SCALE_1-1)/2
		Y_MIN=Y_END_READ-(Y_START_READ-Y_END_READ)*(SCALE_1-1)/2
		Y_MAX=Y_START_READ+(Y_START_READ-Y_END_READ)*(SCALE_1-1)/2
		PLOT,[X_MIN,X_MAX],[Y_MIN,Y_MAX] ,MAX_VALUE=Y_MAX, MIN_VALUE=Y_MIN	$
				,XMARGIN=[0,0],YMARGIN=[0,0]	$
				,XRANGE=[X_MIN,X_MAX],YRANGE=[Y_MIN,Y_MAX]

		TV,RGB_ARR,/ORDER, TRUE=3

		;*******************************************************
		;开始矢量数据的显示,主要包括两个部分的内容
		;(1) 省界的显示
		;(2) 省会名称的显示

		;-------------省界的显示-----------------------------
		;(1) 省界的显示
		SHAPE_LINE=SHAPE_LINE
		MYSHAPE=OBJ_NEW('IDLFFSHAPE', SHAPE_LINE)
        MYSHAPE -> IDLFFSHAPE::GETPROPERTY, N_ENTITIES=NUM_ENT
        MYSHAPE -> IDLFFSHAPE::GETPROPERTY,ENTITY_TYPE  = TYPE
       	IF TYPE EQ 5 OR TYPE EQ 3 THEN BEGIN
         	FOR I=0,NUM_ENT-1 DO BEGIN
	            ATTR = MYSHAPE -> IDLFFSHAPE::GETATTRIBUTES(I)
	            ENT = MYSHAPE -> IDLFFSHAPE::GETENTITY(I,/ATTRIBUTES )
	            NUMPOINTS = ENT.N_VERTICES-1
	            X = (*ENT.VERTICES)[0,1:NUMPOINTS-1]
	            Y = (*ENT.VERTICES)[1,1:NUMPOINTS-1]
				;把多边形画在图上
				WSET,2
				OPLOT,X,Y,COLOR=0;255+256L*(255L+255*256L),黑色
	            MYSHAPE -> IDLFFSHAPE::DESTROYENTITY, ENT
         	ENDFOR
       ENDIF
       OBJ_DESTROY, MYSHAPE
       ;-------------省界的显示-----------------------------

       ;-------------省会名称的显示-----------------------------
       ;(2) 省会名称的显示
       X_OFFSET=0.3
       SHAPE_POINT= SHAPE_POINT
	   MYSHAPE=OBJ_NEW('IDLFFSHAPE', SHAPE_POINT)
	   MYSHAPE -> IDLFFSHAPE::GETPROPERTY, N_ENTITIES=NUM_ENT
	   MYSHAPE -> IDLFFSHAPE::GETPROPERTY,ATTRIBUTE_INFO = INFOR
	   MYSHAPE -> IDLFFSHAPE::GETPROPERTY,ATTRIBUTE_NAMES  = NAMES
	   MYSHAPE -> IDLFFSHAPE::GETPROPERTY,ENTITY_TYPE  = TYPE

	   WSET,2
	   FONTNAME=['SIMSUN','宋体','隶书', 'SIMHEI','黑体']
	   DEVICE, SET_FONT=FONTNAME[4], /TT_FONT ;

	   SYMBELLAYER = OBJ_NEW('IDLGRMODEL', NAME='SYMBELLAYER')
	   IF TYPE EQ 1 THEN BEGIN

	       IF NUM_ENT LE 1 THEN RETURN,0
		       COORDINATES = FLTARR(2,NUM_ENT)
		       ANNOTATIONS = STRARR(NUM_ENT)
	       FOR I=0,NUM_ENT-1 DO BEGIN
	            ATTR = MYSHAPE -> IDLFFSHAPE::GETATTRIBUTES(I)
	            ENT = MYSHAPE -> IDLFFSHAPE::GETENTITY(I)

	            X = ENT.BOUNDS[0]
	            Y = ENT.BOUNDS[1]

	            ANNOTATIONS[I] = ATTR.ATTRIBUTE_0
	            ;PRINT,X,Y,ATTR.ATTRIBUTE_0
	            XYOUTS,X-X_OFFSET,Y,ATTR.ATTRIBUTE_0,COLOR=0,FONT=1	$
	            	,CHARSIZE=(X_SIZE_INPUT)^0.7*0.02,CHARTHICK=1
	            	PRINT,X_SIZE_INPUT
	        ENDFOR

	   	ENDIF
	   	OBJ_DESTROY, MYSHAPE
       	;-------------省会名称的显示-----------------------------
		;*******************************************************

		GEOMETRY = WIDGET_INFO((*pstate).DRAW_RT_JPG, /GEOMETRY)
		X_TEMP=GEOMETRY.SCR_XSIZE
		Y_TEMP=GEOMETRY.SCR_YSIZE
		IF((FLOAT(X_SIZE)/FLOAT(Y_SIZE)) GT X_TEMP/Y_TEMP) THEN BEGIN
			Y_TEMP=X_TEMP/(FLOAT(X_SIZE)/FLOAT(Y_SIZE))
		ENDIF ELSE BEGIN
			X_TEMP=Y_TEMP*(FLOAT(X_SIZE)/FLOAT(Y_SIZE))
		ENDELSE

		IMAGE = TVRD(true=3,/order)
		IMAGE_SHOW = CONGRID(IMAGE,X_TEMP,Y_TEMP,3)
		WIDGET_CONTROL,(*pstate).DRAW_RT_JPG,get_value=DRAW_RT_JPG
		WSET,DRAW_RT_JPG
		ERASE
		TEMP=INDGEN(2)
  		PLOT,TEMP,BACKGROUND=WHITE
  		;BBBB
		TV,IMAGE_SHOW,/ORDER, TRUE=3

		;RGB_ARR,要把这个数据传出去进行JPG文件的写出
		;以一个指针的形式将变量传出去
		(*PSTATE).P_IMAGE_RT	=PTR_NEW(IMAGE)

		(*pstate).SHOW_RT_DONE=1
		PRINT,(*pstate).SHOW_RT_DONE
		WIDGET_CONTROL,(*pstate).CMD_OUTPUT_JPG,SENSITIVE=1

	ENDIF ELSE BEGIN
	   	WINDOW,2,XSIZE=X_SIZE,YSIZE = Y_SIZE,TITLE='实时监测结果';, /PIXMAP
		;加上这个关键字就可以把窗口进行隐藏, /PIXMAP
		WSET,2
		ERASE

		TV,RGB_ARR,/ORDER, TRUE=3

		;************************************************************************************
		;在这里进行缩略图的显示
		;DRAW_RT_JPG
		;SCR_XSIZE=200 ,SCR_YSIZE=189
		GEOMETRY = WIDGET_INFO((*pstate).DRAW_RT_JPG, /GEOMETRY)
		X_THUMB=GEOMETRY.SCR_XSIZE
		Y_THUMB=GEOMETRY.SCR_YSIZE
		IF((FLOAT(X_SIZE)/FLOAT(Y_SIZE)) GT X_THUMB/Y_THUMB) THEN BEGIN
			Y_TEMP=X_THUMB/(FLOAT(X_SIZE)/FLOAT(Y_SIZE))
			X_TEMP=X_THUMB
		ENDIF ELSE BEGIN
			X_TEMP=Y_THUMB*(FLOAT(X_SIZE)/FLOAT(Y_SIZE))
			Y_TEMP=Y_THUMB
		ENDELSE
		x_offset = (X_THUMB - X_TEMP)/2.0
		y_offset = (Y_THUMB - Y_TEMP)/2.0

		;IMAGE = TVRD(true=3,/order)
		IMAGE_SHOW = CONGRID(RGB_ARR,X_TEMP,Y_TEMP,3,/center)
		WIDGET_CONTROL,(*pstate).DRAW_RT_JPG,get_value=DRAW_RT_JPG
		WSET,DRAW_RT_JPG
		ERASE
		TEMP=INDGEN(2)
  		PLOT,TEMP,BACKGROUND=WHITE
		TV, IMAGE_SHOW, x_offset, y_offset, /ORDER, TRUE=3
		IMAGE_SHOW=0

		;RGB_ARR,要把这个数据传出去进行JPG文件的写出
		;以一个指针的形式将变量传出去
		(*PSTATE).P_IMAGE_RT	=PTR_NEW(RGB_ARR)
		;************************************************************************************
		(*pstate).SHOW_RT_DONE=1
		PRINT,(*pstate).SHOW_RT_DONE
		WIDGET_CONTROL,(*pstate).CMD_OUTPUT_JPG,SENSITIVE=1
	ENDELSE
	RETURN,1
END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
FUNCTION ZS_SHOW_JPG_PRO, PSTATE
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	if PTR_VALID((*PSTATE).P_IMAGE_PRO) then ptr_free,(*PSTATE).P_IMAGE_PRO

	PRINT,'进行了过程监测图像的显示'
	;1)获取界面的参数
	YEAR_START_LY_PRO	=(*PSTATE).YEAR_START_LY_PRO
	MONTH_START_LY_PRO	=(*PSTATE).MONTH_START_LY_PRO
	day_START_LY_PRO	=(*PSTATE).day_START_LY_PRO

	YEAR_END_LY_PRO		=(*PSTATE).YEAR_END_LY_PRO
	MONTH_END_LY_PRO	=(*PSTATE).MONTH_END_LY_PRO
	day_END_LY_PRO		=(*PSTATE).day_END_LY_PRO

	YEAR_START_TY_PRO	=(*PSTATE).YEAR_START_TY_PRO
	MONTH_START_TY_PRO	=(*PSTATE).MONTH_START_TY_PRO
	day_START_TY_PRO	=(*PSTATE).day_START_TY_PRO

	YEAR_END_TY_PRO		=(*PSTATE).YEAR_END_TY_PRO
	MONTH_END_TY_PRO	=(*PSTATE).MONTH_END_TY_PRO
	day_END_TY_PRO		=(*PSTATE).day_END_TY_PRO

    WIDGET_CONTROL,(*PSTATE).TXT_XSIZE_PRO_JPG,GET_VALUE=XSIZE_PRO_JPG
    XSIZE_PRO_JPG=FIX(XSIZE_PRO_JPG)
    WIDGET_CONTROL,(*PSTATE).TXT_YSIZE_PRO_JPG,GET_VALUE=YSIZE_PRO_JPG
    YSIZE_PRO_JPG=FIX(YSIZE_PRO_JPG)

	;***********************************************************************
	;这里是个额外的要求,两年曲线的起始时间相同
	IF((MONTH_START_LY_PRO NE MONTH_START_TY_PRO) OR day_START_LY_PRO NE day_START_TY_PRO) THEN BEGIN
		TEMP=DIALOG_MESSAGE('两年数据的起始时间要保持一致',TITLE='提示')
		RETURN,0
	ENDIF
	;这里是个额外的要求,两年曲线的结束时间相同
	IF((MONTH_END_LY_PRO NE MONTH_END_TY_PRO) OR day_END_LY_PRO NE day_END_TY_PRO) THEN BEGIN
		TEMP=DIALOG_MESSAGE('两年数据的起始时间要保持一致',TITLE='提示')
		RETURN,0
	ENDIF
	;***********************************************************************
	;


    SCALE_PRO			=	(*PSTATE).SCALE_PRO
    RANGE_PRO			=	(*PSTATE).RANGE_PRO
	RANGE_CODE_PRO		=	(*PSTATE).RANGE_CODE_PRO

	DRAW_PRO_JPG		=	(*PSTATE).DRAW_PRO_JPG
	OD					=	DBobj

	;构造用于进行数据查询的变量
	day_START_LY	=LONG(YEAR_START_LY_PRO-1980)*500+MONTH_START_LY_PRO*40+day_START_LY_PRO
	day_END_LY	=LONG(YEAR_END_LY_PRO-1980)*500+MONTH_END_LY_PRO*40+day_END_LY_PRO
	day_START_TY	=LONG(YEAR_START_TY_PRO-1980)*500+MONTH_START_TY_PRO*40+day_START_TY_PRO
	day_END_TY	=LONG(YEAR_END_TY_PRO-1980)*500+MONTH_END_TY_PRO*40+day_END_TY_PRO



	PRINT,YEAR_START_LY_PRO,MONTH_START_LY_PRO,day_START_LY_PRO
	PRINT,YEAR_END_LY_PRO,MONTH_END_LY_PRO,day_END_LY_PRO
	PRINT,YEAR_START_TY_PRO,MONTH_START_TY_PRO,day_START_TY_PRO
	PRINT,YEAR_END_TY_PRO,MONTH_END_TY_PRO,day_END_TY_PRO

	PRINT,'SCALE_PRO',SCALE_PRO
	PRINT,'RANGE_PRO',RANGE_PRO
	PRINT,'RANGE_CODE_PRO',RANGE_CODE_PRO

	PRINT,'XSIZE_PRO_JPG',XSIZE_PRO_JPG
	PRINT,'YSIZE_PRO_JPG',YSIZE_PRO_JPG



	IF(SCALE_PRO EQ '') THEN BEGIN
		TEMP=DIALOG_MESSAGE('请选择过程监测的尺度',TITLE='提示')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF

	IF(RANGE_PRO EQ '请选择') THEN BEGIN
		TEMP=DIALOG_MESSAGE('请选择一个范围',TITLE='提示')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF

	IF((day_START_LY GE day_END_LY) OR (day_START_TY GE day_END_TY)) THEN BEGIN
		TEMP=DIALOG_MESSAGE('时间不合理,请重新选择时间',TITLE='提示')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF

	;2)读取数据
	;构造分区域进行表名变化的变量
	CASE SCALE_PRO OF
		'PROVINCE'	:BEGIN
			TABLE_NAME='PARAMETER_PROCESS_PROVINCE'
			CODE_NAME='PROVINCE_CODE'
			CODE=RANGE_CODE_PRO
			PLACE_PRO=RANGE_PRO
		END
		'ROI'	:BEGIN
			TABLE_NAME='PARAMETER_PROCESS_ROI'
			CODE_NAME='ROI_CODE'
			CODE=RANGE_CODE_PRO
			PLACE_PRO=RANGE_PRO
		END
		'COUNTY'	:BEGIN
			TABLE_NAME='PARAMETER_PROCESS_COUNTY'
			CODE_NAME='COUNTY_CODE'
			CODE=RANGE_CODE_PRO
			PLACE_PRO=RANGE_PRO
		END
		ELSE:TEMP=DIALOG_MESSAGE('尺度错误',TITLE='提示')
	ENDCASE

	;构造分区域进行表名变化的变量
	CASE (*PSTATE).LAND_TYPE_PRO OF
		'耕地'	:BEGIN
			STR_SQL_LANDTYPE='AVG_PLOWLAND'
		END
		'水田'	:BEGIN
			STR_SQL_LANDTYPE='AVG_PADDY_FIELD'
		END
		'旱地'	:BEGIN
			STR_SQL_LANDTYPE='AVG_DRY_LAND'
		END
		ELSE:TEMP=DIALOG_MESSAGE('尺度错误',TITLE='提示')
	ENDCASE

	;A)读取出去年的数据
	;下面的几个变量应该从程序中动态的去获取,
	;受时间的限制,在这里直接给变量赋值
;	PERIOD=10
;	DATA_TYPE='NDVI_H'
;	SENSOR_CODE='1'
	;*****************************************************************************
	;MODIFIED 2006.	11.23
	PERIOD=(*PSTATE).PERIOD
	DATA_TYPE=(*PSTATE).DATA_TYPE
	SENSOR_CODE=STRTRIM((*PSTATE).SENSOR_CODE,2)
	;*****************************************************************************

;===杨绍锷添加，20070905===========================================
	if SCALE_PRO eq 'COUNTY' then begin
		DATA_TYPE=DATA_TYPE+'_H'
	endif
;============================================================================

	SQL='SELECT '+STR_SQL_LANDTYPE+',MONTH,DAY'
	SQL=SQL+' FROM '+TABLE_NAME+' WHERE '
	SQL=SQL+' ('+CODE_NAME+' = '+"'"+CODE+"'"+')'
	SQL=SQL+' AND (((YEAR-1980)*500+MONTH*40+day) >='+STRTRIM(STRING(day_START_LY),2)+')'
	SQL=SQL+' AND (((YEAR-1980)*500+MONTH*40+day) <='+STRTRIM(STRING(day_END_LY),2)+')'
	SQL=SQL+' AND (DATA_TYPE = '+"'"+DATA_TYPE+"'"+')'
	SQL=SQL+' AND (SENSOR_CODE = '+"'"+SENSOR_CODE+"'"+')'
	SQL=SQL+' AND (PERIODS = '+STRTRIM(PERIOD,2)+')'
	SQL=SQL+' ORDER BY YEAR,MONTH,day'
	PRINT,SQL
	;定义读取数据的大小
	;这个变量要通过数据记录的长度来获取
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM=RecordNum
	Obj_Destroy,RecordNumOBJ

	NUM_LY	=NUM
	IF	(NUM_LY LE 0)	THEN BEGIN
		TEMP=DIALOG_MESSAGE('未找到去年的数据!',TITLE='提示')
		RETURN,0
	ENDIF
	ARR_DATA_LY=FLTARR(NUM_LY)
	ARR_MONTH_LY=FLTARR(NUM_LY)
	ARR_DAY_LY=FLTARR(NUM_LY)
	PRINT,NUM


	ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)

	COUNT_DB=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

		ARR_DATA_LY[COUNT_DB]=ORS -> GETFIELD(0)
		ARR_MONTH_LY[COUNT_DB]=ORS -> GETFIELD(1)
		ARR_DAY_LY[COUNT_DB]=ORS -> GETFIELD(2)

		WHILE (ORS->MOVECURSOR(/NEXT)) DO BEGIN
			COUNT_DB=COUNT_DB+1
			ARR_DATA_LY[COUNT_DB]=ORS -> GETFIELD(0)
			ARR_MONTH_LY[COUNT_DB]=ORS -> GETFIELD(1)
			ARR_DAY_LY[COUNT_DB]=ORS -> GETFIELD(2)
		ENDWHILE
	ENDIF
	OBJ_DESTROY,ORS
	PRINT,ARR_DATA_LY
	PRINT,ARR_MONTH_LY
	PRINT,ARR_DAY_LY


	COUNT_DB=COUNT_DB+1
	PRINT,COUNT_DB,ARR_DATA_LY
	IF(COUNT_DB EQ 0) THEN BEGIN
		TEMP=DIALOG_MESSAGE('去年所选时间内未找到数据',TITLE='提示')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF
	IF(COUNT_DB LT NUM_LY) THEN BEGIN
		TEMP=DIALOG_MESSAGE('去年所选时间内数据不全',TITLE='提示')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF

	;B)读取出今年的数据
	SQL='SELECT '+STR_SQL_LANDTYPE+',MONTH,DAY'
	SQL=SQL+' FROM '+TABLE_NAME+' WHERE '
	SQL=SQL+' ('+CODE_NAME+' = '+"'"+CODE+"'"+')'
	SQL=SQL+' AND (((YEAR-1980)*500+MONTH*40+day) >='+STRTRIM(STRING(day_START_TY),2)+')'
	SQL=SQL+' AND (((YEAR-1980)*500+MONTH*40+day) <='+STRTRIM(STRING(day_END_TY),2)+')'
	SQL=SQL+' AND (DATA_TYPE = '+"'"+DATA_TYPE+"'"+')'
	SQL=SQL+' AND (SENSOR_CODE = '+"'"+SENSOR_CODE+"'"+')'
	SQL=SQL+' AND (PERIODS = '+STRTRIM(PERIOD,2)+')'
	SQL=SQL+' ORDER BY YEAR,MONTH,day'

	;这个变量要通过数据记录的长度来获取
	RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	RecordNum = RecordNumOBJ->GETFIELD(0)
	NUM=RecordNum
	Obj_Destroy,RecordNumOBJ
	NUM_TY	=NUM
	IF	(NUM_TY LE 0)	THEN BEGIN
		TEMP=DIALOG_MESSAGE('未找到今年的数据!',TITLE='提示')
		RETURN,0
	ENDIF
	ARR_DATA_TY=FLTARR(NUM_TY)
	ARR_MONTH_TY=FLTARR(NUM_TY)
	ARR_DAY_TY=FLTARR(NUM_TY)


	ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)

	COUNT_DB=0
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

		ARR_DATA_TY[COUNT_DB]=ORS -> GETFIELD(0)
		ARR_MONTH_TY[COUNT_DB]=ORS -> GETFIELD(1)
		ARR_DAY_TY[COUNT_DB]=ORS -> GETFIELD(2)

		WHILE (ORS->MOVECURSOR(/NEXT)) DO BEGIN
			COUNT_DB=COUNT_DB+1
			ARR_DATA_TY[COUNT_DB]=ORS -> GETFIELD(0)
			ARR_MONTH_TY[COUNT_DB]=ORS -> GETFIELD(1)
			ARR_DAY_TY[COUNT_DB]=ORS -> GETFIELD(2)
		ENDWHILE
	ENDIF
	OBJ_DESTROY,ORS
	PRINT,ARR_DATA_LY
	PRINT,ARR_MONTH_LY
	PRINT,ARR_DAY_LY


	COUNT_DB=COUNT_DB+1
	PRINT,COUNT_DB,ARR_DATA_TY
	IF(COUNT_DB EQ 0) THEN BEGIN
		TEMP=DIALOG_MESSAGE('今年所选时间内未找到数据',TITLE='数据未找到')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF
	IF(COUNT_DB LT NUM_TY) THEN BEGIN
		TEMP=DIALOG_MESSAGE('今年所选时间内数据不全',TITLE='数据不全')
		CLOSE,/ALL
 		RETURN, 0 ; BY DEFAULT, RETURN THE EVENT.
	ENDIF


	;3)图像的显示
	;*****************************************************************************************

	N_LENGTH=NUM_LY>NUM_TY
	PRINT,'数据长度',N_LENGTH

;======杨绍锷添加，20070830==============================
	print,'NUM_LY=',NUM_LY
	print,'NUM_TY=',NUM_TY
;	if SCALE_PRO ne 'COUNTY' then begin
		if N_LENGTH ne NUM_LY then begin
			TEMP=DIALOG_MESSAGE('去年所选时间内数据不全',TITLE='提示')
			CLOSE,/ALL
	 		RETURN, 0
		endif

		if N_LENGTH ne NUM_TY then begin
			TEMP=DIALOG_MESSAGE('今年所选时间内数据不全',TITLE='提示')
			CLOSE,/ALL
	 		RETURN, 0
		endif
;	endif
;========================================================

	;定义一个数组作为横坐标

	;WINDOW,1,XSIZE=300,YSIZE=210, YPOS=0
	WINDOW,1,XSIZE=400,YSIZE=280, YPOS=0,TITLE='过程监测结果';, /PIXMAP
	;加上这个关键字就可以把窗口进行隐藏, /PIXMAP
	WSET,1
	;**********************************************************************
	;定义一些画图使用的公共变量
	;**********************************************************************
	FONT_PRO		=	2
	CHARSIZE_PRO	=	1*1.5
	CHARTHICK_PRO	=	1
	RATE_MAGNIFY	=	1.0
	;**********************************************************************

	white=!D.N_COLORS-1

	TITLE=RANGE_PRO+(*PSTATE).LAND_TYPE_PRO+'植被指数过程'
;	X_TICKS_all=['5-Jan','5-Feb','5-Mar','5-Apr','5-May','5-Jun','5-Jul','5-Aug'	$
;				,'5-Sep','5-Oct','5-Nov','5-Dec']
;	X_TICKS_all=['5-1','5-2','5-3','5-4','5-5','5-6','5-7','5-8'	$
;				,'5-9','5-10','5-11','5-12',' ']
;		X_TICKS_all=['1月','2月','3月','4月','5月','6月','7月','8月'	$
;					,'9月','10月','11月','12月',' ']

	Y_TICKS_all=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8'	$
				,'0.9','1.0']
	Y_TICKS_all=[' ',' ',' ',' ',' ',' ',' ',' '	$
				,' ',' ']
	X_TICKS_all=[' ',' ',' ',' ',' ',' ',' ',' '	$
				,' ',' ']

	;进行系统的预设
	fontname=['SimSun','宋体','隶书', 'SimHei','黑体']
	device, set_font=fontname[4], /TT_FONT ;

	;画图分为以下几步
	;1)先画出图框
	;2)画出去年的曲线
	;3)画出今年的曲线
	;4)写出图名
	;5)画出图例

	;*********************************************
	;数据的长度是:N_LENGTH
	;*********************************************

	;1)先画出图框
	TEMP_X=INDGEN(N_LENGTH+1)
	TEMP_Y=BYTARR(N_LENGTH+1)
	;device, set_font='宋体', /TT_FONT


	;准备好刻度

	;KEDU_X_1=['16' ,'31' ,'17', '05' ,'21' ,'06' ,'22' ,'08' ,'24' ,'09' ,'25' ,'11' ,'27' ,'12' ,'28' ,'13' ,'29' ,'15' ,'31' ,'16' ,'02' ,'18' ]
	;KEDU_X_1=['16' ,'  ' ,'17', '  ' ,'21' ,'  ' ,'22' ,'  ' ,'24' ,'  ' ,'25' ,'  ' ,'27' ,'  ' ,'28' ,'  ' ,'29' ,'  ' ,'31' ,'  ' ,'02' ,'  ' ]
	;KEDU_X_2=['Jan','Jan','Feb','Mar','Mar','Apr','Apr','May','May','Jun','Jun','Jul','Jul','Aug','Aug','Sep','Sep','Oct','Oct','Nov','Dec','Dec']
	;KEDU_X_2=['Jan','   ','Feb','   ','Mar','   ','Apr','   ','May','   ','Jun','   ','Jul','   ','Aug','   ','Sep','   ','Oct','   ','Dec','   ']

	N_TEMP=3
	KEDU_X_1=STRARR(N_LENGTH+N_TEMP)
	FOR I=0,N_LENGTH+N_TEMP-1 DO BEGIN
		KEDU_X_1[I]=' '
	ENDFOR

	KEDU_X_1[0:N_LENGTH-1]=STRTRIM(FIX(ARR_MONTH_LY),2)

	;2007.04.02*****************************************************************
	;为了调试程序，进行变量的显示
	print,'KEDU_X_1',KEDU_X_1
	;print,'KEDU_X_2',KEDU_X_2
	;2007.04.02*****************************************************************

	ARR_DAY_LY=STRTRIM(FIX(ARR_DAY_LY),2)
	FOR I=0,NUM_LY-1 DO BEGIN
	;FOR I=0,N_LENGTH-1 DO BEGIN
		IF((I MOD N_TEMP) NE 0) THEN  KEDU_X_1[I]=' '
		IF((I MOD N_TEMP) NE 0) THEN  ARR_DAY_LY[I]=' '
	ENDFOR

	;2007.04.02*****************************************************************
	;为了调试程序，进行变量的显示
	print,'KEDU_X_1',KEDU_X_1
	;print,'KEDU_X_2',KEDU_X_2
	;2007.04.02*****************************************************************

	KEDU_Y=['0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0']

	PLOT,TEMP_X,TEMP_Y,BACKGROUND=WHITE,SYMSIZE=0.6$
		,MAX_VALUE=1.0,MIN_VALUE=0,/YNOZERO		$
		,COLOR=0	$;255+256L*(255L+255*256L)	$
		;,/device	$
		,XMARGIN=[7,1.6]*RATE_MAGNIFY	$	;控制边白的大小
		,YMARGIN=[2.7,2]*RATE_MAGNIFY	$	;控制边白的大小

		,YCHARSIZE=1.0*CHARSIZE_PRO*0	$	;控制X标注的大小
		,YRANGE=[0,1.0]	$
		,YTHICK=1.0*RATE_MAGNIFY	$
		,YTICKNAME=[' ',' ',' ',' ',' ',' ','','','','','','','','','','','','']	$

		,XTHICK=1.0*RATE_MAGNIFY	$
		,XRANGE=[0,N_LENGTH]	$
		,XMINOR=1	$					;控制低级坐标轴的显示
		,XCHARSIZE=1.0*CHARSIZE_PRO	$
		,XTICKLAYOUT=1	$
		,XTICKINTERVAL=1 $
		;,XTICKNAME=KEDU_X_1	$
		,XTICKNAME=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '	$
					,' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']	$

		,XTICKLEN=0.05*RATE_MAGNIFY	$
		,FONT	=FONT_PRO	$
;		,TITLE	=PLACE_PRO+'耕地植被指数过程'	$	;原代码
		,TITLE	=PLACE_PRO+(*PSTATE).LAND_TYPE_PRO+'植被指数过程'	$	;杨绍锷修改，20070906
		,CHARSIZE=CHARSIZE_PRO*RATE_MAGNIFY	$
		,CHARTHICK=1*RATE_MAGNIFY



	;画出X轴的刻度
	FOR I=0,N_LENGTH*1 DO BEGIN
		 OPLOT,[I,I],[0,0.03],COLOR=0+256L*(0L+0*256L)
	ENDFOR



	;******************************************************************************
	;代码修改2006.11.23

	;根据数据是奇数个还是偶数个进行标签个数的选择
	TEMP_N=N_LENGTH
	KEDU_X_2=ARR_DAY_LY
	;----------------------------------------------------------------------------
	;对月的刻度进行数字到字母的转换
	B=WHERE(KEDU_X_1 EQ '1')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Jan'
	B=WHERE(KEDU_X_1 EQ '2')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Feb'
	B=WHERE(KEDU_X_1 EQ '3')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Mar'
	B=WHERE(KEDU_X_1 EQ '4')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Apr'
	B=WHERE(KEDU_X_1 EQ '5')
	IF B[0] NE -1 THEN KEDU_X_1[B]='May'
	B=WHERE(KEDU_X_1 EQ '6')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Jun'
	B=WHERE(KEDU_X_1 EQ '7')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Jul'
	B=WHERE(KEDU_X_1 EQ '8')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Aug'
	B=WHERE(KEDU_X_1 EQ '9')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Sep'
	B=WHERE(KEDU_X_1 EQ '10')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Oct'
	B=WHERE(KEDU_X_1 EQ '11')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Nov'
	B=WHERE(KEDU_X_1 EQ '12')
	IF B[0] NE -1 THEN KEDU_X_1[B]='Dec'
	;----------------------------------------------------------------------------

	;TEMP_N=((TEMP_N-1)/2+1)*2+1



	FOR I=0,TEMP_N-1 DO BEGIN
		xyouts, I-0.6-0.012*(I), -0.22,KEDU_X_1[I],color=0, charsize=1.6,font=2;,/device
		xyouts, I-0.2-0.012*(I), -0.12,KEDU_X_2[I],color=0, charsize=1.6,font=2;,/device
		;xyouts, (I-1)*3-(1.2), -0.22,KEDU_X_2[I-1],color=0, charsize=1,font=2;,/device
	ENDFOR
	;******************************************************************************


	;画出两条Y轴
	OPLOT,[0,0],[0,1],COLOR=0
	OPLOT,[N_LENGTH*3+3-3,N_LENGTH*3+3-3],[0,1],COLOR=0


	;画出Y轴的刻度值

	FOR I=1,11 DO BEGIN
		xyouts, N_LENGTH*(-0.1),-0.03+0.1*(I-1),KEDU_Y[I-1],color=0, charsize=1,font=2;,/device
	ENDFOR



	;画出Y轴的刻度
	FOR I=1,10 DO BEGIN
		OPLOT,[0,N_LENGTH+3],[0.1*I,0.1*I],COLOR=200+256L*(200L+200*256L)
		OPLOT,[0,0.01*(N_LENGTH+3)],[0.1*I,0.1*I],COLOR=0+256L*(0L+0*256L)
	ENDFOR


	;画出Y轴的顶轴
	OPLOT,[0,N_LENGTH*3+3],[1.0,1.0],COLOR=0+256L*(0L+0*256L)


	;2)画出去年的曲线
	;定义去年的图例
	X = [-1.5, 0, 1.5, 0, -1.5]*1.2*RATE_MAGNIFY
	Y = [0, 1.5, 0, -1.5, 0]*1.2*RATE_MAGNIFY
	USERSYM, X, Y ,/FILL
	OPLOT,FINDGEN(36),ARR_DATA_LY,$
		SYMSIZE=0.6$
		,MAX_VALUE=1.0,MIN_VALUE=0	$
		,COLOR=0+256L*(255L+0*256L)

	OPLOT,FINDGEN(36),ARR_DATA_LY,PSYM=8,$
		SYMSIZE=0.6$
		,MAX_VALUE=1.0,MIN_VALUE=0	$
		,COLOR=0+256L*(255L+0*256L)

	;3)画出今年的曲线
	;定义今年的图例
   	X = [-1, 1, 0, -1]*1.8*RATE_MAGNIFY
	Y = [-0.777,-0.777,1.155, -0.777]*1.8*RATE_MAGNIFY
	USERSYM, X, Y ,/FILL
	OPLOT,FINDGEN(36),ARR_DATA_TY,$
		SYMSIZE=0.6$
		,MAX_VALUE=1.0,MIN_VALUE=0	$
		,COLOR=255+256L*(0L+0*256L)
		;,/device	$
	OPLOT,FINDGEN(36),ARR_DATA_TY,PSYM=8,$
		SYMSIZE=0.6$
		,MAX_VALUE=1.0,MIN_VALUE=0	$
		,COLOR=255+256L*(0L+0*256L)
		;,/device	$

	;4)写出图名
;		;显示图的名称(横向)
;		;!P.FONT=3
;		xyouts, 150, 238, TITLE,color=0, charsize=2,CHARTHICK=2,font=0,/device
	TEMP_ARR=['作','物','植','被','指','数']
	FOR I=0,5 DO BEGIN
		xyouts, (N_LENGTH-1)*(-0.65)/3, 0.8-0.12*I, TEMP_ARR[I],color=0, charsize=2,font=3
	ENDFOR


	;5)画出图例
	;**********************************************************************
	;由于NDVI出现了大于0.8的情况,
	;需要对原来的图例进行上移,上移的幅度为0.1个单位,
	;即由原来的0.8-0.9移到了0.9-1.0
	;下面的变量就是为图例的上移所用
	TEMP_OFFSET=0.1
	;**********************************************************************
	;PRINT,N_X_TICKS_MONTH
	temp=1.1
	TEMP=TEMP/3
	OPLOT,[(N_LENGTH-1)*0.7*temp,(N_LENGTH-1)*2.2*temp],[0.90,0.90]+TEMP_OFFSET,COLOR=0+256L*(0L+0*256L)
	OPLOT,[(N_LENGTH-1)*0.7*temp,(N_LENGTH-1)*2.2*temp],[0.80,0.80]+TEMP_OFFSET,COLOR=0+256L*(0L+0*256L)
	OPLOT,[(N_LENGTH-1)*0.7*temp,(N_LENGTH-1)*0.7*temp],[0.80,0.90]+TEMP_OFFSET,COLOR=0+256L*(0L+0*256L)
	OPLOT,[(N_LENGTH-1)*2.2*temp,(N_LENGTH-1)*2.2*temp],[0.80,0.90]+TEMP_OFFSET,COLOR=0+256L*(0L+0*256L)

	OPLOT,[(N_LENGTH-1)*0.78*temp,(N_LENGTH-1)*0.98*temp],[0.85,0.85]+TEMP_OFFSET,COLOR=0+256L*(255L+0*256L)
	OPLOT,[(N_LENGTH-1)*1.47*temp,(N_LENGTH-1)*1.67*temp],[0.85,0.85]+TEMP_OFFSET,COLOR=255+256L*(0L+0*256L)


	X = [-1.5, 0, 1.5, 0, -1.5]*0.8*RATE_MAGNIFY
	Y = [0, 1.5, 0, -1.5, 0]*0.8*RATE_MAGNIFY
	USERSYM, X, Y ,/FILL
	OPLOT,[(N_LENGTH-1)*0.9*temp],[0.85]+TEMP_OFFSET,COLOR=0+256L*(255L+0*256L),PSYM=8
	X = [-1, 1, 0, -1]*1.2*RATE_MAGNIFY
	Y = [-0.777,-0.777,1.155, -0.777]*1.2*RATE_MAGNIFY
	USERSYM, X, Y ,/FILL
	OPLOT,[(N_LENGTH-1)*1.57*temp],[0.85]+TEMP_OFFSET,COLOR=255+256L*(0L+0*256L),PSYM=8

	;写出两个时间(年)
	XYOUTS, (N_LENGTH-1)*0.86*temp, 0.82*RATE_MAGNIFY+TEMP_OFFSET, YEAR_START_LY_PRO	$
		,COLOR=0, CHARSIZE=CHARSIZE_PRO*RATE_MAGNIFY,FONT=2;,charsize=1;FONT=FONT_PRO
	XYOUTS, (N_LENGTH-1)*1.53*temp, 0.82*RATE_MAGNIFY+TEMP_OFFSET, YEAR_START_TY_PRO	$
		,COLOR=0, CHARSIZE=CHARSIZE_PRO*RATE_MAGNIFY,FONT=2;,charsize=1;FONT=FONT_PRO


	;************************************************************************************
	;在这里进行缩略图的显示
	;窗口的大小参考:WINDOW,1,XSIZE=350*1.25,YSIZE=225*1.2
	;DRAW_PRO_JPG
	;SCR_XSIZE=200 ,SCR_YSIZE=189
	GEOMETRY = WIDGET_INFO((*pstate).DRAW_PRO_JPG, /GEOMETRY)
	X_TEMP_PRO=GEOMETRY.SCR_XSIZE
	Y_TEMP_PRO=GEOMETRY.SCR_YSIZE

	IMAGE_PRO = TVRD(true=3,/order)

	;以一个指针的形式将变量传出去
	(*PSTATE).P_IMAGE_PRO	=PTR_NEW(IMAGE_PRO)

;	X_TEMP_PRO=198.0
;	Y_TEMP_PRO=189.0*210/300
	IMAGE_PRO_SHOW=CONGRID(IMAGE_PRO,X_TEMP_PRO,Y_TEMP_PRO,3)

	WIDGET_CONTROL,(*pstate).DRAW_PRO_JPG,get_value=DRAW_PRO_JPG
	WSET,DRAW_PRO_JPG
	white=!D.N_COLORS-1
	ERASE,white
	TV,IMAGE_PRO_SHOW,/ORDER, TRUE=3

	;************************************************************************************
	WIDGET_CONTROL,(*pstate).CMD_OUTPUT_JPG,SENSITIVE=1
	(*pstate).SHOW_PRO_DONE=1
	RETURN,0

END

;-----------------------------------------------------------------

PRO BASE_TOP_JPG_EVENT, EVENT

  WTARGET = (WIDGET_INFO(EVENT.ID,/NAME) EQ 'TREE' ?  $
      WIDGET_INFO(EVENT.ID, /TREE_ROOT) : EVENT.ID)

  common common_setpath, ppath
  zs_out_path = (*ppath).zs_out_path

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
  WWIDGET =  EVENT.TOP
  WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
  CASE WTARGET OF

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_FILE_JPG_RT_JPG'): BEGIN
    	file=DIALOG_PICKFILE(filter='*.jpg',path=zs_out_path,title='选择输出jpg图片', DIALOG_PARENT=Event.id)

     	IF (file NE '') THEN BEGIN
     		if strmatch(file,'*.jpg') eq 0 then file += '.jpg'
			WIDGET_CONTROL,(*PSTATE).TXT_JPG_RT_JPG,SET_VALUE=file
     	ENDIF
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_FILE_CLASSIFIED_RT_JPG'): BEGIN
    	file=DIALOG_PICKFILE(filter='*.*',path=zs_out_path,title='选择输入分级图像', DIALOG_PARENT=Event.id, /MUST_EXIST)

	    IF (file NE '') THEN BEGIN
			WIDGET_CONTROL,(*PSTATE).TXT_FILE_CLASSIFIED_RT_JPG,SET_VALUE=FILE
	    ENDIF
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_PROVINCE_PRO_JPG'): BEGIN
    	(*PSTATE).SCALE_PRO='PROVINCE'
    	(*PSTATE).SCALE_RT='PROVINCE'
		WIDGET_CONTROL,(*PSTATE).DST_RANGE_PRO,SET_VALUE=' '
		WIDGET_CONTROL,(*PSTATE).DST_RANGE_PRO,SENSITIVE=0
;		(*PSTATE).RANGE_PRO='黑龙江' ;原程序
		PROVINCE_NAME = ''
		SQL = "SELECT Name FROM PROVINCE_CODE WHERE Code = '" + PROVINCE_CODE + "'";
		oRS = OBJ_NEW('IDLdbRecordset', DBobj, SQL=SQL)
		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN
		    PROVINCE_NAME = ORS->GetField(0)
		ENDIF
		obj_destroy, oRS
		(*PSTATE).RANGE_PRO=PROVINCE_NAME
		(*PSTATE).RANGE_CODE_PRO=PROVINCE_CODE
		(*PSTATE).RANGE_RT=PROVINCE_NAME
		(*PSTATE).RANGE_CODE_RT=PROVINCE_CODE
		(*PSTATE).RASTER_VALUE=1
		print,'scale:',(*PSTATE).SCALE_PRO
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_ROI_PRO_JPG'): BEGIN
    	IF(EVENT.SELECT EQ 0) THEN RETURN
	  	(*PSTATE).SCALE_PRO='ROI'
	  	(*PSTATE).SCALE_RT='ROI'
	  	;进行国家名称数据的读取
	  	;把国家名称的数据从数据库中查询出来
		oRS = OBJ_NEW('IDLdbRecordset', DBobj, TABLE='ROI_code')
		num_of_RANGE=1
		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

			;先找出国家的个数
			WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
				num_of_RANGE=num_of_RANGE+1
			ENDWHILE

			;然后将国家的名称读入数组
			ARR_RANGE=STRARR(NUM_OF_RANGE+1)
			ARR_RANGE_CODE=STRARR(NUM_OF_RANGE+1)

			TEMP=oRS->MoveCursor(/FIRST)
			COUNT=0
			ARR_RANGE[COUNT]='请选择'

			REPEAT BEGIN
				COUNT=COUNT+1
				ARR_RANGE[COUNT]=ORS->GetField(1)
				ARR_RANGE_CODE[COUNT]=ORS->GetField(0)
			ENDREP UNTIL (oRS->MoveCursor(/next) NE 1)
     		WIDGET_CONTROL,(*PSTATE).DST_RANGE_PRO,SET_VALUE=ARR_RANGE
     		WIDGET_CONTROL,(*PSTATE).DST_RANGE_PRO,SENSITIVE=1
		  	;WIDGET_CONTROL,DST_RANGE_BATCH,SET_VALUE=ARR_RANGE

		  	(*PSTATE).RANGE_PRO='请选择'
		  	(*PSTATE).ARR_RANGE_PRO[0:NUM_OF_RANGE]=ARR_RANGE[0:NUM_OF_RANGE]
		  	(*PSTATE).ARR_RANGE_CODE_PRO[0:NUM_OF_RANGE]=ARR_RANGE_CODE[0:NUM_OF_RANGE]
		  	(*PSTATE).NUM_OF_RANGE_PRO=NUM_OF_RANGE

			(*PSTATE).RANGE_RT='请选择'
		  	(*PSTATE).ARR_RANGE_RT[0:NUM_OF_RANGE]=ARR_RANGE[0:NUM_OF_RANGE]
		  	(*PSTATE).ARR_RANGE_CODE_RT[0:NUM_OF_RANGE]=ARR_RANGE_CODE[0:NUM_OF_RANGE]
		  	(*PSTATE).NUM_OF_RANGE_RT=NUM_OF_RANGE

		ENDIF
		OBJ_DESTROY, ORS
		print,'scale:',(*PSTATE).SCALE_PRO
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_COUNTY_PRO_JPG'): BEGIN

    	IF(EVENT.SELECT EQ 0) THEN RETURN
    	WIDGET_CONTROL,(*PSTATE).DST_RANGE_PRO,SENSITIVE=1

	  	(*PSTATE).SCALE_PRO='COUNTY'
	  	(*PSTATE).SCALE_RT='COUNTY'
	  	;进行国家名称数据的读取
	  	;把国家名称的数据从数据库中查询出来
	  	SQL='SELECT T1.CODE,T1.NAME,T2.RASTER_VALUE FROM '
	  	SQL=SQL+'COUNTY_CODE T1,COUNTY_CODE_RASTER T2 WHERE (T1.PROVINCE_CODE='
	  	SQL=SQL+"'"+PROVINCE_CODE+"'"+')'
	  	SQL=SQL+'AND (T1.CODE=T2.CODE)'
		oRS = OBJ_NEW('IDLdbRecordset', DBobj, SQL=SQL)
		num_of_RANGE=1
		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

			;先找出国家的个数
			WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
				num_of_RANGE=num_of_RANGE+1

			ENDWHILE

			;然后将国家的名称读入数组
			ARR_RANGE=STRARR(NUM_OF_RANGE+1)
			ARR_RANGE_CODE=STRARR(NUM_OF_RANGE+1)
			ARR_RASTER_VALUE=BYTARR(NUM_OF_RANGE+1)

			TEMP=oRS->MoveCursor(/FIRST)
			COUNT=0
			ARR_RANGE[COUNT]='请选择'

			REPEAT BEGIN
					COUNT=COUNT+1
					ARR_RANGE[COUNT]=ORS->GetField(1)
					ARR_RANGE_CODE[COUNT]=ORS->GetField(0)
					ARR_RASTER_VALUE[COUNT]=ORS->GetField(2)
			ENDREP UNTIL (oRS->MoveCursor(/next) NE 1)
     		WIDGET_CONTROL,(*PSTATE).DST_RANGE_PRO,SET_VALUE=ARR_RANGE

			(*PSTATE).RANGE_PRO='请选择'
		  	(*PSTATE).ARR_RANGE_PRO[0:NUM_OF_RANGE]=ARR_RANGE[0:NUM_OF_RANGE]
		  	(*PSTATE).ARR_RANGE_CODE_PRO[0:NUM_OF_RANGE]=ARR_RANGE_CODE[0:NUM_OF_RANGE]
		  	(*PSTATE).NUM_OF_RANGE_PRO=NUM_OF_RANGE

			(*PSTATE).RANGE_RT='请选择'
		  	(*PSTATE).ARR_RANGE_RT[0:NUM_OF_RANGE]=ARR_RANGE[0:NUM_OF_RANGE]
		  	(*PSTATE).ARR_RANGE_CODE_RT[0:NUM_OF_RANGE]=ARR_RANGE_CODE[0:NUM_OF_RANGE]
		  	(*PSTATE).ARR_RASTER_VALUE[0:NUM_OF_RANGE]=ARR_RASTER_VALUE[0:NUM_OF_RANGE]
		  	(*PSTATE).NUM_OF_RANGE_RT=NUM_OF_RANGE
		ENDIF
		OBJ_DESTROY, ORS
		print,'scale:',(*PSTATE).SCALE_PRO
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_RANGE_PRO'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).RANGE_PRO=(*PSTATE).ARR_RANGE_PRO[INDEX]
	    (*PSTATE).RANGE_CODE_PRO=(*PSTATE).ARR_RANGE_CODE_PRO[INDEX]
	    (*PSTATE).RANGE_RT=(*PSTATE).ARR_RANGE_PRO[INDEX]
	    (*PSTATE).RANGE_CODE_RT=(*PSTATE).ARR_RANGE_CODE_PRO[INDEX]
	    (*PSTATE).RASTER_VALUE=(*PSTATE).ARR_RASTER_VALUE[INDEX]
	    PRINT,(*PSTATE).RANGE_PRO,(*PSTATE).RANGE_CODE_PRO
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_LAND_TYPE_PRO_JPG'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).LAND_TYPE_RT=(*PSTATE).ARR_LAND_TYPE[INDEX]
    	(*PSTATE).LAND_TYPE_PRO=(*PSTATE).ARR_LAND_TYPE[INDEX]
;    	(*PSTATE).SUBRANGE_PRO=(*PSTATE).ARR_SUBRANGE_PRO[INDEX]
;	    (*PSTATE).SUBRANGE_CODE_PRO=(*PSTATE).ARR_SUBRANGE_CODE_PRO[INDEX]
;	    (*PSTATE).SUBRANGE_RT=(*PSTATE).ARR_SUBRANGE_PRO[INDEX]
;	    (*PSTATE).SUBRANGE_CODE_RT=(*PSTATE).ARR_SUBRANGE_CODE_PRO[INDEX]
;	    print,(*PSTATE).SUBRANGE_PRO
;	    print,(*PSTATE).SUBRANGE_CODE_PRO
    END
    ;****************************************************************************
    ;补充代码,2006.11.23
    ;频率
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_PERIOD_PRO_JPG'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).PERIOD=(*PSTATE).ARR_DAY[INDEX]
	    print,'日',(*PSTATE).PERIOD
    END
    ;指数类型
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_DATA_TYPE_PRO_JPG'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).DATA_TYPE=(*PSTATE).ARR_DATA_TYPE[INDEX]
	    print,'数据类型',(*PSTATE).DATA_TYPE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_SENSOR_TYPE_PRO_JPG'): BEGIN
    	INDEX=EVENT.INDEX
    	(*PSTATE).SENSOR_CODE=(*PSTATE).ARR_SENSOR_CODE[INDEX]
	    print,'传感器',(*PSTATE).SENSOR_CODE
    END
    ;****************************************************************************
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_FILE_JPG_PRO_JPG'): BEGIN
    	file=DIALOG_PICKFILE(filter='*.jpg',path=zs_out_path,title='选择输出jpg图片', DIALOG_PARENT=Event.id)

		IF (file NE '') THEN BEGIN
			if strmatch(file,'*.jpg') eq 0 then file += '.jpg'
			WIDGET_CONTROL,(*PSTATE).TXT_JPG_PRO_JPG,SET_VALUE=file
	    ENDIF
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_SHOW_PRO_JPG'): BEGIN
    	(*PSTATE).SHOW_PRO=EVENT.SELECT
	    WIDGET_CONTROL,(*PSTATE).BASE_PRO_JPG,SENSITIVE=EVENT.SELECT
	    IF(((*PSTATE).SHOW_PRO EQ 0) AND ((*PSTATE).SHOW_RT EQ 0)) THEN BEGIN
	     	WIDGET_CONTROL,(*PSTATE).CMD_SHOW_JPG,SENSITIVE=0
	     	WIDGET_CONTROL,(*PSTATE).CMD_OUTPUT_JPG,SENSITIVE=0
	    ENDIF ELSE BEGIN
	     	WIDGET_CONTROL,(*PSTATE).CMD_SHOW_JPG,SENSITIVE=1
	    ENDELSE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_SHOW_RT_JPG'): BEGIN
    	(*PSTATE).SHOW_RT=EVENT.SELECT
	    WIDGET_CONTROL,(*PSTATE).BASE_RT_JPG,SENSITIVE=EVENT.SELECT
	    IF(((*PSTATE).SHOW_PRO EQ 0) AND ((*PSTATE).SHOW_RT EQ 0)) THEN BEGIN
	     	WIDGET_CONTROL,(*PSTATE).CMD_SHOW_JPG,SENSITIVE=0
	     	WIDGET_CONTROL,(*PSTATE).CMD_OUTPUT_JPG,SENSITIVE=0
	    ENDIF ELSE BEGIN
	     	WIDGET_CONTROL,(*PSTATE).CMD_SHOW_JPG,SENSITIVE=1
	    ENDELSE
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_SHOW_JPG'): BEGIN
    	IF((*PSTATE).SHOW_RT EQ 1) THEN BEGIN	;进行实时图像的显示
    		A=ZS_SHOW_JPG_RT(PSTATE)
    	ENDIF
    	IF((*PSTATE).SHOW_PRO EQ 1) THEN BEGIN	;进行实时图像的显示
    		A=ZS_SHOW_JPG_PRO(PSTATE)
    	ENDIF
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_OUTPUT_JPG'): BEGIN
    	;这也是系统的主要部分,将结果以JPG图片的形式进行输出

	   	;*********************************************************************************
	   	;1)首先是实时监测的图像
	   	;*********************************************************************************
	   	IF((*PSTATE).SHOW_RT EQ 1) THEN BEGIN
		  	IF((*PSTATE).SHOW_RT_DONE NE 1) THEN BEGIN
	  			msg=DIALOG_MESSAGE('请先进行图像的显示!',/INFORMATION,TITLE='请先显示图像')
		        CLOSE,/all
		        RETURN
	  		ENDIF
		  	WIDGET_CONTROL,(*pstate).TXT_JPG_RT_JPG,get_value=FILE_JPG_RT_JPG
		  	IF(FILE_JPG_RT_JPG EQ '') THEN BEGIN
		        msg=DIALOG_MESSAGE('请选择输出的图片文件!',/INFORMATION,TITLE='选择文件')
		        CLOSE,/all
		        RETURN
		    ENDIF
		    FILE_JPG_RT_JPG=FILE_JPG_RT_JPG[0]
		    RESULT=FILE_INFO(FILE_JPG_RT_JPG)
		    IF(RESULT.EXISTS NE 0) THEN BEGIN
		        msg=DIALOG_MESSAGE('输出文件已经存在,要覆盖么?',TITLE='提示',/Question)
		        IF(MSG NE 'Yes') THEN BEGIN
		       		CLOSE,/all
		         	RETURN
		        ENDIF
		    ENDIF

		    ;获取数据文件
			WRITE_JPEG, FILE_JPG_RT_JPG ,*((*PSTATE).P_IMAGE_RT) , /ORDER, /PROGRESSIVE, QUALITY=100,TRUE=3
			TEMP=DIALOG_MESSAGE('实时监测结果图像已经输出',TITLE='提示',/INFORMATION)
			log, '长势监测-结果分析', 1
		ENDIF
		;*********************************************************************************
	   	;2)然后是过程监测的图像
	   	;*********************************************************************************
	   	IF((*PSTATE).SHOW_PRO EQ 1) THEN BEGIN
		  	IF((*PSTATE).SHOW_PRO_DONE NE 1) THEN BEGIN
	  			msg=DIALOG_MESSAGE('请先进行过程监测图像的显示!',/INFORMATION,TITLE='请先显示图像')
		        CLOSE,/all
		        RETURN
		  	ENDIF
		  	WIDGET_CONTROL,(*pstate).TXT_JPG_PRO_JPG,get_value=FILE_JPG_PRO_JPG
		  	IF(FILE_JPG_PRO_JPG EQ '') THEN BEGIN
		        msg=DIALOG_MESSAGE('请选择输出的图片文件!',/INFORMATION,TITLE='选择文件')
		        CLOSE,/all
		        RETURN
		    ENDIF
		    FILE_JPG_PRO_JPG=FILE_JPG_PRO_JPG[0]
		    RESULT=FILE_INFO(FILE_JPG_PRO_JPG)
		    IF(RESULT.EXISTS NE 0) THEN BEGIN
		        msg=DIALOG_MESSAGE('输出文件已经存在,要覆盖么?',TITLE='提示',/Question)
		        IF(MSG NE 'Yes') THEN BEGIN
		       		CLOSE,/all
		         	RETURN
		        ENDIF
		    ENDIF

		    ;获取数据文件
			WRITE_JPEG , FILE_JPG_PRO_JPG ,*((*PSTATE).P_IMAGE_PRO) , /ORDER, /PROGRESSIVE, QUALITY=100,TRUE=3
			TEMP=DIALOG_MESSAGE('过程监测结果图像已经输出',TITLE='提示',/INFORMATION)
			log, '长势监测-结果分析', 1
		ENDIF
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_HELP_JPG'): BEGIN

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, "监测结果成图", BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
    END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_CLOSE_JPG'): BEGIN
    common_log,'关闭长势结果分析'
    	CLOSE,/ALL
     	WIDGET_CONTROL, EVENT.TOP, /DESTROY
    END
    ELSE:
  ENDCASE

END

PRO ZS_BASE_TOP_JPG,GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

COMMON COMMON_SETPATH,ppath
zs_in_path = (*ppath).zs_in_path
zs_out_path = (*ppath).zs_out_path
common current_date, c_year, c_month, c_day

  BASE_TOP_JPG = WIDGET_BASE( GROUP_LEADER=WGROUP,  $
      UNAME='BASE_TOP_JPG' ,XOFFSET=120 ,YOFFSET=200  $
      ,TITLE='监测结果成图' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1,/column)

  BASE_RT_JPG = WIDGET_BASE(BASE_TOP_JPG, UNAME='BASE_RT_JPG'  $
      ,FRAME=1,SPACE=3 ,XPAD=3 ,YPAD=3,/row)

  BASE_PRO_JPG = WIDGET_BASE(BASE_TOP_JPG, UNAME='BASE_PRO_JPG'  $
      ,FRAME=1,SPACE=3 ,XPAD=3 ,YPAD=3,/row)

  BASE_DATE_RT_JPG = WIDGET_BASE(BASE_RT_JPG,  $
      UNAME='BASE_DATE_RT_JPG' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)

  BASE_draw_RT_JPG = WIDGET_BASE(BASE_RT_JPG,  $
      UNAME='BASE_DRAW_RT_JPG' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)
  DRAW_RT_JPG = WIDGET_DRAW(BASE_DRAW_RT_JPG, UNAME='DRAW_RT_JPG'  $
      ,SCR_XSIZE=250 ,SCR_YSIZE=175)

 BASE_DATE_PRO_JPG = WIDGET_BASE(BASE_PRO_JPG,  $
      UNAME='BASE_DATE_PRO_JPG' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)

  BASE_draw_PRO_JPG = WIDGET_BASE(BASE_PRO_JPG,  $
      UNAME='BASE_DRAW_PRO_JPG' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)
  DRAW_PRO_JPG = WIDGET_DRAW(BASE_draw_PRO_JPG, UNAME='DRAW_PRO_JPG'  $
      ,SCR_XSIZE=250 ,SCR_YSIZE=175)

  WID_LABEL = WIDGET_LABEL(BASE_DATE_RT_JPG,/ALIGN_center ,VALUE='实时监测成图')
  WID_LABEL = WIDGET_LABEL(BASE_DATE_PRO_JPG, /ALIGN_center ,VALUE='过程监测成图')

  subbase = widget_base(BASE_DATE_RT_JPG,SPACE=3 ,XPAD=3,YPAD=3,/row,/frame)
  WID_LABEL = Widget_Label(subbase,/ALIGN_LEFT ,VALUE='时间:')
  WID_LABEL_date = Widget_text(subbase,xsize=13, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))
  CMD_pick_date = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date', $
  VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_jpg'})

  BASE_FILE_RT_JPG = WIDGET_BASE(BASE_DATE_RT_JPG,  $
      UNAME='BASE_FILE_RT_JPG' ,FRAME=1, SPACE=3 ,XPAD=3  $
      ,YPAD=3,/row)
  LBL_FILE_CLASSIFIED_RT_JPG = WIDGET_LABEL(BASE_FILE_RT_JPG,  $
      /ALIGN_LEFT ,VALUE='输入分级图像:')
  TXT_FILE_CLASSIFIED_RT_JPG = WIDGET_TEXT(BASE_FILE_RT_JPG,  $
      UNAME='TXT_FILE_CLASSIFIED_RT_JPG',XSIZE=39)
  CMD_FILE_CLASSIFIED_RT_JPG = WIDGET_BUTTON(BASE_FILE_RT_JPG,  $
      UNAME='CMD_FILE_CLASSIFIED_RT_JPG' $
      ,SCR_XSIZE=36,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='.\Image\open.bmp' ,/BITMAP)


  BASE_FILE_RT_JPG2 = WIDGET_BASE(BASE_DATE_RT_JPG,  $
      UNAME='BASE_FILE_RT_JPG' ,FRAME=1, SPACE=3 ,XPAD=3  $
      ,YPAD=3,/row)
  LBL_JPG_RT_JPG = WIDGET_LABEL(BASE_FILE_RT_JPG2,  $
      UNAME='LBL_JPG_RT_JPG' ,/ALIGN_LEFT ,VALUE='输出JPG图片:')
  TXT_JPG_RT_JPG = WIDGET_TEXT(BASE_FILE_RT_JPG2,  $
      UNAME='TXT_JPG_RT_JPG' ,XSIZE=40)
  CMD_FILE_JPG_RT_JPG = WIDGET_BUTTON(BASE_FILE_RT_JPG2,  $
      UNAME='CMD_FILE_JPG_RT_JPG' $
      ,SCR_XSIZE=36,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='.\Image\open.bmp' ,/BITMAP)

  label_base = WIDGET_BASE(BASE_DATE_RT_JPG,/row,space=3,xpad=3,FRAME=1)
  LBL_SIZE_RT_JPG = WIDGET_LABEL(label_base, /ALIGN_LEFT ,VALUE='图片尺寸(象素):')
  LBL_XSIZE_RT_JPG = WIDGET_LABEL(label_base,/ALIGN_LEFT ,VALUE='    X:')
  TXT_XSIZE_RT_JPG = WIDGET_TEXT(label_base,  $
      UNAME='TXT_XSIZE_RT_JPG',/EDITABLE ,VALUE='400',xsize=5)
  LBL_YSIZE_RT_JPG = WIDGET_LABEL(label_base,/ALIGN_LEFT ,VALUE='        Y:')
  TXT_YSIZE_RT_JPG = WIDGET_TEXT(label_base,  $
      UNAME='TXT_YSIZE_RT_JPG' ,/EDITABLE ,VALUE='400',xsize=5)

  subbase = widget_base(BASE_DATE_PRO_JPG,SPACE=3 ,XPAD=3,YPAD=3,/row,/frame)
  WID_LABEL = Widget_Label(subbase,/ALIGN_LEFT ,VALUE='起始时间:')
  WID_LABEL_date = Widget_text(subbase,xsize=13, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim('1',2)+'-'+strtrim('1',2))
  CMD_pick_date_s = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date_s', $
  VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_s_jpg'})

  WID_LABEL = Widget_Label(subbase,/ALIGN_LEFT ,VALUE='结束时间:')
  WID_LABEL_date = Widget_text(subbase,xsize=13, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim('12',2)+'-'+strtrim('31',2))
  CMD_pick_date_e = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date_e', $
  VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_e_jpg'})

  ;另加的模块,在界面上加入了传感器\频率\及数据类型的选项
  BASE_TEMP_PRO_JPG = WIDGET_BASE(BASE_DATE_PRO_JPG,FRAME=1,SPACE=3 ,XPAD=3 ,YPAD=3, /row)

  LBL_SCALE_PRO_JPG = WIDGET_LABEL(BASE_TEMP_PRO_JPG,/ALIGN_LEFT ,VALUE='频率:')
  DST_PERIOD_PRO_JPG = WIDGET_DROPLIST(BASE_TEMP_PRO_JPG, UNAME='DST_PERIOD_PRO_JPG',xsize=50)

  LBL_RANGE_PRO_JPG = WIDGET_LABEL(BASE_TEMP_PRO_JPG, /ALIGN_LEFT ,VALUE='数据类型:')
  DST_DATA_TYPE_PRO_JPG = WIDGET_DROPLIST(BASE_TEMP_PRO_JPG, UNAME='DST_DATA_TYPE_PRO_JPG',xsize=70)

  LBL_LAND_TYPE_PRO_JPG = WIDGET_LABEL(BASE_TEMP_PRO_JPG, /ALIGN_LEFT ,VALUE='传感器:')
  DST_SENSOR_TYPE_PRO_JPG = WIDGET_DROPLIST(BASE_TEMP_PRO_JPG, UNAME='DST_SENSOR_TYPE_PRO_JPG',xsize=70)

  BASE_FILE_PRO_JPG2 = WIDGET_BASE(BASE_DATE_PRO_JPG,  $
      UNAME='BASE_FILE_PRO_JPG' ,FRAME=1, SPACE=3 ,XPAD=3  $
      ,YPAD=3,/row)
  LBL_JPG_PRO_JPG = WIDGET_LABEL(BASE_FILE_PRO_JPG2,  $
      UNAME='LBL_JPG_PRO_JPG' ,/ALIGN_LEFT ,VALUE='输出JPG图片:')
  TXT_JPG_PRO_JPG = WIDGET_TEXT(BASE_FILE_PRO_JPG2,  $
      UNAME='TXT_JPG_PRO_JPG' ,XSIZE=40)
  CMD_FILE_JPG_PRO_JPG = WIDGET_BUTTON(BASE_FILE_PRO_JPG2,  $
      UNAME='CMD_FILE_JPG_PRO_JPG' $
      ,SCR_XSIZE=36,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='.\Image\open.bmp' ,/BITMAP)

  label_base = WIDGET_BASE(BASE_DATE_PRO_JPG,/row,space=3,xpad=3,FRAME=1)
  LBL_SIZE_PRO_JPG = WIDGET_LABEL(label_base, /ALIGN_LEFT ,VALUE='图片尺寸(象素):')
  LBL_XSIZE_PRO_JPG = WIDGET_LABEL(label_base,/ALIGN_LEFT ,VALUE='    X:')
  TXT_XSIZE_PRO_JPG = WIDGET_TEXT(label_base,  $
      UNAME='TXT_XSIZE_PRO_JPG',VALUE='400',xsize=5)
  LBL_YSIZE_PRO_JPG = WIDGET_LABEL(label_base,/ALIGN_LEFT ,VALUE='        Y:')
  TXT_YSIZE_PRO_JPG = WIDGET_TEXT(label_base,  $
      UNAME='TXT_YSIZE_PRO_JPG' ,VALUE='400',xsize=5)
  ;*****************************************************************************
  BASE_DONE_WHAT_2_JPG = WIDGET_BASE(BASE_TOP_JPG,FRAME=1 ,SPACE=3 ,XPAD=3 ,YPAD=3,/row)
  LBL_SCALE_PRO_JPG = WIDGET_LABEL(BASE_DONE_WHAT_2_JPG,/ALIGN_center ,VALUE='  尺度:')

  BASE_SCALE_PRO_JPG = WIDGET_BASE(BASE_DONE_WHAT_2_JPG, UNAME='BASE_SCALE_PRO_JPG'  $
      ,ROW=1 ,/EXCLUSIVE,SPACE=3 ,XPAD=3 ,YPAD=1)
  CMD_PROVINCE_PRO_JPG = WIDGET_BUTTON(BASE_SCALE_PRO_JPG,  $
      UNAME='CMD_PROVINCE_PRO_JPG',/ALIGN_center ,VALUE='省')
  WIDGET_CONTROL,CMD_PROVINCE_PRO_JPG,SET_BUTTON=1
  CMD_ROI_PRO_JPG = WIDGET_BUTTON(BASE_SCALE_PRO_JPG,  $
      UNAME='CMD_ROI_PRO_JPG',/ALIGN_center,VALUE='监测区')
  CMD_COUNTY_PRO_JPG = WIDGET_BUTTON(BASE_SCALE_PRO_JPG, $
  	  UNAME='CMD_COUNTY_PRO_JPG',/ALIGN_center ,VALUE='县')

  label = widget_label(BASE_DONE_WHAT_2_JPG,value='区域: ')
  DST_RANGE_PRO = WIDGET_COMBOBOX(BASE_DONE_WHAT_2_JPG,  $
      UNAME='DST_RANGE_PRO',xsize=150,SENSITIVE=0)

  label = widget_label(BASE_DONE_WHAT_2_JPG,value='  耕地: ')
  DST_LAND_TYPE_PRO_JPG = WIDGET_COMBOBOX(BASE_DONE_WHAT_2_JPG,  $
      UNAME='DST_LAND_TYPE_PRO_JPG',xsize=150)

  ;**************************************************************************************
  BASE_CMD_JPG = WIDGET_BASE(BASE_TOP_JPG, UNAME='BASE_CMD_JPG'  $
      ,FRAME=1 ,SPACE=5 ,XPAD=5 ,YPAD=3,/row)

  BASE_DONE_WHAT_1_JPG = WIDGET_BASE(BASE_CMD_JPG,  $
      UNAME='BASE_DONE_WHAT_1_JPG' ,ROW=1 ,/NONEXCLUSIVE $
      ,SPACE=3 ,XPAD=3 ,YPAD=0,/row)
  CMD_SHOW_RT_JPG = WIDGET_BUTTON(BASE_DONE_WHAT_1_JPG,  $
      UNAME='CMD_SHOW_RT_JPG' ,/ALIGN_LEFT ,VALUE='实时监测成图')
  WIDGET_CONTROL,CMD_SHOW_RT_JPG,SET_BUTTON=1
  CMD_SHOW_PRO_JPG = WIDGET_BUTTON(BASE_DONE_WHAT_1_JPG,  $
      UNAME='CMD_SHOW_PRO_JPG',/ALIGN_LEFT,VALUE='过程监测成图')
  WIDGET_CONTROL,CMD_SHOW_PRO_JPG,SET_BUTTON=1

  CMD_SHOW_JPG = WIDGET_BUTTON(BASE_CMD_JPG, UNAME='CMD_SHOW_JPG' $
      ,/ALIGN_CENTER ,VALUE='图像预览',xsize=100,ysize=23)

  CMD_OUTPUT_JPG = WIDGET_BUTTON(BASE_CMD_JPG, UNAME='CMD_OUTPUT_JPG'  $
      ,/ALIGN_CENTER ,VALUE='图像输出',xsize=100,ysize=23)
  WIDGET_CONTROL,CMD_OUTPUT_JPG,SENSITIVE=0

  CMD_HELP_JPG = WIDGET_BUTTON(BASE_CMD_JPG, UNAME='CMD_HELP_JPG'  $
      ,/ALIGN_CENTER ,VALUE='帮助',xsize=100,ysize=23)

  CMD_CLOSE_JPG = WIDGET_BUTTON(BASE_CMD_JPG, UNAME='CMD_CLOSE_JPG'  $
      ,/ALIGN_CENTER ,VALUE='关闭',xsize=100,ysize=23)
 ;**************************************************************************************

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

  STATE = { $
        widget_top : BASE_TOP_JPG,$
        ARR_DAY			:	['1', '2', '3','4', '5', '6', '7', '8', '9', '10', $
							'11', '12', '13','14', '15', '16', '17', '18', '19', '20',$
							'21', '22', '23','24', '25', '26', '27', '28', '29', '30', '31'] ,$
		ARR_DATA_TYPE 	:   ['NDVI','LAI','NPP'],$
		DATA_TYPE     	:  	'NDVI'   ,$
		ARR_SENSOR      : 	[ 'AVHRR', 'MODIS', 'VGT'] ,$
		ARR_SENSOR_CODE : 	[ 1, 2, 3] ,$
		SENSOR_CODE     :    1       ,$
		period         	:    1       ,$
		YEAR				:	1990	,$
		MONTH			:	1		,$
		day				:	1		,$

		YEAR_START_LY_PRO	:	1990	,$
		MONTH_START_LY_PRO	:	1		,$
		day_START_LY_PRO	:	1		,$

		YEAR_END_LY_PRO		:	1990	,$
		MONTH_END_LY_PRO	:	1		,$
		day_END_LY_PRO		:	1		,$

		YEAR_START_TY_PRO	:	1990	,$
		MONTH_START_TY_PRO	:	1		,$
		day_START_TY_PRO	:	1		,$

		YEAR_END_TY_PRO		:	1990	,$
		MONTH_END_TY_PRO	:	1		,$
		day_END_TY_PRO		:	1		,$

        TXT_XSIZE_PRO_JPG   : 	TXT_XSIZE_PRO_JPG   ,$
        TXT_YSIZE_PRO_JPG   : 	TXT_YSIZE_PRO_JPG   ,$
        TXT_XSIZE_RT_JPG   	: 	TXT_XSIZE_RT_JPG    ,$
        TXT_YSIZE_RT_JPG   	: 	TXT_YSIZE_RT_JPG    ,$

        TXT_JPG_PRO_JPG     : 	TXT_JPG_PRO_JPG     ,$
        TXT_JPG_RT_JPG      : 	TXT_JPG_RT_JPG      ,$

        DRAW_RT_JPG			: 	DRAW_RT_JPG			,$
        DRAW_PRO_JPG		: 	DRAW_PRO_JPG		,$

        SCALE_RT			:	''					,$
        SCALE_PRO			:	''					,$

		SHOW_PRO			:	1					,$
		SHOW_RT				:	1					,$
		SHOW_PRO_DONE		:	0					,$
		SHOW_RT_DONE		:	0					,$

		;用来放从数据库中读取出来的范围数组
		ARR_RANGE_PRO			:	STRARR(150)			,$
		ARR_RANGE_CODE_PRO		:	STRARR(150)			,$

		RANGE_PRO				:	''					,$
		RANGE_CODE_PRO			:	''					,$
		NUM_OF_RANGE_PRO		:	0					,$

		ARR_RANGE_RT			:	STRARR(150)			,$
		ARR_RANGE_CODE_RT		:	STRARR(150)			,$
		ARR_RASTER_VALUE		:	STRARR(150)			,$

		RASTER_VALUE			:	0					,$

		RANGE_RT				:	''					,$
		RANGE_CODE_RT			:	''					,$
		NUM_OF_RANGE_RT			:	0					,$

		ARR_LAND_TYPE			:	['耕地','水田','旱地']	,$
		LAND_TYPE_PRO		:	'耕地'		,$
		LAND_TYPE_RT		:	'耕地'		,$

		BASE_PRO_JPG		:	BASE_PRO_JPG		,$
		BASE_RT_JPG			:	BASE_RT_JPG			,$

		CMD_SHOW_JPG		:	CMD_SHOW_JPG		,$
		CMD_OUTPUT_JPG		:	CMD_OUTPUT_JPG		,$

        TXT_FILE_CLASSIFIED_RT_JPG	: TXT_FILE_CLASSIFIED_RT_JPG,$

		DBCO_ID				:	yesORno			,$
;       	DBCO				:	obj_new('IDLdbDatabase'),$

		;定义两个指针,用来指向两个最后成图的数据
		P_IMAGE_PRO			:PTR_NEW()	,$
		P_IMAGE_RT			:PTR_NEW()	,$

		DST_RANGE_PRO		:DST_RANGE_PRO $
   }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
;    DBobj=DBobj
    WIDGET_CONTROL, BASE_TOP_JPG, SET_UVALUE=PSTATE

  ;************************************************************************
  ;给下拉列表赋值,有的数据需要从数据库中读取

  WIDGET_CONTROL,DST_LAND_TYPE_PRO_JPG,SET_VALUE=(*PSTATE).ARR_LAND_TYPE

  WIDGET_CONTROL,DST_PERIOD_PRO_JPG,SET_VALUE=(*PSTATE).ARR_DAY
  WIDGET_CONTROL,DST_DATA_TYPE_PRO_JPG,SET_VALUE=(*PSTATE).ARR_DATA_TYPE
  WIDGET_CONTROL,DST_SENSOR_TYPE_PRO_JPG,SET_VALUE=(*PSTATE).ARR_SENSOR
  ;************************************************************************
    WIDGET_CONTROL, CMD_pick_date, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_s, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_s, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_e, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date_e, set_uvalue=staff
  ;************************************************************************

  WIDGET_CONTROL, /REALIZE, BASE_TOP_JPG
  WIDGET_CONTROL,CMD_CLOSE_JPG,/INPUT_FOCUS
  white=!D.N_COLORS-1
  WIDGET_CONTROL,DRAW_RT_JPG,GET_VALUE=TMEP
  WSET,TMEP
  TEMP=INDGEN(2)
  PLOT,TEMP,BACKGROUND=WHITE

  WIDGET_CONTROL,DRAW_PRO_JPG,GET_VALUE=TMEP
  WSET,TMEP
  TEMP=INDGEN(2)
  PLOT,TEMP,BACKGROUND=WHITE

  ;***********************************************************************
  TEMP=1
  IF(TEMP EQ 1)THEN BEGIN
  	;(1)过程监测的内容
  	WIDGET_CONTROL,DST_PERIOD_PRO_JPG, SET_DROPLIST_SELECT=10-1

  	(*PSTATE).YEAR_START_LY_PRO		=	c_year-1
	(*PSTATE).MONTH_START_LY_PRO	=	1
	(*PSTATE).day_START_LY_PRO		=	1

	(*PSTATE).YEAR_END_LY_PRO  		=	c_year-1
	(*PSTATE).MONTH_END_LY_PRO		=	12
	(*PSTATE).day_END_LY_PRO		=	31

	(*PSTATE).YEAR_START_TY_PRO		=	c_year
	(*PSTATE).MONTH_START_TY_PRO	=	1
	(*PSTATE).day_START_TY_PRO		=	1

	(*PSTATE).YEAR_END_TY_PRO  		=	c_year
	(*PSTATE).MONTH_END_TY_PRO		=	12
	(*PSTATE).day_END_TY_PRO		=	31
	(*PSTATE).PERIOD				=	10
	(*PSTATE).SCALE_PRO = 'PROVINCE'

	;(2)实时监测的内容
	(*PSTATE).YEAR	=	c_year
  	(*PSTATE).MONTH	=	c_month
  	(*PSTATE).DAY	=	c_day
  	(*PSTATE).SCALE_RT = 'PROVINCE'

  	defaultnames_zsjpg,{ID:(*PSTATE).widget_top, TOP:(*PSTATE).widget_top}

	BASE_TOP_JPG_EVENT,{ID:CMD_PROVINCE_PRO_JPG, TOP:BASE_TOP_JPG, HANDLER:0L, SELECT:0}
  ENDIF
  ;***********************************************************************
  XMANAGER, 'BASE_TOP_JPG', BASE_TOP_JPG, /NO_BLOCK, CLEANUP='ZS_JPG_cleanup'
END

pro ZS_JPG_cleanup, id
	Widget_Control, id, get_uvalue=pstate
	heap_free, pstate
end

PRO ZS_JPG, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
	common_log,'启动长势结果分析'
  ;避免同一个界面重复出现
  IF ( XREGISTERED('BASE_TOP_JPG') NE 0 ) THEN RETURN
  ZS_BASE_TOP_JPG, GROUP_LEADER=WGROUP;, _EXTRA=_VWBEXTRA_
END

pro defaultnames_zsjpg, event
	Widget_Control, event.top, get_uvalue=pstate

	COMMON COMMON_SETPATH,ppath

	v_zs_out_path = (*ppath).zs_out_path

	clf_prefix=(*ppath).clf_prefix
	clf_suffix=(*ppath).clf_suffix

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

  	WIDGET_CONTROL,(*PSTATE).TXT_FILE_CLASSIFIED_RT_JPG, SET_VALUE= v_zs_out_path + clf_prefix + time + clf_suffix
;	WIDGET_CONTROL,(*PSTATE).TXT_JPG_PRO_JPG, SET_VALUE= v_zs_out_path
;	WIDGET_CONTROL,(*PSTATE).TXT_JPG_RT_JPG, SET_VALUE= v_zs_out_path
 end
