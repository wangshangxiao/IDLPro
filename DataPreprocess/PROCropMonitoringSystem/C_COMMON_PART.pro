

;***************************************************************
;GET_COMMON_AERA函数用来获取几个文件的公共区域的坐标
;FILE_1,FILE_2,FILE_3,FILE_4分别表示输入的文件
;函数返回文件公共区域的地理坐标结构体RESULT,
;结构体里面有X_LEFT_COMMON,X_RIGHT_COMMON
;Y_TOP_COMMON,Y_BOTTOM_COMMON四个元素代表公共区域的地理坐标
;***************************************************************
;'_3'用来表示获取三个文件的公共部分
FUNCTION GET_COMMON_AREA_3,FILE_1,FILE_2,FILE_3

	RESULT = {$
		X_LEFT_COMMON:0.0,$
		X_RIGHT_COMMON:0.0,$
		Y_TOP_COMMON:0.0,$
		Y_BOTTOM_COMMON:0.0$
	}

	INFO_FILE_1 = GET_IMAGE_INFO(FILE_1)
	INFO_FILE_2 = GET_IMAGE_INFO(FILE_2)
	INFO_FILE_3 = GET_IMAGE_INFO(FILE_3)
;	INFO_FILE_4 = GET_IMAGE_INFO(FILE_4)

    PRINT,'HHHHH'
;	X_LEFT_FILE_1 = *(GET_IMAGE_INFO(FILE_1)).STARTX
;	X_LEFT_FILE_2 = *(GET_IMAGE_INFO(FILE_2)).STARTX
;	X_LEFT_FILE_3 = *(GET_IMAGE_INFO(FILE_3)).STARTX
;	X_LEFT_FILE_4 = *(GET_IMAGE_INFO(FILE_4)).STARTX
;	Y_TOP_FILE_1 = *(GET_IMAGE_INFO(FILE_1)).STARTY
;	Y_TOP_FILE_2 = *(GET_IMAGE_INFO(FILE_2)).STARTY
;	Y_TOP_FILE_3 = *(GET_IMAGE_INFO(FILE_3)).STARTY
;	Y_TOP_FILE_4 = *(GET_IMAGE_INFO(FILE_4)).STARTY
	;......
	X_LEFT_COMMON = INFO_FILE_1.STARTX>INFO_FILE_2.STARTX>INFO_FILE_3.STARTX;>INFO_FILE_4.STARTX
	X_RIGHT_COMMON = (INFO_FILE_1.STARTX+INFO_FILE_1.XSIZE*INFO_FILE_1.X_PIXELSIZE)	$
     				<(INFO_FILE_2.STARTX+INFO_FILE_2.XSIZE*INFO_FILE_2.X_PIXELSIZE)	$
     				<(INFO_FILE_3.STARTX+INFO_FILE_3.XSIZE*INFO_FILE_3.X_PIXELSIZE)
;     				<(INFO_FILE_4.STARTX+INFO_FILE_4.XSIZE*INFO_FILE_4.X_PIXELSIZE)
    Y_TOP_COMMON = INFO_FILE_1.STARTY<INFO_FILE_2.STARTY<INFO_FILE_3.STARTY;>INFO_FILE_4.STARTY
	 ;经纬度的坐标由下到上是越来越大,所以纵向和横向所用的符号相反
	Y_BOTTOM_COMMON = (INFO_FILE_1.STARTY-INFO_FILE_1.YSIZE*INFO_FILE_1.Y_PIXELSIZE)	$
     				>(INFO_FILE_2.STARTY-INFO_FILE_2.YSIZE*INFO_FILE_2.Y_PIXELSIZE)	$
     				>(INFO_FILE_3.STARTY-INFO_FILE_3.YSIZE*INFO_FILE_3.Y_PIXELSIZE)
;     				>(INFO_FILE_4.STARTY-INFO_FILE_4.YSIZE*INFO_FILE_4.Y_PIXELSIZE)

	RESULT.X_LEFT_COMMON=X_LEFT_COMMON
	RESULT.X_RIGHT_COMMON=X_RIGHT_COMMON
	RESULT.Y_TOP_COMMON=Y_TOP_COMMON
	RESULT.Y_BOTTOM_COMMON=Y_BOTTOM_COMMON

	;......
	RETURN,RESULT

END


;***************************************************************
;本函数GET_LINE_SAMPLE用来获取公共区域文件的起始行列号，总行数，
;总列数，
;参数FILE代表公共区域的文件，COMMON_AREA_INFO是本函数调用
;函数GET_COMMON_AREA里面的参数，
;本函数返回一个结构体GET_RESULT,
;GET_RESULT里面的四个参数分别代表起始行、起始列、总行、总列
;***************************************************************
FUNCTION GET_LINE_SAMPLE,FILE,COMMON_AREA_INFO

	GET_RESULT = {   $
		LINES_START:0,  $
		SAMPLES_START:0,   $
		LINES_TOTAL:0,   $
		SAMPLES_TOTAL:0   $
	}


	INFO_IMAGE = GET_IMAGE_INFO(FILE)
	;PRINT,INFO_IMAGE
	SAMPLES_START = (COMMON_AREA_INFO.X_LEFT_COMMON - INFO_IMAGE.STARTX)/INFO_IMAGE.X_PIXELSIZE
	;PRINT,SAMPLES_START
	LINES_START = ABS(COMMON_AREA_INFO.Y_TOP_COMMON-INFO_IMAGE.STARTY)/INFO_IMAGE.Y_PIXELSIZE
	;PRINT,LINES_START
	SAMPLES_TOTAL = (COMMON_AREA_INFO.X_RIGHT_COMMON-COMMON_AREA_INFO.X_LEFT_COMMON)/INFO_IMAGE.X_PIXELSIZE
	;PRINT,SAMPLES_TOTAL
	LINES_TOTAL =(COMMON_AREA_INFO.Y_TOP_COMMON -COMMON_AREA_INFO.Y_BOTTOM_COMMON)/INFO_IMAGE.Y_PIXELSIZE
	;PRINT,LINES_TOTAL

	GET_RESULT.LINES_START = LINES_START
	GET_RESULT.SAMPLES_START = SAMPLES_START
	GET_RESULT.LINES_TOTAL = LINES_TOTAL
	GET_RESULT.SAMPLES_TOTAL = SAMPLES_TOTAL

	RETURN,GET_RESULT

END