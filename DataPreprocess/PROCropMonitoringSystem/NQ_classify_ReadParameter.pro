FUNCTION c_fjReadParameter,district   ;变量district为区域

	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    OPENR,lun,'land\parametersetting.txt',/GET_LUN
    FileInfo = FSTAT(lun)
    paradata = BYTARR(FileInfo.SIZE)
    READU,lun,paradata
    FREE_LUN,lun

    index = STRPOS(paradata, district)           ;查找所选区域相关参数是否有.
    IF index EQ -1 THEN RETURN,''

    enter=string(byte(13))+string(byte(10))   					  ;回车符ASCII码值

    index0 = STRPOS(paradata, 'samples = ',index)
    index1 = STRPOS(paradata, 'lines   = ',index)
    index2 = STRPOS(paradata, 'resolution = ',index)
    index7 = STRPOS(paradata,'central_meridian = ',index)      ;这里是"index7",后来加的,没有错.
    index3 = STRPOS(paradata, 'albers_ul = {',index)
    index4 = STRPOS(paradata, '}',index)
    index5 = STRPOS(paradata, 'stationtable = ',index)
    index6 = STRPOS(paradata, enter,index5)   					 ;注意此处是index5.

	ULinfo = STRMID(paradata,index3+13,index4-index3-13)
	UlX_Y_postion = STRSPLIT(ULinfo,',',ESCAPE=' ',/EXTRACT )
	UlX = UlX_Y_postion[0]                       			;左上角X坐标
	UlY = UlX_Y_postion[1]									;左上角Y坐标

	CenMeridian = STRMID(paradata,index7+19,index3-index7-19-2)  ;中央子午以标识是105,还是110
	samples     = STRMID(paradata,index0+10,index1-index0-10-2)  ;减去2,是因为回车符占2个字节长度.
    lines       = STRMID(paradata,index1+10,index2-index1-10-2)
    resolution  = STRMID(paradata,index2+13,index7-index2-13-2)

    IF index6 NE -1 THEN  BEGIN            					;以防结束时没有回车符.
   	   stationtable= STRMID(paradata,index5+15,index6-index5-15)
   	ENDIF ELSE stationtable= STRMID(paradata,index5+15)

	Result = {resolution	:	resolution ,$
			  samples		:	samples ,$
			  lines			:	lines ,$
			  CenMeridian	:   CenMeridian ,$
			  UlX			:	UlX,$
			  UlY			:	UlY,$
			  stationtable	:	stationtable}

	RETURN,Result
END