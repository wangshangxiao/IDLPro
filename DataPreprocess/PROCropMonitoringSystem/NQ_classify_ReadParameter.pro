FUNCTION c_fjReadParameter,district   ;����districtΪ����

	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    OPENR,lun,'land\parametersetting.txt',/GET_LUN
    FileInfo = FSTAT(lun)
    paradata = BYTARR(FileInfo.SIZE)
    READU,lun,paradata
    FREE_LUN,lun

    index = STRPOS(paradata, district)           ;������ѡ������ز����Ƿ���.
    IF index EQ -1 THEN RETURN,''

    enter=string(byte(13))+string(byte(10))   					  ;�س���ASCII��ֵ

    index0 = STRPOS(paradata, 'samples = ',index)
    index1 = STRPOS(paradata, 'lines   = ',index)
    index2 = STRPOS(paradata, 'resolution = ',index)
    index7 = STRPOS(paradata,'central_meridian = ',index)      ;������"index7",�����ӵ�,û�д�.
    index3 = STRPOS(paradata, 'albers_ul = {',index)
    index4 = STRPOS(paradata, '}',index)
    index5 = STRPOS(paradata, 'stationtable = ',index)
    index6 = STRPOS(paradata, enter,index5)   					 ;ע��˴���index5.

	ULinfo = STRMID(paradata,index3+13,index4-index3-13)
	UlX_Y_postion = STRSPLIT(ULinfo,',',ESCAPE=' ',/EXTRACT )
	UlX = UlX_Y_postion[0]                       			;���Ͻ�X����
	UlY = UlX_Y_postion[1]									;���Ͻ�Y����

	CenMeridian = STRMID(paradata,index7+19,index3-index7-19-2)  ;���������Ա�ʶ��105,����110
	samples     = STRMID(paradata,index0+10,index1-index0-10-2)  ;��ȥ2,����Ϊ�س���ռ2���ֽڳ���.
    lines       = STRMID(paradata,index1+10,index2-index1-10-2)
    resolution  = STRMID(paradata,index2+13,index7-index2-13-2)

    IF index6 NE -1 THEN  BEGIN            					;�Է�����ʱû�лس���.
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