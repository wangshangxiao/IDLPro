FUNCTION nq_readparameter,district      ;����districtΪ����(�ַ���)

	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller
	ParaFile = 'text\NQ_parametersetting.txt'
    IF NOT FILE_TEST(ParaFile) THEN BEGIN
       INFO = DIALOG_MESSAGE('ϵͳ��װĿ¼���Ҳ��������ļ�NQ_parametersetting.txt!',TITLE='����')
       RETURN,''
	ENDIF


    OPENR,lun,ParaFile,/GET_LUN
    FileInfo = FSTAT(lun)
    paradata = BYTARR(FileInfo.SIZE)
    READU,lun,paradata
    FREE_LUN,lun

    index = STRPOS(paradata, district)           ;������ѡ������ز����Ƿ���.
    IF index EQ -1 THEN RETURN,''

    enter=string(byte(13))+string(byte(10))   					  ;�س���ASCII��ֵ

    index0 = STRPOS(paradata, 'ulx = ',index)
    index1 = STRPOS(paradata, 'uly = ',index)
    index2 = STRPOS(paradata, 'rlx = ',index)
    index3 = STRPOS(paradata, 'rly = ',index)
    index4 = STRPOS(paradata, 'shppath = ',index)
    index5 = STRPOS(paradata, 'gridpath = ',index)
    index6 = STRPOS(paradata, enter,index5)

    ulx     = STRMID(paradata,index0+6,index1-index0-6-2)
    uly     = STRMID(paradata,index1+6,index2-index1-6-2)
    lrx     = STRMID(paradata,index2+6,index3-index2-6-2)
    lry     = STRMID(paradata,index3+6,index4-index3-6-2)
    shppath = STRMID(paradata,index4+10,index5-index4-10-2)

    IF index6 NE -1 THEN  BEGIN            					;�Է�����ʱû�лس���.
   	   gridpath= STRMID(paradata,index5+11,index6-index5-11)
   	ENDIF ELSE gridpath= STRMID(paradata,index5+11)





;    gridpath= STRMID(paradata,index5+11,index6-index5-11-2)

  	Result = {ulx	:	ulx ,$
			  uly		:	uly ,$
			  lrx			:	lrx ,$
			  lry	:   lry ,$
			  shppath			:	shppath,$
			  gridpath	:	gridpath}

	RETURN,Result

END