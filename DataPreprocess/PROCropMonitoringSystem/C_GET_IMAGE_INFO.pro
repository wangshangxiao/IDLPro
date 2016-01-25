FUNCTION GET_IMAGE_INFO,INFILE

;;  ���Ӻ���������ļ��Ķ�ȡ,���ؽ��Ϊ��¼���ļ���ϢFILEINFO��ͷ�ļ���Ϣ������
;;  ����Ĳ���Ϊ���ļ���
;;------------------------------------���ļ�����-------------------------------------
;;  ������������ж�
	FILEINFO={            $
      DATATYPE	:0		, $
      BANDNUM	:0 		, $
      STARTX	:0.0	, $
      STARTY	:0.0	, $
      XSIZE		:LONG(0)		, $
      YSIZE		:LONG(0)		, $
      X_PIXELSIZE	:DOUBLE(0.0)  	, $
      Y_PIXELSIZE	:DOUBLE(0.0)  	, $
      FILE_RIGHT:1 		  $
      }


    IF (FILE_TEST(INFILE) EQ 0) THEN BEGIN
;       TEXT=DIALOG_MESSAGE('�Ҳ����ļ�:'+INFILE+',����·����!')
       PRINT,'�Ҳ����ļ�:'+INFILE+',����·����!'
       FILEINFO.FILE_RIGHT=0
       RETURN,FILEINFO
    ENDIF

	;�ж�ͷ�ļ��Ĵ�����
    INDEX = STRPOS(INFILE, '.')
    HEAD1 = ''
    IF INDEX EQ -1 THEN BEGIN
      INHEAD=STRTRIM(INFILE,2)+'.HDR'
    ENDIF ELSE BEGIN
      HEAD1 = STRMID(INFILE,0,INDEX)
      INHEAD=HEAD1+'.HDR'
    ENDELSE
    FINDRESULT=FINDFILE(INHEAD)
    IF (FINDRESULT[0] EQ '') THEN BEGIN
;       TEXT=DIALOG_MESSAGE(INHEAD+'ͷ�ļ������ڻ��ļ���ʽ����,����!')
       PRINT,INHEAD+'ͷ�ļ������ڻ��ļ���ʽ����,����!'
       FILEINFO.FILE_RIGHT=0
       RETURN,FILEINFO
    ENDIF

 ;  ��ͷ�ļ�
    OPENR,LUN,INHEAD,/GET_LUN
    RESULT = FSTAT(LUN)
    HEADDATA = BYTARR(RESULT.SIZE-1)
    READU,LUN,HEADDATA
    FREE_LUN,LUN
    HEADFILE=STRING(HEADDATA)
    FREE_LUN,LUN

;   ��ȡ���кţ������������Ͻ�����
    index0 = STRPOS(headfile, 'samples = ')
    index1 = STRPOS(headfile, 'lines   = ')
    index2 = STRPOS(headfile, 'bands   = ')
    index3 = STRPOS(headfile, 'header offset = ')
    index4 = STRpos(headfile, 'map info = ')
    index5 = STRpos(headfile, 'projection info = ')
    index6 = STRPOS(headfile, 'data type = ')
    index7 = STRPOS(headfile, 'interleave = ')
    index8 = STRPOS(headfile, 'pixel size = ')
    samples = STRMID(headfile,index0+10,index1-index0-10-1)
    lines =  STRMID(headfile,index1+10,index2-index1-10-1)
    bands=STRMID(headfile,index2+10,index3-index2-10-1)
    data_type=STRMID(headfile,index6+12,index7-index6-12-1)

    ;; ��ʼ������Ļ�ȡ
    INDEX_TEMP=STRPOS(HEADFILE,'}',INDEX4)
    info=strmid(headfile,index4+11,INDEX_TEMP)
    ;����������ͷ�ļ���MAP_INFO

	;һ�λ��һ��','��λ��,ͨ����Щ','���������ݵĶ�ȡ
    INDEX_TEMP_1=STRPOS(INFO,',')
    INDEX_TEMP_2=STRPOS(INFO,',',INDEX_TEMP_1+1)
    INDEX_TEMP_3=STRPOS(INFO,',',INDEX_TEMP_2+1)
    INDEX_TEMP_4=STRPOS(INFO,',',INDEX_TEMP_3+1)
    INDEX_TEMP_5=STRPOS(INFO,',',INDEX_TEMP_4+1)
    INDEX_TEMP_6=STRPOS(INFO,',',INDEX_TEMP_5+1)
    INDEX_TEMP_7=STRPOS(INFO,',',INDEX_TEMP_6+1)
    X_START		=STRMID(INFO,INDEX_TEMP_3+1,INDEX_TEMP_4-INDEX_TEMP_3-1)
    Y_START		=STRMID(INFO,INDEX_TEMP_4+1,INDEX_TEMP_5-INDEX_TEMP_4-1)
    X_PIXEL_SIZE=STRMID(INFO,INDEX_TEMP_5+1,INDEX_TEMP_6-INDEX_TEMP_5-1)
    Y_PIXEL_SIZE=STRMID(INFO,INDEX_TEMP_6+1,INDEX_TEMP_7-INDEX_TEMP_6-1)
;    PRINT,X_START
;    PRINT,Y_START
;    PRINT,X_PIXEL_SIZE
;    PRINT,Y_PIXEL_SIZE
;
;   ����ȡ����Ϣת��Ϊ����
    XSIZE=LONG(SAMPLES)
    YSIZE=LONG(LINES)
    BANDNUM=FIX(BANDS)
    DATATP=FIX(DATA_TYPE)
    ULX=DOUBLE(X_START)
    ULY=DOUBLE(Y_START)
    X_PIXELSIZE=DOUBLE(X_PIXEL_SIZE)
    Y_PIXELSIZE=DOUBLE(Y_PIXEL_SIZE)

;;  �����������Ͷ�������
PRINT,'DATATP',DATATP
    CASE DATATP OF
      1: BEGIN
             ;INDATA=BYTARR(XSIZE,YSIZE,BANDNUM)
             TPSIZE=1
         END
      2: BEGIN
             ;INDATA=INTARR(XSIZE,YSIZE,BANDNUM)
             TPSIZE=2
         END
      4: BEGIN
             ;INDATA=FLTARR(XSIZE,YSIZE,BANDNUM)
             TPSIZE=4
         END
      12:BEGIN
             ;INDATA=UINTARR(XSIZE,YSIZE,BANDNUM)
             TPSIZE=2
         END
      13:BEGIN
             ;INDATA=ULONARR(XSIZE,YSIZE,BANDNUM)
             TPSIZE=4
         END
      ELSE:BEGIN
;             TEXT=DIALOG_MESSAGE(DATA_TYPE+':û�ж������������!')
             PRINT,INFILE+'@@'+DATA_TYPE+':û�ж������������!'
             FILEINFO.FILE_RIGHT=0
       		 RETURN,FILEINFO
         END
    ENDCASE


    IF((FILE_INFO(INFILE)).SIZE NE long64(XSIZE)*YSIZE*TPSIZE) THEN BEGIN
    	PRINT,INFILE+'ͷ�ļ���Ϣ���ļ���С��һ�£�����!'
    	FILEINFO.FILE_RIGHT=0
    	; PRINT,'A'
       	RETURN,FILEINFO
    ENDIF

    IF(bandnum NE 1) THEN BEGIN
    	FILEINFO.FILE_RIGHT=0
       	RETURN,FILEINFO
    ENDIF
    FILEINFO.DATATYPE	=DATATP
    FILEINFO.BANDNUM	=BANDNUM
    FILEINFO.STARTX		=ULX
    FILEINFO.STARTY		=ULY
    FILEINFO.XSIZE		=XSIZE
    FILEINFO.YSIZE		=YSIZE
    FILEINFO.X_PIXELSIZE=X_PIXELSIZE
    FILEINFO.Y_PIXELSIZE=Y_PIXELSIZE
;    HELP,FILEINFO,/STRUCT

    RETURN,FILEINFO
END