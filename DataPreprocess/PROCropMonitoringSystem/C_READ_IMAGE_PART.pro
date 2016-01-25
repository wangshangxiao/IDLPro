;***********************************************************************************
;����������ȡһ��ͼ���һ����,Ŀǰֻ�Ե����ε����ݽ��д���,����Ĳ���˵������:
;FILE_NAME	:�ļ�������,�ļ�Ϊһ��ENVI STANDARD ��ʽ�ļ�,Ҫ��ͷ�ļ�
;ROW_START	:��ʼ���к�
;COL_START	:��ʼ���к�
;LINES		:Ҫ��ȡ������
;SAMPLES	:Ҫ��ȡ������
;�����ķ���ֵΪһ���ṹ��
;***********************************************************************************
FUNCTION READ_IMAGE_PART,FILE_IN,ROW_START,COL_START,LINES,SAMPLES

	;(1)������Ҫ���صĽṹ��,����������ݽ��м���
	RESULT_STRUCT={   $
      		SUCCESS		:	1			, $	;���ݵĶ�ȡ�Ƿ�ɹ�
      		P_DATA		:	PTR_NEW()	, $ ;ָ��,ָ���ȡ������(����)
      		LINES_REAL	:	0			, $	;ʵ�ʶ�ȡ�����ݵ�����
      		SAMPLES_REAL:	0      }		;ʵ�ʶ�ȡ�����ݵ�����
	;���������ݽ��м���
    IF ((LINES LE 0) OR(SAMPLES LE 0)) THEN BEGIN
		PRINT,'��ʼ���겻��Ϊ��ֵ'
		RESULT_STRUCT.SUCCESS=0
		RETURN,RESULT_STRUCT
    ENDIF

	;(2)���ȼ���ļ��Ƿ����
    IF (FILE_TEST(FILE_IN) EQ 0) THEN BEGIN
		PRINT,'�Ҳ����ļ�:'+FILE_IN+',����·����!'
		RESULT_STRUCT.SUCCESS=0
		RETURN,RESULT_STRUCT
    ENDIF

    ;(3)�ټ��ͷ�ļ��Ƿ����,��һЩ��Ϣ��Ҫ��ͷ�ļ��л��
    INDEX = STRPOS(FILE_IN, '.')
    HEAD1 = ''
    IF INDEX EQ -1 THEN BEGIN
      	INHEAD=STRTRIM(FILE_IN,2)+'.HDR'
    ENDIF ELSE BEGIN
      	HEAD1 = STRMID(FILE_IN,0,INDEX)
      	INHEAD=HEAD1+'.HDR'
    ENDELSE
    FINDRESULT=FINDFILE(INHEAD)
    IF (FINDRESULT[0] EQ '') THEN BEGIN
       	PRINT,INHEAD+'ͷ�ļ������ڻ��ļ���ʽ����,����!'
       	RESULT_STRUCT.SUCCESS=0
       	RETURN,RESULT_STRUCT
    ENDIF

 	;(4)��ͷ�ļ��ж�ȡ��Ҫ����Ϣ
    OPENR,LUN,INHEAD,/GET_LUN
    RESULT = FSTAT(LUN)
    HEADDATA = BYTARR(RESULT.SIZE-1)
    READU,LUN,HEADDATA
    FREE_LUN,LUN
    HEADFILE=STRING(HEADDATA)
    FREE_LUN,LUN

	;��ȡ���кţ������������Ͻ�����
    INDEX0 = STRPOS(HEADFILE, 'samples = ')
    INDEX1 = STRPOS(HEADFILE, 'lines   = ')
    INDEX2 = STRPOS(HEADFILE, 'bands   = ')
    INDEX3 = STRPOS(HEADFILE, 'header offset = ')
    INDEX4 = STRPOS(HEADFILE, 'map info = ')
    INDEX5 = STRPOS(HEADFILE, 'projection info = ')
    INDEX6 = STRPOS(HEADFILE, 'data type = ')
    INDEX7 = STRPOS(HEADFILE, 'interleave = ')
    INDEX8 = STRPOS(HEADFILE, 'pixel size = ')
    SAMPLES_FILE 	= STRMID(HEADFILE,INDEX0+10,INDEX1-INDEX0-10-1)
    SAMPLES_FILE=LONG(SAMPLES_FILE)
    LINES_FILE 		=  STRMID(HEADFILE,INDEX1+10,INDEX2-INDEX1-10-1)
    LINES_FILE=LONG(LINES_FILE)
    BANDS=STRMID(HEADFILE,INDEX2+10,INDEX3-INDEX2-10-1)
    DATA_TYPE=STRMID(HEADFILE,INDEX6+12,INDEX7-INDEX6-12-1)
    ;��ʼ������Ļ�ȡ
    INFO=STRMID(HEADFILE,INDEX4+11,INDEX5-INDEX4-13)
    INDEX41= STRPOS(INFO,',')
    INFO2=STRMID(HEADFILE,INDEX4+11+INDEX41+1,INDEX5-(INDEX4+11+INDEX41)-3)
    INDEX42= STRPOS(INFO2,',')
    INFO3=STRMID(HEADFILE,INDEX4+11+INDEX41+1+INDEX42+1,INDEX5-(INDEX4+11+INDEX41+1+INDEX42)-3)
    INDEX43= STRPOS(INFO3,',')
    INFO4=STRMID(HEADFILE,INDEX4+11+INDEX41+1+INDEX42+1+INDEX43+1,INDEX5-(INDEX4+11+INDEX41+1+INDEX42+1+INDEX43)-3)
    INDEX44= STRPOS(INFO4,',')
    INFO5=STRMID(HEADFILE,INDEX4+11+INDEX41+1+INDEX42+1+INDEX43+1+INDEX44+1,INDEX5-(INDEX4+11+INDEX41+1+INDEX42+1+INDEX43+1+INDEX44)-3)
    INDEX45=STRPOS(INFO5,',')
    SULX=STRMID(INFO4,0,INDEX44)
    SULY=STRMID(INFO5,0,INDEX45)
    ;��Ԫ��С�Ļ�ȡ,����X��Y��һ����С������÷ֱ���ȡX��Y�Ĵ�С
    INFO80=STRMID(HEADFILE,INDEX8+14,30)
    INDEX81=STRPOS(INFO80,',')
    PIXEL_SIZE=STRMID(HEADFILE,INDEX8+14,INDEX81)

	;����ȡ����Ϣת��Ϊ����
    X_SIZE=LONG(SAMPLES_FILE)
    Y_SIZE=LONG(LINES_FILE)
    BANDNUM=LONG(BANDS)
    DATATP=LONG(DATA_TYPE)
    ULX=LONG(SULX)
    ULY=LONG(SULY)
    PIXELSIZE=FLOAT(PIXEL_SIZE)

	;(5)�����ļ�ʵ��Ҫ��ȡ�����ݵĴ�С
	;,������Ϊʵ���ܶ�ȡ���������ݺܿ��ܱ����ȡ����������С
	;HELP,COL_START,X_SIZE
	IF((COL_START GT X_SIZE) OR (ROW_START GT Y_SIZE)) THEN BEGIN

		PRINT,'Ҫ��ȡ�����ݷ�Χȫ����ͼ������'
		RESULT_STRUCT.SUCCESS=0
        RETURN,RESULT_STRUCT
	ENDIF
	IF(COL_START+SAMPLES-1 GT X_SIZE) THEN BEGIN
		SAMPLES=X_SIZE-COL_START+1
	ENDIF
	IF(ROW_START+LINES-1 GT Y_SIZE) THEN BEGIN
		LINES=Y_SIZE-ROW_START+1
	ENDIF

	;(6)�������ݵ����ͽ�������Ķ���
    CASE DATATP OF
      1: BEGIN
             INDATA		=BYTARR(SAMPLES,LINES)	;��������󷵻ص�����
             DATA_LINE	=BYTARR(SAMPLES)		;������һ�е�����
             TPSIZE		=1
         END
      2: BEGIN
             INDATA		=INTARR(SAMPLES,LINES)
             DATA_LINE	=INTARR(SAMPLES)
             TPSIZE		=2
         END
      4: BEGIN
             INDATA		=FLTARR(SAMPLES,LINES)
             DATA_LINE	=FLTARR(SAMPLES)
             TPSIZE		=4
         END
      12:BEGIN
             INDATA		=UINTARR(SAMPLES,LINES)
             DATA_LINE	=UINTARR(SAMPLES)
             TPSIZE		=2
         END
      13:BEGIN
             INDATA		=ULONARR(SAMPLES,LINES)
             DATA_LINE	=ULONARR(SAMPLES)
             TPSIZE		=4
         END
      ELSE:BEGIN
             PRINT,FILE_IN+'@@'+DATA_TYPE+':û�ж������������!'
             RESULT_STRUCT.SUCCESS=0
             RETURN,RESULT_STRUCT
         END
    ENDCASE

	;(7)���ļ�,���������ݵĶ�ȡ
    OPENR,LUN,FILE_IN,/GET_LUN
    ;�����ﻹ�Զ������ݵĴ�С��������֤
    FILESIZE=FSTAT(LUN)
    IF FILESIZE.SIZE NE LONG(X_SIZE)*Y_SIZE*TPSIZE THEN BEGIN
       	;PRINT,FILE_IN+'ͷ�ļ���Ϣ���ļ���С��һ�£�����!'
       	RESULT_STRUCT.SUCCESS=0
        RETURN,RESULT_STRUCT
    END
    FILESIZE=0

    ;�����ļ���ʼλ�õļ���
    ;!!!!ROW_START�Ǵ��㿪ʼ��
    POSITION=((LONG64(ROW_START))*SAMPLES_FILE+COL_START-1+1)*TPSIZE
    POSITION=POSITION-SAMPLES_FILE*TPSIZE

    ;HELP,POSITION
    ;���������Ҫ��Ϊ�˵ڴ�ѭ�������Լ�SAMPLES
	;HELP,INDATA,DATA_LINE,TPSIZE
    FOR I=0,LINES-1 DO BEGIN
    	IF(I EQ 1934) THEN PRINT,'AAAAAA',POSITION
    	POSITION=POSITION+SAMPLES_FILE*TPSIZE
    	POINT_LUN,LUN,POSITION
    	READU,LUN,DATA_LINE
    	INDATA[*,I]=DATA_LINE[0:SAMPLES-1]
    ENDFOR

    FREE_LUN,LUN
    RESULT_STRUCT.P_DATA=PTR_NEW(INDATA)
    ;PRINT,'HERE'
    RETURN,RESULT_STRUCT

END