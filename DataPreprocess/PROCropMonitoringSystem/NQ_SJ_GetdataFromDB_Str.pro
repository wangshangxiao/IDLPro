

;*****************�����ݿ��ж�ȡ����,����Ϊ�ַ�����********************************
FUNCTION NQ_SJ_GetdataFromDB_Str,SQLstr	 $      	;SQL���.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;�����ݿ�����ȡ����ʱ�Ļ�������,Ϊ����ֵ
							 ,N_RECORDS = NumReocrd	 $ 	;�õ���¼����
							 ,Column_Name=columnname $
							 ,N_columns = Clomun

	 ;�ú���������ʽΪ:
	 ;result = NQ_SJ_GetdataFromDB_Str(SQLstr,Num_BUFFERS = Num,N_RECORDS = var,column_name=var1,N_columns=var2)
	 ;���������:������DC_GetDataFromDB()
	 ;(1) ���ز�ѯ��������,Ϊ�ַ�������.
	 ;(2) ����Ϊһ�п�ֵ(����Ϊ��ֵ)����ѯ���ɹ�ʱ.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 100		;Ĭ�ϻ�������Ϊ10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) as Num from ('+SQLstr+')')
	  RecordNumOBJ2 = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=SQLstr)

	  if (obj_valid(RecordNumOBJ) eq 0) or (obj_valid(RecordNumOBJ2) eq 0) then begin

			NumReocrd = 0
			return,''
	  endif

	 ;�˴�û��"RecordNumOBJ->MoveCursor(/FIRST)",����Ϊ��һ������ֻ��һ����¼.����Ϊ�ṹ��
	  N_Record = RecordNumOBJ->GetRecord()
	  a=RecordNumOBJ2->nfields()
	  IF N_Record.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ
;	      IF ARG_PRESENT(Clomun) THEN Clomun = N_Record.(0)
          IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = N_Record.(0)	      	;ֵΪ0,��N_Record.(0)Ϊ0

          RecordNumOBJ2->getproperty,FIELD_INFO =fieldinfo
          Column_Names=strarr(1,1)
                for i=0,Clomun-1 do begin
                   Column_Names=[Column_Names,strTRIM(fieldinfo(i).field_name,2)]
                endfor
          Column_Names=Column_Names[1:*]
	      IF ARG_PRESENT(columnname) THEN columnname=Column_Names
	      RETURN,''
	  ENDIF ELSE begin
	  OBJ_DESTROY,RecordNumOBJ
	  OBJ_DESTROY,RecordNumOBJ2
      endelse

	   DataRecOBJ = OBJ_NEW('IDLdbRecordset',DBobj,SQL=SQLstr,N_BUFFERS=Num_BUFFERS)
	   ColumnNum  = DataRecOBJ->NFields()   ;�˴���ColumnNum = Columns,����û���õ�,���ղ�ѯ�����
	   N = ColumnNum  & DATA = STRARR(N,1) & RecordNum=0L
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN

	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()       ;�õ���Ӧ�ػ���������Ӧ��ݵĲ���********
				 FOR i=0,N-1 DO Temp[i,0] = DataValue.(i)
	             	DATA=[[DATA],[Temp]]				;�����д���
	             ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;ȥ������,�õ�ȫ������
	       ENDIF

     DataRecOBJ->getproperty,FIELD_INFO =a
     temp1=strarr(1,1)
     for i=0,ColumnNum-1 do begin
          temp1=[temp1,strTRIM(a(i).field_name,2)]
     endfor
          temp1=temp1[1:*]

	   OBJ_DESTROY,DataRecOBJ

	   IF ARG_PRESENT(Clomun) THEN Clomun = ColumnNum

	   IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum
 	   IF ARG_PRESENT(columnname) THEN columnname = temp1
	   DATA = STRTRIM(TEMPORARY(DATA),2)
	   RETURN,DATA
END


FUNCTION NQ_SJ_GetdataFromDB_Str_gai,SQLstr	 $      	;SQL���.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;�����ݿ�����ȡ����ʱ�Ļ�������,Ϊ����ֵ
							 ,N_RECORDS = NumReocrd	 $ 	;�õ���¼����
							 ,Column_Name=columnname $
							 ,N_columns = Clomun

	 ;�ú���������ʽΪ:
	 ;result = NQ_SJ_GetdataFromDB_Str(SQLstr,Num_BUFFERS = Num,N_RECORDS = var,column_name=var1,N_columns=var2)
	 ;���������:������DC_GetDataFromDB()
	 ;(1) ���ز�ѯ��������,Ϊ�ַ�������.
	 ;(2) ����Ϊһ�п�ֵ(����Ϊ��ֵ)����ѯ���ɹ�ʱ.


	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;Ĭ�ϻ�������Ϊ10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+SQLstr+')')

	  RecordNumOBJ2 = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=SQLstr)

	 ;�˴�û��"RecordNumOBJ->MoveCursor(/FIRST)",����Ϊ��һ������ֻ��һ����¼.����Ϊ�ṹ��

      N_Record = RecordNumOBJ->GetRecord()
	  a=RecordNumOBJ2->nfields()

	  IF N_Record.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ

          IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = N_Record.(0)	      	;ֵΪ0,��N_Record.(0)Ϊ0

          RecordNumOBJ2->getproperty,FIELD_INFO =fieldinfo
          Column_Names=strarr(1,1)
                for i=0,Clomun-1 do begin
                   Column_Names=[Column_Names,strTRIM(fieldinfo(i).field_name,2)]
                endfor
          Column_Names=Column_Names[1:*]
	      IF ARG_PRESENT(columnname) THEN columnname=Column_Names
	      RETURN,''
	  ENDIF ELSE begin

		  OBJ_DESTROY,RecordNumOBJ
		  OBJ_DESTROY,RecordNumOBJ2
      endelse
		   DataRecOBJ = OBJ_NEW('IDLdbRecordset',DBobj,SQL=SQLstr,N_BUFFERS=Num_BUFFERS)
		   ColumnNum  = DataRecOBJ->NFields()   ;�˴���ColumnNum = Columns,����û���õ�,���ղ�ѯ�����
		   N = ColumnNum  & DATA = STRARR(N,1) & RecordNum=0L
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN
	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()       ;�õ���Ӧ�ػ���������Ӧ��ݵĲ���********

				 FOR i=0,N-1 DO begin
					 	test = DataRecOBJ->GetField(i, is_null=is_null)
					 	if is_null eq 0 then begin
					   	    Temp[i,0] = DataValue.(i)
                        endif else begin
 					    	Temp[i,0] = ''
                        endelse

		        endfor
				 		DATA=[[DATA],[Temp]]				;�����д���.
	             ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;ȥ������,�õ�ȫ������
	       ENDIF

     DataRecOBJ->getproperty,FIELD_INFO =a
     temp1=strarr(1,1)
     for i=0,ColumnNum-1 do begin
          temp1=[temp1,strTRIM(a(i).field_name,2)]
     endfor
          temp1=temp1[1:*]

	   OBJ_DESTROY,DataRecOBJ

	   IF ARG_PRESENT(Clomun) THEN Clomun = ColumnNum

	   IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum
 	   IF ARG_PRESENT(columnname) THEN columnname = temp1
	   DATA = STRTRIM(TEMPORARY(DATA),2)

	   RETURN,DATA

END




FUNCTION NQ_SJ_GetdataFromDB_gai,Sql	 $    		      		;SQL���.�ַ���
					    ,Num_BUFFERS = Num_BUFFERS $  	;�����ݿ�����ȡ����ʱ�Ļ�������,Ϊ����ֵ
						,N_RECORDS = NumReocrd $	  		;�õ���¼����
						,Column_Name=columnname $
						,N_columns = Clomun
	 ;�ú���������ʽΪ:
	 ;result = GetDataFromDB(Sql,Num_BUFFERS = Num,N_RECORDS = var)
	 ;���������:
	 ;(1) ���ز�ѯ��������,Ϊ�ṹ������
	 ;(2) ���ؿ�ֵ����ѯ���ɹ�ʱ.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 30		;Ĭ�ϻ�������Ϊ10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	 ;�˴�û��"RecordNumOBJ->MoveCursor(/FIRST)",����Ϊ��һ������ֻ��һ����¼.����Ϊ�ṹ��
	  RecordNum = RecordNumOBJ->GetRecord()
	  RecordNumOBJ2 = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=Sql)

	  a=RecordNumOBJ2->nfields()

      RecordNumOBJ2->getproperty,FIELD_INFO =fieldinfo
      Column_Names=strarr(1,1)
            for i=0,a-1 do begin
               Column_Names=[Column_Names,strTRIM(fieldinfo(i).field_name,2)]
            endfor
      Column_Names=Column_Names[1:*]




	  IF RecordNum.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ
	  	  IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)		;ֵΪ0,��RecordNum.(0)Ϊ0
	      IF ARG_PRESENT(columnname) THEN columnname=Column_Names
	      RETURN,''
	  ENDIF ELSE begin
	      IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)	      	;ֵΪ0,��N_Record.(0)Ϊ0

;          RecordNumOBJ2->getproperty,FIELD_INFO =fieldinfo
;          Column_Names=strarr(1,1)
;                for i=0,Clomun-1 do begin
;                   Column_Names=[Column_Names,strTRIM(fieldinfo(i).field_name,2)]
;                endfor
;          Column_Names=Column_Names[1:*]
	      IF ARG_PRESENT(columnname) THEN columnname=Column_Names

	  OBJ_DESTROY,RecordNumOBJ
	  OBJ_DESTROY,RecordNumOBJ2

      endelse



	  GetDataOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=Sql,N_BUFFERS=Num_BUFFERS)
	  DataValue = GetDataOBJ->GetRecord()         ;��Ϊ�õ�һ����ʼ���Ľṹ��,һ���ǲ�ѯ����ĵ�һ��¼.

	  DATA=REPLICATE(DataValue,RecordNum.(0))    ;���ɽṹ������
	  ReIndex = 0L

	   IF (GetDataOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	       REPEAT BEGIN
			DATA[ReIndex] = GetDataOBJ->GetRecord()
			ReIndex = ReIndex+1						;���յ�ReIndexӦ�õ���RecordNum.(0)
	       ENDREP UNTIL(GetDataOBJ->MoveCursor(/NEXT) EQ 0)
	   ENDIF


	OBJ_DESTROY,GetDataOBJ

	IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)

	RETURN,DATA

END