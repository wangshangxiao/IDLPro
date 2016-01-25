

;*****************从数据库中读取数据,返回为字符数组********************************
FUNCTION NQ_SJ_GetdataFromDB_Str,SQLstr	 $      	;SQL语句.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;从数据库中提取数据时的缓存数量,为整型值
							 ,N_RECORDS = NumReocrd	 $ 	;得到记录条数
							 ,Column_Name=columnname $
							 ,N_columns = Clomun

	 ;该函数调用形式为:
	 ;result = NQ_SJ_GetdataFromDB_Str(SQLstr,Num_BUFFERS = Num,N_RECORDS = var,column_name=var1,N_columns=var2)
	 ;结果有两种:区别函数DC_GetDataFromDB()
	 ;(1) 返回查询到的数据,为字符型数组.
	 ;(2) 返回为一行空值(个数为列值)当查询不成功时.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 100		;默认缓存数量为10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) as Num from ('+SQLstr+')')
	  RecordNumOBJ2 = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=SQLstr)

	  if (obj_valid(RecordNumOBJ) eq 0) or (obj_valid(RecordNumOBJ2) eq 0) then begin

			NumReocrd = 0
			return,''
	  endif

	 ;此处没用"RecordNumOBJ->MoveCursor(/FIRST)",是因为它一定有且只有一条记录.类型为结构体
	  N_Record = RecordNumOBJ->GetRecord()
	  a=RecordNumOBJ2->nfields()
	  IF N_Record.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ
;	      IF ARG_PRESENT(Clomun) THEN Clomun = N_Record.(0)
          IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = N_Record.(0)	      	;值为0,即N_Record.(0)为0

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
	   ColumnNum  = DataRecOBJ->NFields()   ;此处的ColumnNum = Columns,这里没有用到,防空查询的情况
	   N = ColumnNum  & DATA = STRARR(N,1) & RecordNum=0L
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN

	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()       ;得到相应县或区划的相应年份的产量********
				 FOR i=0,N-1 DO Temp[i,0] = DataValue.(i)
	             	DATA=[[DATA],[Temp]]				;进行列串接
	             ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;去掉首行,得到全部数据
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


FUNCTION NQ_SJ_GetdataFromDB_Str_gai,SQLstr	 $      	;SQL语句.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;从数据库中提取数据时的缓存数量,为整型值
							 ,N_RECORDS = NumReocrd	 $ 	;得到记录条数
							 ,Column_Name=columnname $
							 ,N_columns = Clomun

	 ;该函数调用形式为:
	 ;result = NQ_SJ_GetdataFromDB_Str(SQLstr,Num_BUFFERS = Num,N_RECORDS = var,column_name=var1,N_columns=var2)
	 ;结果有两种:区别函数DC_GetDataFromDB()
	 ;(1) 返回查询到的数据,为字符型数组.
	 ;(2) 返回为一行空值(个数为列值)当查询不成功时.


	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;默认缓存数量为10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+SQLstr+')')

	  RecordNumOBJ2 = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=SQLstr)

	 ;此处没用"RecordNumOBJ->MoveCursor(/FIRST)",是因为它一定有且只有一条记录.类型为结构体

      N_Record = RecordNumOBJ->GetRecord()
	  a=RecordNumOBJ2->nfields()

	  IF N_Record.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ

          IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = N_Record.(0)	      	;值为0,即N_Record.(0)为0

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
		   ColumnNum  = DataRecOBJ->NFields()   ;此处的ColumnNum = Columns,这里没有用到,防空查询的情况
		   N = ColumnNum  & DATA = STRARR(N,1) & RecordNum=0L
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN
	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()       ;得到相应县或区划的相应年份的产量********

				 FOR i=0,N-1 DO begin
					 	test = DataRecOBJ->GetField(i, is_null=is_null)
					 	if is_null eq 0 then begin
					   	    Temp[i,0] = DataValue.(i)
                        endif else begin
 					    	Temp[i,0] = ''
                        endelse

		        endfor
				 		DATA=[[DATA],[Temp]]				;进行列串接.
	             ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;去掉首行,得到全部数据
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




FUNCTION NQ_SJ_GetdataFromDB_gai,Sql	 $    		      		;SQL语句.字符串
					    ,Num_BUFFERS = Num_BUFFERS $  	;从数据库中提取数据时的缓存数量,为整型值
						,N_RECORDS = NumReocrd $	  		;得到记录条数
						,Column_Name=columnname $
						,N_columns = Clomun
	 ;该函数调用形式为:
	 ;result = GetDataFromDB(Sql,Num_BUFFERS = Num,N_RECORDS = var)
	 ;结果有两种:
	 ;(1) 返回查询到的数据,为结构体数组
	 ;(2) 返回空值当查询不成功时.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 30		;默认缓存数量为10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	 ;此处没用"RecordNumOBJ->MoveCursor(/FIRST)",是因为它一定有且只有一条记录.类型为结构体
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
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)		;值为0,即RecordNum.(0)为0
	      IF ARG_PRESENT(columnname) THEN columnname=Column_Names
	      RETURN,''
	  ENDIF ELSE begin
	      IF ARG_PRESENT(Clomun) THEN Clomun =a
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)	      	;值为0,即N_Record.(0)为0

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
	  DataValue = GetDataOBJ->GetRecord()         ;是为得到一个初始化的结构体,一般是查询结果的第一记录.

	  DATA=REPLICATE(DataValue,RecordNum.(0))    ;生成结构体数组
	  ReIndex = 0L

	   IF (GetDataOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	       REPEAT BEGIN
			DATA[ReIndex] = GetDataOBJ->GetRecord()
			ReIndex = ReIndex+1						;最终的ReIndex应该等于RecordNum.(0)
	       ENDREP UNTIL(GetDataOBJ->MoveCursor(/NEXT) EQ 0)
	   ENDIF


	OBJ_DESTROY,GetDataOBJ

	IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)

	RETURN,DATA

END