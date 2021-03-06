
;说明:DC_A_Pro_Fun.pro
;作用:该文件存放单产预测分析计算中所用的公用函数或过程
;时间:2006.8.18
;作者:徐新刚博士(2004届)
;注意:(1)调用这些过程或函数之前,应保证文件先被编译.
;	  (2)另外,最好不要再改动各函数或过程的顺序,因为它们之间也有被调用关系.

;******自定义过程:当一个窗口消亡时,释放堆变量
PRO DC_CleanAllHeap,TLB
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2

    WIDGET_CONTROL,TLB,GET_UVALUE=PA
    HEAP_FREE,PA   ;
;  	HEAP_GC,/VERBOSE
END
;*******自定义函数:判断字符输入是否正确*************
FUNCTION DC_JudgeInputChar,Inputchar $   	  ;输入的要判数的字符串
 						  ,Desc=Desc $	      ;用户输入的错误提示描述字符前缀.
 						  ,INTEGER=integer $  ;用于判断输入是否只是整型.默认判断输入为浮点型
						  ,NEGATIVE=negative  ;标明输入可以为负,默认输入只能为正
      ;调用形式为: Result = DC_JudgeInputChar(Inputchar,[Desc=Description],[/INTEGER],[/NEGATIVE])
      ;返回值:(1)浮点型或整型值
      ;		  (2)值-1 (输入了非法字符)

       IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	   ON_ERROR,2

       IF NOT KEYWORD_SET(Desc) THEN Desc = ''

       CompareStr=['0','1','2','3','4','5','6','7','8','9','.']     ;输入的字符只在这11个当中.
	   IF KEYWORD_SET(NEGATIVE) THEN BEGIN
       		CompareStr=['-','0','1','2','3','4','5','6','7','8','9','.']     ;输入的字符只在这12个当中.
	   ENDIF

        IF STRLEN(Inputchar) EQ 0 THEN BEGIN                        ;判断没有输入的情况
           Prompt=DIALOG_MESSAGE(Desc+'没有输入值,请先输入!',TITLE='警告')
           RETURN,-1
        ENDIF

  		N=0
  		FOR i=0,STRLEN(Inputchar)-1 DO BEGIN                          ;判断非法字符的情况
           JudgeChar=STRMID(Inputchar,i,1)
           aa = WHERE(CompareStr EQ JudgeChar,Count)
           IF Count EQ 0 THEN BEGIN
              Prompt=DIALOG_MESSAGE(Desc+'输入了非数字字符,请重新设置!',TITLE='警告')
              RETURN,-1
           ENDIF
           IF JudgeChar EQ '.' THEN N=N+1           ;N用于记录小数点的个数
        ENDFOR

       IF KEYWORD_SET(INTEGER) THEN BEGIN
		  IF N NE 0 THEN BEGIN
              Prompt=DIALOG_MESSAGE(Desc+'应输入整型值,请重新设置!',TITLE='警告')
              RETURN,-1
		  ENDIF ELSE BEGIN
              RETURN,LONG(Inputchar)
		  ENDELSE
       ENDIF

       IF N GT 1 THEN BEGIN
          Prompt=DIALOG_MESSAGE(Desc+'输入了多个小数点,请重新设置!',TITLE='警告')
          RETURN,-1
       ENDIF

       IF (STRMID(Inputchar,0,1) EQ '.') THEN BEGIN
		  Inputchar = '0'+Inputchar
          RETURN,FLOAT(Inputchar)
       ENDIF

       RETURN,FLOAT(Inputchar)
 END
;*****************从数据库中读取数据,返回为字符数组********************************
FUNCTION DC_GetdataFromDB_Str,Columns $        	;要得到的数据列数,必须与SQLstr里的一致
							 ,SQLstr	 $      	;SQL语句.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;从数据库中提取数据时的缓存数量,为整型值
							 ,N_RECORDS = NumReocrd	  		;得到记录条数

	 ;该函数调用形式为:
	 ;result = DC_GetdataFromDB_Str(Columns,SQLstr,Num_BUFFERS = Num,N_RECORDS = var)
	 ;结果有两种:区别函数DC_GetDataFromDB()
	 ;(1) 返回查询到的数据,为字符型数组.
	 ;(2) 返回为一行空值(个数为列值)当查询不成功时.

;	  IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
;	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;默认缓存数量为10

	   N = FIX(Columns)
	   DATA = STRARR(N,1)
	   RecordNum=0L
	   IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return,DATA
	ENDIF

	   DataRecOBJ = OBJ_NEW('IDLdbRecordset',DBobj,SQL=SQLstr,N_BUFFERS=Num_BUFFERS)
;;	   ColumnNum  = DataRecOBJ->NFields()   ;此处的ColumnNum = Columns,这里没有用到,防空查询的情况
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN
	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()            ;得到相应县或区划的相应年份的产量********

				;===杨绍锷修改，20070903=============================
;	             FOR i=0,N-1 DO Temp[i,0] = DataValue.(i)	;原代码

				FOR i=0,N-1 DO begin
					if size(DataValue.(i),/type) eq 10 then begin	;数据为指针，数据类型为10，即数据库中的备注
						Temp[i,0] = string(*(DataValue.(i)))
					endif else begin
						Temp[i,0] = DataValue.(i)
					endelse
				endfor
				;====================================================
	             DATA=[[DATA],[Temp]]				;进行列串接.
	           ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;去掉首行,得到全部数据
	       ENDIF

	   OBJ_DESTROY,DataRecOBJ

	   IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum

	   DATA = STRTRIM(TEMPORARY(DATA),2)
	   RETURN,DATA
END
;*****************从数据库中读取数据,返回为结构体数组********************************
FUNCTION DC_GetDataFromDB,Sql	 $    		      		;SQL语句.字符串
					    ,Num_BUFFERS = Num_BUFFERS $  	;从数据库中提取数据时的缓存数量,为整型值
						,N_RECORDS = NumReocrd	  		;得到记录条数
	 ;该函数调用形式为:
	 ;result = GetDataFromDB(Sql,Num_BUFFERS = Num,N_RECORDS = var)
	 ;结果有两种:
	 ;(1) 返回查询到的数据,为结构体数组
	 ;(2) 返回空值当查询不成功时.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;默认缓存数量为10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	 ;此处没用"RecordNumOBJ->MoveCursor(/FIRST)",是因为它一定有且只有一条记录.类型为结构体
	  RecordNum = RecordNumOBJ->GetRecord()

	  IF RecordNum.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)		;值为0,即RecordNum.(0)为0
	      RETURN,''
	  ENDIF ELSE OBJ_DESTROY,RecordNumOBJ

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
;--自定义函数:用于读取或保存用户的工作路径---------------------------------------
FUNCTION DC_PathSetting,WritePath1=writepath1 $     ;写到第一路径(存储输入路径)
					  ,WritePath2=writepath2 $     ;写到第二路径(存储输出路径)
					  ,ReadPath2=readpath2         ;只读取第二路径,默认读取第一路径

 ;该函数调用形式为:
 ;result = DC_PathSetting([WritePath1=writepath1,WritePath2=writepath2,/ReadPath2])

 ;(1)当缺省所有变量时，返回值为默认读取的第一路径（即输入路径),字符串(下同)
 ;(2)当只用/ReadPath2,返回值为读取的第二路径
 ;(3)当同时设置了读与写时,只进行写,将路径保存到文件"pathsetting.txt"中,返回值为空
 ;(4)当同时设置写时,将路径保存到文件"pathsetting.txt"中,返回值为空
 ;(5)当输入变量时，则发生错误。

 ;   IF (N_ELEMENTS(readpath) EQ 0) THEN readpath = 0
 ; 	 IF NOT (KEYWORD_SET(readpath))  THEN readpath = 0

;    OPENR,lun,'land\pathsetting.txt',/GET_LUN
;    result = FSTAT(lun)
;    SettingPath = BYTARR(result.SIZE)
;    READU,lun,SettingPath
;    FREE_LUN,lun

    IF (N_PARAMS() NE 0) THEN MESSAGE, 'Should not have arguments'
 	 ON_ERROR, 2						;return to caller

;	FileState = FILE_INFO('land\pathsetting.txt')
	SettingPath = STRARR(1)
	File = 'text\pathsetting.txt'
	OPENR,lun,File,/get_lun       ;读出文件中的路径
	READF,lun,SettingPath                           ;注意这里是用"READF"
	FREE_LUN,lun
	Allpath = STRSPLIT(SettingPath, '|',/EXTRACT)

	  IF KEYWORD_SET(writepath1) OR KEYWORD_SET(writepath2) THEN BEGIN
		   IF KEYWORD_SET(writepath1) AND NOT KEYWORD_SET(writepath2) THEN BEGIN
		    	Allpath[0] = writepath1
		   ENDIF

		   IF KEYWORD_SET(writepath2) AND NOT KEYWORD_SET(writepath1) THEN BEGIN
		    	Allpath[1] = writepath2
		   ENDIF

		   IF KEYWORD_SET(writepath1) AND KEYWORD_SET(writepath2) THEN BEGIN
		    	Allpath[0] = writepath1
		    	Allpath[1] = writepath2
		   ENDIF

	       	StorePath = STRJOIN(Allpath,'|')
			OPENW,lun,File,/get_lun        ;将路径写入到文件中.
			PRINTF,lun,StorePath
			FREE_LUN,lun

	  		RETURN,''
	  ENDIF

    IF KEYWORD_SET(ReadPath2) THEN BEGIN
    	RETURN,Allpath[1]                  ;读保存的输出路径。（第二路径）
    ENDIF ELSE BEGIN
    	RETURN,Allpath[0]    			   ;读保存的输入路径。（第一路径）
    ENDELSE

END


 ;***************自定义函数:对单旬波动数据进行组合********************************************
;主要用于波动产量参数提取以及批处理运算模块(DC_Floatyield.pro和DC_Floatyield_2.pro)
FUNCTION DC_FactorCombination,AllData,MeteoTableIndex,RowsNum,StartMonth,EndMonth

	IF (N_PARAMS() NE 5) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

       SelectedFactorIndex=WHERE(MeteoTableIndex EQ 1,FactorNum)   ;FactorNum指七种因子中被选的个数
       CombinationData=Fltarr(1,RowsNum)
       T_ColumnNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3
       FOR k=0,FactorNum-1 DO BEGIN                                         ;这一步是对获得的单旬数据进行组合相邻两旬和三旬组合.
           InterimData=FLOAT(AllData[k*T_ColumnNum:(k+1)*T_ColumnNum-1,0:*]);得到一种波动因子的所有单旬数据
           aa=WHERE(STRMATCH(['1','2','4'],STRTRIM(SelectedFactorIndex[k],2)) EQ 1,Count)
           Interim2Initial=Fltarr(1,RowsNum)
           Interim3Initial=Fltarr(1,RowsNum)
           IF Count NE 0 THEN BEGIN                                         ;以求和方式进组合(只对"降水/日照/积温",分别对应['1','2','4'])
              FOR i=0,T_ColumnNum-2 DO BEGIN
                  Interim2Data=InterimData[i,0:*]+InterimData[i+1,0:*]
                  Interim2Initial=[Interim2Initial,[Interim2Data]]
              ENDFOR
              FOR i=0,T_ColumnNum-3 DO BEGIN
                  Interim3Data=InterimData[i,0:*]+InterimData[i+1,0:*]+InterimData[i+2,0:*]
                  Interim3Initial=[Interim3Initial,[Interim3Data]]
              ENDFOR
              Interim2Initial=Interim2Initial[1:*,0:*]
              Interim3Initial=Interim3Initial[1:*,0:*]
              Interim23Data=[Interim2Initial,[Interim3Initial]]
           ENDIF ELSE BEGIN                                                 ;否则以求平均方式组合
              FOR i=0,T_ColumnNum-2 DO BEGIN
                  Interim2Data=(InterimData[i,0:*]+InterimData[i+1,0:*])/2.0
                  Interim2Initial=[Interim2Initial,[Interim2Data]]
              ENDFOR
              FOR i=0,T_ColumnNum-3 DO BEGIN
                  Interim3Data=(InterimData[i,0:*]+InterimData[i+1,0:*]+InterimData[i+2,0:*])/3.0
                  Interim3Initial=[Interim3Initial,[Interim3Data]]
              ENDFOR
              Interim2Initial=Interim2Initial[1:*,0:*]
              Interim3Initial=Interim3Initial[1:*,0:*]
              Interim23Data=[Interim2Initial,[Interim3Initial]]
           ENDELSE
            CombinationData=[CombinationData,[Interim23Data]]
        ENDFOR
           CombinationData=STRTRIM(CombinationData[1:*,0:*],2)     ;转化为字符型
   return,CombinationData
END

;****************自定义函数:提取单旬的气象波动因子数据(重要部分)********************************
;主要用于波动产量参数提取以及批处理运算模块(DC_Floatyield.pro和DC_Floatyield_2.pro)
FUNCTION DC_SingleTendayFactor,StartYear_,EndYear_,StartMonth_,EndMonth_,MeteoTableIndex,station_code,YieldType
    ;调用:Result=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,EndMonth,MeteoTableIndex,station_code,/AgroTable)

	IF (N_PARAMS() NE 7) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	WIDGET_CONTROL, /HOURGLASS

	TableName=['TENDAY_TEMP_AVG','TENDAY_RAINFALL','TENDAY_SUNSHINE','TENDAY_AIR_HU', $
               'TENDAY_TEMP_0_ACCUMULATE','TENDAY_TEMP_MAX','TENDAY_TEMP_MIN']      ;注意对应界面中的因子顺序

	IF YieldType THEN BEGIN
	   TableName = 'AGRO_'+TableName
	ENDIF ELSE TableName = 'METEO_'+TableName

   StartYear = FIX(StartYear_)   & EndYear = FIX(EndYear_)
   StartMonth = FIX(StartMonth_) & EndMonth = FIX(EndMonth_)
  ;之所以用波动因子\年\月\旬的四层循环,可以保证一些站点一些旬无数据时,用空值代替.精度可以确保,但速度慢下来.
    RowsNum =EndYear-StartYear+1                                 ;即年数
    T_ColumnNum=(EndMonth-StartMonth+1)*3                        ;单旬数
    AllData=STRARR(1,RowsNum)+'%'                                ;初始化列值

	progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='正在数据处理中,请稍候...',TITLE='筛选因子');,/CANCELBUTTON)
	progressTimer->START        ;启动进度条

	IsSelectID = WHERE(MeteoTableIndex EQ 1,COUNT)
	TOL = LONG(COUNT)*RowsNum & NN=1

       FOR i=0,6 DO BEGIN

          IF MeteoTableIndex[i] EQ 1 THEN BEGIN
              TempyearMeteo=STRARR(T_ColumnNum,1)+'*'            ;初始化行值
             FOR j=StartYear,EndYear DO BEGIN

;				CANCELLED = progressTimer->CHECKCANCEL()    ;注意调试??????????????????
;				IF CANCELLED THEN BEGIN
;					OK = DIALOG_MESSAGE('您终止了"筛选"操作!',TITLE='警告')
;					OBJ_DESTROY,progressTimer ;结束进度条
;				    RETURN,''
;				ENDIF
      	        progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;启动进度条
      	         NN+=1

                TempmonthMeteo=STRARR(1)
                MeteoData=STRARR(T_ColumnNum,1)

                 Sqlstr0='select count(*) as recordNum from '+TableName[i]+' where station_id='+"'"+station_code+"'"+' and '+ $
                        'year='+STRTRIM(j,2)+' and month between '+STRTRIM(StartMonth,2)+' and '+STRTRIM(EndMonth,2)

				recordNum=LONG(DC_GetdataFromDB_Str(1,Sqlstr0,Num_BUFFERS = 500))
                 IF recordNum EQ T_ColumnNum THEN BEGIN  ;说明所有月各旬都有数据.不缺旬.
                    Sqlstr='select val from '+TableName[i]+' where station_id='+"'"+station_code+"'"+' and '+ $
                           'year='+STRTRIM(j,2)+' and month between '+STRTRIM(StartMonth,2)+' and '+STRTRIM(EndMonth,2)+' order by year,month,tenday'
					MeteoData[*,0]=DC_GetdataFromDB_Str(1,Sqlstr,Num_BUFFERS = 500)
                    TempyearMeteo=[[TempyearMeteo],[MeteoData]]
                  ENDIF ELSE BEGIN
                    FOR k=StartMonth,EndMonth DO BEGIN
                        TemptendayMeteo=STRARR(1)
                       FOR L=1,3 DO BEGIN
                              tendayMeteo=STRARR(1)
                          Sqlstr='select val from '+TableName[i]+' where station_id='+"'"+station_code+"'"+' and '+ $
                             'year='+STRTRIM(j,2)+' and month='+STRTRIM(k,2)+' and tenday='+STRTRIM(L,2)
						  tendayMeteo=DC_GetdataFromDB_Str(1,Sqlstr,Num_BUFFERS = 500)
                          TemptendayMeteo=[TemptendayMeteo,tendayMeteo]
                        ENDFOR
                          TemptendayMeteo=TemptendayMeteo[1:*]                         ;得到一个月三旬的数据
                          TempmonthMeteo=[TempmonthMeteo,TemptendayMeteo]
                     ENDFOR
                          TempmonthMeteo=TempmonthMeteo[1:*]                           ;得到一个年所有月的数据
                          TempyearMeteo=[[TempyearMeteo],[TempmonthMeteo]]             ;使用数组"列串接"技巧
                   ENDELSE

              ENDFOR
                TempyearMeteo=TempyearMeteo[0:*,1:*]                        ;去掉第一行*号初始的值,得到一种波动数据所有年份的数据
                AllData=[AllData,[TempyearMeteo]]                           ;使用数组"行串接"技巧,数组横排串成行
           ENDIF
        ENDFOR

;;        progressTimer->DESTROY ;销毁进度条
		OBJ_DESTROY,progressTimer

        AllData=AllData[1:*,0:*]                                           ;得到所有单旬的因子数据

   return,AllData
END

;*******自定义函数:对单旬波动数据中的空值以所在列其他非空值的均值来替换*************
;主要用于波动产量参数提取模块(DC_Floatyield_2.pro)
;当然本函数也可以作其他用途
FUNCTION DC_ProcessBlank,DataValue $        ;要进行处理的单旬气象数据.字符型
					 	,Rows $		     ;单旬数据的行数,实际上也是年数
					 	,BlankId = BlankId  ;为空值的索引号

	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

  	   Temp = WHERE(DataValue EQ '',Count)

	   IF Count NE 0 THEN BEGIN
	   	  BlankId = ARRAY_INDICES(DataValue,Temp)
	   ENDIF ELSE BlankId = [-1,-1]

	   FOR I=0,N_ELEMENTS(DataValue)/Rows-1 DO BEGIN
	   	  Temp_id = WHERE(DataValue[I,*] EQ '',Num,COMPLEMENT=NoID,NCOMPLEMENT=Num0)
	   	  IF Num NE 0 THEN BEGIN
	   	  	 Temp = FLOAT(DataValue[I,*])
	   	  	 IF Num0 NE 0 THEN BEGIN
		   	  	 Temp[Temp_id] = MEAN(Temp[NoID])
		   	  	 DataValue[I,*] = STRTRIM(Temp,2)
		   	 ENDIF ELSE BEGIN
		   	  	 Temp[Temp_id] = 0.0			    ;即全为空,则以0来取代
		   	  	 DataValue[I,*] = STRTRIM(Temp,2)
		   	 ENDELSE
	   	  ENDIF
	   ENDFOR

   	  RETURN,DataValue
END

;***********自定义函数:组合旬名*******************************************************
;主要用于波动产量参数提取模块(DC_Floatyield_2.pro)
 FUNCTION DC_CombinationTendayName,TendayFactorName,MeteoTableIndex,SingleTendayNum

	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    SelectedFactorIndex = WHERE(MeteoTableIndex EQ 1,FactorNum)
    SingleFactor=[''] & CombinationFactor=[''] & CombinationName=TendayFactorName
    FOR i=0,FactorNum-1 DO BEGIN
         TendayName=TendayFactorName & FactorPostfix2=[''] & FactorPostfix3 = ['']
        CASE SelectedFactorIndex[i] OF
           0 : Postfix='均温'
           1 : Postfix='降水'
           2 : Postfix='日照'
           3 : Postfix='湿度'
           4 : Postfix='积温'
           5 : Postfix='最高温'
           6 : Postfix='最低温'
        ENDCASE
        TendayName=TendayName+Postfix
        SingleFactor=[SingleFactor,[TendayName]]             ;得到的是单旬的因子名,含初始值

        FOR j=0,SingleTendayNum-2 DO BEGIN
           TempPostfix2=CombinationName[j]+CombinationName[j+1]
           FactorPostfix2=[FactorPostfix2,[TempPostfix2]]
        ENDFOR
        FOR j=0,SingleTendayNum-3 DO BEGIN
           TempPostfix3=CombinationName[j]+CombinationName[j+1]+CombinationName[j+2]
           FactorPostfix3=[FactorPostfix3,[TempPostfix3]]
        ENDFOR
        FactorPostfix2=FactorPostfix2[1:*]+Postfix                 ;相邻2旬组合
        FactorPostfix3=FactorPostfix3[1:*]+Postfix                 ;相邻3旬组合
        FactorPostfix23=[FactorPostfix2,[FactorPostfix3]]
        CombinationFactor=[CombinationFactor,[FactorPostfix23]]    ;得到的是组合旬的因子名,含初始值

     ENDFOR
     SingleFactor=SingleFactor[1:*] & CombinationFactor=CombinationFactor[1:*]
     AllFactorName=[SingleFactor,[CombinationFactor]]
   RETURN,AllFactorName
END

;*********自定义函数:得到单旬名*******************************************************
;主要用于波动产量参数提取模块(DC_Floatyield_2.pro)
 FUNCTION DC_SingleTendayName,StartMonth_,EndMonth_
	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

      StartMonth=FIX(StartMonth_) & EndMonth=FIX(EndMonth_)
      FactorName=['']
    FOR i=StartMonth,Endmonth DO BEGIN
        FOR j=1,3 DO BEGIN
            CASE j OF
                  1: Tenday='上旬'
                  2: Tenday='中旬'
                  3: Tenday='下旬'
               ELSE:
            ENDCASE

            FactorName=[FactorName,STRTRIM(STRING(i),2)+'月'+Tenday]
        ENDFOR
     ENDFOR
     FactorName=FactorName[1:*]
   RETURN,FactorName
END

;****************自定义函数:提取遥感因子数据********************************
;主要用于波动产量参数提取以及批处理运算模块(DC_Floatyield.pro和DC_Floatyield_2.pro)
FUNCTION DC_GetRsData,StartYear_,EndYear_,StartMonth_,EndMonth_,Code_,CropFiled,IsAvgOrSum,Sensor,DataType,YieldType
    ;调用:Result=DC_GetRsData(StartYear,EndYear,StartMonth,EndMonth,,Code,CropFiled,Sensor,DataType,YieldType)
	IF (N_PARAMS() NE 10) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	WIDGET_CONTROL, /HOURGLASS

	IF YieldType EQ 0 THEN BEGIN
	    TableRS = 'PARAMETER_PROCESS_COUNTY'
	    WhereCol = 'County_code'
	ENDIF ELSE  BEGIN
		TableRS = 'PARAMETER_PROCESS_AGRO_STATION'
	    WhereCol = 'Station_id'
	ENDELSE

	Code = "'"+Code_+"'"

   StartYear = FIX(StartYear_)   & EndYear = FIX(EndYear_)
   StartMonth = FIX(StartMonth_) & EndMonth = FIX(EndMonth_)

    RowsNum =EndYear-StartYear+1                                 ;即年数
    AllData=STRARR(1)+'%'                                ;初始化列值

	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='正在数据处理中,请稍候...',TITLE='筛选因子')
	progressTimer->START                         ;启动进度条

       FOR i=StartYear,EndYear DO BEGIN
	      progressTimer->UPDATE, (FLOAT(i)/RowsNum*100.0)  ;启动进度条

			Sqlstr ='select '+CropFiled+' from '+TableRS+' where '+WhereCol+'='+Code+' and '+ $
			 		'year='+STRTRIM(i,2)+' and month between '+STRTRIM(StartMonth,2)+' and ' $
			 		+ STRTRIM(EndMonth,2)+" and data_type='"+DataType+"' and Sensor_code='" $
			 		+ Sensor+"'"
			Temp = DC_GetdataFromDB_Str(1,Sqlstr)

			IF IsAvgOrSum EQ 0 THEN BEGIN   								;求平均
				AllData = [AllData,STRTRIM(MEAN(FLOAT(Temp)),2)]
			ENDIF ELSE AllData = [AllData,STRTRIM(TOTAL(FLOAT(Temp)),2)]	;求累计值

       ENDFOR

;;	    progressTimer->DESTROY ;销毁进度条
 		OBJ_DESTROY,progressTimer
	    AllData=AllData[1:*,0:*]       ;得到所有单旬的因子数据

	return,AllData
END

;----------函数Latlon_to_Albers_()将经纬度坐标转为Alber坐标-------------------------------
FUNCTION DC_Latlon_to_Albers ,Lon_Lat $                         ;即经度和纬度数据对.
						  ,CenterMidian105 = CenterMidian105  ;转为Albers105,默认为110.
   ;;调用形式为: Result = DC_Latlon_to_Albers( Lon_Lat,[/CenterMidian105])  返回为值为(X,Y)笛卡尔坐标
   ;若使用了关键字,则将经纬度返回为Albers105的坐标值.
	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

   StdLat1 = 25.0
   StdLat2 = 47.0
   MidLon = 110.0
   False_easting = 4000000.0

   IF KEYWORD_SET(CenterMidian105) THEN BEGIN
      MidLon = 105.0
      False_easting = 0.0
   ENDIF
	; 构造投影信息The following are GCTP projections:
	; setting Projection = 103 means Albers Equal Area projection with following parameters: SEMIMAJOR_AXIS, SEMIMINOR_AXIS, STANDARD_PAR1, STANDARD_PAR2, CENTER_LONGITUDE, CENTER_LATITUDE, FALSE_EASTING, FALSE_NORTHING
	; node:FALSE_EASTING = 0.0 means FALSE_EASTING is 4 000 000.00
	; DATUM = 15 means datum is Krassovsky 6378245.0 6356863.0188
	projection = map_proj_init(103 , DATUM = 15, /GCTP, SEMIMAJOR_AXIS = 6378245.0, SEMIMINOR_AXIS = 6356863.0188, STANDARD_PAR1 = StdLat1, STANDARD_PAR2 = StdLat2, $
	                           CENTER_LONGITUDE = MidLon, CENTER_LATITUDE = 0.0, FALSE_EASTING = False_easting, FALSE_NORTHING = 0.0)
	; To get latitude and longitude of the coordinates of the point
	; MAP_STRUCTURE is the projection information

	;返回为值为(X,Y)笛卡尔坐标
	X_Y = MAP_PROJ_FORWARD (Lon_Lat, MAP_STRUCTURE = projection)

    return, X_Y; [X/Y]数据对.
END

;----------函数DC_Albers_to_Latlon()将Alber坐标转为经纬度坐标-------------------------------
FUNCTION DC_Albers_to_Latlon,X_Y $             ;笛卡尔坐标数据对
                         ,CenterMidian105 = CenterMidian105
   ;;调用形式为: Result = DC_Albers_to_Latlon( X, Y,/CenterMidian105)  返回为值为lat, lon
   ;若使用了关键字,则为将Albers105的坐标值返回经纬度.
	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

   StdLat1 = 25.0
   StdLat2 = 47.0
   MidLon = 110.0
   False_easting = 4000000.0D

   IF KEYWORD_SET(CenterMidian105) THEN BEGIN
      MidLon = 105.0
      False_easting = 0.0D
   ENDIF
	; 构造投影信息The following are GCTP projections:
	; setting Projection = 103 means Albers Equal Area projection with following parameters: SEMIMAJOR_AXIS, SEMIMINOR_AXIS, STANDARD_PAR1, STANDARD_PAR2, CENTER_LONGITUDE, CENTER_LATITUDE, FALSE_EASTING, FALSE_NORTHING
	; node:FALSE_EASTING = 0.0 means FALSE_EASTING is 4 000 000.00
	; DATUM = 15 means datum is Krassovsky 6378245.0 6356863.0188
	projection = map_proj_init(103 , DATUM = 15, /GCTP, SEMIMAJOR_AXIS = 6378245.0, SEMIMINOR_AXIS = 6356863.0188, STANDARD_PAR1 = StdLat1, STANDARD_PAR2 = StdLat2, $
	                           CENTER_LONGITUDE = MidLon, CENTER_LATITUDE = 0.0, FALSE_EASTING = False_easting, FALSE_NORTHING = 0.0)
	; To get latitude and longitude of the coordinates of the point
	; MAP_STRUCTURE is the projection information

	;返回值为2列的数组,依次为longitude/latitude coordinates
	Lon_lat = MAP_PROJ_INVERSE(X_Y, MAP_STRUCTURE = projection)

    return, Lon_lat       ; [经度/纬度]
END
;----------------------------------------------------------------------------
;_____________________________主函数__________________________________________________
;-------------依据指定的左上角坐标和行列数生成插值图----------------------------------
 FUNCTION DC_CREATE_INTERP_GRID,Lon_lat,value,ulx_,uly_,cellsize_,samples_,lines_,MeteoDataType,CenMedian105 = CenMedian105
   ;调用形式: Result = DC_CREATE_INTERP_GRID(Lon_Lat,value,ulx_,uly_,cellsize_,samples_,lines_,MeteoDataType,[/CenMedian105])
   ;返回值为:指定的行列数的插值二维数据,坐标值为Albers110投影坐标
	IF (N_PARAMS() NE 8) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    ulx = DOUBLE(ulx_)
    uly = DOUBLE(uly_)
    cellsize = FIX(cellsize_)
    samples  = FIX(samples_)
    lines    = FIX(lines_)

	lrx=ulx+(samples)*cellsize
	lry=uly-(lines)*cellsize

	IF KEYWORD_SET(CenMedian105) THEN BEGIN
		coor = DC_Latlon_to_Albers(Lon_lat,/CenterMidian105)  ;用到Albers105投影.
	ENDIF ELSE BEGIN
		coor = DC_Latlon_to_Albers(Lon_lat) 				   ;用到Albers110投影.
	ENDELSE

	xbox=[[ulx],[ulx],[lrx],[lrx]]
	ybox=[[uly],[lry],[uly],[lry]]

	Result=MIN((coor[0,*]-ulx)^2+(coor[1,*]-uly)^2,index)   ;到左上角最小距离
	v1=value[index]
	Result=MIN((coor[0,*]-ulx)^2+(coor[1,*]-lry)^2,index)	;到左下角最小距离
	v2=value[index]
	Result=MIN((coor[0,*]-lrx)^2+(coor[1,*]-uly)^2,index)	;到右上角最小距离
	v3=value[index]
	Result=MIN((coor[0,*]-lrx)^2+(coor[1,*]-lry)^2,index)	;到右下角最小距离
	v4=value[index]

	vbox=[[v1],[v2],[v3],[v4]]

	;去重复的点      四个角的值以最近来替代.�
	GRID_INPUT,[[coor[0,*]],[xbox]],[[coor[1,*]],[ybox]],[[Value],[vbox]],xSorted,ySorted,dataSorted

	TRIANGULATE,xSorted,ySorted,triangle
	;--------------------------------------------------------
;	;FUNCTION INTERP_METHOD,dataSorted,xSorted,ySorted,Samples,lines,triangle,MeteoDataType
;
;	;根据不同的要素(由"MeteoDataType"决定)选择不同的插值方式

	DIMENSION=[samples,lines]
	CASE STRUPCASE(MeteoDataType) OF
		'PRE':BEGIN
			powerhvalue=2
			Smoothvalue=0.0
			grid=GRIDDATA(xSorted,ySorted,dataSorted,$
							POWER=powerhvalue,SMOOTHING=Smoothvalue,missing=NAN,$
							DIMENSION=DIMENSION,METHOD='InverseDistance')
		END
		'HUMD':BEGIN
			grid=MIN_CURVE_SURF(dataSorted,xSorted,ySorted,/TPS,$
						        NX=samples,NY=lines)
		END
		'SUNT':BEGIN;日照时数
			powerhvalue=2
			Smoothvalue=1.0
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
						 ,POWER=powerhvalue,SMOOTHING=Smoothvalue $
						 ,DIMENSION=DIMENSION,METHOD='InverseDistance' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )
		END
		'TMAX':BEGIN
			grid=MIN_CURVE_SURF(dataSorted,xSorted,ySorted,/TPS,$
								NX=samples,NY=lines)
		END
		'TAVE':BEGIN
			grid=MIN_CURVE_SURF(dataSorted,xSorted,ySorted,/TPS,$
								NX=samples,NY=lines)
		END
		'TMIN':BEGIN
			grid=MIN_CURVE_SURF(dataSorted,xSorted,ySorted,/TPS,$
								NX=samples,NY=lines)
		END
		'WINV':BEGIN    	;风速
			powerhvalue=2
			Smoothvalue=1.0
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
						 ,POWER=powerhvalue,SMOOTHING=Smoothvalue $
						 ,DIMENSION=DIMENSION,METHOD='InverseDistance' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )
		END
		'KING':BEGIN    	;用于波动单产空间插值
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
;						 ,VARIOGRAM = [1,8,1,1]	$			;[ Type, Range, Nugget, Scale]
						 ,DIMENSION=DIMENSION,METHOD='Kriging' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )

		END
	    ELSE:
	ENDCASE

;   图像倒转,以得到正确的图像.
;	gridnew=MAKE_ARRAY(samples,lines,TYPE=SIZE(grid,/TYPE))
;	FOR j=0,lines-1 DO gridnew[*,lines-1-j]=grid[*,j]

    grid = REVERSE(TEMPORARY(grid), 2)
	RETURN,grid
END
;******************************************************************************
;******该过程生成标准的ENVI文件:影像数据 + 相应的头文件.
 PRO DC_SaveImageFile,SAVE_FILE       $ ;待保存的文件名.
 				     ,ImageData       $ ;要保存的数据,注意ImageData是指针类型.
 				     ,samples,lines,DataType $  ;DataType指数据是float,Doble,char,int
 				     ,sensortype $              ;long,byte等类型ID,看size()函数
 				     ,ULX,ULY,Resolution ,CenterMedian
;调用:DC_SaveImageFile,SAVE_FILE,ImageData,samples,lines,DataType,sensortype,ULX,ULY,Resolution ,CenterMedian

	IF (N_PARAMS() NE 10) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

        enter=string(byte(13))+string(byte(10))     ;回车符ASCII码值
        time=systime()                              ;生成时间
        col =STRTRIM(samples,2)                     ;生成影像列数
        line=STRTRIM(lines,2)                       ;生成影像行数
        DataType_   = STRTRIM(DataType,2)           ;数据类型
        sensortype_ = STRTRIM(sensortype,2)         ;传感器类型
        tulx  = STRTRIM(ULX,2)                      ;左上角X坐标(ALBERS110)
        tuly  = STRTRIM(ULY,2)                      ;左上角Y坐标(ALBERS110)
		Resolution_   = STRTRIM(Resolution,2)		;像素大小.也即分辨率.
		CenterMedian_ = STRTRIM(CenterMedian,2)		;是110,还是105
		IF CenterMedian_ EQ '110' THEN BEGIN
		   EastingFalse = '4000000'					;东偏多少公里.
		ENDIF ELSE EastingFalse = '0.0'

        SaveFileName = SAVE_FILE
        HeadFileName = SAVE_FILE+'.hdr'

        IF STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH) NE -1 THEN BEGIN
        	SaveFileName = STRMID(SAVE_FILE,0,STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH))
        	HeadFileName = SAVE_FILE
        ENDIF

        Bandname = STRMID(SaveFileName,STRPOS(SaveFileName,'\',/REVERSE_SEARCH)+1)  ;不包含路径的文件名.

        HeadInfomation='ENVI'+enter+$
        'description = {' + enter+ $
        '  Create New File Time ['+time+']}'+enter+$
        'samples = '+col+enter+$
        'lines   = '+line+enter+$
        'bands   = 1'+enter+$
        'header offset = 0'+enter+$
        'file type = ENVI Standard'+enter+$
        'data type = '+DataType_+enter+$
        'interleave = bsq'+enter+$
        'sensor type = '+sensortype_+enter+$
        'byte order = 0'+enter+$
        'map info = {Albers Conical Equal Area, 1.0000, 1.0000, ' +tulx+', ' +tuly+', '+Resolution_+', '+Resolution_+', Krasovsky, units=Meters}'+enter+$
        'projection info = {9, 6378245.0, 6356863.0, 0.000000, '+CenterMedian_+', '+EastingFalse+', 0.0, 25.000000, 47.000000, Krasovsky, Albers Conical Equal Area, units=Meters}'+enter+$
        'wavelength units = Unknown'+enter+$
        'band names = {'+Bandname+'}'+enter+$
        'pixel size = {1000.000000, 1000.000000, units=Meters}'+enter

         openw,lun,HeadFileName,/get_lun     ;注意:当文件已存时,这个写过程不会作任何提示,将已有文件替换掉.下同.,
         writeu,lun,HeadInfomation
         free_lun,lun

         openw,lun,SaveFileName,/get_lun
         writeu,lun,*ImageData				;	ImageData是指针类型
         free_lun,lun

END

;------该函数主要是为读取区域或影像的投影坐标参数--------------------
FUNCTION DC_ReadParameter,district      ;变量district为区域(字符型)
	;Re=DC_ReadParameter(district)
	;返回值:(1)返回为区域的相应参数.
	;		(2)如果没有相应的参数,则返回值为空值
	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	ParaFile = 'text\parametersetting.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('系统安装目录下找不到参数设置文件"parametersetting.txt"!',TITLE='警告')
		RETURN,''
	ENDIF

    OPENR,lun,ParaFile,/GET_LUN
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

;**************读ENVI标准文件的头文件*********************************************
FUNCTION DC_ReadHead_file,inputfile_ $              ;输入要读头的文件
						 ,PROMPT_DES = PROMPT_DES   ;用户指定的用于提示的提示语,字符型
   ;调用:Re = DC_ReadHead_file(inputfile_,[PROMPT_DES = Describe])
   ;注意该程序只限用于我们写的标准ENVI头文件的读取.返回值如下:
   ;(1) 读取成功,返回为相应文件的头文件信息.
   ;(2) 读取失败,返回为空

	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    inputfile = inputfile_
    IF NOT KEYWORD_SET(PROMPT_DES) THEN PROMPT_DES=''

    IF ~FILE_TEST(inputfile) THEN BEGIN
       Info = DIALOG_MESSAGE('找不到'+PROMPT_DES+'相应的头文件',TITLE='警告')
       RETURN,''
    ENDIF

    IF STRPOS(inputfile, '.',/REVERSE_SEARCH) EQ -1 THEN BEGIN
       inputfile = STRTRIM(inputfile,2)+'.hdr'
    ENDIF

    OPENR,lun,inputfile,/GET_LUN
    result = FSTAT(lun)

	 if result.size eq 0 then begin
	 	Info = DIALOG_MESSAGE(inputfile+'头文件不正确!',TITLE='警告')
	 	return,{error	:	-1}
	 endif
    headdata = BYTARR(result.SIZE)
    READU,lun,headdata
    FREE_LUN,lun

 ;   获取行列号，波段数，数据读取方式,数据类型.左上角XY坐标.

    index0 = STRPOS(headdata, 'samples = ')
    index1 = STRPOS(headdata, 'lines   = ')
    index2 = STRPOS(headdata, 'bands   = ')
    index3 = STRPOS(headdata, 'data type = ')
    index4 = STRPOS(headdata, 'interleave = ')
    index5 = STRPOS(headdata, 'sensor type = ')
    index6 = STRPOS(headdata, 'byte order = ')

    index7 = STRPOS(headdata, 'map info = ')
    index8 = STRPOS(headdata, 'units=Meters}')
    index9  = STRPOS(headdata, 'projection info = ')
    index10 = STRPOS(headdata, '}',index9)                  ;注意这里加了定位位置.

	 if (where([index0,index1,index2,index3,index4,index5,index6,index7,index8,index9,index10] eq -1))[0] ne -1 then begin
	 	Info = DIALOG_MESSAGE(inputfile+'头文件不正确!',TITLE='警告')
	 	return,{error	:	-1}
	 endif

	samples    = STRMID(headdata,index0+10,index1-index0-10-2) ;之所以减2是因回车符占2个字节长.
    lines      = STRMID(headdata,index1+10,index2-index1-10-2)
    bands      = STRMID(headdata,index2+10,3)                  ;这里截取长度是3,而不是1,看回车符的字节长度.
    datatype   = STRMID(headdata,index3+12,3)                  ;这里截取长度是3,而不是1,看回车符的字节长度.
    interleave = STRMID(headdata,index4+13,index5-index4-13-2)
    sensortype = STRMID(headdata,index5+14,index6-index5-14-2)

	Mapinfo = STRMID(headdata,index7+11,index8-index7-11)
	Mapinfomation = STRSPLIT(Mapinfo,',',ESCAPE=' ',/EXTRACT )
	UlX = Mapinfomation[3]                       			;左上角X坐标
	UlY = Mapinfomation[4]									;左上角Y坐标
	Resolution = Mapinfomation[5]	;注意这里有垂直和水平方向分辨率之分,一般是相同的.Mapinfomation[5]=Mapinfomation[6]

	Pro_info = STRMID(headdata,index9+18,index10-index9-18)
	Projection = STRSPLIT(Pro_info,',',ESCAPE=' ',/EXTRACT )
	CenterMedian = STRTRIM(FIX(Projection[4]),2)	      ;中央子午线,以确定是110,还是105.

;    print,Mapinfo
;    print,UlX_Y_postion
;    print,UlX,ULY
;    print,PixelInfo
;    print,PixelSize
;·	1 - byte (8-bits)
;·	2 - integer (16-bits)
;·	3 - long integer (32-bits)
;·	4 - floating-point (32-bits)
;·	5 - double-precision floating point (64-bits)
;·	6 - complex (2x32-bits)
;·	9 - double -precision complex (2x64-bits)
;·	12 - unsigned integer (16-bits)
;·	13 - unsigned long integer (32-bits)
;·	14 - long 64-bit integer
;·	15 - unsigned long 64-bit integer.

	CASE FIX(datatype) OF     ;因为不同的数据类型,占据的"字节数或者二进制位数"是不尽相同的.
		1: ByteNum = 1        ;1字节(byte) = 8二进制位(bit).即 1 byte = 8 bit
		2: ByteNum = 2
		3: ByteNum = 4
		4: ByteNum = 4
		5: ByteNum = 8
;		6: ByteNum = 2*4
;		9: ByteNum = 2*8
		12: ByteNum = 2
		13: ByteNum = 4
		14: ByteNum = 8
		15: ByteNum = 8
	  ELSE: ByteNum = 0
	ENDCASE

  ;请千万不要改动结构体中域的顺序,因为在其他程序中引用时,有按域索引号来引用的.
  ;,如fileinfo.(3)表引用波段数,若改动,会出现错误.要修改程序添加域,只能在后面添加.
  ;;当然是按域名来引用,则没有任何影响

   RETURN,fileinfo={samples     :  samples   	,$
                    lines       :  lines       	,$
                    bands       :  STRTRIM(FIX(bands),2)        ,$   ;去掉回车符.下同.
                    datatype    :  STRTRIM(FIX(datatype),2) 	,$
                    interleave  :  interleave  	,$
                    sensortype  :  sensortype 	,$
                    ByteNum		:  ByteNum 		,$
                    UlX			:  UlX 			,$
                    UlY			:  UlY 			,$
                    Resolution	:  STRTRIM(LONG(FLOAT(Resolution)),2) 	,$   ;这样做是因为有时FIX(1.0000000000e+003)结果为1,但float()不会.
                    CenterMedian:  CenterMedian}
END

;**************读ENVI标准文件的数据*********************************************
FUNCTION DC_Read_ENVIData,inputfile_ $  				;输入的文件名(字符型)
					  ,SUCCESSSTATUS = SuccessStatus $	;读取是否成功的状态.
					  ,DESCRIPTION  = Description		;用户输入的提示语,字符型
	;调用形式:Resutl = DC_Read_ENVIData(inputfile_,[SUCCESSSTATUS = var],[DESCRIPTION=Decscription])
	;返回值为:(1)SuccessStatus=0 ,读取不成功为0
	;		  (2)SuccessStatus=1 ,读取成功,返回为数据
    ;注意该程序只限用于读标准ENVI文件(数据文件+头文件)
    ;,且只限读取单波段数据(改造后也许可读多波段的)
 	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'

	forward_function DC_ReadHead_file
; 	ON_ERROR, 2						;return to caller

    inputfile = inputfile_

	IF NOT KEYWORD_SET(Description) THEN Description=''

	DotPosition = STRPOS(inputfile, '.',/REVERSE_SEARCH)
    IF DotPosition EQ -1 THEN BEGIN
       HeadFile = STRTRIM(inputfile,2)+'.hdr'
       DataFile = STRTRIM(inputfile,2)
    ENDIF ELSE BEGIN
       HeadFile = STRMID(inputfile,0,DotPosition)+'.hdr'
       DataFile = STRMID(inputfile,0,DotPosition)
    ENDELSE

    CASE 1 OF
        ~FILE_TEST(HeadFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Description+'头文件不存在!',TITLE='警告')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END

        ~FILE_TEST(DataFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Description+'数据文件不存在!',TITLE='警告')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END
        ELSE:
    ENDCASE

    DataFileInfo = FILE_INFO(DataFile)
    HeadFileInfo = DC_ReadHead_file(HeadFile)
    if n_tags(HeadFileInfo) eq 1 then begin
    	SuccessStatus = 0
    	return,0
	 endif

    ;因为程序中文件大小均是以字节计算长度,而数据类型不同,计算行列数时也会有所不同.
    IF ROUND((DataFileInfo.SIZE)/FIX(HeadFileInfo.ByteNum),/L64) NE $
       ULONG(HeadFileInfo.samples)*(HeadFileInfo.lines) THEN BEGIN
       prompt = DIALOG_MESSAGE(Description+'数据文件与头文件信息行列数不一致!',TITLE='警告')
	   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
       RETURN,0
    ENDIF

    Type_code = FIX(HeadFileInfo.datatype)
    Data = MAKE_ARRAY(LONG(HeadFileInfo.samples),LONG(HeadFileInfo.lines),TYPE=Type_code)

	 case HeadFileInfo.datatype of
	 	'1': d=1
	 	'2': d=2
	 	'4': d=4
	 	'5': d=8
	 	'12': d=2
	 	'13': d=4
	 	'14': d=8
	 	'15': d=8
	 	else:d=1
	 endcase
    OPENR,Lun,DataFile,/GET_LUN
    result = fstat(lun)
    if long(result.SIZE/d) ne (LONG(HeadFileInfo.samples)*LONG(HeadFileInfo.lines)) then begin
    	Info = DIALOG_MESSAGE(DataFile+'数据文件不正确!',TITLE='警告')
    	SuccessStatus = 0
	 	return,-1
	 endif
    READU,Lun,Data
    FREE_LUN,Lun


	IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 1

   RETURN,Data

END
;***********************画插值图**************************************
;=================在"对象图形窗口"中打开影像图和矢量图=============================
PRO DC_Draw_image,INputFile   $	;被显示的影像文件名,字符串
				  ,WID_DRAW   $	;Draw组件,如果其有用户值data,则data维大小与INputFile文件影像维数据必须相同
				  ,OView=View $ ;返回的要得到的视图对象
				  ,MINVALUE=minvalue $  用于指定被BYTSCL()进行拉伸的最小值
				  ,WHITE=WHITE	 ;指定背景色为白色.
; 调用:DC_Draw_image,INputFile,WID_DRAW[,OView=View,MINVALUE=minvalue,/WHITE]
 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	forward_function DC_ReadHead_file

	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
	oWindow->ERASE,COLOR=255
	oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	ShapeFileName='data_vector\province.shp'
	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&组件

	;(1)------------读影像图-----------------------------------------
;	oColor = [128,128,128]
	IF (N_ELEMENTS(data) EQ 0) THEN data= DC_Read_ENVIData(INputFile) ;如果WID_DRAW没有用户值,则用输入影像
	IF ~KEYWORD_SET(WHITE)      THEN oColor = [255,255,255] ELSE oColor = [0,0,0]
	IF ~KEYWORD_SET(MINVALUE)  THEN minvalue = MIN(data)

	oScene = Obj_New('IDLgrScene', Color = oColor)
	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;创建视图
	Image_Layer   = Obj_New('IDLgrModel')	  ;影像图层
	Polygon_Layer = obj_new('IDLgrModel')     ;矢量图层

	oWindow->SetProperty, Graphics_Tree = oScene
	oWindow->SetCurrentCursor, 'ARROW'    	   ;鼠标指针为箭头,不是十字叉.

	oScene->Add, oView
	oView ->Add, Image_Layer
	oView ->Add, Polygon_Layer


	;将数值拉伸到256色显示范围内

	dataDis = BYTSCL(data,MIN=minvalue)

	Palette = obj_new('IDLgrPalette')
	Palette->LoadCt,39
	Palette->SetRGB,0,255,255,255

	;TVLCT,0,0,0    ;使0号颜色索引号颜色为白色(这里其实可以看成是背景色,默认是黑色),

	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
			               ,dataDis)   ;这里dataDis是拉伸数据

	Image_Layer->Add,oImage

	;----将矢量数据进行缩放的参数---------------------------------------
	Para = DC_ReadHead_file(INputFile)
	if n_tags(Para) eq 1 then 	return

	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;左上角
	samples = Double(Para.samples) & Lines = Double(Para.Lines)  ;
;;	Resolution = FIX(Para.Resolution)	用FIx时,若变量为"1.0000000000e+003",则不能被正确识别.被看成1
	Resolution = FLOAT(Para.Resolution)
	uvRange = [UlX,UlY-Lines*Resolution,UlX+samples*Resolution,UlY]
	uRange  = uvRange[2]-uvRange[0]
	vRange  = uvRange[3]-uvRange[1]

	Datasize = SIZE(data,/DIMENSIONS)
	xSize = Datasize[0]
	ySize = Datasize[1]
	drawSize = [r.scr_xsize,r.scr_ysize]

	;----读矢量数据图层----------------------------------------
	IF file_test(ShapeFileName) EQ 0  THEN return
	myshape=OBJ_NEW('IDLffShape', ShapeFileName)
	myshape -> IDLffShape::GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type

	IF (type EQ 5 OR type EQ 3) THEN BEGIN
	 FOR i=0,num_ent-1 DO BEGIN
	    ent = myshape -> IDLffShape::GetEntity(i)
	    NumPoints = ent.N_VERTICES-1
	    x = (*ent.vertices)[0,0:NumPoints]
	    y = (*ent.vertices)[1,0:NumPoints]
	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;这里将X,Y坐标进行缩放
		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange

	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[0,0,0],thick = 2,LINESTYLE = 0) ;[245,122,182]
	    Polygon_Layer->Add,oPolyline
	    myshape -> IDLffShape::DestroyEntity, ent
	 ENDFOR
	ENDIF
	OBJ_DESTROY, myshape

	;view的坐标体系
	viewPlane = [0,0,xSize,ySize]
	;view的大小
	Ratio = 1.0
	scale = Min((Double(drawSize*Ratio)/[xSize,ySize])<1)
	IF scale NE 1. THEN BEGIN
		viewDim = [Fix(scale*xSize), Fix(scale*ySize)]
	ENDIF ELSE BEGIN
		IF 1.*xSize/ySize GE 1.*drawSize[0]/drawSize[1] THEN BEGIN
			viewDim = [Fix(Ratio*drawSize[0]),Fix(Ratio*ySize/xSize*drawSize[0])]
		ENDIF ELSE BEGIN
			viewDim = [Fix(Ratio*xSize/ySize*drawSize[1]),Fix(Ratio*drawSize[1])]
		ENDELSE
	ENDELSE

	;view的插入位置
	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]

	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
	oWindow->Draw,oScene

	OBJ_DESTROY,Palette

	IF ARG_PRESENT(View) THEN View=oView
END

;$$$$$$$$$$$$$$$$$$$$$画柱状条$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO DC_Draw_BAR_PLOT,drawID $			;DRAW组件的ID值,即GET_VALUE
					,AnalysisData $		;要画的数据
					,XLabel	$			;X轴的标签�
					,LINE = Line $		;只画线性图,默认为只画柱状图
					,CHARTLINE = ChartLine  ;同时画线状和柱状图
;调用:DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel[,/Line,/ChartLine]
;本程序只适用于"产量融合模块"的使用,对于其他程序,可能不适用.
 	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	  PlotNum = N_ELEMENTS(XLabel)    ;类型的个数

	  DEVICE,GET_DECOMPOSED=old_color     ;获取当前DECOMPOSED值
;      DEVICE,GET_CURRENT_FONT=oldFont
;      DEVICE,SET_FONT='宋体',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;用IDL提供后备存储,使用颜色查询表(停用颜色分解功能),
		r=[0,255,  0,  0,255,255]   	  ;依次为黑\红\绿\蓝\黄\白
		g=[0,  0,255,  0,255,255]
		b=[0,  0,  0,255,  0,220]
 		TVLCT, r, g, b   ;缺省第四个省数,则使颜色表中索引号为0,1,2,3,4,5的颜色为相应的RGB组合


    OldWin = !D.WINDOW     			   ;保存系统的窗口
    OldBackup = !P.BACKGROUND
	Old_p     = !P.POSITION
    Old_Font  = !P.FONT
    OldFontSiz = !P.CHARSIZE
    OClor = !P.COLOR
    OldYticks = !Y.TICKS
    !P.FONT = 0
	!P.BACKGROUND = 255
	!P.COLOR = 800   ;轴的颜色
    !P.CHARSIZE = 0.8

    Colors = INTARR(PlotNum)
	FOR I = 0, PlotNum-1 DO Colors[I]= I ;(2*I)+100

   	WSET, drawID
	PlotData = FLOAT(AnalysisData)
	Psym = 3   ;(点标识)
	IF KEYWORD_SET(ChartLine) THEN BEGIN   ;画柱状和线状图
	    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+4)+' ' $ 	;YTICKLEN控制标志的长度
			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKS用于控制主标志的个数
;		PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
;	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN控制标志的长度
;			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKS用于控制主标志的个数

		OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;用白颜色将PLOT画的线覆盖掉,还要注意给XRANGE负值,不然画图效果差

	    FirstOFFSET = 0.3 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
	    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
			,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
			,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-6
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(Line) THEN BEGIN  ;只画线状图
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
		         ,POSITION=[0.09,0.28,0.96,0.93] ,XTICKNAME=[' ',' ',XLabel,' ',' ',' '] $ 	;YTICKLEN控制标志的长度
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)] $
				 ,XTICKLEN=1 ; ,TITLE = TITLE				;YTICKS用于控制主标志的个数

			OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-5
		ENDIF ELSE BEGIN				;只画柱状图
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+3)+' ' $ 	;YTICKLEN控制标志的长度
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKS用于控制主标志的个数
;			PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
;		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN控制标志的长度
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKS用于控制主标志的个数


			OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;用白颜色将PLOT画的线覆盖掉,还要注意给XRANGE负值,不然画图效果差
		    FirstOFFSET = 0.8 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
		    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $
				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
				,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		ENDELSE
	ENDELSE

	!P.BACKGROUND = OldBackup		;还原
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor
	!Y.TICKS = OldYticks
;	DEVICE,SET_FONT=oldFont
	DEVICE,DECOMPOSED=old_color   ;返回原来的DECOMPOSED值,因为自定义函数MyColor改变了,须还原.

	WSET, OldWin				;还原原来窗口.

END

;$$$$$$$$将二维数组保存为*.txt格式文件$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO DC_SaveTextData,Save_Data 			$		;要保存的二维数组
				   ,Parent				$		;WidgetID,作为文件选择对话窗口的父组件.
				   ,FILENAME = FILENAME $ 		;指定要保存的文件名
				   ,NOSavePath = NOSavePath		;不保存路径,默认保存
;调用:DC_SaveTextData,Save_Data,Parent,[FILENAME=Filename,/NOSavePath]

;注意:如果Save_Data是一行数据,则该程序可能出现问题,在于SIZE(/DIMENSIONS)的原因

 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	IF NOT KEYWORD_SET(FILENAME) THEN FILENAME=''

	TEMP = WHERE(Save_Data EQ '',COUNT)
	IF COUNT NE 0 THEN Save_Data[TEMP]='---'

	Filename=DIALOG_PICKFILE(TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
	  ,/OVERWRITE,FILE = FILENAME,/WRITE,PATH=DC_PathSetting() $
	  ,GET_PATH=SavePath,DIALOG_PARENT=Parent)

	IF Filename EQ '' THEN RETURN

	DataSize = SIZE(Save_Data,/DIMENSIONS)
	Samples = DataSize[0]
	OPENW,LUN,Filename,/GET_LUN ,WIDTH=Samples*(MAX(STRLEN(STRTRIM(Save_Data,2)))+1)
	PRINTF,LUN,Save_Data;,FORMAT='(5(A20,2X))'
	FREE_LUN,LUN

	INFO = DIALOG_MESSAGE('保存成功!',/INFORMATION,TITLE='提示')

	IF KEYWORD_SET(NOSavePath) THEN RETURN

	path = DC_PathSetting(WRITEPATH1= SavePath)

END
;------------加权到省-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,这里用CROP_AREA_COUNTY
;注意下面取作物面积时,如果当前年份的面积数据没有,则用库中最近年份Newestyear的面积数据
FUNCTION DC_WeightToPro,CountyYield  	$	;2列值,县码\估算产量,
					   ,Crop_id 		$	;所有变量均是字符型
					   ,CalYear 		$
					   ,ProID 			$        ;2位
					   ,model_type_id  		;省估算产量结果表中类型ID,

;调用形式: DC_WeightToPro,CountyYield,Crop_id,CalYear,ProID,model_type_id
;返回值:1 计算成功
;		0 计算不成功

 	IF (N_PARAMS() NE 5) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	COMMON COMMON_BLOCK

	CropIDList = ['11','12','21','22','23','31','32','41']    ;CropIDList必须与CropFieldList对应
    CropFieldList = ['SPRING_WHEAT','WINTER_WHEAT','EARLY_RICE','SEMILATE_RICE','LATE_RICE','SPRING_CORN','SUMMER_CORN','SOYBEAN']

    C_Index = WHERE(CropIDList EQ crop_id[0],count)
    IF count EQ 0 THEN BEGIN
       INFO = DIALOG_MESSAGE('数据库中没有作物编号"'+Crop_id[0]+'",请确认!',TITLE='警告')
       RETURN,0
    ENDIF

    CropField = CropFieldList[C_Index]
    Sqlstr ='select distinct Year as NewestYear from CROP_AREA_COUNTY where YEAR='+CalYear
    Currentyear = DC_GetdataFromDB_Str(1,SQLstr,N_RECORDS = Num)
    IF Num EQ 0 THEN BEGIN
       INFO = DIALOG_MESSAGE(['数据库中CROP_AREA_COUNTY表中没有当前年份相应' $
       						 ,'县作物的面积数据,使用最近年份的面积数据吗?!'],/QUESTION,TITLE='询问')
       IF INFO EQ 'No' THEN RETURN,0
       Sqlstr='select max(year) as NewestYear from CROP_AREA_COUNTY' ;这一步是为最近年份
    ENDIF

    Sqlstr='select county_code,'+CropField+' from CROP_AREA_COUNTY a,('+Sqlstr $
    		+') b where a.year=b.NewestYear and '+CropField+'>0 ' $
    		+'and LEFT(a.county_code,2)='	+ProID
	CountyCropArea = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = Num)   ;2列值,县码\作物面积
	IF Num EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('数据库中CROP_AREA_COUNTY表中没有相应县作物的面积数据!',TITLE='警告')
		RETURN,0
	ENDIF

    CountyNum=N_ELEMENTS(CountyYield)/2

      CountyYieldArea=STRARR(3,1) & Count = 0
      FOR i=0,CountyNum-1 DO BEGIN
          Temp=STRARR(3,1)
           FOR j=0,Num-1 DO BEGIN
               IF CountyYield[0,i] EQ CountyCropArea[0,j] THEN BEGIN
                  Temp[0,0]= CountyYield[0,i]    	;县码
                  Temp[1,0]= CountyYield[1,i]    	;县估算产量
                  Temp[2,0]= CountyCropArea[1,j]    ;县作物面积
                  Count += 1
          		  CountyYieldArea=[[CountyYieldArea],[Temp]]
               	  BREAK
               ENDIF
           ENDFOR
      ENDFOR
      IF Count EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('数据库中CROP_AREA_COUNTY表中没有相应县作物的面积数据!',TITLE='警告')
		RETURN,0
      ENDIF
      CountyYieldArea = CountyYieldArea[*,1:*]
      Area = FLOAT(CountyYieldArea[2,*])
      ;得到加权的省作物单产量,乘以15,则单位为公斤/公顷
	  ProYield = STRTRIM(TOTAL((Area/TOTAL(Area))*FLOAT(CountyYieldArea[1,*]))*15,2)

      Sqlstr1="delete from PROVINCE_ESTIMATED_YIELD where crop_id='"+ crop_id+ $
             "' and Year="+CalYear+' and model_type_id='+model_type_id $
             +' and LEFT(Province_Code,2)='+ProID

	  Sqlstr2="insert into PROVINCE_ESTIMATED_YIELD values('"+crop_id+"','"+ $
	         ProID+'0000'+"',"+ProYield+','+model_type_id+','+CalYear+')'

	  DBobj->ExecuteSQL,Sqlstr1
	  DBobj->ExecuteSQL,Sqlstr2

	  RETURN,1

END
;---------------------------------------
PRO DC_A_Pro_Fun  ;文件头可要可不要
END