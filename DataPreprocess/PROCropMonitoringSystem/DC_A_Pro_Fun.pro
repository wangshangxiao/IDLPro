
;ËµÃ÷:DC_A_Pro_Fun.pro
;×÷ÓÃ:¸ÃÎÄ¼þ´æ·Åµ¥²úÔ¤²â·ÖÎö¼ÆËãÖÐËùÓÃµÄ¹«ÓÃº¯Êý»ò¹ý³Ì
;Ê±¼ä:2006.8.18
;×÷Õß:ÐìÐÂ¸Õ²©Ê¿(2004½ì)
;×¢Òâ:(1)µ÷ÓÃÕâÐ©¹ý³Ì»òº¯ÊýÖ®Ç°,Ó¦±£Ö¤ÎÄ¼þÏÈ±»±àÒë.
;	  (2)ÁíÍâ,×îºÃ²»ÒªÔÙ¸Ä¶¯¸÷º¯Êý»ò¹ý³ÌµÄË³Ðò,ÒòÎªËüÃÇÖ®¼äÒ²ÓÐ±»µ÷ÓÃ¹ØÏµ.

;******×Ô¶¨Òå¹ý³Ì:µ±Ò»¸ö´°¿ÚÏûÍöÊ±,ÊÍ·Å¶Ñ±äÁ¿
PRO DC_CleanAllHeap,TLB
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2

    WIDGET_CONTROL,TLB,GET_UVALUE=PA
    HEAP_FREE,PA   ;
;  	HEAP_GC,/VERBOSE
END
;*******×Ô¶¨Òåº¯Êý:ÅÐ¶Ï×Ö·ûÊäÈëÊÇ·ñÕýÈ·*************
FUNCTION DC_JudgeInputChar,Inputchar $   	  ;ÊäÈëµÄÒªÅÐÊýµÄ×Ö·û´®
 						  ,Desc=Desc $	      ;ÓÃ»§ÊäÈëµÄ´íÎóÌáÊ¾ÃèÊö×Ö·ûÇ°×º.
 						  ,INTEGER=integer $  ;ÓÃÓÚÅÐ¶ÏÊäÈëÊÇ·ñÖ»ÊÇÕûÐÍ.Ä¬ÈÏÅÐ¶ÏÊäÈëÎª¸¡µãÐÍ
						  ,NEGATIVE=negative  ;±êÃ÷ÊäÈë¿ÉÒÔÎª¸º,Ä¬ÈÏÊäÈëÖ»ÄÜÎªÕý
      ;µ÷ÓÃÐÎÊ½Îª: Result = DC_JudgeInputChar(Inputchar,[Desc=Description],[/INTEGER],[/NEGATIVE])
      ;·µ»ØÖµ:(1)¸¡µãÐÍ»òÕûÐÍÖµ
      ;		  (2)Öµ-1 (ÊäÈëÁË·Ç·¨×Ö·û)

       IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	   ON_ERROR,2

       IF NOT KEYWORD_SET(Desc) THEN Desc = ''

       CompareStr=['0','1','2','3','4','5','6','7','8','9','.']     ;ÊäÈëµÄ×Ö·ûÖ»ÔÚÕâ11¸öµ±ÖÐ.
	   IF KEYWORD_SET(NEGATIVE) THEN BEGIN
       		CompareStr=['-','0','1','2','3','4','5','6','7','8','9','.']     ;ÊäÈëµÄ×Ö·ûÖ»ÔÚÕâ12¸öµ±ÖÐ.
	   ENDIF

        IF STRLEN(Inputchar) EQ 0 THEN BEGIN                        ;ÅÐ¶ÏÃ»ÓÐÊäÈëµÄÇé¿ö
           Prompt=DIALOG_MESSAGE(Desc+'Ã»ÓÐÊäÈëÖµ,ÇëÏÈÊäÈë!',TITLE='¾¯¸æ')
           RETURN,-1
        ENDIF

  		N=0
  		FOR i=0,STRLEN(Inputchar)-1 DO BEGIN                          ;ÅÐ¶Ï·Ç·¨×Ö·ûµÄÇé¿ö
           JudgeChar=STRMID(Inputchar,i,1)
           aa = WHERE(CompareStr EQ JudgeChar,Count)
           IF Count EQ 0 THEN BEGIN
              Prompt=DIALOG_MESSAGE(Desc+'ÊäÈëÁË·ÇÊý×Ö×Ö·û,ÇëÖØÐÂÉèÖÃ!',TITLE='¾¯¸æ')
              RETURN,-1
           ENDIF
           IF JudgeChar EQ '.' THEN N=N+1           ;NÓÃÓÚ¼ÇÂ¼Ð¡ÊýµãµÄ¸öÊý
        ENDFOR

       IF KEYWORD_SET(INTEGER) THEN BEGIN
		  IF N NE 0 THEN BEGIN
              Prompt=DIALOG_MESSAGE(Desc+'Ó¦ÊäÈëÕûÐÍÖµ,ÇëÖØÐÂÉèÖÃ!',TITLE='¾¯¸æ')
              RETURN,-1
		  ENDIF ELSE BEGIN
              RETURN,LONG(Inputchar)
		  ENDELSE
       ENDIF

       IF N GT 1 THEN BEGIN
          Prompt=DIALOG_MESSAGE(Desc+'ÊäÈëÁË¶à¸öÐ¡Êýµã,ÇëÖØÐÂÉèÖÃ!',TITLE='¾¯¸æ')
          RETURN,-1
       ENDIF

       IF (STRMID(Inputchar,0,1) EQ '.') THEN BEGIN
		  Inputchar = '0'+Inputchar
          RETURN,FLOAT(Inputchar)
       ENDIF

       RETURN,FLOAT(Inputchar)
 END
;*****************´ÓÊý¾Ý¿âÖÐ¶ÁÈ¡Êý¾Ý,·µ»ØÎª×Ö·ûÊý×é********************************
FUNCTION DC_GetdataFromDB_Str,Columns $        	;ÒªµÃµ½µÄÊý¾ÝÁÐÊý,±ØÐëÓëSQLstrÀïµÄÒ»ÖÂ
							 ,SQLstr	 $      	;SQLÓï¾ä.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;´ÓÊý¾Ý¿âÖÐÌáÈ¡Êý¾ÝÊ±µÄ»º´æÊýÁ¿,ÎªÕûÐÍÖµ
							 ,N_RECORDS = NumReocrd	  		;µÃµ½¼ÇÂ¼ÌõÊý

	 ;¸Ãº¯Êýµ÷ÓÃÐÎÊ½Îª:
	 ;result = DC_GetdataFromDB_Str(Columns,SQLstr,Num_BUFFERS = Num,N_RECORDS = var)
	 ;½á¹ûÓÐÁ½ÖÖ:Çø±ðº¯ÊýDC_GetDataFromDB()
	 ;(1) ·µ»Ø²éÑ¯µ½µÄÊý¾Ý,Îª×Ö·ûÐÍÊý×é.
	 ;(2) ·µ»ØÎªÒ»ÐÐ¿ÕÖµ(¸öÊýÎªÁÐÖµ)µ±²éÑ¯²»³É¹¦Ê±.

;	  IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
;	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;Ä¬ÈÏ»º´æÊýÁ¿Îª10

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
;;	   ColumnNum  = DataRecOBJ->NFields()   ;´Ë´¦µÄColumnNum = Columns,ÕâÀïÃ»ÓÐÓÃµ½,·À¿Õ²éÑ¯µÄÇé¿ö
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN
	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()            ;µÃµ½ÏàÓ¦ÏØ»òÇø»®µÄÏàÓ¦Äê·ÝµÄ²úÁ¿********

				;===ÑîÉÜïÉÐÞ¸Ä£¬20070903=============================
;	             FOR i=0,N-1 DO Temp[i,0] = DataValue.(i)	;Ô­´úÂë

				FOR i=0,N-1 DO begin
					if size(DataValue.(i),/type) eq 10 then begin	;Êý¾ÝÎªÖ¸Õë£¬Êý¾ÝÀàÐÍÎª10£¬¼´Êý¾Ý¿âÖÐµÄ±¸×¢
						Temp[i,0] = string(*(DataValue.(i)))
					endif else begin
						Temp[i,0] = DataValue.(i)
					endelse
				endfor
				;====================================================
	             DATA=[[DATA],[Temp]]				;½øÐÐÁÐ´®½Ó.
	           ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;È¥µôÊ×ÐÐ,µÃµ½È«²¿Êý¾Ý
	       ENDIF

	   OBJ_DESTROY,DataRecOBJ

	   IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum

	   DATA = STRTRIM(TEMPORARY(DATA),2)
	   RETURN,DATA
END
;*****************´ÓÊý¾Ý¿âÖÐ¶ÁÈ¡Êý¾Ý,·µ»ØÎª½á¹¹ÌåÊý×é********************************
FUNCTION DC_GetDataFromDB,Sql	 $    		      		;SQLÓï¾ä.×Ö·û´®
					    ,Num_BUFFERS = Num_BUFFERS $  	;´ÓÊý¾Ý¿âÖÐÌáÈ¡Êý¾ÝÊ±µÄ»º´æÊýÁ¿,ÎªÕûÐÍÖµ
						,N_RECORDS = NumReocrd	  		;µÃµ½¼ÇÂ¼ÌõÊý
	 ;¸Ãº¯Êýµ÷ÓÃÐÎÊ½Îª:
	 ;result = GetDataFromDB(Sql,Num_BUFFERS = Num,N_RECORDS = var)
	 ;½á¹ûÓÐÁ½ÖÖ:
	 ;(1) ·µ»Ø²éÑ¯µ½µÄÊý¾Ý,Îª½á¹¹ÌåÊý×é
	 ;(2) ·µ»Ø¿ÕÖµµ±²éÑ¯²»³É¹¦Ê±.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;Ä¬ÈÏ»º´æÊýÁ¿Îª10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	 ;´Ë´¦Ã»ÓÃ"RecordNumOBJ->MoveCursor(/FIRST)",ÊÇÒòÎªËüÒ»¶¨ÓÐÇÒÖ»ÓÐÒ»Ìõ¼ÇÂ¼.ÀàÐÍÎª½á¹¹Ìå
	  RecordNum = RecordNumOBJ->GetRecord()

	  IF RecordNum.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)		;ÖµÎª0,¼´RecordNum.(0)Îª0
	      RETURN,''
	  ENDIF ELSE OBJ_DESTROY,RecordNumOBJ

	  GetDataOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL=Sql,N_BUFFERS=Num_BUFFERS)
	  DataValue = GetDataOBJ->GetRecord()         ;ÊÇÎªµÃµ½Ò»¸ö³õÊ¼»¯µÄ½á¹¹Ìå,Ò»°ãÊÇ²éÑ¯½á¹ûµÄµÚÒ»¼ÇÂ¼.

	  DATA=REPLICATE(DataValue,RecordNum.(0))    ;Éú³É½á¹¹ÌåÊý×é
	  ReIndex = 0L

	   IF (GetDataOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	       REPEAT BEGIN
			DATA[ReIndex] = GetDataOBJ->GetRecord()
			ReIndex = ReIndex+1						;×îÖÕµÄReIndexÓ¦¸ÃµÈÓÚRecordNum.(0)
	       ENDREP UNTIL(GetDataOBJ->MoveCursor(/NEXT) EQ 0)
	   ENDIF

	OBJ_DESTROY,GetDataOBJ

	IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)

	RETURN,DATA

END
;--×Ô¶¨Òåº¯Êý:ÓÃÓÚ¶ÁÈ¡»ò±£´æÓÃ»§µÄ¹¤×÷Â·¾¶---------------------------------------
FUNCTION DC_PathSetting,WritePath1=writepath1 $     ;Ð´µ½µÚÒ»Â·¾¶(´æ´¢ÊäÈëÂ·¾¶)
					  ,WritePath2=writepath2 $     ;Ð´µ½µÚ¶þÂ·¾¶(´æ´¢Êä³öÂ·¾¶)
					  ,ReadPath2=readpath2         ;Ö»¶ÁÈ¡µÚ¶þÂ·¾¶,Ä¬ÈÏ¶ÁÈ¡µÚÒ»Â·¾¶

 ;¸Ãº¯Êýµ÷ÓÃÐÎÊ½Îª:
 ;result = DC_PathSetting([WritePath1=writepath1,WritePath2=writepath2,/ReadPath2])

 ;(1)µ±È±Ê¡ËùÓÐ±äÁ¿Ê±£¬·µ»ØÖµÎªÄ¬ÈÏ¶ÁÈ¡µÄµÚÒ»Â·¾¶£¨¼´ÊäÈëÂ·¾¶),×Ö·û´®(ÏÂÍ¬)
 ;(2)µ±Ö»ÓÃ/ReadPath2,·µ»ØÖµÎª¶ÁÈ¡µÄµÚ¶þÂ·¾¶
 ;(3)µ±Í¬Ê±ÉèÖÃÁË¶ÁÓëÐ´Ê±,Ö»½øÐÐÐ´,½«Â·¾¶±£´æµ½ÎÄ¼þ"pathsetting.txt"ÖÐ,·µ»ØÖµÎª¿Õ
 ;(4)µ±Í¬Ê±ÉèÖÃÐ´Ê±,½«Â·¾¶±£´æµ½ÎÄ¼þ"pathsetting.txt"ÖÐ,·µ»ØÖµÎª¿Õ
 ;(5)µ±ÊäÈë±äÁ¿Ê±£¬Ôò·¢Éú´íÎó¡£

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
	OPENR,lun,File,/get_lun       ;¶Á³öÎÄ¼þÖÐµÄÂ·¾¶
	READF,lun,SettingPath                           ;×¢ÒâÕâÀïÊÇÓÃ"READF"
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
			OPENW,lun,File,/get_lun        ;½«Â·¾¶Ð´Èëµ½ÎÄ¼þÖÐ.
			PRINTF,lun,StorePath
			FREE_LUN,lun

	  		RETURN,''
	  ENDIF

    IF KEYWORD_SET(ReadPath2) THEN BEGIN
    	RETURN,Allpath[1]                  ;¶Á±£´æµÄÊä³öÂ·¾¶¡££¨µÚ¶þÂ·¾¶£©
    ENDIF ELSE BEGIN
    	RETURN,Allpath[0]    			   ;¶Á±£´æµÄÊäÈëÂ·¾¶¡££¨µÚÒ»Â·¾¶£©
    ENDELSE

END


 ;***************×Ô¶¨Òåº¯Êý:¶Ôµ¥Ñ®²¨¶¯Êý¾Ý½øÐÐ×éºÏ********************************************
;Ö÷ÒªÓÃÓÚ²¨¶¯²úÁ¿²ÎÊýÌáÈ¡ÒÔ¼°Åú´¦ÀíÔËËãÄ£¿é(DC_Floatyield.proºÍDC_Floatyield_2.pro)
FUNCTION DC_FactorCombination,AllData,MeteoTableIndex,RowsNum,StartMonth,EndMonth

	IF (N_PARAMS() NE 5) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

       SelectedFactorIndex=WHERE(MeteoTableIndex EQ 1,FactorNum)   ;FactorNumÖ¸ÆßÖÖÒò×ÓÖÐ±»Ñ¡µÄ¸öÊý
       CombinationData=Fltarr(1,RowsNum)
       T_ColumnNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3
       FOR k=0,FactorNum-1 DO BEGIN                                         ;ÕâÒ»²½ÊÇ¶Ô»ñµÃµÄµ¥Ñ®Êý¾Ý½øÐÐ×éºÏÏàÁÚÁ½Ñ®ºÍÈýÑ®×éºÏ.
           InterimData=FLOAT(AllData[k*T_ColumnNum:(k+1)*T_ColumnNum-1,0:*]);µÃµ½Ò»ÖÖ²¨¶¯Òò×ÓµÄËùÓÐµ¥Ñ®Êý¾Ý
           aa=WHERE(STRMATCH(['1','2','4'],STRTRIM(SelectedFactorIndex[k],2)) EQ 1,Count)
           Interim2Initial=Fltarr(1,RowsNum)
           Interim3Initial=Fltarr(1,RowsNum)
           IF Count NE 0 THEN BEGIN                                         ;ÒÔÇóºÍ·½Ê½½ø×éºÏ(Ö»¶Ô"½µË®/ÈÕÕÕ/»ýÎÂ",·Ö±ð¶ÔÓ¦['1','2','4'])
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
           ENDIF ELSE BEGIN                                                 ;·ñÔòÒÔÇóÆ½¾ù·½Ê½×éºÏ
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
           CombinationData=STRTRIM(CombinationData[1:*,0:*],2)     ;×ª»¯Îª×Ö·ûÐÍ
   return,CombinationData
END

;****************×Ô¶¨Òåº¯Êý:ÌáÈ¡µ¥Ñ®µÄÆøÏó²¨¶¯Òò×ÓÊý¾Ý(ÖØÒª²¿·Ö)********************************
;Ö÷ÒªÓÃÓÚ²¨¶¯²úÁ¿²ÎÊýÌáÈ¡ÒÔ¼°Åú´¦ÀíÔËËãÄ£¿é(DC_Floatyield.proºÍDC_Floatyield_2.pro)
FUNCTION DC_SingleTendayFactor,StartYear_,EndYear_,StartMonth_,EndMonth_,MeteoTableIndex,station_code,YieldType
    ;µ÷ÓÃ:Result=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,EndMonth,MeteoTableIndex,station_code,/AgroTable)

	IF (N_PARAMS() NE 7) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	WIDGET_CONTROL, /HOURGLASS

	TableName=['TENDAY_TEMP_AVG','TENDAY_RAINFALL','TENDAY_SUNSHINE','TENDAY_AIR_HU', $
               'TENDAY_TEMP_0_ACCUMULATE','TENDAY_TEMP_MAX','TENDAY_TEMP_MIN']      ;×¢Òâ¶ÔÓ¦½çÃæÖÐµÄÒò×ÓË³Ðò

	IF YieldType THEN BEGIN
	   TableName = 'AGRO_'+TableName
	ENDIF ELSE TableName = 'METEO_'+TableName

   StartYear = FIX(StartYear_)   & EndYear = FIX(EndYear_)
   StartMonth = FIX(StartMonth_) & EndMonth = FIX(EndMonth_)
  ;Ö®ËùÒÔÓÃ²¨¶¯Òò×Ó\Äê\ÔÂ\Ñ®µÄËÄ²ãÑ­»·,¿ÉÒÔ±£Ö¤Ò»Ð©Õ¾µãÒ»Ð©Ñ®ÎÞÊý¾ÝÊ±,ÓÃ¿ÕÖµ´úÌæ.¾«¶È¿ÉÒÔÈ·±£,µ«ËÙ¶ÈÂýÏÂÀ´.
    RowsNum =EndYear-StartYear+1                                 ;¼´ÄêÊý
    T_ColumnNum=(EndMonth-StartMonth+1)*3                        ;µ¥Ñ®Êý
    AllData=STRARR(1,RowsNum)+'%'                                ;³õÊ¼»¯ÁÐÖµ

	progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='ÕýÔÚÊý¾Ý´¦ÀíÖÐ,ÇëÉÔºò...',TITLE='É¸Ñ¡Òò×Ó');,/CANCELBUTTON)
	progressTimer->START        ;Æô¶¯½ø¶ÈÌõ

	IsSelectID = WHERE(MeteoTableIndex EQ 1,COUNT)
	TOL = LONG(COUNT)*RowsNum & NN=1

       FOR i=0,6 DO BEGIN

          IF MeteoTableIndex[i] EQ 1 THEN BEGIN
              TempyearMeteo=STRARR(T_ColumnNum,1)+'*'            ;³õÊ¼»¯ÐÐÖµ
             FOR j=StartYear,EndYear DO BEGIN

;				CANCELLED = progressTimer->CHECKCANCEL()    ;×¢Òâµ÷ÊÔ??????????????????
;				IF CANCELLED THEN BEGIN
;					OK = DIALOG_MESSAGE('ÄúÖÕÖ¹ÁË"É¸Ñ¡"²Ù×÷!',TITLE='¾¯¸æ')
;					OBJ_DESTROY,progressTimer ;½áÊø½ø¶ÈÌõ
;				    RETURN,''
;				ENDIF
      	        progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;Æô¶¯½ø¶ÈÌõ
      	         NN+=1

                TempmonthMeteo=STRARR(1)
                MeteoData=STRARR(T_ColumnNum,1)

                 Sqlstr0='select count(*) as recordNum from '+TableName[i]+' where station_id='+"'"+station_code+"'"+' and '+ $
                        'year='+STRTRIM(j,2)+' and month between '+STRTRIM(StartMonth,2)+' and '+STRTRIM(EndMonth,2)

				recordNum=LONG(DC_GetdataFromDB_Str(1,Sqlstr0,Num_BUFFERS = 500))
                 IF recordNum EQ T_ColumnNum THEN BEGIN  ;ËµÃ÷ËùÓÐÔÂ¸÷Ñ®¶¼ÓÐÊý¾Ý.²»È±Ñ®.
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
                          TemptendayMeteo=TemptendayMeteo[1:*]                         ;µÃµ½Ò»¸öÔÂÈýÑ®µÄÊý¾Ý
                          TempmonthMeteo=[TempmonthMeteo,TemptendayMeteo]
                     ENDFOR
                          TempmonthMeteo=TempmonthMeteo[1:*]                           ;µÃµ½Ò»¸öÄêËùÓÐÔÂµÄÊý¾Ý
                          TempyearMeteo=[[TempyearMeteo],[TempmonthMeteo]]             ;Ê¹ÓÃÊý×é"ÁÐ´®½Ó"¼¼ÇÉ
                   ENDELSE

              ENDFOR
                TempyearMeteo=TempyearMeteo[0:*,1:*]                        ;È¥µôµÚÒ»ÐÐ*ºÅ³õÊ¼µÄÖµ,µÃµ½Ò»ÖÖ²¨¶¯Êý¾ÝËùÓÐÄê·ÝµÄÊý¾Ý
                AllData=[AllData,[TempyearMeteo]]                           ;Ê¹ÓÃÊý×é"ÐÐ´®½Ó"¼¼ÇÉ,Êý×éºáÅÅ´®³ÉÐÐ
           ENDIF
        ENDFOR

;;        progressTimer->DESTROY ;Ïú»Ù½ø¶ÈÌõ
		OBJ_DESTROY,progressTimer

        AllData=AllData[1:*,0:*]                                           ;µÃµ½ËùÓÐµ¥Ñ®µÄÒò×ÓÊý¾Ý

   return,AllData
END

;*******×Ô¶¨Òåº¯Êý:¶Ôµ¥Ñ®²¨¶¯Êý¾ÝÖÐµÄ¿ÕÖµÒÔËùÔÚÁÐÆäËû·Ç¿ÕÖµµÄ¾ùÖµÀ´Ìæ»»*************
;Ö÷ÒªÓÃÓÚ²¨¶¯²úÁ¿²ÎÊýÌáÈ¡Ä£¿é(DC_Floatyield_2.pro)
;µ±È»±¾º¯ÊýÒ²¿ÉÒÔ×÷ÆäËûÓÃÍ¾
FUNCTION DC_ProcessBlank,DataValue $        ;Òª½øÐÐ´¦ÀíµÄµ¥Ñ®ÆøÏóÊý¾Ý.×Ö·ûÐÍ
					 	,Rows $		     ;µ¥Ñ®Êý¾ÝµÄÐÐÊý,Êµ¼ÊÉÏÒ²ÊÇÄêÊý
					 	,BlankId = BlankId  ;Îª¿ÕÖµµÄË÷ÒýºÅ

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
		   	  	 Temp[Temp_id] = 0.0			    ;¼´È«Îª¿Õ,ÔòÒÔ0À´È¡´ú
		   	  	 DataValue[I,*] = STRTRIM(Temp,2)
		   	 ENDELSE
	   	  ENDIF
	   ENDFOR

   	  RETURN,DataValue
END

;***********×Ô¶¨Òåº¯Êý:×éºÏÑ®Ãû*******************************************************
;Ö÷ÒªÓÃÓÚ²¨¶¯²úÁ¿²ÎÊýÌáÈ¡Ä£¿é(DC_Floatyield_2.pro)
 FUNCTION DC_CombinationTendayName,TendayFactorName,MeteoTableIndex,SingleTendayNum

	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    SelectedFactorIndex = WHERE(MeteoTableIndex EQ 1,FactorNum)
    SingleFactor=[''] & CombinationFactor=[''] & CombinationName=TendayFactorName
    FOR i=0,FactorNum-1 DO BEGIN
         TendayName=TendayFactorName & FactorPostfix2=[''] & FactorPostfix3 = ['']
        CASE SelectedFactorIndex[i] OF
           0 : Postfix='¾ùÎÂ'
           1 : Postfix='½µË®'
           2 : Postfix='ÈÕÕÕ'
           3 : Postfix='Êª¶È'
           4 : Postfix='»ýÎÂ'
           5 : Postfix='×î¸ßÎÂ'
           6 : Postfix='×îµÍÎÂ'
        ENDCASE
        TendayName=TendayName+Postfix
        SingleFactor=[SingleFactor,[TendayName]]             ;µÃµ½µÄÊÇµ¥Ñ®µÄÒò×ÓÃû,º¬³õÊ¼Öµ

        FOR j=0,SingleTendayNum-2 DO BEGIN
           TempPostfix2=CombinationName[j]+CombinationName[j+1]
           FactorPostfix2=[FactorPostfix2,[TempPostfix2]]
        ENDFOR
        FOR j=0,SingleTendayNum-3 DO BEGIN
           TempPostfix3=CombinationName[j]+CombinationName[j+1]+CombinationName[j+2]
           FactorPostfix3=[FactorPostfix3,[TempPostfix3]]
        ENDFOR
        FactorPostfix2=FactorPostfix2[1:*]+Postfix                 ;ÏàÁÚ2Ñ®×éºÏ
        FactorPostfix3=FactorPostfix3[1:*]+Postfix                 ;ÏàÁÚ3Ñ®×éºÏ
        FactorPostfix23=[FactorPostfix2,[FactorPostfix3]]
        CombinationFactor=[CombinationFactor,[FactorPostfix23]]    ;µÃµ½µÄÊÇ×éºÏÑ®µÄÒò×ÓÃû,º¬³õÊ¼Öµ

     ENDFOR
     SingleFactor=SingleFactor[1:*] & CombinationFactor=CombinationFactor[1:*]
     AllFactorName=[SingleFactor,[CombinationFactor]]
   RETURN,AllFactorName
END

;*********×Ô¶¨Òåº¯Êý:µÃµ½µ¥Ñ®Ãû*******************************************************
;Ö÷ÒªÓÃÓÚ²¨¶¯²úÁ¿²ÎÊýÌáÈ¡Ä£¿é(DC_Floatyield_2.pro)
 FUNCTION DC_SingleTendayName,StartMonth_,EndMonth_
	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

      StartMonth=FIX(StartMonth_) & EndMonth=FIX(EndMonth_)
      FactorName=['']
    FOR i=StartMonth,Endmonth DO BEGIN
        FOR j=1,3 DO BEGIN
            CASE j OF
                  1: Tenday='ÉÏÑ®'
                  2: Tenday='ÖÐÑ®'
                  3: Tenday='ÏÂÑ®'
               ELSE:
            ENDCASE

            FactorName=[FactorName,STRTRIM(STRING(i),2)+'ÔÂ'+Tenday]
        ENDFOR
     ENDFOR
     FactorName=FactorName[1:*]
   RETURN,FactorName
END

;****************×Ô¶¨Òåº¯Êý:ÌáÈ¡Ò£¸ÐÒò×ÓÊý¾Ý********************************
;Ö÷ÒªÓÃÓÚ²¨¶¯²úÁ¿²ÎÊýÌáÈ¡ÒÔ¼°Åú´¦ÀíÔËËãÄ£¿é(DC_Floatyield.proºÍDC_Floatyield_2.pro)
FUNCTION DC_GetRsData,StartYear_,EndYear_,StartMonth_,EndMonth_,Code_,CropFiled,IsAvgOrSum,Sensor,DataType,YieldType
    ;µ÷ÓÃ:Result=DC_GetRsData(StartYear,EndYear,StartMonth,EndMonth,,Code,CropFiled,Sensor,DataType,YieldType)
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

    RowsNum =EndYear-StartYear+1                                 ;¼´ÄêÊý
    AllData=STRARR(1)+'%'                                ;³õÊ¼»¯ÁÐÖµ

	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='ÕýÔÚÊý¾Ý´¦ÀíÖÐ,ÇëÉÔºò...',TITLE='É¸Ñ¡Òò×Ó')
	progressTimer->START                         ;Æô¶¯½ø¶ÈÌõ

       FOR i=StartYear,EndYear DO BEGIN
	      progressTimer->UPDATE, (FLOAT(i)/RowsNum*100.0)  ;Æô¶¯½ø¶ÈÌõ

			Sqlstr ='select '+CropFiled+' from '+TableRS+' where '+WhereCol+'='+Code+' and '+ $
			 		'year='+STRTRIM(i,2)+' and month between '+STRTRIM(StartMonth,2)+' and ' $
			 		+ STRTRIM(EndMonth,2)+" and data_type='"+DataType+"' and Sensor_code='" $
			 		+ Sensor+"'"
			Temp = DC_GetdataFromDB_Str(1,Sqlstr)

			IF IsAvgOrSum EQ 0 THEN BEGIN   								;ÇóÆ½¾ù
				AllData = [AllData,STRTRIM(MEAN(FLOAT(Temp)),2)]
			ENDIF ELSE AllData = [AllData,STRTRIM(TOTAL(FLOAT(Temp)),2)]	;ÇóÀÛ¼ÆÖµ

       ENDFOR

;;	    progressTimer->DESTROY ;Ïú»Ù½ø¶ÈÌõ
 		OBJ_DESTROY,progressTimer
	    AllData=AllData[1:*,0:*]       ;µÃµ½ËùÓÐµ¥Ñ®µÄÒò×ÓÊý¾Ý

	return,AllData
END

;----------º¯ÊýLatlon_to_Albers_()½«¾­Î³¶È×ø±ê×ªÎªAlber×ø±ê-------------------------------
FUNCTION DC_Latlon_to_Albers ,Lon_Lat $                         ;¼´¾­¶ÈºÍÎ³¶ÈÊý¾Ý¶Ô.
						  ,CenterMidian105 = CenterMidian105  ;×ªÎªAlbers105,Ä¬ÈÏÎª110.
   ;;µ÷ÓÃÐÎÊ½Îª: Result = DC_Latlon_to_Albers( Lon_Lat,[/CenterMidian105])  ·µ»ØÎªÖµÎª(X,Y)µÑ¿¨¶û×ø±ê
   ;ÈôÊ¹ÓÃÁË¹Ø¼ü×Ö,Ôò½«¾­Î³¶È·µ»ØÎªAlbers105µÄ×ø±êÖµ.
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
	; ¹¹ÔìÍ¶Ó°ÐÅÏ¢The following are GCTP projections:
	; setting Projection = 103 means Albers Equal Area projection with following parameters: SEMIMAJOR_AXIS, SEMIMINOR_AXIS, STANDARD_PAR1, STANDARD_PAR2, CENTER_LONGITUDE, CENTER_LATITUDE, FALSE_EASTING, FALSE_NORTHING
	; node:FALSE_EASTING = 0.0 means FALSE_EASTING is 4 000 000.00
	; DATUM = 15 means datum is Krassovsky 6378245.0 6356863.0188
	projection = map_proj_init(103 , DATUM = 15, /GCTP, SEMIMAJOR_AXIS = 6378245.0, SEMIMINOR_AXIS = 6356863.0188, STANDARD_PAR1 = StdLat1, STANDARD_PAR2 = StdLat2, $
	                           CENTER_LONGITUDE = MidLon, CENTER_LATITUDE = 0.0, FALSE_EASTING = False_easting, FALSE_NORTHING = 0.0)
	; To get latitude and longitude of the coordinates of the point
	; MAP_STRUCTURE is the projection information

	;·µ»ØÎªÖµÎª(X,Y)µÑ¿¨¶û×ø±ê
	X_Y = MAP_PROJ_FORWARD (Lon_Lat, MAP_STRUCTURE = projection)

    return, X_Y; [X/Y]Êý¾Ý¶Ô.
END

;----------º¯ÊýDC_Albers_to_Latlon()½«Alber×ø±ê×ªÎª¾­Î³¶È×ø±ê-------------------------------
FUNCTION DC_Albers_to_Latlon,X_Y $             ;µÑ¿¨¶û×ø±êÊý¾Ý¶Ô
                         ,CenterMidian105 = CenterMidian105
   ;;µ÷ÓÃÐÎÊ½Îª: Result = DC_Albers_to_Latlon( X, Y,/CenterMidian105)  ·µ»ØÎªÖµÎªlat, lon
   ;ÈôÊ¹ÓÃÁË¹Ø¼ü×Ö,ÔòÎª½«Albers105µÄ×ø±êÖµ·µ»Ø¾­Î³¶È.
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
	; ¹¹ÔìÍ¶Ó°ÐÅÏ¢The following are GCTP projections:
	; setting Projection = 103 means Albers Equal Area projection with following parameters: SEMIMAJOR_AXIS, SEMIMINOR_AXIS, STANDARD_PAR1, STANDARD_PAR2, CENTER_LONGITUDE, CENTER_LATITUDE, FALSE_EASTING, FALSE_NORTHING
	; node:FALSE_EASTING = 0.0 means FALSE_EASTING is 4 000 000.00
	; DATUM = 15 means datum is Krassovsky 6378245.0 6356863.0188
	projection = map_proj_init(103 , DATUM = 15, /GCTP, SEMIMAJOR_AXIS = 6378245.0, SEMIMINOR_AXIS = 6356863.0188, STANDARD_PAR1 = StdLat1, STANDARD_PAR2 = StdLat2, $
	                           CENTER_LONGITUDE = MidLon, CENTER_LATITUDE = 0.0, FALSE_EASTING = False_easting, FALSE_NORTHING = 0.0)
	; To get latitude and longitude of the coordinates of the point
	; MAP_STRUCTURE is the projection information

	;·µ»ØÖµÎª2ÁÐµÄÊý×é,ÒÀ´ÎÎªlongitude/latitude coordinates
	Lon_lat = MAP_PROJ_INVERSE(X_Y, MAP_STRUCTURE = projection)

    return, Lon_lat       ; [¾­¶È/Î³¶È]
END
;----------------------------------------------------------------------------
;_____________________________Ö÷º¯Êý__________________________________________________
;-------------ÒÀ¾ÝÖ¸¶¨µÄ×óÉÏ½Ç×ø±êºÍÐÐÁÐÊýÉú³É²åÖµÍ¼----------------------------------
 FUNCTION DC_CREATE_INTERP_GRID,Lon_lat,value,ulx_,uly_,cellsize_,samples_,lines_,MeteoDataType,CenMedian105 = CenMedian105
   ;µ÷ÓÃÐÎÊ½: Result = DC_CREATE_INTERP_GRID(Lon_Lat,value,ulx_,uly_,cellsize_,samples_,lines_,MeteoDataType,[/CenMedian105])
   ;·µ»ØÖµÎª:Ö¸¶¨µÄÐÐÁÐÊýµÄ²åÖµ¶þÎ¬Êý¾Ý,×ø±êÖµÎªAlbers110Í¶Ó°×ø±ê
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
		coor = DC_Latlon_to_Albers(Lon_lat,/CenterMidian105)  ;ÓÃµ½Albers105Í¶Ó°.
	ENDIF ELSE BEGIN
		coor = DC_Latlon_to_Albers(Lon_lat) 				   ;ÓÃµ½Albers110Í¶Ó°.
	ENDELSE

	xbox=[[ulx],[ulx],[lrx],[lrx]]
	ybox=[[uly],[lry],[uly],[lry]]

	Result=MIN((coor[0,*]-ulx)^2+(coor[1,*]-uly)^2,index)   ;µ½×óÉÏ½Ç×îÐ¡¾àÀë
	v1=value[index]
	Result=MIN((coor[0,*]-ulx)^2+(coor[1,*]-lry)^2,index)	;µ½×óÏÂ½Ç×îÐ¡¾àÀë
	v2=value[index]
	Result=MIN((coor[0,*]-lrx)^2+(coor[1,*]-uly)^2,index)	;µ½ÓÒÉÏ½Ç×îÐ¡¾àÀë
	v3=value[index]
	Result=MIN((coor[0,*]-lrx)^2+(coor[1,*]-lry)^2,index)	;µ½ÓÒÏÂ½Ç×îÐ¡¾àÀë
	v4=value[index]

	vbox=[[v1],[v2],[v3],[v4]]

	;È¥ÖØ¸´µÄµã      ËÄ¸ö½ÇµÄÖµÒÔ×î½üÀ´Ìæ´ú.¥
	GRID_INPUT,[[coor[0,*]],[xbox]],[[coor[1,*]],[ybox]],[[Value],[vbox]],xSorted,ySorted,dataSorted

	TRIANGULATE,xSorted,ySorted,triangle
	;--------------------------------------------------------
;	;FUNCTION INTERP_METHOD,dataSorted,xSorted,ySorted,Samples,lines,triangle,MeteoDataType
;
;	;¸ù¾Ý²»Í¬µÄÒªËØ(ÓÉ"MeteoDataType"¾ö¶¨)Ñ¡Ôñ²»Í¬µÄ²åÖµ·½Ê½

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
		'SUNT':BEGIN;ÈÕÕÕÊ±Êý
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
		'WINV':BEGIN    	;·çËÙ
			powerhvalue=2
			Smoothvalue=1.0
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
						 ,POWER=powerhvalue,SMOOTHING=Smoothvalue $
						 ,DIMENSION=DIMENSION,METHOD='InverseDistance' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )
		END
		'KING':BEGIN    	;ÓÃÓÚ²¨¶¯µ¥²ú¿Õ¼ä²åÖµ
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
;						 ,VARIOGRAM = [1,8,1,1]	$			;[ Type, Range, Nugget, Scale]
						 ,DIMENSION=DIMENSION,METHOD='Kriging' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )

		END
	    ELSE:
	ENDCASE

;   Í¼Ïñµ¹×ª,ÒÔµÃµ½ÕýÈ·µÄÍ¼Ïñ.
;	gridnew=MAKE_ARRAY(samples,lines,TYPE=SIZE(grid,/TYPE))
;	FOR j=0,lines-1 DO gridnew[*,lines-1-j]=grid[*,j]

    grid = REVERSE(TEMPORARY(grid), 2)
	RETURN,grid
END
;******************************************************************************
;******¸Ã¹ý³ÌÉú³É±ê×¼µÄENVIÎÄ¼þ:Ó°ÏñÊý¾Ý + ÏàÓ¦µÄÍ·ÎÄ¼þ.
 PRO DC_SaveImageFile,SAVE_FILE       $ ;´ý±£´æµÄÎÄ¼þÃû.
 				     ,ImageData       $ ;Òª±£´æµÄÊý¾Ý,×¢ÒâImageDataÊÇÖ¸ÕëÀàÐÍ.
 				     ,samples,lines,DataType $  ;DataTypeÖ¸Êý¾ÝÊÇfloat,Doble,char,int
 				     ,sensortype $              ;long,byteµÈÀàÐÍID,¿´size()º¯Êý
 				     ,ULX,ULY,Resolution ,CenterMedian
;µ÷ÓÃ:DC_SaveImageFile,SAVE_FILE,ImageData,samples,lines,DataType,sensortype,ULX,ULY,Resolution ,CenterMedian

	IF (N_PARAMS() NE 10) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

        enter=string(byte(13))+string(byte(10))     ;»Ø³µ·ûASCIIÂëÖµ
        time=systime()                              ;Éú³ÉÊ±¼ä
        col =STRTRIM(samples,2)                     ;Éú³ÉÓ°ÏñÁÐÊý
        line=STRTRIM(lines,2)                       ;Éú³ÉÓ°ÏñÐÐÊý
        DataType_   = STRTRIM(DataType,2)           ;Êý¾ÝÀàÐÍ
        sensortype_ = STRTRIM(sensortype,2)         ;´«¸ÐÆ÷ÀàÐÍ
        tulx  = STRTRIM(ULX,2)                      ;×óÉÏ½ÇX×ø±ê(ALBERS110)
        tuly  = STRTRIM(ULY,2)                      ;×óÉÏ½ÇY×ø±ê(ALBERS110)
		Resolution_   = STRTRIM(Resolution,2)		;ÏñËØ´óÐ¡.Ò²¼´·Ö±æÂÊ.
		CenterMedian_ = STRTRIM(CenterMedian,2)		;ÊÇ110,»¹ÊÇ105
		IF CenterMedian_ EQ '110' THEN BEGIN
		   EastingFalse = '4000000'					;¶«Æ«¶àÉÙ¹«Àï.
		ENDIF ELSE EastingFalse = '0.0'

        SaveFileName = SAVE_FILE
        HeadFileName = SAVE_FILE+'.hdr'

        IF STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH) NE -1 THEN BEGIN
        	SaveFileName = STRMID(SAVE_FILE,0,STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH))
        	HeadFileName = SAVE_FILE
        ENDIF

        Bandname = STRMID(SaveFileName,STRPOS(SaveFileName,'\',/REVERSE_SEARCH)+1)  ;²»°üº¬Â·¾¶µÄÎÄ¼þÃû.

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

         openw,lun,HeadFileName,/get_lun     ;×¢Òâ:µ±ÎÄ¼þÒÑ´æÊ±,Õâ¸öÐ´¹ý³Ì²»»á×÷ÈÎºÎÌáÊ¾,½«ÒÑÓÐÎÄ¼þÌæ»»µô.ÏÂÍ¬.,
         writeu,lun,HeadInfomation
         free_lun,lun

         openw,lun,SaveFileName,/get_lun
         writeu,lun,*ImageData				;	ImageDataÊÇÖ¸ÕëÀàÐÍ
         free_lun,lun

END

;------¸Ãº¯ÊýÖ÷ÒªÊÇÎª¶ÁÈ¡ÇøÓò»òÓ°ÏñµÄÍ¶Ó°×ø±ê²ÎÊý--------------------
FUNCTION DC_ReadParameter,district      ;±äÁ¿districtÎªÇøÓò(×Ö·ûÐÍ)
	;Re=DC_ReadParameter(district)
	;·µ»ØÖµ:(1)·µ»ØÎªÇøÓòµÄÏàÓ¦²ÎÊý.
	;		(2)Èç¹ûÃ»ÓÐÏàÓ¦µÄ²ÎÊý,Ôò·µ»ØÖµÎª¿ÕÖµ
	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	ParaFile = 'text\parametersetting.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('ÏµÍ³°²×°Ä¿Â¼ÏÂÕÒ²»µ½²ÎÊýÉèÖÃÎÄ¼þ"parametersetting.txt"!',TITLE='¾¯¸æ')
		RETURN,''
	ENDIF

    OPENR,lun,ParaFile,/GET_LUN
    FileInfo = FSTAT(lun)
    paradata = BYTARR(FileInfo.SIZE)
    READU,lun,paradata
    FREE_LUN,lun

    index = STRPOS(paradata, district)           ;²éÕÒËùÑ¡ÇøÓòÏà¹Ø²ÎÊýÊÇ·ñÓÐ.
    IF index EQ -1 THEN RETURN,''

    enter=string(byte(13))+string(byte(10))   					  ;»Ø³µ·ûASCIIÂëÖµ

    index0 = STRPOS(paradata, 'samples = ',index)
    index1 = STRPOS(paradata, 'lines   = ',index)
    index2 = STRPOS(paradata, 'resolution = ',index)
    index7 = STRPOS(paradata,'central_meridian = ',index)      ;ÕâÀïÊÇ"index7",ºóÀ´¼ÓµÄ,Ã»ÓÐ´í.
    index3 = STRPOS(paradata, 'albers_ul = {',index)
    index4 = STRPOS(paradata, '}',index)
    index5 = STRPOS(paradata, 'stationtable = ',index)
    index6 = STRPOS(paradata, enter,index5)   					 ;×¢Òâ´Ë´¦ÊÇindex5.

	ULinfo = STRMID(paradata,index3+13,index4-index3-13)
	UlX_Y_postion = STRSPLIT(ULinfo,',',ESCAPE=' ',/EXTRACT )
	UlX = UlX_Y_postion[0]                       			;×óÉÏ½ÇX×ø±ê
	UlY = UlX_Y_postion[1]									;×óÉÏ½ÇY×ø±ê

	CenMeridian = STRMID(paradata,index7+19,index3-index7-19-2)  ;ÖÐÑë×ÓÎçÒÔ±êÊ¶ÊÇ105,»¹ÊÇ110
	samples     = STRMID(paradata,index0+10,index1-index0-10-2)  ;¼õÈ¥2,ÊÇÒòÎª»Ø³µ·ûÕ¼2¸ö×Ö½Ú³¤¶È.
    lines       = STRMID(paradata,index1+10,index2-index1-10-2)
    resolution  = STRMID(paradata,index2+13,index7-index2-13-2)

    IF index6 NE -1 THEN  BEGIN            					;ÒÔ·À½áÊøÊ±Ã»ÓÐ»Ø³µ·û.
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

;**************¶ÁENVI±ê×¼ÎÄ¼þµÄÍ·ÎÄ¼þ*********************************************
FUNCTION DC_ReadHead_file,inputfile_ $              ;ÊäÈëÒª¶ÁÍ·µÄÎÄ¼þ
						 ,PROMPT_DES = PROMPT_DES   ;ÓÃ»§Ö¸¶¨µÄÓÃÓÚÌáÊ¾µÄÌáÊ¾Óï,×Ö·ûÐÍ
   ;µ÷ÓÃ:Re = DC_ReadHead_file(inputfile_,[PROMPT_DES = Describe])
   ;×¢Òâ¸Ã³ÌÐòÖ»ÏÞÓÃÓÚÎÒÃÇÐ´µÄ±ê×¼ENVIÍ·ÎÄ¼þµÄ¶ÁÈ¡.·µ»ØÖµÈçÏÂ:
   ;(1) ¶ÁÈ¡³É¹¦,·µ»ØÎªÏàÓ¦ÎÄ¼þµÄÍ·ÎÄ¼þÐÅÏ¢.
   ;(2) ¶ÁÈ¡Ê§°Ü,·µ»ØÎª¿Õ

	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    inputfile = inputfile_
    IF NOT KEYWORD_SET(PROMPT_DES) THEN PROMPT_DES=''

    IF ~FILE_TEST(inputfile) THEN BEGIN
       Info = DIALOG_MESSAGE('ÕÒ²»µ½'+PROMPT_DES+'ÏàÓ¦µÄÍ·ÎÄ¼þ',TITLE='¾¯¸æ')
       RETURN,''
    ENDIF

    IF STRPOS(inputfile, '.',/REVERSE_SEARCH) EQ -1 THEN BEGIN
       inputfile = STRTRIM(inputfile,2)+'.hdr'
    ENDIF

    OPENR,lun,inputfile,/GET_LUN
    result = FSTAT(lun)

	 if result.size eq 0 then begin
	 	Info = DIALOG_MESSAGE(inputfile+'Í·ÎÄ¼þ²»ÕýÈ·!',TITLE='¾¯¸æ')
	 	return,{error	:	-1}
	 endif
    headdata = BYTARR(result.SIZE)
    READU,lun,headdata
    FREE_LUN,lun

 ;   »ñÈ¡ÐÐÁÐºÅ£¬²¨¶ÎÊý£¬Êý¾Ý¶ÁÈ¡·½Ê½,Êý¾ÝÀàÐÍ.×óÉÏ½ÇXY×ø±ê.

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
    index10 = STRPOS(headdata, '}',index9)                  ;×¢ÒâÕâÀï¼ÓÁË¶¨Î»Î»ÖÃ.

	 if (where([index0,index1,index2,index3,index4,index5,index6,index7,index8,index9,index10] eq -1))[0] ne -1 then begin
	 	Info = DIALOG_MESSAGE(inputfile+'Í·ÎÄ¼þ²»ÕýÈ·!',TITLE='¾¯¸æ')
	 	return,{error	:	-1}
	 endif

	samples    = STRMID(headdata,index0+10,index1-index0-10-2) ;Ö®ËùÒÔ¼õ2ÊÇÒò»Ø³µ·ûÕ¼2¸ö×Ö½Ú³¤.
    lines      = STRMID(headdata,index1+10,index2-index1-10-2)
    bands      = STRMID(headdata,index2+10,3)                  ;ÕâÀï½ØÈ¡³¤¶ÈÊÇ3,¶ø²»ÊÇ1,¿´»Ø³µ·ûµÄ×Ö½Ú³¤¶È.
    datatype   = STRMID(headdata,index3+12,3)                  ;ÕâÀï½ØÈ¡³¤¶ÈÊÇ3,¶ø²»ÊÇ1,¿´»Ø³µ·ûµÄ×Ö½Ú³¤¶È.
    interleave = STRMID(headdata,index4+13,index5-index4-13-2)
    sensortype = STRMID(headdata,index5+14,index6-index5-14-2)

	Mapinfo = STRMID(headdata,index7+11,index8-index7-11)
	Mapinfomation = STRSPLIT(Mapinfo,',',ESCAPE=' ',/EXTRACT )
	UlX = Mapinfomation[3]                       			;×óÉÏ½ÇX×ø±ê
	UlY = Mapinfomation[4]									;×óÉÏ½ÇY×ø±ê
	Resolution = Mapinfomation[5]	;×¢ÒâÕâÀïÓÐ´¹Ö±ºÍË®Æ½·½Ïò·Ö±æÂÊÖ®·Ö,Ò»°ãÊÇÏàÍ¬µÄ.Mapinfomation[5]=Mapinfomation[6]

	Pro_info = STRMID(headdata,index9+18,index10-index9-18)
	Projection = STRSPLIT(Pro_info,',',ESCAPE=' ',/EXTRACT )
	CenterMedian = STRTRIM(FIX(Projection[4]),2)	      ;ÖÐÑë×ÓÎçÏß,ÒÔÈ·¶¨ÊÇ110,»¹ÊÇ105.

;    print,Mapinfo
;    print,UlX_Y_postion
;    print,UlX,ULY
;    print,PixelInfo
;    print,PixelSize
;¡¤	1 - byte (8-bits)
;¡¤	2 - integer (16-bits)
;¡¤	3 - long integer (32-bits)
;¡¤	4 - floating-point (32-bits)
;¡¤	5 - double-precision floating point (64-bits)
;¡¤	6 - complex (2x32-bits)
;¡¤	9 - double -precision complex (2x64-bits)
;¡¤	12 - unsigned integer (16-bits)
;¡¤	13 - unsigned long integer (32-bits)
;¡¤	14 - long 64-bit integer
;¡¤	15 - unsigned long 64-bit integer.

	CASE FIX(datatype) OF     ;ÒòÎª²»Í¬µÄÊý¾ÝÀàÐÍ,Õ¼¾ÝµÄ"×Ö½ÚÊý»òÕß¶þ½øÖÆÎ»Êý"ÊÇ²»¾¡ÏàÍ¬µÄ.
		1: ByteNum = 1        ;1×Ö½Ú(byte) = 8¶þ½øÖÆÎ»(bit).¼´ 1 byte = 8 bit
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

  ;ÇëÇ§Íò²»Òª¸Ä¶¯½á¹¹ÌåÖÐÓòµÄË³Ðò,ÒòÎªÔÚÆäËû³ÌÐòÖÐÒýÓÃÊ±,ÓÐ°´ÓòË÷ÒýºÅÀ´ÒýÓÃµÄ.
  ;,Èçfileinfo.(3)±íÒýÓÃ²¨¶ÎÊý,Èô¸Ä¶¯,»á³öÏÖ´íÎó.ÒªÐÞ¸Ä³ÌÐòÌí¼ÓÓò,Ö»ÄÜÔÚºóÃæÌí¼Ó.
  ;;µ±È»ÊÇ°´ÓòÃûÀ´ÒýÓÃ,ÔòÃ»ÓÐÈÎºÎÓ°Ïì

   RETURN,fileinfo={samples     :  samples   	,$
                    lines       :  lines       	,$
                    bands       :  STRTRIM(FIX(bands),2)        ,$   ;È¥µô»Ø³µ·û.ÏÂÍ¬.
                    datatype    :  STRTRIM(FIX(datatype),2) 	,$
                    interleave  :  interleave  	,$
                    sensortype  :  sensortype 	,$
                    ByteNum		:  ByteNum 		,$
                    UlX			:  UlX 			,$
                    UlY			:  UlY 			,$
                    Resolution	:  STRTRIM(LONG(FLOAT(Resolution)),2) 	,$   ;ÕâÑù×öÊÇÒòÎªÓÐÊ±FIX(1.0000000000e+003)½á¹ûÎª1,µ«float()²»»á.
                    CenterMedian:  CenterMedian}
END

;**************¶ÁENVI±ê×¼ÎÄ¼þµÄÊý¾Ý*********************************************
FUNCTION DC_Read_ENVIData,inputfile_ $  				;ÊäÈëµÄÎÄ¼þÃû(×Ö·ûÐÍ)
					  ,SUCCESSSTATUS = SuccessStatus $	;¶ÁÈ¡ÊÇ·ñ³É¹¦µÄ×´Ì¬.
					  ,DESCRIPTION  = Description		;ÓÃ»§ÊäÈëµÄÌáÊ¾Óï,×Ö·ûÐÍ
	;µ÷ÓÃÐÎÊ½:Resutl = DC_Read_ENVIData(inputfile_,[SUCCESSSTATUS = var],[DESCRIPTION=Decscription])
	;·µ»ØÖµÎª:(1)SuccessStatus=0 ,¶ÁÈ¡²»³É¹¦Îª0
	;		  (2)SuccessStatus=1 ,¶ÁÈ¡³É¹¦,·µ»ØÎªÊý¾Ý
    ;×¢Òâ¸Ã³ÌÐòÖ»ÏÞÓÃÓÚ¶Á±ê×¼ENVIÎÄ¼þ(Êý¾ÝÎÄ¼þ+Í·ÎÄ¼þ)
    ;,ÇÒÖ»ÏÞ¶ÁÈ¡µ¥²¨¶ÎÊý¾Ý(¸ÄÔìºóÒ²Ðí¿É¶Á¶à²¨¶ÎµÄ)
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
	       prompt = DIALOG_MESSAGE(Description+'Í·ÎÄ¼þ²»´æÔÚ!',TITLE='¾¯¸æ')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END

        ~FILE_TEST(DataFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Description+'Êý¾ÝÎÄ¼þ²»´æÔÚ!',TITLE='¾¯¸æ')
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

    ;ÒòÎª³ÌÐòÖÐÎÄ¼þ´óÐ¡¾ùÊÇÒÔ×Ö½Ú¼ÆËã³¤¶È,¶øÊý¾ÝÀàÐÍ²»Í¬,¼ÆËãÐÐÁÐÊýÊ±Ò²»áÓÐËù²»Í¬.
    IF ROUND((DataFileInfo.SIZE)/FIX(HeadFileInfo.ByteNum),/L64) NE $
       ULONG(HeadFileInfo.samples)*(HeadFileInfo.lines) THEN BEGIN
       prompt = DIALOG_MESSAGE(Description+'Êý¾ÝÎÄ¼þÓëÍ·ÎÄ¼þÐÅÏ¢ÐÐÁÐÊý²»Ò»ÖÂ!',TITLE='¾¯¸æ')
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
    	Info = DIALOG_MESSAGE(DataFile+'Êý¾ÝÎÄ¼þ²»ÕýÈ·!',TITLE='¾¯¸æ')
    	SuccessStatus = 0
	 	return,-1
	 endif
    READU,Lun,Data
    FREE_LUN,Lun


	IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 1

   RETURN,Data

END
;***********************»­²åÖµÍ¼**************************************
;=================ÔÚ"¶ÔÏóÍ¼ÐÎ´°¿Ú"ÖÐ´ò¿ªÓ°ÏñÍ¼ºÍÊ¸Á¿Í¼=============================
PRO DC_Draw_image,INputFile   $	;±»ÏÔÊ¾µÄÓ°ÏñÎÄ¼þÃû,×Ö·û´®
				  ,WID_DRAW   $	;Draw×é¼þ,Èç¹ûÆäÓÐÓÃ»§Öµdata,ÔòdataÎ¬´óÐ¡ÓëINputFileÎÄ¼þÓ°ÏñÎ¬Êý¾Ý±ØÐëÏàÍ¬
				  ,OView=View $ ;·µ»ØµÄÒªµÃµ½µÄÊÓÍ¼¶ÔÏó
				  ,MINVALUE=minvalue $  ÓÃÓÚÖ¸¶¨±»BYTSCL()½øÐÐÀ­ÉìµÄ×îÐ¡Öµ
				  ,WHITE=WHITE	 ;Ö¸¶¨±³¾°É«Îª°×É«.
; µ÷ÓÃ:DC_Draw_image,INputFile,WID_DRAW[,OView=View,MINVALUE=minvalue,/WHITE]
 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	forward_function DC_ReadHead_file

	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
	oWindow->ERASE,COLOR=255
	oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	ShapeFileName='data_vector\province.shp'
	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&×é¼þ

	;(1)------------¶ÁÓ°ÏñÍ¼-----------------------------------------
;	oColor = [128,128,128]
	IF (N_ELEMENTS(data) EQ 0) THEN data= DC_Read_ENVIData(INputFile) ;Èç¹ûWID_DRAWÃ»ÓÐÓÃ»§Öµ,ÔòÓÃÊäÈëÓ°Ïñ
	IF ~KEYWORD_SET(WHITE)      THEN oColor = [255,255,255] ELSE oColor = [0,0,0]
	IF ~KEYWORD_SET(MINVALUE)  THEN minvalue = MIN(data)

	oScene = Obj_New('IDLgrScene', Color = oColor)
	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;´´½¨ÊÓÍ¼
	Image_Layer   = Obj_New('IDLgrModel')	  ;Ó°ÏñÍ¼²ã
	Polygon_Layer = obj_new('IDLgrModel')     ;Ê¸Á¿Í¼²ã

	oWindow->SetProperty, Graphics_Tree = oScene
	oWindow->SetCurrentCursor, 'ARROW'    	   ;Êó±êÖ¸ÕëÎª¼ýÍ·,²»ÊÇÊ®×Ö²æ.

	oScene->Add, oView
	oView ->Add, Image_Layer
	oView ->Add, Polygon_Layer


	;½«ÊýÖµÀ­Éìµ½256É«ÏÔÊ¾·¶Î§ÄÚ

	dataDis = BYTSCL(data,MIN=minvalue)

	Palette = obj_new('IDLgrPalette')
	Palette->LoadCt,39
	Palette->SetRGB,0,255,255,255

	;TVLCT,0,0,0    ;Ê¹0ºÅÑÕÉ«Ë÷ÒýºÅÑÕÉ«Îª°×É«(ÕâÀïÆäÊµ¿ÉÒÔ¿´³ÉÊÇ±³¾°É«,Ä¬ÈÏÊÇºÚÉ«),

	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
			               ,dataDis)   ;ÕâÀïdataDisÊÇÀ­ÉìÊý¾Ý

	Image_Layer->Add,oImage

	;----½«Ê¸Á¿Êý¾Ý½øÐÐËõ·ÅµÄ²ÎÊý---------------------------------------
	Para = DC_ReadHead_file(INputFile)
	if n_tags(Para) eq 1 then 	return

	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;×óÉÏ½Ç
	samples = Double(Para.samples) & Lines = Double(Para.Lines)  ;
;;	Resolution = FIX(Para.Resolution)	ÓÃFIxÊ±,Èô±äÁ¿Îª"1.0000000000e+003",Ôò²»ÄÜ±»ÕýÈ·Ê¶±ð.±»¿´³É1
	Resolution = FLOAT(Para.Resolution)
	uvRange = [UlX,UlY-Lines*Resolution,UlX+samples*Resolution,UlY]
	uRange  = uvRange[2]-uvRange[0]
	vRange  = uvRange[3]-uvRange[1]

	Datasize = SIZE(data,/DIMENSIONS)
	xSize = Datasize[0]
	ySize = Datasize[1]
	drawSize = [r.scr_xsize,r.scr_ysize]

	;----¶ÁÊ¸Á¿Êý¾ÝÍ¼²ã----------------------------------------
	IF file_test(ShapeFileName) EQ 0  THEN return
	myshape=OBJ_NEW('IDLffShape', ShapeFileName)
	myshape -> IDLffShape::GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type

	IF (type EQ 5 OR type EQ 3) THEN BEGIN
	 FOR i=0,num_ent-1 DO BEGIN
	    ent = myshape -> IDLffShape::GetEntity(i)
	    NumPoints = ent.N_VERTICES-1
	    x = (*ent.vertices)[0,0:NumPoints]
	    y = (*ent.vertices)[1,0:NumPoints]
	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;ÕâÀï½«X,Y×ø±ê½øÐÐËõ·Å
		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange

	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[0,0,0],thick = 2,LINESTYLE = 0) ;[245,122,182]
	    Polygon_Layer->Add,oPolyline
	    myshape -> IDLffShape::DestroyEntity, ent
	 ENDFOR
	ENDIF
	OBJ_DESTROY, myshape

	;viewµÄ×ø±êÌåÏµ
	viewPlane = [0,0,xSize,ySize]
	;viewµÄ´óÐ¡
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

	;viewµÄ²åÈëÎ»ÖÃ
	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]

	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
	oWindow->Draw,oScene

	OBJ_DESTROY,Palette

	IF ARG_PRESENT(View) THEN View=oView
END

;$$$$$$$$$$$$$$$$$$$$$»­Öù×´Ìõ$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO DC_Draw_BAR_PLOT,drawID $			;DRAW×é¼þµÄIDÖµ,¼´GET_VALUE
					,AnalysisData $		;Òª»­µÄÊý¾Ý
					,XLabel	$			;XÖáµÄ±êÇ©é
					,LINE = Line $		;Ö»»­ÏßÐÔÍ¼,Ä¬ÈÏÎªÖ»»­Öù×´Í¼
					,CHARTLINE = ChartLine  ;Í¬Ê±»­Ïß×´ºÍÖù×´Í¼
;µ÷ÓÃ:DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel[,/Line,/ChartLine]
;±¾³ÌÐòÖ»ÊÊÓÃÓÚ"²úÁ¿ÈÚºÏÄ£¿é"µÄÊ¹ÓÃ,¶ÔÓÚÆäËû³ÌÐò,¿ÉÄÜ²»ÊÊÓÃ.
 	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	  PlotNum = N_ELEMENTS(XLabel)    ;ÀàÐÍµÄ¸öÊý

	  DEVICE,GET_DECOMPOSED=old_color     ;»ñÈ¡µ±Ç°DECOMPOSEDÖµ
;      DEVICE,GET_CURRENT_FONT=oldFont
;      DEVICE,SET_FONT='ËÎÌå',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;ÓÃIDLÌá¹©ºó±¸´æ´¢,Ê¹ÓÃÑÕÉ«²éÑ¯±í(Í£ÓÃÑÕÉ«·Ö½â¹¦ÄÜ),
		r=[0,255,  0,  0,255,255]   	  ;ÒÀ´ÎÎªºÚ\ºì\ÂÌ\À¶\»Æ\°×
		g=[0,  0,255,  0,255,255]
		b=[0,  0,  0,255,  0,220]
 		TVLCT, r, g, b   ;È±Ê¡µÚËÄ¸öÊ¡Êý,ÔòÊ¹ÑÕÉ«±íÖÐË÷ÒýºÅÎª0,1,2,3,4,5µÄÑÕÉ«ÎªÏàÓ¦µÄRGB×éºÏ


    OldWin = !D.WINDOW     			   ;±£´æÏµÍ³µÄ´°¿Ú
    OldBackup = !P.BACKGROUND
	Old_p     = !P.POSITION
    Old_Font  = !P.FONT
    OldFontSiz = !P.CHARSIZE
    OClor = !P.COLOR
    OldYticks = !Y.TICKS
    !P.FONT = 0
	!P.BACKGROUND = 255
	!P.COLOR = 800   ;ÖáµÄÑÕÉ«
    !P.CHARSIZE = 0.8

    Colors = INTARR(PlotNum)
	FOR I = 0, PlotNum-1 DO Colors[I]= I ;(2*I)+100

   	WSET, drawID
	PlotData = FLOAT(AnalysisData)
	Psym = 3   ;(µã±êÊ¶)
	IF KEYWORD_SET(ChartLine) THEN BEGIN   ;»­Öù×´ºÍÏß×´Í¼
	    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+4)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý
;		PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
;	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
;			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý

		OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;ÓÃ°×ÑÕÉ«½«PLOT»­µÄÏß¸²¸Çµô,»¹Òª×¢Òâ¸øXRANGE¸ºÖµ,²»È»»­Í¼Ð§¹û²î

	    FirstOFFSET = 0.3 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
	    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
			,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
			,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-6
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(Line) THEN BEGIN  ;Ö»»­Ïß×´Í¼
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
		         ,POSITION=[0.09,0.28,0.96,0.93] ,XTICKNAME=[' ',' ',XLabel,' ',' ',' '] $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)] $
				 ,XTICKLEN=1 ; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý

			OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-5
		ENDIF ELSE BEGIN				;Ö»»­Öù×´Í¼
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+3)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý
;			PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
;		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý


			OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;ÓÃ°×ÑÕÉ«½«PLOT»­µÄÏß¸²¸Çµô,»¹Òª×¢Òâ¸øXRANGE¸ºÖµ,²»È»»­Í¼Ð§¹û²î
		    FirstOFFSET = 0.8 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
		    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $
				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
				,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		ENDELSE
	ENDELSE

	!P.BACKGROUND = OldBackup		;»¹Ô­
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor
	!Y.TICKS = OldYticks
;	DEVICE,SET_FONT=oldFont
	DEVICE,DECOMPOSED=old_color   ;·µ»ØÔ­À´µÄDECOMPOSEDÖµ,ÒòÎª×Ô¶¨Òåº¯ÊýMyColor¸Ä±äÁË,Ðë»¹Ô­.

	WSET, OldWin				;»¹Ô­Ô­À´´°¿Ú.

END

;$$$$$$$$½«¶þÎ¬Êý×é±£´æÎª*.txt¸ñÊ½ÎÄ¼þ$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO DC_SaveTextData,Save_Data 			$		;Òª±£´æµÄ¶þÎ¬Êý×é
				   ,Parent				$		;WidgetID,×÷ÎªÎÄ¼þÑ¡Ôñ¶Ô»°´°¿ÚµÄ¸¸×é¼þ.
				   ,FILENAME = FILENAME $ 		;Ö¸¶¨Òª±£´æµÄÎÄ¼þÃû
				   ,NOSavePath = NOSavePath		;²»±£´æÂ·¾¶,Ä¬ÈÏ±£´æ
;µ÷ÓÃ:DC_SaveTextData,Save_Data,Parent,[FILENAME=Filename,/NOSavePath]

;×¢Òâ:Èç¹ûSave_DataÊÇÒ»ÐÐÊý¾Ý,Ôò¸Ã³ÌÐò¿ÉÄÜ³öÏÖÎÊÌâ,ÔÚÓÚSIZE(/DIMENSIONS)µÄÔ­Òò

 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	IF NOT KEYWORD_SET(FILENAME) THEN FILENAME=''

	TEMP = WHERE(Save_Data EQ '',COUNT)
	IF COUNT NE 0 THEN Save_Data[TEMP]='---'

	Filename=DIALOG_PICKFILE(TITLE='Áí´æÎª£º',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
	  ,/OVERWRITE,FILE = FILENAME,/WRITE,PATH=DC_PathSetting() $
	  ,GET_PATH=SavePath,DIALOG_PARENT=Parent)

	IF Filename EQ '' THEN RETURN

	DataSize = SIZE(Save_Data,/DIMENSIONS)
	Samples = DataSize[0]
	OPENW,LUN,Filename,/GET_LUN ,WIDTH=Samples*(MAX(STRLEN(STRTRIM(Save_Data,2)))+1)
	PRINTF,LUN,Save_Data;,FORMAT='(5(A20,2X))'
	FREE_LUN,LUN

	INFO = DIALOG_MESSAGE('±£´æ³É¹¦!',/INFORMATION,TITLE='ÌáÊ¾')

	IF KEYWORD_SET(NOSavePath) THEN RETURN

	path = DC_PathSetting(WRITEPATH1= SavePath)

END
;------------¼ÓÈ¨µ½Ê¡-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,ÕâÀïÓÃCROP_AREA_COUNTY
;×¢ÒâÏÂÃæÈ¡×÷ÎïÃæ»ýÊ±,Èç¹ûµ±Ç°Äê·ÝµÄÃæ»ýÊý¾ÝÃ»ÓÐ,ÔòÓÃ¿âÖÐ×î½üÄê·ÝNewestyearµÄÃæ»ýÊý¾Ý
FUNCTION DC_WeightToPro,CountyYield  	$	;2ÁÐÖµ,ÏØÂë\¹ÀËã²úÁ¿,
					   ,Crop_id 		$	;ËùÓÐ±äÁ¿¾ùÊÇ×Ö·ûÐÍ
					   ,CalYear 		$
					   ,ProID 			$        ;2Î»
					   ,model_type_id  		;Ê¡¹ÀËã²úÁ¿½á¹û±íÖÐÀàÐÍID,

;µ÷ÓÃÐÎÊ½: DC_WeightToPro,CountyYield,Crop_id,CalYear,ProID,model_type_id
;·µ»ØÖµ:1 ¼ÆËã³É¹¦
;		0 ¼ÆËã²»³É¹¦

 	IF (N_PARAMS() NE 5) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	COMMON COMMON_BLOCK

	CropIDList = ['11','12','21','22','23','31','32','41']    ;CropIDList±ØÐëÓëCropFieldList¶ÔÓ¦
    CropFieldList = ['SPRING_WHEAT','WINTER_WHEAT','EARLY_RICE','SEMILATE_RICE','LATE_RICE','SPRING_CORN','SUMMER_CORN','SOYBEAN']

    C_Index = WHERE(CropIDList EQ crop_id[0],count)
    IF count EQ 0 THEN BEGIN
       INFO = DIALOG_MESSAGE('Êý¾Ý¿âÖÐÃ»ÓÐ×÷Îï±àºÅ"'+Crop_id[0]+'",ÇëÈ·ÈÏ!',TITLE='¾¯¸æ')
       RETURN,0
    ENDIF

    CropField = CropFieldList[C_Index]
    Sqlstr ='select distinct Year as NewestYear from CROP_AREA_COUNTY where YEAR='+CalYear
    Currentyear = DC_GetdataFromDB_Str(1,SQLstr,N_RECORDS = Num)
    IF Num EQ 0 THEN BEGIN
       INFO = DIALOG_MESSAGE(['Êý¾Ý¿âÖÐCROP_AREA_COUNTY±íÖÐÃ»ÓÐµ±Ç°Äê·ÝÏàÓ¦' $
       						 ,'ÏØ×÷ÎïµÄÃæ»ýÊý¾Ý,Ê¹ÓÃ×î½üÄê·ÝµÄÃæ»ýÊý¾ÝÂð?!'],/QUESTION,TITLE='Ñ¯ÎÊ')
       IF INFO EQ 'No' THEN RETURN,0
       Sqlstr='select max(year) as NewestYear from CROP_AREA_COUNTY' ;ÕâÒ»²½ÊÇÎª×î½üÄê·Ý
    ENDIF

    Sqlstr='select county_code,'+CropField+' from CROP_AREA_COUNTY a,('+Sqlstr $
    		+') b where a.year=b.NewestYear and '+CropField+'>0 ' $
    		+'and LEFT(a.county_code,2)='	+ProID
	CountyCropArea = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = Num)   ;2ÁÐÖµ,ÏØÂë\×÷ÎïÃæ»ý
	IF Num EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('Êý¾Ý¿âÖÐCROP_AREA_COUNTY±íÖÐÃ»ÓÐÏàÓ¦ÏØ×÷ÎïµÄÃæ»ýÊý¾Ý!',TITLE='¾¯¸æ')
		RETURN,0
	ENDIF

    CountyNum=N_ELEMENTS(CountyYield)/2

      CountyYieldArea=STRARR(3,1) & Count = 0
      FOR i=0,CountyNum-1 DO BEGIN
          Temp=STRARR(3,1)
           FOR j=0,Num-1 DO BEGIN
               IF CountyYield[0,i] EQ CountyCropArea[0,j] THEN BEGIN
                  Temp[0,0]= CountyYield[0,i]    	;ÏØÂë
                  Temp[1,0]= CountyYield[1,i]    	;ÏØ¹ÀËã²úÁ¿
                  Temp[2,0]= CountyCropArea[1,j]    ;ÏØ×÷ÎïÃæ»ý
                  Count += 1
          		  CountyYieldArea=[[CountyYieldArea],[Temp]]
               	  BREAK
               ENDIF
           ENDFOR
      ENDFOR
      IF Count EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('Êý¾Ý¿âÖÐCROP_AREA_COUNTY±íÖÐÃ»ÓÐÏàÓ¦ÏØ×÷ÎïµÄÃæ»ýÊý¾Ý!',TITLE='¾¯¸æ')
		RETURN,0
      ENDIF
      CountyYieldArea = CountyYieldArea[*,1:*]
      Area = FLOAT(CountyYieldArea[2,*])
      ;µÃµ½¼ÓÈ¨µÄÊ¡×÷Îïµ¥²úÁ¿,³ËÒÔ15,Ôòµ¥Î»Îª¹«½ï/¹«Çê
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
PRO DC_A_Pro_Fun  ;ÎÄ¼þÍ·¿ÉÒª¿É²»Òª
END