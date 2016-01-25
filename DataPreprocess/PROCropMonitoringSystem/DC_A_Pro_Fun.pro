
;˵��:DC_A_Pro_Fun.pro
;����:���ļ���ŵ���Ԥ��������������õĹ��ú��������
;ʱ��:2006.8.18
;����:���¸ղ�ʿ(2004��)
;ע��:(1)������Щ���̻���֮ǰ,Ӧ��֤�ļ��ȱ�����.
;	  (2)����,��ò�Ҫ�ٸĶ�����������̵�˳��,��Ϊ����֮��Ҳ�б����ù�ϵ.

;******�Զ������:��һ����������ʱ,�ͷŶѱ���
PRO DC_CleanAllHeap,TLB
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2

    WIDGET_CONTROL,TLB,GET_UVALUE=PA
    HEAP_FREE,PA   ;
;  	HEAP_GC,/VERBOSE
END
;*******�Զ��庯��:�ж��ַ������Ƿ���ȷ*************
FUNCTION DC_JudgeInputChar,Inputchar $   	  ;�����Ҫ�������ַ���
 						  ,Desc=Desc $	      ;�û�����Ĵ�����ʾ�����ַ�ǰ׺.
 						  ,INTEGER=integer $  ;�����ж������Ƿ�ֻ������.Ĭ���ж�����Ϊ������
						  ,NEGATIVE=negative  ;�����������Ϊ��,Ĭ������ֻ��Ϊ��
      ;������ʽΪ: Result = DC_JudgeInputChar(Inputchar,[Desc=Description],[/INTEGER],[/NEGATIVE])
      ;����ֵ:(1)�����ͻ�����ֵ
      ;		  (2)ֵ-1 (�����˷Ƿ��ַ�)

       IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	   ON_ERROR,2

       IF NOT KEYWORD_SET(Desc) THEN Desc = ''

       CompareStr=['0','1','2','3','4','5','6','7','8','9','.']     ;������ַ�ֻ����11������.
	   IF KEYWORD_SET(NEGATIVE) THEN BEGIN
       		CompareStr=['-','0','1','2','3','4','5','6','7','8','9','.']     ;������ַ�ֻ����12������.
	   ENDIF

        IF STRLEN(Inputchar) EQ 0 THEN BEGIN                        ;�ж�û����������
           Prompt=DIALOG_MESSAGE(Desc+'û������ֵ,��������!',TITLE='����')
           RETURN,-1
        ENDIF

  		N=0
  		FOR i=0,STRLEN(Inputchar)-1 DO BEGIN                          ;�жϷǷ��ַ������
           JudgeChar=STRMID(Inputchar,i,1)
           aa = WHERE(CompareStr EQ JudgeChar,Count)
           IF Count EQ 0 THEN BEGIN
              Prompt=DIALOG_MESSAGE(Desc+'�����˷������ַ�,����������!',TITLE='����')
              RETURN,-1
           ENDIF
           IF JudgeChar EQ '.' THEN N=N+1           ;N���ڼ�¼С����ĸ���
        ENDFOR

       IF KEYWORD_SET(INTEGER) THEN BEGIN
		  IF N NE 0 THEN BEGIN
              Prompt=DIALOG_MESSAGE(Desc+'Ӧ��������ֵ,����������!',TITLE='����')
              RETURN,-1
		  ENDIF ELSE BEGIN
              RETURN,LONG(Inputchar)
		  ENDELSE
       ENDIF

       IF N GT 1 THEN BEGIN
          Prompt=DIALOG_MESSAGE(Desc+'�����˶��С����,����������!',TITLE='����')
          RETURN,-1
       ENDIF

       IF (STRMID(Inputchar,0,1) EQ '.') THEN BEGIN
		  Inputchar = '0'+Inputchar
          RETURN,FLOAT(Inputchar)
       ENDIF

       RETURN,FLOAT(Inputchar)
 END
;*****************�����ݿ��ж�ȡ����,����Ϊ�ַ�����********************************
FUNCTION DC_GetdataFromDB_Str,Columns $        	;Ҫ�õ�����������,������SQLstr���һ��
							 ,SQLstr	 $      	;SQL���.
					    	 ,Num_BUFFERS = Num_BUFFERS $  	;�����ݿ�����ȡ����ʱ�Ļ�������,Ϊ����ֵ
							 ,N_RECORDS = NumReocrd	  		;�õ���¼����

	 ;�ú���������ʽΪ:
	 ;result = DC_GetdataFromDB_Str(Columns,SQLstr,Num_BUFFERS = Num,N_RECORDS = var)
	 ;���������:������DC_GetDataFromDB()
	 ;(1) ���ز�ѯ��������,Ϊ�ַ�������.
	 ;(2) ����Ϊһ�п�ֵ(����Ϊ��ֵ)����ѯ���ɹ�ʱ.

;	  IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
;	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;Ĭ�ϻ�������Ϊ10

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
;;	   ColumnNum  = DataRecOBJ->NFields()   ;�˴���ColumnNum = Columns,����û���õ�,���ղ�ѯ�����
	       IF (DataRecOBJ->MoveCursor(/FIRST) EQ 1) THEN BEGIN
	           REPEAT BEGIN
	             RecordNum = RecordNum+1
	             Temp = STRARR(N,1)
	             DataValue = DataRecOBJ->GetRecord()            ;�õ���Ӧ�ػ���������Ӧ��ݵĲ���********

				;===�������޸ģ�20070903=============================
;	             FOR i=0,N-1 DO Temp[i,0] = DataValue.(i)	;ԭ����

				FOR i=0,N-1 DO begin
					if size(DataValue.(i),/type) eq 10 then begin	;����Ϊָ�룬��������Ϊ10�������ݿ��еı�ע
						Temp[i,0] = string(*(DataValue.(i)))
					endif else begin
						Temp[i,0] = DataValue.(i)
					endelse
				endfor
				;====================================================
	             DATA=[[DATA],[Temp]]				;�����д���.
	           ENDREP UNTIL(DataRecOBJ->MoveCursor(/NEXT) EQ 0)
			   DATA = DATA[*,1:*]     			 ;ȥ������,�õ�ȫ������
	       ENDIF

	   OBJ_DESTROY,DataRecOBJ

	   IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum

	   DATA = STRTRIM(TEMPORARY(DATA),2)
	   RETURN,DATA
END
;*****************�����ݿ��ж�ȡ����,����Ϊ�ṹ������********************************
FUNCTION DC_GetDataFromDB,Sql	 $    		      		;SQL���.�ַ���
					    ,Num_BUFFERS = Num_BUFFERS $  	;�����ݿ�����ȡ����ʱ�Ļ�������,Ϊ����ֵ
						,N_RECORDS = NumReocrd	  		;�õ���¼����
	 ;�ú���������ʽΪ:
	 ;result = GetDataFromDB(Sql,Num_BUFFERS = Num,N_RECORDS = var)
	 ;���������:
	 ;(1) ���ز�ѯ��������,Ϊ�ṹ������
	 ;(2) ���ؿ�ֵ����ѯ���ɹ�ʱ.

	  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
	  ON_ERROR, 2						;return to caller

      COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	  IF NOT KEYWORD_SET(Num_BUFFERS) THEN Num_BUFFERS = 10		;Ĭ�ϻ�������Ϊ10

	  RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBobj,SQL='select count(*) from ('+Sql+')')
	 ;�˴�û��"RecordNumOBJ->MoveCursor(/FIRST)",����Ϊ��һ������ֻ��һ����¼.����Ϊ�ṹ��
	  RecordNum = RecordNumOBJ->GetRecord()

	  IF RecordNum.(0) EQ 0 THEN BEGIN
	  	  OBJ_DESTROY,RecordNumOBJ
	      IF ARG_PRESENT(NumReocrd) THEN NumReocrd = RecordNum.(0)		;ֵΪ0,��RecordNum.(0)Ϊ0
	      RETURN,''
	  ENDIF ELSE OBJ_DESTROY,RecordNumOBJ

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
;--�Զ��庯��:���ڶ�ȡ�򱣴��û��Ĺ���·��---------------------------------------
FUNCTION DC_PathSetting,WritePath1=writepath1 $     ;д����һ·��(�洢����·��)
					  ,WritePath2=writepath2 $     ;д���ڶ�·��(�洢���·��)
					  ,ReadPath2=readpath2         ;ֻ��ȡ�ڶ�·��,Ĭ�϶�ȡ��һ·��

 ;�ú���������ʽΪ:
 ;result = DC_PathSetting([WritePath1=writepath1,WritePath2=writepath2,/ReadPath2])

 ;(1)��ȱʡ���б���ʱ������ֵΪĬ�϶�ȡ�ĵ�һ·����������·��),�ַ���(��ͬ)
 ;(2)��ֻ��/ReadPath2,����ֵΪ��ȡ�ĵڶ�·��
 ;(3)��ͬʱ�����˶���дʱ,ֻ����д,��·�����浽�ļ�"pathsetting.txt"��,����ֵΪ��
 ;(4)��ͬʱ����дʱ,��·�����浽�ļ�"pathsetting.txt"��,����ֵΪ��
 ;(5)���������ʱ����������

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
	OPENR,lun,File,/get_lun       ;�����ļ��е�·��
	READF,lun,SettingPath                           ;ע����������"READF"
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
			OPENW,lun,File,/get_lun        ;��·��д�뵽�ļ���.
			PRINTF,lun,StorePath
			FREE_LUN,lun

	  		RETURN,''
	  ENDIF

    IF KEYWORD_SET(ReadPath2) THEN BEGIN
    	RETURN,Allpath[1]                  ;����������·�������ڶ�·����
    ENDIF ELSE BEGIN
    	RETURN,Allpath[0]    			   ;�����������·��������һ·����
    ENDELSE

END


 ;***************�Զ��庯��:�Ե�Ѯ�������ݽ������********************************************
;��Ҫ���ڲ�������������ȡ�Լ�����������ģ��(DC_Floatyield.pro��DC_Floatyield_2.pro)
FUNCTION DC_FactorCombination,AllData,MeteoTableIndex,RowsNum,StartMonth,EndMonth

	IF (N_PARAMS() NE 5) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

       SelectedFactorIndex=WHERE(MeteoTableIndex EQ 1,FactorNum)   ;FactorNumָ���������б�ѡ�ĸ���
       CombinationData=Fltarr(1,RowsNum)
       T_ColumnNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3
       FOR k=0,FactorNum-1 DO BEGIN                                         ;��һ���ǶԻ�õĵ�Ѯ���ݽ������������Ѯ����Ѯ���.
           InterimData=FLOAT(AllData[k*T_ColumnNum:(k+1)*T_ColumnNum-1,0:*]);�õ�һ�ֲ������ӵ����е�Ѯ����
           aa=WHERE(STRMATCH(['1','2','4'],STRTRIM(SelectedFactorIndex[k],2)) EQ 1,Count)
           Interim2Initial=Fltarr(1,RowsNum)
           Interim3Initial=Fltarr(1,RowsNum)
           IF Count NE 0 THEN BEGIN                                         ;����ͷ�ʽ�����(ֻ��"��ˮ/����/����",�ֱ��Ӧ['1','2','4'])
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
           ENDIF ELSE BEGIN                                                 ;��������ƽ����ʽ���
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
           CombinationData=STRTRIM(CombinationData[1:*,0:*],2)     ;ת��Ϊ�ַ���
   return,CombinationData
END

;****************�Զ��庯��:��ȡ��Ѯ�����󲨶���������(��Ҫ����)********************************
;��Ҫ���ڲ�������������ȡ�Լ�����������ģ��(DC_Floatyield.pro��DC_Floatyield_2.pro)
FUNCTION DC_SingleTendayFactor,StartYear_,EndYear_,StartMonth_,EndMonth_,MeteoTableIndex,station_code,YieldType
    ;����:Result=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,EndMonth,MeteoTableIndex,station_code,/AgroTable)

	IF (N_PARAMS() NE 7) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	WIDGET_CONTROL, /HOURGLASS

	TableName=['TENDAY_TEMP_AVG','TENDAY_RAINFALL','TENDAY_SUNSHINE','TENDAY_AIR_HU', $
               'TENDAY_TEMP_0_ACCUMULATE','TENDAY_TEMP_MAX','TENDAY_TEMP_MIN']      ;ע���Ӧ�����е�����˳��

	IF YieldType THEN BEGIN
	   TableName = 'AGRO_'+TableName
	ENDIF ELSE TableName = 'METEO_'+TableName

   StartYear = FIX(StartYear_)   & EndYear = FIX(EndYear_)
   StartMonth = FIX(StartMonth_) & EndMonth = FIX(EndMonth_)
  ;֮�����ò�������\��\��\Ѯ���Ĳ�ѭ��,���Ա�֤һЩվ��һЩѮ������ʱ,�ÿ�ֵ����.���ȿ���ȷ��,���ٶ�������.
    RowsNum =EndYear-StartYear+1                                 ;������
    T_ColumnNum=(EndMonth-StartMonth+1)*3                        ;��Ѯ��
    AllData=STRARR(1,RowsNum)+'%'                                ;��ʼ����ֵ

	progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='�������ݴ�����,���Ժ�...',TITLE='ɸѡ����');,/CANCELBUTTON)
	progressTimer->START        ;����������

	IsSelectID = WHERE(MeteoTableIndex EQ 1,COUNT)
	TOL = LONG(COUNT)*RowsNum & NN=1

       FOR i=0,6 DO BEGIN

          IF MeteoTableIndex[i] EQ 1 THEN BEGIN
              TempyearMeteo=STRARR(T_ColumnNum,1)+'*'            ;��ʼ����ֵ
             FOR j=StartYear,EndYear DO BEGIN

;				CANCELLED = progressTimer->CHECKCANCEL()    ;ע�����??????????????????
;				IF CANCELLED THEN BEGIN
;					OK = DIALOG_MESSAGE('����ֹ��"ɸѡ"����!',TITLE='����')
;					OBJ_DESTROY,progressTimer ;����������
;				    RETURN,''
;				ENDIF
      	        progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;����������
      	         NN+=1

                TempmonthMeteo=STRARR(1)
                MeteoData=STRARR(T_ColumnNum,1)

                 Sqlstr0='select count(*) as recordNum from '+TableName[i]+' where station_id='+"'"+station_code+"'"+' and '+ $
                        'year='+STRTRIM(j,2)+' and month between '+STRTRIM(StartMonth,2)+' and '+STRTRIM(EndMonth,2)

				recordNum=LONG(DC_GetdataFromDB_Str(1,Sqlstr0,Num_BUFFERS = 500))
                 IF recordNum EQ T_ColumnNum THEN BEGIN  ;˵�������¸�Ѯ��������.��ȱѮ.
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
                          TemptendayMeteo=TemptendayMeteo[1:*]                         ;�õ�һ������Ѯ������
                          TempmonthMeteo=[TempmonthMeteo,TemptendayMeteo]
                     ENDFOR
                          TempmonthMeteo=TempmonthMeteo[1:*]                           ;�õ�һ���������µ�����
                          TempyearMeteo=[[TempyearMeteo],[TempmonthMeteo]]             ;ʹ������"�д���"����
                   ENDELSE

              ENDFOR
                TempyearMeteo=TempyearMeteo[0:*,1:*]                        ;ȥ����һ��*�ų�ʼ��ֵ,�õ�һ�ֲ�������������ݵ�����
                AllData=[AllData,[TempyearMeteo]]                           ;ʹ������"�д���"����,������Ŵ�����
           ENDIF
        ENDFOR

;;        progressTimer->DESTROY ;���ٽ�����
		OBJ_DESTROY,progressTimer

        AllData=AllData[1:*,0:*]                                           ;�õ����е�Ѯ����������

   return,AllData
END

;*******�Զ��庯��:�Ե�Ѯ���������еĿ�ֵ�������������ǿ�ֵ�ľ�ֵ���滻*************
;��Ҫ���ڲ�������������ȡģ��(DC_Floatyield_2.pro)
;��Ȼ������Ҳ������������;
FUNCTION DC_ProcessBlank,DataValue $        ;Ҫ���д���ĵ�Ѯ��������.�ַ���
					 	,Rows $		     ;��Ѯ���ݵ�����,ʵ����Ҳ������
					 	,BlankId = BlankId  ;Ϊ��ֵ��������

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
		   	  	 Temp[Temp_id] = 0.0			    ;��ȫΪ��,����0��ȡ��
		   	  	 DataValue[I,*] = STRTRIM(Temp,2)
		   	 ENDELSE
	   	  ENDIF
	   ENDFOR

   	  RETURN,DataValue
END

;***********�Զ��庯��:���Ѯ��*******************************************************
;��Ҫ���ڲ�������������ȡģ��(DC_Floatyield_2.pro)
 FUNCTION DC_CombinationTendayName,TendayFactorName,MeteoTableIndex,SingleTendayNum

	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    SelectedFactorIndex = WHERE(MeteoTableIndex EQ 1,FactorNum)
    SingleFactor=[''] & CombinationFactor=[''] & CombinationName=TendayFactorName
    FOR i=0,FactorNum-1 DO BEGIN
         TendayName=TendayFactorName & FactorPostfix2=[''] & FactorPostfix3 = ['']
        CASE SelectedFactorIndex[i] OF
           0 : Postfix='����'
           1 : Postfix='��ˮ'
           2 : Postfix='����'
           3 : Postfix='ʪ��'
           4 : Postfix='����'
           5 : Postfix='�����'
           6 : Postfix='�����'
        ENDCASE
        TendayName=TendayName+Postfix
        SingleFactor=[SingleFactor,[TendayName]]             ;�õ����ǵ�Ѯ��������,����ʼֵ

        FOR j=0,SingleTendayNum-2 DO BEGIN
           TempPostfix2=CombinationName[j]+CombinationName[j+1]
           FactorPostfix2=[FactorPostfix2,[TempPostfix2]]
        ENDFOR
        FOR j=0,SingleTendayNum-3 DO BEGIN
           TempPostfix3=CombinationName[j]+CombinationName[j+1]+CombinationName[j+2]
           FactorPostfix3=[FactorPostfix3,[TempPostfix3]]
        ENDFOR
        FactorPostfix2=FactorPostfix2[1:*]+Postfix                 ;����2Ѯ���
        FactorPostfix3=FactorPostfix3[1:*]+Postfix                 ;����3Ѯ���
        FactorPostfix23=[FactorPostfix2,[FactorPostfix3]]
        CombinationFactor=[CombinationFactor,[FactorPostfix23]]    ;�õ��������Ѯ��������,����ʼֵ

     ENDFOR
     SingleFactor=SingleFactor[1:*] & CombinationFactor=CombinationFactor[1:*]
     AllFactorName=[SingleFactor,[CombinationFactor]]
   RETURN,AllFactorName
END

;*********�Զ��庯��:�õ���Ѯ��*******************************************************
;��Ҫ���ڲ�������������ȡģ��(DC_Floatyield_2.pro)
 FUNCTION DC_SingleTendayName,StartMonth_,EndMonth_
	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

      StartMonth=FIX(StartMonth_) & EndMonth=FIX(EndMonth_)
      FactorName=['']
    FOR i=StartMonth,Endmonth DO BEGIN
        FOR j=1,3 DO BEGIN
            CASE j OF
                  1: Tenday='��Ѯ'
                  2: Tenday='��Ѯ'
                  3: Tenday='��Ѯ'
               ELSE:
            ENDCASE

            FactorName=[FactorName,STRTRIM(STRING(i),2)+'��'+Tenday]
        ENDFOR
     ENDFOR
     FactorName=FactorName[1:*]
   RETURN,FactorName
END

;****************�Զ��庯��:��ȡң����������********************************
;��Ҫ���ڲ�������������ȡ�Լ�����������ģ��(DC_Floatyield.pro��DC_Floatyield_2.pro)
FUNCTION DC_GetRsData,StartYear_,EndYear_,StartMonth_,EndMonth_,Code_,CropFiled,IsAvgOrSum,Sensor,DataType,YieldType
    ;����:Result=DC_GetRsData(StartYear,EndYear,StartMonth,EndMonth,,Code,CropFiled,Sensor,DataType,YieldType)
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

    RowsNum =EndYear-StartYear+1                                 ;������
    AllData=STRARR(1)+'%'                                ;��ʼ����ֵ

	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='�������ݴ�����,���Ժ�...',TITLE='ɸѡ����')
	progressTimer->START                         ;����������

       FOR i=StartYear,EndYear DO BEGIN
	      progressTimer->UPDATE, (FLOAT(i)/RowsNum*100.0)  ;����������

			Sqlstr ='select '+CropFiled+' from '+TableRS+' where '+WhereCol+'='+Code+' and '+ $
			 		'year='+STRTRIM(i,2)+' and month between '+STRTRIM(StartMonth,2)+' and ' $
			 		+ STRTRIM(EndMonth,2)+" and data_type='"+DataType+"' and Sensor_code='" $
			 		+ Sensor+"'"
			Temp = DC_GetdataFromDB_Str(1,Sqlstr)

			IF IsAvgOrSum EQ 0 THEN BEGIN   								;��ƽ��
				AllData = [AllData,STRTRIM(MEAN(FLOAT(Temp)),2)]
			ENDIF ELSE AllData = [AllData,STRTRIM(TOTAL(FLOAT(Temp)),2)]	;���ۼ�ֵ

       ENDFOR

;;	    progressTimer->DESTROY ;���ٽ�����
 		OBJ_DESTROY,progressTimer
	    AllData=AllData[1:*,0:*]       ;�õ����е�Ѯ����������

	return,AllData
END

;----------����Latlon_to_Albers_()����γ������תΪAlber����-------------------------------
FUNCTION DC_Latlon_to_Albers ,Lon_Lat $                         ;�����Ⱥ�γ�����ݶ�.
						  ,CenterMidian105 = CenterMidian105  ;תΪAlbers105,Ĭ��Ϊ110.
   ;;������ʽΪ: Result = DC_Latlon_to_Albers( Lon_Lat,[/CenterMidian105])  ����ΪֵΪ(X,Y)�ѿ�������
   ;��ʹ���˹ؼ���,�򽫾�γ�ȷ���ΪAlbers105������ֵ.
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
	; ����ͶӰ��ϢThe following are GCTP projections:
	; setting Projection = 103 means Albers Equal Area projection with following parameters: SEMIMAJOR_AXIS, SEMIMINOR_AXIS, STANDARD_PAR1, STANDARD_PAR2, CENTER_LONGITUDE, CENTER_LATITUDE, FALSE_EASTING, FALSE_NORTHING
	; node:FALSE_EASTING = 0.0 means FALSE_EASTING is 4 000 000.00
	; DATUM = 15 means datum is Krassovsky 6378245.0 6356863.0188
	projection = map_proj_init(103 , DATUM = 15, /GCTP, SEMIMAJOR_AXIS = 6378245.0, SEMIMINOR_AXIS = 6356863.0188, STANDARD_PAR1 = StdLat1, STANDARD_PAR2 = StdLat2, $
	                           CENTER_LONGITUDE = MidLon, CENTER_LATITUDE = 0.0, FALSE_EASTING = False_easting, FALSE_NORTHING = 0.0)
	; To get latitude and longitude of the coordinates of the point
	; MAP_STRUCTURE is the projection information

	;����ΪֵΪ(X,Y)�ѿ�������
	X_Y = MAP_PROJ_FORWARD (Lon_Lat, MAP_STRUCTURE = projection)

    return, X_Y; [X/Y]���ݶ�.
END

;----------����DC_Albers_to_Latlon()��Alber����תΪ��γ������-------------------------------
FUNCTION DC_Albers_to_Latlon,X_Y $             ;�ѿ����������ݶ�
                         ,CenterMidian105 = CenterMidian105
   ;;������ʽΪ: Result = DC_Albers_to_Latlon( X, Y,/CenterMidian105)  ����ΪֵΪlat, lon
   ;��ʹ���˹ؼ���,��Ϊ��Albers105������ֵ���ؾ�γ��.
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
	; ����ͶӰ��ϢThe following are GCTP projections:
	; setting Projection = 103 means Albers Equal Area projection with following parameters: SEMIMAJOR_AXIS, SEMIMINOR_AXIS, STANDARD_PAR1, STANDARD_PAR2, CENTER_LONGITUDE, CENTER_LATITUDE, FALSE_EASTING, FALSE_NORTHING
	; node:FALSE_EASTING = 0.0 means FALSE_EASTING is 4 000 000.00
	; DATUM = 15 means datum is Krassovsky 6378245.0 6356863.0188
	projection = map_proj_init(103 , DATUM = 15, /GCTP, SEMIMAJOR_AXIS = 6378245.0, SEMIMINOR_AXIS = 6356863.0188, STANDARD_PAR1 = StdLat1, STANDARD_PAR2 = StdLat2, $
	                           CENTER_LONGITUDE = MidLon, CENTER_LATITUDE = 0.0, FALSE_EASTING = False_easting, FALSE_NORTHING = 0.0)
	; To get latitude and longitude of the coordinates of the point
	; MAP_STRUCTURE is the projection information

	;����ֵΪ2�е�����,����Ϊlongitude/latitude coordinates
	Lon_lat = MAP_PROJ_INVERSE(X_Y, MAP_STRUCTURE = projection)

    return, Lon_lat       ; [����/γ��]
END
;----------------------------------------------------------------------------
;_____________________________������__________________________________________________
;-------------����ָ�������Ͻ���������������ɲ�ֵͼ----------------------------------
 FUNCTION DC_CREATE_INTERP_GRID,Lon_lat,value,ulx_,uly_,cellsize_,samples_,lines_,MeteoDataType,CenMedian105 = CenMedian105
   ;������ʽ: Result = DC_CREATE_INTERP_GRID(Lon_Lat,value,ulx_,uly_,cellsize_,samples_,lines_,MeteoDataType,[/CenMedian105])
   ;����ֵΪ:ָ�����������Ĳ�ֵ��ά����,����ֵΪAlbers110ͶӰ����
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
		coor = DC_Latlon_to_Albers(Lon_lat,/CenterMidian105)  ;�õ�Albers105ͶӰ.
	ENDIF ELSE BEGIN
		coor = DC_Latlon_to_Albers(Lon_lat) 				   ;�õ�Albers110ͶӰ.
	ENDELSE

	xbox=[[ulx],[ulx],[lrx],[lrx]]
	ybox=[[uly],[lry],[uly],[lry]]

	Result=MIN((coor[0,*]-ulx)^2+(coor[1,*]-uly)^2,index)   ;�����Ͻ���С����
	v1=value[index]
	Result=MIN((coor[0,*]-ulx)^2+(coor[1,*]-lry)^2,index)	;�����½���С����
	v2=value[index]
	Result=MIN((coor[0,*]-lrx)^2+(coor[1,*]-uly)^2,index)	;�����Ͻ���С����
	v3=value[index]
	Result=MIN((coor[0,*]-lrx)^2+(coor[1,*]-lry)^2,index)	;�����½���С����
	v4=value[index]

	vbox=[[v1],[v2],[v3],[v4]]

	;ȥ�ظ��ĵ�      �ĸ��ǵ�ֵ����������.�
	GRID_INPUT,[[coor[0,*]],[xbox]],[[coor[1,*]],[ybox]],[[Value],[vbox]],xSorted,ySorted,dataSorted

	TRIANGULATE,xSorted,ySorted,triangle
	;--------------------------------------------------------
;	;FUNCTION INTERP_METHOD,dataSorted,xSorted,ySorted,Samples,lines,triangle,MeteoDataType
;
;	;���ݲ�ͬ��Ҫ��(��"MeteoDataType"����)ѡ��ͬ�Ĳ�ֵ��ʽ

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
		'SUNT':BEGIN;����ʱ��
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
		'WINV':BEGIN    	;����
			powerhvalue=2
			Smoothvalue=1.0
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
						 ,POWER=powerhvalue,SMOOTHING=Smoothvalue $
						 ,DIMENSION=DIMENSION,METHOD='InverseDistance' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )
		END
		'KING':BEGIN    	;���ڲ��������ռ��ֵ
			grid=GRIDDATA(xSorted,ySorted,dataSorted $
;						 ,VARIOGRAM = [1,8,1,1]	$			;[ Type, Range, Nugget, Scale]
						 ,DIMENSION=DIMENSION,METHOD='Kriging' $
						 ,MIN_POINTS=8,TRIANGLES=triangle,SECTORS=8 )

		END
	    ELSE:
	ENDCASE

;   ͼ��ת,�Եõ���ȷ��ͼ��.
;	gridnew=MAKE_ARRAY(samples,lines,TYPE=SIZE(grid,/TYPE))
;	FOR j=0,lines-1 DO gridnew[*,lines-1-j]=grid[*,j]

    grid = REVERSE(TEMPORARY(grid), 2)
	RETURN,grid
END
;******************************************************************************
;******�ù������ɱ�׼��ENVI�ļ�:Ӱ������ + ��Ӧ��ͷ�ļ�.
 PRO DC_SaveImageFile,SAVE_FILE       $ ;��������ļ���.
 				     ,ImageData       $ ;Ҫ���������,ע��ImageData��ָ������.
 				     ,samples,lines,DataType $  ;DataTypeָ������float,Doble,char,int
 				     ,sensortype $              ;long,byte������ID,��size()����
 				     ,ULX,ULY,Resolution ,CenterMedian
;����:DC_SaveImageFile,SAVE_FILE,ImageData,samples,lines,DataType,sensortype,ULX,ULY,Resolution ,CenterMedian

	IF (N_PARAMS() NE 10) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

        enter=string(byte(13))+string(byte(10))     ;�س���ASCII��ֵ
        time=systime()                              ;����ʱ��
        col =STRTRIM(samples,2)                     ;����Ӱ������
        line=STRTRIM(lines,2)                       ;����Ӱ������
        DataType_   = STRTRIM(DataType,2)           ;��������
        sensortype_ = STRTRIM(sensortype,2)         ;����������
        tulx  = STRTRIM(ULX,2)                      ;���Ͻ�X����(ALBERS110)
        tuly  = STRTRIM(ULY,2)                      ;���Ͻ�Y����(ALBERS110)
		Resolution_   = STRTRIM(Resolution,2)		;���ش�С.Ҳ���ֱ���.
		CenterMedian_ = STRTRIM(CenterMedian,2)		;��110,����105
		IF CenterMedian_ EQ '110' THEN BEGIN
		   EastingFalse = '4000000'					;��ƫ���ٹ���.
		ENDIF ELSE EastingFalse = '0.0'

        SaveFileName = SAVE_FILE
        HeadFileName = SAVE_FILE+'.hdr'

        IF STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH) NE -1 THEN BEGIN
        	SaveFileName = STRMID(SAVE_FILE,0,STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH))
        	HeadFileName = SAVE_FILE
        ENDIF

        Bandname = STRMID(SaveFileName,STRPOS(SaveFileName,'\',/REVERSE_SEARCH)+1)  ;������·�����ļ���.

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

         openw,lun,HeadFileName,/get_lun     ;ע��:���ļ��Ѵ�ʱ,���д���̲������κ���ʾ,�������ļ��滻��.��ͬ.,
         writeu,lun,HeadInfomation
         free_lun,lun

         openw,lun,SaveFileName,/get_lun
         writeu,lun,*ImageData				;	ImageData��ָ������
         free_lun,lun

END

;------�ú�����Ҫ��Ϊ��ȡ�����Ӱ���ͶӰ�������--------------------
FUNCTION DC_ReadParameter,district      ;����districtΪ����(�ַ���)
	;Re=DC_ReadParameter(district)
	;����ֵ:(1)����Ϊ�������Ӧ����.
	;		(2)���û����Ӧ�Ĳ���,�򷵻�ֵΪ��ֵ
	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	ParaFile = 'text\parametersetting.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('ϵͳ��װĿ¼���Ҳ������������ļ�"parametersetting.txt"!',TITLE='����')
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

;**************��ENVI��׼�ļ���ͷ�ļ�*********************************************
FUNCTION DC_ReadHead_file,inputfile_ $              ;����Ҫ��ͷ���ļ�
						 ,PROMPT_DES = PROMPT_DES   ;�û�ָ����������ʾ����ʾ��,�ַ���
   ;����:Re = DC_ReadHead_file(inputfile_,[PROMPT_DES = Describe])
   ;ע��ó���ֻ����������д�ı�׼ENVIͷ�ļ��Ķ�ȡ.����ֵ����:
   ;(1) ��ȡ�ɹ�,����Ϊ��Ӧ�ļ���ͷ�ļ���Ϣ.
   ;(2) ��ȡʧ��,����Ϊ��

	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    inputfile = inputfile_
    IF NOT KEYWORD_SET(PROMPT_DES) THEN PROMPT_DES=''

    IF ~FILE_TEST(inputfile) THEN BEGIN
       Info = DIALOG_MESSAGE('�Ҳ���'+PROMPT_DES+'��Ӧ��ͷ�ļ�',TITLE='����')
       RETURN,''
    ENDIF

    IF STRPOS(inputfile, '.',/REVERSE_SEARCH) EQ -1 THEN BEGIN
       inputfile = STRTRIM(inputfile,2)+'.hdr'
    ENDIF

    OPENR,lun,inputfile,/GET_LUN
    result = FSTAT(lun)

	 if result.size eq 0 then begin
	 	Info = DIALOG_MESSAGE(inputfile+'ͷ�ļ�����ȷ!',TITLE='����')
	 	return,{error	:	-1}
	 endif
    headdata = BYTARR(result.SIZE)
    READU,lun,headdata
    FREE_LUN,lun

 ;   ��ȡ���кţ������������ݶ�ȡ��ʽ,��������.���Ͻ�XY����.

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
    index10 = STRPOS(headdata, '}',index9)                  ;ע��������˶�λλ��.

	 if (where([index0,index1,index2,index3,index4,index5,index6,index7,index8,index9,index10] eq -1))[0] ne -1 then begin
	 	Info = DIALOG_MESSAGE(inputfile+'ͷ�ļ�����ȷ!',TITLE='����')
	 	return,{error	:	-1}
	 endif

	samples    = STRMID(headdata,index0+10,index1-index0-10-2) ;֮���Լ�2����س���ռ2���ֽڳ�.
    lines      = STRMID(headdata,index1+10,index2-index1-10-2)
    bands      = STRMID(headdata,index2+10,3)                  ;�����ȡ������3,������1,���س������ֽڳ���.
    datatype   = STRMID(headdata,index3+12,3)                  ;�����ȡ������3,������1,���س������ֽڳ���.
    interleave = STRMID(headdata,index4+13,index5-index4-13-2)
    sensortype = STRMID(headdata,index5+14,index6-index5-14-2)

	Mapinfo = STRMID(headdata,index7+11,index8-index7-11)
	Mapinfomation = STRSPLIT(Mapinfo,',',ESCAPE=' ',/EXTRACT )
	UlX = Mapinfomation[3]                       			;���Ͻ�X����
	UlY = Mapinfomation[4]									;���Ͻ�Y����
	Resolution = Mapinfomation[5]	;ע�������д�ֱ��ˮƽ����ֱ���֮��,һ������ͬ��.Mapinfomation[5]=Mapinfomation[6]

	Pro_info = STRMID(headdata,index9+18,index10-index9-18)
	Projection = STRSPLIT(Pro_info,',',ESCAPE=' ',/EXTRACT )
	CenterMedian = STRTRIM(FIX(Projection[4]),2)	      ;����������,��ȷ����110,����105.

;    print,Mapinfo
;    print,UlX_Y_postion
;    print,UlX,ULY
;    print,PixelInfo
;    print,PixelSize
;��	1 - byte (8-bits)
;��	2 - integer (16-bits)
;��	3 - long integer (32-bits)
;��	4 - floating-point (32-bits)
;��	5 - double-precision floating point (64-bits)
;��	6 - complex (2x32-bits)
;��	9 - double -precision complex (2x64-bits)
;��	12 - unsigned integer (16-bits)
;��	13 - unsigned long integer (32-bits)
;��	14 - long 64-bit integer
;��	15 - unsigned long 64-bit integer.

	CASE FIX(datatype) OF     ;��Ϊ��ͬ����������,ռ�ݵ�"�ֽ������߶�����λ��"�ǲ�����ͬ��.
		1: ByteNum = 1        ;1�ֽ�(byte) = 8������λ(bit).�� 1 byte = 8 bit
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

  ;��ǧ��Ҫ�Ķ��ṹ�������˳��,��Ϊ����������������ʱ,�а��������������õ�.
  ;,��fileinfo.(3)�����ò�����,���Ķ�,����ִ���.Ҫ�޸ĳ��������,ֻ���ں������.
  ;;��Ȼ�ǰ�����������,��û���κ�Ӱ��

   RETURN,fileinfo={samples     :  samples   	,$
                    lines       :  lines       	,$
                    bands       :  STRTRIM(FIX(bands),2)        ,$   ;ȥ���س���.��ͬ.
                    datatype    :  STRTRIM(FIX(datatype),2) 	,$
                    interleave  :  interleave  	,$
                    sensortype  :  sensortype 	,$
                    ByteNum		:  ByteNum 		,$
                    UlX			:  UlX 			,$
                    UlY			:  UlY 			,$
                    Resolution	:  STRTRIM(LONG(FLOAT(Resolution)),2) 	,$   ;����������Ϊ��ʱFIX(1.0000000000e+003)���Ϊ1,��float()����.
                    CenterMedian:  CenterMedian}
END

;**************��ENVI��׼�ļ�������*********************************************
FUNCTION DC_Read_ENVIData,inputfile_ $  				;������ļ���(�ַ���)
					  ,SUCCESSSTATUS = SuccessStatus $	;��ȡ�Ƿ�ɹ���״̬.
					  ,DESCRIPTION  = Description		;�û��������ʾ��,�ַ���
	;������ʽ:Resutl = DC_Read_ENVIData(inputfile_,[SUCCESSSTATUS = var],[DESCRIPTION=Decscription])
	;����ֵΪ:(1)SuccessStatus=0 ,��ȡ���ɹ�Ϊ0
	;		  (2)SuccessStatus=1 ,��ȡ�ɹ�,����Ϊ����
    ;ע��ó���ֻ�����ڶ���׼ENVI�ļ�(�����ļ�+ͷ�ļ�)
    ;,��ֻ�޶�ȡ����������(�����Ҳ��ɶ��ನ�ε�)
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
	       prompt = DIALOG_MESSAGE(Description+'ͷ�ļ�������!',TITLE='����')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END

        ~FILE_TEST(DataFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Description+'�����ļ�������!',TITLE='����')
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

    ;��Ϊ�������ļ���С�������ֽڼ��㳤��,���������Ͳ�ͬ,����������ʱҲ��������ͬ.
    IF ROUND((DataFileInfo.SIZE)/FIX(HeadFileInfo.ByteNum),/L64) NE $
       ULONG(HeadFileInfo.samples)*(HeadFileInfo.lines) THEN BEGIN
       prompt = DIALOG_MESSAGE(Description+'�����ļ���ͷ�ļ���Ϣ��������һ��!',TITLE='����')
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
    	Info = DIALOG_MESSAGE(DataFile+'�����ļ�����ȷ!',TITLE='����')
    	SuccessStatus = 0
	 	return,-1
	 endif
    READU,Lun,Data
    FREE_LUN,Lun


	IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 1

   RETURN,Data

END
;***********************����ֵͼ**************************************
;=================��"����ͼ�δ���"�д�Ӱ��ͼ��ʸ��ͼ=============================
PRO DC_Draw_image,INputFile   $	;����ʾ��Ӱ���ļ���,�ַ���
				  ,WID_DRAW   $	;Draw���,��������û�ֵdata,��dataά��С��INputFile�ļ�Ӱ��ά���ݱ�����ͬ
				  ,OView=View $ ;���ص�Ҫ�õ�����ͼ����
				  ,MINVALUE=minvalue $  ����ָ����BYTSCL()�����������Сֵ
				  ,WHITE=WHITE	 ;ָ������ɫΪ��ɫ.
; ����:DC_Draw_image,INputFile,WID_DRAW[,OView=View,MINVALUE=minvalue,/WHITE]
 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	forward_function DC_ReadHead_file

	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
	oWindow->ERASE,COLOR=255
	oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	ShapeFileName='data_vector\province.shp'
	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&���

	;(1)------------��Ӱ��ͼ-----------------------------------------
;	oColor = [128,128,128]
	IF (N_ELEMENTS(data) EQ 0) THEN data= DC_Read_ENVIData(INputFile) ;���WID_DRAWû���û�ֵ,��������Ӱ��
	IF ~KEYWORD_SET(WHITE)      THEN oColor = [255,255,255] ELSE oColor = [0,0,0]
	IF ~KEYWORD_SET(MINVALUE)  THEN minvalue = MIN(data)

	oScene = Obj_New('IDLgrScene', Color = oColor)
	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;������ͼ
	Image_Layer   = Obj_New('IDLgrModel')	  ;Ӱ��ͼ��
	Polygon_Layer = obj_new('IDLgrModel')     ;ʸ��ͼ��

	oWindow->SetProperty, Graphics_Tree = oScene
	oWindow->SetCurrentCursor, 'ARROW'    	   ;���ָ��Ϊ��ͷ,����ʮ�ֲ�.

	oScene->Add, oView
	oView ->Add, Image_Layer
	oView ->Add, Polygon_Layer


	;����ֵ���쵽256ɫ��ʾ��Χ��

	dataDis = BYTSCL(data,MIN=minvalue)

	Palette = obj_new('IDLgrPalette')
	Palette->LoadCt,39
	Palette->SetRGB,0,255,255,255

	;TVLCT,0,0,0    ;ʹ0����ɫ��������ɫΪ��ɫ(������ʵ���Կ����Ǳ���ɫ,Ĭ���Ǻ�ɫ),

	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
			               ,dataDis)   ;����dataDis����������

	Image_Layer->Add,oImage

	;----��ʸ�����ݽ������ŵĲ���---------------------------------------
	Para = DC_ReadHead_file(INputFile)
	if n_tags(Para) eq 1 then 	return

	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;���Ͻ�
	samples = Double(Para.samples) & Lines = Double(Para.Lines)  ;
;;	Resolution = FIX(Para.Resolution)	��FIxʱ,������Ϊ"1.0000000000e+003",���ܱ���ȷʶ��.������1
	Resolution = FLOAT(Para.Resolution)
	uvRange = [UlX,UlY-Lines*Resolution,UlX+samples*Resolution,UlY]
	uRange  = uvRange[2]-uvRange[0]
	vRange  = uvRange[3]-uvRange[1]

	Datasize = SIZE(data,/DIMENSIONS)
	xSize = Datasize[0]
	ySize = Datasize[1]
	drawSize = [r.scr_xsize,r.scr_ysize]

	;----��ʸ������ͼ��----------------------------------------
	IF file_test(ShapeFileName) EQ 0  THEN return
	myshape=OBJ_NEW('IDLffShape', ShapeFileName)
	myshape -> IDLffShape::GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type

	IF (type EQ 5 OR type EQ 3) THEN BEGIN
	 FOR i=0,num_ent-1 DO BEGIN
	    ent = myshape -> IDLffShape::GetEntity(i)
	    NumPoints = ent.N_VERTICES-1
	    x = (*ent.vertices)[0,0:NumPoints]
	    y = (*ent.vertices)[1,0:NumPoints]
	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;���ｫX,Y�����������
		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange

	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[0,0,0],thick = 2,LINESTYLE = 0) ;[245,122,182]
	    Polygon_Layer->Add,oPolyline
	    myshape -> IDLffShape::DestroyEntity, ent
	 ENDFOR
	ENDIF
	OBJ_DESTROY, myshape

	;view��������ϵ
	viewPlane = [0,0,xSize,ySize]
	;view�Ĵ�С
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

	;view�Ĳ���λ��
	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]

	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
	oWindow->Draw,oScene

	OBJ_DESTROY,Palette

	IF ARG_PRESENT(View) THEN View=oView
END

;$$$$$$$$$$$$$$$$$$$$$����״��$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO DC_Draw_BAR_PLOT,drawID $			;DRAW�����IDֵ,��GET_VALUE
					,AnalysisData $		;Ҫ��������
					,XLabel	$			;X��ı�ǩ�
					,LINE = Line $		;ֻ������ͼ,Ĭ��Ϊֻ����״ͼ
					,CHARTLINE = ChartLine  ;ͬʱ����״����״ͼ
;����:DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel[,/Line,/ChartLine]
;������ֻ������"�����ں�ģ��"��ʹ��,������������,���ܲ�����.
 	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	  PlotNum = N_ELEMENTS(XLabel)    ;���͵ĸ���

	  DEVICE,GET_DECOMPOSED=old_color     ;��ȡ��ǰDECOMPOSEDֵ
;      DEVICE,GET_CURRENT_FONT=oldFont
;      DEVICE,SET_FONT='����',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;��IDL�ṩ�󱸴洢,ʹ����ɫ��ѯ��(ͣ����ɫ�ֽ⹦��),
		r=[0,255,  0,  0,255,255]   	  ;����Ϊ��\��\��\��\��\��
		g=[0,  0,255,  0,255,255]
		b=[0,  0,  0,255,  0,220]
 		TVLCT, r, g, b   ;ȱʡ���ĸ�ʡ��,��ʹ��ɫ����������Ϊ0,1,2,3,4,5����ɫΪ��Ӧ��RGB���


    OldWin = !D.WINDOW     			   ;����ϵͳ�Ĵ���
    OldBackup = !P.BACKGROUND
	Old_p     = !P.POSITION
    Old_Font  = !P.FONT
    OldFontSiz = !P.CHARSIZE
    OClor = !P.COLOR
    OldYticks = !Y.TICKS
    !P.FONT = 0
	!P.BACKGROUND = 255
	!P.COLOR = 800   ;�����ɫ
    !P.CHARSIZE = 0.8

    Colors = INTARR(PlotNum)
	FOR I = 0, PlotNum-1 DO Colors[I]= I ;(2*I)+100

   	WSET, drawID
	PlotData = FLOAT(AnalysisData)
	Psym = 3   ;(���ʶ)
	IF KEYWORD_SET(ChartLine) THEN BEGIN   ;����״����״ͼ
	    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+4)+' ' $ 	;YTICKLEN���Ʊ�־�ĳ���
			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKS���ڿ�������־�ĸ���
;		PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
;	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN���Ʊ�־�ĳ���
;			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKS���ڿ�������־�ĸ���

		OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;�ð���ɫ��PLOT�����߸��ǵ�,��Ҫע���XRANGE��ֵ,��Ȼ��ͼЧ����

	    FirstOFFSET = 0.3 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
	    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
			,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
			,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-6
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(Line) THEN BEGIN  ;ֻ����״ͼ
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
		         ,POSITION=[0.09,0.28,0.96,0.93] ,XTICKNAME=[' ',' ',XLabel,' ',' ',' '] $ 	;YTICKLEN���Ʊ�־�ĳ���
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)] $
				 ,XTICKLEN=1 ; ,TITLE = TITLE				;YTICKS���ڿ�������־�ĸ���

			OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-5
		ENDIF ELSE BEGIN				;ֻ����״ͼ
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+3)+' ' $ 	;YTICKLEN���Ʊ�־�ĳ���
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKS���ڿ�������־�ĸ���
;			PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
;		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN���Ʊ�־�ĳ���
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKS���ڿ�������־�ĸ���


			OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;�ð���ɫ��PLOT�����߸��ǵ�,��Ҫע���XRANGE��ֵ,��Ȼ��ͼЧ����
		    FirstOFFSET = 0.8 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
		    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $
				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
				,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		ENDELSE
	ENDELSE

	!P.BACKGROUND = OldBackup		;��ԭ
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor
	!Y.TICKS = OldYticks
;	DEVICE,SET_FONT=oldFont
	DEVICE,DECOMPOSED=old_color   ;����ԭ����DECOMPOSEDֵ,��Ϊ�Զ��庯��MyColor�ı���,�뻹ԭ.

	WSET, OldWin				;��ԭԭ������.

END

;$$$$$$$$����ά���鱣��Ϊ*.txt��ʽ�ļ�$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO DC_SaveTextData,Save_Data 			$		;Ҫ����Ķ�ά����
				   ,Parent				$		;WidgetID,��Ϊ�ļ�ѡ��Ի����ڵĸ����.
				   ,FILENAME = FILENAME $ 		;ָ��Ҫ������ļ���
				   ,NOSavePath = NOSavePath		;������·��,Ĭ�ϱ���
;����:DC_SaveTextData,Save_Data,Parent,[FILENAME=Filename,/NOSavePath]

;ע��:���Save_Data��һ������,��ó�����ܳ�������,����SIZE(/DIMENSIONS)��ԭ��

 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	IF NOT KEYWORD_SET(FILENAME) THEN FILENAME=''

	TEMP = WHERE(Save_Data EQ '',COUNT)
	IF COUNT NE 0 THEN Save_Data[TEMP]='---'

	Filename=DIALOG_PICKFILE(TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
	  ,/OVERWRITE,FILE = FILENAME,/WRITE,PATH=DC_PathSetting() $
	  ,GET_PATH=SavePath,DIALOG_PARENT=Parent)

	IF Filename EQ '' THEN RETURN

	DataSize = SIZE(Save_Data,/DIMENSIONS)
	Samples = DataSize[0]
	OPENW,LUN,Filename,/GET_LUN ,WIDTH=Samples*(MAX(STRLEN(STRTRIM(Save_Data,2)))+1)
	PRINTF,LUN,Save_Data;,FORMAT='(5(A20,2X))'
	FREE_LUN,LUN

	INFO = DIALOG_MESSAGE('����ɹ�!',/INFORMATION,TITLE='��ʾ')

	IF KEYWORD_SET(NOSavePath) THEN RETURN

	path = DC_PathSetting(WRITEPATH1= SavePath)

END
;------------��Ȩ��ʡ-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,������CROP_AREA_COUNTY
;ע������ȡ�������ʱ,�����ǰ��ݵ��������û��,���ÿ���������Newestyear���������
FUNCTION DC_WeightToPro,CountyYield  	$	;2��ֵ,����\�������,
					   ,Crop_id 		$	;���б��������ַ���
					   ,CalYear 		$
					   ,ProID 			$        ;2λ
					   ,model_type_id  		;ʡ������������������ID,

;������ʽ: DC_WeightToPro,CountyYield,Crop_id,CalYear,ProID,model_type_id
;����ֵ:1 ����ɹ�
;		0 ���㲻�ɹ�

 	IF (N_PARAMS() NE 5) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR,2						;return to caller

	COMMON COMMON_BLOCK

	CropIDList = ['11','12','21','22','23','31','32','41']    ;CropIDList������CropFieldList��Ӧ
    CropFieldList = ['SPRING_WHEAT','WINTER_WHEAT','EARLY_RICE','SEMILATE_RICE','LATE_RICE','SPRING_CORN','SUMMER_CORN','SOYBEAN']

    C_Index = WHERE(CropIDList EQ crop_id[0],count)
    IF count EQ 0 THEN BEGIN
       INFO = DIALOG_MESSAGE('���ݿ���û��������"'+Crop_id[0]+'",��ȷ��!',TITLE='����')
       RETURN,0
    ENDIF

    CropField = CropFieldList[C_Index]
    Sqlstr ='select distinct Year as NewestYear from CROP_AREA_COUNTY where YEAR='+CalYear
    Currentyear = DC_GetdataFromDB_Str(1,SQLstr,N_RECORDS = Num)
    IF Num EQ 0 THEN BEGIN
       INFO = DIALOG_MESSAGE(['���ݿ���CROP_AREA_COUNTY����û�е�ǰ�����Ӧ' $
       						 ,'��������������,ʹ�������ݵ����������?!'],/QUESTION,TITLE='ѯ��')
       IF INFO EQ 'No' THEN RETURN,0
       Sqlstr='select max(year) as NewestYear from CROP_AREA_COUNTY' ;��һ����Ϊ������
    ENDIF

    Sqlstr='select county_code,'+CropField+' from CROP_AREA_COUNTY a,('+Sqlstr $
    		+') b where a.year=b.NewestYear and '+CropField+'>0 ' $
    		+'and LEFT(a.county_code,2)='	+ProID
	CountyCropArea = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = Num)   ;2��ֵ,����\�������
	IF Num EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('���ݿ���CROP_AREA_COUNTY����û����Ӧ��������������!',TITLE='����')
		RETURN,0
	ENDIF

    CountyNum=N_ELEMENTS(CountyYield)/2

      CountyYieldArea=STRARR(3,1) & Count = 0
      FOR i=0,CountyNum-1 DO BEGIN
          Temp=STRARR(3,1)
           FOR j=0,Num-1 DO BEGIN
               IF CountyYield[0,i] EQ CountyCropArea[0,j] THEN BEGIN
                  Temp[0,0]= CountyYield[0,i]    	;����
                  Temp[1,0]= CountyYield[1,i]    	;�ع������
                  Temp[2,0]= CountyCropArea[1,j]    ;���������
                  Count += 1
          		  CountyYieldArea=[[CountyYieldArea],[Temp]]
               	  BREAK
               ENDIF
           ENDFOR
      ENDFOR
      IF Count EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('���ݿ���CROP_AREA_COUNTY����û����Ӧ��������������!',TITLE='����')
		RETURN,0
      ENDIF
      CountyYieldArea = CountyYieldArea[*,1:*]
      Area = FLOAT(CountyYieldArea[2,*])
      ;�õ���Ȩ��ʡ���ﵥ����,����15,��λΪ����/����
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
PRO DC_A_Pro_Fun  ;�ļ�ͷ��Ҫ�ɲ�Ҫ
END