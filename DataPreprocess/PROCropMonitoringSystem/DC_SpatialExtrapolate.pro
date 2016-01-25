
;******ͳ�Ʋ�������������************************************
FUNCTION DC_StatisticFloat,StaFile,WeightFile,STATUS = status
	;StaFile  ��ͳ���ļ�;WeightFileȨ���ļ�;status�����Ƿ�ɹ���״̬
	;������ʽ:Re = DC_StatisticFloat(StaFile,WeightFile,[STATUS=status]) ע��ֻ����status
	;����ֵ: ���ؽṹ��
	;�㷨:Sum(Pix*W)/sum(W)  Pix��Ԫֵ;WΪȨ��

	CountyRaster = 'data_grid\county_raster'

	WeightData = DC_Read_ENVIData(WeightFile,SUCCESSSTATUS = StatusW,Description='����Ȩ���ļ�')
	IF NOT StatusW THEN BEGIN		;WeightData��0-100֮��
		STATUS=StatusW
		RETURN,0
	ENDIF

	StaData    = DC_Read_ENVIData(StaFile,SUCCESSSTATUS = StatusS,Description='��ֵ�ļ�')
	IF NOT StatusS THEN BEGIN		;StaData ����ֵ
		STATUS=StatusS
		RETURN,0
	ENDIF

	CountyData= DC_Read_ENVIData(CountyRaster,SUCCESSSTATUS = StatusC,Description='��դ���ļ�')
	IF NOT StatusC THEN BEGIN
		STATUS=StatusC
		RETURN,0
	ENDIF

	Sinfo = DC_ReadHead_file(StaFile)
	Winfo = DC_ReadHead_file(WeightFile)
	Cinfo = DC_ReadHead_file(CountyRaster)
	IF FIX(Sinfo.samples) NE FIX(Winfo.samples) OR FIX(Sinfo.samples) NE FIX(Cinfo.samples) OR   $
	   FIX(Sinfo.lines)   NE FIX(Winfo.lines)   OR FIX(Sinfo.lines)   NE FIX(Cinfo.lines) THEN BEGIN
		Info = DIALOG_MESSAGE('����ͳ�ƵĻ����������ֵ�ļ���С��һ��,ͳ��ʧ��!',TITLE='����')
		STATUS=0
		RETURN,0
	ENDIF

	CountyID = CountyData[UNIQ(CountyData,SORT(CountyData))]    ;�õ�Ψһ����Index (�����ش���)
	CountyID = CountyID[WHERE(CountyID NE 0)]					;ȥ��0 Index

	Result = REPLICATE({CountyID:0,FloatYield:0.0},N_ELEMENTS(CountyID))

	FOR I=0,N_ELEMENTS(CountyID)-1 DO BEGIN
		County  = WHERE(CountyData EQ CountyID[I])
		DataV   = StaData[County]			;DOUBLE��
		W_Value = WeightData[County]*1.0	;BYTE��,��תΪ������.
		DataV   = TEMPORARY(DataV)*W_Value

		Valid   = WHERE(W_Value GE 10)				;ֻͳ��Ȩ�ش���10%

		Result[I].FloatYield = TOTAL(DataV[Valid])/TOTAL(W_Value[Valid])
		Result[I].CountyID   = CountyID[I]

	ENDFOR

	status = 1
	RETURN,Result

END

;**********************************************************************
PRO DC_CLEARUP_DATA,EventTop
   Widget_Control ,EventTop,GET_UVALUE=state
      Widget_Control,(*state).FloatYield_Table,GET_VALUE=MeteorologyYield & MeteorologyYield=''
      Widget_Control,(*state).FloatYield_Table,SET_VALUE=MeteorologyYield[*,*],ROW_LABELS=''

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=StaYield & StaYield=''
      Widget_Control,(*state).StaYield_TABLE,SET_VALUE=StaYield[*,*],ROW_LABELS=''

      Widget_Control,(*state).StaYield_LABEL,SET_VALUE='��ֵ���ͳ�Ʋ���'

      Widget_Control,(*state).prompt_TEXT ,SET_VALUE=''

      Widget_Control,(*state).Extrapolate_DRAW ,GET_VALUE=Owindow
      Owindow->ERASE,COLOR=255
	  oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	  IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	 (*state).Savefile = ''   ;ͬʱʹ���������ļ���Ϊ��

   Widget_Control ,EventTop,SET_UVALUE=state
END
;**********�Զ������:��ȡվ��ģ��Ĳ�������************************************
PRO DC_TakeSimpleDotData,EventTop

	ON_ERROR,2
    DC_CLEARUP_DATA,EventTop

    Widget_control,/HOURGLASS

    Widget_Control,EventTop ,GET_UVALUE=state
    CropName = STRCOMPRESS((*state).CropName[(*state).Cropindex],/REMOVE_ALL)
	CropId = (*state).CropIDList[(*state).Cropindex]

;    ProName = (*state).Province[WHERE((*state).ProIDList EQ (*state).ProID)]

;*********�ؼ���䣬20070825��������**********************************************

;==========ԭ���=========================================================================
;	IF (*state).DataType THEN ModelData_id = '2' ELSE ModelData_id = '1'
;
;    Sqlstr='select AgroMeteoStation_CODE,Meteo_OR_Fluctuate_Yield,year from AGROSTATION_Crop_MeteoOrFluctuate_yield ' $
;    		+"where crop_id='"+CropId+"' and year="+(*state).CalcYear $
;    		+' and ModelData_id='+ ModelData_id
;    Sqlstr='select b.year,code,name,b.Meteo_OR_Fluctuate_Yield,longitude,latitude ' $
;    		+'from AGRO_METEO_STATION_INFO a,('+Sqlstr+') b '$
;    		+'where a.code=b.AgroMeteoStation_CODE AND LEFT(a.COUNTY_CODE,2)=' $
;    		+(*state).ProID+' order by b.AgroMeteoStation_CODE'
 ;����Ϊԭ���==========================================================================
 ;===========����Ϊ�������޸ģ�20070825=================================================
; 	IF (*state).DataType THEN ModelData_id = '2' ELSE ModelData_id = '1' ;DataType  0Ϊ����1Ϊң��
; 	IF (*state).YieldType THEN table = 'AGROSTATION_' ELSE table = 'COUNTY_'

	;===========�޸ģ�20070907=================================================

	;DataType  0Ϊ����1Ϊң��
	;YieldType   ��(0)  վ��(1)
	;
	;ModelData_id
	; 1:�������󲨶��������ݼ���;2:��ң�в�����������;3:��վ������������ݲ�ֵ����;4:վ��ң�в����������ݲ�ֵ����
	;
	;Model_type_id
	;0�����յ���;1:��ͳ������ũ��ģ�ͼ���õ���;2:վ������ũҵģ�͵õ���3����ң�����ݼ���õ���4��վ��ң�м���õ���
	;5����������ֵ�õ���6���������ջ�ϵ���õ���7�����ɷַ�����һ���ɷ֣�8����һ���ɷֽ�ģ�õ�

	IF (*state).DataType THEN begin
		if (*state).YieldType then ModelData_id = '4' else ModelData_id = '2'
	endif else begin
		if (*state).YieldType then ModelData_id = '3' else ModelData_id = '1'
	endelse

	if (*state).YieldType then begin	;վ��(1)
	    Sqlstr='select AgroMeteoStation_CODE,Meteo_OR_Fluctuate_Yield,year from AGROSTATION_Crop_MeteoOrFluctuate_yield ' $
	    		+"where crop_id='"+CropId+"' and year="+(*state).CalcYear $
	    		+' and ModelData_id='+ ModelData_id
	    Sqlstr='select b.year,code,name,b.Meteo_OR_Fluctuate_Yield,longitude,latitude ' $
	    		+'from AGRO_METEO_STATION_INFO a,('+Sqlstr+') b '$
	    		+'where a.code=b.AgroMeteoStation_CODE AND LEFT(a.COUNTY_CODE,2)=' $
	    		+(*state).ProID+' order by b.AgroMeteoStation_CODE'
    endif else begin	;��(0)
    	Sqlstr='select County_CODE,Meteo_OR_Fluctuate_Yield,year from COUNTY_Crop_MeteoOrFluctuate_yield ' $
	    		+"where crop_id='"+CropId+"' and year="+(*state).CalcYear $
	    		+' and ModelData_id='+ ModelData_id
	    Sqlstr='select b.year,code,name,b.Meteo_OR_Fluctuate_Yield,longitude,latitude ' $
	    		+'from COUNTY_JINGWEI a,('+Sqlstr+') b '$
	    		+'where a.code=b.County_CODE AND LEFT(a.CODE,2)=' $
	    		+(*state).ProID+' order by b.COUNTY_CODE'
    endelse
 ;======================================================================================

;********************************************************************************




	MeteorologyYield=DC_GetdataFromDB_Str(6,Sqlstr,N_RECORDS = NumReocrd)
	IF NumReocrd EQ 0 THEN BEGIN
;;		 DC_CLEARUP_DATA,EventTop
;		 Prompt=DIALOG_MESSAGE('����û�ж�"'+ProName+CropName+'"����'+(*state).CalcYear+'��'+ $
;		                       '��վ�㲨������ģ�����,����ģ��!',TITLE='��ʾ',/INFORMATION)
		 Prompt=DIALOG_MESSAGE('����û�ж�"'+CropName+'"����'+(*state).CalcYear+'��'+ $
		                       '���ز�������ģ�����,����ģ��!',TITLE='��ʾ',/INFORMATION)
	 	 RETURN
	ENDIF


    RowsNum = NumReocrd   & ColumnLabel=['�ش���','����','��������','����','γ��']
    Widget_Control,(*state).FloatYield_Table, TABLE_YSIZE=RowsNum,SET_VALUE=MeteorologyYield[1:*,0:*] $
              ,COLUMN_LABELS=ColumnLabel,ALIGNMENT=2 $,ROW_LABELS=MeteorologyYield[0,0:*] $
              ,SET_UVALUE=MeteorologyYield              ;�û�ֵ���ڱ�����(�����).
;    Widget_Control,(*state).prompt_TEXT ,SET_VALUE='��ʾ:��ǰѡ�����'+ProName+(*state).CalcYear+'��ģ���'+CropName+ $
;                'վ�㲨������!��'+ STRTRIM(STRING(RowsNum),2)+'��վ��!'
	Widget_Control,(*state).prompt_TEXT ,SET_VALUE='��ʾ:��ǰѡ�����'+(*state).CalcYear+'��ģ���'+CropName+ $
                '�ز�������!��'+ STRTRIM(STRING(RowsNum),2)+'����!'

END
;****���ݲ�ֵͼ���в���ͳ�Ƶ�"ͳ��"��ť**********************************
PRO DC_StatisticEV,EVENT

	Widget_Control,Event.top ,GET_UVALUE=state
	CropName =(*state).CropName
	CROP     =(*state).CropIDlist                ;��������ID��,ע��Ҫ��Ӧ����DROPLIST
	crop_id   = CROP[(*state).Cropindex]    &   year=(*state).CalcYear
	Crop_name = CropName[(*state).Cropindex]
	YN_TakeSaveFile = 0      ;����ָʾ�Ƿ����ѱ����Ӱ����ͳ��:0--��;1--��

	Widget_control,/HOURGLASS

;	CATCH, Error_status               ;��ȡ����.
;	IF Error_status NE 0 THEN BEGIN
;		infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
;		CATCH, /CANCEL
;		RETURN
;	ENDIF

        IF  (*state).Savefile EQ '' THEN BEGIN              ;�õ�һ�������ж�.
			YN_TakeSaveFile = 1
			Prompt=DIALOG_MESSAGE(['����û�н��пռ��ֵ,���Ȳ�ֵ!���Ҫͳ���ѱ���Ĳ�ֵ' $
			                 ,'ͼ,�밴"��(Y)"ť,����"��(N)"�ص���һ���Ƚ��в�ֵ!'],TITLE='��ʾ',/QUESTION)
			IF Prompt EQ 'No' THEN BEGIN
				Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0
				RETURN
			ENDIF
			Filename=DIALOG_PICKFILE(TITLE='��ѡ���ֵͼ��', FILTER=['*.hdr'],PATH=DC_PathSetting(),/MUST_EXIST, DIALOG_PARENT=Event.id)

			IF Filename EQ '' THEN RETURN

			StatisMapFile=STRMID(STRTRIM(Filename,2),0,STRLEN(Filename)-4)    ;��Ϊȥ��".hdr"

			CropNameEnglish=['','SpringWheat','WinterWheat' ,'EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			Pos = STRPOS( StatisMapFile,'\',/REVERSE_SEARCH)
			E_crop=STRMID(StatisMapFile,Pos+5)                                ;Ӣ�ĵ�������
			aa = WHERE(CropNameEnglish EQ E_crop,Count)
			IF Count eq 0 THEN BEGIN
			   Prompt=DIALOG_MESSAGE(['������ȷ�����Ĳ�ֵͼ���ļ�,' $
			   						 ,'��ȷ��������ʽ��"2006SpringWheat"!'],TITLE='����')
			   RETURN
			ENDIF
			Crop_name=CropName[aa]
			crop_id=CROP[aa]
			year=STRMID(StatisMapFile,Pos+1,4)

			IN_file=StatisMapFile

			PaddyCrop=['21','22','23'] ;��Ӧ��PaddyCrop=['EarlyRice','SemiRice','LateRice']
			IF WHERE(PaddyCrop EQ crop_id[0]) EQ -1 THEN BEGIN
				FarmlandFile='data_grid\farm_drought'                         ;�������ˮ��,���ú��ذٷֱ��ļ�.
			ENDIF ELSE BEGIN
				FarmlandFile='data_grid\farm_rice'
			ENDELSE

			FarmData = DC_Read_ENVIData(FarmlandFile,SUCCESSSTATUS = Status,DESCRIPTION='���ز�����ļ���')
			IF Status EQ 0 THEN RETURN

			InterpData = DC_Read_ENVIData(IN_file,SUCCESSSTATUS = Status,DESCRIPTION='��ѡ�ļ���')
			IF Status EQ 0 THEN RETURN

			ZeroValue=WHERE(FarmData EQ 0,COMPLEMENT=NoZero)         ;Ϊȡ��ʡ���ر߽������������
			(*state).AddMin = 300.0-MIN(InterpData[NoZero])
			InterpData[ZeroValue]=0                                     ;�����ر߽����ֵ��Ϊ0
			InterpData[NoZero]   = InterpData[NoZero]+(*state).AddMin   ;�������ڵ�ֵ������300

			WIDGET_CONTROL,(*state).Extrapolate_DRAW,SET_UVALUE = InterpData

			DC_Draw_image,IN_file,(*state).Extrapolate_DRAW,oView=oView,MINVALUE=299.99,white=1
			OBJ_DESTROY,(*state).oView
			(*state).oView = oView

         ENDIF ELSE BEGIN
           IN_file=(*state).Savefile
         ENDELSE

      IF YN_TakeSaveFile EQ 1 THEN BEGIN
         Widget_Control,(*state).prompt_TEXT ,SET_VALUE='��ʾ:��ǰѡ��ͳ�Ƶ���'+year+'��'+Crop_name+'��������!'
      END

  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='ͳ��',MESSAGE='����ͳ�ƴ�����,���Ժ�!') ;�½�����������
	 progressTimer->START
     progressTimer->UPDATE,(0.2 * 100.0)  ;���½�����

     PaddyCrop=['21','22','23']
     IF WHERE(PaddyCrop EQ crop_id[0]) EQ -1 THEN BEGIN
        LandWeightFile='data_grid\DRY_LAND_ratio'                         ;�������ˮ��,���ú��ذٷֱ��ļ�.
     ENDIF ELSE BEGIN
        LandWeightFile='data_grid\PADDY_FIELD_ratio'
     ENDELSE

    Infile=IN_file

;  �õ���RESULT�ǽṹ��,����CountyID(��id,���ش���),FloatYield(ƽ��ֵ)������Ҫ�Ĳ�������
	RESULT = DC_StatisticFloat(Infile,LandWeightFile,STATUS = status)
    IF status EQ 0 THEN RETURN   ;ͳ�Ʋ��ɹ�����

    progressTimer->UPDATE, (0.75 * 100.0)  ;���½�����

    RowsNUM = N_elements(RESULT)

	StatisticYield = [TRANSPOSE(STRTRIM(RESULT.FloatYield,2)),TRANSPOSE(STRTRIM(RESULT.CountyID,2))]
; 	ProName = (*state).Province[WHERE((*state).ProIDlist EQ (*state).ProID)]

    SQLstr='select code,raster_value from COUNTY_CODE_RASTER where LEFT(code,2)='+(*state).ProID
	CountyCodeID = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = n)  ;��һ����Ϊ���County_code_raster���ش���.
	IF n EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('������COUNTY_CODE_RASTER��û�������Ӧ����!',TITLE='����')
;;        progressTimer->DESTROY ;���ٽ�����
         OBJ_DESTROY,progressTimer
		RETURN
	ENDIF

    progressTimer->UPDATE, (0.8 * 100.0)  ;���½�����

     CountyMeteoYield=STRARR(2,1)
     FOR i=0,RowsNUM-1 DO BEGIN
         MeteoYield=STRARR(2,1)
         FOR j=0,n-1 DO BEGIN
            IF FIX(StatisticYield[1,i]) EQ fIX(CountyCodeID[1,j]) THEN BEGIN
               MeteoYield[0,0]=CountyCodeID[0,j]
               MeteoYield[1,0]=StatisticYield[0,i]                ;����CountyID��raster_value�Ķ�Ӧ,���ش���Ͳ���������Ӧ
               CountyMeteoYield=[[CountyMeteoYield],[MeteoYield]]
               BREAK
            ENDIF
         ENDFOR
     ENDFOR

    TempYield=CountyMeteoYield[0:*,1:*]     ;�������һ��Ϊ�ش���,�ڶ���Ϊ��ֵͳ�ƵĲ�������.
    CountyMeteoYield = 0B  ;�ͷ��ڴ�
    progressTimer->UPDATE, (0.9 * 100.0)  ;���½�����
  ;------------------------------------------------------------------------
   SQLstr="select county_code,trend_yield from COUNTY_CROP_TREND_YIELD where crop_id='" $
          + crop_id+"' and year="+year+' and LEFT(county_code,2)='+(*state).ProID  ;��һ����Ϊ�����Ʋ������л���ش�������Ʋ���.
	CountyCodeTR = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = n)
	IF n EQ 0 THEN BEGIN
       INFO=DIALOG_MESSAGE('����û�н���'+year+'��"'+Crop_name+'"������ģ��,���Ƚ�������ģ��!',TITLE='����')
;;       progressTimer->DESTROY ;���ٽ�����
        OBJ_DESTROY,progressTimer
	   RETURN
	ENDIF
 ;-----------------------------------------------------------

     progressTimer->UPDATE, (0.98 * 100.0)  ;���½�����

     SimuYield=STRARR(3,1) & RowsNUM=N_ELEMENTS(TempYield)/2
     FOR i=0,n-1 DO BEGIN
         MeteoYield=STRARR(3,1)
         FOR j=0,RowsNUM-1 DO BEGIN
            IF CountyCodeTR[0,i] EQ TempYield[0,j] THEN BEGIN
               MeteoYield[0,0]=CountyCodeTR[0,i]                ;�ش���
               MeteoYield[1,0]=CountyCodeTR[1,i]                ;���Ʋ���
               MeteoYield[2,0]=TempYield[1,j]                   ;��������
               SimuYield=[[SimuYield],[MeteoYield]]
               BREAK
            ENDIF
         ENDFOR
     ENDFOR          ;��������Ŀ����Ϊȥ��û�и��������,��Щ��֮�������˲���,����Ϊ��ֵ��ԭ��
    SimuYield=SimuYield[0:*,1:*]     ;�������һ��Ϊ�ش���,�ڶ���Ϊ���Ʋ���,������Ϊ��ֵͳ�ƵĲ�������.
    TempYield=FLOAT(SimuYield[1,0:*])+FLOAT(SimuYield[2,0:*]) ;���������������Ʋ���������,�ظ���TempYieldΪ���ڴ�
    TempYield=STRTRIM(TEMPORARY(TempYield),2)
    SimuYield=[SimuYield,[TempYield]]     ;4��ֵ
      ;------------------------------------------------------------
    n=N_ELEMENTS(SimuYield)/4
    IF (*state).DataType THEN DataType = 'ң������' ELSE DataType = '��������'
    SimuYield = [SimuYield,STRARR(1,n)+DataType]
    CropYear={CropID:crop_id,Year:year}
    Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=2
    Widget_Control,(*state).StaYield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=n,SET_VALUE=SimuYield $
              ,COLUMN_LABELS=['�ش���','���Ʋ���','��������','ģ�����','��������'],ALIGNMENT=2 $
              ,ROW_LABELS=REPLICATE(year,n)+'��',SET_UVALUE=CropYear
    Widget_Control,(*state).StaYield_LABEL,SET_VALUE='��ֵ���ͳ�Ʋ���(ͳ�ƹ�'+STRTRIM(n,2)+'����)'

     progressTimer->UPDATE, (1 * 100.0)  ;���½�����
;;     progressTimer->DESTROY ;���ٽ�����
      OBJ_DESTROY,progressTimer

   Prompt=DIALOG_MESSAGE('ͳ�����!',TITLE='��ʾ',/INFORMATION)

END
;**********************���������뵽���ݿ���*******************************************
PRO DC_InputDB_EV,Event
     Widget_Control,Event.top ,GET_UVALUE=state
     Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield $
                   ,GET_UVALUE=CropYear                       ;�ñ�����������ID�����
      Widget_control,/HOURGLASS

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('û�в�������,�����Ƚ��в�ֵ��ͳ��!',TITLE='��ʾ',/INFORMATION)
        RETURN
     ENDIF

  	 CountyYield = STRTRIM(TEMPORARY(CountyYield),2)
     crop_id=CropYear.cropid    &   CalYear=CropYear.year

      CountyNum=N_ELEMENTS(CountyYield)/5

;========�������޸ģ�20070907===================================================
;      IF (*state).Datatype  THEN BEGIN
;      	  model_type_id='4'
;      	  modelData_id = '4'
;      ENDIF ELSE BEGIN
;      	  model_type_id='2'
;      	  modelData_id = '3'
;      ENDELSE
;
;	  COMMON COMMON_BLOCK
;
;      Sqlstr1="delete from COUNTY_ESTIMATED_YIELD where crop_id='"+ crop_id+ $
;             "' and Year="+CalYear+' and model_type_id='+model_type_id $
;             +' and LEFT(county_code,2)='+(*state).ProID
;      Sqlstr2="delete from COUNTY_Crop_MeteoOrFluctuate_yield where crop_id='"+ crop_id+ $
;             "' and Year="+CalYear+' and modelData_id='+modelData_id $
;             +' and LEFT(county_code,2)='+(*state).ProID
;
;      DBobj->ExecuteSQL,Sqlstr1
;	  DBobj->ExecuteSQL,Sqlstr2
;
;	 FOR i=0,CountyNum-1 DO BEGIN     ;����ֵ��ͳ�Ƶĸ��ع���Ͳ����������.
;	    Sqlstr1="insert into COUNTY_ESTIMATED_YIELD values('"+crop_id+"','"+ $
;	            CountyYield[0,i]+"',"+STRTRIM(CountyYield[3,i],2)+','+model_type_id+ $
;	            ','+CalYear+')'
;	    Sqlstr2="insert into COUNTY_Crop_MeteoOrFluctuate_yield values('"+CountyYield[0,i]+"','"+ $
;	            crop_id+"',"+STRTRIM(CountyYield[2,i],2)+','+CalYear+ $
;	            ','+modelData_id+')'
;
;	     DBobj->ExecuteSQL,Sqlstr1
;	     DBobj->ExecuteSQL,Sqlstr2
;	 ENDFOR
;===����Ϊԭ���룬����Ϊ�������޸ģ�20070907=======================================================================
;	 IF (*state).Datatype  THEN BEGIN
;      	  model_type_id='4'
;      	  modelData_id = '4'
;      ENDIF ELSE BEGIN
;      	  model_type_id='2'
;      	  modelData_id = '3'
;      ENDELSE

		IF (*state).DataType THEN begin
			if (*state).YieldType then begin
				ModelData_id = '4'
				model_type_id= '4'
			endif else begin
				ModelData_id = '2'
				model_type_id= '3'
			endelse
		endif else begin
;			if (*state).YieldType then ModelData_id = '3' else ModelData_id = '1'
			if (*state).YieldType then begin
				ModelData_id = '3'
				model_type_id= '2'
			endif else begin
				ModelData_id = '1'
				model_type_id= '1'
			endelse
		endelse

		IF (*state).YieldType THEN table = 'AGROSTATION_' ELSE table = 'COUNTY_'

	  COMMON COMMON_BLOCK

      Sqlstr1="delete from COUNTY_ESTIMATED_YIELD where crop_id='"+ crop_id+ $
             "' and Year="+CalYear+' and model_type_id='+model_type_id $
             +' and LEFT(county_code,2)='+(*state).ProID
      Sqlstr2="delete from "+table+"Crop_MeteoOrFluctuate_yield where crop_id='"+ crop_id+ $
             "' and Year="+CalYear+' and modelData_id='+modelData_id $
             +' and LEFT(county_code,2)='+(*state).ProID

      DBobj->ExecuteSQL,Sqlstr1
	  DBobj->ExecuteSQL,Sqlstr2

	 FOR i=0,CountyNum-1 DO BEGIN     ;����ֵ��ͳ�Ƶĸ��ع���Ͳ����������.
	    Sqlstr1="insert into COUNTY_ESTIMATED_YIELD values('"+crop_id+"','"+ $
	            CountyYield[0,i]+"',"+STRTRIM(CountyYield[3,i],2)+','+model_type_id+ $
	            ','+CalYear+')'
	    Sqlstr2="insert into "+table+"Crop_MeteoOrFluctuate_yield values('"+CountyYield[0,i]+"','"+ $
	            crop_id+"',"+STRTRIM(CountyYield[2,i],2)+','+CalYear+ $
	            ','+modelData_id+')'

	     DBobj->ExecuteSQL,Sqlstr1
	     DBobj->ExecuteSQL,Sqlstr2
	 ENDFOR
;==========================================================================

	;------------��Ȩ��ʡ-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,������CROP_AREA_COUNTY
	;ע������ȡ�������ʱ,�����ǰ��ݵ��������û��,���ÿ���������Newestyear���������

	INFO = DIALOG_MESSAGE('����Ҫ���ع��������Ȩ��ʡ��?',/QUESTION,TITLE='ѯ��')
	IF INFO EQ 'No' THEN RETURN
	CountyYield = [CountyYield[0,*],CountyYield[3,*]]		;2��ֵ,����\�������
	Status = DC_WeightToPro(CountyYield,Crop_id,CalYear,(*state).ProID,model_type_id)
	IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('��Ȩ����ɹ�!',/INFORMATION,TITLE='��ʾ')

END
;*************************************************************************
PRO DC_SpatialExtrapolate_event,event
   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
   wWidget =  Event.top

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:' $
                                   ,[!ERROR_STATE.MSG]],TITLE='����',/ERROR,DIALOG_PARENT=Event.top)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

   Widget_control,/HOURGLASS
   Widget_Control,wWidget,GET_UVALUE=state
  CASE wTarget OF
   Widget_Info(wWidget, FIND_BY_UNAME='Crop_DROPLIST'): BEGIN
	 	DC_CLEARUP_DATA,event.top
         (*state).Cropindex=Event.index

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
           RETURN
         ENDIF
       DC_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Pro_DROPLIST'): BEGIN
		DC_CLEARUP_DATA,event.top
         (*state).ProID = (*state).ProIDList[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
           RETURN
         ENDIF
       DC_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Year_DROPLIST'): BEGIN
        DC_CLEARUP_DATA,event.top
		Widget_Control,EVENT.ID,GET_VALUE= Datayear
        (*state).CalcYear=Datayear[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
           RETURN
         ENDIF
       DC_TakeSimpleDotData,event.top
   END
   ;------------���пռ��ֵ-----------------------------
   Widget_Info(wWidget, FIND_BY_UNAME='Next_Extrapolate_bu'): BEGIN   ;����"�ռ��ֵ"��ť�¼�

      Widget_Control,(*state).FloatYield_Table,GET_VALUE=FloatYield
      CropList =(*state).CropName
		IF (*state).Cropindex EQ 0 THEN BEGIN
		    Prompt=DIALOG_MESSAGE('����û��ѡ������!',TITLE='��ʾ',/INFORMATION)
		    RETURN
		ENDIF

		IF FloatYield[0,0] EQ '' THEN BEGIN       ;�õ�һ�������ж�.
		   Prompt=DIALOG_MESSAGE('û��"'+CropList[(*state).Cropindex]+'"�ز�������,���Ƚ���ģ��!',TITLE='��ʾ',/INFORMATION)
		ENDIF ELSE BEGIN
	            ;��������Ӣ������,ע��Ҫ��Ӧ����DROPLIST
			CropName=['','SpringWheat','WinterWheat' ,'EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			crop_id=CropName[(*state).Cropindex]    &   year=(*state).CalcYear

			SAVE_FILE=DIALOG_PICKFILE(TITLE='�����ֵͼ(��ò�Ҫ�Ķ��Զ����õ��ļ���)��',FILE=year+crop_id  $
			           ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)
			IF SAVE_FILE EQ '' THEN RETURN

			(*state).Savefile = SAVE_FILE   ;ע���ļ�����û�д���չ����

			progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='���ݴ�����,���Ժ�!',TITLE='�ռ��ֵ',/CANCEL) ;�½�����������
			progressTimer->START
			progressTimer->UPDATE, (0.1 * 100.0)  ;���½�����

			;----���ɲ�ֵӰ���ļ�-------------------------------
	        ProviceName = (*state).province[WHERE((*state).proIDlist EQ (*state).proID)]
			ParaInfo = (*state).ProjectPara    ;�õ�ʡ��Χ�ڵ�ͶӰ����
	        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;�����صĲ��ǽṹ��,���ǿ�.
	        	OBJ_DESTROY,progressTimer
	        	PRMPT = DIALOG_MESSAGE('û��'+ProviceName+'�Ļ���������Ϣ,��鿴��Ӧ�Ĳ��������ļ�!',TITLE='����')
	        	RETURN
	        ENDIF

			progressTimer->UPDATE, (0.5 * 100.0)  ;���½�����

	        samples    = ParaInfo.samples		  &        lines = ParaInfo.lines
	        ULX        = ParaInfo.UlX			  &        ULY   = ParaInfo.UlY
	        Resolution = ParaInfo.resolution      & CenterMedian = ParaInfo.CenMeridian
			Lon_Lat = FLOAT(FloatYield[3:4,0:*])  & value = FLOAT(FloatYield[2,0:*])
			;�˴��Ĳ�ֵ����DC_CREATE_INTERP_GRID()���д��ڸĽ�
			if n_elements(value) lt 5 then begin
				Prompt=DIALOG_MESSAGE('���ݿ��м�¼��������5�����ܽ��в�ֵ!',TITLE='��ʾ',/INFORMATION)
				return
			endif
			InterpData = DC_CREATE_INTERP_GRID(Lon_Lat,value,ULX,ULY,Resolution,samples,lines,'KING')

			PaddyCrop=[3,4,5] ;��Ӧ��PaddyCrop=['EarlyRice','SemiRice','LateRice']
			IF WHERE(PaddyCrop EQ (*state).Cropindex) EQ -1 THEN BEGIN
				FarmlandFile='data_grid\farm_drought'                         ;�������ˮ��,���ú��ذٷֱ��ļ�.
			ENDIF ELSE BEGIN
				FarmlandFile='data_grid\farm_rice'
			ENDELSE

			FarmData = DC_Read_ENVIData(FarmlandFile,SUCCESSSTATUS = Status,DESCRIPTION='���ز�����ļ���')
			IF Status EQ 0 THEN RETURN

			ZeroValue=WHERE(FarmData EQ 0,COMPLEMENT=NoZero)         ;Ϊȡ��ʡ���ر߽������������
            (*state).AddMin = 300.0-MIN(InterpData[NoZero])
			InterpData[ZeroValue]=0                                     ;�����ر߽����ֵ��Ϊ0

			SavedImage = InterpData										;��ֵ��ȥ���Ǹ��غ󽫱����������

			InterpData[NoZero]   = InterpData[NoZero]+(*state).AddMin   ;�������ڵ�ֵ������300

	        WIDGET_CONTROL,(*state).Extrapolate_DRAW,SET_UVALUE = InterpData

			progressTimer->UPDATE, (0.75 * 100.0)  ;���½�����

	        DataType = SIZE(SavedImage,/TYPE) & ImageData = PTR_NEW(SavedImage,/NO_COPY)
			DC_SaveImageFile,SAVE_FILE,ImageData,samples,lines,DataType,'Unknown',ULX,ULY,Resolution,CenterMedian
	 		PTR_FREE,ImageData

			;-----------------------------------
			progressTimer->UPDATE, (0.95 * 100.0)  ;���½�����
;;SAVE_FILE='H:\temp\2006Maize'   ;������
			DC_Draw_image,SAVE_FILE,(*state).Extrapolate_DRAW,oView = oView,MINVALUE=299.99
			OBJ_DESTROY,(*state).oView
			(*state).oView = oView

			progressTimer->UPDATE, (1 * 100.0)  ;���½�����
;;			progressTimer->DESTROY ;���ٽ�����
			OBJ_DESTROY,progressTimer

			Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=1

		ENDELSE
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Return_DRAW'): BEGIN

        Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0
      Widget_Control,wWidget ,SET_UVALUE=state
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Return_yield'): BEGIN

        Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=1
      Widget_Control,wWidget ,SET_UVALUE=state
   END

   Widget_Info(wWidget, FIND_BY_UNAME='SaveDotYield_bu'): BEGIN

      Widget_Control,(*state).FloatYield_Table,GET_VALUE=Yield,GET_UVALUE=SimpleDotYield
      CropName=STRCOMPRESS((*state).CropName[(*state).Cropindex],/REMOVE_ALL)
	CASE 1 OF
     (*state).Cropindex EQ 0: BEGIN
        Prompt=DIALOG_MESSAGE('����û��ѡ������!',TITLE='��ʾ',/INFORMATION)
        RETURN
     END

     Yield[0,0] EQ '' :BEGIN
        Prompt=DIALOG_MESSAGE('����û�н���'+(*state).CalcYear+'��'+CropName+'�ز�������ģ��,����ģ��!',TITLE='����')
        RETURN
     END
	 ELSE:
	ENDCASE

      RowsNum=N_ELEMENTS(SimpleDotYield)/6
      Estimation_yield=STRARR(7,RowsNum+1)
      Estimation_yield[0:*,0]=['������','���','�ش���','����','��������','����','γ��']
      Estimation_yield[0,1:*]=REPLICATE(CropName,RowsNum)  & Estimation_yield[1:*,1:*]=SimpleDotYield

	TEMP = WHERE(Estimation_yield EQ '',Count)
    IF Count NE 0 THEN BEGIN    ;˵���пո�
        Estimation_yield[TEMP]='---'
    ENDIF

      Filename=DIALOG_PICKFILE(PATH=DC_PathSetting(),TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,GET_PATH=Savepath, DIALOG_PARENT=Event.id)

      IF Filename EQ '' THEN RETURN

	  temp = DC_PathSetting(WRITEPATH1=Savepath)       ;��·��д�뱣������

      OPENW,LUN,Filename,/GET_LUN,WIDTH=7*MAX(STRLEN(Estimation_yield))
      PRINTF,LUN,Estimation_yield
      FREE_LUN,LUN

      Prompt=DIALOG_MESSAGE('�������!',TITLE='��ʾ',/INFORMATION)
   END

  Widget_Info(wWidget, FIND_BY_UNAME='SaveStatice_bu'): BEGIN

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield,GET_UVALUE=CropYear

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('û�в�������,�����Ƚ��в�ֵ��ͳ��!',TITLE='��ʾ',/INFORMATION)
        RETURN
     ENDIF

      Widget_control,/HOURGLASS
      year=CropYear.year   &   crop_id=CropYear.cropid
      CropName=STRCOMPRESS((*state).Cropname[WHERE((*state).CropIdList EQ crop_id[0])],/REMOVE_ALL)

      RowsNum=N_ELEMENTS(CountyYield)/5
      Estimation_yield=STRARR(7,RowsNum+1)
      Estimation_yield[0:*,0]=['����','�ش���','���Ʋ���','��������','ģ�����','��������','���']
      Estimation_yield[0,1:*]=REPLICATE(CropName,RowsNum)  & Estimation_yield[6,1:*]=REPLICATE(year,RowsNum)
      Estimation_yield[1:5,1:*]=CountyYield

		BlankID = WHERE(Estimation_yield EQ '',Count)
	    IF Count NE 0 THEN BEGIN    ;˵���пո�
	        Estimation_yield[BlankID]='---'
	    ENDIF

      Filename=DIALOG_PICKFILE(TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)

      IF Filename EQ '' THEN RETURN

      OPENW,LUN,Filename,/GET_LUN,WIDTH=7*MAX(STRLEN(Estimation_yield))
      PRINTF,LUN,Estimation_yield
      FREE_LUN,LUN

      Prompt=DIALOG_MESSAGE('�������!',TITLE='��ʾ',/INFORMATION)
   END

     Widget_Info(wWidget, FIND_BY_UNAME='Extrapolate_DRAW'): BEGIN

        IF NOT OBJ_VALID((*state).oView) THEN RETURN   ;����ǿն����򷵻�.

		ParaInfo = (*state).ProjectPara    			;�õ�ʡ��Χ�ڵ�ͶӰ����
        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;�����صĲ��ǽṹ��,���ǿ�.
        	ProviceName = (*state).Province[WHERE((*state).proIDlist EQ (*state).proID)]
        	PRMPT = DIALOG_MESSAGE('û��'+ProviceName+'�Ļ���������Ϣ,��鿴��Ӧ�Ĳ��������ļ�!',TITLE='����')
        	RETURN
        ENDIF

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN

		    (*state).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = FIX(ParaInfo.samples)  & ySize = FIX(ParaInfo.lines) ;Ӱ����
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return

            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;֮���԰��з�ת,����ΪDRAW��������Ǵ����½ǿ�ʼ��.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(dataValue-(*state).AddMin,2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

     Widget_Info(wWidget, FIND_BY_UNAME='FactorType'): BEGIN
     	IF NOT EVENT.SELECT THEN RETURN
		WIDGET_CONTROL,EVENT.ID,GET_VALUE=DataType
		(*state).DataType = DataType
		IF (*state).Cropindex EQ 0 THEN RETURN
		DC_TakeSimpleDotData,event.top
    END
  ;*******��������ӣ�20070825********************************************
	Widget_Info(wWidget, FIND_BY_UNAME='YieldType'): BEGIN
     	IF NOT EVENT.SELECT THEN RETURN
		WIDGET_CONTROL,EVENT.ID,GET_VALUE=YieldType
		(*state).YieldType = YieldType
		IF (*state).Cropindex EQ 0 THEN RETURN
		DC_TakeSimpleDotData,event.top
    END
    ;*************************************************************

   Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '��������������', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;   ONLINE_HELP,BOOK='HELP\HELP.chm',/FULL_PATH,'������������'
   Widget_Info(wWidget, FIND_BY_UNAME='quit_bu'):begin
   	common_log,'�رտռ����Ʋ�ֵ'
   	widget_control,Event.top,/DESTROY
   end
  ELSE:
  ENDCASE

END
;************�ռ����Ʋ�ֵ����*******************************
PRO DC_SpatialExtrapolate,GROUP_LEADER=groupleader

	common_log,'�����ռ����Ʋ�ֵ'

   IF ( XREGISTERED('DC_SpatialExtrapolate') NE 0 ) THEN RETURN

  TLB = Widget_Base( GROUP_LEADER=groupleader, UNAME='TLB' ,XOFFSET=300 ,YOFFSET=200  $
      ,SCR_XSIZE=506 ,SCR_YSIZE=405 ,TITLE='�����ռ��ֵ������ͳ��',SPACE=1  $
      ,XPAD=0 ,YPAD=0,COLUMN=1 ,TLB_FRAME_ATTR=1,/TAB_MODE)
;-----------------------------------------------------------------------------------
  Widegt_TAB = Widget_Tab(TLB,UNAME='Widegt_TAB' ,/FRAME,SCR_XSIZE=490 ,SCR_YSIZE=341)
;----------****************-վ�㲨������------------------------------
   tab0 = Widget_Base(Widegt_TAB, UNAME='tab0'  $
		      ,SCR_YSIZE=312 ,TITLE='��������' ,SPACE=1  $
		      ,XPAD=0 ,YPAD=0,/COLUMN,/BASE_ALIGN_LEFT)
		 ;--------------------------------------------
		 Crop_year = Widget_Base(tab0, UNAME='Crop_year' ,FRAME=1,SCR_XSIZE=490 $
		  		 ,SCR_YSIZE=25 ,SPACE=1,XPAD=0 ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)
			Province = ['����','���','�ӱ�','ɽ��','���ɹ�','����','����','������','�Ϻ�','����' $
						,'�㽭','��΢','����','����','ɽ��','����','����','����','�㶫','����','����' $
						,'����','�Ĵ�','����','����','����','����','����','�ຣ','����','�½�']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			CropName = ['δѡ��','��С��','��С��','��  ��','��  ��','��  ��','������','������','��  ��']
			CropIDList = ['','11','12','21','22','23','31','32','41']				;CropName��CropIDListӦ��Ӧ
			ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;��������ݱ仯,ϵͳ���б��е����Ҳ��仯.

;			Pro_Droplist  = Widget_Droplist(Crop_year,UNAME='Pro_DROPLIST',TITLE='ʡ��:')
			Year_Droplist = Widget_Droplist(Crop_year,UNAME='Year_DROPLIST',TITLE='���:')

			SeperateLine1 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)

			;**********��������ӣ�20070825********************************
			YieldType = CW_BGROUP(Crop_year, UNAME='YieldType',['��','վ��'],/ROW,/EXCLUSIVE $
					,SPACE=0, LABEL_LEFT='����:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=18);,XSIZE=145)

			SeperateLine1 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)
			;********************************************************************

			FactorType = CW_BGROUP(Crop_year, UNAME='FactorType',['����','ң��'],/ROW,/EXCLUSIVE $
					,SPACE=0, LABEL_LEFT='����:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=18);,XSIZE=145)


			SeperateLine2 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)
			Crop_Droplist = Widget_Droplist(Crop_year,UNAME='Crop_DROPLIST',TITLE='����:')
		;----------------------------��----------------
		 FloatYield_Table = Widget_Table(tab0, UNAME='FloatYield_Table'$
		 			,SCR_XSIZE=490,SCR_YSIZE=250 ,XSIZE=5 ,YSIZE=20,/FRAME,ROW_LABELS='')
		;----------------------------------------------------------------------------------
		 Save_Extrapo_base = Widget_Base(tab0, UNAME='Save_Extrapo_base',SCR_XSIZE=490,/FRAME $
		     , SCR_YSIZE=31,SPACE=40 ,XPAD=47 ,YPAD=2 ,ROW=1,/BASE_ALIGN_TOP)
 		   Width_W = 100
			 SaveDotYield_bu = Widget_Button(Save_Extrapo_base, UNAME='SaveDotYield_bu'  $
			      ,SCR_XSIZE=Width_W,SCR_YSIZE=20 ,TOOLTIP='��վ�㲨���������浽���ش�����' ,VALUE='����')

			 Next_Extrapolate_bu = Widget_Button(Save_Extrapo_base, UNAME='Next_Extrapolate_bu'  $
			      ,SCR_XSIZE=Width_W,SCR_YSIZE=20 ,TOOLTIP='��һ�����пռ��ֵ' ,VALUE='��ֵ>>')

			 Help_bu = Widget_Button(Save_Extrapo_base, UNAME='Help_bu'  $
			      ,SCR_XSIZE=Width_W,SCR_YSIZE=20,VALUE='����')

 ;------*****************-�ռ����Ʋ�ֵ--------------------------------------
  tab1 = Widget_Base(Widegt_TAB, UNAME='tab1' ,FRAME=1  $
      ,SCR_XSIZE=490 ,SCR_YSIZE=312 ,TITLE='�ռ����Ʋ�ֵ' ,SPACE=0  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER)

		  Extrapolate_DRAW = Widget_Draw(tab1, UNAME='Extrapolate_DRAW' ,RETAIN=2  $
		     ,SCR_XSIZE=480 ,SCR_YSIZE=275 ,GRAPHICS_LEVEL=2 $
		     ,/BUTTON_EVENTS,/MOTION_EVENTS)       ;������һ���û�ֵ,��Draw��û��Ӱ��ʱ,�ƶ�ָ�벻�ᱨ��.

;;		 Cusor_LABEL = Widget_Label(tab1,UNAME='Cusor_LABEL' $
;;		      ,SCR_XSIZE=266 ,SCR_YSIZE=12 ,/ALIGN_CENTER,VALUE='ָ����ָ��������ֵ��')

		  Button_BASE = Widget_Base(tab1, UNAME='Button_BASE'   $
		      ,SCR_XSIZE=480 ,SCR_YSIZE=31,SPACE=70 ,XPAD=80 ,YPAD=0  $
		      ,/ROW,FRAME=0,/BASE_ALIGN_TOP)

		  Return_DRAW = Widget_Button(Button_BASE, UNAME='Return_DRAW'  $
		      ,SCR_XSIZE=119 ,SCR_YSIZE=20,/ALIGN_CENTER ,VALUE='<<����')

		  Next_StaResult_bu = Widget_Button(Button_BASE,  $
		      UNAME='Next_StaResult_bu' ,SENSITIVE=1,SCR_XSIZE=119 ,SCR_YSIZE=20 $
		     ,/ALIGN_CENTER ,VALUE='ͳ��>>',EVENT_PRO='DC_StatisticEV' $
		     ,TOOLTIP='���ݲ�ֵ���ͳ�Ƹ��ز�������')
;-----*******************-����ͳ��*************-----------------
  tab2 = Widget_Base(Widegt_TAB, UNAME='tab2' $
      ,SCR_XSIZE=490 ,SCR_YSIZE=312 ,TITLE='����ͳ�ƽ��' ,SPACE=1  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_LEFT,/FRAME)

		  StaYield_LABEL = Widget_Label(tab2, UNAME='StaYield_LABEL'  $
		      ,XOFFSET=46 ,YOFFSET=3 ,SCR_XSIZE=386 ,SCR_YSIZE=15  $
		      ,/ALIGN_CENTER ,VALUE='��ֵ��ͳ�Ʋ���')

		  StaYield_TABLE = Widget_Table(tab2, UNAME='StaYield_TABLE'  $
		      ,SCR_XSIZE=488 ,SCR_YSIZE=253 ,XSIZE=6  $
		      ,YSIZE=20,/FRAME)
		  ;----------------------------
		  Button_BASE_0 = Widget_Base(tab2, UNAME='Button_BASE_0'  $
		      ,XOFFSET=3 ,YOFFSET=273 ,SCR_XSIZE=472 ,SCR_YSIZE=29  $
		      ,SPACE=40 ,XPAD=50 ,YPAD=2 ,ROW=1,/BASE_ALIGN_TOP)
		   BuWidth=100  &  BuHeight=20
		  Return_yield = Widget_Button(Button_BASE_0, UNAME='Return_yield'  $
		      ,XOFFSET=50 ,YOFFSET=4 ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight  $
		      ,/ALIGN_CENTER ,VALUE='<<����',TOOLTIP='���ز鿴�ռ��ֵͼ')

		  InputDB_yield = Widget_Button(Button_BASE_0, UNAME='InputDB_yield'  $
		      ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight,/ALIGN_CENTER ,VALUE='���' $
		      ,TOOLTIP='������Ĳ������浽���ݿ���',EVENT_PRO='DC_InputDB_EV')

		  SaveStatice_bu = Widget_Button(Button_BASE_0, UNAME='SaveStatice_bu' ,XOFFSET=239  $
		      ,YOFFSET=4 ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight ,/ALIGN_CENTER  $
		      ,VALUE='����',TOOLTIP='������Ĳ������浽���ش�����')

  ;-----**************�������������ʾ�ı�************-------------
  Prompt_BASE = Widget_Base(TLB,SCR_XSIZE=450 ,SCR_YSIZE=35 ,SPACE=3,XPAD=1  $
      ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  prompt_TEXT = Widget_Text(Prompt_BASE, UNAME='prompt_TEXT' ,FRAME=1  $
	      ,SCR_XSIZE=430 ,SCR_YSIZE=25 ,XSIZE=20  $
	      ,YSIZE=1,VALUE='��ʾ��')
	  quit_bu = Widget_Button(Prompt_BASE, UNAME='quit_bu' ,XOFFSET=348  $
	      ,YOFFSET=5 ,SCR_XSIZE=55 ,SCR_YSIZE=23  $
	      ,TOOLTIP='�˳��ռ��ֵģ��' ,VALUE='�ر�')
;---------------------------------------------------------------------------


		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
		PRO_COMMON_DC
		COMMON DC_BLOCK,NewCropID
		ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�
		CropID = NewCropID
		CalcYear = strmid(systime(),3,4,/REVERSE_OFFSET)

;		WIDGET_CONTROL,Pro_Droplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
		WIDGET_CONTROL,Crop_Droplist,SET_VALUE =CropName;,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)
		WIDGET_CONTROL,Year_Droplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)

		Widget_Control,FactorType,SET_VALUE=0
		Widget_Control,FloatYield_Table ,SET_TABLE_SELECT=[-1,-1,-1,-1]
		Widget_Control,StaYield_TABLE    ,SET_TABLE_SELECT=[-1,-1,-1,-1]

 		Widget_Control, /REALIZE, TLB
		Widget_Control,quit_bu,/INPUT_FOCUS

		Widget_Control,Extrapolate_DRAW,GET_VALUE=Owindow
		Owindow->ERASE,COLOR=255

		ProjectPara = DC_ReadParameter(Province[WHERE(ProIDList EQ ProCode)])    ;�õ�ʡ��Χ�ڵ�ͶӰ����,û���򷵻�Ϊ��,�����ǽṹ��.
		state={  Cropindex             :  0 				,$
			     CropIDList			   :  CropIDList		,$	 ;����ID�б�
			     CropName			   :  CropName			,$   ;�������б�
			     DataType			   :  0					,$   ;��������,Ĭ��Ϊ��������(0)����Ĳ���������(1)Ϊң������
			     YieldType			   :  0					,$	 ;��ʶ����(0)����վ�����(1)   ;���������
			     CalcYear              :  CalcYear	 		,$
       		     ProID				   :  ProCode			,$	  ;��ѡʡID,2λ�ַ���
			     ProIDList			   :  ProIDList			,$	  ;ʡID�б�
			     Province			   :  Province			,$    ;ʡ���б�
			     FloatYield_Table  	   :  FloatYield_Table  ,$
			     StaYield_TABLE        :  StaYield_TABLE  	,$
			     StaYield_LABEL        :  StaYield_LABEL 	,$
			     prompt_TEXT           :  prompt_TEXT 		,$
			     Widegt_TAB			   :  Widegt_TAB		,$
			     Extrapolate_DRAW      :  Extrapolate_DRAW  ,$
			     oView				   :  OBJ_NEW()			,$		;��ͼ����
			     AddMin				   :  0.0				,$      ;�������ڵĲ�ֵ�������ʶ��Ĳ���
			     ProjectPara		   :  ProjectPara		,$		;���ڶ���ʡ��Ӧ��Χ�ڵ�ͶӰ����ֵ(��"text\parametersetting.txt")
			     Savefile              :  ''}

	Widget_Control,TLB ,SET_UVALUE=PTR_NEW(state,/NO_COPY)

	XManager, 'DC_SpatialExtrapolate', TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK


END