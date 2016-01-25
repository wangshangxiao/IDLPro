;ɸѡ������������

;******�Զ������:���ԭ��������*********************************
 PRO DC_ClearOldData,EventTop
   Widget_Control,EventTop,GET_UVALUE = SelectFactor

     Widget_Control,(*SelectFactor).AgorFactor_TABLE,GET_VALUE=MetoYield & MetoYield[*]=''
     Widget_Control,(*SelectFactor).AgorFactor_TABLE,SET_VALUE=MetoYield  $
                    ,COLUMN_LABELS=[''],ROW_LABELS=[''],BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

     Widget_Control,(*SelectFactor).factor_TABLE,GET_VALUE=Factor & Factor[*]=''
     Widget_Control,(*SelectFactor).factor_TABLE,SET_VALUE=Factor $
                   ,COLUMN_LABELS=[''],BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

     Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='����ɸѡ��'
   Widget_Control,EventTop,SET_UVALUE = SelectFactor
 END
;***********************"ɸѡ����"��ť�¼�*************************************
PRO DC_FilterFactor,EVENT

		 CATCH, Error_status
		IF Error_status NE 0 THEN BEGIN
		   infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
		   CATCH, /CANCEL
		   RETURN                    ;������������,������ִ����������,�Ի���ִ���.
		ENDIF
	WIDGET_CONTROL, /HOURGLASS                             ;ִ�и��¼�ʱָ�봦�ڵȴ�״̬,ֱ�������ָ�.

	DC_ClearOldData,Event.top
	Widget_Control,Event.top,GET_UVALUE = SelectFactor
	Widget_Control,(*SelectFactor).threshold_TEXT,GET_VALUE =Threshold

	ThresholdValue = DC_JudgeInputChar(Threshold[0],Desc='��ֵ�ı���')
	IF ThresholdValue EQ -1 THEN RETURN

	AllYearNum=N_ELEMENTS((*SelectFactor).ARRAY_YEAR)

    Datayear=(*SelectFactor).ARRAY_YEAR		;���൱�����list
    GrowthMonth=STRTRIM(INDGEN(12)+1,2)     ;���൱���·�List

    StartYear  = Datayear[(*SelectFactor).StartYearID]     & EndYear = Datayear[(*SelectFactor).EndYearID]
    StartMonth = GrowthMonth[(*SelectFactor).StartMonthID] & EndMonth= GrowthMonth[(*SelectFactor).EndMonthID];���ĸ�����ָ��Ӧ�Ŀ�ʼ��������ݺ������·�,�ַ���
	RowsNum = FIX(EndYear)-FIX(StartYear)+1

	YieldType = (*SelectFactor).YieldType
    ;�����IF�жϷ���������.
	IF (*SelectFactor).FactorType EQ 0 THEN BEGIN    ;�������������
		Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex
		IF ARRAY_EQUAL(MeteoTableIndex,0,/NO_TYPECONV) EQ 1 THEN BEGIN         ;���û��ѡ������,����ʾ.
		   Prompt=DIALOG_MESSAGE('������ѡ��һ������',TITLE='��ʾ',/INFORMATION)
		   RETURN
		ENDIF

	    IF (*SelectFactor).YieldType EQ 0 THEN BEGIN    ;�ж����ػ���վ�����
	        Sqlstr='select weatherstation_code from tainsen_polygen where county_code='+"'"+(*SelectFactor).CountyCode+"'"
			station_code=DC_GetdataFromDB_Str(1,Sqlstr,N_RECORDS = NumReocrd,Num_BUFFERS = 500)
			IF NumReocrd EQ 0 THEN BEGIN
		      Info=DIALOG_MESSAGE("û����'"+(*SelectFactor).CountyCode+"'������,��鿴���ݿ��е�̩ɭ����α�!",TITLE='����')
		      RETURN
			ENDIF
		ENDIF ELSE   station_code=(*SelectFactor).CountyCode

	    Widget_Control,(*SelectFactor).MeteorologyFactor,SET_UVALUE =station_code   ;��"��һ��"��ť��

	    IF ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
	        SingleTendayNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3             ;��Ѯ�ĸ���,���ֲ�����������
	        AllData=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,EndMonth,MeteoTableIndex,station_code,YieldType)
	        IF ARRAY_EQUAL(AllData,'') THEN RETURN   ;�µ��Ե�**************************
	        AllData = DC_ProcessBlank(AllData,RowsNum,BlankId = BlankID)
	        CombinationData= DC_FactorCombination(AllData,MeteoTableIndex,RowsNum,StartMonth,EndMonth)
	        AllData=[TEMPORARY(AllData),[CombinationData]]                                         ;�������յĲ�������.
	        SingleTendayFactorName=DC_SingleTendayName(StartMonth,EndMonth)
	        AllFactorName = DC_CombinationTendayName(SingleTendayFactorName,MeteoTableIndex,SingleTendayNum) ;�������յ�����������.��ͬ.
	    ENDIF ELSE BEGIN                                                               ;ע�ⶬС���ǿ����,������ʼ�ͽ����´�С����ֱ����������������.
	        SingleTendayNum = (13+FIX(EndMonth)-FIX(StartMonth))*3
	        AllData1=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,'12',MeteoTableIndex,station_code,YieldType)
	        AllData2=DC_SingleTendayFactor(Fix(StartYear)+1,Fix(EndYear)+1,'1',EndMonth,MeteoTableIndex,station_code,YieldType)
	        IF ARRAY_EQUAL(AllData1,'') OR ARRAY_EQUAL(AllData2,'') THEN RETURN   ;�µ��Ե�**************************
	        AllData=[AllData1,[AllData2]]
	        AllData = DC_ProcessBlank(AllData,RowsNum,BlankId = BlankID)
	        CombinationData= DC_FactorCombination(AllData,MeteoTableIndex,RowsNum,1,13+FIX(EndMonth)-FIX(StartMonth))   ;�������������,������ʼ�ºͽ�����,����Ϊ��
	        AllData=[TEMPORARY(AllData),[CombinationData]]                                                                      ;���庯������������ֻ����õ�����,���Կ�������������.
	        SingleTendayFactorName=[DC_SingleTendayName(StartMonth,12),[DC_SingleTendayName(1,EndMonth)]]
	        AllFactorName = DC_CombinationTendayName(SingleTendayFactorName,MeteoTableIndex,SingleTendayNum)
	    ENDELSE

		ZeroNum = N_ELEMENTS(BlankID)/2
		SelectedFactorIndex = WHERE(MeteoTableIndex EQ 1,FactorNum)

		CASE 1 OF
			(ZeroNum EQ (SingleTendayNum*FactorNum*RowsNum)):BEGIN
			  Prompt=DIALOG_MESSAGE('���ݿ���û��Ŀ���������������!',TITLE='����')
			  RETURN
			END

			(ZeroNum GT (3*(SingleTendayNum-1)*FactorNum)*RowsNum/3.0):BEGIN      ;����ȱʧ����֮һ,����ʾ
			  Prompt=DIALOG_MESSAGE('��Ѯ��������ȱʧ̫��,�Ƿ�����ѡ���ػ��߲�ͬʱ������ݽ���ɸѡ!',TITLE='ѯ��',/QUESTION)
			  IF Prompt EQ 'Yes' THEN RETURN
			END
		  ELSE:
		ENDCASE

	    YieldStartYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ StartYear)    ;�������������������ʼ�����Ӧ������
	    YieldEndtYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ EndYear)

	    ActualYield = (*SelectFactor).YieldData[*,0]
	    TrendYield  = (*SelectFactor).YieldData[*,1]
		ZeroId = WHERE(ActualYield EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
			ActualYield[ZeroId] = MEAN(ActualYield[NotZeroID])  ;�Ծ�ֵ������հ���Ĳ���ֵ
		ENDIF

		FloatYield = ActualYield-TrendYield

		RegressMeteoYield=FloatYield[YieldStartYearID:YieldEndtYearID]  ;�����Ӧ��ݲ�������,һ��

		Regress__FactorName = {AllFactorName:AllFactorName,RegressYield:RegressMeteoYield,EndYear:EndYear}
		Widget_Control,(*SelectFactor).AgorFactor_TABLE,TABLE_XSIZE=N_ELEMENTS(AllFactorName)  $
		             ,TABLE_YSIZE=RowsNum,SET_VALUE=Alldata,SET_UVALUE=Regress__FactorName    $         ; ע����û�ֵ������.��"��һ��"���õ�
		             ,COLUMN_LABELS =AllFactorName ,ROW_LABELS =STRTRIM(INDGEN(RowsNum)+FIX(StartYear),2) $
		             ,ALIGNMENT=2,COLUMN_WIDTHS=120

		WIDGET_CONTROL,(*SelectFactor).AgorFactor_TABLE,USE_TABLE_SELECT=BlankID $
					,BACKGROUND_COLOR  = [255,255,0],SET_TABLE_VIEW = BlankID[*,0]   ;��ʵ����ݲ���Ϊ0(����ƽ��ֵ���滻�����)�ĵ�Ԫ����ɻ�ɫ��ʾ.

		correla=['']   ;����֮������ѭ��,����Ϊ�ٶȿ�,����Ҳ������������ע�͵������"MeteoResult=....."һ������,����ʱ�ٶ�����,ԭ�����!
		FOR i=0,N_ELEMENTS(Alldata)/RowsNum-1 DO BEGIN
		   bb=REGRESS(FLOAT(Alldata[i,0:*]),FLOAT(RegressMeteoYield), CORRELATION=correlation)
		   correlaTemp=STRING(correlation)
		   correla=[correla,[correlaTemp]]
		ENDFOR
		correlation=correla[1:*]

		; MeteoResult=REGRESS(FLOAT(Alldata),FLOAT(RegressMeteoYield), CORRELATION=correlation);,STATUS=ynSuccess)                  ;�����еĲ������ӽ�����ط���,�Եõ����ϵ��
	    ;���������ֵ�ж�

	    SelectedFactorID=WHERE(ABS(correlation) GE ThresholdValue[0],Count)    ;֮�����þ���ֵ,������ؽϴ�ʱ,����Ҳ���������.

	    IF Count GT 0 THEN BEGIN
	        SelectedFactor=Intarr(2,Count) & SelectedFactor[0,0:*]=SelectedFactorID
	        WIDGET_CONTROL,(*SelectFactor).factor_TABLE,TABLE_XSIZE=N_ELEMENTS(AllFactorName) $
	                  ,COLUMN_LABELS =AllFactorName,SET_VALUE=correlation $
	                  ,COLUMN_WIDTHS=120,SET_TABLE_VIEW=SelectedFactor[*,0] $
					  ,SET_UVALUE=SelectedFactor              ;��ֵ��¼��ѡ�е����ӵ�������.
			WIDGET_CONTROL,(*SelectFactor).factor_TABLE,USE_TABLE_SELECT=SelectedFactor $
						,BACKGROUND_COLOR = [0,255,0],SET_TABLE_VIEW=SelectedFactor[*,0]     	 ;��ʵ����ݲ���Ϊ0(����ƽ��ֵ���滻�����)�ĵ�Ԫ�������ɫ��ʾ.

	        Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='����ɸѡ��'+'(��ǰɸѡ��'+STRTRIM(STRING(Count),2)+'����������)'
	      IF (Count GT RowsNum-1) THEN BEGIN
	           Prompt=DIALOG_MESSAGE(['��ѡ���Ӹ�������"��������ȥ1"���ֵ(������������ȥ1��','��ֵ),����һ��������ģ�ͻ�û�о���,��������������ֵ','����������ȡ��ͬʱ��������ٽ���ɸѡ!'],TITLE='��ʾ',/INFORMATION)
	      ENDIF
	    ENDIF ELSE BEGIN
	       Widget_Control,(*SelectFactor).factor_TABLE,TABLE_XSIZE=N_ELEMENTS(AllFactorName) $
	                  ,COLUMN_LABELS =AllFactorName,SET_VALUE=STRING(correlation) $
	                  ,COLUMN_WIDTHS=120,SET_UVALUE=[-1,-1]
	       Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='����ɸѡ��'
	       Prompt=DIALOG_MESSAGE(['��ǰû������"��ֵѡ������"�Ĳ�������,��������','����ֵ��ˢ�»���������ȡ��ͬʱ��������ٽ���ɸѡ!'],TITLE='��ʾ',/INFORMATION)
	    ENDELSE
   ;------------------------------
   ENDIF ELSE BEGIN    ;�����ң������
		Colnames = ['NPP��ֵ','NPP�ۼ�ֵ','LAI��ֵ','LAI�ۼ�ֵ','NDVI��ֵ','NDVI�ۼ�ֵ']
   		names=['NPP','NPP','LAI','LAI','NDVI','NDVI']
		Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex
		DataType = names[MeteoTableIndex]
		Sensor_type = (*SelectFactor).SensorType
		IsAvgOrSum = MeteoTableIndex MOD 2        ;IsAvgOrSumֻΪ0��1 ,0ʱȡƽ��,1ʱȡ�ۼ�.

		Sensor = ['AVHRR','MODIS','VGT'] & Sensor_code = ['1','2','3']
		Sensorname = Sensor[WHERE(Sensor_code EQ (*SelectFactor).SensorType)]

		CropIDList = (*SelectFactor).CropIDList				;Crop��CropIDListӦ��Ӧ
		CropCol = ['AVG_SPRING_WHEAT','AVG_WINTER_WHEAT','AVG_EARLY_RICE','AVG_SEMILATE_RICE' $
				  ,'AVG_LATE_RICE','AVG_SPRING_MAIZE','AVG_SUMMER_MAIZE','AVG_SOYBEAN']				;Crop��CropIDListӦ��Ӧ
		CropFiled = CropCol[WHERE(CropIDList EQ (*SelectFactor).CropId)]
		TableColnames = Colnames[MeteoTableIndex]

		Code = (*SelectFactor).CountyCode
		;�����õ��ĺ�����DC_GetRsData()��"DC_FloatYield"��
		Data = DC_GetRsData(StartYear,EndYear,StartMonth,EndMonth,Code,CropFiled,IsAvgOrSum,Sensor_type,DataType,YieldType)
		IF ARRAY_EQUAL(FLOAT(Data),0.0) THEN BEGIN
			INFO =DIALOG_MESSAGE("���ݿ���û�и�����'"+Sensorname+"'��'"+DataType+"'����!",TITLE='����')
			RETURN
		ENDIF

		Rsdata = FLOAT(TRANSPOSE(Data))
		ZeroId = WHERE(Rsdata EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
			Rsdata[ZeroId] = MEAN(Rsdata[NotZeroID])  ;�Ծ�ֵ������հ���Ĳ���ֵ
			ZeroCell = [INTARR(1,ZeroNum),TRANSPOSE(ZeroID)]
		ENDIF ELSE ZeroCell =[-1,-1]

	    YieldStartYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ StartYear)    ;��ò��������������ʼ�����Ӧ������
	    YieldEndtYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ EndYear)
	    ActualYield = (*SelectFactor).YieldData[*,0]
	    TrendYield  = (*SelectFactor).YieldData[*,1]
		ZeroId = WHERE(ActualYield EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
			ActualYield[ZeroId] = MEAN(ActualYield[NotZeroID])  ;�Ծ�ֵ������հ���Ĳ���ֵ
		ENDIF

		FloatYield = ActualYield-TrendYield

		RegressMeteoYield=FloatYield[YieldStartYearID:YieldEndtYearID]  ;�����Ӧ��ݲ�������,һ��
		Regress__FactorName = {AllFactorName:TableColnames,RegressYield:RegressMeteoYield,EndYear:EndYear}

		Widget_Control,(*SelectFactor).AgorFactor_TABLE,TABLE_XSIZE=1  $
		             ,TABLE_YSIZE=RowsNum,SET_VALUE=STRTRIM(Rsdata,2),SET_UVALUE=Regress__FactorName    $     ;ע����û�ֵ������.��"��һ��"���õ�
		             ,COLUMN_LABELS =[TableColnames] ,ROW_LABELS =STRTRIM(INDGEN(RowsNum)+FIX(StartYear),2) $
		             ,ALIGNMENT=2,COLUMN_WIDTHS=120

		WIDGET_CONTROL,(*SelectFactor).AgorFactor_TABLE,USE_TABLE_SELECT=ZeroCell $
					,BACKGROUND_COLOR  = [255,255,0],SET_TABLE_VIEW = ZeroCell[*,0]   ;��ʵ������Ϊ0(����ƽ��ֵ���滻�����)�ĵ�Ԫ����ɻ�ɫ��ʾ.

		bb = REGRESS(Rsdata,FLOAT(RegressMeteoYield), CORRELATION=correlation)
		SelectedFactor = [0,0]
		WIDGET_CONTROL,(*SelectFactor).factor_TABLE,TABLE_XSIZE=1 $
		          ,COLUMN_LABELS =[TableColnames],SET_VALUE=STRTRIM(correlation,2) $
				  ,SET_UVALUE=SelectedFactor             ;��ֵ��¼��ѡ�е����ӵ�������.��ʱֻ��һ��

   ENDELSE

END
;*************************"��һ��"��ť***********************************
PRO DC_SimulatResult,Event          	;��һ����Ҫ�Ǽ��������������������һ��������.

	CATCH, Error_status               ;��ȡ����.
	IF Error_status NE 0 THEN BEGIN
		infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
		CATCH, /CANCEL
		RETURN                    ;������������,������ִ����������,�Ի���ִ���.
	ENDIF

	Widget_Control,Event.top,GET_UVALUE = SelectFactor

	AllYearNum=N_ELEMENTS((*SelectFactor).ARRAY_YEAR)

	Datayear=(*SelectFactor).ARRAY_YEAR		;���൱�����list
	GrowthMonth=STRTRIM(INDGEN(12)+1,2)     ;���൱���·�List

	StartYear  = Datayear[(*SelectFactor).StartYearID]     & EndYear = Datayear[(*SelectFactor).EndYearID]
	StartMonth = GrowthMonth[(*SelectFactor).StartMonthID] & EndMonth= GrowthMonth[(*SelectFactor).EndMonthID];���ĸ�����ָ��Ӧ�Ŀ�ʼ��������ݺ������·�,�ַ���
	RowsNum = FIX(EndYear)-FIX(StartYear)+1

	YieldType = (*SelectFactor).YieldType

	Widget_Control,(*SelectFactor).factor_TABLE,GET_UVALUE=FactorID    ;�õ���ѡ�е�����ID��,ע����������,��һ��ֵΪҪ������ID
	Widget_Control,(*SelectFactor).AgorFactor_TABLE,GET_VALUE=Alldata,GET_UVALUE=Regress__FactorName

	CASE 1 OF
		ARRAY_EQUAL(Alldata,'',/NO_TYPECONV): BEGIN
		  Prompt=DIALOG_MESSAGE('����ɸѡ����!',TITLE='��ʾ',/INFORMATION)
		  RETURN
		END

		ARRAY_EQUAL(FactorID,[-1,-1],/NO_TYPECONV):BEGIN
		  Prompt=DIALOG_MESSAGE(['��ǰû������"��ֵѡ������"�Ĳ�������,��������','����ֵ��ˢ�»�'+ $
		                         '��������ȡ��ͬʱ��������ٽ���ɸѡ!'],TITLE='��ʾ',/INFORMATION)
		  RETURN
		END
	ELSE:
	ENDCASE


	IF (*SelectFactor).FactorType EQ 0 THEN BEGIN

		CurrentYear=(*SelectFactor).CalcYear

		IF N_ELEMENTS(FactorID[0,*]) GT RowsNum-1 THEN BEGIN
		   Prompt=DIALOG_MESSAGE(['��ѡ���Ӹ�������"��������ȥ1"���ֵ(������������ȥ1��','��ֵ),'+ $
		                         '����һ��������ģ�ͻ�û�о���,��������������ֵ','����������ȡ��ͬʱ��������ٽ���ɸѡ!'],TITLE='��ʾ',/INFORMATION)
		   RETURN
		ENDIF

		;������ȡ��ǰ��ݵĲ���������������
		 Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex,GET_UVALUE =station_code

		IF ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
		    SingleTendayNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3             ;��Ѯ�ĸ���,�������Ѯ������������
		    AllData0=DC_SingleTendayFactor(CurrentYear,CurrentYear,StartMonth,EndMonth,MeteoTableIndex,station_code,YieldType)
		    SingleTendayData = AllData0     ;��Ѯ����������������ж�
		    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,StartMonth,EndMonth)
		    AllData0=[AllData0,[CombinationData]]                             ;�������յĲ�������.
		ENDIF ELSE BEGIN                                                      ;ע�ⶬС���ǿ����,������ʼ�ͽ����´�С����ֱ����������������.
		    Lastyear=STRTRIM(FIX(CurrentYear)-1,2)                    ;���ڶ�С��,�����ڴ�����11�¿�ʼ��.������Lastyear
		    SingleTendayNum = (13+FIX(EndMonth)-FIX(StartMonth))*3
		    AllData1=DC_SingleTendayFactor(Lastyear,Lastyear,StartMonth,'12',MeteoTableIndex,station_code,YieldType)
		    AllData2=DC_SingleTendayFactor(Fix(Lastyear)+1,Fix(Lastyear)+1,'1',EndMonth,MeteoTableIndex,station_code,YieldType)
		    AllData0=[AllData1,[AllData2]]
		    SingleTendayData = AllData0     ;��Ѯ����������������ж�
		    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,1,13+FIX(EndMonth)-FIX(StartMonth))   ;�������������,������ʼ�ºͽ�����,����Ϊ��
		    AllData0=[AllData0,[CombinationData]]                                                               ;���庯������������ֻ����õ�����,���Կ�������������.
		ENDELSE

		IF ARRAY_EQUAL(SingleTendayData,'') THEN BEGIN   ;��һ�����жϻ��д��ڽ�һ������
		   Prompt=DIALOG_MESSAGE('���ݿ���û��'+STRTRIM(CurrentYear,2)+'��������������!',TITLE='����')
		   RETURN
		ENDIF

		AllFactorName=Regress__FactorName.AllFactorName  &  ActualMeteoYield=Regress__FactorName.RegressYield
		CurrentYearFactorData=[''] &  SelectedFactor_Name=['']  & SatisfactoryFactorData=STRARR(1,RowsNum)
		FactorID=TRANSPOSE(FactorID[0,0:*])   ;һ��Ҫ���ױ��ϵ�ѡ��ı�ʾ����
		FOR i=0,N_ELEMENTS(FactorID)-1 DO BEGIN
		   CurrentYearFactorData = [CurrentYearFactorData,[Alldata0[FactorID[i],0:*]]]      ;��ȡ���겨��������ģ�⵱��Ĳ�������
		   SelectedFactor_Name     = [SelectedFactor_Name,[AllFactorName[FactorID[i]]]]     ;���Բ���ѭ����ʽ,���� AllFactorName[FactorID,*] ����,������ͬ.
		   SatisfactoryFactorData = [SatisfactoryFactorData,[Alldata[FactorID[i],0:*]]]     ;�˱�����ѡ����ݵĶ���ĵĲ�������
		ENDFOR
		CurrentYearFactorData=FLOAT(CurrentYearFactorData[1:*])  ;ԭ���룬20070831
;		;=========�������޸ģ�20070831===========================
;		CurrentYearFactorData=double(CurrentYearFactorData[1:*])
;		;=========================================================
		SelectedFactor_Name=SelectedFactor_Name[1:*]                                       ;�õ���ѡ�е�������
		SatisfactoryFactorData=SatisfactoryFactorData[1:*,0:*]                           ;�õ�������ֵ���������Ӳ�������

		SimuMeteoResult=REGRESS(SatisfactoryFactorData,ActualMeteoYield,CONST=constant $
		                  ,MCORRELATION=M_correlation,FTEST=F_check,YFIT=SimuMeteoYield, STATUS=status) ;��������� STATUS,20070831
		if status ne 0 then begin
			INFO =DIALOG_MESSAGE('��ѡ���Ӳ��ܽ�����һ�����㣬������ѡ������',TITLE='��ʾ')
			RETURN
		endif

		CurrentYearMeteoYield=0.0
		FOR j=0,N_ELEMENTS(FactorID)-1 DO BEGIN
		  CurrentYearMeteoYield=CurrentYearMeteoYield+SimuMeteoResult[j]*CurrentYearFactorData[j]
		END

		CurrentYearMeteoYield=constant+CurrentYearMeteoYield     ;���ϳ�����,�õ�����Ĳ�������

	ENDIF ELSE BEGIN
		Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex

		Colnames = ['NPP��ֵ','NPP�ۼ�ֵ','LAI��ֵ','LAI�ۼ�ֵ','NDVI��ֵ','NDVI�ۼ�ֵ']
   		names=['NPP','NPP','LAI','LAI','NDVI','NDVI']
		DataType = names[MeteoTableIndex]
		Sensor_type = (*SelectFactor).SensorType
		IsAvgOrSum = MeteoTableIndex MOD 2        ;IsAvgOrSumֻΪ0��1 ,0ʱȡƽ��,1ʱȡ�ۼ�.

		Sensor = ['AVHRR','MODIS','VGT'] & Sensor_code = ['1','2','3']
		Sensorname = Sensor[WHERE(Sensor_code EQ (*SelectFactor).SensorType)]

		CropIDList = (*SelectFactor).CropIDList				;Crop��CropIDListӦ��Ӧ
		CropCol = ['AVG_SPRING_WHEAT','AVG_WINTER_WHEAT','AVG_EARLY_RICE','AVG_SEMILATE_RICE' $
				  ,'AVG_LATE_RICE','AVG_SPRING_MAIZE','AVG_SUMMER_MAIZE','AVG_SOYBEAN']				;Crop��CropIDListӦ��Ӧ
		CropFiled = CropCol[WHERE(CropIDList EQ (*SelectFactor).CropId)]
		TableColnames = Colnames[MeteoTableIndex]

		Code = (*SelectFactor).CountyCode
		CurrentYear = (*SelectFactor).CalcYear
		;������ݵ�ң������
		Data = DC_GetRsData(CurrentYear,CurrentYear,StartMonth,EndMonth,Code,CropFiled,IsAvgOrSum,Sensor_type,DataType,YieldType)
		IF ARRAY_EQUAL(FLOAT(Data),0.0) THEN BEGIN
			INFO =DIALOG_MESSAGE('���ݿ���û�м������'+CurrentYear+"��'"+Sensorname+"'��'"+DataType+"'����!",TITLE='����')
			RETURN
		ENDIF

		ActualMeteoYield=Regress__FactorName.RegressYield
		SimuMeteoResult=REGRESS(Alldata,ActualMeteoYield,CONST=constant $
		                  ,MCORRELATION=M_correlation,FTEST=F_check,YFIT=SimuMeteoYield)

		CurrentYearMeteoYield=constant+SimuMeteoResult* FLOAT(Data)    ;���ϳ�����,�õ�����Ĳ�������
		SelectedFactor_Name = TableColnames

	ENDELSE

		CropList=(*SelectFactor).CropIDList
		CropName=(*SelectFactor).CropNameList

		MeteoYieldResult={CropName            :  CropName[WHERE(CropList EQ (*SelectFactor).CropId)] ,$
		                  CountyCode          :  (*SelectFactor).CountyCode ,$
		                  ActualMeteoYield    :  ActualMeteoYield     ,$
		                  SimuMeteoYield      :  SimuMeteoYield ,$
		                  Const               :  constant ,$
		                  ModelCoefficient    :  STRTRIM(SimuMeteoResult,2) ,$
		                  F_check             :  F_check ,$
		                  M_correlation       :  M_correlation ,$
		                  SelectedFactor_Name :  SelectedFactor_Name ,$
		                CurrentYearMeteoYield :  CurrentYearMeteoYield, $
		                  CurrentYear         :  CurrentYear ,$
		                  StartYear           :  StartYear }

		IF (XREGISTERED('DC_FloatYield_3') NE 0) THEN BEGIN
		    Widget_Control,(*SelectFactor).Next_TLB,/DESTROY
		ENDIF

		DC_FloatYield_3,MeteoYieldResult,SelectFactor,GROUP_LEADER=Event.ID,ITS_TOPBASE=its_topbase
		(*SelectFactor).Next_TLB=its_topbase
		log, '����Ԥ��-��������', 0

END
;*********************************���沨������*********************************
PRO DC_SaveMeteoData,EVENT
	Widget_Control,Event.top,GET_UVALUE = SelectFactor
	Widget_Control,(*SelectFactor).AgorFactor_TABLE,GET_VALUE=MeteoData,GET_UVALUE = Regress__FactorName
	Widget_Control,(*SelectFactor).factor_TABLE,GET_VALUE=Correlation

	IF ARRAY_EQUAL(MeteoData, '') OR ARRAY_EQUAL(Correlation, '')  THEN BEGIN
	  Info=DIALOG_MESSAGE('����û��ɸѡ����,����ɸѡ!' ,TITLE='��ʾ',/INFORMATION)
	  RETURN
	ENDIF

	AllYearNum=FIX(STRMID(SYSTIME(),3,4,/REVERSE_OFFSET))-1980
	Datayear=INDGEN(AllYearNum)+1980
	StartYear  = Datayear[(*SelectFactor).StartYearID]

	CropList=(*SelectFactor).CropIDList
	CropName=(*SelectFactor).CropNameList

	AllFactorName = Regress__FactorName.AllFactorName
	CountyCode =  (*SelectFactor).CountyCode
	CropName = CropName[WHERE(CropList EQ (*SelectFactor).CropId)]
	MeteoYield = TRANSPOSE(Regress__FactorName.RegressYield)      ;ת��һ��

	YearsNum = (*SelectFactor).EndYearID - (*SelectFactor).StartYearID +1
	YieldYear=STRTRIM(INDGEN(YearsNum)+StartYear,2)
	FactorNum = N_ELEMENTS(AllFactorName)

	Savedata=STRARR(FactorNum+4,YearsNum+2)
	Savedata[*,0]=['��Code','����','���','��������',AllFactorName]
	Savedata[0,1:YearsNum]=REPLICATE(CountyCode,YearsNum)
	Savedata[1,1:YearsNum]=REPLICATE(CropName,YearsNum)
	Savedata[2,1:YearsNum]=YieldYear
	Savedata[3,1:YearsNum]=MeteoYield
	Savedata[4:*,1:YearsNum]=MeteoData
	Savedata[0:3,YearsNum+1]=['���ϵ��','--','--','--']
	Savedata[4:*,YearsNum+1]=Correlation

	IF WHERE(Savedata EQ '') NE [-1] THEN BEGIN    ;˵���пո�.ע����-1��[-1]������.
	    Savedata[WHERE(Savedata EQ '')]='---'
	ENDIF

	Filename=DIALOG_PICKFILE(TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
	      ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting, DIALOG_PARENT=Event.id)

	IF Filename EQ '' THEN RETURN

	OPENW,LUN,Filename,/GET_LUN,WIDTH=(FactorNum+4)*30
	PRINTF,LUN,Savedata
	FREE_LUN,LUN

	Info=DIALOG_MESSAGE('����ɹ�! ',TITLE='��ʾ',/INFORMATION)

	Widget_Control,Event.top,SET_UVALUE=SelectFactor


END
;*****************����"��������ģ��"�����еĲ����¼�****************************
PRO DC_FloatYield_2_event,event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ? widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='Refurbish'): BEGIN

		WIDGET_CONTROL,Event.id,BACKGROUND_COLOR  = [255,255,255]

       Widget_Control,Event.top,GET_UVALUE = SelectFactor

        Widget_Control,(*SelectFactor).threshold_TEXT,GET_VALUE =ThresholdValue ;����ֻ��һ��Ԫ�ص�����.
		ThresholdValue = DC_JudgeInputChar(ThresholdValue[0],Desc='��ֵ�ı���')
		IF ThresholdValue EQ -1 THEN RETURN

        Widget_Control,(*SelectFactor).factor_TABLE,GET_VALUE=AllCorrelation,BACKGROUND_COLOR=[255,255,255]

        IF AllCorrelation[0,0] EQ '' THEN RETURN

        AllCorrelation=FLOAT(AllCorrelation) ; &   ThresholdValue=FLOAT(ThresholdValue)     ;�õ�Ĭ�ϵ���ֵ
        SelectedFactorID=WHERE(ABS(AllCorrelation) GE ThresholdValue[0],Count)
       IF Count GT 0 THEN BEGIN
          SelectedFactor=Intarr(2,Count) & SelectedFactor[0,0:*]=SelectedFactorID         ;���ӱ�Ķϵ�ѡ��ʽ/DISJOINT_SELECTIONҪ��2�е�����.
          Widget_Control,(*SelectFactor).factor_TABLE,USE_TABLE_SELECT=SelectedFactor,SET_UVALUE=SelectedFactor $
                        ,SET_TABLE_VIEW = SelectedFactor[*,0],BACKGROUND_COLOR  = [0,255,0]
          Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='����ɸѡ��'+'(��ǰɸѡ��'+STRTRIM(STRING(Count),2)+'����������)'
       ENDIF ELSE BEGIN
          Widget_Control,(*SelectFactor).factor_TABLE,SET_TABLE_SELECT=[-1,-1],SET_UVALUE=[-1,-1]
          Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='����ɸѡ��'
          Prompt=DIALOG_MESSAGE(['��ǰû������"��ֵѡ������"�Ĳ�������,��������','����ֵ����������ȡ��ͬʱ��������ٽ���ɸѡ!'],TITLE='��ʾ',/INFORMATION)
       ENDELSE

       Widget_Control,Event.top,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Start_DROPLIST'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
       (*SelectFactor).StartYearID = Event.Index
;;       Info=['Ĭ�ϵ�"��ʼ���1980��"��������������������',$
;;             '��Ӧ����С���,�������ѡ��,���Ҫ����ѡ����?']
;;       Prompt=DIALOG_MESSAGE(TITLE='ѯ ��',Info,/QUESTION)
;;       IF Prompt EQ 'No' THEN BEGIN
;;          Widget_Control,Event.id,SET_DROPLIST_SELECT = 0   ;��Ϊ��С���
;;          (*SelectFactor).StartYearID = 0
;;          Widget_Control,wWidget,SET_UVALUE = SelectFactor
;;          RETURN
;;       ENDIF
       IF Event.Index GE (*SelectFactor).EndYearID THEN BEGIN
          Prompt=DIALOG_MESSAGE(TITLE='����','��ʼ��ݱ���С�ڽ������')
          Widget_Control,Event.id,SET_DROPLIST_SELECT = 0
          (*SelectFactor).StartYearID = 0
       ENDIF
       Widget_Control,wWidget,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='End_DROPLIST'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
        (*SelectFactor).EndYearID=Event.Index
;;       Info=['Ĭ�ϵ�"�������"���������������������Ӧ���������,����',$
;;             '���ѡ��,���Ҫѡ��ֻ��ѡ��ȴ�С�����,���Ҫ����ѡ����?']
;;       Prompt=DIALOG_MESSAGE(TITLE='ѯ ��',Info,/QUESTION)
;;       IF Prompt EQ 'No' THEN BEGIN
;;          Widget_Control,Event.id,SET_DROPLIST_SELECT = (*SelectFactor).DefaultEndYearID
;;          (*SelectFactor).EndYearID=(*SelectFactor).DefaultEndYearID
;;          Widget_Control,wWidget,SET_UVALUE = SelectFactor
;;          RETURN
;;       ENDIF
       EndYear = Event.Index
          IF EndYear GT (*SelectFactor).DefaultEndYearID THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','������ѡ���Ĭ��"�������"������')
             Widget_Control,Event.id,SET_DROPLIST_SELECT = (*SelectFactor).DefaultEndYearID
             (*SelectFactor).EndYearID=(*SelectFactor).DefaultEndYearID
          ENDIF ELSE BEGIN
             IF EndYear LE (*SelectFactor).StartYearID THEN BEGIN
                Prompt=DIALOG_MESSAGE(TITLE='����','������ݱ��������ʼ���')
                Widget_Control,Event.id,SET_DROPLIST_SELECT = (*SelectFactor).DefaultEndYearID
                (*SelectFactor).EndYearID=(*SelectFactor).DefaultEndYearID
             ENDIF
          ENDELSE
       Widget_Control,wWidget,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='YNStride'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
       StartMonth_Drop = Widget_Info(wWidget, FIND_BY_UNAME='StartMonth_Drop')   ;�õ���Ӧ���
       EndMonth_Drop = Widget_Info(wWidget, FIND_BY_UNAME='EndMonth_Drop')
       StartMonth= (*SelectFactor).StartMonthID
       EndMonth  = (*SelectFactor).EndMonthID
          IF (Event.SELECT EQ 1) AND (StartMonth LE EndMonth) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','��������ڿ���,����������ʼ�·�Ӧ���ڽ����·�!������ѡ���·�!')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT=10   ;��Ϊ��С���Ĭ��ֵ,��Ϊ���������ʱ,��������Ƕ�С��.
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT=4
             (*SelectFactor).StartMonthID=10 & (*SelectFactor).EndMonthID=4
          ENDIF
          IF (Event.SELECT EQ 0) AND (StartMonth GE EndMonth) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','��������ڲ�����,����������ʼ�·�ӦС�ڽ����·�!������ѡ���·�!')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT=EndMonth     ;�����ǵ�ֵ�Ե�
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT=StartMonth
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       (*SelectFactor).StrideYear = Event.SELECT
       Widget_Control,Event.top,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='StartMonth_Drop'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
       EndMonth_Drop = Widget_Info(wWidget, FIND_BY_UNAME='EndMonth_Drop')
       (*SelectFactor).StartMonthID=Event.Index
       StartMonth= Event.Index
       EndMonth  = (*SelectFactor).EndMonthID
       CropName=(*SelectFactor).CropNameList[WHERE((*SelectFactor).CropIDList EQ (*SelectFactor).CropID)]
       YNsensitive=Widget_Info(Widget_Info(wWidget, FIND_BY_UNAME='YNStride'),/SENSITIVE)
       IF YNsensitive EQ 0 THEN BEGIN
          IF StartMonth GE EndMonth THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����',CropName+'��������ʼ�·�ӦС�ڽ����·�')
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = EndMonth
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT = StartMonth   ;�����ǵ�ֵ�Ե�
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       ENDIF ELSE BEGIN
          IF (StartMonth LE EndMonth) AND ((*SelectFactor).StrideYear EQ 1) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','��������ڿ���,����������ʼ�·�Ӧ���ڽ����·�')
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = 10
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT = 4   ;���Ĭ�϶�С����·�.
             (*SelectFactor).StartMonthID=10 & (*SelectFactor).EndMonthID=4
          ENDIF
          IF (StartMonth GE EndMonth) AND ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','��������ڲ�����,����������ʼ�·�ӦС�ڽ����·�!������ѡ���·�!')
             Widget_Control,Event.ID,SET_DROPLIST_SELECT=EndMonth     ;�����ǵ�ֵ�Ե�
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT=StartMonth
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       ENDELSE
       Widget_Control,Event.top,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='EndMonth_Drop'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
       StartMonth_Drop = Widget_Info(wWidget, FIND_BY_UNAME='StartMonth_Drop')
       StartMonth  = (*SelectFactor).StartMonthID
       EndMonth= Event.Index
       (*SelectFactor).EndMonthID=Event.Index
       CropName=(*SelectFactor).CropNameList[WHERE((*SelectFactor).CropIDList EQ (*SelectFactor).CropID)]
       YNsensitive=Widget_Info(Widget_Info(wWidget, FIND_BY_UNAME='YNStride'),/SENSITIVE)
       IF YNsensitive EQ 0 THEN BEGIN
          IF StartMonth GE EndMonth THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����',CropName+'��������ʼ�·�ӦС�ڽ����·�')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT = EndMonth
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = StartMonth   ;�����ǵ�ֵ�Ե�
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       ENDIF ELSE BEGIN
          IF (StartMonth LE EndMonth) AND ((*SelectFactor).StrideYear EQ 1) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','��������ڿ���,����������ʼ�·�Ӧ���ڽ����·�')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT = 10
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = 4   ;���Ĭ�϶�С����·�.
             (*SelectFactor).StartMonthID=10 & (*SelectFactor).EndMonthID=4
          ENDIF
          IF (StartMonth GE EndMonth) AND ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='����','��������ڲ�����,����������ʼ�·�ӦС�ڽ����·�!������ѡ���·�!')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT=EndMonth     ;�����ǵ�ֵ�Ե�
             Widget_Control,Event.ID,SET_DROPLIST_SELECT=StartMonth
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       ENDELSE
       Widget_Control,Event.top,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='MeteorologyFactor'): BEGIN
    	Widget_Control,wWidget,GET_UVALUE = SelectFactor
    	IF (*SelectFactor).FactorType THEN DC_ClearOldData,Event.top
    END


    Widget_Info(wWidget, FIND_BY_UNAME='SensorDrop'): BEGIN
    	Widget_Control,wWidget,GET_UVALUE = SelectFactor
    	DC_ClearOldData,Event.top
        Sensor = ['1','2','3']    ;��Ӧ�ڴ�����LIST�Ĵ���
        (*SelectFactor).SensorType = Sensor[EVENT.INDEX]
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Cancel_BUTTON'): widget_control,Event.top,/DESTROY
    ELSE:
  ENDCASE

END

;**************"��������ģ��"����һ�����ӽ���**********************************
PRO DC_FloatYield_2,state,GROUP_LEADER=groupleader,ITS_TLB=its_tlb

   TLB = Widget_Base(GROUP_LEADER=groupleader,  $
      UNAME='TLB' ,XOFFSET=150 ,YOFFSET=200 ,TITLE='ɸѡ������������'  $
      ,SPACE=3 ,XPAD=1 ,YPAD=1 ,ROW=2 ,TLB_FRAME_ATTR=1);,/MODAL)

  ;----------------------------***BASE�ϲ�*********************-----------------
  AgorFactor = Widget_Base(TLB, UNAME='AgorFactor'  $
      ,SPACE=3 ,XPAD=0 ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  ;--------------------BASE�ϲ���ߵ�BASE----------------------------------
	  AgorFactor_left = Widget_Base(AgorFactor, UNAME='AgorFactor_left'  $
	      ,FRAME=1,SCR_XSIZE=172 ,SCR_YSIZE=447  $
	      ,/BASE_ALIGN_LEFT ,SPACE=3 ,XPAD=1 ,YPAD=1  $
	      ,COLUMN=1)
		;--------------------------������Ϣ-------------------------------------
		  Crop_conty_info = Widget_Base(AgorFactor_left,  $
		      UNAME='Crop_conty_info' ,FRAME=1,SPACE=0 ,xpad=1,YPAD=1,/COLUMN,/BASE_ALIGN_TOP)

		  ;-------------------------����label��text�ĸ߶ȺͿ��------------------------
		  labelwidth = 59 & labelheight = 18 & textwidth = 88 & textheight = 18

		  target_info=WIDGET_BASE(Crop_conty_info,/ROW,/BASE_ALIGN_CENTER)
		  Info_label = Widget_Label(target_info,UNAME='Info_label'  $
		      ,SCR_XSIZE=labelwidth ,SCR_YSIZE=labelheight,VALUE='Ŀ����Ϣ��')

		  county_code=WIDGET_BASE(Crop_conty_info,/ROW,/BASE_ALIGN_CENTER)
		  county_LABEL = Widget_Label(county_code, UNAME='county_LABEL'  $
		       ,SCR_XSIZE=labelwidth ,SCR_YSIZE=labelheight)
		  county_TEXT = Widget_Text(county_code, UNAME='county_TEXT'  $
		      ,SCR_XSIZE=textwidth-3 ,SCR_YSIZE=textheight)

		  crop_name=WIDGET_BASE(Crop_conty_info,/ROW,/BASE_ALIGN_CENTER)
		  Crop_LABEL = Widget_Label(crop_name, UNAME='Crop_LABEL'  $
		      ,SCR_XSIZE=labelwidth ,SCR_YSIZE=labelheight ,/ALIGN_left  $
		      ,VALUE='�������ƣ�')
		  Crop_TEXT = Widget_Text(crop_name, UNAME='Crop_TEXT'  $
		      ,SCR_XSIZE=textwidth-3 ,SCR_YSIZE=textheight)
		  ;----------------------------------------------------------------------

		  Factor_year = Widget_Base(AgorFactor_left,  $
		      UNAME='Factor_year' ,FRAME=1,SPACE=0 ,xpad=1,YPAD=1,/COLUMN,/BASE_ALIGN_TOP,SCR_XSIZE=162)

		  year_info=WIDGET_BASE(Factor_year,/ROW,/BASE_ALIGN_LEFT)
		  Year_LABEL = Widget_Label(year_info, UNAME='Year_LABEL'  $
		       ,SCR_XSIZE=85 ,SCR_YSIZE=15   $
		      ,VALUE='������ݣ�')

		  ;---------------------����������ʼ_�������------------------------
;		  Year_BASE = Widget_Base(Factor_year, UNAME='Year_BASE' $
;		      ,SPACE=2 ,XPAD=3 ,YPAD=1 ,/column,SCR_XSIZE=170)
		  ;-------------------------------------------------------------------
		  labelwidth1 = 63 & labelheight1 = 15 & Droplistwidth1 = 70 & Droplistheight1 = 18

			  start_year=WIDGET_BASE(Factor_year,/ROW,/BASE_ALIGN_CENTER)
			  StartYear_LABEL = Widget_Label(start_year, UNAME='StartYear_LABEL'  $
			      ,SCR_XSIZE=labelwidth1 ,SCR_YSIZE=labelheight1 ,/ALIGN_LEFT  $
			      ,VALUE='��ʼ��ݣ�')

			  Start_DROPLIST = Widget_Droplist(start_year, UNAME='Start_DROPLIST'  $
			      ,SCR_XSIZE=Droplistwidth1 ,SCR_YSIZE=Droplistheight1)

			  end_year=WIDGET_BASE(Factor_year,/ROW,/BASE_ALIGN_CENTER)
			  EndYear_LABEL = Widget_Label(end_year, UNAME='EndYear_LABEL'  $
			      ,SCR_XSIZE=labelwidth1 ,SCR_YSIZE=labelheight1  $
			      ,/ALIGN_LEFT ,VALUE='������ݣ�')

			  End_DROPLIST = Widget_Droplist(end_year, UNAME='End_DROPLIST'  $
			      ,SCR_XSIZE=Droplistwidth1 ,SCR_YSIZE=Droplistheight1)

		  ;--------------------�й������ں��Ƿ�����-----------------
		  GrowthPeriod_BASE = Widget_Base(AgorFactor_left,  $
		      UNAME='GrowthPeriod_BASE' ,SCR_XSIZE=162 $
		      ,SPACE=0 ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER,/FRAME)
		  ;-----------------������-----------------
		  GrowthLabel_BASE = Widget_Base(GrowthPeriod_BASE,  $
		      UNAME='GrowthLabel_BASE'  $
		      ,SCR_XSIZE=160,SCR_YSIZE=23 ,/BASE_ALIGN_LEFT  $
		      ,SPACE=1 ,XPAD=3 ,YPAD=0 ,ROW=1)
		  ;-------------------
		  growth_LABEL = Widget_Label(GrowthLabel_BASE, UNAME='growth_LABEL'  $
		      ,XOFFSET=0 ,YOFFSET=3 ,SCR_XSIZE=45 ,SCR_YSIZE=15   $
		      ,VALUE='�����ڣ�')

		  CropGrowthMonth=STRTRIM(STRING(INDGEN(12)+1),2)
		  StartMonth_Drop = Widget_DropList(GrowthLabel_BASE,VALUE=CropGrowthMonth,  $
		      UNAME='StartMonth_Drop'  ,SCR_XSIZE=35,SCR_YSIZE=18,/ALIGN_TOP)

		  growthTo_LABEL = Widget_Label(GrowthLabel_BASE,  $
		      UNAME='growthTo_LABEL' ,XOFFSET=90 ,YOFFSET=3 ,SCR_XSIZE=13  $
		      ,SCR_YSIZE=15,/ALIGN_LEFT ,VALUE='��')

		  EndMonth_Drop = Widget_DropList(GrowthLabel_BASE, UNAME='EndMonth_Drop'  $
		      ,SCR_XSIZE=35 ,SCR_YSIZE=18,VALUE=CropGrowthMonth)

		  growthEnd_LABEL = Widget_Label(GrowthLabel_BASE,  $
		      UNAME='growthEnd_LABEL' ,XOFFSET=142 ,YOFFSET=3 ,SCR_XSIZE=25  $
		      ,SCR_YSIZE=15 ,/ALIGN_LEFT ,VALUE='��')

		 ;-----------�������Ƿ���갴ť--------------------------------------------
		  YNStride_BASE = Widget_Base(GrowthPeriod_BASE,  $
		      UNAME='YNStride_BASE' ,SCR_XSIZE=170  $
		      ,SCR_YSIZE=22 ,COLUMN=1 ,/NONEXCLUSIVE,xpad=7)

		  YNStride = Widget_Button(YNStride_BASE, UNAME='YNStride'  $
		      ,SCR_XSIZE=140 ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='�����ڿ���(ע�ⶬС��)')

		  ;---------------------��������BASE-----------------------------------
		  MeteorFactor_BA=Widget_Base(AgorFactor_left,XPAD=0,YPAD=0,/COLUMN,SPACE=2)

		    SensorBase = Widget_Base(MeteorFactor_BA,XPAD=0,YPAD=1,SPACE=2,/ROW,/FRAME,/align_left,SCR_XSIZE=162)
		    SensorDrop = WIDGET_DROPLIST(SensorBase,UNAME='SensorDrop',TITLE='���������ͣ�' $
		    							 ,VALUE=['AVHRR','MODIS','VGT'],SCR_XSIZE=155)
		  FACTOR=WIDGET_BASE(MeteorFactor_BA,/FRAME,/COLUMN,SPACE=2)
		  MeteorFactor_Label=Widget_Label(FACTOR,VALUE='�������ӣ�' $
		      ,SCR_YSIZE=15, /ALIGN_LEFT)
	  ;--------------------------�������ӱ�----------------------------------
	  TABLE=WIDGET_BASE(AgorFactor,/COLUMN,XPAD=0,YPAD=0,SPACE=3)
	  AgorFactor_TABLE = Widget_Table(TABLE,  $
	      UNAME='AgorFactor_TABLE' ,XOFFSET=200 ,YOFFSET=3 ,SCR_XSIZE=568, /DISJOINT_SELECTION  $
	      ,SCR_YSIZE=359 ,/EDITABLE ,XSIZE=20 ,YSIZE=20,/FRAME,/RESIZEABLE_COLUMNS)


  ;----------***************--------BASE�°벿----------***********------------------
  threshold = Widget_Base(TLB, UNAME='threshold'  $
      ,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)

	  ;---------------------------------------------------------
	  SelectFactor_BASE = Widget_Base(AgorFactor_left,  $
	      UNAME='SelectFactor_BASE',SPACE=2 ,XPAD=0 $
	      ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  ;------------------��ֵ����BASE------------------------------------------
	  thredshodValue_BASE = Widget_Base(SelectFactor_BASE,  $
	      UNAME='thredshodValue_BASE' ,FRAME=1  $
	      ,/BASE_ALIGN_LEFT,SPACE=5 ,XPAD=1 ,YPAD=1 ,ROW=1,SCR_XSIZE=162)

		  threshold_LABEL = Widget_Label(thredshodValue_BASE,  $
		      UNAME='threshold_LABEL' ,SCR_XSIZE=60  $
		      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='��ֵ���ã�')
		  threshold_TEXT = Widget_Text(thredshodValue_BASE, UNAME='threshold_TEXT'  $
		      ,FRAME=1 ,SCR_XSIZE=30 ,SCR_YSIZE=20 ,VALUE = '0.5' $
		      ,/EDITABLE ,XSIZE=20 ,YSIZE=1)
		  Refurbish = Widget_Button(thredshodValue_BASE, UNAME='Refurbish'  $
		      ,SCR_XSIZE=45 ,SCR_YSIZE=20 ,/ALIGN_CENTER,VALUE='ˢ��'  $
		       ,TOOLTIP='��ֵ�����趨��ˢ������ɸѡ��' )
	;-------------------------����ɸѡ��-----------------------------------
	  ChoiceFactor_BASE = Widget_Base(TABLE, $
	      UNAME='ChoiceFactor_BASE' ,FRAME=1 ,SPACE=1,SCR_XSIZE=568 $
	      ,SCR_YSIZE=83,XPAD=0 ,YPAD=1,COLUMN=1,/BASE_ALIGN_LEFT)

		  factor_LABEL = Widget_Label(ChoiceFactor_BASE, UNAME='factor_LABEL'  $
		      ,SCR_XSIZE=273 ,SCR_YSIZE=15,/ALIGN_CENTER ,VALUE='����ɸѡ��')

		  factor_TABLE = Widget_Table(ChoiceFactor_BASE, UNAME='factor_TABLE',ROW_LABELS=['���ϵ��']  $
		       ,SCR_XSIZE=568 ,SCR_YSIZE=63 ,XSIZE=10,YSIZE=1,/FRAME,/DISJOINT_SELECTION,/RESIZEABLE_COLUMNS)
	 ;------------------------��ť����--------------------------------------
	  DivideSpace=75
	  Simulate_save_BASE = Widget_Base(threshold,  $
	      UNAME='Simulate_save_BASE' ,/BASE_ALIGN_TOP  $
	      ,SCR_XSIZE=745 ,SCR_YSIZE=36,/FRAME   $
	      ,SPACE=DivideSpace,XPAD=71 ,YPAD=0 ,ROW=1)
	  ;----------------------------------------------
;		  Simulate_BASE = Widget_Base(Simulate_save_BASE,FRAME=0  $
;		      ,UNAME='Simulate_BASE' ,SCR_YSIZE=34,/BASE_ALIGN_TOP $
;		       ,SPACE=DivideSpace ,XPAD=DivideSpace,YPAD=3,ROW=1)

		      Button_width = 90 & Button_height = 22  ;���尴ť�Ŀ�Ⱥ͸߶�
			  Select_BUTTON = Widget_Button(Simulate_save_BASE, UNAME='Select_BUTTON'  $
			      ,FRAME=0 ,XOFFSET=70 ,YOFFSET=2 ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height  $
			      ,/ALIGN_CENTER ,TOOLTIP='������ֵɸѡ���ʵĲ�������' ,VALUE='ɸѡ'$
			      ,EVENT_PRO='DC_FilterFactor')
			  Simulation_BUTTON = Widget_Button(Simulate_save_BASE,  $
			      UNAME='Simulation_BUTTON' ,FRAME=0 ,XOFFSET=276 ,YOFFSET=2  $
			      ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height ,/ALIGN_CENTER  $
			      ,TOOLTIP='��һ�����в�������ģ��' ,VALUE='��һ��',EVENT_PRO='DC_SimulatResult')
			 Save_BUTTON = Widget_Button(Simulate_save_BASE,UNAME='Save_BUTTON' ,FRAME=0  $
			      ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height ,/ALIGN_CENTER  $
			      ,TOOLTIP='���������ݺ����ϵ�����浽���ش���' ,VALUE='����',EVENT_PRO='DC_SaveMeteoData')
		;-------------------------------------------------------------------
		  Cancel_BUTTON = Widget_Button(Simulate_save_BASE, UNAME='Cancel_BUTTON'  $
		      ,FRAME=0 ,XOFFSET=70 ,YOFFSET=3 ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height  $
		      ,/ALIGN_CENTER ,TOOLTIP='�˳�����ģ��ڶ���' ,VALUE='ȡ��')

;--------------------------����ʹ�ò������в���������ֵ����-------------------------------------
	IF (*state).FactorType EQ 0 THEN BEGIN
		  names=['ƽ������','��ˮ','����','�������ʪ��','0�����','�������','�������']

		  MeteorologyFactor = CW_BGROUP(FACTOR, names,UNAME='MeteorologyFactor' $
		        ,YPAD=0,XPAD=5,XSIZE=138,Y_SCROLL_SIZE=108,/COLUMN, /NONEXCLUSIVE $
		        ,/RETURN_NAME,/SCROLL,SPACE=0)
		WIDGET_CONTROL,MeteorologyFactor,SET_VALUE=[1,1,1,0,0,0,0]          ;���þ��¡���ˮ������ΪĬ��ֵ��ע�����ַ�ʽֻ������/NONEXCLUSIVE
		WIDGET_CONTROL,SensorBase,SENSITIVE=0
	ENDIF ELSE BEGIN
		  names=['NPP��ֵ','NPP�ۼ�ֵ','LAI��ֵ','LAI�ۼ�ֵ','NDVI��ֵ','NDVI�ۼ�ֵ']

		  MeteorologyFactor = CW_BGROUP(FACTOR, names,UNAME='MeteorologyFactor' $
		        ,YPAD=0,XPAD=5,XSIZE=138,Y_SCROLL_SIZE=108,/COLUMN, /EXCLUSIVE $
		        ,/FRAME,/RETURN_NAME,/SCROLL,SPACE=0)
		WIDGET_CONTROL,MeteorologyFactor,SET_VALUE=4          ;����NDVI�ۼ�ֵΪĬ��ֵ��ע�����ַ�ʽֻ������/EXCLUSIVE
		WIDGET_CONTROL,thredshodValue_BASE,SENSITIVE=0
	ENDELSE
;----------------------------------------------------------------------------
  Widget_Control, /REALIZE, TLB
  WIDGET_CONTROL,Cancel_BUTTON,/INPUT_FOCUS

   GrowthMonth = (*state).GrowthMonth

	WIDGET_CONTROL,(*state).DistrictName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)
		Crop = STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDList EQ (*state).cropID)],/RE) ;�õ�������
    YearHaveData = *((*state).YearHaveData)
	IF (*state).YieldType THEN ObjType='վ����룺' ELSE ObjType='�� �� �룺'

	Widget_Control, county_LABEL,SET_VALUE=ObjType
	Widget_Control, county_TEXT,SET_VALUE=County[0]
	Widget_Control, Crop_TEXT,SET_VALUE=Crop

	Widget_Control, StartMonth_Drop,SET_DROPLIST_SELECT=GrowthMonth[0]-1
	Widget_Control, EndMonth_Drop  ,SET_DROPLIST_SELECT=GrowthMonth[1]-1

	Widget_Control, YNStride,SET_BUTTON=GrowthMonth[2],SENSITIVE=GrowthMonth[2]
	Widget_Control, Start_DROPLIST,SET_VALUE= YearHaveData
	Widget_Control, End_DROPLIST,SET_VALUE= YearHaveData,SET_DROPLIST_SELECT=GrowthMonth[3]     ;��������ݶ���������.

  Widget_Control,AgorFactor_TABLE,SET_TABLE_SELECT=[-1,-1]
  Widget_Control,factor_TABLE,SET_TABLE_SELECT=[-1,-1]

 		para = {CropId         		:   (*state).CropId 		,$	;����ID
                CropIDList			:	(*state).CropIDList		,$	;����ID�б�
                CropNameList		:	(*state).CropNameList	,$	;���������б�
                CountyCode        	:   County[0] 			,$	;�ػ�վ��Ĵ���
				CalcYear			:	(*state).CalcYear	,$	;Ҫ��������Ĳ�������
				FactorType			:	(*state).FactorType ,$  ;��������
                YieldType			:	(*state).YieldType	,$	;��������
                SensorType			:	'1'					,$	;����������Ĭ��ΪAVHRR
                ARRAY_YEAR			:	YearHaveData			,$	;�����������,����ָ��
                YieldData			:	*((*state).YieldData)	,$	;��ʶ�ӿ�����ȡ��ʵ�ʺ����Ʋ���,2��ֵ(FLOAT).
                StrideYear        	:   GrowthMonth[2]			,$  ;0��ʾ������,1��ʾ����.
                StartYearID       	:   0					,$
                EndYearID         	:   GrowthMonth[3] 		,$
                DefaultEndYearID  	:   GrowthMonth[3]  	,$
                StartMonthID     	:   GrowthMonth[0]-1	,$
                EndMonthID        	:   GrowthMonth[1]-1 	,$
                AgorFactor_TABLE  	:   AgorFactor_TABLE 	,$
                MeteorologyFactor 	:   MeteorologyFactor	,$  ;�����б����,Ŀ���ǽ�ѡ�еĲ������Ӵ�����.
                factor_TABLE      	:   factor_TABLE 		,$
                threshold_TEXT    	:   threshold_TEXT		,$
                factor_LABEL      	:   factor_LABEL		,$
                Next_TLB          	:   0L}           			;��ȡ��һ���ڵĶ���BASE

  its_tlb = TLB   ;��ȡ����BASE

  SelectFactor = PTR_NEW(para, /NO_COPY)
  Widget_Control, TLB ,SET_UVALUE=SelectFactor

  XManager, 'DC_FloatYield_2', TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END