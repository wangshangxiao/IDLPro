;������������ģ��

;**�Զ������:��ȡ����Ӧ����ĸ�����(����û�в���������Կ�ֵ�����)*********************
FUNCTION DC_GetYield,StartYear,EndYear,CropID,DistrictCode,YieldType

		ActualYield = STRARR(1)
		TrendYield  = STRARR(1)

		StartYear_ = FIX(StartYear)
		EndYear_   = FIX(EndYear)

		IF YieldType EQ 0 THEN BEGIN
			Table = "county_crop_sta_yield where crop_id='"
			TrendTable = "COUNTY_CROP_TREND_YIELD where crop_id='"
			Join = "' and county_code='"
		ENDIF ELSE BEGIN
			Table = "AGROSTATION_CROP_STA_YIELD where crop_id='"
			TrendTable = "AGROSTATION_CROP_TREND_YIELD where crop_id='"
			Join = "' and AgroMeteoStation_CODE='"
		ENDELSE


		FOR I=StartYear_,EndYear_ DO BEGIN
			Sqlstr='select crop_yield from '+Table +CropID+Join+DistrictCode+"' and year="+STRTRIM(I,2)
			ActualYield = [ActualYield,TRANSPOSE(DC_GetdataFromDB_Str(1,Sqlstr))]   ;ÿ��DC_GetdataFromDB_Str(1,Sqlstr)ֻ�õ�һ������.
		ENDFOR
		ActualYield = ActualYield[1:*]          ;��ʷʵ�ʲ���

		FOR I=StartYear_,EndYear_ DO BEGIN
			Sqlstr='select trend_yield from '+TrendTable +CropID+Join+DistrictCode+"' and year="+STRTRIM(I,2)
			TrendYield = [TrendYield,TRANSPOSE(DC_GetdataFromDB_Str(1,Sqlstr))]   ;ÿ��DC_GetdataFromDB_Str(1,Sqlstr)ֻ�õ�һ������.
		ENDFOR
		TrendYield = TrendYield[1:*]			;��ʷ���Ʋ���

		YieldData = [[ActualYield],[TrendYield]]

		RETURN,YieldData           ;�˴����շ����Ƕ�������.�ַ���.

END

;**�Զ������:����ʡ��������ȡ��������ػ�վ��Ĵ���********************************
PRO DC_FloatCountyOrSation	,ListWiget     $	;�б�list���ID
							,NameTextWidgt $	;�ı�Text���ID.
							,YieldType     $    ;��ʶ���ػ���վ���������
							,ProCode	   $	;ʡID
							,CropID		   $	;����ID
							,NumReocrd = NumReocrd  ;�õ���¼��
	IF YieldType EQ 0 THEN BEGIN
		Sql = 'select code,name from county_code where LEFT(code,2)='+ProCode
		Sql = 'select distinct code,name from COUNTY_CROP_TREND_YIELD a,('+Sql+ $
			") b where a.county_code=b.code and a.crop_id='"+CropID+"'"
	ENDIF ELSE BEGIN
		Sql = 'select distinct code,name,county_code from AGROSTATION_CROP_TREND_YIELD a,AGRO_METEO_STATION_INFO b ' $
			  +'where a.AgroMeteoStation_CODE=b.code'
		Sql = 'select code,name from ('+Sql+') where LEFT(county_code,2)='+ProCode
	ENDELSE
	DistrictCode = DC_GetDataFromDB(Sql,N_RECORDS = NumReocrd)

	IF NOT ARRAY_EQUAL(DistrictCode,'',/NO_TYPECONV ) THEN BEGIN
		WIDGET_CONTROL,ListWiget,SET_VALUE=' '+DistrictCode.CODE+'  '+DistrictCode.NAME $
							   ,SET_UVALUE=DistrictCode, SET_LIST_SELECT=-1
		WIDGET_CONTROL,NameTextWidgt,SET_VALUE=''
	ENDIF ELSE BEGIN      ;û�����������Ĳ�ѯ
		WIDGET_CONTROL,ListWiget,SET_VALUE='',SET_UVALUE=''
		WIDGET_CONTROL,NameTextWidgt,SET_VALUE=''
	ENDELSE

END
;**�Զ������:�����Ӧ�����ֵ********************************
PRO DC_CleanFloatYield,TLB

	WIDGET_CONTROL,TLB,GET_UVALUE=PA

	WIDGET_CONTROL,(*PA).TableTitle,SET_VALUE='�������ݱ�'

	WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE=TRYield & TRYield[*,*] = ''
	WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_VALUE=TRYield $
				  ,BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

END
;888888888888888888888������Ĳ����¼�����8888888888888888888888888888888888888888888888888888
PRO DC_FloatYield_event, Event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  widget_info(Event.id, /trLee_root) : event.id)

;        CATCH, Error_status               ;��ȡ����.
;     IF Error_status NE 0 THEN BEGIN
;        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
;        CATCH, /CANCEL
;        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
;     ENDIF

	wWidget =  Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	WIDGET_CONTROL,/HOURGLASS

  CASE wTarget OF
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='ProDroplist'): BEGIN
		  (*PA).ProID = (*PA).ProIDList[EVENT.INDEX]

		  DC_CleanFloatYield,EVENT.TOP
		  DC_FloatCountyOrSation,(*PA).DistrictList,(*PA).DistrictName,(*PA).YieldType $
		  						  ,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd

		(*PA).IsBlankList = NumReocrd EQ 0

		IF (*PA).YieldType THEN  District = 'վ��' ELSE	 District = '��'

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='��ѡ��'+District
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).DistrictList,GET_UVALUE=DistrictCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =DistrictCode[0].CODE+' '+DistrictCode[0].NAME
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,(*PA).BatchBU,/SENSITIVE
				WIDGET_CONTROL,(*PA).SaveTo,SENSITIVE=0
				WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE=CouttyValue
		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',TITLE='��ʾ',/INFOR)
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',TITLE='��ʾ',/INFOR)
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CropDroplist'): BEGIN

		 (*PA).CropID = (*PA).CropIDList[EVENT.INDEX]

		  DC_CleanFloatYield,EVENT.TOP
		  DC_FloatCountyOrSation,(*PA).DistrictList,(*PA).DistrictName,(*PA).YieldType $
		  						  ,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd

		(*PA).IsBlankList = NumReocrd EQ 0

		IF (*PA).YieldType THEN  District = 'վ��' ELSE	 District = '��'

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='��ѡ��'+District
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).DistrictList,GET_UVALUE=DistrictCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =DistrictCode[0].CODE+' '+DistrictCode[0].NAME
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,(*PA).BatchBU,/SENSITIVE
				WIDGET_CONTROL,(*PA).SaveTo,SENSITIVE=0
				WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE=CouttyValue
		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',TITLE='��ʾ',/INFOR)
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',TITLE='��ʾ',/INFOR)
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='YearDroplist'): BEGIN
		(*PA).CalcYear=(*PA).ARRAY_YEAR[EVENT.INDEX]
;		IF NOT (*PA).IsCalPara THEN RETURN
;
;		IF NOT (*PA).IsBlankList THEN BEGIN
;			Sel = WIDGET_INFO((*PA).DistrictList,/LIST_SELECT)
;			IF NOT ARRAY_EQUAL(Sel,-1) THEN BEGIN
;;				ComputeParameter,Event.top
;			END
;		ENDIF
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DistrictList'): BEGIN

		  DC_CleanFloatYield,EVENT.TOP
		IF (*PA).IsBlankList THEN BEGIN
			IF (*PA).YieldType EQ 0 THEN BEGIN
				INFO = DIALOG_MESSAGE('�б���û�п�ѡ����!',/INFO,TITLE='��ʾ')
			ENDIF ELSE 	INFO = DIALOG_MESSAGE('�б���û�п�ѡ��վ��!',/INFO,TITLE='��ʾ')

			RETURN
		ENDIF

		WIDGET_CONTROL,EVENT.ID,GET_UVALUE=District

		WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE=District[EVENT.INDEX].CODE+' '+District[EVENT.INDEX].NAME

		IF NOT (*PA).IsCalPara THEN RETURN

		SelectedDistrict = District[EVENT.INDEX].CODE
		Cropid = (*PA).Cropid
		IF (*PA).YieldType EQ 0 THEN BEGIN
			SQL = 'select Yield_Start_year,Yield_End_year from COUNTY_TREND_YIELD_PARAMETER '+$
				 "where county_code='"+SelectedDistrict+"' and crop_id='"+Cropid+"'"
		ENDIF ELSE BEGIN
			SQL = 'select Yield_Start_year,Yield_End_year from AGROSTATION_TREND_YIELD_PARAMETER '+$
				 "where AgroMeteoStation_CODE='"+SelectedDistrict+"' and crop_id='"+Cropid+"'"
		ENDELSE

		YearThreshold = DC_GetDataFromDB(SQL)
		IF ARRAY_EQUAL(YearThreshold,'', /NO_TYPECONV) THEN BEGIN
			IF (*PA).YieldType EQ 0 THEN BEGIN
		   		INFO = DIALOG_MESSAGE('����û�ж�"'+District[EVENT.INDEX].NAME+'"���в������Ʒ���!',TITLE='����')
		   	ENDIF ELSE BEGIN
		   		INFO = DIALOG_MESSAGE('����û�ж�"'+District[EVENT.INDEX].NAME+'վ"���в������Ʒ���!',TITLE='����')
		   	ENDELSE
		   RETURN
		ENDIF

		StartYear = STRTRIM(YearThreshold.(0),2)
		EndYear = STRTRIM(YearThreshold.(1),2)
		YearNum = FIX(EndYear)-FIX(StartYear)+1

		YearHaveData = STRTRIM(INDGEN(YearNum)+FIX(StartYear),2)
		RowLabels = YearHaveData+'��'

		PTR_FREE,(*PA).YearHaveData
		(*PA).YearHaveData = PTR_NEW(YearHaveData,/NO_COPY)

		;�����YieldDataΪ�ַ���,��������
		YieldData = DC_GetYield(StartYear,EndYear,(*PA).CropID,SelectedDistrict,(*PA).YieldType)
		ActualYield = FLOAT(YieldData[*,0])
		TrendYield  = FLOAT(YieldData[*,1])

		ZeroId = WHERE(ActualYield EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
	  		ZeroCell = [INTARR(1,ZeroNum),TRANSPOSE(ZeroID)]
			ActualYield[ZeroId] = MEAN(ActualYield[NotZeroID])
		ENDIF ELSE ZeroCell =[-1,-1]

		FloatYield = ActualYield-TrendYield

		PTR_FREE,(*PA).YieldData
		(*PA).YieldData = PTR_NEW(FLOAT(YieldData),/NO_COPY)

		TableYield = STRTRIM(TRANSPOSE([[ActualYield],[TrendYield],[FloatYield]]),2)
		ColumnLabels = ['ʵ�ʲ���','���Ʋ���','��������']
		WIDGET_CONTROL,(*PA).TrendYield_TABLE,TABLE_YSIZE=YearNum $
		            ,SET_UVALUE=RowLabels $   ;����֮�������û�ֵ,��Ϊ��������ʱ"�������".
		            ,ROW_LABELS=RowLabels  ,COLUMN_LABELS=ColumnLabels $
		            ,SET_VALUE=TableYield  ,ALIGNMENT=2,COLUMN_WIDTHS=65

		WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=ZeroCell,SET_TABLE_VIEW = ZeroCell[*,0] $
					,BACKGROUND_COLOR  = [255,255,0]     			 ;��ʵ����ݲ���Ϊ0(����ƽ��ֵ���滻�����)�ĵ�Ԫ����ɻ�ɫ��ʾ.
;		WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_TABLE_SELECT=[-1,-1]  ;���ζԱ�WIDGET_CONTROL,��Ϊ�û�ɫ�Զ�����������.


		IF (*PA).YieldType EQ 0 THEN BEGIN
	   		INFO = District[EVENT.INDEX].NAME+'�������ݱ�'
	   	ENDIF ELSE INFO = District[EVENT.INDEX].NAME+'վ�������ݱ�'

		WIDGET_CONTROL,(*PA).TableTitle,SET_VALUE=INFO

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='YieldType'): BEGIN
		IF NOT EVENT.SELECT THEN RETURN

 		(*PA).YieldType = EVENT.VALUE

		IF (*PA).YieldType THEN  District = 'վ��' ELSE	 District = '��'

		WIDGET_CONTROL,(*PA).TitleName,SET_VALUE =District+':'

		DC_CleanFloatYield,EVENT.TOP
		DC_FloatCountyOrSation,(*PA).DistrictList,(*PA).DistrictName,(*PA).YieldType $
		  						,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd


		(*PA).IsBlankList = NumReocrd EQ 0

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='��ѡ��'+District
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).DistrictList,GET_UVALUE=DistrictCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =DistrictCode[0].CODE+' '+DistrictCode[0].NAME
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,(*PA).BatchBU,/SENSITIVE
				WIDGET_CONTROL,(*PA).SaveTo,SENSITIVE=0
				WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE=CouttyValue

		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='FactorType'): BEGIN
  		IF NOT EVENT.SELECT THEN RETURN
  		(*PA).FactorType = EVENT.VALUE

		IF (*PA).IsCalPara THEN RETURN

		IF (*PA).IsBlankList THEN BEGIN
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=0
		ENDELSE

 	END

	WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU'): BEGIN   ;�������¼�
		IF (*PA).IsBlankList THEN BEGIN
			INFO = DIALOG_MESSAGE('��ߵ��б�Ϊ��,û�п�ѡ��������!',/INFOR,TITLE='��ʾ')
			RETURN
		ENDIF

		COMMON COMMON_BLOCK

		CropId = (*PA).CROPID
		CalcYear = (*PA).CalcYear
		WIDGET_CONTROL,(*PA).DistrictList,GET_UVALUE=District   ;ע��District�ǽṹ������
		NumCounty = N_ELEMENTS(District)
		NoComputedID = INTARR(1) & Num = 0

		YieldType  = (*PA).YieldType
		FactorType = (*PA).FactorType

		CountyYield = STRARR(2,1)

		IsNostation   =0B
   		IsNoTrendPara =0B
   		IsNoMeteoData =0B
   		IsNoRsData    =0B
   		IsNoTrendYield=0B


;====������ӣ�20070903��������============================================================
		FOR i=0,NumCounty-1 DO BEGIN
			DisCode = District[i].(0) & DisName = District[i].(1)
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=i

		   CASE 1 OF
			FactorType EQ 0 :BEGIN
				 IF YieldType EQ 0 THEN BEGIN
					ParaTable = "COUNTY_Meteo_yield_PARAMETER where crop_id='"
				 	YieldTable = 'COUNTY_Crop_MeteoOrFluctuate_yield'
					Scale = 'county_code'
					ModelTypeId = '1'
					EstimateTable = 'COUNTY_ESTIMATED_YIELD'

			        Sqlstr='select weatherstation_code from tainsen_polygen where county_code='+"'"+DisCode+"'"
					station_code=DC_GetdataFromDB_Str(1,Sqlstr,N_RECORDS = NumReocrd)

					IF NumReocrd EQ 0 THEN BEGIN   ;����,��Ϊ��̩ɭ����α���û��������Ӧ��վ��
						Num = Num+1
						NoComputedID = [NoComputedID,i]
						IF IsNostation THEN GOTO,Next
						DC_Ask_YesOrNo,'���ݿ����û����"'+DisName+'"��Ӧ������վ��,��������ļ�����!',EVENT.ID,ReplyID =ReplyID
						IF (*ReplyID EQ 2) THEN BEGIN
						    IsNostation = 1B
						ENDIF ELSE BEGIN
						    IF (*ReplyID EQ 3) OR (*ReplyID EQ 4) OR (*ReplyID EQ 5) THEN BEGIN
						       PTR_FREE,ReplyID
						       RETURN
						    ENDIF
						ENDELSE
						PTR_FREE,ReplyID
					    GOTO,Next
					ENDIF
				 ENDIF ELSE BEGIN
					ParaTable = "AGROSTATION_Meteo_yield_PARAMETER where crop_id='"
				 	YieldTable = 'AGROSTATION_Crop_MeteoOrFluctuate_yield'
					Scale = 'AgroMeteoStation_CODE'
					ModelTypeId = '2'
					EstimateTable = 'AGROSTATION_ESTIMATED_YIELD'

					station_code=DisCode
				 ENDELSE

				SQL = 'select Coefficient,FactorSel,Factor_position,phe_Start_month,phe_End_month from ' $
					 + ParaTable+CropId+"' and "+Scale+"='"+DisCode+"'"

;				;====������ӣ�20070903��������============================================================
;				SQL_y = 'select Coefficient,FactorSel,Factor_position,phe_Start_month,phe_End_month from ' $
;					 + ParaTable+CropId+"' and "+Scale+"='360123'"
;				Para = DC_GetdataFromDB_Str(5 ,SQL_y,N_RECORDS = NumReocrd)
				;================================================================

				Para = DC_GetdataFromDB_Str(5 ,SQL,N_RECORDS = NumReocrd)   ;���ص���5��һ�е�����
				IF NumReocrd EQ 0 THEN BEGIN   ;����,,��Ϊ��û�н��в���ģ��,�޲���
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoTrendPara THEN GOTO,Next
					DC_Ask_YesOrNo,'���ݿ���û��"'+DisName+'"��Ӧ��ģ�����,���Ƚ��и��صĲ���ģ��,��������ļ�����!',EVENT.ID,ReplyID =ReplyID
					IF (*ReplyID EQ 2) THEN BEGIN
					    IsNoTrendPara = 1B
					ENDIF ELSE BEGIN
					    IF (*ReplyID EQ 3) OR (*ReplyID EQ 4) OR (*ReplyID EQ 5) THEN BEGIN
					       PTR_FREE,ReplyID
					       RETURN
					    ENDIF
					ENDELSE
					PTR_FREE,ReplyID
					GOTO,Next
				ENDIF

				Coefficency = FLOAT(STRSPLIT(Para[0,0],'/',/EXTRACT))
				MeteoTableIndex = FIX(STRSPLIT(Para[1,0],'/',/EXTRACT))
				SelectFactorID = FIX(STRSPLIT(Para[2,0],'/',/EXTRACT))

				StartMonth = Para[3,0] & EndMonth = Para[4,0]
				StrideYear = FIX(StartMonth) GT FIX(EndMonth)    		  ;�ж��Ƿ����
				IF (StrideYear EQ 0) THEN BEGIN ;�����õ��ĺ�����"DC_FloatYield_2"Line 125
				    SingleTendayNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3   ;��Ѯ�ĸ���,�������Ѯ������������
				    AllData0=DC_SingleTendayFactor(CalcYear,CalcYear,StartMonth,EndMonth,MeteoTableIndex,station_code,YieldType)
				    SingleTendayData = AllData0     ;��Ѯ����������������ж�
				    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,StartMonth,EndMonth)
				    AllData0=[AllData0,[CombinationData]]                                         ;�������յĲ�������.
				ENDIF ELSE BEGIN                                           ;ע�ⶬС���ǿ����,������ʼ�ͽ����´�С����ֱ����������������.
				    Lastyear=STRTRIM(FIX(CalcYear)-1,2)                    ;���ڶ�С��,�����ڴ�����11�¿�ʼ��.������Lastyear
				    SingleTendayNum = (13+FIX(EndMonth)-FIX(StartMonth))*3
				    AllData1=DC_SingleTendayFactor(Lastyear,Lastyear,StartMonth,'12',MeteoTableIndex,station_code,YieldType)
				    AllData2=DC_SingleTendayFactor(Fix(Lastyear)+1,Fix(Lastyear)+1,'1',EndMonth,MeteoTableIndex,station_code,YieldType)
				    AllData0=[AllData1,[AllData2]]
				    SingleTendayData = AllData0     ;��Ѯ����������������ж�
				    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,1,13+FIX(EndMonth)-FIX(StartMonth))   ;�������������,������ʼ�ºͽ�����,����Ϊ��
				    AllData0=[AllData0,[CombinationData]]                                                               ;���庯������������ֻ����õ�����,���Կ�������������.
				ENDELSE

				IF ARRAY_EQUAL(SingleTendayData,'') THEN BEGIN   ;����,��Ϊû�е�Ѯ������������
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoMeteoData THEN GOTO,Next
					DC_Ask_YesOrNo,'���ݿ���û��"'+DisName+'"��Ӧ����������,��������ļ�����!',EVENT.ID,ReplyID =ReplyID
					IF (*ReplyID EQ 2) THEN BEGIN
					    IsNoMeteoData = 1B
					ENDIF ELSE BEGIN
					    IF (*ReplyID EQ 3) OR (*ReplyID EQ 4) OR (*ReplyID EQ 5) THEN BEGIN
					       PTR_FREE,ReplyID
					       RETURN
					    ENDIF
					ENDELSE
					PTR_FREE,ReplyID
					GOTO,Next
				ENDIF ELSE BEGIN   ;������жϻ����д��ڿ���.��Ϊ�����������ݱ�ӦΪ�յ�������0,
					NumSingle10 = N_ELEMENTS(SingleTendayData)
					Temp = WHERE(SelectFactorID LE (NumSingle10-1),Cout)
					IF Cout NE 0 THEN BEGIN
					   IF ARRAY_EQUAL(SingleTendayData[SelectFactorID[Temp]],'') THEN BEGIN
						  Num = Num+1
						  NoComputedID = [NoComputedID,i]
						  GOTO,Next
					   ENDIF
					ENDIF
				ENDELSE

				SatisfactyData = FLOAT(AllData0[SelectFactorID])

				CurrentYearMeteoYield=0.0
				FOR j=0,N_ELEMENTS(SelectFactorID)-1 DO BEGIN
				  CurrentYearMeteoYield=CurrentYearMeteoYield+Coefficency[j+1]*SatisfactyData[j]
				END

				CurrentYearMeteoYield=Coefficency[0]+CurrentYearMeteoYield     ;���ϳ�����,�õ�����Ĳ�������

         	;=====�������޸ģ�20070908=====================================
;			    Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $          ;�����������
;			         +DisCode+"' and crop_id='"+ CropId+ $       ;ԭ����
;			         "' and year="+CalcYear+' and ModelData_id=1'
;				Sqlstr2='insert into '+YieldTable+" values('"+DisCode+"','"+CropId+"'," $
;					+STRTRIM(CurrentYearMeteoYield,2)+','+CalcYear+',1)'

				Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�����������
			         +DisCode+"' and crop_id='"+ CropId+ $
			         "' and year="+CalcYear+' and ModelData_id='+ModelTypeId
				Sqlstr2='insert into '+YieldTable+" values('"+DisCode+"','"+CropId+"'," $
					+STRTRIM(CurrentYearMeteoYield,2)+','+CalcYear+','+ModelTypeId+')'

       		 ;=====================================================================
				DBobj->ExecuteSQL,Sqlstr1
				DBobj->ExecuteSQL,Sqlstr2
			END

			FactorType EQ 1 :BEGIN
				 IF YieldType EQ 0 THEN BEGIN
					ParaTable = "COUNTY_Fluctuate_yield_PARAMETER where crop_id='"
				 	YieldTable = 'COUNTY_Crop_MeteoOrFluctuate_yield'
					Scale = 'county_code'
					ModelTypeId = '3'
					EstimateTable = 'COUNTY_ESTIMATED_YIELD'
				 ENDIF ELSE BEGIN
					ParaTable = "AGROSTATION_Fluctuate_yield_PARAMETER where crop_id='"
				 	YieldTable = 'AGROSTATION_Crop_MeteoOrFluctuate_yield'
					Scale = 'AgroMeteoStation_CODE'
					ModelTypeId = '4'
					EstimateTable = 'AGROSTATION_ESTIMATED_YIELD'
				 ENDELSE

				SQL = 'select Rsdata_ID,Coefficient,phe_Start_month,phe_End_month,SENSOR_CODE from ' $
					 + ParaTable+CropId+"' and "+Scale+"='"+DisCode+"'"
				Para = DC_GetdataFromDB_Str(5 ,SQL,N_RECORDS = NumReocrd)   ;���ص���5��һ�е�����

				IF NumReocrd EQ 0 THEN BEGIN  ;����,��ΪĿ������û�н��в���ģ��,�޲���
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoTrendPara THEN GOTO,Next
					DC_Ask_YesOrNo,'���ݿ���û��"'+DisName+'"��Ӧ��ģ�����,���Ƚ��и��صĲ���ģ��,��������ļ�����!',EVENT.ID,ReplyID =ReplyID
					IF (*ReplyID EQ 2) THEN BEGIN
					    IsNoTrendPara = 1B
					ENDIF ELSE BEGIN
					    IF (*ReplyID EQ 3) OR (*ReplyID EQ 4) OR (*ReplyID EQ 5) THEN BEGIN
					       PTR_FREE,ReplyID
					       RETURN
					    ENDIF
					ENDELSE
					PTR_FREE,ReplyID
					GOTO,Next
				ENDIF

				MeteoTableIndex = FIX(Para[0,0])
				Coefficency = FLOAT(STRSPLIT(Para[1,0],'/',/EXTRACT))
				Sensor_type = Para[4,0]
				StartMonth = Para[2,0] & EndMonth = Para[3,0]

		   		names=['NPP','NPP','LAI','LAI','NDVI','NDVI']
				DataType = names[MeteoTableIndex]

				IsAvgOrSum = MeteoTableIndex MOD 2        ;IsAvgOrSumֻΪ0��1 ,0ʱȡƽ��,1ʱȡ�ۼ�.

				CropIDList = (*PA).CropIDList				;Crop��CropIDListӦ��Ӧ
				CropCol = ['AVG_SPRING_WHEAT','AVG_WINTER_WHEAT','AVG_EARLY_RICE','AVG_SEMILATE_RICE' $
						  ,'AVG_LATE_RICE','AVG_SPRING_MAIZE','AVG_SUMMER_MAIZE','AVG_SOYBEAN']				;Crop��CropIDListӦ��Ӧ
				CropFiled = CropCol[WHERE(CropIDList EQ CropId)]

				;������ݵ�ң������
				Data = DC_GetRsData(CalcYear,CalcYear,StartMonth,EndMonth,DisCode,CropFiled,IsAvgOrSum,Sensor_type,DataType,YieldType)
				IF ARRAY_EQUAL(FLOAT(Data),0.0) THEN BEGIN ;����,��Ϊû����Ӧ���������͵�����
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoRsData THEN GOTO,Next
					DC_Ask_YesOrNo,'���ݿ���û��"'+DisName+'"��Ӧ��ң������,��������ļ�����!',EVENT.ID,ReplyID =ReplyID
					IF (*ReplyID EQ 2) THEN BEGIN
					    IsNoRsData = 1B
					ENDIF ELSE BEGIN
					    IF (*ReplyID EQ 3) OR (*ReplyID EQ 4) OR (*ReplyID EQ 5) THEN BEGIN
					       PTR_FREE,ReplyID
					       RETURN
					    ENDIF
					ENDELSE
					PTR_FREE,ReplyID
					GOTO,Next
				ENDIF

				CurrentYearMeteoYield = Coefficency[0]+Coefficency[1]* FLOAT(Data)    ;���ϳ�����,�õ�����Ĳ�������

			;=====�������޸ģ�20070908=====================================
;			    Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�����������
;			         +DisCode+"' and crop_id='"+ CropId+ $	;ԭ����
;			         "' and year="+CalcYear+' and ModelData_id=2'				;�˴�Ϊ2
;				Sqlstr2='insert into '+YieldTable+" values('"+DisCode+"','"+CropId+"'," $
;					+STRTRIM(CurrentYearMeteoYield,2)+','+CalcYear+',2)'


				Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�����������
			         +DisCode+"' and crop_id='"+ CropId+ $
			         "' and year="+CalcYear+' and ModelData_id='+ModelTypeId				;�˴�Ϊ2
				Sqlstr2='insert into '+YieldTable+" values('"+DisCode+"','"+CropId+"'," $
					+STRTRIM(CurrentYearMeteoYield,2)+','+CalcYear+','+ModelTypeId+')'

			;====================================================================

				DBobj->ExecuteSQL,Sqlstr1
				DBobj->ExecuteSQL,Sqlstr2

			END

			ELSE:
		   ENDCASE

			IF YieldType THEN BEGIN
				Scale_code = 'AgroMeteoStation_CODE'
				TrendYieldTab = 'AGROSTATION_CROP_TREND_YIELD'
			ENDIF ELSE BEGIN
				Scale_code = 'COUNTY_CODE'
				TrendYieldTab = 'COUNTY_CROP_TREND_YIELD'
			ENDELSE
			SQL = 'select trend_Yield from '+TrendYieldTab+" where crop_id='"+CropId+"' and " $
				+Scale_code +"='"+DisCode+"' and year="+CalcYear
			Trendyield = DC_GetdataFromDB_Str(1,SQL,N_RECORDS = NumReocrd)	 ;char��

			IF NumReocrd EQ 0 THEN BEGIN   ;����,,��Ϊ��û�н������Ʋ�������
				Num = Num+1
				NoComputedID = [NoComputedID,i]
				IF IsNoTrendYield THEN GOTO,Next
				DC_Ask_YesOrNo,'���ݿ���û��"'+DisName+'"��Ӧ�Ĺ������Ʋ���,��������ļ�����!',EVENT.ID,ReplyID =ReplyID
				IF (*ReplyID EQ 2) THEN BEGIN
				    IsNoTrendYield = 1B
				ENDIF ELSE BEGIN
				    IF (*ReplyID EQ 3) OR (*ReplyID EQ 4) OR (*ReplyID EQ 5) THEN BEGIN
				       PTR_FREE,ReplyID
				       RETURN
				    ENDIF
				ENDELSE
				PTR_FREE,ReplyID
				GOTO,Next
			ENDIF

			Estimate = STRTRIM(FLOAT(Trendyield[0,0])+CurrentYearMeteoYield,2)
			Sqlstr5='delete from '+EstimateTable+' where '+Scale+"='"+DisCode $           ;����������
			     +"' and crop_id='"+ CropId+"' and year="+CalcYear+' and Model_type_id='+ModelTypeId
			Sqlstr6='insert into '+EstimateTable+" values('"+CropId+"','"+DisCode+"'," $
				+Estimate+','+ModelTypeId+','+CalcYear+')'

			DBobj->ExecuteSQL,Sqlstr5
			DBobj->ExecuteSQL,Sqlstr6

			IF YieldType EQ 0 THEN BEGIN
				TCoYield = STRARR(2,1)
				TCoYield[0,0] = DisCode
				TCoYield[1,0] = Estimate
				CountyYield = [[CountyYield],[TCoYield]]
			ENDIF

		  Next:
		ENDFOR

		IF Num NE 0 THEN BEGIN
			NoComputedID = NoComputedID[1:*]
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=NoComputedID
			INFO = DIALOG_MESSAGE(['������������!','�б��б�ѡ�е���Ϊû�в����������!'],/INFOR,TITLE='��ʾ')
			log, '����Ԥ��-��������', 0
		ENDIF ELSE BEGIN
			IF YieldType EQ 0 THEN BEGIN
				INFO = DIALOG_MESSAGE(['���������ɹ����!','��Ҫ���ع��������Ȩ��ʡ��?'],/QUESTION,TITLE='ѯ��')
				IF INFO EQ 'No' THEN RETURN
				CountyYield = CountyYield[*,1:*]
				Status = DC_WeightToPro(CountyYield,CropId,CalcYear,(*PA).ProID,ModelTypeId)
				IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('��Ȩ����ɹ�!',/INFORMATION,TITLE='��ʾ')

			ENDIF ELSE INFO = DIALOG_MESSAGE('���������ɹ����!',/INFOR,TITLE='��ʾ')
			log, '����Ԥ��-��������', 0
		ENDELSE

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='ParaBu'): BEGIN
		(*PA).IsCalPara = EVENT.SELECT

		DC_CleanFloatYield,EVENT.TOP
		IF (*PA).YieldType THEN  District = 'վ��' ELSE	 District = '��'

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='��ѡ��'+District
			WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
			WIDGET_CONTROL,(*PA).TableBase,/SENSITIVE
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).DistrictList,GET_UVALUE=DistrictCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =DistrictCode[0].CODE+' '+DistrictCode[0].NAME
				WIDGET_CONTROL,(*PA).DistrictList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,(*PA).BatchBU,/SENSITIVE
				WIDGET_CONTROL,(*PA).SaveTo,SENSITIVE=0
				WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE=CouttyValue
				WIDGET_CONTROL,(*PA).TableBase,SENSITIVE=0
		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='saveToCompute'): BEGIN

		WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE = YieldData,GET_UVALUE=YearName

       IF ARRAY_EQUAL(YieldData,'') THEN BEGIN
          Info=DIALOG_MESSAGE('��������û������!',/INFORMATION,TITLE='��ʾ')
          RETURN
       ENDIF

		WIDGET_CONTROL,(*PA).DistrictName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)
;		PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
		Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).cropID)],/RE) ;�õ�������
		IF (*PA).YieldType THEN BEGIN
;			PlaceCounty = PROVINCE+'-'+County[1]+'-'+Crop+'��������'
			PlaceCounty = County[1]+'-'+Crop+'��������'
;		ENDIF ELSE PlaceCounty = PROVINCE+'-'+County[1]+'-'+Crop+'��������'
		ENDIF ELSE PlaceCounty = County[1]+'-'+Crop+'��������'

		ActualYield = (*((*PA).YieldData))[*,0]
		ZeroId = WHERE(ActualYield EQ '',Num)
		IF Num NE 0 THEN BEGIN				;��һ����Ŀ���ǽ��þ�ֵ���������ݲ�����"*"����ʶ.
			 Temp = YieldData[0,*]
			 Temp[ZeroId] = Temp[ZeroId]+'*'
			 YieldData[0,*] = Temp
		ENDIF

		TOTAL_COL = 7      ;-----------------������
		TableHead=['����','����','����','���','ʵ�ʲ���','���Ʋ���','��������']
		D0_2 = {QQ:[County,Crop]}            ;����һ���ṹ��,�Ա�����ú���REPLICATE()
		DATA0_2=REPLICATE(D0_2,N_ELEMENTS(YearName))
		DATA3 =TRANSPOSE(STRMID(YearName,0,4))
		DATA4_6 = STRTRIM(YieldData,2)
		Data = [DATA0_2.QQ,DATA3,DATA4_6]

		SaveData=[[TableHead],[Data]]

		Temp = WHERE(SaveData EQ '',Num)
		IF Num NE 0 THEN SaveData[Temp] = '---'

		Filename=DIALOG_PICKFILE(TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
		  ,/OVERWRITE_PROMPT,FILE = PlaceCounty+'.txt',/WRITE,PATH=DC_PathSetting(),GET_PATH=SavePath, DIALOG_PARENT=Event.id)

		IF Filename EQ '' THEN RETURN

		path = DC_PathSetting(WRITEPATH1= SavePath)

		OPENW,LUN,Filename,/GET_LUN ,WIDTH=TOTAL_COL*(MAX(STRLEN(SaveData))+1)
		PRINTF,LUN,SaveData;,FORMAT='(5(A20,2X))'
		FREE_LUN,LUN

		Info=DIALOG_MESSAGE('����ɹ�!',TITLE='��ʾ',/INFORMATION)

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='NextBU'): BEGIN
		IF XREGISTERED('DC_FloatYield_2') NE 0 THEN BEGIN
			WIDGET_CONTROL,(*PA).NextTLB,/DESTROY
		ENDIF

 		WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE = YieldData,GET_UVALUE=YearName

       IF ARRAY_EQUAL(YieldData,'') THEN BEGIN
          Info=DIALOG_MESSAGE('��������û������!',/INFORMATION,TITLE='��ʾ')
          RETURN
       ENDIF
		;ע��˴���GrowthMonth���Ӧ�ĳɴ����ݿ�����ȡ
   		 GrowthMonth = (*PA).GrowthMonth  ;Ԫ��1��ֵ����������ʼ��,2�������,3���Ƿ����,4���������±�.
         CASE (*PA).CROPID OF
          '11' : GrowthMonth[0:2]=[4,7,0]
          '12' : GrowthMonth[0:2]=[11,5,1]
          '21' : GrowthMonth[0:2]=[3,6,0]
          '22' : GrowthMonth[0:2]=[5,8,0]
          '23' : GrowthMonth[0:2]=[7,9,0]
          '31' : GrowthMonth[0:2]=[4,7,0]
          '32' : GrowthMonth[0:2]=[5,8,0]
          '41' : GrowthMonth[0:2]=[5,8,0]
         ELSE  :
        ENDCASE
	    GrowthMonth[3]=N_ELEMENTS(YearName)-1
	    (*PA).GrowthMonth = GrowthMonth
	    ;-----------------------------------
		DC_FloatYield_2,PA,GROUP_LEADER=EVENT.ID,ITS_TLB=its_tlb
		(*PA).NextTLB = its_tlb
		log, '����Ԥ��-��������', 0
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '������������', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;	ONLINE_HELP,BOOK='HELP\HELP.CHM','������������'
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='quit_bu'): BEGIN
		common_log,'�رղ�����������'
	  	WIDGET_CONTROL,EVENT.TOP,/DESTROY
	END

  ELSE:
  ENDCASE

END

;PRO DC_CleanAllHeap,TLB
;;    WIDGET_CONTROL,TLB,GET_UVALUE=PA
;;    PTR_FREE,PA
;  	HEAP_GC,/VERBOSE
;END
;&&&&&&&&&&&&&&&&&&&&&&�����������������洰��:�����������%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO DC_FloatYield, GROUP_LEADER=wGroup

	common_log,'����������������'
	IF ( XREGISTERED('DC_FloatYield') NE 0 ) THEN RETURN

	TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
			,XOFFSET=250 ,YOFFSET=200,TITLE='������������', TLB_FRAME_ATTR=1 $
			,XPAD=1 ,YPAD=1,/COLUMN,TAB_MODE=1,SPACE=3);,/TLB_KILL_REQUEST_EVENTS)
	;------------------------------------------------------
	Condition = Widget_Base( TLB_BASE, UNAME='Condition_BASE'  $
			,XPAD=0 ,YPAD=1,/COLUMN,TAB_MODE=1,/BASE_ALIGN_LEFT,SPACE=3)
		ConWith = 455
		ConUpBase = Widget_Base( Condition, UNAME='ConUpBase' ,XPAD=0 ,YPAD=1 $
				,/ROW,TAB_MODE=1,/FRAME,SPACE=22,SCR_XSIZE=ConWith)
;			Province = ['������','�����','�ӱ�','ɽ��','���ɹ�','����','����','������','�Ϻ���','����' $
;						,'�㽭','��΢','����','����','ɽ��','����','����','����','�㶫','����','����' $
;						,'������','�Ĵ�','����','����','����','����','����','�ຣ','����','�½�']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			Crop = ['��С��','��С��','��  ��','��  ��','��  ��','������','������','��  ��']
		    CropIDList = ['11','12','21','22','23','31','32','41']				;Crop��CropIDListӦ��Ӧ
		    ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;��������ݱ仯,ϵͳ���б��е����Ҳ��仯.

;			ProDroplist  = Widget_Droplist(ConUpBase,UNAME='ProDroplist',TITLE='ʡ��:',SCR_XSIZE=100)   ;��/frame��frame=1����ͬ��
			CropDroplist = Widget_Droplist(ConUpBase,UNAME='CropDroplist',TITLE='����:',SCR_XSIZE=100)
			YearDroplist = Widget_Droplist(ConUpBase,UNAME='YearDroplist',TITLE='�������:',SCR_XSIZE=110)

			BatchBU = Widget_Button(ConUpBase,UNAME='BatchBU',SCR_XSIZE=60,SCR_YSIZE=20 $
									,VALUE='������',TOOLTIP='�����������صĲ�������')

		ConDownBase = Widget_Base(Condition,XPAD=0 ,YPAD=0,/ROW,TAB_MODE=1 $
								,/BASE_ALIGN_CENTER,SCR_YSIZE=28,SPACE=3,SCR_XSIZE=ConWith)

			Seperator1 = Widget_Base(ConDownBase,XPAD=0 ,YPAD=0,SCR_YSIZE=28,/FRAME,/ROW,/BASE_ALIGN_CENTER)
			YieldType = CW_BGROUP(Seperator1, UNAME='YieldType',['��','վ��'],/ROW,/EXCLUSIVE $
						,SPACE=0, LABEL_LEFT='��������:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=14,XSIZE=85)

			Seperator2 = Widget_Base(ConDownBase,XPAD=0 ,YPAD=0,SCR_YSIZE=28,/FRAME,/ROW,/BASE_ALIGN_CENTER)
			FactorType = CW_BGROUP(Seperator2, UNAME='FactorType',['��������','ң��ָ��'],/ROW,/EXCLUSIVE $
						,SPACE=0, LABEL_LEFT='��������:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=14,XSIZE=145)

			Seperator3 = Widget_Base(ConDownBase,XPAD=0 ,YPAD=0,SCR_XSIZE=87,SCR_YSIZE=28,/FRAME,/ROW,/BASE_ALIGN_CENTER)
			CheckParaBase = Widget_Base(Seperator3,XPAD=1,YPAD=0,/NONEXCLUSIVE)
			ParaBu = Widget_Button(CheckParaBase,UNAME='ParaBu',VALUE='��������',/ALIGN_CENTER)

	LargeBase = Widget_Base( TLB_BASE,XPAD=0,YPAD=0,SPACE=3,/BASE_ALIGN_TOP,/ROW)
	CountyBase = Widget_Base(LargeBase,XPAD=0,YPAD=0,/BASE_ALIGN_LEFT,/COLUMN,/FRAME,SCR_YSIZE=361)
		TitleBase = Widget_Base(CountyBase,XPAD=1,YPAD=1,/BASE_ALIGN_LEFT,/ROW,SPACE=0)
			TitleName = Widget_Label(TitleBase,UNAME='TitleName',VALUE='��:',SCR_XSIZE=30,/ALIGN_CENTER)
			DistrictName = Widget_Text(TitleBase,UNAME='DistrictName',XSIZE=15)
		DistrictList = Widget_List(CountyBase,UNAME='DistrictList',SCR_XSIZE=130,SCR_YSIZE=325,/MULTIPLE,/ALIGN_CENTER)
	;-------------------------�ұ�ģ�ⲿ��%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------------
	RigthBase = Widget_Base( LargeBase,UNAME='RigthBase'  $
	      ,SPACE=3 ,XPAD=0 ,YPAD=0,COLUMN=1,/BASE_ALIGN_RIGHT)

		TableBase = Widget_Base(RigthBase,UNAME='TableBase',SCR_YSIZE=361  $
		      ,SPACE=3 ,XPAD=0 ,YPAD=1,COLUMN=1,/BASE_ALIGN_LEFT,FRAME=1,SENSITIVE=0)

			TableTitle = Widget_Label(TableBase,UNAME='TableTitle',VALUE='�������ݱ�' $
					,/ALIGN_CENTER,SCR_XSIZE=200)
			TrendYield_TABLE = Widget_Table(TableBase,UNAME='TrendYield_TABLE',SCR_XSIZE=308 $
			      ,SCR_YSIZE=340  ,XSIZE=3 ,YSIZE=30,/FRAME,/DISJOINT_SELECTION)
		 ;---------------------------��ť���ڵ�BASE---------------------------------------
	    SAVE_BASE = Widget_Base(TLB_BASE, UNAME='SAVE_BASE',FRAME=1  $
	       		,SPACE=60,XPAD=30,YPAD=1 ,ROW=1,/BASE_ALIGN_CENTER,SCR_XSIZE=455)

		   Button_width=50 & Button_height=22 ;���尴ť��Ⱥ͸߶�
		   SaveTo=Widget_Base(SAVE_BASE,XPAD=0,YPAD=0,/ROW,SPACE=60,SENSITIVE=0)

			  saveToCompute=Widget_Button(SaveTo, UNAME='saveToCompute',VALUE='����' $
			         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height,TOOLTIP='���浽��������') ;$
			  NextBU =Widget_Button(SaveTo, UNAME='NextBU',VALUE='��һ��' $
			         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height,TOOLTIP='���㲨���������̲���'); $

		   Help_bu=Widget_Button(SAVE_BASE, UNAME='Help_bu',VALUE='����' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

		   quit_bu=Widget_Button(SAVE_BASE, UNAME='quit_bu',VALUE='�ر�' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

;-----------------------------------------------------------------------------
	Widget_Control, /REALIZE, TLB_BASE

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�
	CropID = '12';NewCropID
	CalcYear = '2006';strmid(systime(),3,4,/REVERSE_OFFSET)

	DC_FloatCountyOrSation,DistrictList,DistrictName,0,ProCode,CropID,NumReocrd = NumReocrd

	WIDGET_CONTROL,DistrictList,GET_UVALUE=DistrictCode
	IF NOT ARRAY_EQUAL(DistrictCode ,'',/NO_TYPECONV) THEN BEGIN
		WIDGET_CONTROL,DistrictList,SET_LIST_SELECT=0
		WIDGET_CONTROL,DistrictName,SET_VALUE=DistrictCode[0].CODE+' '+DistrictCode[0].NAME
	ENDIF

;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,CropDroplist,SET_VALUE =Crop,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)
	WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)

	WIDGET_CONTROL,TrendYield_TABLE,SET_TABLE_SELECT=[-1,-1]
	WIDGET_CONTROL,YieldType,SET_VALUE=0						  ;Ĭ�ϲ�������Ϊ�ز�����������
	WIDGET_CONTROL,FactorType,SET_VALUE=0                         ;����Ĭ�ϵ�����Ϊ"��������"����
	WIDGET_CONTROL,quit_bu,/INPUT_FOCUS


   STATE = { $
;            ProNameList			:	Province		,$				;ʡ���б�
            ProIDList			:	ProIDList		,$				;ʡID�б�
            ProID				:	ProCode			,$				;��ѡʡID
            CropIDList			:	CropIDList		,$				;����ID�б�
            CropNameList		:	Crop			,$      		;�������б�
            CropID				:	CropID			,$
            CalcYear			:	CalcYear		,$
            IsBlankList  	    :   NumReocrd EQ 0 	,$         		;��ʶList���ֵ�Ƿ�Ϊ��,1Ϊ��
            IsCalPara  		    :   0        	 	,$         		;��ʶ�Ƿ�Ҫ���в�������
         	YieldType			:	0				,$				;��ʶ���ػ���վ�����
         	FactorType			:	0    			,$				;ָ���������������ң��ָ������
			YearHaveData		:	PTR_NEW()		,$				;��ʶ��ѡ����Ӧ���������ƺͲ����������������.
			YieldData			:	PTR_NEW()		,$				;��ʶ�ӿ�����ȡ��ʵ�ʺ����Ʋ���.2��
         	BatchBU				:	BatchBU			,$				;������ť
         	TableBase			:	TableBase		,$
         	SaveTo				:	SaveTo			,$				;��һ����ť
			DistrictList		:	DistrictList	,$
			TitleName			:	TitleName		,$
			DistrictName		:	DistrictName	,$
			TableTitle			:	TableTitle		,$
         	TrendYield_TABLE	:	TrendYield_TABLE,$
         	ARRAY_YEAR			: 	ARRAY_YEAR  	,$				;�������DROPlist���õ��������
			GrowthMonth			:	Intarr(4) 		,$				;��¼������,Ԫ��1:����������ʼ��,2�������,3���Ƿ����,4��������±�.
            NextTLB				:	0L				 $				;��¼"��һ��"�������ڵĶ���BASE��ID
            }

    PA = PTR_NEW(STATE, /NO_COPY)

    WIDGET_CONTROL, TLB_BASE, SET_UVALUE=PA

    XManager, 'DC_FloatYield', TLB_BASE, CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
