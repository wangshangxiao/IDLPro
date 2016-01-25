;波动产量计算模块

;**自定义过程:提取县相应作物的各产量(其中没有产量的年份以空值来替代)*********************
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
			ActualYield = [ActualYield,TRANSPOSE(DC_GetdataFromDB_Str(1,Sqlstr))]   ;每次DC_GetdataFromDB_Str(1,Sqlstr)只得到一个数据.
		ENDFOR
		ActualYield = ActualYield[1:*]          ;历史实际产量

		FOR I=StartYear_,EndYear_ DO BEGIN
			Sqlstr='select trend_yield from '+TrendTable +CropID+Join+DistrictCode+"' and year="+STRTRIM(I,2)
			TrendYield = [TrendYield,TRANSPOSE(DC_GetdataFromDB_Str(1,Sqlstr))]   ;每次DC_GetdataFromDB_Str(1,Sqlstr)只得到一个数据.
		ENDFOR
		TrendYield = TrendYield[1:*]			;历史趋势产量

		YieldData = [[ActualYield],[TrendYield]]

		RETURN,YieldData           ;此处最终返回是二行数据.字符型.

END

;**自定义过程:依据省和作物提取有作物的县或站点的代码********************************
PRO DC_FloatCountyOrSation	,ListWiget     $	;列表list组件ID
							,NameTextWidgt $	;文本Text组件ID.
							,YieldType     $    ;标识是县还是站点产量类型
							,ProCode	   $	;省ID
							,CropID		   $	;作物ID
							,NumReocrd = NumReocrd  ;得到记录数
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
	ENDIF ELSE BEGIN      ;没有满足条件的查询
		WIDGET_CONTROL,ListWiget,SET_VALUE='',SET_UVALUE=''
		WIDGET_CONTROL,NameTextWidgt,SET_VALUE=''
	ENDELSE

END
;**自定义过程:清空相应组件的值********************************
PRO DC_CleanFloatYield,TLB

	WIDGET_CONTROL,TLB,GET_UVALUE=PA

	WIDGET_CONTROL,(*PA).TableTitle,SET_VALUE='产量数据表'

	WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE=TRYield & TRYield[*,*] = ''
	WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_VALUE=TRYield $
				  ,BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

END
;888888888888888888888主界面的部分事件处理8888888888888888888888888888888888888888888888888888
PRO DC_FloatYield_event, Event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  widget_info(Event.id, /trLee_root) : event.id)

;        CATCH, Error_status               ;截取错误.
;     IF Error_status NE 0 THEN BEGIN
;        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
;        CATCH, /CANCEL
;        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
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

		IF (*PA).YieldType THEN  District = '站点' ELSE	 District = '县'

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='请选择'+District
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
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;得到省名
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;得到作物名
;			INFO = DIALOG_MESSAGE('数据库中没有"'+PROVINCE+Crop+'"产量数据',TITLE='提示',/INFOR)
			INFO = DIALOG_MESSAGE('数据库中没有"'+Crop+'"产量数据',TITLE='提示',/INFOR)
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CropDroplist'): BEGIN

		 (*PA).CropID = (*PA).CropIDList[EVENT.INDEX]

		  DC_CleanFloatYield,EVENT.TOP
		  DC_FloatCountyOrSation,(*PA).DistrictList,(*PA).DistrictName,(*PA).YieldType $
		  						  ,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd

		(*PA).IsBlankList = NumReocrd EQ 0

		IF (*PA).YieldType THEN  District = '站点' ELSE	 District = '县'

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='请选择'+District
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
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;得到省名
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;得到作物名
;			INFO = DIALOG_MESSAGE('数据库中没有"'+PROVINCE+Crop+'"产量数据',TITLE='提示',/INFOR)
			INFO = DIALOG_MESSAGE('数据库中没有"'+Crop+'"产量数据',TITLE='提示',/INFOR)
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
				INFO = DIALOG_MESSAGE('列表中没有可选择县!',/INFO,TITLE='提示')
			ENDIF ELSE 	INFO = DIALOG_MESSAGE('列表中没有可选择站点!',/INFO,TITLE='提示')

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
		   		INFO = DIALOG_MESSAGE('您还没有对"'+District[EVENT.INDEX].NAME+'"进行产量趋势分析!',TITLE='警告')
		   	ENDIF ELSE BEGIN
		   		INFO = DIALOG_MESSAGE('您还没有对"'+District[EVENT.INDEX].NAME+'站"进行产量趋势分析!',TITLE='警告')
		   	ENDELSE
		   RETURN
		ENDIF

		StartYear = STRTRIM(YearThreshold.(0),2)
		EndYear = STRTRIM(YearThreshold.(1),2)
		YearNum = FIX(EndYear)-FIX(StartYear)+1

		YearHaveData = STRTRIM(INDGEN(YearNum)+FIX(StartYear),2)
		RowLabels = YearHaveData+'年'

		PTR_FREE,(*PA).YearHaveData
		(*PA).YearHaveData = PTR_NEW(YearHaveData,/NO_COPY)

		;下面的YieldData为字符型,两行数据
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
		ColumnLabels = ['实际产量','趋势产量','波动产量']
		WIDGET_CONTROL,(*PA).TrendYield_TABLE,TABLE_YSIZE=YearNum $
		            ,SET_UVALUE=RowLabels $   ;这里之所以设用户值,是为保存数据时"保存年份".
		            ,ROW_LABELS=RowLabels  ,COLUMN_LABELS=ColumnLabels $
		            ,SET_VALUE=TableYield  ,ALIGNMENT=2,COLUMN_WIDTHS=65

		WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=ZeroCell,SET_TABLE_VIEW = ZeroCell[*,0] $
					,BACKGROUND_COLOR  = [255,255,0]     			 ;将实际年份产量为0(但以平均值来替换的年份)的单元格填成黄色警示.
;		WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_TABLE_SELECT=[-1,-1]  ;两次对表WIDGET_CONTROL,是为让黄色自动弹到视线中.


		IF (*PA).YieldType EQ 0 THEN BEGIN
	   		INFO = District[EVENT.INDEX].NAME+'产量数据表'
	   	ENDIF ELSE INFO = District[EVENT.INDEX].NAME+'站产量数据表'

		WIDGET_CONTROL,(*PA).TableTitle,SET_VALUE=INFO

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='YieldType'): BEGIN
		IF NOT EVENT.SELECT THEN RETURN

 		(*PA).YieldType = EVENT.VALUE

		IF (*PA).YieldType THEN  District = '站点' ELSE	 District = '县'

		WIDGET_CONTROL,(*PA).TitleName,SET_VALUE =District+':'

		DC_CleanFloatYield,EVENT.TOP
		DC_FloatCountyOrSation,(*PA).DistrictList,(*PA).DistrictName,(*PA).YieldType $
		  						,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd


		(*PA).IsBlankList = NumReocrd EQ 0

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='请选择'+District
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
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;得到省名
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;得到作物名
;			INFO = DIALOG_MESSAGE('数据库中没有"'+PROVINCE+Crop+'"产量数据',/INFOR,TITLE='提示')
			INFO = DIALOG_MESSAGE('数据库中没有"'+Crop+'"产量数据',/INFOR,TITLE='提示')
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

	WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU'): BEGIN   ;批处理事件
		IF (*PA).IsBlankList THEN BEGIN
			INFO = DIALOG_MESSAGE('左边的列表为空,没有可选择计算的项!',/INFOR,TITLE='提示')
			RETURN
		ENDIF

		COMMON COMMON_BLOCK

		CropId = (*PA).CROPID
		CalcYear = (*PA).CalcYear
		WIDGET_CONTROL,(*PA).DistrictList,GET_UVALUE=District   ;注意District是结构体数组
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


;====调试添加，20070903，杨绍锷============================================================
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

					IF NumReocrd EQ 0 THEN BEGIN   ;跳过,因为在泰森多边形表中没有与县相应的站点
						Num = Num+1
						NoComputedID = [NoComputedID,i]
						IF IsNostation THEN GOTO,Next
						DC_Ask_YesOrNo,'数据库表中没有与"'+DisName+'"相应的气象站点,继续下面的计算吗!',EVENT.ID,ReplyID =ReplyID
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

;				;====调试添加，20070903，杨绍锷============================================================
;				SQL_y = 'select Coefficient,FactorSel,Factor_position,phe_Start_month,phe_End_month from ' $
;					 + ParaTable+CropId+"' and "+Scale+"='360123'"
;				Para = DC_GetdataFromDB_Str(5 ,SQL_y,N_RECORDS = NumReocrd)
				;================================================================

				Para = DC_GetdataFromDB_Str(5 ,SQL,N_RECORDS = NumReocrd)   ;返回的是5列一行的数组
				IF NumReocrd EQ 0 THEN BEGIN   ;跳过,,因为还没有进行参数模拟,无参数
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoTrendPara THEN GOTO,Next
					DC_Ask_YesOrNo,'数据库中没有"'+DisName+'"相应的模拟参数,请先进行该县的参数模拟,继续下面的计算吗!',EVENT.ID,ReplyID =ReplyID
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
				StrideYear = FIX(StartMonth) GT FIX(EndMonth)    		  ;判断是否跨年
				IF (StrideYear EQ 0) THEN BEGIN ;下面用到的函数见"DC_FloatYield_2"Line 125
				    SingleTendayNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3   ;单旬的个数,不分组合旬波动因子种类
				    AllData0=DC_SingleTendayFactor(CalcYear,CalcYear,StartMonth,EndMonth,MeteoTableIndex,station_code,YieldType)
				    SingleTendayData = AllData0     ;单旬数据以用于下面的判断
				    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,StartMonth,EndMonth)
				    AllData0=[AllData0,[CombinationData]]                                         ;这是最终的波动数据.
				ENDIF ELSE BEGIN                                           ;注意冬小麦是跨年的,所以起始和结束月大小不能直接用在两个函数中.
				    Lastyear=STRTRIM(FIX(CalcYear)-1,2)                    ;对于冬小麦,生长期从上年11月开始的.所以用Lastyear
				    SingleTendayNum = (13+FIX(EndMonth)-FIX(StartMonth))*3
				    AllData1=DC_SingleTendayFactor(Lastyear,Lastyear,StartMonth,'12',MeteoTableIndex,station_code,YieldType)
				    AllData2=DC_SingleTendayFactor(Fix(Lastyear)+1,Fix(Lastyear)+1,'1',EndMonth,MeteoTableIndex,station_code,YieldType)
				    AllData0=[AllData1,[AllData2]]
				    SingleTendayData = AllData0     ;单旬数据以用于下面的判断
				    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,1,13+FIX(EndMonth)-FIX(StartMonth))   ;这里的两个参数,不是起始月和结束月,是因为此
				    AllData0=[AllData0,[CombinationData]]                                                               ;定义函数用这两参数只是想得到月数,所以可以这样赋参数.
				ENDELSE

				IF ARRAY_EQUAL(SingleTendayData,'') THEN BEGIN   ;跳过,因为没有单旬气象因子数据
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoMeteoData THEN GOTO,Next
					DC_Ask_YesOrNo,'数据库中没有"'+DisName+'"相应的气象数据,继续下面的计算吗!',EVENT.ID,ReplyID =ReplyID
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
				ENDIF ELSE BEGIN   ;下面的判断还是有待于考虑.因为复合气象数据本应为空的数据是0,
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

				CurrentYearMeteoYield=Coefficency[0]+CurrentYearMeteoYield     ;加上常数项,得到当年的波动产量

         	;=====杨绍锷修改，20070908=====================================
;			    Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $          ;波动产量入库
;			         +DisCode+"' and crop_id='"+ CropId+ $       ;原代码
;			         "' and year="+CalcYear+' and ModelData_id=1'
;				Sqlstr2='insert into '+YieldTable+" values('"+DisCode+"','"+CropId+"'," $
;					+STRTRIM(CurrentYearMeteoYield,2)+','+CalcYear+',1)'

				Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;波动产量入库
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
				Para = DC_GetdataFromDB_Str(5 ,SQL,N_RECORDS = NumReocrd)   ;返回的是5列一行的数组

				IF NumReocrd EQ 0 THEN BEGIN  ;跳过,因为目标区域还没有进行参数模拟,无参数
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoTrendPara THEN GOTO,Next
					DC_Ask_YesOrNo,'数据库中没有"'+DisName+'"相应的模拟参数,请先进行该县的参数模拟,继续下面的计算吗!',EVENT.ID,ReplyID =ReplyID
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

				IsAvgOrSum = MeteoTableIndex MOD 2        ;IsAvgOrSum只为0或1 ,0时取平均,1时取累计.

				CropIDList = (*PA).CropIDList				;Crop与CropIDList应对应
				CropCol = ['AVG_SPRING_WHEAT','AVG_WINTER_WHEAT','AVG_EARLY_RICE','AVG_SEMILATE_RICE' $
						  ,'AVG_LATE_RICE','AVG_SPRING_MAIZE','AVG_SUMMER_MAIZE','AVG_SOYBEAN']				;Crop与CropIDList应对应
				CropFiled = CropCol[WHERE(CropIDList EQ CropId)]

				;计算年份的遥感数据
				Data = DC_GetRsData(CalcYear,CalcYear,StartMonth,EndMonth,DisCode,CropFiled,IsAvgOrSum,Sensor_type,DataType,YieldType)
				IF ARRAY_EQUAL(FLOAT(Data),0.0) THEN BEGIN ;跳过,因为没有相应传感器类型的数据
					Num = Num+1
					NoComputedID = [NoComputedID,i]
					IF IsNoRsData THEN GOTO,Next
					DC_Ask_YesOrNo,'数据库中没有"'+DisName+'"相应的遥感数据,继续下面的计算吗!',EVENT.ID,ReplyID =ReplyID
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

				CurrentYearMeteoYield = Coefficency[0]+Coefficency[1]* FLOAT(Data)    ;加上常数项,得到当年的波动产量

			;=====杨绍锷修改，20070908=====================================
;			    Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;波动产量入库
;			         +DisCode+"' and crop_id='"+ CropId+ $	;原代码
;			         "' and year="+CalcYear+' and ModelData_id=2'				;此处为2
;				Sqlstr2='insert into '+YieldTable+" values('"+DisCode+"','"+CropId+"'," $
;					+STRTRIM(CurrentYearMeteoYield,2)+','+CalcYear+',2)'


				Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;波动产量入库
			         +DisCode+"' and crop_id='"+ CropId+ $
			         "' and year="+CalcYear+' and ModelData_id='+ModelTypeId				;此处为2
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
			Trendyield = DC_GetdataFromDB_Str(1,SQL,N_RECORDS = NumReocrd)	 ;char型

			IF NumReocrd EQ 0 THEN BEGIN   ;跳过,,因为还没有进行趋势产量计算
				Num = Num+1
				NoComputedID = [NoComputedID,i]
				IF IsNoTrendYield THEN GOTO,Next
				DC_Ask_YesOrNo,'数据库中没有"'+DisName+'"相应的估算趋势产量,继续下面的计算吗!',EVENT.ID,ReplyID =ReplyID
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
			Sqlstr5='delete from '+EstimateTable+' where '+Scale+"='"+DisCode $           ;估算产量入库
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
			INFO = DIALOG_MESSAGE(['批处理计算完成!','列表中被选中的县为没有参数计算的县!'],/INFOR,TITLE='提示')
			log, '单产预测-波动产量', 0
		ENDIF ELSE BEGIN
			IF YieldType EQ 0 THEN BEGIN
				INFO = DIALOG_MESSAGE(['批处理计算成功完成!','需要将县估算产量加权到省吗?'],/QUESTION,TITLE='询问')
				IF INFO EQ 'No' THEN RETURN
				CountyYield = CountyYield[*,1:*]
				Status = DC_WeightToPro(CountyYield,CropId,CalcYear,(*PA).ProID,ModelTypeId)
				IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('加权计算成功!',/INFORMATION,TITLE='提示')

			ENDIF ELSE INFO = DIALOG_MESSAGE('批处理计算成功完成!',/INFOR,TITLE='提示')
			log, '单产预测-波动产量', 0
		ENDELSE

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='ParaBu'): BEGIN
		(*PA).IsCalPara = EVENT.SELECT

		DC_CleanFloatYield,EVENT.TOP
		IF (*PA).YieldType THEN  District = '站点' ELSE	 District = '县'

		IF (*PA).IsCalPara THEN BEGIN
			WIDGET_CONTROL,(*PA).BatchBU,SENSITIVE=0
			WIDGET_CONTROL,(*PA).SaveTo,/SENSITIVE
			WIDGET_CONTROL,(*PA).TitleName,SET_VALUE = District+':'
			WIDGET_CONTROL,(*PA).DistrictName,SET_VALUE='请选择'+District
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
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;得到省名
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;得到作物名
;			INFO = DIALOG_MESSAGE('数据库中没有"'+PROVINCE+Crop+'"产量数据',/INFOR,TITLE='提示')
			INFO = DIALOG_MESSAGE('数据库中没有"'+Crop+'"产量数据',/INFOR,TITLE='提示')
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='saveToCompute'): BEGIN

		WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE = YieldData,GET_UVALUE=YearName

       IF ARRAY_EQUAL(YieldData,'') THEN BEGIN
          Info=DIALOG_MESSAGE('产量表中没有数据!',/INFORMATION,TITLE='提示')
          RETURN
       ENDIF

		WIDGET_CONTROL,(*PA).DistrictName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)
;		PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;得到省名
		Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).cropID)],/RE) ;得到作物名
		IF (*PA).YieldType THEN BEGIN
;			PlaceCounty = PROVINCE+'-'+County[1]+'-'+Crop+'产量数据'
			PlaceCounty = County[1]+'-'+Crop+'产量数据'
;		ENDIF ELSE PlaceCounty = PROVINCE+'-'+County[1]+'-'+Crop+'产量数据'
		ENDIF ELSE PlaceCounty = County[1]+'-'+Crop+'产量数据'

		ActualYield = (*((*PA).YieldData))[*,0]
		ZeroId = WHERE(ActualYield EQ '',Num)
		IF Num NE 0 THEN BEGIN				;这一步的目的是将用均值来替代的年份产量以"*"来标识.
			 Temp = YieldData[0,*]
			 Temp[ZeroId] = Temp[ZeroId]+'*'
			 YieldData[0,*] = Temp
		ENDIF

		TOTAL_COL = 7      ;-----------------总列数
		TableHead=['代码','名称','作物','年份','实际产量','趋势产量','波动产量']
		D0_2 = {QQ:[County,Crop]}            ;定义一个结构体,以便可以用函数REPLICATE()
		DATA0_2=REPLICATE(D0_2,N_ELEMENTS(YearName))
		DATA3 =TRANSPOSE(STRMID(YearName,0,4))
		DATA4_6 = STRTRIM(YieldData,2)
		Data = [DATA0_2.QQ,DATA3,DATA4_6]

		SaveData=[[TableHead],[Data]]

		Temp = WHERE(SaveData EQ '',Num)
		IF Num NE 0 THEN SaveData[Temp] = '---'

		Filename=DIALOG_PICKFILE(TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
		  ,/OVERWRITE_PROMPT,FILE = PlaceCounty+'.txt',/WRITE,PATH=DC_PathSetting(),GET_PATH=SavePath, DIALOG_PARENT=Event.id)

		IF Filename EQ '' THEN RETURN

		path = DC_PathSetting(WRITEPATH1= SavePath)

		OPENW,LUN,Filename,/GET_LUN ,WIDTH=TOTAL_COL*(MAX(STRLEN(SaveData))+1)
		PRINTF,LUN,SaveData;,FORMAT='(5(A20,2X))'
		FREE_LUN,LUN

		Info=DIALOG_MESSAGE('保存成功!',TITLE='提示',/INFORMATION)

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='NextBU'): BEGIN
		IF XREGISTERED('DC_FloatYield_2') NE 0 THEN BEGIN
			WIDGET_CONTROL,(*PA).NextTLB,/DESTROY
		ENDIF

 		WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE = YieldData,GET_UVALUE=YearName

       IF ARRAY_EQUAL(YieldData,'') THEN BEGIN
          Info=DIALOG_MESSAGE('产量表中没有数据!',/INFORMATION,TITLE='提示')
          RETURN
       ENDIF
		;注意此处的GrowthMonth今后应改成从数据库中提取
   		 GrowthMonth = (*PA).GrowthMonth  ;元素1的值表生长期起始月,2表结束月,3表是否跨年,4表结束年的下标.
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
		log, '单产预测-波动产量', 0
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '波动产量分析', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;	ONLINE_HELP,BOOK='HELP\HELP.CHM','波动产量分析'
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='quit_bu'): BEGIN
		common_log,'关闭产量参数计算'
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
;&&&&&&&&&&&&&&&&&&&&&&波动产量分析主界面窗口:参数计算分析%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO DC_FloatYield, GROUP_LEADER=wGroup

	common_log,'启动产量参数计算'
	IF ( XREGISTERED('DC_FloatYield') NE 0 ) THEN RETURN

	TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
			,XOFFSET=250 ,YOFFSET=200,TITLE='波动产量计算', TLB_FRAME_ATTR=1 $
			,XPAD=1 ,YPAD=1,/COLUMN,TAB_MODE=1,SPACE=3);,/TLB_KILL_REQUEST_EVENTS)
	;------------------------------------------------------
	Condition = Widget_Base( TLB_BASE, UNAME='Condition_BASE'  $
			,XPAD=0 ,YPAD=1,/COLUMN,TAB_MODE=1,/BASE_ALIGN_LEFT,SPACE=3)
		ConWith = 455
		ConUpBase = Widget_Base( Condition, UNAME='ConUpBase' ,XPAD=0 ,YPAD=1 $
				,/ROW,TAB_MODE=1,/FRAME,SPACE=22,SCR_XSIZE=ConWith)
;			Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
;						,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
;						,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			Crop = ['春小麦','冬小麦','早  稻','中  稻','晚  稻','春玉米','夏玉米','大  豆']
		    CropIDList = ['11','12','21','22','23','31','32','41']				;Crop与CropIDList应对应
		    ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;这样随年份变化,系统下列表中的年份也会变化.

;			ProDroplist  = Widget_Droplist(ConUpBase,UNAME='ProDroplist',TITLE='省名:',SCR_XSIZE=100)   ;用/frame与frame=1是相同的
			CropDroplist = Widget_Droplist(ConUpBase,UNAME='CropDroplist',TITLE='作物:',SCR_XSIZE=100)
			YearDroplist = Widget_Droplist(ConUpBase,UNAME='YearDroplist',TITLE='计算年份:',SCR_XSIZE=110)

			BatchBU = Widget_Button(ConUpBase,UNAME='BatchBU',SCR_XSIZE=60,SCR_YSIZE=20 $
									,VALUE='批处理',TOOLTIP='批处理计算各县的波动产量')

		ConDownBase = Widget_Base(Condition,XPAD=0 ,YPAD=0,/ROW,TAB_MODE=1 $
								,/BASE_ALIGN_CENTER,SCR_YSIZE=28,SPACE=3,SCR_XSIZE=ConWith)

			Seperator1 = Widget_Base(ConDownBase,XPAD=0 ,YPAD=0,SCR_YSIZE=28,/FRAME,/ROW,/BASE_ALIGN_CENTER)
			YieldType = CW_BGROUP(Seperator1, UNAME='YieldType',['县','站点'],/ROW,/EXCLUSIVE $
						,SPACE=0, LABEL_LEFT='产量类型:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=14,XSIZE=85)

			Seperator2 = Widget_Base(ConDownBase,XPAD=0 ,YPAD=0,SCR_YSIZE=28,/FRAME,/ROW,/BASE_ALIGN_CENTER)
			FactorType = CW_BGROUP(Seperator2, UNAME='FactorType',['气象因子','遥感指数'],/ROW,/EXCLUSIVE $
						,SPACE=0, LABEL_LEFT='因子类型:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=14,XSIZE=145)

			Seperator3 = Widget_Base(ConDownBase,XPAD=0 ,YPAD=0,SCR_XSIZE=87,SCR_YSIZE=28,/FRAME,/ROW,/BASE_ALIGN_CENTER)
			CheckParaBase = Widget_Base(Seperator3,XPAD=1,YPAD=0,/NONEXCLUSIVE)
			ParaBu = Widget_Button(CheckParaBase,UNAME='ParaBu',VALUE='参数计算',/ALIGN_CENTER)

	LargeBase = Widget_Base( TLB_BASE,XPAD=0,YPAD=0,SPACE=3,/BASE_ALIGN_TOP,/ROW)
	CountyBase = Widget_Base(LargeBase,XPAD=0,YPAD=0,/BASE_ALIGN_LEFT,/COLUMN,/FRAME,SCR_YSIZE=361)
		TitleBase = Widget_Base(CountyBase,XPAD=1,YPAD=1,/BASE_ALIGN_LEFT,/ROW,SPACE=0)
			TitleName = Widget_Label(TitleBase,UNAME='TitleName',VALUE='县:',SCR_XSIZE=30,/ALIGN_CENTER)
			DistrictName = Widget_Text(TitleBase,UNAME='DistrictName',XSIZE=15)
		DistrictList = Widget_List(CountyBase,UNAME='DistrictList',SCR_XSIZE=130,SCR_YSIZE=325,/MULTIPLE,/ALIGN_CENTER)
	;-------------------------右边模拟部分%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------------
	RigthBase = Widget_Base( LargeBase,UNAME='RigthBase'  $
	      ,SPACE=3 ,XPAD=0 ,YPAD=0,COLUMN=1,/BASE_ALIGN_RIGHT)

		TableBase = Widget_Base(RigthBase,UNAME='TableBase',SCR_YSIZE=361  $
		      ,SPACE=3 ,XPAD=0 ,YPAD=1,COLUMN=1,/BASE_ALIGN_LEFT,FRAME=1,SENSITIVE=0)

			TableTitle = Widget_Label(TableBase,UNAME='TableTitle',VALUE='产量数据表' $
					,/ALIGN_CENTER,SCR_XSIZE=200)
			TrendYield_TABLE = Widget_Table(TableBase,UNAME='TrendYield_TABLE',SCR_XSIZE=308 $
			      ,SCR_YSIZE=340  ,XSIZE=3 ,YSIZE=30,/FRAME,/DISJOINT_SELECTION)
		 ;---------------------------按钮所在的BASE---------------------------------------
	    SAVE_BASE = Widget_Base(TLB_BASE, UNAME='SAVE_BASE',FRAME=1  $
	       		,SPACE=60,XPAD=30,YPAD=1 ,ROW=1,/BASE_ALIGN_CENTER,SCR_XSIZE=455)

		   Button_width=50 & Button_height=22 ;定义按钮宽度和高度
		   SaveTo=Widget_Base(SAVE_BASE,XPAD=0,YPAD=0,/ROW,SPACE=60,SENSITIVE=0)

			  saveToCompute=Widget_Button(SaveTo, UNAME='saveToCompute',VALUE='保存' $
			         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height,TOOLTIP='保存到本机磁盘') ;$
			  NextBU =Widget_Button(SaveTo, UNAME='NextBU',VALUE='下一步' $
			         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height,TOOLTIP='计算波动产量方程参数'); $

		   Help_bu=Widget_Button(SAVE_BASE, UNAME='Help_bu',VALUE='帮助' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

		   quit_bu=Widget_Button(SAVE_BASE, UNAME='quit_bu',VALUE='关闭' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

;-----------------------------------------------------------------------------
	Widget_Control, /REALIZE, TLB_BASE

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
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
	WIDGET_CONTROL,YieldType,SET_VALUE=0						  ;默认产量类型为县波动产量数据
	WIDGET_CONTROL,FactorType,SET_VALUE=0                         ;设置默认的类型为"气象因子"数据
	WIDGET_CONTROL,quit_bu,/INPUT_FOCUS


   STATE = { $
;            ProNameList			:	Province		,$				;省名列表
            ProIDList			:	ProIDList		,$				;省ID列表
            ProID				:	ProCode			,$				;被选省ID
            CropIDList			:	CropIDList		,$				;作物ID列表
            CropNameList		:	Crop			,$      		;作物名列表
            CropID				:	CropID			,$
            CalcYear			:	CalcYear		,$
            IsBlankList  	    :   NumReocrd EQ 0 	,$         		;标识List组件值是否为空,1为空
            IsCalPara  		    :   0        	 	,$         		;标识是否要进行参数计算
         	YieldType			:	0				,$				;标识是县还是站点产量
         	FactorType			:	0    			,$				;指明因子是用气象或遥感指数因子
			YearHaveData		:	PTR_NEW()		,$				;标识所选县相应作物有趋势和波动产量的数据年份.
			YieldData			:	PTR_NEW()		,$				;标识从库中提取的实际和趋势产量.2行
         	BatchBU				:	BatchBU			,$				;批处理按钮
         	TableBase			:	TableBase		,$
         	SaveTo				:	SaveTo			,$				;下一步按钮
			DistrictList		:	DistrictList	,$
			TitleName			:	TitleName		,$
			DistrictName		:	DistrictName	,$
			TableTitle			:	TableTitle		,$
         	TrendYield_TABLE	:	TrendYield_TABLE,$
         	ARRAY_YEAR			: 	ARRAY_YEAR  	,$				;计算年份DROPlist所用的数据年份
			GrowthMonth			:	Intarr(4) 		,$				;记录生长期,元素1:表生长期起始月,2表结束月,3表是否跨年,4表结束年下标.
            NextTLB				:	0L				 $				;记录"下一步"弹出窗口的顶级BASE的ID
            }

    PA = PTR_NEW(STATE, /NO_COPY)

    WIDGET_CONTROL, TLB_BASE, SET_UVALUE=PA

    XManager, 'DC_FloatYield', TLB_BASE, CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
