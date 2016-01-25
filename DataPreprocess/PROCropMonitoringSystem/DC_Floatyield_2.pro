;筛选波动产量因子

;******自定义过程:清除原来的数据*********************************
 PRO DC_ClearOldData,EventTop
   Widget_Control,EventTop,GET_UVALUE = SelectFactor

     Widget_Control,(*SelectFactor).AgorFactor_TABLE,GET_VALUE=MetoYield & MetoYield[*]=''
     Widget_Control,(*SelectFactor).AgorFactor_TABLE,SET_VALUE=MetoYield  $
                    ,COLUMN_LABELS=[''],ROW_LABELS=[''],BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

     Widget_Control,(*SelectFactor).factor_TABLE,GET_VALUE=Factor & Factor[*]=''
     Widget_Control,(*SelectFactor).factor_TABLE,SET_VALUE=Factor $
                   ,COLUMN_LABELS=[''],BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

     Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='因子筛选表'
   Widget_Control,EventTop,SET_UVALUE = SelectFactor
 END
;***********************"筛选因子"按钮事件*************************************
PRO DC_FilterFactor,EVENT

		 CATCH, Error_status
		IF Error_status NE 0 THEN BEGIN
		   infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
		   CATCH, /CANCEL
		   RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
		ENDIF
	WIDGET_CONTROL, /HOURGLASS                             ;执行该事件时指针处于等待状态,直到结束恢复.

	DC_ClearOldData,Event.top
	Widget_Control,Event.top,GET_UVALUE = SelectFactor
	Widget_Control,(*SelectFactor).threshold_TEXT,GET_VALUE =Threshold

	ThresholdValue = DC_JudgeInputChar(Threshold[0],Desc='阀值文本框')
	IF ThresholdValue EQ -1 THEN RETURN

	AllYearNum=N_ELEMENTS((*SelectFactor).ARRAY_YEAR)

    Datayear=(*SelectFactor).ARRAY_YEAR		;这相当于年份list
    GrowthMonth=STRTRIM(INDGEN(12)+1,2)     ;这相当于月份List

    StartYear  = Datayear[(*SelectFactor).StartYearID]     & EndYear = Datayear[(*SelectFactor).EndYearID]
    StartMonth = GrowthMonth[(*SelectFactor).StartMonthID] & EndMonth= GrowthMonth[(*SelectFactor).EndMonthID];这四个变量指相应的开始及结束年份和生长月份,字符型
	RowsNum = FIX(EndYear)-FIX(StartYear)+1

	YieldType = (*SelectFactor).YieldType
    ;下面的IF判断分因子类型.
	IF (*SelectFactor).FactorType EQ 0 THEN BEGIN    ;如果是气象因子
		Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex
		IF ARRAY_EQUAL(MeteoTableIndex,0,/NO_TYPECONV) EQ 1 THEN BEGIN         ;如果没有选择因子,则提示.
		   Prompt=DIALOG_MESSAGE('请至少选择一种因子',TITLE='提示',/INFORMATION)
		   RETURN
		ENDIF

	    IF (*SelectFactor).YieldType EQ 0 THEN BEGIN    ;判断是县还是站点产量
	        Sqlstr='select weatherstation_code from tainsen_polygen where county_code='+"'"+(*SelectFactor).CountyCode+"'"
			station_code=DC_GetdataFromDB_Str(1,Sqlstr,N_RECORDS = NumReocrd,Num_BUFFERS = 500)
			IF NumReocrd EQ 0 THEN BEGIN
		      Info=DIALOG_MESSAGE("没有县'"+(*SelectFactor).CountyCode+"'的数据,请查看数据库中的泰森多边形表!",TITLE='警告')
		      RETURN
			ENDIF
		ENDIF ELSE   station_code=(*SelectFactor).CountyCode

	    Widget_Control,(*SelectFactor).MeteorologyFactor,SET_UVALUE =station_code   ;给"下一步"按钮用

	    IF ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
	        SingleTendayNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3             ;单旬的个数,不分波动因子种类
	        AllData=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,EndMonth,MeteoTableIndex,station_code,YieldType)
	        IF ARRAY_EQUAL(AllData,'') THEN RETURN   ;新调试的**************************
	        AllData = DC_ProcessBlank(AllData,RowsNum,BlankId = BlankID)
	        CombinationData= DC_FactorCombination(AllData,MeteoTableIndex,RowsNum,StartMonth,EndMonth)
	        AllData=[TEMPORARY(AllData),[CombinationData]]                                         ;这是最终的波动数据.
	        SingleTendayFactorName=DC_SingleTendayName(StartMonth,EndMonth)
	        AllFactorName = DC_CombinationTendayName(SingleTendayFactorName,MeteoTableIndex,SingleTendayNum) ;这是最终的所有因子名.下同.
	    ENDIF ELSE BEGIN                                                               ;注意冬小麦是跨年的,所以起始和结束月大小不能直接用在两个函数中.
	        SingleTendayNum = (13+FIX(EndMonth)-FIX(StartMonth))*3
	        AllData1=DC_SingleTendayFactor(StartYear,EndYear,StartMonth,'12',MeteoTableIndex,station_code,YieldType)
	        AllData2=DC_SingleTendayFactor(Fix(StartYear)+1,Fix(EndYear)+1,'1',EndMonth,MeteoTableIndex,station_code,YieldType)
	        IF ARRAY_EQUAL(AllData1,'') OR ARRAY_EQUAL(AllData2,'') THEN RETURN   ;新调试的**************************
	        AllData=[AllData1,[AllData2]]
	        AllData = DC_ProcessBlank(AllData,RowsNum,BlankId = BlankID)
	        CombinationData= DC_FactorCombination(AllData,MeteoTableIndex,RowsNum,1,13+FIX(EndMonth)-FIX(StartMonth))   ;这里的两个参数,不是起始月和结束月,是因为此
	        AllData=[TEMPORARY(AllData),[CombinationData]]                                                                      ;定义函数用这两参数只是想得到月数,所以可以这样赋参数.
	        SingleTendayFactorName=[DC_SingleTendayName(StartMonth,12),[DC_SingleTendayName(1,EndMonth)]]
	        AllFactorName = DC_CombinationTendayName(SingleTendayFactorName,MeteoTableIndex,SingleTendayNum)
	    ENDELSE

		ZeroNum = N_ELEMENTS(BlankID)/2
		SelectedFactorIndex = WHERE(MeteoTableIndex EQ 1,FactorNum)

		CASE 1 OF
			(ZeroNum EQ (SingleTendayNum*FactorNum*RowsNum)):BEGIN
			  Prompt=DIALOG_MESSAGE('数据库中没有目标区域的气象数据!',TITLE='警告')
			  RETURN
			END

			(ZeroNum GT (3*(SingleTendayNum-1)*FactorNum)*RowsNum/3.0):BEGIN      ;数据缺失三分之一,就提示
			  Prompt=DIALOG_MESSAGE('单旬气象数据缺失太多,是否重新选择县或者不同时间段数据进行筛选!',TITLE='询问',/QUESTION)
			  IF Prompt EQ 'Yes' THEN RETURN
			END
		  ELSE:
		ENDCASE

	    YieldStartYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ StartYear)    ;获量产量数据年份中起始年份相应索引号
	    YieldEndtYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ EndYear)

	    ActualYield = (*SelectFactor).YieldData[*,0]
	    TrendYield  = (*SelectFactor).YieldData[*,1]
		ZeroId = WHERE(ActualYield EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
			ActualYield[ZeroId] = MEAN(ActualYield[NotZeroID])  ;以均值来代替空白年的产量值
		ENDIF

		FloatYield = ActualYield-TrendYield

		RegressMeteoYield=FloatYield[YieldStartYearID:YieldEndtYearID]  ;获得相应年份波动产量,一行

		Regress__FactorName = {AllFactorName:AllFactorName,RegressYield:RegressMeteoYield,EndYear:EndYear}
		Widget_Control,(*SelectFactor).AgorFactor_TABLE,TABLE_XSIZE=N_ELEMENTS(AllFactorName)  $
		             ,TABLE_YSIZE=RowsNum,SET_VALUE=Alldata,SET_UVALUE=Regress__FactorName    $         ; 注意该用户值的内容.在"下一步"中用到
		             ,COLUMN_LABELS =AllFactorName ,ROW_LABELS =STRTRIM(INDGEN(RowsNum)+FIX(StartYear),2) $
		             ,ALIGNMENT=2,COLUMN_WIDTHS=120

		WIDGET_CONTROL,(*SelectFactor).AgorFactor_TABLE,USE_TABLE_SELECT=BlankID $
					,BACKGROUND_COLOR  = [255,255,0],SET_TABLE_VIEW = BlankID[*,0]   ;将实际年份产量为0(但以平均值来替换的年份)的单元格填成黄色警示.

		correla=['']   ;这里之所以用循环,是因为速度快,尽管也可以用下面已注释掉的语句"MeteoResult=....."一次算完,但有时速度奇慢,原因待查!
		FOR i=0,N_ELEMENTS(Alldata)/RowsNum-1 DO BEGIN
		   bb=REGRESS(FLOAT(Alldata[i,0:*]),FLOAT(RegressMeteoYield), CORRELATION=correlation)
		   correlaTemp=STRING(correlation)
		   correla=[correla,[correlaTemp]]
		ENDFOR
		correlation=correla[1:*]

		; MeteoResult=REGRESS(FLOAT(Alldata),FLOAT(RegressMeteoYield), CORRELATION=correlation);,STATUS=ynSuccess)                  ;用所有的波动因子进行相关分析,以得到相关系数
	    ;下面进行阈值判断

	    SelectedFactorID=WHERE(ABS(correlation) GE ThresholdValue[0],Count)    ;之所以用绝对值,当负相关较大时,因子也是有意义的.

	    IF Count GT 0 THEN BEGIN
	        SelectedFactor=Intarr(2,Count) & SelectedFactor[0,0:*]=SelectedFactorID
	        WIDGET_CONTROL,(*SelectFactor).factor_TABLE,TABLE_XSIZE=N_ELEMENTS(AllFactorName) $
	                  ,COLUMN_LABELS =AllFactorName,SET_VALUE=correlation $
	                  ,COLUMN_WIDTHS=120,SET_TABLE_VIEW=SelectedFactor[*,0] $
					  ,SET_UVALUE=SelectedFactor              ;该值记录被选中的因子的索引号.
			WIDGET_CONTROL,(*SelectFactor).factor_TABLE,USE_TABLE_SELECT=SelectedFactor $
						,BACKGROUND_COLOR = [0,255,0],SET_TABLE_VIEW=SelectedFactor[*,0]     	 ;将实际年份产量为0(但以平均值来替换的年份)的单元格填成绿色警示.

	        Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='因子筛选表'+'(当前筛选出'+STRTRIM(STRING(Count),2)+'个满足因子)'
	      IF (Count GT RowsNum-1) THEN BEGIN
	           Prompt=DIALOG_MESSAGE(['所选因子个数大于"样本量减去1"后的值(即数据年数减去1后','的值),则下一步建立的模型会没有精度,请您重新设置阈值','或者重新提取不同时间的数据再进行筛选!'],TITLE='提示',/INFORMATION)
	      ENDIF
	    ENDIF ELSE BEGIN
	       Widget_Control,(*SelectFactor).factor_TABLE,TABLE_XSIZE=N_ELEMENTS(AllFactorName) $
	                  ,COLUMN_LABELS =AllFactorName,SET_VALUE=STRING(correlation) $
	                  ,COLUMN_WIDTHS=120,SET_UVALUE=[-1,-1]
	       Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='因子筛选表'
	       Prompt=DIALOG_MESSAGE(['当前没有满足"阈值选择条件"的波动因子,请重新设','置阈值并刷新或者重新提取不同时间的数据再进行筛选!'],TITLE='提示',/INFORMATION)
	    ENDELSE
   ;------------------------------
   ENDIF ELSE BEGIN    ;如果是遥感因子
		Colnames = ['NPP均值','NPP累计值','LAI均值','LAI累计值','NDVI均值','NDVI累计值']
   		names=['NPP','NPP','LAI','LAI','NDVI','NDVI']
		Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex
		DataType = names[MeteoTableIndex]
		Sensor_type = (*SelectFactor).SensorType
		IsAvgOrSum = MeteoTableIndex MOD 2        ;IsAvgOrSum只为0或1 ,0时取平均,1时取累计.

		Sensor = ['AVHRR','MODIS','VGT'] & Sensor_code = ['1','2','3']
		Sensorname = Sensor[WHERE(Sensor_code EQ (*SelectFactor).SensorType)]

		CropIDList = (*SelectFactor).CropIDList				;Crop与CropIDList应对应
		CropCol = ['AVG_SPRING_WHEAT','AVG_WINTER_WHEAT','AVG_EARLY_RICE','AVG_SEMILATE_RICE' $
				  ,'AVG_LATE_RICE','AVG_SPRING_MAIZE','AVG_SUMMER_MAIZE','AVG_SOYBEAN']				;Crop与CropIDList应对应
		CropFiled = CropCol[WHERE(CropIDList EQ (*SelectFactor).CropId)]
		TableColnames = Colnames[MeteoTableIndex]

		Code = (*SelectFactor).CountyCode
		;下面用到的函数见DC_GetRsData()在"DC_FloatYield"中
		Data = DC_GetRsData(StartYear,EndYear,StartMonth,EndMonth,Code,CropFiled,IsAvgOrSum,Sensor_type,DataType,YieldType)
		IF ARRAY_EQUAL(FLOAT(Data),0.0) THEN BEGIN
			INFO =DIALOG_MESSAGE("数据库中没有该区域'"+Sensorname+"'的'"+DataType+"'数据!",TITLE='警告')
			RETURN
		ENDIF

		Rsdata = FLOAT(TRANSPOSE(Data))
		ZeroId = WHERE(Rsdata EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
			Rsdata[ZeroId] = MEAN(Rsdata[NotZeroID])  ;以均值来代替空白年的产量值
			ZeroCell = [INTARR(1,ZeroNum),TRANSPOSE(ZeroID)]
		ENDIF ELSE ZeroCell =[-1,-1]

	    YieldStartYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ StartYear)    ;获得产量数据年份中起始年份相应索引号
	    YieldEndtYearID=WHERE((*SelectFactor).ARRAY_YEAR EQ EndYear)
	    ActualYield = (*SelectFactor).YieldData[*,0]
	    TrendYield  = (*SelectFactor).YieldData[*,1]
		ZeroId = WHERE(ActualYield EQ 0.0,ZeroNum,COMPLEMENT=NotZeroID)
		IF ZeroNum NE 0 THEN BEGIN
			ActualYield[ZeroId] = MEAN(ActualYield[NotZeroID])  ;以均值来代替空白年的产量值
		ENDIF

		FloatYield = ActualYield-TrendYield

		RegressMeteoYield=FloatYield[YieldStartYearID:YieldEndtYearID]  ;获得相应年份波动产量,一行
		Regress__FactorName = {AllFactorName:TableColnames,RegressYield:RegressMeteoYield,EndYear:EndYear}

		Widget_Control,(*SelectFactor).AgorFactor_TABLE,TABLE_XSIZE=1  $
		             ,TABLE_YSIZE=RowsNum,SET_VALUE=STRTRIM(Rsdata,2),SET_UVALUE=Regress__FactorName    $     ;注意该用户值的内容.在"下一步"中用到
		             ,COLUMN_LABELS =[TableColnames] ,ROW_LABELS =STRTRIM(INDGEN(RowsNum)+FIX(StartYear),2) $
		             ,ALIGNMENT=2,COLUMN_WIDTHS=120

		WIDGET_CONTROL,(*SelectFactor).AgorFactor_TABLE,USE_TABLE_SELECT=ZeroCell $
					,BACKGROUND_COLOR  = [255,255,0],SET_TABLE_VIEW = ZeroCell[*,0]   ;将实际数据为0(但以平均值来替换的年份)的单元格填成黄色警示.

		bb = REGRESS(Rsdata,FLOAT(RegressMeteoYield), CORRELATION=correlation)
		SelectedFactor = [0,0]
		WIDGET_CONTROL,(*SelectFactor).factor_TABLE,TABLE_XSIZE=1 $
		          ,COLUMN_LABELS =[TableColnames],SET_VALUE=STRTRIM(correlation,2) $
				  ,SET_UVALUE=SelectedFactor             ;该值记录被选中的因子的索引号.此时只有一个

   ENDELSE

END
;*************************"下一步"按钮***********************************
PRO DC_SimulatResult,Event          	;这一步主要是计算出波动产量并传到下一个窗口中.

	CATCH, Error_status               ;截取错误.
	IF Error_status NE 0 THEN BEGIN
		infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
		CATCH, /CANCEL
		RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
	ENDIF

	Widget_Control,Event.top,GET_UVALUE = SelectFactor

	AllYearNum=N_ELEMENTS((*SelectFactor).ARRAY_YEAR)

	Datayear=(*SelectFactor).ARRAY_YEAR		;这相当于年份list
	GrowthMonth=STRTRIM(INDGEN(12)+1,2)     ;这相当于月份List

	StartYear  = Datayear[(*SelectFactor).StartYearID]     & EndYear = Datayear[(*SelectFactor).EndYearID]
	StartMonth = GrowthMonth[(*SelectFactor).StartMonthID] & EndMonth= GrowthMonth[(*SelectFactor).EndMonthID];这四个变量指相应的开始及结束年份和生长月份,字符型
	RowsNum = FIX(EndYear)-FIX(StartYear)+1

	YieldType = (*SelectFactor).YieldType

	Widget_Control,(*SelectFactor).factor_TABLE,GET_UVALUE=FactorID    ;得到被选中的因子ID号,注意它有两列,第一列值为要的因子ID
	Widget_Control,(*SelectFactor).AgorFactor_TABLE,GET_VALUE=Alldata,GET_UVALUE=Regress__FactorName

	CASE 1 OF
		ARRAY_EQUAL(Alldata,'',/NO_TYPECONV): BEGIN
		  Prompt=DIALOG_MESSAGE('请先筛选因子!',TITLE='提示',/INFORMATION)
		  RETURN
		END

		ARRAY_EQUAL(FactorID,[-1,-1],/NO_TYPECONV):BEGIN
		  Prompt=DIALOG_MESSAGE(['当前没有满足"阈值选择条件"的波动因子,请重新设','置阈值并刷新或'+ $
		                         '者重新提取不同时间的数据再进行筛选!'],TITLE='提示',/INFORMATION)
		  RETURN
		END
	ELSE:
	ENDCASE


	IF (*SelectFactor).FactorType EQ 0 THEN BEGIN

		CurrentYear=(*SelectFactor).CalcYear

		IF N_ELEMENTS(FactorID[0,*]) GT RowsNum-1 THEN BEGIN
		   Prompt=DIALOG_MESSAGE(['所选因子个数大于"样本量减去1"后的值(即数据年数减去1后','的值),'+ $
		                         '则下一步建立的模型会没有精度,请您重新设置阈值','或者重新提取不同时间的数据再进行筛选!'],TITLE='提示',/INFORMATION)
		   RETURN
		ENDIF

		;下面提取当前年份的波动气象因子数据
		 Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex,GET_UVALUE =station_code

		IF ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
		    SingleTendayNum=(FIX(EndMonth)-FIX(StartMonth)+1)*3             ;单旬的个数,不分组合旬波动因子种类
		    AllData0=DC_SingleTendayFactor(CurrentYear,CurrentYear,StartMonth,EndMonth,MeteoTableIndex,station_code,YieldType)
		    SingleTendayData = AllData0     ;单旬数据以用于下面的判断
		    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,StartMonth,EndMonth)
		    AllData0=[AllData0,[CombinationData]]                             ;这是最终的波动数据.
		ENDIF ELSE BEGIN                                                      ;注意冬小麦是跨年的,所以起始和结束月大小不能直接用在两个函数中.
		    Lastyear=STRTRIM(FIX(CurrentYear)-1,2)                    ;对于冬小麦,生长期从上年11月开始的.所以用Lastyear
		    SingleTendayNum = (13+FIX(EndMonth)-FIX(StartMonth))*3
		    AllData1=DC_SingleTendayFactor(Lastyear,Lastyear,StartMonth,'12',MeteoTableIndex,station_code,YieldType)
		    AllData2=DC_SingleTendayFactor(Fix(Lastyear)+1,Fix(Lastyear)+1,'1',EndMonth,MeteoTableIndex,station_code,YieldType)
		    AllData0=[AllData1,[AllData2]]
		    SingleTendayData = AllData0     ;单旬数据以用于下面的判断
		    CombinationData= DC_FactorCombination(AllData0,MeteoTableIndex,1,1,13+FIX(EndMonth)-FIX(StartMonth))   ;这里的两个参数,不是起始月和结束月,是因为此
		    AllData0=[AllData0,[CombinationData]]                                                               ;定义函数用这两参数只是想得到月数,所以可以这样赋参数.
		ENDELSE

		IF ARRAY_EQUAL(SingleTendayData,'') THEN BEGIN   ;这一步的判断还有待于进一步考虑
		   Prompt=DIALOG_MESSAGE('数据库中没有'+STRTRIM(CurrentYear,2)+'年气象因子数据!',TITLE='警告')
		   RETURN
		ENDIF

		AllFactorName=Regress__FactorName.AllFactorName  &  ActualMeteoYield=Regress__FactorName.RegressYield
		CurrentYearFactorData=[''] &  SelectedFactor_Name=['']  & SatisfactoryFactorData=STRARR(1,RowsNum)
		FactorID=TRANSPOSE(FactorID[0,0:*])   ;一定要明白表格断点选择的表示方法
		FOR i=0,N_ELEMENTS(FactorID)-1 DO BEGIN
		   CurrentYearFactorData = [CurrentYearFactorData,[Alldata0[FactorID[i],0:*]]]      ;提取当年波动数据以模拟当年的波动产量
		   SelectedFactor_Name     = [SelectedFactor_Name,[AllFactorName[FactorID[i]]]]     ;可以不用循环方式,而用 AllFactorName[FactorID,*] 即可,其它相同.
		   SatisfactoryFactorData = [SatisfactoryFactorData,[Alldata[FactorID[i],0:*]]]     ;此变量是选择年份的多年的的波动数据
		ENDFOR
		CurrentYearFactorData=FLOAT(CurrentYearFactorData[1:*])  ;原代码，20070831
;		;=========杨绍锷修改，20070831===========================
;		CurrentYearFactorData=double(CurrentYearFactorData[1:*])
;		;=========================================================
		SelectedFactor_Name=SelectedFactor_Name[1:*]                                       ;得到被选中的因子名
		SatisfactoryFactorData=SatisfactoryFactorData[1:*,0:*]                           ;得到满足阈值条件的因子波动数据

		SimuMeteoResult=REGRESS(SatisfactoryFactorData,ActualMeteoYield,CONST=constant $
		                  ,MCORRELATION=M_correlation,FTEST=F_check,YFIT=SimuMeteoYield, STATUS=status) ;杨绍锷添加 STATUS,20070831
		if status ne 0 then begin
			INFO =DIALOG_MESSAGE('所选因子不能进行下一步运算，请重新选择因子',TITLE='提示')
			RETURN
		endif

		CurrentYearMeteoYield=0.0
		FOR j=0,N_ELEMENTS(FactorID)-1 DO BEGIN
		  CurrentYearMeteoYield=CurrentYearMeteoYield+SimuMeteoResult[j]*CurrentYearFactorData[j]
		END

		CurrentYearMeteoYield=constant+CurrentYearMeteoYield     ;加上常数项,得到当年的波动产量

	ENDIF ELSE BEGIN
		Widget_Control,(*SelectFactor).MeteorologyFactor,GET_VALUE =MeteoTableIndex

		Colnames = ['NPP均值','NPP累计值','LAI均值','LAI累计值','NDVI均值','NDVI累计值']
   		names=['NPP','NPP','LAI','LAI','NDVI','NDVI']
		DataType = names[MeteoTableIndex]
		Sensor_type = (*SelectFactor).SensorType
		IsAvgOrSum = MeteoTableIndex MOD 2        ;IsAvgOrSum只为0或1 ,0时取平均,1时取累计.

		Sensor = ['AVHRR','MODIS','VGT'] & Sensor_code = ['1','2','3']
		Sensorname = Sensor[WHERE(Sensor_code EQ (*SelectFactor).SensorType)]

		CropIDList = (*SelectFactor).CropIDList				;Crop与CropIDList应对应
		CropCol = ['AVG_SPRING_WHEAT','AVG_WINTER_WHEAT','AVG_EARLY_RICE','AVG_SEMILATE_RICE' $
				  ,'AVG_LATE_RICE','AVG_SPRING_MAIZE','AVG_SUMMER_MAIZE','AVG_SOYBEAN']				;Crop与CropIDList应对应
		CropFiled = CropCol[WHERE(CropIDList EQ (*SelectFactor).CropId)]
		TableColnames = Colnames[MeteoTableIndex]

		Code = (*SelectFactor).CountyCode
		CurrentYear = (*SelectFactor).CalcYear
		;计算年份的遥感数据
		Data = DC_GetRsData(CurrentYear,CurrentYear,StartMonth,EndMonth,Code,CropFiled,IsAvgOrSum,Sensor_type,DataType,YieldType)
		IF ARRAY_EQUAL(FLOAT(Data),0.0) THEN BEGIN
			INFO =DIALOG_MESSAGE('数据库中没有计算年份'+CurrentYear+"年'"+Sensorname+"'的'"+DataType+"'数据!",TITLE='警告')
			RETURN
		ENDIF

		ActualMeteoYield=Regress__FactorName.RegressYield
		SimuMeteoResult=REGRESS(Alldata,ActualMeteoYield,CONST=constant $
		                  ,MCORRELATION=M_correlation,FTEST=F_check,YFIT=SimuMeteoYield)

		CurrentYearMeteoYield=constant+SimuMeteoResult* FLOAT(Data)    ;加上常数项,得到当年的波动产量
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
		log, '单产预测-波动产量', 0

END
;*********************************保存波动数据*********************************
PRO DC_SaveMeteoData,EVENT
	Widget_Control,Event.top,GET_UVALUE = SelectFactor
	Widget_Control,(*SelectFactor).AgorFactor_TABLE,GET_VALUE=MeteoData,GET_UVALUE = Regress__FactorName
	Widget_Control,(*SelectFactor).factor_TABLE,GET_VALUE=Correlation

	IF ARRAY_EQUAL(MeteoData, '') OR ARRAY_EQUAL(Correlation, '')  THEN BEGIN
	  Info=DIALOG_MESSAGE('您还没有筛选因子,请先筛选!' ,TITLE='提示',/INFORMATION)
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
	MeteoYield = TRANSPOSE(Regress__FactorName.RegressYield)      ;转成一列

	YearsNum = (*SelectFactor).EndYearID - (*SelectFactor).StartYearID +1
	YieldYear=STRTRIM(INDGEN(YearsNum)+StartYear,2)
	FactorNum = N_ELEMENTS(AllFactorName)

	Savedata=STRARR(FactorNum+4,YearsNum+2)
	Savedata[*,0]=['县Code','作物','年份','波动产量',AllFactorName]
	Savedata[0,1:YearsNum]=REPLICATE(CountyCode,YearsNum)
	Savedata[1,1:YearsNum]=REPLICATE(CropName,YearsNum)
	Savedata[2,1:YearsNum]=YieldYear
	Savedata[3,1:YearsNum]=MeteoYield
	Savedata[4:*,1:YearsNum]=MeteoData
	Savedata[0:3,YearsNum+1]=['相关系数','--','--','--']
	Savedata[4:*,YearsNum+1]=Correlation

	IF WHERE(Savedata EQ '') NE [-1] THEN BEGIN    ;说明有空格.注意用-1与[-1]的区别.
	    Savedata[WHERE(Savedata EQ '')]='---'
	ENDIF

	Filename=DIALOG_PICKFILE(TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
	      ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting, DIALOG_PARENT=Event.id)

	IF Filename EQ '' THEN RETURN

	OPENW,LUN,Filename,/GET_LUN,WIDTH=(FactorNum+4)*30
	PRINTF,LUN,Savedata
	FREE_LUN,LUN

	Info=DIALOG_MESSAGE('保存成功! ',TITLE='提示',/INFORMATION)

	Widget_Control,Event.top,SET_UVALUE=SelectFactor


END
;*****************这是"波动产量模拟"界面中的部分事件****************************
PRO DC_FloatYield_2_event,event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ? widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='Refurbish'): BEGIN

		WIDGET_CONTROL,Event.id,BACKGROUND_COLOR  = [255,255,255]

       Widget_Control,Event.top,GET_UVALUE = SelectFactor

        Widget_Control,(*SelectFactor).threshold_TEXT,GET_VALUE =ThresholdValue ;它是只有一个元素的数组.
		ThresholdValue = DC_JudgeInputChar(ThresholdValue[0],Desc='阀值文本框')
		IF ThresholdValue EQ -1 THEN RETURN

        Widget_Control,(*SelectFactor).factor_TABLE,GET_VALUE=AllCorrelation,BACKGROUND_COLOR=[255,255,255]

        IF AllCorrelation[0,0] EQ '' THEN RETURN

        AllCorrelation=FLOAT(AllCorrelation) ; &   ThresholdValue=FLOAT(ThresholdValue)     ;得到默认的阈值
        SelectedFactorID=WHERE(ABS(AllCorrelation) GE ThresholdValue[0],Count)
       IF Count GT 0 THEN BEGIN
          SelectedFactor=Intarr(2,Count) & SelectedFactor[0,0:*]=SelectedFactorID         ;因子表的断点选择方式/DISJOINT_SELECTION要求2列的数组.
          Widget_Control,(*SelectFactor).factor_TABLE,USE_TABLE_SELECT=SelectedFactor,SET_UVALUE=SelectedFactor $
                        ,SET_TABLE_VIEW = SelectedFactor[*,0],BACKGROUND_COLOR  = [0,255,0]
          Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='因子筛选表'+'(当前筛选出'+STRTRIM(STRING(Count),2)+'个满足因子)'
       ENDIF ELSE BEGIN
          Widget_Control,(*SelectFactor).factor_TABLE,SET_TABLE_SELECT=[-1,-1],SET_UVALUE=[-1,-1]
          Widget_Control,(*SelectFactor).factor_LABEL,SET_VALUE='因子筛选表'
          Prompt=DIALOG_MESSAGE(['当前没有满足"阈值选择条件"的波动因子,请重新设','置阈值或者重新提取不同时间的数据再进行筛选!'],TITLE='提示',/INFORMATION)
       ENDELSE

       Widget_Control,Event.top,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Start_DROPLIST'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
       (*SelectFactor).StartYearID = Event.Index
;;       Info=['默认的"起始年份1980年"是与产量更新数据年份相',$
;;             '对应的最小年份,请您最好选择,真的要重新选择吗?']
;;       Prompt=DIALOG_MESSAGE(TITLE='询 问',Info,/QUESTION)
;;       IF Prompt EQ 'No' THEN BEGIN
;;          Widget_Control,Event.id,SET_DROPLIST_SELECT = 0   ;设为最小年份
;;          (*SelectFactor).StartYearID = 0
;;          Widget_Control,wWidget,SET_UVALUE = SelectFactor
;;          RETURN
;;       ENDIF
       IF Event.Index GE (*SelectFactor).EndYearID THEN BEGIN
          Prompt=DIALOG_MESSAGE(TITLE='警告','起始年份必须小于结束年份')
          Widget_Control,Event.id,SET_DROPLIST_SELECT = 0
          (*SelectFactor).StartYearID = 0
       ENDIF
       Widget_Control,wWidget,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='End_DROPLIST'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
        (*SelectFactor).EndYearID=Event.Index
;;       Info=['默认的"结束年份"是与产量更新数据年份相对应的最新年份,请您',$
;;             '最好选择,如果要选择只能选择比此小的年份,真的要重新选择吗?']
;;       Prompt=DIALOG_MESSAGE(TITLE='询 问',Info,/QUESTION)
;;       IF Prompt EQ 'No' THEN BEGIN
;;          Widget_Control,Event.id,SET_DROPLIST_SELECT = (*SelectFactor).DefaultEndYearID
;;          (*SelectFactor).EndYearID=(*SelectFactor).DefaultEndYearID
;;          Widget_Control,wWidget,SET_UVALUE = SelectFactor
;;          RETURN
;;       ENDIF
       EndYear = Event.Index
          IF EndYear GT (*SelectFactor).DefaultEndYearID THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','您不能选择比默认"结束年份"大的年份')
             Widget_Control,Event.id,SET_DROPLIST_SELECT = (*SelectFactor).DefaultEndYearID
             (*SelectFactor).EndYearID=(*SelectFactor).DefaultEndYearID
          ENDIF ELSE BEGIN
             IF EndYear LE (*SelectFactor).StartYearID THEN BEGIN
                Prompt=DIALOG_MESSAGE(TITLE='警告','结束年份必须大于起始年份')
                Widget_Control,Event.id,SET_DROPLIST_SELECT = (*SelectFactor).DefaultEndYearID
                (*SelectFactor).EndYearID=(*SelectFactor).DefaultEndYearID
             ENDIF
          ENDELSE
       Widget_Control,wWidget,SET_UVALUE = SelectFactor
    END

    Widget_Info(wWidget, FIND_BY_UNAME='YNStride'): BEGIN
       Widget_Control,wWidget,GET_UVALUE = SelectFactor
       StartMonth_Drop = Widget_Info(wWidget, FIND_BY_UNAME='StartMonth_Drop')   ;得到相应组件
       EndMonth_Drop = Widget_Info(wWidget, FIND_BY_UNAME='EndMonth_Drop')
       StartMonth= (*SelectFactor).StartMonthID
       EndMonth  = (*SelectFactor).EndMonthID
          IF (Event.SELECT EQ 1) AND (StartMonth LE EndMonth) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','如果生长期跨年,则生长期起始月份应大于结束月份!请重新选择月份!')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT=10   ;设为冬小麦的默认值,因为该组件可用时,作物必须是冬小麦.
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT=4
             (*SelectFactor).StartMonthID=10 & (*SelectFactor).EndMonthID=4
          ENDIF
          IF (Event.SELECT EQ 0) AND (StartMonth GE EndMonth) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','如果生长期不跨年,则生长期起始月份应小于结束月份!请重新选择月份!')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT=EndMonth     ;将它们的值对调
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
             Prompt=DIALOG_MESSAGE(TITLE='警告',CropName+'生长期起始月份应小于结束月份')
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = EndMonth
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT = StartMonth   ;将它们的值对调
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       ENDIF ELSE BEGIN
          IF (StartMonth LE EndMonth) AND ((*SelectFactor).StrideYear EQ 1) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','如果生长期跨年,则生长期起始月份应大于结束月份')
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = 10
             Widget_Control,EndMonth_Drop,SET_DROPLIST_SELECT = 4   ;设成默认冬小麦的月份.
             (*SelectFactor).StartMonthID=10 & (*SelectFactor).EndMonthID=4
          ENDIF
          IF (StartMonth GE EndMonth) AND ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','如果生长期不跨年,则生长期起始月份应小于结束月份!请重新选择月份!')
             Widget_Control,Event.ID,SET_DROPLIST_SELECT=EndMonth     ;将它们的值对调
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
             Prompt=DIALOG_MESSAGE(TITLE='警告',CropName+'生长期起始月份应小于结束月份')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT = EndMonth
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = StartMonth   ;将它们的值对调
             (*SelectFactor).StartMonthID=EndMonth & (*SelectFactor).EndMonthID=StartMonth
          ENDIF
       ENDIF ELSE BEGIN
          IF (StartMonth LE EndMonth) AND ((*SelectFactor).StrideYear EQ 1) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','如果生长期跨年,则生长期起始月份应大于结束月份')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT = 10
             Widget_Control,Event.ID,SET_DROPLIST_SELECT = 4   ;设成默认冬小麦的月份.
             (*SelectFactor).StartMonthID=10 & (*SelectFactor).EndMonthID=4
          ENDIF
          IF (StartMonth GE EndMonth) AND ((*SelectFactor).StrideYear EQ 0) THEN BEGIN
             Prompt=DIALOG_MESSAGE(TITLE='警告','如果生长期不跨年,则生长期起始月份应小于结束月份!请重新选择月份!')
             Widget_Control,StartMonth_Drop,SET_DROPLIST_SELECT=EndMonth     ;将它们的值对调
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
        Sensor = ['1','2','3']    ;对应于传感器LIST的代码
        (*SelectFactor).SensorType = Sensor[EVENT.INDEX]
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Cancel_BUTTON'): widget_control,Event.top,/DESTROY
    ELSE:
  ENDCASE

END

;**************"波动产量模拟"的下一步的子界面**********************************
PRO DC_FloatYield_2,state,GROUP_LEADER=groupleader,ITS_TLB=its_tlb

   TLB = Widget_Base(GROUP_LEADER=groupleader,  $
      UNAME='TLB' ,XOFFSET=150 ,YOFFSET=200 ,TITLE='筛选波动产量因子'  $
      ,SPACE=3 ,XPAD=1 ,YPAD=1 ,ROW=2 ,TLB_FRAME_ATTR=1);,/MODAL)

  ;----------------------------***BASE上部*********************-----------------
  AgorFactor = Widget_Base(TLB, UNAME='AgorFactor'  $
      ,SPACE=3 ,XPAD=0 ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  ;--------------------BASE上部左边的BASE----------------------------------
	  AgorFactor_left = Widget_Base(AgorFactor, UNAME='AgorFactor_left'  $
	      ,FRAME=1,SCR_XSIZE=172 ,SCR_YSIZE=447  $
	      ,/BASE_ALIGN_LEFT ,SPACE=3 ,XPAD=1 ,YPAD=1  $
	      ,COLUMN=1)
		;--------------------------单点信息-------------------------------------
		  Crop_conty_info = Widget_Base(AgorFactor_left,  $
		      UNAME='Crop_conty_info' ,FRAME=1,SPACE=0 ,xpad=1,YPAD=1,/COLUMN,/BASE_ALIGN_TOP)

		  ;-------------------------定义label与text的高度和宽度------------------------
		  labelwidth = 59 & labelheight = 18 & textwidth = 88 & textheight = 18

		  target_info=WIDGET_BASE(Crop_conty_info,/ROW,/BASE_ALIGN_CENTER)
		  Info_label = Widget_Label(target_info,UNAME='Info_label'  $
		      ,SCR_XSIZE=labelwidth ,SCR_YSIZE=labelheight,VALUE='目标信息：')

		  county_code=WIDGET_BASE(Crop_conty_info,/ROW,/BASE_ALIGN_CENTER)
		  county_LABEL = Widget_Label(county_code, UNAME='county_LABEL'  $
		       ,SCR_XSIZE=labelwidth ,SCR_YSIZE=labelheight)
		  county_TEXT = Widget_Text(county_code, UNAME='county_TEXT'  $
		      ,SCR_XSIZE=textwidth-3 ,SCR_YSIZE=textheight)

		  crop_name=WIDGET_BASE(Crop_conty_info,/ROW,/BASE_ALIGN_CENTER)
		  Crop_LABEL = Widget_Label(crop_name, UNAME='Crop_LABEL'  $
		      ,SCR_XSIZE=labelwidth ,SCR_YSIZE=labelheight ,/ALIGN_left  $
		      ,VALUE='作物名称：')
		  Crop_TEXT = Widget_Text(crop_name, UNAME='Crop_TEXT'  $
		      ,SCR_XSIZE=textwidth-3 ,SCR_YSIZE=textheight)
		  ;----------------------------------------------------------------------

		  Factor_year = Widget_Base(AgorFactor_left,  $
		      UNAME='Factor_year' ,FRAME=1,SPACE=0 ,xpad=1,YPAD=1,/COLUMN,/BASE_ALIGN_TOP,SCR_XSIZE=162)

		  year_info=WIDGET_BASE(Factor_year,/ROW,/BASE_ALIGN_LEFT)
		  Year_LABEL = Widget_Label(year_info, UNAME='Year_LABEL'  $
		       ,SCR_XSIZE=85 ,SCR_YSIZE=15   $
		      ,VALUE='因子年份：')

		  ;---------------------波动因子起始_结束年份------------------------
;		  Year_BASE = Widget_Base(Factor_year, UNAME='Year_BASE' $
;		      ,SPACE=2 ,XPAD=3 ,YPAD=1 ,/column,SCR_XSIZE=170)
		  ;-------------------------------------------------------------------
		  labelwidth1 = 63 & labelheight1 = 15 & Droplistwidth1 = 70 & Droplistheight1 = 18

			  start_year=WIDGET_BASE(Factor_year,/ROW,/BASE_ALIGN_CENTER)
			  StartYear_LABEL = Widget_Label(start_year, UNAME='StartYear_LABEL'  $
			      ,SCR_XSIZE=labelwidth1 ,SCR_YSIZE=labelheight1 ,/ALIGN_LEFT  $
			      ,VALUE='起始年份：')

			  Start_DROPLIST = Widget_Droplist(start_year, UNAME='Start_DROPLIST'  $
			      ,SCR_XSIZE=Droplistwidth1 ,SCR_YSIZE=Droplistheight1)

			  end_year=WIDGET_BASE(Factor_year,/ROW,/BASE_ALIGN_CENTER)
			  EndYear_LABEL = Widget_Label(end_year, UNAME='EndYear_LABEL'  $
			      ,SCR_XSIZE=labelwidth1 ,SCR_YSIZE=labelheight1  $
			      ,/ALIGN_LEFT ,VALUE='结束年份：')

			  End_DROPLIST = Widget_Droplist(end_year, UNAME='End_DROPLIST'  $
			      ,SCR_XSIZE=Droplistwidth1 ,SCR_YSIZE=Droplistheight1)

		  ;--------------------有关生长期和是否跨年份-----------------
		  GrowthPeriod_BASE = Widget_Base(AgorFactor_left,  $
		      UNAME='GrowthPeriod_BASE' ,SCR_XSIZE=162 $
		      ,SPACE=0 ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER,/FRAME)
		  ;-----------------生长期-----------------
		  GrowthLabel_BASE = Widget_Base(GrowthPeriod_BASE,  $
		      UNAME='GrowthLabel_BASE'  $
		      ,SCR_XSIZE=160,SCR_YSIZE=23 ,/BASE_ALIGN_LEFT  $
		      ,SPACE=1 ,XPAD=3 ,YPAD=0 ,ROW=1)
		  ;-------------------
		  growth_LABEL = Widget_Label(GrowthLabel_BASE, UNAME='growth_LABEL'  $
		      ,XOFFSET=0 ,YOFFSET=3 ,SCR_XSIZE=45 ,SCR_YSIZE=15   $
		      ,VALUE='生长期：')

		  CropGrowthMonth=STRTRIM(STRING(INDGEN(12)+1),2)
		  StartMonth_Drop = Widget_DropList(GrowthLabel_BASE,VALUE=CropGrowthMonth,  $
		      UNAME='StartMonth_Drop'  ,SCR_XSIZE=35,SCR_YSIZE=18,/ALIGN_TOP)

		  growthTo_LABEL = Widget_Label(GrowthLabel_BASE,  $
		      UNAME='growthTo_LABEL' ,XOFFSET=90 ,YOFFSET=3 ,SCR_XSIZE=13  $
		      ,SCR_YSIZE=15,/ALIGN_LEFT ,VALUE='至')

		  EndMonth_Drop = Widget_DropList(GrowthLabel_BASE, UNAME='EndMonth_Drop'  $
		      ,SCR_XSIZE=35 ,SCR_YSIZE=18,VALUE=CropGrowthMonth)

		  growthEnd_LABEL = Widget_Label(GrowthLabel_BASE,  $
		      UNAME='growthEnd_LABEL' ,XOFFSET=142 ,YOFFSET=3 ,SCR_XSIZE=25  $
		      ,SCR_YSIZE=15 ,/ALIGN_LEFT ,VALUE='月')

		 ;-----------生长期是否跨年按钮--------------------------------------------
		  YNStride_BASE = Widget_Base(GrowthPeriod_BASE,  $
		      UNAME='YNStride_BASE' ,SCR_XSIZE=170  $
		      ,SCR_YSIZE=22 ,COLUMN=1 ,/NONEXCLUSIVE,xpad=7)

		  YNStride = Widget_Button(YNStride_BASE, UNAME='YNStride'  $
		      ,SCR_XSIZE=140 ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='生长期跨年(注意冬小麦)')

		  ;---------------------波动因子BASE-----------------------------------
		  MeteorFactor_BA=Widget_Base(AgorFactor_left,XPAD=0,YPAD=0,/COLUMN,SPACE=2)

		    SensorBase = Widget_Base(MeteorFactor_BA,XPAD=0,YPAD=1,SPACE=2,/ROW,/FRAME,/align_left,SCR_XSIZE=162)
		    SensorDrop = WIDGET_DROPLIST(SensorBase,UNAME='SensorDrop',TITLE='传感器类型：' $
		    							 ,VALUE=['AVHRR','MODIS','VGT'],SCR_XSIZE=155)
		  FACTOR=WIDGET_BASE(MeteorFactor_BA,/FRAME,/COLUMN,SPACE=2)
		  MeteorFactor_Label=Widget_Label(FACTOR,VALUE='波动因子：' $
		      ,SCR_YSIZE=15, /ALIGN_LEFT)
	  ;--------------------------波动因子表----------------------------------
	  TABLE=WIDGET_BASE(AgorFactor,/COLUMN,XPAD=0,YPAD=0,SPACE=3)
	  AgorFactor_TABLE = Widget_Table(TABLE,  $
	      UNAME='AgorFactor_TABLE' ,XOFFSET=200 ,YOFFSET=3 ,SCR_XSIZE=568, /DISJOINT_SELECTION  $
	      ,SCR_YSIZE=359 ,/EDITABLE ,XSIZE=20 ,YSIZE=20,/FRAME,/RESIZEABLE_COLUMNS)


  ;----------***************--------BASE下半部----------***********------------------
  threshold = Widget_Base(TLB, UNAME='threshold'  $
      ,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)

	  ;---------------------------------------------------------
	  SelectFactor_BASE = Widget_Base(AgorFactor_left,  $
	      UNAME='SelectFactor_BASE',SPACE=2 ,XPAD=0 $
	      ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  ;------------------阈值设置BASE------------------------------------------
	  thredshodValue_BASE = Widget_Base(SelectFactor_BASE,  $
	      UNAME='thredshodValue_BASE' ,FRAME=1  $
	      ,/BASE_ALIGN_LEFT,SPACE=5 ,XPAD=1 ,YPAD=1 ,ROW=1,SCR_XSIZE=162)

		  threshold_LABEL = Widget_Label(thredshodValue_BASE,  $
		      UNAME='threshold_LABEL' ,SCR_XSIZE=60  $
		      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='阈值设置：')
		  threshold_TEXT = Widget_Text(thredshodValue_BASE, UNAME='threshold_TEXT'  $
		      ,FRAME=1 ,SCR_XSIZE=30 ,SCR_YSIZE=20 ,VALUE = '0.5' $
		      ,/EDITABLE ,XSIZE=20 ,YSIZE=1)
		  Refurbish = Widget_Button(thredshodValue_BASE, UNAME='Refurbish'  $
		      ,SCR_XSIZE=45 ,SCR_YSIZE=20 ,/ALIGN_CENTER,VALUE='刷新'  $
		       ,TOOLTIP='阈值重新设定后，刷新因子筛选表' )
	;-------------------------因子筛选表-----------------------------------
	  ChoiceFactor_BASE = Widget_Base(TABLE, $
	      UNAME='ChoiceFactor_BASE' ,FRAME=1 ,SPACE=1,SCR_XSIZE=568 $
	      ,SCR_YSIZE=83,XPAD=0 ,YPAD=1,COLUMN=1,/BASE_ALIGN_LEFT)

		  factor_LABEL = Widget_Label(ChoiceFactor_BASE, UNAME='factor_LABEL'  $
		      ,SCR_XSIZE=273 ,SCR_YSIZE=15,/ALIGN_CENTER ,VALUE='因子筛选表')

		  factor_TABLE = Widget_Table(ChoiceFactor_BASE, UNAME='factor_TABLE',ROW_LABELS=['相关系数']  $
		       ,SCR_XSIZE=568 ,SCR_YSIZE=63 ,XSIZE=10,YSIZE=1,/FRAME,/DISJOINT_SELECTION,/RESIZEABLE_COLUMNS)
	 ;------------------------按钮部分--------------------------------------
	  DivideSpace=75
	  Simulate_save_BASE = Widget_Base(threshold,  $
	      UNAME='Simulate_save_BASE' ,/BASE_ALIGN_TOP  $
	      ,SCR_XSIZE=745 ,SCR_YSIZE=36,/FRAME   $
	      ,SPACE=DivideSpace,XPAD=71 ,YPAD=0 ,ROW=1)
	  ;----------------------------------------------
;		  Simulate_BASE = Widget_Base(Simulate_save_BASE,FRAME=0  $
;		      ,UNAME='Simulate_BASE' ,SCR_YSIZE=34,/BASE_ALIGN_TOP $
;		       ,SPACE=DivideSpace ,XPAD=DivideSpace,YPAD=3,ROW=1)

		      Button_width = 90 & Button_height = 22  ;定义按钮的宽度和高度
			  Select_BUTTON = Widget_Button(Simulate_save_BASE, UNAME='Select_BUTTON'  $
			      ,FRAME=0 ,XOFFSET=70 ,YOFFSET=2 ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height  $
			      ,/ALIGN_CENTER ,TOOLTIP='依据阈值筛选合适的波动因子' ,VALUE='筛选'$
			      ,EVENT_PRO='DC_FilterFactor')
			  Simulation_BUTTON = Widget_Button(Simulate_save_BASE,  $
			      UNAME='Simulation_BUTTON' ,FRAME=0 ,XOFFSET=276 ,YOFFSET=2  $
			      ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height ,/ALIGN_CENTER  $
			      ,TOOLTIP='下一步进行波动产量模拟' ,VALUE='下一步',EVENT_PRO='DC_SimulatResult')
			 Save_BUTTON = Widget_Button(Simulate_save_BASE,UNAME='Save_BUTTON' ,FRAME=0  $
			      ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height ,/ALIGN_CENTER  $
			      ,TOOLTIP='将波动数据和相关系数保存到本地磁盘' ,VALUE='保存',EVENT_PRO='DC_SaveMeteoData')
		;-------------------------------------------------------------------
		  Cancel_BUTTON = Widget_Button(Simulate_save_BASE, UNAME='Cancel_BUTTON'  $
		      ,FRAME=0 ,XOFFSET=70 ,YOFFSET=3 ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height  $
		      ,/ALIGN_CENTER ,TOOLTIP='退出波动模拟第二步' ,VALUE='取消')

;--------------------------下面使用参数进行部分相关组件值设置-------------------------------------
	IF (*state).FactorType EQ 0 THEN BEGIN
		  names=['平均气温','降水','日照','空气相对湿度','0℃积温','最高气温','最低气温']

		  MeteorologyFactor = CW_BGROUP(FACTOR, names,UNAME='MeteorologyFactor' $
		        ,YPAD=0,XPAD=5,XSIZE=138,Y_SCROLL_SIZE=108,/COLUMN, /NONEXCLUSIVE $
		        ,/RETURN_NAME,/SCROLL,SPACE=0)
		WIDGET_CONTROL,MeteorologyFactor,SET_VALUE=[1,1,1,0,0,0,0]          ;设置均温、降水和日照为默认值，注意这种方式只用于有/NONEXCLUSIVE
		WIDGET_CONTROL,SensorBase,SENSITIVE=0
	ENDIF ELSE BEGIN
		  names=['NPP均值','NPP累计值','LAI均值','LAI累计值','NDVI均值','NDVI累计值']

		  MeteorologyFactor = CW_BGROUP(FACTOR, names,UNAME='MeteorologyFactor' $
		        ,YPAD=0,XPAD=5,XSIZE=138,Y_SCROLL_SIZE=108,/COLUMN, /EXCLUSIVE $
		        ,/FRAME,/RETURN_NAME,/SCROLL,SPACE=0)
		WIDGET_CONTROL,MeteorologyFactor,SET_VALUE=4          ;设置NDVI累计值为默认值，注意这种方式只用于有/EXCLUSIVE
		WIDGET_CONTROL,thredshodValue_BASE,SENSITIVE=0
	ENDELSE
;----------------------------------------------------------------------------
  Widget_Control, /REALIZE, TLB
  WIDGET_CONTROL,Cancel_BUTTON,/INPUT_FOCUS

   GrowthMonth = (*state).GrowthMonth

	WIDGET_CONTROL,(*state).DistrictName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)
		Crop = STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDList EQ (*state).cropID)],/RE) ;得到作物名
    YearHaveData = *((*state).YearHaveData)
	IF (*state).YieldType THEN ObjType='站点代码：' ELSE ObjType='县 代 码：'

	Widget_Control, county_LABEL,SET_VALUE=ObjType
	Widget_Control, county_TEXT,SET_VALUE=County[0]
	Widget_Control, Crop_TEXT,SET_VALUE=Crop

	Widget_Control, StartMonth_Drop,SET_DROPLIST_SELECT=GrowthMonth[0]-1
	Widget_Control, EndMonth_Drop  ,SET_DROPLIST_SELECT=GrowthMonth[1]-1

	Widget_Control, YNStride,SET_BUTTON=GrowthMonth[2],SENSITIVE=GrowthMonth[2]
	Widget_Control, Start_DROPLIST,SET_VALUE= YearHaveData
	Widget_Control, End_DROPLIST,SET_VALUE= YearHaveData,SET_DROPLIST_SELECT=GrowthMonth[3]     ;将结束年份定在最后年份.

  Widget_Control,AgorFactor_TABLE,SET_TABLE_SELECT=[-1,-1]
  Widget_Control,factor_TABLE,SET_TABLE_SELECT=[-1,-1]

 		para = {CropId         		:   (*state).CropId 		,$	;作物ID
                CropIDList			:	(*state).CropIDList		,$	;作物ID列表
                CropNameList		:	(*state).CropNameList	,$	;作物名称列表
                CountyCode        	:   County[0] 			,$	;县或站点的代码
				CalcYear			:	(*state).CalcYear	,$	;要计算哪年的波动产量
				FactorType			:	(*state).FactorType ,$  ;因子类型
                YieldType			:	(*state).YieldType	,$	;产量类型
                SensorType			:	'1'					,$	;传感器类型默认为AVHRR
                ARRAY_YEAR			:	YearHaveData			,$	;产量年份数组,不是指针
                YieldData			:	*((*state).YieldData)	,$	;标识从库中提取的实际和趋势产量,2行值(FLOAT).
                StrideYear        	:   GrowthMonth[2]			,$  ;0表示不跨年,1表示跨年.
                StartYearID       	:   0					,$
                EndYearID         	:   GrowthMonth[3] 		,$
                DefaultEndYearID  	:   GrowthMonth[3]  	,$
                StartMonthID     	:   GrowthMonth[0]-1	,$
                EndMonthID        	:   GrowthMonth[1]-1 	,$
                AgorFactor_TABLE  	:   AgorFactor_TABLE 	,$
                MeteorologyFactor 	:   MeteorologyFactor	,$  ;因子列表组件,目的是将选中的波动因子传过来.
                factor_TABLE      	:   factor_TABLE 		,$
                threshold_TEXT    	:   threshold_TEXT		,$
                factor_LABEL      	:   factor_LABEL		,$
                Next_TLB          	:   0L}           			;获取下一窗口的顶级BASE

  its_tlb = TLB   ;获取顶级BASE

  SelectFactor = PTR_NEW(para, /NO_COPY)
  Widget_Control, TLB ,SET_UVALUE=SelectFactor

  XManager, 'DC_FloatYield_2', TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END