
;******统计波动产量到各县************************************
FUNCTION DC_B_StatisticFloat,StaFile,WeightFile,STATUS = status
	;StaFile  被统计文件;WeightFile权重文件;status计算是否成功的状态
	;调用形式:Re = DC_B_StatisticFloat(StaFile,WeightFile,[STATUS=status]) 注意只能用status
	;返回值: 各县结构体
	;算法:Sum(Pix*W)/sum(W)  Pix像元值;W为权重

	CountyRaster = 'data_grid\county_raster'

	WeightData = DC_Read_ENVIData(WeightFile,SUCCESSSTATUS = StatusW,Description='耕地权重文件')
	IF NOT StatusW THEN BEGIN		;WeightData在0-100之间
		STATUS=StatusW
		RETURN,0
	ENDIF

	StaData    = DC_Read_ENVIData(StaFile,SUCCESSSTATUS = StatusS,Description='产量文件')
	IF NOT StatusS THEN BEGIN		;StaData 浮点值
		STATUS=StatusS
		RETURN,0
	ENDIF

	CountyData= DC_Read_ENVIData(CountyRaster,SUCCESSSTATUS = StatusC,Description='县栅格文件')
	IF NOT StatusC THEN BEGIN
		STATUS=StatusC
		RETURN,0
	ENDIF

	Sinfo = DC_ReadHead_file(StaFile)
	Winfo = DC_ReadHead_file(WeightFile)
	Cinfo = DC_ReadHead_file(CountyRaster)
	IF FIX(Sinfo.samples) NE FIX(Winfo.samples) OR FIX(Sinfo.samples) NE FIX(Cinfo.samples) OR   $
	   FIX(Sinfo.lines)   NE FIX(Winfo.lines)   OR FIX(Sinfo.lines)   NE FIX(Cinfo.lines) THEN BEGIN
		Info = DIALOG_MESSAGE('用于统计的基础数据与产量文件大小不一致,统计失败!',TITLE='警告')
		STATUS=0
		RETURN,0
	ENDIF

	CountyID = CountyData[UNIQ(CountyData,SORT(CountyData))]    ;得到唯一的县Index (不是县代码)
	CountyID = CountyID[WHERE(CountyID NE 0)]					;去掉0 Index

	Result = REPLICATE({CountyID:0,FloatYield:0.0},N_ELEMENTS(CountyID))

	FOR I=0,N_ELEMENTS(CountyID)-1 DO BEGIN
		County  = WHERE(CountyData EQ CountyID[I])
		DataV   = StaData[County]			;DOUBLE型
		W_Value = WeightData[County]*1.0	;BYTE型,后转为浮点型.
		DataV   = TEMPORARY(DataV)*W_Value

		Valid   = WHERE(W_Value GE 10)				;只统计权重大于10%

		Result[I].FloatYield = TOTAL(DataV[Valid])/TOTAL(W_Value[Valid])
		Result[I].CountyID   = CountyID[I]

	ENDFOR

	status = 1
	RETURN,Result

END

;**********************************************************************
PRO DC_B_CLEARUP_DATA,EventTop
   Widget_Control ,EventTop,GET_UVALUE=state
      Widget_Control,(*state).FloatYield_Table,GET_VALUE=MeteorologyYield & MeteorologyYield=''
      Widget_Control,(*state).FloatYield_Table,SET_VALUE=MeteorologyYield[*,*],ROW_LABELS=''

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=StaYield & StaYield=''
      Widget_Control,(*state).StaYield_TABLE,SET_VALUE=StaYield[*,*],ROW_LABELS=''

      Widget_Control,(*state).StaYield_LABEL,SET_VALUE='收获指数法计算的单产结果'

      Widget_Control,(*state).prompt_TEXT ,SET_VALUE=''

      Widget_Control,(*state).Extrapolate_DRAW ,GET_VALUE=Owindow
      Owindow->ERASE,COLOR=255
	  oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	  IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

      Widget_Control,(*state).Yield_DRAW ,GET_VALUE=Owindow
      Owindow->ERASE,COLOR=218
	  oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	  IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	  (*state).Savefile = ''			;同时使HI插值文件名为空

   Widget_Control ,EventTop,SET_UVALUE=state
END

;*******自定义过程:提取站点模拟的估算产量和生物量数据,以计算站点收获指数*******************
PRO DC_B_TakeSimpleDotData,EventTop

	ON_ERROR,2
    DC_B_CLEARUP_DATA,EventTop

    Widget_control,/HOURGLASS

    Widget_Control,EventTop,GET_UVALUE=state
    CropName = STRCOMPRESS((*state).CropName[(*state).Cropindex],/REMOVE_ALL)
	CropId = (*state).CropIDList[(*state).Cropindex]
;    CropBioName = ['','spring_wheat','winter_wheat','early_rice','semilate_rice','late_rice','maize','soybean']
    CropBioName = ['','spring_wheat','winter_wheat','early_rice','semilate_rice','late_rice','spring_maize','summer_maize','soybean']
    BioTableCrop = CropBioName[(*state).Cropindex]

    ProName = (*state).Province[WHERE((*state).ProIDList EQ (*state).ProID)]

	IF (*state).StationYieldID EQ '' THEN BEGIN
		 Prompt=DIALOG_MESSAGE('至少选择一种站点产量估算"类型"',/INFORMATION,TITLE='警告')
	 	 RETURN
	ENDIF

	ModelData_id = (*state).StationYieldID

    Sqlstr='select AgroMeteoStation_CODE,ROUND(AVG(Yield),3) AS YieldNew from AGROSTATION_ESTIMATED_YIELD ' $
    		+"where crop_id='"+CropId+"' and year="+(*state).CalcYear+' and Model_type_id in (' $
    		+ ModelData_id+') group by AgroMeteoStation_CODE'
    Sqlstr='select code,name,b.YieldNew,longitude,latitude from AGRO_METEO_STATION_INFO a,('+Sqlstr+') b '$
    		+'where a.code=b.AgroMeteoStation_CODE AND LEFT(a.COUNTY_CODE,2)=' $
    		+(*state).ProID+' order by b.AgroMeteoStation_CODE'
    Sqlstr='select d.name,d.code,d.YieldNew,c.biomass,ROUND(d.YieldNew/c.biomass,3) as HI,longitude,d.latitude ' $
    		+ 'from Bio_'+BioTableCrop+'_AgroStation c,('+Sqlstr+') d ' $
    		+'where d.code=c.STATION_ID AND c.year='+(*state).CalcYear+" and c.crop_id='"+CropId+"'"

	MeteorologyYield=DC_GetdataFromDB_Str(7,Sqlstr,N_RECORDS = NumReocrd)
	IF NumReocrd EQ 0 THEN BEGIN
;;		 DC_B_CLEARUP_DATA,EventTop
		 Prompt=DIALOG_MESSAGE('数据库中"'+ProName+CropName+(*state).CalcYear+'年'+ $
		                       '或没有相应站点估算产量或没有站点生物量,请查数据库!',TITLE='提示',/INFORMATION)
	 	 RETURN
	ENDIF


    RowsNum = NumReocrd   & ColumnLabel=['站号','估算产量','生物量','收获指数','经度','纬度']
    Widget_Control,(*state).FloatYield_Table,TABLE_YSIZE=RowsNum,SET_VALUE=MeteorologyYield[1:*,0:*] $
              ,ROW_LABELS=MeteorologyYield[0,0:*],COLUMN_LABELS=ColumnLabel,ALIGNMENT=0 $
              ,COLUMN_WIDTHS=60,SET_UVALUE=MeteorologyYield              ;用户值用于保存用.
    Widget_Control,(*state).prompt_TEXT ,SET_VALUE='提示:当前选择的是'+ProName+(*state).CalcYear+'年模拟的'+CropName+ $
                '站点收获指数!共'+ STRTRIM(RowsNum,2)+'个站点!'

END
;****依据插值图进行产量统计的"统计"按钮**********************************
PRO DC_B_StatisticEV,EVENT

	Widget_Control,Event.top,GET_UVALUE=state
	CropName =(*state).CropName
	CROP     =(*state).CropIDlist                ;定义作物ID号,注意要对应作物DROPLIST
	crop_id   = CROP[(*state).Cropindex]      &   year=(*state).CalcYear
	Crop_name = STRCOMPRESS(CropName[(*state).Cropindex],/REMOVE_ALL)

	IF (*state).TempCalcYear NE '' THEN BEGIN  ;这一步是当使用现有HI和产量进行统计的情形
		crop_id  = CROP[(*state).TempCropindex]
		year  = (*state).TempCalcYear
	    Crop_name = STRCOMPRESS(CropName[(*state).TempCropindex],/REMOVE_ALL)
	ENDIF

	Widget_control,/HOURGLASS

	CATCH, Error_status               ;截取错误.
	IF Error_status NE 0 THEN BEGIN
		infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
		CATCH, /CANCEL
		RETURN
	ENDIF

        IF  (*state).SaveYieldfile EQ '' THEN BEGIN

			Prompt=DIALOG_MESSAGE('请先计算得到产量分布图后再统计!',/INFORMATION,TITLE='提示')
			RETURN

         ENDIF ELSE BEGIN
           Yfile=(*state).SaveYieldfile
         ENDELSE

  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='统计',MESSAGE='正在统计处理中,请稍候!') ;新建进度条对象
	 progressTimer->START
     progressTimer->UPDATE,(0.1 * 100.0)  ;更新进度条

     PaddyCrop=['21','22','23']
     IF WHERE(PaddyCrop EQ crop_id[0]) EQ -1 THEN BEGIN
        LandWeightFile='data_grid\DRY_LAND_ratio'                         ;如果不是水田,则用旱地百分比文件.
     ENDIF ELSE BEGIN
        LandWeightFile='data_grid\PADDY_FIELD_ratio'
     ENDELSE

;   得到的Y_Result是结构体,其中CountyID(县id,非县代码),FloatYield(平均值)就是需要的产量
	Y_Result = DC_B_StatisticFloat(Yfile,LandWeightFile,STATUS = status1)
    IF status1 EQ 0 THEN BEGIN   ;统计不成功返回
        OBJ_DESTROY,progressTimer
		RETURN
	ENDIF
;   得到的HI_Result是结构体,其中CountyID(县id,非县代码),FloatYield就是需要的收获指数HI
	HI_Result = DC_B_StatisticFloat((*state).Savefile,LandWeightFile,STATUS = status2)
    IF status2 EQ 0 THEN BEGIN
        OBJ_DESTROY,progressTimer
		RETURN
	ENDIF

    progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条

    RowsNUM = N_elements(Y_Result)

	StatisticYield = [TRANSPOSE(STRTRIM(Y_Result.FloatYield,2)), $
					  TRANSPOSE(STRTRIM(Y_Result.CountyID,2)), 	 $
					  TRANSPOSE(STRTRIM(HI_Result.FloatYield,2))]
 	ProName = (*state).Province[WHERE((*state).ProIDlist EQ (*state).ProID)]

    SQLstr='select code,raster_value from COUNTY_CODE_RASTER where LEFT(code,2)='+(*state).ProID
;;	SQLstr='select b.code,b.raster_value,a.name from county_code a,('+SQLstr+') b where a.code=b.code'
	CountyCodeID = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = n)  ;这一步是为获得County_code_raster的县代码.
	IF n EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('基础表COUNTY_CODE_RASTER中没有'+ProName+'的县码对应索引!',TITLE='警告')
        OBJ_DESTROY,progressTimer
		RETURN
	ENDIF

    progressTimer->UPDATE, (0.8 * 100.0)  ;更新进度条

     CountyMeteoYield=STRARR(3,1)
     FOR i=0,RowsNUM-1 DO BEGIN
         MeteoYield=STRARR(3,1)
         FOR j=0,n-1 DO BEGIN
            IF FIX(StatisticYield[1,i]) EQ FIX(CountyCodeID[1,j]) THEN BEGIN
               MeteoYield[0,0]=CountyCodeID[0,j]	;县代码
               MeteoYield[1,0]=StatisticYield[0,i]  ;根据CountyID与raster_value的对应,产量建立
               MeteoYield[2,0]=StatisticYield[2,i]  ;根据CountyID与raster_value的对应,收获指数
               CountyMeteoYield=[[CountyMeteoYield],[MeteoYield]]
               BREAK
            ENDIF
         ENDFOR
     ENDFOR

    TempYield=CountyMeteoYield[0:*,1:*]     ;该数组第一列为县代码,第二列为统计的产量.第三列为收获指数
    TempYield[1,*] = STRTRIM(FLOAT(TempYield[1,*])*2/3,2)   ;统计的产量单位:g/m2,这里转为公斤/亩
    CountyMeteoYield = 0B 					;释放内存
    progressTimer->UPDATE,(0.9*100.0)  	;更新进度条

	CropYear={CropID:crop_id,Year:year}
    n=N_ELEMENTS(TempYield)/3
    SimuYield = [TempYield,STRARR(1,N)+Crop_name,STRARR(1,N)+year]
    Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=3
    Widget_Control,(*state).StaYield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=n,SET_VALUE=SimuYield $
              ,COLUMN_LABELS=['县码','模拟产量','收获指数','作物','年份'],ALIGNMENT=1 $
              ,ROW_LABELS=STRTRIM(INDGEN(n)+1,2),COLUMN_WIDTHS=68,SET_UVALUE=CropYear
;;    Widget_Control,(*state).StaYield_LABEL,SET_VALUE='统计产量(统计共'+STRTRIM(n,2)+'个县)'
    Widget_Control,(*state).StaYield_LABEL,SET_VALUE='收获指数法计算的单产结果'
     progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
	 OBJ_DESTROY,progressTimer
   Prompt=DIALOG_MESSAGE('统计完成!',TITLE='提示',/INFORMATION)

END
;**********************将产量输入到数据库中*******************************************
PRO DC_B_InputDB_EV,Event
     Widget_Control,Event.top ,GET_UVALUE=state
     Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield $
                   ,GET_UVALUE=CropYear                       ;该变量带有作物ID和年份
      Widget_control,/HOURGLASS

     CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('没有产量数据,请您先进行统计!',TITLE='提示',/INFORMATION)
        RETURN
     ENDIF

  	 CountyYield = STRTRIM(TEMPORARY(CountyYield),2)
     crop_id=CropYear.cropid    &   CalYear=CropYear.year

      CountyNum=N_ELEMENTS(CountyYield)/5

	 COMMON COMMON_BLOCK

	progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='正在将估算产量数据入到库中,请稍候...' $
							,TITLE='估算单产入库')
	progressTimer->START                         ;启动进度条

	 FOR i=0,CountyNum-1 DO BEGIN     ;将插值后统计的各县估算和收获指数入库.

		progressTimer->UPDATE,i*1./CountyNum*100

		Sqlstr1="delete from COUNTY_ESTIMATED_YIELD where crop_id='"+ crop_id+ $
			    "' and Year="+CalYear+" and model_type_id=6 and county_code='" $
			    +CountyYield[0,i]+"'"
		Sqlstr2="insert into COUNTY_ESTIMATED_YIELD values('"+crop_id+"','"+ $
				CountyYield[0,i]+"',"+STRTRIM(CountyYield[1,i],2)+',6,' $
				+CalYear+')'
		DBobj->ExecuteSQL,Sqlstr1
		DBobj->ExecuteSQL,Sqlstr2

		Sqlstr1="delete from Harvest_Index_County where crop_id='"+ crop_id+ $
			    "' and Year="+CalYear+" and county_code='"+CountyYield[0,i]+"'"
		Sqlstr2="insert into Harvest_Index_County values('"+CountyYield[0,i]+"','" $
				 +crop_id+"',"+STRTRIM(CountyYield[2,i],2)+','+CalYear+')'
		DBobj->ExecuteSQL,Sqlstr1
		DBobj->ExecuteSQL,Sqlstr2

	 ENDFOR

  	progressTimer->UPDATE,100.0
  	OBJ_DESTROY,progressTimer

	;--加权到省-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,这里用CROP_AREA_COUNTY
	;注意下面取作物面积时,如果当前年份的面积数据没有,则用库中最近年份Newestyear的面积数据

	INFO = DIALOG_MESSAGE('还需要将县估算产量加权到省吗?',/QUESTION,TITLE='询问')
	IF INFO EQ 'No' THEN RETURN
	CountyYield = CountyYield[0:1,*]		;2列值,县码\估算产量
	Status = DC_WeightToPro(CountyYield,Crop_id,CalYear,(*state).ProID,'6')
	IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('加权计算成功!',/INFORMATION,TITLE='提示')

END
;*************************************************************************
PRO DC_Bio_HI_event,event
   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
   wWidget =  Event.top

     CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['运算中止,原因如下:' $
                                   ,[!ERROR_STATE.MSG]],TITLE='错误',/ERROR,DIALOG_PARENT=Event.top)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF

   Widget_control,/HOURGLASS
   Widget_Control,wWidget,GET_UVALUE=state
  CASE wTarget OF
   Widget_Info(wWidget, FIND_BY_UNAME='Crop_DROPLIST'): BEGIN
	 	DC_B_CLEARUP_DATA,event.top
         (*state).Cropindex=Event.index

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF
       DC_B_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Pro_DROPLIST'): BEGIN
		DC_B_CLEARUP_DATA,event.top
         (*state).ProID = (*state).ProIDList[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF
       DC_B_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Year_DROPLIST'): BEGIN
        DC_B_CLEARUP_DATA,event.top
		Widget_Control,EVENT.ID,GET_VALUE= Datayear
        (*state).CalcYear=Datayear[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF
       DC_B_TakeSimpleDotData,event.top
   END
   ;------------进行空间插值-----------------------------
   Widget_Info(wWidget, FIND_BY_UNAME='Next_Extrapolate_bu'): BEGIN   ;进行"空间插值"按钮事件

      Widget_Control,(*state).FloatYield_Table,GET_VALUE=FloatYield
;;      CropList=['','春小麦','冬小麦' ,'早稻','中稻','晚稻','玉米','大豆']
      CropList = STRCOMPRESS((*state).CropName,/REMOVE_ALL)
		IF (*state).Cropindex EQ 0 THEN BEGIN
		    Prompt=DIALOG_MESSAGE('您还没有选择作物!',TITLE='提示',/INFORMATION)
		    RETURN
		ENDIF

		IF FloatYield[0,0] EQ '' THEN BEGIN       ;用第一个进行判断.
		   Prompt=DIALOG_MESSAGE('没有"'+CropList[(*state).Cropindex]+'"站点收获指数!',TITLE='警告')
		ENDIF ELSE BEGIN
	        ;定义作物英文名称,注意要对应作物DROPLIST
;;			CropName=['','SpringWheat','WinterWheat','EarlyRice','SemiRice','LateRice','Maize','Soybean']
			CropName=['','SpringWheat','WinterWheat','EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			crop_id=CropName[(*state).Cropindex]    &   year=(*state).CalcYear

			SAVE_FILE=DIALOG_PICKFILE(TITLE='保存插值图(最好不要改动自动设置的文件名)：',FILE=year+'HI'+crop_id  $
			           ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)
			IF SAVE_FILE EQ '' THEN RETURN

			(*state).Savefile = SAVE_FILE   ;注意文件名，没有带扩展名的

			progressTimer = Obj_New("ShowProgress",tlb,MESSAGE='数据处理中,请稍候!',TITLE='空间插值') ;新建进度条对象
			progressTimer->START
			progressTimer->UPDATE,(0.2 * 100.0)  ;更新进度条

			;----生成插值影像文件-------------------------------
	        ProviceName = (*state).province[WHERE((*state).proIDlist EQ (*state).proID)]
			ParaInfo = (*state).ProjectPara    ;得到省范围内的投影参数
	        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;即返回的不是结构体,而是空.
	        	PRMPT = DIALOG_MESSAGE('没有'+ProviceName+'的基础参数信息,请查看相应的参数设置文件!',TITLE='警告')
	        	RETURN
	        ENDIF

	        samples    = ParaInfo.samples		  & lines = ParaInfo.lines
	        ULX        = ParaInfo.UlX			  & ULY   = ParaInfo.UlY
	        Resolution = ParaInfo.resolution      & CenterMedian = ParaInfo.CenMeridian
			Lon_Lat = FLOAT(FloatYield[4:5,0:*])  & value = FLOAT(FloatYield[3,0:*])
			;此处的插值方法DC_CREATE_INTERP_GRID()还有待于改进
			InterpData = DC_CREATE_INTERP_GRID(Lon_Lat,value,ULX,ULY,Resolution,samples,lines,'TAVE')

			PaddyCrop=[3,4,5] ;对应于PaddyCrop=['EarlyRice','SemiRice','LateRice']
			IF WHERE(PaddyCrop EQ (*state).Cropindex) EQ -1 THEN BEGIN
				FarmlandFile='data_grid\farm_drought'                 ;如果不是水田,则用旱地的百分比文件.
			ENDIF ELSE BEGIN
				FarmlandFile='data_grid\farm_rice'
			ENDELSE

			FarmData = DC_Read_ENVIData(FarmlandFile,SUCCESSSTATUS = Status,DESCRIPTION='耕地层基础文件的')
			IF Status EQ 0 THEN RETURN

			ZeroValue=WHERE(FarmData EQ 0,COMPLEMENT=NoZero)         ;为取得省耕地边界外的坐标索引
            (*state).AddMin = 300.0-MIN(InterpData[NoZero])
			InterpData[ZeroValue]=0                                     ;将耕地边界外的值赋为0

			SavedImage = InterpData										;插值后并去除非耕地后将被保存的数据

			InterpData[NoZero]   = InterpData[NoZero]+(*state).AddMin   ;将耕地内的值均加上300

	        WIDGET_CONTROL,(*state).Extrapolate_DRAW,SET_UVALUE = InterpData

			progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条

	        DataType = SIZE(SavedImage,/TYPE) & ImageData = PTR_NEW(SavedImage,/NO_COPY)
			DC_SaveImageFile,SAVE_FILE,ImageData,samples,lines,DataType,'Unknown',ULX,ULY,Resolution,CenterMedian
	 		PTR_FREE,ImageData
			;-----------------------------------
			progressTimer->UPDATE, (0.95 * 100.0)  ;更新进度条
;;SAVE_FILE='H:\temp\2006Maize'   ;测试用
			DC_Draw_image,SAVE_FILE,(*state).Extrapolate_DRAW,oView = oView,MINVALUE=299.99
			OBJ_DESTROY,(*state).oView
			(*state).oView = oView

			progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
;;			progressTimer->DESTROY ;销毁进度条
			OBJ_DESTROY,progressTimer

			Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=1

		ENDELSE
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Return_DRAW'): BEGIN

      Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0

   END

   Widget_Info(wWidget, FIND_BY_UNAME='ReturnY_DRAW'): BEGIN  ;产量空间分布中的"返回"

      Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=1

   END

   Widget_Info(wWidget, FIND_BY_UNAME='Return_yield'): BEGIN

        Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=1

   END

   Widget_Info(wWidget, FIND_BY_UNAME='SaveDotYield_bu'): BEGIN

      Widget_Control,(*state).FloatYield_Table,GET_VALUE=Yield,GET_UVALUE=SimpleDotYield
      ProName = (*state).province[WHERE((*state).ProIDlist EQ (*state).ProID)]
      CropName=STRCOMPRESS((*state).CropName[(*state).Cropindex],/REMOVE_ALL)
	CASE 1 OF
     (*state).Cropindex EQ 0: BEGIN
        Prompt=DIALOG_MESSAGE('您还没有选择作物!',TITLE='提示',/INFORMATION)
        RETURN
     END

     Yield[0,0] EQ '' :BEGIN
        Prompt=DIALOG_MESSAGE('表中没有"'+ProName+(*state).CalcYear+'年'+CropName+'"站点HI数据!',TITLE='警告')
        RETURN
     END
	 ELSE:
	ENDCASE

      RowsNum=N_ELEMENTS(SimpleDotYield)/7
      Estimation_yield=STRARR(9,RowsNum+1)
      Estimation_yield[0:*,0]=['作物名','年份','站名','站号','估算产量','生物量','HI','经度','纬度']
      Estimation_yield[0:1,1:*]=[STRARR(1,RowsNum)+CropName,STRARR(1,RowsNum)+(*state).CalcYear]
	  Estimation_yield[2:*,1:*]=SimpleDotYield
	  DC_SaveTextData,Estimation_yield,EVENT.ID $
				   ,FILENAME=(*state).CalcYear+ProName+CropName+'站点HI.txt'

   END

  Widget_Info(wWidget, FIND_BY_UNAME='SaveStatice_bu'): BEGIN

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield,GET_UVALUE=CropYear

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('没有产量数据,请您先进行统计!',TITLE='提示',/INFORMATION)
        RETURN
     ENDIF

      Widget_control,/HOURGLASS
      year=CropYear.year   &   crop_id=CropYear.cropid
      CropName=STRCOMPRESS((*state).Cropname[WHERE((*state).CropIdList EQ crop_id[0])],/REMOVE_ALL)
	  ProName = (*state).province[WHERE((*state).ProIDlist EQ (*state).ProID)]
      CountyYield=[['县代码','模拟产量','收获指数','作物','年份'],[CountyYield]]
      DC_SaveTextData,CountyYield,EVENT.ID,FILENAME=year+'年'+ProName+CropName+'作物估算产量.txt'

   END

   Widget_Info(wWidget, FIND_BY_UNAME='Extrapolate_DRAW'): BEGIN  ;插值draw组件中的事件

        IF NOT OBJ_VALID((*state).oView) THEN RETURN   ;如果是空对象则返回.

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
            ImageSize = SIZE(Imagedata,/DIMENSIONS)

		    (*state).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]  & ySize = ImageSize[1] ;影像宽高
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;之所以按行反转,是因为DRAW组件坐标是从左下角开始的.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(dataValue-(*state).AddMin,2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

   Widget_Info(wWidget, FIND_BY_UNAME='Yield_DRAW'): BEGIN  ;产量draw组件中的事件

        IF NOT OBJ_VALID((*state).YView) THEN RETURN   ;如果是空对象则返回.

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
            ImageSize = SIZE(Imagedata,/DIMENSIONS)

		    (*state).YView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]  & ySize = ImageSize[1] ;影像宽高
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;之所以按行反转,是因为DRAW组件坐标是从左下角开始的.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(dataValue,2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

     Widget_Info(wWidget, FIND_BY_UNAME='FactorType'): BEGIN

		DC_B_CLEARUP_DATA,event.top

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF

		WIDGET_CONTROL,EVENT.ID,GET_VALUE = value

		SelId = WHERE(value EQ 1,COUNT)
		IF COUNT EQ 0 THEN BEGIN
			(*state).StationYieldID = ''
		ENDIF ELSE BEGIN
			ModelId = ['2','4']
			(*state).StationYieldID = STRJOIN(ModelId[SelId], ',')
		ENDELSE
       DC_B_TakeSimpleDotData,event.top

    END

     Widget_Info(wWidget, FIND_BY_UNAME='CalcYield_bu'): BEGIN

    	ProName  = (*state).province[WHERE((*state).ProIDlist EQ (*state).ProID)]

    	CalcYear = (*state).CalcYear
    	Cropname = STRCOMPRESS((*state).CropName[(*state).Cropindex],/REMOVE_ALL)

        YN_TakeSaveFile = 0
        IF  (*state).Savefile EQ '' THEN BEGIN

			Prompt=DIALOG_MESSAGE(['您还没有空间插值生成HI图,请先插值!如果要统计已有的HI图,' $
			                      ,'请按"是(Y)"钮,否则按"否(N)"回到上一步先进行插值!'] $
			                      ,TITLE='询问',/QUESTION)
			IF Prompt EQ 'No' THEN BEGIN
				Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0
				RETURN
			ENDIF
			Filename=DIALOG_PICKFILE(TITLE='请选择现有的HI图像文件：', FILTER=['*.hdr'],PATH=DC_PathSetting(),/MUST_EXIST, DIALOG_PARENT=Event.id)

			IF Filename EQ '' THEN RETURN

			StatisMapFile=STRMID(STRTRIM(Filename,2),0,STRLEN(Filename)-4)    ;是为去掉".hdr"

			CropNameEnglish=['','SpringWheat','WinterWheat' ,'EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			Pos = STRPOS( StatisMapFile,'\',/REVERSE_SEARCH)
			E_crop=STRMID(StatisMapFile,Pos+7)                                ;英文的作物名
			aa = WHERE(CropNameEnglish EQ E_crop,Count)
			IF Count eq 0 THEN BEGIN
			   ;正确命名的文件:年(4位)+HI+作物英文名,如"2006HISpringWheat"
			   Prompt=DIALOG_MESSAGE(['不是正确命名的HI图像文件!' $
			   						 ,'正确命名形如"2006HISpringWheat"'],TITLE='警告')
			   RETURN
			ENDIF

			CalcYear=STRMID(StatisMapFile,Pos+1,4)
			Cropname = STRCOMPRESS((*state).CropName[aa],/REMOVE_ALL)

			(*state).TempCropindex = aa[0]
			(*state).TempCalcYear  = CalcYear

			HIfile=StatisMapFile

			InterpData = DC_Read_ENVIData(HIfile,SUCCESSSTATUS = Status,DESCRIPTION='所选文件的')
			IF Status EQ 0 THEN RETURN

			WIDGET_CONTROL,(*state).Extrapolate_DRAW,SET_UVALUE = InterpData

			DC_Draw_image,HIfile,(*state).Extrapolate_DRAW,oView=oView ,MINVALUE=0.001
			OBJ_DESTROY,(*state).oView
			(*state).oView = oView

        	YN_TakeSaveFile = 1
        	(*state).Savefile =HIfile
         ENDIF ELSE BEGIN
           HIfile=(*state).Savefile
         ENDELSE

      IF YN_TakeSaveFile EQ 1 THEN BEGIN
         Widget_Control,(*state).prompt_TEXT ,SET_VALUE='提示:当前选择统计的是"'+ProName+'"'+CalcYear+'年'+Cropname+'的收获指数和产量!'
      END

    	ProBiomassFile = DIALOG_PICKFILE(TITLE='请选择"'+ProName+CalcYear+'年'+Cropname+'"作物生物量空间数据文件' $
    					   ,PATH=DC_PathSetting(),FILTER ='*.hdr;*.HDR',/MUST_EXIST,DIALOG_PARENT=EVENT.ID)

		IF ProBiomassFile EQ '' THEN  RETURN

		HIinfo = DC_ReadHead_file(HIfile)    ;收获指数文件
		Binfo  = DC_ReadHead_file(ProBiomassFile)		;生物量文件

		CASE 1 OF
			SIZE(HIinfo,/TYPE) NE 8: RETURN
			SIZE(Binfo,/TYPE) NE 8: RETURN
			FIX(HIinfo.samples) NE FIX(Binfo.samples) OR FIX(HIinfo.lines) NE FIX(Binfo.lines): BEGIN
				Info = DIALOG_MESSAGE('选择的生物量数据与外推生成的收获指数文件大小不一致!',TITLE='警告')
				RETURN
			END
		  ELSE:
		ENDCASE

 		DataHI = DC_Read_ENVIData(HIfile,SUCCESSSTATUS = HIstaus,DESCRIPTION='收获指数的')
 		DataB  = DC_Read_ENVIData(ProBiomassFile,SUCCESSSTATUS = Bstaus,DESCRIPTION='生物量数据的')

		CASE 0 OF
			HIstaus: RETURN
			Bstaus : RETURN
		  ELSE:
		ENDCASE

    	YieldFile = DIALOG_PICKFILE(TITLE='保存空间产量数据',GET_PATH = PathNew $
                        	,PATH=DC_PathSetting(),/WRITE,FILTER ='*.hdr;*.HDR' $
                        	,FILE=CalcYear+ProName+Cropname+'Yield.hdr',/OVERWRITE_PROMPT $
                        	,DEFAULT_EXTENSION='hdr',DIALOG_PARENT=EVENT.ID)

		IF YieldFile EQ '' THEN  RETURN

		(*state).SaveYieldfile = YieldFile

		SavedImage = DataB*DataHI
		WIDGET_CONTROL,(*state).Yield_DRAW,SET_UVALUE = SavedImage

        samples    = Binfo.samples		  &        lines = Binfo.lines
        ULX        = Binfo.UlX			  &        ULY   = Binfo.UlY
        Resolution = Binfo.resolution     & CenterMedian = Binfo.CenterMedian

        DataType = SIZE(SavedImage,/TYPE) & ImageData = PTR_NEW(SavedImage,/NO_COPY)
		DC_SaveImageFile,YieldFile,ImageData,samples,lines,DataType,'Unknown',ULX,ULY,Resolution,CenterMedian
 		PTR_FREE,ImageData

;	YieldFile='H:\temp\2006Maize'   ;测试用
		DC_Draw_image,YieldFile,(*state).Yield_DRAW,oView=YView,MINVALUE=0.001
		OBJ_DESTROY,(*state).YView
		(*state).YView = YView

		Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=2
    END

   Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
    	if file_test('help\help.chm') then begin
    		ONLINE_HELP, BOOK='help\help.chm',/FULL_PATH,"'生物量-产量分析'"
    	endif else begin
			info_help=dialog_message('找不到帮助文档',title='警告')
		endelse
    end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,"'生物量-产量分析'"

   Widget_Info(wWidget, FIND_BY_UNAME='quit_bu'):begin
   	common_log,'关闭收获指数法计算'
   	widget_control,Event.top,/DESTROY
   end
  ELSE:
  ENDCASE

END
;************空间外推插值界面*******************************
PRO DC_Bio_HI,GROUP_LEADER=groupleader

	common_log,'启动收获指数法计算'
   IF ( XREGISTERED('DC_Bio_HI') NE 0 ) THEN RETURN

  TLB = Widget_Base( GROUP_LEADER=groupleader, UNAME='TLB' ,XOFFSET=300 ,YOFFSET=200  $
      ,SCR_XSIZE=466 ,SCR_YSIZE=399 ,TITLE='收获指数法计算',SPACE=1  $
      ,XPAD=0 ,YPAD=0,COLUMN=1 ,TLB_FRAME_ATTR=1,/TAB_MODE)
;-----------------------------------------------------------------------------------
  Widegt_TAB = Widget_Tab(TLB,UNAME='Widegt_TAB' ,/FRAME,SCR_XSIZE=450 ,SCR_YSIZE=335)
;----------****************-站点收获指数------------------------------
   tab0 = Widget_Base(Widegt_TAB, UNAME='tab0'  $
		      ,SCR_YSIZE=308 ,TITLE='站点收获指数计算' ,SPACE=1  $
		      ,XPAD=0 ,YPAD=0,/COLUMN,/BASE_ALIGN_LEFT)
		 ;--------------------------------------------
		 Crop_year = Widget_Base(tab0, UNAME='Crop_year' ,FRAME=1,SCR_XSIZE=450 $
		  		 ,SCR_YSIZE=25 ,SPACE=1,XPAD=0 ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)
			Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
						,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
						,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			CropName = ['未选择','春小麦','冬小麦','早  稻','中  稻','晚  稻','春玉米','夏玉米','大  豆']
			CropIDList = ['','11','12','21','22','23','31','32','41']				;CropName与CropIDList应对应
			ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;这样随年份变化,系统下列表中的年份也会变化.

;			Pro_Droplist  = Widget_Droplist(Crop_year,UNAME='Pro_DROPLIST',TITLE='省名:')
			Year_Droplist = Widget_Droplist(Crop_year,UNAME='Year_DROPLIST',TITLE='年份:')

			SeperateLine1 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)

			FactorType = CW_BGROUP(Crop_year, UNAME='FactorType',['气象','遥感'],/ROW,/NONEXCLUSIVE $
					,SPACE=0, LABEL_LEFT='类型:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=18);,XSIZE=145)

			SeperateLine2 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)
			Crop_Droplist = Widget_Droplist(Crop_year,UNAME='Crop_DROPLIST',TITLE='作物:')
		;----------------------------表----------------
		 FloatYield_Table = Widget_Table(tab0, UNAME='FloatYield_Table'$
		 			,SCR_XSIZE=450,SCR_YSIZE=250 ,XSIZE=6 ,YSIZE=20,/FRAME)
		;----------------------------------------------------------------------------------
		 Save_Extrapo_base = Widget_Base(tab0, UNAME='Save_Extrapo_base',SCR_XSIZE=450,/FRAME $
		     , SCR_YSIZE=27,SPACE=60 ,XPAD=38 ,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)
             Width_C = 80
			 SaveDotYield_bu = Widget_Button(Save_Extrapo_base, UNAME='SaveDotYield_bu'  $
			      ,SCR_XSIZE=Width_C,SCR_YSIZE=20 ,TOOLTIP='将站点收获指数保存到本地磁盘中' ,VALUE='保存')

			 Next_Extrapolate_bu = Widget_Button(Save_Extrapo_base, UNAME='Next_Extrapolate_bu'  $
			      ,SCR_XSIZE=Width_C,SCR_YSIZE=20 ,TOOLTIP='下一步进行空间插值' ,VALUE='插值>>')

			 Help_bu = Widget_Button(Save_Extrapo_base, UNAME='Help_bu'  $
			      ,SCR_XSIZE=Width_C,SCR_YSIZE=20  ,VALUE='帮助')
 ;------*****************-空间外推插值--------------------------------------
  tab1 = Widget_Base(Widegt_TAB, UNAME='tab1' ,FRAME=1  $
      ,SCR_XSIZE=450 ,SCR_YSIZE=310 ,TITLE='站点HI空间外推' ,SPACE=0  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER)

		  Extrapolate_DRAW = Widget_Draw(tab1, UNAME='Extrapolate_DRAW' ,RETAIN=2  $
		     ,SCR_XSIZE=440 ,SCR_YSIZE=275 ,GRAPHICS_LEVEL=2 $
		     ,/BUTTON_EVENTS,/MOTION_EVENTS)

;;		 Cusor_LABEL = Widget_Label(tab1,UNAME='Cusor_LABEL' $
;;		      ,SCR_XSIZE=266 ,SCR_YSIZE=12 ,/ALIGN_CENTER,VALUE='指针所指波动产量值：')

		  Button_BASE = Widget_Base(tab1, UNAME='Button_BASE'   $
		      ,SCR_XSIZE=440 ,SCR_YSIZE=31,SPACE=70 ,XPAD=60 ,YPAD=0  $
		      ,/ROW,FRAME=0,/BASE_ALIGN_TOP)

		  Return_DRAW = Widget_Button(Button_BASE, UNAME='Return_DRAW'  $
		      ,SCR_XSIZE=119 ,SCR_YSIZE=20,/ALIGN_CENTER ,VALUE='<<返回')

		  CalcYield_bu = Widget_Button(Button_BASE,  $
		      UNAME='CalcYield_bu' ,SENSITIVE=1,SCR_XSIZE=119 ,SCR_YSIZE=20 $
		     ,/ALIGN_CENTER ,VALUE='产量计算>>')
 ;------*****************-产量空间分布--------------------------------------
  tab3 = Widget_Base(Widegt_TAB, UNAME='tab3' ,FRAME=1  $
      ,SCR_XSIZE=450 ,SCR_YSIZE=310 ,TITLE='产量空间分布' ,SPACE=0  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER)

		  Yield_DRAW = Widget_Draw(tab3, UNAME='Yield_DRAW' ,RETAIN=2  $
		     ,SCR_XSIZE=440 ,SCR_YSIZE=275 ,GRAPHICS_LEVEL=2 $
		     ,/BUTTON_EVENTS,/MOTION_EVENTS)

		  Bu_BASE = Widget_Base(tab3, UNAME='Bu_BASE'   $
		      ,SCR_XSIZE=440 ,SCR_YSIZE=31,SPACE=70 ,XPAD=60 ,YPAD=0  $
		      ,/ROW,FRAME=0,/BASE_ALIGN_TOP)

		  ReturnY_DRAW = Widget_Button(Bu_BASE, UNAME='ReturnY_DRAW'  $
		      ,SCR_XSIZE=119 ,SCR_YSIZE=20,/ALIGN_CENTER ,VALUE='<<返回')

		  Next_StaResult_bu = Widget_Button(Bu_BASE,  $
		      UNAME='Next_StaResult_bu' ,SENSITIVE=1,SCR_XSIZE=119 ,SCR_YSIZE=20 $
		     ,/ALIGN_CENTER ,VALUE='统计>>',EVENT_PRO='DC_B_StatisticEV' $
		     ,TOOLTIP='依据插值结果统计各县收获指数')
;-----*******************-产量统计*************-----------------
  tab2 = Widget_Base(Widegt_TAB, UNAME='tab2' $
      ,SCR_XSIZE=450 ,SCR_YSIZE=310 ,TITLE='收获指数统计及产量计算' ,SPACE=1  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_LEFT,/FRAME)

		  StaYield_LABEL = Widget_Label(tab2, UNAME='StaYield_LABEL'  $
		      ,XOFFSET=46 ,YOFFSET=3 ,SCR_XSIZE=346 ,SCR_YSIZE=15  $
		      ,/ALIGN_CENTER ,VALUE='收获指数法计算的单产结果')

		  StaYield_TABLE = Widget_Table(tab2, UNAME='StaYield_TABLE'  $
		      ,SCR_XSIZE=445 ,SCR_YSIZE=253 ,XSIZE=6  $
		      ,YSIZE=20,/FRAME)
		  ;----------------------------
		  Button_BASE_0 = Widget_Base(tab2, UNAME='Button_BASE_0'  $
		      ,XOFFSET=3 ,YOFFSET=273 ,SCR_XSIZE=432 ,SCR_YSIZE=29  $
		      ,SPACE=40 ,XPAD=30 ,YPAD=1 ,ROW=1)
		   BuWidth=100  &  BuHeight=20
		  Return_yield = Widget_Button(Button_BASE_0, UNAME='Return_yield'  $
		      ,XOFFSET=50 ,YOFFSET=4 ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight  $
		      ,/ALIGN_CENTER ,VALUE='<<返回',TOOLTIP='返回查看空间插值图')

		  InputDB_yield = Widget_Button(Button_BASE_0, UNAME='InputDB_yield'  $
		      ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight,/ALIGN_CENTER ,VALUE='入库' $
		      ,TOOLTIP='将估算的产量保存到数据库中',EVENT_PRO='DC_B_InputDB_EV')

		  SaveStatice_bu = Widget_Button(Button_BASE_0, UNAME='SaveStatice_bu' ,XOFFSET=239  $
		      ,YOFFSET=4 ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight ,/ALIGN_CENTER  $
		      ,VALUE='保存',TOOLTIP='将估算的产量保存到本地磁盘中')

  ;-----**************界面最下面的提示文本************-------------
  Prompt_BASE = Widget_Base(TLB,SCR_XSIZE=450 ,SCR_YSIZE=35 ,SPACE=3,XPAD=1  $
      ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  prompt_TEXT = Widget_Text(Prompt_BASE, UNAME='prompt_TEXT' ,FRAME=1  $
	      ,SCR_XSIZE=390 ,SCR_YSIZE=25 ,XSIZE=20  $
	      ,YSIZE=1,VALUE='提示：')
	  quit_bu = Widget_Button(Prompt_BASE, UNAME='quit_bu' ,XOFFSET=348  $
	      ,YOFFSET=5 ,SCR_XSIZE=55 ,SCR_YSIZE=23  $
	      ,TOOLTIP='退出空间插值模块' ,VALUE='关闭')
;---------------------------------------------------------------------------

		PRO_COMMON_DC
		COMMON DC_BLOCK,NewCropID
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

		ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
		CropID = NewCropID
		CalcYear = strmid(systime(),3,4,/REVERSE_OFFSET)

;		WIDGET_CONTROL,Pro_Droplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
		WIDGET_CONTROL,Crop_Droplist,SET_VALUE =CropName;,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)
		WIDGET_CONTROL,Year_Droplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)

		Widget_Control,FactorType,SET_VALUE=[0,1]
		Widget_Control,FloatYield_Table ,SET_TABLE_SELECT=[-1,-1,-1,-1]
		Widget_Control,StaYield_TABLE,SET_TABLE_SELECT=[-1,-1,-1,-1]

 		Widget_Control, /REALIZE, TLB
		Widget_Control,quit_bu,/INPUT_FOCUS

		Widget_Control,Extrapolate_DRAW,GET_VALUE=Owindow
		Owindow->ERASE,COLOR=255

		Widget_Control,Yield_DRAW,GET_VALUE=Owindow
		Owindow->ERASE,COLOR=100

		ProjectPara = DC_ReadParameter(Province[WHERE(ProIDList EQ ProCode)])    ;得到省范围内的投影参数,没有则返回为空,否则是结构体.
		state={  Cropindex             :  0 				,$
			     CropIDList			   :  CropIDList		,$	 ;作物ID列表
			     CropName			   :  CropName			,$   ;作物名列表
;			     DataType			   :  0					,$   ;数据类型,默认为气象数据计算的波动产量
			     CalcYear              :  CalcYear	 		,$
       		     ProID				   :  ProCode			,$	  ;被选省ID,2位字符型
			     ProIDList			   :  ProIDList			,$	  ;省ID列表
			     Province			   :  Province			,$    ;省名列表
			     FloatYield_Table  	   :  FloatYield_Table  ,$
			     StaYield_TABLE        :  StaYield_TABLE  	,$
			     StaYield_LABEL        :  StaYield_LABEL 	,$
			     prompt_TEXT           :  prompt_TEXT 		,$
			     Widegt_TAB			   :  Widegt_TAB		,$
			     Extrapolate_DRAW      :  Extrapolate_DRAW  ,$
			     Yield_DRAW     	   :  Yield_DRAW 		,$
			     TempCropindex		   :  0					,$   ;该参数用于记录使用现有数据进行产量和收获指数统计时的作物ID
			     TempCalcYear		   :  ''				,$   ;该参数用于记录使用现有数据进行产量和收获指数统计时的年份
			     oView				   :  OBJ_NEW()			,$		;HI的视图对象
			     YView				   :  OBJ_NEW()			,$		;Yiled的视图对象
			     AddMin				   :  0.0				,$      ;将耕地内的HI插值结果进行识别的参数
			     ProjectPara		   :  ProjectPara		,$		;用于定义省相应范围内的投影参数值(见"text\parametersetting.txt")
			     Savefile              :  ''				,$		;被保存的收获指数文件.
			     SaveYieldfile         :  ''				,$		;被保存的空间分布产量文件.
			     StationYieldID		   :  '4'}						;用于标识估算的站点产量类型,2,站点农气模型估算产量,4为遥感的估算产量

	Widget_Control,TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)

	XManager, 'DC_Bio_HI', TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END