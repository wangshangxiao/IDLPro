
;******统计波动产量到各县************************************
FUNCTION DC_StatisticFloat,StaFile,WeightFile,STATUS = status
	;StaFile  被统计文件;WeightFile权重文件;status计算是否成功的状态
	;调用形式:Re = DC_StatisticFloat(StaFile,WeightFile,[STATUS=status]) 注意只能用status
	;返回值: 各县结构体
	;算法:Sum(Pix*W)/sum(W)  Pix像元值;W为权重

	CountyRaster = 'data_grid\county_raster'

	WeightData = DC_Read_ENVIData(WeightFile,SUCCESSSTATUS = StatusW,Description='耕地权重文件')
	IF NOT StatusW THEN BEGIN		;WeightData在0-100之间
		STATUS=StatusW
		RETURN,0
	ENDIF

	StaData    = DC_Read_ENVIData(StaFile,SUCCESSSTATUS = StatusS,Description='插值文件')
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
		Info = DIALOG_MESSAGE('用于统计的基础数据与插值文件大小不一致,统计失败!',TITLE='警告')
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
PRO DC_CLEARUP_DATA,EventTop
   Widget_Control ,EventTop,GET_UVALUE=state
      Widget_Control,(*state).FloatYield_Table,GET_VALUE=MeteorologyYield & MeteorologyYield=''
      Widget_Control,(*state).FloatYield_Table,SET_VALUE=MeteorologyYield[*,*],ROW_LABELS=''

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=StaYield & StaYield=''
      Widget_Control,(*state).StaYield_TABLE,SET_VALUE=StaYield[*,*],ROW_LABELS=''

      Widget_Control,(*state).StaYield_LABEL,SET_VALUE='插值后的统计产量'

      Widget_Control,(*state).prompt_TEXT ,SET_VALUE=''

      Widget_Control,(*state).Extrapolate_DRAW ,GET_VALUE=Owindow
      Owindow->ERASE,COLOR=255
	  oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	  IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	 (*state).Savefile = ''   ;同时使波动产量文件名为空

   Widget_Control ,EventTop,SET_UVALUE=state
END
;**********自定义过程:提取站点模拟的波动产量************************************
PRO DC_TakeSimpleDotData,EventTop

	ON_ERROR,2
    DC_CLEARUP_DATA,EventTop

    Widget_control,/HOURGLASS

    Widget_Control,EventTop ,GET_UVALUE=state
    CropName = STRCOMPRESS((*state).CropName[(*state).Cropindex],/REMOVE_ALL)
	CropId = (*state).CropIDList[(*state).Cropindex]

;    ProName = (*state).Province[WHERE((*state).ProIDList EQ (*state).ProID)]

;*********关键语句，20070825，杨绍锷**********************************************

;==========原语句=========================================================================
;	IF (*state).DataType THEN ModelData_id = '2' ELSE ModelData_id = '1'
;
;    Sqlstr='select AgroMeteoStation_CODE,Meteo_OR_Fluctuate_Yield,year from AGROSTATION_Crop_MeteoOrFluctuate_yield ' $
;    		+"where crop_id='"+CropId+"' and year="+(*state).CalcYear $
;    		+' and ModelData_id='+ ModelData_id
;    Sqlstr='select b.year,code,name,b.Meteo_OR_Fluctuate_Yield,longitude,latitude ' $
;    		+'from AGRO_METEO_STATION_INFO a,('+Sqlstr+') b '$
;    		+'where a.code=b.AgroMeteoStation_CODE AND LEFT(a.COUNTY_CODE,2)=' $
;    		+(*state).ProID+' order by b.AgroMeteoStation_CODE'
 ;以上为原语句==========================================================================
 ;===========以下为杨绍锷修改，20070825=================================================
; 	IF (*state).DataType THEN ModelData_id = '2' ELSE ModelData_id = '1' ;DataType  0为气象，1为遥感
; 	IF (*state).YieldType THEN table = 'AGROSTATION_' ELSE table = 'COUNTY_'

	;===========修改，20070907=================================================

	;DataType  0为气象，1为遥感
	;YieldType   县(0)  站点(1)
	;
	;ModelData_id
	; 1:由县气象波动产量数据计算;2:县遥感波动产量数据;3:由站点气象产量数据插值而来;4:站点遥感波动产量数据插值而来
	;
	;Model_type_id
	;0：最终单产;1:县统计数据农气模型计算得到的;2:站点数据农业模型得到；3：县遥感数据计算得到；4：站点遥感计算得到；
	;5：生物量比值得到；6：生物量收获系数得到；7：主成分分析第一主成分；8：第一主成分建模得到

	IF (*state).DataType THEN begin
		if (*state).YieldType then ModelData_id = '4' else ModelData_id = '2'
	endif else begin
		if (*state).YieldType then ModelData_id = '3' else ModelData_id = '1'
	endelse

	if (*state).YieldType then begin	;站点(1)
	    Sqlstr='select AgroMeteoStation_CODE,Meteo_OR_Fluctuate_Yield,year from AGROSTATION_Crop_MeteoOrFluctuate_yield ' $
	    		+"where crop_id='"+CropId+"' and year="+(*state).CalcYear $
	    		+' and ModelData_id='+ ModelData_id
	    Sqlstr='select b.year,code,name,b.Meteo_OR_Fluctuate_Yield,longitude,latitude ' $
	    		+'from AGRO_METEO_STATION_INFO a,('+Sqlstr+') b '$
	    		+'where a.code=b.AgroMeteoStation_CODE AND LEFT(a.COUNTY_CODE,2)=' $
	    		+(*state).ProID+' order by b.AgroMeteoStation_CODE'
    endif else begin	;县(0)
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
;		 Prompt=DIALOG_MESSAGE('您还没有对"'+ProName+CropName+'"进行'+(*state).CalcYear+'年'+ $
;		                       '的站点波动产量模拟分析,请先模拟!',TITLE='提示',/INFORMATION)
		 Prompt=DIALOG_MESSAGE('您还没有对"'+CropName+'"进行'+(*state).CalcYear+'年'+ $
		                       '的县波动产量模拟分析,请先模拟!',TITLE='提示',/INFORMATION)
	 	 RETURN
	ENDIF


    RowsNum = NumReocrd   & ColumnLabel=['县代码','县名','波动产量','经度','纬度']
    Widget_Control,(*state).FloatYield_Table, TABLE_YSIZE=RowsNum,SET_VALUE=MeteorologyYield[1:*,0:*] $
              ,COLUMN_LABELS=ColumnLabel,ALIGNMENT=2 $,ROW_LABELS=MeteorologyYield[0,0:*] $
              ,SET_UVALUE=MeteorologyYield              ;用户值用于保存用(含年份).
;    Widget_Control,(*state).prompt_TEXT ,SET_VALUE='提示:当前选择的是'+ProName+(*state).CalcYear+'年模拟的'+CropName+ $
;                '站点波动产量!共'+ STRTRIM(STRING(RowsNum),2)+'个站点!'
	Widget_Control,(*state).prompt_TEXT ,SET_VALUE='提示:当前选择的是'+(*state).CalcYear+'年模拟的'+CropName+ $
                '县波动产量!共'+ STRTRIM(STRING(RowsNum),2)+'个县!'

END
;****依据插值图进行产量统计的"统计"按钮**********************************
PRO DC_StatisticEV,EVENT

	Widget_Control,Event.top ,GET_UVALUE=state
	CropName =(*state).CropName
	CROP     =(*state).CropIDlist                ;定义作物ID号,注意要对应作物DROPLIST
	crop_id   = CROP[(*state).Cropindex]    &   year=(*state).CalcYear
	Crop_name = CropName[(*state).Cropindex]
	YN_TakeSaveFile = 0      ;用来指示是否用已保存的影像来统计:0--否;1--是

	Widget_control,/HOURGLASS

;	CATCH, Error_status               ;截取错误.
;	IF Error_status NE 0 THEN BEGIN
;		infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
;		CATCH, /CANCEL
;		RETURN
;	ENDIF

        IF  (*state).Savefile EQ '' THEN BEGIN              ;用第一个进行判断.
			YN_TakeSaveFile = 1
			Prompt=DIALOG_MESSAGE(['您还没有进行空间插值,请先插值!如果要统计已保存的插值' $
			                 ,'图,请按"是(Y)"钮,否则按"否(N)"回到上一步先进行插值!'],TITLE='提示',/QUESTION)
			IF Prompt EQ 'No' THEN BEGIN
				Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0
				RETURN
			ENDIF
			Filename=DIALOG_PICKFILE(TITLE='请选择插值图像：', FILTER=['*.hdr'],PATH=DC_PathSetting(),/MUST_EXIST, DIALOG_PARENT=Event.id)

			IF Filename EQ '' THEN RETURN

			StatisMapFile=STRMID(STRTRIM(Filename,2),0,STRLEN(Filename)-4)    ;是为去掉".hdr"

			CropNameEnglish=['','SpringWheat','WinterWheat' ,'EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			Pos = STRPOS( StatisMapFile,'\',/REVERSE_SEARCH)
			E_crop=STRMID(StatisMapFile,Pos+5)                                ;英文的作物名
			aa = WHERE(CropNameEnglish EQ E_crop,Count)
			IF Count eq 0 THEN BEGIN
			   Prompt=DIALOG_MESSAGE(['不是正确命名的插值图像文件,' $
			   						 ,'正确命名的形式如"2006SpringWheat"!'],TITLE='警告')
			   RETURN
			ENDIF
			Crop_name=CropName[aa]
			crop_id=CROP[aa]
			year=STRMID(StatisMapFile,Pos+1,4)

			IN_file=StatisMapFile

			PaddyCrop=['21','22','23'] ;对应于PaddyCrop=['EarlyRice','SemiRice','LateRice']
			IF WHERE(PaddyCrop EQ crop_id[0]) EQ -1 THEN BEGIN
				FarmlandFile='data_grid\farm_drought'                         ;如果不是水田,则用旱地百分比文件.
			ENDIF ELSE BEGIN
				FarmlandFile='data_grid\farm_rice'
			ENDELSE

			FarmData = DC_Read_ENVIData(FarmlandFile,SUCCESSSTATUS = Status,DESCRIPTION='耕地层基础文件的')
			IF Status EQ 0 THEN RETURN

			InterpData = DC_Read_ENVIData(IN_file,SUCCESSSTATUS = Status,DESCRIPTION='所选文件的')
			IF Status EQ 0 THEN RETURN

			ZeroValue=WHERE(FarmData EQ 0,COMPLEMENT=NoZero)         ;为取得省耕地边界外的坐标索引
			(*state).AddMin = 300.0-MIN(InterpData[NoZero])
			InterpData[ZeroValue]=0                                     ;将耕地边界外的值赋为0
			InterpData[NoZero]   = InterpData[NoZero]+(*state).AddMin   ;将耕地内的值均加上300

			WIDGET_CONTROL,(*state).Extrapolate_DRAW,SET_UVALUE = InterpData

			DC_Draw_image,IN_file,(*state).Extrapolate_DRAW,oView=oView,MINVALUE=299.99,white=1
			OBJ_DESTROY,(*state).oView
			(*state).oView = oView

         ENDIF ELSE BEGIN
           IN_file=(*state).Savefile
         ENDELSE

      IF YN_TakeSaveFile EQ 1 THEN BEGIN
         Widget_Control,(*state).prompt_TEXT ,SET_VALUE='提示:当前选择统计的是'+year+'年'+Crop_name+'波动产量!'
      END

  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='统计',MESSAGE='正在统计处理中,请稍候!') ;新建进度条对象
	 progressTimer->START
     progressTimer->UPDATE,(0.2 * 100.0)  ;更新进度条

     PaddyCrop=['21','22','23']
     IF WHERE(PaddyCrop EQ crop_id[0]) EQ -1 THEN BEGIN
        LandWeightFile='data_grid\DRY_LAND_ratio'                         ;如果不是水田,则用旱地百分比文件.
     ENDIF ELSE BEGIN
        LandWeightFile='data_grid\PADDY_FIELD_ratio'
     ENDELSE

    Infile=IN_file

;  得到的RESULT是结构体,其中CountyID(县id,非县代码),FloatYield(平均值)就是需要的波动产量
	RESULT = DC_StatisticFloat(Infile,LandWeightFile,STATUS = status)
    IF status EQ 0 THEN RETURN   ;统计不成功返回

    progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条

    RowsNUM = N_elements(RESULT)

	StatisticYield = [TRANSPOSE(STRTRIM(RESULT.FloatYield,2)),TRANSPOSE(STRTRIM(RESULT.CountyID,2))]
; 	ProName = (*state).Province[WHERE((*state).ProIDlist EQ (*state).ProID)]

    SQLstr='select code,raster_value from COUNTY_CODE_RASTER where LEFT(code,2)='+(*state).ProID
	CountyCodeID = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = n)  ;这一步是为获得County_code_raster的县代码.
	IF n EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('基础表COUNTY_CODE_RASTER中没有县码对应索引!',TITLE='警告')
;;        progressTimer->DESTROY ;销毁进度条
         OBJ_DESTROY,progressTimer
		RETURN
	ENDIF

    progressTimer->UPDATE, (0.8 * 100.0)  ;更新进度条

     CountyMeteoYield=STRARR(2,1)
     FOR i=0,RowsNUM-1 DO BEGIN
         MeteoYield=STRARR(2,1)
         FOR j=0,n-1 DO BEGIN
            IF FIX(StatisticYield[1,i]) EQ fIX(CountyCodeID[1,j]) THEN BEGIN
               MeteoYield[0,0]=CountyCodeID[0,j]
               MeteoYield[1,0]=StatisticYield[0,i]                ;根据CountyID与raster_value的对应,将县代码和产量建立对应
               CountyMeteoYield=[[CountyMeteoYield],[MeteoYield]]
               BREAK
            ENDIF
         ENDFOR
     ENDFOR

    TempYield=CountyMeteoYield[0:*,1:*]     ;该数组第一列为县代码,第二列为插值统计的波动产量.
    CountyMeteoYield = 0B  ;释放内存
    progressTimer->UPDATE, (0.9 * 100.0)  ;更新进度条
  ;------------------------------------------------------------------------
   SQLstr="select county_code,trend_yield from COUNTY_CROP_TREND_YIELD where crop_id='" $
          + crop_id+"' and year="+year+' and LEFT(county_code,2)='+(*state).ProID  ;这一步是为从趋势产量表中获得县代码和趋势产量.
	CountyCodeTR = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = n)
	IF n EQ 0 THEN BEGIN
       INFO=DIALOG_MESSAGE('您还没有进行'+year+'年"'+Crop_name+'"的趋势模拟,请先进行趋势模拟!',TITLE='警告')
;;       progressTimer->DESTROY ;销毁进度条
        OBJ_DESTROY,progressTimer
	   RETURN
	ENDIF
 ;-----------------------------------------------------------

     progressTimer->UPDATE, (0.98 * 100.0)  ;更新进度条

     SimuYield=STRARR(3,1) & RowsNUM=N_ELEMENTS(TempYield)/2
     FOR i=0,n-1 DO BEGIN
         MeteoYield=STRARR(3,1)
         FOR j=0,RowsNUM-1 DO BEGIN
            IF CountyCodeTR[0,i] EQ TempYield[0,j] THEN BEGIN
               MeteoYield[0,0]=CountyCodeTR[0,i]                ;县代码
               MeteoYield[1,0]=CountyCodeTR[1,i]                ;趋势产量
               MeteoYield[2,0]=TempYield[1,j]                   ;波动产量
               SimuYield=[[SimuYield],[MeteoYield]]
               BREAK
            ENDIF
         ENDFOR
     ENDFOR          ;这样做的目的是为去除没有该作物的县,这些县之所以有了产量,是因为插值的原因
    SimuYield=SimuYield[0:*,1:*]     ;该数组第一列为县代码,第二列为趋势产量,第三列为插值统计的波动产量.
    TempYield=FLOAT(SimuYield[1,0:*])+FLOAT(SimuYield[2,0:*]) ;将波动产量和趋势产量加起来,重复用TempYield为放内存
    TempYield=STRTRIM(TEMPORARY(TempYield),2)
    SimuYield=[SimuYield,[TempYield]]     ;4列值
      ;------------------------------------------------------------
    n=N_ELEMENTS(SimuYield)/4
    IF (*state).DataType THEN DataType = '遥感数据' ELSE DataType = '气象数据'
    SimuYield = [SimuYield,STRARR(1,n)+DataType]
    CropYear={CropID:crop_id,Year:year}
    Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=2
    Widget_Control,(*state).StaYield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=n,SET_VALUE=SimuYield $
              ,COLUMN_LABELS=['县代码','趋势产量','波动产量','模拟产量','数据类型'],ALIGNMENT=2 $
              ,ROW_LABELS=REPLICATE(year,n)+'年',SET_UVALUE=CropYear
    Widget_Control,(*state).StaYield_LABEL,SET_VALUE='插值后的统计产量(统计共'+STRTRIM(n,2)+'个县)'

     progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
;;     progressTimer->DESTROY ;销毁进度条
      OBJ_DESTROY,progressTimer

   Prompt=DIALOG_MESSAGE('统计完成!',TITLE='提示',/INFORMATION)

END
;**********************将产量输入到数据库中*******************************************
PRO DC_InputDB_EV,Event
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
        Prompt=DIALOG_MESSAGE('没有产量数据,请您先进行插值并统计!',TITLE='提示',/INFORMATION)
        RETURN
     ENDIF

  	 CountyYield = STRTRIM(TEMPORARY(CountyYield),2)
     crop_id=CropYear.cropid    &   CalYear=CropYear.year

      CountyNum=N_ELEMENTS(CountyYield)/5

;========杨绍锷修改，20070907===================================================
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
;	 FOR i=0,CountyNum-1 DO BEGIN     ;将插值后统计的各县估算和波动产量入库.
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
;===以上为原代码，以下为杨绍锷修改，20070907=======================================================================
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

	 FOR i=0,CountyNum-1 DO BEGIN     ;将插值后统计的各县估算和波动产量入库.
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

	;------------加权到省-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,这里用CROP_AREA_COUNTY
	;注意下面取作物面积时,如果当前年份的面积数据没有,则用库中最近年份Newestyear的面积数据

	INFO = DIALOG_MESSAGE('还需要将县估算产量加权到省吗?',/QUESTION,TITLE='询问')
	IF INFO EQ 'No' THEN RETURN
	CountyYield = [CountyYield[0,*],CountyYield[3,*]]		;2列值,县码\估算产量
	Status = DC_WeightToPro(CountyYield,Crop_id,CalYear,(*state).ProID,model_type_id)
	IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('加权计算成功!',/INFORMATION,TITLE='提示')

END
;*************************************************************************
PRO DC_SpatialExtrapolate_event,event
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
	 	DC_CLEARUP_DATA,event.top
         (*state).Cropindex=Event.index

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF
       DC_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Pro_DROPLIST'): BEGIN
		DC_CLEARUP_DATA,event.top
         (*state).ProID = (*state).ProIDList[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF
       DC_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Year_DROPLIST'): BEGIN
        DC_CLEARUP_DATA,event.top
		Widget_Control,EVENT.ID,GET_VALUE= Datayear
        (*state).CalcYear=Datayear[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('您还没有选择作物,请先选择!',TITLE='提示',/INFORMATION)
           RETURN
         ENDIF
       DC_TakeSimpleDotData,event.top
   END
   ;------------进行空间插值-----------------------------
   Widget_Info(wWidget, FIND_BY_UNAME='Next_Extrapolate_bu'): BEGIN   ;进行"空间插值"按钮事件

      Widget_Control,(*state).FloatYield_Table,GET_VALUE=FloatYield
      CropList =(*state).CropName
		IF (*state).Cropindex EQ 0 THEN BEGIN
		    Prompt=DIALOG_MESSAGE('您还没有选择作物!',TITLE='提示',/INFORMATION)
		    RETURN
		ENDIF

		IF FloatYield[0,0] EQ '' THEN BEGIN       ;用第一个进行判断.
		   Prompt=DIALOG_MESSAGE('没有"'+CropList[(*state).Cropindex]+'"县波动产量,请先进行模拟!',TITLE='提示',/INFORMATION)
		ENDIF ELSE BEGIN
	            ;定义作物英文名称,注意要对应作物DROPLIST
			CropName=['','SpringWheat','WinterWheat' ,'EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			crop_id=CropName[(*state).Cropindex]    &   year=(*state).CalcYear

			SAVE_FILE=DIALOG_PICKFILE(TITLE='保存插值图(最好不要改动自动设置的文件名)：',FILE=year+crop_id  $
			           ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)
			IF SAVE_FILE EQ '' THEN RETURN

			(*state).Savefile = SAVE_FILE   ;注意文件名，没有带扩展名的

			progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='空间插值',/CANCEL) ;新建进度条对象
			progressTimer->START
			progressTimer->UPDATE, (0.1 * 100.0)  ;更新进度条

			;----生成插值影像文件-------------------------------
	        ProviceName = (*state).province[WHERE((*state).proIDlist EQ (*state).proID)]
			ParaInfo = (*state).ProjectPara    ;得到省范围内的投影参数
	        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;即返回的不是结构体,而是空.
	        	OBJ_DESTROY,progressTimer
	        	PRMPT = DIALOG_MESSAGE('没有'+ProviceName+'的基础参数信息,请查看相应的参数设置文件!',TITLE='警告')
	        	RETURN
	        ENDIF

			progressTimer->UPDATE, (0.5 * 100.0)  ;更新进度条

	        samples    = ParaInfo.samples		  &        lines = ParaInfo.lines
	        ULX        = ParaInfo.UlX			  &        ULY   = ParaInfo.UlY
	        Resolution = ParaInfo.resolution      & CenterMedian = ParaInfo.CenMeridian
			Lon_Lat = FLOAT(FloatYield[3:4,0:*])  & value = FLOAT(FloatYield[2,0:*])
			;此处的插值方法DC_CREATE_INTERP_GRID()还有待于改进
			if n_elements(value) lt 5 then begin
				Prompt=DIALOG_MESSAGE('数据库中记录数量少于5条不能进行插值!',TITLE='提示',/INFORMATION)
				return
			endif
			InterpData = DC_CREATE_INTERP_GRID(Lon_Lat,value,ULX,ULY,Resolution,samples,lines,'KING')

			PaddyCrop=[3,4,5] ;对应于PaddyCrop=['EarlyRice','SemiRice','LateRice']
			IF WHERE(PaddyCrop EQ (*state).Cropindex) EQ -1 THEN BEGIN
				FarmlandFile='data_grid\farm_drought'                         ;如果不是水田,则用旱地百分比文件.
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
        Prompt=DIALOG_MESSAGE('您还没有选择作物!',TITLE='提示',/INFORMATION)
        RETURN
     END

     Yield[0,0] EQ '' :BEGIN
        Prompt=DIALOG_MESSAGE('您还没有进行'+(*state).CalcYear+'年'+CropName+'县波动产量模拟,请先模拟!',TITLE='警告')
        RETURN
     END
	 ELSE:
	ENDCASE

      RowsNum=N_ELEMENTS(SimpleDotYield)/6
      Estimation_yield=STRARR(7,RowsNum+1)
      Estimation_yield[0:*,0]=['作物名','年份','县代码','县名','波动产量','经度','纬度']
      Estimation_yield[0,1:*]=REPLICATE(CropName,RowsNum)  & Estimation_yield[1:*,1:*]=SimpleDotYield

	TEMP = WHERE(Estimation_yield EQ '',Count)
    IF Count NE 0 THEN BEGIN    ;说明有空格
        Estimation_yield[TEMP]='---'
    ENDIF

      Filename=DIALOG_PICKFILE(PATH=DC_PathSetting(),TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,GET_PATH=Savepath, DIALOG_PARENT=Event.id)

      IF Filename EQ '' THEN RETURN

	  temp = DC_PathSetting(WRITEPATH1=Savepath)       ;将路径写入保存下来

      OPENW,LUN,Filename,/GET_LUN,WIDTH=7*MAX(STRLEN(Estimation_yield))
      PRINTF,LUN,Estimation_yield
      FREE_LUN,LUN

      Prompt=DIALOG_MESSAGE('保存完成!',TITLE='提示',/INFORMATION)
   END

  Widget_Info(wWidget, FIND_BY_UNAME='SaveStatice_bu'): BEGIN

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield,GET_UVALUE=CropYear

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('没有产量数据,请您先进行插值并统计!',TITLE='提示',/INFORMATION)
        RETURN
     ENDIF

      Widget_control,/HOURGLASS
      year=CropYear.year   &   crop_id=CropYear.cropid
      CropName=STRCOMPRESS((*state).Cropname[WHERE((*state).CropIdList EQ crop_id[0])],/REMOVE_ALL)

      RowsNum=N_ELEMENTS(CountyYield)/5
      Estimation_yield=STRARR(7,RowsNum+1)
      Estimation_yield[0:*,0]=['作物','县代码','趋势产量','波动产量','模拟产量','数据类型','年份']
      Estimation_yield[0,1:*]=REPLICATE(CropName,RowsNum)  & Estimation_yield[6,1:*]=REPLICATE(year,RowsNum)
      Estimation_yield[1:5,1:*]=CountyYield

		BlankID = WHERE(Estimation_yield EQ '',Count)
	    IF Count NE 0 THEN BEGIN    ;说明有空格
	        Estimation_yield[BlankID]='---'
	    ENDIF

      Filename=DIALOG_PICKFILE(TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)

      IF Filename EQ '' THEN RETURN

      OPENW,LUN,Filename,/GET_LUN,WIDTH=7*MAX(STRLEN(Estimation_yield))
      PRINTF,LUN,Estimation_yield
      FREE_LUN,LUN

      Prompt=DIALOG_MESSAGE('保存完成!',TITLE='提示',/INFORMATION)
   END

     Widget_Info(wWidget, FIND_BY_UNAME='Extrapolate_DRAW'): BEGIN

        IF NOT OBJ_VALID((*state).oView) THEN RETURN   ;如果是空对象则返回.

		ParaInfo = (*state).ProjectPara    			;得到省范围内的投影参数
        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;即返回的不是结构体,而是空.
        	ProviceName = (*state).Province[WHERE((*state).proIDlist EQ (*state).proID)]
        	PRMPT = DIALOG_MESSAGE('没有'+ProviceName+'的基础参数信息,请查看相应的参数设置文件!',TITLE='警告')
        	RETURN
        ENDIF

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN

		    (*state).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = FIX(ParaInfo.samples)  & ySize = FIX(ParaInfo.lines) ;影像宽高
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return

            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;之所以按行反转,是因为DRAW组件坐标是从左下角开始的.
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
  ;*******杨绍锷添加，20070825********************************************
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
				ONLINE_HELP, '生物量产量分析', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;   ONLINE_HELP,BOOK='HELP\HELP.chm',/FULL_PATH,'波动产量分析'
   Widget_Info(wWidget, FIND_BY_UNAME='quit_bu'):begin
   	common_log,'关闭空间外推插值'
   	widget_control,Event.top,/DESTROY
   end
  ELSE:
  ENDCASE

END
;************空间外推插值界面*******************************
PRO DC_SpatialExtrapolate,GROUP_LEADER=groupleader

	common_log,'启动空间外推插值'

   IF ( XREGISTERED('DC_SpatialExtrapolate') NE 0 ) THEN RETURN

  TLB = Widget_Base( GROUP_LEADER=groupleader, UNAME='TLB' ,XOFFSET=300 ,YOFFSET=200  $
      ,SCR_XSIZE=506 ,SCR_YSIZE=405 ,TITLE='产量空间插值外推与统计',SPACE=1  $
      ,XPAD=0 ,YPAD=0,COLUMN=1 ,TLB_FRAME_ATTR=1,/TAB_MODE)
;-----------------------------------------------------------------------------------
  Widegt_TAB = Widget_Tab(TLB,UNAME='Widegt_TAB' ,/FRAME,SCR_XSIZE=490 ,SCR_YSIZE=341)
;----------****************-站点波动产量------------------------------
   tab0 = Widget_Base(Widegt_TAB, UNAME='tab0'  $
		      ,SCR_YSIZE=312 ,TITLE='波动产量' ,SPACE=1  $
		      ,XPAD=0 ,YPAD=0,/COLUMN,/BASE_ALIGN_LEFT)
		 ;--------------------------------------------
		 Crop_year = Widget_Base(tab0, UNAME='Crop_year' ,FRAME=1,SCR_XSIZE=490 $
		  		 ,SCR_YSIZE=25 ,SPACE=1,XPAD=0 ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)
			Province = ['北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海','江苏' $
						,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
						,'重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			CropName = ['未选择','春小麦','冬小麦','早  稻','中  稻','晚  稻','春玉米','夏玉米','大  豆']
			CropIDList = ['','11','12','21','22','23','31','32','41']				;CropName与CropIDList应对应
			ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;这样随年份变化,系统下列表中的年份也会变化.

;			Pro_Droplist  = Widget_Droplist(Crop_year,UNAME='Pro_DROPLIST',TITLE='省名:')
			Year_Droplist = Widget_Droplist(Crop_year,UNAME='Year_DROPLIST',TITLE='年份:')

			SeperateLine1 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)

			;**********杨绍锷添加，20070825********************************
			YieldType = CW_BGROUP(Crop_year, UNAME='YieldType',['县','站点'],/ROW,/EXCLUSIVE $
					,SPACE=0, LABEL_LEFT='产量:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=18);,XSIZE=145)

			SeperateLine1 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)
			;********************************************************************

			FactorType = CW_BGROUP(Crop_year, UNAME='FactorType',['气象','遥感'],/ROW,/EXCLUSIVE $
					,SPACE=0, LABEL_LEFT='类型:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=18);,XSIZE=145)


			SeperateLine2 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)
			Crop_Droplist = Widget_Droplist(Crop_year,UNAME='Crop_DROPLIST',TITLE='作物:')
		;----------------------------表----------------
		 FloatYield_Table = Widget_Table(tab0, UNAME='FloatYield_Table'$
		 			,SCR_XSIZE=490,SCR_YSIZE=250 ,XSIZE=5 ,YSIZE=20,/FRAME,ROW_LABELS='')
		;----------------------------------------------------------------------------------
		 Save_Extrapo_base = Widget_Base(tab0, UNAME='Save_Extrapo_base',SCR_XSIZE=490,/FRAME $
		     , SCR_YSIZE=31,SPACE=40 ,XPAD=47 ,YPAD=2 ,ROW=1,/BASE_ALIGN_TOP)
 		   Width_W = 100
			 SaveDotYield_bu = Widget_Button(Save_Extrapo_base, UNAME='SaveDotYield_bu'  $
			      ,SCR_XSIZE=Width_W,SCR_YSIZE=20 ,TOOLTIP='将站点波动产量保存到本地磁盘中' ,VALUE='保存')

			 Next_Extrapolate_bu = Widget_Button(Save_Extrapo_base, UNAME='Next_Extrapolate_bu'  $
			      ,SCR_XSIZE=Width_W,SCR_YSIZE=20 ,TOOLTIP='下一步进行空间插值' ,VALUE='插值>>')

			 Help_bu = Widget_Button(Save_Extrapo_base, UNAME='Help_bu'  $
			      ,SCR_XSIZE=Width_W,SCR_YSIZE=20,VALUE='帮助')

 ;------*****************-空间外推插值--------------------------------------
  tab1 = Widget_Base(Widegt_TAB, UNAME='tab1' ,FRAME=1  $
      ,SCR_XSIZE=490 ,SCR_YSIZE=312 ,TITLE='空间外推插值' ,SPACE=0  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER)

		  Extrapolate_DRAW = Widget_Draw(tab1, UNAME='Extrapolate_DRAW' ,RETAIN=2  $
		     ,SCR_XSIZE=480 ,SCR_YSIZE=275 ,GRAPHICS_LEVEL=2 $
		     ,/BUTTON_EVENTS,/MOTION_EVENTS)       ;这里设一个用户值,是Draw中没有影像时,移动指针不会报错.

;;		 Cusor_LABEL = Widget_Label(tab1,UNAME='Cusor_LABEL' $
;;		      ,SCR_XSIZE=266 ,SCR_YSIZE=12 ,/ALIGN_CENTER,VALUE='指针所指波动产量值：')

		  Button_BASE = Widget_Base(tab1, UNAME='Button_BASE'   $
		      ,SCR_XSIZE=480 ,SCR_YSIZE=31,SPACE=70 ,XPAD=80 ,YPAD=0  $
		      ,/ROW,FRAME=0,/BASE_ALIGN_TOP)

		  Return_DRAW = Widget_Button(Button_BASE, UNAME='Return_DRAW'  $
		      ,SCR_XSIZE=119 ,SCR_YSIZE=20,/ALIGN_CENTER ,VALUE='<<返回')

		  Next_StaResult_bu = Widget_Button(Button_BASE,  $
		      UNAME='Next_StaResult_bu' ,SENSITIVE=1,SCR_XSIZE=119 ,SCR_YSIZE=20 $
		     ,/ALIGN_CENTER ,VALUE='统计>>',EVENT_PRO='DC_StatisticEV' $
		     ,TOOLTIP='依据插值结果统计各县波动产量')
;-----*******************-产量统计*************-----------------
  tab2 = Widget_Base(Widegt_TAB, UNAME='tab2' $
      ,SCR_XSIZE=490 ,SCR_YSIZE=312 ,TITLE='产量统计结果' ,SPACE=1  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_LEFT,/FRAME)

		  StaYield_LABEL = Widget_Label(tab2, UNAME='StaYield_LABEL'  $
		      ,XOFFSET=46 ,YOFFSET=3 ,SCR_XSIZE=386 ,SCR_YSIZE=15  $
		      ,/ALIGN_CENTER ,VALUE='插值后统计产量')

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
		      ,/ALIGN_CENTER ,VALUE='<<返回',TOOLTIP='返回查看空间插值图')

		  InputDB_yield = Widget_Button(Button_BASE_0, UNAME='InputDB_yield'  $
		      ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight,/ALIGN_CENTER ,VALUE='入库' $
		      ,TOOLTIP='将估算的产量保存到数据库中',EVENT_PRO='DC_InputDB_EV')

		  SaveStatice_bu = Widget_Button(Button_BASE_0, UNAME='SaveStatice_bu' ,XOFFSET=239  $
		      ,YOFFSET=4 ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight ,/ALIGN_CENTER  $
		      ,VALUE='保存',TOOLTIP='将估算的产量保存到本地磁盘中')

  ;-----**************界面最下面的提示文本************-------------
  Prompt_BASE = Widget_Base(TLB,SCR_XSIZE=450 ,SCR_YSIZE=35 ,SPACE=3,XPAD=1  $
      ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  prompt_TEXT = Widget_Text(Prompt_BASE, UNAME='prompt_TEXT' ,FRAME=1  $
	      ,SCR_XSIZE=430 ,SCR_YSIZE=25 ,XSIZE=20  $
	      ,YSIZE=1,VALUE='提示：')
	  quit_bu = Widget_Button(Prompt_BASE, UNAME='quit_bu' ,XOFFSET=348  $
	      ,YOFFSET=5 ,SCR_XSIZE=55 ,SCR_YSIZE=23  $
	      ,TOOLTIP='退出空间插值模块' ,VALUE='关闭')
;---------------------------------------------------------------------------


		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
		PRO_COMMON_DC
		COMMON DC_BLOCK,NewCropID
		ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
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

		ProjectPara = DC_ReadParameter(Province[WHERE(ProIDList EQ ProCode)])    ;得到省范围内的投影参数,没有则返回为空,否则是结构体.
		state={  Cropindex             :  0 				,$
			     CropIDList			   :  CropIDList		,$	 ;作物ID列表
			     CropName			   :  CropName			,$   ;作物名列表
			     DataType			   :  0					,$   ;数据类型,默认为气象数据(0)计算的波动产量，(1)为遥感数据
			     YieldType			   :  0					,$	 ;标识是县(0)还是站点产量(1)   ;杨绍锷添加
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
			     oView				   :  OBJ_NEW()			,$		;视图对象
			     AddMin				   :  0.0				,$      ;将耕地内的插值结果进行识别的参数
			     ProjectPara		   :  ProjectPara		,$		;用于定义省相应范围内的投影参数值(见"text\parametersetting.txt")
			     Savefile              :  ''}

	Widget_Control,TLB ,SET_UVALUE=PTR_NEW(state,/NO_COPY)

	XManager, 'DC_SpatialExtrapolate', TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK


END