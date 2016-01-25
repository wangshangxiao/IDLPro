;产量融合模块
;********自定义过程:清除表数据***********************************
PRO DC_ClearTableData,TLB

    Widget_Control,TLB,GET_UVALUE=PA

    Widget_Control,(*PA).ResultYield_LA,SET_VALUE='最终单产量'
    Widget_Control,(*PA).ResultTABLE,GET_VALUE=Yield & Yield[*,*]=''
    Widget_Control,(*PA).ResultTABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1] $
    			  ,COLUMN_LABELS='',ROW_LABELS=''

    Widget_Control,(*PA).EstiYield_LA,SET_VALUE='计算模拟产量'
    Widget_Control,(*PA).EstiTABLE,GET_VALUE=Yield & Yield[*,*]=''
    Widget_Control,(*PA).EstiTABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1] $
    			  ,BACKGROUND_COLOR=[255,255,255],COLUMN_LABELS='',ROW_LABELS=''

	Widget_Control,(*PA).DrawWid,GET_VALUE=DrawID
	WSET,DrawID
	ERASE,COLOR=!D.N_COLORS-1

	PTR_FREE,(*PA).EstiTypeID
	(*PA).EstiTypeID = PTR_NEW()
	(*PA).CountyName = ''
END
;********自定义函数:主成分分析计算函数***********************************
FUNCTION DC_PrincipalComponentAnaysis,Data_ $ 					;注意Data的列数表示变量个数
									 ,EvectorNum = EvectorNum   ;指定要计算主成分载荷的特征值个数.默认为前2个
   ;调用形式:Result = DC_PrincipalComponentAnaysis(Data,[EvectorNum = EvectorNum])
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2

	Data = FLOAT(Data_)
	DataNum=SIZE(Data,/N_ELEMENTS)
	DataSize = SIZE(Data,/DIMENSIONS)
	M = DataSize[0]      ;变量数,即列数
	N = DataNum/M   	 ;标本数,即行数
	IF KEYWORD_SET(EvectorNum) THEN BEGIN
	   IF EvectorNum GT M THEN Num = M ELSE Num=EvectorNum
	ENDIF ELSE Num = 2   ;默认计算前两个主成分载荷

;;1,进行数据标准化处理(按列标准化)。公式为"标准值 = (列各值-列均值)/列标准差"
	FOR i=0,M-1 DO BEGIN
		Temp = Data[i,*]
		Ave  = MEAN(Temp)				 	 ;均值
		Sde  = SQRT(TOTAL((Temp-Ave)^2)/N)   ;标准差，这里没有用函数Variance()),是为不用N-1

		if(Sde ne 0) then begin
			Data[i,*]=(Temp-Ave)/Sde			 ;标准值
		endif else begin
			Data[i,*]=0
		endelse

	ENDFOR
	print,Data
;;2,求标准化数据的相关系数矩阵
	Cor_Coe = CORRELATE(Data,/double)
;;3,计算特征值,即解特征方程|λI-R|=0式中λ为特征值.
	Eval = HQR(ELMHES(Cor_Coe))			     ;此时的特征值以降序排列,都是以复数形式出现

;;	H = LA_ELMHES(Cor_Coe,q,PERMUTE_RESULT=permute,SCALE_RESULT=scale) ;Create Hessenberg upper Tragular array
;;   Eval = LA_HQR(H,q,PERMUTE_RESULT = permute)
;;4,计算对应于特征值λ的特征向量e
	Evec = EIGENVEC(Cor_Coe,Eval) 	   ;计算特征值对应的特征向量

;;	Select = [1,1,1,REPLICATE(0,M-3)]  ;为1表示计算对应于相应特征值的特征向量,一般计算头3个
;;	Evec = LA_EIGENVEC(H,q,EIGENINDEX = eigenindex,PERMUTE_RESULT = permute, $
;;					   SCALE_RESULT = scale,SELECT = Select)

;;	print,'%%%',Eval,'%%%%%',Evec

;;5,由特征值计算主成分贡献率和累计贡献率
	Evalue = REAL_PART(Eval)    ;注意得到的特征值和特征向量都是复数形式,但虚部均为0,实部即为所需要的值
	Cum_Contribute = FLTARR(M) & Cum=0.0
	FOR i=0,M-1 DO BEGIN        ;M应该等于N_ELEMENTS(Evalue)
		Cum +=Evalue[i]
		Cum_Contribute[i] = Cum/TOTAL(Evalue)
	ENDFOR
	Contribute = Evalue/TOTAL(Evalue)*100.0     ;贡献率%
	Cum_Contribute = Cum_Contribute*100.0	    ;累计贡献率%

;;	print,'Contribute%%%',Contribute,'Cum_Contribute%%%%%',Cum_Contribute

;;6计算主成分载荷ρ=SQRT(λ)e, 求前几个特征值对应的主成分,计算其载荷
	Evalue2 = Evalue[0:Num-1]			   ;取前两个特征值
	Evector2 = REAL_PART(Evec[*,0:Num-1])  ;取自上而下的几行.即为对应于前两个特征值的特征向量
	LoadVal = Evector2					   ;初始化LoadVal
	FOR i=0,Num-1 DO LoadVal[*,i] = SQRT(Evalue2[i])*Evector2[*,i]   ;主成分载荷

;;	print,'LoadVal%%%',LoadVal
;;7计算前2个的主成分的得分以及综合得分
	PCAScore = FLTARR(1,N)
	FOR i=0,Num-1 DO BEGIN
		Temp = FLTARR(1)
		FOR J=0,N-1 DO BEGIN
			Temp = [Temp,TOTAL(LoadVal[*,i]*Data[*,J])]
		ENDFOR
		Temp = Temp[1:*]
		PCAScore = [PCAScore,TRANSPOSE(Temp)]
	ENDFOR
	PCAScore = PCAScore[1:*,*]

	PcaValSum = FLTARR(Num,N)
	FOR i=0,Num-1 DO BEGIN
		PcaValSum[i,*] = Contribute[i]*PCAScore[i,*]/100.0		  ;此时用贡献率小数形式,故除100.0
	ENDFOR
	Order = TOTAL(PcaValSum,1)									  ;TOTAL(PcaValSum,1),数组横向求和,结果仍是数组
	Order[SORT(Order)]=REVERSE(INDGEN(N)+1)						  ;将综合得分按名次顺序排名
	PCAScore = [PCAScore,TRANSPOSE([[TOTAL(PcaValSum,1)],[Order]])]   ;下面的PCAScore数组有Num+2列,最后几列为综合得分及其排名

;;	print,'PCAScore%%%',PCAScore

	 RETURN,{Cor_Coe        :	Cor_Coe,					$	;相关系数矩阵
	 		 Evalue			:	TRANSPOSE(Evalue),			$	;特征值,转置成列,均FLOAT型,下同
	 		 Contribute		:	TRANSPOSE(Contribute),		$	;贡献率
	 		 Cum_Contribute :	TRANSPOSE(Cum_Contribute),	$	;累积贡献率
	 		 LoadVal		:	TRANSPOSE(LoadVal),			$	;前几个主成分的载荷(即新变量方程的系数)
	 		 PCAScore		:	PCAScore					$	;前几个主成分,综合得分和排名(对应最后两列)
	 		 }
END
;**********主成分分析事件************************************
PRO DC_PCA_EV,EVENT

    WIDGET_CONTROL, /HOURGLASS
    Widget_Control,Event.top,GET_UVALUE=PA
	IF NOT EVENT.SELECT THEN BEGIN
		IF XREGISTERED('DC_PCA_Result') NE 0 THEN BEGIN
			WIDGET_CONTROL,(*PA).PCA_TLB,/DESTROY
		END
		RETURN
	ENDIF

    Widget_Control,(*PA).EstiTABLE,GET_VALUE=Yield

    IF ARRAY_EQUAL(Yield,'') THEN BEGIN
    	INFO = DIALOG_MESSAGE('请先提取各"计算模拟产量"！',/INFORMATION,title='提示')
    	WIDGET_CONTROL,EVENT.ID,SET_BUTTON=0
    	RETURN
    ENDIF

    Cropid   = (*PA).CropID
    CalcYear = (*PA).CalcYear
    CropName = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDlist EQ Cropid)],/REMOVE_ALL)
	ProName  = (*PA).PronameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]

	CountyName = Yield[0,*]            	;Yield第一列是县名
	Yield = FLOAT(Yield[1:*,*])
	DataSize = SIZE(Yield,/DIMENSIONS)

	IF DataSize[0] LT 2 THEN BEGIN      ;DataSize[0]为变量数,即列数,应等于N_ELEMENTS((*PA).EstiTypeID)
       INFO = DIALOG_MESSAGE('只有一列数据,不能进行主成分分析!',title='提示')
	   RETURN
	ENDIF

	IF XREGISTERED('DC_PCA_Result') NE 0 THEN BEGIN
		WIDGET_CONTROL,(*PA).PCA_TLB,/DESTROY
	END

	RE = DC_PrincipalComponentAnaysis(Yield);,EvectorNum=6)
;	RE=PCOMP(Yield)

	PcaData = {Para	: RE,VarName : *((*PA).EstiTypeID),CountyName : CountyName}

	DC_PCA_Result,PcaData,GROUP_LEADER=EVENT.ID,PCA_TLB = ItTLB
	(*PA).PCA_TLB = ItTLB

	MaxCoe = MAX(RE.LoadVal[0,*],MaxId)
	Rows = DataSize[1]		;下面是将第一主成分中载荷值最大所对应的估算产量类型作为最终产量
	Widget_Control,(*PA).ResultTABLE,USE_TABLE_SELECT=[INTARR(1,Rows)+1,INDGEN(1,Rows)] $
				  ,SET_VALUE=STRTRIM(Yield[MaxId,*],2),FOREGROUND_COLOR=[0,0,255]


END
;**************提取最终估算产量数据************************************
PRO DC_TakeAllEstiYeildEV,EVENT
	DC_ClearTableData,EVENT.TOP

    WIDGET_CONTROL, /HOURGLASS
    Widget_Control,Event.top,GET_UVALUE=PA

    Cropid= (*PA).CropID
    CalcYear = (*PA).CalcYear
    CropName=STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDlist EQ Cropid)],/REMOVE_ALL)
	ProName = (*PA).PronameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]

    Sqlstr="select distinct County_Code from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(County_Code,2)=' $
    		+ (*PA).ProID+' order by county_code'
    Sqlstr='select Code,NAME from COUNTY_CODE a,('+Sqlstr+') b where a.code=b.county_code'

    EstiCounty = DC_GetdataFromDB_Str(2,Sqlstr,N_RECORDS = CountyNum)  ;得到已估算:县码/县名
    IF CountyNum EQ 0 THEN BEGIN
       Prompt=DIALOG_MESSAGE('您还没有进行'+ProName+CalcYear+'年'+CropName+'各县的任何估算产量,请先模拟计算!',title='警告')
       RETURN
    ENDIF

    Sqlstr="select distinct Model_type_id from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(County_Code,2)='+ (*PA).ProID
    EstiTypeID = DC_GetdataFromDB_Str(1,Sqlstr,N_RECORDS = TypeNum)   ;得到估算的产量类型


	ALLEstiYield = STRARR(1,CountyNum)

	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='正在数据提取中,请稍候...' $
							,TITLE='提取估算单产')
	progressTimer->START                         ;启动进度条
	TOL = Long(CountyNum)*TypeNum & NN=1

	FOR i=0,TypeNum-1 DO BEGIN
		Temp = STRARR(1,CountyNum)

		FOR j=0,CountyNum-1 DO BEGIN
  	      progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;启动进度条
  	      NN+=1

		    Sqlstr="select yield from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
		    		+"' and year="+CalcYear+' and Model_type_id='+EstiTypeID[i] $
		    		+" and County_Code='"+ EstiCounty[0,j]+"'"
			Temp[0,j] = (DC_GetdataFromDB_Str(1,Sqlstr))[0,0]
		ENDFOR
		ALLEstiYield = [ALLEstiYield,Temp]
	ENDFOR

	ALLEstiYield = ALLEstiYield[1:*,*]     ;产量列


;===杨绍锷添加，用于省产量数据的融合，20070908=====================================

;	Sqlstr_p="select distinct Province_Code from PROVINCE_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(Province_Code,2)=' $
;    		+ (*PA).ProID+' order by Province_Code'
;    Sqlstr_p='select Code,NAME from PROVINCE_CODE a,('+Sqlstr_p	+') b where a.code=b.Province_Code'
;
;    EstiCounty_p = DC_GetdataFromDB_Str(2,Sqlstr_p,N_RECORDS = CountyNum_p)  ;得到已估算:省码/省名
;    IF CountyNum_p EQ 0 THEN BEGIN
;       Prompt=DIALOG_MESSAGE('您还没有进行'+ProName+CalcYear+'年'+CropName+'的任何估算产量,请先模拟计算!',title='警告')
;       RETURN
;    ENDIF
;
;    Sqlstr_p="select distinct Model_type_id from PROVINCE_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(Province_Code,2)='+ (*PA).ProID
;    EstiTypeID_p = DC_GetdataFromDB_Str(1,Sqlstr_p,N_RECORDS = TypeNum_p)   ;得到估算的产量类型
;
;
;	ALLEstiYield_p = STRARR(1,CountyNum_p)
;
;;	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='正在数据提取中,请稍候...' $
;;							,TITLE='提取估算单产')
;;	progressTimer->START                         ;启动进度条
;;	TOL = Long(CountyNum)*TypeNum & NN=1
;
;	FOR i=0,TypeNum_p-1 DO BEGIN
;		Temp_p = STRARR(1,CountyNum_p)
;
;		FOR j=0,CountyNum_p-1 DO BEGIN
;;  	      progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;启动进度条
;;  	      NN+=1
;
;		    Sqlstr_p="select yield from PROVINCE_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;		    		+"' and year="+CalcYear+' and Model_type_id='+EstiTypeID_p[i] $
;		    		+" and Province_Code='"+ EstiCounty_p[0,j]+"'"
;			Temp_p[0,j] = (DC_GetdataFromDB_Str(1,Sqlstr_p))[0,0]
;		ENDFOR
;		ALLEstiYield_p = [ALLEstiYield_p,Temp_p]
;	ENDFOR
;
;	ALLEstiYield_p = ALLEstiYield_p[1:*,*]     ;产量列
;
;;ALLEstiYield=rotate([rotate(ALLEstiYield,1),rotate(ALLEstiYield_p,1)],3)
;	EstiCounty=rotate([rotate(EstiCounty,1),rotate(EstiCounty_p,1)],3)
;;	EstiTypeID=rotate([rotate(EstiTypeID,1),rotate(EstiTypeID_p,1)],3)
;	ALLEstiYield=[ALLEstiYield_p,ALLEstiYield]

;==================================================================

  ;VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
   Temp = WHERE(ALLEstiYield EQ '',Count)
   IF Count NE 0 THEN BEGIN
   	  BlankIndex = ARRAY_INDICES(ALLEstiYield,Temp)
   	  BlankIndex[0,*] = BlankIndex[0,*]+1   ;将空值元素的列下标值均加上1
   ENDIF ELSE BlankIndex = [-1,-1]			;原因是在估算表中显示时,多加了一列.

	FOR I=0,CountyNum-1 DO BEGIN     ;这一步是将空产量值以所在行的非空均值来代替
		TEMP = ALLEstiYield[*,I]
		BlankID = WHERE(TEMP EQ '',COUNT,COMPLEMENT=NoBlankID)
		IF COUNT NE 0 THEN BEGIN
			TEMP[BlankID] = MEAN(FLOAT(TEMP[NoBlankID]))
			ALLEstiYield[*,I] = STRTRIM(TEMP,2)
		ENDIF
	ENDFOR
  ;VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
;    progressTimer->DESTROY ;销毁进度条
	OBJ_DESTROY,progressTimer

	ColHead = STRARR(1)
	FOR I=0,TypeNum-1 DO BEGIN
		ColHead = [ColHead,'Yield'+EstiTypeID[0,I]]
	ENDFOR
	ColHead = ColHead[1:*]

    ColumnName=['县名',ColHead]

    Widget_Control,(*PA).EstiYield_LA,SET_VALUE='估算的'+ProName[0]+CalcYear+'年'+CropName[0]+'各县多种单产'
    Widget_Control,(*PA).EstiTABLE,TABLE_XSIZE=TypeNum+1,TABLE_YSIZE=CountyNum,ALIGNMENT=2  $
                  ,SET_VALUE=[EstiCounty[1,*],ALLEstiYield],SET_UVALUE= EstiCounty[1,*] $         ;这里的用户值用于传递县名
                  ,ROW_LABELS=EstiCounty[0,*],COLUMN_WIDTHS=60 $
                  ,COLUMN_LABELS=ColumnName

    Widget_Control,(*PA).ResultYield_LA,SET_VALUE=ProName[0]+CalcYear+'年'+CropName[0]+'各县最终估算单产'
    Widget_Control,(*PA).ResultTABLE,TABLE_XSIZE=2,TABLE_YSIZE=CountyNum,ALIGNMENT=1  $
                  ,SET_VALUE=[EstiCounty[1,*],STRARR(1,CountyNum)],SET_UVALUE= EstiCounty[0,*] $  ;这里的用户值用于传递县码
                  ,ROW_LABELS=EstiCounty[0,*],COLUMN_WIDTHS=110 $
                  ,COLUMN_LABELS=['县名','最终单产']

	WIDGET_CONTROL,(*PA).EstiTABLE,USE_TABLE_SELECT=BlankIndex $   ;将用均值代替的单元格以红色警示.
				  ,FOREGROUND_COLOR  = [255,0,0]

	WIDGET_CONTROL,(*PA).EstiTABLE,USE_TABLE_SELECT=[INDGEN(1,TypeNum)+1,INTARR(1,TypeNum)] $
				  ,BACKGROUND_COLOR  = [0,255,0],GET_VALUE=AnalysisData

	;-----------画图-------------------------
	WIDGET_CONTROL,(*PA).DrawWid,GET_VALUE = drawID

	DC_Draw_BAR_PLOT,drawID,AnalysisData,ColHead

	PTR_FREE,(*PA).EstiTypeID,(*PA).DisData
	(*PA).EstiTypeID = PTR_NEW(ColHead,/NO_COPY)
	(*PA).DisData = PTR_NEW(AnalysisData,/NO_COPY)
	(*PA).CountyName = EstiCounty[1,0]

END
;**********估算表中事件**************************************************
PRO DC_EstiTableEV,EVENT
	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	View = Widget_Info(	EVENT.ID,/TABLE_VIEW)
	WIDGET_CONTROL,(*PA).ResultTABLE,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1]

	IF (*PA).CountyName EQ '' THEN RETURN

	IF EVENT.TYPE EQ 4 THEN BEGIN
		IF EVENT.SEL_TOP EQ -1 THEN RETURN

		XLabel = *((*PA).EstiTypeID)

		Num = N_ELEMENTS(XLabel)
		UseCell    = INTARR(2,Num)
		UseCell[0,*] =INDGEN(Num)+1
		UseCell[1,*] = EVENT.SEL_TOP

		WIDGET_CONTROL,EVENT.ID,BACKGROUND_COLOR=[255,255,255]
		WIDGET_CONTROL,EVENT.ID,USE_TABLE_SELECT=UseCell,GET_VALUE=AnalysisData $
					   ,BACKGROUND_COLOR=[0,255,0],GET_UVALUE=County

		WIDGET_CONTROL,(*PA).DrawWid,GET_VALUE = drawID
		WSET,drawID
		ERASE,COLOR=!D.N_COLORS-1

		DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel,LINE=(*PA).DisL,CHARTLINE=(*PA).DisLC

		PTR_FREE,(*PA).DisData
		(*PA).DisData = PTR_NEW(AnalysisData,/NO_COPY)
		(*PA).CountyName = County[EVENT.SEL_TOP]

	ENDIF

END

;***********结果表中事件**************************************************
PRO DC_ResultTableEV,EVENT

	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	View = Widget_Info(EVENT.ID,/TABLE_VIEW)
	WIDGET_CONTROL,(*PA).EstiTABLE,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1]

END
;***********主界面的部分事件******************************************************
PRO DC_YieldMerge_event,Event
   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

     CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_PA.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF

	wWidget = Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=PA

;;	WIDGET_CONTROL,/HOURGLASS

  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='CropDroplist'): BEGIN
         DC_ClearTableData,Event.top
        (*PA).CropID=(*PA).CropIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='ProDroplist'): BEGIN
         DC_ClearTableData,Event.top
         (*PA).ProID=(*PA).ProIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='YearDroplist'): BEGIN
       DC_ClearTableData,Event.top
      (*PA).CalcYear=(*PA).ARRAY_YEAR[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='DisChartLine'): BEGIN
    	IF (*PA).DisLC THEN BEGIN
    		WIDGET_CONTROL,EVENT.ID,SET_BUTTON = 0
    		(*PA).DisLC = 0
    	ENDIF ELSE BEGIN
    		WIDGET_CONTROL,EVENT.ID,SET_BUTTON = 1
    		(*PA).DisLC=1
    	ENDELSE

		WIDGET_CONTROL,(*PA).DrawWid,GET_VALUE = drawID
		WSET,drawID
		ERASE,COLOR=!D.N_COLORS-1
		AnalysisData = *((*PA).DisData)
		XLabel       = *((*PA).EstiTypeID)
		DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel,LINE=(*PA).DisL,CHARTLINE=(*PA).DisLC

    END


    Widget_Info(wWidget, FIND_BY_UNAME='DisLine'): BEGIN
    	IF (*PA).DisL THEN BEGIN
    		WIDGET_CONTROL,EVENT.ID,SET_BUTTON = 0
    		(*PA).DisL = 0
    	ENDIF ELSE BEGIN
    		WIDGET_CONTROL,EVENT.ID,SET_BUTTON = 1
    		(*PA).DisL=1
    	ENDELSE

		WIDGET_CONTROL,(*PA).DrawWid,GET_VALUE = drawID
		WSET,drawID
		ERASE,COLOR=!D.N_COLORS-1
		AnalysisData = *((*PA).DisData)
		XLabel       = *((*PA).EstiTypeID)
		DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel,LINE=(*PA).DisL,CHARTLINE=(*PA).DisLC

    END

    Widget_Info(wWidget, FIND_BY_UNAME='DrawWid'): BEGIN

        IF (*PA).CountyName EQ '' THEN RETURN   ;如果是空则返回.

		IF TAG_NAMES(EVENT,/STRUCTURE_NAME) EQ 'WIDGET_TRACKING' THEN BEGIN
			IF (EVENT.ENTER EQ 1) THEN BEGIN
			    ProText='"'+(*PA).CountyName+'"'+(*PA).CalcYear+'年各估算单产比较图'
				WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
			ENDIF ELSE BEGIN
				WIDGET_CONTROL,EVENT.ID,TOOLTIP=''
			ENDELSE
		ENDIF ELSE BEGIN   ;鼠标点击事件(只取单击左键)
			IF (Event.CLICKS EQ 2) THEN BEGIN
		       IF ( XREGISTERED('DC_YieldDescription') NE 0 ) THEN BEGIN  ;不等于0则说明产量描述窗口已弹出.
		         WIDGET_CONTROL,(*PA).YieldTypeDes,/DESTROY                ;(*PA).YieldTypeDes中已存储了产量描述窗口的TLB
		       	 RETURN
		       ENDIF
		       DC_YieldDescription,GROUP_LEADER=Event.id,ITTLB=DesTLB   ;后面的关键字用于取得产量描述窗口的顶级TLB
		       (*PA).YieldTypeDes = DesTLB
		    ENDIF

			IF (Event.PRESS EQ 4) THEN BEGIN  ;右击鼠标
			     contextBase = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'DrawContext')
			     WIDGET_DISPLAYCONTEXTMENU,EVENT.ID, EVENT.X,EVENT.Y+45,contextBase
			ENDIF

		ENDELSE
    END

    Widget_Info(wWidget, FIND_BY_UNAME='UseSel'): BEGIN
		Refresh_bu = Widget_Info(wWidget,FIND_BY_UNAME='Refresh_bu')
		WIDGET_CONTROL,Refresh_bu,SENSITIVE = EVENT.SELECT
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Refresh_bu'): BEGIN
    	IF (*PA).CountyName EQ '' THEN RETURN

		CellSel = WIDGET_INFO((*PA).EstiTABLE,/TABLE_SELECT)

		Temp = WHERE(CellSel[0,*] EQ -1,COUNT)
		IF COUNT NE 0 THEN RETURN    	;没有选择则返回

		WIDGET_CONTROL,(*PA).EstiTABLE,GET_VALUE=YieldSel,/USE_TABLE_SELECT,GET_UVALUE = CountyName
		CellSelYield = [CellSel,TRANSPOSE(FLOAT(YieldSel))]  ;此处为3列的FLOAT型数组

		Temp = WHERE(CellSelYield[0,*] EQ 0,COUNT,COMPLEMENT=No0ID,NCOMPLEMENT=Num)
		IF COUNT NE 0 THEN BEGIN
			IF Num EQ 0 THEN RETURN    ;全部选择的是第一列的县代码.
			CellSelYield = CellSelYield[*,No0ID]
		ENDIF

		Yield = CellSelYield[2,*]			;产量
		Row   = CellSelYield[1,*]
		RowIndex = Row[UNIQ(Row,SORT(Row))]
		AvgYield = FLTARR(1)
		FOR I=0,N_ELEMENTS(RowIndex)-1 DO BEGIN
			SelYield = Yield[WHERE(Row EQ RowIndex[I])]
			Temp = WHERE(SelYield EQ 0.0,COUNT,COMPLEMENT=No0ID,NCOMPLEMENT=Num)
			IF COUNT NE 0 THEN BEGIN
				IF Num EQ 0 THEN BEGIN
					INFO = DIALOG_MESSAGE('您对"'+CountyName[RowIndex[I]]+'"选择了空值!',title='提示')
					TYield = 0.0
				ENDIF ELSE TYield = MEAN(SelYield[No0ID])
			ENDIF ELSE TYield = MEAN(SelYield)
			AvgYield = [AvgYield,TYield]
		ENDFOR

		AvgYield = AvgYield[1:*]
		ReCell = LONARR(2,N_ELEMENTS(RowIndex))+1
		ReCell[1,*] = LONG(RowIndex)

		WIDGET_CONTROL,(*PA).ResultTABLE,USE_TABLE_SELECT=ReCell,SET_VALUE=STRTRIM(AvgYield,2) $
					  ,FOREGROUND_COLOR = [0,0,0]
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Save_bu'): BEGIN

    	IF (*PA).CountyName EQ '' THEN RETURN

    	WIDGET_CONTROL,(*PA).ResultTABLE,GET_VALUE=ReYield,GET_UVALUE=CountyCode
    	WIDGET_CONTROL,(*PA).EstiTABLE,GET_VALUE=EsYield
		CalcYear = (*PA).CalcYear
		Cropid  = (*PA).CropID
		CropName=STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDlist EQ Cropid)],/REMOVE_ALL)
		ProName = (*PA).PronameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]

    	RowNum = N_ELEMENTS(CountyCode)
    	CropYear = STRARR(2,RowNum)
    	CropYear[0,*] = CropName
    	CropYear[1,*] = CalcYear
    	Head = ['县码','县名','最终单产',*((*PA).EstiTypeID),'作物','年份']
    	SaveData = [[Head],[CountyCode,ReYield,EsYield[1:*,*],CropYear]]

    	FILE = ProName+CalcYear+CropName+'各县最终估算单产.txt'

		DC_SaveTextData,SaveData,EVENT.ID,FILENAME=FILE

    END

    Widget_Info(wWidget, FIND_BY_UNAME='ResultTODB'): BEGIN

    	IF (*PA).CountyName EQ '' THEN RETURN

    	WIDGET_CONTROL,(*PA).ResultTABLE,GET_VALUE=ReYield,GET_UVALUE=CountyCode
		CountyYield = [CountyCode,ReYield[1,*]]
		Temp = WHERE(ReYield[1,*] EQ '',COUNT,COMPLEMENT=NO0ID,NCOMPLEMENT=Num0)
		IF COUNT NE 0 THEN BEGIN
			IF Num0 EQ 0 THEN BEGIN
				INFO = DIALOG_MESSAGE('入库时,最终单产结果不能全部是空值!',title='警告')
				RETURN
			ENDIF
			CountyYield = CountyYield[*,NO0ID]
		ENDIF

		CalYear = (*PA).CalcYear
		Crop_id  = (*PA).CropID

		COMMON COMMON_BLOCK

		Num = N_ELEMENTS(CountyYield)/2

	  	progressTimer = Obj_New("ShowProgress",tlb,TITLE='产量入库',MESSAGE='正在将最终产量入到数据库中,请稍候!') ;新建进度条对象
		progressTimer->START

		FOR i=0,Num-1 DO BEGIN     ;将有最终产量的县入库
	     	progressTimer->UPDATE,((i+1)*1./Num * 100.0)  ;更新进度条

			Sqlstr1="delete from COUNTY_ESTIMATED_YIELD where crop_id='"+ crop_id+ $
			         "' and Year="+CalYear+" and model_type_id=0 and county_code ='" $
			         +CountyYield[0,i]+"'"

			Sqlstr2="insert into COUNTY_ESTIMATED_YIELD values('"+crop_id+"','"+ $
			        CountyYield[0,i]+"',"+STRTRIM(CountyYield[1,i],2)+',0,'+CalYear+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2
		ENDFOR

		;======杨绍锷添加，将融合后的县数据加权到省，20070908======================================================
		;------------加权到省-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,这里用CROP_AREA_COUNTY
		;注意下面取作物面积时,如果当前年份的面积数据没有,则用库中最近年份Newestyear的面积数据

		INFO = DIALOG_MESSAGE('是否需要将县估算产量加权到省?',/QUESTION,TITLE='询问')
		IF INFO EQ 'Yes' THEN begin
			Status = DC_WeightToPro(CountyYield,Crop_id,CalYear,(*PA).ProID,'0')
			IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('加权计算成功!',/INFORMATION,TITLE='提示')
		endif
		;===============================================================================

;;		progressTimer->DESTROY
		OBJ_DESTROY,progressTimer

		INFO = DIALOG_MESSAGE('完成入库!',/INFORMATION,title='提示')
		log, '单产预测-产量融合', 1
    END
    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '产量融合分析', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('找不到帮助文档',title='警告')
		endelse
	end

;		ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,'产量融合分析'
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'关闭产量融合分析'
    	WIDGET_CONTROL,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END

;*****************产量融合主界面*************************************
PRO DC_YieldMerge,GROUP_LEADER=groupleader

	common_log,'启动产量融合分析'

   IF ( XREGISTERED('DC_YieldMerge') NE 0 ) THEN RETURN  ;如果该窗口已弹出,则不重复弹出.

   TLB = Widget_Base(UNAME='TLB',XOFFSET=180 ,YOFFSET=200   $
      ,TITLE='产量融合分析' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=1 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1,GROUP_LEADER=groupleader) ;XOFFSET=320
   ;------------横向第一层BASE--四个DROPlist所在的BASE----------------------------------;
	  ConditionB = Widget_Base(TLB,UNAME='ConditionB',FRAME=1,SCR_XSIZE=695 $
	  			   ,SCR_YSIZE=32,SPACE=80,XPAD=80,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

		D_WIDTH=100
		Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
					,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
					,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
		ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
					'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

		Crop = ['春小麦','冬小麦','早  稻','中  稻','晚  稻','春玉米','夏玉米','大  豆']
	    CropIDList = ['11','12','21','22','23','31','32','41']				;Crop与CropIDList应对应
	    ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;这样随年份变化,系统下列表中的年份也会变化.
		YearNum = N_ELEMENTS(ARRAY_YEAR)
;		ProDroplist  = Widget_Droplist(ConditionB,UNAME='ProDroplist',TITLE='省名:',SCR_XSIZE=D_WIDTH)   ;用/frame与frame=1是相同的
		CropDroplist = Widget_Droplist(ConditionB,UNAME='CropDroplist',TITLE='作物:',SCR_XSIZE=D_WIDTH)
		YearDroplist = Widget_Droplist(ConditionB,UNAME='YearDroplist',TITLE='计算年份:',SCR_XSIZE=D_WIDTH+15)

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=80 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='提取',EVENT_PRO='DC_TakeAllEstiYeildEV')

;----------------界面中间的Base部分---------------------------------------
  Middle_BASE = Widget_Base(TLB,SPACE=1,XPAD=0,YPAD=0,/ROW,/BASE_ALIGN_TOP)
	;中间的左部分
  	ResultYield_BASE = Widget_Base(Middle_BASE,FRAME=0,SPACE=3 ,XPAD=0,YPAD=0,/COL,/BASE_ALIGN_LEFT)

  	     ResultTaleBASE = Widget_Base(ResultYield_BASE,SCR_YSIZE=275,FRAME=1,SPACE=1 ,XPAD=0,YPAD=1,/COL,/BASE_ALIGN_LEFT,SCR_XSIZE=310)
			  ResultYield_LA= Widget_Label(ResultTaleBASE,UNAME='ResultYield_LA',SCR_XSIZE=228  $
			      ,SCR_YSIZE=18,/ALIGN_CENTER ,VALUE='最终单产量')

			  ResultTABLE = Widget_Table(ResultTaleBASE, UNAME='ResultTABLE',/ALL_EVENTS $
			      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=310 ,SCR_YSIZE=250 ,XSIZE=7 $
			      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=75,/DISJOINT_SELECTION,EVENT_PRO='DC_ResultTableEV')

        SelectBASE = Widget_Base(ResultYield_BASE,FRAME=1,SCR_XSIZE=310,SCR_YSIZE=72,SPACE=1 ,XPAD=0,YPAD=8,/COL,/BASE_ALIGN_CENTER)
			 Prompt_LA= Widget_Label(SelectBASE,UNAME='ResultYield_LA',SCR_XSIZE=228  $
				      ,SCR_YSIZE=18,/ALIGN_LEFT ,VALUE=' 产量确定方式：')
	  	     PCA_BASE = Widget_Base(SelectBASE,/FRAME,SCR_XSIZE=290,SPACE=30,XPAD=25,YPAD=1,/ROW,/BASE_ALIGN_CENTER)
				  PCA_BU = Widget_Button(PCA_BASE,UNAME='PCA_BU',XSIZE=100,SCR_YSIZE=22,/ALIGN_CENTER,VALUE='主成分分析(PCA)',EVENT_PRO='DC_PCA_EV')
				  UseSel = Widget_Button(PCA_BASE,UNAME='UseSel',XSIZE=100,SCR_YSIZE=22,/ALIGN_CENTER,VALUE='用户选择')
  ;中间的右部分
  	EstiYield_BASE = Widget_Base(Middle_BASE,FRAME=1,SPACE=3 ,XPAD=0,YPAD=0,/COL,/BASE_ALIGN_LEFT)

  	     EstimateTaleBASE = Widget_Base(EstiYield_BASE,SCR_YSIZE=275,FRAME=0,SPACE=1,XPAD=0,YPAD=1,/COL,/BASE_ALIGN_LEFT)
			  EstiYield_LA = Widget_Label(EstimateTaleBASE,UNAME='EstiYield_LA',SCR_XSIZE=250  $
			      ,SCR_YSIZE=18,/ALIGN_CENTER ,VALUE='计算模拟产量')
			  EstiTABLE = Widget_Table(EstimateTaleBASE, UNAME='EstiTABLE',/ALL_EVENTS $
			      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=380 ,SCR_YSIZE=250 ,XSIZE=7 $
			      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=75,/DISJOINT_SELECTION,EVENT_PRO='DC_EstiTableEV')

	 	 DrawWid = Widget_Draw(EstiYield_BASE,SCR_XSIZE=380,SCR_YSIZE=70,GRAPHICS_LEVEL=1,RETAIN=2,/FRAME $
	      						,UNAME='DrawWid',/TRACKING_EVENTS,/BUTTON_EVENTS);,/MOTION_EVENTS)

  DrawContext = WIDGET_BASE(DrawWid,/CONTEXT_MENU,UNAME  = 'DrawContext') ;只有Draw设置了/BUTTON_EVENTS以后,DrawWid才可作为parent
	  DisLine = Widget_Button(DrawContext,UNAME='DisLine',VALUE='折线图',/CHECKED_MENU)
	  DisChartLine =Widget_Button(DrawContext,UNAME='DisChartLine',VALUE='柱状折线图',/CHECKED_MENU)


;---界面最下的Base部分--------------------------------------
   	button_BASE = Widget_Base(TLB,FRAME=1,SCR_XSIZE=695,SPACE=60 ,XPAD=21,YPAD=1,/ROW,/BASE_ALIGN_LEFT)
  ;-----------------------------------------------------------------
	 BUWidth = 80
	  Refresh_bu = Widget_Button(button_BASE, UNAME='Refresh_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='刷新',SENSITIVE=0)
	  ResultTODB = Widget_Button(button_BASE, UNAME='ResultTODB' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='入库',TOOLTIP='将最终产量入库')

	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将提取的数据保存到本地磁盘' ,VALUE='保存')

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='帮助')


	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='关闭')
;------------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
	CropID = '12';NewCropID
	CalcYear = '2006';strmid(systime(),3,4,/REVERSE_OFFSET)

	temp = WHERE(ARRAY_YEAR EQ CalcYear,Count)
	IF Count NE 0 THEN BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)
	ENDIF ELSE BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=YearNum-1
		CalcYear = ARRAY_YEAR[YearNum-1]
	ENDELSE

;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,CropDroplist,SET_VALUE =Crop,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)

	Widget_Control,ResultTABLE,SET_TABLE_SELECT=[-1,-1]
	Widget_Control,EstiTABLE,SET_TABLE_SELECT=[-1,-1]

	Widget_Control,/REALIZE, TLB

	Widget_Control,Quit_bu,/INPUT_FOCUS

	Widget_Control,DrawWid,GET_VALUE=Drawid
	WSET,Drawid
	ERASE,COLOR=!D.N_COLORS-1

  PA={   ProNameList			:	Province		,$				;省名列表
		    ProIDList			:	ProIDList		,$				;省ID列表
		    ProID				:	ProCode			,$				;被选省ID
		    CropIDList			:	CropIDList		,$				;作物ID列表
		    CropNameList		:	Crop			,$      		;作物名列表
		    CropID				:	CropID			,$
		    ARRAY_YEAR			:	ARRAY_YEAR		,$
		    CalcYear			:	CalcYear		,$
			ResultYield_LA		:	ResultYield_LA	,$
         	ResultTABLE 		: 	ResultTABLE 	,$
 			EstiYield_LA		:	EstiYield_LA 	,$
         	EstiTABLE   	    : 	EstiTABLE		,$
         	YieldTypeDes		:	0L				,$				;产量类型描述的TLB
         	EstiTypeID			:	PTR_NEW()	 	,$				;放置单产类型标识,或者说是X轴的标签
         	DisL				:	0				,$				;是否使用线性图
         	DisLC				:	0				,$ 				;是否使用柱状和线性图
         	DisData				:	PTR_NEW()	 	,$				;画图所显示的数据,
         	CountyName			:	''				,$				;画图中所用数据的县
         	PCA_TLB				:	0L				,$				;主成分分析界面的顶级TLB
         	DrawWid        		: 	DrawWid	}

  Widget_Control,TLB,SET_UVALUE=PTR_NEW(PA,/NO_COPY)

  XManager, 'DC_YieldMerge',TLB,CLEANUP='DC_CleanAllHeap', /NO_BLOCK

END
