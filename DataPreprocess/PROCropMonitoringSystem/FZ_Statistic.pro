
;$$$$$$$$$$$$$$$$$$$$$画柱状条$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO FZ_Draw_BAR_PLOT,drawID $			;DRAW组件的ID值,即GET_VALUE
					,AnalysisData ;$		;要画的数据
;					,XLabel	$			;X轴的标签�
;					,LINE = Line $		;只画线性图,默认为只画柱状图
;					,CHARTLINE = ChartLine  ;同时画线状和柱状图
;调用:FZ_Draw_BAR_PLOT,drawID,AnalysisData    ,XLabel[,/Line,/ChartLine]
;本程序只适用于"产量融合模块"的使用,对于其他程序,可能不适用.
 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'

 	  ON_ERROR, 2						;return to caller
	  PlotNum = N_ELEMENTS(AnalysisData)    ;个数

	  DEVICE,GET_DECOMPOSED=old_color     ;获取当前DECOMPOSED值
;      DEVICE,GET_CURRENT_FONT=oldFont
;      DEVICE,SET_FONT='宋体',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;用IDL提供后备存储,使用颜色查询表(停用颜色分解功能),
		r=[0,255,  0,  0,255,255]   	  ;依次为黑\红\绿\蓝\黄\白
		g=[0,  0,255,  0,255,255]
		b=[0,  0,  0,255,  0,220]
 		TVLCT, r, g, b   ;缺省第四个省数,则使颜色表中索引号为0,1,2,3,4,5的颜色为相应的RGB组合

;;	TITLE='"'+EstiCounty[1,0]+'"'+'各估算单产比较图'
    OldWin = !D.WINDOW     			   ;保存系统的窗口
    OldBackup = !P.BACKGROUND
	Old_p     = !P.POSITION
    Old_Font  = !P.FONT
    OldFontSiz = !P.CHARSIZE
    OClor = !P.COLOR
    OldYticks = !Y.TICKS
;    !Y.TICKS = PlotNum<4
;    !P.POSITION= [0.09,0.25,0.95,0.95]
    !P.FONT = 0
	!P.BACKGROUND = 255
	!P.COLOR = 800   ;轴的颜色
    !P.CHARSIZE = 0.8

;    Colors = INTARR(PlotNum)
;	FOR I = 0, PlotNum-1 DO Colors[I]= I ;(2*I)+100

   	WSET, drawID
	PlotData = FLOAT(AnalysisData)

	Nsum = 1
	Psym = 3   ;(点标识)
    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,CHARSIZE=1,XRANGE=[1,PlotNum/Nsum] $
         ,POSITION=[0.09,0.20,0.96,0.90],NSUM=Nsum $ ,XTICKNAME=[' '] $ 	;YTICKLEN控制标志的长度
		 ,YTICKLEN=1 ,YTICKS=4,YRANGE=[0,MAX(PlotData)+100] $
		 ,XTICKLEN=1 				;YTICKS用于控制主标志的个数

XYOUTS,1,0.5,'复种指数/县编码', /DEVICE,color=1
	OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-1

;	Psym = 3   ;(点标识)
;	IF KEYWORD_SET(ChartLine) THEN BEGIN   ;画柱状和线状图
;	    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
;	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN控制标志的长度
;			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKS用于控制主标志的个数
;		OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;用白颜色将PLOT画的线覆盖掉,还要注意给XRANGE负值,不然画图效果差
;
;	    FirstOFFSET = 0.3 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
;	    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
;			,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
;			,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT
;
;		OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-6
;	ENDIF ELSE BEGIN
;		IF KEYWORD_SET(Line) THEN BEGIN  ;只画线状图
;		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
;		         ,POSITION=[0.09,0.28,0.96,0.93] ,XTICKNAME=[' ',XLabel,' '] $ 	;YTICKLEN控制标志的长度
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)] $
;				 ,XTICKLEN=1 ; ,TITLE = TITLE				;YTICKS用于控制主标志的个数
;
;			OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-5
;		ENDIF ELSE BEGIN				;只画柱状图
;		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
;		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN控制标志的长度
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKS用于控制主标志的个数
;
;			OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;用白颜色将PLOT画的线覆盖掉,还要注意给XRANGE负值,不然画图效果差
;		    FirstOFFSET = 0.8 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
;		    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
;				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
;				,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT
;		ENDELSE
;	ENDELSE

	!P.BACKGROUND = OldBackup		;还原
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor
	!Y.TICKS = OldYticks

	DEVICE,DECOMPOSED=old_color   ;返回原来的DECOMPOSED值,因为自定义函数MyColor改变了,须还原.

	WSET, OldWin				;还原原来窗口.

END
;******统计复种指数到各县************************************
FUNCTION FZ_StatisticCropping,StaFile,STATUS = status
	;StaFile  被统计文件;status计算是否成功的状态
	;调用形式:Re = FZ_StatisticCropping(StaFile,[STATUS=status]) 注意只能用status
	;返回值: 各县结构体
	;算法:Sum(Pix*W)/sum(W)  Pix像元值;W为权重

	CountyRaster = 'data_grid\county_raster'

	StaData    = DC_Read_ENVIData(StaFile,SUCCESSSTATUS = StatusS,Description='复种指数文件')
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
	Cinfo = DC_ReadHead_file(CountyRaster)
	IF FIX(Sinfo.samples) NE FIX(Cinfo.samples) OR   $
	   FIX(Sinfo.lines)   NE FIX(Cinfo.lines) THEN BEGIN
		Info = DIALOG_MESSAGE('用于统计的基础数据与复种指数文件大小不一致,统计失败!',TITLE='警告')
		STATUS=0
		RETURN,0
	ENDIF

	CountyID = CountyData[UNIQ(CountyData,SORT(CountyData))]    ;得到唯一的县Index (不是县代码)
	CountyID = CountyID[WHERE(CountyID NE 0)]					;去掉0 Index

	Result = REPLICATE({CountyID:0,CropIndex:0.0},N_ELEMENTS(CountyID))

	FOR I=0,N_ELEMENTS(CountyID)-1 DO BEGIN
		County  = WHERE(CountyData EQ CountyID[I])
		DataV   = StaData[County]*1D		;BYTE型,后转为双精度,理解help,255B*255B
		Croping = WHERE(DataV GT 0,Count)
		IF Count NE 0 THEN BEGIN			;增加了判断20070309
			Result[I].CropIndex = MEAN(DataV[WHERE(DataV GT 0)])*100.0
			Result[I].CountyID   = CountyID[I]
		ENDIF ELSE BEGIN
			Result[I].CropIndex = 0.0
			Result[I].CountyID  = CountyID[I]
		ENDELSE

	ENDFOR

	status = 1
	RETURN,Result

END
;------清除数据-----------------------------
PRO FZ_ClearFZData,TLB

    Widget_Control,TLB,GET_UVALUE=PA

    Widget_Control,(*PA).CroppingTable,GET_VALUE=Yield & Yield[*,*]=''
    Widget_Control,(*PA).CroppingTable,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1,-1,-1]

    Widget_Control,(*PA).FilePathText,SET_VALUE=''
    Widget_Control,(*PA).TableLabel,SET_VALUE='统计的各县复种指数结果'

	Widget_Control,(*PA).ChartDraw,GET_VALUE=ChartID
	WSET,ChartID
	ERASE,COLOR=!D.N_COLORS-1

;	Widget_Control,(*PA).MAPDraw,GET_VALUE=MAPID
;	MAPID->ERASE,COLOR=255

;	widget_Control,(*PA).MAPDraw,GET_VALUE = MAPID
;	MAPID->ERASE,COLOR=255
;	MAPID->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
;	IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ


END
;888888888888888888888主界面的部分事件处理8888888888888888888888888888888888888888888888888888
PRO FZ_Statistic_event, Event

	on_error,2

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  widget_info(Event.id, /trLee_root) : event.id)

;     CATCH, Error_status               ;截取错误.
;     IF Error_status NE 0 THEN BEGIN
;        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
;        CATCH, /CANCEL
;        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
;     ENDIF

	wWidget =  Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	WIDGET_CONTROL,/HOURGLASS

  CASE wTarget OF

    Widget_Info(wWidget, FIND_BY_UNAME='ProDroplist'): BEGIN
         FZ_ClearFZData,Event.top
         (*PA).ProID=(*PA).ProIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='YearDroplist'): BEGIN
       FZ_ClearFZData,Event.top
      (*PA).CalcYear=(*PA).ARRAY_YEAR[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='SelectFile'): BEGIN
        FZ_ClearFZData,Event.top

		COMMON COMMON_SETPATH,ppath
		v_fz_out_path  = (*ppath).fz_out_path

        SelectedFile = DIALOG_PICKFILE(TITLE='选择复种指数文件',GET_PATH=SelectedPath,/MUST_EXIST $
                            	  ,FILTER ='*.hdr;*.HDR',PATH = v_fz_out_path $ ;PATH = DC_PathSetting(/READPATH2)
                            	  ,DIALOG_PARENT=event.id)  ;加了筛选后缀

		IF SelectedFile EQ '' THEN RETURN

		RelaFile = STRMID(SelectedFile,STRPOS(SelectedFile,'\',/REVERSE_SEARCH)+1)  ;相对头文件.
        RelaDataFile = STRMID(RelaFile,0,STRPOS(RelaFile,'.',/REVERSE_SEARCH))		;相对数据文件.

		SelectedDataFile = STRMID(SelectedFile,0,STRPOS(SelectedFile,'.',/REVERSE_SEARCH)) ;不带hdr的全路径文件.

		IF ~FILE_TEST(SelectedDataFile) THEN BEGIN
    		Info = DIALOG_MESSAGE('找不到指定文件"'+RelaFile+'"相应的数据文件"'+RelaDataFile+'"!',/INFORMATION,TITLE='提示')
    	 	RETURN
        ENDIF

		WIDGET_CONTROL,(*PA).FilePathText,SET_VALUE = SelectedFile

		SelectedDataFile = STRMID(SelectedFile,0,STRPOS(SelectedFile,'.',/REVERSE_SEARCH))

;		Data = DC_Read_ENVIData(SelectedFile,SUCCESSSTATUS=Status,DESCRIPTION='复种指数文件')
;
;		IF Status EQ 0 THEN RETURN
;
;		WIDGET_CONTROL,(*PA).MAPDraw,SET_UVALUE = Data
;		DC_Draw_image,SelectedFile,(*PA).MAPDraw,oView=oView,/WHITE
;		OBJ_DESTROY,(*PA).oView
;		(*PA).oView = oView
;
;        temp = DC_PathSetting(WritePath2=SelectedPath)
		PSTATE = PA
		fileinfo = read_file(SelectedDataFile)
		if n_tags(fileinfo) eq 1 then return

		ARR_DATA=fileinfo.dataarr

	 	pstaff_display = (*PSTATE).pstaff_display
	 	ptr_free,(*pstaff_display).image
	 	(*pstaff_display).image = ptr_new(ARR_DATA, /no_copy)

		(*pstaff_display).startx = fileinfo.startx
		(*pstaff_display).starty = fileinfo.starty
		(*pstaff_display).xsize	= fileinfo.xsize
		(*pstaff_display).ysize = fileinfo.ysize
		(*pstaff_display).pixelsize = fileinfo.pixelsize

		(*pstaff_display).shapefile = '.\data_vector\county.shp'
		(*pstaff_display).class=1
	 	refresh, pstaff_display

    END

    Widget_Info(wWidget, FIND_BY_UNAME='MAPDraw'): BEGIN

        IF NOT OBJ_VALID((*PA).oView) THEN RETURN   ;如果是空对象则返回.

;		ParaInfo = (*PA).ProjectPara    			;得到省范围内的投影参数
;        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;即返回的不是结构体,而是空.
;        	ProviceName = (*PA).province[WHERE((*PA).proIDlist EQ (*PA).proID)]
;        	PRMPT = DIALOG_MESSAGE('没有'+ProviceName+'的基础参数信息,请查看相应的参数设置文件!')
;        	RETURN
;        ENDIF

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
            ImageSize = SIZE(Imagedata,/DIMENSIONS)

		    (*PA).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]  & ySize = ImageSize[1] ;影像宽高
			Zoom = MIN([1.*viewDim[0]/xSize,1.*viewDim[1]/ySize])
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return

;            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;之所以按行反转,是因为DRAW组件坐标是从左下角开始的.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(FLOAT(dataValue),2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

    Widget_Info(wWidget, FIND_BY_UNAME='StaCompute'): BEGIN
    	WIDGET_CONTROL,(*PA).FilePathText,GET_VALUE=FlieName
    	CroppingFile = FlieName[0]
		IF CroppingFile EQ '' THEN BEGIN
			INFO = DIALOG_MESSAGE('请先选择复种指数影像文件!',/INFOR,TITLE='提示')
			RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).ChartDraw,GET_VALUE=MapID
		WSET,MapID
		ERASE,COLOR=!D.N_COLORS-1

	  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='统计',MESSAGE='正在统计处理中,请稍候!') ;新建进度条对象
		 progressTimer->START
	     progressTimer->UPDATE,(0.2 * 100.0)  ;更新进度条

	   ;  得到的RESULT是结构体,其中CountyID(县id,非县代码),CropIndex(平均值)就是需要的复种指数
		RESULT = FZ_StatisticCropping(CroppingFile,STATUS = status)
	    IF status EQ 0 THEN BEGIN
			progressTimer->DESTROY ;销毁进度条
;			OBJ_DESTROY,progressTimer
	    	RETURN   				;统计不成功返回
	    ENDIF

	    progressTimer->UPDATE, (0.70 * 100.0)  ;更新进度条

	    RowsNUM = N_elements(RESULT)

		StatisticCrop = [TRANSPOSE(STRTRIM(RESULT.CropIndex,2)),TRANSPOSE(STRTRIM(RESULT.CountyID,2))]
	 	ProName = (*PA).ProNameList[WHERE((*PA).ProIDlist EQ (*PA).ProID)]

	    SQLstr='select code,raster_value from COUNTY_CODE_RASTER where LEFT(code,2)='+(*PA).ProID
	    SQLstr='select b.code,b.raster_value,a.name from COUNTY_CODE a,('+SQLstr+') b where a.code=b.code'

		CountyCodeID = DC_GetdataFromDB_Str(3,SQLstr,N_RECORDS = n)  ;这一步是为获得County_code_raster的县代码.
		IF n EQ 0 THEN BEGIN
			progressTimer->DESTROY ;销毁进度条
;			OBJ_DESTROY,progressTimer
			INFO = DIALOG_MESSAGE('基础表COUNTY_CODE_RASTER中没有'+ProName+'的县码对应索引!',TITLE='警告')
			RETURN
		ENDIF

	    progressTimer->UPDATE, (0.8 * 100.0)  ;更新进度条

	     CountyCropIndex=STRARR(3,1)
	     FOR i=0,RowsNUM-1 DO BEGIN
	         Temp=STRARR(3,1)
	         FOR j=0,n-1 DO BEGIN
	            IF FIX(StatisticCrop[1,i]) EQ fIX(CountyCodeID[1,j]) THEN BEGIN
	               Temp[0,0]=CountyCodeID[2,j]                 ;县名
	               Temp[1,0]=CountyCodeID[0,j]				   ;县码
	               Temp[2,0]=StatisticCrop[0,i]                ;复种指数

	               CountyCropIndex=[[CountyCropIndex],[Temp]]
	               BREAK
	            ENDIF
	         ENDFOR
	     ENDFOR

	    CountyCropIndex=CountyCropIndex[0:*,1:*]     ;该数组三列为县名\县代码\复种指数.

	    progressTimer->UPDATE, (0.9 * 100.0)  ;更新进度条

		NewRowNum = N_ELEMENTS(CountyCropIndex)/3
   		ColHaed = ['县名','县代码','复种指数']
   		Widget_Control,(*PA).CroppingTable,TABLE_XSIZE=3,TABLE_YSIZE=NewRowNum,SET_VALUE=CountyCropIndex $
              ,COLUMN_LABELS=ColHaed,ALIGNMENT=2,SET_UVALUE=ColHaed $   ;用户值为列名
              ,ROW_LABELS=STRTRIM(INDGEN(NewRowNum)+1,2),COLUMN_WIDTHS=100

		Widget_Control,(*PA).TableLabel,SET_VALUE=strtrim((*PA).CalcYear,2)+'年'+ProName[0]+'各县复种指数统计结果'

		FZ_Draw_BAR_PLOT,MapID,CountyCropIndex[2,*]

		progressTimer->UPDATE, (1 * 100.0)  ;更新进度条ChartDraw
;;		progressTimer->DESTROY ;销毁进度条
		OBJ_DESTROY,progressTimer

		INFO=DIALOG_MESSAGE('复种指数统计完成',/INFO,TITLE='提示')
		log, '复种指数-空间统计', 0
    END

    Widget_Info(wWidget, FIND_BY_UNAME='saveToCompute'): BEGIN
		WIDGET_CONTROL,(*PA).CroppingTable,GET_VALUE=IndexValue,GET_UVALUE=TableHead
		IF ARRAY_EQUAL(IndexValue,'') THEN BEGIN
			INFO = DIALOG_MESSAGE('统计表中没有数据!',/INFOR,TITLE='提示')
			RETURN
		ENDIF

		SaveDATA = [[TableHead],[IndexValue]]
		ProName = (*PA).ProNameList[WHERE((*PA).ProIDlist EQ (*PA).ProID)]
		Filename = ProName[0]+strtrim((*PA).CalcYear,2)+'年各县复种指数统计结果'
		DC_SaveTextData,SaveDATA,EVENT.ID,FILENAME=Filename,/NOSavePath

    END

    Widget_Info(wWidget, FIND_BY_UNAME='saveTodb'): BEGIN
		WIDGET_CONTROL,(*PA).CroppingTable,GET_VALUE=IndexValue
		IF ARRAY_EQUAL(IndexValue,'') THEN BEGIN
			INFO = DIALOG_MESSAGE('统计表中没有数据!',/INFOR,TITLE='提示')
			RETURN
		ENDIF

		COMMON COMMON_BLOCK

	  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='统计入库',MESSAGE='复种指数统计结果正在入库中,请稍候!') ;新建进度条对象
		 progressTimer->START

		CounytNum = N_ELEMENTS(IndexValue)/3
		FOR i=0,CounytNum-1 DO BEGIN
	        progressTimer->UPDATE, ((i+1)*1./CounytNum * 100.0)  ;更新进度条
			Sqlstr1='delete from CROPPING_INDEX_COUNTY where Year='+strtrim((*PA).CalcYear,2)+ $
					" and county_code ='"+IndexValue[1,i]+"'"

			Sqlstr2="insert into CROPPING_INDEX_COUNTY values('"+IndexValue[1,i]+ $
					"',"+strtrim((*PA).CalcYear,2)+','+STRTRIM(IndexValue[2,i],2)+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2
		ENDFOR

;;		progressTimer->DESTROY ;销毁进度条
		OBJ_DESTROY,progressTimer

		INFO = DIALOG_MESSAGE('完成入库!',/INFORMATION,TITLE='提示')
		log, '复种指数-空间统计', 1
    END
    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '复种指数统计', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,'复种指数统计'
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'关闭复种指数统计'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE
END
;&&&&&&&&&&&&&&&&&&&&&&波动产量分析主界面窗口:参数计算分析%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO FZ_Statistic, GROUP_LEADER=wGroup

	common_log,'启动复种指数统计'
	IF ( XREGISTERED('FZ_Statistic') NE 0 ) THEN RETURN

	TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
			,XOFFSET=180 ,YOFFSET=200,TITLE='复种指数统计' $
			,XPAD=3, YPAD=3, space=3, /ROW,TLB_FRAME_ATTR=1,TAB_MODE=1)
	;-----左边的BASE----------------------------------------------
	LeftBase = Widget_Base( TLB_BASE, UNAME='LeftBase',XPAD=2 ,YPAD=2,space=3,/COLUMN $
							,TAB_MODE=1,/BASE_ALIGN_LEFT,FRAME=0)
		ConWith = 450
		ConUpBase = Widget_Base( LeftBase, UNAME='ConUpBase',XPAD=5 ,YPAD=3,/ROW,TAB_MODE=1 $
								,/FRAME,SCR_XSIZE=ConWith)
			Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
						,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
						,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;这样随年份变化,系统下列表中的年份也会变化.

;			ProDroplist  = Widget_Droplist(ConUpBase,UNAME='ProDroplist',TITLE='省名:',SCR_XSIZE=100)   ;用/frame与frame=1是相同的
			label = widget_label(ConUpBase, value='年份：')
			YearDroplist = Widget_combobox(ConUpBase,UNAME='YearDroplist',SCR_XSIZE=60)


		ConDownBase = Widget_Base(LeftBase,XPAD=5 ,YPAD=3,/ROW,TAB_MODE=1,/FRAME $
							,/BASE_ALIGN_LEFT,SPACE=3,SCR_XSIZE=ConWith)

			FileLabel = WIDGET_LABEL(ConDownBase,VALUE='复种指数文件：',/ALIGN_CENTER,SCR_YSIZE=15)
			FileText = WIDGET_TEXT(ConDownBase,SCR_XSIZE=300)
			SelectFile = Widget_Button(ConDownBase,UNAME='SelectFile',SCR_XSIZE=36,SCR_YSIZE=20 $
				,VALUE='open.bmp',/BITMAP,TOOLTIP='选择复种指数文件')
		;-------------------------表BASE---------------------------------------
		TableBASE = Widget_Base(LeftBase, UNAME='TableBASE',FRAME=1,SPACE=1,XPAD=0,YPAD=1 $
								,/COL,/BASE_ALIGN_LEFT,SCR_XSIZE=ConWith)
			TableLabel = WIDGET_LABEL(TableBASE,VALUE='各县复种指数统计结果',SCR_XSIZE=300,SCR_YSIZE=15,/ALIGN_CENTER)
			CroppingTable= WIDGET_TABLE(TableBASE,/FRAME,SCR_XSIZE=448,SCR_YSIZE=110,COLUMN_WIDTHS=60, /ALL_EVENTS,event_pro='highlight_table',uname='highlight_table')


	 ChartDraw = WIDGET_DRAW(LeftBase,SCR_XSIZE=ConWith,SCR_YSIZE=108,GRAPHICS_LEVEL=1,RETAIN=2,/FRAME)
		;------------------------按钮BASE---------------------------------------
		SAVE_BASE = Widget_Base(LeftBase,FRAME=1,SPACE=40,XPAD=12,YPAD=3 ,ROW=1,/BASE_ALIGN_LEFT,SCR_XSIZE=ConWith)

		Button_width=50 & Button_height=22 ;定义按钮宽度和高度
;		SaveTo=Widget_Base(SAVE_BASE,FRAME=0,SCR_XSIZE=250,XPAD=15,YPAD=0,/ROW,SPACE=35,/BASE_ALIGN_TOP)

		  StaCompute=Widget_Button(SAVE_BASE, UNAME='StaCompute',VALUE='统计' $
		         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height) ;$
		  saveToCompute=Widget_Button(SAVE_BASE, UNAME='saveToCompute',VALUE='保存' $
		         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height,TOOLTIP='保存到本机磁盘') ;$
		  saveTodb =Widget_Button(SAVE_BASE, UNAME='saveTodb',VALUE='入库' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

		Help_bu=Widget_Button(SAVE_BASE, UNAME='Help_bu',VALUE='帮助' $
		     ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

		quit_bu=Widget_Button(SAVE_BASE, UNAME='Quit_bu',VALUE='关闭' $
		     ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

;====右边的BASE=====================================
	    RigthBASE = Widget_Base(TLB_BASE,XPAD=0 ,YPAD=3,space=3,/COLUMN, $
	    						/BASE_ALIGN_LEFT,FRAME=0)
;			MapLabel = WIDGET_LABEL(RigthBASE,UNAME='MapLabel',/ALIGN_CENTER,VALUE='复种指数影像文件：')
;			MAPDraw  = WIDGET_DRAW(RigthBASE,SCR_XSIZE=308,SCR_YSIZE=370,RETAIN=2,GRAPHICS_LEVEL=2 $
;								  ,/FRAME,/MOTION_EVENTS,UNAME='MAPDraw')
		base_id = Widget_Base(RigthBASE,SCR_XSIZE=395 ,SCR_YSIZE=372,/col,xpad=0,ypad=0,space=0,/frame)
		colorLevel = $
			[[255B,	255B,	255B],$;1级0
			 [0B,	0B,		255B],$;2级1
			 [255B,	255B,	0B],$;3级2
			 [0B,	255B,	  0B],$;4级3
			 [255B,	128B,	  0B]];,$;5级4
;			 [255B,	  0B,	  0B]];6级5
		class=1
		staff_display = {$
			base_id  :base_id,$
			image    :ptr_new(/ALLOCATE_HEAP,/no_copy),$
			startx	 :0.0    , $
		    starty	 :0.0    , $
		    xsize	 :0.0    , $
		    ysize	 :0.0    , $
		    pixelsize:0.0    , $
			palette	 :colorLevel, $
			shapefile:'',$
			legend   :'',$
			class	 :class ,$
			title    :''}
		pstaff_display = ptr_new(staff_display, /NO_COPY)
		widget_control, base_id, set_uvalue=pstaff_display
		display,pstaff_display

;-----------------------------------------------------------------------------
	Widget_Control, /REALIZE, TLB_BASE

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改


;====杨绍锷修改，20070906======================================
;	CalcYear = Year
	CalcYear = strmid(systime(),3,4,/REVERSE_OFFSET)
;==============================================================

;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_COMBOBOX_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)

	WIDGET_CONTROL,CroppingTable,SET_TABLE_SELECT=[-1,-1,-1,-1]

	WIDGET_CONTROL,quit_bu,/INPUT_FOCUS

	WIDGET_CONTROL,ChartDraw,GET_VALUE=OwinChart
	WSET,OwinChart
	ERASE,COLOR=!D.N_COLORS-1

;	WIDGET_CONTROL,MAPDraw,GET_VALUE=OwinMap
;	OwinMap->ERASE,COLOR=255

;;	ProjectPara = DC_ReadParameter(Province[WHERE(ProIDList EQ ProCode)])    ;得到省范围内的投影参数,没有则返回为空,否则是结构体.
   	STATE = { $
            ProNameList			:	Province		,$				;省名列表
            ProIDList			:	ProIDList		,$				;省ID列表
            ProID				:	ProCode			,$				;被选省ID
            CalcYear			:	CalcYear		,$
            ARRAY_YEAR			:	ARRAY_YEAR		,$
            FilePathText		:	FileText		,$
            ChartDraw			:	ChartDraw		,$
;            MAPDraw			:	MAPDraw			,$
;            oView				:	OBJ_NEW()		,$				;MAPDraw中的视图
;            ProjectPara		:	ProjectPara		,$
            TableLabel			:	TableLabel		,$
            CroppingTable		:	CroppingTable	,$
            pstaff_display 		: 	pstaff_display 	 $
            }

    PA = PTR_NEW(STATE, /NO_COPY)

    WIDGET_CONTROL, TLB_BASE, SET_UVALUE=PA

    XManager, 'FZ_Statistic', TLB_BASE, CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
