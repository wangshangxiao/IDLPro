;***********清空数据********************************************
PRO CL_ClearProduction,EventTop

   Widget_Control,EventTop,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield & Yield[*,*]=''
     Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1]
     Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='模拟的产量'

     Widget_Control,(*state).ChartDraw,GET_VALUE=ChartID
     WSET,ChartID
     ERASE,COLOR=!D.N_COLORS-1
;
;     IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
;        DisplayDraw = Widget_Info(EventTop,FIND_BY_UNAME='DisplayDraw')
;        Widget_Control,DisplayDraw,SET_BUTTON = 0
;        (*state).DisplayYesOrNo = 0
;        Widget_Control,(*state).Draw_shape,GET_VALUE=Owindow
;        Owindow->ERASE,COLOR=255
;      ENDIF

END
;**************保存数据******************************************
PRO CL_SaveYieldEV,EVENT

	FORWARD_FUNCTION DC_PathSetting

   Widget_Control,Event.Top,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield,GET_UVALUE=TableHead

   IF Yield[0,0] EQ '' THEN BEGIN
      Prompt=DIALOG_MESSAGE('总产表中没有数据!',TITLE='提示',/INFORMATION)
      RETURN
   ENDIF

   TableHead[2] = TableHead[2]+'(公斤/亩)'
   TableHead[3] = TableHead[3]+'(公顷)'
   TableHead[4] = TableHead[4]+'(千吨)'

   SaveData=[[TableHead],[Yield]]

    IF WHERE(SaveData EQ '') NE [-1] THEN BEGIN    ;说明有空格.注意用-1与[-1]的区别.
        SaveData[WHERE(SaveData EQ '')]='---'
    ENDIF

   Filename=DIALOG_PICKFILE(TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)

   IF Filename EQ '' THEN RETURN

   OPENW,LUN,Filename,/GET_LUN,WIDTH=MAX(STRLEN(SaveData))*5
   PRINTF,LUN,SaveData
   FREE_LUN,LUN

   INFO = DIALOG_MESSAGE('保存完成',/INFOR,TITLE='提示')

END
;888888888888888888888888888888888888888888888888888888888888888888888
PRO FZ_TakeAreaYeildEV,EVENT

	FORWARD_FUNCTION DC_GetdataFromDB_Str

    Widget_Control,Event.top,GET_UVALUE=state
;;    Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield;,SET_TABLE_SELECT=[-1,-1]
    WIDGET_CONTROL, /HOURGLASS

    CropFieldList = ['SPRING_WHEAT','WINTER_WHEAT','EARLY_RICE','SEMILATE_RICE','LATE_RICE','SPRING_CORN','SUMMER_CORN','SOYBEAN']
    CropField = CropFieldList[WHERE((*state).CropIDList EQ (*state).CropID)]

    Cropid= (*state).CropID
    CalcYear = (*state).CalcYear
    CropName=STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDlist EQ Cropid)],/REMOVE_ALL)
	ProName = (*state).PronameList[WHERE((*state).ProIDList EQ (*state).ProID)]

    ColumnName=['省/县码','省/县名','模拟单产','估算面积','总产']

    Sqlstr="select County_Code,Yield from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(County_Code,2)='+ (*state).ProID
    Sqlstr='select County_Code,name,Yield from COUNTY_CODE a,('+Sqlstr+') b where a.code=b.county_code'+ $
            ' order by County_code'
  			;千万注意单位,数据库表中:单产:公斤/亩;面积:平方公里;
    Sqlstr='select c.County_Code,d.name,d.Yield,'+CropField+'*100,d.Yield*'+CropField $
    		+'*0.0015 as Production from CROP_AREA_COUNTY c,('+Sqlstr $
    		+') d where c.county_code=d.county_code and c.year='+CalcYear+' order by c.County_code'
;-----------------------------------
	EstiProduction = DC_GetdataFromDB_Str(5,Sqlstr,N_RECORDS=RowsNum)  ;总产单位:千吨

    IF (RowsNum EQ 0) or ((where(float(EstiProduction[4,*]) ne 0.0))[0] eq -1) THEN BEGIN
       Prompt=DIALOG_MESSAGE('数据库中没有'+ProName+CalcYear+'年'+CropName+'各县估算单产或面积,请先模拟计算!',TITLE='警告')
       RETURN
    ENDIF

;;    SQLstr="select province_Code,Yield from Province_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;;    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(province_Code,2)='+ (*state).ProID
;;    ProYield = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = Num)
;;    IF Num EQ 0 THEN BEGIN
;;       Prompt=DIALOG_MESSAGE('数据库中没有'+ProName+CalcYear+'年'+CropName+'省级估算单产,请进行产量融合分析!',/INFO)
;;    	ProYield[0,0] = (*state).ProID+'0000'
;;    ENDIF
;;
;;    Sqlstr='select County_Code,d.name,d.Yield,'+CropField+' from CROP_AREA_COUNTY c,('+Sqlstr+') d where c.county_code=d.county_code'+ $
;;            ' order by c.County_code'
;	ProYield=[ProYield[0,0],ProName,ProYield[1,0],CropName[0],CalcYear]
;	EstiProduction = [[ProYield],[EstiProduction]]

    Widget_Control,(*state).Yield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=RowsNum,ALIGNMENT=2  $
                  ,SET_VALUE=EstiProduction  ,SET_UVALUE= ColumnName $  ;这里的用户值用于传递表头
                  ,ROW_LABELS=STRTRIM(INDGEN(RowsNum)+1,2),COLUMN_WIDTHS=66 $
                  ,COLUMN_LABELS=ColumnName
;	WIDGET_CONTROL,(*state).Yield_TABLE,USE_TABLE_SELECT=[INDGEN(1,5),INTARR(1,5)] $
;					,FOREGROUND_COLOR  = [0,0,255]

	;-----------画图-------------------------
	WIDGET_CONTROL,(*state).ChartDraw,GET_VALUE = drawID

	  DEVICE,GET_DECOMPOSED=old_color     ;获取当前DECOMPOSED值
      DEVICE,GET_CURRENT_FONT=oldFont
      DEVICE,SET_FONT='SimHei',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;用IDL提供后备存储,使用颜色查询表(停用颜色分解功能),
;		r=[0,255,  0,  0,255,255]   	  ;依次为黑\红\绿\蓝\黄\白
;		g=[0,  0,255,  0,255,255]
;		b=[0,  0,  0,255,  0,220]
; 		TVLCT, r, g, b   ;缺省第四个省数,则使颜色表中索引号为0,1,2,3,4,5的颜色为相应的RGB组合
	LOADCT,39
    OldWin = !D.WINDOW     			   ;保存系统的窗口
    OldBackup = !P.BACKGROUND
	Old_p     = !P.POSITION
    Old_Font  = !P.FONT
    OldFontSiz = !P.CHARSIZE
    OClor = !P.COLOR
    ;!P.POSITION= [0,0,1,1]
    !P.FONT = 0
	!P.BACKGROUND = 255
	!P.COLOR = 0000   		 ;轴的颜色
    !P.CHARSIZE = 0.7

   	Colors = INTARR(RowsNum)
	FOR I = 0, RowsNum-1 DO Colors[I]=I*10+5
	TITLE=ProName+CalcYear+'年各县'+CropName+'总产比较图'

   	WSET, drawID
   	AnalysisData = EstiProduction[4,*]
	PlotData = FLOAT(AnalysisData);-FLOOR(MIN(FLOAT(AnalysisData)))

;    PLOT,PlotData,PSYM=-2 $ ;,XRANGE=[1,TypeNum], ,TITLE = TITLE
;         ,THICK=0,XSTYLE=1,COLOR=220,FONT=0,CHARSIZE=1 $
;		 ,POSITION=[0.08,0.15,0.96,0.84];,YMARGIN=[4,2]
;	OPLOT,PlotData,THICK=1,COLOR=200,PSYM=-2
;	XYOUTS,0.25,0.875,TITLE,ALIGNMENT=0,COLOR=70,/NORMAL,FONT=0;,CHARSIZE=100   ;此处CHARSIZE没有用

    FirstOFFSET = 0.7  & Bar_width =10 & Base_range = 0.06 & Space = 6
    ;==========杨绍锷修改，20070904=======================================================
;    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255,/OUTLINE,BARWIDTH=Bar_width $	;原代码
;		, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range;,BARNAMES = EstiProduction[1,*]
	num_yang=n_elements(PlotData);<60
	PlotData=PlotData[0:num_yang-1]

	if num_yang le 8 then begin
		BARNAMES = EstiProduction[1,0:num_yang-1]
	endif else begin
		BARNAMES = strtrim(string(indgen(num_yang)+1),2)
	endelse
	BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255,/OUTLINE,BARWIDTH=Bar_width $	;原代码
		, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range,BARNAMES = BARNAMES
	;=======================================================================================

	XYOUTS,0.25,0.875,TITLE,ALIGNMENT=0,COLOR=70,/NORMAL,FONT=0;,CHARSIZE=100   ;此处CHARSIZE没有用

	!P.BACKGROUND = OldBackup		;还原
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor

	DEVICE,SET_FONT=oldFont
	DEVICE,DECOMPOSED=old_color   ;返回原来的DECOMPOSED值,因为自定义函数MyColor改变了,须还原.
    DEVICE,SET_FONT=oldFont

	WSET, OldWin				;还原原来窗口.
	;----------------------------------------

;	LABEL = '模拟的'+ProName[0]+CalcYear+'年'+CropName[0]+'各县总产;单位-单产:公斤/亩;面积:平方公里;总产:千吨'
    Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='模拟的'+ProName[0]+CalcYear+'年'+CropName[0]+'各县总产'
	Widget_Control,(*state).Unit_LABEL,SET_VALUE='(单位-单产:公斤/亩;面积:公顷;总产:千吨)'
    IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN

       Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
       DistrictCode=Yield[0,*]                            ;第一列值为行政区域代码

       Draw_shape = (*state).Draw_shape

       ShapeFileName = (*state).ShapeFileName

       Map_para=DC_DrawShapeMape(ShapeFileName,DistrictCode,Draw_shape)

       Widget_Control,Draw_shape,SET_UVALUE=Map_para       ;将相关参数作为DRAW的用户值.
    ENDIF

END

PRO CL_event,Event

   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

     CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF

	wWidget =  Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=state
	WIDGET_CONTROL,/HOURGLASS

  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='CropDroplist'): BEGIN
;         CL_ClearProduction,Event.top
        (*state).CropID=(*state).CropIDList[Event.index]

    END

;    Widget_Info(wWidget, FIND_BY_UNAME='ProDroplist'): BEGIN
;         CL_ClearProduction,Event.top
;         (*state).ProID=(*state).ProIDList[Event.index]
;
;    END

    Widget_Info(wWidget, FIND_BY_UNAME='YearDroplist'): BEGIN
;       CL_ClearProduction,Event.top
      (*state).CalcYear=(*state).ARRAY_YEAR[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='InputDB_bu'): BEGIN
	   Widget_Control,Event.Top,GET_UVALUE=PA
	     Widget_Control,(*PA).Yield_TABLE,GET_VALUE=Yield

	   IF Yield[0,0] EQ '' THEN BEGIN
	      Prompt=DIALOG_MESSAGE('总产表中没有数据!',TITLE='提示',/INFORMATION)
	      RETURN
	   ENDIF

     COMMON COMMON_BLOCK

	  	progressTimer = Obj_New("ShowProgress",tlb,TITLE='各县总产入库',MESSAGE='县总产结果正在入库中,请稍候!') ;新建进度条对象
		progressTimer->START

		CounytNum = N_ELEMENTS(Yield)/5
		FOR i=0,CounytNum-1 DO BEGIN
		  progressTimer->UPDATE,((i+1)*1./CounytNum * 100.0)  ;更新进度条
			Sqlstr1='delete from CROP_PRODUCTION_county where Year='+(*PA).CalcYear+ $
					" and county_code ='"+Yield[0,i]+"' and crop_code='"+(*PA).CropID+"'"

			Sqlstr2="insert into CROP_PRODUCTION_county values('"+Yield[0,i]+ $
					"',"+(*PA).CalcYear+",'"+(*PA).CropID+"',"+STRTRIM(Yield[4,i]*1000.0,2)+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2
		ENDFOR

;;		progressTimer->DESTROY ;销毁进度条
		 OBJ_DESTROY,progressTimer

		INFO = DIALOG_MESSAGE('完成入库!',/INFORMATION,TITLE='提示')
		log, '产量估算', 1
    END


	Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
		if file_test('HELP\HELP.chm') then begin
		    ONLINE_HELP,BOOK='HELP\HELP.chm','作物产量估算模块', /FULL_PATH
		endif else begin
			info_help=dialog_message('系统暂没有帮助')
		endelse
	end

    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'关闭产量计算'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END

;------------------------------------------------------------------
;*******************************"总产计算分析"模块界面*********************************

PRO CL,GROUP_LEADER=groupleader

	common_log,'启动产量计算'
   IF (XREGISTERED('CL') NE 0 ) THEN RETURN   ;如果窗口已找开,不用再弹出新窗口.

	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
;	,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET
 X_TEMP=100
  Y_TEMP=0+114

   CL_TLB = Widget_Base(GROUP_LEADER=BASE_TOP,UNAME='CL_TLB'   $
      ,YOFFSET=200 ,SCR_XSIZE=875,SCR_YSIZE=390,XOFFSET=X_TEMP+X_OFFSET-140   $
      ,TITLE='总产计算分析' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=2 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1) ;XOFFSET=320
   ;------------左边BASE------------------------------------;
   SynthesisOutputBase = Widget_Base(CL_TLB,UNAME='SynthesisOutputBase'   $
      ,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)
   ;------------------条件BASE即四个DROPlist所在的BASE-----------------
	  ConditionB = Widget_Base(SynthesisOutputBase,UNAME='ConditionB' ,FRAME=1 $
	      ,SCR_XSIZE=420,SCR_YSIZE=32,SPACE=10,XPAD=1,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

	D_WIDTH=100
;----------------------------------------------------------------
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
		;====杨绍锷修改，20070906=============================
		YearDroplist = Widget_Droplist(ConditionB,UNAME='YearDroplist',TITLE='计算年份:',SCR_XSIZE=D_WIDTH+15)
;		YearDroplist = Widget_Combobox(ConditionB,UNAME='YearDroplist',TITLE='计算年份:',SCR_XSIZE=D_WIDTH+15)
		;===========================================================

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=60 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='提取',EVENT_PRO='FZ_TakeAreaYeildEV')

;----------------界面中间的表格部分---------------------------------------
  YieldTable_BASE = Widget_Base(SynthesisOutputBase,SCR_XSIZE=420 , $
      UNAME='YieldTable_BASE' ,FRAME=1,SPACE=2 ,XPAD=0,YPAD=1 $
      ,SCR_YSIZE=275,/COLUMN,/BASE_ALIGN_LEFT)

     LableBASE = Widget_Base(YieldTable_BASE,SCR_XSIZE=415 , $
				  UNAME='YieldTable_BASE' ,FRAME=0,SPACE=0 ,XPAD=0,YPAD=0 $
				  ,SCR_YSIZE=25,/COLUMN,/BASE_ALIGN_CENTER)

		  wholeYield_LABEL = Widget_Label(LableBASE,  $
		      UNAME='wholeYield_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=228  $
		      ,SCR_YSIZE=12 ,/ALIGN_CENTER ,VALUE='模拟的产量',FRAME=0)

		  Unit_LABEL = Widget_Label(LableBASE,  $
		      UNAME='UNIT_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=400  $
		      ,SCR_YSIZE=12 ,/ALIGN_CENTER ,VALUE='',FRAME=0)

	  Yield_TABLE = Widget_Table(YieldTable_BASE, UNAME='Yield_TABLE',/ALL_EVENTS $
	      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=418 ,SCR_YSIZE=125,XSIZE=7 $
	      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=66,/DISJOINT_SELECTION,EVENT_PRO='DC_TableToMapEV')
  	  ChartDraw = Widget_DRAW(YieldTable_BASE, UNAME='ChartDraw' ,SCR_XSIZE=418 ,SCR_YSIZE=130 $
  	  	  ,FRAME=1)

;---------------------------界面中的按钮部分-----------------------
  button_BASE = Widget_Base(SynthesisOutputBase, UNAME='button_BASE'  $
      ,FRAME=1 ,SCR_XSIZE=419,SCR_YSIZE=34  $
      ,SPACE=28 ,XPAD=5,YPAD=1 ,ROW=1)
 ;-------------------------是否显示矢量图-------------
	  Draw_BASE= Widget_Base(button_BASE, UNAME='Draw_BASE'   $
	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,XPAD=0 ,YPAD=0 ,ROW=1,/NONEXCLUSIVE $
	     ,/BASE_ALIGN_TOP)
		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='显示空间图' $
		      ,EVENT_PRO='DC_DisplayShapeEV')
  ;-----------------------------------------------------------------
	  BUWith = 45
	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将提取的数据保存到本地磁盘' ,VALUE='保存' $
	      ,EVENT_PRO='CL_SaveYieldEV')

	  InputDB_bu = Widget_Button(button_BASE, UNAME='InputDB_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将计算数据入到数据库中' ,VALUE='入库' )

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='帮助')

	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='关闭')

;---------------------右边的DRAW_base部分---------------------------
  DrawBase = Widget_Base(CL_TLB,UNAME='SynthesisOutputBase'   $
      ,SCR_XSIZE=437,SCR_YSIZE=352,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1 $
      ,/BASE_ALIGN_LEFT,/FRAME)
	  Draw_shape = Widget_Draw(DrawBase, UNAME='Draw_shape'  $
	      ,SCR_XSIZE=435 ,SCR_YSIZE=350,GRAPHICS_LEVEL=2,RETAIN=2,/BUTTON_EVENTS $
	      ,/FRAME,EVENT_PRO='DC_DrawWidget_EV')
;------------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改

	;====杨绍锷修改，20070906====================================
	CropID = '12'
;	CalcYear = Year
	CalcYear=strmid(systime(),3,4,/REVERSE_OFFSET)
	CalcYear = '2003'

	temp = WHERE(ARRAY_YEAR EQ CalcYear,Count)

;	IF Count NE 0 THEN BEGIN
;		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_combobox_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)
;	ENDIF ELSE BEGIN
;		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_combobox_SELECT=YearNum-1
;		CalcYear = ARRAY_YEAR[YearNum-1]
;	ENDELSE

	IF Count NE 0 THEN BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_droplist_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)
	ENDIF ELSE BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_droplist_SELECT=YearNum-1
		CalcYear = ARRAY_YEAR[YearNum-1]
	ENDELSE

	;============================================================
;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,CropDroplist,SET_VALUE =Crop,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)

	Widget_Control,DisplayDraw,SET_BUTTON=0
    Widget_Control,Yield_TABLE,SET_TABLE_SELECT=[-1,-1]
    Widget_Control, /REALIZE, CL_TLB

	Widget_Control,Draw_shape,GET_VALUE=DrawOBJ
	DrawOBJ->ERASE,COLOR=255

	Widget_Control,ChartDraw,GET_VALUE=DrawGRA
	WSET,DrawGRA
	ERASE,COLOR=!D.N_COLORS-1


  state={   ProNameList			:	Province		,$				;省名列表
		    ProIDList			:	ProIDList		,$				;省ID列表
		    ProID				:	ProCode			,$				;被选省ID
		    CropIDList			:	CropIDList		,$				;作物ID列表
		    CropNameList		:	Crop			,$      		;作物名列表
		    CropID				:	CropID			,$
		    CalcYear			:	CalcYear		,$
			ARRAY_YEAR	 		:	ARRAY_YEAR		,$
         	wholeYield_LABEL  : wholeYield_LABEL 	,$
         	Unit_LABEL		  :	Unit_LABEL			,$
         	Yield_TABLE       : Yield_TABLE		 	,$
         	Draw_shape        : Draw_shape		 	,$
         	ChartDraw		  :	ChartDraw			,$
         	DisplayYesOrNo    : 0 				 	,$       ;是否显示了查询图:1--显示;0--没有显示.
         	ShapeFileName     : 'data_vector\county.shp'}

   	 Widget_Control,CL_TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)


	 Widget_Control,Quit_bu,/INPUT_FOCUS    ;使用关键字INPUT_FOCUS,必须在组件实现之后才能生效

;	 Widget_Control,CL_TLB,SET_UVALUE=groupleader
	 XManager, 'CL', CL_TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
