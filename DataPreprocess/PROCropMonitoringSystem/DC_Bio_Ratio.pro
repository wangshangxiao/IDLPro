
;***********清空数据********************************************
PRO DC_B_ClearUp,EventTop

   Widget_Control,EventTop,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield & Yield[*,*]=''
     Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1],COLUMN_LABELS='',ROW_LABELS=''
     Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='模拟的产量'
;;     IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
;;        DisplayDraw = Widget_Info(EventTop,FIND_BY_UNAME='DisplayDraw')
;;        Widget_Control,DisplayDraw,SET_BUTTON = 0
;;        (*state).DisplayYesOrNo = 0
;;        Widget_Control,(*state).Draw_shape,GET_VALUE=Owindow
;;        Owindow->ERASE,COLOR=255
;;     ENDIF

END
;**************提取当年及上年的生物量,及上年的作物单产量数据****************************
PRO DC_TakeBiomassEV,EVENT

    Widget_Control,Event.top,GET_UVALUE=state
    Widget_Control,(*state).Yield_TABLE,SET_TABLE_SELECT=[-1,-1]
    WIDGET_CONTROL, /HOURGLASS

    Cropid   = (*state).CropID
    CalcYear = (*state).CalcYear
    LastYear = STRTRIM(FIX(CalcYear)-1,2)		;上一年份
;;    CropBioName = ['spring_wheat','winter_wheat','early_rice','semilate_rice','late_rice','maize','soybean']
    CropBioName = ['spring_wheat','winter_wheat','early_rice','semilate_rice','late_rice','spring_maize','summer_maize','soybean']
    BioTableCrop = CropBioName[WHERE((*state).CropIDlist EQ Cropid)]
    CropName=STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDlist EQ Cropid)],/REMOVE_ALL)

	ProName = (*state).PronameList[WHERE((*state).ProIDList EQ (*state).ProID)]

    ColumnName=['县码',LastYear+'年单产',LastYear+'年生物量',CalcYear+'年生物量',CalcYear+'年单产']

    Sqlstr="select County_Code,Yield from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+LastYear+' and Model_type_id=0 and LEFT(County_Code,2)='+ (*state).ProID
    Sqlstr='select County_Code,name,Yield from COUNTY_CODE a,('+Sqlstr+') b where a.code=b.county_code'+ $
            ' order by County_code'

    Sqlstr ='select d.County_Code,d.name,d.Yield,c.Biomass from Bio_'+BioTableCrop[0]+'_county c,('+Sqlstr+') d where c.county_code=d.county_code'+ $
            ' and c.year='+LastYear+" and c.crop_id='"+Cropid+"' order by d.County_code"

    Sqlstr ='select f.name,f.County_Code,f.Yield,f.Biomass,e.Biomass from Bio_'+BioTableCrop[0]+'_county e,('+Sqlstr+') f where e.county_code=f.county_code'+ $
            ' and e.year='+CalcYear+" and e.crop_id='"+Cropid+"' order by e.County_code"

    EstimationYield = DC_GetdataFromDB_Str(5,Sqlstr,N_RECORDS = RowsNum)
    IF RowsNum EQ 0 THEN BEGIN
       Prompt=DIALOG_MESSAGE('数据库中'+ProName+'各县或上年生物量或上年单产或当前年生物量之一缺少数据,请查看数据库!',title='警告')
       RETURN
    ENDIF

	EstiYeild = STRTRIM(FLOAT(EstimationYield[4,*])/FLOAT(EstimationYield[3,*])*FLOAT(EstimationYield[2,*]),2)


    Widget_Control,(*state).Yield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=RowsNum,ALIGNMENT=2  $
                  ,SET_VALUE=[EstimationYield[1:4,*],EstiYeild],SET_UVALUE= EstimationYield[0,*] $  ;这里的用户值用于传递表头
                  ,ROW_LABELS=EstimationYield[0,*],COLUMN_WIDTHS=78 $
                  ,COLUMN_LABELS=ColumnName

    Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='比值法模拟的'+ProName[0]+CalcYear+'年'+CropName[0]+'各县单产'

;    IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
;
;       Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
;       DistrictCode=Yield[0,*]                            ;第一列值为行政区域代码
;
;       Draw_shape = (*state).Draw_shape
;
;       ShapeFileName = (*state).ShapeFileName
;
;       Map_para=DC_DrawShapeMape(ShapeFileName,DistrictCode,Draw_shape)
;
;       Widget_Control,Draw_shape,SET_UVALUE=Map_para       ;将相关参数作为DRAW的用户值.
;    ENDIF
;------------------------------------------------------------------------------------

END
;************************************************************************************
PRO DC_Bio_Ratio_event,Event
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
         DC_B_ClearUp,Event.top
        (*state).CropID=(*state).CropIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='ProDroplist'): BEGIN
         DC_B_ClearUp,Event.top
         (*state).ProID=(*state).ProIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='YearDroplist'): BEGIN
       DC_B_ClearUp,Event.top
      (*state).CalcYear=(*state).ARRAY_YEAR[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='ToDB_bu'): BEGIN
		WIDGET_CONTROL,(*state).Yield_TABLE,GET_VALUE=CountyYield
		IF ARRAY_EQUAL(CountyYield,'') THEN RETURN

		COMMON COMMON_BLOCK

		progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='正在将估算产量数据入到库中,请稍候...' $
								,TITLE='估算单产入库')
		progressTimer->START                         ;启动进度条

    	Cropid   = (*state).CropID
    	CalcYear = (*state).CalcYear
		CountyNum = N_ELEMENTS(CountyYield)/5
		FOR i=0,CountyNum-1 DO BEGIN

  	       progressTimer->UPDATE,i*1.0/CountyNum*100.0

			Sqlstr1="delete from COUNTY_ESTIMATED_YIELD where Crop_id='"+ Cropid+ $
			         "' and Year="+CalcYear+" and model_type_id=5 and county_code ='" $
			         +CountyYield[0,i]+"'"

			Sqlstr2="insert into COUNTY_ESTIMATED_YIELD values('"+Cropid+"','"+ $
			        CountyYield[0,i]+"',"+STRTRIM(CountyYield[4,i],2)+',5,'+CalcYear+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2

		ENDFOR

   	  progressTimer->UPDATE,100.0
      OBJ_DESTROY,progressTimer

	 INFO = DIALOG_MESSAGE('还需要将县估算产量加权到省吗?',/QUESTION,title='询问')
	 IF INFO EQ 'No' THEN RETURN

	 CountyYield = CountyYield[[0,4],*]				;2列值,县码\估算产量
	 Status = DC_WeightToPro(CountyYield,Cropid,CalcYear,(*state).ProID,'5')
	 IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('加权计算成功!',/INFORMATION,title='提示')

    END

    Widget_Info(wWidget, FIND_BY_UNAME='Save_bu'): BEGIN
		WIDGET_CONTROL,(*state).Yield_TABLE,GET_VALUE=CountyYield,GET_UVALUE=code
		IF ARRAY_EQUAL(CountyYield,'') THEN RETURN

	    Cropid   = (*state).CropID
	    CalcYear = (*state).CalcYear
	    LastYear = STRTRIM(FIX(CalcYear)-1,2)		;上一年份
	    CropName=STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDlist EQ Cropid)],/REMOVE_ALL)

		ProName = (*state).PronameList[WHERE((*state).ProIDList EQ (*state).ProID)]

	    ColumnName=['县名','县码',LastYear+'年单产',LastYear+'年生物量',CalcYear+'年生物量',CalcYear+'年单产','作物']

		RowNum = N_ELEMENTS(CountyYield)/5
		Save_Data = [[ColumnName],[code,CountyYield,STRARR(1,RowNum)+CropName[0]]]
		Filename = '比值法估算的'+ProName+CalcYear+'各县'+CropName+'单产.txt'
		DC_SaveTextData,Save_Data,EVENT.ID,FILENAME=Filename,/NOSavePath

    END

    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, "'生物量-产量分析'", BOOK='HELP\HELP.chm', /FULL_PATH	;
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,"'生物量-产量分析'"
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'关闭简单比值法计算'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END
;***********结果显示输出模块"界面************************
PRO DC_Bio_Ratio,GROUP_LEADER=groupleader

	common_log,'启动简单比值法计算'
   IF ( XREGISTERED('DC_Bio_Ratio') NE 0 ) THEN RETURN  ;如果该窗口已弹出,则不重复弹出.

   TLB = Widget_Base(GROUP_LEADER=groupleader,UNAME='TLB'   $
      ,XOFFSET=320 ,YOFFSET=200,SCR_XSIZE=500,SCR_YSIZE=386 ,/TAB_MODE  $
      ,TITLE='比值法计算单产' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=2 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1)

   ;------------左边BASE------------------------------------;
   bWIDTH = 485
   SynthesisOutputBase = Widget_Base(TLB,UNAME='SynthesisOutputBase'   $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)
   ;------------------条件BASE即四个DROPlist所在的BASE-----------------
	  ConditionB = Widget_Base(SynthesisOutputBase,UNAME='ConditionB' ,FRAME=1 $
	      ,SCR_XSIZE=bWIDTH,SCR_YSIZE=32,SPACE=25,XPAD=1,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

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
		YearDroplist = Widget_Droplist(ConditionB,UNAME='YearDroplist',TITLE='计算年份:',SCR_XSIZE=D_WIDTH+15)

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=66 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='计算',EVENT_PRO='DC_TakeBiomassEV')

;----------------界面中间的表格部分---------------------------------------
  YieldTable_BASE = Widget_Base(SynthesisOutputBase,SCR_XSIZE=bWIDTH , $
      UNAME='YieldTable_BASE' ,FRAME=1,SPACE=1 ,XPAD=0,YPAD=1 $
      ,SCR_YSIZE=275,/COLUMN,/BASE_ALIGN_LEFT)

	  wholeYield_LABEL = Widget_Label(YieldTable_BASE,  $
	      UNAME='wholeYield_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=228  $
	      ,SCR_YSIZE=18 ,/ALIGN_CENTER ,VALUE='模拟的产量')

	  Yield_TABLE = Widget_Table(YieldTable_BASE, UNAME='Yield_TABLE' $,/ALL_EVENTS $
	      ,SCR_XSIZE=bWIDTH ,SCR_YSIZE=250 ,XSIZE=7,YSIZE=19,/FRAME,COLUMN_WIDTHS=75 $
	      ,/DISJOINT_SELECTION,/RESIZEABLE_COLUMNS);,EVENT_PRO='DC_TableToMapEV')
;---------------------------界面中的按钮部分-----------------------
  button_BASE = Widget_Base(SynthesisOutputBase, UNAME='button_BASE'  $
      ,FRAME=1 ,SCR_XSIZE=bWIDTH,SCR_YSIZE=34  $
      ,SPACE=40 ,XPAD=16,YPAD=1 ,ROW=1)
 ;-------------------------是否显示矢量图-------------
;	  Draw_BASE= Widget_Base(button_BASE, UNAME='Draw_BASE'   $
;	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,XPAD=0 ,YPAD=0 ,ROW=1,/NONEXCLUSIVE $
;	     ,/BASE_ALIGN_TOP)
;		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
;		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='显示空间图' $
;		      ,EVENT_PRO='DC_DisplayShapeEV')
  ;-----------------------------------------------------------------
  	  Buwith = 80
	  ToDB_bu = Widget_Button(button_BASE, UNAME='ToDB_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将计算的产量保存到数据库中' ,VALUE='入库')

	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将计算的数据保存到本地磁盘' ,VALUE='保存')

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将计算的数据保存到本地磁盘' ,VALUE='帮助')

	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='关闭')

;---------------------右边的DRAW_base部分---------------------------
;;  DrawBase = Widget_Base(TLB,UNAME='SynthesisOutputBase'   $
;;      ,SCR_XSIZE=437,SCR_YSIZE=352,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1 $
;;      ,/BASE_ALIGN_LEFT,/FRAME)
;;	  Draw_shape = Widget_Draw(DrawBase, UNAME='Draw_shape'  $
;;	      ,SCR_XSIZE=435 ,SCR_YSIZE=350,GRAPHICS_LEVEL=2,RETAIN=2,/BUTTON_EVENTS $
;;	      ,/FRAME,EVENT_PRO='DC_DrawWidget_EV')
;------------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
	CropID = NewCropID
	CalcYear = strmid(systime(),3,4,/REVERSE_OFFSET)

	temp = WHERE(ARRAY_YEAR EQ CalcYear,Count)
	IF Count NE 0 THEN BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)
	ENDIF ELSE BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=YearNum-1
		CalcYear = ARRAY_YEAR[YearNum-1]
	ENDELSE

;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,CropDroplist,SET_VALUE =Crop,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)


  Widget_Control,Yield_TABLE,SET_TABLE_SELECT=[-1,-1]
  Widget_Control, /REALIZE, TLB
  Widget_Control, /INPUT_FOCUS, Quit_bu

  state={   $
  			ProNameList			:	Province		,$				;省名列表
		    ProIDList			:	ProIDList		,$				;省ID列表
		    ProID				:	ProCode			,$				;被选省ID
		    CropIDList			:	CropIDList		,$				;作物ID列表
		    CropNameList		:	Crop			,$      		;作物名列表
		    CropID				:	CropID			,$
		    CalcYear			:	CalcYear		,$
			ARRAY_YEAR	 		:	ARRAY_YEAR		,$
         	wholeYield_LABEL  : wholeYield_LABEL 	,$
         	Yield_TABLE       : Yield_TABLE		 	}
;         	Draw_shape        : Draw_shape		 	,$
;         	DisplayYesOrNo    : 0 				 	,$       ;是否显示了查询图:1--显示;0--没有显示.
;         	ShapeFileName     : 'data_vector\county.shp'}

 Widget_Control,TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)

  XManager, 'DC_Bio_Ratio',TLB,CLEANUP='DC_CleanAllHeap', /NO_BLOCK

END