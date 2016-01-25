;***********�������********************************************
PRO CL_ClearProduction,EventTop

   Widget_Control,EventTop,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield & Yield[*,*]=''
     Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1]
     Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='ģ��Ĳ���'

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
;**************��������******************************************
PRO CL_SaveYieldEV,EVENT

	FORWARD_FUNCTION DC_PathSetting

   Widget_Control,Event.Top,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield,GET_UVALUE=TableHead

   IF Yield[0,0] EQ '' THEN BEGIN
      Prompt=DIALOG_MESSAGE('�ܲ�����û������!',TITLE='��ʾ',/INFORMATION)
      RETURN
   ENDIF

   TableHead[2] = TableHead[2]+'(����/Ķ)'
   TableHead[3] = TableHead[3]+'(����)'
   TableHead[4] = TableHead[4]+'(ǧ��)'

   SaveData=[[TableHead],[Yield]]

    IF WHERE(SaveData EQ '') NE [-1] THEN BEGIN    ;˵���пո�.ע����-1��[-1]������.
        SaveData[WHERE(SaveData EQ '')]='---'
    ENDIF

   Filename=DIALOG_PICKFILE(TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)

   IF Filename EQ '' THEN RETURN

   OPENW,LUN,Filename,/GET_LUN,WIDTH=MAX(STRLEN(SaveData))*5
   PRINTF,LUN,SaveData
   FREE_LUN,LUN

   INFO = DIALOG_MESSAGE('�������',/INFOR,TITLE='��ʾ')

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

    ColumnName=['ʡ/����','ʡ/����','ģ�ⵥ��','�������','�ܲ�']

    Sqlstr="select County_Code,Yield from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(County_Code,2)='+ (*state).ProID
    Sqlstr='select County_Code,name,Yield from COUNTY_CODE a,('+Sqlstr+') b where a.code=b.county_code'+ $
            ' order by County_code'
  			;ǧ��ע�ⵥλ,���ݿ����:����:����/Ķ;���:ƽ������;
    Sqlstr='select c.County_Code,d.name,d.Yield,'+CropField+'*100,d.Yield*'+CropField $
    		+'*0.0015 as Production from CROP_AREA_COUNTY c,('+Sqlstr $
    		+') d where c.county_code=d.county_code and c.year='+CalcYear+' order by c.County_code'
;-----------------------------------
	EstiProduction = DC_GetdataFromDB_Str(5,Sqlstr,N_RECORDS=RowsNum)  ;�ܲ���λ:ǧ��

    IF (RowsNum EQ 0) or ((where(float(EstiProduction[4,*]) ne 0.0))[0] eq -1) THEN BEGIN
       Prompt=DIALOG_MESSAGE('���ݿ���û��'+ProName+CalcYear+'��'+CropName+'���ع��㵥�������,����ģ�����!',TITLE='����')
       RETURN
    ENDIF

;;    SQLstr="select province_Code,Yield from Province_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;;    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(province_Code,2)='+ (*state).ProID
;;    ProYield = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = Num)
;;    IF Num EQ 0 THEN BEGIN
;;       Prompt=DIALOG_MESSAGE('���ݿ���û��'+ProName+CalcYear+'��'+CropName+'ʡ�����㵥��,����в����ںϷ���!',/INFO)
;;    	ProYield[0,0] = (*state).ProID+'0000'
;;    ENDIF
;;
;;    Sqlstr='select County_Code,d.name,d.Yield,'+CropField+' from CROP_AREA_COUNTY c,('+Sqlstr+') d where c.county_code=d.county_code'+ $
;;            ' order by c.County_code'
;	ProYield=[ProYield[0,0],ProName,ProYield[1,0],CropName[0],CalcYear]
;	EstiProduction = [[ProYield],[EstiProduction]]

    Widget_Control,(*state).Yield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=RowsNum,ALIGNMENT=2  $
                  ,SET_VALUE=EstiProduction  ,SET_UVALUE= ColumnName $  ;������û�ֵ���ڴ��ݱ�ͷ
                  ,ROW_LABELS=STRTRIM(INDGEN(RowsNum)+1,2),COLUMN_WIDTHS=66 $
                  ,COLUMN_LABELS=ColumnName
;	WIDGET_CONTROL,(*state).Yield_TABLE,USE_TABLE_SELECT=[INDGEN(1,5),INTARR(1,5)] $
;					,FOREGROUND_COLOR  = [0,0,255]

	;-----------��ͼ-------------------------
	WIDGET_CONTROL,(*state).ChartDraw,GET_VALUE = drawID

	  DEVICE,GET_DECOMPOSED=old_color     ;��ȡ��ǰDECOMPOSEDֵ
      DEVICE,GET_CURRENT_FONT=oldFont
      DEVICE,SET_FONT='SimHei',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;��IDL�ṩ�󱸴洢,ʹ����ɫ��ѯ��(ͣ����ɫ�ֽ⹦��),
;		r=[0,255,  0,  0,255,255]   	  ;����Ϊ��\��\��\��\��\��
;		g=[0,  0,255,  0,255,255]
;		b=[0,  0,  0,255,  0,220]
; 		TVLCT, r, g, b   ;ȱʡ���ĸ�ʡ��,��ʹ��ɫ����������Ϊ0,1,2,3,4,5����ɫΪ��Ӧ��RGB���
	LOADCT,39
    OldWin = !D.WINDOW     			   ;����ϵͳ�Ĵ���
    OldBackup = !P.BACKGROUND
	Old_p     = !P.POSITION
    Old_Font  = !P.FONT
    OldFontSiz = !P.CHARSIZE
    OClor = !P.COLOR
    ;!P.POSITION= [0,0,1,1]
    !P.FONT = 0
	!P.BACKGROUND = 255
	!P.COLOR = 0000   		 ;�����ɫ
    !P.CHARSIZE = 0.7

   	Colors = INTARR(RowsNum)
	FOR I = 0, RowsNum-1 DO Colors[I]=I*10+5
	TITLE=ProName+CalcYear+'�����'+CropName+'�ܲ��Ƚ�ͼ'

   	WSET, drawID
   	AnalysisData = EstiProduction[4,*]
	PlotData = FLOAT(AnalysisData);-FLOOR(MIN(FLOAT(AnalysisData)))

;    PLOT,PlotData,PSYM=-2 $ ;,XRANGE=[1,TypeNum], ,TITLE = TITLE
;         ,THICK=0,XSTYLE=1,COLOR=220,FONT=0,CHARSIZE=1 $
;		 ,POSITION=[0.08,0.15,0.96,0.84];,YMARGIN=[4,2]
;	OPLOT,PlotData,THICK=1,COLOR=200,PSYM=-2
;	XYOUTS,0.25,0.875,TITLE,ALIGNMENT=0,COLOR=70,/NORMAL,FONT=0;,CHARSIZE=100   ;�˴�CHARSIZEû����

    FirstOFFSET = 0.7  & Bar_width =10 & Base_range = 0.06 & Space = 6
    ;==========�������޸ģ�20070904=======================================================
;    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255,/OUTLINE,BARWIDTH=Bar_width $	;ԭ����
;		, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range;,BARNAMES = EstiProduction[1,*]
	num_yang=n_elements(PlotData);<60
	PlotData=PlotData[0:num_yang-1]

	if num_yang le 8 then begin
		BARNAMES = EstiProduction[1,0:num_yang-1]
	endif else begin
		BARNAMES = strtrim(string(indgen(num_yang)+1),2)
	endelse
	BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255,/OUTLINE,BARWIDTH=Bar_width $	;ԭ����
		, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range,BARNAMES = BARNAMES
	;=======================================================================================

	XYOUTS,0.25,0.875,TITLE,ALIGNMENT=0,COLOR=70,/NORMAL,FONT=0;,CHARSIZE=100   ;�˴�CHARSIZEû����

	!P.BACKGROUND = OldBackup		;��ԭ
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor

	DEVICE,SET_FONT=oldFont
	DEVICE,DECOMPOSED=old_color   ;����ԭ����DECOMPOSEDֵ,��Ϊ�Զ��庯��MyColor�ı���,�뻹ԭ.
    DEVICE,SET_FONT=oldFont

	WSET, OldWin				;��ԭԭ������.
	;----------------------------------------

;	LABEL = 'ģ���'+ProName[0]+CalcYear+'��'+CropName[0]+'�����ܲ�;��λ-����:����/Ķ;���:ƽ������;�ܲ�:ǧ��'
    Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='ģ���'+ProName[0]+CalcYear+'��'+CropName[0]+'�����ܲ�'
	Widget_Control,(*state).Unit_LABEL,SET_VALUE='(��λ-����:����/Ķ;���:����;�ܲ�:ǧ��)'
    IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN

       Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
       DistrictCode=Yield[0,*]                            ;��һ��ֵΪ�����������

       Draw_shape = (*state).Draw_shape

       ShapeFileName = (*state).ShapeFileName

       Map_para=DC_DrawShapeMape(ShapeFileName,DistrictCode,Draw_shape)

       Widget_Control,Draw_shape,SET_UVALUE=Map_para       ;����ز�����ΪDRAW���û�ֵ.
    ENDIF

END

PRO CL_event,Event

   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
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
	      Prompt=DIALOG_MESSAGE('�ܲ�����û������!',TITLE='��ʾ',/INFORMATION)
	      RETURN
	   ENDIF

     COMMON COMMON_BLOCK

	  	progressTimer = Obj_New("ShowProgress",tlb,TITLE='�����ܲ����',MESSAGE='���ܲ�������������,���Ժ�!') ;�½�����������
		progressTimer->START

		CounytNum = N_ELEMENTS(Yield)/5
		FOR i=0,CounytNum-1 DO BEGIN
		  progressTimer->UPDATE,((i+1)*1./CounytNum * 100.0)  ;���½�����
			Sqlstr1='delete from CROP_PRODUCTION_county where Year='+(*PA).CalcYear+ $
					" and county_code ='"+Yield[0,i]+"' and crop_code='"+(*PA).CropID+"'"

			Sqlstr2="insert into CROP_PRODUCTION_county values('"+Yield[0,i]+ $
					"',"+(*PA).CalcYear+",'"+(*PA).CropID+"',"+STRTRIM(Yield[4,i]*1000.0,2)+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2
		ENDFOR

;;		progressTimer->DESTROY ;���ٽ�����
		 OBJ_DESTROY,progressTimer

		INFO = DIALOG_MESSAGE('������!',/INFORMATION,TITLE='��ʾ')
		log, '��������', 1
    END


	Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
		if file_test('HELP\HELP.chm') then begin
		    ONLINE_HELP,BOOK='HELP\HELP.chm','�����������ģ��', /FULL_PATH
		endif else begin
			info_help=dialog_message('ϵͳ��û�а���')
		endelse
	end

    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'�رղ�������'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END

;------------------------------------------------------------------
;*******************************"�ܲ��������"ģ�����*********************************

PRO CL,GROUP_LEADER=groupleader

	common_log,'������������'
   IF (XREGISTERED('CL') NE 0 ) THEN RETURN   ;����������ҿ�,�����ٵ����´���.

	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
;	,XOFFSET=X_TEMP+X_OFFSET-140 ,YOFFSET=Y_TEMP+Y_OFFSET
 X_TEMP=100
  Y_TEMP=0+114

   CL_TLB = Widget_Base(GROUP_LEADER=BASE_TOP,UNAME='CL_TLB'   $
      ,YOFFSET=200 ,SCR_XSIZE=875,SCR_YSIZE=390,XOFFSET=X_TEMP+X_OFFSET-140   $
      ,TITLE='�ܲ��������' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=2 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1) ;XOFFSET=320
   ;------------���BASE------------------------------------;
   SynthesisOutputBase = Widget_Base(CL_TLB,UNAME='SynthesisOutputBase'   $
      ,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)
   ;------------------����BASE���ĸ�DROPlist���ڵ�BASE-----------------
	  ConditionB = Widget_Base(SynthesisOutputBase,UNAME='ConditionB' ,FRAME=1 $
	      ,SCR_XSIZE=420,SCR_YSIZE=32,SPACE=10,XPAD=1,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

	D_WIDTH=100
;----------------------------------------------------------------
		Province = ['������','�����','�ӱ�','ɽ��','���ɹ�','����','����','������','�Ϻ���','����' $
					,'�㽭','��΢','����','����','ɽ��','����','����','����','�㶫','����','����' $
					,'������','�Ĵ�','����','����','����','����','����','�ຣ','����','�½�']
		ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
					'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

		Crop = ['��С��','��С��','��  ��','��  ��','��  ��','������','������','��  ��']
	    CropIDList = ['11','12','21','22','23','31','32','41']				;Crop��CropIDListӦ��Ӧ
	    ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;��������ݱ仯,ϵͳ���б��е����Ҳ��仯.
		YearNum = N_ELEMENTS(ARRAY_YEAR)
;		ProDroplist  = Widget_Droplist(ConditionB,UNAME='ProDroplist',TITLE='ʡ��:',SCR_XSIZE=D_WIDTH)   ;��/frame��frame=1����ͬ��
		CropDroplist = Widget_Droplist(ConditionB,UNAME='CropDroplist',TITLE='����:',SCR_XSIZE=D_WIDTH)
		;====�������޸ģ�20070906=============================
		YearDroplist = Widget_Droplist(ConditionB,UNAME='YearDroplist',TITLE='�������:',SCR_XSIZE=D_WIDTH+15)
;		YearDroplist = Widget_Combobox(ConditionB,UNAME='YearDroplist',TITLE='�������:',SCR_XSIZE=D_WIDTH+15)
		;===========================================================

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=60 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='��ȡ',EVENT_PRO='FZ_TakeAreaYeildEV')

;----------------�����м�ı�񲿷�---------------------------------------
  YieldTable_BASE = Widget_Base(SynthesisOutputBase,SCR_XSIZE=420 , $
      UNAME='YieldTable_BASE' ,FRAME=1,SPACE=2 ,XPAD=0,YPAD=1 $
      ,SCR_YSIZE=275,/COLUMN,/BASE_ALIGN_LEFT)

     LableBASE = Widget_Base(YieldTable_BASE,SCR_XSIZE=415 , $
				  UNAME='YieldTable_BASE' ,FRAME=0,SPACE=0 ,XPAD=0,YPAD=0 $
				  ,SCR_YSIZE=25,/COLUMN,/BASE_ALIGN_CENTER)

		  wholeYield_LABEL = Widget_Label(LableBASE,  $
		      UNAME='wholeYield_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=228  $
		      ,SCR_YSIZE=12 ,/ALIGN_CENTER ,VALUE='ģ��Ĳ���',FRAME=0)

		  Unit_LABEL = Widget_Label(LableBASE,  $
		      UNAME='UNIT_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=400  $
		      ,SCR_YSIZE=12 ,/ALIGN_CENTER ,VALUE='',FRAME=0)

	  Yield_TABLE = Widget_Table(YieldTable_BASE, UNAME='Yield_TABLE',/ALL_EVENTS $
	      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=418 ,SCR_YSIZE=125,XSIZE=7 $
	      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=66,/DISJOINT_SELECTION,EVENT_PRO='DC_TableToMapEV')
  	  ChartDraw = Widget_DRAW(YieldTable_BASE, UNAME='ChartDraw' ,SCR_XSIZE=418 ,SCR_YSIZE=130 $
  	  	  ,FRAME=1)

;---------------------------�����еİ�ť����-----------------------
  button_BASE = Widget_Base(SynthesisOutputBase, UNAME='button_BASE'  $
      ,FRAME=1 ,SCR_XSIZE=419,SCR_YSIZE=34  $
      ,SPACE=28 ,XPAD=5,YPAD=1 ,ROW=1)
 ;-------------------------�Ƿ���ʾʸ��ͼ-------------
	  Draw_BASE= Widget_Base(button_BASE, UNAME='Draw_BASE'   $
	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,XPAD=0 ,YPAD=0 ,ROW=1,/NONEXCLUSIVE $
	     ,/BASE_ALIGN_TOP)
		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='��ʾ�ռ�ͼ' $
		      ,EVENT_PRO='DC_DisplayShapeEV')
  ;-----------------------------------------------------------------
	  BUWith = 45
	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='����ȡ�����ݱ��浽���ش���' ,VALUE='����' $
	      ,EVENT_PRO='CL_SaveYieldEV')

	  InputDB_bu = Widget_Button(button_BASE, UNAME='InputDB_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='�����������뵽���ݿ���' ,VALUE='���' )

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='����')

	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='�ر�')

;---------------------�ұߵ�DRAW_base����---------------------------
  DrawBase = Widget_Base(CL_TLB,UNAME='SynthesisOutputBase'   $
      ,SCR_XSIZE=437,SCR_YSIZE=352,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1 $
      ,/BASE_ALIGN_LEFT,/FRAME)
	  Draw_shape = Widget_Draw(DrawBase, UNAME='Draw_shape'  $
	      ,SCR_XSIZE=435 ,SCR_YSIZE=350,GRAPHICS_LEVEL=2,RETAIN=2,/BUTTON_EVENTS $
	      ,/FRAME,EVENT_PRO='DC_DrawWidget_EV')
;------------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�

	;====�������޸ģ�20070906====================================
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


  state={   ProNameList			:	Province		,$				;ʡ���б�
		    ProIDList			:	ProIDList		,$				;ʡID�б�
		    ProID				:	ProCode			,$				;��ѡʡID
		    CropIDList			:	CropIDList		,$				;����ID�б�
		    CropNameList		:	Crop			,$      		;�������б�
		    CropID				:	CropID			,$
		    CalcYear			:	CalcYear		,$
			ARRAY_YEAR	 		:	ARRAY_YEAR		,$
         	wholeYield_LABEL  : wholeYield_LABEL 	,$
         	Unit_LABEL		  :	Unit_LABEL			,$
         	Yield_TABLE       : Yield_TABLE		 	,$
         	Draw_shape        : Draw_shape		 	,$
         	ChartDraw		  :	ChartDraw			,$
         	DisplayYesOrNo    : 0 				 	,$       ;�Ƿ���ʾ�˲�ѯͼ:1--��ʾ;0--û����ʾ.
         	ShapeFileName     : 'data_vector\county.shp'}

   	 Widget_Control,CL_TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)


	 Widget_Control,Quit_bu,/INPUT_FOCUS    ;ʹ�ùؼ���INPUT_FOCUS,���������ʵ��֮�������Ч

;	 Widget_Control,CL_TLB,SET_UVALUE=groupleader
	 XManager, 'CL', CL_TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
