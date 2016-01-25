;��������ģ��
;**************** �Զ��庯��:���弸����ɫ*******************************
FUNCTION DC_MyColor,color
    DEVICE, RETAIN=2, DECOMPOSED=0      ;��IDL�ṩ�󱸴洢,ʹ����ɫ��ѯ��(ͣ����ɫ�ֽ⹦��),
;;    !P.BACKGROUND=0        ;"!P":The main plotting system variable structure

     ;Define a color table
    r=BYTARR(256) & g=BYTARR(256) & b=BYTARR(256)
    r[0]=0   & g[0]=0   & b[0]=0           ;Definition of black
    r[1]=255 & g[1]=0   & b[1]=0           ;Definition of red
    r[2]=0   & g[2]=255 & b[2]=0           ;Definition of green
    r[3]=0   & g[3]=0   & b[3]=255         ;Definition of blue
    r[4]=255 & g[4]=255 & b[4]=0           ;Definition of yellow
    r[255]=255 & g[255]=255 & b[255]=255   ;Definition of white

    TVLCT, r, g, b   ;ȱʡ���ĸ�ʡ��,��ʹ��ɫ����������Ϊ0,1,2,3,255����ɫΪ��Ӧ��RGB���
    black=0 & red=1 & green=2 & blue=3  & yellow=4 & white=255  ;��ȡ��Ӧ����ɫ������.
    case color of
        0 : return, 0         ;black
        1 : return, 1         ;red
        2 : return, 2         ;green
        3 : return, 3         ;blue
        4 : return, 4         ;yellow
      255 : return, 255       ;white
    else  : return, 1         ;Ĭ��Ϊ��ɫ
    endcase
END
;************����ģ���¼�****************************
PRO DC_SaveYieldModelEV,EVENT
    Widget_Control, Event.top, GET_UVALUE=ThirdStep
    Widget_Control,(*ThirdStep).modelParameter_TABLE,GET_VALUE=ModelParameter
    Widget_Control,(*ThirdStep).meteroYileld_TABLE,GET_VALUE=MeteoYileld
    ParameterName =['�����ϵ��','��׼��','F����ֵ','ģ�ⷽ��','������']
    TableHead=['����','����','���','ʵ�ʲ�������','ģ�Ⲩ������','***','ģ�Ͳ���','����ֵ']
    RowsNum=0
      IF N_ELEMENTS((*ThirdStep).YieldTableLabel) GE 5 THEN BEGIN  ;�����6ָ���ǲ��������ݸ���
         RowsNum=N_ELEMENTS((*ThirdStep).YieldTableLabel)
      ENDIF ELSE BEGIN
        RowsNum=5
      ENDELSE

    SaveData=STRARR(8,RowsNum+1)
    SaveData[0:*,0]=TableHead    &   SaveData[0,1:*]=REPLICATE((*ThirdStep).CountyCode,1,RowsNum)
    SaveData[1,1:*]=REPLICATE((*ThirdStep).CropName,1,RowsNum)  & SaveData[2,1:*]=TRANSPOSE((*ThirdStep).YieldTableLabel)
    SaveData[3:4,1:*]=TRANSPOSE(MeteoYileld)  & SaveData[5,1:*]= REPLICATE('***',1,RowsNum)
    SaveData[6,1:5]=TRANSPOSE(ParameterName)  & SaveData[7,1:5]=TRANSPOSE(ModelParameter)
    SaveData[3,RowsNum]=['---']

    IF WHERE(SaveData EQ '') NE [-1] THEN BEGIN
      SaveData[WHERE(SaveData EQ '')]='---'
    ENDIF

   Filename=DIALOG_PICKFILE(DIALOG_PARENT=EVENT.ID,TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting())

   IF Filename EQ '' THEN RETURN

   OPENW,LUN,Filename,/GET_LUN,WIDTH=8*16
   PRINTF,LUN,SaveData
   FREE_LUN,LUN

   Prompt=DIALOG_MESSAGE('����ɹ�!',TITLE='��ʾ',/INFORMATION)

    Widget_Control, Event.top, SET_UVALUE=ThirdStep
END
;********��������¼�*********************************************************
PRO DC_InputDBEV,EVENT
	COMMON COMMON_BLOCK;,yesORno,DBobj

	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		 infomation=DIALOG_MESSAGE(['���ʧ��,����������Ƿ���ȷ,','����ѡ��ͬʱ������ݼ���!'],TITLE='����',/ERROR)
		 CATCH, /CANCEL
		 RETURN                    ;������������,������ִ����������,�Ի���ִ���.
	ENDIF

	Widget_Control, Event.top, GET_UVALUE=ThirdStep
	Widget_Control,(*ThirdStep).modelParameter_TABLE,GET_VALUE=ModelParameter
	Widget_Control,(*ThirdStep).meteroYileld_TABLE,GET_VALUE=MeteoYileld

	FloatYield = MeteoYileld[(N_ELEMENTS(MeteoYileld)/2)-1,1]
	Trendyield = (*ThirdStep).Trendyield
	Estimate   = STRTRIM(FLOAT(FloatYield)+FLOAT(Trendyield),2)

	LastPtr	=(*ThirdStep).LastPtr    ;�ϼ�BASE���û�ֵ,ָ������

	Datayear=(*LastPtr).ARRAY_YEAR		       ;���൱�����list
	GrowthMonth=STRTRIM(INDGEN(12)+1,2)        ;���൱���·�List
	StartYear  = Datayear[(*LastPtr).StartYearID]     & EndYear = Datayear[(*LastPtr).EndYearID]
	StartMonth = GrowthMonth[(*LastPtr).StartMonthID] & EndMonth= GrowthMonth[(*LastPtr).EndMonthID];���ĸ�����ָ��Ӧ�Ŀ�ʼ��������ݺ������·�,�ַ���
	CalParaYear	= STRTRIM((BIN_DATE())[0],2)   ;������������,һ����ϵͳ�����


	CROP = (*LastPtr).CropIDList
	CropList=(*LastPtr).CropNameList
	Cropindex=CROP[WHERE(CropList EQ (*ThirdStep).CropName[0])]
	CurrentYear = STRTRIM((*ThirdStep).CurrentYear,2)   ;���㲨�����������

	WIDGET_CONTROL,(*LastPtr).MeteorologyFactor,GET_VALUE=FactorSel

	CASE 1 OF ;AGROSTATION_ESTIMATED_YIELD
	  ((*LastPtr).FactorType EQ 0):BEGIN
		Widget_Control,(*LastPtr).factor_TABLE,GET_UVALUE=FactorID    ;�õ���ѡ�е�����ID��,ע����������,��һ��ֵΪҪ������ID

		FactorSel = STRJOIN(STRTRIM(FactorSel,2),'/')
		Coefficient = STRJOIN(STRTRIM((*ThirdStep).Coefficient,2),'/')
		FactorID=STRJOIN(STRTRIM(TRANSPOSE(FactorID[0,0:*]),2),'/')

		IF ((*LastPtr).YieldType EQ 0) THEN BEGIN
			YieldTable = 'COUNTY_Crop_MeteoOrFluctuate_yield'
			ParaTable = 'county_Meteo_yield_PARAMETER'
			Scale = 'county_code'
			ModelTypeId = '1'
			EstimateTable = 'COUNTY_ESTIMATED_YIELD'
		ENDIF ELSE BEGIN
			YieldTable = 'AGROSTATION_Crop_MeteoOrFluctuate_yield'
			ParaTable = 'AGROSTATION_Meteo_yield_PARAMETER'
			Scale = 'AgroMeteoStation_CODE'
			ModelTypeId = '2'
			EstimateTable = 'AGROSTATION_ESTIMATED_YIELD'
		ENDELSE


		;=====�������޸ģ�20070908=====================================
;	    Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�������
;	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+ $	;ԭ����
;	         "' and year="+CurrentYear+' and ModelData_id=1'
;		Sqlstr2='insert into '+YieldTable+" values('"+(*ThirdStep).CountyCode+"','"+Cropindex+"'," $
;			+MeteoYileld[(N_ELEMENTS(MeteoYileld)/2)-1,1]+','+CurrentYear+',1)'

		Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�������
	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+ $
	         "' and year="+CurrentYear+' and ModelData_id='+ModelTypeId
		Sqlstr2='insert into '+YieldTable+" values('"+(*ThirdStep).CountyCode+"','"+Cropindex+"'," $
			+MeteoYileld[(N_ELEMENTS(MeteoYileld)/2)-1,1]+','+CurrentYear+','+ModelTypeId+')'

		;========================================================================

		F_Value = STRMID(ModelParameter[2],STRPOS(ModelParameter[2],'=')+1)  ;�������
	    Sqlstr3='delete from '+ParaTable+' where '+Scale+"='" $
	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+"'"
		Sqlstr4='insert into '+ParaTable+" values('"+(*ThirdStep).CountyCode+"','"+Cropindex+"','"+ $
		     Coefficient+"','"+FactorSel+"','"+FactorID+"','"+ModelParameter[4]+"',"+StartMonth+','+EndMonth $
		     +','+ModelParameter[0]+','+F_Value+','+ModelParameter[1]+','+StartYear+','+EndYear+',' $
		     +CalParaYear+')'
	  END


	((*LastPtr).FactorType EQ 1):BEGIN

		FactorSel = STRTRIM(FactorSel,2)
		Coefficient = STRJOIN(STRTRIM((*ThirdStep).Coefficient,2),'/')

		IF ((*LastPtr).YieldType EQ 0) THEN BEGIN
			YieldTable = 'COUNTY_Crop_MeteoOrFluctuate_yield'
			ParaTable = 'County_Fluctuate_yield_PARAMETER'
			Scale = 'county_code'
			ModelTypeId = '3'
			EstimateTable = 'COUNTY_ESTIMATED_YIELD'
		ENDIF ELSE BEGIN
			YieldTable = 'AGROSTATION_Crop_MeteoOrFluctuate_yield'
			ParaTable = 'AGROSTATION_Fluctuate_yield_PARAMETER'
			Scale = 'AgroMeteoStation_CODE'
			ModelTypeId = '4'
			EstimateTable = 'AGROSTATION_ESTIMATED_YIELD'
		ENDELSE

		;=====�������޸ģ�20070908=====================================
;	    Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�������
;	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+ $	;ԭ����
;	         "' and year="+CurrentYear+' and ModelData_id=2'				;�˴�Ϊ2
;		Sqlstr2='insert into '+YieldTable+" values('"+(*ThirdStep).CountyCode+"','"+Cropindex+"'," $
;			+MeteoYileld[(N_ELEMENTS(MeteoYileld)/2)-1,1]+','+CurrentYear+',2)'

		Sqlstr1='delete from '+YieldTable+' where '+Scale+"='" $           ;�������
	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+ $
	         "' and year="+CurrentYear+' and ModelData_id='+ModelTypeId				;�˴�Ϊ2
		Sqlstr2='insert into '+YieldTable+" values('"+(*ThirdStep).CountyCode+"','"+Cropindex+"'," $
			+MeteoYileld[(N_ELEMENTS(MeteoYileld)/2)-1,1]+','+CurrentYear+','+ModelTypeId+')'
		;===================================================================

		SensorType = (*LastPtr).SensorType									;����������
		F_Value = STRMID(ModelParameter[2],STRPOS(ModelParameter[2],'=')+1)  ;�������
	    Sqlstr3='delete from '+ParaTable+' where '+Scale+"='" $
	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+"'"
		Sqlstr4='insert into '+ParaTable+" values('"+(*ThirdStep).CountyCode+"','"+Cropindex+"'," $
		     +FactorSel+",'"+ModelParameter[4]+"','"+Coefficient+"',"+ModelParameter[0]+','+F_Value +',' $
		     +ModelParameter[1]+','+StartMonth+','+EndMonth+','+StartYear+','+EndYear+',' $
		     +CalParaYear+",'"+SensorType+"')"
	 END

	 ELSE:
	ENDCASE

	    Sqlstr5='delete from '+EstimateTable+' where '+Scale+"='" $           ;����������
	         +(*ThirdStep).CountyCode+"' and crop_id='"+ Cropindex+ $
	         "' and year="+CurrentYear+' and Model_type_id='+ModelTypeId
		Sqlstr6='insert into '+EstimateTable+" values('"+Cropindex+"','"+(*ThirdStep).CountyCode+"'," $
			+Estimate+','+ModelTypeId+','+CurrentYear+')'

	DBobj->ExecuteSQL,Sqlstr1
	DBobj->ExecuteSQL,Sqlstr2
	DBobj->ExecuteSQL,Sqlstr3
	DBobj->ExecuteSQL,Sqlstr4
	DBobj->ExecuteSQL,Sqlstr5
	DBobj->ExecuteSQL,Sqlstr6

	Prompt=DIALOG_MESSAGE('���ɹ�!',TITLE='��ʾ',/INFORMATION)
	log, '����Ԥ��-��������', 1
END
;---------------------------------------
;PRO CleanAllHeap,TLB
;;    WIDGET_CONTROL,TLB,GET_UVALUE=PA
;;    PTR_FREE,PA
;  	HEAP_GC,/VERBOSE
;END
;*******************************"����ģ�������"�¼�****************************
PRO DC_FloatYield_3_event,event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top
  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='quit'):widget_control,Event.top,/DESTROY
    ELSE:
  ENDCASE
END

;*****************************��������ģ�����������******************************
PRO DC_FloatYield_3,MeteoYieldResult,LastPtr,GROUP_LEADER=groupleader,ITS_TOPBASE=its_topbase

  IF (*LastPtr).YieldType THEN BEGIN
  	Scale = 'վ��'
  	Scale_code = 'AgroMeteoStation_CODE'
  	TrendYieldTab = 'AGROSTATION_CROP_TREND_YIELD'
  ENDIF ELSE BEGIN
  	Scale = '��'
  	Scale_code = 'COUNTY_CODE'
  	TrendYieldTab = 'COUNTY_CROP_TREND_YIELD'
  ENDELSE

  MeteoYieldSimulation = Widget_Base(GROUP_LEADER=groupleader,  $
      UNAME='MeteoYieldSimulation' ,XOFFSET=200 ,YOFFSET=200  $
      ,SCR_XSIZE=567 ,SCR_YSIZE=472 ,/BASE_ALIGN_LEFT  $
      ,TITLE='��������ģ��' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
      ,COLUMN=1,TLB_FRAME_ATTR=1);,/MODAL)
  ;-----------------------�����ϰ벿��---------------------------------
  Des_map_BASE = Widget_Base(MeteoYieldSimulation,  $
      UNAME='Des_map_BASE',SCR_XSIZE=548 ,SPACE=3,$
      XPAD=0,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

;---------------------�ϰ벿����߲���--------------------------------
  description = Widget_Base(Des_map_BASE, UNAME='description'  $
      ,/FRAME,SCR_XSIZE=185,SPACE=3 ,XPAD=1 ,YPAD=1 ,COLUMN=1)

	  dotInfo_LABEL = Widget_Label(description, UNAME='dotInfo_LABEL'  $
	      ,XOFFSET=3 ,YOFFSET=3 ,SCR_XSIZE=65 ,SCR_YSIZE=13 ,/ALIGN_LEFT  $
	      ,VALUE='Ŀ����Ϣ��')
	  ;------------------����������Ϣ--------------
	  county_crop = Widget_Base(description, UNAME='county_crop' ,FRAME=1  $
	      ,SCR_XSIZE=177 ,SCR_YSIZE=56  $
	      ,SPACE=0 ,XPAD=1 ,YPAD=1 ,COLUMN=1)

	          width=70 & height = 19
		  county=WIDGET_BASE(county_crop,/ROW,/BASE_ALIGN_CENTER)
		  county_LABEL = Widget_Label(county, UNAME='county_LABEL'  $
		      ,SCR_XSIZE=width,/ALIGN_LEFT  $
		      ,VALUE= Scale +'���룺')
		  Count_TEXT = Widget_Text(county, UNAME='Count_TEXT' ,FRAME=1  $
		      ,SCR_XSIZE=width ,SCR_YSIZE=height   $
		      ,XSIZE=20 ,YSIZE=1)

		  crop=WIDGET_BASE(county_crop,/ROW,/BASE_ALIGN_CENTER)
		  Crop_LABEL = Widget_Label(crop, UNAME='Crop_LABEL'  $
		      ,XOFFSET=3 ,YOFFSET=25 ,SCR_XSIZE=width  ,/ALIGN_LEFT  $
		      ,VALUE='�������ƣ�')
		  Cro_TEXT = Widget_Text(crop, UNAME='Cro_TEXT' ,FRAME=1  $
		      ,XOFFSET=70 ,YOFFSET=25 ,SCR_XSIZE=width ,SCR_YSIZE=height   $
		      ,XSIZE=20 ,YSIZE=1)
	  ;----------�����ı�----------
	  yield_BASE = Widget_Base(description, UNAME='yield_BASE' ,FRAME=1  $
	        ,SCR_XSIZE=175 ,SCR_YSIZE=55  $
	      ,SPACE=6 ,XPAD=1 ,YPAD=3 ,ROW=2,/BASE_ALIGN_TOP,/align_center)

		  yieldPrompt=Scale+STRTRIM(MeteoYieldResult.CurrentYear,2)+'�겨��/���������'
		  year_LABEL = Widget_Label(yield_BASE, UNAME='year_LABEL' $
		     ,SCR_XSIZE=160 ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE=yieldPrompt)
		  Yield_TEXT = Widget_Text(yield_BASE, UNAME='Yield_TEXT' ,XOFFSET=3  $
		      ,YOFFSET=26 ,SCR_XSIZE=164 ,SCR_YSIZE=21)

	  ;----------��ʾ����------
	  prompt_TEXT = Widget_Text(description, UNAME='prompt_TEXT' $
	      ,VALUE='���ѣ��ۺϲ���ģ�Ͳ�����������Ϣ�������ģ��������������,��ص���һ�����¼��㡣'$
	      ,YSIZE=4,SCR_XSIZE=175,SCR_YSIZE=56,/FRAME,/WRAP)

  ;-------------------DRAW����------------------------------------------
  map_DRAW = Widget_Draw(Des_map_BASE, UNAME='map_DRAW' ,/FRAME $
      ,XOFFSET=191 ,YOFFSET=3 ,SCR_XSIZE=358 ,SCR_YSIZE=205)

  ;--------****************�°벿��******---------------------------------------
  model_yield_BASE = Widget_Base(MeteoYieldSimulation,  $
      UNAME='model_yield_BASE'    $
      ,SCR_XSIZE=548 ,SCR_YSIZE=178 ,/BASE_ALIGN_LEFT   $
      ,SPACE=3 ,XPAD=0,YPAD=0 ,COLUMN=1)
	  ;-------------------ģ�Ͳ�����--------------------------------
	parameter=widget_base(model_yield_base,FRAME=1,/column,SCR_XSIZE=548,SCR_YSIZE=75,XPAD=0,YPAD=1)
	  model_description = Widget_Label(parameter,  $
	      UNAME='model_description'   $
	      ,SCR_XSIZE=249 ,SCR_YSIZE=13 ,/ALIGN_CENTER  $
	      ,VALUE='��������ģ�Ͳ���')

	; ColumnLabels=['�����ϵ��','��׼��','F����ֵ','�ο�F0.05(6,14)','�ο�F0.05(1,19)','ģ�ⷽ��','������']
	 ColumnLabels=['�����ϵ��','��׼��','F����ֵ','ģ�ⷽ��','������']
	  modelParameter_TABLE = Widget_Table(parameter,  $
	      UNAME='modelParameter_TABLE' ,SCR_XSIZE=547 ,SCR_YSIZE=58  $
	       ,/EDITABLE ,/RESIZEABLE_COLUMNS,XSIZE=5,YSIZE=1,/FRAME  $
	      ,ROW_LABELS=['����ֵ'],COLUMN_LABELS=ColumnLabels $
	      ,COLUMN_WIDTHS=[80,80,100,300,400],/ALIGN_LEFT)

	  ;--------------����������-------------------------------
	  FIELD=WIDGET_BASE(model_yield_base,FRAME=1,/column,SCR_XSIZE=548,SCR_YSIZE=91,XPAD=0,YPAD=1)
	  meteroYileld_LABEL = Widget_Label(FIELD,  $
	      UNAME='meteroYileld_LABEL' ,XOFFSET=213 ,YOFFSET=90  $
	      ,SCR_XSIZE=121 ,SCR_YSIZE=13 ,/ALIGN_CENTER ,VALUE='ģ��Ĳ�������')

	  rowlabel=['ʵ��ֵ','ģ��ֵ']
	  meteroYileld_TABLE = Widget_Table(FIELD,  $
	      UNAME='meteroYileld_TABLE' ,ROW_LABELS=rowlabel,/RESIZEABLE_COLUMNS $
	      ,SCR_XSIZE=547 ,SCR_YSIZE=74 ,/EDITABLE ,XSIZE=25 ,YSIZE=2,/FRAME)

  ;----------------------��ť����--------------------------
  save_quit_BASE = Widget_Base(MeteoYieldSimulation,  $
      UNAME='save_quit_BASE' ,FRAME=1   $
      ,SCR_XSIZE=548 ,SCR_YSIZE=38 ,/BASE_ALIGN_TOP ,TITLE='IDL'  $
      ,SPACE=56 ,XPAD=42 ,YPAD=1 ,ROW=1)

  ;------------------���水ť����-----------------------------------
;	  save_BASE = Widget_Base(save_quit_BASE, UNAME='save_BASE'  $
;	      ,SCR_XSIZE=380 ,SCR_YSIZE=29,SPACE=35 ,XPAD=10 ,YPAD=1 ,ROW=1)
		  Width=120 & height=22
		  InputDB = Widget_Button(save_quit_BASE, UNAME='InputDB' ,XOFFSET=20  $
		      ,YOFFSET=4 ,SCR_XSIZE=Width ,SCR_YSIZE=height ,/ALIGN_CENTER  $
		      ,TOOLTIP='��ģ��Ĳ������뵽���ݿ���' ,VALUE='�������',/SENSITIVE,EVENT_PRO='DC_InputDBEV')

		  SaveYieldModel = Widget_Button(save_quit_BASE, UNAME='SaveYieldModel' ,XOFFSET=110  $
		      ,YOFFSET=3 ,SCR_XSIZE=Width ,SCR_YSIZE=height ,/ALIGN_CENTER  $
		      ,TOOLTIP='��ģ�ͺͲ������浽���ش���' ,VALUE='�������ģ��',EVENT_PRO='DC_SaveYieldModelEV')
	  ;----------------------------�˳���ť-------------------------
	  quit = Widget_Button(save_quit_BASE, UNAME='quit' ,XOFFSET=374  $
	      ,YOFFSET=6 ,SCR_XSIZE=100 ,SCR_YSIZE=height ,/ALIGN_CENTER  $
	      ,VALUE='�ر�',TOOLTIP='�˳���������ģ�������')
	  ;---------------------------------------------------------------

  K=N_ELEMENTS(MeteoYieldResult.ModelCoefficient)
  N =N_ELEMENTS(MeteoYieldResult.ActualMeteoYield)   ;�Ա���������������

  ModelTable=STRARR(5)         ;���ڷ���ģ�Ͳ���
  ModelTable[0]=STRTRIM(MeteoYieldResult.M_correlation,2)      ;ModelTable[1]Ϊ����ı�׼��,��ʽ��<<ң��ѧ��>>2004,8(6)P492
  ModelTable[1]=STRTRIM(SQRT(TOTAL((FLOAT(MeteoYieldResult.ActualMeteoYield)-FLOAT(MeteoYieldResult.SimuMeteoYield))^2)/(N-K-1)),2)   ;
  ModelTable[2]='F('+STRTRIM(K,2)+','+STRTRIM(N-K-1,2)+')='+STRTRIM(MeteoYieldResult.F_check,2)
;;  ModelTable[3]='2.85' &  ModelTable[4]='4.38'                               ;���Ŷ�Ϊ0.05,���ɶ�f1�ֱ�Ϊ1,6;f2�ֱ�Ϊ19,14��F����ֵ.
   Equation='Y='+STRTRIM(MeteoYieldResult.Const,2)
     FOR i=0,N_ELEMENTS(MeteoYieldResult.ModelCoefficient)-1 DO BEGIN
        IF FLOAT(MeteoYieldResult.ModelCoefficient[i]) LT 0 THEN BEGIN
           Temp=STRTRIM(MeteoYieldResult.ModelCoefficient[i],2)    ;�����˸���
        ENDIF ELSE BEGIN
           Temp='+'+STRTRIM(MeteoYieldResult.ModelCoefficient[i],2)
        ENDELSE
         Equation=Equation+ Temp+'X'+STRTRIM(i+1,2)
     ENDFOR
  ModelTable[3]=Equation    ;����ʽ
  ModelTable[4]=STRJOIN(MeteoYieldResult.SelectedFactor_Name,'/')

  YieldYearNum=N_ELEMENTS(MeteoYieldResult.ActualMeteoYield)
  MeteoYield=STRARR(YieldYearNum+1,2)    ;������������
  YieldTableLabel=STRTRIM([INDGEN(YieldYearNum)+FIX(MeteoYieldResult.StartYear),[MeteoYieldResult.CurrentYear]],2)  ;�����������
  MeteoYield[0:YieldYearNum-1,0]=FLOAT(MeteoYieldResult.ActualMeteoYield)
  MeteoYield[0:YieldYearNum-1,1]=FLOAT(MeteoYieldResult.SimuMeteoYield)
  MeteoYield[YieldYearNum,1]=FLOAT(MeteoYieldResult.CurrentYearMeteoYield)

  Widget_Control,meteroYileld_TABLE,TABLE_XSIZE=YieldYearNum+1,SET_VALUE=MeteoYield $
                ,COLUMN_LABELS=YieldTableLabel, ALIGNMENT=2,SET_TABLE_SELECT=[-1,-1,-1,-1]

  Widget_Control,modelParameter_TABLE,SET_VALUE=ModelTable $
                ,SET_TABLE_SELECT=[-1,-1,-1,-1]

  CurrentYear = STRTRIM(MeteoYieldResult.CurrentYear,2)   ;���㲨�����������
  CountyCode  = MeteoYieldResult.CountyCode
  CropName    = MeteoYieldResult.CropName
  CropId  = (*LastPtr).CropId

  SQL = 'select trend_Yield from '+TrendYieldTab+" where crop_id='"+CropId+"' and " $
  		+Scale_code +"='"+CountyCode+"' and year="+CurrentYear
  Trendyield = DC_GetdataFromDB_Str(1,SQL)  				 ;char��
  Floatyield = MeteoYieldResult.CurrentYearMeteoYield   ;float��
  Estimate   = STRTRIM(FLOAT(Trendyield[0,0])+Floatyield,2)

  Widget_Control,Count_TEXT,SET_VALUE= CountyCode
  Widget_Control,Cro_TEXT,SET_VALUE= CropName
  Widget_Control,Yield_TEXT,SET_VALUE=STRING(Floatyield)+'/'+Estimate

  Widget_Control, /REALIZE, MeteoYieldSimulation
  WIDGET_CONTROL,quit,/INPUT_FOCUS
;---------------------------
  WIDGET_CONTROL,map_DRAW,GET_VALUE=drawID                 ;ע��Ҫ�õ����ֵ,������"����ܹ�ʵ��֮��"/REALIZE��ſ��Եõ�.
  Save_win = !D.WINDOW
  OldBackup = !P.BACKGROUND
  !P.BACKGROUND = 255
  WSET,drawID
  DEVICE,GET_DECOMPOSED=old_color                               ;��ȡ��ǰDECOMPOSEDֵ
  DEVICE,GET_CURRENT_FONT=oldFont,SET_FONT='*'   ;ע��:SET_FONT='*'
  DEVICE,SET_FONT='SimHei',/TT_FONT

    PLOT,INDGEN(1,YieldYearNum)+1,MeteoYield[0:YieldYearNum-1,0],XRANGE=[1,YieldYearNum], PSYM=-2 $
         ,THICK=0,XSTYLE=1,XTITLE='�������:'+YieldTableLabel[0]+'-'+YieldTableLabel[YieldYearNum-1] $
         ,COLOR=DC_MyColor(1),POSITION=[0.08,0.2,0.98,0.98],FONT=0;;,CHARSIZE=1
;;    XYOUTS,0.6,0.32,'--*--*--*--   ʵ�ʲ���',ALIGNMENT=0,COLOR=DC_MyColor(1),/NORMAL,FONT=0     ;ע�͵�Ϊ��������
;;    XYOUTS,0.6,0.24,'.............  ģ�����',ALIGNMENT=0,COLOR=DC_MyColor(2),/NORMAL,FONT=0
    XYOUTS,0.6,0.32,'--*--*- ʵ�ʲ���',ALIGNMENT=0,COLOR=DC_MyColor(1),/NORMAL,FONT=1,CHARSIZE=1.6
    XYOUTS,0.6,0.24,'--*--*- ģ�����',ALIGNMENT=0,COLOR=DC_MyColor(3),/NORMAL,FONT=1,CHARSIZE=1.6

    OPLOT,INDGEN(1,YieldYearNum)+1,MeteoYield[0:YieldYearNum-1,1] ,LINESTYLE=0 $
    	 ,THICK=1,COLOR=DC_MyColor(3),PSYM=-2
  DEVICE,SET_FONT=oldFont
  DEVICE,DECOMPOSED=old_color                                   ;����ԭ����DECOMPOSEDֵ,��Ϊ�Զ��庯��DC_MyColor�ı���,�뻹ԭ.
  !P.BACKGROUND = OldBackup
  WSET, Save_win
  its_topbase = MeteoYieldSimulation   ;����TLB

  Coefficient = [MeteoYieldResult.Const,TRANSPOSE(MeteoYieldResult.ModelCoefficient)]   ;����ϵ��(��������)

  ThirdStep={CropName         		:  CropName					    ,$
             CountyCode       		:  CountyCode				 	,$
             YieldTableLabel  		:  YieldTableLabel           	,$    ;�ڱ���ʱ���Եõ����
             modelParameter_TABLE	:  modelParameter_TABLE    		,$
             meteroYileld_TABLE  	:  meteroYileld_TABLE    		,$
             CurrentYear      		:  CurrentYear					,$
			 Trendyield				:  Trendyield[0,0]				,$	  ;���Ʋ���
			 Coefficient  			:  Coefficient					,$    ;����ϵ��(������)
             LastPtr				:  LastPtr}			;LastPtr�Ǻ����ӵ�,���������������ص�����

  Widget_Control, MeteoYieldSimulation, SET_UVALUE=PTR_NEW(ThirdStep,/NO_COPY)
  XManager,'DC_FloatYield_3',MeteoYieldSimulation,CLEANUP='DC_3_CleanAllHeap',/NO_BLOCK

END
;*******************************************************************************************

PRO DC_3_CleanAllHeap,TLB
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2

    WIDGET_CONTROL,TLB,GET_UVALUE=PA
	PTR_FREE,PA
;;  HEAP_FREE,PA   ;�˴�������HEAP_FREE,����Ϊ���������ϼ�ָ��LastPtr,��DC_FloatYield_2��������û�ֵ.
;;�ϼ�ָ��LastPtr�е������������õ�.
;;(*LastPtr).ARRAY_YEAR
;;(*LastPtr).StartYearID
;;(*LastPtr).EndYearID
;;(*LastPtr).StartMonthID
;;(*LastPtr).EndMonthID
;;(*LastPtr).MeteorologyFactor
;;(*LastPtr).FactorType
;;(*LastPtr).factor_TABLE
;;(*LastPtr).YieldType
;;(*LastPtr).SensorType
END
