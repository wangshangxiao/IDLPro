
;******ͳ�Ʋ�������������************************************
FUNCTION DC_B_StatisticFloat,StaFile,WeightFile,STATUS = status
	;StaFile  ��ͳ���ļ�;WeightFileȨ���ļ�;status�����Ƿ�ɹ���״̬
	;������ʽ:Re = DC_B_StatisticFloat(StaFile,WeightFile,[STATUS=status]) ע��ֻ����status
	;����ֵ: ���ؽṹ��
	;�㷨:Sum(Pix*W)/sum(W)  Pix��Ԫֵ;WΪȨ��

	CountyRaster = 'data_grid\county_raster'

	WeightData = DC_Read_ENVIData(WeightFile,SUCCESSSTATUS = StatusW,Description='����Ȩ���ļ�')
	IF NOT StatusW THEN BEGIN		;WeightData��0-100֮��
		STATUS=StatusW
		RETURN,0
	ENDIF

	StaData    = DC_Read_ENVIData(StaFile,SUCCESSSTATUS = StatusS,Description='�����ļ�')
	IF NOT StatusS THEN BEGIN		;StaData ����ֵ
		STATUS=StatusS
		RETURN,0
	ENDIF

	CountyData= DC_Read_ENVIData(CountyRaster,SUCCESSSTATUS = StatusC,Description='��դ���ļ�')
	IF NOT StatusC THEN BEGIN
		STATUS=StatusC
		RETURN,0
	ENDIF

	Sinfo = DC_ReadHead_file(StaFile)
	Winfo = DC_ReadHead_file(WeightFile)
	Cinfo = DC_ReadHead_file(CountyRaster)
	IF FIX(Sinfo.samples) NE FIX(Winfo.samples) OR FIX(Sinfo.samples) NE FIX(Cinfo.samples) OR   $
	   FIX(Sinfo.lines)   NE FIX(Winfo.lines)   OR FIX(Sinfo.lines)   NE FIX(Cinfo.lines) THEN BEGIN
		Info = DIALOG_MESSAGE('����ͳ�ƵĻ�������������ļ���С��һ��,ͳ��ʧ��!',TITLE='����')
		STATUS=0
		RETURN,0
	ENDIF

	CountyID = CountyData[UNIQ(CountyData,SORT(CountyData))]    ;�õ�Ψһ����Index (�����ش���)
	CountyID = CountyID[WHERE(CountyID NE 0)]					;ȥ��0 Index

	Result = REPLICATE({CountyID:0,FloatYield:0.0},N_ELEMENTS(CountyID))

	FOR I=0,N_ELEMENTS(CountyID)-1 DO BEGIN
		County  = WHERE(CountyData EQ CountyID[I])
		DataV   = StaData[County]			;DOUBLE��
		W_Value = WeightData[County]*1.0	;BYTE��,��תΪ������.
		DataV   = TEMPORARY(DataV)*W_Value

		Valid   = WHERE(W_Value GE 10)				;ֻͳ��Ȩ�ش���10%

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

      Widget_Control,(*state).StaYield_LABEL,SET_VALUE='�ջ�ָ��������ĵ������'

      Widget_Control,(*state).prompt_TEXT ,SET_VALUE=''

      Widget_Control,(*state).Extrapolate_DRAW ,GET_VALUE=Owindow
      Owindow->ERASE,COLOR=255
	  oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	  IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

      Widget_Control,(*state).Yield_DRAW ,GET_VALUE=Owindow
      Owindow->ERASE,COLOR=218
	  oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	  IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ

	  (*state).Savefile = ''			;ͬʱʹHI��ֵ�ļ���Ϊ��

   Widget_Control ,EventTop,SET_UVALUE=state
END

;*******�Զ������:��ȡվ��ģ��Ĺ������������������,�Լ���վ���ջ�ָ��*******************
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
		 Prompt=DIALOG_MESSAGE('����ѡ��һ��վ���������"����"',/INFORMATION,TITLE='����')
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
		 Prompt=DIALOG_MESSAGE('���ݿ���"'+ProName+CropName+(*state).CalcYear+'��'+ $
		                       '��û����Ӧվ����������û��վ��������,������ݿ�!',TITLE='��ʾ',/INFORMATION)
	 	 RETURN
	ENDIF


    RowsNum = NumReocrd   & ColumnLabel=['վ��','�������','������','�ջ�ָ��','����','γ��']
    Widget_Control,(*state).FloatYield_Table,TABLE_YSIZE=RowsNum,SET_VALUE=MeteorologyYield[1:*,0:*] $
              ,ROW_LABELS=MeteorologyYield[0,0:*],COLUMN_LABELS=ColumnLabel,ALIGNMENT=0 $
              ,COLUMN_WIDTHS=60,SET_UVALUE=MeteorologyYield              ;�û�ֵ���ڱ�����.
    Widget_Control,(*state).prompt_TEXT ,SET_VALUE='��ʾ:��ǰѡ�����'+ProName+(*state).CalcYear+'��ģ���'+CropName+ $
                'վ���ջ�ָ��!��'+ STRTRIM(RowsNum,2)+'��վ��!'

END
;****���ݲ�ֵͼ���в���ͳ�Ƶ�"ͳ��"��ť**********************************
PRO DC_B_StatisticEV,EVENT

	Widget_Control,Event.top,GET_UVALUE=state
	CropName =(*state).CropName
	CROP     =(*state).CropIDlist                ;��������ID��,ע��Ҫ��Ӧ����DROPLIST
	crop_id   = CROP[(*state).Cropindex]      &   year=(*state).CalcYear
	Crop_name = STRCOMPRESS(CropName[(*state).Cropindex],/REMOVE_ALL)

	IF (*state).TempCalcYear NE '' THEN BEGIN  ;��һ���ǵ�ʹ������HI�Ͳ�������ͳ�Ƶ�����
		crop_id  = CROP[(*state).TempCropindex]
		year  = (*state).TempCalcYear
	    Crop_name = STRCOMPRESS(CropName[(*state).TempCropindex],/REMOVE_ALL)
	ENDIF

	Widget_control,/HOURGLASS

	CATCH, Error_status               ;��ȡ����.
	IF Error_status NE 0 THEN BEGIN
		infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
		CATCH, /CANCEL
		RETURN
	ENDIF

        IF  (*state).SaveYieldfile EQ '' THEN BEGIN

			Prompt=DIALOG_MESSAGE('���ȼ���õ������ֲ�ͼ����ͳ��!',/INFORMATION,TITLE='��ʾ')
			RETURN

         ENDIF ELSE BEGIN
           Yfile=(*state).SaveYieldfile
         ENDELSE

  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='ͳ��',MESSAGE='����ͳ�ƴ�����,���Ժ�!') ;�½�����������
	 progressTimer->START
     progressTimer->UPDATE,(0.1 * 100.0)  ;���½�����

     PaddyCrop=['21','22','23']
     IF WHERE(PaddyCrop EQ crop_id[0]) EQ -1 THEN BEGIN
        LandWeightFile='data_grid\DRY_LAND_ratio'                         ;�������ˮ��,���ú��ذٷֱ��ļ�.
     ENDIF ELSE BEGIN
        LandWeightFile='data_grid\PADDY_FIELD_ratio'
     ENDELSE

;   �õ���Y_Result�ǽṹ��,����CountyID(��id,���ش���),FloatYield(ƽ��ֵ)������Ҫ�Ĳ���
	Y_Result = DC_B_StatisticFloat(Yfile,LandWeightFile,STATUS = status1)
    IF status1 EQ 0 THEN BEGIN   ;ͳ�Ʋ��ɹ�����
        OBJ_DESTROY,progressTimer
		RETURN
	ENDIF
;   �õ���HI_Result�ǽṹ��,����CountyID(��id,���ش���),FloatYield������Ҫ���ջ�ָ��HI
	HI_Result = DC_B_StatisticFloat((*state).Savefile,LandWeightFile,STATUS = status2)
    IF status2 EQ 0 THEN BEGIN
        OBJ_DESTROY,progressTimer
		RETURN
	ENDIF

    progressTimer->UPDATE, (0.75 * 100.0)  ;���½�����

    RowsNUM = N_elements(Y_Result)

	StatisticYield = [TRANSPOSE(STRTRIM(Y_Result.FloatYield,2)), $
					  TRANSPOSE(STRTRIM(Y_Result.CountyID,2)), 	 $
					  TRANSPOSE(STRTRIM(HI_Result.FloatYield,2))]
 	ProName = (*state).Province[WHERE((*state).ProIDlist EQ (*state).ProID)]

    SQLstr='select code,raster_value from COUNTY_CODE_RASTER where LEFT(code,2)='+(*state).ProID
;;	SQLstr='select b.code,b.raster_value,a.name from county_code a,('+SQLstr+') b where a.code=b.code'
	CountyCodeID = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = n)  ;��һ����Ϊ���County_code_raster���ش���.
	IF n EQ 0 THEN BEGIN
		INFO = DIALOG_MESSAGE('������COUNTY_CODE_RASTER��û��'+ProName+'�������Ӧ����!',TITLE='����')
        OBJ_DESTROY,progressTimer
		RETURN
	ENDIF

    progressTimer->UPDATE, (0.8 * 100.0)  ;���½�����

     CountyMeteoYield=STRARR(3,1)
     FOR i=0,RowsNUM-1 DO BEGIN
         MeteoYield=STRARR(3,1)
         FOR j=0,n-1 DO BEGIN
            IF FIX(StatisticYield[1,i]) EQ FIX(CountyCodeID[1,j]) THEN BEGIN
               MeteoYield[0,0]=CountyCodeID[0,j]	;�ش���
               MeteoYield[1,0]=StatisticYield[0,i]  ;����CountyID��raster_value�Ķ�Ӧ,��������
               MeteoYield[2,0]=StatisticYield[2,i]  ;����CountyID��raster_value�Ķ�Ӧ,�ջ�ָ��
               CountyMeteoYield=[[CountyMeteoYield],[MeteoYield]]
               BREAK
            ENDIF
         ENDFOR
     ENDFOR

    TempYield=CountyMeteoYield[0:*,1:*]     ;�������һ��Ϊ�ش���,�ڶ���Ϊͳ�ƵĲ���.������Ϊ�ջ�ָ��
    TempYield[1,*] = STRTRIM(FLOAT(TempYield[1,*])*2/3,2)   ;ͳ�ƵĲ�����λ:g/m2,����תΪ����/Ķ
    CountyMeteoYield = 0B 					;�ͷ��ڴ�
    progressTimer->UPDATE,(0.9*100.0)  	;���½�����

	CropYear={CropID:crop_id,Year:year}
    n=N_ELEMENTS(TempYield)/3
    SimuYield = [TempYield,STRARR(1,N)+Crop_name,STRARR(1,N)+year]
    Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=3
    Widget_Control,(*state).StaYield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=n,SET_VALUE=SimuYield $
              ,COLUMN_LABELS=['����','ģ�����','�ջ�ָ��','����','���'],ALIGNMENT=1 $
              ,ROW_LABELS=STRTRIM(INDGEN(n)+1,2),COLUMN_WIDTHS=68,SET_UVALUE=CropYear
;;    Widget_Control,(*state).StaYield_LABEL,SET_VALUE='ͳ�Ʋ���(ͳ�ƹ�'+STRTRIM(n,2)+'����)'
    Widget_Control,(*state).StaYield_LABEL,SET_VALUE='�ջ�ָ��������ĵ������'
     progressTimer->UPDATE, (1 * 100.0)  ;���½�����
	 OBJ_DESTROY,progressTimer
   Prompt=DIALOG_MESSAGE('ͳ�����!',TITLE='��ʾ',/INFORMATION)

END
;**********************���������뵽���ݿ���*******************************************
PRO DC_B_InputDB_EV,Event
     Widget_Control,Event.top ,GET_UVALUE=state
     Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield $
                   ,GET_UVALUE=CropYear                       ;�ñ�����������ID�����
      Widget_control,/HOURGLASS

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('û�в�������,�����Ƚ���ͳ��!',TITLE='��ʾ',/INFORMATION)
        RETURN
     ENDIF

  	 CountyYield = STRTRIM(TEMPORARY(CountyYield),2)
     crop_id=CropYear.cropid    &   CalYear=CropYear.year

      CountyNum=N_ELEMENTS(CountyYield)/5

	 COMMON COMMON_BLOCK

	progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='���ڽ�������������뵽����,���Ժ�...' $
							,TITLE='���㵥�����')
	progressTimer->START                         ;����������

	 FOR i=0,CountyNum-1 DO BEGIN     ;����ֵ��ͳ�Ƶĸ��ع�����ջ�ָ�����.

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

	;--��Ȩ��ʡ-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,������CROP_AREA_COUNTY
	;ע������ȡ�������ʱ,�����ǰ��ݵ��������û��,���ÿ���������Newestyear���������

	INFO = DIALOG_MESSAGE('����Ҫ���ع��������Ȩ��ʡ��?',/QUESTION,TITLE='ѯ��')
	IF INFO EQ 'No' THEN RETURN
	CountyYield = CountyYield[0:1,*]		;2��ֵ,����\�������
	Status = DC_WeightToPro(CountyYield,Crop_id,CalYear,(*state).ProID,'6')
	IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('��Ȩ����ɹ�!',/INFORMATION,TITLE='��ʾ')

END
;*************************************************************************
PRO DC_Bio_HI_event,event
   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
   wWidget =  Event.top

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:' $
                                   ,[!ERROR_STATE.MSG]],TITLE='����',/ERROR,DIALOG_PARENT=Event.top)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

   Widget_control,/HOURGLASS
   Widget_Control,wWidget,GET_UVALUE=state
  CASE wTarget OF
   Widget_Info(wWidget, FIND_BY_UNAME='Crop_DROPLIST'): BEGIN
	 	DC_B_CLEARUP_DATA,event.top
         (*state).Cropindex=Event.index

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
           RETURN
         ENDIF
       DC_B_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Pro_DROPLIST'): BEGIN
		DC_B_CLEARUP_DATA,event.top
         (*state).ProID = (*state).ProIDList[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
           RETURN
         ENDIF
       DC_B_TakeSimpleDotData,event.top
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Year_DROPLIST'): BEGIN
        DC_B_CLEARUP_DATA,event.top
		Widget_Control,EVENT.ID,GET_VALUE= Datayear
        (*state).CalcYear=Datayear[Event.index]

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
           RETURN
         ENDIF
       DC_B_TakeSimpleDotData,event.top
   END
   ;------------���пռ��ֵ-----------------------------
   Widget_Info(wWidget, FIND_BY_UNAME='Next_Extrapolate_bu'): BEGIN   ;����"�ռ��ֵ"��ť�¼�

      Widget_Control,(*state).FloatYield_Table,GET_VALUE=FloatYield
;;      CropList=['','��С��','��С��' ,'�絾','�е�','��','����','��']
      CropList = STRCOMPRESS((*state).CropName,/REMOVE_ALL)
		IF (*state).Cropindex EQ 0 THEN BEGIN
		    Prompt=DIALOG_MESSAGE('����û��ѡ������!',TITLE='��ʾ',/INFORMATION)
		    RETURN
		ENDIF

		IF FloatYield[0,0] EQ '' THEN BEGIN       ;�õ�һ�������ж�.
		   Prompt=DIALOG_MESSAGE('û��"'+CropList[(*state).Cropindex]+'"վ���ջ�ָ��!',TITLE='����')
		ENDIF ELSE BEGIN
	        ;��������Ӣ������,ע��Ҫ��Ӧ����DROPLIST
;;			CropName=['','SpringWheat','WinterWheat','EarlyRice','SemiRice','LateRice','Maize','Soybean']
			CropName=['','SpringWheat','WinterWheat','EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			crop_id=CropName[(*state).Cropindex]    &   year=(*state).CalcYear

			SAVE_FILE=DIALOG_PICKFILE(TITLE='�����ֵͼ(��ò�Ҫ�Ķ��Զ����õ��ļ���)��',FILE=year+'HI'+crop_id  $
			           ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)
			IF SAVE_FILE EQ '' THEN RETURN

			(*state).Savefile = SAVE_FILE   ;ע���ļ�����û�д���չ����

			progressTimer = Obj_New("ShowProgress",tlb,MESSAGE='���ݴ�����,���Ժ�!',TITLE='�ռ��ֵ') ;�½�����������
			progressTimer->START
			progressTimer->UPDATE,(0.2 * 100.0)  ;���½�����

			;----���ɲ�ֵӰ���ļ�-------------------------------
	        ProviceName = (*state).province[WHERE((*state).proIDlist EQ (*state).proID)]
			ParaInfo = (*state).ProjectPara    ;�õ�ʡ��Χ�ڵ�ͶӰ����
	        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;�����صĲ��ǽṹ��,���ǿ�.
	        	PRMPT = DIALOG_MESSAGE('û��'+ProviceName+'�Ļ���������Ϣ,��鿴��Ӧ�Ĳ��������ļ�!',TITLE='����')
	        	RETURN
	        ENDIF

	        samples    = ParaInfo.samples		  & lines = ParaInfo.lines
	        ULX        = ParaInfo.UlX			  & ULY   = ParaInfo.UlY
	        Resolution = ParaInfo.resolution      & CenterMedian = ParaInfo.CenMeridian
			Lon_Lat = FLOAT(FloatYield[4:5,0:*])  & value = FLOAT(FloatYield[3,0:*])
			;�˴��Ĳ�ֵ����DC_CREATE_INTERP_GRID()���д��ڸĽ�
			InterpData = DC_CREATE_INTERP_GRID(Lon_Lat,value,ULX,ULY,Resolution,samples,lines,'TAVE')

			PaddyCrop=[3,4,5] ;��Ӧ��PaddyCrop=['EarlyRice','SemiRice','LateRice']
			IF WHERE(PaddyCrop EQ (*state).Cropindex) EQ -1 THEN BEGIN
				FarmlandFile='data_grid\farm_drought'                 ;�������ˮ��,���ú��صİٷֱ��ļ�.
			ENDIF ELSE BEGIN
				FarmlandFile='data_grid\farm_rice'
			ENDELSE

			FarmData = DC_Read_ENVIData(FarmlandFile,SUCCESSSTATUS = Status,DESCRIPTION='���ز�����ļ���')
			IF Status EQ 0 THEN RETURN

			ZeroValue=WHERE(FarmData EQ 0,COMPLEMENT=NoZero)         ;Ϊȡ��ʡ���ر߽������������
            (*state).AddMin = 300.0-MIN(InterpData[NoZero])
			InterpData[ZeroValue]=0                                     ;�����ر߽����ֵ��Ϊ0

			SavedImage = InterpData										;��ֵ��ȥ���Ǹ��غ󽫱����������

			InterpData[NoZero]   = InterpData[NoZero]+(*state).AddMin   ;�������ڵ�ֵ������300

	        WIDGET_CONTROL,(*state).Extrapolate_DRAW,SET_UVALUE = InterpData

			progressTimer->UPDATE, (0.75 * 100.0)  ;���½�����

	        DataType = SIZE(SavedImage,/TYPE) & ImageData = PTR_NEW(SavedImage,/NO_COPY)
			DC_SaveImageFile,SAVE_FILE,ImageData,samples,lines,DataType,'Unknown',ULX,ULY,Resolution,CenterMedian
	 		PTR_FREE,ImageData
			;-----------------------------------
			progressTimer->UPDATE, (0.95 * 100.0)  ;���½�����
;;SAVE_FILE='H:\temp\2006Maize'   ;������
			DC_Draw_image,SAVE_FILE,(*state).Extrapolate_DRAW,oView = oView,MINVALUE=299.99
			OBJ_DESTROY,(*state).oView
			(*state).oView = oView

			progressTimer->UPDATE, (1 * 100.0)  ;���½�����
;;			progressTimer->DESTROY ;���ٽ�����
			OBJ_DESTROY,progressTimer

			Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=1

		ENDELSE
   END

   Widget_Info(wWidget, FIND_BY_UNAME='Return_DRAW'): BEGIN

      Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0

   END

   Widget_Info(wWidget, FIND_BY_UNAME='ReturnY_DRAW'): BEGIN  ;�����ռ�ֲ��е�"����"

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
        Prompt=DIALOG_MESSAGE('����û��ѡ������!',TITLE='��ʾ',/INFORMATION)
        RETURN
     END

     Yield[0,0] EQ '' :BEGIN
        Prompt=DIALOG_MESSAGE('����û��"'+ProName+(*state).CalcYear+'��'+CropName+'"վ��HI����!',TITLE='����')
        RETURN
     END
	 ELSE:
	ENDCASE

      RowsNum=N_ELEMENTS(SimpleDotYield)/7
      Estimation_yield=STRARR(9,RowsNum+1)
      Estimation_yield[0:*,0]=['������','���','վ��','վ��','�������','������','HI','����','γ��']
      Estimation_yield[0:1,1:*]=[STRARR(1,RowsNum)+CropName,STRARR(1,RowsNum)+(*state).CalcYear]
	  Estimation_yield[2:*,1:*]=SimpleDotYield
	  DC_SaveTextData,Estimation_yield,EVENT.ID $
				   ,FILENAME=(*state).CalcYear+ProName+CropName+'վ��HI.txt'

   END

  Widget_Info(wWidget, FIND_BY_UNAME='SaveStatice_bu'): BEGIN

      Widget_Control,(*state).StaYield_TABLE,GET_VALUE=CountyYield,GET_UVALUE=CropYear

     IF CountyYield[0,0] EQ '' THEN BEGIN
        Prompt=DIALOG_MESSAGE('û�в�������,�����Ƚ���ͳ��!',TITLE='��ʾ',/INFORMATION)
        RETURN
     ENDIF

      Widget_control,/HOURGLASS
      year=CropYear.year   &   crop_id=CropYear.cropid
      CropName=STRCOMPRESS((*state).Cropname[WHERE((*state).CropIdList EQ crop_id[0])],/REMOVE_ALL)
	  ProName = (*state).province[WHERE((*state).ProIDlist EQ (*state).ProID)]
      CountyYield=[['�ش���','ģ�����','�ջ�ָ��','����','���'],[CountyYield]]
      DC_SaveTextData,CountyYield,EVENT.ID,FILENAME=year+'��'+ProName+CropName+'����������.txt'

   END

   Widget_Info(wWidget, FIND_BY_UNAME='Extrapolate_DRAW'): BEGIN  ;��ֵdraw����е��¼�

        IF NOT OBJ_VALID((*state).oView) THEN RETURN   ;����ǿն����򷵻�.

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
            ImageSize = SIZE(Imagedata,/DIMENSIONS)

		    (*state).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]  & ySize = ImageSize[1] ;Ӱ����
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;֮���԰��з�ת,����ΪDRAW��������Ǵ����½ǿ�ʼ��.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(dataValue-(*state).AddMin,2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

   Widget_Info(wWidget, FIND_BY_UNAME='Yield_DRAW'): BEGIN  ;����draw����е��¼�

        IF NOT OBJ_VALID((*state).YView) THEN RETURN   ;����ǿն����򷵻�.

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
            ImageSize = SIZE(Imagedata,/DIMENSIONS)

		    (*state).YView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]  & ySize = ImageSize[1] ;Ӱ����
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;֮���԰��з�ת,����ΪDRAW��������Ǵ����½ǿ�ʼ��.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(dataValue,2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

     Widget_Info(wWidget, FIND_BY_UNAME='FactorType'): BEGIN

		DC_B_CLEARUP_DATA,event.top

         IF ARRAY_EQUAL((*state).Cropindex,0) THEN BEGIN
;           Prompt=DIALOG_MESSAGE('����û��ѡ������,����ѡ��!',TITLE='��ʾ',/INFORMATION)
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

			Prompt=DIALOG_MESSAGE(['����û�пռ��ֵ����HIͼ,���Ȳ�ֵ!���Ҫͳ�����е�HIͼ,' $
			                      ,'�밴"��(Y)"ť,����"��(N)"�ص���һ���Ƚ��в�ֵ!'] $
			                      ,TITLE='ѯ��',/QUESTION)
			IF Prompt EQ 'No' THEN BEGIN
				Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=0
				RETURN
			ENDIF
			Filename=DIALOG_PICKFILE(TITLE='��ѡ�����е�HIͼ���ļ���', FILTER=['*.hdr'],PATH=DC_PathSetting(),/MUST_EXIST, DIALOG_PARENT=Event.id)

			IF Filename EQ '' THEN RETURN

			StatisMapFile=STRMID(STRTRIM(Filename,2),0,STRLEN(Filename)-4)    ;��Ϊȥ��".hdr"

			CropNameEnglish=['','SpringWheat','WinterWheat' ,'EarlyRice','SemiRice','LateRice','SpringMaize','SummerMaize','Soybean']
			Pos = STRPOS( StatisMapFile,'\',/REVERSE_SEARCH)
			E_crop=STRMID(StatisMapFile,Pos+7)                                ;Ӣ�ĵ�������
			aa = WHERE(CropNameEnglish EQ E_crop,Count)
			IF Count eq 0 THEN BEGIN
			   ;��ȷ�������ļ�:��(4λ)+HI+����Ӣ����,��"2006HISpringWheat"
			   Prompt=DIALOG_MESSAGE(['������ȷ������HIͼ���ļ�!' $
			   						 ,'��ȷ��������"2006HISpringWheat"'],TITLE='����')
			   RETURN
			ENDIF

			CalcYear=STRMID(StatisMapFile,Pos+1,4)
			Cropname = STRCOMPRESS((*state).CropName[aa],/REMOVE_ALL)

			(*state).TempCropindex = aa[0]
			(*state).TempCalcYear  = CalcYear

			HIfile=StatisMapFile

			InterpData = DC_Read_ENVIData(HIfile,SUCCESSSTATUS = Status,DESCRIPTION='��ѡ�ļ���')
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
         Widget_Control,(*state).prompt_TEXT ,SET_VALUE='��ʾ:��ǰѡ��ͳ�Ƶ���"'+ProName+'"'+CalcYear+'��'+Cropname+'���ջ�ָ���Ͳ���!'
      END

    	ProBiomassFile = DIALOG_PICKFILE(TITLE='��ѡ��"'+ProName+CalcYear+'��'+Cropname+'"�����������ռ������ļ�' $
    					   ,PATH=DC_PathSetting(),FILTER ='*.hdr;*.HDR',/MUST_EXIST,DIALOG_PARENT=EVENT.ID)

		IF ProBiomassFile EQ '' THEN  RETURN

		HIinfo = DC_ReadHead_file(HIfile)    ;�ջ�ָ���ļ�
		Binfo  = DC_ReadHead_file(ProBiomassFile)		;�������ļ�

		CASE 1 OF
			SIZE(HIinfo,/TYPE) NE 8: RETURN
			SIZE(Binfo,/TYPE) NE 8: RETURN
			FIX(HIinfo.samples) NE FIX(Binfo.samples) OR FIX(HIinfo.lines) NE FIX(Binfo.lines): BEGIN
				Info = DIALOG_MESSAGE('ѡ����������������������ɵ��ջ�ָ���ļ���С��һ��!',TITLE='����')
				RETURN
			END
		  ELSE:
		ENDCASE

 		DataHI = DC_Read_ENVIData(HIfile,SUCCESSSTATUS = HIstaus,DESCRIPTION='�ջ�ָ����')
 		DataB  = DC_Read_ENVIData(ProBiomassFile,SUCCESSSTATUS = Bstaus,DESCRIPTION='���������ݵ�')

		CASE 0 OF
			HIstaus: RETURN
			Bstaus : RETURN
		  ELSE:
		ENDCASE

    	YieldFile = DIALOG_PICKFILE(TITLE='����ռ��������',GET_PATH = PathNew $
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

;	YieldFile='H:\temp\2006Maize'   ;������
		DC_Draw_image,YieldFile,(*state).Yield_DRAW,oView=YView,MINVALUE=0.001
		OBJ_DESTROY,(*state).YView
		(*state).YView = YView

		Widget_Control,(*state).Widegt_TAB,SET_TAB_CURRENT=2
    END

   Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
    	if file_test('help\help.chm') then begin
    		ONLINE_HELP, BOOK='help\help.chm',/FULL_PATH,"'������-��������'"
    	endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
    end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,"'������-��������'"

   Widget_Info(wWidget, FIND_BY_UNAME='quit_bu'):begin
   	common_log,'�ر��ջ�ָ��������'
   	widget_control,Event.top,/DESTROY
   end
  ELSE:
  ENDCASE

END
;************�ռ����Ʋ�ֵ����*******************************
PRO DC_Bio_HI,GROUP_LEADER=groupleader

	common_log,'�����ջ�ָ��������'
   IF ( XREGISTERED('DC_Bio_HI') NE 0 ) THEN RETURN

  TLB = Widget_Base( GROUP_LEADER=groupleader, UNAME='TLB' ,XOFFSET=300 ,YOFFSET=200  $
      ,SCR_XSIZE=466 ,SCR_YSIZE=399 ,TITLE='�ջ�ָ��������',SPACE=1  $
      ,XPAD=0 ,YPAD=0,COLUMN=1 ,TLB_FRAME_ATTR=1,/TAB_MODE)
;-----------------------------------------------------------------------------------
  Widegt_TAB = Widget_Tab(TLB,UNAME='Widegt_TAB' ,/FRAME,SCR_XSIZE=450 ,SCR_YSIZE=335)
;----------****************-վ���ջ�ָ��------------------------------
   tab0 = Widget_Base(Widegt_TAB, UNAME='tab0'  $
		      ,SCR_YSIZE=308 ,TITLE='վ���ջ�ָ������' ,SPACE=1  $
		      ,XPAD=0 ,YPAD=0,/COLUMN,/BASE_ALIGN_LEFT)
		 ;--------------------------------------------
		 Crop_year = Widget_Base(tab0, UNAME='Crop_year' ,FRAME=1,SCR_XSIZE=450 $
		  		 ,SCR_YSIZE=25 ,SPACE=1,XPAD=0 ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)
			Province = ['������','�����','�ӱ�','ɽ��','���ɹ�','����','����','������','�Ϻ���','����' $
						,'�㽭','��΢','����','����','ɽ��','����','����','����','�㶫','����','����' $
						,'������','�Ĵ�','����','����','����','����','����','�ຣ','����','�½�']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			CropName = ['δѡ��','��С��','��С��','��  ��','��  ��','��  ��','������','������','��  ��']
			CropIDList = ['','11','12','21','22','23','31','32','41']				;CropName��CropIDListӦ��Ӧ
			ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;��������ݱ仯,ϵͳ���б��е����Ҳ��仯.

;			Pro_Droplist  = Widget_Droplist(Crop_year,UNAME='Pro_DROPLIST',TITLE='ʡ��:')
			Year_Droplist = Widget_Droplist(Crop_year,UNAME='Year_DROPLIST',TITLE='���:')

			SeperateLine1 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)

			FactorType = CW_BGROUP(Crop_year, UNAME='FactorType',['����','ң��'],/ROW,/NONEXCLUSIVE $
					,SPACE=0, LABEL_LEFT='����:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=18);,XSIZE=145)

			SeperateLine2 = Widget_Base(Crop_year,SCR_XSIZE=1 ,SCR_YSIZE=25,/FRAME)
			Crop_Droplist = Widget_Droplist(Crop_year,UNAME='Crop_DROPLIST',TITLE='����:')
		;----------------------------��----------------
		 FloatYield_Table = Widget_Table(tab0, UNAME='FloatYield_Table'$
		 			,SCR_XSIZE=450,SCR_YSIZE=250 ,XSIZE=6 ,YSIZE=20,/FRAME)
		;----------------------------------------------------------------------------------
		 Save_Extrapo_base = Widget_Base(tab0, UNAME='Save_Extrapo_base',SCR_XSIZE=450,/FRAME $
		     , SCR_YSIZE=27,SPACE=60 ,XPAD=38 ,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)
             Width_C = 80
			 SaveDotYield_bu = Widget_Button(Save_Extrapo_base, UNAME='SaveDotYield_bu'  $
			      ,SCR_XSIZE=Width_C,SCR_YSIZE=20 ,TOOLTIP='��վ���ջ�ָ�����浽���ش�����' ,VALUE='����')

			 Next_Extrapolate_bu = Widget_Button(Save_Extrapo_base, UNAME='Next_Extrapolate_bu'  $
			      ,SCR_XSIZE=Width_C,SCR_YSIZE=20 ,TOOLTIP='��һ�����пռ��ֵ' ,VALUE='��ֵ>>')

			 Help_bu = Widget_Button(Save_Extrapo_base, UNAME='Help_bu'  $
			      ,SCR_XSIZE=Width_C,SCR_YSIZE=20  ,VALUE='����')
 ;------*****************-�ռ����Ʋ�ֵ--------------------------------------
  tab1 = Widget_Base(Widegt_TAB, UNAME='tab1' ,FRAME=1  $
      ,SCR_XSIZE=450 ,SCR_YSIZE=310 ,TITLE='վ��HI�ռ�����' ,SPACE=0  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER)

		  Extrapolate_DRAW = Widget_Draw(tab1, UNAME='Extrapolate_DRAW' ,RETAIN=2  $
		     ,SCR_XSIZE=440 ,SCR_YSIZE=275 ,GRAPHICS_LEVEL=2 $
		     ,/BUTTON_EVENTS,/MOTION_EVENTS)

;;		 Cusor_LABEL = Widget_Label(tab1,UNAME='Cusor_LABEL' $
;;		      ,SCR_XSIZE=266 ,SCR_YSIZE=12 ,/ALIGN_CENTER,VALUE='ָ����ָ��������ֵ��')

		  Button_BASE = Widget_Base(tab1, UNAME='Button_BASE'   $
		      ,SCR_XSIZE=440 ,SCR_YSIZE=31,SPACE=70 ,XPAD=60 ,YPAD=0  $
		      ,/ROW,FRAME=0,/BASE_ALIGN_TOP)

		  Return_DRAW = Widget_Button(Button_BASE, UNAME='Return_DRAW'  $
		      ,SCR_XSIZE=119 ,SCR_YSIZE=20,/ALIGN_CENTER ,VALUE='<<����')

		  CalcYield_bu = Widget_Button(Button_BASE,  $
		      UNAME='CalcYield_bu' ,SENSITIVE=1,SCR_XSIZE=119 ,SCR_YSIZE=20 $
		     ,/ALIGN_CENTER ,VALUE='��������>>')
 ;------*****************-�����ռ�ֲ�--------------------------------------
  tab3 = Widget_Base(Widegt_TAB, UNAME='tab3' ,FRAME=1  $
      ,SCR_XSIZE=450 ,SCR_YSIZE=310 ,TITLE='�����ռ�ֲ�' ,SPACE=0  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_CENTER)

		  Yield_DRAW = Widget_Draw(tab3, UNAME='Yield_DRAW' ,RETAIN=2  $
		     ,SCR_XSIZE=440 ,SCR_YSIZE=275 ,GRAPHICS_LEVEL=2 $
		     ,/BUTTON_EVENTS,/MOTION_EVENTS)

		  Bu_BASE = Widget_Base(tab3, UNAME='Bu_BASE'   $
		      ,SCR_XSIZE=440 ,SCR_YSIZE=31,SPACE=70 ,XPAD=60 ,YPAD=0  $
		      ,/ROW,FRAME=0,/BASE_ALIGN_TOP)

		  ReturnY_DRAW = Widget_Button(Bu_BASE, UNAME='ReturnY_DRAW'  $
		      ,SCR_XSIZE=119 ,SCR_YSIZE=20,/ALIGN_CENTER ,VALUE='<<����')

		  Next_StaResult_bu = Widget_Button(Bu_BASE,  $
		      UNAME='Next_StaResult_bu' ,SENSITIVE=1,SCR_XSIZE=119 ,SCR_YSIZE=20 $
		     ,/ALIGN_CENTER ,VALUE='ͳ��>>',EVENT_PRO='DC_B_StatisticEV' $
		     ,TOOLTIP='���ݲ�ֵ���ͳ�Ƹ����ջ�ָ��')
;-----*******************-����ͳ��*************-----------------
  tab2 = Widget_Base(Widegt_TAB, UNAME='tab2' $
      ,SCR_XSIZE=450 ,SCR_YSIZE=310 ,TITLE='�ջ�ָ��ͳ�Ƽ���������' ,SPACE=1  $
      ,XPAD=0 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_LEFT,/FRAME)

		  StaYield_LABEL = Widget_Label(tab2, UNAME='StaYield_LABEL'  $
		      ,XOFFSET=46 ,YOFFSET=3 ,SCR_XSIZE=346 ,SCR_YSIZE=15  $
		      ,/ALIGN_CENTER ,VALUE='�ջ�ָ��������ĵ������')

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
		      ,/ALIGN_CENTER ,VALUE='<<����',TOOLTIP='���ز鿴�ռ��ֵͼ')

		  InputDB_yield = Widget_Button(Button_BASE_0, UNAME='InputDB_yield'  $
		      ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight,/ALIGN_CENTER ,VALUE='���' $
		      ,TOOLTIP='������Ĳ������浽���ݿ���',EVENT_PRO='DC_B_InputDB_EV')

		  SaveStatice_bu = Widget_Button(Button_BASE_0, UNAME='SaveStatice_bu' ,XOFFSET=239  $
		      ,YOFFSET=4 ,SCR_XSIZE=BuWidth ,SCR_YSIZE=BuHeight ,/ALIGN_CENTER  $
		      ,VALUE='����',TOOLTIP='������Ĳ������浽���ش�����')

  ;-----**************�������������ʾ�ı�************-------------
  Prompt_BASE = Widget_Base(TLB,SCR_XSIZE=450 ,SCR_YSIZE=35 ,SPACE=3,XPAD=1  $
      ,YPAD=0 ,ROW=1,/BASE_ALIGN_TOP)

	  prompt_TEXT = Widget_Text(Prompt_BASE, UNAME='prompt_TEXT' ,FRAME=1  $
	      ,SCR_XSIZE=390 ,SCR_YSIZE=25 ,XSIZE=20  $
	      ,YSIZE=1,VALUE='��ʾ��')
	  quit_bu = Widget_Button(Prompt_BASE, UNAME='quit_bu' ,XOFFSET=348  $
	      ,YOFFSET=5 ,SCR_XSIZE=55 ,SCR_YSIZE=23  $
	      ,TOOLTIP='�˳��ռ��ֵģ��' ,VALUE='�ر�')
;---------------------------------------------------------------------------

		PRO_COMMON_DC
		COMMON DC_BLOCK,NewCropID
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

		ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�
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

		ProjectPara = DC_ReadParameter(Province[WHERE(ProIDList EQ ProCode)])    ;�õ�ʡ��Χ�ڵ�ͶӰ����,û���򷵻�Ϊ��,�����ǽṹ��.
		state={  Cropindex             :  0 				,$
			     CropIDList			   :  CropIDList		,$	 ;����ID�б�
			     CropName			   :  CropName			,$   ;�������б�
;			     DataType			   :  0					,$   ;��������,Ĭ��Ϊ�������ݼ���Ĳ�������
			     CalcYear              :  CalcYear	 		,$
       		     ProID				   :  ProCode			,$	  ;��ѡʡID,2λ�ַ���
			     ProIDList			   :  ProIDList			,$	  ;ʡID�б�
			     Province			   :  Province			,$    ;ʡ���б�
			     FloatYield_Table  	   :  FloatYield_Table  ,$
			     StaYield_TABLE        :  StaYield_TABLE  	,$
			     StaYield_LABEL        :  StaYield_LABEL 	,$
			     prompt_TEXT           :  prompt_TEXT 		,$
			     Widegt_TAB			   :  Widegt_TAB		,$
			     Extrapolate_DRAW      :  Extrapolate_DRAW  ,$
			     Yield_DRAW     	   :  Yield_DRAW 		,$
			     TempCropindex		   :  0					,$   ;�ò������ڼ�¼ʹ���������ݽ��в������ջ�ָ��ͳ��ʱ������ID
			     TempCalcYear		   :  ''				,$   ;�ò������ڼ�¼ʹ���������ݽ��в������ջ�ָ��ͳ��ʱ�����
			     oView				   :  OBJ_NEW()			,$		;HI����ͼ����
			     YView				   :  OBJ_NEW()			,$		;Yiled����ͼ����
			     AddMin				   :  0.0				,$      ;�������ڵ�HI��ֵ�������ʶ��Ĳ���
			     ProjectPara		   :  ProjectPara		,$		;���ڶ���ʡ��Ӧ��Χ�ڵ�ͶӰ����ֵ(��"text\parametersetting.txt")
			     Savefile              :  ''				,$		;��������ջ�ָ���ļ�.
			     SaveYieldfile         :  ''				,$		;������Ŀռ�ֲ������ļ�.
			     StationYieldID		   :  '4'}						;���ڱ�ʶ�����վ���������,2,վ��ũ��ģ�͹������,4Ϊң�еĹ������

	Widget_Control,TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)

	XManager, 'DC_Bio_HI', TLB,CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END