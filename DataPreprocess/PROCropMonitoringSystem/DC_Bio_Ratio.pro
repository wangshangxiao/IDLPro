
;***********�������********************************************
PRO DC_B_ClearUp,EventTop

   Widget_Control,EventTop,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield & Yield[*,*]=''
     Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1],COLUMN_LABELS='',ROW_LABELS=''
     Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='ģ��Ĳ���'
;;     IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
;;        DisplayDraw = Widget_Info(EventTop,FIND_BY_UNAME='DisplayDraw')
;;        Widget_Control,DisplayDraw,SET_BUTTON = 0
;;        (*state).DisplayYesOrNo = 0
;;        Widget_Control,(*state).Draw_shape,GET_VALUE=Owindow
;;        Owindow->ERASE,COLOR=255
;;     ENDIF

END
;**************��ȡ���꼰�����������,����������ﵥ��������****************************
PRO DC_TakeBiomassEV,EVENT

    Widget_Control,Event.top,GET_UVALUE=state
    Widget_Control,(*state).Yield_TABLE,SET_TABLE_SELECT=[-1,-1]
    WIDGET_CONTROL, /HOURGLASS

    Cropid   = (*state).CropID
    CalcYear = (*state).CalcYear
    LastYear = STRTRIM(FIX(CalcYear)-1,2)		;��һ���
;;    CropBioName = ['spring_wheat','winter_wheat','early_rice','semilate_rice','late_rice','maize','soybean']
    CropBioName = ['spring_wheat','winter_wheat','early_rice','semilate_rice','late_rice','spring_maize','summer_maize','soybean']
    BioTableCrop = CropBioName[WHERE((*state).CropIDlist EQ Cropid)]
    CropName=STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDlist EQ Cropid)],/REMOVE_ALL)

	ProName = (*state).PronameList[WHERE((*state).ProIDList EQ (*state).ProID)]

    ColumnName=['����',LastYear+'�굥��',LastYear+'��������',CalcYear+'��������',CalcYear+'�굥��']

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
       Prompt=DIALOG_MESSAGE('���ݿ���'+ProName+'���ػ����������������굥����ǰ��������֮һȱ������,��鿴���ݿ�!',title='����')
       RETURN
    ENDIF

	EstiYeild = STRTRIM(FLOAT(EstimationYield[4,*])/FLOAT(EstimationYield[3,*])*FLOAT(EstimationYield[2,*]),2)


    Widget_Control,(*state).Yield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=RowsNum,ALIGNMENT=2  $
                  ,SET_VALUE=[EstimationYield[1:4,*],EstiYeild],SET_UVALUE= EstimationYield[0,*] $  ;������û�ֵ���ڴ��ݱ�ͷ
                  ,ROW_LABELS=EstimationYield[0,*],COLUMN_WIDTHS=78 $
                  ,COLUMN_LABELS=ColumnName

    Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='��ֵ��ģ���'+ProName[0]+CalcYear+'��'+CropName[0]+'���ص���'

;    IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
;
;       Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
;       DistrictCode=Yield[0,*]                            ;��һ��ֵΪ�����������
;
;       Draw_shape = (*state).Draw_shape
;
;       ShapeFileName = (*state).ShapeFileName
;
;       Map_para=DC_DrawShapeMape(ShapeFileName,DistrictCode,Draw_shape)
;
;       Widget_Control,Draw_shape,SET_UVALUE=Map_para       ;����ز�����ΪDRAW���û�ֵ.
;    ENDIF
;------------------------------------------------------------------------------------

END
;************************************************************************************
PRO DC_Bio_Ratio_event,Event
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

		progressTimer = Obj_New("ShowProgress",TLB,MESSAGE='���ڽ�������������뵽����,���Ժ�...' $
								,TITLE='���㵥�����')
		progressTimer->START                         ;����������

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

	 INFO = DIALOG_MESSAGE('����Ҫ���ع��������Ȩ��ʡ��?',/QUESTION,title='ѯ��')
	 IF INFO EQ 'No' THEN RETURN

	 CountyYield = CountyYield[[0,4],*]				;2��ֵ,����\�������
	 Status = DC_WeightToPro(CountyYield,Cropid,CalcYear,(*state).ProID,'5')
	 IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('��Ȩ����ɹ�!',/INFORMATION,title='��ʾ')

    END

    Widget_Info(wWidget, FIND_BY_UNAME='Save_bu'): BEGIN
		WIDGET_CONTROL,(*state).Yield_TABLE,GET_VALUE=CountyYield,GET_UVALUE=code
		IF ARRAY_EQUAL(CountyYield,'') THEN RETURN

	    Cropid   = (*state).CropID
	    CalcYear = (*state).CalcYear
	    LastYear = STRTRIM(FIX(CalcYear)-1,2)		;��һ���
	    CropName=STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDlist EQ Cropid)],/REMOVE_ALL)

		ProName = (*state).PronameList[WHERE((*state).ProIDList EQ (*state).ProID)]

	    ColumnName=['����','����',LastYear+'�굥��',LastYear+'��������',CalcYear+'��������',CalcYear+'�굥��','����']

		RowNum = N_ELEMENTS(CountyYield)/5
		Save_Data = [[ColumnName],[code,CountyYield,STRARR(1,RowNum)+CropName[0]]]
		Filename = '��ֵ�������'+ProName+CalcYear+'����'+CropName+'����.txt'
		DC_SaveTextData,Save_Data,EVENT.ID,FILENAME=Filename,/NOSavePath

    END

    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, "'������-��������'", BOOK='HELP\HELP.chm', /FULL_PATH	;
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,"'������-��������'"
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'�رռ򵥱�ֵ������'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END
;***********�����ʾ���ģ��"����************************
PRO DC_Bio_Ratio,GROUP_LEADER=groupleader

	common_log,'�����򵥱�ֵ������'
   IF ( XREGISTERED('DC_Bio_Ratio') NE 0 ) THEN RETURN  ;����ô����ѵ���,���ظ�����.

   TLB = Widget_Base(GROUP_LEADER=groupleader,UNAME='TLB'   $
      ,XOFFSET=320 ,YOFFSET=200,SCR_XSIZE=500,SCR_YSIZE=386 ,/TAB_MODE  $
      ,TITLE='��ֵ�����㵥��' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=2 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1)

   ;------------���BASE------------------------------------;
   bWIDTH = 485
   SynthesisOutputBase = Widget_Base(TLB,UNAME='SynthesisOutputBase'   $
      ,SPACE=1 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)
   ;------------------����BASE���ĸ�DROPlist���ڵ�BASE-----------------
	  ConditionB = Widget_Base(SynthesisOutputBase,UNAME='ConditionB' ,FRAME=1 $
	      ,SCR_XSIZE=bWIDTH,SCR_YSIZE=32,SPACE=25,XPAD=1,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

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
		YearDroplist = Widget_Droplist(ConditionB,UNAME='YearDroplist',TITLE='�������:',SCR_XSIZE=D_WIDTH+15)

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=66 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='����',EVENT_PRO='DC_TakeBiomassEV')

;----------------�����м�ı�񲿷�---------------------------------------
  YieldTable_BASE = Widget_Base(SynthesisOutputBase,SCR_XSIZE=bWIDTH , $
      UNAME='YieldTable_BASE' ,FRAME=1,SPACE=1 ,XPAD=0,YPAD=1 $
      ,SCR_YSIZE=275,/COLUMN,/BASE_ALIGN_LEFT)

	  wholeYield_LABEL = Widget_Label(YieldTable_BASE,  $
	      UNAME='wholeYield_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=228  $
	      ,SCR_YSIZE=18 ,/ALIGN_CENTER ,VALUE='ģ��Ĳ���')

	  Yield_TABLE = Widget_Table(YieldTable_BASE, UNAME='Yield_TABLE' $,/ALL_EVENTS $
	      ,SCR_XSIZE=bWIDTH ,SCR_YSIZE=250 ,XSIZE=7,YSIZE=19,/FRAME,COLUMN_WIDTHS=75 $
	      ,/DISJOINT_SELECTION,/RESIZEABLE_COLUMNS);,EVENT_PRO='DC_TableToMapEV')
;---------------------------�����еİ�ť����-----------------------
  button_BASE = Widget_Base(SynthesisOutputBase, UNAME='button_BASE'  $
      ,FRAME=1 ,SCR_XSIZE=bWIDTH,SCR_YSIZE=34  $
      ,SPACE=40 ,XPAD=16,YPAD=1 ,ROW=1)
 ;-------------------------�Ƿ���ʾʸ��ͼ-------------
;	  Draw_BASE= Widget_Base(button_BASE, UNAME='Draw_BASE'   $
;	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,XPAD=0 ,YPAD=0 ,ROW=1,/NONEXCLUSIVE $
;	     ,/BASE_ALIGN_TOP)
;		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
;		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='��ʾ�ռ�ͼ' $
;		      ,EVENT_PRO='DC_DisplayShapeEV')
  ;-----------------------------------------------------------------
  	  Buwith = 80
	  ToDB_bu = Widget_Button(button_BASE, UNAME='ToDB_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='������Ĳ������浽���ݿ���' ,VALUE='���')

	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='����������ݱ��浽���ش���' ,VALUE='����')

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='����������ݱ��浽���ش���' ,VALUE='����')

	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=Buwith ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='�ر�')

;---------------------�ұߵ�DRAW_base����---------------------------
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
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�
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
  			ProNameList			:	Province		,$				;ʡ���б�
		    ProIDList			:	ProIDList		,$				;ʡID�б�
		    ProID				:	ProCode			,$				;��ѡʡID
		    CropIDList			:	CropIDList		,$				;����ID�б�
		    CropNameList		:	Crop			,$      		;�������б�
		    CropID				:	CropID			,$
		    CalcYear			:	CalcYear		,$
			ARRAY_YEAR	 		:	ARRAY_YEAR		,$
         	wholeYield_LABEL  : wholeYield_LABEL 	,$
         	Yield_TABLE       : Yield_TABLE		 	}
;         	Draw_shape        : Draw_shape		 	,$
;         	DisplayYesOrNo    : 0 				 	,$       ;�Ƿ���ʾ�˲�ѯͼ:1--��ʾ;0--û����ʾ.
;         	ShapeFileName     : 'data_vector\county.shp'}

 Widget_Control,TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)

  XManager, 'DC_Bio_Ratio',TLB,CLEANUP='DC_CleanAllHeap', /NO_BLOCK

END