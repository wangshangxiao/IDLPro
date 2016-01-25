;�����ں�ģ��
;********�Զ������:���������***********************************
PRO DC_ClearTableData,TLB

    Widget_Control,TLB,GET_UVALUE=PA

    Widget_Control,(*PA).ResultYield_LA,SET_VALUE='���յ�����'
    Widget_Control,(*PA).ResultTABLE,GET_VALUE=Yield & Yield[*,*]=''
    Widget_Control,(*PA).ResultTABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1] $
    			  ,COLUMN_LABELS='',ROW_LABELS=''

    Widget_Control,(*PA).EstiYield_LA,SET_VALUE='����ģ�����'
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
;********�Զ��庯��:���ɷַ������㺯��***********************************
FUNCTION DC_PrincipalComponentAnaysis,Data_ $ 					;ע��Data��������ʾ��������
									 ,EvectorNum = EvectorNum   ;ָ��Ҫ�������ɷ��غɵ�����ֵ����.Ĭ��Ϊǰ2��
   ;������ʽ:Result = DC_PrincipalComponentAnaysis(Data,[EvectorNum = EvectorNum])
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2

	Data = FLOAT(Data_)
	DataNum=SIZE(Data,/N_ELEMENTS)
	DataSize = SIZE(Data,/DIMENSIONS)
	M = DataSize[0]      ;������,������
	N = DataNum/M   	 ;�걾��,������
	IF KEYWORD_SET(EvectorNum) THEN BEGIN
	   IF EvectorNum GT M THEN Num = M ELSE Num=EvectorNum
	ENDIF ELSE Num = 2   ;Ĭ�ϼ���ǰ�������ɷ��غ�

;;1,�������ݱ�׼������(���б�׼��)����ʽΪ"��׼ֵ = (�и�ֵ-�о�ֵ)/�б�׼��"
	FOR i=0,M-1 DO BEGIN
		Temp = Data[i,*]
		Ave  = MEAN(Temp)				 	 ;��ֵ
		Sde  = SQRT(TOTAL((Temp-Ave)^2)/N)   ;��׼�����û���ú���Variance()),��Ϊ����N-1

		if(Sde ne 0) then begin
			Data[i,*]=(Temp-Ave)/Sde			 ;��׼ֵ
		endif else begin
			Data[i,*]=0
		endelse

	ENDFOR
	print,Data
;;2,���׼�����ݵ����ϵ������
	Cor_Coe = CORRELATE(Data,/double)
;;3,��������ֵ,������������|��I-R|=0ʽ�Ц�Ϊ����ֵ.
	Eval = HQR(ELMHES(Cor_Coe))			     ;��ʱ������ֵ�Խ�������,�����Ը�����ʽ����

;;	H = LA_ELMHES(Cor_Coe,q,PERMUTE_RESULT=permute,SCALE_RESULT=scale) ;Create Hessenberg upper Tragular array
;;   Eval = LA_HQR(H,q,PERMUTE_RESULT = permute)
;;4,�����Ӧ������ֵ�˵���������e
	Evec = EIGENVEC(Cor_Coe,Eval) 	   ;��������ֵ��Ӧ����������

;;	Select = [1,1,1,REPLICATE(0,M-3)]  ;Ϊ1��ʾ�����Ӧ����Ӧ����ֵ����������,һ�����ͷ3��
;;	Evec = LA_EIGENVEC(H,q,EIGENINDEX = eigenindex,PERMUTE_RESULT = permute, $
;;					   SCALE_RESULT = scale,SELECT = Select)

;;	print,'%%%',Eval,'%%%%%',Evec

;;5,������ֵ�������ɷֹ����ʺ��ۼƹ�����
	Evalue = REAL_PART(Eval)    ;ע��õ�������ֵ�������������Ǹ�����ʽ,���鲿��Ϊ0,ʵ����Ϊ����Ҫ��ֵ
	Cum_Contribute = FLTARR(M) & Cum=0.0
	FOR i=0,M-1 DO BEGIN        ;MӦ�õ���N_ELEMENTS(Evalue)
		Cum +=Evalue[i]
		Cum_Contribute[i] = Cum/TOTAL(Evalue)
	ENDFOR
	Contribute = Evalue/TOTAL(Evalue)*100.0     ;������%
	Cum_Contribute = Cum_Contribute*100.0	    ;�ۼƹ�����%

;;	print,'Contribute%%%',Contribute,'Cum_Contribute%%%%%',Cum_Contribute

;;6�������ɷ��غɦ�=SQRT(��)e, ��ǰ��������ֵ��Ӧ�����ɷ�,�������غ�
	Evalue2 = Evalue[0:Num-1]			   ;ȡǰ��������ֵ
	Evector2 = REAL_PART(Evec[*,0:Num-1])  ;ȡ���϶��µļ���.��Ϊ��Ӧ��ǰ��������ֵ����������
	LoadVal = Evector2					   ;��ʼ��LoadVal
	FOR i=0,Num-1 DO LoadVal[*,i] = SQRT(Evalue2[i])*Evector2[*,i]   ;���ɷ��غ�

;;	print,'LoadVal%%%',LoadVal
;;7����ǰ2�������ɷֵĵ÷��Լ��ۺϵ÷�
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
		PcaValSum[i,*] = Contribute[i]*PCAScore[i,*]/100.0		  ;��ʱ�ù�����С����ʽ,�ʳ�100.0
	ENDFOR
	Order = TOTAL(PcaValSum,1)									  ;TOTAL(PcaValSum,1),����������,�����������
	Order[SORT(Order)]=REVERSE(INDGEN(N)+1)						  ;���ۺϵ÷ְ�����˳������
	PCAScore = [PCAScore,TRANSPOSE([[TOTAL(PcaValSum,1)],[Order]])]   ;�����PCAScore������Num+2��,�����Ϊ�ۺϵ÷ּ�������

;;	print,'PCAScore%%%',PCAScore

	 RETURN,{Cor_Coe        :	Cor_Coe,					$	;���ϵ������
	 		 Evalue			:	TRANSPOSE(Evalue),			$	;����ֵ,ת�ó���,��FLOAT��,��ͬ
	 		 Contribute		:	TRANSPOSE(Contribute),		$	;������
	 		 Cum_Contribute :	TRANSPOSE(Cum_Contribute),	$	;�ۻ�������
	 		 LoadVal		:	TRANSPOSE(LoadVal),			$	;ǰ�������ɷֵ��غ�(���±������̵�ϵ��)
	 		 PCAScore		:	PCAScore					$	;ǰ�������ɷ�,�ۺϵ÷ֺ�����(��Ӧ�������)
	 		 }
END
;**********���ɷַ����¼�************************************
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
    	INFO = DIALOG_MESSAGE('������ȡ��"����ģ�����"��',/INFORMATION,title='��ʾ')
    	WIDGET_CONTROL,EVENT.ID,SET_BUTTON=0
    	RETURN
    ENDIF

    Cropid   = (*PA).CropID
    CalcYear = (*PA).CalcYear
    CropName = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDlist EQ Cropid)],/REMOVE_ALL)
	ProName  = (*PA).PronameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]

	CountyName = Yield[0,*]            	;Yield��һ��������
	Yield = FLOAT(Yield[1:*,*])
	DataSize = SIZE(Yield,/DIMENSIONS)

	IF DataSize[0] LT 2 THEN BEGIN      ;DataSize[0]Ϊ������,������,Ӧ����N_ELEMENTS((*PA).EstiTypeID)
       INFO = DIALOG_MESSAGE('ֻ��һ������,���ܽ������ɷַ���!',title='��ʾ')
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
	Rows = DataSize[1]		;�����ǽ���һ���ɷ����غ�ֵ�������Ӧ�Ĺ������������Ϊ���ղ���
	Widget_Control,(*PA).ResultTABLE,USE_TABLE_SELECT=[INTARR(1,Rows)+1,INDGEN(1,Rows)] $
				  ,SET_VALUE=STRTRIM(Yield[MaxId,*],2),FOREGROUND_COLOR=[0,0,255]


END
;**************��ȡ���չ����������************************************
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

    EstiCounty = DC_GetdataFromDB_Str(2,Sqlstr,N_RECORDS = CountyNum)  ;�õ��ѹ���:����/����
    IF CountyNum EQ 0 THEN BEGIN
       Prompt=DIALOG_MESSAGE('����û�н���'+ProName+CalcYear+'��'+CropName+'���ص��κι������,����ģ�����!',title='����')
       RETURN
    ENDIF

    Sqlstr="select distinct Model_type_id from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(County_Code,2)='+ (*PA).ProID
    EstiTypeID = DC_GetdataFromDB_Str(1,Sqlstr,N_RECORDS = TypeNum)   ;�õ�����Ĳ�������


	ALLEstiYield = STRARR(1,CountyNum)

	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='����������ȡ��,���Ժ�...' $
							,TITLE='��ȡ���㵥��')
	progressTimer->START                         ;����������
	TOL = Long(CountyNum)*TypeNum & NN=1

	FOR i=0,TypeNum-1 DO BEGIN
		Temp = STRARR(1,CountyNum)

		FOR j=0,CountyNum-1 DO BEGIN
  	      progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;����������
  	      NN+=1

		    Sqlstr="select yield from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
		    		+"' and year="+CalcYear+' and Model_type_id='+EstiTypeID[i] $
		    		+" and County_Code='"+ EstiCounty[0,j]+"'"
			Temp[0,j] = (DC_GetdataFromDB_Str(1,Sqlstr))[0,0]
		ENDFOR
		ALLEstiYield = [ALLEstiYield,Temp]
	ENDFOR

	ALLEstiYield = ALLEstiYield[1:*,*]     ;������


;===��������ӣ�����ʡ�������ݵ��ںϣ�20070908=====================================

;	Sqlstr_p="select distinct Province_Code from PROVINCE_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(Province_Code,2)=' $
;    		+ (*PA).ProID+' order by Province_Code'
;    Sqlstr_p='select Code,NAME from PROVINCE_CODE a,('+Sqlstr_p	+') b where a.code=b.Province_Code'
;
;    EstiCounty_p = DC_GetdataFromDB_Str(2,Sqlstr_p,N_RECORDS = CountyNum_p)  ;�õ��ѹ���:ʡ��/ʡ��
;    IF CountyNum_p EQ 0 THEN BEGIN
;       Prompt=DIALOG_MESSAGE('����û�н���'+ProName+CalcYear+'��'+CropName+'���κι������,����ģ�����!',title='����')
;       RETURN
;    ENDIF
;
;    Sqlstr_p="select distinct Model_type_id from PROVINCE_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;    		+"' and year="+CalcYear+' and Model_type_id between 1 and 6 and LEFT(Province_Code,2)='+ (*PA).ProID
;    EstiTypeID_p = DC_GetdataFromDB_Str(1,Sqlstr_p,N_RECORDS = TypeNum_p)   ;�õ�����Ĳ�������
;
;
;	ALLEstiYield_p = STRARR(1,CountyNum_p)
;
;;	progressTimer = Obj_New("ShowProgress",TLB,/CANCELBUTTON,MESSAGE='����������ȡ��,���Ժ�...' $
;;							,TITLE='��ȡ���㵥��')
;;	progressTimer->START                         ;����������
;;	TOL = Long(CountyNum)*TypeNum & NN=1
;
;	FOR i=0,TypeNum_p-1 DO BEGIN
;		Temp_p = STRARR(1,CountyNum_p)
;
;		FOR j=0,CountyNum_p-1 DO BEGIN
;;  	      progressTimer->UPDATE, (1.0*NN/TOL * 100.0)  ;����������
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
;	ALLEstiYield_p = ALLEstiYield_p[1:*,*]     ;������
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
   	  BlankIndex[0,*] = BlankIndex[0,*]+1   ;����ֵԪ�ص����±�ֵ������1
   ENDIF ELSE BlankIndex = [-1,-1]			;ԭ�����ڹ��������ʾʱ,�����һ��.

	FOR I=0,CountyNum-1 DO BEGIN     ;��һ���ǽ��ղ���ֵ�������еķǿվ�ֵ������
		TEMP = ALLEstiYield[*,I]
		BlankID = WHERE(TEMP EQ '',COUNT,COMPLEMENT=NoBlankID)
		IF COUNT NE 0 THEN BEGIN
			TEMP[BlankID] = MEAN(FLOAT(TEMP[NoBlankID]))
			ALLEstiYield[*,I] = STRTRIM(TEMP,2)
		ENDIF
	ENDFOR
  ;VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
;    progressTimer->DESTROY ;���ٽ�����
	OBJ_DESTROY,progressTimer

	ColHead = STRARR(1)
	FOR I=0,TypeNum-1 DO BEGIN
		ColHead = [ColHead,'Yield'+EstiTypeID[0,I]]
	ENDFOR
	ColHead = ColHead[1:*]

    ColumnName=['����',ColHead]

    Widget_Control,(*PA).EstiYield_LA,SET_VALUE='�����'+ProName[0]+CalcYear+'��'+CropName[0]+'���ض��ֵ���'
    Widget_Control,(*PA).EstiTABLE,TABLE_XSIZE=TypeNum+1,TABLE_YSIZE=CountyNum,ALIGNMENT=2  $
                  ,SET_VALUE=[EstiCounty[1,*],ALLEstiYield],SET_UVALUE= EstiCounty[1,*] $         ;������û�ֵ���ڴ�������
                  ,ROW_LABELS=EstiCounty[0,*],COLUMN_WIDTHS=60 $
                  ,COLUMN_LABELS=ColumnName

    Widget_Control,(*PA).ResultYield_LA,SET_VALUE=ProName[0]+CalcYear+'��'+CropName[0]+'�������չ��㵥��'
    Widget_Control,(*PA).ResultTABLE,TABLE_XSIZE=2,TABLE_YSIZE=CountyNum,ALIGNMENT=1  $
                  ,SET_VALUE=[EstiCounty[1,*],STRARR(1,CountyNum)],SET_UVALUE= EstiCounty[0,*] $  ;������û�ֵ���ڴ�������
                  ,ROW_LABELS=EstiCounty[0,*],COLUMN_WIDTHS=110 $
                  ,COLUMN_LABELS=['����','���յ���']

	WIDGET_CONTROL,(*PA).EstiTABLE,USE_TABLE_SELECT=BlankIndex $   ;���þ�ֵ����ĵ�Ԫ���Ժ�ɫ��ʾ.
				  ,FOREGROUND_COLOR  = [255,0,0]

	WIDGET_CONTROL,(*PA).EstiTABLE,USE_TABLE_SELECT=[INDGEN(1,TypeNum)+1,INTARR(1,TypeNum)] $
				  ,BACKGROUND_COLOR  = [0,255,0],GET_VALUE=AnalysisData

	;-----------��ͼ-------------------------
	WIDGET_CONTROL,(*PA).DrawWid,GET_VALUE = drawID

	DC_Draw_BAR_PLOT,drawID,AnalysisData,ColHead

	PTR_FREE,(*PA).EstiTypeID,(*PA).DisData
	(*PA).EstiTypeID = PTR_NEW(ColHead,/NO_COPY)
	(*PA).DisData = PTR_NEW(AnalysisData,/NO_COPY)
	(*PA).CountyName = EstiCounty[1,0]

END
;**********��������¼�**************************************************
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

;***********��������¼�**************************************************
PRO DC_ResultTableEV,EVENT

	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	View = Widget_Info(EVENT.ID,/TABLE_VIEW)
	WIDGET_CONTROL,(*PA).EstiTABLE,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1]

END
;***********������Ĳ����¼�******************************************************
PRO DC_YieldMerge_event,Event
   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_PA.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
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

        IF (*PA).CountyName EQ '' THEN RETURN   ;����ǿ��򷵻�.

		IF TAG_NAMES(EVENT,/STRUCTURE_NAME) EQ 'WIDGET_TRACKING' THEN BEGIN
			IF (EVENT.ENTER EQ 1) THEN BEGIN
			    ProText='"'+(*PA).CountyName+'"'+(*PA).CalcYear+'������㵥���Ƚ�ͼ'
				WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
			ENDIF ELSE BEGIN
				WIDGET_CONTROL,EVENT.ID,TOOLTIP=''
			ENDELSE
		ENDIF ELSE BEGIN   ;������¼�(ֻȡ�������)
			IF (Event.CLICKS EQ 2) THEN BEGIN
		       IF ( XREGISTERED('DC_YieldDescription') NE 0 ) THEN BEGIN  ;������0��˵���������������ѵ���.
		         WIDGET_CONTROL,(*PA).YieldTypeDes,/DESTROY                ;(*PA).YieldTypeDes���Ѵ洢�˲����������ڵ�TLB
		       	 RETURN
		       ENDIF
		       DC_YieldDescription,GROUP_LEADER=Event.id,ITTLB=DesTLB   ;����Ĺؼ�������ȡ�ò����������ڵĶ���TLB
		       (*PA).YieldTypeDes = DesTLB
		    ENDIF

			IF (Event.PRESS EQ 4) THEN BEGIN  ;�һ����
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
		IF COUNT NE 0 THEN RETURN    	;û��ѡ���򷵻�

		WIDGET_CONTROL,(*PA).EstiTABLE,GET_VALUE=YieldSel,/USE_TABLE_SELECT,GET_UVALUE = CountyName
		CellSelYield = [CellSel,TRANSPOSE(FLOAT(YieldSel))]  ;�˴�Ϊ3�е�FLOAT������

		Temp = WHERE(CellSelYield[0,*] EQ 0,COUNT,COMPLEMENT=No0ID,NCOMPLEMENT=Num)
		IF COUNT NE 0 THEN BEGIN
			IF Num EQ 0 THEN RETURN    ;ȫ��ѡ����ǵ�һ�е��ش���.
			CellSelYield = CellSelYield[*,No0ID]
		ENDIF

		Yield = CellSelYield[2,*]			;����
		Row   = CellSelYield[1,*]
		RowIndex = Row[UNIQ(Row,SORT(Row))]
		AvgYield = FLTARR(1)
		FOR I=0,N_ELEMENTS(RowIndex)-1 DO BEGIN
			SelYield = Yield[WHERE(Row EQ RowIndex[I])]
			Temp = WHERE(SelYield EQ 0.0,COUNT,COMPLEMENT=No0ID,NCOMPLEMENT=Num)
			IF COUNT NE 0 THEN BEGIN
				IF Num EQ 0 THEN BEGIN
					INFO = DIALOG_MESSAGE('����"'+CountyName[RowIndex[I]]+'"ѡ���˿�ֵ!',title='��ʾ')
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
    	Head = ['����','����','���յ���',*((*PA).EstiTypeID),'����','���']
    	SaveData = [[Head],[CountyCode,ReYield,EsYield[1:*,*],CropYear]]

    	FILE = ProName+CalcYear+CropName+'�������չ��㵥��.txt'

		DC_SaveTextData,SaveData,EVENT.ID,FILENAME=FILE

    END

    Widget_Info(wWidget, FIND_BY_UNAME='ResultTODB'): BEGIN

    	IF (*PA).CountyName EQ '' THEN RETURN

    	WIDGET_CONTROL,(*PA).ResultTABLE,GET_VALUE=ReYield,GET_UVALUE=CountyCode
		CountyYield = [CountyCode,ReYield[1,*]]
		Temp = WHERE(ReYield[1,*] EQ '',COUNT,COMPLEMENT=NO0ID,NCOMPLEMENT=Num0)
		IF COUNT NE 0 THEN BEGIN
			IF Num0 EQ 0 THEN BEGIN
				INFO = DIALOG_MESSAGE('���ʱ,���յ����������ȫ���ǿ�ֵ!',title='����')
				RETURN
			ENDIF
			CountyYield = CountyYield[*,NO0ID]
		ENDIF

		CalYear = (*PA).CalcYear
		Crop_id  = (*PA).CropID

		COMMON COMMON_BLOCK

		Num = N_ELEMENTS(CountyYield)/2

	  	progressTimer = Obj_New("ShowProgress",tlb,TITLE='�������',MESSAGE='���ڽ����ղ����뵽���ݿ���,���Ժ�!') ;�½�����������
		progressTimer->START

		FOR i=0,Num-1 DO BEGIN     ;�������ղ����������
	     	progressTimer->UPDATE,((i+1)*1./Num * 100.0)  ;���½�����

			Sqlstr1="delete from COUNTY_ESTIMATED_YIELD where crop_id='"+ crop_id+ $
			         "' and Year="+CalYear+" and model_type_id=0 and county_code ='" $
			         +CountyYield[0,i]+"'"

			Sqlstr2="insert into COUNTY_ESTIMATED_YIELD values('"+crop_id+"','"+ $
			        CountyYield[0,i]+"',"+STRTRIM(CountyYield[1,i],2)+',0,'+CalYear+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2
		ENDFOR

		;======��������ӣ����ںϺ�������ݼ�Ȩ��ʡ��20070908======================================================
		;------------��Ȩ��ʡ-------CROP_AREA_COUNTY;PLOWLAND_AREA_COUNTY,������CROP_AREA_COUNTY
		;ע������ȡ�������ʱ,�����ǰ��ݵ��������û��,���ÿ���������Newestyear���������

		INFO = DIALOG_MESSAGE('�Ƿ���Ҫ���ع��������Ȩ��ʡ?',/QUESTION,TITLE='ѯ��')
		IF INFO EQ 'Yes' THEN begin
			Status = DC_WeightToPro(CountyYield,Crop_id,CalYear,(*PA).ProID,'0')
			IF Status EQ 1 THEN Prompt=DIALOG_MESSAGE('��Ȩ����ɹ�!',/INFORMATION,TITLE='��ʾ')
		endif
		;===============================================================================

;;		progressTimer->DESTROY
		OBJ_DESTROY,progressTimer

		INFO = DIALOG_MESSAGE('������!',/INFORMATION,title='��ʾ')
		log, '����Ԥ��-�����ں�', 1
    END
    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '�����ںϷ���', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
	end

;		ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,'�����ںϷ���'
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'�رղ����ںϷ���'
    	WIDGET_CONTROL,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END

;*****************�����ں�������*************************************
PRO DC_YieldMerge,GROUP_LEADER=groupleader

	common_log,'���������ںϷ���'

   IF ( XREGISTERED('DC_YieldMerge') NE 0 ) THEN RETURN  ;����ô����ѵ���,���ظ�����.

   TLB = Widget_Base(UNAME='TLB',XOFFSET=180 ,YOFFSET=200   $
      ,TITLE='�����ںϷ���' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=1 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1,GROUP_LEADER=groupleader) ;XOFFSET=320
   ;------------�����һ��BASE--�ĸ�DROPlist���ڵ�BASE----------------------------------;
	  ConditionB = Widget_Base(TLB,UNAME='ConditionB',FRAME=1,SCR_XSIZE=695 $
	  			   ,SCR_YSIZE=32,SPACE=80,XPAD=80,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

		D_WIDTH=100
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

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=80 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='��ȡ',EVENT_PRO='DC_TakeAllEstiYeildEV')

;----------------�����м��Base����---------------------------------------
  Middle_BASE = Widget_Base(TLB,SPACE=1,XPAD=0,YPAD=0,/ROW,/BASE_ALIGN_TOP)
	;�м���󲿷�
  	ResultYield_BASE = Widget_Base(Middle_BASE,FRAME=0,SPACE=3 ,XPAD=0,YPAD=0,/COL,/BASE_ALIGN_LEFT)

  	     ResultTaleBASE = Widget_Base(ResultYield_BASE,SCR_YSIZE=275,FRAME=1,SPACE=1 ,XPAD=0,YPAD=1,/COL,/BASE_ALIGN_LEFT,SCR_XSIZE=310)
			  ResultYield_LA= Widget_Label(ResultTaleBASE,UNAME='ResultYield_LA',SCR_XSIZE=228  $
			      ,SCR_YSIZE=18,/ALIGN_CENTER ,VALUE='���յ�����')

			  ResultTABLE = Widget_Table(ResultTaleBASE, UNAME='ResultTABLE',/ALL_EVENTS $
			      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=310 ,SCR_YSIZE=250 ,XSIZE=7 $
			      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=75,/DISJOINT_SELECTION,EVENT_PRO='DC_ResultTableEV')

        SelectBASE = Widget_Base(ResultYield_BASE,FRAME=1,SCR_XSIZE=310,SCR_YSIZE=72,SPACE=1 ,XPAD=0,YPAD=8,/COL,/BASE_ALIGN_CENTER)
			 Prompt_LA= Widget_Label(SelectBASE,UNAME='ResultYield_LA',SCR_XSIZE=228  $
				      ,SCR_YSIZE=18,/ALIGN_LEFT ,VALUE=' ����ȷ����ʽ��')
	  	     PCA_BASE = Widget_Base(SelectBASE,/FRAME,SCR_XSIZE=290,SPACE=30,XPAD=25,YPAD=1,/ROW,/BASE_ALIGN_CENTER)
				  PCA_BU = Widget_Button(PCA_BASE,UNAME='PCA_BU',XSIZE=100,SCR_YSIZE=22,/ALIGN_CENTER,VALUE='���ɷַ���(PCA)',EVENT_PRO='DC_PCA_EV')
				  UseSel = Widget_Button(PCA_BASE,UNAME='UseSel',XSIZE=100,SCR_YSIZE=22,/ALIGN_CENTER,VALUE='�û�ѡ��')
  ;�м���Ҳ���
  	EstiYield_BASE = Widget_Base(Middle_BASE,FRAME=1,SPACE=3 ,XPAD=0,YPAD=0,/COL,/BASE_ALIGN_LEFT)

  	     EstimateTaleBASE = Widget_Base(EstiYield_BASE,SCR_YSIZE=275,FRAME=0,SPACE=1,XPAD=0,YPAD=1,/COL,/BASE_ALIGN_LEFT)
			  EstiYield_LA = Widget_Label(EstimateTaleBASE,UNAME='EstiYield_LA',SCR_XSIZE=250  $
			      ,SCR_YSIZE=18,/ALIGN_CENTER ,VALUE='����ģ�����')
			  EstiTABLE = Widget_Table(EstimateTaleBASE, UNAME='EstiTABLE',/ALL_EVENTS $
			      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=380 ,SCR_YSIZE=250 ,XSIZE=7 $
			      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=75,/DISJOINT_SELECTION,EVENT_PRO='DC_EstiTableEV')

	 	 DrawWid = Widget_Draw(EstiYield_BASE,SCR_XSIZE=380,SCR_YSIZE=70,GRAPHICS_LEVEL=1,RETAIN=2,/FRAME $
	      						,UNAME='DrawWid',/TRACKING_EVENTS,/BUTTON_EVENTS);,/MOTION_EVENTS)

  DrawContext = WIDGET_BASE(DrawWid,/CONTEXT_MENU,UNAME  = 'DrawContext') ;ֻ��Draw������/BUTTON_EVENTS�Ժ�,DrawWid�ſ���Ϊparent
	  DisLine = Widget_Button(DrawContext,UNAME='DisLine',VALUE='����ͼ',/CHECKED_MENU)
	  DisChartLine =Widget_Button(DrawContext,UNAME='DisChartLine',VALUE='��״����ͼ',/CHECKED_MENU)


;---�������µ�Base����--------------------------------------
   	button_BASE = Widget_Base(TLB,FRAME=1,SCR_XSIZE=695,SPACE=60 ,XPAD=21,YPAD=1,/ROW,/BASE_ALIGN_LEFT)
  ;-----------------------------------------------------------------
	 BUWidth = 80
	  Refresh_bu = Widget_Button(button_BASE, UNAME='Refresh_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='ˢ��',SENSITIVE=0)
	  ResultTODB = Widget_Button(button_BASE, UNAME='ResultTODB' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='���',TOOLTIP='�����ղ������')

	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='����ȡ�����ݱ��浽���ش���' ,VALUE='����')

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='����')


	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=BUWidth ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='�ر�')
;------------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�
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

  PA={   ProNameList			:	Province		,$				;ʡ���б�
		    ProIDList			:	ProIDList		,$				;ʡID�б�
		    ProID				:	ProCode			,$				;��ѡʡID
		    CropIDList			:	CropIDList		,$				;����ID�б�
		    CropNameList		:	Crop			,$      		;�������б�
		    CropID				:	CropID			,$
		    ARRAY_YEAR			:	ARRAY_YEAR		,$
		    CalcYear			:	CalcYear		,$
			ResultYield_LA		:	ResultYield_LA	,$
         	ResultTABLE 		: 	ResultTABLE 	,$
 			EstiYield_LA		:	EstiYield_LA 	,$
         	EstiTABLE   	    : 	EstiTABLE		,$
         	YieldTypeDes		:	0L				,$				;��������������TLB
         	EstiTypeID			:	PTR_NEW()	 	,$				;���õ������ͱ�ʶ,����˵��X��ı�ǩ
         	DisL				:	0				,$				;�Ƿ�ʹ������ͼ
         	DisLC				:	0				,$ 				;�Ƿ�ʹ����״������ͼ
         	DisData				:	PTR_NEW()	 	,$				;��ͼ����ʾ������,
         	CountyName			:	''				,$				;��ͼ���������ݵ���
         	PCA_TLB				:	0L				,$				;���ɷַ�������Ķ���TLB
         	DrawWid        		: 	DrawWid	}

  Widget_Control,TLB,SET_UVALUE=PTR_NEW(PA,/NO_COPY)

  XManager, 'DC_YieldMerge',TLB,CLEANUP='DC_CleanAllHeap', /NO_BLOCK

END
