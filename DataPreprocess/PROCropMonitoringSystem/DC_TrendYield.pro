;���Ʋ���ģ��
;**�Զ������:���������********************************
PRO DC_ComputeParameter,TLB ;TLB�Ƕ���BASE

	DC_CleanTrendield,TLB          ;�����

	WIDGET_CONTROL,TLB,GET_UVALUE=PA

;	PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��

	WIDGET_CONTROL,(*PA).CountyName,GET_VALUE=SelectCounty
	County = STRSPLIT(SelectCounty[0],/EXTRACT)

	StartYear = FIX((*PA).StartYear)   &  EndYear = FIX((*PA).EndYear)
	CalcYear =  FIX((*PA).CalcYear)   &
    YearNum = EndYear-StartYear+1

	if YearNum le 1 then return

	ActualYield = FLOAT(*((*PA).ActualYield))

	ZeroID = WHERE(ActualYield EQ 0.0,ZeroNum,COMPLEMENT = NotZeroID)
	IF ZeroNum NE 0 THEN BEGIN
	  ZeroCell = [TRANSPOSE(ZeroID),INTARR(1,ZeroNum)]
	  ActualYield[ZeroID] = MEAN(ActualYield[NotZeroID]) 	  ;��ȱʧ��������в�����ݵ�ƽ��ֵ���滻.
	ENDIF ELSE ZeroCell =[-1,-1]

	SmoothYield = ActualYield


	CASE (*PA).SmootID OF
	  0 :  BEGIN
	         FOR i=1,(YearNum-2) DO BEGIN
	            SmoothYield[i]=(ActualYield[i-1]+ActualYield[i]+ActualYield[i+1])/3    ;��������ƽ��
	         ENDFOR
	  END
	  1 :  BEGIN
	         FOR i=2,(YearNum-3) DO BEGIN
	            SmoothYield[i]=(ActualYield[i-2]+ActualYield[i-1]+ActualYield[i]+ActualYield[i+1]+ActualYield[i+2])/5    ;�������ƽ��
	         ENDFOR
	  END
	  2 :  BEGIN
	         FOR i=3,(YearNum-4) DO BEGIN
	            SmoothYield[i]=(ActualYield[i-3]+ActualYield[i-2]+ActualYield[i-1]+ActualYield[i]+ActualYield[i+1]+ActualYield[i+2]+ActualYield[i+3])/7    ;�����ߵ�ƽ��
	         ENDFOR
	  END
	ENDCASE

	  CASE (*PA).Modetype OF
	  		'lg10'  : BEGIN
		        TrendResult=REGRESS(ALOG10(INDGEN(1,YearNum)+StartYear),SmoothYield,CONST=const $   ;	�Ա�������ʼ��ݿ�ʼ
		                      ,MCORRELATION=correlation,FTEST=F_check,YFIT=TrendYield)
				  IF TrendResult[0] GE 0.0 THEN  BEGIN
				  	 Coefficiency ='+'+ STRTRIM(TrendResult[0],2)
				  ENDIF ELSE  Coefficiency = STRTRIM(TrendResult[0],2)

		        (*PA).Equation_Coe = [const,TrendResult[0]]

		        Equation ='Y='+STRTRIM(const,2)+Coefficiency+ 'LgX'
		        CalcYearYield = STRTRIM(const+TrendResult[0]*ALOG10(CalcYear),2)

	  		END

	  		'linear': BEGIN

		        TrendResult=REGRESS(INDGEN(1,YearNum)+StartYear,SmoothYield,CONST=const $   ;	�Ա�������ʼ��ݿ�ʼ
		                      ,MCORRELATION=correlation,FTEST=F_check,YFIT=TrendYield)
				  IF TrendResult[0] GE 0.0 THEN  BEGIN
				  	 Coefficiency ='+'+ STRTRIM(TrendResult[0],2)
				  ENDIF ELSE  Coefficiency = STRTRIM(TrendResult[0],2)

		        (*PA).Equation_Coe = [const,TrendResult[0]]

		        Equation ='Y='+STRTRIM(const,2)+Coefficiency + 'X'
		        CalcYearYield = STRTRIM((const+TrendResult[0]*CalcYear),2)

	  		END
	  	ELSE:
	  ENDCASE

;HELP,(*PA).Modetype
;PRINT,TRANSPOSE(TrendYield)

	Cor_efficiency = STRTRIM(correlation,2)
	Ftest = STRTRIM(F_check,2)
	SMR = STRTRIM(SQRT(TOTAL((SmoothYield-TRANSPOSE(TrendYield))^2)/(YearNum-1-1)),2)     ;�㷨��ʽ����ң��ѧ����2004,8(6) P492
	WIDGET_CONTROL,(*PA).Equation_Text,SET_VALUE =Equation
	WIDGET_CONTROL,(*PA).Correlation_Text,SET_VALUE =STRMID(Cor_efficiency,0,5)
	WIDGET_CONTROL,(*PA).F_TEXT,SET_VALUE =STRMID(Ftest,0,5)
	WIDGET_CONTROL,(*PA).SMR_TEXT,SET_VALUE = STRMID(SMR,0,5)
;;	WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_TABLE_SELECT=[0,0]      ;�������ζԱ����,��Ϊû��0ֵ����ʱ�ص����Ͻ���ͼ.
;;	WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_TABLE_SELECT=[-1,-1]    ;���ֻ��[-1,-1],�򲻻����κη�ӳ..

	  RowLabels=['ʵ�ʲ���','ƽ������','���Ʋ���','ʵ��-����']
	  ColumnLabels= [STRTRIM(INDGEN(YearNum)+StartYear,2),STRTRIM(CalcYear,2)]+'��'        ;���鴮�� ,����е��б���
	  TableYield=STRARR(YearNum+1,4)
	  TableYield[0:YearNum-1,0] = STRTRIM(ActualYield,2)    &     TableYield[0:YearNum-1,1] = STRTRIM(SmoothYield,2)                 ;���ĸ������ֱ��Ӧ"RowLabels"���ֵ
	  TableYield[0:YearNum-1,2] = STRTRIM(Transpose(TrendYield),2) &  TableYield[0:YearNum-1,3]= STRTRIM(ActualYield-Transpose(TrendYield),2)
	  TableYield[YearNum,2] = CalcYearYield          ;���ǵ�ǰ��ݵ����Ʋ���ֵ.

	  WIDGET_CONTROL,(*PA).TrendYield_TABLE,TABLE_XSIZE=YearNum + 1 $
	                ,SET_UVALUE=ColumnLabels $   ;����֮�������û�ֵ,��Ϊ��������ʱ"�������".
	                ,ROW_LABELS=RowLabels  ,COLUMN_LABELS=ColumnLabels $
	                ,SET_VALUE=TableYield  ,ALIGNMENT=2,COLUMN_WIDTHS=65
;�˴�֮���Է����������Ʊ�,;,����Ϊֻ����ʵ�ֱ������TABLE_XSIZE�������Ժ�ſ���USE_TABLE_SELECT��ָ��������
;��Ȼ��ֻ������TABLE_X(Y)SIZE����ǰ���л�����.����,��ʹ���˶ϵ�ѡ��ʽ./DISJOINT_SELECTION
     CellColor = BYTARR(3,(YearNum+1)*3)
     CellColor[0,          0:YearNum    ]=255B
     CellColor[1,  YearNum+1:2*YearNum+1]=255B
     CellColor[2,2*YearNum+2:3*YearNum+2]=255B
     N = YearNum+1
     CellSelect = [TRANSPOSE([INDGEN(N),INDGEN(N),INDGEN(N)]) $
     			  ,TRANSPOSE([INTARR(N),INTARR(N)+1,INTARR(N)+2])]
	 WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=CellSelect $
					,FOREGROUND_COLOR  = CellColor

;;	 WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=[INDGEN(1,YearNum+1),INTARR(1,YearNum+1)] $
;;					,FOREGROUND_COLOR =[BYTARR(1,YearNum+1)+255B,BYTARR(2,YearNum+1)]
;;	 WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=[INDGEN(1,YearNum+1),INTARR(1,YearNum+1)+1]  $
;;					,FOREGROUND_COLOR =[BYTARR(1,YearNum+1),BYTARR(1,YearNum+1)+255B,BYTARR(1,YearNum+1)]
;;	 WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=[INDGEN(1,YearNum+1),INTARR(1,YearNum+1)+2] $
;;					,FOREGROUND_COLOR =[BYTARR(2,YearNum+1),BYTARR(1,YearNum+1)+255B]
	 WIDGET_CONTROL,(*PA).TrendYield_TABLE,	USE_TABLE_SELECT=ZeroCell,SET_TABLE_SELECT = ZeroCell $
					,BACKGROUND_COLOR  = [255,255,0]     			 ;��ʵ����ݲ���Ϊ0(����ƽ��ֵ���滻�����)�ĵ�Ԫ����ɻ�ɫ��ʾ.
	 WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_TABLE_SELECT=[-1,-1]  ;���ζԱ�WIDGET_CONTROL,��Ϊ�û�ɫ�Զ�����������.

      Save_win = !D.WINDOW     			   ;����ϵͳ�Ĵ���
      OldBackup = !P.BACKGROUND
;	  OldRANGE = !Y.RANGE

	  !P.BACKGROUND = 255
;      !Y.RANGE = [0, MAX(ActualYield)>MAX(SmoothYield)>MAX(TrendYield)+10]

      DEVICE,GET_CURRENT_FONT=oldFont,SET_FONT='*'   ;ע��:SET_FONT='*'
      DEVICE,SET_FONT='SimHei',/TT_FONT

	  DEVICE,GET_DECOMPOSED=old_color     ;��ȡ��ǰDECOMPOSEDֵ
      DEVICE,RETAIN=2, DECOMPOSED=0      ;��IDL�ṩ�󱸴洢,ʹ����ɫ��ѯ��(ͣ����ɫ�ֽ⹦��),
		r=[0,255,  0,  0,255,255]   	  ;����Ϊ��\��\��\��\��\��
		g=[0,  0,255,  0,255,255]
		b=[0,  0,  0,255,  0,255]
 		TVLCT, r, g, b   ;ȱʡ���ĸ�ʡ��,��ʹ��ɫ����������Ϊ0,1,2,3,4,5����ɫΪ��Ӧ��RGB���

      WIDGET_CONTROL, (*PA).Trend_DRAW, GET_VALUE=drawID
  	  WSET, drawID

	  IF (*PA).YieldType EQ 0 THEN BEGIN
;	  	  TITLE='"'+PROVINCE+'-'+County[1]+'"'+'���Ʋ�������ͼ'
	  	  TITLE='"'+County[1]+'"'+'���Ʋ�������ͼ'	;�������޸ģ�20070726
  	  ENDIF ELSE BEGIN
; 	  	  TITLE='"'+PROVINCE+'վ��-'+County[1]+'"'+'���Ʋ�������ͼ'
 	  	  TITLE='"'+'վ��-'+County[1]+'"'+'���Ʋ�������ͼ'	;�������޸ģ�20070726
  	  ENDELSE

	  X = INDGEN(1,YearNum)+StartYear
  	  DisModel =  'LINE'
  	  CASE DisModel OF        ;��ʾģʽ,�������߻�������������ͼ.����д����Ϊ�Ժ�Ľ�;
  	    'LINE': BEGIN
;           Y_rang = [0, MAX(ActualYield)>MAX(SmoothYield)>MAX(TrendYield)+10]
;           FontID = 0  ;(FontID:-1,0,1����ֵ��ѡ)
;		    PLOT,X,ActualYield,XRANGE=[StartYear,EndYear],YRANGE=Y_rang,PSYM=-2 $   ;�������豸����
;		         ,THICK=0,XSTYLE=1,COLOR=1,FONT=FontID,CHARSIZE=1 $
;				 ,POSITION=[0.08,0.1,0.95,0.9];,YMARGIN=[4,2]
;		    XYOUTS,0.25,0.92,TITLE,ALIGNMENT=0,COLOR=3,/NORMAL,CHARSIZE=1.5,CHARTHICK=30,FONT=FontID
;		    XYOUTS,[0.6,0.6,0.6],[0.29,0.22,0.15],COLOR=[1,2,3],ALIGNMENT=0,/NORMAL,FONT=FontID $
;		    	  ,['--*--*--*--   ʵ�ʲ���','.............  ƽ������','_______  ���Ʋ���']

            FontID = 0  ;(FontID:-1,0,1����ֵ��ѡ)
            Y_rang = [0, MAX(ActualYield)>MAX(SmoothYield)>MAX(TrendYield)+10]
		    PLOT,X,ActualYield,XRANGE=[StartYear,EndYear], PSYM=-2 $
		         ,THICK=0,XSTYLE=1,YSTYLE=1,COLOR=1,FONT=FontID ,CHARSIZE=0 $   ;�˴���CHARSIZE�������ʶ��Զ��
				 ,POSITION=[0.08,0.1,0.955,0.9],YRANGE=Y_rang;,YMARGIN=[4,2]
		    XYOUTS,0.25,0.92,TITLE,ALIGNMENT=0,COLOR=3,/NORMAL,FONT=FontID+1,CHARSIZE=1.7   ;�˴���FONT=0,��˴���CHARSIZEû����
		    XYOUTS,[0.61,0.61,0.61],[0.29,0.22,0.15],COLOR=[1,2,3],ALIGNMENT=0,/NORMAL $
		    	  ,FONT=FontID+1,CHARSIZE=1.6 $
		    	  ,['--*--*--ʵ�ʲ���','........ƽ������','--------���Ʋ���']

;            FontID = 1  ;(FontID:-1,0,1����ֵ��ѡ)
;		    PLOT,X,ActualYield,XRANGE=[StartYear,EndYear], PSYM=-2 $
;		         ,THICK=0,XSTYLE=1,YSTYLE=1,COLOR=1,FONT=FontID ,CHARSIZE=1.0 $   ;�˴���CHARSIZE�������ʶ��Զ��
;				 ,POSITION=[0.08,0.12,0.965,0.9],YRANGE=Y_rang;,YMARGIN=[4,100]
;		    XYOUTS,0.25,0.92,TITLE,ALIGNMENT=0,COLOR=3,/NORMAL,FONT=FontID,CHARSIZE=1.0   ;�˴�CHARSIZE����
;		    XYOUTS,[0.65,0.65,0.65],[0.29,0.22,0.15],COLOR=[1,2,3],ALIGNMENT=0,/NORMAL,FONT=FontID,CHARSIZE=1 $
;		    	  ,['--*--*--ʵ�ʲ���','........ƽ������','--------���Ʋ���']

		    OPLOT,X,SmoothYield,LINESTYLE=1,THICK=3,COLOR=2
		    OPLOT,X,TrendYield,LINESTYLE=0,COLOR=3

	     END

  	    'BAR': BEGIN
		    Old_p = !P.POSITION
		    Old_Font = !P.FONT
		    !P.POSITION=[0.07,0.1,0.98,0.90]
		    !P.FONT = 0
		    FirstOFFSET = 1  & Bar_width =3 & Base_range = 0.075 & Space = 10
		    BAR_PLOT, ActualYield, COLORS=INTARR(YearNum)+1, BACKGROUND=0,TITLE = TITLE $
				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
				,BARNAMES = STRTRIM(X,2)

		    BAR_PLOT, SmoothYield, COLORS=INTARR(YearNum)+2, BACKGROUND=255, $
				BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET+1,BASERANGE=Base_range,/OVERPLOT

		    BAR_PLOT, TrendYield, COLORS=INTARR(YearNum)+3, BACKGROUND=255, $
				BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET+2,BASERANGE=Base_range,/OVERPLOT

		    OPLOT,X,TrendYield,LINESTYLE=0,COLOR=3
;		    XYOUTS,0.3,0.92,TITLE,ALIGNMENT=0,COLOR=4,/NORMAL,CHARSIZE=1,CHARTHICK=1,FONT=0

		    !P.POSITION = Old_p
		    !P.FONT = Old_Font
	     END

		 ELSE:
	ENDCASE

      DEVICE,SET_FONT=oldFont
	  DEVICE,DECOMPOSED=old_color   ;����ԭ����DECOMPOSEDֵ,��Ϊ�Զ��庯��MyColor�ı���,�뻹ԭ.
  	  WSET, Save_win				;��ԭԭ������.
	  !P.BACKGROUND = OldBackup		;��ԭ
;	  !Y.RANGE =  OldRANGE
END

;**�Զ������:��ȡ����Ӧ�����ʵ�ʲ���(����û�в���������Կ�ֵ�����)*********************
FUNCTION DC_GetActualYield,StartYear,EndYear,CropID,CountyCode,YieldType

		ActualYield = STRARR(1)
		StartYear_ = FIX(StartYear)
		EndYear_   = FIX(EndYear)

		IF YieldType EQ 0 THEN BEGIN
			Table = "county_crop_sta_yield where crop_id='"
			DistrictCode = "' and county_code='"
		ENDIF ELSE BEGIN
			Table = "AGROSTATION_CROP_STA_YIELD where crop_id='"
			DistrictCode = "' and AgroMeteoStation_CODE='"
		ENDELSE


		FOR I=StartYear_,EndYear_ DO BEGIN
			Sqlstr='select crop_yield from '+Table +CropID+DistrictCode+CountyCode+"' and year="+STRTRIM(I,2)
			ActualYield = [ActualYield,TRANSPOSE(DC_GetdataFromDB_Str(1,Sqlstr))]   ;ÿ��DC_GetdataFromDB_Str(1,Sqlstr)ֻ�õ�һ������.
		ENDFOR

		ActualYield = ActualYield[1:*]

		RETURN,ActualYield   ;�˴����շ�����һ������.

END
;*******�Զ������:���"��ʼ�����������ݰ�ť"���¼�����.********************************
PRO DC_ReSetYearBu,TLB,IsYieldYear = IsYieldYear

		WIDGET_CONTROL,TLB,GET_UVALUE = PA
		StartYear = (*PA).StartYear
		EndYear = (*PA).EndYear
		SmootID = (*PA).SmootID
		YearNum = FIX(EndYear)-FIX(StartYear)+1
		CASE 1 OF
			(FIX(StartYear) GE FIX(EndYear)):BEGIN
				INFO = DIALOG_MESSAGE('������ʼ��ݱ���С�ڽ������!',TITLE='����')
				IsYieldYear = 0
				RETURN
			END

			(FIX(EndYear)-FIX(StartYear)+1) LT 3:BEGIN
				INFO = DIALOG_MESSAGE('������Ҫ3��Ĳ�������!',/INFORMATION,TITLE='��ʾ')
				IsYieldYear = 0
				RETURN
			END

			(FIX(EndYear)-FIX(StartYear)+1) LT 5 AND SmootID EQ 1:BEGIN
				INFO = DIALOG_MESSAGE('��ѡ������ݲ��ܽ���5��ƽ��!',TITLE='����')
				IsYieldYear = 0
				RETURN
			END

			(FIX(EndYear)-FIX(StartYear)+1) LT 7 AND SmootID EQ 2:BEGIN
				INFO = DIALOG_MESSAGE('��ѡ������ݲ��ܽ���7��ƽ��!',TITLE='����')
				IsYieldYear = 0
				RETURN
			END
		    ELSE:
		ENDCASE

		WIDGET_CONTROL,(*PA).CountyName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)
		CropID = (*PA).CropID

		ActualYield = DC_GetActualYield(StartYear,EndYear,CropID,County[0],(*PA).YieldType)

		ActualYield = FLOAT(ActualYield)
		SmoothYield = ActualYield

		CASE SmootID OF
		  0 :  BEGIN
		         FOR i=1,(YearNum-2) DO BEGIN
		            SmoothYield[i]=(ActualYield[i-1]+ActualYield[i]+ActualYield[i+1])/3    ;��������ƽ��
		         ENDFOR
		  END
		  1 :  BEGIN
		         FOR i=2,(YearNum-3) DO BEGIN
		            SmoothYield[i]=(ActualYield[i-2]+ActualYield[i-1]+ActualYield[i]+ActualYield[i+1]+ActualYield[i+2])/5    ;�������ƽ��
		         ENDFOR
		  END
		  2 :  BEGIN
		         FOR i=3,(YearNum-4) DO BEGIN
		            SmoothYield[i]=(ActualYield[i-3]+ActualYield[i-2]+ActualYield[i-1]+ActualYield[i]+ActualYield[i+1]+ActualYield[i+2]+ActualYield[i+3])/7    ;�����ߵ�ƽ��
		         ENDFOR
		  END
		ENDCASE

		ZEROValue = WHERE(SmoothYield EQ 0.0,Num)
		IF (Num NE 0) AND ((*PA).Modetype EQ 'lg10') THEN BEGIN
				INFO = DIALOG_MESSAGE(['��ѡ��ݲ�������ƽ����������ֵ,', $
										'����ʹ�ö���ģ�ͽ���ģ��!'],TITLE='����')
				IsYieldYear = 0
				RETURN
		ENDIF

		IsYieldYear = 1
		PTR_FREE,(*PA).ActualYield
		(*PA).ActualYield = PTR_NEW(ActualYield,/NO_COPY)
;;		HEAP_GC,/PTR

		DC_ComputeParameter,TLB

	END

;**�Զ������:����ʡ��������ȡ��������ػ�վ��Ĵ���********************************
PRO DC_TrendCountyOrSation,CountyList $	;�б�list���ID
							,CountyName $	;��ʾ�ص����ID.
							,YieldType  $   ;��ʶ���ػ���վ���������
							,ProCode	$	;ʡID
							,CropID		$	;����ID
							,NumReocrd = NumReocrd  ;�õ���¼��
	IF YieldType EQ 0 THEN BEGIN
		Sql = 'select code,name from county_code where LEFT(code,2)='+ProCode
		Sql = 'select distinct code,name from county_crop_sta_yield a,('+Sql+ $
			") b where a.county_code=b.code and a.crop_id='"+CropID+"'"
	ENDIF ELSE BEGIN
		Sql = 'select code,name from AGRO_METEO_STATION_INFO where LEFT(county_code,2)='+ProCode
		Sql = 'select distinct code,name from AGROSTATION_CROP_STA_YIELD a,('+Sql+ $
			") b where a.agrometeoStation_code = b.code and a.crop_id='"+CropID+"'"
	ENDELSE

	CountyCode = DC_GetDataFromDB(Sql,N_RECORDS = NumReocrd)

	IF NOT ARRAY_EQUAL(CountyCode,'',/NO_TYPECONV ) THEN BEGIN
		WIDGET_CONTROL,CountyList,SET_VALUE=' '+CountyCode.CODE+'  '+CountyCode.NAME $
							   ,SET_UVALUE=CountyCode, SET_LIST_SELECT=-1
		WIDGET_CONTROL,CountyName,SET_VALUE=''
	ENDIF ELSE BEGIN      ;û�����������Ĳ�ѯ
		WIDGET_CONTROL,CountyList,SET_VALUE='',SET_UVALUE=''
		WIDGET_CONTROL,CountyName,SET_VALUE=''
	ENDELSE

END
;88888888888888888888888888888888888888888888888888888888888888888888888888888888
;**�Զ������:�����Ӧ�����ֵ********************************
PRO DC_CleanTrendield,TLB

	WIDGET_CONTROL,TLB,GET_UVALUE=PA

;    Save_win = !D.WINDOW     ;����ϵͳ�Ĵ���
	WIDGET_CONTROL,(*PA).Trend_DRAW,GET_VALUE=winID
	WSET,winID
	ERASE,COLOR=!D.N_COLORS-1
;	WSET,Save_win

	WIDGET_CONTROL,(*PA).Equation_Text,SET_VALUE=''
	WIDGET_CONTROL,(*PA).Correlation_Text,SET_VALUE=''
	WIDGET_CONTROL,(*PA).SMR_TEXT,SET_VALUE=''
	WIDGET_CONTROL,(*PA).F_TEXT,SET_VALUE=''

	WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE=TRYield & TRYield[*,*] = ''
	WIDGET_CONTROL,(*PA).TrendYield_TABLE,SET_VALUE=TRYield $
				  ,BACKGROUND_COLOR=[255,255,255],SET_TABLE_SELECT=[-1,-1]

END
;--------------���������Ĳ����¼�---------------------------------------------
PRO DC_TrendYield_event, Event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  widget_info(Event.id, /trLee_root) : event.id)

        CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

	wWidget =  Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	WIDGET_CONTROL,/HOURGLASS

  CASE wTarget OF
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='ProDroplist'): BEGIN
		  (*PA).ProID = (*PA).ProIDList[EVENT.INDEX]
		  DC_CleanTrendield,EVENT.TOP
		  DC_TrendCountyOrSation,(*PA).CountyList,(*PA).CountyName,(*PA).YieldType $
		  						  ,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd

		(*PA).IsBlankList = NumReocrd EQ 0

		Batchbutton = WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU')
		IF (*PA).IsCalPara THEN BEGIN

			WIDGET_CONTROL,Batchbutton,SENSITIVE=0
			WIDGET_CONTROL,(*PA).TrendParaBase,/SENSITIVE
			WIDGET_CONTROL,(*PA).Saveto,/SENSITIVE
			WIDGET_CONTROL,(*PA).CountyName,SET_VALUE='��ѡ����'
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).CountyList,GET_UVALUE=CountyCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =CountyCode[0].CODE+' '+CountyCode[0].NAME
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,Batchbutton,/SENSITIVE
				WIDGET_CONTROL,(*PA).TrendParaBase,SENSITIVE=0
				WIDGET_CONTROL,(*PA).Saveto,SENSITIVE=0
				WIDGET_CONTROL,(*PA).CountyName,SET_VALUE=CouttyValue

		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
		ENDIF

	END


	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CropDroplist'): BEGIN

		(*PA).CropID = (*PA).CropIDList[EVENT.INDEX]

		PRO_COMMON_DC
		COMMON DC_BLOCK,NewCropID	;���ò��ı�
		NewCropID = (*PA).CropID    ;ע��:�ڵ������ģ����,���˴��ı�����ID,�����ط���������.

		  DC_CleanTrendield,EVENT.TOP
		  DC_TrendCountyOrSation,(*PA).CountyList,(*PA).CountyName,(*PA).YieldType $
								,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd
		  (*PA).IsBlankList = NumReocrd EQ 0

		Batchbutton = WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU')
		IF (*PA).IsCalPara THEN BEGIN

			WIDGET_CONTROL,Batchbutton,SENSITIVE=0
			WIDGET_CONTROL,(*PA).TrendParaBase,/SENSITIVE
			WIDGET_CONTROL,(*PA).Saveto,/SENSITIVE
			WIDGET_CONTROL,(*PA).CountyName,SET_VALUE='��ѡ����'
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).CountyList,GET_UVALUE=CountyCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =CountyCode[0].CODE+' '+CountyCode[0].NAME
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,Batchbutton,/SENSITIVE
				WIDGET_CONTROL,(*PA).TrendParaBase,SENSITIVE=0
				WIDGET_CONTROL,(*PA).Saveto,SENSITIVE=0
				WIDGET_CONTROL,(*PA).CountyName,SET_VALUE=CouttyValue

		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='YearDroplist'): BEGIN

		(*PA).CalcYear=(*PA).ARRAY_YEAR[EVENT.INDEX]
		IF NOT (*PA).IsCalPara THEN RETURN

		IF (NOT (*PA).IsBlankList) AND ((*PA).IsYieldYear EQ 1) THEN BEGIN
			Sel = WIDGET_INFO((*PA).CountyList,/LIST_SELECT)
			IF NOT ARRAY_EQUAL(Sel,-1) THEN BEGIN
				DC_ComputeParameter,Event.top
			END
		ENDIF


	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='ParaBu'): BEGIN
		DC_CleanTrendield,EVENT.TOP

		(*PA).IsCalPara = EVENT.SELECT
		Batchbutton = WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU')
		IF (*PA).IsCalPara THEN BEGIN

			WIDGET_CONTROL,Batchbutton,SENSITIVE=0
			WIDGET_CONTROL,(*PA).TrendParaBase,/SENSITIVE
			WIDGET_CONTROL,(*PA).Saveto,/SENSITIVE
			WIDGET_CONTROL,(*PA).CountyName,SET_VALUE='��ѡ����'
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).CountyList,GET_UVALUE=CountyCode
			IF (*PA).IsBlankList THEN BEGIN
				CouttyValue = ''
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CouttyValue =CountyCode[0].CODE+' '+CountyCode[0].NAME
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,Batchbutton,/SENSITIVE
				WIDGET_CONTROL,(*PA).TrendParaBase,SENSITIVE=0
				WIDGET_CONTROL,(*PA).Saveto,SENSITIVE=0
				WIDGET_CONTROL,(*PA).CountyName,SET_VALUE=CouttyValue

		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
		ENDIF

	END


	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CountyList'): BEGIN
        DC_CleanTrendield,EVENT.TOP

		IF (*PA).IsBlankList THEN BEGIN
			IF (*PA).YieldType EQ 0 THEN BEGIN
				INFO = DIALOG_MESSAGE('�б���û�п�ѡ����!',/INFO,TITLE='��ʾ')
			ENDIF ELSE 	INFO = DIALOG_MESSAGE('�б���û�п�ѡ��վ��!',/INFO,TITLE='��ʾ')

			RETURN
		ENDIF

		WIDGET_CONTROL,EVENT.ID,GET_UVALUE=County

		WIDGET_CONTROL,(*PA).CountyName,SET_VALUE=County[EVENT.INDEX].CODE+' '+County[EVENT.INDEX].NAME

		IF NOT (*PA).IsCalPara THEN RETURN

		SelectedDistrict = County[EVENT.INDEX].CODE
		Cropid = (*PA).Cropid
		IF (*PA).YieldType EQ 0 THEN BEGIN
			SQL = 'select max(year) as maxyear, min(year) as minyear from county_crop_sta_yield '+$
				 "where county_code='"+SelectedDistrict+"' and crop_id='"+Cropid+"'"
		ENDIF ELSE BEGIN
			SQL = 'select max(year) as maxyear, min(year) as minyear from AGROSTATION_CROP_STA_YIELD '+$
				 "where AgroMeteoStation_CODE='"+SelectedDistrict+"' and crop_id='"+Cropid+"'"
		ENDELSE
		YearThreshold = DC_GetDataFromDB(SQL)
		MaxYear = STRTRIM(YearThreshold.maxyear,2)
		MinYear = STRTRIM(YearThreshold.minyear,2)
		YearHaveData = STRTRIM(INDGEN(FIX(MaxYear)-FIX(MinYear)+1)+FIX(MinYear),2)

		WIDGET_CONTROL,(*PA).StartYearDr,SET_VALUE=YearHaveData $
					  ,SET_DROPLIST_SELECT=WHERE(YearHaveData EQ MinYear)
		WIDGET_CONTROL,(*PA).EndYearDr,SET_VALUE=YearHaveData $
					  ,SET_DROPLIST_SELECT=WHERE(YearHaveData EQ maxyear)

		(*PA).StartYear= MinYear
		(*PA).EndYear  = MaxYear
		PTR_FREE,(*PA).YearHaveData
		(*PA).YearHaveData = PTR_NEW(YearHaveData,/NO_COPY)

		ActualYield = DC_GetActualYield((*PA).StartYear,(*PA).EndYear,(*PA).CropID,SelectedDistrict,(*PA).YieldType)
;;		ActualYield_ = ActualYield

		PTR_FREE,(*PA).ActualYield
		(*PA).ActualYield = PTR_NEW(ActualYield,/NO_COPY)

		YearNum = FIX(MaxYear)-FIX(MinYear)+1
		IF (FIX(MaxYear)-FIX(MinYear)+1) LT 3 THEN BEGIN
			INFO = DIALOG_MESSAGE('���ݿ��и���(��)����'+STRTRIM(YearNum,2)+'������,����������Ҫ3��Ĳ�������!',/INFORMATION,TITLE='��ʾ')
			RETURN
		ENDIF

		DC_ComputeParameter,Event.top

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='StartYearDr'): BEGIN
		DC_CleanTrendield,EVENT.TOP
		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
			RETURN
		ENDIF ELSE BEGIN
			Sel = WIDGET_INFO((*PA).CountyList,/LIST_SELECT)
			IF ARRAY_EQUAL(Sel,-1) THEN BEGIN
		       INFO = DIALOG_MESSAGE('�������б���ѡ��һ��!',/INFOR,TITLE='��ʾ')
			   RETURN
			END
		ENDELSE

		(*PA).StartYear = (*((*PA).YearHaveData))[EVENT.INDEX]
		DC_ReSetYearBu,EVENT.TOP,IsYieldYear=IsYieldYear
		(*PA).IsYieldYear = IsYieldYear

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='EndYearDr'): BEGIN
		DC_CleanTrendield,EVENT.TOP
		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
			RETURN
		ENDIF ELSE BEGIN
			Sel = WIDGET_INFO((*PA).CountyList,/LIST_SELECT)
			IF ARRAY_EQUAL(Sel,-1) THEN BEGIN
		       INFO = DIALOG_MESSAGE('�������б���ѡ��һ��!',/INFOR,TITLE='��ʾ')
			   RETURN
			END
		ENDELSE

		(*PA).EndYear = (*((*PA).YearHaveData))[EVENT.INDEX]
		DC_ReSetYearBu,EVENT.TOP,IsYieldYear = IsYieldYear
		(*PA).IsYieldYear = IsYieldYear
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='lg10_BUTTON'): BEGIN
		IF NOT EVENT.SELECT THEN RETURN

		(*PA).Modetype = 'lg10'

		IF (*PA).IsYieldYear EQ 0 THEN BEGIN
			INFO = DIALOG_MESSAGE('����������ò���ȷ!',TITLE='����')
			RETURN
		ENDIF

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
			RETURN
		ENDIF ELSE BEGIN
			Sel = WIDGET_INFO((*PA).CountyList,/LIST_SELECT)
			IF ARRAY_EQUAL(Sel,-1) THEN BEGIN
		       INFO = DIALOG_MESSAGE('�������б���ѡ��һ��!',/INFOR,TITLE='��ʾ')
			   RETURN
			END
		ENDELSE

		DC_ComputeParameter,EVENT.TOP
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='linear_BUTTON'): BEGIN
		IF NOT EVENT.SELECT THEN RETURN
			(*PA).Modetype = 'linear'

		IF (*PA).IsYieldYear EQ 0 THEN BEGIN
			INFO = DIALOG_MESSAGE('����������ò���ȷ!',TITLE='����')
			RETURN
		ENDIF

			IF (*PA).IsBlankList THEN BEGIN
;				PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
				Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;				INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
				INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
				RETURN
			ENDIF ELSE BEGIN
				Sel = WIDGET_INFO((*PA).CountyList,/LIST_SELECT)
				IF ARRAY_EQUAL(Sel,-1) THEN BEGIN
			       INFO = DIALOG_MESSAGE('�������б���ѡ��һ��!',/INFOR,TITLE='��ʾ')
				   RETURN
				END
			ENDELSE

			DC_ComputeParameter,EVENT.TOP

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='YieldType'): BEGIN   ;��������ѡ���¼�
		IF NOT EVENT.SELECT THEN RETURN
		  DC_CleanTrendield,EVENT.TOP

		(*PA).YieldType	= EVENT.VALUE
		IF (*PA).YieldType THEN Title='վ��' ELSE Title='��'
		WIDGET_CONTROL,(*PA).TitleName,SET_VALUE =Title+':'

		DC_TrendCountyOrSation,(*PA).CountyList,(*PA).CountyName,(*PA).YieldType $
		  						,(*PA).ProID,(*PA).CropID,NumReocrd = NumReocrd

		(*PA).IsBlankList = NumReocrd EQ 0

		Batchbutton = WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU')
		IF (*PA).IsCalPara THEN BEGIN

			WIDGET_CONTROL,Batchbutton,SENSITIVE=0
			WIDGET_CONTROL,(*PA).TrendParaBase,/SENSITIVE
			WIDGET_CONTROL,(*PA).Saveto,/SENSITIVE
			WIDGET_CONTROL,(*PA).CountyName,SET_VALUE='��ѡ��'+Title
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
		ENDIF ELSE BEGIN
			WIDGET_CONTROL,(*PA).CountyList,GET_UVALUE=CountyCode
			IF (*PA).IsBlankList THEN BEGIN
				CountyValue = ''
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=-1
			ENDIF ELSE BEGIN
				CountyValue =CountyCode[0].CODE+' '+CountyCode[0].NAME
				WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=0
			ENDELSE
				WIDGET_CONTROL,Batchbutton,/SENSITIVE
				WIDGET_CONTROL,(*PA).TrendParaBase,SENSITIVE=0
				WIDGET_CONTROL,(*PA).Saveto,SENSITIVE=0
				WIDGET_CONTROL,(*PA).CountyName,SET_VALUE=CountyValue

		ENDELSE

		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
		ENDIF

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='SmootWay'): BEGIN
		IF NOT EVENT.SELECT THEN RETURN

		IF (*PA).IsYieldYear EQ 0 THEN BEGIN
			(*PA).SmootID = EVENT.VALUE
			INFO = DIALOG_MESSAGE('����������ò���ȷ!',TITLE='����')
			RETURN
		ENDIF


		OldID = (*PA).SmootID
		IF (*PA).IsBlankList THEN BEGIN
;			PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
			Crop = STRCOMPRESS((*PA).CropNameList[WHERE((*PA).CropIDList EQ (*PA).CropID)],/REMOVE_ALL)   ;�õ�������
;			INFO = DIALOG_MESSAGE('���ݿ���û��"'+PROVINCE+Crop+'"��������',/INFOR,TITLE='��ʾ')
			INFO = DIALOG_MESSAGE('���ݿ���û��"'+Crop+'"��������',/INFOR,TITLE='��ʾ')
			RETURN
		ENDIF ELSE BEGIN
			Sel = WIDGET_INFO((*PA).CountyList,/LIST_SELECT)
			IF ARRAY_EQUAL(Sel,-1) THEN BEGIN
		       (*PA).SmootID = EVENT.VALUE
		       INFO = DIALOG_MESSAGE('�������б���ѡ��һ��!',/INFOR,TITLE='��ʾ')
			   RETURN
			END
		ENDELSE

		MinYear = (*PA).StartYear
		MaxYear = (*PA).EndYear
		YearNum = FIX(MaxYear)-FIX(MinYear)+1
		IF (FIX(MaxYear)-FIX(MinYear)+1) LT 3 THEN BEGIN
			(*PA).SmootID = EVENT.VALUE
			INFO = DIALOG_MESSAGE('���ݿ��и���(��)����'+STRTRIM(YearNum,2)+'������,����������Ҫ3��Ĳ�������!',/INFORMATION,TITLE='��ʾ')
			RETURN
		ENDIF

		YearNum = N_ELEMENTS(*((*PA).ActualYield))
		CASE 1 OF
			(YearNum LT 7) AND (EVENT.VALUE EQ 2):BEGIN
				INFO = DIALOG_MESSAGE(['��ѡ��ݲ������ݲ����Խ����ߵ�ƽ��!'],TITLE='����')

				WIDGET_CONTROL,EVENT.ID,SET_VALUE=OldID
				(*PA).SmootID = OldID
				RETURN
			END

			(YearNum LT 5) AND (EVENT.VALUE EQ 1):BEGIN
				INFO = DIALOG_MESSAGE(['��ѡ��ݲ������ݲ����Խ������ƽ��!'],TITLE='����')

				WIDGET_CONTROL,EVENT.ID,SET_VALUE=OldID
				(*PA).SmootID = OldID
				RETURN
			END
		 	ELSE:
		 ENDCASE

		 (*PA).SmootID = EVENT.VALUE    ;ֵΪ0,1,2,��Ӧ��\��\�ߵ�ƽ����ʽ.
		DC_ComputeParameter,EVENT.TOP
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='saveToCompute'): BEGIN

		IF (*PA).IsYieldYear EQ 0 THEN BEGIN
			INFO = DIALOG_MESSAGE('����������ò���ȷ,���������ü�����ٱ���!',TITLE='����')
			RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).Equation_Text,GET_VALUE = Equation
		WIDGET_CONTROL,(*PA).Correlation_Text,GET_VALUE = Correlation
		WIDGET_CONTROL,(*PA).F_TEXT,GET_VALUE = F
		WIDGET_CONTROL,(*PA).SMR_TEXT,GET_VALUE = SMR
		WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE = YieldData,GET_UVALUE=YearName

       IF ARRAY_EQUAL(YieldData,'') THEN BEGIN
          Info=DIALOG_MESSAGE('����û�н������Ʋ���ģ��!',/INFORMATION,TITLE='��ʾ')
          RETURN
       ENDIF

		WIDGET_CONTROL,(*PA).CountyName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)
;		PROVINCE = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]   ;�õ�ʡ��
;		PlaceCounty = PROVINCE+'-'+County[1]+'���Ʋ�������'
		PlaceCounty = County[1]+'���Ʋ�������'

		TOTAL_COL = N_ELEMENTS(YearName)+3           ;������
		RowLabels = TRANSPOSE(['���','ʵ�ʲ���','ƽ������','���Ʋ���','ʵ��-����'])
		ParaIterm = TRANSPOSE(['������','���ϵ��R','F����ֵ','��׼��','����'])
		ParaValue = TRANSPOSE(['����ֵ',Correlation,F ,SMR,Equation])
		Data = [[YearName],[TEMPORARY(YieldData)]]

		SaveData=[ParaIterm,ParaValue,RowLabels,Data]  ;�����鴮��-------

		Temp = WHERE(SaveData EQ '',Num)
		IF Num NE 0 THEN SaveData[Temp] = '---'

		Filename=DIALOG_PICKFILE(TITLE='���Ϊ��',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
		  ,/OVERWRITE_PROMPT,FILE = PlaceCounty+'.txt',/WRITE,PATH=DC_PathSetting(),GET_PATH=SavePath, DIALOG_PARENT=Event.id)

		IF Filename EQ '' THEN RETURN

		path = DC_PathSetting(WRITEPATH1= SavePath)
		;----������ͼ��������-----------------------------------------
		OPENW,LUN,Filename,/GET_LUN ,WIDTH=TOTAL_COL*(MAX(STRLEN(SaveData))+1)
		PRINTF,LUN,SaveData;,FORMAT='(5(A20,2X))'
		FREE_LUN,LUN

		OldWin = !D.WINDOW
		WIDGET_CONTROL,(*PA).Trend_DRAW,GET_VALUE = DrawID
		WSET,DrawID
		TrendMap = TVRD(TRUE=1)
		Result = DIALOG_WRITE_IMAGE (TrendMap,FILE = PlaceCounty+'.bmp',TITLE='����Ϊ:' $
		                            ,/WARN_EXIST,TYPE='.BMP',PATH=DC_PathSetting())

		WSET, OldWin
		;-----------------------------------------------------------------------
		Info=DIALOG_MESSAGE('����ɹ�!',TITLE='��ʾ',/INFORMATION)

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='saveToDatabase'): BEGIN
		IF (*PA).IsYieldYear EQ 0 THEN BEGIN
			INFO = DIALOG_MESSAGE('����������ò���ȷ,���������ü���������!',TITLE='����')
			RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).Equation_Text,GET_VALUE = Equation
		WIDGET_CONTROL,(*PA).Correlation_Text,GET_VALUE = Correlation
		WIDGET_CONTROL,(*PA).F_TEXT,GET_VALUE = F
		WIDGET_CONTROL,(*PA).SMR_TEXT,GET_VALUE = SMR
		WIDGET_CONTROL,(*PA).TrendYield_TABLE,GET_VALUE = YieldData,GET_UVALUE=YearName

		IF ARRAY_EQUAL(YieldData,'') THEN BEGIN
		  Info=DIALOG_MESSAGE('����û�н������Ʋ���ģ��!',/INFORMATION,TITLE='��ʾ')
		  RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).CountyName,GET_VALUE=SelectCounty
		County = STRSPLIT(SelectCounty[0],/EXTRACT)

		IF (*PA).Modetype EQ 'linear' THEN Modetype=1 ELSE Modetype=2
		CASE (*PA).SmootID OF
			0: SmootID = 3
			1: SmootID = 5
			2: SmootID = 7
			ELSE:
		ENDCASE

		IF (*PA).YieldType EQ 0 THEN BEGIN
			Table = 'COUNTY_TREND_YIELD_PARAMETER'
			TrendTable = 'COUNTY_CROP_TREND_YIELD'
			DistrictCode = "' and county_code='"
		ENDIF ELSE BEGIN
			Table = 'AGROSTATION_TREND_YIELD_PARAMETER'
			TrendTable = 'AGROSTATION_CROP_TREND_YIELD'
			DistrictCode = "' and AgroMeteoStation_CODE='"
		ENDELSE

		Equation_Coe = STRTRIM((*PA).Equation_Coe,2)

		Sqlstr0='delete from '+Table+" where Crop_ID='"+(*PA).Cropid + DistrictCode $
				+ County[0]+"'"
		Sqlstr1='insert into '+Table+" values('"+County[0]+"','"+(*PA).Cropid $
		       +"',"+STRTRIM(Modetype,2)+','+Equation_Coe[0]+','+Equation_Coe[1]+','+Correlation[0] $
		       +','+F[0]+','+SMR[0]+','+STRTRIM(SmootID,2)+','+(*PA).StartYear+','+(*PA).EndYear+',' $
		       +STRMID(SYSTIME(),3,/REVERSE_OFFSET)+')'

		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

		DBobj->ExecuteSQL,Sqlstr0
		DBobj->ExecuteSQL,Sqlstr1

		YearName = STRMID(YearName,0,4)          ;ȥ��"��"��,�õ����
		TrendYield = YieldData[*,2]

		Sqlstr0='delete from '+TrendTable+" where Crop_ID='"+(*PA).Cropid + DistrictCode $
				+ County[0]+"'"
		DBobj->ExecuteSQL,Sqlstr0

		FOR i=0,N_ELEMENTS(YearName)-1 DO BEGIN  ;�����Ʋ������
			Sqlstr1='insert into '+TrendTable+" values('"+County[0]+"','"+(*PA).Cropid $
			       +"',"+STRTRIM(TrendYield[i,0],2)+','+YearName[i]+')'

			DBobj->ExecuteSQL,Sqlstr1

		ENDFOR

		Info=DIALOG_MESSAGE('������!',/INFORMATION,TITLE='��ʾ')
		log, '����Ԥ��-���Ʋ���', 1
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Trend_DRAW'): BEGIN

;;		IF EVENT.TYPE EQ 2 THEN BEGIN
;;		   WIDGET_CONTROL,EVENT.ID,TOOLTIP=STRTRIM(EVENT.X,2)+','+STRTRIM(EVENT.Y,2)
;;		ENDIF

	END

	WIDGET_INFO(Event.top, FIND_BY_UNAME='BatchBU'): BEGIN
		IF (*PA).IsBlankList THEN BEGIN
			INFO = DIALOG_MESSAGE('��ߵ��б�Ϊ��,û�п�ѡ��������!',/INFOR,TITLE='��ʾ')
			RETURN
		ENDIF

		CropId = (*PA).CROPID
		CalcYear = FIX((*PA).CalcYear)
		WIDGET_CONTROL,(*PA).CountyList,GET_UVALUE=CountyCode   ;ע��CountyCode�ǽṹ������.
		NumCounty = N_ELEMENTS(CountyCode)
		NoComputedID = INTARR(1) & Num = 0

		IF (*PA).YieldType EQ 0 THEN BEGIN
			Table = "COUNTY_TREND_YIELD_PARAMETER where crop_id='"
			TrendTable = 'COUNTY_CROP_TREND_YIELD'
			DistrictCode = "' and county_code='"
		ENDIF ELSE BEGIN
			Table = "AGROSTATION_TREND_YIELD_PARAMETER where crop_id='"
			TrendTable = 'AGROSTATION_CROP_TREND_YIELD'
			DistrictCode = "' and AgroMeteoStation_CODE='"
		ENDELSE

		FOR i = 0,NumCounty-1 DO BEGIN
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=i
			WIDGET_CONTROL,(*PA).CountyName,SET_VALUE=CountyCode[i].CODE+' '+CountyCode[i].NAME

			SQLstr = 'select Model_type,Coefficient_A,Coefficient_B from '+Table+CropId $
					+DistrictCode+CountyCode[i].(0)+"'"
			Para = DC_GetdataFromDB_Str(3 ,SQLstr,N_RECORDS = NumReocrd)   ;���ص���3��һ�е�����

			IF NumReocrd EQ 0 THEN BEGIN
				Num = Num+1
				NoComputedID = [NoComputedID,i]
				GOTO,Next
			ENDIF

			CASE FIX(Para[0]) OF
				1: TrendYield = FLOAT(Para[1])+FLOAT(Para[2])*CalcYear

				2: TrendYield = FLOAT(Para[1])+FLOAT(Para[2])*ALOG10(CalcYear)
				ELSE:
			ENDCASE

			COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
			Sqlstr0='delete from '+TrendTable+" where crop_id='"+CropId+DistrictCode $
					+ CountyCode[i].(0)+"' AND year = "+(*PA).CalcYear
			Sqlstr1='insert into '+TrendTable+" values('"+CountyCode[i].(0)+"','"+CropId $
			       +"',"+STRTRIM(TrendYield,2)+','+(*PA).CalcYear+')'
			DBobj->ExecuteSQL,Sqlstr0
			DBobj->ExecuteSQL,Sqlstr1

			Next:

		ENDFOR

		IF Num NE 0 THEN BEGIN
			NoComputedID = NoComputedID[1:*]
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_SELECT=NoComputedID
			WIDGET_CONTROL,(*PA).CountyList,SET_LIST_TOP = NoComputedID[0]
			INFO = DIALOG_MESSAGE(['�������!','�б��б�ѡ�е���Ϊû�в����������!'],/INFOR,TITLE='��ʾ')
			log, '����Ԥ��-���Ʋ���', 0
		ENDIF ELSE BEGIN
			INFO = DIALOG_MESSAGE('����ɹ����!',/INFOR,TITLE='��ʾ')
			log, '����Ԥ��-���Ʋ���', 1
		ENDELSE

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '���Ʋ�������', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;	ONLINE_HELP,BOOK='HELP\HELP.CHM','���Ʋ�������'
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='quit_bu'):begin
		common_log,'�ر����Ʋ�������'
		WIDGET_CONTROL,EVENT.TOP,/DESTROY
	end

  ELSE:
  ENDCASE

END

;&&&&&&&&&&&&&&&&&&&&&&�������Ʒ��������洰��%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO DC_TrendYield, GROUP_LEADER=wGroup

	common_log,'�������Ʋ�������'

	IF ( XREGISTERED('DC_TrendYield') NE 0 ) THEN RETURN

	TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
			,XOFFSET=200 ,YOFFSET=200,TITLE='���Ʋ�������', TLB_FRAME_ATTR=1 $
			,XPAD=1 ,YPAD=1,/COLUMN,TAB_MODE=1);,/TLB_KILL_REQUEST_EVENTS)
	;------------------------------------------------------
	ConditionBase = Widget_Base( TLB_BASE,XPAD=1,YPAD=1,SPACE=3,SCR_XSIZE=664,SCR_YSIZE=28 $
								,/ROW,/FRAME,/ALIGN_CENTER )

;		Province = ['������','�����','�ӱ�','ɽ��','���ɹ�','����','����','������','�Ϻ���','����' $
;					,'�㽭','��΢','����','����','ɽ��','����','����','����','�㶫','����','����' $
;					,'������','�Ĵ�','����','����','����','����','����','�ຣ','����','�½�']
		ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
					'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

		Crop = ['��С��','��С��','��  ��','��  ��','��  ��','������','������','��  ��']
	    CropIDList = ['11','12','21','22','23','31','32','41']				;Crop��CropIDListӦ��Ӧ
	    ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;��������ݱ仯,ϵͳ���б��е����Ҳ��仯.

		;====��������ӣ�20070906====================================
;		CalcYear=strmid(systime(),3,4,/REVERSE_OFFSET)
;		temp = WHERE(ARRAY_YEAR EQ CalcYear,Count)
		;=======================================================================

;		ProDroplist  = Widget_Droplist(ConditionBase,UNAME='ProDroplist',TITLE='ʡ��:',SCR_XSIZE=100)   ;��/frame��frame=1����ͬ��
		CropDroplist = Widget_Droplist(ConditionBase,UNAME='CropDroplist',TITLE='����:',SCR_XSIZE=100)
		SPACE=WIDGET_BASE(ConditionBase,SCR_XSIZE=15)
		YearDroplist = Widget_Droplist(ConditionBase,UNAME='YearDroplist',TITLE='�������:',SCR_XSIZE=110)
		SPACE=WIDGET_BASE(ConditionBase,SCR_XSIZE=15)
		;====��������ӣ�20070906====================================
;		IF Count NE 0 THEN BEGIN
;			WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_droplist_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)
;		ENDIF ELSE BEGIN
;			WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_droplist_SELECT=YearNum-1
;			CalcYear = ARRAY_YEAR[YearNum-1]
;		ENDELSE
		;=======================================================================

		YieldType = CW_BGROUP(ConditionBase, UNAME='YieldType',['��','վ��'],/ROW,/EXCLUSIVE $
		,SPACE=0, LABEL_LEFT='��������:',XPAD=0,YPAD=0,/RETURN_INDEX,YSIZE=14)
		SPACE=WIDGET_BASE(ConditionBase,SCR_XSIZE=15)
		CheckParaBase = Widget_Base(ConditionBase,XPAD=0,YPAD=0,/NONEXCLUSIVE,/ALIGN_CENTER)
		ParaBu = Widget_Button(CheckParaBase,UNAME='ParaBu',VALUE='���Ʋ�������',/ALIGN_CENTER)
		SPACE=WIDGET_BASE(ConditionBase,SCR_XSIZE=15)
		BatchBU = Widget_Button(ConditionBase,UNAME='BatchBU',SCR_XSIZE=58,SCR_YSIZE=20 $
								,VALUE='������',TOOLTIP='�����������ص����Ʋ���',/ALIGN_CENTER)
	;-------------------------------------------------------

	LargeBase = Widget_Base( TLB_BASE,XPAD=1,YPAD=1,SPACE=3,/BASE_ALIGN_TOP,/ROW)
	;��ѡ��
	CountyBase = Widget_Base(LargeBase,XPAD=0,YPAD=0,/BASE_ALIGN_LEFT,/COLUMN,/FRAME,SCR_YSIZE=366)
		TitleBase = Widget_Base(CountyBase,XPAD=0,YPAD=0,/BASE_ALIGN_LEFT,/ROW,SPACE=0)
			TitleName  = Widget_Label(TitleBase,UNAME='TitleName',VALUE='��:',SCR_XSIZE=30,/ALIGN_CENTER)
			CountyName = CW_FIELD(TitleBase,UNAME='CountyName',TITLE='',XSIZE=15,/NOEDIT)   ;����Ӧ����Widget_Text,���ᵼ��̫��ظĶ�.����û�и�
		CountyList = Widget_List(CountyBase,UNAME='CountyList',SCR_XSIZE=130,SCR_YSIZE=330,/MULTIPLE,/ALIGN_CENTER)

	;-------------------------�ұ�ģ�ⲿ��%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%------------------
	RigthBase = Widget_Base( LargeBase,UNAME='TrendParaBase'  $
	      ,SPACE=3 ,XPAD=0 ,YPAD=0,COLUMN=1,/BASE_ALIGN_LEFT)

	TrendParaBase = Widget_Base(RigthBase,UNAME='TrendParaBase'  $
	      ,SPACE=3 ,XPAD=0 ,YPAD=0,COLUMN=1,/BASE_ALIGN_LEFT,SENSITIVE=0)
	;---------------����ϲ�����BASE-*******************************----------------------------
	TrendEquation_base = Widget_Base(TrendParaBase, $
	      UNAME='TrendEquation_base' ,SPACE=3,ROW=1,XPAD=0,YPAD=0)
			  ;---------------------DRAW��ߵ�BASE--***********************----------------
			  EqutionDes_base = Widget_Base(TrendEquation_base,  $
			      UNAME='EqutionDes_base' ,SPACE=3,XPAD=0,YPAD=0,COLUMN=1,/BASE_ALIGN_LEFT)
			   Base_width=140
			  ;--------------------------------------------------------
			  code_BASE = Widget_Base(EqutionDes_base, UNAME='code_BASE' ,FRAME=1  $
			     ,SPACE=0 ,XPAD=0 ,YPAD=0 ,/COLUMN,/BASE_ALIGN_LEFT,SCR_XSIZE=Base_width)
			  StartYearDr = Widget_Droplist(code_BASE,UNAME='StartYearDr',TITLE='������ʼ���: ',VALUE=['2005'])
			  EndYearDr = Widget_Droplist(code_BASE,UNAME='EndYearDr',TITLE='�����������: ',VALUE=['2005'])

			  ;-----------------------ѡ��ģ������BASE----------------------------------
			  EqutionType_BASE = Widget_Base(EqutionDes_base, UNAME='EqutionType_BASE'  $
			      ,FRAME=1 ,SPACE=1 ,XPAD=1 ,YPAD=1 ,COLUMN=1,/BASE_ALIGN_LEFT,SCR_XSIZE=Base_width)

			  EqutionType_LABEL = Widget_Label(EqutionType_BASE,  $
			      UNAME='EqutionType_LABEL',/ALIGN_LEFT ,VALUE='ѡ��ģ�����ͣ�')
			  ;--------------------
			  Modetypel_BASE = Widget_Base(EqutionType_BASE, UNAME='Modetypel_BASE' $
			       ,SPACE=3 ,XPAD=30,YPAD=0 ,/COL,/EXCLUSIVE)

			  linear_BUTTON = Widget_Button(Modetypel_BASE, UNAME='linear_BUTTON'  $
			      ,/ALIGN_LEFT ,VALUE='����ģ��')

			  lg10_BUTTON = Widget_Button(Modetypel_BASE,  $
			      UNAME='lg10_BUTTON' ,/ALIGN_LEFT ,VALUE='����ģ��')              ;10Ϊ�׵ĳ��ö���.
			 ;------------------------------------------------
			 SMOOTH_BASE = Widget_Base(EqutionDes_base, UNAME='SMOOTH_BASE'  $
			      ,FRAME=1,SCR_XSIZE=Base_width,SPACE=1,XPAD=1,YPAD=1,/BASE_ALIGN_LEFT,/COL)

			 Smoot_Label=Widget_Label(SMOOTH_BASE, UNAME='Smoot_Label',VALUE='����ƽ����ʽ��')
			 values=['����ƽ��','���ƽ��','�ߵ�ƽ��']
			 SmootWay = CW_BGROUP(SMOOTH_BASE, UNAME='SmootWay',values, /COLUMN, /EXCLUSIVE $
			    ,SPACE=0,XPAD=30,YPAD=0,/RETURN_INDEX,/SCROLL,X_SCROLL_SIZE=200)
			 ;-----------------------������ͼ------------------------------------
			 Trend_DRAW = Widget_Draw(TrendEquation_base, UNAME='Trend_DRAW'  $
			      ,FRAME=1 ,SCR_XSIZE=376 ,SCR_YSIZE=216, RETAIN =2,/MOTION_EVENTS)
	 ;----------------------���Ʒ��̲���---------------------------------
	Eqution_R_BASE = Widget_Base(TrendParaBase, UNAME='Eqution_R_BASE'  $
	      ,FRAME=1,SPACE=1,XPAD=1,YPAD=1,/BASE_ALIGN_TOP,/ROW)

			  labelwidth=80 & textwidth=40
			  Equation_LABEL = Widget_Label(Eqution_R_BASE,  $
			      UNAME='Equation_LABEL' ,SCR_XSIZE=labelwidth $
			      ,/ALIGN_LEFT ,VALUE='���Ʋ������̣�')

			  Equation_Text = Widget_Text(Eqution_R_BASE, UNAME='Equation_Text'  $
			      ,FRAME=1  ,SCR_XSIZE=140 )
			;--------------
			  R2_LABEL = Widget_Label(Eqution_R_BASE, UNAME='R2_LABEL' $
			       ,VALUE='���ϵ��R��')

			  Correlation_Text = Widget_Text(Eqution_R_BASE, UNAME='Correlation_Text'  $
			      ,/FRAME,SCR_XSIZE=textwidth)
			;--------------
			  F_LABEL = Widget_Label(Eqution_R_BASE, UNAME='F_LABEL' $
			       ,VALUE='F���飺')

			  F_TEXT = Widget_Text(Eqution_R_BASE, UNAME='F_TEXT'  $
			      ,/FRAME,SCR_XSIZE=textwidth)
			;--------------
			  SMR_LABEL = Widget_Label(Eqution_R_BASE, UNAME='SMR_LABEL' $
			       ,VALUE='��׼��:')

			  SMR_TEXT = Widget_Text(Eqution_R_BASE, UNAME='SMR_TEXT'  $
			      ,/FRAME,SCR_XSIZE=textwidth)

	;---------------------������-------------------------------------------
	TrendYield_TABLE = Widget_Table(TrendParaBase,UNAME='TrendYield_TABLE',SCR_XSIZE=521 $
	      ,SCR_YSIZE=109  ,XSIZE=20 ,YSIZE=4,/FRAME,/DISJOINT_SELECTION);,BACKGROUND_COLOR=[255,0,0])

	 ;---------------------------��ť���ڵ�BASE---------------------------------------
    SAVE_BASE = Widget_Base(TLB_BASE, UNAME='SAVE_BASE' ,FRAME=1  $
      ,SCR_YSIZE=35 ,SCR_XSIZE=664,SPACE=65,XPAD=70,YPAD=1 ,ROW=1,/ALIGN_CENTER,/BASE_ALIGN_CENTER)

	  Button_width=80 & Button_height=22 ;���尴ť��Ⱥ͸߶�
	  SaveTo=Widget_Base(SAVE_BASE,XPAD=0,YPAD=0 ,ROW=1,SPACE=65,SENSITIVE=0,/ALIGN_CENTER,/BASE_ALIGN_CENTER)

		  saveToDatabase=Widget_Button(SaveTo, UNAME='saveToDatabase',VALUE='���' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height,TOOLTIP='���浽���ݿ���'); $
		;	         ,EVENT_PRO='SaveTrendData_To_database')
		  saveToCompute=Widget_Button(SaveTo, UNAME='saveToCompute',VALUE='����' $
		         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height,TOOLTIP='���浽��������') ;$
		;	         ,EVENT_PRO='SaveTrendData_To_me')

	  Help_bu=Widget_Button(SAVE_BASE, UNAME='Help_bu',VALUE='����' $
	         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

	  quit_bu=Widget_Button(SAVE_BASE, UNAME='quit_bu',VALUE='�ر�' $
	         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)
	  ;-----------------------------------------------------------------------------
	Widget_Control, /REALIZE, TLB_BASE

	PRO_COMMON_DC

	COMMON DC_BLOCK,NewCropID		;���ù���ģ��,ֻ��������
									;���������ù���ģ��,��ʡ��ϵͳͨ�õ�

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�
	CropID = '12';NewCropID
	CalcYear = '2006';strmid(systime(),3,4,/REVERSE_OFFSET)	;�������޸ģ�20070906

;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,CropDroplist,SET_VALUE =Crop,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)
	WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)

	DC_TrendCountyOrSation,CountyList,CountyName,0,ProCode,CropID,NumReocrd = NumReocrd

	WIDGET_CONTROL,CountyList,GET_UVALUE=CountyCode
	IF NOT ARRAY_EQUAL(CountyCode ,'',/NO_TYPECONV) THEN BEGIN
		WIDGET_CONTROL,CountyList,SET_LIST_SELECT=0
		WIDGET_CONTROL,CountyName,SET_VALUE=CountyCode[0].CODE+' '+CountyCode[0].NAME
	ENDIF

	WIDGET_CONTROL,TrendYield_TABLE,SET_TABLE_SELECT=[-1,-1]
	WIDGET_CONTROL,linear_BUTTON,SET_BUTTON=1
	WIDGET_CONTROL,YieldType,SET_VALUE=0 						  ;����Ĭ�ϵĲ�������Ϊ�ز���
	WIDGET_CONTROL,SmootWay,SET_VALUE=0                           ;����Ĭ�ϵ�ƽ����ʽΪ"����ƽ��"
	WIDGET_CONTROL,quit_bu,/INPUT_FOCUS

    Widget_Control,Trend_DRAW,GET_VALUE=Owindow
	WSET,Owindow
    ERASE,COLOR=!D.N_COLORS-1

   STATE = { $
;            ProNameList			:	Province		,$				;ʡ���б�
            ProIDList			:	ProIDList		,$				;ʡID�б�
            ProID				:	ProCode			,$				;��ѡʡID
            CropIDList			:	CropIDList		,$
            CropNameList		:	Crop			,$      		;�������б�
            CropID				:	CropID			,$
            CalcYear			:	CalcYear		,$
            IsYieldYear  	    :   1			 	,$         		;��ʶ��������Ƿ�������ȷ,Ĭ����ȷΪ1
            IsBlankList  	    :   NumReocrd EQ 0 	,$         		;��ʶList���ֵ�Ƿ�Ϊ��
            IsCalPara  		    :   0        	 	,$         		;��ʶ�Ƿ�Ҫ���в�������
         	StartYear			:	'2005'			,$
         	EndYear				:	'2005'    		,$
         	Modetype			:	'linear'		,$				;Ĭ��Ϊ���ö���ģ��
         	YieldType			:	0				,$				;Ĭ��Ϊ�ز�������.
         	SmootID				:	0				,$				;Ĭ��Ϊ����ƽ��
			StartYearDr 		:	StartYearDr		,$
			EndYearDr 			:	EndYearDr		,$
			YearHaveData		:	PTR_NEW()		,$				;��ʶ��ѡ����Ӧ�����в������������.
			ActualYield			:	PTR_NEW()		,$				;��ʶ��staryear��endyear��Ӧ�Ĵӿ�����ȡ��ʵ�ʲ���..
         	TrendParaBase		:	TrendParaBase	,$				;���в���ģ���BASE
         	SaveTo				:	SaveTo			,$				;�������水ť���ڵ�BASE
			TitleName			:	TitleName		,$
			CountyList			:	CountyList		,$
			CountyName			:	CountyName		,$
			Trend_DRAW			:	Trend_DRAW		,$
         	Equation_Text		:	Equation_Text	,$
         	Equation_Coe		:	FLTARR(2)		,$   			;���̲���(������/ϵ��)
         	Correlation_Text	:	Correlation_Text,$
         	F_TEXT				:	F_TEXT			,$
         	SMR_TEXT			:	SMR_TEXT		,$
         	TrendYield_TABLE	:	TrendYield_TABLE,$
         	ARRAY_YEAR			: 	ARRAY_YEAR  	 $				;�������DROPlist���õ��������
            }

    PA = PTR_NEW(STATE, /NO_COPY)

    WIDGET_CONTROL, TLB_BASE, SET_UVALUE=PA

  XManager, 'DC_TrendYield', TLB_BASE, CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
