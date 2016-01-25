
;$$$$$$$$$$$$$$$$$$$$$»­Öù×´Ìõ$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
PRO FZ_Draw_BAR_PLOT,drawID $			;DRAW×é¼þµÄIDÖµ,¼´GET_VALUE
					,AnalysisData ;$		;Òª»­µÄÊý¾Ý
;					,XLabel	$			;XÖáµÄ±êÇ©é
;					,LINE = Line $		;Ö»»­ÏßÐÔÍ¼,Ä¬ÈÏÎªÖ»»­Öù×´Í¼
;					,CHARTLINE = ChartLine  ;Í¬Ê±»­Ïß×´ºÍÖù×´Í¼
;µ÷ÓÃ:FZ_Draw_BAR_PLOT,drawID,AnalysisData    ,XLabel[,/Line,/ChartLine]
;±¾³ÌÐòÖ»ÊÊÓÃÓÚ"²úÁ¿ÈÚºÏÄ£¿é"µÄÊ¹ÓÃ,¶ÔÓÚÆäËû³ÌÐò,¿ÉÄÜ²»ÊÊÓÃ.
 	IF (N_PARAMS() NE 2) THEN MESSAGE, 'Incorrect number of arguments'

 	  ON_ERROR, 2						;return to caller
	  PlotNum = N_ELEMENTS(AnalysisData)    ;¸öÊý

	  DEVICE,GET_DECOMPOSED=old_color     ;»ñÈ¡µ±Ç°DECOMPOSEDÖµ
;      DEVICE,GET_CURRENT_FONT=oldFont
;      DEVICE,SET_FONT='ËÎÌå',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;ÓÃIDLÌá¹©ºó±¸´æ´¢,Ê¹ÓÃÑÕÉ«²éÑ¯±í(Í£ÓÃÑÕÉ«·Ö½â¹¦ÄÜ),
		r=[0,255,  0,  0,255,255]   	  ;ÒÀ´ÎÎªºÚ\ºì\ÂÌ\À¶\»Æ\°×
		g=[0,  0,255,  0,255,255]
		b=[0,  0,  0,255,  0,220]
 		TVLCT, r, g, b   ;È±Ê¡µÚËÄ¸öÊ¡Êý,ÔòÊ¹ÑÕÉ«±íÖÐË÷ÒýºÅÎª0,1,2,3,4,5µÄÑÕÉ«ÎªÏàÓ¦µÄRGB×éºÏ

;;	TITLE='"'+EstiCounty[1,0]+'"'+'¸÷¹ÀËãµ¥²ú±È½ÏÍ¼'
    OldWin = !D.WINDOW     			   ;±£´æÏµÍ³µÄ´°¿Ú
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
	!P.COLOR = 800   ;ÖáµÄÑÕÉ«
    !P.CHARSIZE = 0.8

;    Colors = INTARR(PlotNum)
;	FOR I = 0, PlotNum-1 DO Colors[I]= I ;(2*I)+100

   	WSET, drawID
	PlotData = FLOAT(AnalysisData)

	Nsum = 1
	Psym = 3   ;(µã±êÊ¶)
    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,CHARSIZE=1,XRANGE=[1,PlotNum/Nsum] $
         ,POSITION=[0.09,0.20,0.96,0.90],NSUM=Nsum $ ,XTICKNAME=[' '] $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
		 ,YTICKLEN=1 ,YTICKS=4,YRANGE=[0,MAX(PlotData)+100] $
		 ,XTICKLEN=1 				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý

XYOUTS,1,0.5,'¸´ÖÖÖ¸Êý/ÏØ±àÂë', /DEVICE,color=1
	OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-1

;	Psym = 3   ;(µã±êÊ¶)
;	IF KEYWORD_SET(ChartLine) THEN BEGIN   ;»­Öù×´ºÍÏß×´Í¼
;	    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
;	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
;			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý
;		OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;ÓÃ°×ÑÕÉ«½«PLOT»­µÄÏß¸²¸Çµô,»¹Òª×¢Òâ¸øXRANGE¸ºÖµ,²»È»»­Í¼Ð§¹û²î
;
;	    FirstOFFSET = 0.3 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
;	    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
;			,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
;			,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT
;
;		OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-6
;	ENDIF ELSE BEGIN
;		IF KEYWORD_SET(Line) THEN BEGIN  ;Ö»»­Ïß×´Í¼
;		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
;		         ,POSITION=[0.09,0.28,0.96,0.93] ,XTICKNAME=[' ',XLabel,' '] $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)] $
;				 ,XTICKLEN=1 ; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý
;
;			OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-5
;		ENDIF ELSE BEGIN				;Ö»»­Öù×´Í¼
;		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum-1] $
;		         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
;				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý
;
;			OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;ÓÃ°×ÑÕÉ«½«PLOT»­µÄÏß¸²¸Çµô,»¹Òª×¢Òâ¸øXRANGE¸ºÖµ,²»È»»­Í¼Ð§¹û²î
;		    FirstOFFSET = 0.8 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
;		    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
;				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
;				,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT
;		ENDELSE
;	ENDELSE

	!P.BACKGROUND = OldBackup		;»¹Ô­
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor
	!Y.TICKS = OldYticks

	DEVICE,DECOMPOSED=old_color   ;·µ»ØÔ­À´µÄDECOMPOSEDÖµ,ÒòÎª×Ô¶¨Òåº¯ÊýMyColor¸Ä±äÁË,Ðë»¹Ô­.

	WSET, OldWin				;»¹Ô­Ô­À´´°¿Ú.

END
;******Í³¼Æ¸´ÖÖÖ¸Êýµ½¸÷ÏØ************************************
FUNCTION FZ_StatisticCropping,StaFile,STATUS = status
	;StaFile  ±»Í³¼ÆÎÄ¼þ;status¼ÆËãÊÇ·ñ³É¹¦µÄ×´Ì¬
	;µ÷ÓÃÐÎÊ½:Re = FZ_StatisticCropping(StaFile,[STATUS=status]) ×¢ÒâÖ»ÄÜÓÃstatus
	;·µ»ØÖµ: ¸÷ÏØ½á¹¹Ìå
	;Ëã·¨:Sum(Pix*W)/sum(W)  PixÏñÔªÖµ;WÎªÈ¨ÖØ

	CountyRaster = 'data_grid\county_raster'

	StaData    = DC_Read_ENVIData(StaFile,SUCCESSSTATUS = StatusS,Description='¸´ÖÖÖ¸ÊýÎÄ¼þ')
	IF NOT StatusS THEN BEGIN		;StaData ¸¡µãÖµ
		STATUS=StatusS
		RETURN,0
	ENDIF

	CountyData= DC_Read_ENVIData(CountyRaster,SUCCESSSTATUS = StatusC,Description='ÏØÕ¤¸ñÎÄ¼þ')
	IF NOT StatusC THEN BEGIN
		STATUS=StatusC
		RETURN,0
	ENDIF

	Sinfo = DC_ReadHead_file(StaFile)
	Cinfo = DC_ReadHead_file(CountyRaster)
	IF FIX(Sinfo.samples) NE FIX(Cinfo.samples) OR   $
	   FIX(Sinfo.lines)   NE FIX(Cinfo.lines) THEN BEGIN
		Info = DIALOG_MESSAGE('ÓÃÓÚÍ³¼ÆµÄ»ù´¡Êý¾ÝÓë¸´ÖÖÖ¸ÊýÎÄ¼þ´óÐ¡²»Ò»ÖÂ,Í³¼ÆÊ§°Ü!',TITLE='¾¯¸æ')
		STATUS=0
		RETURN,0
	ENDIF

	CountyID = CountyData[UNIQ(CountyData,SORT(CountyData))]    ;µÃµ½Î¨Ò»µÄÏØIndex (²»ÊÇÏØ´úÂë)
	CountyID = CountyID[WHERE(CountyID NE 0)]					;È¥µô0 Index

	Result = REPLICATE({CountyID:0,CropIndex:0.0},N_ELEMENTS(CountyID))

	FOR I=0,N_ELEMENTS(CountyID)-1 DO BEGIN
		County  = WHERE(CountyData EQ CountyID[I])
		DataV   = StaData[County]*1D		;BYTEÐÍ,ºó×ªÎªË«¾«¶È,Àí½âhelp,255B*255B
		Croping = WHERE(DataV GT 0,Count)
		IF Count NE 0 THEN BEGIN			;Ôö¼ÓÁËÅÐ¶Ï20070309
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
;------Çå³ýÊý¾Ý-----------------------------
PRO FZ_ClearFZData,TLB

    Widget_Control,TLB,GET_UVALUE=PA

    Widget_Control,(*PA).CroppingTable,GET_VALUE=Yield & Yield[*,*]=''
    Widget_Control,(*PA).CroppingTable,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1,-1,-1]

    Widget_Control,(*PA).FilePathText,SET_VALUE=''
    Widget_Control,(*PA).TableLabel,SET_VALUE='Í³¼ÆµÄ¸÷ÏØ¸´ÖÖÖ¸Êý½á¹û'

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
;888888888888888888888Ö÷½çÃæµÄ²¿·ÖÊÂ¼þ´¦Àí8888888888888888888888888888888888888888888888888888
PRO FZ_Statistic_event, Event

	on_error,2

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  widget_info(Event.id, /trLee_root) : event.id)

;     CATCH, Error_status               ;½ØÈ¡´íÎó.
;     IF Error_status NE 0 THEN BEGIN
;        infomation=DIALOG_MESSAGE(['ÔËËãÖÐÖ¹,Ô­ÒòÈçÏÂ:',[!ERROR_STATE.MSG]],TITLE='´íÎó',/ERROR)
;        CATCH, /CANCEL
;        RETURN                    ;Èç¹û²»¼ÓÉÏÕâ¾ä,Ôò»á¼ÌÐøÖ´ÐÐÏÂÃæµÄÓï¾ä,ÈÔ»á³öÏÖ´íÎó.
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

        SelectedFile = DIALOG_PICKFILE(TITLE='Ñ¡Ôñ¸´ÖÖÖ¸ÊýÎÄ¼þ',GET_PATH=SelectedPath,/MUST_EXIST $
                            	  ,FILTER ='*.hdr;*.HDR',PATH = v_fz_out_path $ ;PATH = DC_PathSetting(/READPATH2)
                            	  ,DIALOG_PARENT=event.id)  ;¼ÓÁËÉ¸Ñ¡ºó×º

		IF SelectedFile EQ '' THEN RETURN

		RelaFile = STRMID(SelectedFile,STRPOS(SelectedFile,'\',/REVERSE_SEARCH)+1)  ;Ïà¶ÔÍ·ÎÄ¼þ.
        RelaDataFile = STRMID(RelaFile,0,STRPOS(RelaFile,'.',/REVERSE_SEARCH))		;Ïà¶ÔÊý¾ÝÎÄ¼þ.

		SelectedDataFile = STRMID(SelectedFile,0,STRPOS(SelectedFile,'.',/REVERSE_SEARCH)) ;²»´øhdrµÄÈ«Â·¾¶ÎÄ¼þ.

		IF ~FILE_TEST(SelectedDataFile) THEN BEGIN
    		Info = DIALOG_MESSAGE('ÕÒ²»µ½Ö¸¶¨ÎÄ¼þ"'+RelaFile+'"ÏàÓ¦µÄÊý¾ÝÎÄ¼þ"'+RelaDataFile+'"!',/INFORMATION,TITLE='ÌáÊ¾')
    	 	RETURN
        ENDIF

		WIDGET_CONTROL,(*PA).FilePathText,SET_VALUE = SelectedFile

		SelectedDataFile = STRMID(SelectedFile,0,STRPOS(SelectedFile,'.',/REVERSE_SEARCH))

;		Data = DC_Read_ENVIData(SelectedFile,SUCCESSSTATUS=Status,DESCRIPTION='¸´ÖÖÖ¸ÊýÎÄ¼þ')
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

        IF NOT OBJ_VALID((*PA).oView) THEN RETURN   ;Èç¹ûÊÇ¿Õ¶ÔÏóÔò·µ»Ø.

;		ParaInfo = (*PA).ProjectPara    			;µÃµ½Ê¡·¶Î§ÄÚµÄÍ¶Ó°²ÎÊý
;        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;¼´·µ»ØµÄ²»ÊÇ½á¹¹Ìå,¶øÊÇ¿Õ.
;        	ProviceName = (*PA).province[WHERE((*PA).proIDlist EQ (*PA).proID)]
;        	PRMPT = DIALOG_MESSAGE('Ã»ÓÐ'+ProviceName+'µÄ»ù´¡²ÎÊýÐÅÏ¢,Çë²é¿´ÏàÓ¦µÄ²ÎÊýÉèÖÃÎÄ¼þ!')
;        	RETURN
;        ENDIF

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
            ImageSize = SIZE(Imagedata,/DIMENSIONS)

		    (*PA).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]  & ySize = ImageSize[1] ;Ó°Ïñ¿í¸ß
			Zoom = MIN([1.*viewDim[0]/xSize,1.*viewDim[1]/ySize])
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return

;            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;Ö®ËùÒÔ°´ÐÐ·´×ª,ÊÇÒòÎªDRAW×é¼þ×ø±êÊÇ´Ó×óÏÂ½Ç¿ªÊ¼µÄ.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(FLOAT(dataValue),2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
		ENDIF

    END

    Widget_Info(wWidget, FIND_BY_UNAME='StaCompute'): BEGIN
    	WIDGET_CONTROL,(*PA).FilePathText,GET_VALUE=FlieName
    	CroppingFile = FlieName[0]
		IF CroppingFile EQ '' THEN BEGIN
			INFO = DIALOG_MESSAGE('ÇëÏÈÑ¡Ôñ¸´ÖÖÖ¸ÊýÓ°ÏñÎÄ¼þ!',/INFOR,TITLE='ÌáÊ¾')
			RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).ChartDraw,GET_VALUE=MapID
		WSET,MapID
		ERASE,COLOR=!D.N_COLORS-1

	  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='Í³¼Æ',MESSAGE='ÕýÔÚÍ³¼Æ´¦ÀíÖÐ,ÇëÉÔºò!') ;ÐÂ½¨½ø¶ÈÌõ¶ÔÏó
		 progressTimer->START
	     progressTimer->UPDATE,(0.2 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ

	   ;  µÃµ½µÄRESULTÊÇ½á¹¹Ìå,ÆäÖÐCountyID(ÏØid,·ÇÏØ´úÂë),CropIndex(Æ½¾ùÖµ)¾ÍÊÇÐèÒªµÄ¸´ÖÖÖ¸Êý
		RESULT = FZ_StatisticCropping(CroppingFile,STATUS = status)
	    IF status EQ 0 THEN BEGIN
			progressTimer->DESTROY ;Ïú»Ù½ø¶ÈÌõ
;			OBJ_DESTROY,progressTimer
	    	RETURN   				;Í³¼Æ²»³É¹¦·µ»Ø
	    ENDIF

	    progressTimer->UPDATE, (0.70 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ

	    RowsNUM = N_elements(RESULT)

		StatisticCrop = [TRANSPOSE(STRTRIM(RESULT.CropIndex,2)),TRANSPOSE(STRTRIM(RESULT.CountyID,2))]
	 	ProName = (*PA).ProNameList[WHERE((*PA).ProIDlist EQ (*PA).ProID)]

	    SQLstr='select code,raster_value from COUNTY_CODE_RASTER where LEFT(code,2)='+(*PA).ProID
	    SQLstr='select b.code,b.raster_value,a.name from COUNTY_CODE a,('+SQLstr+') b where a.code=b.code'

		CountyCodeID = DC_GetdataFromDB_Str(3,SQLstr,N_RECORDS = n)  ;ÕâÒ»²½ÊÇÎª»ñµÃCounty_code_rasterµÄÏØ´úÂë.
		IF n EQ 0 THEN BEGIN
			progressTimer->DESTROY ;Ïú»Ù½ø¶ÈÌõ
;			OBJ_DESTROY,progressTimer
			INFO = DIALOG_MESSAGE('»ù´¡±íCOUNTY_CODE_RASTERÖÐÃ»ÓÐ'+ProName+'µÄÏØÂë¶ÔÓ¦Ë÷Òý!',TITLE='¾¯¸æ')
			RETURN
		ENDIF

	    progressTimer->UPDATE, (0.8 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ

	     CountyCropIndex=STRARR(3,1)
	     FOR i=0,RowsNUM-1 DO BEGIN
	         Temp=STRARR(3,1)
	         FOR j=0,n-1 DO BEGIN
	            IF FIX(StatisticCrop[1,i]) EQ fIX(CountyCodeID[1,j]) THEN BEGIN
	               Temp[0,0]=CountyCodeID[2,j]                 ;ÏØÃû
	               Temp[1,0]=CountyCodeID[0,j]				   ;ÏØÂë
	               Temp[2,0]=StatisticCrop[0,i]                ;¸´ÖÖÖ¸Êý

	               CountyCropIndex=[[CountyCropIndex],[Temp]]
	               BREAK
	            ENDIF
	         ENDFOR
	     ENDFOR

	    CountyCropIndex=CountyCropIndex[0:*,1:*]     ;¸ÃÊý×éÈýÁÐÎªÏØÃû\ÏØ´úÂë\¸´ÖÖÖ¸Êý.

	    progressTimer->UPDATE, (0.9 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ

		NewRowNum = N_ELEMENTS(CountyCropIndex)/3
   		ColHaed = ['ÏØÃû','ÏØ´úÂë','¸´ÖÖÖ¸Êý']
   		Widget_Control,(*PA).CroppingTable,TABLE_XSIZE=3,TABLE_YSIZE=NewRowNum,SET_VALUE=CountyCropIndex $
              ,COLUMN_LABELS=ColHaed,ALIGNMENT=2,SET_UVALUE=ColHaed $   ;ÓÃ»§ÖµÎªÁÐÃû
              ,ROW_LABELS=STRTRIM(INDGEN(NewRowNum)+1,2),COLUMN_WIDTHS=100

		Widget_Control,(*PA).TableLabel,SET_VALUE=strtrim((*PA).CalcYear,2)+'Äê'+ProName[0]+'¸÷ÏØ¸´ÖÖÖ¸ÊýÍ³¼Æ½á¹û'

		FZ_Draw_BAR_PLOT,MapID,CountyCropIndex[2,*]

		progressTimer->UPDATE, (1 * 100.0)  ;¸üÐÂ½ø¶ÈÌõChartDraw
;;		progressTimer->DESTROY ;Ïú»Ù½ø¶ÈÌõ
		OBJ_DESTROY,progressTimer

		INFO=DIALOG_MESSAGE('¸´ÖÖÖ¸ÊýÍ³¼ÆÍê³É',/INFO,TITLE='ÌáÊ¾')
		log, '¸´ÖÖÖ¸Êý-¿Õ¼äÍ³¼Æ', 0
    END

    Widget_Info(wWidget, FIND_BY_UNAME='saveToCompute'): BEGIN
		WIDGET_CONTROL,(*PA).CroppingTable,GET_VALUE=IndexValue,GET_UVALUE=TableHead
		IF ARRAY_EQUAL(IndexValue,'') THEN BEGIN
			INFO = DIALOG_MESSAGE('Í³¼Æ±íÖÐÃ»ÓÐÊý¾Ý!',/INFOR,TITLE='ÌáÊ¾')
			RETURN
		ENDIF

		SaveDATA = [[TableHead],[IndexValue]]
		ProName = (*PA).ProNameList[WHERE((*PA).ProIDlist EQ (*PA).ProID)]
		Filename = ProName[0]+strtrim((*PA).CalcYear,2)+'Äê¸÷ÏØ¸´ÖÖÖ¸ÊýÍ³¼Æ½á¹û'
		DC_SaveTextData,SaveDATA,EVENT.ID,FILENAME=Filename,/NOSavePath

    END

    Widget_Info(wWidget, FIND_BY_UNAME='saveTodb'): BEGIN
		WIDGET_CONTROL,(*PA).CroppingTable,GET_VALUE=IndexValue
		IF ARRAY_EQUAL(IndexValue,'') THEN BEGIN
			INFO = DIALOG_MESSAGE('Í³¼Æ±íÖÐÃ»ÓÐÊý¾Ý!',/INFOR,TITLE='ÌáÊ¾')
			RETURN
		ENDIF

		COMMON COMMON_BLOCK

	  	 progressTimer = Obj_New("ShowProgress",tlb,TITLE='Í³¼ÆÈë¿â',MESSAGE='¸´ÖÖÖ¸ÊýÍ³¼Æ½á¹ûÕýÔÚÈë¿âÖÐ,ÇëÉÔºò!') ;ÐÂ½¨½ø¶ÈÌõ¶ÔÏó
		 progressTimer->START

		CounytNum = N_ELEMENTS(IndexValue)/3
		FOR i=0,CounytNum-1 DO BEGIN
	        progressTimer->UPDATE, ((i+1)*1./CounytNum * 100.0)  ;¸üÐÂ½ø¶ÈÌõ
			Sqlstr1='delete from CROPPING_INDEX_COUNTY where Year='+strtrim((*PA).CalcYear,2)+ $
					" and county_code ='"+IndexValue[1,i]+"'"

			Sqlstr2="insert into CROPPING_INDEX_COUNTY values('"+IndexValue[1,i]+ $
					"',"+strtrim((*PA).CalcYear,2)+','+STRTRIM(IndexValue[2,i],2)+')'

			 DBobj->ExecuteSQL,Sqlstr1
			 DBobj->ExecuteSQL,Sqlstr2
		ENDFOR

;;		progressTimer->DESTROY ;Ïú»Ù½ø¶ÈÌõ
		OBJ_DESTROY,progressTimer

		INFO = DIALOG_MESSAGE('Íê³ÉÈë¿â!',/INFORMATION,TITLE='ÌáÊ¾')
		log, '¸´ÖÖÖ¸Êý-¿Õ¼äÍ³¼Æ', 1
    END
    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '¸´ÖÖÖ¸ÊýÍ³¼Æ', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('ÕÒ²»µ½°ïÖúÎÄµµ',title='¾¯¸æ')
			endelse
		end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,'¸´ÖÖÖ¸ÊýÍ³¼Æ'
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'¹Ø±Õ¸´ÖÖÖ¸ÊýÍ³¼Æ'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE
END
;&&&&&&&&&&&&&&&&&&&&&&²¨¶¯²úÁ¿·ÖÎöÖ÷½çÃæ´°¿Ú:²ÎÊý¼ÆËã·ÖÎö%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO FZ_Statistic, GROUP_LEADER=wGroup

	common_log,'Æô¶¯¸´ÖÖÖ¸ÊýÍ³¼Æ'
	IF ( XREGISTERED('FZ_Statistic') NE 0 ) THEN RETURN

	TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
			,XOFFSET=180 ,YOFFSET=200,TITLE='¸´ÖÖÖ¸ÊýÍ³¼Æ' $
			,XPAD=3, YPAD=3, space=3, /ROW,TLB_FRAME_ATTR=1,TAB_MODE=1)
	;-----×ó±ßµÄBASE----------------------------------------------
	LeftBase = Widget_Base( TLB_BASE, UNAME='LeftBase',XPAD=2 ,YPAD=2,space=3,/COLUMN $
							,TAB_MODE=1,/BASE_ALIGN_LEFT,FRAME=0)
		ConWith = 450
		ConUpBase = Widget_Base( LeftBase, UNAME='ConUpBase',XPAD=5 ,YPAD=3,/ROW,TAB_MODE=1 $
								,/FRAME,SCR_XSIZE=ConWith)
			Province = ['±±¾©ÊÐ','Ìì½òÊÐ','ºÓ±±','É½Î÷','ÄÚÃÉ¹Å','ÁÉÄþ','¼ªÁÖ','ºÚÁú½­','ÉÏº£ÊÐ','½­ËÕ' $
						,'Õã½­','°²Î¢','¸£½¨','½­Î÷','É½¶«','ºÓÄÏ','ºþ±±','ºþÄÏ','¹ã¶«','¹ãÎ÷','º£ÄÏ' $
						,'ÖØÇìÊÐ','ËÄ´¨','¹óÖÝ','ÔÆÄÏ','Î÷²Ø','ÉÂÎ÷','¸ÊËà','Çàº£','ÄþÏÄ','ÐÂ½®']
			ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

			ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;ÕâÑùËæÄê·Ý±ä»¯,ÏµÍ³ÏÂÁÐ±íÖÐµÄÄê·ÝÒ²»á±ä»¯.

;			ProDroplist  = Widget_Droplist(ConUpBase,UNAME='ProDroplist',TITLE='Ê¡Ãû:',SCR_XSIZE=100)   ;ÓÃ/frameÓëframe=1ÊÇÏàÍ¬µÄ
			label = widget_label(ConUpBase, value='Äê·Ý£º')
			YearDroplist = Widget_combobox(ConUpBase,UNAME='YearDroplist',SCR_XSIZE=60)


		ConDownBase = Widget_Base(LeftBase,XPAD=5 ,YPAD=3,/ROW,TAB_MODE=1,/FRAME $
							,/BASE_ALIGN_LEFT,SPACE=3,SCR_XSIZE=ConWith)

			FileLabel = WIDGET_LABEL(ConDownBase,VALUE='¸´ÖÖÖ¸ÊýÎÄ¼þ£º',/ALIGN_CENTER,SCR_YSIZE=15)
			FileText = WIDGET_TEXT(ConDownBase,SCR_XSIZE=300)
			SelectFile = Widget_Button(ConDownBase,UNAME='SelectFile',SCR_XSIZE=36,SCR_YSIZE=20 $
				,VALUE='open.bmp',/BITMAP,TOOLTIP='Ñ¡Ôñ¸´ÖÖÖ¸ÊýÎÄ¼þ')
		;-------------------------±íBASE---------------------------------------
		TableBASE = Widget_Base(LeftBase, UNAME='TableBASE',FRAME=1,SPACE=1,XPAD=0,YPAD=1 $
								,/COL,/BASE_ALIGN_LEFT,SCR_XSIZE=ConWith)
			TableLabel = WIDGET_LABEL(TableBASE,VALUE='¸÷ÏØ¸´ÖÖÖ¸ÊýÍ³¼Æ½á¹û',SCR_XSIZE=300,SCR_YSIZE=15,/ALIGN_CENTER)
			CroppingTable= WIDGET_TABLE(TableBASE,/FRAME,SCR_XSIZE=448,SCR_YSIZE=110,COLUMN_WIDTHS=60, /ALL_EVENTS,event_pro='highlight_table',uname='highlight_table')


	 ChartDraw = WIDGET_DRAW(LeftBase,SCR_XSIZE=ConWith,SCR_YSIZE=108,GRAPHICS_LEVEL=1,RETAIN=2,/FRAME)
		;------------------------°´Å¥BASE---------------------------------------
		SAVE_BASE = Widget_Base(LeftBase,FRAME=1,SPACE=40,XPAD=12,YPAD=3 ,ROW=1,/BASE_ALIGN_LEFT,SCR_XSIZE=ConWith)

		Button_width=50 & Button_height=22 ;¶¨Òå°´Å¥¿í¶ÈºÍ¸ß¶È
;		SaveTo=Widget_Base(SAVE_BASE,FRAME=0,SCR_XSIZE=250,XPAD=15,YPAD=0,/ROW,SPACE=35,/BASE_ALIGN_TOP)

		  StaCompute=Widget_Button(SAVE_BASE, UNAME='StaCompute',VALUE='Í³¼Æ' $
		         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height) ;$
		  saveToCompute=Widget_Button(SAVE_BASE, UNAME='saveToCompute',VALUE='±£´æ' $
		         ,SCR_XSIZE=Button_width ,SCR_YSIZE=Button_height,TOOLTIP='±£´æµ½±¾»ú´ÅÅÌ') ;$
		  saveTodb =Widget_Button(SAVE_BASE, UNAME='saveTodb',VALUE='Èë¿â' $
		         ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

		Help_bu=Widget_Button(SAVE_BASE, UNAME='Help_bu',VALUE='°ïÖú' $
		     ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

		quit_bu=Widget_Button(SAVE_BASE, UNAME='Quit_bu',VALUE='¹Ø±Õ' $
		     ,SCR_XSIZE=Button_width,SCR_YSIZE=Button_height)

;====ÓÒ±ßµÄBASE=====================================
	    RigthBASE = Widget_Base(TLB_BASE,XPAD=0 ,YPAD=3,space=3,/COLUMN, $
	    						/BASE_ALIGN_LEFT,FRAME=0)
;			MapLabel = WIDGET_LABEL(RigthBASE,UNAME='MapLabel',/ALIGN_CENTER,VALUE='¸´ÖÖÖ¸ÊýÓ°ÏñÎÄ¼þ£º')
;			MAPDraw  = WIDGET_DRAW(RigthBASE,SCR_XSIZE=308,SCR_YSIZE=370,RETAIN=2,GRAPHICS_LEVEL=2 $
;								  ,/FRAME,/MOTION_EVENTS,UNAME='MAPDraw')
		base_id = Widget_Base(RigthBASE,SCR_XSIZE=395 ,SCR_YSIZE=372,/col,xpad=0,ypad=0,space=0,/frame)
		colorLevel = $
			[[255B,	255B,	255B],$;1¼¶0
			 [0B,	0B,		255B],$;2¼¶1
			 [255B,	255B,	0B],$;3¼¶2
			 [0B,	255B,	  0B],$;4¼¶3
			 [255B,	128B,	  0B]];,$;5¼¶4
;			 [255B,	  0B,	  0B]];6¼¶5
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

	ProCode = STRMID(PROVINCE_CODE,0,2)      ;ÕâÈýÁ¿ÉèÔÚÕâÀïÊÇÎª½ÓÊÜÏµÍ³ÉèÖÃËùÔ¤ÉèµÄ²ÎÊý,½«À´ÏµÍ³¼¯³ÉÊ±Ó¦×÷ÐÞ¸Ä


;====ÑîÉÜïÉÐÞ¸Ä£¬20070906======================================
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

;;	ProjectPara = DC_ReadParameter(Province[WHERE(ProIDList EQ ProCode)])    ;µÃµ½Ê¡·¶Î§ÄÚµÄÍ¶Ó°²ÎÊý,Ã»ÓÐÔò·µ»ØÎª¿Õ,·ñÔòÊÇ½á¹¹Ìå.
   	STATE = { $
            ProNameList			:	Province		,$				;Ê¡ÃûÁÐ±í
            ProIDList			:	ProIDList		,$				;Ê¡IDÁÐ±í
            ProID				:	ProCode			,$				;±»Ñ¡Ê¡ID
            CalcYear			:	CalcYear		,$
            ARRAY_YEAR			:	ARRAY_YEAR		,$
            FilePathText		:	FileText		,$
            ChartDraw			:	ChartDraw		,$
;            MAPDraw			:	MAPDraw			,$
;            oView				:	OBJ_NEW()		,$				;MAPDrawÖÐµÄÊÓÍ¼
;            ProjectPara		:	ProjectPara		,$
            TableLabel			:	TableLabel		,$
            CroppingTable		:	CroppingTable	,$
            pstaff_display 		: 	pstaff_display 	 $
            }

    PA = PTR_NEW(STATE, /NO_COPY)

    WIDGET_CONTROL, TLB_BASE, SET_UVALUE=PA

    XManager, 'FZ_Statistic', TLB_BASE, CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END
