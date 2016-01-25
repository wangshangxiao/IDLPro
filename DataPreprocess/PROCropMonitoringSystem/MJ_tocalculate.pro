;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	09/20/2006 15:39.23
;


;-------------½çÃæ°ïÖú----------------------------
pro MJ_caculatehelp_event,event
	 PRINT,'Ãæ»ý¼ÆËã,°ïÖú'
	 if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, 'Ãæ»ý¼ÆËã', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('ÕÒ²»µ½°ïÖúÎÄµµ',title='¾¯¸æ')
	endelse
;	 ONLINE_HELP,  BOOK='HELP\HELP.chm','Ãæ»ý¼ÆËã'
end
;-------------½çÃæ¹Ø±Õ----------------------------
pro MJ_caculateclose_event,Event
	common_log,'¹Ø±ÕÃæ»ý¼ÆËã'
	 CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy
     RETURN
END

;------------------ÒÔÏÂÊÇ»­Öù×´Í¼
PRO MJ_Draw_BAR_PLOT,drawID $			;DRAW×é¼þµÄIDÖµ,¼´GET_VALUE
					,AnalysisData $		;Òª»­µÄÊý¾Ý
					,XLabel	$			;XÖáµÄ±êÇ©é
					,LINE = Line $		;Ö»»­ÏßÐÔÍ¼,Ä¬ÈÏÎªÖ»»­Öù×´Í¼
					,CHARTLINE = ChartLine  ;Í¬Ê±»­Ïß×´ºÍÖù×´Í¼
;µ÷ÓÃ:DC_Draw_BAR_PLOT,drawID,AnalysisData,XLabel[,/Line,/ChartLine]
;±¾³ÌÐòÖ»ÊÊÓÃÓÚ"²úÁ¿ÈÚºÏÄ£¿é"µÄÊ¹ÓÃ,¶ÔÓÚÆäËû³ÌÐò,¿ÉÄÜ²»ÊÊÓÃ.
 	IF (N_PARAMS() NE 3) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

	  PlotNum = N_ELEMENTS(XLabel)    ;ÀàÐÍµÄ¸öÊý

	  DEVICE,GET_DECOMPOSED=old_color     ;»ñÈ¡µ±Ç°DECOMPOSEDÖµ
;      DEVICE,GET_CURRENT_FONT=oldFont
;      DEVICE,SET_FONT='ËÎÌå',/TT_FONT
      DEVICE,RETAIN=2, DECOMPOSED=0      ;ÓÃIDLÌá¹©ºó±¸´æ´¢,Ê¹ÓÃÑÕÉ«²éÑ¯±í(Í£ÓÃÑÕÉ«·Ö½â¹¦ÄÜ),
		r=[0,255,  0,  0,255,212,155,255]   	  ;ÒÀ´ÎÎªºÚ\ºì\ÂÌ\À¶\»Æ\°×
		g=[0,  0,255,  0,255,0,166,255]
		b=[0,  0,  0,255,  0,18,20,220]
 		TVLCT, r, g, b   ;È±Ê¡µÚËÄ¸öÊ¡Êý,ÔòÊ¹ÑÕÉ«±íÖÐË÷ÒýºÅÎª0,1,2,3,4,5µÄÑÕÉ«ÎªÏàÓ¦µÄRGB×éºÏ

;;	TITLE='"'+EstiCounty[1,0]+'"'+'¸÷¹ÀËãµ¥²ú±È½ÏÍ¼'
    OldWin = !D.WINDOW		   ;±£´æÏµÍ³µÄ´°¿Ú
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
    !P.CHARSIZE = 0.95
    Colors = INTARR(PlotNum)
	FOR I = 0, PlotNum-1 DO Colors[I]= I ;(2*I)+100

   	WSET, drawID
	PlotData = FLOAT(AnalysisData)
	Psym = 3   ;(µã±êÊ¶)
	IF KEYWORD_SET(ChartLine) THEN BEGIN   ;»­Öù×´ºÍÏß×´Í¼
	    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-0.5,PlotNum] $
	         ,POSITION=[0.09,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
			 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)]; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý
		OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;ÓÃ°×ÑÕÉ«½«PLOT»­µÄÏß¸²¸Çµô,»¹Òª×¢Òâ¸øXRANGE¸ºÖµ,²»È»»­Í¼Ð§¹û²î

	    FirstOFFSET = 0.3 & Bar_width =9 & Base_range = 0.06 & Space = 6.5
	    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
			,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
			,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT

		OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-6
	ENDIF ELSE BEGIN
		IF KEYWORD_SET(Line) THEN BEGIN  ;Ö»»­Ïß×´Í¼
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
		         ,POSITION=[0.25,0.28,0.96,0.93] ,XTICKNAME=[' ',XLabel,' '] $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)] $
				 ,XTICKLEN=1 ; ,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý

			OPLOT,PlotData,THICK=1,COLOR=3,PSYM=-5
		ENDIF ELSE BEGIN				;Ö»»­Öù×´Í¼
		    PLOT,PlotData,PSYM=Psym ,THICK=0,XSTYLE=1,COLOR=1,FONT=0,CHARSIZE=1 ,XRANGE=[-1,PlotNum] $
		         ,POSITION=[0.12,0.25,0.96,0.95] ,XTICKNAME=STRARR(PlotNum+1)+' ' $ 	;YTICKLEN¿ØÖÆ±êÖ¾µÄ³¤¶È
				 ,YTICKLEN=1 ,YTICKS=PlotNum<3,YSTYLE=1,YRANGE=[0,MAX(PlotData)];,TITLE = TITLE				;YTICKSÓÃÓÚ¿ØÖÆÖ÷±êÖ¾µÄ¸öÊý

			OPLOT,PlotData,THICK=1,COLOR=5,PSYM=Psym   		;ÓÃ°×ÑÕÉ«½«PLOT»­µÄÏß¸²¸Çµô,»¹Òª×¢Òâ¸øXRANGE¸ºÖµ,²»È»»­Í¼Ð§¹û²î
		    FirstOFFSET = 0.05 & Bar_width =5.6 & Base_range = 0.067 & Space = 10.3
		    BAR_PLOT,PlotData, COLORS=Colors, BACKGROUND=255 $,TITLE = TITLE $
				,BARWIDTH=Bar_width, BARSPACE=Space, BAROFFSET=FirstOFFSET,BASERANGE=Base_range $
				,BARNAMES = XLabel,/OUTLINE ,/OVERPLOT
		ENDELSE
	ENDELSE

	!P.BACKGROUND = OldBackup		;»¹Ô­
	!P.POSITION   = Old_p
	!P.FONT       = Old_Font
	!P.CHARSIZE   = OldFontSiz
	!P.COLOR      = OClor
	!Y.TICKS = OldYticks
;	DEVICE,SET_FONT=oldFont
	DEVICE,DECOMPOSED=old_color   ;·µ»ØÔ­À´µÄDECOMPOSEDÖµ,ÒòÎª×Ô¶¨Òåº¯ÊýMyColor¸Ä±äÁË,Ðë»¹Ô­.

	WSET, OldWin				;»¹Ô­Ô­À´´°¿Ú.

END
;---------------------------------------------------------------------
pro MJ_area_caculate,event
     WIDGET_CONTROL, /HOURGLASS
     Widget_Control,Event.top,GET_UVALUE=PA
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD

     progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='Êý¾Ý´¦ÀíÖÐ,ÇëÉÔºò!',TITLE='Ãæ»ý¼ÆËã') ;ÐÂ½¨½ø¶ÈÌõ¶ÔÏó
	 progressTimer->START
	 progressTimer->UPDATE, (0.2 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ
     com_year = widget_info((*PA).com_year, /COMBOBOX_GETTEXT)

     ProCode=(*PA).ProCode
     c='%'
     likestring=ProCode+c
     SQL='select distinct County_code from SEL_AREA_Crop where year='+ STRTRIM(com_year,2) +' and county_code like '''+ STRTRIM(likestring,2) +''''   ;Õâ¾ä»°ÊÇÓÃÀ´ÏÞ¶¨Ê¹ÓÃµÄÊ¡·ÝµÄ£¬ÒâË¼¾ÍÊÇÖ»Ñ¡³öÎÒÃÇÕýÔÚ×öµÄÊ¡·Ý
 	 County_codearray = NQ_SJ_GetdataFromDB_Str(SQL,N_RECORDS = TOTALS,Column_Name=temp1,N_columns = m)

	 SQL_CROP_TYPES_PROPORTION_EXT='select [CROP_PLANT_PROPORTION_EXT].[proportion] from CROP_PLANT_PROPORTION_EXT where year='+ STRTRIM(com_year,2) +''
 	 xx= NQ_SJ_GetdataFromDB_Str(SQL_CROP_TYPES_PROPORTION_EXT,N_RECORDS = TOTALXX,Column_Name=temp1X,N_columns = mx)


     IF (TOTALS EQ 0) or (totalxx eq 0) THEN BEGIN
    	 OBJ_DESTROY,progressTimer ;Ïú»Ù½ø¶ÈÌõ
	     TEXT=DIALOG_MESSAGE('Êý¾Ý¿âÖÐÔÝÊ±Ã»ÓÐµ±ÄêÃæ»ý¼ÆËãËùÐèµÄÊý¾Ý!',title='ÌáÊ¾')

	     RETURN
	 ENDIF
	print,'Êý¾ÝÁ¬½ÓÕý³£'
	progressTimer->UPDATE, (0.4 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ


	;É¾³ýÊý¾Ý¿âÖÐÓëÒª²åÈëµÄ¼ÇÂ¼Äê·ÝÏàÍ¬µÄ¼ÇÂ¼
	SQL='DELETE FROM CROP_AREA_COUNTY WHERE '
	SQL_Delete=SQL+'YEAR = '+STRTRIM(com_year,2)
	DBobj->ExecuteSQL,SQL_Delete

  for i=0,TOTALS-1 do begin
  		 CATCH, Error_status               ;½ØÈ¡´íÎó.
		 IF Error_status NE 0 THEN BEGIN
   			 CATCH, /CANCEL
    		 Goto,nexts
         ENDIF
	    sql2="INSERT INTO CROP_AREA_COUNTY SELECT '"+ County_codearray[i]+"' AS county_code," $
	    +''+ STRTRIM(com_year,2) +' AS [year],' $
	    +' [values_wwheat] AS WINTER_WHEAT, [values_spwheat] AS SPRING_WHEAT, [values_earlyr] AS EARLY_RICE,' $
	    +' [values_semil] AS SEMILATE_RICE, [values_late] AS LATE_RICE,' $
	    +' [values_spricorn] AS SPRING_CORN, [values_sumcorn] AS SUMMER_CORN,[values_soybean] AS SOYBEAN ' $
	    +" FROM SEL_AREA_Crop WHERE County_Code='"+ County_codearray[i]+"'
	    DBobj->ExecuteSQL,SQL2
    nexts:
   endfor
	progressTimer->UPDATE, (0.8 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ
;	SQL3='select * from CROP_area_county where year='+ STRTRIM(com_year,2) +''

	SQL3="select A.county_code,B.NAME,A.year,A.winter_wheat,A.spring_wheat,A.early_rice," $
	+'A.semilate_rice,A.late_rice,A.spring_corn,A.summer_corn,A.soybean' $
	+' from CROP_area_county A,COUNTY_CODE B where year='+ STRTRIM(com_year,2) +'' $
	+' and A.county_code=B.code'

    print,sql3

 	Aear_table = NQ_SJ_GetdataFromDB_Str(SQL3,N_RECORDS = TOTAL,Column_Name=temp2,N_columns = m2)
    temp2=['ÏØ´úÂë','ÏØÃû³Æ','Äê','¶¬Ð¡Âó','´ºÐ¡Âó',$
		'Ôçµ¾','ÖÐµ¾','Ííµ¾','´ºÓñÃ×','ÏÄÓñÃ×','´ó¶¹']

	;--------------------------------
	;Ãæ»ýµ¥Î»£ºÊý¾Ý¿âÖÐÊÇÆ½·½¹«Àï-¡·½çÃæÖÐÊÇÇ§¹«Çê£¬1Æ½·½¹«Àï=0.1Ç§¹«Çê
	area_v = Aear_table[3:*,*]
	Aear_table[3:*,*] = strtrim(float(area_v)*0.1, 2)
	;--------------------------------

	progressTimer->UPDATE, (0.9 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ
    Widget_Control, (*PA).WID_MJ_table,TABLE_YSIZE=TOTAL,TABLE_XSIZE=11 $
         ,SET_VALUE=Aear_table $
         ,COLUMN_LABELS = temp2 $
         ,ALIGNMENT=0
    Aear_table=0B
    (*PA).table_row=total
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
    progressTimer->UPDATE, (1 * 100.0)  ;¸üÐÂ½ø¶ÈÌõ
    OBJ_DESTROY,progressTimer ;Ïú»Ù½ø¶ÈÌõ
    TEXT=DIALOG_MESSAGE('ÒÑ¾­Íê³ÉÃæ»ý¼ÆËã²¢Èë¿â£¡',title='ÌáÊ¾')
    log, 'Ãæ»ý¹ÀËã-Ãæ»ý¹ÀËã', 1
end
;----------------------------------------------------------------------


pro MJ_ResultTableEV,event

    WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	View = Widget_Info(	EVENT.ID,/TABLE_VIEW)
	WIDGET_CONTROL,(*PA).WID_MJ_table,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1]

	IF (*PA).table_row EQ 0 THEN RETURN

	IF EVENT.TYPE EQ 4 THEN BEGIN
		IF EVENT.SEL_TOP EQ -1 THEN RETURN

;		XLabel = *((*PA).EstiTypeID)
        XLabel=['¶¬Ð¡Âó','´ºÐ¡Âó','Ôçµ¾','ÖÐµ¾','Ííµ¾','´ºÓñÃ×','ÏÄÓñÃ×','´ó¶¹']
		Num = N_ELEMENTS(XLabel)
		UseCell    = INTARR(2,Num)
		UseCell[0,*] =INDGEN(Num)+3
		UseCell[1,*] = EVENT.SEL_TOP


		WIDGET_CONTROL,EVENT.ID,BACKGROUND_COLOR=[255,255,255]
		WIDGET_CONTROL,EVENT.ID,USE_TABLE_SELECT=UseCell,GET_VALUE=AnalysisData $
					   ,BACKGROUND_COLOR=[0,255,0],GET_UVALUE=County

		WIDGET_CONTROL,(*PA).WID_picbar,GET_VALUE = drawID
		ERASE,COLOR=!D.N_COLORS-1


		if total(float(AnalysisData)) ne 0 then begin    ;ÑîÉÜïÉËù¼ÓÌõ¼þ
			MJ_Draw_BAR_PLOT,drawID,AnalysisData,XLabel,LINE=0,CHARTLINE=0
		endif


	ENDIF






end
;----------------------------------------------------------------------------
pro WID_BASE_0_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    else:
  endcase

end


pro WID_BASE_0, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

;Resolve_Routine, 'MJ_tocalculate_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
ProCode = STRMID(PROVINCE_CODE,0,2)
  WID_BASE_0 = Widget_Base(GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=270 ,YOFFSET=200 ,SCR_XSIZE=453 ,SCR_YSIZE=476  $
      ,TITLE='Ãæ»ý¼ÆËã' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1)


  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'  $
      ,XOFFSET=6 ,YOFFSET=6 ,SCR_XSIZE=436 ,SCR_YSIZE=434  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=390 ,SCR_XSIZE=434 ,SCR_YSIZE=42  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  MJ_wjcaculate = Widget_Button(WID_BASE_2, UNAME='MJ_wjcaculate'  $
      ,XOFFSET=64 ,YOFFSET=9 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='¼ÆËã',event_pro='MJ_area_caculate')


  MJ_caculatehelp = Widget_Button(WID_BASE_2, UNAME='MJ_caculatehelp'  $
      ,XOFFSET=192 ,YOFFSET=9 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='°ïÖú',event_pro='MJ_caculatehelp_event')


  MJ_caculateclose = Widget_Button(WID_BASE_2, UNAME='MJ_caculateclose'  $
      ,XOFFSET=313 ,YOFFSET=9 ,SCR_XSIZE=58 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='¹Ø±Õ',event_pro='MJ_caculateclose_event')


  WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=231 ,SCR_XSIZE=434 ,SCR_YSIZE=151  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_picbar = Widget_Draw(WID_BASE_3, UNAME='WID_picbar' ,XOFFSET=4,GRAPHICS_LEVEL=1,RETAIN=2  $
      ,YOFFSET=7 ,SCR_XSIZE=425 ,SCR_YSIZE=135,/TRACKING_EVENTS,/BUTTON_EVENTS)


  WID_BASE_4 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=40 ,SCR_XSIZE=434 ,SCR_YSIZE=183  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

 columnlabel=['ÏØ´úÂë','ÏØÃû','Äê','¶¬Ð¡Âó','´ºÐ¡Âó',$
		'Ôçµ¾','ÖÐµ¾','Ííµ¾','´ºÓñÃ×','ÏÄÓñÃ×','´ó¶¹']

 WID_MJ_table = Widget_Table(WID_BASE_4, UNAME='WID_MJ_table'  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=432 ,SCR_YSIZE=181 ,XSIZE=11  $
      ,YSIZE=8,COLUMN_WIDTHS=87,COLUMN_LABELS=columnlabel,$
      /DISJOINT_SELECTION,EVENT_PRO='MJ_ResultTableEV',/ALL_EVENTS)


  WID_BASE_5 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_5' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=434 ,SCR_YSIZE=33 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(WID_BASE_5, UNAME='WID_LABEL_0'  $
      ,XOFFSET=28 ,YOFFSET=10 ,SCR_XSIZE=130 ,SCR_YSIZE=19 ,/ALIGN_LEFT  $
      ,VALUE='ÇëÑ¡ÔñÃæ»ý¼ÆËãµÄÄê·Ý:')

  list_year=['1980','1981','1982','1983','1984','1985','1986',$
  '1987','1988','1989','1990','1991','1992','1993','1994',$
  '1995','1996','1997','1998','1999','2000','2001','2002',$
  '2003','2004','2005','2006','2007','2008','2009','2010',$
  '2011','2012','2013','2014','2015']


;====ÑîÉÜïÉÐÞ¸Ä£¬20070906======================================

	temp=(bin_date())[0]-1980

  com_year = Widget_combobox(WID_BASE_5, UNAME='com_year'  $	;Ô­´úÂë
      ,XOFFSET=161 ,YOFFSET=6 ,SCR_XSIZE=63 ,SCR_YSIZE=18,value=list_year)

	widget_control,com_year, SET_COMBOBOX_SELECT=temp
 ;=================================================================

;===ÑîÉÜïÉÌí¼Ó£¬20070905======================================================
	WID_LABEL_y = Widget_Label(WID_BASE_5, UNAME='WID_LABEL_y'  $
      ,XOFFSET=328 ,YOFFSET=10 ,SCR_XSIZE=150 ,SCR_YSIZE=19 ,/ALIGN_LEFT  $
      ,VALUE='µ¥Î»:Ç§¹«Çê')
;=========================================================
  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,MJ_caculateclose,/INPUT_FOCUS

	Widget_Control,WID_picbar,GET_VALUE=Drawid
	ERASE,COLOR=!D.N_COLORS-1

  state = { $
              com_year:com_year,$
              ProCode:ProCode,$
              WID_MJ_table:WID_MJ_table,$
              WID_picbar:WID_picbar,$
              table_row:0 $
;         	  DisData				:	PTR_NEW()	 	$
    }
  	pstate = PTR_NEW(state, /no_copy)
	widget_control, WID_BASE_0, set_uvalue=pstate
    XManager, 'WID_BASE_0', WID_BASE_0,/NO_BLOCK, CLEANUP='ZS_PRO_SUM_cleanup'
end



pro MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'Æô¶¯Ãæ»ý¼ÆËã'
	if (xregistered('WID_BASE_0') ne 0) then return

    WID_BASE_0, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
