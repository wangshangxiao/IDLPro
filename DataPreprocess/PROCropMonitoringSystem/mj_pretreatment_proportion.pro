;;;´´½¨Ê±¼ä£º2007.9.12
;;;´´½¨ÈË£ºÑîÉÜïÉ
;;*******************************************************************************************
;;*******************************************************************************************

;pro Data_Select,event
;
;	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
;	View = Widget_Info(	EVENT.ID,/TABLE_VIEW)
;	help,EVENT, /STRUCT
;	WIDGET_CONTROL,(*PA).TABLE_data,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1,-1,-1]
;
;	IF (*PA).NUM_SHOW EQ 0 THEN RETURN
;
;;	help,PA, /STRUCT
;
;	case EVENT.TYPE of
;
;		4:begin		;Cell Selection (TYPE = 4)
;;			IF EVENT.TYPE EQ 4 THEN BEGIN
;			IF EVENT.SEL_TOP EQ -1 THEN RETURN
;;			print,'aaaaaaaaaaaaaaaaaaaaaa'
;
;			lines=EVENT.SEL_BOTTOM-EVENT.SEL_TOP+1
;			samples=EVENT.SEL_RIGHT-EVENT.SEL_LEFT+1
;			print,'samples=',samples
;			print,'lines=',lines
;
;			sel_data=strarr(6,lines)
;;			sle_data=(*((*PSTATE).Point_arr_show))[*,EVENT.SEL_TOP:EVENT.SEL_BOTTOM]
;;			print,sle_data
;
;
;	;;		XLabel = *((*PA).EstiTypeID)
;	;        XLabel=['¶¬Ð¡Âó','´ºÐ¡Âó','Ôçµ¾','ÖÐµ¾','Ííµ¾','´ºÓñÃ×','ÏÄÓñÃ×','´ó¶¹']
;	;		Num = N_ELEMENTS(XLabel)
;	;		UseCell    = INTARR(2,Num)
;;			UseCell[0,*] =INDGEN(Num)+3
;	;		UseCell[1,*] = EVENT.SEL_TOP
;	;
;			UseCell=[0,EVENT.SEL_TOP,5,EVENT.SEL_BOTTOM]
;	;		WIDGET_CONTROL,EVENT.ID,BACKGROUND_COLOR=[255,255,255]
;			WIDGET_CONTROL,EVENT.ID,USE_TABLE_SELECT=UseCell,GET_VALUE=sel_data
;			WIDGET_CONTROL,EVENT.ID, SET_TABLE_SELECT= UseCell
;			print,sel_data
;	;					   ,BACKGROUND_COLOR=[0,255,0],GET_UVALUE=County
;	;
;	;		WIDGET_CONTROL,(*PA).WID_picbar,GET_VALUE = drawID
;	;		ERASE,COLOR=!D.N_COLORS-1
;	;
;	;
;	;		if total(float(AnalysisData)) ne 0 then begin    ;ÑîÉÜïÉËù¼ÓÌõ¼þ
;	;			MJ_Draw_BAR_PLOT,drawID,AnalysisData,XLabel,LINE=0,CHARTLINE=0
;	;		endif
;
;
;;			ENDIF
;		end
;		else:
;	endcase
;
;end



pro MJ_pretreatment_PROPORTION_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
;;;===========================================================================
	CATCH, Error_status               ;½ØÈ¡´íÎó.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['³öÏÖÒÔÏÂ´íÎó£º',[!ERROR_STATE.MSG]],TITLE='´íÎó',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;Èç¹û²»¼ÓÉÏÕâ¾ä,Ôò»á¼ÌÐøÖ´ÐÐÏÂÃæµÄÓï¾ä,ÈÔ»á³öÏÖ´íÎó.
     ENDIF
 ;;;===========================================================================

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	ProID=STRMID(PROVINCE_CODE,0,2)		;======¼ÓÉÏ²»Í¬Ê¡·ÝµÄÑ¡Ôñ===

;;;===========================================================================

  case wTarget of
;
;;=====Ñ¡ÔñÍâÍÆµÄÄê·Ý======================================================================
	Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_year'):	BEGIN   ;Ñ¡ÔñÍâÍÆµÄÄê·Ý

		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse


		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;-----------------------------------------------------------------------
		;ÓÃÓÚÏÔÊ¾ÊÇ·ñÓÐÓÐÐ§Êý¾ÝµÄ²éÑ¯SQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ


      	;»ñÈ¡ÓÐÐ§Êý¾Ý¼ÇÂ¼µÄ¸öÊý
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************ÒÔÉÏÎªÔ­´úÂë£¬20070822**********************************************************************
		;ÓÃÓÚÁÐ±íÏÔÊ¾µÄ²éÑ¯SQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¸
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;ÐÞ¸Ä£¬20070822
;************************************************************************************

      	;»ñÈ¡Êý¾Ý³¤¶È
      	;»ñÈ¡±íÖÐÊý¾Ý¼ÇÂ¼µÄ¸öÊý
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
            temp[*,*]=''
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

		;***************************************************************************

		arr_data_show=strarr(6,(*PSTATE).NUM_SHOW)
		ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
		count=0
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			REPEAT BEGIN

				for i=0,5 do begin
					arr_data_show[i,count]=ORS -> GETFIELD(i)
				endfor

				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF

		Obj_Destroy,ORS


		;°ÑÎ´µ÷²éµ½µÄÏØÏÔÊ¾Îª¿Õ
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;±íÊ¾ÒÑ¾­½øÐÐÊý¾Ýµ¼Èë
		(*PSTATE).Ext_Flag=0	;±íÊ¾ÉÐÎ´½øÐÐÍâÍÆ

      	;(3)½á¹ûÏÔÊ¾
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

	END
;;;;===========================================================================
;;;*******************************************************************************************
;;;*****Ñ¡ÔñÏÄÁ¸»òÇïÁ¸**************************************************************************************
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='DROPLIST_SU_OR_AU'):begin

		(*PSTATE).SU_OR_AU =event.index           ;Ñ¡ÔñÏÄÁ¸»òÇïÁ¸
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------Ìõ¼þ¸Ä±ä£¬°ÑÏàÓ¦Êý¾ÝÏÔÊ¾ÔÚ±íÖÐ------------------------
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;-----------------------------------------------------------------------
		;ÓÃÓÚÏÔÊ¾ÊÇ·ñÓÐÓÐÐ§Êý¾ÝµÄ²éÑ¯SQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ


      	;»ñÈ¡ÓÐÐ§Êý¾Ý¼ÇÂ¼µÄ¸öÊý
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************ÒÔÉÏÎªÔ­´úÂë£¬20070822**********************************************************************
		;ÓÃÓÚÁÐ±íÏÔÊ¾µÄ²éÑ¯SQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¸
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;ÐÞ¸Ä£¬20070822
;************************************************************************************


      	;»ñÈ¡Êý¾Ý³¤¶È
      	;»ñÈ¡±íÖÐÊý¾Ý¼ÇÂ¼µÄ¸öÊý
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
            temp[*,*]=''
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

		;***************************************************************************

		arr_data_show=strarr(6,(*PSTATE).NUM_SHOW)
		ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
		count=0
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			REPEAT BEGIN

				for i=0,5 do begin
					arr_data_show[i,count]=ORS -> GETFIELD(i)
				endfor

				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF

		Obj_Destroy,ORS


		;°ÑÎ´µ÷²éµ½µÄÏØÏÔÊ¾Îª¿Õ
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;±íÊ¾ÒÑ¾­½øÐÐÊý¾Ýµ¼Èë
		(*PSTATE).Ext_Flag=0	;±íÊ¾ÉÐÎ´½øÐÐÍâÍÆ

      	;(3)½á¹ûÏÔÊ¾
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
     end
;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='Droplist_Ext_Type'):begin

		(*PSTATE).Ext_Type =event.index           ;Ñ¡ÔñÍâÍÆ·½Ê½

;		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------Ìõ¼þ¸Ä±ä£¬°ÑÏàÓ¦Êý¾ÝÏÔÊ¾ÔÚ±íÖÐ------------------------

		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;=============¼ÓÉÏ²»Í¬Ê¡·ÝµÄÑ¡Ôñ===============================================

;	ProID=STRMID(PROVINCE_CODE,0,2)

;========================================================================
		case (*PSTATE).Ext_Type of

;----------°´ÐÐÕþÇø»®ÅÅÐò-------------------------------------------------------------

			0:begin    ;°´ÐÐÕþÇø»®ÅÅÐò

;-----------------------------------------------------------------------
				;ÓÃÓÚÏÔÊ¾ÊÇ·ñÓÐÓÐÐ§Êý¾ÝµÄ²éÑ¯SQL
				SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
				SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
				SQL=SQL+' where'
			    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
			    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ


		      	;»ñÈ¡ÓÐÐ§Êý¾Ý¼ÇÂ¼µÄ¸öÊý
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM LE 0 then begin
		            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
		            temp[*,*]=''
		            (*PSTATE).NUM_SHOW=0
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

					;ÓÃÓÚÁÐ±íÏÔÊ¾µÄ²éÑ¯SQL_SHOW;

				SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
				SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
				SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¸
				SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó
				SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;ÐÞ¸Ä£¬20070822
		;************************************************************************************


		      	;»ñÈ¡Êý¾Ý³¤¶È
		      	;»ñÈ¡±íÖÐÊý¾Ý¼ÇÂ¼µÄ¸öÊý
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM_SHOW=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM_SHOW LE 0 then begin
		            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
		            temp[*,*]=''
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

				;***************************************************************************

				arr_data_show=strarr(6,(*PSTATE).NUM_SHOW)
				ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
				count=0
				IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
					REPEAT BEGIN

						for i=0,5 do begin
							arr_data_show[i,count]=ORS -> GETFIELD(i)
						endfor

						COUNT=COUNT+1
					ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
				ENDIF

				Obj_Destroy,ORS


				;°ÑÎ´µ÷²éµ½µÄÏØÏÔÊ¾Îª¿Õ
				for j=0,(*PSTATE).NUM_SHOW-1 do begin
					if arr_data_show[3,j] eq 0 then begin
						arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
						for m=4,5 do arr_data_show[m,j]=''
					endif
				endfor

				(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
				(*PSTATE).Input_Flag=1	;±íÊ¾ÒÑ¾­½øÐÐÊý¾Ýµ¼Èë
				(*PSTATE).Ext_Flag=0	;±íÊ¾ÉÐÎ´½øÐÐÍâÍÆ

		      	;(3)½á¹ûÏÔÊ¾
		      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

		      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

			end
      ;-------°´Å©ÒµÇø»®ÅÅÐò-----------------------------------------------------------------
			1:begin    ;°´Å©ÒµÇø»®ÅÅÐò
				;-----------------------------------------------------------------------
				;ÓÃÓÚÏÔÊ¾ÊÇ·ñÓÐÓÐÐ§Êý¾ÝµÄ²éÑ¯SQL
				SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
				SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
				SQL=SQL+' where'
			    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
			    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ


		      	;»ñÈ¡ÓÐÐ§Êý¾Ý¼ÇÂ¼µÄ¸öÊý
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM LE 0 then begin
		            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
		            temp[*,*]=''
		            (*PSTATE).NUM_SHOW=0
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

					;ÓÃÓÚÁÐ±íÏÔÊ¾µÄ²éÑ¯SQL_SHOW;

				SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
				SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
				SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¸
				SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó
				SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by cint(NZ_CODE.nz_code)'     ;ÐÞ¸Ä£¬20070822
		;************************************************************************************


		      	;»ñÈ¡Êý¾Ý³¤¶È
		      	;»ñÈ¡±íÖÐÊý¾Ý¼ÇÂ¼µÄ¸öÊý
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM_SHOW=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM_SHOW LE 0 then begin
		            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
		            temp[*,*]=''
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

				;***************************************************************************

				arr_data_show=strarr(6,(*PSTATE).NUM_SHOW)
				ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
				count=0
				IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
					REPEAT BEGIN

						for i=0,5 do begin
							arr_data_show[i,count]=ORS -> GETFIELD(i)
						endfor

						COUNT=COUNT+1
					ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
				ENDIF

				Obj_Destroy,ORS


				;°ÑÎ´µ÷²éµ½µÄÏØÏÔÊ¾Îª¿Õ
				for j=0,(*PSTATE).NUM_SHOW-1 do begin
					if arr_data_show[3,j] eq 0 then begin
						arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
						for m=4,5 do arr_data_show[m,j]=''
					endif
				endfor

				(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
				(*PSTATE).Input_Flag=1	;±íÊ¾ÒÑ¾­½øÐÐÊý¾Ýµ¼Èë
				(*PSTATE).Ext_Flag=0	;±íÊ¾ÉÐÎ´½øÐÐÍâÍÆ

		      	;(3)½á¹ûÏÔÊ¾
		      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

		      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
			end
			else:
		endcase
	end
;
;;;*******************************************************************************************
;;;*******************************************************************************************
;;+++++++++µ¼ÈëÊý¾Ý+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget,FIND_BY_UNAME='butt_ImportData'):	BEGIN          ;µ¼ÈëÊý¾Ý
      	;(1)»ñÈ¡Êý¾Ý¿âÁ´½Ó
      	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------------------------------------------------------------
		;ÓÃÓÚÏÔÊ¾ÊÇ·ñÓÐÓÐÐ§Êý¾ÝµÄ²éÑ¯SQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ


      	;»ñÈ¡ÓÐÐ§Êý¾Ý¼ÇÂ¼µÄ¸öÊý
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************ÒÔÉÏÎªÔ­´úÂë£¬20070822**********************************************************************
		;ÓÃÓÚÁÐ±íÏÔÊ¾µÄ²éÑ¯SQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¸
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order    ;ÐÞ¸Ä£¬20070822
;************************************************************************************

      	;»ñÈ¡Êý¾Ý³¤¶È
      	;»ñÈ¡±íÖÐÊý¾Ý¼ÇÂ¼µÄ¸öÊý
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('Î´·¢ÏÖ·ûºÏÌõ¼þµÄÊý¾Ý',/information,title='ÌáÊ¾')
            temp[*,*]=''
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

		;***************************************************************************

		arr_data_show=strarr(6,(*PSTATE).NUM_SHOW)
		ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL_SHOW)
		count=0
		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			REPEAT BEGIN

				for i=0,5 do begin
					arr_data_show[i,count]=ORS -> GETFIELD(i)
				endfor

				COUNT=COUNT+1
			ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
		ENDIF

		Obj_Destroy,ORS


		;°ÑÎ´µ÷²éµ½µÄÏØÏÔÊ¾Îª¿Õ
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;±íÊ¾ÒÑ¾­½øÐÐÊý¾Ýµ¼Èë
		(*PSTATE).Ext_Flag=0	;±íÊ¾ÉÐÎ´½øÐÐÍâÍÆ

      	;(3)½á¹ûÏÔÊ¾
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

      END

;;*******************************************************************************************


;;++++++Êý¾ÝÈë¿â+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_data_to_DB'):begin     ;Êý¾ÝÈë¿â

		IF (*PSTATE).sel_lines LE 0 then begin
            temp=dialog_message('Î´Ñ¡¶¨Êý¾Ý£¬ÇëÏÈÑ¡Ôñ',/information,title='ÌáÊ¾')
            return
        endif

;		if (*PSTATE).NUM_SHOW eq 1 then begin		;»ã×Üµ½Ê¡µÄÊý¾ÝÈë¿â
;			widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
;
;;			arr_data_show=*((*PSTATE).Point_arr_show)	;»ñÈ¡±íÖÐµÄÊý¾Ý
;			year_input=arr_data_show[2,0]
;
;			DBCO=obj_new('IDLdbDatabase')
;			DBCO=DBobj
;
;			;(A)½«Êý¾Ý¿âÖÐÔ­ÓÐµÄÊý¾Ý½øÐÐÉ¾³ý
;
;			;É¾³ýÊý¾Ý¿âÖÐÓëÒª²åÈëµÄ¼ÇÂ¼Äê·ÝÏàÍ¬µÄ¼ÇÂ¼
;			SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT_PROVINCE WHERE '
;			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
;			SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
;			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
;;			PRINT,SQL
;			DBCO->EXECUTESQL,SQL
;
;;			(B)²åÈëÐÂµÄÊý¾Ý
;
;			if (STRTRIM((arr_data_show[1]),2) eq '') or  $
;				(STRTRIM((arr_data_show[2]),2) eq '') then begin
;				temp=dialog_message('Çë¼ì²é£¬Ê¡´úÂë»òÄê·Ý²»ÄÜÎª¿Õ',/information,title='ÌáÊ¾')
;        		return
;        	endif else begin
;        		value2=STRTRIM((arr_data_show[1]),2)
;				value3=STRTRIM((arr_data_show[2]),2)
;			endelse
;
;			if STRTRIM((arr_data_show[3]),2) eq '' then begin
;				value4='NULL'
;        	endif else begin
;				value4=STRTRIM((arr_data_show[3]),2)
;			endelse
;
;			if STRTRIM((arr_data_show[4]),2) eq '' then begin
;				value5='NULL'
;        	endif else begin
;        		value5=STRTRIM((arr_data_show[4]),2)
;			endelse
;
;
;			SQL='INSERT INTO CROP_PLANT_PROPORTION_EXT_PROVINCE '
;			SQL=SQL+'(Province_code ,year ,proportion,SU_OR_AU ) '
;			SQL=SQL+'VALUES ('
;			SQL=SQL+ value2			+','
;			SQL=SQL+ value3			+','
;			SQL=SQL+ value4			+','
;			SQL=SQL+ value5			+')'
;
;;				PRINT,SQL
;			DBCO->EXECUTESQL,SQL
;
;
;;				print,'×Ü¹²Èë¿â¼ÇÂ¼Êý',jj
;			(*PSTATE).Input_Success_Province ='Yes'		;ÅÐ¶ÏÍâÍÆµÄÊý¾ÝÊÇ·ñÒÑ¾­Èë¿â
;			temp=dialog_message('Ê¡¼¶ÍâÍÆ½á¹ûÈë¿â³É¹¦',/information,title='ÌáÊ¾')
;		endif

;=====================================================================================
;===============ÏØ¼¶ÍâÍÆ½á¹ûÈë¿â======================================================
;		if (*PSTATE).NUM_SHOW GT 1 then begin		;ÏØ¼¶ÍâÍÆ½á¹ûÈë¿â
;			IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0±íÊ¾ÉÐÎ´½øÐÐÍâÍÆ
;	            (*PSTATE).Input_Yes_ON=dialog_message('ÉÐÎ´½øÐÐÍâÍÆ£¬ÊÇ·ñÈ·¶¨ÒªÈë¿â£¿', /QUESTION,title='ÌáÊ¾')
;	        endif

;			if (*PSTATE).Input_Yes_ON eq 'Yes' then begin   ;È·¶¨ÊÇ·ñÒªÈë¿â

;				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
				data_input=*((*PSTATE).Point_sel_data)
;				print,'data_input=',data_input
				lines=(*PSTATE).sel_lines

				year_input=data_input[3,0]

				DBCO=obj_new('IDLdbDatabase')
				DBCO=DBobj

				;(A)½«Êý¾Ý¿âÖÐÔ­ÓÐµÄÊý¾Ý½øÐÐÉ¾³ý

				;É¾³ýÊý¾Ý¿âÖÐÓëÒª²åÈëµÄ¼ÇÂ¼Äê·ÝÏàÍ¬µÄ¼ÇÂ¼

				FOR i=0,lines-1 do begin

					SQL='DELETE FROM CROP_PLANT_PROPORTION_ORI_SEL WHERE '
					SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
					SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
					SQL=SQL+' AND COUNTY_CODE = '+"'"+STRTRIM(data_input[2,i],2)+"'"     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó

					PRINT,SQL
					DBCO->EXECUTESQL,SQL

				endfor

	;			(B)²åÈëÐÂµÄÊý¾Ý
				jj=0

				FOR I=0,(*PSTATE).sel_lines-1 DO BEGIN

					if (STRTRIM((data_input[2,I]),2) eq '') or  $
						(STRTRIM((data_input[3,I]),2) eq '') then begin
						temp=dialog_message('Çë¼ì²é£¬ÏØ´úÂë»òÄê·Ý²»ÄÜÎª¿Õ',/information,title='ÌáÊ¾')
	            		return
	            	endif else begin
	            		value2=STRTRIM((data_input[2,I]),2)
						value3=STRTRIM((data_input[3,I]),2)
					endelse

					if STRTRIM((data_input[4,I]),2) eq '' then begin
						value4='NULL'
	            	endif else begin
						value4=STRTRIM((data_input[4,I]),2)
					endelse

;					if STRTRIM((arr_data_show[5,I]),2) eq '' then begin
;						value5='NULL'
;	            	endif else begin
;	            		value5=STRTRIM((arr_data_show[5,I]),2)
;					endelse

					if STRTRIM((data_input[5,I]),2) eq '' then begin
						value5=STRTRIM((*PSTATE).SU_OR_AU,2)
		        	endif else begin
		        		value5=STRTRIM((data_input[5,I]),2)
					endelse


					SQL='INSERT INTO CROP_PLANT_PROPORTION_ORI_SEL '
					SQL=SQL+'(County_code ,year ,proportion,SU_OR_AU ) '
					SQL=SQL+'VALUES ('
					SQL=SQL+ value2			+','
					SQL=SQL+ value3			+','
					SQL=SQL+ value4			+','
					SQL=SQL+ value5			+')'

					PRINT,SQL
					DBCO->EXECUTESQL,SQL
					CATCH, Error_status

				  	IF Error_status NE 0 THEN BEGIN
				   	  temp=dialog_message('ÎÞ·¨Èë¿â£¬ÇëÈ·±£Êý¾ÝÕýÈ·ÊäÈë¦',/error,title='´íÎó')
				      return
				   	ENDIF
						jj=jj+1

				ENDFOR

				UseCell= (*PSTATE).UseCell
				WIDGET_CONTROL,(*PSTATE).TABLE_data, USE_TABLE_SELECT= UseCell,BACKGROUND_COLOR=[0,255,255]

				print,'×Ü¹²Èë¿â¼ÇÂ¼Êý',jj
				(*PSTATE).Input_Success_County ='Yes'		;ÅÐ¶ÏÍâÍÆµÄÊý¾ÝÊÇ·ñÒÑ¾­Èë¿â
				temp=dialog_message('Èë¿â³É¹¦',/information,title='ÌáÊ¾')
;			endif
;		endif


;		(*PSTATE).Input_Yes_ON = 'Yes'		;Ä¬ÈÏµÄ'Input_Yes_ON'ÊÇ'Yes'£¬Ö±½ÓÊäÈë

	end
;;++++++Êý¾ÝÌÞ³ý+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_delete'):begin     ;Êý¾ÝÌÞ³ý

		IF (*PSTATE).sel_lines LE 0 then begin
            temp=dialog_message('Î´Ñ¡¶¨Êý¾Ý£¬ÇëÏÈÑ¡Ôñ',/information,title='ÌáÊ¾')
            return
        endif

;		widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
		data_input=*((*PSTATE).Point_sel_data)
;				print,'data_input=',data_input
		lines=(*PSTATE).sel_lines

		year_input=data_input[3,0]

		DBCO=obj_new('IDLdbDatabase')
		DBCO=DBobj

		;(A)½«Êý¾Ý¿âÖÐÔ­ÓÐµÄÊý¾Ý½øÐÐÉ¾³ý

		;É¾³ýÊý¾Ý¿âÖÐÓëÒª²åÈëµÄ¼ÇÂ¼Äê·ÝÏàÍ¬µÄ¼ÇÂ¼

		FOR i=0,lines-1 do begin

			SQL='DELETE FROM CROP_PLANT_PROPORTION_ORI_SEL WHERE '
			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0ÎªÏÄÁ¸£¬1ÎªÇïÁ¿
			SQL=SQL+' AND COUNTY_CODE = '+"'"+STRTRIM(data_input[2,i],2)+"'"     ;²»Í¬Ê¡·ÝµÄÑ¡Ôñ    20070406Ìí¼Ó

			PRINT,SQL
			DBCO->EXECUTESQL,SQL

		endfor


		UseCell= (*PSTATE).UseCell
		WIDGET_CONTROL,(*PSTATE).TABLE_data, USE_TABLE_SELECT= UseCell,BACKGROUND_COLOR=[255,0,255]
;			WIDGET_CONTROL,EVENT.ID,USE_TABLE_SELECT=UseCell,GET_VALUE=sel_data
;			WIDGET_CONTROL,EVENT.ID, SET_TABLE_SELECT= UseCell

		CATCH, Error_status
;
			  	IF Error_status NE 0 THEN BEGIN
			   	  temp=dialog_message('ÎÞ·¨É¾³ý£¬ÇëÈ·±£Êý¾Ý¿âÕýÈ·²Ù×÷',/error,title='´íÎó')
			      return
			   	ENDIF


		print,'×Ü¹²É¾³ý¼ÇÂ¼Êý',lines
		(*PSTATE).Input_Success_County ='Yes'		;ÅÐ¶ÏÍâÍÆµÄÊý¾ÝÊÇ·ñÒÑ¾­Èë¿â
		temp=dialog_message('É¾³ý³É¹¦',/information,title='ÌáÊ¾')


	end

;;;*******************************************************************************************
;;;*******************************************************************************************
;
;;;===========================================================================
    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;¹Ø±Õ

		widget_control,event.top,/destroy

  end 		;¹Ø±Õ½áÊø
;;;*******************************************************************************************
;;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;°ïÖú

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, 'ÍâÍÆÄ£¿é', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('ÕÒ²»µ½°ïÖúÎÄµµ',title='¾¯¸æ')
		endelse
	end
;		ONLINE_HELP,'ÍâÍÆÄ£¿é',  BOOK='HELP\HELP.chm', /FULL_PATH
;;        temp=dialog_message('ÏµÍ³ÔÝÃ»ÓÐ°ïÖú')
;	  end
;;============================================================================
;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='TABLE_data', /TABLE_ALL_EVENTS):begin       ;
;		WIDGET_CONTROL,Event.top,GET_UVALUE=PA
		View = Widget_Info(	EVENT.ID,/TABLE_VIEW)
		help,EVENT, /STRUCT
		WIDGET_CONTROL,(*PSTATE).TABLE_data,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1,-1,-1]

		IF (*PSTATE).NUM_SHOW EQ 0 THEN RETURN

	;	help,PA, /STRUCT

		case EVENT.TYPE of

			4:begin		;Cell Selection (TYPE = 4)
	;			IF EVENT.TYPE EQ 4 THEN BEGIN
				IF EVENT.SEL_TOP EQ -1 THEN RETURN
	;			print,'aaaaaaaaaaaaaaaaaaaaaa'

				lines=EVENT.SEL_BOTTOM-EVENT.SEL_TOP+1
				samples=EVENT.SEL_RIGHT-EVENT.SEL_LEFT+1
				print,'samples=',samples
				print,'lines=',lines
				(*PSTATE).sel_lines = lines

				sel_data=strarr(6,lines)
				UseCell=[0,EVENT.SEL_TOP,5,EVENT.SEL_BOTTOM]
;				WIDGET_CONTROL,EVENT.ID,BACKGROUND_COLOR=[105,100,100]
				WIDGET_CONTROL,EVENT.ID,USE_TABLE_SELECT=UseCell,GET_VALUE=sel_data
				WIDGET_CONTROL,EVENT.ID, SET_TABLE_SELECT= UseCell
				print,sel_data
				(*PSTATE).Point_sel_data=ptr_new(sel_data)
				(*PSTATE).UseCell=UseCell
;				sle_data=(*((*PSTATE).Point_arr_show))[*,EVENT.SEL_TOP:EVENT.SEL_BOTTOM]
;				print,sle_data


		;;		XLabel = *((*PA).EstiTypeID)
		;        XLabel=['¶¬Ð¡Âó','´ºÐ¡Âó','Ôçµ¾','ÖÐµ¾','Ííµ¾','´ºÓñÃ×','ÏÄÓñÃ×','´ó¶¹']
		;		Num = N_ELEMENTS(XLabel)
		;		UseCell    = INTARR(2,Num)
		;		UseCell[0,*] =INDGEN(Num)+3
		;		UseCell[1,*] = EVENT.SEL_TOP
		;
		;
		;		WIDGET_CONTROL,EVENT.ID,BACKGROUND_COLOR=[255,255,255]
		;		WIDGET_CONTROL,EVENT.ID,USE_TABLE_SELECT=UseCell,GET_VALUE=AnalysisData $
		;					   ,BACKGROUND_COLOR=[0,255,0],GET_UVALUE=County
		;
		;		WIDGET_CONTROL,(*PA).WID_picbar,GET_VALUE = drawID
		;		ERASE,COLOR=!D.N_COLORS-1
		;
		;
		;		if total(float(AnalysisData)) ne 0 then begin    ;ÑîÉÜïÉËù¼ÓÌõ¼þ
		;			MJ_Draw_BAR_PLOT,drawID,AnalysisData,XLabel,LINE=0,CHARTLINE=0
		;		endif


	;			ENDIF
			end
			else:
		endcase

	end

;;============================================================================

    else:
  endcase

end			; event µÄ½áÊø

;;*******************************************************************************************
;;*******************************************************************************************
pro MJ_pretreatment_PROPORTION, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

IF ( XREGISTERED('MJ_pretreatment_PROPORTION') NE 0 ) THEN RETURN

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=180 ,YOFFSET=200 ,SCR_XSIZE=624 ,SCR_YSIZE=475,TLB_FRAME_ATTR =1  $
      ,TITLE='ÖÖÖ²³ÉÊýÊý¾ÝÉ¸Ñ¡' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'   $
      ,XOFFSET=6 ,YOFFSET=59 ,SCR_XSIZE=622 ,SCR_YSIZE=336  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  ROW_LABEL=strtrim(indgen(16),2)
;  labels_county=['NZ_Code','County_name','County_code','Year','Proportion',$
;                      'SU_OR_AU'    ]   ;¹²6ÁÐ
  labels_county=['Å©ÒµÇø»®´úÂë','ÏØÃû','ÏØ´úÂë','Äê·Ý','ÖÖÖ²³ÉÊý',$
                      'Éú³¤¼¾'    ]   ;¹²6ÁÐ
;  labels_province=['Province_name','Province_code','year','Proportion', $
;                      'SU_OR_AU'    ]    ;¹²5ÁÐ
  labels_province=['Ê¡Ãû','Ê¡´úÂë','Äê·Ý','ÖÖÖ²³ÉÊý', $
                      'Éú³¤¼¾'    ]    ;¹²5ÁÐ
  TABLE_data = Widget_Table(WID_BASE_1, UNAME='TABLE_data' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=4 ,SCR_XSIZE=606 ,SCR_YSIZE=327 ,XSIZE=6  $
      ,/RESIZEABLE_COLUMNS,ALIGNMENT=0,ROW_LABELS=ROW_LABEL  $
      ,/align_center,y_scroll_size=16   $
      ,column_labels=labels_county,/ALL_EVENTS);,EVENT_PRO='Data_Select')
	WIDGET_CONTROL,TABLE_data,SET_TABLE_SELECT=[-1,-1,-1,-1]   ;ÈÃ½çÃæ±í¸ñ³õÊ¼Ê±ÎÞÒõÓ°


  WID_BASE_3 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=402 ,SCR_XSIZE=606 ,SCR_YSIZE=34  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_BASE_4 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=606 ,SCR_YSIZE=46 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


Value_Ext_Type=['°´ÐÐÕþÇø»®','°´Å©ÒµÇø»®']
Droplist_Ext_Type=widget_droplist(WID_BASE_4,/no_copy,$
                  uname='Droplist_Ext_Type' ,$
                  value=Value_Ext_Type,title='ÅÅÐò·½Ê½',$
                  scr_xsize=160,Xoffset=369,YOFFSET=10)


  WID_BUTTON_6 = Widget_Button(WID_BASE_3, UNAME='butt_delete'  $
      ,XOFFSET=280 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='Êý¾ÝÌÞ³ý')


  WID_BUTTON_7 = Widget_Button(WID_BASE_3, UNAME='butt_Close'  $
      ,XOFFSET=510 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='¹Ø±Õ')


  WID_BUTTON_8 = Widget_Button(WID_BASE_3, UNAME='butt_Help'  $
      ,XOFFSET=408 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='°ïÖú')


  WID_BUTTON_9 = Widget_Button(WID_BASE_3, UNAME='butt_ImportData'  $
      ,XOFFSET=30 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='µ¼ÈëÊý¾Ý')


  WID_BUTTON_10 = Widget_Button(WID_BASE_3, UNAME='butt_data_to_DB'  $
      ,XOFFSET=155 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='µ¼ÈëÉ¸Ñ¡±í')


; Stat_To_Province = Widget_Button(WID_BASE_3, UNAME='Stat_To_Province'  $
;      ,XOFFSET=217 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
;      ,/ALIGN_CENTER ,VALUE='»ã×Üµ½Ê¡')


  WID_LABEL_1 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_1'  $
      ,XOFFSET=85,YOFFSET=15 ,SCR_XSIZE=132 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='ÇëÑ¡Ôñ£º½øÐÐÉ¸Ñ¡µÄÊý¾Ý')

;;-------------------------ÊÇ·ñÊ¹ÓÃÑ¡ÔñºóµÄÊý¾Ý-------------
;	  Draw_BASE= Widget_Base(WID_BASE_4, UNAME='Draw_BASE'   $
;	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,ROW=1,xoffset=490,yoffset=15,/NONEXCLUSIVE $
;	     ,/BASE_ALIGN_center)
;		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
;		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='Ê¹ÓÃÑ¡ÔñºóµÄÊý¾Ý' );$
;		      ;,EVENT_PRO='DC_DisplayShapeEV')
;  ;-----------------------------------------------------------------


  ARRYEAR=STRTRIM(INDGEN(36)+1980,2)
  DROPLIST_year = Widget_combobox(WID_BASE_4, UNAME='DROPLIST_year'  $
      ,XOFFSET=222 ,YOFFSET=11 ,SCR_XSIZE=50 ,SCR_YSIZE=23 ,VALUE=ARRYEAR)
  temp=(bin_date())[0]-1980+1
  widget_control,DROPLIST_year, SET_COMBOBOX_SELECT=temp-1


  ARR_SU_OR_AU=['ÏÄÁ¸(0)','ÇïÁ¸(1)']
  DROPLIST_SU_OR_AU = Widget_Droplist(WID_BASE_4, UNAME='DROPLIST_SU_OR_AU'  $
      ,XOFFSET=292 ,YOFFSET=11 ,SCR_XSIZE=68 ,SCR_YSIZE=27 ,VALUE=ARR_SU_OR_AU)

	temp=(bin_date())[1]
	if temp ge 3 and temp le 8 then begin
		SU_OR_AU=0		;0±íÊ¾ÎªÏÄÁ¸
	endif else begin
		SU_OR_AU=1		;1±íÊ¾ÎªÇïÁ¸
	endelse
   WIDGET_CONTROL,DROPLIST_SU_OR_AU,SET_DROPLIST_SELECT=SU_OR_AU


  WID_LABEL_2 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_2'  $
      ,XOFFSET=274 ,YOFFSET=15 ,SCR_XSIZE=18 ,SCR_YSIZE=11  $
      ,/ALIGN_LEFT ,VALUE='Äê')


  Point_arr_show=ptr_new()
  Point_sel_data=ptr_new()

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_7,/INPUT_FOCUS


	STATE = { $
  		TABLE_data	:	TABLE_data , $
  		Point_arr_show	:	Point_arr_show, $	;ÓÃÓÚ»ñÈ¡½çÃæ±í¸ñÖÐµÄÊý¾Ý
  		sel_lines	:	0,  $
  		UseCell		:	[-1,-1,-1,-1],   $
  		Point_sel_data	:	Point_sel_data , $
  		ARRYEAR		:   ARRYEAR ,$
  		NUM			:	0,  $       ;ÓÐÐ§¼ÇÂ¼ÊýÄ¿
  		NUM_SHOW	:	0,  $		;ÔÚ½çÃæ±íÖÐÏÔÊ¾µÄ¼ÇÂ¼ÊýÄ¿
  		Ext_Type	:	0,  $
  		Ext_Flag	:	0,  $
  		SU_OR_AU	:	SU_OR_AU,  $
  		Input_Flag	:	0,  $		;±ê¼ÇÊÇ·ñÒÑ¾­µ¼ÈëÊý¾Ý
  		Input_Yes_ON	:	'Yes'	,  $
		Sta_Yes_ON	:	'Yes'	,  $
		Input_Success_County	:	'Yes'   ,  $
		Input_Success_Province	:	'Yes'   ,  $
		labels_county	:	labels_county   ,  $
  		labels_province	:	labels_province	,  $
  		SELYEAR		:	(bin_date())[0] 	$
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=PSTATE

  XManager, 'MJ_pretreatment_PROPORTION', WID_BASE_0, /NO_BLOCK

end