;;;创建时间：2007.9.12
;;;创建人：杨绍锷
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
;	;        XLabel=['冬小麦','春小麦','早稻','中稻','晚稻','春玉米','夏玉米','大豆']
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
;	;		if total(float(AnalysisData)) ne 0 then begin    ;杨绍锷所加条件
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
	CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['出现以下错误：',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF
 ;;;===========================================================================

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	ProID=STRMID(PROVINCE_CODE,0,2)		;======加上不同省份的选择===

;;;===========================================================================

  case wTarget of
;
;;=====选择外推的年份======================================================================
	Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_year'):	BEGIN   ;选择外推的年份

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
		;用于显示是否有有效数据的查询SQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;不同省份的选择


      	;获取有效数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************以上为原代码，20070822**********************************************************************
		;用于列表显示的查询SQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋粮
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;不同省份的选择    20070406添加
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;修改，20070822
;************************************************************************************

      	;获取数据长度
      	;获取表中数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
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


		;把未调查到的县显示为空
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;表示已经进行数据导入
		(*PSTATE).Ext_Flag=0	;表示尚未进行外推

      	;(3)结果显示
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

	END
;;;;===========================================================================
;;;*******************************************************************************************
;;;*****选择夏粮或秋粮**************************************************************************************
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='DROPLIST_SU_OR_AU'):begin

		(*PSTATE).SU_OR_AU =event.index           ;选择夏粮或秋粮
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------条件改变，把相应数据显示在表中------------------------
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;-----------------------------------------------------------------------
		;用于显示是否有有效数据的查询SQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;不同省份的选择


      	;获取有效数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************以上为原代码，20070822**********************************************************************
		;用于列表显示的查询SQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋粮
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;不同省份的选择    20070406添加
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;修改，20070822
;************************************************************************************


      	;获取数据长度
      	;获取表中数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
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


		;把未调查到的县显示为空
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;表示已经进行数据导入
		(*PSTATE).Ext_Flag=0	;表示尚未进行外推

      	;(3)结果显示
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
     end
;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='Droplist_Ext_Type'):begin

		(*PSTATE).Ext_Type =event.index           ;选择外推方式

;		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------条件改变，把相应数据显示在表中------------------------

		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;=============加上不同省份的选择===============================================

;	ProID=STRMID(PROVINCE_CODE,0,2)

;========================================================================
		case (*PSTATE).Ext_Type of

;----------按行政区划排序-------------------------------------------------------------

			0:begin    ;按行政区划排序

;-----------------------------------------------------------------------
				;用于显示是否有有效数据的查询SQL
				SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
				SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
				SQL=SQL+' where'
			    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
			    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;不同省份的选择


		      	;获取有效数据记录的个数
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM LE 0 then begin
		            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
		            temp[*,*]=''
		            (*PSTATE).NUM_SHOW=0
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

					;用于列表显示的查询SQL_SHOW;

				SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
				SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
				SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋粮
				SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;不同省份的选择    20070406添加
				SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;修改，20070822
		;************************************************************************************


		      	;获取数据长度
		      	;获取表中数据记录的个数
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM_SHOW=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM_SHOW LE 0 then begin
		            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
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


				;把未调查到的县显示为空
				for j=0,(*PSTATE).NUM_SHOW-1 do begin
					if arr_data_show[3,j] eq 0 then begin
						arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
						for m=4,5 do arr_data_show[m,j]=''
					endif
				endfor

				(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
				(*PSTATE).Input_Flag=1	;表示已经进行数据导入
				(*PSTATE).Ext_Flag=0	;表示尚未进行外推

		      	;(3)结果显示
		      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

		      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

			end
      ;-------按农业区划排序-----------------------------------------------------------------
			1:begin    ;按农业区划排序
				;-----------------------------------------------------------------------
				;用于显示是否有有效数据的查询SQL
				SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
				SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
				SQL=SQL+' where'
			    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
			    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;不同省份的选择


		      	;获取有效数据记录的个数
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM LE 0 then begin
		            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
		            temp[*,*]=''
		            (*PSTATE).NUM_SHOW=0
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

					;用于列表显示的查询SQL_SHOW;

				SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
				SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
				SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋粮
				SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;不同省份的选择    20070406添加
				SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by cint(NZ_CODE.nz_code)'     ;修改，20070822
		;************************************************************************************


		      	;获取数据长度
		      	;获取表中数据记录的个数
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM_SHOW=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM_SHOW LE 0 then begin
		            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
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


				;把未调查到的县显示为空
				for j=0,(*PSTATE).NUM_SHOW-1 do begin
					if arr_data_show[3,j] eq 0 then begin
						arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
						for m=4,5 do arr_data_show[m,j]=''
					endif
				endfor

				(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
				(*PSTATE).Input_Flag=1	;表示已经进行数据导入
				(*PSTATE).Ext_Flag=0	;表示尚未进行外推

		      	;(3)结果显示
		      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

		      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
			end
			else:
		endcase
	end
;
;;;*******************************************************************************************
;;;*******************************************************************************************
;;+++++++++导入数据+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget,FIND_BY_UNAME='butt_ImportData'):	BEGIN          ;导入数据
      	;(1)获取数据库链接
      	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------------------------------------------------------------
		;用于显示是否有有效数据的查询SQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;不同省份的选择


      	;获取有效数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************以上为原代码，20070822**********************************************************************
		;用于列表显示的查询SQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋粮
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;不同省份的选择    20070406添加
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order    ;修改，20070822
;************************************************************************************

      	;获取数据长度
      	;获取表中数据记录的个数
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('未发现符合条件的数据',/information,title='提示')
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


		;把未调查到的县显示为空
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;表示已经进行数据导入
		(*PSTATE).Ext_Flag=0	;表示尚未进行外推

      	;(3)结果显示
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

      END

;;*******************************************************************************************


;;++++++数据入库+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_data_to_DB'):begin     ;数据入库

		IF (*PSTATE).sel_lines LE 0 then begin
            temp=dialog_message('未选定数据，请先选择',/information,title='提示')
            return
        endif

;		if (*PSTATE).NUM_SHOW eq 1 then begin		;汇总到省的数据入库
;			widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
;
;;			arr_data_show=*((*PSTATE).Point_arr_show)	;获取表中的数据
;			year_input=arr_data_show[2,0]
;
;			DBCO=obj_new('IDLdbDatabase')
;			DBCO=DBobj
;
;			;(A)将数据库中原有的数据进行删除
;
;			;删除数据库中与要插入的记录年份相同的记录
;			SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT_PROVINCE WHERE '
;			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
;			SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
;			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
;;			PRINT,SQL
;			DBCO->EXECUTESQL,SQL
;
;;			(B)插入新的数据
;
;			if (STRTRIM((arr_data_show[1]),2) eq '') or  $
;				(STRTRIM((arr_data_show[2]),2) eq '') then begin
;				temp=dialog_message('请检查，省代码或年份不能为空',/information,title='提示')
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
;;				print,'总共入库记录数',jj
;			(*PSTATE).Input_Success_Province ='Yes'		;判断外推的数据是否已经入库
;			temp=dialog_message('省级外推结果入库成功',/information,title='提示')
;		endif

;=====================================================================================
;===============县级外推结果入库======================================================
;		if (*PSTATE).NUM_SHOW GT 1 then begin		;县级外推结果入库
;			IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0表示尚未进行外推
;	            (*PSTATE).Input_Yes_ON=dialog_message('尚未进行外推，是否确定要入库？', /QUESTION,title='提示')
;	        endif

;			if (*PSTATE).Input_Yes_ON eq 'Yes' then begin   ;确定是否要入库

;				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
				data_input=*((*PSTATE).Point_sel_data)
;				print,'data_input=',data_input
				lines=(*PSTATE).sel_lines

				year_input=data_input[3,0]

				DBCO=obj_new('IDLdbDatabase')
				DBCO=DBobj

				;(A)将数据库中原有的数据进行删除

				;删除数据库中与要插入的记录年份相同的记录

				FOR i=0,lines-1 do begin

					SQL='DELETE FROM CROP_PLANT_PROPORTION_ORI_SEL WHERE '
					SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
					SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
					SQL=SQL+' AND COUNTY_CODE = '+"'"+STRTRIM(data_input[2,i],2)+"'"     ;不同省份的选择    20070406添加

					PRINT,SQL
					DBCO->EXECUTESQL,SQL

				endfor

	;			(B)插入新的数据
				jj=0

				FOR I=0,(*PSTATE).sel_lines-1 DO BEGIN

					if (STRTRIM((data_input[2,I]),2) eq '') or  $
						(STRTRIM((data_input[3,I]),2) eq '') then begin
						temp=dialog_message('请检查，县代码或年份不能为空',/information,title='提示')
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
				   	  temp=dialog_message('无法入库，请确保数据正确输入�',/error,title='错误')
				      return
				   	ENDIF
						jj=jj+1

				ENDFOR

				UseCell= (*PSTATE).UseCell
				WIDGET_CONTROL,(*PSTATE).TABLE_data, USE_TABLE_SELECT= UseCell,BACKGROUND_COLOR=[0,255,255]

				print,'总共入库记录数',jj
				(*PSTATE).Input_Success_County ='Yes'		;判断外推的数据是否已经入库
				temp=dialog_message('入库成功',/information,title='提示')
;			endif
;		endif


;		(*PSTATE).Input_Yes_ON = 'Yes'		;默认的'Input_Yes_ON'是'Yes'，直接输入

	end
;;++++++数据剔除+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_delete'):begin     ;数据剔除

		IF (*PSTATE).sel_lines LE 0 then begin
            temp=dialog_message('未选定数据，请先选择',/information,title='提示')
            return
        endif

;		widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
		data_input=*((*PSTATE).Point_sel_data)
;				print,'data_input=',data_input
		lines=(*PSTATE).sel_lines

		year_input=data_input[3,0]

		DBCO=obj_new('IDLdbDatabase')
		DBCO=DBobj

		;(A)将数据库中原有的数据进行删除

		;删除数据库中与要插入的记录年份相同的记录

		FOR i=0,lines-1 do begin

			SQL='DELETE FROM CROP_PLANT_PROPORTION_ORI_SEL WHERE '
			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0为夏粮，1为秋量
			SQL=SQL+' AND COUNTY_CODE = '+"'"+STRTRIM(data_input[2,i],2)+"'"     ;不同省份的选择    20070406添加

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
			   	  temp=dialog_message('无法删除，请确保数据库正确操作',/error,title='错误')
			      return
			   	ENDIF


		print,'总共删除记录数',lines
		(*PSTATE).Input_Success_County ='Yes'		;判断外推的数据是否已经入库
		temp=dialog_message('删除成功',/information,title='提示')


	end

;;;*******************************************************************************************
;;;*******************************************************************************************
;
;;;===========================================================================
    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;关闭

		widget_control,event.top,/destroy

  end 		;关闭结束
;;;*******************************************************************************************
;;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;帮助

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '外推模块', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('找不到帮助文档',title='警告')
		endelse
	end
;		ONLINE_HELP,'外推模块',  BOOK='HELP\HELP.chm', /FULL_PATH
;;        temp=dialog_message('系统暂没有帮助')
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
		;        XLabel=['冬小麦','春小麦','早稻','中稻','晚稻','春玉米','夏玉米','大豆']
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
		;		if total(float(AnalysisData)) ne 0 then begin    ;杨绍锷所加条件
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

end			; event 的结束

;;*******************************************************************************************
;;*******************************************************************************************
pro MJ_pretreatment_PROPORTION, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

IF ( XREGISTERED('MJ_pretreatment_PROPORTION') NE 0 ) THEN RETURN

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=180 ,YOFFSET=200 ,SCR_XSIZE=624 ,SCR_YSIZE=475,TLB_FRAME_ATTR =1  $
      ,TITLE='种植成数数据筛选' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'   $
      ,XOFFSET=6 ,YOFFSET=59 ,SCR_XSIZE=622 ,SCR_YSIZE=336  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  ROW_LABEL=strtrim(indgen(16),2)
;  labels_county=['NZ_Code','County_name','County_code','Year','Proportion',$
;                      'SU_OR_AU'    ]   ;共6列
  labels_county=['农业区划代码','县名','县代码','年份','种植成数',$
                      '生长季'    ]   ;共6列
;  labels_province=['Province_name','Province_code','year','Proportion', $
;                      'SU_OR_AU'    ]    ;共5列
  labels_province=['省名','省代码','年份','种植成数', $
                      '生长季'    ]    ;共5列
  TABLE_data = Widget_Table(WID_BASE_1, UNAME='TABLE_data' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=4 ,SCR_XSIZE=606 ,SCR_YSIZE=327 ,XSIZE=6  $
      ,/RESIZEABLE_COLUMNS,ALIGNMENT=0,ROW_LABELS=ROW_LABEL  $
      ,/align_center,y_scroll_size=16   $
      ,column_labels=labels_county,/ALL_EVENTS);,EVENT_PRO='Data_Select')
	WIDGET_CONTROL,TABLE_data,SET_TABLE_SELECT=[-1,-1,-1,-1]   ;让界面表格初始时无阴影


  WID_BASE_3 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=402 ,SCR_XSIZE=606 ,SCR_YSIZE=34  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_BASE_4 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=606 ,SCR_YSIZE=46 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


Value_Ext_Type=['按行政区划','按农业区划']
Droplist_Ext_Type=widget_droplist(WID_BASE_4,/no_copy,$
                  uname='Droplist_Ext_Type' ,$
                  value=Value_Ext_Type,title='排序方式',$
                  scr_xsize=160,Xoffset=369,YOFFSET=10)


  WID_BUTTON_6 = Widget_Button(WID_BASE_3, UNAME='butt_delete'  $
      ,XOFFSET=280 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='数据剔除')


  WID_BUTTON_7 = Widget_Button(WID_BASE_3, UNAME='butt_Close'  $
      ,XOFFSET=510 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='关闭')


  WID_BUTTON_8 = Widget_Button(WID_BASE_3, UNAME='butt_Help'  $
      ,XOFFSET=408 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='帮助')


  WID_BUTTON_9 = Widget_Button(WID_BASE_3, UNAME='butt_ImportData'  $
      ,XOFFSET=30 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='导入数据')


  WID_BUTTON_10 = Widget_Button(WID_BASE_3, UNAME='butt_data_to_DB'  $
      ,XOFFSET=155 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='导入筛选表')


; Stat_To_Province = Widget_Button(WID_BASE_3, UNAME='Stat_To_Province'  $
;      ,XOFFSET=217 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
;      ,/ALIGN_CENTER ,VALUE='汇总到省')


  WID_LABEL_1 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_1'  $
      ,XOFFSET=85,YOFFSET=15 ,SCR_XSIZE=132 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='请选择：进行筛选的数据')

;;-------------------------是否使用选择后的数据-------------
;	  Draw_BASE= Widget_Base(WID_BASE_4, UNAME='Draw_BASE'   $
;	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,ROW=1,xoffset=490,yoffset=15,/NONEXCLUSIVE $
;	     ,/BASE_ALIGN_center)
;		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
;		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='使用选择后的数据' );$
;		      ;,EVENT_PRO='DC_DisplayShapeEV')
;  ;-----------------------------------------------------------------


  ARRYEAR=STRTRIM(INDGEN(36)+1980,2)
  DROPLIST_year = Widget_combobox(WID_BASE_4, UNAME='DROPLIST_year'  $
      ,XOFFSET=222 ,YOFFSET=11 ,SCR_XSIZE=50 ,SCR_YSIZE=23 ,VALUE=ARRYEAR)
  temp=(bin_date())[0]-1980+1
  widget_control,DROPLIST_year, SET_COMBOBOX_SELECT=temp-1


  ARR_SU_OR_AU=['夏粮(0)','秋粮(1)']
  DROPLIST_SU_OR_AU = Widget_Droplist(WID_BASE_4, UNAME='DROPLIST_SU_OR_AU'  $
      ,XOFFSET=292 ,YOFFSET=11 ,SCR_XSIZE=68 ,SCR_YSIZE=27 ,VALUE=ARR_SU_OR_AU)

	temp=(bin_date())[1]
	if temp ge 3 and temp le 8 then begin
		SU_OR_AU=0		;0表示为夏粮
	endif else begin
		SU_OR_AU=1		;1表示为秋粮
	endelse
   WIDGET_CONTROL,DROPLIST_SU_OR_AU,SET_DROPLIST_SELECT=SU_OR_AU


  WID_LABEL_2 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_2'  $
      ,XOFFSET=274 ,YOFFSET=15 ,SCR_XSIZE=18 ,SCR_YSIZE=11  $
      ,/ALIGN_LEFT ,VALUE='年')


  Point_arr_show=ptr_new()
  Point_sel_data=ptr_new()

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_7,/INPUT_FOCUS


	STATE = { $
  		TABLE_data	:	TABLE_data , $
  		Point_arr_show	:	Point_arr_show, $	;用于获取界面表格中的数据
  		sel_lines	:	0,  $
  		UseCell		:	[-1,-1,-1,-1],   $
  		Point_sel_data	:	Point_sel_data , $
  		ARRYEAR		:   ARRYEAR ,$
  		NUM			:	0,  $       ;有效记录数目
  		NUM_SHOW	:	0,  $		;在界面表中显示的记录数目
  		Ext_Type	:	0,  $
  		Ext_Flag	:	0,  $
  		SU_OR_AU	:	SU_OR_AU,  $
  		Input_Flag	:	0,  $		;标记是否已经导入数据
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