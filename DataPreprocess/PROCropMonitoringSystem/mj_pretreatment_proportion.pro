;;;����ʱ�䣺2007.9.12
;;;�����ˣ�������
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
;	;        XLabel=['��С��','��С��','�絾','�е�','��','������','������','��']
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
;	;		if total(float(AnalysisData)) ne 0 then begin    ;��������������
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
	CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['�������´���',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF
 ;;;===========================================================================

COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	ProID=STRMID(PROVINCE_CODE,0,2)		;======���ϲ�ͬʡ�ݵ�ѡ��===

;;;===========================================================================

  case wTarget of
;
;;=====ѡ�����Ƶ����======================================================================
	Widget_Info(wWidget,FIND_BY_UNAME='DROPLIST_year'):	BEGIN   ;ѡ�����Ƶ����

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
		;������ʾ�Ƿ�����Ч���ݵĲ�ѯSQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��


      	;��ȡ��Ч���ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************����Ϊԭ���룬20070822**********************************************************************
		;�����б���ʾ�Ĳ�ѯSQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;�޸ģ�20070822
;************************************************************************************

      	;��ȡ���ݳ���
      	;��ȡ�������ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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


		;��δ���鵽������ʾΪ��
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
		(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

      	;(3)�����ʾ
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

	END
;;;;===========================================================================
;;;*******************************************************************************************
;;;*****ѡ������������**************************************************************************************
;;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='DROPLIST_SU_OR_AU'):begin

		(*PSTATE).SU_OR_AU =event.index           ;ѡ������������
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------�����ı䣬����Ӧ������ʾ�ڱ���------------------------
		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;-----------------------------------------------------------------------
		;������ʾ�Ƿ�����Ч���ݵĲ�ѯSQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��


      	;��ȡ��Ч���ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************����Ϊԭ���룬20070822**********************************************************************
		;�����б���ʾ�Ĳ�ѯSQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;�޸ģ�20070822
;************************************************************************************


      	;��ȡ���ݳ���
      	;��ȡ�������ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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


		;��δ���鵽������ʾΪ��
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
		(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

      	;(3)�����ʾ
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
     end
;===========================================================================
	Widget_Info(wWidget, FIND_BY_UNAME='Droplist_Ext_Type'):begin

		(*PSTATE).Ext_Type =event.index           ;ѡ�����Ʒ�ʽ

;		(*PSTATE).SELYEAR =(*PSTATE).ARRYEAR[event.index]
		(*PSTATE).Input_Flag=0

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------�����ı䣬����Ӧ������ʾ�ڱ���------------------------

		COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

;=============���ϲ�ͬʡ�ݵ�ѡ��===============================================

;	ProID=STRMID(PROVINCE_CODE,0,2)

;========================================================================
		case (*PSTATE).Ext_Type of

;----------��������������-------------------------------------------------------------

			0:begin    ;��������������

;-----------------------------------------------------------------------
				;������ʾ�Ƿ�����Ч���ݵĲ�ѯSQL
				SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
				SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
				SQL=SQL+' where'
			    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
			    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��


		      	;��ȡ��Ч���ݼ�¼�ĸ���
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM LE 0 then begin
		            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
		            temp[*,*]=''
		            (*PSTATE).NUM_SHOW=0
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

					;�����б���ʾ�Ĳ�ѯSQL_SHOW;

				SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
				SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
				SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
				SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
				SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order     ;�޸ģ�20070822
		;************************************************************************************


		      	;��ȡ���ݳ���
		      	;��ȡ�������ݼ�¼�ĸ���
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM_SHOW=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM_SHOW LE 0 then begin
		            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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


				;��δ���鵽������ʾΪ��
				for j=0,(*PSTATE).NUM_SHOW-1 do begin
					if arr_data_show[3,j] eq 0 then begin
						arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
						for m=4,5 do arr_data_show[m,j]=''
					endif
				endfor

				(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
				(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
				(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

		      	;(3)�����ʾ
		      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

		      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

			end
      ;-------��ũҵ��������-----------------------------------------------------------------
			1:begin    ;��ũҵ��������
				;-----------------------------------------------------------------------
				;������ʾ�Ƿ�����Ч���ݵĲ�ѯSQL
				SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
				SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
				SQL=SQL+' where'
			    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
			    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
			    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��


		      	;��ȡ��Ч���ݼ�¼�ĸ���
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM LE 0 then begin
		            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
		            temp[*,*]=''
		            (*PSTATE).NUM_SHOW=0
		            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
		            return
		        endif

					;�����б���ʾ�Ĳ�ѯSQL_SHOW;

				SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
				SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
				SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
				SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
				SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by cint(NZ_CODE.nz_code)'     ;�޸ģ�20070822
		;************************************************************************************


		      	;��ȡ���ݳ���
		      	;��ȡ�������ݼ�¼�ĸ���
				RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
				RecordNum = RecordNumOBJ->GETFIELD(0)
				(*PSTATE).NUM_SHOW=RecordNum

				Obj_Destroy,RecordNumOBJ


				IF (*PSTATE).NUM_SHOW LE 0 then begin
		            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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


				;��δ���鵽������ʾΪ��
				for j=0,(*PSTATE).NUM_SHOW-1 do begin
					if arr_data_show[3,j] eq 0 then begin
						arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
						for m=4,5 do arr_data_show[m,j]=''
					endif
				endfor

				(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
				(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
				(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

		      	;(3)�����ʾ
		      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

		      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county
			end
			else:
		endcase
	end
;
;;;*******************************************************************************************
;;;*******************************************************************************************
;;+++++++++��������+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget,FIND_BY_UNAME='butt_ImportData'):	BEGIN          ;��������
      	;(1)��ȡ���ݿ�����
      	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
      	DBCO=obj_new('IDLdbDatabase')
      	DBCO=DBobj

		if (*PSTATE).Ext_Type then begin
			order = 'cint(NZ_CODE.nz_code)'
		endif else begin
			order = 'NZ_CODE.County_Code'
		endelse

;-----------------------------------------------------------------------
		;������ʾ�Ƿ�����Ч���ݵĲ�ѯSQL
		SQL='select NZ_CODE.NZ_Code,CROP_PLANT_PROPORTION_ORI.*'
		SQL=SQL+' from NZ_CODE, CROP_PLANT_PROPORTION_ORI'
		SQL=SQL+' where'
	    SQL=SQL+' CROP_PLANT_PROPORTION_ORI.year ='+STRTRIM((*PSTATE).SELYEAR,2)
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.County_code = NZ_CODE.County_code'
	    SQL=SQL+' AND CROP_PLANT_PROPORTION_ORI.SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
	    SQL=SQL+' AND LEFT(NZ_CODE.County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��


      	;��ȡ��Ч���ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
            temp[*,*]=''
            (*PSTATE).NUM_SHOW=0
            widget_control,(*PSTATE).TABLE_data,set_value=temp[*,*]
            return
        endif

;****************����Ϊԭ���룬20070822**********************************************************************
		;�����б���ʾ�Ĳ�ѯSQL_SHOW;

		SQL='SELECT NZ_CODE.nz_code, NZ_CODE.County_name, NZ_CODE.County_Code,c.year, c.proportion, c.SU_OR_AU'
		SQL=SQL+' FROM NZ_CODE LEFT JOIN (select County_Code, year, proportion, SU_OR_AU from CROP_PLANT_PROPORTION_ORI where year='+STRTRIM((*PSTATE).SELYEAR,2)
		SQL=SQL+' AND SU_OR_AU ='+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
		SQL=SQL+' AND LEFT(County_code,2)='+ProID     ;��ͬʡ�ݵ�ѡ��    20070406���
		SQL_SHOW=SQL+') c ON NZ_CODE.County_code = c.County_code order by '+order    ;�޸ģ�20070822
;************************************************************************************

      	;��ȡ���ݳ���
      	;��ȡ�������ݼ�¼�ĸ���
		RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+SQL_SHOW+')')
		RecordNum = RecordNumOBJ->GETFIELD(0)
		(*PSTATE).NUM_SHOW=RecordNum

		Obj_Destroy,RecordNumOBJ


		IF (*PSTATE).NUM_SHOW LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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


		;��δ���鵽������ʾΪ��
		for j=0,(*PSTATE).NUM_SHOW-1 do begin
			if arr_data_show[3,j] eq 0 then begin
				arr_data_show[3,j]=STRTRIM((*PSTATE).SELYEAR)
				for m=4,5 do arr_data_show[m,j]=''
			endif
		endfor

		(*PSTATE).Point_arr_show=ptr_new(arr_data_show)
		(*PSTATE).Input_Flag=1	;��ʾ�Ѿ��������ݵ���
		(*PSTATE).Ext_Flag=0	;��ʾ��δ��������

      	;(3)�����ʾ
      	widget_control,(*PSTATE).TABLE_data,table_ysize=(*PSTATE).NUM_SHOW,ROW_LABEL=strtrim(indgen((*PSTATE).NUM_SHOW),2)

      	widget_control,(*PSTATE).TABLE_data,set_value=arr_data_show,COLUMN_LABELS=(*PSTATE).labels_county

      END

;;*******************************************************************************************


;;++++++�������+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_data_to_DB'):begin     ;�������

		IF (*PSTATE).sel_lines LE 0 then begin
            temp=dialog_message('δѡ�����ݣ�����ѡ��',/information,title='��ʾ')
            return
        endif

;		if (*PSTATE).NUM_SHOW eq 1 then begin		;���ܵ�ʡ���������
;			widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
;
;;			arr_data_show=*((*PSTATE).Point_arr_show)	;��ȡ���е�����
;			year_input=arr_data_show[2,0]
;
;			DBCO=obj_new('IDLdbDatabase')
;			DBCO=DBobj
;
;			;(A)�����ݿ���ԭ�е����ݽ���ɾ��
;
;			;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼
;			SQL='DELETE FROM CROP_PLANT_PROPORTION_EXT_PROVINCE WHERE '
;			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
;			SQL=SQL+' AND Province_code ='+"'"+STRTRIM(arr_data_show[1],2)+"'"
;			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
;;			PRINT,SQL
;			DBCO->EXECUTESQL,SQL
;
;;			(B)�����µ�����
;
;			if (STRTRIM((arr_data_show[1]),2) eq '') or  $
;				(STRTRIM((arr_data_show[2]),2) eq '') then begin
;				temp=dialog_message('���飬ʡ�������ݲ���Ϊ��',/information,title='��ʾ')
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
;;				print,'�ܹ�����¼��',jj
;			(*PSTATE).Input_Success_Province ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
;			temp=dialog_message('ʡ�����ƽ�����ɹ�',/information,title='��ʾ')
;		endif

;=====================================================================================
;===============�ؼ����ƽ�����======================================================
;		if (*PSTATE).NUM_SHOW GT 1 then begin		;�ؼ����ƽ�����
;			IF (*PSTATE).Ext_Flag eq 0 then begin	;Ext_Flag=0��ʾ��δ��������
;	            (*PSTATE).Input_Yes_ON=dialog_message('��δ�������ƣ��Ƿ�ȷ��Ҫ��⣿', /QUESTION,title='��ʾ')
;	        endif

;			if (*PSTATE).Input_Yes_ON eq 'Yes' then begin   ;ȷ���Ƿ�Ҫ���

;				widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
				data_input=*((*PSTATE).Point_sel_data)
;				print,'data_input=',data_input
				lines=(*PSTATE).sel_lines

				year_input=data_input[3,0]

				DBCO=obj_new('IDLdbDatabase')
				DBCO=DBobj

				;(A)�����ݿ���ԭ�е����ݽ���ɾ��

				;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼

				FOR i=0,lines-1 do begin

					SQL='DELETE FROM CROP_PLANT_PROPORTION_ORI_SEL WHERE '
					SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
					SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
					SQL=SQL+' AND COUNTY_CODE = '+"'"+STRTRIM(data_input[2,i],2)+"'"     ;��ͬʡ�ݵ�ѡ��    20070406���

					PRINT,SQL
					DBCO->EXECUTESQL,SQL

				endfor

	;			(B)�����µ�����
				jj=0

				FOR I=0,(*PSTATE).sel_lines-1 DO BEGIN

					if (STRTRIM((data_input[2,I]),2) eq '') or  $
						(STRTRIM((data_input[3,I]),2) eq '') then begin
						temp=dialog_message('���飬�ش������ݲ���Ϊ��',/information,title='��ʾ')
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
				   	  temp=dialog_message('�޷���⣬��ȷ��������ȷ����',/error,title='����')
				      return
				   	ENDIF
						jj=jj+1

				ENDFOR

				UseCell= (*PSTATE).UseCell
				WIDGET_CONTROL,(*PSTATE).TABLE_data, USE_TABLE_SELECT= UseCell,BACKGROUND_COLOR=[0,255,255]

				print,'�ܹ�����¼��',jj
				(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
				temp=dialog_message('���ɹ�',/information,title='��ʾ')
;			endif
;		endif


;		(*PSTATE).Input_Yes_ON = 'Yes'		;Ĭ�ϵ�'Input_Yes_ON'��'Yes'��ֱ������

	end
;;++++++�����޳�+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	Widget_Info(wWidget, FIND_BY_UNAME='butt_delete'):begin     ;�����޳�

		IF (*PSTATE).sel_lines LE 0 then begin
            temp=dialog_message('δѡ�����ݣ�����ѡ��',/information,title='��ʾ')
            return
        endif

;		widget_control,(*PSTATE).TABLE_data,get_value=arr_data_show
		data_input=*((*PSTATE).Point_sel_data)
;				print,'data_input=',data_input
		lines=(*PSTATE).sel_lines

		year_input=data_input[3,0]

		DBCO=obj_new('IDLdbDatabase')
		DBCO=DBobj

		;(A)�����ݿ���ԭ�е����ݽ���ɾ��

		;ɾ�����ݿ�����Ҫ����ļ�¼�����ͬ�ļ�¼

		FOR i=0,lines-1 do begin

			SQL='DELETE FROM CROP_PLANT_PROPORTION_ORI_SEL WHERE '
			SQL=SQL+'YEAR = '+STRTRIM(year_input,2)
			SQL=SQL+' AND SU_OR_AU = '+STRTRIM((*PSTATE).SU_OR_AU,2)  ;0Ϊ������1Ϊ����
			SQL=SQL+' AND COUNTY_CODE = '+"'"+STRTRIM(data_input[2,i],2)+"'"     ;��ͬʡ�ݵ�ѡ��    20070406���

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
			   	  temp=dialog_message('�޷�ɾ������ȷ�����ݿ���ȷ����',/error,title='����')
			      return
			   	ENDIF


		print,'�ܹ�ɾ����¼��',lines
		(*PSTATE).Input_Success_County ='Yes'		;�ж����Ƶ������Ƿ��Ѿ����
		temp=dialog_message('ɾ���ɹ�',/information,title='��ʾ')


	end

;;;*******************************************************************************************
;;;*******************************************************************************************
;
;;;===========================================================================
    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;�ر�

		widget_control,event.top,/destroy

  end 		;�رս���
;;;*******************************************************************************************
;;;*******************************************************************************************
    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;����

		if file_test('HELP\HELP.chm') then begin
			ONLINE_HELP, '����ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
		endif else begin
			info_help=dialog_message('�Ҳ��������ĵ�',title='����')
		endelse
	end
;		ONLINE_HELP,'����ģ��',  BOOK='HELP\HELP.chm', /FULL_PATH
;;        temp=dialog_message('ϵͳ��û�а���')
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
		;        XLabel=['��С��','��С��','�絾','�е�','��','������','������','��']
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
		;		if total(float(AnalysisData)) ne 0 then begin    ;��������������
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

end			; event �Ľ���

;;*******************************************************************************************
;;*******************************************************************************************
pro MJ_pretreatment_PROPORTION, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

IF ( XREGISTERED('MJ_pretreatment_PROPORTION') NE 0 ) THEN RETURN

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=180 ,YOFFSET=200 ,SCR_XSIZE=624 ,SCR_YSIZE=475,TLB_FRAME_ATTR =1  $
      ,TITLE='��ֲ��������ɸѡ' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_1 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_1'   $
      ,XOFFSET=6 ,YOFFSET=59 ,SCR_XSIZE=622 ,SCR_YSIZE=336  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  ROW_LABEL=strtrim(indgen(16),2)
;  labels_county=['NZ_Code','County_name','County_code','Year','Proportion',$
;                      'SU_OR_AU'    ]   ;��6��
  labels_county=['ũҵ��������','����','�ش���','���','��ֲ����',$
                      '������'    ]   ;��6��
;  labels_province=['Province_name','Province_code','year','Proportion', $
;                      'SU_OR_AU'    ]    ;��5��
  labels_province=['ʡ��','ʡ����','���','��ֲ����', $
                      '������'    ]    ;��5��
  TABLE_data = Widget_Table(WID_BASE_1, UNAME='TABLE_data' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=4 ,SCR_XSIZE=606 ,SCR_YSIZE=327 ,XSIZE=6  $
      ,/RESIZEABLE_COLUMNS,ALIGNMENT=0,ROW_LABELS=ROW_LABEL  $
      ,/align_center,y_scroll_size=16   $
      ,column_labels=labels_county,/ALL_EVENTS);,EVENT_PRO='Data_Select')
	WIDGET_CONTROL,TABLE_data,SET_TABLE_SELECT=[-1,-1,-1,-1]   ;�ý������ʼʱ����Ӱ


  WID_BASE_3 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=402 ,SCR_XSIZE=606 ,SCR_YSIZE=34  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  WID_BASE_4 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_4' ,FRAME=1  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=606 ,SCR_YSIZE=46 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


Value_Ext_Type=['����������','��ũҵ����']
Droplist_Ext_Type=widget_droplist(WID_BASE_4,/no_copy,$
                  uname='Droplist_Ext_Type' ,$
                  value=Value_Ext_Type,title='����ʽ',$
                  scr_xsize=160,Xoffset=369,YOFFSET=10)


  WID_BUTTON_6 = Widget_Button(WID_BASE_3, UNAME='butt_delete'  $
      ,XOFFSET=280 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�����޳�')


  WID_BUTTON_7 = Widget_Button(WID_BASE_3, UNAME='butt_Close'  $
      ,XOFFSET=510 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�ر�')


  WID_BUTTON_8 = Widget_Button(WID_BASE_3, UNAME='butt_Help'  $
      ,XOFFSET=408 ,YOFFSET=5 ,SCR_XSIZE=60 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_BUTTON_9 = Widget_Button(WID_BASE_3, UNAME='butt_ImportData'  $
      ,XOFFSET=30 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='��������')


  WID_BUTTON_10 = Widget_Button(WID_BASE_3, UNAME='butt_data_to_DB'  $
      ,XOFFSET=155 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����ɸѡ��')


; Stat_To_Province = Widget_Button(WID_BASE_3, UNAME='Stat_To_Province'  $
;      ,XOFFSET=217 ,YOFFSET=5 ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
;      ,/ALIGN_CENTER ,VALUE='���ܵ�ʡ')


  WID_LABEL_1 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_1'  $
      ,XOFFSET=85,YOFFSET=15 ,SCR_XSIZE=132 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='��ѡ�񣺽���ɸѡ������')

;;-------------------------�Ƿ�ʹ��ѡ��������-------------
;	  Draw_BASE= Widget_Base(WID_BASE_4, UNAME='Draw_BASE'   $
;	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,ROW=1,xoffset=490,yoffset=15,/NONEXCLUSIVE $
;	     ,/BASE_ALIGN_center)
;		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
;		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='ʹ��ѡ��������' );$
;		      ;,EVENT_PRO='DC_DisplayShapeEV')
;  ;-----------------------------------------------------------------


  ARRYEAR=STRTRIM(INDGEN(36)+1980,2)
  DROPLIST_year = Widget_combobox(WID_BASE_4, UNAME='DROPLIST_year'  $
      ,XOFFSET=222 ,YOFFSET=11 ,SCR_XSIZE=50 ,SCR_YSIZE=23 ,VALUE=ARRYEAR)
  temp=(bin_date())[0]-1980+1
  widget_control,DROPLIST_year, SET_COMBOBOX_SELECT=temp-1


  ARR_SU_OR_AU=['����(0)','����(1)']
  DROPLIST_SU_OR_AU = Widget_Droplist(WID_BASE_4, UNAME='DROPLIST_SU_OR_AU'  $
      ,XOFFSET=292 ,YOFFSET=11 ,SCR_XSIZE=68 ,SCR_YSIZE=27 ,VALUE=ARR_SU_OR_AU)

	temp=(bin_date())[1]
	if temp ge 3 and temp le 8 then begin
		SU_OR_AU=0		;0��ʾΪ����
	endif else begin
		SU_OR_AU=1		;1��ʾΪ����
	endelse
   WIDGET_CONTROL,DROPLIST_SU_OR_AU,SET_DROPLIST_SELECT=SU_OR_AU


  WID_LABEL_2 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_2'  $
      ,XOFFSET=274 ,YOFFSET=15 ,SCR_XSIZE=18 ,SCR_YSIZE=11  $
      ,/ALIGN_LEFT ,VALUE='��')


  Point_arr_show=ptr_new()
  Point_sel_data=ptr_new()

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_7,/INPUT_FOCUS


	STATE = { $
  		TABLE_data	:	TABLE_data , $
  		Point_arr_show	:	Point_arr_show, $	;���ڻ�ȡ�������е�����
  		sel_lines	:	0,  $
  		UseCell		:	[-1,-1,-1,-1],   $
  		Point_sel_data	:	Point_sel_data , $
  		ARRYEAR		:   ARRYEAR ,$
  		NUM			:	0,  $       ;��Ч��¼��Ŀ
  		NUM_SHOW	:	0,  $		;�ڽ��������ʾ�ļ�¼��Ŀ
  		Ext_Type	:	0,  $
  		Ext_Flag	:	0,  $
  		SU_OR_AU	:	SU_OR_AU,  $
  		Input_Flag	:	0,  $		;����Ƿ��Ѿ���������
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