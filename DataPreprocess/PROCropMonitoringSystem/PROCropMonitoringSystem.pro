;========���ڹر�ϵͳʱ����ڴ�=====================
pro clearwarp, id
;   widget_control,id,get_uvalue = pstate
;   PTR_FREE,pstate
;	TIPS = DIALOG_MESSAGE('ȷ��Ҫ�˳�ʡ��ũ��ң�м��ϵͳ?',TITLE='��ʾ',/QUESTION)
;
;	if TIPS EQ 'No' then return

   COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;Ҳ���Բ���ʼ��
   OBJ_DESTROY, DBobj
   COMMON COMMON_SETPATH, ppath
   ptr_free, ppath
   HEAP_GC , /VERBOSE
   exit,/NO_CONFIRM
end
;=========================================================================

pro TLB_BASE_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  ;��ȡ��������
  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;Ҳ���Բ���ʼ��
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE

  wGroup = BASE_TOP

  ;========��������ӣ�20070808=====================
	IF yesORno EQ 0 THEN BEGIN
		widget_control,Widget_Info(wWidget, FIND_BY_UNAME='DataManger_TOP') ,sensitive=0
		widget_control,Widget_Info(wWidget, FIND_BY_UNAME='CropAnalysis') ,sensitive=0
	endif
;=========================================================================

  case wTarget of
		;������ƶ�
		Widget_Info(wWidget, FIND_BY_UNAME='TLB_BASE'): begin
	      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TLB_MOVE' ) THEN BEGIN
	        WIDGET_CONTROL,Event.top,get_UVALUE = pstate
	        X_OFFSET=EVENT.X
	        Y_OFFSET=EVENT.Y
;	        PRINT,X_OFFSET,Y_OFFSET

		  END
	    end

		;ϵͳ�趨
		Widget_Info(wWidget, FIND_BY_UNAME='SystemSettingBU'): begin
   		   	SD_INTERFACE
   		   	PRINT,'������س���'           ;���ó������
		END

		;���ݹ���
		Widget_Info(wWidget, FIND_BY_UNAME='DataManagerBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
		      ;SJ_Interface,GROUP_LEADER=wGroup
		      SJ_Interface,GROUP_LEADER=Event.top
		    ENDELSE
		END

		;���Ƽ��
		Widget_Info(wWidget, FIND_BY_UNAME='GrowthBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		     ZS_INTERFACE,GROUP_LEADER=Event.top
		      PRINT,'������س���'            ;���ó������
		    ENDELSE
		END

		;����Ԥ��
		Widget_Info(wWidget, FIND_BY_UNAME='YieldBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
		      ;DC,GROUP_LEADER=groupleader
		      DC,GROUP_LEADER=Event.top   ;�޸�-------------------------------------------------------;

		    ENDELSE
		END

		;�������
		Widget_Info(wWidget, FIND_BY_UNAME='AreaBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
;		      MJ_Interface
		      MJ_Interface,GROUP_LEADER=Event.top
		    ENDELSE
		END

		;��������
		Widget_Info(wWidget, FIND_BY_UNAME='ProductionBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
		      ;CL,GROUP_LEADER=groupleader
		      CL,GROUP_LEADER=Event.top   ;�޸�-------------------------------------------------------
		    ENDELSE
		END

		;ũ������
		Widget_Info(wWidget, FIND_BY_UNAME='AgroMeteoBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
		      ;NQ_Interface,GROUP_LEADER=wGroup
		      NQ_Interface,GROUP_LEADER=Event.top
		    ENDELSE
		END

		;����ָ��
		Widget_Info(wWidget, FIND_BY_UNAME='MutipleCroppingBU'): begin
		     IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
		      FZ,GROUP_LEADER=Event.top   ;�޸�-------------------------------------------------------
		    ENDELSE
		END

		;ҵ�����
		Widget_Info(wWidget, FIND_BY_UNAME='ProcessManagerBU'): begin
		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'������س���'            ;���ó������
		      GL_PRO;,group_leader=GLleader
		    ENDELSE
		END

		;*****************************************************************************
		;�����￪ʼ�ǲ˵��Ĳ���
		;----�������ݿ�
		Widget_Info(wWidget, FIND_BY_UNAME='Conn_Database'):begin;Connect_DataBase
			 IF(yesORno EQ 1) THEN BEGIN
				TEMP=DIALOG_MESSAGE('���ݿ��Ѿ�����,Ҫ���½�������ô?',/QUESTION,title='�Ƿ����½�������')
				print,temp
				IF(TEMP EQ 'No') THEN RETURN
			 ENDIF
	         SD_DB_CONNECT, GROUP_LEADER=wGroup
	    end

	    ;----�Ͽ����ݿ�
;		Widget_Info(wWidget, FIND_BY_UNAME='Disconn_database'):begin;'�Ͽ�����')
;			 WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;			 IF(yesORno EQ 1) THEN BEGIN
;				DBobj=OBJ_NEW('IDLdbDatabase')
;				yesORno=0
;				;ʹ�����˵�����
;	            WIDGET_CONTROL,MENU_OPERATION,SENSITIVE=0
;	            WIDGET_CONTROL,MENU_MANAGE,SENSITIVE=0
;				TEMP=DIALOG_MESSAGE('�ѶϿ����ݿ�',TITLE='����',/INFORMATION)
;			 ENDIF ELSE BEGIN
;				TEMP=DIALOG_MESSAGE('���ݿⲢû������',TITLE='����',/INFORMATION)
;			 ENDELSE;INTERFACE_MONITOR_RT,PSTATE, GROUP_LEADER=(*pstate).BASE_TOP_GROWTH_MONITOR
;	    end

		;----·���趨
		Widget_Info(wWidget, FIND_BY_UNAME='PATH_SETUP'):SD_Preference, GROUP_LEADER=wGroup                    ;·���趨

		;----�Զ�������
		Widget_Info(wWidget, FIND_BY_UNAME='ROI_DEFINE'):BEGIN
			IF yesORno EQ 0 THEN BEGIN
		      	no_conn_info=DIALOG_MESSAGE('����û�н������ݿ����ӣ�',TITLE='��ʾ',/INFORMATION)
		    ENDIF ELSE BEGIN
		      	PRINT,'������س���'            ;���ó������
		     	SD_ROI_Manage, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
		    ENDELSE
		       ;���������
		END
		;----�˳�
		Widget_Info(wWidget, FIND_BY_UNAME='Exit_System'):begin;�˳�
			TIPS = DIALOG_MESSAGE('ȷ��Ҫ�˳�ʡ��ũ��ң�м��ϵͳ?',TITLE='��ʾ',/QUESTION)

			if TIPS EQ 'No' then return
			common_log,'�˳�ʡ��ũ��ң�м��ϵͳ'
			 CLOSE,/ALL
			 HEAP_GC , /VERBOSE  ;��������ӣ����ڹر�ϵͳʱ����ڴ棬20070717
			 widget_control,event.top,/destroy
			 exit,/NO_CONFIRM
	    end

	    ;----���ݼ��
		Widget_Info(wWidget, FIND_BY_UNAME='DATA_CHECK'):SJ_abnormalcheck, GROUP_LEADER=event.top, _EXTRA=_VWBExtra_


		;----�������
		Widget_Info(wWidget, FIND_BY_UNAME='DATA_LOAD'):SJ_todatabase, GROUP_LEADER=event.top


		;----����ά��
		Widget_Info(wWidget, FIND_BY_UNAME='Data_MODIFY'):SJ_datamaintance, GROUP_LEADER=event.top, _EXTRA=_VWBExtra


		;----��־����
		Widget_Info(wWidget, FIND_BY_UNAME='LOG_MANAGER'):	log_mgr,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----��������
		Widget_Info(wWidget, FIND_BY_UNAME='MISSION_STATUS'):	begin
			CAUTION = Dialog_Message('�Ƿ�ȷ��Ҫ������������?',TITLE='��ʾ',/QUESTION)
			if CAUTION EQ 'No' then return

			log, 'all',-1
			logfile = '.\text\log.txt'
			OPENW, lun, logfile, /GET_LUN
			PRINTF, lun, 'logfile,�����1,δ���-1,�������0'
			CLOSE, lun
			FREE_LUN, lun
		end

		;----ͼ����ʾ
		Widget_Info(wWidget, FIND_BY_UNAME='Viewer'):COMMON_VIEWER, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----���ξ���
		Widget_Info(wWidget, FIND_BY_UNAME='Geo_Corr'):GEO_CORRECTION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----���ξ���
		Widget_Info(wWidget, FIND_BY_UNAME='Terr_Corr'):TOPOGRAPHIC_CORRECTION, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----Ӱ�����
		Widget_Info(wWidget, FIND_BY_UNAME='Type_Trans'):FORMAT_CONVERTOR, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----Ӱ�����
		Widget_Info(wWidget, FIND_BY_UNAME='Layer_Stack'):LAYER_STACKING, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----ֲ��ָ������
		Widget_Info(wWidget, FIND_BY_UNAME='VI'):AGM_VI_Calculation, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----Ҷ���ָ������
		Widget_Info(wWidget, FIND_BY_UNAME='LAI'):LAI_Calculation, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----����FPAR
		Widget_Info(wWidget, FIND_BY_UNAME='FPAR'):FPAR_CALCULATION, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----��߲���ģ��
		Widget_Info(wWidget, FIND_BY_UNAME='RED_EDGE'):RED_EDGE_SIMULATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----�߹������ݴ���
		Widget_Info(wWidget, FIND_BY_UNAME='HSI'):preHSI_base,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----AVHRR����Ԥ����
		Widget_Info(wWidget, FIND_BY_UNAME='AVHRR_Geo'):AVHRR_PREPROCESSING,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----AVHRR����Ԥ����
		Widget_Info(wWidget, FIND_BY_UNAME='MODIS_Geo'):DROUGHT_PRE_GUI,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----������CCD���ݷ��䶨��
		Widget_Info(wWidget, FIND_BY_UNAME='HJ_Cal'):HJ_CALIBRATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----̫��λ�ò�������
		Widget_Info(wWidget, FIND_BY_UNAME='Solar_Position'):SOLPOS_CALCULATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----�Ǽල����
		Widget_Info(wWidget, FIND_BY_UNAME='UNCLS'):UNSUPERVISED_CLASSIFICATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----������ر���
		Widget_Info(wWidget, FIND_BY_UNAME='CLS_RECODE'):RECODE, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----�����ѯ
		Widget_Info(wWidget, FIND_BY_UNAME='MISSION_QUERY'):GL_xianshi, GROUP_LEADER=event.top

		;----�������
		Widget_Info(wWidget, FIND_BY_UNAME='MISSION_LOAD'):GL_diaodu,GROUP_LEADER=event.top, _EXTRA=_VWBExtra_

		;----�����ֵ
		Widget_Info(wWidget, FIND_BY_UNAME='AGRO_INTERPOLATE'):NQ_InterPolate_intermain, GROUP_LEADER=event.top

		;----�Աȷ���
		Widget_Info(wWidget, FIND_BY_UNAME='AGRO_COMPARE'):NQ_classify_classifyagri, GROUP_LEADER=event.top

		;----��ֵ�ּ�
		Widget_Info(wWidget, FIND_BY_UNAME='CLASSIFY_RT'):ZS_classify, GROUP_LEADER=event.top

		;----ͳ�Ƶ���
		Widget_Info(wWidget, FIND_BY_UNAME='STA_RT'):ZS_RT_STA, GROUP_LEADER=event.top

		;----����
		Widget_Info(wWidget, FIND_BY_UNAME='SUM_RT'):ZS_STA_RT_ABOVE_COUNTY, GROUP_LEADER=event.top

		;----ͳ�Ƶ���
		Widget_Info(wWidget, FIND_BY_UNAME='STA_PRO'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=event.top

		;----�������ع�
		Widget_Info(wWidget, FIND_BY_UNAME='HANTS_PRO'):ZS_HANTS, GROUP_LEADER=event.top

		;----����
		Widget_Info(wWidget, FIND_BY_UNAME='SUM_PRO'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=event.top

		;----�������
		Widget_Info(wWidget, FIND_BY_UNAME='ANA_GROWTH'):ZS_JPG, GROUP_LEADER=event.top

		;----���Ʋ�������
		Widget_Info(wWidget, FIND_BY_UNAME='Y_TREND_AN'):DC_TrendYield, GROUP_LEADER=event.top
		;----������������
		Widget_Info(wWidget, FIND_BY_UNAME='Y_FLU_AN_COMPUTE'):DC_FloatYield, GROUP_LEADER=event.top
		;----�ռ��ֵ����
		Widget_Info(wWidget, FIND_BY_UNAME='Y_FLU_AN_EXP'):DC_SpatialExtrapolate, GROUP_LEADER=event.top
		;----�򵥱�ֵ������
		Widget_Info(wWidget, FIND_BY_UNAME='Y_BIOMASS_ratio'):DC_Bio_Ratio, GROUP_LEADER=event.top
		;----�ջ�ָ��������
		Widget_Info(wWidget, FIND_BY_UNAME='Y_BIOMASS_HI'):DC_Bio_HI, GROUP_LEADER=event.top
		;----�����ںϷ���
		Widget_Info(wWidget, FIND_BY_UNAME='Y_FUSE'):DC_YieldMerge, GROUP_LEADER=event.top
		;----�����ʾ���
		Widget_Info(wWidget, FIND_BY_UNAME='Y_SHOW_OUTPUT'):DC_SynthesisOutput, GROUP_LEADER=event.top


;		;----�Ǽල����
;		Widget_Info(wWidget, FIND_BY_UNAME='AREA_CLASSIFY'):ABOUT, GROUP_LEADER=event.top
;		;----�����ʶ
;		Widget_Info(wWidget, FIND_BY_UNAME='AREA_LABEL'):ABOUT, GROUP_LEADER=event.top
		;----ͳ��
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_STA'):MJ_statistical, GROUP_LEADER=event.top

		Widget_Info(wWidget, FIND_BY_UNAME='GVG_IMP'):GVG_TO_DATABASE, GROUP_LEADER=event.top, _EXTRA=_VWBExtra         ;GVG�������
;		;----����
;		Widget_Info(wWidget, FIND_BY_UNAME='AREA_EXP'):ABOUT, GROUP_LEADER=event.top

		;----GVG����
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_EXP_GVG'):MJ_Extrapolate_GVG, GROUP_LEADER=event.top
		;----��ֲ��������
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_EXP_PROPORTION'):MJ_Extrapolate_Proportion, GROUP_LEADER=event.top
		;----�������
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_COMPUTE'):MJ_tocalculate, GROUP_LEADER=event.top

		;----��������
		Widget_Info(wWidget, FIND_BY_UNAME='Production'):CL, GROUP_LEADER=event.top
		;----����ָ������
		Widget_Info(wWidget, FIND_BY_UNAME='CI_COMPUTE'):FZ_calculate, GROUP_LEADER=event.top
		;----����ָ��ͳ��
		Widget_Info(wWidget, FIND_BY_UNAME='CI_STA'):FZ_statistic, GROUP_LEADER=event.top
		;----�����ĵ�
		Widget_Info(wWidget, FIND_BY_UNAME='HelpDocument'):begin
			common_log,'��������'
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end

		;----��ϵ����
		Widget_Info(wWidget, FIND_BY_UNAME='CONNECT_US'):CONTACT, GROUP_LEADER=event.top

		;----����ϵͳ
		Widget_Info(wWidget, FIND_BY_UNAME='AboutSystem'):ABOUT, GROUP_LEADER=event.top

		;*****************************************************************************

	   widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_close'): begin
	     TIPS = DIALOG_MESSAGE('ȷ��Ҫ�˳�ʡ��ũ��ң�м��ϵͳ?',TITLE='��ʾ',/QUESTION)

		  if TIPS EQ 'No' then return
	   	common_log,'�ر�ʡ��ũ��ң�м��ϵͳ'
		  widget_control,event.top,/destroy
		  exit, /NO_CONFIRM
	   end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_help'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '�������', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end

;		ONLINE_HELP, '�������', BOOK='HELP\HELP.chm', /FULL_PATH
;        temp=dialog_message('ϵͳ��û�а���')
;     end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_GL_mis_search'):GL_xianshi,GROUP_LEADER=wGroup            ;������ʾ
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datainput'):SJ_todatabase, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra          ;���ݵ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datamaintance'):SJ_datamaintance, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra   ;����ά��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_InterPolate'):NQ_InterPolate_intermain, GROUP_LEADER=wGroup;�����ֵ
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_classify'):NQ_classify_classifyagri, GROUP_LEADER=wGroup   ;�Աȷ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_calculate'):FZ_calculate, GROUP_LEADER=wGroup       ;ָ����ȡ
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_statistic'):FZ_statistic, GROUP_LEADER=wGroup       ;�ռ�ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_trendyield'):DC_TrendYield, GROUP_LEADER=wGroup     ;���Ʋ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_floatyield'):DC_FloatYield, GROUP_LEADER=wGroup     ;��������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_yieldmerge'):DC_YieldMerge,GROUP_LEADER=wGroup ;�����ں�
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_21'):SD_ROI_Manage, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_      ;���������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_22'):SD_Preference, GROUP_LEADER=wGroup                    ;·���趨
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_23'):SD_DB_Connect, GROUP_LEADER=wGroup                    ;���ݿ�����
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_24'):SJ_abnormalcheck, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_           ;�쳣���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_29'):ZS_PRO_STA, GROUP_LEADER=wGroup                       ;ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_30'):ZS_HANTS, GROUP_LEADER=wGroup                         ;�����ع�
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_31'):ZS_classify, GROUP_LEADER=wGroup                      ;��ֵ����
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_32'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=wGroup          ;���ܣ����̣�
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_33'):ZS_STA_RT_ABOVE_COUNTY, GROUP_LEADER=wGroup           ;���ܣ�ʵʱ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_34'):ZS_RT_STA, GROUP_LEADER=wGroup                        ;�ռ�ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_0'):ZS_JPG, GROUP_LEADER=WGROUP
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_35'):MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;��������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_36'):GVG_TO_DATABASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra         ;GVG�������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_37'):MJ_Extrapolate_Proportion, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;��������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_38'):MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_       ;���ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_39'):MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_    ;�������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_43'):CL, GROUP_LEADER=wGroup                               ;��������

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    else:
  endcase
;==============��������ӣ����ڹر�ϵͳʱ����ڴ棬20070809=================================
	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' then begin

;	     CROP_MONITOR_exit=DIALOG_MESSAGE('ȷ���˳����ϵͳ��?',TITLE='ѯ��',/QUESTION)
;	     if CROP_MONITOR_exit eq 'Yes' then begin
			TIPS = DIALOG_MESSAGE('ȷ��Ҫ�˳�ʡ��ũ��ң�м��ϵͳ?',TITLE='��ʾ',/QUESTION)

			if TIPS EQ 'No' then return

			COMMON COMMON_SETPATH, ppath
			ptr_free, ppath
	     	HEAP_GC , /VERBOSE  ;��������ӣ����ڹر�ϵͳʱ����ڴ棬20070809
	      widget_control,event.top,/DESTROY
	      exit,/NO_CONFIRM
;	     endif

  	ENDIF
  ;====================================================================

end
;/////////////////////////////////////////////////////////////////////////////////
pro TLB_BASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'����ʡ��ũ��ң�м��ϵͳ'

	C_COMMON_BLOCK   ;���й�������

	PRO_COMMON_SETPATH

	common_current_date

  TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
      ,XOFFSET=140 ,YOFFSET=0 ,/TLB_MOVE_EVENTS  $  $
      ,TITLE='ʡ��ũ��ң�м��ϵͳ' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 ,MBAR=TLB_BASE_MBAR,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)	;���������/TLB_KILL_REQUEST_EVENTS��20070809

;*********************************************************************
  Configuration = Widget_Button(TLB_BASE_MBAR, UNAME='Configuration'  $
      ,XOFFSET=3 ,YOFFSET=0 ,/MENU ,VALUE='ϵͳ(&S)')


;  DataBaseConnecton = Widget_Button(Configuration,  $
;      UNAME='DataBaseConnecton' ,/MENU ,VALUE='���ݿ�����')


  Conn_Database = Widget_Button(Configuration,  $
      UNAME='Conn_Database' ,VALUE='���ݿ�����')


;  Disconn_database = Widget_Button(DataBaseConnecton,  $
;      UNAME='Disconn_database' ,VALUE='�Ͽ�����')

;---------------------------------------------------------
  PATH_SETUP = Widget_Button(Configuration, UNAME='PATH_SETUP'  $
      ,/SEPARATOR ,VALUE='·���趨')


  ROI_DEFINE = Widget_Button(Configuration, UNAME='ROI_DEFINE'  $
      ,VALUE='��������')


;  DefaultSet = Widget_Button(Configuration, UNAME='DefaultSet'  $
;      ,VALUE='ϵͳ����')


  Exit_System = Widget_Button(Configuration, UNAME='Exit_System'  $
      ,/SEPARATOR ,VALUE='�˳�(&E)')

;*****************************************************************************
  DataManger_TOP = Widget_Button(TLB_BASE_MBAR, UNAME='DataManger_TOP' ,/MENU  $
      ,VALUE='����(&M)',SENSITIVE=0)

  DataManger = Widget_Button(DataManger_TOP, UNAME='DataManger' ,/MENU  $
      ,VALUE='���ݹ���')


  DATA_CHECK = Widget_Button(DataManger, UNAME='DATA_CHECK'  $
      ,VALUE='���ݼ��')


  DATA_LOAD = Widget_Button(DataManger, UNAME='DATA_LOAD'  $
      ,VALUE='�������')


  Data_MODIFY = Widget_Button(DataManger, UNAME='Data_MODIFY'  $
      ,VALUE='����ά��')


  BUSINESS_Manger = Widget_Button(DataManger_TOP, UNAME='BUSINESS_Manger' ,/MENU  $
      ,VALUE='ҵ�����')


  MISSION_QUERY = Widget_Button(BUSINESS_Manger, UNAME='MISSION_QUERY'  $
      ,VALUE='�����ѯ')


  MISSION_LOAD = Widget_Button(BUSINESS_Manger, UNAME='MISSION_LOAD'  $
      ,VALUE='�������')

  LOG_MANAGER = Widget_Button(DataManger_TOP, UNAME='LOG_MANAGER'   $
      ,VALUE='��־����')

  MISSION_STATUS = Widget_Button(DataManger_TOP, UNAME='MISSION_STATUS'   $
      ,VALUE='��������')
;*********************************************************************************************
  Data_Prep = Widget_Button(TLB_BASE_MBAR, UNAME='Data_Prep' ,/MENU  $
      ,VALUE='Ԥ����(&P)',SENSITIVE=1)

;   Atmos = Widget_Button(Data_Prep, UNAME='Atmos',VALUE='��������')

;   Geo_Corr = Widget_Button(Data_Prep, UNAME='Geo_Corr',/MENU,VALUE='���ξ���')


	Solar_Position = Widget_Button(Data_Prep, UNAME='Solar_Position',VALUE='̫��λ�ò�������')
	Calibration = Widget_Button(Data_Prep, UNAME='Data_Spec',/MENU,VALUE='���䶨��')
		HJ_Cal = Widget_Button(Calibration, UNAME='HJ_Cal',VALUE='������ CCD')
;		ETM_Cal = Widget_Button(Calibration, UNAME='ETM_Cal',VALUE='ETM')

	Geo_Corr = Widget_Button(Data_Prep, UNAME='Geo_Corr',/SEPARATOR,VALUE='���ξ���')

	Terr_Corr = Widget_Button(Data_Prep, UNAME='Terr_Corr',/SEPARATOR,VALUE='���ξ���')
;		C_Corr = Widget_Button(Terr_Corr, UNAME='C_Corr',VALUE='C ����')
;		M_Corr = Widget_Button(Terr_Corr, UNAME='M_Corr',VALUE='Minnaert ����')

	Type_Trans = Widget_Button(Data_Prep, UNAME='Type_Trans',/SEPARATOR,VALUE='��ʽת��')
	Layer_Stack = Widget_Button(Data_Prep, UNAME='Layer_Stack',VALUE='ͼ�����')

	Viewer = Widget_Button(Data_Prep, UNAME='Viewer',/SEPARATOR,VALUE='ͼ����ʾ')

   Index_Cal = Widget_Button(Data_Prep, UNAME='Index_Cal',/SEPARATOR,/MENU,VALUE='ָ������')
   	VI = Widget_Button(Index_Cal, UNAME='VI',VALUE='ֲ��ָ��')
   	LAI = Widget_Button(Index_Cal, UNAME='LAI',VALUE='Ҷ���ָ��')
;   	NPP = Widget_Button(Index_Cal, UNAME='NPP',VALUE='NPP')
   	FPAR = Widget_Button(Index_Cal, UNAME='FPAR',VALUE='FPAR')
   	RED_EDGE = Widget_Button(Index_Cal, UNAME='RED_EDGE',VALUE='ֲ����߲���ģ��')

	Data_Spec = Widget_Button(Data_Prep, UNAME='Data_Spec',/SEPARATOR,/MENU,VALUE='�ض����ݴ���')
		HSI = Widget_Button(Data_Spec, UNAME='HSI',VALUE='�߹������ݴ���')
		AVHRR_Geo = Widget_Button(Data_Spec, UNAME='AVHRR_Geo',VALUE='AVHRR���ݴ���')
		MODIS_Geo = Widget_Button(Data_Spec, UNAME='MODIS_Geo',VALUE='MODIS���ݴ���')

	UNCLS = Widget_Button(Data_Prep, UNAME='UNCLS',/SEPARATOR,VALUE='�Ǽල����')
	CLS_RECODE = Widget_Button(Data_Prep, UNAME='CLS_RECODE',VALUE='�����ر���')
;*********************************************************************************************
  CropAnalysis = Widget_Button(TLB_BASE_MBAR, UNAME='CropAnalysis',/MENU ,VALUE='ũ�����(&A)',SENSITIVE=0)

  AgroMeteoAnalysis = Widget_Button(CropAnalysis,UNAME='AgroMeteoAnalysis',/MENU ,VALUE='ũҵ�������')
  AGRO_INTERPOLATE = Widget_Button(AgroMeteoAnalysis,UNAME='AGRO_INTERPOLATE' ,VALUE='�����ֵ')
  AGRO_COMPARE = Widget_Button(AgroMeteoAnalysis,UNAME='AGRO_COMPARE',VALUE='�Աȷ���')

  Growth = Widget_Button(CropAnalysis, UNAME='Growth',/MENU ,/SEPARATOR,VALUE='���Ƽ��')
  GROWTH_RT = Widget_Button(Growth, UNAME='GROWTH_RT',/MENU ,VALUE='ʵʱ���')
  CLASSIFY_RT = Widget_Button(GROWTH_RT, UNAME='CLASSIFY_RT',VALUE='��ֵ�ּ�')
  STA_RT = Widget_Button(GROWTH_RT, UNAME='STA_RT',VALUE='ͳ�Ƶ���')
  SUM_RT = Widget_Button(GROWTH_RT, UNAME='SUM_RT',VALUE='����')

  GROWTH_PRO = Widget_Button(Growth, UNAME='GROWTH_PRO',/MENU ,VALUE='���̼��')
  STA_PRO = Widget_Button(GROWTH_PRO, UNAME='STA_PRO',VALUE='ͳ�Ƶ���')
  HANTS_PRO = Widget_Button(GROWTH_PRO, UNAME='HANTS_PRO',VALUE='�����ع�')
  SUM_PRO = Widget_Button(GROWTH_PRO, UNAME='SUM_PRO',VALUE='����')

  ANA_GROWTH = Widget_Button(Growth, UNAME='ANA_GROWTH' ,/SEPARATOR,VALUE='�������ͼ')

  Yield = Widget_Button(CropAnalysis, UNAME='Yield',/MENU,VALUE='����Ԥ��')
  Y_TREND_AN=Widget_Button(Yield, UNAME='Y_TREND_AN',VALUE='���Ʋ�������')
  Y_FLU_AN=Widget_Button(Yield, UNAME='Y_FLU_AN',/MENU,VALUE='������������')
  Y_FLU_AN_COMPUTE=Widget_Button(Y_FLU_AN, UNAME='Y_FLU_AN_COMPUTE',VALUE='������������')
  Y_FLU_AN_EXP=Widget_Button(Y_FLU_AN, UNAME='Y_FLU_AN_EXP',VALUE='�ռ��ֵ����')
;  Y_BIOMASS=Widget_Button(Yield, UNAME='Y_BIOMASS',/MENU,VALUE='��������������')
;  Y_BIOMASS_ratio=Widget_Button(Y_BIOMASS, UNAME='Y_BIOMASS_ratio',VALUE='�򵥱�ֵ������')
;  Y_BIOMASS_HI=Widget_Button(Y_BIOMASS, UNAME='Y_BIOMASS_HI',VALUE='�ջ�ָ��������')
  Y_FUSE=Widget_Button(Yield, UNAME='Y_FUSE' ,/SEPARATOR,VALUE='�����ںϷ���')
  Y_SHOW_OUTPUT=Widget_Button(Yield, UNAME='Y_SHOW_OUTPUT',VALUE='�����ʾ���')


  Area = Widget_Button(CropAnalysis,UNAME='Area',/MENU ,VALUE='�������')
  ;AREA_CLASSIFY = Widget_Button(Area, UNAME='AREA_CLASSIFY',VALUE='�Ǽල����')
  ;AREA_LABEL = Widget_Button(Area, UNAME='AREA_LABEL' ,VALUE='�����ʶ')
  ;AREA_STA = Widget_Button(Area, UNAME='AREA_STA',/SEPARATOR,VALUE='ͳ��')
  AREA_STA = Widget_Button(Area, UNAME='AREA_STA',VALUE='�ռ�ͳ��')
  GVG_IMP = Widget_Button(Area, UNAME='GVG_IMP',VALUE='GVG�������')
  AREA_EXP = Widget_Button(Area, UNAME='AREA_EXP',/MENU,VALUE='����')
  		AREA_EXP_GVG=Widget_Button(AREA_EXP,uname='AREA_EXP_GVG',value='��������')
  		AREA_EXP_PROPORTION=Widget_Button(AREA_EXP,uname='AREA_EXP_PROPORTION',value='��������')
  AREA_COMPUTE = Widget_Button(Area, UNAME='AREA_COMPUTE',VALUE='�������')

  Production = Widget_Button(CropAnalysis,UNAME='Production' ,VALUE='��������')

  Mutiple_cropping = Widget_Button(CropAnalysis,UNAME='Mutiple_cropping' ,/MENU,/SEPARATOR ,VALUE='����ָ��')
  CI_COMPUTE = Widget_Button(Mutiple_cropping,UNAME='CI_COMPUTE' ,VALUE='����ָ����ȡ')
  CI_STA = Widget_Button(Mutiple_cropping,UNAME='CI_STA' ,VALUE='����ָ��ͳ��')

  ;***********************************************************************************************
  SystemHelp = Widget_Button(TLB_BASE_MBAR, UNAME='SystemHelp' ,/MENU  $
      ,VALUE='����(&H)')

  HelpDocument = Widget_Button(SystemHelp, UNAME='HelpDocument' ,VALUE='�����ĵ�')

  CONNECT_US = Widget_Button(SystemHelp, UNAME='CONNECT_US',VALUE='��ϵ����')

  AboutSystem = Widget_Button(SystemHelp, UNAME='AboutSystem',/SEPARATOR ,VALUE='����ϵͳ')

;****************************************************************************
  Modal_BASE = Widget_Base(TLB_BASE, UNAME='Modal_BASE'  $
      ,XOFFSET=3 ,YOFFSET=6,SPACE=6 ,XPAD=3 ,YPAD=3 ,ROW=1, frame=1)

;------------------------------------------------------------------
  SystemSettingBU = Widget_Button(Modal_BASE, UNAME='SystemSettingBU'  $
      ,/ALIGN_Left,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,VALUE='Image\ϵͳ.bmp' ,/BITMAP)

  DataManagerBU = Widget_Button(Modal_BASE,  $
      UNAME='DataManagerBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\����.bmp' ,/BITMAP)

  GrowthBU = Widget_Button(Modal_BASE,  $
      UNAME='GrowthBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\����.bmp' ,/BITMAP)


  YieldBU = Widget_Button(Modal_BASE,  $
      UNAME='YieldBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\����.bmp' ,/BITMAP)

  AreaBU = Widget_Button(Modal_BASE,  $
      UNAME='AreaBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\���.bmp' ,/BITMAP)

  ProductionBU = Widget_Button(Modal_BASE,  $
      UNAME='ProductionBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\����.bmp' ,/BITMAP)

  AgroMeteoBU = Widget_Button(Modal_BASE,  $
      UNAME='AgroMeteoBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\ũ��.bmp' ,/BITMAP)

  MutipleCroppingBU = Widget_Button(Modal_BASE,  $
      UNAME='MutipleCroppingBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\����.bmp' ,/BITMAP)

  ProcessManagerBU = Widget_Button(Modal_BASE,  $
      UNAME='ProcessManagerBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\ҵ��.bmp' ,/BITMAP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

diaodu_Modal_BASE = Widget_Base(TLB_BASE, ROW=1, frame=1)
WID_BASE_1 = Widget_Base(diaodu_Modal_BASE, UNAME='WID_BASE_1'  $
      ,SCR_XSIZE=750 ,SCR_YSIZE=375 )


;  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,FRAME=1  $
;      ,XOFFSET=7 ,YOFFSET=385 ,SCR_XSIZE=748 ,SCR_YSIZE=40  $
;      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)
;
;
;  WID_BUTTON_close = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_close'  $
;      ,XOFFSET=572 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
;      ,/ALIGN_CENTER ,VALUE='�ر�')
;
;
;  WID_BUTTON_help = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_help'  $
;      ,XOFFSET=80 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
;      ,/ALIGN_CENTER ,VALUE='����')


  WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3' ,FRAME=0  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=748 ,SCR_YSIZE=371  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_0 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_0' ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=31 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

;-------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          'ҵ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'

  WID_label_yewu = Widget_label(WID_BASE_0, UNAME='WID_label_yewu' ,XOFFSET=1  $
      ,YOFFSET=0 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104, /ALIGN_CENTER  $
      ,value=text)

;-------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          'ϵ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          'ͳ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'

  WID_label_xitong = Widget_label(WID_BASE_0, UNAME='WID_label_xitong' ,XOFFSET=1  $
      ,YOFFSET=125 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'

  WID_label_shuju = Widget_label(WID_BASE_0, UNAME='WID_label_shuju' ,XOFFSET=1  $
      ,YOFFSET=250 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)
;------------------------------------------------------------------------------------

 WID_BASE_4 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_4' ,XOFFSET=37  $
      ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  WID_LABEL_42 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_42'  $
      ,YOFFSET=48 ,/ALIGN_RIGHT ,VALUE='��')

  WID_LABEL_45 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_45'  $
      ,XOFFSET=11 ,YOFFSET=48 ,/ALIGN_LEFT ,VALUE='-->')

  WID_LABEL_68 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_68'  $
      ,YOFFSET=175 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_69 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_69'  $
      ,XOFFSET=10 ,YOFFSET=161 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_70 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_70'  $
      ,XOFFSET=14 ,YOFFSET=175 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_71 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_71'  $
      ,XOFFSET=14 ,YOFFSET=142 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_72 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_72'  $
      ,XOFFSET=10 ,YOFFSET=147 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_73 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_73'  $
      ,XOFFSET=10 ,YOFFSET=175 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_80 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_80'  $
      ,XOFFSET=10 ,YOFFSET=189 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_81 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_81'  $
      ,XOFFSET=10 ,YOFFSET=203 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_82 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_82'  $
      ,XOFFSET=14 ,YOFFSET=209 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_74 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_74'  $
      ,YOFFSET=302 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_85 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_85'  $
      ,XOFFSET=14 ,YOFFSET=334 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_86 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_86'  $
      ,XOFFSET=14 ,YOFFSET=302 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_87 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_87'  $
      ,XOFFSET=14 ,YOFFSET=269 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_88 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_88'  $
      ,XOFFSET=10 ,YOFFSET=328 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_89 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_89'  $
      ,XOFFSET=10 ,YOFFSET=314 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_90 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_90'  $
      ,XOFFSET=10 ,YOFFSET=300 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_91 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_91'  $
      ,XOFFSET=10 ,YOFFSET=274 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_92 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_92'  $
      ,XOFFSET=10 ,YOFFSET=286 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')

;===============================================================================


  WID_BASE_5 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_5' ,XOFFSET=67  $
      ,YOFFSET=7 ,SCR_XSIZE=96 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  WID_BUTTON_GL_mis_search = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_GL_mis_search'  $
      ,YOFFSET=42 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/�����ѯ.bmp',/bitmap)        ;�����ѯ


  WID_BUTTON_21 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_21'  $   ;���������
      ,YOFFSET=199 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/���������.bmp',/bitmap)


  WID_BUTTON_22 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_22'  $    ;·���趨
      ,YOFFSET=167 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='image/·���趨.bmp',/bitmap)
;      ,VALUE='Image\LUJING01.bmp' ,/BITMAP)


  WID_BUTTON_23 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_23'  $
      ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/���ݿ�����.bmp',/bitmap)       ;���ݿ�����


  WID_BUTTON_24 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_24'  $
      ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/�쳣���.bmp',/bitmap)                      ;�쳣���


  WID_BUTTON_SJ_datamaintance = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datamaintance'  $
      ,YOFFSET=295 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='Image\����ά��.bmp' ,/BITMAP)       ;����ά��


  WID_BUTTON_SJ_datainput = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datainput'  $
      ,YOFFSET=263 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='Image\���ݵ���.bmp' ,/BITMAP)      ;���ݵ���

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          'ũ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          'ң'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          'ҵ'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          'ģ'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'



  WID_label_mokuai = Widget_label(WID_BASE_3, UNAME='WID_label_mokuai'  $
      ,XOFFSET=184,frame=1 ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=353   $
      ,/ALIGN_CENTER,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_zhangshi = Widget_label(WID_BASE_3, UNAME='WID_label_zhangshi'  $
      ,XOFFSET=233 ,YOFFSET=31 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'


  WID_labe_mianji = Widget_label(WID_BASE_3, UNAME='WID_labe_mianji'  $
      ,XOFFSET=233 ,YOFFSET=126 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' Ԥ ��'


  WID_label_danchan = Widget_label(WID_BASE_3, UNAME='label_danchan'  $
      ,XOFFSET=233 ,YOFFSET=198 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' ũ ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_nongqi = Widget_label(WID_BASE_3, UNAME='WID_label_nongqi'  $
      ,XOFFSET=233 ,YOFFSET=257 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------
    text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' ָ ��'

  WID_label_fuzhong = Widget_label(WID_BASE_3, UNAME='WID_label_fuzhong'  $
      ,XOFFSET=233 ,YOFFSET=315 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------


  WID_BUTTON_29 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_29'  $
      ,XOFFSET=366 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��0.bmp' ,/BITMAP)       ;ͳ��

;------------------------------------------------------------------------------------
      text=' �� ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_guocheng = Widget_label(WID_BASE_3, UNAME='WID_label_guocheng'  $
      ,XOFFSET=302 ,YOFFSET=17 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)


      text=  $
          ' ʵ ʱ'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_shishi = Widget_label(WID_BASE_3, UNAME='WID_label_shishi'  $
      ,XOFFSET=302 ,YOFFSET=65 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------
  WID_BUTTON_30 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_30'  $    ;�����ع�
      ,XOFFSET=481 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�����ع�0.bmp' ,/BITMAP )


  WID_BUTTON_31 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_31'  $
      ,XOFFSET=366 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��ֵ�ּ�0.bmp' ,/BITMAP )            ;��ֵ�ּ�


  WID_BUTTON_32 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_32'  $
      ,XOFFSET=594 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\����0.bmp' ,/BITMAP)      ;����


  WID_LABEL_0 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_0'  $
      ,XOFFSET=460 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_1 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_1'  $
      ,XOFFSET=345 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_2 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_2'  $
      ,XOFFSET=345 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_3 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_3'  $
      ,XOFFSET=575 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_4 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_4'  $
      ,XOFFSET=213 ,YOFFSET=45 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_6 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_6'  $
      ,XOFFSET=213 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_7 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_7'  $
      ,XOFFSET=213 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_8 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_8'  $
      ,XOFFSET=213 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_9 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_9'  $
      ,XOFFSET=164 ,YOFFSET=152 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_10 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_10'  $
      ,XOFFSET=164 ,YOFFSET=183 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_11 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_11'  $
      ,XOFFSET=164 ,YOFFSET=215 ,/ALIGN_LEFT ,VALUE='-->')

  WID_LABEL_12 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_12'  $
      ,XOFFSET=460 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_33 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_33'  $
      ,XOFFSET=594 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\����0.bmp' ,/BITMAP)      ;����


  WID_BUTTON_34 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_34'  $
      ,XOFFSET=481 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��0.bmp' ,/BITMAP)     ;�ռ�ͳ��


  WID_LABEL_13 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_13'  $
      ,XOFFSET=574 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')

  WID_BUTTON_0 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_0'  $   ;�������
      ,XOFFSET=714 ,YOFFSET=10 ,SCR_XSIZE=25 ,SCR_YSIZE=88  $
      ,/ALIGN_CENTER ,VALUE='Image\�������0.BMP',/BITMAP)

;-------------------------------------------------------------------------

  WID_BUTTON_35 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_35'  $
      ,XOFFSET=481 ,YOFFSET=160 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��������0.bmp' ,/BITMAP)       ;��������


  WID_LABEL_14 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_14'  $
      ,XOFFSET=460 ,YOFFSET=166 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_5 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_5'  $
      ,XOFFSET=213 ,YOFFSET=140 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_15 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_15'  $
      ,XOFFSET=345 ,YOFFSET=166 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_16 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_16'  $
      ,XOFFSET=345 ,YOFFSET=118 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_17 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_17'  $
      ,XOFFSET=460 ,YOFFSET=118 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_36 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_36'  $
      ,XOFFSET=366 ,YOFFSET=160 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�������0.bmp' ,/BITMAP)            ;�������


  WID_BUTTON_37 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_37'  $
      ,XOFFSET=481 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER,VALUE='Image\��������0.bmp' ,/BITMAP)       ;��������
;==================================================================================

     text=' �� ֲ'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_jiegou = Widget_label(WID_BASE_3, UNAME='WID_label_jiegou'  $
      ,XOFFSET=302 ,YOFFSET=158 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------

     text=' �� ֲ'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_chengshu = Widget_label(WID_BASE_3, UNAME='WID_label_chengshu'  $
      ,XOFFSET=302 ,YOFFSET=111 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;==================================================================================
  WID_BUTTON_38 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_38'  $
      ,XOFFSET=366 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��0.bmp' ,/BITMAP)     ;�ռ�ͳ��

  WID_BUTTON_39 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_39'  $
      ,XOFFSET=602 ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�������0.bmp' ,/BITMAP)           ;�������


  WID_LABEL_18 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_18'  $
      ,XOFFSET=504 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_floatyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_floatyield'  $
      ,XOFFSET=411 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��������0.bmp' ,/BITMAP)      ;��������


  WID_BUTTON_DC_yieldmerge = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_yieldmerge'  $
      ,XOFFSET=524 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�����ں�0.bmp' ,/BITMAP)     ;�����ں�


  WID_LABEL_19 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_19'  $
      ,XOFFSET=390 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_20 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_20'  $
      ,XOFFSET=276 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_trendyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_trendyield'  $
      ,XOFFSET=296 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\���Ʋ���0.bmp' ,/BITMAP)       ;���Ʋ���


  WID_BUTTON_43 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_43'  $
      ,XOFFSET=638 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��������0.bmp' ,/BITMAP)        ;��������


  WID_LABEL_21 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_21'  $
      ,XOFFSET=618 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')

  WID_LABEL_23 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_23'  $
      ,XOFFSET=659 ,YOFFSET=190 ,/ALIGN_LEFT ,VALUE='\/')


  WID_LABEL_24 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_24'  $
      ,XOFFSET=661 ,YOFFSET=177 ,/ALIGN_LEFT ,VALUE='|')


  WID_BUTTON_NQ_InterPolate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_InterPolate'  $
      ,XOFFSET=296 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�����ֵ0.bmp' ,/BITMAP )             ;�����ֵ


  WID_LABEL_26 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_26'  $
      ,XOFFSET=276 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_27 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_27'  $
      ,XOFFSET=390 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_NQ_classify = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_classify'  $
      ,XOFFSET=411 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�Աȷ���0.bmp' ,/BITMAP)    ;�Աȷ���


  WID_BUTTON_FZ_calculate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_calculate'  $
      ,XOFFSET=296 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\ָ����ȡ0.bmp' ,/BITMAP)     ;ָ����ȡ


  WID_LABEL_28 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_28'  $
      ,XOFFSET=276 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_29 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_29'  $
      ,XOFFSET=390 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')

  WID_BUTTON_FZ_statistic = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_statistic'  $
      ,XOFFSET=411 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��0.bmp' ,/BITMAP)      ;�ռ�ͳ��

  WID_LABEL_25 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_25'  $
      ,XOFFSET=661 ,YOFFSET=163 ,/ALIGN_LEFT ,VALUE='|')

    WID_LABEL_31 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_31'  $
      ,XOFFSET=283 ,YOFFSET=31 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_32 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_32'  $
      ,XOFFSET=287 ,YOFFSET=26 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_35 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_35'  $
      ,XOFFSET=283 ,YOFFSET=60 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_37 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_37'  $
      ,XOFFSET=287 ,YOFFSET=67 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_95 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_95'  $
      ,XOFFSET=283 ,YOFFSET=45 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')

  WID_LABEL_40 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_40'  $
      ,XOFFSET=275 ,YOFFSET=46 ,/ALIGN_RIGHT ,VALUE='��')

  WID_LABEL_95 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_95'  $
      ,XOFFSET=284 ,YOFFSET=140 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_31 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_31'  $
      ,XOFFSET=284 ,YOFFSET=126 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_32 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_32'  $
      ,XOFFSET=288 ,YOFFSET=121 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_35 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_35'  $
      ,XOFFSET=284 ,YOFFSET=155 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_37 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_37'  $
      ,XOFFSET=288 ,YOFFSET=162 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_40 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_40'  $
      ,XOFFSET=276 ,YOFFSET=141 ,/ALIGN_RIGHT ,VALUE='��')


;=========���ܺ�ļ�ͷ======================================================================


  WID_LABEL_101 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_101'  $
      ,XOFFSET=696 ,YOFFSET=29 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_102 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_102'  $
      ,XOFFSET=696 ,YOFFSET=42 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_104 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_104'  $
      ,XOFFSET=696 ,YOFFSET=55 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_106 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_106'  $
      ,XOFFSET=696 ,YOFFSET=68 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_108 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_108'  $
      ,XOFFSET=699 ,YOFFSET=49 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_98 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_98'  $
      ,XOFFSET=688 ,YOFFSET=24 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_99 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_99'  $
      ,XOFFSET=688 ,YOFFSET=74 ,/ALIGN_RIGHT ,VALUE='��')

 ;==============�������ƺ�ļ�ͷ=====================================================


  WID_LABEL_109 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_109'  $
      ,XOFFSET=587 ,YOFFSET=142 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_110 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_110'  $
      ,XOFFSET=584 ,YOFFSET=163 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_111 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_111'  $
      ,XOFFSET=584 ,YOFFSET=150 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_112 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_112'  $
      ,XOFFSET=584 ,YOFFSET=137 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_113 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_113'  $
      ,XOFFSET=584 ,YOFFSET=124 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_114 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_114'  $
      ,XOFFSET=576 ,YOFFSET=169 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_115 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_115'  $
      ,XOFFSET=576 ,YOFFSET=119 ,/ALIGN_RIGHT ,VALUE='��')

  text=''+string(byte(13))+string(byte(10))+   $
          '     ����ʾδ��� '+string(byte(13))+string(byte(10))+   $
          '     ����ʾ����� ';+string(byte(13))+string(byte(10))+   $
       ;   '     ����ʾ������� '

  WID_BUTTON_TL1 = Widget_Button(WID_BASE_3 $
      ,XOFFSET=625 ,YOFFSET=322 ,SCR_XSIZE=35 ,SCR_YSIZE=10  $
      ,/ALIGN_CENTER ,VALUE='Image\��.bmp' ,/BITMAP)

  WID_BUTTON_TL2 = Widget_Button(WID_BASE_3 $
      ,XOFFSET=625 ,YOFFSET=335 ,SCR_XSIZE=35 ,SCR_YSIZE=10  $
      ,/ALIGN_CENTER ,VALUE='Image\��.bmp' ,/BITMAP)

;  WID_BUTTON_TL3 = Widget_Button(WID_BASE_3 $
;      ,XOFFSET=625 ,YOFFSET=348 ,SCR_XSIZE=35 ,SCR_YSIZE=10  $
;      ,/ALIGN_CENTER ,VALUE='Image\��.bmp' ,/BITMAP)


  WID_label_tuli = Widget_label(WID_BASE_3, UNAME='WID_label_tuli'  $
      ,XOFFSET=620 ,YOFFSET=310 ,SCR_XSIZE=120 ,SCR_YSIZE=42,frame=1   $
      ,value=text)

  Widget_Control, diaodu_Modal_BASE, set_uvalue={$
					PRO_SAT:WID_BUTTON_29,$;���̼��-ͳ��
					PRO_HANTS:WID_BUTTON_30,$;���̼��-�����ع�
					PRO_SUM:WID_BUTTON_32,$;���̼��-����
					RT_CLASSIFY:WID_BUTTON_31,$;ʵʱ���-��ֵ�ּ�
					RT_SAT:WID_BUTTON_34,$;ʵʱ���-�ռ�ͳ��
					RT_SUM:WID_BUTTON_33,$;ʵʱ���-����
					ZS_RESULT:WID_BUTTON_0,$;���Ƽ��-�������
					MJ_CS_TJ:WID_BUTTON_38,$;��ֲ����-�ռ�ͳ��
					MJ_CS_WT:WID_BUTTON_37,$;��ֲ����-��������
					MJ_JG_RK:WID_BUTTON_36,$;��ֲ�ṹ-�������
					MJ_JG_WT:WID_BUTTON_35,$;��ֲ�ṹ-��������
					MJ_AREA:WID_BUTTON_39,$;�������-�������
					DC_TREND:WID_BUTTON_DC_trendyield,$;����Ԥ��-���Ʋ���
					DC_WAVE:WID_BUTTON_DC_floatyield,$;����Ԥ��-��������
					DC_FUSE:WID_BUTTON_DC_yieldmerge,$;����Ԥ��-�����ں�
					CL_YIELD:WID_BUTTON_43,$;��������
					NQ_CZ:WID_BUTTON_NQ_InterPolate,$;ũ������-�����ֵ
					NQ_DB:WID_BUTTON_NQ_classify,$;ũ������-�Աȷ���
					FZ_TQ:WID_BUTTON_FZ_calculate,$;����ָ��-ָ����ȡ
					FZ_TJ:WID_BUTTON_FZ_statistic $;����ָ��-�ռ�ͳ��
				}
  Widget_Control, diaodu_Modal_BASE, /REALIZE
  Widget_Control, diaodu_Modal_BASE, SENSITIVE=0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  widget_control, TLB_BASE, set_uvalue = {diaodu_Modal_BASE:diaodu_Modal_BASE}
  Widget_Control, /REALIZE, TLB_BASE

  ;*******************************************
  ;2006.12.06,MJH
  Result = WIDGET_INFO(TLB_BASE, FIND_BY_UNAME='CropAnalysis')
  Result_1 = WIDGET_INFO(TLB_BASE, FIND_BY_UNAME='TLB_BASE')
  ;Result_2 = WIDGET_INFO(TLB_BASE, FIND_BY_UNAME='Manger')
  Result_2 = WIDGET_INFO(TLB_BASE, FIND_BY_UNAME='DataManger_TOP')


  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
  X_OFFSET=140
  Y_OFFSET=0
  MENU_OPERATION=Result
  BASE_TOP=Result_1
  MENU_MANAGE=Result_2
  HELP,MENU_OPERATION
  HELP,MENU_MANAGE

  ;*******************************************
  Widget_Control,SystemSettingBU,/INPUT_FOCUS
;========"CLEANUP"����������ӣ����ڹر�ϵͳʱ����ڴ棬20070717==============
;  XManager, 'TLB_BASE', TLB_BASE, /NO_BLOCK, CLEANUP = 'clearwarp'

  SD_DB_CONNECT, GROUP_LEADER=wGroup

  updateall

  XManager, 'TLB_BASE', TLB_BASE, /NO_BLOCK, CLEANUP = 'clearwarp'

end
;
; Empty stub procedure used for autoloading.
;
pro PROCropMonitoringSystem, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_


  TLB_BASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

end