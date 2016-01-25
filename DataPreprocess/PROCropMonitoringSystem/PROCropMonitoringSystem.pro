;========用于关闭系统时清空内存=====================
pro clearwarp, id
;   widget_control,id,get_uvalue = pstate
;   PTR_FREE,pstate
;	TIPS = DIALOG_MESSAGE('确定要退出省级农情遥感监测系统?',TITLE='提示',/QUESTION)
;
;	if TIPS EQ 'No' then return

   COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;也可以不初始化
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

  ;获取公共参数
  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;也可以不初始化
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE

  wGroup = BASE_TOP

  ;========杨绍锷添加，20070808=====================
	IF yesORno EQ 0 THEN BEGIN
		widget_control,Widget_Info(wWidget, FIND_BY_UNAME='DataManger_TOP') ,sensitive=0
		widget_control,Widget_Info(wWidget, FIND_BY_UNAME='CropAnalysis') ,sensitive=0
	endif
;=========================================================================

  case wTarget of
		;界面的移动
		Widget_Info(wWidget, FIND_BY_UNAME='TLB_BASE'): begin
	      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TLB_MOVE' ) THEN BEGIN
	        WIDGET_CONTROL,Event.top,get_UVALUE = pstate
	        X_OFFSET=EVENT.X
	        Y_OFFSET=EVENT.Y
;	        PRINT,X_OFFSET,Y_OFFSET

		  END
	    end

		;系统设定
		Widget_Info(wWidget, FIND_BY_UNAME='SystemSettingBU'): begin
   		   	SD_INTERFACE
   		   	PRINT,'调用相关程序'           ;调用程序界面
		END

		;数据管理
		Widget_Info(wWidget, FIND_BY_UNAME='DataManagerBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
		      ;SJ_Interface,GROUP_LEADER=wGroup
		      SJ_Interface,GROUP_LEADER=Event.top
		    ENDELSE
		END

		;长势监测
		Widget_Info(wWidget, FIND_BY_UNAME='GrowthBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		     ZS_INTERFACE,GROUP_LEADER=Event.top
		      PRINT,'调用相关程序'            ;调用程序界面
		    ENDELSE
		END

		;单产预测
		Widget_Info(wWidget, FIND_BY_UNAME='YieldBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
		      ;DC,GROUP_LEADER=groupleader
		      DC,GROUP_LEADER=Event.top   ;修改-------------------------------------------------------;

		    ENDELSE
		END

		;面积估算
		Widget_Info(wWidget, FIND_BY_UNAME='AreaBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
;		      MJ_Interface
		      MJ_Interface,GROUP_LEADER=Event.top
		    ENDELSE
		END

		;产量估算
		Widget_Info(wWidget, FIND_BY_UNAME='ProductionBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
		      ;CL,GROUP_LEADER=groupleader
		      CL,GROUP_LEADER=Event.top   ;修改-------------------------------------------------------
		    ENDELSE
		END

		;农气分析
		Widget_Info(wWidget, FIND_BY_UNAME='AgroMeteoBU'): begin
   		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
		      ;NQ_Interface,GROUP_LEADER=wGroup
		      NQ_Interface,GROUP_LEADER=Event.top
		    ENDELSE
		END

		;复种指数
		Widget_Info(wWidget, FIND_BY_UNAME='MutipleCroppingBU'): begin
		     IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
		      FZ,GROUP_LEADER=Event.top   ;修改-------------------------------------------------------
		    ENDELSE
		END

		;业务管理
		Widget_Info(wWidget, FIND_BY_UNAME='ProcessManagerBU'): begin
		    IF yesORno EQ 0 THEN BEGIN
		      no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      PRINT,'调用相关程序'            ;调用程序界面
		      GL_PRO;,group_leader=GLleader
		    ENDELSE
		END

		;*****************************************************************************
		;从这里开始是菜单的操作
		;----联结数据库
		Widget_Info(wWidget, FIND_BY_UNAME='Conn_Database'):begin;Connect_DataBase
			 IF(yesORno EQ 1) THEN BEGIN
				TEMP=DIALOG_MESSAGE('数据库已经连接,要重新进行连接么?',/QUESTION,title='是否重新进行链接')
				print,temp
				IF(TEMP EQ 'No') THEN RETURN
			 ENDIF
	         SD_DB_CONNECT, GROUP_LEADER=wGroup
	    end

	    ;----断开数据库
;		Widget_Info(wWidget, FIND_BY_UNAME='Disconn_database'):begin;'断开链接')
;			 WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;			 IF(yesORno EQ 1) THEN BEGIN
;				DBobj=OBJ_NEW('IDLdbDatabase')
;				yesORno=0
;				;使操作菜单可用
;	            WIDGET_CONTROL,MENU_OPERATION,SENSITIVE=0
;	            WIDGET_CONTROL,MENU_MANAGE,SENSITIVE=0
;				TEMP=DIALOG_MESSAGE('已断开数据库',TITLE='警告',/INFORMATION)
;			 ENDIF ELSE BEGIN
;				TEMP=DIALOG_MESSAGE('数据库并没有连接',TITLE='警告',/INFORMATION)
;			 ENDELSE;INTERFACE_MONITOR_RT,PSTATE, GROUP_LEADER=(*pstate).BASE_TOP_GROWTH_MONITOR
;	    end

		;----路径设定
		Widget_Info(wWidget, FIND_BY_UNAME='PATH_SETUP'):SD_Preference, GROUP_LEADER=wGroup                    ;路径设定

		;----自定义监测区
		Widget_Info(wWidget, FIND_BY_UNAME='ROI_DEFINE'):BEGIN
			IF yesORno EQ 0 THEN BEGIN
		      	no_conn_info=DIALOG_MESSAGE('您还没有进行数据库链接！',TITLE='提示',/INFORMATION)
		    ENDIF ELSE BEGIN
		      	PRINT,'调用相关程序'            ;调用程序界面
		     	SD_ROI_Manage, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
		    ENDELSE
		       ;监测区定义
		END
		;----退出
		Widget_Info(wWidget, FIND_BY_UNAME='Exit_System'):begin;退出
			TIPS = DIALOG_MESSAGE('确定要退出省级农情遥感监测系统?',TITLE='提示',/QUESTION)

			if TIPS EQ 'No' then return
			common_log,'退出省级农情遥感监测系统'
			 CLOSE,/ALL
			 HEAP_GC , /VERBOSE  ;杨绍锷添加，用于关闭系统时清空内存，20070717
			 widget_control,event.top,/destroy
			 exit,/NO_CONFIRM
	    end

	    ;----数据检测
		Widget_Info(wWidget, FIND_BY_UNAME='DATA_CHECK'):SJ_abnormalcheck, GROUP_LEADER=event.top, _EXTRA=_VWBExtra_


		;----数据入库
		Widget_Info(wWidget, FIND_BY_UNAME='DATA_LOAD'):SJ_todatabase, GROUP_LEADER=event.top


		;----数据维护
		Widget_Info(wWidget, FIND_BY_UNAME='Data_MODIFY'):SJ_datamaintance, GROUP_LEADER=event.top, _EXTRA=_VWBExtra


		;----日志管理
		Widget_Info(wWidget, FIND_BY_UNAME='LOG_MANAGER'):	log_mgr,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----任务重置
		Widget_Info(wWidget, FIND_BY_UNAME='MISSION_STATUS'):	begin
			CAUTION = Dialog_Message('是否确定要重置所有任务?',TITLE='提示',/QUESTION)
			if CAUTION EQ 'No' then return

			log, 'all',-1
			logfile = '.\text\log.txt'
			OPENW, lun, logfile, /GET_LUN
			PRINTF, lun, 'logfile,已完成1,未完成-1,部分完成0'
			CLOSE, lun
			FREE_LUN, lun
		end

		;----图像显示
		Widget_Info(wWidget, FIND_BY_UNAME='Viewer'):COMMON_VIEWER, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----几何纠正
		Widget_Info(wWidget, FIND_BY_UNAME='Geo_Corr'):GEO_CORRECTION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----地形纠正
		Widget_Info(wWidget, FIND_BY_UNAME='Terr_Corr'):TOPOGRAPHIC_CORRECTION, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----影像叠加
		Widget_Info(wWidget, FIND_BY_UNAME='Type_Trans'):FORMAT_CONVERTOR, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----影像叠加
		Widget_Info(wWidget, FIND_BY_UNAME='Layer_Stack'):LAYER_STACKING, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----植被指数计算
		Widget_Info(wWidget, FIND_BY_UNAME='VI'):AGM_VI_Calculation, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----叶面积指数计算
		Widget_Info(wWidget, FIND_BY_UNAME='LAI'):LAI_Calculation, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----计算FPAR
		Widget_Info(wWidget, FIND_BY_UNAME='FPAR'):FPAR_CALCULATION, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----红边参数模拟
		Widget_Info(wWidget, FIND_BY_UNAME='RED_EDGE'):RED_EDGE_SIMULATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----高光谱数据处理
		Widget_Info(wWidget, FIND_BY_UNAME='HSI'):preHSI_base,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----AVHRR数据预处理
		Widget_Info(wWidget, FIND_BY_UNAME='AVHRR_Geo'):AVHRR_PREPROCESSING,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----AVHRR数据预处理
		Widget_Info(wWidget, FIND_BY_UNAME='MODIS_Geo'):DROUGHT_PRE_GUI,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----环境星CCD数据辐射定标
		Widget_Info(wWidget, FIND_BY_UNAME='HJ_Cal'):HJ_CALIBRATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----太阳位置参数计算
		Widget_Info(wWidget, FIND_BY_UNAME='Solar_Position'):SOLPOS_CALCULATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----非监督分类
		Widget_Info(wWidget, FIND_BY_UNAME='UNCLS'):UNSUPERVISED_CLASSIFICATION,GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----分类后重编码
		Widget_Info(wWidget, FIND_BY_UNAME='CLS_RECODE'):RECODE, GROUP_LEADER=event.top, _EXTRA=_VWBExtra

		;----任务查询
		Widget_Info(wWidget, FIND_BY_UNAME='MISSION_QUERY'):GL_xianshi, GROUP_LEADER=event.top

		;----任务调度
		Widget_Info(wWidget, FIND_BY_UNAME='MISSION_LOAD'):GL_diaodu,GROUP_LEADER=event.top, _EXTRA=_VWBExtra_

		;----气象插值
		Widget_Info(wWidget, FIND_BY_UNAME='AGRO_INTERPOLATE'):NQ_InterPolate_intermain, GROUP_LEADER=event.top

		;----对比分析
		Widget_Info(wWidget, FIND_BY_UNAME='AGRO_COMPARE'):NQ_classify_classifyagri, GROUP_LEADER=event.top

		;----差值分级
		Widget_Info(wWidget, FIND_BY_UNAME='CLASSIFY_RT'):ZS_classify, GROUP_LEADER=event.top

		;----统计到县
		Widget_Info(wWidget, FIND_BY_UNAME='STA_RT'):ZS_RT_STA, GROUP_LEADER=event.top

		;----汇总
		Widget_Info(wWidget, FIND_BY_UNAME='SUM_RT'):ZS_STA_RT_ABOVE_COUNTY, GROUP_LEADER=event.top

		;----统计到县
		Widget_Info(wWidget, FIND_BY_UNAME='STA_PRO'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=event.top

		;----过程线重构
		Widget_Info(wWidget, FIND_BY_UNAME='HANTS_PRO'):ZS_HANTS, GROUP_LEADER=event.top

		;----汇总
		Widget_Info(wWidget, FIND_BY_UNAME='SUM_PRO'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=event.top

		;----结果分析
		Widget_Info(wWidget, FIND_BY_UNAME='ANA_GROWTH'):ZS_JPG, GROUP_LEADER=event.top

		;----趋势产量分析
		Widget_Info(wWidget, FIND_BY_UNAME='Y_TREND_AN'):DC_TrendYield, GROUP_LEADER=event.top
		;----产量参数计算
		Widget_Info(wWidget, FIND_BY_UNAME='Y_FLU_AN_COMPUTE'):DC_FloatYield, GROUP_LEADER=event.top
		;----空间插值外推
		Widget_Info(wWidget, FIND_BY_UNAME='Y_FLU_AN_EXP'):DC_SpatialExtrapolate, GROUP_LEADER=event.top
		;----简单比值法计算
		Widget_Info(wWidget, FIND_BY_UNAME='Y_BIOMASS_ratio'):DC_Bio_Ratio, GROUP_LEADER=event.top
		;----收获指数法计算
		Widget_Info(wWidget, FIND_BY_UNAME='Y_BIOMASS_HI'):DC_Bio_HI, GROUP_LEADER=event.top
		;----产量融合分析
		Widget_Info(wWidget, FIND_BY_UNAME='Y_FUSE'):DC_YieldMerge, GROUP_LEADER=event.top
		;----结果显示输出
		Widget_Info(wWidget, FIND_BY_UNAME='Y_SHOW_OUTPUT'):DC_SynthesisOutput, GROUP_LEADER=event.top


;		;----非监督分类
;		Widget_Info(wWidget, FIND_BY_UNAME='AREA_CLASSIFY'):ABOUT, GROUP_LEADER=event.top
;		;----分类标识
;		Widget_Info(wWidget, FIND_BY_UNAME='AREA_LABEL'):ABOUT, GROUP_LEADER=event.top
		;----统计
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_STA'):MJ_statistical, GROUP_LEADER=event.top

		Widget_Info(wWidget, FIND_BY_UNAME='GVG_IMP'):GVG_TO_DATABASE, GROUP_LEADER=event.top, _EXTRA=_VWBExtra         ;GVG数据入库
;		;----外推
;		Widget_Info(wWidget, FIND_BY_UNAME='AREA_EXP'):ABOUT, GROUP_LEADER=event.top

		;----GVG外推
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_EXP_GVG'):MJ_Extrapolate_GVG, GROUP_LEADER=event.top
		;----种植成数外推
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_EXP_PROPORTION'):MJ_Extrapolate_Proportion, GROUP_LEADER=event.top
		;----面积计算
		Widget_Info(wWidget, FIND_BY_UNAME='AREA_COMPUTE'):MJ_tocalculate, GROUP_LEADER=event.top

		;----产量估算
		Widget_Info(wWidget, FIND_BY_UNAME='Production'):CL, GROUP_LEADER=event.top
		;----复种指数计算
		Widget_Info(wWidget, FIND_BY_UNAME='CI_COMPUTE'):FZ_calculate, GROUP_LEADER=event.top
		;----复种指数统计
		Widget_Info(wWidget, FIND_BY_UNAME='CI_STA'):FZ_statistic, GROUP_LEADER=event.top
		;----帮助文档
		Widget_Info(wWidget, FIND_BY_UNAME='HelpDocument'):begin
			common_log,'启动帮助'
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP,  BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end

		;----联系我们
		Widget_Info(wWidget, FIND_BY_UNAME='CONNECT_US'):CONTACT, GROUP_LEADER=event.top

		;----关于系统
		Widget_Info(wWidget, FIND_BY_UNAME='AboutSystem'):ABOUT, GROUP_LEADER=event.top

		;*****************************************************************************

	   widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_close'): begin
	     TIPS = DIALOG_MESSAGE('确定要退出省级农情遥感监测系统?',TITLE='提示',/QUESTION)

		  if TIPS EQ 'No' then return
	   	common_log,'关闭省级农情遥感监测系统'
		  widget_control,event.top,/destroy
		  exit, /NO_CONFIRM
	   end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_help'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '任务调度', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end

;		ONLINE_HELP, '任务调度', BOOK='HELP\HELP.chm', /FULL_PATH
;        temp=dialog_message('系统暂没有帮助')
;     end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_GL_mis_search'):GL_xianshi,GROUP_LEADER=wGroup            ;任务显示
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datainput'):SJ_todatabase, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra          ;数据导入
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datamaintance'):SJ_datamaintance, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra   ;数据维护
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_InterPolate'):NQ_InterPolate_intermain, GROUP_LEADER=wGroup;气象插值
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_classify'):NQ_classify_classifyagri, GROUP_LEADER=wGroup   ;对比分析
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_calculate'):FZ_calculate, GROUP_LEADER=wGroup       ;指数提取
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_statistic'):FZ_statistic, GROUP_LEADER=wGroup       ;空间统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_trendyield'):DC_TrendYield, GROUP_LEADER=wGroup     ;趋势产量
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_floatyield'):DC_FloatYield, GROUP_LEADER=wGroup     ;波动产量
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_yieldmerge'):DC_YieldMerge,GROUP_LEADER=wGroup ;产量融合
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_21'):SD_ROI_Manage, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_      ;监测区定义
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_22'):SD_Preference, GROUP_LEADER=wGroup                    ;路径设定
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_23'):SD_DB_Connect, GROUP_LEADER=wGroup                    ;数据库链接
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_24'):SJ_abnormalcheck, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_           ;异常检测
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_29'):ZS_PRO_STA, GROUP_LEADER=wGroup                       ;统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_30'):ZS_HANTS, GROUP_LEADER=wGroup                         ;曲线重构
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_31'):ZS_classify, GROUP_LEADER=wGroup                      ;差值分类
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_32'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=wGroup          ;汇总（过程）
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_33'):ZS_STA_RT_ABOVE_COUNTY, GROUP_LEADER=wGroup           ;汇总（实时）
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_34'):ZS_RT_STA, GROUP_LEADER=wGroup                        ;空间统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_0'):ZS_JPG, GROUP_LEADER=WGROUP
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_35'):MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;比例外推
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_36'):GVG_TO_DATABASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra         ;GVG数据入库
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_37'):MJ_Extrapolate_Proportion, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;成数外推
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_38'):MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_       ;面积统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_39'):MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_    ;面积估算
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_43'):CL, GROUP_LEADER=wGroup                               ;产量估算

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    else:
  endcase
;==============杨绍锷添加，用于关闭系统时清空内存，20070809=================================
	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' then begin

;	     CROP_MONITOR_exit=DIALOG_MESSAGE('确定退出监测系统吗?',TITLE='询问',/QUESTION)
;	     if CROP_MONITOR_exit eq 'Yes' then begin
			TIPS = DIALOG_MESSAGE('确定要退出省级农情遥感监测系统?',TITLE='提示',/QUESTION)

			if TIPS EQ 'No' then return

			COMMON COMMON_SETPATH, ppath
			ptr_free, ppath
	     	HEAP_GC , /VERBOSE  ;杨绍锷添加，用于关闭系统时清空内存，20070809
	      widget_control,event.top,/DESTROY
	      exit,/NO_CONFIRM
;	     endif

  	ENDIF
  ;====================================================================

end
;/////////////////////////////////////////////////////////////////////////////////
pro TLB_BASE, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'启动省级农情遥感监测系统'

	C_COMMON_BLOCK   ;运行公共变量

	PRO_COMMON_SETPATH

	common_current_date

  TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
      ,XOFFSET=140 ,YOFFSET=0 ,/TLB_MOVE_EVENTS  $  $
      ,TITLE='省级农情遥感监测系统' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 ,MBAR=TLB_BASE_MBAR,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1)	;杨绍锷添加/TLB_KILL_REQUEST_EVENTS，20070809

;*********************************************************************
  Configuration = Widget_Button(TLB_BASE_MBAR, UNAME='Configuration'  $
      ,XOFFSET=3 ,YOFFSET=0 ,/MENU ,VALUE='系统(&S)')


;  DataBaseConnecton = Widget_Button(Configuration,  $
;      UNAME='DataBaseConnecton' ,/MENU ,VALUE='数据库链接')


  Conn_Database = Widget_Button(Configuration,  $
      UNAME='Conn_Database' ,VALUE='数据库链接')


;  Disconn_database = Widget_Button(DataBaseConnecton,  $
;      UNAME='Disconn_database' ,VALUE='断开链接')

;---------------------------------------------------------
  PATH_SETUP = Widget_Button(Configuration, UNAME='PATH_SETUP'  $
      ,/SEPARATOR ,VALUE='路径设定')


  ROI_DEFINE = Widget_Button(Configuration, UNAME='ROI_DEFINE'  $
      ,VALUE='定义监测区')


;  DefaultSet = Widget_Button(Configuration, UNAME='DefaultSet'  $
;      ,VALUE='系统设置')


  Exit_System = Widget_Button(Configuration, UNAME='Exit_System'  $
      ,/SEPARATOR ,VALUE='退出(&E)')

;*****************************************************************************
  DataManger_TOP = Widget_Button(TLB_BASE_MBAR, UNAME='DataManger_TOP' ,/MENU  $
      ,VALUE='管理(&M)',SENSITIVE=0)

  DataManger = Widget_Button(DataManger_TOP, UNAME='DataManger' ,/MENU  $
      ,VALUE='数据管理')


  DATA_CHECK = Widget_Button(DataManger, UNAME='DATA_CHECK'  $
      ,VALUE='数据检测')


  DATA_LOAD = Widget_Button(DataManger, UNAME='DATA_LOAD'  $
      ,VALUE='数据入库')


  Data_MODIFY = Widget_Button(DataManger, UNAME='Data_MODIFY'  $
      ,VALUE='数据维护')


  BUSINESS_Manger = Widget_Button(DataManger_TOP, UNAME='BUSINESS_Manger' ,/MENU  $
      ,VALUE='业务管理')


  MISSION_QUERY = Widget_Button(BUSINESS_Manger, UNAME='MISSION_QUERY'  $
      ,VALUE='任务查询')


  MISSION_LOAD = Widget_Button(BUSINESS_Manger, UNAME='MISSION_LOAD'  $
      ,VALUE='任务调度')

  LOG_MANAGER = Widget_Button(DataManger_TOP, UNAME='LOG_MANAGER'   $
      ,VALUE='日志管理')

  MISSION_STATUS = Widget_Button(DataManger_TOP, UNAME='MISSION_STATUS'   $
      ,VALUE='任务重置')
;*********************************************************************************************
  Data_Prep = Widget_Button(TLB_BASE_MBAR, UNAME='Data_Prep' ,/MENU  $
      ,VALUE='预处理(&P)',SENSITIVE=1)

;   Atmos = Widget_Button(Data_Prep, UNAME='Atmos',VALUE='大气纠正')

;   Geo_Corr = Widget_Button(Data_Prep, UNAME='Geo_Corr',/MENU,VALUE='几何纠正')


	Solar_Position = Widget_Button(Data_Prep, UNAME='Solar_Position',VALUE='太阳位置参数计算')
	Calibration = Widget_Button(Data_Prep, UNAME='Data_Spec',/MENU,VALUE='辐射定标')
		HJ_Cal = Widget_Button(Calibration, UNAME='HJ_Cal',VALUE='环境星 CCD')
;		ETM_Cal = Widget_Button(Calibration, UNAME='ETM_Cal',VALUE='ETM')

	Geo_Corr = Widget_Button(Data_Prep, UNAME='Geo_Corr',/SEPARATOR,VALUE='几何纠正')

	Terr_Corr = Widget_Button(Data_Prep, UNAME='Terr_Corr',/SEPARATOR,VALUE='地形纠正')
;		C_Corr = Widget_Button(Terr_Corr, UNAME='C_Corr',VALUE='C 纠正')
;		M_Corr = Widget_Button(Terr_Corr, UNAME='M_Corr',VALUE='Minnaert 纠正')

	Type_Trans = Widget_Button(Data_Prep, UNAME='Type_Trans',/SEPARATOR,VALUE='格式转换')
	Layer_Stack = Widget_Button(Data_Prep, UNAME='Layer_Stack',VALUE='图层叠加')

	Viewer = Widget_Button(Data_Prep, UNAME='Viewer',/SEPARATOR,VALUE='图像显示')

   Index_Cal = Widget_Button(Data_Prep, UNAME='Index_Cal',/SEPARATOR,/MENU,VALUE='指数计算')
   	VI = Widget_Button(Index_Cal, UNAME='VI',VALUE='植被指数')
   	LAI = Widget_Button(Index_Cal, UNAME='LAI',VALUE='叶面积指数')
;   	NPP = Widget_Button(Index_Cal, UNAME='NPP',VALUE='NPP')
   	FPAR = Widget_Button(Index_Cal, UNAME='FPAR',VALUE='FPAR')
   	RED_EDGE = Widget_Button(Index_Cal, UNAME='RED_EDGE',VALUE='植被红边参数模拟')

	Data_Spec = Widget_Button(Data_Prep, UNAME='Data_Spec',/SEPARATOR,/MENU,VALUE='特定数据处理')
		HSI = Widget_Button(Data_Spec, UNAME='HSI',VALUE='高光谱数据处理')
		AVHRR_Geo = Widget_Button(Data_Spec, UNAME='AVHRR_Geo',VALUE='AVHRR数据处理')
		MODIS_Geo = Widget_Button(Data_Spec, UNAME='MODIS_Geo',VALUE='MODIS数据处理')

	UNCLS = Widget_Button(Data_Prep, UNAME='UNCLS',/SEPARATOR,VALUE='非监督分类')
	CLS_RECODE = Widget_Button(Data_Prep, UNAME='CLS_RECODE',VALUE='分类重编码')
;*********************************************************************************************
  CropAnalysis = Widget_Button(TLB_BASE_MBAR, UNAME='CropAnalysis',/MENU ,VALUE='农情分析(&A)',SENSITIVE=0)

  AgroMeteoAnalysis = Widget_Button(CropAnalysis,UNAME='AgroMeteoAnalysis',/MENU ,VALUE='农业气象分析')
  AGRO_INTERPOLATE = Widget_Button(AgroMeteoAnalysis,UNAME='AGRO_INTERPOLATE' ,VALUE='气象插值')
  AGRO_COMPARE = Widget_Button(AgroMeteoAnalysis,UNAME='AGRO_COMPARE',VALUE='对比分析')

  Growth = Widget_Button(CropAnalysis, UNAME='Growth',/MENU ,/SEPARATOR,VALUE='长势监测')
  GROWTH_RT = Widget_Button(Growth, UNAME='GROWTH_RT',/MENU ,VALUE='实时监测')
  CLASSIFY_RT = Widget_Button(GROWTH_RT, UNAME='CLASSIFY_RT',VALUE='差值分级')
  STA_RT = Widget_Button(GROWTH_RT, UNAME='STA_RT',VALUE='统计到县')
  SUM_RT = Widget_Button(GROWTH_RT, UNAME='SUM_RT',VALUE='汇总')

  GROWTH_PRO = Widget_Button(Growth, UNAME='GROWTH_PRO',/MENU ,VALUE='过程监测')
  STA_PRO = Widget_Button(GROWTH_PRO, UNAME='STA_PRO',VALUE='统计到县')
  HANTS_PRO = Widget_Button(GROWTH_PRO, UNAME='HANTS_PRO',VALUE='曲线重构')
  SUM_PRO = Widget_Button(GROWTH_PRO, UNAME='SUM_PRO',VALUE='汇总')

  ANA_GROWTH = Widget_Button(Growth, UNAME='ANA_GROWTH' ,/SEPARATOR,VALUE='监测结果成图')

  Yield = Widget_Button(CropAnalysis, UNAME='Yield',/MENU,VALUE='单产预测')
  Y_TREND_AN=Widget_Button(Yield, UNAME='Y_TREND_AN',VALUE='趋势产量分析')
  Y_FLU_AN=Widget_Button(Yield, UNAME='Y_FLU_AN',/MENU,VALUE='波动产量分析')
  Y_FLU_AN_COMPUTE=Widget_Button(Y_FLU_AN, UNAME='Y_FLU_AN_COMPUTE',VALUE='产量参数计算')
  Y_FLU_AN_EXP=Widget_Button(Y_FLU_AN, UNAME='Y_FLU_AN_EXP',VALUE='空间插值外推')
;  Y_BIOMASS=Widget_Button(Yield, UNAME='Y_BIOMASS',/MENU,VALUE='生产量产量分析')
;  Y_BIOMASS_ratio=Widget_Button(Y_BIOMASS, UNAME='Y_BIOMASS_ratio',VALUE='简单比值法计算')
;  Y_BIOMASS_HI=Widget_Button(Y_BIOMASS, UNAME='Y_BIOMASS_HI',VALUE='收获指数法计算')
  Y_FUSE=Widget_Button(Yield, UNAME='Y_FUSE' ,/SEPARATOR,VALUE='产量融合分析')
  Y_SHOW_OUTPUT=Widget_Button(Yield, UNAME='Y_SHOW_OUTPUT',VALUE='结果显示输出')


  Area = Widget_Button(CropAnalysis,UNAME='Area',/MENU ,VALUE='面积估算')
  ;AREA_CLASSIFY = Widget_Button(Area, UNAME='AREA_CLASSIFY',VALUE='非监督分类')
  ;AREA_LABEL = Widget_Button(Area, UNAME='AREA_LABEL' ,VALUE='分类标识')
  ;AREA_STA = Widget_Button(Area, UNAME='AREA_STA',/SEPARATOR,VALUE='统计')
  AREA_STA = Widget_Button(Area, UNAME='AREA_STA',VALUE='空间统计')
  GVG_IMP = Widget_Button(Area, UNAME='GVG_IMP',VALUE='GVG数据入库')
  AREA_EXP = Widget_Button(Area, UNAME='AREA_EXP',/MENU,VALUE='外推')
  		AREA_EXP_GVG=Widget_Button(AREA_EXP,uname='AREA_EXP_GVG',value='比例外推')
  		AREA_EXP_PROPORTION=Widget_Button(AREA_EXP,uname='AREA_EXP_PROPORTION',value='成数外推')
  AREA_COMPUTE = Widget_Button(Area, UNAME='AREA_COMPUTE',VALUE='面积估算')

  Production = Widget_Button(CropAnalysis,UNAME='Production' ,VALUE='产量估算')

  Mutiple_cropping = Widget_Button(CropAnalysis,UNAME='Mutiple_cropping' ,/MENU,/SEPARATOR ,VALUE='复种指数')
  CI_COMPUTE = Widget_Button(Mutiple_cropping,UNAME='CI_COMPUTE' ,VALUE='复种指数提取')
  CI_STA = Widget_Button(Mutiple_cropping,UNAME='CI_STA' ,VALUE='复种指数统计')

  ;***********************************************************************************************
  SystemHelp = Widget_Button(TLB_BASE_MBAR, UNAME='SystemHelp' ,/MENU  $
      ,VALUE='帮助(&H)')

  HelpDocument = Widget_Button(SystemHelp, UNAME='HelpDocument' ,VALUE='帮助文档')

  CONNECT_US = Widget_Button(SystemHelp, UNAME='CONNECT_US',VALUE='联系我们')

  AboutSystem = Widget_Button(SystemHelp, UNAME='AboutSystem',/SEPARATOR ,VALUE='关于系统')

;****************************************************************************
  Modal_BASE = Widget_Base(TLB_BASE, UNAME='Modal_BASE'  $
      ,XOFFSET=3 ,YOFFSET=6,SPACE=6 ,XPAD=3 ,YPAD=3 ,ROW=1, frame=1)

;------------------------------------------------------------------
  SystemSettingBU = Widget_Button(Modal_BASE, UNAME='SystemSettingBU'  $
      ,/ALIGN_Left,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,VALUE='Image\系统.bmp' ,/BITMAP)

  DataManagerBU = Widget_Button(Modal_BASE,  $
      UNAME='DataManagerBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\数据.bmp' ,/BITMAP)

  GrowthBU = Widget_Button(Modal_BASE,  $
      UNAME='GrowthBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\长势.bmp' ,/BITMAP)


  YieldBU = Widget_Button(Modal_BASE,  $
      UNAME='YieldBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\单产.bmp' ,/BITMAP)

  AreaBU = Widget_Button(Modal_BASE,  $
      UNAME='AreaBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\面积.bmp' ,/BITMAP)

  ProductionBU = Widget_Button(Modal_BASE,  $
      UNAME='ProductionBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\估产.bmp' ,/BITMAP)

  AgroMeteoBU = Widget_Button(Modal_BASE,  $
      UNAME='AgroMeteoBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\农气.bmp' ,/BITMAP)

  MutipleCroppingBU = Widget_Button(Modal_BASE,  $
      UNAME='MutipleCroppingBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\复种.bmp' ,/BITMAP)

  ProcessManagerBU = Widget_Button(Modal_BASE,  $
      UNAME='ProcessManagerBU' ,SCR_XSIZE=75 ,SCR_YSIZE=60  $
      ,/ALIGN_Left ,VALUE='Image\业务.bmp' ,/BITMAP)

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
;      ,/ALIGN_CENTER ,VALUE='关闭')
;
;
;  WID_BUTTON_help = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_help'  $
;      ,XOFFSET=80 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
;      ,/ALIGN_CENTER ,VALUE='帮助')


  WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3' ,FRAME=0  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=748 ,SCR_YSIZE=371  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_0 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_0' ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=31 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

;-------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          '业'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '务'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '管'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '理'

  WID_label_yewu = Widget_label(WID_BASE_0, UNAME='WID_label_yewu' ,XOFFSET=1  $
      ,YOFFSET=0 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104, /ALIGN_CENTER  $
      ,value=text)

;-------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          '系'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '统'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '设'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '定'

  WID_label_xitong = Widget_label(WID_BASE_0, UNAME='WID_label_xitong' ,XOFFSET=1  $
      ,YOFFSET=125 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          '数'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '据'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '管'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '理'

  WID_label_shuju = Widget_label(WID_BASE_0, UNAME='WID_label_shuju' ,XOFFSET=1  $
      ,YOFFSET=250 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)
;------------------------------------------------------------------------------------

 WID_BASE_4 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_4' ,XOFFSET=37  $
      ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  WID_LABEL_42 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_42'  $
      ,YOFFSET=48 ,/ALIGN_RIGHT ,VALUE='－')

  WID_LABEL_45 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_45'  $
      ,XOFFSET=11 ,YOFFSET=48 ,/ALIGN_LEFT ,VALUE='-->')

  WID_LABEL_68 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_68'  $
      ,YOFFSET=175 ,/ALIGN_RIGHT ,VALUE='－')


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
      ,YOFFSET=302 ,/ALIGN_RIGHT ,VALUE='－')


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
      ,VALUE='image/任务查询.bmp',/bitmap)        ;任务查询


  WID_BUTTON_21 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_21'  $   ;监测区定义
      ,YOFFSET=199 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/监测区定义.bmp',/bitmap)


  WID_BUTTON_22 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_22'  $    ;路径设定
      ,YOFFSET=167 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='image/路径设定.bmp',/bitmap)
;      ,VALUE='Image\LUJING01.bmp' ,/BITMAP)


  WID_BUTTON_23 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_23'  $
      ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/数据库链接.bmp',/bitmap)       ;数据库链接


  WID_BUTTON_24 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_24'  $
      ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/异常检测.bmp',/bitmap)                      ;异常检测


  WID_BUTTON_SJ_datamaintance = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datamaintance'  $
      ,YOFFSET=295 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='Image\数据维护.bmp' ,/BITMAP)       ;数据维护


  WID_BUTTON_SJ_datainput = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datainput'  $
      ,YOFFSET=263 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='Image\数据导入.bmp' ,/BITMAP)      ;数据导入

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '农'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '情'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '遥'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '感'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '监'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '测'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '业'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '务'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '模'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '块'



  WID_label_mokuai = Widget_label(WID_BASE_3, UNAME='WID_label_mokuai'  $
      ,XOFFSET=184,frame=1 ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=353   $
      ,/ALIGN_CENTER,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 长 势'+string(byte(13))+string(byte(10))+   $
          ' 监 测'

  WID_label_zhangshi = Widget_label(WID_BASE_3, UNAME='WID_label_zhangshi'  $
      ,XOFFSET=233 ,YOFFSET=31 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 面 积'+string(byte(13))+string(byte(10))+   $
          ' 估 算'


  WID_labe_mianji = Widget_label(WID_BASE_3, UNAME='WID_labe_mianji'  $
      ,XOFFSET=233 ,YOFFSET=126 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 单 产'+string(byte(13))+string(byte(10))+   $
          ' 预 测'


  WID_label_danchan = Widget_label(WID_BASE_3, UNAME='label_danchan'  $
      ,XOFFSET=233 ,YOFFSET=198 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 农 气'+string(byte(13))+string(byte(10))+   $
          ' 分 析'

  WID_label_nongqi = Widget_label(WID_BASE_3, UNAME='WID_label_nongqi'  $
      ,XOFFSET=233 ,YOFFSET=257 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------
    text=''+string(byte(13))+string(byte(10))+   $
          ' 复 种'+string(byte(13))+string(byte(10))+   $
          ' 指 数'

  WID_label_fuzhong = Widget_label(WID_BASE_3, UNAME='WID_label_fuzhong'  $
      ,XOFFSET=233 ,YOFFSET=315 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------


  WID_BUTTON_29 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_29'  $
      ,XOFFSET=366 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计0.bmp' ,/BITMAP)       ;统计

;------------------------------------------------------------------------------------
      text=' 过 程'+string(byte(13))+string(byte(10))+   $
          ' 监 测'

  WID_label_guocheng = Widget_label(WID_BASE_3, UNAME='WID_label_guocheng'  $
      ,XOFFSET=302 ,YOFFSET=17 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)


      text=  $
          ' 实 时'+string(byte(13))+string(byte(10))+   $
          ' 监 测'

  WID_label_shishi = Widget_label(WID_BASE_3, UNAME='WID_label_shishi'  $
      ,XOFFSET=302 ,YOFFSET=65 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------
  WID_BUTTON_30 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_30'  $    ;曲线重构
      ,XOFFSET=481 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\曲线重构0.bmp' ,/BITMAP )


  WID_BUTTON_31 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_31'  $
      ,XOFFSET=366 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\差值分级0.bmp' ,/BITMAP )            ;差值分级


  WID_BUTTON_32 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_32'  $
      ,XOFFSET=594 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\汇总0.bmp' ,/BITMAP)      ;汇总


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
      ,/ALIGN_CENTER ,VALUE='Image\汇总0.bmp' ,/BITMAP)      ;汇总


  WID_BUTTON_34 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_34'  $
      ,XOFFSET=481 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计0.bmp' ,/BITMAP)     ;空间统计


  WID_LABEL_13 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_13'  $
      ,XOFFSET=574 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')

  WID_BUTTON_0 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_0'  $   ;结果分析
      ,XOFFSET=714 ,YOFFSET=10 ,SCR_XSIZE=25 ,SCR_YSIZE=88  $
      ,/ALIGN_CENTER ,VALUE='Image\结果分析0.BMP',/BITMAP)

;-------------------------------------------------------------------------

  WID_BUTTON_35 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_35'  $
      ,XOFFSET=481 ,YOFFSET=160 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\比例外推0.bmp' ,/BITMAP)       ;比例外推


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
      ,/ALIGN_CENTER ,VALUE='Image\数据入库0.bmp' ,/BITMAP)            ;数据入库


  WID_BUTTON_37 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_37'  $
      ,XOFFSET=481 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER,VALUE='Image\成数外推0.bmp' ,/BITMAP)       ;成数外推
;==================================================================================

     text=' 种 植'+string(byte(13))+string(byte(10))+   $
          ' 结 构'

  WID_label_jiegou = Widget_label(WID_BASE_3, UNAME='WID_label_jiegou'  $
      ,XOFFSET=302 ,YOFFSET=158 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------

     text=' 种 植'+string(byte(13))+string(byte(10))+   $
          ' 成 数'

  WID_label_chengshu = Widget_label(WID_BASE_3, UNAME='WID_label_chengshu'  $
      ,XOFFSET=302 ,YOFFSET=111 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;==================================================================================
  WID_BUTTON_38 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_38'  $
      ,XOFFSET=366 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计0.bmp' ,/BITMAP)     ;空间统计

  WID_BUTTON_39 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_39'  $
      ,XOFFSET=602 ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\面积估算0.bmp' ,/BITMAP)           ;面积估算


  WID_LABEL_18 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_18'  $
      ,XOFFSET=504 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_floatyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_floatyield'  $
      ,XOFFSET=411 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\波动产量0.bmp' ,/BITMAP)      ;波动产量


  WID_BUTTON_DC_yieldmerge = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_yieldmerge'  $
      ,XOFFSET=524 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\产量融合0.bmp' ,/BITMAP)     ;产量融合


  WID_LABEL_19 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_19'  $
      ,XOFFSET=390 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_20 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_20'  $
      ,XOFFSET=276 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_trendyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_trendyield'  $
      ,XOFFSET=296 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\趋势产量0.bmp' ,/BITMAP)       ;趋势产量


  WID_BUTTON_43 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_43'  $
      ,XOFFSET=638 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\产量估算0.bmp' ,/BITMAP)        ;产量估算


  WID_LABEL_21 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_21'  $
      ,XOFFSET=618 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')

  WID_LABEL_23 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_23'  $
      ,XOFFSET=659 ,YOFFSET=190 ,/ALIGN_LEFT ,VALUE='\/')


  WID_LABEL_24 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_24'  $
      ,XOFFSET=661 ,YOFFSET=177 ,/ALIGN_LEFT ,VALUE='|')


  WID_BUTTON_NQ_InterPolate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_InterPolate'  $
      ,XOFFSET=296 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\气象插值0.bmp' ,/BITMAP )             ;气象插值


  WID_LABEL_26 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_26'  $
      ,XOFFSET=276 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_27 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_27'  $
      ,XOFFSET=390 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_NQ_classify = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_classify'  $
      ,XOFFSET=411 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\对比分析0.bmp' ,/BITMAP)    ;对比分析


  WID_BUTTON_FZ_calculate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_calculate'  $
      ,XOFFSET=296 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\指数提取0.bmp' ,/BITMAP)     ;指数提取


  WID_LABEL_28 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_28'  $
      ,XOFFSET=276 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_29 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_29'  $
      ,XOFFSET=390 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')

  WID_BUTTON_FZ_statistic = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_statistic'  $
      ,XOFFSET=411 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计0.bmp' ,/BITMAP)      ;空间统计

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
      ,XOFFSET=275 ,YOFFSET=46 ,/ALIGN_RIGHT ,VALUE='－')

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
      ,XOFFSET=276 ,YOFFSET=141 ,/ALIGN_RIGHT ,VALUE='－')


;=========汇总后的箭头======================================================================


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
      ,XOFFSET=688 ,YOFFSET=24 ,/ALIGN_RIGHT ,VALUE='－')


  WID_LABEL_99 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_99'  $
      ,XOFFSET=688 ,YOFFSET=74 ,/ALIGN_RIGHT ,VALUE='－')

 ;==============成数外推后的箭头=====================================================


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
      ,XOFFSET=576 ,YOFFSET=169 ,/ALIGN_RIGHT ,VALUE='－')


  WID_LABEL_115 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_115'  $
      ,XOFFSET=576 ,YOFFSET=119 ,/ALIGN_RIGHT ,VALUE='－')

  text=''+string(byte(13))+string(byte(10))+   $
          '     ：表示未完成 '+string(byte(13))+string(byte(10))+   $
          '     ：表示已完成 ';+string(byte(13))+string(byte(10))+   $
       ;   '     ：表示部分完成 '

  WID_BUTTON_TL1 = Widget_Button(WID_BASE_3 $
      ,XOFFSET=625 ,YOFFSET=322 ,SCR_XSIZE=35 ,SCR_YSIZE=10  $
      ,/ALIGN_CENTER ,VALUE='Image\红.bmp' ,/BITMAP)

  WID_BUTTON_TL2 = Widget_Button(WID_BASE_3 $
      ,XOFFSET=625 ,YOFFSET=335 ,SCR_XSIZE=35 ,SCR_YSIZE=10  $
      ,/ALIGN_CENTER ,VALUE='Image\绿.bmp' ,/BITMAP)

;  WID_BUTTON_TL3 = Widget_Button(WID_BASE_3 $
;      ,XOFFSET=625 ,YOFFSET=348 ,SCR_XSIZE=35 ,SCR_YSIZE=10  $
;      ,/ALIGN_CENTER ,VALUE='Image\黄.bmp' ,/BITMAP)


  WID_label_tuli = Widget_label(WID_BASE_3, UNAME='WID_label_tuli'  $
      ,XOFFSET=620 ,YOFFSET=310 ,SCR_XSIZE=120 ,SCR_YSIZE=42,frame=1   $
      ,value=text)

  Widget_Control, diaodu_Modal_BASE, set_uvalue={$
					PRO_SAT:WID_BUTTON_29,$;过程监测-统计
					PRO_HANTS:WID_BUTTON_30,$;过程监测-曲线重构
					PRO_SUM:WID_BUTTON_32,$;过程监测-汇总
					RT_CLASSIFY:WID_BUTTON_31,$;实时监测-差值分级
					RT_SAT:WID_BUTTON_34,$;实时监测-空间统计
					RT_SUM:WID_BUTTON_33,$;实时监测-汇总
					ZS_RESULT:WID_BUTTON_0,$;长势监测-结果分析
					MJ_CS_TJ:WID_BUTTON_38,$;种植成数-空间统计
					MJ_CS_WT:WID_BUTTON_37,$;种植成数-成数外推
					MJ_JG_RK:WID_BUTTON_36,$;种植结构-数据入库
					MJ_JG_WT:WID_BUTTON_35,$;种植结构-比例外推
					MJ_AREA:WID_BUTTON_39,$;面积估算-面积估算
					DC_TREND:WID_BUTTON_DC_trendyield,$;单产预测-趋势产量
					DC_WAVE:WID_BUTTON_DC_floatyield,$;单产预测-波动产量
					DC_FUSE:WID_BUTTON_DC_yieldmerge,$;单产预测-产量融合
					CL_YIELD:WID_BUTTON_43,$;产量估算
					NQ_CZ:WID_BUTTON_NQ_InterPolate,$;农气分析-气象插值
					NQ_DB:WID_BUTTON_NQ_classify,$;农气分析-对比分析
					FZ_TQ:WID_BUTTON_FZ_calculate,$;复种指数-指数提取
					FZ_TJ:WID_BUTTON_FZ_statistic $;复种指数-空间统计
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
;========"CLEANUP"由杨绍锷添加，用于关闭系统时清空内存，20070717==============
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