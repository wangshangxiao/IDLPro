pro picpath, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		;Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control,event.id,get_uvalue=pfinfo
	field_id = pfinfo.field_id
	title = pfinfo.title
	path = DIALOG_PICKFILE(title=title, /DIRECTORY,DIALOG_PARENT=event.top);envi_pickfile
	if (path eq '') then return else begin
		widget_control,field_id,set_value=path
	end
end

pro picfile, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		;Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control,event.id,get_uvalue=pfinfo
	field_id = pfinfo.field_id
	title = pfinfo.title
	path = DIALOG_PICKFILE(title=title,DIALOG_PARENT=event.top);envi_pickfile
	if (path eq '') then return else begin
		widget_control,field_id,set_value=path
	end
end

pro BASE_CHANGE_PREFERENCE_event, Event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  widget_info(Event.id, /tree_root) : event.id)

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	wWidget =  Event.top
	;先获取数据
	WIDGET_CONTROL,Event.top,get_UVALUE = pstate

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='CMD_PREFERENCE_DEFAULT'): begin

			TIPS = DIALOG_MESSAGE('是否要恢复到缺省设置?',TITLE='提示',/QUESTION)

			if TIPS EQ 'No' then return

			pathfile = '.\text\common_path.txt';
			file_delete, pathfile
			PRO_COMMON_SETPATH
			COMMON COMMON_SETPATH,ppath

			WIDGET_CONTROL,(*pstate).zs_in_path, set_value=(*ppath).zs_in_path
			WIDGET_CONTROL,(*pstate).zs_out_path,set_value=(*ppath).zs_out_path
			WIDGET_CONTROL,(*pstate).mj_in_path, set_value=(*ppath).mj_in_path
			WIDGET_CONTROL,(*pstate).dc_out_path,set_value=(*ppath).dc_out_path
			WIDGET_CONTROL,(*pstate).cl_out_path,set_value=(*ppath).cl_out_path
			WIDGET_CONTROL,(*pstate).nq_in_path, set_value=(*ppath).nq_in_path
			WIDGET_CONTROL,(*pstate).nq_out_path,set_value=(*ppath).nq_out_path
			WIDGET_CONTROL,(*pstate).fz_in_path, set_value=(*ppath).fz_in_path
			WIDGET_CONTROL,(*pstate).fz_out_path,set_value=(*ppath).fz_out_path

			WIDGET_CONTROL,(*pstate).ndvi_prefix,set_value=(*ppath).ndvi_prefix
			WIDGET_CONTROL,(*pstate).ndvi_suffix,set_value=(*ppath).ndvi_suffix
			WIDGET_CONTROL,(*pstate).lai_prefix ,set_value=(*ppath).lai_prefix
			WIDGET_CONTROL,(*pstate).lai_suffix ,set_value=(*ppath).lai_suffix
			WIDGET_CONTROL,(*pstate).npp_prefix ,set_value=(*ppath).npp_prefix
			WIDGET_CONTROL,(*pstate).npp_suffix ,set_value=(*ppath).npp_suffix
			WIDGET_CONTROL,(*pstate).cld_prefix ,set_value=(*ppath).cld_prefix
			WIDGET_CONTROL,(*pstate).cld_suffix ,set_value=(*ppath).cld_suffix
			WIDGET_CONTROL,(*pstate).clf_prefix ,set_value=(*ppath).clf_prefix
			WIDGET_CONTROL,(*pstate).clf_suffix ,set_value=(*ppath).clf_suffix

			WIDGET_CONTROL,(*pstate).nq_dem_file ,set_value=(*ppath).nq_dem_file
		end

    	Widget_Info(wWidget, FIND_BY_UNAME='CMD_PREFERENCE_OK'): begin
			;获取三个参数后,把三个对数写入到顶级的公共数据区中

			TIPS = DIALOG_MESSAGE('是否保存所做更改?',TITLE='提示',/QUESTION)

			if TIPS EQ 'No' then return

			COMMON COMMON_SETPATH,ppath

			;获取参数,并写入公共变量区
;			WIDGET_CONTROL,(*pstate).TXT_DSN,get_value=DSN
;			DSN=STRTRIM(DSN[0])
;
;			WIDGET_CONTROL,(*pstate).TXT_USER_NAME,get_value=USER_NAME
;			USER_NAME=STRTRIM(USER_NAME[0])
;
;			WIDGET_CONTROL,(*pstate).TXT_PWD,get_value=PWD
;			PWD=STRTRIM(PWD[0])

			WIDGET_CONTROL,(*pstate).zs_in_path,get_value=zs_in_path
			WIDGET_CONTROL,(*pstate).zs_out_path,get_value=zs_out_path
			WIDGET_CONTROL,(*pstate).mj_in_path,get_value=mj_in_path
			WIDGET_CONTROL,(*pstate).dc_out_path,get_value=dc_out_path
			WIDGET_CONTROL,(*pstate).cl_out_path,get_value=cl_out_path
			WIDGET_CONTROL,(*pstate).nq_in_path,get_value=nq_in_path
			WIDGET_CONTROL,(*pstate).nq_out_path,get_value=nq_out_path
			WIDGET_CONTROL,(*pstate).fz_in_path,get_value=fz_in_path
			WIDGET_CONTROL,(*pstate).fz_out_path,get_value=fz_out_path

			WIDGET_CONTROL,(*pstate).ndvi_prefix,get_value=ndvi_prefix
			WIDGET_CONTROL,(*pstate).ndvi_suffix,get_value=ndvi_suffix
			WIDGET_CONTROL,(*pstate).lai_prefix ,get_value=lai_prefix
			WIDGET_CONTROL,(*pstate).lai_suffix ,get_value=lai_suffix
			WIDGET_CONTROL,(*pstate).npp_prefix ,get_value=npp_prefix
			WIDGET_CONTROL,(*pstate).npp_suffix ,get_value=npp_suffix
			WIDGET_CONTROL,(*pstate).cld_prefix ,get_value=cld_prefix
			WIDGET_CONTROL,(*pstate).cld_suffix ,get_value=cld_suffix
			WIDGET_CONTROL,(*pstate).clf_prefix ,get_value=clf_prefix
			WIDGET_CONTROL,(*pstate).clf_suffix ,get_value=clf_suffix

			WIDGET_CONTROL,(*pstate).nq_dem_file ,get_value=nq_dem_file

			(*ppath).zs_in_path  = zs_in_path
			(*ppath).zs_out_path = zs_out_path
			(*ppath).mj_in_path  = mj_in_path
			(*ppath).dc_out_path = dc_out_path
			(*ppath).cl_out_path = cl_out_path
			(*ppath).nq_in_path  = nq_in_path
			(*ppath).nq_out_path = nq_out_path
			(*ppath).fz_in_path  = fz_in_path
			(*ppath).fz_out_path = fz_out_path

			(*ppath).ndvi_prefix = ndvi_prefix
			(*ppath).ndvi_suffix = ndvi_suffix
			(*ppath).lai_prefix  = lai_prefix
			(*ppath).lai_suffix  = lai_suffix
			(*ppath).npp_prefix  = npp_prefix
			(*ppath).npp_suffix  = npp_suffix
			(*ppath).cld_prefix  = cld_prefix
			(*ppath).cld_suffix  = cld_suffix
			(*ppath).clf_prefix  = clf_prefix
			(*ppath).clf_suffix  = clf_suffix

			(*ppath).nq_dem_file = nq_dem_file

			pathfile = '.\text\common_path.txt'
			OPENW, lun, pathfile, /GET_LUN
			context = '公共路径设定'
			context = [[context],zs_in_path ]
			context = [[context],zs_out_path]
			context = [[context],mj_in_path ]
			context = [[context],dc_out_path]
			context = [[context],cl_out_path]
			context = [[context],nq_in_path ]
			context = [[context],nq_out_path]
			context = [[context],fz_in_path ]
			context = [[context],fz_out_path]

			context = [[context],ndvi_prefix]
			context = [[context],ndvi_suffix]
			context = [[context],lai_prefix ]
			context = [[context],lai_suffix ]
			context = [[context],npp_prefix ]
			context = [[context],npp_suffix ]
			context = [[context],cld_prefix ]
			context = [[context],cld_suffix ]
			context = [[context],clf_prefix ]
			context = [[context],clf_suffix ]
			context = [[context],nq_dem_file ]
			context = [[context],'END']

			PRINTF, lun, context, FORMAT='(%"%s")'
			CLOSE, lun
			FREE_LUN, lun

			;========下面为杨绍锷修改的代码=20070813===============================
			openw,lun,'text\SD_PREFERENCE.txt',/GET_LUN
			enter=string(byte(13))+string(byte(10))
			FILE_PATH1=enter
			DSN1=DSN+enter
			USER_NAME1=USER_NAME
			PWD1=PWD+enter
			printf,lun,FILE_PATH1,DSN1,USER_NAME1,PWD1
			free_lun,lun
			;=======以上为杨绍锷修改的代码=20070813=========================

			;参数写完后,关闭对话框
			CLOSE,/all
			WIDGET_CONTROL, event.top, /destroy
		end
			Widget_Info(wWidget, FIND_BY_UNAME='CMD_PREFERENCE_CANCEL'): begin
			common_log,'关闭路径设定'
			CLOSE,/all
			WIDGET_CONTROL, event.top, /destroy
    	end
    	else:
	endcase
end

pro BASE_CHANGE_PREFERENCE,GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_

  IF ( XREGISTERED('BASE_CHANGE_PREFERENCE') NE 0 ) THEN RETURN

  ;获取公共变量
  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
  COMMON COMMON_SETPATH,ppath

v_zs_in_path  = (*ppath).zs_in_path
v_zs_out_path = (*ppath).zs_out_path
v_mj_in_path  = (*ppath).mj_in_path
v_dc_out_path = (*ppath).dc_out_path
v_cl_out_path = (*ppath).cl_out_path
v_nq_in_path  = (*ppath).nq_in_path
v_nq_out_path = (*ppath).nq_out_path
v_fz_in_path  = (*ppath).fz_in_path
v_fz_out_path = (*ppath).fz_out_path

v_ndvi_prefix=(*ppath).ndvi_prefix
v_ndvi_suffix=(*ppath).ndvi_suffix
v_lai_prefix =(*ppath).lai_prefix
v_lai_suffix =(*ppath).lai_suffix
v_npp_prefix =(*ppath).npp_prefix
v_npp_suffix =(*ppath).npp_suffix
v_cld_prefix =(*ppath).cld_prefix
v_cld_suffix =(*ppath).cld_suffix
v_clf_prefix =(*ppath).clf_prefix
v_clf_suffix =(*ppath).clf_suffix

v_nq_dem_file =(*ppath).nq_dem_file

  BASE_CHANGE_PREFERENCE = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='BASE_CHANGE_PREFERENCE' ,XOFFSET=308 ,YOFFSET=200  $
      ,TITLE='路径设定' ,SPACE=3  $;,SCR_XSIZE=267 ,SCR_YSIZE=270
      ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1,/column)

  BASE_CHANGE_FILE_PATH = Widget_Base(BASE_CHANGE_PREFERENCE,  $
      UNAME='BASE_CHANGE_FILE_PATH' ,FRAME=1 $
      ,SCR_XSIZE=385, SPACE=0,/column)

	row_base1 = WIDGET_BASE(BASE_CHANGE_FILE_PATH, /row)
	LBL_CHANGE_FILE_PATH = Widget_Label(row_base1,/ALIGN_LEFT  $
	  , VALUE='改变默认文件路径设置')

	wtab = WIDGET_TAB(BASE_CHANGE_FILE_PATH)
	wtab_zs = WIDGET_BASE(wtab, title='长势', /column)
	wtab_mj = WIDGET_BASE(wtab, title='面积', /column)
	wtab_dc = WIDGET_BASE(wtab, title='单产', /column)
	wtab_cl = WIDGET_BASE(wtab, title='产量', /column)
	wtab_nq = WIDGET_BASE(wtab, title='农气', /column)
	wtab_fz = WIDGET_BASE(wtab, title='复种', /column)
	wtab_wj = WIDGET_BASE(wtab, title='文件命名', /column)

	row_base2 = WIDGET_BASE(wtab_zs, /row)
	row_base3 = WIDGET_BASE(wtab_zs, /row)
	row_base4 = WIDGET_BASE(wtab_mj, /row)
	row_base5 = WIDGET_BASE(wtab_dc, /row)
	row_base6 = WIDGET_BASE(wtab_cl, /row)
	row_base16 = WIDGET_BASE(wtab_nq, /row)
	row_base7 = WIDGET_BASE(wtab_nq, /row)
	row_base8 = WIDGET_BASE(wtab_nq, /row)
	row_base9 = WIDGET_BASE(wtab_fz, /row)

	row_base10 = WIDGET_BASE(wtab_fz, /row)
	row_base11 = WIDGET_BASE(wtab_wj, /row)
	row_base12 = WIDGET_BASE(wtab_wj, /row)
	row_base13 = WIDGET_BASE(wtab_wj, /row)
	row_base14 = WIDGET_BASE(wtab_wj, /row)
	row_base15 = WIDGET_BASE(wtab_wj, /row)

	zs_in_path = CW_FIELD(row_base2, TITLE = '长势监测输入文件路径:', /string, uname='zs_in_path', XSIZE=30, /noedit, value=v_zs_in_path)
	zs_in_btn = widget_button(row_base2, EVENT_PRO='picpath', $
	   uname='zs_in_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	  , uvalue={field_id:zs_in_path, title:'选择长势监测输入文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

    zs_out_path = CW_FIELD(row_base3, TITLE = '长势监测输出文件路径:', /string, uname='zs_out_path', XSIZE=30, /noedit, value=v_zs_out_path)
	zs_out_btn = widget_button(row_base3, EVENT_PRO='picpath', $
	   uname='zs_out_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	  , uvalue={field_id:zs_out_path, title:'选择长势监测输出文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	mj_in_path = CW_FIELD(row_base4, TITLE = '面积估算输入文件路径:', /string, uname='mj_in_path', XSIZE=30, /noedit, value=v_mj_in_path)
	mj_in_btn = widget_button(row_base4, EVENT_PRO='picpath', $
	   uname='mj_in_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:mj_in_path, title:'选择面积估算输入文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	dc_out_path = CW_FIELD(row_base5, TITLE = '单产预测输出文件路径:', /string, uname='dc_out_path', XSIZE=30, /noedit, value=v_dc_out_path)
	dc_out_btn = widget_button(row_base5, EVENT_PRO='picpath', $
	   uname='dc_out_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:dc_out_path, title:'选择单产预测输出文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	cl_out_path = CW_FIELD(row_base6, TITLE = '产量估算输出文件路径:', /string, uname='cl_out_path', XSIZE=30, /noedit, value=v_cl_out_path)
	cl_out_btn = widget_button(row_base6, EVENT_PRO='picpath', $
	   uname='cl_out_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:cl_out_path, title:'选择产量估算输出文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	nq_dem_file = CW_FIELD(row_base16, TITLE = '农气分析DEM文件路径: ', /string, uname='nq_dem_file', XSIZE=30, /noedit, value=v_nq_dem_file)
	nq_dem_btn = widget_button(row_base16, EVENT_PRO='picpath', $
	   uname='nq_dem_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:nq_dem_file, title:'选择农气分析DEM文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	nq_in_path = CW_FIELD(row_base7, TITLE = '插值结果输出文件路径:', /string, uname='nq_in_path', XSIZE=30, /noedit, value=v_nq_in_path)
	nq_in_btn = widget_button(row_base7, EVENT_PRO='picpath', $
	   uname='nq_in_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:nq_in_path, title:'选择农气分析插值结果输出文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

    nq_out_path = CW_FIELD(row_base8, TITLE = '对比分析输出文件路径:', /string, uname='nq_out_path', XSIZE=30, /noedit, value=v_nq_out_path)
	nq_out_btn = widget_button(row_base8, EVENT_PRO='picpath', $
	   uname='nq_out_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:nq_out_path, title:'选择农气分析对比分析输出文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	fz_in_path = CW_FIELD(row_base9, TITLE = '复种指数输入文件路径:', /string, uname='fz_in_path', XSIZE=30, /noedit, value=v_fz_in_path)
	fz_in_btn = widget_button(row_base9, EVENT_PRO='picpath', $
	   uname='fz_in_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:fz_in_path, title:'选择复种指数输入文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

    fz_out_path = CW_FIELD(row_base10, TITLE = '复种指数输出文件路径:', /string, uname='fz_out_path', XSIZE=30, /noedit, value=v_fz_out_path)
	fz_out_btn = widget_button(row_base10, EVENT_PRO='picpath', $
	   uname='fz_out_btn', SCR_XSIZE=36 ,SCR_YSIZE=22   $
	   , uvalue={field_id:fz_out_path, title:'选择复种指数输出文件路径'}  $
      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP)

	label = widget_label(row_base11, xsize=80, value='NDVI文件：')
	ndvi_prefix = widget_text(row_base11, xsize=5, /EDITABLE, value=v_ndvi_prefix)
	label = widget_label(row_base11, xsize=80, value=' + 年月日 + ',/ALIGN_CENTER)
	ndvi_suffix = widget_text(row_base11, xsize=5, /EDITABLE, value=v_ndvi_suffix)

	label = widget_label(row_base12, xsize=80, value='LAI文件：')
	lai_prefix = widget_text(row_base12, xsize=5, /EDITABLE, value=v_lai_prefix)
	label = widget_label(row_base12, xsize=80, value=' + 年月日 + ',/ALIGN_CENTER)
	lai_suffix = widget_text(row_base12, xsize=5, /EDITABLE, value=v_lai_suffix)

	label = widget_label(row_base13, xsize=80, value='NPP文件：')
	npp_prefix = widget_text(row_base13, xsize=5, /EDITABLE, value=v_npp_prefix)
	label = widget_label(row_base13, xsize=80, value=' + 年月日 + ',/ALIGN_CENTER)
	npp_suffix = widget_text(row_base13, xsize=5, /EDITABLE, value=v_npp_suffix)

	label = widget_label(row_base14, xsize=80, value='云文件：')
	cld_prefix = widget_text(row_base14, xsize=5, /EDITABLE, value=v_cld_prefix)
	label = widget_label(row_base14, xsize=80, value=' + 年月日 + ',/ALIGN_CENTER)
	cld_suffix = widget_text(row_base14, xsize=5, /EDITABLE, value=v_cld_suffix)

	label = widget_label(row_base15, xsize=80, value='差值分级图像：')
	clf_prefix = widget_text(row_base15, xsize=5, /EDITABLE, value=v_clf_prefix)
	label = widget_label(row_base15, xsize=80, value=' + 年月日 + ',/ALIGN_CENTER)
	clf_suffix = widget_text(row_base15, xsize=5, /EDITABLE, value=v_clf_suffix)


;  BASE_DB_USER_PWD = Widget_Base(BASE_CHANGE_PREFERENCE,  $
;      UNAME='BASE_DB_USER_PWD' ,FRAME=1 ,XOFFSET=5 ,YOFFSET=71  $
;      ,SCR_XSIZE=250 ,SCR_YSIZE=121 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
;      ,YPAD=3)
;
;  LBL_CHANGE_USER_PWD = Widget_Label(BASE_DB_USER_PWD,  $
;      UNAME='LBL_CHANGE_USER_PWD' ,XOFFSET=6 ,YOFFSET=8  $
;      ,SCR_XSIZE=124 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
;      ,VALUE='改变数据库访问设置')
;
;  LBL_DSN = Widget_Label(BASE_DB_USER_PWD,  $
;      UNAME='LBL_DSN' ,XOFFSET=110 ,YOFFSET=38 ,SCR_XSIZE=52  $
;      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='数据源名:')
;
;  LBL_USER_NAME = Widget_Label(BASE_DB_USER_PWD,  $
;      UNAME='LBL_USER_NAME' ,XOFFSET=110 ,YOFFSET=38+26 ,SCR_XSIZE=52  $
;      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='用户名  :')
;
;  LBL_PWD = Widget_Label(BASE_DB_USER_PWD, UNAME='LBL_PWD'  $
;      ,XOFFSET=110 ,YOFFSET=65+26 ,SCR_XSIZE=52 ,SCR_YSIZE=18  $
;      ,/ALIGN_LEFT ,VALUE='密码    :')
;
;  TXT_USER_NAME = Widget_Text(BASE_DB_USER_PWD, UNAME='TXT_USER_NAME'  $
;      ,XOFFSET=167 ,YOFFSET=33+26 ,SCR_XSIZE=113 ,SCR_YSIZE=20 ,/EDITABLE  $
;      ,XSIZE=20 ,YSIZE=1,value=USER_NAME)
;
;  TXT_PWD = Widget_Text(BASE_DB_USER_PWD, UNAME='TXT_PWD' ,XOFFSET=167  $
;      ,YOFFSET=62+26 ,SCR_XSIZE=113 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20  $
;      ,YSIZE=1,value=PWD)
;
;  TXT_DSN=Widget_Text(BASE_DB_USER_PWD, UNAME='TXT_DSN' ,XOFFSET=167  $
;      ,YOFFSET=33 ,SCR_XSIZE=113 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20  $
;      ,YSIZE=1,value=DSN)

  BASE_CMD_OK_CANCEL = Widget_Base(BASE_CHANGE_PREFERENCE,  $
      UNAME='BASE_CMD_OK_CANCEL' ,FRAME=1 ,XOFFSET=5 ,YOFFSET=172+26  $
      ,SCR_XSIZE=250 ,SCR_YSIZE=36 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)

  CMD_PREFERENCE_OK = Widget_Button(BASE_CMD_OK_CANCEL,  $
      UNAME='CMD_PREFERENCE_OK' ,XOFFSET=50 ,YOFFSET=6 ,SCR_XSIZE=80  $
      ,SCR_YSIZE=22  ,/ALIGN_CENTER  $
      ,VALUE='确定')

  CMD_PREFERENCE_DEFAULT = Widget_Button(BASE_CMD_OK_CANCEL,  $
      UNAME='CMD_PREFERENCE_DEFAULT' ,XOFFSET=155 ,YOFFSET=6 ,SCR_XSIZE=80  $
      ,SCR_YSIZE=22  ,/ALIGN_CENTER  $
      ,VALUE='恢复缺省')

  CMD_PREFERENCE_CANCEL = Widget_Button(BASE_CMD_OK_CANCEL,  $
      UNAME='CMD_PREFERENCE_CANCEL' ,XOFFSET=260 ,YOFFSET=6  $
      ,SCR_XSIZE=80 ,SCR_YSIZE=22   $
      ,/ALIGN_CENTER ,VALUE='关闭')

	state = { $
;				TXT_USER_NAME 	:  TXT_USER_NAME, $
;				TXT_PWD  		:  TXT_PWD	, $
;				TXT_DSN			:  TXT_DSN ,$
				zs_in_path  : zs_in_path  ,$
				zs_out_path : zs_out_path ,$
				mj_in_path  : mj_in_path  ,$
				dc_out_path : dc_out_path ,$
				cl_out_path : cl_out_path ,$
				nq_in_path  : nq_in_path  ,$
				nq_out_path : nq_out_path ,$
				fz_in_path  : fz_in_path  ,$
				fz_out_path : fz_out_path ,$

				ndvi_prefix: ndvi_prefix,$
				ndvi_suffix: ndvi_suffix,$
				lai_prefix : lai_prefix ,$
				lai_suffix : lai_suffix ,$
				npp_prefix : npp_prefix ,$
				npp_suffix : npp_suffix ,$
				cld_prefix : cld_prefix ,$
				cld_suffix : cld_suffix ,$
				clf_prefix : clf_prefix ,$
				clf_suffix : clf_suffix ,$

				nq_dem_file: nq_dem_file $
			}

    pstate = PTR_NEW(state, /no_copy)
    Widget_Control, BASE_CHANGE_PREFERENCE, set_uvalue=pstate
    Widget_Control, /REALIZE, BASE_CHANGE_PREFERENCE
    WIDGET_CONTROL,CMD_PREFERENCE_CANCEL,/INPUT_FOCUS

    XManager, 'BASE_CHANGE_PREFERENCE', BASE_CHANGE_PREFERENCE, /NO_BLOCK, cleanup='SD_Connect_cleanup'
end

pro SD_PREFERENCE, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  common_log,'启动路径设定'
  BASE_CHANGE_PREFERENCE,GROUP_LEADER=BASE_TOP;, _EXTRA=_VWBExtra_
end