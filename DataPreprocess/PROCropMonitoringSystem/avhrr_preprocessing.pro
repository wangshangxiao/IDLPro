PRO AVHRR_PREP,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	fname=*((*PSTATE).FNAME)

	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=OUTPUT_PATH

;	restore,'save2\envi.sav'
	envi, /restore_base_save_files

	envi_batch_init

	for i=0,n_elements(fname)-1 do begin

		envi_open_data_file, fname[i], $
		r_fid=fid, /avhrr

		if (fid eq -1) then begin
;			envi_batch_exit
			CAUTION = dialog_message('未找到影像文件!',title='警告')
			return
		endif
		;
		; Set the keywords
		;
		envi_file_query, fid, ns=ns, nl=nl, nb=nb
		dims = [-1l, 0, ns-1, 0, nl-1]
		pos = lindgen(nb)
		;
		; Call the doit
		;
		envi_doit, 'envi_avhrr_calibrate_doit', $
		fid=fid, dims=dims, pos=pos, $
			/in_memory, /correct_solarz, $
			r_fid=c_fid


		;以下为几何纠正
		envi_file_query, fid, dims=dims, ns=ns, nl=nl
		method = [1,1,0,0]

		envi_doit, 'envi_avhrr_geometry_doit', $
			fid=fid, dims=dims, /in_memory, $
			out_dt=4, method=method, r_fid=g_fid

		compile_opt strictarr
		;  Figure out what UTM zone we're in.
		query_dims = [-1L, long(dims[2]/2), long(dims[2]/2), long(dims[4]/2),long(dims[4]/2)]
		lat_lon = [envi_get_data(fid=g_fid, dims=query_dims, pos=0), $
				envi_get_data(fid=g_fid, dims=query_dims, pos=1)]
		zone = fix(31.0 + lat_lon[1]/6.0)
		south = (lat_lon[0] lt 0)

		;  Make the GLT.
		;	envi_file_query, fid
		pixel_size=1100.0D
		rotation=0.0
		i_proj = envi_proj_create(/geographic)
		o_proj = envi_proj_create(/utm, zone=zone, south=south)

		envi_glt_doit, i_proj=i_proj, $
			o_proj=o_proj, /in_memory, $
			pixel_size=pixel_size, r_fid=glt_fid, $
			rotation=rotation, x_fid=g_fid, y_fid=g_fid, $
			x_pos=1, y_pos=0

		if glt_fid eq -1 then return

		;  Georeference the image from the GLT.

		envi_doit, 'envi_georef_from_glt_doit', fid=c_fid, $
			glt_fid=glt_fid, /in_memory, pos=[0,1], $
			subset=dims, r_fid=r_fid

		field=strsplit(file_basename(fname[i]),'.',/extract)

		out_name=OUTPUT_PATH+field[0]+'.tif'
		ENVI_FILE_QUERY, r_fid, ns=ns, nl=nl
		dims = [-1, 0, ns-1, 0, nl-1]
		ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=r_fid,pos=[0,1],out_name=out_name,/tiff

		envi_file_mng, id=c_fid, /remove
		envi_file_mng, id=fid, /remove
		envi_file_mng, id=g_fid, /remove
		envi_file_mng, id=glt_fid, /remove
		envi_file_mng, id=r_fid, /remove
	endfor

	CAUTION = Dialog_Message('完成AVHRR数据处理',title='提示')

END

PRO AVHRR_PREPROCESSING_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
		widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		widget_control,event.id,get_uvalue=pfinfo
				field_id = pfinfo.field_id
				filter = pfinfo.filter
				title = pfinfo.title
				filename = DIALOG_PICKFILE(title=title, filter=filter,/MULTIPLE_FILES);envi_pickfile
				if (filename[0] eq '') then return else begin
					widget_control,field_id,set_value=filename
					(*PSTATE).FNAME=PTR_NEW(filename,/NO_COPY)
					Widget_Control,event.top,set_uvalue=PSTATE
				end
      		common_log,'打开输入影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE_PATH,event
      		common_log,'设定输出影像文件夹'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				AVHRR_PREP,EVENT
      		common_log,'进行AVHRR数据预处理'
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
      		common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '特定数据处理', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
      		Widget_Control,event.top,/destroy
      		common_log,'退出AVHRR数据预处理程序'
      		return
		end
		else :
	endcase

  	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
		common_log,'退出AVHRR数据预处理程序'
	ENDIF

END

PRO AVHRR_PREPROCESSING, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'打开AVHRR数据预处理程序'

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

	AP_TLB = Widget_Base( GROUP_LEADER=wGroup,/COLUMN,UNAME='AP_TLB' ,SPACE=3$
       ,TITLE='AVHRR数据预处理',XPAD=1,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,TLB_FRAME_ATTR=1)

	FILE_BASE = Widget_Base(AP_TLB,/COLUMN,UNAME='FILE_BASE',/FRAME)

	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE')
	INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='输入数据',XSIZE=35,YSIZE=1,NOEDIT=1)
	INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,YSIZE=10)

	pfinfo = {field_id:INPUT_FIELD, filter:'*.1B', title:'输入AVHRR 1B数据'}
	Widget_Control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE')
	OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='输出数据',XSIZE=35,YSIZE=1,NOEDIT=1)
	OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
					,SCR_XSIZE=36 ,YSIZE=10)
	pfinfo = {field_id:OUTPUT_FIELD, title:'输出影像文件夹'}
	Widget_Control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	CMD_BASE = Widget_Base(AP_TLB,UNAME='CMD_BASE',/FRAME,/row,space=53,XPAD=15)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='计算',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	INFO_TEXT = Widget_Text(AP_TLB,UNAME='INFO_TEXT',VALUE= $
		'用于对AVHRR 1B数据进行辐射定标和几何粗纠正,可选择多个文件进行批量处理',/WRAP,YSIZE=2)

	Widget_Control,AP_TLB,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					FNAME	:	PTR_NEW(/NO_COPY) $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,AP_TLB,set_uvalue=PSTATE

	Xmanager,'AVHRR_PREPROCESSING',AP_TLB,/NO_BLOCK

END