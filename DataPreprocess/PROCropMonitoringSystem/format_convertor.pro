PRO FORMAT_CONV,EVENT

	Widget_Control,event.top,get_uvalue=PSTATE
	Widget_Control,(*PSTATE).OUTPUT_FIELD,GET_VALUE=outpath

	fname = *((*PSTATE).FILE_LIST)
	type = Widget_Info((*PSTATE).OUTPUT_TYPE_LIST,/COMBOBOX_GETTEXT)

;	restore,'save2\envi.sav'
	envi, /restore_base_save_files
	envi_batch_init

	status = 0

	for i=0,n_elements(fname)-1 do begin
		field=strsplit(file_basename(fname[i]),'.',/extract)
		if field[1] eq 'hdr' then begin
			inputname=field[0]
		endif else begin
			inputname=field[0]+'.'+field[1]
		endelse
		if file_test(file_dirname(fname[i],/MARK_DIRECTORY)+inputname) eq 0 then begin
			CAUTION = dialog_message('未指定输入数据或输入数据无效!',title='警告')
			return
		endif
		envi_open_data_file, file_dirname(fname[i],/MARK_DIRECTORY)+inputname, r_fid=fid $
		,/TIFF,/IMAGINE,/ESRI_GRID,/PCI,/NITF,/ERMAPPER,/MODIS,/envi

		ENVI_FILE_QUERY, fid, ns=ns, nl=nl,nb=nb,dims=dims

		opos=indgen(nb)
;		dims = [-1, 0, ns-1, 0, nl-1]

		case type of

			'TIFF'	:	begin
				outfile=outpath+field[0]+'.tif'
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/tiff
				status += 1
			end
			'NITF'	:	begin
				outfile=outpath+field[0]+'.nitf'
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/NITF
				status += 1
			end
			'ENVI Standard Format'	:	begin
				outfile=outpath+field[0]
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/ENVI
				status += 1
			end
			'ERDAS IMAGINE'	:	begin
				outfile=outpath+field[0]+'.img'
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/IMAGINE
				status += 1
			end
			'ESRI GRID'	:	begin
				outfile=outpath+field[0]
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/ESRI_GRID
				status += 1
			end
			'PCI Geomatics format'	:	begin
				outfile=outpath+field[0]+'.pix'
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/PCI
				status += 1
			end
			'ERMAPPER'	:	begin
				outfile=outpath+field[0]+'.ers'
				if file_test(outfile) eq 1 then begin
					CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   	if CAUTION EQ 'No' THEN BEGIN
				   	break
					endif
				endif
				ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=fid,pos=opos,out_name=outfile,/ERMAPPER
				status += 1
			end
			ELSE	:
		endcase
	endfor

	if status eq n_elements(fname) then $
		CAUTION=dialog_message('格式转换完成！',title='提示')
	envi_file_mng, id=fid, /remove

END

PRO FORMAT_CONVERTOR_EVENT,EVENT

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
			FILE_FILTER = ['*.hdr','*.img','*.tif','*.pix','*.ers','*.nitf']
			INPUTFILE=DIALOG_PICKFILE(title='输入影像文件', filter=FILE_FILTER,/MULTIPLE_FILES,dialog_parent=event.top)

      	infile = INPUTFILE[0]
			for i=1,n_elements(INPUTFILE)-1,1 do begin
				temp = ','+INPUTFILE[i]
				infile += temp
			endfor
			Widget_Control,(*PSTATE).INPUT_FIELD,SET_VALUE=infile

			if strcompress(INPUTFILE[0]) ne '' then begin
				(*PSTATE).FILE_LIST = PTR_NEW(INPUTFILE,/NO_COPY)
	      	Widget_Control,event.top,set_uvalue=PSTATE
      	endif

      	common_log,'输入文件'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin

			OUTPUTFILE=DIALOG_PICKFILE(title='输出影像文件夹', /DIRECTORY ,dialog_parent=event.top)
			Widget_Control,(*PSTATE).OUTPUT_FIELD,SET_VALUE=OUTPUTFILE
      	common_log,'设置输出文件夹'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin

;			if PTR_VALID((*PSTATE).FILE_LIST) eq 0 then begin
;				CAUTION = dialog_message('未指定输入数据或输入数据无效!',title='警告')
;				return
;			endif else begin
;				file_check = where(file_test(*((*PSTATE).FILE_LIST)) eq 0)
;				if ( file_check[0] ne -1) then begin
;					CAUTION = dialog_message('未指定输入数据或输入数据无效!',title='警告')
;		    		return
;				endif
;			endelse

			Widget_Control,(*PSTATE).INPUT_FIELD,GET_VALUE=INPUTFILE
			INPUT = STRSPLIT(INPUTFILE,',',/EXTRACT)
			file_check = where(file_test(INPUT) eq 0)
			if ( file_check[0] ne -1) then begin
				CAUTION = dialog_message('未指定输入数据或输入数据无效!',title='警告')
	    		return
			endif

			Widget_Control,(*PSTATE).OUTPUT_FIELD,GET_VALUE=OUTPUTFILE

			path_check = file_test(OUTPUTFILE,/DIRECTORY)
			if (path_check eq 0) then begin
				CAUTION = dialog_message('未指定输出路径或路径无效!',title='警告')
	    		return
			endif

			FORMAT_CONV,EVENT

      	common_log,'进行影像格式转换'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '格式转换', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'退出格式转换'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出格式转换'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO FORMAT_CONVERTOR,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

	FCON_TLB = Widget_Base(GROUP_LEADER=wGroup,/COLUMN,UNAME='FCON_TLB' ,SPACE=2,TITLE='格式转换', $
		XPAD=2,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,TLB_FRAME_ATTR=1,/modal)

	FILE_BASE = Widget_Base(FCON_TLB,/COLUMN,/FRAME,UNAME='FILE_BASE',SPACE=2,XPAD=1,YPAD=1,/BASE_ALIGN_CENTER)

	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)

		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',XSIZE=30 ,YSIZE=1,TITLE='输入影像',NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

	OUTPUT_BASE = Widget_Base(FILE_BASE,/COLUMN,UNAME='OUTPUT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)

	OUTPUT_FILE_BASE = Widget_Base(OUTPUT_BASE,/ROW,UNAME='OUTPUT_FILE_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_FILE_BASE,UNAME='OUTPUT_FIELD',XSIZE=30 ,YSIZE=1,TITLE='输出影像',NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_FILE_BASE,UNAME='OUTPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)
	OUTPUT_TYPE_BASE	= Widget_Base(OUTPUT_BASE,/ROW,UNAME='OUTPUT_TYPE_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)
		OUTPUT_TYPE_LABEL = Widget_Label(OUTPUT_TYPE_BASE,UNAME='OUTPUT_TYPE_LABEL',VALUE='输出类型')
		TYPE_LIST = ['TIFF','NITF','ENVI Standard Format','ERDAS IMAGINE' $
						,'ESRI GRID','PCI Geomatics format','ERMAPPER']
		OUTPUT_TYPE_LIST = Widget_Combobox(OUTPUT_TYPE_BASE,UNAME='OUTPUT_TYPE_LIST',VALUE=TYPE_LIST,XSIZE=230)

	CMD_BASE = Widget_Base(FCON_TLB,/ROW,UNAME='CMD_BASE',XPAD=9,SPACE=40,/FRAME,/BASE_ALIGN_CENTER)

	CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='运行',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
	HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
	EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,FCON_TLB,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					FILE_LIST	:	PTR_NEW(/NO_COPY) , $
					OUTPUT_TYPE_LIST	:	OUTPUT_TYPE_LIST $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,FCON_TLB,SET_UVALUE=PSTATE

	XManager, 'FORMAT_CONVERTOR',FCON_TLB,/NO_BLOCK

END