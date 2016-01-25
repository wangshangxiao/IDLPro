;进行波段叠加操作，可对各输入的影像波段进行排序
;钮立明
;2010年3月

FUNCTION LAYER_STACK_EXECUTE,event

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		common_log,'计算出错'
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return,0
	ENDIF

	group_id=event.top
	widget_control, group_id, get_uvalue=PSTATE

	widget_control, (*PSTATE).OUTPUT_FIELD,get_value = outputfile

	file_stack=(*PSTATE).file_list

	if PTR_VALID(file_stack) eq 0 then begin
		CAUTION=dialog_message('未导入文件！',title='警告')
		return,0
	endif

	options=(*PSTATE).option

	if strcompress(file_basename(outputfile, '.tif'), /REMOVE_ALL) eq '' then begin
		Result = DIALOG_MESSAGE('输出文件名称不正确！' + outputfile, /CENTER)
		return,0
	endif

	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	fname=*file_stack
	fnum=n_elements(fname)

	if fnum lt 2 then begin
		Result = DIALOG_MESSAGE('至少需要2个输入数据', /CENTER)
		return,0
	endif

	if strcompress(file_basename(outputfile, '.tif'), /REMOVE_ALL) eq '' then begin
		Result = DIALOG_MESSAGE('输出文件名称不正确！' + outputfile, /CENTER)
		return,0
	endif

	envi, /restore_base_save_files
	envi_batch_init

	t_fid=lonarr(fnum)
	t_ns=lonarr(fnum)
	t_nl=lonarr(fnum)
	t_nb=lonarr(fnum)

	compile_opt strictarr

	for i=0,fnum-1,1 do begin

		envi_open_file, fname[i], r_fid=d_fid
		t_fid[i]=d_fid
		if (t_fid[i] eq -1) then begin
			CAUTION = dialog_message('未找到影像文件!',title='警告')
			return,0
		endif

		envi_file_query, t_fid[i],ns=d_ns, nl=d_nl, nb=d_nb
		t_ns[i]=d_ns
		t_nl[i]=d_nl
		t_nb[i]=d_nb
	endfor

	nb = long(total(t_nb))
	fid = lonarr(nb)
	pos = lonarr(nb)
	dims = lonarr(5,nb)

	j=0
	count=0
	for i=0L,nb-1 do begin
		fid[i] = t_fid[j]
		pos[i] = count
		dims[0,i] = [-1,0,t_ns[j]-1,0,t_nl[j]-1]
		count++
		if count eq t_nb[j] then begin
			count=0
			j++
		endif
	endfor

	out_proj = envi_get_projection( fid=t_fid[0], pixel_size=out_ps)
	out_name = outputfile
	out_dt = 4

	envi_doit, 'envi_layer_stacking_doit', $
	fid=fid, pos=pos, dims=dims, exclusive=options, $
	out_dt=out_dt,/in_memory, $
	interp=2, out_ps=out_ps, $
	out_proj=out_proj, r_fid=r_fid

	ENVI_FILE_QUERY, r_fid,dims=dims
	pos=indgen(nb)
	ENVI_OUTPUT_TO_EXTERNAL_FORMAT,dims=dims,fid=r_fid,pos=pos,out_name=out_name,/tiff

	envi_file_mng, id=d_fid, /remove
	envi_file_mng, id=fid, /remove
	envi_file_mng, id=r_fid, /remove
	envi_file_mng, id=t_fid[1:*], /remove

	return,1

END

PRO REORDER_LIST_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=QSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	ORDER_INDEX = Widget_Info((*QSTATE).REORDER_FILE_LIST,/LIST_SELECT)
	NUM_LIST = Widget_Info((*QSTATE).REORDER_FILE_LIST,/LIST_NUMBER)

	case wTarget of

		Widget_Info(wWidget, FIND_BY_UNAME='UP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				if (ORDER_INDEX[0] eq 0)or (ORDER_INDEX[0] eq -1) then $
					return

				NEW_ORDER_INDEX=ORDER_INDEX-1

				temp1=(*((*QSTATE).FILE_LIST))
				temp2=(*((*QSTATE).FILE_LIST))
				temp1[NEW_ORDER_INDEX]=''
				temp2[ORDER_INDEX]=''
				kindex1=where(temp1 ne '')
				kindex2=where(temp2 ne '')

				temp1[NEW_ORDER_INDEX]=(*((*QSTATE).FILE_LIST))[ORDER_INDEX]
				temp1[kindex1]=(*((*QSTATE).FILE_LIST))[kindex2]

				(*QSTATE).FILE_LIST=PTR_NEW(temp1)
				Widget_Control,(*QSTATE).REORDER_FILE_LIST,set_value=temp1
				Widget_Control,(*QSTATE).REORDER_FILE_LIST,SET_LIST_SELECT=NEW_ORDER_INDEX
				Widget_Control,event.top,set_uvalue=QSTATE
		end

		Widget_Info(wWidget, FIND_BY_UNAME='DOWN_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		if (ORDER_INDEX[n_elements(ORDER_INDEX)-1] eq NUM_LIST-1)or (ORDER_INDEX[0] eq -1) then $
					return

				NEW_ORDER_INDEX=ORDER_INDEX+1

				temp1=(*((*QSTATE).FILE_LIST))
				temp2=(*((*QSTATE).FILE_LIST))
				temp1[NEW_ORDER_INDEX]=''
				temp2[ORDER_INDEX]=''
				kindex1=where(temp1 ne '')
				kindex2=where(temp2 ne '')

				temp1[NEW_ORDER_INDEX]=(*((*QSTATE).FILE_LIST))[ORDER_INDEX]
				temp1[kindex1]=(*((*QSTATE).FILE_LIST))[kindex2]

				(*QSTATE).FILE_LIST=PTR_NEW(temp1)
				Widget_Control,(*QSTATE).REORDER_FILE_LIST,set_value=temp1
				Widget_Control,(*QSTATE).REORDER_FILE_LIST,SET_LIST_SELECT=NEW_ORDER_INDEX
				Widget_Control,event.top,set_uvalue=QSTATE
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CONFIRM_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		Widget_Control,(*QSTATE).INPUT_LIST,set_value=(*((*QSTATE).FILE_LIST))
      		(*((*QSTATE).PSTATE)).FILE_LIST=PTR_NEW((*((*QSTATE).FILE_LIST)),/NO_COPY)
      		Widget_Control,(*QSTATE).tlb,SET_UVALUE=(*QSTATE).PSTATE
      		Widget_Control,event.top,/DESTROY
		end

		Widget_Info(wWidget, FIND_BY_UNAME='CANCEL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		widget_control,event.top,/DESTROY
		end

		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出影像叠加'
  		PTR_FREE,QSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF
END

PRO REORDER_LIST,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	Widget_Control,(*PSTATE).LAYSTK_TLB,tlb_get_offset=tlb_offset

	w_xoffset=tlb_offset[0]+305
	w_yoffset=tlb_offset[1]

	REORDER_TLB = Widget_Base(GROUP_LEADER=(*PSTATE).LAYSTK_TLB,/COLUMN,UNAME='REORDER_TLB' ,SPACE=1$
       ,TITLE='影像文件排序',XPAD=0,YPAD=1,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,/modal)

   REORDER_BASE = Widget_Base(REORDER_TLB,/ROW,UNAME='REORDER_BASE',SPACE=1,/FRAME,XPAD=1)

	REORDER_LIST_BASE = Widget_Base(REORDER_BASE,/COLUMN,UNAME='REORDER_LIST_BASE',SPACE=1,XPAD=0,YPAD=0, $
			/BASE_ALIGN_CENTER)
		REORDER_LIST_LABEL = Widget_Label(REORDER_LIST_BASE,UNAME='REORDER_LIST_LABEL',VALUE='影像文件排序')
		REORDER_FILE_LIST = Widget_LIST(REORDER_LIST_BASE,UNAME='REORDER_FILE_LIST', $
			VALUE=(*((*PSTATE).FILE_LIST)),/MULTIPLE,XSIZE=38,YSIZE=15)

		Widget_Control,REORDER_FILE_LIST,SET_LIST_SELECT=0

	REORDER_CTL_BASE = Widget_Base(REORDER_BASE,/COLUMN,UNAME='REORDER_CTL_BASE',/BASE_ALIGN_CENTER, $
			SPACE=10,XPAD=0,YPAD=70)
		UP_BUTTON = Widget_Button(REORDER_CTL_BASE,UNAME='UP_BUTTON',SCR_XSIZE=20,SCR_YSIZE=20, $
				/ALIGN_CENTER,VALUE='.\Image\arrow_up_fat.bmp',/BITMAP)
		DOWN_BUTTON = Widget_Button(REORDER_CTL_BASE,UNAME='DOWN_BUTTON',SCR_XSIZE=20,SCR_YSIZE=20, $
				/ALIGN_CENTER,VALUE='.\Image\arrow_down_fat.bmp',/BITMAP)

	REORDER_CMD_BASE = Widget_Base(REORDER_TLB,/ROW,UNAME='REORDER_CMD_BASE',SPACE=90,/FRAME,XPAD=30)
		CONFIRM_BUTTON = Widget_Button(REORDER_CMD_BASE,VALUE='确定',UNAME='CONFIRM_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		CANCEL_BUTTON = Widget_Button(REORDER_CMD_BASE,VALUE='取消',UNAME='CANCEL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	winfo = {	REORDER_FILE_LIST	:	REORDER_FILE_LIST	, $
					FILE_LIST	:	(*PSTATE).FILE_LIST	, $
					INPUT_LIST	:	(*PSTATE).INPUT_LIST	, $
					PSTATE	:	PSTATE	, $
					tlb	:	event.top	}

	QSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,REORDER_TLB,set_uvalue=QSTATE

	Widget_Control,REORDER_TLB,/REALIZE

	XManager, 'REORDER_LIST',REORDER_TLB,/NO_BLOCK
END

PRO LAYER_STACKING_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='IMPORT_BUTTON'): begin
			INPUTFILE=DIALOG_PICKFILE(title='输入叠加影像文件', filter='*.tif',/MULTIPLE_FILES,dialog_parent=event.top)

			if INPUTFILE[0] ne '' then begin
				if PTR_VALID((*PSTATE).FILE_LIST) eq 0 then begin
					(*PSTATE).file_list=PTR_NEW(INPUTFILE,/NO_COPY)
				endif else begin
					(*PSTATE).file_list=PTR_NEW([*((*PSTATE).file_list),INPUTFILE])
				endelse
	      	Widget_Control,(*PSTATE).INPUT_LIST,set_value=*((*PSTATE).FILE_LIST)
	      	Widget_Control,event.top,set_uvalue=PSTATE
      	endif

      	common_log,'输入文件'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='REORDER_BUTTON'): begin
			file_stack=(*PSTATE).file_list

			if PTR_VALID(file_stack) eq 0 then begin
				CAUTION=dialog_message('未导入文件！',title='警告')
				return
			endif

			if n_elements(*file_stack) LE 1 then begin
				CAUTION=dialog_message('少于两个文件，不需要排序！',title='警告')
				return
			endif

			REORDER_LIST,event
      	common_log,'进行文件排序'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='DEL_BUTTON'): begin
			file_stack=(*PSTATE).file_list

			if PTR_VALID(file_stack) eq 0 then begin
				CAUTION=dialog_message('未导入文件！',title='警告')
				return
			endif

			index = Widget_Info((*PSTATE).INPUT_LIST,/LIST_SELECT)

			if index[0] ne -1 then begin
				(*file_stack)[index]=''
			endif else begin
				CAUTION=dialog_message('未选择文件！',title='警告')
				return
			endelse

			kindex=where((*file_stack) ne '')

			if kindex[0] ne -1 then begin
				Widget_Control,(*PSTATE).INPUT_LIST,set_value=(*file_stack)[kindex]
      		(*PSTATE).FILE_LIST=PTR_NEW((*file_stack)[kindex],/NO_COPY)
      	endif else begin
      		Widget_Control,(*PSTATE).INPUT_LIST,set_value=''
      		(*PSTATE).FILE_LIST=PTR_NEW(/NO_COPY)
			endelse

      	Widget_Control,event.top,set_uvalue=PSTATE
      	common_log,'删除文件'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='UNION_BUTTON'): begin
	      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				(*PSTATE).OPTION=0
				WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
				common_log,'设置输出影像取各影像并集'
    	end
    	Widget_Info(wWidget, FIND_BY_UNAME='INTERSEACTION_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				(*PSTATE).OPTION=1
				WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
				common_log,'设置输出影像取各影像交集'
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				PICK_FILE,Event
				WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
				common_log,'设置输出文件名'
    	end

		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin

			Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=FILE
			tempfile=strsplit(FILE,'.',/extract)
			outputfile=tempfile[0]+'.tif'
			if file_test(outputfile) eq 1 then begin
				CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
			   if CAUTION EQ 'No' THEN BEGIN
				   return
				endif
			endif

			status = LAYER_STACK_EXECUTE(event)

			if status eq 1 then begin
				CAUTION=dialog_message('影像叠加完成！',title='提示')
			endif

      	common_log,'进行影像叠加'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP,'图层叠加',BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'退出影像叠加'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出影像叠加'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO LAYER_STACKING, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	on_error,2

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

	LAYSTK_TLB = Widget_Base(GROUP_LEADER=wGroup,/COLUMN,UNAME='LAYSTK_TLB' ,SPACE=1$
       ,TITLE='图层叠加',XPAD=1,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset $
       ,TLB_FRAME_ATTR=1)

	INPUT_BASE = Widget_Base(LAYSTK_TLB,/COLUMN,UNAME='INPUT_BASE',SPACE=1,/FRAME)

	INPUT_LIST_BASE = Widget_Base(INPUT_BASE,/COLUMN,UNAME='LIST_BASE',SPACE=1,XPAD=0,YPAD=0, $
			/BASE_ALIGN_CENTER)
		LIST_LABEL = Widget_Label(INPUT_LIST_BASE,UNAME='LIST_LABEL',VALUE='选择用于叠加的影像文件')
		INPUT_LIST = Widget_LIST(INPUT_LIST_BASE,UNAME='INPUT_LIST',/MULTIPLE,XSIZE=43,YSIZE=9)

	INPUT_BUTTON_BASE = Widget_Base(INPUT_BASE,/ROW,UNAME='INPUT_BUTTON_BASE',SPACE=27,XPAD=3,YPAD=0, $
			/BASE_ALIGN_CENTER)
		IMPORT_BUTTON = Widget_BUTTON(INPUT_BUTTON_BASE,UNAME='IMPORT_BUTTON',XSIZE=70,YSIZE=25, $
			VALUE='输入文件')
		REORDER_BUTTON = Widget_BUTTON(INPUT_BUTTON_BASE,UNAME='REORDER_BUTTON',XSIZE=70,YSIZE=25, $
			VALUE='文件排序')
		DEL_BUTTON = Widget_BUTTON(INPUT_BUTTON_BASE,UNAME='DEL_BUTTON',XSIZE=70,YSIZE=25, $
			VALUE='删除文件')

	RANGE_BASE = Widget_Base(LAYSTK_TLB,/COLUMN,UNAME='RANGE_BASE',SPACE=0,/FRAME)

	RANGE_LABEL_BASE = Widget_Base(RANGE_BASE,UNAME='RANGE_LABEL_BASE',/ROW,XPAD=90)
		RANGE_LABEL = Widget_Label(RANGE_LABEL_BASE,UNAME='RANGE_LABEL',VALUE='选择输出文件范围',/ALIGN_CENTER)

	RANGE_OPTIONS_BASE = Widget_Base(RANGE_BASE,UNAME='WID_BASE_OPTIONS',/COLUMN,/EXCLUSIVE)

		UNION_BUTTON = Widget_Button(RANGE_OPTIONS_BASE,UNAME='UNION_BUTTON',/ALIGN_LEFT  $
			,VALUE='并集: 包含所有文件的范围')
		INTERSEACTION_BUTTON = Widget_Button(RANGE_OPTIONS_BASE,UNAME='INTERSEACTION_BUTTON',/ALIGN_LEFT $
			,VALUE='交集: 包含文件重叠区域的范围')

	OUTPUT_BASE = Widget_Base(LAYSTK_TLB,/ROW,UNAME='OUTPUT_BASE',SPACE=0,/FRAME)

	OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',XSIZE=30 ,YSIZE=1,TITLE='输出影像',NOEDIT=1)

	OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

	pfinfo = {field_id:OUTPUT_FIELD, filter:'*.tif', title:'输出影像文件'}
	widget_control,OUTPUT_BUTTON,set_uvalue=pfinfo,/NO_COPY

	CMD_BASE = Widget_Base(LAYSTK_TLB,/ROW,UNAME='CMD_BASE',XPAD=9,SPACE=40,/FRAME,/BASE_ALIGN_CENTER)

	CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='运行',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
	HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
	EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	WIDGET_CONTROL,INTERSEACTION_BUTTON,/SET_BUTTON

	WINFO = {	LAYSTK_TLB	:	LAYSTK_TLB , $
					INPUT_LIST	:	INPUT_LIST , $
					FILE_LIST	:	PTR_NEW(/NO_COPY) , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					OPTION	:	1 $
				}

	PSTATE = PTR_NEW(WINFO,/NO_COPY)

	Widget_Control,LAYSTK_TLB,set_uvalue=PSTATE

	Widget_Control,LAYSTK_TLB,/REALIZE

	XManager, 'LAYER_STACKING',LAYSTK_TLB,/NO_BLOCK

END