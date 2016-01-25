;
; Generated on:	11/30/2009 17:11.31
;

PRO LAYER_STACK,event

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
		return
  ENDIF

  group_id=event.top
  widget_control, group_id, get_uvalue=winfo
  widget_control, winfo.input, get_value = inputfile
  widget_control, winfo.output, get_value = outputfile

  ;
  ; First restore all the base save files.
  ;
  envi, /restore_base_save_files
  ;
  ; Initialize ENVI and send all errors
  ; and warnings to the file batch.txt
  ;
  envi_batch_init, log_file='batch.txt'
  ;
  ; Open the first input file.
  ; We will also open the one band
  ; dem file to layer stack with
  ; this file.
  ;

  options=winfo.options
  if file_test(inputfile) eq 0 then begin
		Result = DIALOG_MESSAGE('请指定数据所在文件夹!', /CENTER)
		return
  endif

  if strcompress(file_basename(outputfile, '.tif'), /REMOVE_ALL) eq '' then begin
		Result = DIALOG_MESSAGE('输出文件名称不正确！' + outputfile, /CENTER)
		return
  endif

  tempfile=strsplit(outputfile,'.',/extract)
  outputfile=tempfile[0]+'.tif'

  file_path=inputfile
  fname=file_search(file_path,'*.tif')
  fnum=n_elements(fname)


  if fnum lt 2 then begin
	Result = DIALOG_MESSAGE('至少需要2个输入数据', /CENTER)
	return
  endif

;  if query_tiff(inputfile) eq 0 then begin
;		Result = DIALOG_MESSAGE('数据文件格式错误！', /CENTER)
;		return
;  endif

  if strcompress(file_basename(outputfile, '.tif'), /REMOVE_ALL) eq '' then begin
		Result = DIALOG_MESSAGE('输出文件名称不正确！' + outputfile, /CENTER)
		return
  endif


  t_fid=lonarr(fnum)
  t_ns=lonarr(fnum)
  t_nl=lonarr(fnum)
  t_nb=lonarr(fnum)

  compile_opt strictarr

  for i=0,fnum-1,1 do begin

	  envi_open_file, fname[i], r_fid=d_fid
	  if (t_fid[i] eq -1) then begin
	    envi_batch_exit
	    return
	  endif
 	  t_fid[i]=d_fid
	  ;
	  ; Use all the bands from both files
	  ; and all spatial pixels. First build the
	  ; array of FID, POS and DIMS for both
	  ; files.
	  ;
	  envi_file_query, t_fid[i],ns=d_ns, nl=d_nl, nb=d_nb
  	  t_ns[i]=d_ns
  	  t_nl[i]=d_nl
  	  t_nb[i]=d_nb
  endfor
  ;
  nb = long(total(t_nb))
  fid = lonarr(nb)
  pos = lonarr(nb)
  dims = lonarr(5,nb)
  ;
  for i=0L,nb-1 do begin
    fid[i] = t_fid[i]
    pos[i] = 0
    dims[0,i] = [-1,0,t_ns[i]-1,0,t_nl[i]-1]
  endfor

  out_proj = envi_get_projection( fid=t_fid[0], pixel_size=out_ps)
  out_name = outputfile
  out_dt = 4
  ;
  ; Call the layer stacking routine. Do not
  ; set the exclusive keyword allow for an
  ; inclusive result. Use cubic convolution
  ; for the interpolation method.
  ;

  envi_doit, 'envi_layer_stacking_doit', $
    fid=fid, pos=pos, dims=dims, exclusive=options, $
    out_dt=out_dt, out_name=out_name, $
    interp=2, out_ps=out_ps, $
    out_proj=out_proj, r_fid=r_fid
  ;
  ; Exit ENVI
  ;
  	envi_file_mng, id=d_fid, /remove
	envi_file_mng, id=fid, /remove
	envi_file_mng, id=r_fid, /remove
	envi_file_mng, id=t_fid, /remove

;  envi_batch_exit
END

pro WID_BASE_LAYERSTACK_event, Event

  WIDGET_CONTROL, event.top, GET_UVALUE=PSTATE
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_INPUT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE_PATH, Event
        common_log,'设置输入文件路径'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_OUTPUT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE, Event
        common_log,'设置输出文件路径'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_UNION'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (PSTATE).OPTIONS=0
        WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
        common_log,'设置输出影像取各影像并集'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_INTERSEACTION'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (PSTATE).OPTIONS=1
        WIDGET_CONTROL, event.top, SET_UVALUE=PSTATE,/NO_COPY
        common_log,'设置输出影像取各影像交集'
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_RUN'): begin
   if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
   common_log,'开始进行影像叠加'
   layer_stack, Event
      end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXIT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
		  common_log,'退出影像叠加程序'
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro WID_BASE_LAYERSTACK, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  WID_TLB = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='WID_TLB' ,XOFFSET=5 ,YOFFSET=5  $
      ,SCR_XSIZE=326 ,SCR_YSIZE=210 ,TITLE='影像合并' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1)


  WID_LABEL_INPUT = Widget_Label(WID_TLB,  $
      UNAME='WID_LABEL_INPUT' ,XOFFSET=15 ,YOFFSET=25 ,SCR_XSIZE=50  $
      ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='输入数据')


  WID_LABEL_OUTPUT = Widget_Label(WID_TLB,  $
      UNAME='WID_LABEL_OUTPUT' ,XOFFSET=15 ,YOFFSET=80 ,SCR_XSIZE=50  $
      ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='输出数据')


  WID_TEXT_INPUT = Widget_Text(WID_TLB,  $
      UNAME='WID_TEXT_INPUT' ,XOFFSET=70 ,YOFFSET=18 ,SCR_XSIZE=170  $
      ,SCR_YSIZE=26 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  WID_TEXT_OUTPUT = Widget_Text(WID_TLB,  $
      UNAME='WID_TEXT_OUTPUT' ,XOFFSET=70 ,YOFFSET=74 ,SCR_XSIZE=170  $
      ,SCR_YSIZE=26 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  WID_BUTTON_INPUT = Widget_Button(WID_TLB,  $
      UNAME='WID_BUTTON_INPUT' ,XOFFSET=254 ,YOFFSET=16 ,SCR_XSIZE=52  $
      ,SCR_YSIZE=30 ,/ALIGN_CENTER ,VALUE='浏览')

  pfinfo = {field_id:WID_TEXT_input, title:'输入影像文件夹'}
  widget_control,WID_BUTTON_input, set_uvalue=pfinfo,/NO_COPY


  WID_BUTTON_OUTPUT = Widget_Button(WID_TLB,  $
      UNAME='WID_BUTTON_OUTPUT' ,XOFFSET=254 ,YOFFSET=72  $
      ,SCR_XSIZE=52 ,SCR_YSIZE=30 ,/ALIGN_CENTER ,VALUE='浏览')

  pfinfo = {field_id:WID_TEXT_output, filter:'*.tif', title:'输出影像文件'}
  widget_control,WID_BUTTON_output, set_uvalue=pfinfo,/NO_COPY


  WID_LABEL_OPTIONS = Widget_Label(WID_TLB, XOFFSET=15 ,YOFFSET=120, $
      UNAME='WID_LABEL_OPTIONS', VALUE='输出范围',/ALIGN_LEFT)

  WID_BASE_OPTIONS = Widget_Base(WID_TLB,  $
      UNAME='WID_BASE_OPTIONS' ,XOFFSET=70 ,YOFFSET=113  $
      ,SCR_XSIZE=199 ,SCR_YSIZE=22 ,TITLE='影像叠加范围' ,ROW=1 ,/EXCLUSIVE)

  WID_BUTTON_UNION = Widget_Button(WID_BASE_OPTIONS,  $
      UNAME='WID_BUTTON_UNION' ,/ALIGN_LEFT ,VALUE='并集')


  WID_BUTTON_INTERSEACTION = Widget_Button(WID_BASE_OPTIONS,  $
      UNAME='WID_BUTTON_INTERSEACTION' ,/ALIGN_LEFT ,VALUE='交集')


  WID_BUTTON_RUN = Widget_Button(WID_TLB,  $
      UNAME='WID_BUTTON_RUN' ,XOFFSET=50 ,YOFFSET=148 ,SCR_XSIZE=80  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='运行')


  WID_BUTTON_EXIT = Widget_Button(WID_TLB,  $
      UNAME='WID_BUTTON_EXIT' ,XOFFSET=180 ,YOFFSET=148 ,SCR_XSIZE=80  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='退出')

  Widget_Control, /REALIZE, WID_TLB

  winfo = {input	:	WID_TEXT_input, $
  		     output	:	WID_TEXT_output, $
  		     OPTIONS	:	1}

  WIDGET_CONTROL,WID_BUTTON_INTERSEACTION,/SET_BUTTON
  widget_control, WID_TLB, set_uvalue=winfo,/NO_COPY


  XManager, 'WID_BASE_LAYERSTACK', WID_TLB, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro COMMON_LAYER_STACK, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  common_log,'打开影像叠加处理程序'
  WID_BASE_LAYERSTACK, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
