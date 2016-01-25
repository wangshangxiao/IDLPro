;植被指数计算模块
;钮立明
pro WID_BASE_VI_event, Event

	on_error,2

  WIDGET_CONTROL, event.top, GET_UVALUE=PSTATE
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_INPUT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE,Event
        common_log,'设置输入文件名'

        widget_control, (*PSTATE).input, get_value = inputfile
        if file_test(inputfile) ne 0 then begin

		     result=query_image(inputfile,channels=Bandnum)
		     Bandlist=strtrim(string(indgen(Bandnum)+1),2)

		     widget_control,(*PSTATE).WID_DROPLIST_blue,set_value=Bandlist
		     widget_control,(*PSTATE).WID_DROPLIST_green,set_value=Bandlist
		     widget_control,(*PSTATE).WID_DROPLIST_red,set_value=Bandlist
		     widget_control,(*PSTATE).WID_DROPLIST_nir,set_value=Bandlist

  			endif
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_OUTPUT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE_PATH,Event
		  common_log,'设置输出文件名'
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_RVI'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (*PSTATE).RVI_Status=Event.select
        widget_control,event.top,set_uvalue=PSTATE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_NDVI'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (*PSTATE).NDVI_Status=Event.select
        widget_control,event.top,set_uvalue=PSTATE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EVI'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (*PSTATE).EVI_Status=Event.select
        widget_control,event.top,set_uvalue=PSTATE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SAVI'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (*PSTATE).SAVI_Status=Event.select
        widget_control,event.top,set_uvalue=PSTATE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_VARI'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        (*PSTATE).VARI_Status=Event.select
        widget_control,event.top,set_uvalue=PSTATE
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_CAL'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        common_log,'开始运行'
        VI_CALCULATION, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_HELP'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
      	common_log,'打开帮助文档'
      	if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP,'指数计算',BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		endif
    end

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXIT'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        common_log,'退出植被指数计算程序'
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro WID_BASE_VI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  WID_TLB = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_TLB'  $
      ,XOFFSET=w_xoffset ,YOFFSET=w_yoffset    $
      ,TITLE='植被指数计算' ,SPACE=2 ,XPAD=2 ,YPAD=2,/COLUMN  $
      ,TLB_FRAME_ATTR=1)

  WID_BASE_FILE = Widget_Base(WID_TLB,/FRAME,/COLUMN)

  WID_BASE_INPUT = Widget_Base(WID_BASE_FILE,/ROW,/BASE_ALIGN_CENTER)

;  WID_LABEL_INPUT = Widget_Label(WID_BASE_INPUT, UNAME='WID_LABEL_INPUT'  $
;      ,XOFFSET=13 ,YOFFSET=18 ,SCR_XSIZE=50 ,SCR_YSIZE=20  $
;      ,/ALIGN_LEFT ,VALUE='输入数据')

  WID_TEXT_INPUT = CW_FIELD(WID_BASE_INPUT, UNAME='WID_TEXT_INPUT'  $
      ,TITLE='输入数据' ,XSIZE=25 ,YSIZE=1,NOEDIT=1)

  WID_BUTTON_INPUT = Widget_Button(WID_BASE_INPUT,  $
      UNAME='WID_BUTTON_INPUT' ,XOFFSET=270 ,YOFFSET=12 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_input, filter:'*.tif', title:'输入影像文件'}
  widget_control,WID_BUTTON_input, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_OUTPUT = Widget_Base(WID_BASE_FILE,/ROW,/BASE_ALIGN_CENTER)

;  WID_LABEL_OUTPUT = Widget_Label(WID_BASE_OUTPUT,  $
;      UNAME='WID_LABEL_OUTPUT' ,XOFFSET=13 ,YOFFSET=58 ,SCR_XSIZE=50  $
;      ,SCR_YSIZE=20 ,/ALIGN_LEFT ,VALUE='输出数据')

  WID_TEXT_OUTPUT = CW_FIELD(WID_BASE_OUTPUT, UNAME='WID_TEXT_OUTPUT'  $
      ,TITLE='输出数据',XSIZE=25 ,YSIZE=1,NOEDIT=1)

  WID_BUTTON_OUTPUT = Widget_Button(WID_BASE_OUTPUT,  $
      UNAME='WID_BUTTON_OUTPUT' ,XOFFSET=270 ,YOFFSET=52  $
      ,SCR_XSIZE=45 ,SCR_YSIZE=25 ,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:WID_TEXT_output, title:'输出影像文件夹'}
  widget_control,WID_BUTTON_output, set_uvalue=pfinfo,/NO_COPY

  WID_BASE_PARA = Widget_Base(WID_TLB,/FRAME,/ROW,XPAD=10,SPACE=20)


  WID_BASE_setting = Widget_Base(WID_BASE_PARA,  $
      UNAME='WID_BASE_setting' ,FRAME=0 ,XOFFSET=12 ,YOFFSET=90  $
      ,SCR_XSIZE=135 ,SCR_YSIZE=158 ,TITLE='IDL' ,SPACE=5 ,XPAD=3  $
      ,YPAD=3 ,COLUMN=1)


  WID_LABEL_setting = Widget_Label(WID_BASE_setting,  $
      UNAME='WID_LABEL_setting' ,SCR_XSIZE=58 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='波段设置',FONT='宋体*bold*12')


  WID_BASE_blue = Widget_Base(WID_BASE_setting, UNAME='WID_BASE_blue'  $
      ,SCR_XSIZE=132 ,SCR_YSIZE=23 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)

  Bandnum=1L
  Bandlist=strtrim(string(indgen(Bandnum)+1),2)

  WID_LABEL_blue = Widget_Label(WID_BASE_blue, UNAME='WID_LABEL_blue'  $
      ,YOFFSET=4 ,SCR_XSIZE=62 ,SCR_YSIZE=21 ,/ALIGN_LEFT  $
      ,VALUE='蓝光波段')


  WID_DROPLIST_blue = Widget_Droplist(WID_BASE_blue,  $
      UNAME='WID_DROPLIST_blue' ,XOFFSET=72 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=21 ,VALUE=Bandlist)


  WID_BASE_green = Widget_Base(WID_BASE_setting,  $
      UNAME='WID_BASE_green' ,SCR_XSIZE=132 ,SCR_YSIZE=23  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_green = Widget_Label(WID_BASE_green,  $
      UNAME='WID_LABEL_green' ,YOFFSET=4 ,SCR_XSIZE=62 ,SCR_YSIZE=21  $
      ,/ALIGN_LEFT ,VALUE='绿光波段')


  WID_DROPLIST_green = Widget_Droplist(WID_BASE_green,  $
      UNAME='WID_DROPLIST_green' ,XOFFSET=72 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=21 ,VALUE=Bandlist)


  WID_BASE_red = Widget_Base(WID_BASE_setting, UNAME='WID_BASE_red'  $
      ,SCR_XSIZE=132 ,SCR_YSIZE=23 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  WID_LABEL_red = Widget_Label(WID_BASE_red, UNAME='WID_LABEL_red'  $
      ,YOFFSET=4 ,SCR_XSIZE=62 ,SCR_YSIZE=21 ,/ALIGN_LEFT  $
      ,VALUE='红光波段')


  WID_DROPLIST_red = Widget_Droplist(WID_BASE_red,  $
      UNAME='WID_DROPLIST_red' ,XOFFSET=72 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=21 ,VALUE=Bandlist)


  WID_BASE_nir = Widget_Base(WID_BASE_setting, UNAME='WID_BASE_nir'  $
      ,SCR_XSIZE=132 ,SCR_YSIZE=23 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  WID_LABEL_nir = Widget_Label(WID_BASE_nir, UNAME='WID_LABEL_nir'  $
      ,YOFFSET=4 ,SCR_XSIZE=62 ,SCR_YSIZE=21 ,/ALIGN_LEFT  $
      ,VALUE='近红外波段')


  WID_DROPLIST_nir = Widget_Droplist(WID_BASE_nir,  $
      UNAME='WID_DROPLIST_nir' ,XOFFSET=72 ,SCR_XSIZE=45  $
      ,SCR_YSIZE=21 ,VALUE=Bandlist)


  WID_BASE_choose = Widget_Base(WID_BASE_PARA, UNAME='WID_BASE_choose'  $
      ,FRAME=0 ,TITLE='IDL' ,XPAD=3 ,YPAD=1 ,COLUMN=1,SPACE=0)


  WID_LABEL_choose = Widget_Label(WID_BASE_choose,  $
      UNAME='WID_LABEL_choose'  $
      ,/ALIGN_CENTER ,VALUE='植被指数类型',FONT='宋体*bold*12')


  WID_BASE_RVI = Widget_Base(WID_BASE_choose, UNAME='WID_BASE_RVI'  $
      ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)


  WID_BUTTON_RVI = Widget_Button(WID_BASE_RVI, UNAME='WID_BUTTON_RVI'  $
      ,SCR_XSIZE=49 ,SCR_YSIZE=22 ,/ALIGN_LEFT  $
      ,TOOLTIP='比值植被指数' ,VALUE='RVI')


  WID_BASE_NDVI = Widget_Base(WID_BASE_choose, UNAME='WID_BASE_NDVI'  $
      ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)


  WID_BUTTON_NDVI = Widget_Button(WID_BASE_NDVI,  $
      UNAME='WID_BUTTON_NDVI' ,/ALIGN_LEFT ,TOOLTIP='归一化植被指数'  $
      ,VALUE='NDVI')


  WID_BASE_EVI = Widget_Base(WID_BASE_choose, UNAME='WID_BASE_EVI'  $
      ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)


  WID_BUTTON_EVI = Widget_Button(WID_BASE_EVI, UNAME='WID_BUTTON_EVI'  $
      ,/ALIGN_LEFT ,TOOLTIP='增强型植被指数' ,VALUE='EVI')


  WID_BASE_SAVI = Widget_Base(WID_BASE_choose, UNAME='WID_BASE_SAVI'  $
      ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)


  WID_BUTTON_SAVI = Widget_Button(WID_BASE_SAVI,  $
      UNAME='WID_BUTTON_SAVI' ,/ALIGN_LEFT  $
      ,TOOLTIP='土壤调整植被指数' ,VALUE='SAVI')


  WID_BASE_VARI = Widget_Base(WID_BASE_choose, UNAME='WID_BASE_VARI'  $
      ,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)


  WID_BUTTON_VARI = Widget_Button(WID_BASE_VARI,  $
      UNAME='WID_BUTTON_VARI' ,/ALIGN_LEFT  $
      ,TOOLTIP='可见光大气阻抗植被指数' ,VALUE='VARI')


;  WID_BASE_OPERATION = Widget_Base(WID_TLB,  $
;      UNAME='WID_BASE_OPERATION' ,XOFFSET=249 ,YOFFSET=92  $
;      ,SCR_XSIZE=74 ,SCR_YSIZE=150 ,TITLE='IDL' ,SPACE=15 ,XPAD=3  $
;      ,YPAD=15 ,COLUMN=1)

  WID_BASE_CMD = Widget_Base(WID_TLB,/FRAME,/ROW,XPAD=10,SPACE=30)

  WID_BUTTON_CAL = Widget_Button(WID_BASE_CMD,  $
      UNAME='WID_BUTTON_CAL' ,SCR_XSIZE=64 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='计算')


  WID_BUTTON_HELP = Widget_Button(WID_BASE_CMD,  $
      UNAME='WID_BUTTON_HELP' ,SCR_XSIZE=64 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='帮助')


  WID_BUTTON_EXIT = Widget_Button(WID_BASE_CMD,  $
      UNAME='WID_BUTTON_EXIT' ,SCR_XSIZE=64 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='退出')

  Widget_Control, /REALIZE, WID_TLB

  winfo = {input			:	WID_TEXT_input, $
  		     output			:	WID_TEXT_output, $
  		     RVI_Status	:	0, $
  		     NDVI_Status	:	0, $
  		     EVI_Status	:	0, $
  		     SAVI_Status	:	0,	$
  		     VARI_Status	:	0,	$
  		     WID_DROPLIST_blue	:	WID_DROPLIST_blue, $
  		     WID_DROPLIST_green	:	WID_DROPLIST_green, $
  		     WID_DROPLIST_red	:	WID_DROPLIST_red, $
  		     WID_DROPLIST_nir	:	WID_DROPLIST_nir $
  		     }

  PSTATE=PTR_NEW(winfo,/no_copy)
  widget_control, WID_TLB, set_uvalue=PSTATE,/NO_COPY

  XManager, 'WID_BASE_VI', WID_TLB, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro AGM_VI_Calculation, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  common_log,'打开植被指数计算程序'
  WID_BASE_VI, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
