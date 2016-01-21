PRO GetFileHistForFarmDamage,file,Background=Background,errorFile=errorFile, $
PercentArray=PercentArray,ValueArray=ValueArray,AreaArray=AreaArray
    print,'file='+file
    COMPILE_OPT idl2
    ;file='D:\work\农田损毁和农作物损失评估\测试数据\Class\isotemp.tif'
    background=-1
    IF N_ELEMENTS(Background) GT 0 THEN BEGIN
        Background=FIX(Background)
    ENDIF ELSE BEGIN
        Background=0
    ENDELSE
    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    ENVI_OPEN_FILE,file,R_FID=fid
    IF fid EQ 0 THEN BEGIN
        OPENW,lun,errorFile,/GET_LUN,/APPEND
        PRINTF,lun,'File cannot be opened. File name:',file
        FREE_LUN,lun
    ENDIF
    mapinfo=ENVI_GET_MAP_INFO(FID=fid)
    PixelSize=mapinfo.ps[0:1]
    PixelArea=double(PixelSize[0]*PixelSize[1])
    ENVI_FILE_QUERY,fid,DIMS=dims
    ENVI_DOIT, 'ENVI_STATS_DOIT', COMP_FLAG=3 ,DMax=max,dmin=min,$
        DIMS=dims , FID=fid , HIST=hist, POS=0
    num_bins=N_ELEMENTS(hist)
    binSize=(DOUBLE(max)-min)/(num_bins-1)
    ValueArray=INDGEN(num_bins)*binSize[0]+min[0]
    ValueArray=ROUND(ValueArray)
    index=WHERE(hist NE 0)
    hist=hist[index]
    AreaArray=PixelArea*hist
    ValueArray=ValueArray[index]
    index=WHERE(ValueArray NE Background,count)
    IF count NE 0 THEN BEGIN
        hist=hist[index]
        ValueArray=ValueArray[index]
    ENDIF
    PercentArray=hist/TOTAL(hist)
;        FOR i=0,N_ELEMENTS(HIST)-1 DO BEGIN
;            PRINT,'Nums: ',PercentArray[i],"  value:",ValueArray[i],"  hist:",hist[i]," Area:",AreaArray[i]
;        ENDFOR
    ;ENVI_BATCH_EXIT
END

Pro DoStatistic_Event,EVENT
    WIDGET_CONTROL,Event.top,GET_UVALUE=PSTATE
    View = Widget_Info( EVENT.ID,/TABLE_VIEW)
    WIDGET_CONTROL,(*PSTATE).WID_SHL_table,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1]
    Widget_Control,(*PSTATE).INPUT_FIELD,get_value=FILE
    ;print,File
      if FILE EQ '' then return

      if file_test(FILE) eq 0 then begin
        CAUTION = dialog_message('输入文件不存在!',title='警告')
         return
      endif

      if query_tiff(FILE) eq 0 then begin
        CAUTION = dialog_message('输入文件不是正确的TIFF文件!',title='警告')
         return
      endif
      
    GetFileHistForFarmDamage,FILE,PercentArray=PercentArray,ValueArray=ValueArray,AreaArray=AreaArray
    AreaArray=float(AreaArray/6667000)
    
    ;AreaArray = strtrim(string(AreaArray,format='(I)'),2)
    AreaArray=transpose(AreaArray)
    
    ValueArray=transpose(ValueArray)
    
    table_value=[ValueArray,AreaArray]
    ;print,table_value[0,*]
    ;index=where(table_value[0,*] ne 0.000000)
    index=where(table_value[0,*] eq 1.00000)
    ValueArray_new=table_value[0,[index]]
    AreaArray_new=table_value[1,[index]]
    
    if index eq -1 then begin
      AreaArray_new[*,*]=[0]
    endif
    
    row = n_elements(AreaArray_new)
    cropnames=['农田损毁']
    table_value_new=AreaArray_new
    Widget_Control,(*PSTATE).WID_SHL_table,TABLE_YSIZE=row,ROW_LABELS=cropnames
    Widget_Control,(*PSTATE).WID_SHL_table,SET_VALUE=table_value_new,ALIGNMENT=1
END


PRO DamageStatistic_EVENT,EVENT

  on_error,2

  Widget_Control,Event.top,get_uvalue=PSTATE

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
    widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE,event
      common_log,'打开输入影像'
      
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        DoStatistic_Event,event
      common_log,'损毁计算'
    end

;    Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
;      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
;        common_log,'退出程序'
;      PTR_FREE,PSTATE
;      HEAP_GC, /VERBOSE
;      widget_control,event.top,/DESTROY
;    end
  else :
endcase

IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  common_log,'退出程序'
  PTR_FREE,PSTATE
  HEAP_GC, /VERBOSE
  widget_control,event.top,/DESTROY
ENDIF
END

pro DamageStatistic, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  SHGS = Widget_Base(GROUP_LEADER=wGroup,/COLUMN,UNAME='SHGS' ,SPACE=2,TITLE='农田损毁量估算', $
    XPAD=2,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,TLB_FRAME_ATTR=1)

  FILE_BASE = Widget_Base(SHGS,/COLUMN,/FRAME,UNAME='FILE_BASE',SPACE=2,XPAD=1,YPAD=1,/BASE_ALIGN_CENTER)

  INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)

  INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',XSIZE=50 ,YSIZE=1,TITLE='  输入损毁区域影像  ',NOEDIT=1,VALUE='')
  INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'    输入损毁区域影像'}
  widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY


   ;------------横向第一层BASE--四个DROPlist所在的BASE----------------------------------;

  CMD_BASE = Widget_Base(SHGS,/ROW,UNAME='CMD_BASE',XPAD=9,SPACE=40,/FRAME,/BASE_ALIGN_CENTER)

  CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='损毁量估算',UNAME='CAL_BUTTON',XOFFSET=10,SCR_XSIZE=100,SCR_YSIZE=25)
  ;EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

  WID_BASE_SHL = Widget_Base(SHGS, UNAME='WID_BASE_SHL' ,FRAME=1  $
    ,XOFFSET=0 ,YOFFSET=40 ,SCR_XSIZE=550 ,SCR_YSIZE=120  $
    ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

 columnlabel=['农田损毁面积(万亩)']

 WID_SHL_table = Widget_Table(WID_BASE_SHL, UNAME='WID_SHL_table'  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=550 ,SCR_YSIZE=100 ,XSIZE=1  $
      ,YSIZE=1,COLUMN_WIDTHS=550, ROW_HEIGHTS=25,COLUMN_LABELS=columnlabel,$
      /DISJOINT_SELECTION,/EDITABLE,EVENT_PRO='DoStatistic_Event',/ALL_EVENTS)

  Widget_Control,SHGS,/REALIZE

  winfo = { INPUT_FIELD : INPUT_FIELD, $
    WID_SHL_table : WID_SHL_table}

  PSTATE = PTR_NEW(winfo,/NO_COPY)

  Widget_Control,SHGS,SET_UVALUE=PSTATE

  XManager, 'DamageStatistic',SHGS,/NO_BLOCK
end