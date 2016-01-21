pro readDroughtLevelcode,data=data
  DATA$ = '.\text\DroughtLevelCode.txt'

  array = strarr(file_lines(DATA$))

  OPENR,lun,DATA$,/GET_LUN

  READF, lun, array

  FREE_LUN, lun

  data = strarr(3,n_elements(array))

  ;print,n_elements(array)
  for i=0,n_elements(array)-1 do begin

    data[*,i] = STRSPLIT(reform(array[i]), ',', /EXTRACT)
    ;print,data[*,i]
  endfor
  ;print,data
end
pro getDroughtTypeByCode,LevelCode,DroughtType=DroughtType
  
  readDroughtLevelcode,data=data
  
  index=where(data[0,*] eq LevelCode)
  ;print,index
  DroughtType=data[1,[index]]
  ;print,CropName
END
pro getpercentByCode,LevelCode,lostpercent=lostpercent
  
  readDroughtLevelcode,data=data
  
  index=where(data[0,*] eq LevelCode)
  ;print,index
  lostpercent=data[2,[index]]
END
PRO CalculteYieldLostForDrought, file1,file2,DataArray=DataArray
    
    ;file1='D:\hunan_sub\droughtlevel\syn20130703_rsi_test.tif';����
    ;file2='D:\hunan_sub\recode\croptype.tif';����
    
        COMPILE_OPT idl2

    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    ENVI_OPEN_FILE,file2,R_FID=fid
    IF fid EQ 0 THEN BEGIN
        OPENW,lun,errorFile,/GET_LUN,/APPEND
        PRINTF,lun,'File cannot be opened. File name:',file2
        FREE_LUN,lun
    ENDIF
    mapinfo=ENVI_GET_MAP_INFO(FID=fid)
    PixelSize=mapinfo.ps[0:1]
    PixelArea=double(PixelSize[0]*PixelSize[1])
    ;print,PixelArea
    
    background=0;���������ʹ���
    data1=READ_TIFF(file1,GEOTIFF=geotiff);����
    data2=READ_TIFF(file2,GEOTIFF=geotiff);����
    uniqType=data2[uniq(data2,SORT(data2))];����
    ;print,uniqType
    uniqLevel=data1[uniq(data1,SORT(data1))];����
    index=WHERE(uniqType NE background,count);������Ԫ������
    IF count NE 0 THEN uniqType=uniqType[index];����
    
    
    Levelindex=WHERE(uniqLevel NE background,Levelcount);������Ԫ������
    IF Levelcount NE 0 THEN uniqLevel=uniqLevel[Levelindex];����
    
    
    N_Type=N_ELEMENTS(uniqType)
    N_Level=N_ELEMENTS(uniqLevel)
    StatisArray=DBLARR(N_Type,N_Level)
    FOR iType=0,N_Type-1 DO BEGIN
        typecode=uniqType[iType]
        index=WHERE(data2 EQ typecode,count)
        IF count NE 0 THEN BEGIN
            tempDestroy=data1[index]
            FOR iLevel=0,N_Level-1 DO BEGIN
                index=WHERE(tempDestroy EQ uniqLevel[iLevel],count)
                StatisArray[iType,iLevel]=count
            ENDFOR
        ENDIF
    ENDFOR
    ;PRINT,StatisArray
    
    AreaArray=PixelArea*StatisArray
    N_AreaArray=N_ELEMENTS(AreaArray)
    ;print,N_AreaArray
    DataArray=[0,0,0,0]
    FOR iType=0,N_Type-1 DO BEGIN
        ;PRINT,'���ͱ���:',uniqType[iType]
        
        FOR iLevel=0,N_Level-1 DO BEGIN
            ;PRINT,'���ͱ���:',uniqType[iType],'�ɺ�����:',uniqLevel[iLevel],'��Ԫ������',StatisArray[iType,iLevel],'�����',AreaArray[iType,iLevel]
            DataLine=[uniqType[iType],uniqLevel[iLevel],StatisArray[iType,iLevel],AreaArray[iType,iLevel]]
            DataArray=[[DataArray],[DataLine]]
        ENDFOR
    ENDFOR
END

Pro DoLostStatistic_Event,EVENT
    WIDGET_CONTROL,Event.top,GET_UVALUE=PSTATE
    View = Widget_Info( EVENT.ID,/TABLE_VIEW)
    WIDGET_CONTROL,(*PSTATE).WID_SHL_table,SET_TABLE_VIEW=View,SET_TABLE_SELECT=[-1,-1]
    Widget_Control,(*PSTATE).INPUT_FIELD,get_value=FILE
    ;print,File
      if FILE EQ '' then return

      if file_test(FILE) eq 0 then begin
        CAUTION = dialog_message('�����ļ�������!',title='����')
         return
      endif

      if query_tiff(FILE) eq 0 then begin
        CAUTION = dialog_message('������������ļ�������ȷ��TIFF�ļ�!',title='����')
         return
      endif
      ;-----------------���뺵���ļ�-------------------------------------------------------
      Widget_Control,(*PSTATE).INPUT_DROUGHT,get_value=DroughtLevelFile
      
      if file_test(DroughtLevelFile) eq 0 then begin
        CAUTION = dialog_message('����ĺ���ȼ��ļ�δ����!',title='����')
        return
      endif
  ;----------------------------------------------------------------------------------
      
    CalculteYieldLostForDrought,DroughtLevelFile,FILE,DataArray=DataArray
    
    ;print,DataArray
    ;index=where((DataArray[0,*] ne 0) and (DataArray[3,*] ne 0.00000000));
    index=where(DataArray[3,*] ne 0.00000000); 
    ;print,index
    
    
    CropCodeArray=DataArray[0,[index]]
    ;print,CropCodeArray
    DroughtLevelArray=DataArray[1,[index]]
    ;print,DroughtLevelArray
    DroughtLevelArray = strtrim(string(DroughtLevelArray,format='(I)'),2)
    
    if n_elements(DroughtLevelArray) GT 1 then begin
      DroughtLevelArray=transpose(DroughtLevelArray)
    endif
    ;print,DroughtLevelArray
    AreaArray=DataArray[3,[index]]
    AreaArray=float(AreaArray/666.7);�����Ķ
    ;AreaArray = strtrim(string(AreaArray,format='(I)'),2)
    ;AreaArray=transpose(AreaArray)
    ;print,AreaArray
    ;
    CropCodeArray = strtrim(string(CropCodeArray,format='(I)'),2)
    
    if n_elements(CropCodeArray) GT 1 then begin
      CropCodeArray=transpose(CropCodeArray)
    endif
    
    ;print,CropCodeArray
    
    row = n_elements(AreaArray)
    ;print,row
    cropnames=['x']
    yields=[1000]
    
    ;lostyield=[1000]
    for i=0,row-1 do begin
      getCropByCode,CropCodeArray[i],CropName=CropName
      getYieldByCode,CropCodeArray[i],CropYield=CropYield
      
      cropnames=[[cropnames],[cropname]]
      yields=[[yields],[CropYield]]
      
    endfor
    lostpercents=[0.01]
    N_Level=n_elements(DroughtLevelArray)
    for j=0,N_Level-1 do begin
      getpercentByCode,DroughtLevelArray[j],lostpercent=lostpercent
      lostpercents=[[lostpercents],[lostpercent]]
    endfor
    
    cropnames=cropnames[1:*]
    yields=yields[1:*]
    lostpercents=lostpercents[1:*]
;    percent=lostpercents(where(lostpercents eq 1))
    ;print,lostpercents
    
    ;lostpercents=strtrim(string(lostpercents,format='(f20.4)'))
    if n_elements(lostpercents) GT 1 then begin
      lostpercents=transpose(lostpercents)
    endif
    
    
    yields=strtrim(string(yields,format='(f20.4)'))
    if n_elements(yields) GT 1 then begin
      yields=transpose(yields)
    endif
    
    ;-----------��ʧ����=����ƽ������*��ʧ����----------------------------------------------------
    LostYields=yields*lostpercents
    LostYields=strtrim(string(LostYields,format='(f20.4)'))
    if n_elements(LostYields) GT 1 then begin
      LostYields=transpose(LostYields)
    endif

    TotalProduction=AreaArray*LostYields/10000
    TotalProduction=strtrim(string(TotalProduction,format='(f20.4)'))
    if n_elements(TotalProduction) GT 1 then begin
      TotalProduction=transpose(TotalProduction)
    endif
    
    
    AreaArray = strtrim(string(AreaArray,format='(f20.4)'))
    if n_elements(AreaArray) GT 1 then begin
      AreaArray=transpose(AreaArray)
    endif
    
    if n_elements(cropnames) GT 1 then begin
      cropnames=transpose(cropnames)
    endif
    
    
    ;�������٣����һ������ټ�������ʾ
;    if row eq 0 then begin
;      AreaArray_new=[[AreaArray_new],[0]]
;      yields=[[yields],[0]]
;      TotalProduction=[[TotalProduction],[0]]
;      cropnames=[[cropnames],['�������']]
;      row=row+1
;    endif
;    print,AreaArray_new
;    print,yields
;    print,DamageFlag
    
    table_value_new=[DroughtLevelArray,AreaArray,LostYields,TotalProduction]
    
    ;print,table_value_new
    Widget_Control,(*PSTATE).WID_SHL_table,TABLE_YSIZE=row,ROW_LABELS=cropnames
    Widget_Control,(*PSTATE).WID_SHL_table,SET_VALUE=table_value_new,ALIGNMENT=1;SET_TABLE_SELECT=[-1,3]
END


PRO CropLostStatistic_EVENT,EVENT

  on_error,2

  Widget_Control,Event.top,get_uvalue=PSTATE

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
    widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE,event
      common_log,'������Ӱ��'
      
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='INPUT_DROUGHT_BUTTON'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        PICK_FILE,event
      common_log,'�򿪺���ȼ��ļ�'
    end
    
    Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        DoLostStatistic_Event,event
      common_log,'��ټ���'
    end

;    Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
;      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
;        common_log,'�˳�����'
;      PTR_FREE,PSTATE
;      HEAP_GC, /VERBOSE
;      widget_control,event.top,/DESTROY
;    end
  else :
endcase

IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  common_log,'�˳�����'
  PTR_FREE,PSTATE
  HEAP_GC, /VERBOSE
  widget_control,event.top,/DESTROY
ENDIF
END

pro CropLostStatistic, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  scr_dims = GET_SCREEN_SIZE()
  w_xoffset=(scr_dims[0]-350)/2
  w_yoffset=(scr_dims[1]-350)/2

  SHGS = Widget_Base(GROUP_LEADER=wGroup,/COLUMN,UNAME='SHGS' ,SPACE=2,TITLE='���������', $
    XPAD=2,YPAD=2,/TLB_KILL_REQUEST_EVENTS,XOFFSET=w_xoffset,YOFFSET=w_yoffset,TLB_FRAME_ATTR=1)

  FILE_BASE = Widget_Base(SHGS,/COLUMN,/FRAME,UNAME='FILE_BASE',SPACE=2,XPAD=1,YPAD=1,/BASE_ALIGN_CENTER)

  INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)

  INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',XSIZE=80 ,YSIZE=1,TITLE='������������ļ�  ',NOEDIT=1,VALUE='')
  INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)

  pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'������������ļ�'}
  widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY
;------------------------------------------------���뺵��ȼ��ļ�-------------------------------------------------------------------------
  INPUT_DROUGHT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_DROUGHT_BASE',SPACE=1,XPAD=0,YPAD=0,/BASE_ALIGN_CENTER)
  
  INPUT_DROUGHT = CW_FIELD(INPUT_DROUGHT_BASE,UNAME='INPUT_DROUGHT',XSIZE=80 ,YSIZE=1,TITLE='���뺵��ȼ��ļ�  ',NOEDIT=1,VALUE='')
  INPUT_DROUGHT_BUTTON = Widget_Button(INPUT_DROUGHT_BASE,UNAME='INPUT_DROUGHT_BUTTON',SCR_XSIZE=36,SCR_YSIZE=25,/ALIGN_CENTER ,VALUE='.\Image\Open.bmp',/BITMAP)
  
  pfinfo = {field_id:INPUT_DROUGHT, filter:'*.tif', title:'���뺵��ȼ��ļ�'}
  widget_control,INPUT_DROUGHT_BUTTON, set_uvalue=pfinfo,/NO_COPY
  ;-----------------------------------------------------------------------------------------------------------------------------------


   ;------------�����һ��BASE--�ĸ�DROPlist���ڵ�BASE----------------------------------;

  CMD_BASE = Widget_Base(SHGS,/ROW,UNAME='CMD_BASE',XPAD=9,SPACE=40,/FRAME,/BASE_ALIGN_CENTER)

  CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='������ʧ������',UNAME='CAL_BUTTON',XOFFSET=10,SCR_XSIZE=130,SCR_YSIZE=25)
  ;EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='�˳�',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
  Lable=WIDGET_LABEL(CMD_BASE,VALUE='����ȼ�:1-������2-�ẵ��3-�к���4-�غ���5-����',UNAME='Lable_LEL',XOFFSET=10,SCR_XSIZE=320,SCR_YSIZE=25)
  WID_BASE_SHL = Widget_Base(SHGS, UNAME='WID_BASE_SHL' ,FRAME=1  $
    ,XOFFSET=0 ,YOFFSET=40 ,SCR_XSIZE=920 ,SCR_YSIZE=240  $
    ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

 columnlabel=['����ȼ�','���(Ķ)','������ʧ(����/Ķ)','��ʧ����(�򹫽�)']

 WID_SHL_table = Widget_Table(WID_BASE_SHL, UNAME='WID_SHL_table'  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=920 ,SCR_YSIZE=240 ,XSIZE=4  $
      ,YSIZE=10,COLUMN_WIDTHS=185, ROW_HEIGHTS=25,COLUMN_LABELS=columnlabel,$
      /DISJOINT_SELECTION,/EDITABLE,EVENT_PRO='DoLostStatistic_Event',/ALL_EVENTS)

  Widget_Control,SHGS,/REALIZE

  winfo = { INPUT_FIELD : INPUT_FIELD, $
    INPUT_DROUGHT:INPUT_DROUGHT,$
    WID_SHL_table : WID_SHL_table}

  PSTATE = PTR_NEW(winfo,/NO_COPY)

  Widget_Control,SHGS,SET_UVALUE=PSTATE

  XManager, 'CropLostStatistic',SHGS,/NO_BLOCK
end