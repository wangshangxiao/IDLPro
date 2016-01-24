;+
;:Description:
;    Describe the procedure.
;
; Author: DYQ 2009-3-5;
;
;-

PRO Mutifileregister_event,event
  COMPILE_OPT idl2
  WIDGET_CONTROL,event.top, get_UValue = pState

  ;关闭事件
  IF TAG_NAMES(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    ;
    status = DIALOG_MESSAGE('关闭?',/Question)
    IF status EQ 'No' THEN RETURN
    ;关闭ENVI
    ;
    PTR_FREE, pState
    WIDGET_CONTROL, event.top,/Destroy
    RETURN;
  ENDIF

  uName = WIDGET_INFO(event.id,/uName)
  ;
  CASE uname OF
    ;打开文件
    'open': BEGIN
      files = DIALOG_PICKFILE(/MULTIPLE_FILES)
      IF N_ELEMENTS(files) EQ 0 THEN RETURN
      ;设置显示文件
      WIDGET_CONTROL, (*pState).Wlist, set_value = files
      (*pState).inputFiles = Ptr_New(files)

    END
    ;
       ;选择
    'filepathsele': BEGIN
      ;
      WIDGET_CONTROL, event.id,get_value = value
      WIDGET_CONTROL,(*pState).wSele, Sensitive= value
      WIDGET_CONTROL,(*pState).outPath, Sensitive= value

    END

    'selePath' : BEGIN
      outroot = DIALOG_PICKFILE(/dire)
      WIDGET_CONTROL,(*pState).outPath,set_value = outRoot
    END

    ;重采样
    'warp': BEGIN

      ;获取选择的方法
      WIDGET_CONTROL,(*pState).bgroup, get_Value = mValue

      ;获取文件名
      files = *((*pState).inputFiles)
      ;
      progressbar = Obj_New('progressbar',$
                             xsize=300,$
                             ysize=30,$
                              Color='red', Text='Loop Iteration 0')

         ; Place the progress bar on the display.

      progressbar -> Start
      per = 100./N_ELEMENTS(files)

      FOR i=0,N_ELEMENTS(files)-1 DO BEGIN
        ;
        progressbar -> Update, 1, Text='处理中...'
        ENVI_OPEN_FILE, files[i], r_fid=fid
        ;构建输出文件名
        Widget_Control, (*pState).outPath,get_value= outfiledir

        out_name = outfiledir+file_baseName(files[i])

        IF (fid EQ -1) THEN BEGIN
          tmp = DIALOG_MESSAGE(files[i]+'文件读取错误',/error)
          CONTINUE
        ENDIF
        ;文件信息
        ENVI_FILE_QUERY, fid, dims=dims, nb=nb, $
            nl = nl,ns = ns, bnames = bnames, $
            DATA_TYPE = DATA_TYPE
        MAP_INFO = ENVI_GET_MAP_INFO(fid = fid)
        ;  如果波段小于3个则提示信息并返回
        IF nb LE 3 THEN BEGIN
            msg = DIALOG_MESSAGE('波段数少于3，无法进行RGB合成' )
            RETURN
  ;
        ENDIF ELSE BEGIN

            bandList = [3,2,1]
            ;
            progressbar -> Update, per*i, Text='处理中...'+StrTrim(String(per*i),2)+'%'

            openW,lun,out_name,/get_lun , /APPEND
            FOR ii=0,n_elements(bandlist)-1 do begin
                data = ENVI_GET_DATA(fid=fid, dims=dims, pos=bandlist[ii])
                writeu,lun,data
            endfor
            free_lun,lun
        envi_file_mng, id=fid, /remove


        ENVI_SETUP_HEAD, fname=out_name, $
            nl = nl,ns = ns, nb=3, $
            /write, $
            DATA_TYPE = DATA_TYPE,INTERLEAVE  =0, $
            MAP_INFO = MAP_INFO
        ENDELSE  ;

      ENDFOR
      progressbar -> Destroy


    END
    ELSE:
  ENDCASE

END
;
;--------------------------
;
;
PRO fileBandMix
  ;
  COMPILE_OPT idl2
  ;初始化组件大小
  sz = [600,400]
  ;
  tlb = WIDGET_BASE(MBAR= mBar, $
    /COLUMN , $
    title = ' 批处理波段RGB合成', $
    Space = 0 , $
    XPad = 0, $
    YPad = 0 , $
    /Tlb_Kill_Request_Events, $
    tlb_frame_attr = 1, $
    Map = 0)

  ;创建菜单
  fMenu = WIDGET_BUTTON(mBar, value ='文件',/Menu)
  fExit = WIDGET_BUTTON(fMenu, value = '退出', $
    uName = 'exit',/Sep)
  hMenu =  WIDGET_BUTTON(mBar, value ='帮助',/Menu)
  hHelp = WIDGET_BUTTON(hmenu, value = '关于', $
    uName = 'about',/Sep)
  ;
  ;上面的输入base
  wInputBase = WIDGET_BASE(tlb, $
    xSize =sz[0], $
    /Column)
  ;下面的输出base
  wOutputBase = WIDGET_BASE(tlb, $
    /Row,$
    xSize =sz[0])

  ;中间的参数base
  wSetBase = WIDGET_BASE(tlb, $
    xSize =sz[0], $
    /Row)

  ;-----------------------
  wButton = WIDGET_BUTTON(wInputBase,value ='打开数据文件', $
    uName = 'open')
  wList = WIDGET_LIST(wInputBase, $
    YSize = sz[1]/(2*15),$
    XSize = sz[0]/8)
  ;
  ;-----------------------
  values = ['源文件路径', $
    '另选择路径']
  bgroup = Cw_bgroup(wOutputBase, values, $
    /ROW, /EXCLUSIVE, $
    ;LABEL_TOP='输出路径', $
    /No_Release, $
    SET_VALUE=1, $
    uName = 'filepathsele', $
    /FRAME)

  outPath = WIDGET_TEXT(wOutputBase, $
    value =' ', $
    xSize =35, $
    /Editable, $
    uName = 'outroot')
  wSele = WIDGET_BUTTON(wOutputBase, $
    value ='选择路径', $
    uName ='selePath')


  wButton = WIDGET_BUTTON(wSetBase,$
    value ='波段合成', $
    uName = 'warp')
  ;-----------------------------

  inputFiles = Ptr_New()

  state = {wButton:wButton, $
    tlb : tlb, $
    outPath: outPath, $
    wSele : wSele, $
    bgroup : bgroup , $
    inputFiles : inputFiles, $

    wList : wList }

  pState = PTR_NEW(state)
  ;
  Centertlb,tlb
  WIDGET_CONTROL, tlb,/Realize,/Map,set_uValue = pState
  ;
  ;初始化ENVI
  ;
  ENVI, /restore_base_save_files  ;
  ENVI_BATCH_INIT

  Xmanager,'MutifileRegister',tlb,/No_Block
END
