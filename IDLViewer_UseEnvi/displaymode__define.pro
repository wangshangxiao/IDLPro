PRO DISPLAYMODE::HandleEvent, ev

  COMPILE_OPT idl2
  
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ;Load按钮事件
  (*pState).OTREE.GETPROPERTY, FIDNOW = Fid
  IF fid LT 1 THEN RETURN
  
  CASE ((*pState).OMODE).INDEX OF
  
    ;RGB模式下
    1: BEGIN
    
      (*pState).OTREE.GETPROPERTY, FIDRGB = FidRGB
      (*pState).OTREE.GETPROPERTY, BANDRGB_IDX = BandRGB
      
      ;如果RGB三通道未全部选择，则返回
      IF TOTAL(BandRGB EQ -1) GE 1 THEN RETURN
      
      fid = FidRGB[0]
      
      ;获取RGB三通道各自的列数和行数
      ns_rgb = [0L,0L,0L]
      nl_rgb = [0L,0L,0L]
      FOR i = 0,2 DO BEGIN
        ENVI_FILE_QUERY, fidRGB[i], $
          ns = ns, $
          nl = nl, $
          dims = dims,  $
          data_type = dtype
          
        ns_rgb[i] = ns
        nl_rgb[i] = nl
      ENDFOR
      
      ;如果RGB三通道维度不同，弹出提示并返回
      IF TOTAL(ns_rgb EQ ns_rgb[0]) NE 3 OR TOTAL(nl_rgb EQ nl_rgb[0]) NE 3 THEN BEGIN
        tmp = DIALOG_MESSAGE('所选的RGB三个通道图像需要具有相同的维度！',title = 'IDL Viewer 错误提示', /error)
        RETURN
      ENDIF
      
      data = MAKE_ARRAY(3, ns_rgb[0], nl_rgb[0], type = dtype)
      
      FOR i = 0,2 DO BEGIN
        ;获取数据
        data[i,*,*] = REVERSE(ENVI_GET_DATA(fid = fidRGB[i], dims = dims, pos = BandRGB[i]), 2)
      ENDFOR
      
    END
    
    ;Gray模式
    0: BEGIN
    
      ENVI_FILE_QUERY, fid, $
        ns = ns, $
        nl = nl, $
        dims = dims,  $
        data_type = dtype
        
      (*pState).OTREE.GETPROPERTY, BANDGRAY_IDX = pos
      data = REVERSE(ENVI_GET_DATA(fid = fid, dims = dims, pos = pos), 2)
    END
  ENDCASE
  
  IF Fid NE (*pState).VIEWFID THEN BEGIN
  
    ;调整viewPlane_Rect，居中显示原始大小图像
    drawSizeALL = WIDGET_INFO((*pState).WDRAW, /geom)
    drawSize = [drawSizeALL.XSIZE, drawSizeALL.YSIZE]
    vp = INTARR(4)
    vp[0] = -(drawSize[0]-ns)/2
    vp[1] = -(drawSize[1]-nl)/2
    vp[2:3] = drawSize[0:1]
    
    (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
    (*pState).VIEWFID = Fid
  ENDIF
  
  ;刷新显示
  (*pState).OIMAGE.SETPROPERTY, data = LINEAR2(TEMPORARY(data))
  IDLVIEWER_REFRESHDRAW, pState
  
END





PRO DISPLAYMODE::Change2RGB, ev, fid = fid
  ;
  ;选择RGB Color模式
  COMPILE_OPT idl2
  
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ;设置oMode的index属性，1为RGB，0为Gray
  (*pState).OMODE.SETPROPERTY, index = 1
  
  BandSelect = WIDGET_INFO(ev.TOP, find_by_uname = 'BandSelect')
  
  child = WIDGET_INFO(bandselect, /all_children)
  FOREACH element, child DO WIDGET_CONTROL, element, /destroy
  
  
  ;搭建RGB模式的界面
  RGBBase = WIDGET_BASE(BandSelect,   $
    xsize = 240,  $
    ysize = 90,  $
    /row)
    
  ;判断是否已选波段
  IF N_ELEMENTS(fid) NE 0 THEN BEGIN
    fid = fid
  ENDIF ELSE BEGIN
    (*pState).OTREE.GETPROPERTY, FIDNOW = fid
  ENDELSE
  
  (*pState).OTREE.GETPROPERTY, BANDRGB_NAME = RGB_Band
  
  RGBButtonBase = WIDGET_BASE(RGBBase, /column, /exclusive, xsize = 30, /align_center)
  RGBTextBase = WIDGET_BASE(RGBBase, /column, xsize = 208, /align_center)
  R_Button = WIDGET_BUTTON(RGBButtonBase, value = 'R', uname = 'R')
  R_Text = WIDGET_TEXT(RGBTextBase, value = RGB_Band[0])
  G_Button = WIDGET_BUTTON(RGBButtonBase, value = 'G', uname = 'G')
  G_Text = WIDGET_TEXT(RGBTextBase, value = RGB_Band[1])
  B_Button = WIDGET_BUTTON(RGBButtonBase, value = 'B', uname = 'B')
  B_Text = WIDGET_TEXT(RGBTextBase, value = RGB_Band[2])
  
  ;设置选择情况
  WIDGET_CONTROL, r_button, /set_button
  WIDGET_CONTROL, ((*pState).OMODE).RGB_BUTTON, /set_button
  
  (*pState).OMODE.SETPROPERTY, RGB_Band = [R_Text, G_Text, B_Text, R_Button, G_Button, B_Button]
  (*pState).OMODE.SETPROPERTY, index = 1
  
  (*pState).OMODE.SETPROPERTY, RGB_Idx = 0
  
END



PRO DISPLAYMODE::Change2Gray, ev
  ;
  ;选择Gray Scale模式
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  (*pState).OMODE.SETPROPERTY, index = 0
  
  BandSelect = WIDGET_INFO(ev.TOP, find_by_uname = 'BandSelect')
  child = WIDGET_INFO(bandselect, /all_children)
  FOREACH element, child DO WIDGET_CONTROL, element, /destroy
  
  ;搭建界面
  GrayBase = WIDGET_BASE(bandselect, xsize = 242, ysize = 90, /column, /align_center)
  Label = WIDGET_LABEL(GrayBase, value = 'Selected Band:')
  
  (*pState).OTREE.GETPROPERTY, BANDGRAY_NAME = BANDGRAY_NAME
  Gray_Band = WIDGET_TEXT(GrayBase, value = BANDGRAY_NAME)
  
  WIDGET_CONTROL, ((*pState).OMODE).GRAY_BUTTON, /set_button
  
  (*pState).OMODE.SETPROPERTY, Gray_Band = Gray_Band
  (*pState).OMODE.SETPROPERTY, index = 0
  
END




PRO DISPLAYMODE::SetProperty, index = index,  $
    RGB_idx = RGB_idx,    $
    RGB_Band = RGB_Band,  $
    Gray_Band = Gray_Band,  $
    RGB_Button = RGB_Button,  $
    Gray_Button = Gray_Button,  $
    DimsText = DimsText
  ;
    
  IF N_ELEMENTS(index) NE 0 THEN self.INDEX = index
  IF N_ELEMENTS(RGB_idx) NE 0 THEN self.RGB_IDX = RGB_idx
  IF N_ELEMENTS(RGB_Band) NE 0 THEN self.RGB_BAND = RGB_Band
  IF N_ELEMENTS(Gray_Band) NE 0 THEN self.GRAY_BAND = Gray_Band
  IF N_ELEMENTS(RGB_Button) NE 0 THEN self.RGB_BUTTON = RGB_Button
  IF N_ELEMENTS(Gray_Button) NE 0 THEN self.GRAY_BUTTON = Gray_Button
  IF N_ELEMENTS(DimsText) NE 0 THEN self.DIMSTEXT = DimsText
  
END


PRO DISPLAYMODE::GetProperty, index = index,  $
    RGB_idx = RGB_idx,    $
    RGB_Band = RGB_Band,    $
    Gray_Band = Gray_Band,  $
    RGB_Button = RGB_Button,  $
    Gray_Button = Gray_Button,  $
    DimsText = DimsText
  ;
  INDEX = self.INDEX
  RGB_idx = self.RGB_IDX
  RGB_Band = self.RGB_BAND
  GRAY_Band = self.GRAY_BAND
  RGB_BUTTON = self.RGB_BUTTON
  GRAY_BUTTON = self.GRAY_BUTTON
  DimsText = self.DIMSTEXT
END




PRO DISPLAYMODE::CLEANUP
  ;
  COMPILE_OPT IDL2
END



PRO DISPLAYMODE::Create
  ;
  ;搭建界面，默认为Gray Scale

  TmpBase = WIDGET_BASE(self.PARENT, /row, /frame, xsize = 250, ysize = 33)
  TmpBase2 = WIDGET_BASE(Tmpbase,/row, /align_center)
  Modebase = WIDGET_BASE(TmpBase2, /exclusive ,/row, ysize = 40)
  Gray_Button = WIDGET_BUTTON(ModeBase, value = 'Gray Scale', uname = 'Gray')
  RGB_Button = WIDGET_BUTTON(ModeBase, value = 'RGB Color', uname = 'RGB')
  Load = WIDGET_BUTTON(TmpBase2, value = ' Load ', uname = 'Load')
  
  WIDGET_CONTROL, Gray_Button, /set_button
  
  BandSelect = WIDGET_BASE(self.PARENT, xsize = 250, ysize = 90,/frame , /column, uname = 'BandSelect')
  GrayBase = WIDGET_BASE(BandSelect, xsize = 242, ysize = 90, /column, /align_center)
  Label = WIDGET_LABEL(GrayBase, value = 'Selected Band:')
  Gray_Band = WIDGET_TEXT(GrayBase, value = '')
  
  DimsBase = WIDGET_BASE(self.PARENT, xsize = 250, /row, /frame, ysize = 33, /align_center)
  DimsLabel = WIDGET_LABEL(DimsBase, value = ' Dims ')
  DimsText = WIDGET_TEXT(DimsBase, value = ' ', /align_center, xsize = 32)
  
  self.SETPROPERTY, Gray_Button = Gray_Button
  self.SETPROPERTY, RGB_Button = RGB_Button
  self.SETPROPERTY, Gray_Band = Gray_Band
  self.SETPROPERTY, DimsText = DimsText
  
END




FUNCTION DISPLAYMODE::INIT, parent = parent
  ;
  self.PARENT = parent
  
  self.CREATE
  
  RETURN, 1
  
END





PRO DISPLAYMODE__DEFINE
  ;
  structure = {DisplayMode,           $
    index:0,                          $   ;显示模式，0为灰度，1为RGB
    RGB_idx:0,                        $   ;RGB模式下，R、G、B三个单选按钮选择情况
    parent:0L,                        $   ;父节点
    RGB_Band:[0L,0L,0L,0L,0L,0L],     $   ;[R_Text, G_Text, B_Text, R_Button, G_Button, B_Button]
    Gray_Band:0L,                     $   ;所选灰色波段显示widget_Text组件ID
    RGB_Button:0L,                    $   ;单选按钮 RGB Color ID
    Gray_Button:0L,                   $   ;单选按钮 Gray Scale ID
    DimsText:0L }
    
END