PRO IDLGRTREE::HandleEvent, ev

  ; �Ҽ��˵�
  COMPILE_OPT idl2
  
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  selection = WIDGET_INFO(ev.ID, /tree_select)
  
  fids = ENVI_GET_FILE_IDS()
  
  ; ��ʾ�Ҽ�
  IF TAG_NAMES(ev, /structure_name) EQ 'WIDGET_CONTEXT' THEN BEGIN
  
    ; �հ״��Ҽ�
    IF selection EQ -1 AND fids[0] NE -1 THEN BEGIN
    
      Blank_contextBase = WIDGET_INFO(ev.TOP, find_by_uname = 'Blank_contextMenu')
      WIDGET_DISPLAYCONTEXTMENU, ev.ID, ev.X, ev.Y, Blank_contextBase
      
    ENDIF ELSE IF selection NE -1 THEN BEGIN
    
      ; ���uname���ж�File or Band
      uname = WIDGET_INFO(selection, /uname)
      
      ;���unameΪ�գ���ͶӰ��Ϣ����ʾ��հ״��Ҽ���ͬ�Ĳ˵�
      IF uname EQ '' THEN BEGIN
        Blank_contextBase = WIDGET_INFO(ev.TOP, find_by_uname = 'Blank_contextMenu')
        WIDGET_DISPLAYCONTEXTMENU, ev.ID, ev.X, ev.Y, Blank_contextBase
        RETURN
      ENDIF
      
      file_or_band = STRMID(uname, 0, 11)
      
      IF  file_or_band EQ 'file_branch' THEN BEGIN
      
        ;�ļ�����ʾ�Ҽ�
        File_contextBase = WIDGET_INFO(ev.TOP, find_by_uname = 'File_contextMenu')
        WIDGET_DISPLAYCONTEXTMENU, ev.ID, ev.X, ev.Y, File_contextBase
        
      ENDIF ELSE BEGIN
      
        ;���δ���ʾ�Ҽ�
        Band_contextBase = WIDGET_INFO(ev.TOP, find_by_uname = 'Band_contextMenu')
        WIDGET_DISPLAYCONTEXTMENU, ev.ID, ev.X, ev.Y, Band_contextBase
        
      ENDELSE
    ENDIF
  ENDIF
END



PRO IDLGRTREE::LoadBand, ev
  ;
  COMPILE_OPT idl2
  ;�Ҽ���˫����������ѡ�����¼�
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  fid = ((*pState).OTREE).FIDNOW
  
  ENVI_FILE_QUERY, fid, $
    sname = filename,  $
    bname = bandname,  $
    nb = nb, $
    ns = ns, $
    nl = nl, $
    dims = dims, $
    wl = wavelength, $       ;����
    data_type = dtype
    
  ; ��������洢����
  data = MAKE_ARRAY(ns, nl, type = dtype)
  
  pos = ((*pState).OTREE).BANDGRAY_IDX
  data = REVERSE(ENVI_GET_DATA(fid = fid, dims = dims, pos = pos), 2)
  
  ;������set��oImage��data����
  (*pState).OIMAGE.SETPROPERTY, data = LINEAR2(TEMPORARY(data))
  
  ;�����ѡFID��Draw�е�FID��һ�£�������viewPlane_rect
  IF fid NE (*pState).VIEWFID THEN BEGIN
  
    ;����viewPlane_Rect��������ʾԭʼ��Сͼ��
    drawSizeALL = WIDGET_INFO((*pState).WDRAW, /geom)
    drawSize = [drawSizeALL.XSIZE, drawSizeALL.YSIZE]
    vp = INTARR(4)
    vp[0] = -(drawSize[0]-ns)/2
    vp[1] = -(drawSize[1]-nl)/2
    vp[2:3] = drawSize[0:1]
    
    (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
    (*pState).VIEWFID = fid
  ENDIF
  
  ;����Gray��ѡ��ѡ��״̬
  (*pState).OMODE.GETPROPERTY, GRAY_BUTTON = GRAY_BUTTON
  WIDGET_CONTROL, GRAY_BUTTON, /set_button
  (*pState).OMODE.CHANGE2GRAY, ev
  
  ;ˢ����ʾ
  IDLVIEWER_REFRESHDRAW, pState
  
END




PRO IDLGRTREE::RemoveFile, ev

  ;�Ҽ�ɾ����ѡ�ļ��¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ;��ȡ��ѡ�ļ�ID
  fidnow = ((*pState).OTREE).FIDNOW
  fids = ENVI_GET_FILE_IDS()
  ;ɾ��fidnow
  ENVI_FILE_MNG, id = fidnow[0], /remove
  
  ;��ȡ��ѡ�ļ�branch��uname��������uname��ȡid��Ȼ������
  uname = WIDGET_INFO(WIDGET_INFO(((*pState).OTREE).ID, /tree_select), /uname)
  file_branch = WIDGET_INFO(((*pState).OTREE).ID, find_by_uname = uname)
  WIDGET_CONTROL, file_branch, /destroy
  
  ;�Զ�ѡ�����ϱߵ��ļ��ĵ�һ����
  fids = ENVI_GET_FILE_IDS()
  IF fids[0] NE -1 THEN BEGIN
    ;
    Fidtop = fids[0]
    ;
    ENVI_FILE_QUERY, Fidtop, $
      sname = filename,  $
      bname = bandname,  $
      nb = nb,  $
      ns = ns,  $
      nl = nl,  $
      wl = wavelength,  $
      data_type = dtype,  $
      interleave = i_idx
      
    Leaf = WIDGET_INFO(ev.TOP, find_by_uname = 'band_branch_0 ' + STRTRIM(STRING(Fidtop),2))
    IF Leaf EQ 0 THEN RETURN
    IF N_ELEMENTS(leaf) NE 0 THEN WIDGET_CONTROL, Leaf, /set_tree_select
    
    IF wavelength[0] NE -1 THEN bandname[0] += ' (' + STRTRIM(STRING(wavelength[0]),2) + ')' + ': ' + filename  $
    ELSE  bandname[0] += ': ' + filename
    
    ;����oTree����
    (*pState).OTREE.SETPROPERTY, BandRGB_idx = [0,-1,-1]
    (*pState).OTREE.SETPROPERTY, FidRGB = [Fidtop,-1,-1]
    (*pState).OTREE.SETPROPERTY, BandRGB_name = [bandname[0],'','']
    
    (*pState).OTREE.SETPROPERTY, BandGray_idx = 0
    (*pState).OTREE.SETPROPERTY, BandGray_name = bandname[0]
    
    (*pState).OTREE.SETPROPERTY, FIDNOW = Fidtop
    (*pState).OMODE.SETPROPERTY, RGB_IDX = 0
    
    ;����DimsText����
    Interleave = ['BSQ', 'BIL', 'BIP']
    Type_Name = ['UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT', 'DOUBLE', 'COMPLEX', 'STRING', 'STRUCT', 'DCOMPLEX', 'POINTER', 'OBJREF', 'UINT', 'ULONG', 'LONG64', 'ULONG64']
    DimsText = STRTRIM(STRING(ns),2) + '��' + STRTRIM(STRING(nl),2) + ' (' + TYPE_NAME[dtype] + ') ' + '[' + Interleave[i_idx] + ']'
    
    
  ENDIF ELSE BEGIN
  
    ;����б������ļ�
    (*pState).OTREE.SETPROPERTY, BandRGB_idx = [-1,-1,-1]
    (*pState).OTREE.SETPROPERTY, FidRGB = [-1,-1,-1]
    (*pState).OTREE.SETPROPERTY, BandRGB_name = ['','','']
    
    (*pState).OTREE.SETPROPERTY, BandGray_idx = -1
    (*pState).OTREE.SETPROPERTY, BandGray_name = ''
    
    (*pState).OTREE.SETPROPERTY, FIDNOW = -1
    (*pState).OMODE.SETPROPERTY, RGB_IDX = 0
    
    (*pState).VIEWIDX = 0
    (*pState).MOUSESTATUS = ''
    
    DimsText = ''
    
    ;���ù�������ť���
    IDLVIEWER_BNTSTYLE, ev, 0
    (*pState).OWIN.SETCURRENTCURSOR, 'original'
    
  ENDELSE
  
  
  ;����DimsText����
  (*pState).OMODE.GETPROPERTY, DIMSTEXT = DIMSTEXT_ID
  WIDGET_CONTROL, DIMSTEXT_ID, set_value = DimsText
  
  ;�޸���ѡ�����б�
  (*pState).OMODE.GETPROPERTY, INDEX = INDEX
  IF INDEX EQ 1 THEN BEGIN
  
    ; RGBģʽ��
    (*pState).OMODE.GETPROPERTY, RGB_BAND = RGB_BAND
    FOR i =0, 2 DO BEGIN
      WIDGET_CONTROL, RGB_BAND[i], set_value = (((*pState).OTREE).BANDRGB_NAME)[i]
    ENDFOR
    
  ENDIF ELSE BEGIN
    ;Grayģʽ��
    (*pState).OMODE.GETPROPERTY, GRAY_BAND = GRAY_BAND
    WIDGET_CONTROL, GRAY_BAND, set_value = ((*pState).OTREE).BANDGRAY_NAME
  ENDELSE
  
  
  ;�ж�oImage�е������Ƿ�Ϊfidnow
  IF (*pState).VIEWFID EQ fidnow THEN BEGIN
  
    (*pState).VIEWFID = 0
    ;
    ;�����ͬ����ɾ��oImage��data���ݣ������»���
    OBJ_DESTROY, (*pState).OIMAGE
    (*pState).OIMAGE = OBJ_NEW('IDLgrImage')
    (*pState).OMODEL.ADD, (*pState).OIMAGE
    
    OBJ_DESTROY, (*pState).OPOLYGON
    (*pState).OPOLYGON = OBJ_NEW('IDLgrPolygon')
    (*pState).OMODEL.ADD, (*pState).OPOLYGON
    
    IDLVIEWER_REFRESHDRAW, pState
    
  ENDIF
END



PRO IDLGRTREE::CloseAllFiles, ev

  ;�ر������ļ��¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ;�ļ��б�����
  file_ids = WIDGET_INFO(((*pState).OTREE).ID, /all_children)
  IF file_ids[0] EQ 0 THEN RETURN
  FOREACH element, file_ids DO WIDGET_CONTROL, element, /destroy
  
  ;�ļ�ID�ر�
  fids = ENVI_GET_FILE_IDS()
  
  FOREACH element, fids DO ENVI_FILE_MNG, id = element, /remove
  (*pState).OTREE.SETPROPERTY, FIDNOW = -1
  
  ;����oTree����
  (*pState).OTREE.SETPROPERTY, BandRGB_idx = [-1,-1,-1]
  (*pState).OTREE.SETPROPERTY, FidRGB = [-1,-1,-1]
  (*pState).OTREE.SETPROPERTY, BandRGB_name = ['','','']
  
  (*pState).OTREE.SETPROPERTY, BandGray_idx = -1
  (*pState).OTREE.SETPROPERTY, BandGray_name = ''
  
  (*pState).OTREE.SETPROPERTY, FIDNOW = -1
  (*pState).OMODE.SETPROPERTY, RGB_IDX = 0
  
  
  ;����DimsText����
  (*pState).OMODE.GETPROPERTY, DIMSTEXT = DIMSTEXT_ID
  WIDGET_CONTROL, DIMSTEXT_ID, set_value = ''
  
  ;��Grayģʽ
  ((*pState).OMODE).CHANGE2GRAY, ev
  
  ;���ð�ťͼƬ
  IDLVIEWER_BNTSTYLE, ev, 0
  
  ;���������ʽ
  (*pState).OWIN.SETCURRENTCURSOR, 'Original'
  
  ;�ر���ʾͼ��
  OBJ_DESTROY, (*pState).OIMAGE
  (*pState).OIMAGE = OBJ_NEW('IDLgrImage')
  (*pState).OMODEL.ADD, (*pState).OIMAGE
  
  OBJ_DESTROY, (*pState).OPOLYGON
  (*pState).OPOLYGON = OBJ_NEW('IDLgrPolygon')
  (*pState).OMODEL.ADD, (*pState).OPOLYGON
  
  (*pState).VIEWFID = 0
  (*pState).VIEWIDX = 0
  
  IDLVIEWER_REFRESHDRAW, pState
  
  
END





PRO IDLGRTREE::AddFile, ev, Fid

  ;�ļ��б�����ļ��¼�
  COMPILE_OPT idl2
  
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ;�ж�IDΪFid���ļ��Ƿ����ļ��б��д򿪣�����Ѿ��򿪣��򷵻�
  child = WIDGET_INFO(((*pState).OTREE).ID, /all_children)
  IF child[0] NE 0 THEN BEGIN
    FOREACH element, child DO BEGIN
      uname = WIDGET_INFO(element, /uname)
      IF uname EQ 'file_branch_' + STRTRIM(STRING(Fid),2) THEN RETURN
    ENDFOREACH
  ENDIF
  
  ;��ѯ�ļ�
  ENVI_FILE_QUERY, fid, $
    sname = filename,  $
    bname = bandname,  $
    nb = nb, $
    ns = ns, $
    nl = nl, $
    dims = dims, $
    wl = wavelength, $       ;����
    data_type = dtype,  $
    file_type = ftype
    
  ;�ж��ļ����ͣ��ļ����а���DEM������Ϊ��DEMͼ��ͨ���������ж���ȫɫ�����ף�
  filetype = envi_file_type(ftype)
  IF nb EQ 1 AND ftype NE 3 THEN ftype = 100                    ;������ͼ��
  IF STRPOS(STRUPCASE(filename), 'DEM') NE -1 THEN ftype = 101  ;DEMͼ��
  
  CASE ftype OF
    0: filebmp = *((*pState).ICONBMP[0])       ;ENVI Standard
    3: filebmp = *((*pState).ICONBMP[2])       ;ENVI Classification
    100: filebmp = *((*pState).ICONBMP[1])     ;ȫɫͼ��
    101: filebmp = *((*pState).ICONBMP[3])     ;DEMͼ��
    ELSE: filebmp = *((*pState).ICONBMP[0])
  ENDCASE
  
  ;����ļ��б�
  file_branch = WIDGET_TREE(self.ID,  $
    value = filename,   $
    /folder, /top, /expanded,   $
    uname = 'file_branch_' + STRTRIM(STRING(Fid),2),  $
    bitmap = filebmp)
    
    
  IF wavelength[0] NE -1 THEN bandname += ' (' + STRTRIM(STRING(wavelength),2) + ')'
  
  ;��Ӳ����б�
  FOR i = 0, N_ELEMENTS(bandname)-1 DO BEGIN
    band_leaf = WIDGET_TREE(file_branch,  $
      value = bandname[i],  $
      uname = 'band_branch_' + STRTRIM(STRING(i),2) + ' ' + STRTRIM(STRING(Fid),2), $
      bitmap = *((*pState).ICONBMP[4]) )
    IF i EQ 0 THEN WIDGET_CONTROL, band_leaf, /set_tree_select
  ENDFOR
  
  
  ;���ͶӰ��Ϣ
  Proj = ENVI_GET_PROJ(fid)
  
  IF Proj.PROJ EQ 'Arbitrary' THEN RETURN
  
  ;ͶӰ��Ϣ���ڵ�
  proj_branch = WIDGET_TREE(file_branch,  $
    value = 'Map Info', $
    /folder, $
    bitmap = *((*pState).ICONBMP[5]))
    
  ;ͶӰ��Ϣ
  FOR i = 0, 4 DO BEGIN
    CASE i OF
      ; Proj
      0: BEGIN
        value = 'Proj: ' + STRJOIN(STRSPLIT(Proj.PROJ, '_', /extra), ' ')
      END
      ; Pixel
      1: BEGIN
        value = 'Pixel: ' + NUM_FORMATTER(DOUBLE(Proj.PIXEL), /delZero) + ' Meters'
      ENDCASE
      ;Datum
      2: BEGIN
        value = 'Datum: ' + Proj.DATUM
      END
      ;UL Geo
      3: BEGIN
        ; �жϾ�γ������
        lon_idx = (proj.UL_GEO)[0] LT 0 ? 'W' : 'E'
        lat_idx = (proj.UL_GEO)[3] LT 0 ? 'S' : 'N'
        ul_geo = STRTRIM(STRING(ABS(FIX(proj.UL_GEO))),2)
        
        value = 'UL Geo: ' + ul_geo[0] + '��' +  $
          ul_geo[1] + "'" + $
          NUM_FORMATTER(ABS((proj.UL_GEO)[2])) + '"' + lon_idx + ', ' +  $
          ul_geo[3] + '��' +  $
          ul_geo[4] + "'" + $
          NUM_FORMATTER(ABS((proj.UL_GEO)[5])) + '"' + lat_idx
      END
      ;UL Map
      4: BEGIN
        value = 'UL Map: ' + NUM_FORMATTER((proj.UL_MAP)[0], Decimals = 3) + ', '  $
          + NUM_FORMATTER((proj.UL_MAP)[1], Decimals = 3)
      END
      ELSE: value = ''
    ENDCASE
    
    proj_leaf = WIDGET_TREE(proj_branch, $
      value = value,  $
      bitmap = *((*pState).ICONBMP[4]))
  ENDFOR
  
  
  IF (*pState).VIEWIDX EQ 0 THEN BEGIN
    FOREACH element, (*pState).TOOLGROUP DO WIDGET_CONTROL, element, /SENSITIVE
    IDLVIEWER_BNTSTYLE, ev, 1
  ENDIF
    
END


PRO IDLGRTREE::FoldFile, ev

  ;�����ļ���
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  file_ids = WIDGET_INFO(((*pState).OTREE).ID, /all_children)
  FOREACH element, file_ids DO WIDGET_CONTROL, element, set_tree_expanded = 0
  
END



PRO IDLGRTREE::UnfoldFile, ev

  ;չ���ļ���
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  file_ids = WIDGET_INFO(((*pState).OTREE).ID, /all_children)
  FOREACH element, file_ids DO WIDGET_CONTROL, element, /set_tree_expanded
  
END





PRO IDLGRTREE::SetProperty, uname = uname,  $
    id = id, $
    xsize = xsize, $
    ysize = ysize, $
    fidnow = fidnow, $
    BandRGB_idx = BandRGB_idx,  $
    BandRGB_name = BandRGB_name,  $
    FidRGB = FidRGB,    $
    BandGray_idx = BandGray_idx,  $
    BANDGray_name = BANDGray_name
  ;
  IF N_ELEMENTS(uname) NE 0 THEN self.UNAME = uname
  IF N_ELEMENTS(id) NE 0 THEN self.ID = id
  IF N_ELEMENTS(xsize) NE 0 THEN WIDGET_CONTROL, self.ID, xsize = xsize
  IF N_ELEMENTS(ysize) NE 0 THEN WIDGET_CONTROL, self.ID, ysize = ysize
  IF N_ELEMENTS(fidnow) NE 0 THEN self.FIDNOW = fidnow
  IF N_ELEMENTS(BandRGB_idx) NE 0 THEN self.BANDRGB_IDX = BandRGB_idx
  IF N_ELEMENTS(BandRGB_name) NE 0 THEN self.BANDRGB_NAME = BandRGB_name
  IF N_ELEMENTS(FidRGB) NE 0 THEN self.FIDRGB = FidRGB
  IF N_ELEMENTS(BandGray_idx) NE 0 THEN self.BANDGRAY_IDX = BandGray_idx
  IF N_ELEMENTS(BandGray_name) NE 0 THEN self.BANDGRAY_NAME = BandGray_name
  
END


PRO IDLGRTREE::GetProperty, id = id, $
    fidnow = fidnow, $
    BandRGB_idx = BandRGB_idx, $
    BandRGB_name = BandRGB_name,  $
    BandGray_idx = BandGray_idx, $
    FidRGB = FidRGB,  $
    BANDGray_name = BANDGray_name
    
  ;
  id = self.ID
  fidnow = self.FIDNOW
  BandRGB_idx = self.BANDRGB_IDX
  BandRGB_name = self.BANDRGB_NAME
  FidRGB = self.FIDRGB
  BandGray_idx = self.BANDGRAY_IDX
  BANDGray_name = self.BANDGRAY_NAME
  
END


PRO IDLGRTREE::CLEANUP
  ;
  COMPILE_OPT IDL2
END



PRO IDLGRTREE::Create

  ;�����ļ��б����

  wTree = WIDGET_TREE(self.PARENT, $
    event_pro = self.EVENT_PRO, $
    xsize = self.XSIZE,  $
    ysize = self.YSIZE,  $
    uname = self.UNAME,  $
    /CONTEXT_EVENTS )
    
  self.SETPROPERTY, id = wTree
  
  ;�ļ� - �Ҽ��˵�
  File_contextBase = WIDGET_BASE(self.PARENT, /context_menu, uname = 'File_contextMenu')
  
  Button = WIDGET_BUTTON(File_contextBase, $
    value = '�ر���ѡ�ļ�', $
    uname = "RemoveFile")
    
  Button = WIDGET_BUTTON(File_contextBase, $
    value = '���������ļ�', $
    uname = "FoldFile", /sep)
  Button = WIDGET_BUTTON(File_contextBase, $
    value = '���������ļ�', $
    uname = "UnfoldFile")
    
    
  ;���� - �Ҽ��˵�
  Band_contextBase = WIDGET_BASE(self.PARENT, /context_menu, uname = 'Band_contextMenu')
  
  Button = WIDGET_BUTTON(Band_contextBase, $
    value = '������ѡ����', $
    uname = "LoadBand")
    
  Button = WIDGET_BUTTON(Band_contextBase, $
    value = '���������ļ�', $
    uname = "FoldFile", /sep)
  Button = WIDGET_BUTTON(Band_contextBase, $
    value = '���������ļ�', $
    uname = "UnfoldFile")
    
    
    
  ;�հ� - �Ҽ��˵�
  Blank_contextBase = WIDGET_BASE(self.PARENT, /context_menu, uname = 'Blank_contextMenu')
  
  Button = WIDGET_BUTTON(Blank_contextBase, $
    value = '���������ļ�', $
    uname = "FoldFile")
  Button = WIDGET_BUTTON(Blank_contextBase, $
    value = '���������ļ�', $
    uname = "UnfoldFile")
    
    
END



FUNCTION IDLGRTREE::INIT, parent = parent,  $
    xsize = xsize,    $
    ysize = ysize,    $
    uname = uname,  $
    event_pro = event_pro
    
  self.PARENT = parent
  self.EVENT_PRO = event_pro
  IF N_ELEMENTS(xsize) NE 0 THEN self.XSIZE = xsize
  IF N_ELEMENTS(ysize) NE 0 THEN self.YSIZE = ysize
  IF N_ELEMENTS(uname) NE 0 THEN self.UNAME = uname
  
  self.CREATE
  
  RETURN, 1
  
END



PRO IDLGRTREE__DEFINE

  ;����IDLgrTree�����ļ��б�
  structure = {IDLgrTree,           $
    event_pro:'',                   $   ;�ļ��б�����¼�
    parent:0L,                      $   ;widget_tree���ڵ�
    FidNow:0L,                      $   ;��ǰFID
    FidRGB:[-1,-1,-1],              $   ;RGB����ͨ����Ӧ��Fid
    BandRGB_idx:[-1,-1,-1],         $   ;RGB����ͨ����Ӧ�Ĳ��κ�
    BandRGB_name:['','',''],        $   ;RGB����ͨ��������
    BandGray_idx:-1,                $   ;�Ҷ�ģʽ��ѡ���κ�
    BandGray_name:'',               $   ;�Ҷ�ģʽ��ѡ������
    ID:0L,                          $   ;widget_tree�����ID
    xsize:0L,                       $
    ysize:0L,                       $
    uname:''}
    
END