;+
; :NAME: IDL Viewer use ENVI
;
; :DESCRIPTION:
;   - Support ENVI Raster data, and other filetype supported by ENVI_OPEN_FILE function
;   - Open, View, Band Management, ZoomIn, ZoomOut, Pan, et al.
;
; :AUTHOR: duhj@esrichina.com.cn
;
; :VERSION:  1.0
;-


PRO IDLVIEWER_EVENT, ev

  COMPILE_OPT idl2
  
  ;�������¼�
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  tagName = TAG_NAMES(ev, /structure_name)
  
  IF tagName EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
  
    ;����ر���ʾ
    ret = DIALOG_MESSAGE('ȷ���˳�IDL Viewer��', title = 'IDL Viewer', /question)
    IF ret EQ 'No' THEN RETURN
    ;�ر�����FID
    fids = ENVI_GET_FILE_IDS()
    FOREACH id, fids DO ENVI_FILE_MNG, id = id, /remove
    WIDGET_CONTROL, ev.TOP, /destroy
    
    ENVI_BATCH_EXIT
    RETURN
  ENDIF
  
  uname = WIDGET_INFO(ev.ID, /uname)
  
  CASE uname OF
    'tlb': BEGIN
      ;���ڵ����¼�
      IDLVIEWER_RESIZE, ev
    END
    
    'wDraw': BEGIN
      ;���ֺ�ƽ���¼�
      IF ev.TYPE EQ 4 THEN IDLVIEWER_REFRESHDRAW, pState
      IF ev.TYPE EQ 7 THEN IDLVIEWER_WHEEL, ev
      IDLVIEWER_PAN, ev
      
      ;����Ŵ�
      IDLVIEWER_ZOOMIN, ev
      
      (*pState).OTREE.GETPROPERTY, FidNow = fid
      IF fid LT 1 THEN RETURN
      
      ;˫����ʾ����
      (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
      IF ev.PRESS EQ 1 AND ev.CLICKS EQ 2 THEN BEGIN
      
        CASE OBJ_VALID((*pState).OCURSORDATA) OF
          0: BEGIN
            IF (*pState).VIEWIDX EQ 0 THEN BEGIN
              oCursorData = OBJ_NEW('CursorData', parent = ev.TOP)
              (*pState).OCURSORDATA = oCursorData
              (*pState).OWIN.SETCURRENTCURSOR, 'CROSSHAIR'
            ENDIF
          END
          1: BEGIN
            (*pState).OCURSORDATA.GETPROPERTY, wBase = wBase
            WIDGET_CONTROL, wBase, /destroy
            OBJ_DESTROY, (*pState).OCURSORDATA
            CASE (*pState).VIEWIDX OF
              0: (*pState).OWIN.SETCURRENTCURSOR, 'Original'
              1: (*pState).OWIN.SETCURRENTCURSOR, 'Move'
            ENDCASE
          END
        ENDCASE
      ENDIF
      
      ;���������Ч����Ե����ġ����ȡֵ���Ի������ȡֵ
      IF OBJ_VALID((*pState).OCURSORDATA) EQ 1 THEN BEGIN
        (*pState).OWIN.SETCURRENTCURSOR, 'CROSSHAIR'
        (*pState).OCURSORDATA.SETVALUE, ev
      END
    END
    
    ;��ťLOAD�¼�
    'Load': (*pState).OMODE.HANDLEEVENT, ev
    
    ;������ѡ��ť�¼�
    'Select': BEGIN
      (*pState).VIEWIDX = 0
      ;���������ʽ
      (*pState).OWIN.SETCURRENTCURSOR, 'original'
      
      ;���ð�ťЧ��
      IDLVIEWER_BNTSTYLE, ev, 1
      
    END
    
    ;������ƽ�ư�ť�¼�
    'Hand': BEGIN
      (*pState).VIEWIDX = 1
      (*pState).OWIN.SETCURRENTCURSOR, 'move'
      
      ;���ð�ťЧ��
      IDLVIEWER_BNTSTYLE, ev, 2
    END
    
    ;����Ŵ�
    'Zoom': BEGIN
    
      (*pState).VIEWIDX = 2
      (*pState).OWIN.SETCURRENTCURSOR, 'crosshair'
      
      ;���ð�ťЧ��
      IDLVIEWER_BNTSTYLE, ev, 3
    END
    
    ;�������Ŵ���С��ť
    'Zoomin': IDLVIEWER_ZOOM, ev, 0
    'Zoomout': IDLVIEWER_ZOOM, ev, 1
    
    ;������Reset��ť
    'ResetView': BEGIN
    
      ;����viewPlane_Rect��������ʾԭʼ��Сͼ��
      IF (*pState).VIEWFID EQ 0 THEN RETURN
      ENVI_FILE_QUERY, (*pState).VIEWFID, ns = ns, nl = nl
      
      (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
      
      vp = INTARR(4)
      vp[0] = -(vd[0]-ns)/2
      vp[1] = -(vd[1]-nl)/2
      vp[2:3] = vd[0:1]
      
      (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
      
      IDLVIEWER_REFRESHDRAW, pState
      
    END
    
    ;������FitWin��ť
    'FitWindow': BEGIN
    
      ;����ͼ����Ӧ���ڴ�С
      IF (*pState).VIEWFID EQ 0 THEN RETURN
      ENVI_FILE_QUERY, (*pState).VIEWFID, ns = ns, nl = nl
      
      (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
      
      IF vd[0]/vd[1] LE FLOAT(ns)/nl THEN BEGIN
        vp[0] = 0
        vp[1] = -(vd[1]*ns/vd[0]-nl)/2
        vp[2:3] = [ns, vd[1]*ns/vd[0]]
        
      ENDIF ELSE BEGIN
        vp[1] = 0
        vp[0] = -(vd[0]*nl/vd[1]-ns)/2
        vp[2:3] = [vd[0]*nl/vd[1], nl]
      ENDELSE
      
      
      (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
      
      IDLVIEWER_REFRESHDRAW, pState
    END
    
    ;R��G��B������ѡ��ť
    'R': BEGIN
      IF ev.SELECT EQ 1 THEN BEGIN
        (*pState).OMODE.SETPROPERTY, RGB_idx = 0
      ENDIF
    END
    'G': BEGIN
      IF ev.SELECT EQ 1 THEN BEGIN
        (*pState).OMODE.SETPROPERTY, RGB_idx = 1
      ENDIF
    END
    'B': BEGIN
      IF ev.SELECT EQ 1 THEN BEGIN
        (*pState).OMODE.SETPROPERTY, RGB_idx = 2
      ENDIF
    END
    
    ;�Ҽ��Ƴ��ļ��¼�
    'RemoveFile': (*pState).OTREE.REMOVEFILE, ev
    
    ;�Ƴ������ļ��¼�
    'CloseAllFiles': (*pState).OTREE.CLOSEALLFILES, ev
    
    ;�˳��¼�
    'Exit': BEGIN
      ;����ر���ʾ
      ret = DIALOG_MESSAGE('ȷ���˳�IDL Viewer��', title = 'IDL Viewer', /question)
      IF ret EQ 'No' THEN RETURN
      WIDGET_CONTROL, ev.TOP, /destroy
      fids = ENVI_GET_FILE_IDS()
      FOREACH id, fids DO ENVI_FILE_MNG, id = id, /remove
      ENVI_BATCH_EXIT
    END
    
    ;����ͷ����ļ��б��¼�
    'FoldFile': (*pState).OTREE.FOLDFILE, ev
    'UnfoldFile': (*pState).OTREE.UNFOLDFILE, ev
    
    ;�Ҽ�Load��ѡ�����¼�
    'LoadBand': (*pState).OTREE.LOADBAND, ev
    
    ;Gray Scale ��ѡ��ť
    'Gray': BEGIN
      IF ev.SELECT EQ 1 THEN BEGIN
        (*pState).OMODE.SETPROPERTY, index = 0
        (*pState).OMODE.CHANGE2GRAY, ev
      ENDIF
    END
    
    ;RGB Color ��ѡ��ť
    'RGB': BEGIN
      IF ev.SELECT EQ 1 THEN BEGIN
        (*pState).OMODE.SETPROPERTY, index = 1
        (*pState).OMODE.CHANGE2RGB, ev
      ENDIF
    END
    
    ;���ļ��¼�
    'Open': BEGIN
      file = DIALOG_PICKFILE(path = (*pState).CURPATH, get_path = newPath, /MULTIPLE_FILES)
      (*pState).CURPATH = newPath
      
      ;���ļ���֧�ֶ�ѡ
      FOREACH element, file DO BEGIN
      
        filename = STRJOIN(STRSPLIT(element, '\', /extract), '\')
        IF ~FILE_TEST(filename) THEN RETURN
        
        ;��ȡ�Ѵ������ļ�FID
        fids = ENVI_GET_FILE_IDS()
        
        ENVI_OPEN_FILE, filename, r_fid = fid
        
        ;����ļ��Ѵ򿪣��򷵻�
        IF TOTAL(fids EQ fid) EQ 1 THEN RETURN
        
        ;�ж��Ƿ�һ���Դ��������ļ�
        Newfids = ENVI_GET_FILE_IDS()
        
        ;���ļ��¼�
        IF N_ELEMENTS(Newfids) - N_ELEMENTS(fids) EQ 2 OR (N_ELEMENTS(Newfids) EQ 2 AND fids[0] EQ -1) THEN BEGIN
          IDLVIEWER_OPEN, ev, fid+1
          IDLVIEWER_OPEN, ev, fid
        ENDIF ELSE BEGIN
          IDLVIEWER_OPEN, ev, fid
        ENDELSE
        
      ENDFOREACH
      
      ;ˢ����ʾ
      IDLVIEWER_REFRESHDRAW, pState
      
    END
    
    ;�����¼�
    'About': BEGIN
      oAbout = OBJ_NEW('IDLgrAbout',  $
        parent = ev.TOP,  $
        xsize = 482,  $
        ysize = 294,  $
        logoFile = (*pState).IMAGEPATH + '\IDLViewer_About.jpg',  $
        title = '����IDL Viewer')
    END
    
    ;�򿪰����ĵ�
    'Help': BEGIN
      SPAWN, FILE_DIRNAME(ROUTINE_FILEPATH('IDLViewer_useENVI')) + '\Resource\IDLViewer�����ĵ�.pdf', /hide
    END
    
    ELSE:
  ENDCASE
  
END





PRO IDLVIEWER_OTREE, ev
  ;
  ;oTree�����ļ��б��¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ;�Ҽ��˵��¼�
  (*pState).OTREE.HANDLEEVENT, ev
  
  IF WIDGET_INFO(ev.ID, /tree_select) EQ -1 THEN RETURN
  
  ;��ȡ��ѡTree��֧��value�� ��bandname
  WIDGET_CONTROL, ev.ID, get_value = bandname
  uname = WIDGET_INFO(ev.ID, /uname)
  IF uname EQ 'wTree' OR uname EQ '' THEN RETURN
  
  file_or_band = STRMID(uname, 0, 11)
  
  ;���ѡ������ļ�����ֵFidnow������
  IF  file_or_band EQ 'file_branch' THEN BEGIN
    fidnow = STRMID(uname, 12)
    (*pState).OTREE.SETPROPERTY, fidnow = LONG(fidnow)
    RETURN
  ENDIF ELSE BEGIN
  
    ;���ѡ����ǲ���
    uname_tmp = STRSPLIT(uname, /extract)
    
    ;��ȡ��ǰFID��Band
    fidnow = LONG(uname_tmp[1])
    Bandnow = LONG(STRMID(uname_tmp[0], 12))
    
    ;����oTree����
    (*pState).OTREE.SETPROPERTY, fidnow = LONG(fidnow)
    
    (*pState).OMODE.GETPROPERTY, RGB_IDX = RGB_Idx
    (*pState).OTREE.GETPROPERTY, BandRGB_idx = bandold
    (*pState).OTREE.GETPROPERTY, FidRGB = fidold
    
    bandold[RGB_idx] = Bandnow
    fidold[RGB_idx] = fidnow
    
    (*pState).OTREE.SETPROPERTY, BandRGB_idx = bandold
    (*pState).OTREE.SETPROPERTY, FidRGB = fidold
    
    (*pState).OTREE.SETPROPERTY, BandGray_idx = bandnow
    
    ;˫����ʾ������
    IF ev.CLICKS EQ 2 THEN (*pState).OTREE.LOADBAND, ev
    
    
  ENDELSE
  
  (*pState).OMODE.GETPROPERTY, RGB_IDX = RGB_Idx
  
  ;��ѯ��ǰFID����
  ENVI_FILE_QUERY, fidnow,        $
    sname = filename,             $ ;�ļ���
    nb = nb,                      $ ;������
    ns = ns,                      $ ;����
    nl = nl,                      $ ;����
    dims = dims,                  $ ;Dimensions
    wl = wavelength,              $ ;����
    data_type = dtype,            $ ;�������ͣ�������
    interleave = i_idx              ;�洢��ʽ��������
    
  ;����oTree�����в���������
  (*pState).OTREE.SETPROPERTY, BandGray_name = bandname + ':' + filename
  
  (*pState).OTREE.GETPROPERTY, BandRGB_name = Bandold_name
  Bandold_name[RGB_idx] = bandname + ': ' + filename
  (*pState).OTREE.SETPROPERTY, BandRGB_name = Bandold_name
  
  
  ;����DimsText����
  Interleave = ['BSQ', 'BIL', 'BIP']
  Type_Name = ['UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT',        $
    'DOUBLE', 'COMPLEX', 'STRING', 'STRUCT', 'DCOMPLEX',           $
    'POINTER', 'OBJREF', 'UINT', 'ULONG', 'LONG64', 'ULONG64']
  DimsText = STRTRIM(STRING(ns),2) + '��' + STRTRIM(STRING(nl),2) +  $
    ' (' + TYPE_NAME[dtype] + ') ' +   $
    '[' + Interleave[i_idx] + ']'
  (*pState).OMODE.GETPROPERTY, DIMSTEXT = DIMSTEXT_ID
  WIDGET_CONTROL, DIMSTEXT_ID, set_value = DimsText
  
  ;������ѡ��������
  (*pState).OMODE.GETPROPERTY, index = index
  IF INDEX EQ 1 THEN BEGIN
  
    ; RGBģʽ��
    (*pState).OMODE.GETPROPERTY, RGB_BAND = RGB_BAND
    (*pState).OTREE.GETPROPERTY, BANDRGB_NAME = BANDRGB_NAME
    WIDGET_CONTROL, RGB_BAND[RGB_idx], set_value = BANDRGB_NAME[RGB_idx]
    
    RGB_idx += 1
    IF RGB_idx GT 2 THEN RGB_idx = 0
    WIDGET_CONTROL, (RGB_BAND)[RGB_idx+3], /set_button
    (*pState).OMODE.SETPROPERTY, RGB_idx = RGB_idx
    
  ENDIF ELSE BEGIN
  
    ; Grayģʽ��
    (*pState).OMODE.GETPROPERTY, GRAY_BAND = GRAY_BAND
    (*pState).OTREE.GETPROPERTY, BANDGRAY_NAME = BANDGRAY_NAME
    WIDGET_CONTROL, GRAY_BAND, set_value = BANDGRAY_NAME
    
    RGB_idx += 1
    IF RGB_idx GT 2 THEN RGB_idx = 0
    (*pState).OMODE.SETPROPERTY, RGB_idx = RGB_idx
    
  ENDELSE
  
END




PRO IDLVIEWER_OPEN, ev, fid
  ;
  ;���ļ��¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  IF fid EQ -1 THEN RETURN
  
  ENVI_FILE_QUERY, fid,           $
    bname = bandname,             $ ;������
    sname = filename,             $ ;�ļ���
    nb = nb,                      $ ;������
    ns = ns,                      $ ;����
    nl = nl,                      $ ;����
    dims = dims,                  $ ;Dimensions
    wl = wavelength,              $ ;����
    data_type = dtype,            $ ;�������ͣ�������
    interleave = i_idx              ;�洢��ʽ��������
    
    
  ; ��������洢����
  rgb_or_pan = nb GE 3 ? 3 : 1
  data = MAKE_ARRAY(rgb_or_Pan, ns, nl, type = dtype)
  
  ;��ȡ��ʾ���ݣ�Ĭ�ϼ��� RGB = Band 3/2/1 �������ȫɫͼ����ֻ����Band 1��
  IF nb GE 3 THEN BEGIN
  
    ;������������ڵ���3�����Զ�������ʾģʽΪRGB Color
    (*pState).OMODE.CHANGE2RGB, ev, fid = fid
    (*pState).OMODE.GETPROPERTY, RGB_BUTTON = RGB_BUTTON
    (*pState).OMODE.GETPROPERTY, RGB_BAND = RGB_BAND
    WIDGET_CONTROL, RGB_BUTTON, /set_button
    WIDGET_CONTROL, RGB_BAND[3], /set_button
    
    IF wavelength[0] NE -1 THEN bandname += ' (' + STRTRIM(STRING(wavelength),2) + ')' + ': ' + filename  $
    ELSE bandname += filename
    
    ;����RGB������������ʾ������ȡ��ʾ����
    FOR i = 0, 2 DO BEGIN
      WIDGET_CONTROL, RGB_BAND[i], set_value = bandname[2-i]
      data[i,*,*] = REVERSE(ENVI_GET_DATA(fid = fid, dims = dims, pos = 2-i), 2)
    ENDFOR
    
    (*pState).OTREE.SETPROPERTY, BandRGB_name = REVERSE(bandname[0:2])
    (*pState).OTREE.SETPROPERTY, BandRGB_idx = [2,1,0]
    (*pState).OMODE.SETPROPERTY, RGB_idx = 0
    
  ENDIF ELSE BEGIN
  
    ;���������Ϊ1��С��3�����Զ�������ʾģʽΪGray Scale
    (*pState).OMODE.CHANGE2GRAY, ev
    bandname =  bandname + ': ' + filename
    
    ;������ʾ������
    (*pState).OMODE.GETPROPERTY, GRAY_BAND = GRAY_BAND
    (*pState).OMODE.GETPROPERTY, GRAY_BUTTON = GRAY_BUTTON
    WIDGET_CONTROL, GRAY_BAND, set_value = bandname
    WIDGET_CONTROL,GRAY_BUTTON, /set_button
    data = REVERSE(ENVI_GET_DATA(fid = fid, dims = dims, pos = 0), 2)
    
    (*pState).OTREE.SETPROPERTY, BandRGB_name = [bandname[0], '', '']
    (*pState).OTREE.SETPROPERTY, BandRGB_idx =[0L,-1,-1]
    (*pState).OMODE.SETPROPERTY, RGB_idx = 0
  ENDELSE
  
  ;�ļ��б�����´��ļ����Ͳ�����
  (*pState).OTREE.ADDFILE, ev, fid
  
  ;����DimsText����
  Interleave = ['BSQ', 'BIL', 'BIP']
  DimsText = STRTRIM(STRING(ns),2) + '��' + STRTRIM(STRING(nl),2) +  $
    ' (' + TYPENAME(data) + ') ' + '[' + Interleave[i_idx] + ']'
  (*pState).OMODE.GETPROPERTY, DIMSTEXT = DIMSTEXT_ID
  WIDGET_CONTROL, DIMSTEXT_ID, set_value = DimsText
  
  ;����oTree��Fidnow����
  (*pState).OTREE.SETPROPERTY, fidnow = fid
  (*pState).OTREE.SETPROPERTY, fidRGB = [fid, fid, fid]
  (*pState).OTREE.SETPROPERTY, BandGray_idx = 0
  (*pState).OTREE.SETPROPERTY, BandGray_name = bandname[0]
  
  ;���ð�ťЧ��
  IF (*pState).VIEWIDX EQ 0 THEN BEGIN
    FOREACH element, (*pState).TOOLGROUP DO WIDGET_CONTROL, element, /SENSITIVE
    IDLVIEWER_BNTSTYLE, ev, 1
  ENDIF
  
  ;������set��oImage��data����
  (*pState).OIMAGE.SETPROPERTY, data = LINEAR2(TEMPORARY(data))
  (*pState).VIEWFID = Fid
  
  ;����viewPlane_Rect��������ʾԭʼ��Сͼ��
  drawSizeALL = WIDGET_INFO((*pState).WDRAW, /geom)
  drawSize = [drawSizeALL.XSIZE, drawSizeALL.YSIZE]
  vp = INTARR(4)
  vp[0] = -(drawSize[0]-ns)/2
  vp[1] = -(drawSize[1]-nl)/2
  vp[2:3] = drawSize[0:1]
  
  (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
  
END




PRO IDLVIEWER_WHEEL, ev

  ;�����¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
  
  ;����ϵ��
  tmpScale = 1. - FLOAT(ev.CLICKS)/10
  
  ;��ǰ�����View�е�λ��
  oriLoc = [vp[0] + DOUBLE(ev.X)*vp[2]/vd[0],   $
    vp[1] + DOUBLE(ev.Y)*vp[3]/vd[1]]
    
  ;���ź�View��ʾ����
  vp[2:3] = vp[2:3]*tmpScale
  distance = (oriLoc - vp[0:1])*tmpScale
  vp[0:1] = oriLoc - distance
  
  ;������ʾ��Χ
  (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
  ;ˢ����ʾ
  IDLVIEWER_REFRESHDRAW, pState
  
END



PRO IDLVIEWER_ZOOM, ev, Zoomidx

  ;�����¼�
  ;Zoomidx�� 0 - �Ŵ� 1 - ��С
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
  
  ;����ϵ��
  CASE Zoomidx OF
    0: tmpScale = 1. - 1./10
    1: tmpScale = 1. + 1./10
  ENDCASE
  
  ;��ǰ�����View�е�λ��
  oriLoc = [vp[0] + (vd[0]/2)*vp[2]/vd[0],   $
    vp[1] + (vd[1]/2)*vp[3]/vd[1]]
  ;
  ;���ź�View��ʾ����
  vp[2:3] = vp[2:3]*tmpScale
  distance = (oriLoc - vp[0:1])*tmpScale
  vp[0:1] = oriLoc - distance
  
  ;������ʾ��Χ
  (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
  ;ˢ����ʾ
  IDLVIEWER_REFRESHDRAW, pState
  
END



PRO IDLVIEWER_ZOOMIN, ev

  ;����Ŵ��¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
  
  CASE ev.TYPE OF
    0: BEGIN
      ;�������
      IF ev.PRESS EQ 1 AND (*pState).VIEWIDX EQ 2 THEN BEGIN
        ;������£�Ϊ����Ŵ���׼��
        (*pState).MOUSESTATUS = 'Zoom'
        (*pState).PANSTATUS = [2, ev.X, ev.Y]
      ENDIF
      
    END
    
    1: BEGIN
      IF (*pState).MOUSESTATUS EQ 'Zoom' THEN BEGIN
      
        ;����ͷ�
        IF ev.RELEASE EQ 1 THEN BEGIN
          (*pState).PANSTATUS = 0
          (*pState).OPOLYGON.SETPROPERTY, hide = 1
          
          (*pState).OPOLYGON.GETPROPERTY, data = data
          
          IF N_ELEMENTS(data) EQ 0 THEN RETURN
          
          x1 = data[0,0]
          y1 = data[1,0]
          x2 = data[0,2]
          y2 = data[1,2]
          x_len = ABS(x1-x2)
          y_len = ABS(y1-y2)
          
          x_start = x1<x2
          y_start = y1<y2
          
          ;�������򳤿������ʾ����ıȽϣ�����oView��viewPlane_Rect����
          IF FLOAT(x_len)/y_len GE FLOAT(vd[0])/vd[1] THEN BEGIN
            y_tmp = FLOAT(x_len)*vd[1]/vd[0]
            y_start = y_start - (y_tmp-y_len)/2.
            scale = FLOAT(x_len)/vp[2]
            (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = [x_start, y_start, vp[2]*scale, vp[3]*scale]
          ENDIF ELSE BEGIN
            x_tmp = FLOAT(y_len)*vd[0]/vd[1]
            x_start = x_start - (x_tmp-x_len)/2.
            scale = FLOAT(y_len)/vp[3]
            (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = [x_start, y_start, vp[2]*scale, vp[3]*scale]
          ENDELSE
          
          OBJ_DESTROY, (*pState).OPOLYGON
          (*pState).OPOLYGON = OBJ_NEW('IDLgrPolygon')
          (*pState).OMODEL.ADD, (*pState).OPOLYGON
          IDLVIEWER_REFRESHDRAW, pState
          
        ENDIF
      ENDIF
    END
    
    2: BEGIN
      ;����ƶ�
      IF (*pState).MOUSESTATUS EQ 'Zoom' THEN BEGIN
        IF (*pState).PANSTATUS[0] EQ 2 THEN BEGIN
        
          ;����
          scale = vd[0]/vp[2]
          vp = vp*scale
          x1 = (*pState).PANSTATUS[1] + vp[0]
          y1 = (*pState).PANSTATUS[2] + vp[1]
          x2 = ev.X + vp[0]
          y2 = ev.Y + vp[1]
          
          ;��ʾPolygon��������Data����
          (*pState).OPOLYGON.SETPROPERTY, hide = 0
          (*pState).OPOLYGON.SETPROPERTY, data = [[x1,y1],[x1,y2],[x2,y2],[x2,y1]]/scale, style = 1, color = [255,0,0]
          
          ;������ʾ
          IDLVIEWER_REFRESHDRAW, pState
        ENDIF
      ENDIF
    END
    ELSE:
  ENDCASE
  
END




PRO IDLVIEWER_PAN, ev

  ;ƽ���¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
  
  CASE ev.TYPE OF
  
    0: BEGIN
      ;�������
      IF ev.PRESS EQ 1 AND (*pState).VIEWIDX EQ 1 THEN BEGIN
        ;������£�Ϊƽ����׼��
        (*pState).MOUSESTATUS = 'Pan'
        (*pState).PANSTATUS = [1, ev.X, ev.Y]
      ENDIF
      
      ;�м�����
      IF ev.PRESS EQ 2 THEN BEGIN
        ;�м����£�Ϊƽ����׼��
        (*pState).MOUSESTATUS = 'Pan'
        (*pState).OWIN.SETCURRENTCURSOR, 'move'
        (*pState).PANSTATUS = [1, ev.X, ev.Y]
      ENDIF
      
    END
    
    1: BEGIN
      IF (*pState).MOUSESTATUS EQ 'Pan' THEN BEGIN
        ;������м��ͷ�
        IF ev.RELEASE EQ 1 OR ev.RELEASE EQ 2 THEN BEGIN
          (*pState).PANSTATUS = 0
          IDLVIEWER_REFRESHDRAW, pState
          
          ;���������ʽ
          IF (*pState).VIEWIDX EQ 0 THEN (*pState).OWIN.SETCURRENTCURSOR, 'original'
          IF (*pState).VIEWIDX EQ 2 THEN (*pState).OWIN.SETCURRENTCURSOR, 'crosshair'
          
          OBJ_DESTROY, (*pState).OPOLYGON
          (*pState).OPOLYGON = OBJ_NEW('IDLgrPolygon', hide = 1)
          (*pState).OMODEL.ADD, (*pState).OPOLYGON
        ENDIF
      ENDIF
    END
    
    2: BEGIN
      ;����ƶ�
      IF (*pState).MOUSESTATUS EQ 'Pan' THEN BEGIN
        IF (*pState).PANSTATUS[0] EQ 1 THEN BEGIN
          ;�ƶ���ͼ
          distance = [ev.X, ev.Y] - (*pState).PANSTATUS[1:2]
          geoDis = [distance[0]*vp[2]/vd[0], distance[1]*vp[3]/vd[1]]
          ;
          vp[0:1] = vp[0:1] - geoDis
          (*pState).PANSTATUS[1:2] = [ev.X, ev.Y]
          ;
          (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
          (*pState).OWIN.SETCURRENTCURSOR, 'Move'
          
          ;������ʾ
          IDLVIEWER_REFRESHDRAW, pState
        ENDIF
      ENDIF
    END
    ELSE:
  ENDCASE
  
END




PRO IDLVIEWER_BNTSTYLE, ev, idx

  ;��ť��ʽ�¼�
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  FOR i=1,3 DO BEGIN
    WIDGET_CONTROL, (*pState).TOOLGROUP[i], set_value = (*pState).TOOLBMP[i*2-1], /bitmap
  ENDFOR
  IF idx EQ 0 THEN BEGIN
    FOREACH element, (*pState).TOOLGROUP DO WIDGET_CONTROL, element, SENSITIVE = 0
    WIDGET_CONTROL, (*pState).TOOLGROUP[0], SENSITIVE = 1
    RETURN
  ENDIF
  WIDGET_CONTROL, (*pState).TOOLGROUP[idx], set_value = (*pState).TOOLBMP[idx*2], /bitmap
  
END




PRO IDLVIEWER_RESIZE, ev

  ;�洰���С������ʾ����
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  baseSize = [ev.X > 600,  ev.Y>400]
  
  
  ;��ȡwDrawԭʼ��С
  info = WIDGET_INFO((*pState).WDRAW, /geom)
  oriSize = [info.XSIZE, info.YSIZE]
  
  ;��ȡ�µ�wDraw��С
  newSize = [baseSize[0]-(*pState).OFFSETXY[0], baseSize[1]-(*pState).OFFSETXY[1]]
  
  ;����viewPlane_Rect��dimension����Ӧ�´���
  (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp
  xCenter = vp[0] + vp[2]/2
  yCenter = vp[1] + vp[3]/2
  vp[2:3] = vp[2:3]*newSize/oriSize
  vp[0:1] = [xCenter, yCenter] - vp[2:3]/2
  
  (*pState).OVIEW.SETPROPERTY, viewPlane_Rect = vp
  (*pState).OVIEW.SETPROPERTY, dimension = newSize
  
  ;����Draw��Tree�����С
  WIDGET_CONTROL, (*pState).WDRAW, xsize = newSize[0], ysize = newSize[1]
  (*pState).OTREE.GETPROPERTY, ID = oTreeID
  WIDGET_CONTROL, oTreeID, ysize = newSize[1] - 177, xsize = 250
  
  ;ˢ����ʾ
  IDLVIEWER_REFRESHDRAW, pState
  
END




PRO IDLVIEWER_REFRESHDRAW, pState
  ;
  ;ˢ����ʾ
  (*pState).OWIN.DRAW, (*pState).OVIEW
END




PRO IDLVIEWER_USEENVI

  ;������
  COMPILE_OPT idl2
  
  CATCH, error_status
  IF error_status EQ 0 THEN BEGIN
    ENVI,/restore_base_save_files
    ENVI_BATCH_INIT
  ENDIF
  
  DEVICE, get_screen_size = screenSize
  
  ;ͼƬ��Դ·��
  ImagePath = FILE_DIRNAME(ROUTINE_FILEPATH('IDLViewer_UseEnvi')) + '\Resource'
  
  ;��������
  ScreenImage = ImagePath + '\IDLViewer_Splash.jpg'
  data = READ_IMAGE(ScreenImage)
  SplashBase = SHOW_SPLASH_SCREEN(data, /true)
  
  ;�����
  tlb = WIDGET_BASE(title = 'IDL Viewer',     $
    mbar = mbar,                              $
    uname = 'tlb',                            $
    /tlb_kill_request_events,                 $
    /tlb_size_events,                         $
    /column,                                  $
    map = 0,                                  $   ;����
    event_pro = 'tlb_event')
    
  ;�˵��� - File
  wFile = WIDGET_BUTTON(mbar, /menu, value = '�ļ�')
  wOpen = WIDGET_BUTTON(wFile,  value = '��', uname = 'Open')
  wClose = WIDGET_BUTTON(wFile, value = '�ر������ļ�', uname = 'CloseAllFiles', /sep)
  wExit = WIDGET_BUTTON(wFile, value = '�˳�', uname = 'Exit', /sep)
  
  
  ;*************************����Զ���˵�   ʾ������*****************************************
  wExample = WIDGET_BUTTON(mbar, /menu, value = 'ʾ������')
  wSU = WIDGET_BUTTON(wExample, value = '�����Ԫ�ֽ�', event_pro = 'Spectral_Unmixing')
  ;****************************************************************************************
  
  ;�˵��� - Help
  wHelp = WIDGET_BUTTON(mbar, /menu, value = '����')
  wHelpButton = WIDGET_BUTTON(wHelp, value = '����', uname = 'Help')
  wAbout = WIDGET_BUTTON(wHelp, value = '����IDL Viewer', uname = 'About')
  
  
  ;������ͼ��
  Toolbmp = ImagePath + ['\open.bmp',      $
    '\select.bmp', '\selected.bmp',        $
    '\hand.bmp', '\handed.bmp',            $
    '\zoom.bmp', '\zoomed.bmp',            $
    '\zoom_in.bmp', '\zoom_out.bmp',       $
    '\reset.bmp', '\fitwindow.bmp']
    
  ;������
  wToolbar = WIDGET_BASE(tlb, /row)
  wOpenTool = WIDGET_BUTTON(wToolbar, value = Toolbmp[0], uname = 'Open', /bitmap, /flat, tooltip = '���ļ�')
  wBlank = WIDGET_LABEL(wToolbar, value = ' ')
  wSelect = WIDGET_BUTTON(wToolbar, value = Toolbmp[1], uname = 'Select', /bitmap, /flat, SENSITIVE = 0, tooltip = 'ѡ��')
  wHand = WIDGET_BUTTON(wToolbar, value = Toolbmp[3], uname = 'Hand', /bitmap, /flat, SENSITIVE = 0, tooltip = 'ƽ��')
  wZoom = WIDGET_BUTTON(wToolbar, value = Toolbmp[5], uname = 'Zoom', /bitmap, /flat, SENSITIVE = 0, tooltip = '����Ŵ�')
  wZoomIn = WIDGET_BUTTON(wToolbar, value = Toolbmp[7], uname = 'Zoomin', /bitmap, /flat, SENSITIVE = 0, tooltip = '�Ŵ�')
  wZoomOut = WIDGET_BUTTON(wToolbar, value = Toolbmp[8], uname = 'Zoomout', /bitmap, /flat, SENSITIVE = 0, tooltip = '��С')
  wReset = WIDGET_BUTTON(wToolbar, value = Toolbmp[9], uname = 'ResetView', /bitmap, /flat, SENSITIVE = 0, tooltip = '����')
  wFitWin = WIDGET_BUTTON(wToolbar, value = Toolbmp[10], uname = 'FitWindow', /bitmap, /flat, SENSITIVE = 0, tooltip = '��Ӧ����')
  
  ToolGroup = [wOpenTool, wSelect, wHand, wZoom, wZoomin, wZoomout, wReset, wFitWin]
  
  BaseSize = screenSize*2/3
  BaseSize[1] -= screenSize[1]/10
  
  wBase = WIDGET_BASE(tlb, /row)
  
  ;�ļ��б� - IDLgrTree
  wTreeBase = WIDGET_BASE(wBase, /column, /frame)
  oTree = OBJ_NEW('IDLgrTree',  $
    parent = wTreeBase,   $
    xsize = 250,  $
    ysize = BaseSize[1] - 177,  $
    uname = 'wTree',   $
    event_pro = 'IDLViewer_oTree')
    
  ;��ʾģʽ - DisplayMode
  oMode = OBJ_NEW('DisplayMode', parent = wTreeBase)
  
  ;��ͼ�� - IDLgrDraw
  wDrawBase = WIDGET_BASE(wBase, /column, /frame)
  wDraw = WIDGET_DRAW(wDrawBase, graphics_level = 2,    $
    xsize = BaseSize[0]-250,                            $
    ysize = BaseSize[1],                                $
    retain = 0,                                         $
    uname = 'wDraw',                                    $
    /expose_events,                                     $
    /button_events,                                     $
    /wheel_events,                                      $
    /motion_events)
    
  DrawSize = [BaseSize[0]-250, BaseSize[1]]
  
  ;��ȡ�����б��ͼ�꣨�����ͼ��ȫɫ��DEM�����Ρ�ͶӰ��
  iconPath = ImagePath + ['\img.bmp', '\gray.bmp', '\class.bmp', '\dem.bmp', '\band.bmp', '\proj.bmp']
  iconBmp = PTRARR(6)
  FOR i = 0,5 DO BEGIN
    iconBmp[i] = PTR_NEW(TRANSPOSE(READ_BMP(iconPath[i], /rgb),[1,2,0]), /no_copy)
  ENDFOR
  
  WAIT, 2
  WIDGET_CONTROL, SplashBase, /destroy
  WIDGET_CONTROL, tlb, /realize
  
  ;������������ʾ����Ļ�м�
  geom = WIDGET_INFO(tlb, /Geometry)
  
  xCenter = screenSize[0] * 0.5
  yCenter = screenSize[1] * 0.5
  
  xHalfSize = geom.SCR_XSIZE / 2
  yHalfSize = geom.SCR_YSIZE / 2
  
  XOffset = (xCenter - xHalfSize)
  YOffset = (yCenter - yHalfSize)
  
  WIDGET_CONTROL, tlb, XOffset=XOffset, YOffset=YOffset
  
  ;��ʾ����
  WIDGET_CONTROL, tlb, /map
  
  offsetXY = [geom.XSIZE, geom.YSIZE] - Drawsize
  
  WIDGET_CONTROL, wDraw, get_value = oWin
  oWin.SETCURRENTCURSOR, 'original'
  
  oView = OBJ_NEW('IDLgrView',  $
    color = [255,255,255],      $
    dimensions = DrawSize)
    
  oPolygon = OBJ_NEW('IDLgrPolygon')
  
  oModel = OBJ_NEW('IDLgrModel')
  oImage = OBJ_NEW('IDLgrImage')
  
  oView.ADD, oModel
  oModel.ADD, oImage
  oModel.ADD, oPolygon
  
  oWin.DRAW, oView
  
  
  ;��ʼ��·����ENVI�Դ�����·����
  curPath = envi_get_path() + '\data'
  
  pState = PTR_NEW({         $
    oWin:oWin,               $
    oTree:oTree,             $
    oMode:oMode,             $
    oView:oView,             $
    oModel:oModel,           $
    oImage:oImage,           $
    oPolygon:oPolygon,       $
    wDraw:WDRAW,             $
    panStatus:DBLARR(3),     $
    mouseStatus:'',          $
    offsetXY:offsetXY,       $
    viewFid:0L,              $  ;IDLgrView����ʾ�ļ�ID
    iconBmp:iconBmp,         $
    ImagePath:ImagePath,     $
    curPath:curPath,         $
    viewIdx:0,               $  ;ѡ���֡��Ŵ���С��ʵ�����ء���Ӧ��Ļ
    oCursorData:OBJ_NEW(),   $
    ToolGroup:ToolGroup,     $
    Toolbmp:Toolbmp          $
    })
    
  WIDGET_CONTROL, tlb, set_uvalue = pState
  
  XMANAGER, 'idlviewer', tlb, /no_block
  
  
END