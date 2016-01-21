PRO CURSORDATA, ev

  ;鼠标取值界面事件
  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  tagName = TAG_NAMES(ev, /structure_name)
  
  ;关闭程序事件
  IF tagName EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    OBJ_DESTROY, (*pState).OCURSORDATA
    WIDGET_CONTROL, ev.TOP, /destroy
    
    CASE (*pState).VIEWIDX OF
    
      0: (*pState).OWIN.SETCURRENTCURSOR, 'Original'
      1: (*pState).OWIN.SETCURRENTCURSOR, 'Move'
      
    ENDCASE
    
    RETURN
  ENDIF
END



PRO CURSORDATA::SetValue, ev

  COMPILE_OPT idl2
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  (*pState).OVIEW.GETPROPERTY, viewPlane_Rect = vp, dimensions = vd
  
  ;由ev.X和ev.Y得到真实行列号
  (*pState).OTREE.GETPROPERTY, FIDNOW = fid
  
  ENVI_FILE_QUERY, fid,           $
    bname = bandname,             $ ;波段名
    sname = filename,             $ ;文件名
    nb = nb,                      $ ;波段数
    ns = ns,                      $ ;列数
    nl = nl,                      $ ;行数
    dims = dims,                  $ ;Dimensions
    wl = wavelength,              $ ;波长
    xstart = xstart,              $
    ystart = ystart,              $
    data_type = dtype               ;数据类型（索引）
    
  scale = vd[0]/vp[2]
  vp = vp*scale
  xy = [ev.X+vp[0], ev.Y+vp[1]]
  xy = xy/scale
  xy = FLOOR([xy[0], nl-xy[1]])
  
  ;获取行列号为xy的像元经纬度
  ENVI_CONVERT_FILE_COORDINATES, fid, xy[0], xy[1], xmap, ymap, /to_map
  proj = ENVI_GET_PROJ(fid)
  
  (*pState).OMODE.GETPROPERTY, index = index
  
  IF proj.PROJ EQ 'Arbitrary' THEN BEGIN
  
    ;如果图像不包含投影信息，只显示行列号
    ;
    ;根据显示模式不同设置鼠标取值
    CASE index OF
    
      ;灰度显示模式
      0: BEGIN
        (*pState).OTREE.GETPROPERTY, BandGray_idx = BandGray_idx
        data = ENVI_GET_DATA(fid = fid, dims = dims, pos = BandGray_idx)
        IF dtype EQ 1 THEN data = FIX(data)
        IF xy[0] GE 0 AND xy[0] LT ns AND xy[1] GE 0 AND xy[1] LT nl THEN BEGIN
          value = 'Location: (' + STRTRIM(STRING(xy[0]+xstart), 2) + ',' + STRTRIM(STRING(xy[1]+ystart), 2) + ')' + STRING(13B) + $
            'Data: ' + STRTRIM(STRING(data[xy[0],xy[1]]), 2)
            
        ENDIF ELSE BEGIN
          value = 'Location: (NaN,NaN)' + STRING(13B) +  $
            'Data: NaN'
            
        ENDELSE
      END
      
      ;RGB显示模式
      1: BEGIN
        (*pState).OTREE.GETPROPERTY, FidRGB = FidRGB
        (*pState).OTREE.GETPROPERTY, BandRGB_idx = BandRGB_idx
        
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
        
        ;如果RGB三通道维度不同则返回
        IF TOTAL(ns_rgb EQ ns_rgb[0]) NE 3 OR TOTAL(nl_rgb EQ nl_rgb[0]) NE 3 THEN RETURN
        
        DN = MAKE_ARRAY(3, type = dtype>2)
        
        IF xy[0] GE 0 AND xy[0] LT ns AND xy[1] GE 0 AND xy[1] LT nl THEN BEGIN
        
          FOR i=0,2 DO BEGIN
            DN[i] = (ENVI_GET_DATA(fid = fid, dims = dims, pos = BandRGB_idx[i]))[xy[0], xy[1]]
          ENDFOR
          DN = STRTRIM(STRING(DN), 2)
          
          value = 'Location: (' + STRTRIM(STRING(xy[0]+xstart), 2) + ',' + STRTRIM(STRING(xy[1]+ystart), 2) + ')' + STRING(13B) + $
            'Data: R:' + DN[0] + ' G:' + DN[1] + ' B:' + DN[2]
            
        ENDIF ELSE BEGIN
          value = 'Location: (NaN,NaN)' + STRING(13B) +  $
            'Data: NaN'
            
        ENDELSE
        
      END
    ENDCASE
    
  ENDIF ELSE BEGIN
  
    ;获取投影信息
    proj_str = ENVI_GET_PROJECTION(fid = fid, pixel_size = Pixel)
    
    ;判断经纬度正负
    lon_idx = (proj.UL_GEO)[0] LT 0 ? 'W' : 'E'
    lat_idx = (proj.UL_GEO)[3] LT 0 ? 'S' : 'N'
    
    ;地理坐标
    map = STRTRIM(STRING(ABS(xmap)),2) + lon_idx + ', ' + STRTRIM(STRING(ABS(ymap)),2) + lat_idx
    
    oproj = ENVI_PROJ_CREATE(/geographic)
    
    ;地理坐标转换为经纬度
    ENVI_CONVERT_PROJECTION_COORDINATES,  $
      xmap, ymap, proj_str, $
      oXmap, oYmap, oproj
      
    ;经纬度转化为度分秒格式
    UL_Geo = [CONVERT_LATLON(oXmap),CONVERT_LATLON(oYmap)]
    
    ul_geo_str = STRTRIM(STRING(ABS(FIX(UL_GEO))),2)
    
    ;经纬度
    LL = ul_geo_str[0] + '°' +  $
      ul_geo_str[1] + "'" + $
      NUM_FORMATTER(ABS((UL_GEO)[2])) + '"' + lon_idx + ', ' +  $
      ul_geo_str[3] + '°' +  $
      ul_geo_str[4] + "'" + $
      NUM_FORMATTER(ABS((UL_GEO)[5])) + '"' + lat_idx
      
      
    ;根据显示模式不同设置鼠标取值
    CASE index OF
    
      ;灰度显示模式
      0: BEGIN
        (*pState).OTREE.GETPROPERTY, BandGray_idx = BandGray_idx
        data = ENVI_GET_DATA(fid = fid, dims = dims, pos = BandGray_idx)
        IF dtype EQ 1 THEN data = FIX(data)
        IF xy[0] GE 0 AND xy[0] LT ns AND xy[1] GE 0 AND xy[1] LT nl THEN BEGIN
          value = 'Proj: ' + proj.PROJ + STRING(13B) + $
            'Location: (' + STRTRIM(STRING(xy[0]+xstart), 2) + ',' + STRTRIM(STRING(xy[1]+ystart), 2) + ')' + STRING(13B) + $
            'Map: ' + map + STRING(13B) + $
            'Lon/Lat: ' + LL + STRING(13B) + $
            'Data: ' + STRTRIM(STRING(data[xy[0],xy[1]]), 2)
            
        ENDIF ELSE BEGIN
          value = 'Proj: NaN' + STRING(13B) + $
            'Location: (NaN,NaN)' + STRING(13B) +  $
            'Map: NaN' + STRING(13B) + $
            'Lon/Lat: NaN' + STRING(13B) + $
            'Data: NaN'
            
        ENDELSE
      END
      
      ;RGB显示模式
      1: BEGIN
        (*pState).OTREE.GETPROPERTY, FidRGB = FidRGB
        (*pState).OTREE.GETPROPERTY, BandRGB_idx = BandRGB_idx
        
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
        
        ;如果RGB三通道维度不同则返回
        IF TOTAL(ns_rgb EQ ns_rgb[0]) NE 3 OR TOTAL(nl_rgb EQ nl_rgb[0]) NE 3 THEN RETURN
        
        DN = MAKE_ARRAY(3, type = dtype>2)
        
        IF xy[0] GE 0 AND xy[0] LT ns AND xy[1] GE 0 AND xy[1] LT nl THEN BEGIN
        
          FOR i=0,2 DO BEGIN
            DN[i] = (ENVI_GET_DATA(fid = fid, dims = dims, pos = BandRGB_idx[i]))[xy[0], xy[1]]
          ENDFOR
          DN = STRTRIM(STRING(DN), 2)
          
          value = 'Proj: ' + proj.PROJ + STRING(13B) + $
            'Location: (' + STRTRIM(STRING(xy[0]+xstart), 2) + ',' + STRTRIM(STRING(xy[1]+ystart), 2) + ')' + STRING(13B) + $
            'Map: ' + map + STRING(13B) + $
            'Lon/Lat: ' + LL + STRING(13B) + $
            'Data: R:' + DN[0] + ' G:' + DN[1] + ' B:' + DN[2]
            
        ENDIF ELSE BEGIN
          value = 'Proj: NaN' + STRING(13B) + $
            'Location: (NaN,NaN)' + STRING(13B) +  $
            'Map: NaN' + STRING(13B) + $
            'Lon/Lat: NaN' + STRING(13B) + $
            'Data: NaN'
            
        ENDELSE
        
      END
    ENDCASE
  ENDELSE
  WIDGET_CONTROL, self.LABEL_ID, set_value = value
  
  
END



PRO CURSORDATA::SetProperty, label_ID = Label_ID, wBase = wBase

  IF N_ELEMENTS(Label_ID) NE 0 THEN self.LABEL_ID = Label_ID
  IF N_ELEMENTS(wBase) NE 0 THEN self.WBASE = wBase
  
END




PRO CURSORDATA::GetProperty, parent = parent, wBase = wBase

  parent = self.PARENT
  wBase = self.WBASE
  
END




PRO CURSORDATA::Create

  ;搭建界面
  WIDGET_CONTROL, self.PARENT, get_uvalue = pState
  
  wBase = WIDGET_BASE(group_leader = self.PARENT,   $
    /floating, title = '鼠标取值',  $
    event_pro = 'cursorData',     $
    /tlb_kill_request_events)
    
  wLabel = WIDGET_LABEL(wBase, xsize = 320, ysize = 130,   $
    xoffset = 5, yoffset = 5)
    
  WIDGET_CONTROL, wBase, /realize
  
  WIDGET_CONTROL, wBase, set_uvalue = pState
  
  self.SETPROPERTY, Label_ID = wLabel
  self.SETPROPERTY, wBase = wBase
  
END



FUNCTION CURSORDATA::INIT, parent = parent

  self.PARENT = parent
  self.CREATE
  
  RETURN, 1
  
END



PRO CURSORDATA__DEFINE

  structure = {CursorData,    $
    wBase:0,                  $
    parent:0,                 $
    label_ID:0                $
    }
    
END