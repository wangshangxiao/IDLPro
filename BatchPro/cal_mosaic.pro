;+
;  《IDL程序设计》
;   -数据可视化与ENVI二次开发
;        
; 示例代码
;
; 作者: 董彦卿
;
; 联系方式：sdlcdyq@sina.com
;-
;+
;ENVI二次开发功能代码
;
;Author: DYQ
;问题讨论：
; http://hi.baidu.com/dyqwrp
; http://bbs.esrichina-bj.cn/ESRI/?fromuid=9806
;描述：
; 镶嵌
;
;调用方法：
; cal_mosaic, txtFile
; 输入为txt配置文件完整路径
;
;-
PRO GEOREF_MOSAIC_SETUP, fids=fids, dims=dims, out_ps=out_ps, $
    xsize=xsize, ysize=ysize, x0=x0, y0=y0, map_info=map_info
  COMPILE_OPT IDL2
  
  IF KEYWORD_SET(dims) THEN $
    IF N_ELEMENTS(fids) NE N_ELEMENTS(dims[0,*]) THEN dims=0
  ;
  IF N_ELEMENTS(fids) LT 2 THEN BEGIN
    xsize = -1
    ysize = -1
    x0 = -1
    y0 = -1
    RETURN
  ENDIF
  ; if no DIMS passed in
  ;
  nfiles = N_ELEMENTS(fids)
  IF (KEYWORD_SET(dims) EQ 0) THEN BEGIN
    dims = FLTARR(5, nfiles)
    FOR i=0, nfiles-1 DO BEGIN
      ENVI_FILE_QUERY, fids[i], ns=ns, nl=nl
      dims[*,i] = [-1L, 0, ns-1, 0, nl-1]
    ENDFOR
  ENDIF
  UL_corners_X = DBLARR(nfiles)
  UL_corners_Y = DBLARR(nfiles)
  east = -1e34
  west = 1e34
  north = -1e34
  south = 1e34
  FOR i=0,nfiles-1 DO BEGIN
    pts = [ [dims[1,i], dims[3,i]],   $   ; UL
      [dims[2,i], dims[3,i]],   $   ; UR
      [dims[1,i], dims[4,i]],   $   ; LL
      [dims[2,i], dims[4,i]] ]   ; LR
    ENVI_CONVERT_FILE_COORDINATES, fids[i], pts[0,*], pts[1,*], xmap, ymap, /to_map
    UL_corners_X[i] = xmap[0]
    UL_corners_Y[i] = ymap[0]
    east  = east > MAX(xmap)
    west = west < MIN(xmap)
    north = north > MAX(ymap)
    south = south < MIN(ymap)
  ENDFOR
  xsize = east - west
  ysize = north - south
  ;
  xsize_pix = FIX( xsize/out_ps[0] )+1
  ysize_pix = FIX( ysize/out_ps[1])+1
  ;
  proj = ENVI_GET_PROJECTION(fid=fids[0])
  map_info = ENVI_MAP_INFO_CREATE(proj=proj, mc=[0,0,west,north], ps=out_ps)
  temp = BYTARR(10,10)
  ENVI_ENTER_DATA, temp, map_info=map_info, /no_realize, r_fid=tmp_fid
  ; find the x and y offsets for the images
  ;
  x0 = LONARR(nfiles)
  y0 = LONARR(nfiles)
  FOR i=0,nfiles-1 DO BEGIN
    ENVI_CONVERT_FILE_COORDINATES, tmp_fid, xpix, ypix, UL_corners_X[i], UL_corners_Y[i]
    x0[i] = xpix
    y0[i] = ypix
  ENDFOR
  ; delete the tmp file
  ENVI_FILE_MNG, id=tmp_fid, /remove, /no_warning
END

;
PRO MOSAIC_FILES,info,pos=pos,rasterfilenames=rasterfilenames,output=output,crop=crop,$
    file_pattern=file_pattern
  COMPILE_OPT idl2
  
  IF N_ELEMENTS(background) EQ 0 THEN background=0
  IF N_ELEMENTS(crop) NE 4 THEN crop=[0,0,0,0,0] ELSE crop=[0,crop]*[0,-1,1,-1,1]
  
  IF N_ELEMENTS(rasterfilenames) EQ 0 AND N_ELEMENTS(file_pattern) EQ 0 THEN BEGIN
    IF rasterfilenames[0] EQ '' THEN RETURN
  ENDIF
  IF N_ELEMENTS(file_pattern) NE 0 THEN BEGIN
    rasterfilenames=FILE_SEARCH(file_pattern)
  ENDIF
  numfiles=N_ELEMENTS(rasterfilenames)
  rasterfids=LONARR(numfiles)
  
  IF N_ELEMENTS(output) EQ 0 THEN BEGIN
    output=ENVI_PICKFILE(title='Output Mosaick Filename:')
    IF output EQ '' THEN RETURN
  ENDIF
  ;
  tlb = WIDGET_AUTO_BASE(title='镶嵌参数设置')
  we = WIDGET_PARAM(tlb, dt=4, field=3,  $
    default=-999., uvalue='param', /auto, $
    PROMPT ='忽略数据值')
  result = AUTO_WID_MNG(tlb)
  IF (result.ACCEPT EQ 0) THEN RETURN
  ignoreValue = result.PARAM
  
  ENVI_OPEN_FILE, rasterfilenames[0], r_fid=tempfid
  rasterfids[0]=tempfid
  ENVI_FILE_QUERY,tempfid,nb=nb,ns=tempns,nl=tempnl,data_type=data_type
  map_info = ENVI_GET_MAP_INFO(fid=tempfid)
  out_ps=map_info.PS[0:1]
  IF N_ELEMENTS(pos) EQ 0 OR N_ELEMENTS(pos) GT nb THEN pos=LINDGEN(nb)
  posarr=LONARR(N_ELEMENTS(pos),numfiles)
  FOR i=0,numfiles-1 DO posarr[*,i]=pos
  dimsarr=LONARR(5,numfiles)
  dimsarr[*,0]=[-1,0, tempns-1,0, tempnl-1]-crop
  use_see_through = LONARR(numfiles)
  FOR i=1,numfiles-1 DO BEGIN
    ENVI_OPEN_FILE, rasterfilenames[i], r_fid=tempfid
    rasterfids[i]=tempfid
    ENVI_FILE_QUERY,tempfid,nb=nb,ns=tempns,nl=tempnl
    dimsarr[*,i]=[-1,0, tempns-1,0, tempnl-1]-crop
  ENDFOR
  GEOREF_MOSAIC_SETUP, fids=rasterfids, out_ps=out_ps, dims=dimsarr, xsize=xsize, ysize=ysize,$
    x0=x0, y0=y0, map_info=map_info  ;
  USE_SEE_THROUGH = INTARR(N_ELEMENTS(rasterfids))+1
  seeTv = MAKE_ARRAY(N_ELEMENTS(rasterfids),value =ignoreValue )
  ENVI_DOIT, 'mosaic_doit', fid=rasterfids, pos=posarr, $
    dims=dimsarr, out_name=output, xsize=xsize, $
    ysize=ysize, x0=x0, y0=y0, georef=1, $
    out_dt=data_type, pixel_size=out_ps, $
    background=ignoreValue,SEE_THROUGH_VAL=seeTv,$
    USE_SEE_THROUGH = USE_SEE_THROUGH,$
    map_info=map_info
END


PRO CAL_MOSAIC, txtFile
  COMPILE_OPT idl2
  CATCH, Error_status
  errorshow = 'Sorry to see the error,'+ $
    ' please send the error Information to "dongyq@esrichina-bj.cn"'
  IF Error_status NE 0 THEN BEGIN
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$
      !ERROR_STATE.MSG,/error,title = '错误提示!')
    RETURN
  ENDIF
  
  ;如果文件不存在则返回
  IF ~FILE_TEST(txtFile) THEN RETURN;
  ;解析
  nFiles = FILE_LINES(txtFile)
  ;
  filenames = STRARR(nFiles)
  OPENR,lun,txtfile,/get_
  READF,lun,filenames
  FREE_LUN,lun
  ;
  MOSAIC_FILES,rasterfilenames = filenames[0:nFiles-2],output = filenames[nFiles-1]
END