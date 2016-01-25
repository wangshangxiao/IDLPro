;
;
;待镶嵌数据参数设置
pro georef_mosaic_setup, fids=fids, dims=dims, out_ps=out_ps, $
  xsize=xsize, ysize=ysize, x0=x0, y0=y0, map_info=map_info
  compile_opt strictarr, hidden
  ;
  if keyword_set(dims) then $
    if n_elements(fids) ne n_elements(dims[0,*]) then dims=0
  ;
  if n_elements(fids) lt 2 then begin
    xsize = -1
    ysize = -1
    x0 = -1
    y0 = -1
    return
  endif

  ; if no DIMS passed in
  nfiles = n_elements(fids)
  if (keyword_set(dims) eq 0) then begin
    dims = fltarr(5, nfiles)
    for i=0, nfiles-1 do begin
      envi_file_query, fids[i], ns=ns, nl=nl
      dims[*,i] = [-1L, 0, ns-1, 0, nl-1]
    endfor
  endif

  ; - compute the size of the output mosaic (xsize and ysize)
  ; - store the map coords of the UL corner of each image since you'll need it later
  UL_corners_X = dblarr(nfiles)
  UL_corners_Y = dblarr(nfiles)
  east = -1e34
  west = 1e34
  north = -1e34
  south = 1e34
  for i=0,nfiles-1 do begin
    pts = [ [dims[1,i], dims[3,i]],   $   ; UL
      [dims[2,i], dims[3,i]],   $   ; UR
      [dims[1,i], dims[4,i]],   $   ; LL
      [dims[2,i], dims[4,i]] ]   ; LR
    envi_convert_file_coordinates, fids[i], pts[0,*], pts[1,*], xmap, ymap, /to_map
    UL_corners_X[i] = xmap[0]
    UL_corners_Y[i] = ymap[0]
    east  = east > max(xmap)
    west = west < min(xmap)
    north = north > max(ymap)
    south = south < min(ymap)
  endfor
  xsize = east - west
  ysize = north - south
  ;
  xsize_pix = fix( xsize/out_ps[0] )+1
  ysize_pix = fix( ysize/out_ps[1])+1

  ; to make things easy, create a temp image that's got a header
  ; that's the same as the output mosaic image
  proj = envi_get_projection(fid=fids[0])
  map_info = envi_map_info_create(proj=proj, mc=[0,0,west,north], ps=out_ps)
  temp = bytarr(10,10)
  envi_enter_data, temp, map_info=map_info, /no_realize, r_fid=tmp_fid

  ; find the x and y offsets for the images
  x0 = lonarr(nfiles)
  y0 = lonarr(nfiles)
  for i=0,nfiles-1 do begin
    envi_convert_file_coordinates, tmp_fid, xpix, ypix, UL_corners_X[i], UL_corners_Y[i]
    x0[i] = xpix
    y0[i] = ypix
  endfor

  ; delete the tmp file
  envi_file_mng, id=tmp_fid, /remove, /no_warning

end

;打开tif功能代码
function openTifTfW, infile
  compile_opt idl2
  i = 0
  files= infile
  ;
  potPos = StrPos(files[i],'.',/reverse_search)
  ;
  openR,lun,StrMid(files[i],0,potPos[0])+'.tfw',/get_lun
  if lun NE -1 then begin
    parm = fltarr(6)
    readf,lun,parm
    ENVI_OPEN_FILE,files[i],r_fid = fid,/NO_REALIZE
    ENVI_FILE_QUERY,fid, fname=fname, $
      BNAMES = BNAMES, $
      DESCRIP = DESCRIP, $
      ns=ns, nl=nl, nb=nb, $
      interleave=interleave, data_type=data_type, $
      offset=offset
    map_info = ENVI_MAP_INFO_CREATE(/ARBITRARY,$
      /MAP_BASED,$
      mc = [-1,-1,parm[4:5]],ps = ABS([parm[0],parm[3]]))

    ENVI_FILE_MNG,id=fid,/remove
    ENVI_SETUP_HEAD, fname=fname, $
      FILE_TYPE = ENVI_FILE_TYPE('TIFF'),$
      BNAMES = BNAMES, $
      DESCRIP = DESCRIP, $
      ns=ns, nl=nl, nb=nb, $
      interleave=interleave, data_type=data_type, $
      offset=offset,$
      map_info = map_info, /write
    free_lun,lun    ;
    envi_open_file, fname,r_fid = fid
  endif else envi_open_file, fname,r_fid = fid

  return,fid
end

;
;如果是jpg+jpw时，转换为同名tif
;
Function openJPGTfW, infile
  compile_opt idl2
  ;
  cfg = envi_get_configuration_values()
  outpath = cfg.CURRENT_OUTPUT_DIRECTORY+path_sep()
  basefile = file_basename(infile)
  rootpath = file_dirname(infile)+path_sep()
  ;
  potPos = StrPos(basefile,'.',/reverse_search)
  file_copy, rootpath+StrMid(basefile,0,potPos[0])+'.jgw', outpath+StrMid(basefile,0,potPos[0])+'.tfw',/overwrite
  READ_JPEG, infile, data, TRUE=1
  write_tiff,outpath+StrMid(basefile,0,potPos[0])+'.tif',data

  return,openTifTfW(outpath+StrMid(basefile,0,potPos[0])+'.tif')
end

;打开jpg+jpw功能
function open_rasterfile, infile
  ;
  basename = file_basename(infile)
  posLoc = StrPos(basename,'.',/reverse_s)
  if posLoc eq -1 then begin
    envi_open_file, infile,r_fid = fid
    return,fid
  endif
  case StrUpCase(strMid(basename,posLoc+1,StrLen(basename)-posLoc)) of
    'JPG':fid = openJPGTfW(infile)
    'JPEG': fid = openJPGTfW(infile)
    'TIF':fid = openTifTfW(infile)
    'TIFF':fid = openTifTfW(infile)

    else:envi_open_file, infile,r_fid = fid
  endcase
  return,fid
end

;海量数据批处理功能
pro mosaic_building
  compile_opt idl2

  ackground=0
  crop=[0,0,0,0,0]

  rasterfilenames=envi_pickfile(title='Please select input raster(s):',/multiple_files, $
    FILTER =[['*.img', '*.tif;*.tiff','*.jpg;*.jpeg', '*.*'], $
  [' IMG文件', ' TIFF文件',' JPEG文件',  'All files']])
  if rasterfilenames[0] eq '' then return

  numfiles=n_elements(rasterfilenames)
  rasterfids=lonarr(numfiles)
  output=envi_pickfile(title='Output Mosaick Filename:')

  ;参数输入界面
  tlb = WIDGET_AUTO_BASE(title='ENVI_Batch_Mosaic(V1.5)')
  we = WIDGET_PARAM(tlb, dt=4, field=3,  $
    default=-999., uvalue='param', /auto, $
    PROMPT ='Data Value to Ignore')
  result = AUTO_WID_MNG(tlb)
  IF (result.accept EQ 0) THEN RETURN
  ignoreValue = result.param
  ;栅格文件打开
  tempfid = open_rasterfile( rasterfilenames[0])
  rasterfids[0]=tempfid
  envi_file_query,tempfid,nb=nb,ns=tempns,nl=tempnl,data_type=data_type
  map_info = envi_get_map_info(fid=tempfid)
  out_ps=map_info.ps[0:1]

  if n_elements(pos) eq 0 OR n_elements(pos) gt nb then pos=lindgen(nb)
  posarr=lonarr(n_elements(pos),numfiles)
  for i=0,numfiles-1 do posarr[*,i]=pos

  dimsarr=lonarr(5,numfiles)
  dimsarr[*,0]=[-1,0, tempns-1,0, tempnl-1]-crop
  use_see_through = lonarr(numfiles)
  for i=1,numfiles-1 do begin
    ;打开文件ID
    tempfid = open_rasterfile( rasterfilenames[i])
    rasterfids[i]=tempfid
    envi_file_query,tempfid,nb=nb,ns=tempns,nl=tempnl
    dimsarr[*,i]=[-1,0, tempns-1,0, tempnl-1]-crop
  endfor

  georef_mosaic_setup, fids=rasterfids, out_ps=out_ps, dims=dimsarr, xsize=xsize, ysize=ysize,$
    x0=x0, y0=y0, map_info=map_info
  ;
  USE_SEE_THROUGH = intarr(N_Elements(rasterfids))+1
  seeTv = Make_Array(N_Elements(rasterfids),value =ignoreValue )
  envi_doit, 'mosaic_doit', fid=rasterfids, pos=posarr, $
    dims=dimsarr, out_name=output, xsize=xsize, $
    ysize=ysize, x0=x0, y0=y0, georef=1, $
    out_dt=data_type, pixel_size=out_ps, $
    background=ignoreValue,SEE_THROUGH_VAL=seeTv,$
    USE_SEE_THROUGH = USE_SEE_THROUGH,$
    map_info=map_info
envi_batch_exit
end
;定义菜单在Basic Tools - Mosaicing的最下方，即
PRO Mosaic_Batch_define_buttons, buttonInfo
  compile_opt idl2
  envi_define_menu_button, buttonInfo, event_pro='Mosaic_Batch',$
    uvalue='none', $
    position='after', $
    ref_value='Tiled WorldView Product', $
    /sibling, $
    /SEPARATOR ,$;加分隔线
    value='Mosaic Batch'      ;;

END
;
;可双击运行
;也可在ENVI下运行，支持jpg、tiff、img等ENVI支持的格式

;主调用函数，需要编译后
;保存为Mosic_Batch.sav
pro Mosaic_Batch,event
  CATCH, Error_status
  errorshow = 'Sorry to see the error,'+ $
    ' please send the error Information to "dongyq@esrichina-bj.cn"'
  IF Error_status NE 0 THEN BEGIN
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$
      !ERROR_STATE.MSG,/error,title = '错误提示!')
    return
  ENDIF
  compile_opt idl2
  
  ENVI,/restore_base_save_files
  ENVI_batch_init,LOG_FILE = 'Mosaic_Batch.log';NO_STATUS_WINDOW=1
  ;调用功能函数
  mosaic_building
end
