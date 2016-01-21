;ENVI二次开发代码共享之“面向对象特征提取”
PRO cal_FX,in_file,out_file,scale_level,merge_level,ruleset_filename 
  CATCH, Error_status 
  errorshow = 'Sorry to see the error,'+ $ 
    ' please send the error Information to "dongyq@esrichina-bj.cn"' 
  IF Error_status NE 0 THEN BEGIN 
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$ 
      !ERROR_STATE.MSG,/error,title = '错误提示!') 
    return 
  ENDIF 
  ; 打开文件 
  ENVI_OPEN_FILE, in_file, r_fid=in_fid 
  ;文件打开出错则退出 
  IF (in_fid EQ -1) THEN RETURN 
  ;获取文件信息 
  ENVI_FILE_QUERY, in_fid, dims=dims, nb=nb 
  IF  ~FILE_TEST(ruleset_filename) THEN RETURN 
  IF ~FILE_TEST(FILE_DIRNAME(out_file),/directory) THEN FILE_MKDIR,FILE_DIRNAME(out_file) 
  ; 执行FX. 
  ENVI_DOIT, 'envi_fx_doit', $ 
    pos=LINDGEN(nb), $ 
    dims=dims, $ 
    fid=in_fid, $ 
    scale_level=scale_level, $ 
    merge_level=merge_level, $ 
    vector_filename=out_file, $ 
    conf_threshold=0.10, $ 
    ruleset_filename=ruleset_filename, $ 
    r_fid = out_fid 
END