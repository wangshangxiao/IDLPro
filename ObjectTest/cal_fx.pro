;ENVI���ο������빲��֮���������������ȡ��
PRO cal_FX,in_file,out_file,scale_level,merge_level,ruleset_filename 
  CATCH, Error_status 
  errorshow = 'Sorry to see the error,'+ $ 
    ' please send the error Information to "dongyq@esrichina-bj.cn"' 
  IF Error_status NE 0 THEN BEGIN 
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$ 
      !ERROR_STATE.MSG,/error,title = '������ʾ!') 
    return 
  ENDIF 
  ; ���ļ� 
  ENVI_OPEN_FILE, in_file, r_fid=in_fid 
  ;�ļ��򿪳������˳� 
  IF (in_fid EQ -1) THEN RETURN 
  ;��ȡ�ļ���Ϣ 
  ENVI_FILE_QUERY, in_fid, dims=dims, nb=nb 
  IF  ~FILE_TEST(ruleset_filename) THEN RETURN 
  IF ~FILE_TEST(FILE_DIRNAME(out_file),/directory) THEN FILE_MKDIR,FILE_DIRNAME(out_file) 
  ; ִ��FX. 
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