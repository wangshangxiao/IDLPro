;非监督分类
pro CLASS_ISODATA,inputfile,outputfile,$
    ;感兴趣区文件
    roifile = roifile,$
        
    ;ISO算法参数
    ITERATIONS = ITERATIONS, $
    NUM_CLASSES = NUM_CLASSES , $    
    CHANGE_THRESH = CHANGE_THRESH, $
    ISO_MERGE_DIST = ISO_MERGE_DIST, $
    ISO_MERGE_PAIRS = ISO_MERGE_PAIRS, $
    ISO_MIN_PIXELS = ISO_MIN_PIXELS, $
    ISO_SPLIT_SMULT = ISO_SPLIT_SMULT, $
    ISO_SPLIT_STD = ISO_SPLIT_STD, $
    MIN_CLASSES = MIN_CLASSES
    
  COMPILE_OPT idl2
  CATCH, Error_status
  errorshow = 'Sorry to see the error,'+ $
    ' please send the error Information to "dongyq@esrichina-bj.cn"'
  IF Error_status NE 0 THEN BEGIN
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$
      !ERROR_STATE.MSG,/error,title = '错误提示!')
    return
  ENDIF
  ;输入数据预处理
  ENVI_OPEN_FILE, inputfile, r_fid=fid
  IF (fid EQ -1) THEN BEGIN
    RETURN
  ENDIF
  ;获取文件信息
  ENVI_FILE_QUERY, fid, dims=dims, nb=nb
  pos  = LINDGEN(nb)
  out_name = outputfile

      IF ~KEYWORD_SET(CHANGE_THRESH) THEN CHANGE_THRESH = .05
      IF ~KEYWORD_SET(NUM_CLASSES) THEN NUM_CLASSES = 10
      IF ~KEYWORD_SET(ITERATIONS) THEN ITERATIONS = 1
      IF ~KEYWORD_SET(ISO_MERGE_DIST) THEN ISO_MERGE_DIST = 1
      IF ~KEYWORD_SET(ISO_MERGE_PAIRS) THEN ISO_MERGE_PAIRS = 2
      IF ~KEYWORD_SET(ISO_MIN_PIXELS) THEN ISO_MIN_PIXELS = 1
      IF ~KEYWORD_SET(ISO_SPLIT_SMULT) THEN ISO_SPLIT_SMULT = 1
      IF ~KEYWORD_SET(ISO_SPLIT_STD) THEN ISO_SPLIT_STD = 1
      IF ~KEYWORD_SET(MIN_CLASSES) THEN MIN_CLASSES = 5
      
      out_bname = 'IsoData'
      
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=4, $
        r_fid=r_fid, $
        NUM_CLASSES = NUM_CLASSES, $
        ITERATIONS = ITERATIONS, $
        in_memory=0, $
        CHANGE_THRESH = CHANGE_THRESH, $
        ISO_MERGE_DIST = ISO_MERGE_DIST, $
        ISO_MERGE_PAIRS = ISO_MERGE_PAIRS, $
        ISO_MIN_PIXELS = ISO_MIN_PIXELS, $
        ISO_SPLIT_SMULT = ISO_SPLIT_SMULT, $
        ISO_SPLIT_STD = ISO_SPLIT_STD, $
        MIN_CLASSES = MIN_CLASSES
END