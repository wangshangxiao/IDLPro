PRO Auto_WARP

  COMPILE_OPT idl2
  ENVI,/RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT,/NO_STATUS_WINDOW
  
  inputPath='D:\77211356\Data\GF\HuBei\huanggang'

  WarpFiles=FILE_SEARCH(inputPath,'*.tif',/FOLD_CASE,count=count)
  
  print,'��',count,'    ��Ӱ�񼸺�У��'
  
  FOR i=0,count-1 DO BEGIN
  
    WarpFile=WarpFiles[i]
    
    outPath= file_dirname(WarpFile)
    
    out_name=outPath+'\'+FILE_BASENAME(WarpFile,'.tif',/FOLD_CASE)+'_warp.tif'
    
    BaseFile='D:\77211356\Data\GF\HuBei\basedata\TM8_8\ref\mosaic_121_122_123.tif'
    ;WarpFile='D:\77211356\Data\GF\HuBei\huanggang\20150122\GF1_WFV2_E115.9_N29.3_20150122_L1A0000606437\GF1_WFV2_E115.9_N29.3_20150122_L1A0000606437_rpc.tif'
    
    ;�򿪲ο�Ӱ��
    envi_open_file, BaseFile, r_fid=base_fid
    ;�򿪴�����Ӱ��
    envi_open_file, WarpFile, r_fid=warp_fid
    ;���Ϊ���򷵻�
    if (base_fid eq -1 || warp_fid eq -1) then begin
      envi_batch_exit
      return
    endif
    
    print,'��',i+1,'��      ��ʼ��������',out_name
    
    ;���κţ�0Ϊ��һ����
    base_match_pos = 0L
    ;���κţ�0Ϊ��һ����
    warp_match_pos = 0L
    ;���Ƶ���������У�GF-1һ����Ӱ��һ��530��-750���ο�Ӱ��ΪTM8��ȫɫ��
    num_tie_points = 575
    ;�ƶ����ڣ�Ĭ��Ϊ11��ֵԽ��Խ׼��ʱ��Խ��
    move_win = 11
    ;�������ڣ�Ĭ��Ϊ81��ֵԽ��Խ׼��ʱ��Խ��
    search_win = 255
    ;��Ȥ��������Χ��ֵԽ��Խ׼��ʱ��Խ��
    area_chip_size = 128L
    ;Ĭ��Ϊ1��ֵԽ��Խ׼��ʱ��Խ��
    num_oversamples = 4
    ;���ÿ��Ƶ��ļ����·�����ļ���
    out_tie_points_name = outPath+'\'+FILE_BASENAME(WarpFile,'.tif',/FOLD_CASE)+'_tile.pts'
    print,'��',i+1,'��      ���Ƶ���ҿ�ʼ���ļ�����',out_tie_points_name
    ;
    ; Perform the automatic tie point collection
    ;
    
    envi_doit, 'envi_auto_tie_points_doit', $
      base_fid=base_fid, $
      warp_fid=warp_fid, $
      base_match_pos=base_match_pos, $
      warp_match_pos=warp_match_pos, $
      num_tie_points=num_tie_points, $
      move_win=move_win, $
      search_win=search_win, $
      area_chip_size=area_chip_size, $
  ;    in_tie_points_array=in_tie_points_array, $
      num_oversamples=num_oversamples, $
      ;�����Ƶ����鸳ֵ��PTS��
      OUT_TIE_POINTS_ARRAY=pts,$
      ;�����Ƶ��ļ������ָ���ļ���
      out_tie_points_name=out_tie_points_name
    
    print,'��',i+1,'��     ���Ƶ������� !    ',out_name
    ;������Ӱ���fid 
    fid=warp_fid
    ;
    ; Set the DIMS and POS to keywords
    ; to processes all spatial and all
    ; spectral data. Output the result
    ; to disk.
    ;
    envi_file_query, fid, dims=dims, nb=nb
    pos = lindgen(nb)
    
    
    ;out_name = 'D:\77211356\Data\GF\HuBei\basedata\Warp\GF1_WFV2_E115.9_N29.3_20150122_L1A0000606437_rpc_warp.tif'
  
    ;
    ; Perform the image-to-map registration.
    ;
    envi_doit, 'envi_register_doit', $
      B_FID=base_fid,$
      w_fid=fid, w_pos=pos, w_dims=dims, $
      method=2, out_name=out_name, $
      pts=pts,r_fid=r_fid  
      
    print,'��',i+1,'��Ӱ�񼸺�У����ϣ�'
    ;
    ; Exit ENVI Classic
    ;
  ENDFOR
    
  ENVI_batch_EXIT
  print,'ȫ��Ӱ�񼸺�У����ϣ�'
END