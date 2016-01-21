PRO MutiBandMaskBatch
  
  files = DIALOG_PICKFILE(/MULTIPLE, TITLE = '请选择待掩膜影像')
  scenes = !NULL
  
  nCount=N_ELEMENTS(files)
  print,nCount
  
  ; 将每一个Raster放在一个Scenes中
  FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
    
    ;********文件合法性判断
    if file_test(files[i]) eq 0 then begin
      CAUTION = dialog_message(files[i]+' 输入文件不存在!',title='警告')
      return
    endif

    if query_tiff(files[i],tiffinfo) eq 0 then begin
      CAUTION = dialog_message(files[i]+' 输入文件不是正确的TIFF文件!',title='警告')
      return
    endif
    ;********
    raster = files[i]
    scenes = [scenes, raster]
    
  ENDFOR
  ;定义文件扩展名
  fileextension='.tif'
  ;选择掩膜文件对话框
  FarmMaskFile = DIALOG_PICKFILE(TITLE = '请选择掩膜文件',FILTER='*'+fileextension)
  ;判断是否是TIFF格式文件
  if query_tiff(FarmMaskFile,tiffinfo) eq 0 then begin
    CAUTION = dialog_message(FarmMaskFile + ' 输入文件不是正确的TIFF文件!',title='警告')
    return
  endif
  
;  DATA_FarmMaskFile=READ_TIFF(FarmMaskFile)
;  size_FarmMaskFile=size(DATA_FarmMaskFile,/DIMENSIONS)
;  n_FarmMaskFile=n_elements(size_FarmMaskFile)
;  ;print,'n_FarmMaskFile=',n_FarmMaskFile
;  if n_FarmMaskFile GT 2 then begin
;    CAUTION = dialog_message('输入文件不是正确的掩膜文件，请重新输入!',title='警告')
;    return
;  endif
  
  nC=N_ELEMENTS(scenes)
  print,nC
  FOR i=0, N_ELEMENTS(scenes)-1 DO BEGIN
  
    DATA1=READ_TIFF(scenes[i],GEOTIFF=GEOE)
    size1=size(data1,/DIMENSIONS)
    DATA2=READ_TIFF(FarmMaskFile)
    size2=size(data2,/DIMENSIONS)
    nb=size1[0]
    ns=size1[1]<size2[0]
    nl=size1[2]<size2[1]
    result=lonarr(size1[0],ns,nl)
    for band=0,nb-1 do begin
      result[band,*,*]=data1[band,0:(ns-1),0:(nl-1)]*data2[0:(ns-1),0:(nl-1)]
    endfor
    
    
    ;输出文件夹选择
    

    outputfile=outPath+'\'+FILE_BASENAME(scenes[i],fileExtension,/FOLD_CASE)+'_Mask.tif'
    
    WRITE_TIFF,outputfile,result,GEOTIFF=GEOE,/long
  
  ENDFOR
  
  CAL_INFO=dialog_message('提取完成！',/information)
  
  
END