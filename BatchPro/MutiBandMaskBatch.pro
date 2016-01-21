PRO MutiBandMaskBatch
  
  files = DIALOG_PICKFILE(/MULTIPLE, TITLE = '��ѡ�����ĤӰ��')
  scenes = !NULL
  
  nCount=N_ELEMENTS(files)
  print,nCount
  
  ; ��ÿһ��Raster����һ��Scenes��
  FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
    
    ;********�ļ��Ϸ����ж�
    if file_test(files[i]) eq 0 then begin
      CAUTION = dialog_message(files[i]+' �����ļ�������!',title='����')
      return
    endif

    if query_tiff(files[i],tiffinfo) eq 0 then begin
      CAUTION = dialog_message(files[i]+' �����ļ�������ȷ��TIFF�ļ�!',title='����')
      return
    endif
    ;********
    raster = files[i]
    scenes = [scenes, raster]
    
  ENDFOR
  ;�����ļ���չ��
  fileextension='.tif'
  ;ѡ����Ĥ�ļ��Ի���
  FarmMaskFile = DIALOG_PICKFILE(TITLE = '��ѡ����Ĥ�ļ�',FILTER='*'+fileextension)
  ;�ж��Ƿ���TIFF��ʽ�ļ�
  if query_tiff(FarmMaskFile,tiffinfo) eq 0 then begin
    CAUTION = dialog_message(FarmMaskFile + ' �����ļ�������ȷ��TIFF�ļ�!',title='����')
    return
  endif
  
;  DATA_FarmMaskFile=READ_TIFF(FarmMaskFile)
;  size_FarmMaskFile=size(DATA_FarmMaskFile,/DIMENSIONS)
;  n_FarmMaskFile=n_elements(size_FarmMaskFile)
;  ;print,'n_FarmMaskFile=',n_FarmMaskFile
;  if n_FarmMaskFile GT 2 then begin
;    CAUTION = dialog_message('�����ļ�������ȷ����Ĥ�ļ�������������!',title='����')
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
    
    
    ;����ļ���ѡ��
    

    outputfile=outPath+'\'+FILE_BASENAME(scenes[i],fileExtension,/FOLD_CASE)+'_Mask.tif'
    
    WRITE_TIFF,outputfile,result,GEOTIFF=GEOE,/long
  
  ENDFOR
  
  CAL_INFO=dialog_message('��ȡ��ɣ�',/information)
  
  
END