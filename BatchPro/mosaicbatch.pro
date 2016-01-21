Function getSubFoldersName,InputPath,ExtentionName
  
  Allfiles=FILE_SEARCH(InputPath,ExtentionName,/FOLD_CASE,count=count)
  FolerNameArrays = !NULL
  ;�ռ���������Ŀ¼���ŵ�һ�������У���������ѭ�����������ƴ��ִ��
  FOR k=0,count-1 DO BEGIN
    ;ȡ��·��
    FullInPutPath= file_dirname(Allfiles[k])
    ;print,'FullInPutPath===:   '+FullInPutPath

    ;ȡ���ļ�����Ŀ¼����һ��Ŀ¼
    SubDirs = strsplit(FullInPutPath,'\', /EXTRACT,  COUNT= FileCOUNT)
    ;print,'SubDirs===:   '+SubDirs

    ;��ȡ��һ��Ŀ¼����
    Datedir = SubDirs[ FileCOUNT-2]
    ;print,'Datedir===:   '+Datedir

    FolerNameArrays = [FolerNameArrays, datedir]
    ;print,DatedirArray

  ENDFOR
  ;ȥ���������ظ�������
  FolerNameArrays=FolerNameArrays[uniq(FolerNameArrays)]
  ;print,'FolerNameArraysUniq:======  '+FolerNameArrays
  
  Return,FolerNameArrays
  
END


PRO MOSAICBATCH
  COMPILE_OPT IDL2
  ; ����ENVI 5.1
  e = ENVI()
  ;COMPILE_OPT idl2

  ;ENVI,/RESTORE_BASE_SAVE_FILES
  ;initialize ENVI in batch mode:
  ;ENVI_BATCH_INIT
  ; ѡ�����ļ�
;  files = DIALOG_PICKFILE(/MULTIPLE, $
;    TITLE = 'Select input scenes')
    
  inputPath='D:\77211356\Data\GF\HuBei\huanggang'
  
  DatedirArray=getSubFoldersName(InputPath,'*_rpc.tif')
  print,'DatedirArrayUniq:======  '+DatedirArray
  
  nCount = n_elements(DatedirArray)
  print,nCount
  
  FOR Num=0,nCount-1 DO BEGIN


    ;FullInPutPath= file_dirname(FileNames[Num])
    
    
    ;ȡ���ļ�����Ŀ¼����һ��Ŀ¼
    ;SubDirs = strsplit(FullInPutPath,'\', /EXTRACT,  COUNT= FileCOUNT)

    ;��ȡ��һ��Ŀ¼����
    ;Datedir = SubDirs[ FileCOUNT-2]
    
    subInputPath=!NULL
    ;����µ�Ŀ¼��������
    subInputPath=inputPath+PATH_SEP()+DatedirArray[Num]

    ;��������Ŀ¼������ָ���ļ�
    files=FILE_SEARCH(subInputPath,'*_rpc.tif',/FOLD_CASE,count=count)
    
    
      
    scenes = !NULL
    ; ��ÿһ��Raster����һ��Scenes��
    FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
      raster = e.OpenRaster(files[i])
      scenes = [scenes, raster]
      
    ENDFOR
    
    GF1_Names=strsplit(SubDirs[FileCOUNT-1],'_', /EXTRACT,  COUNT= NameCOUNT)
    
    GF1=GF1_Names[0]+'_'+GF1_Names[1]
    
    
    
    ; �������·��
    newFile = subInputPath+'\'+GF1+'_'+Datedir+'_Mosaic.tif'
    
    print,Datedir,'      ��ʼƴ�ӡ�������',newFile
    
    ; ����ENVIMosaicRaster����
    mosaicRaster = ENVIMosaicRaster(scenes,           $
      background = 0,                                       $
      color_matching_method = 'histogram matching',  $
      color_matching_stats = 'entire scene',      $
      feathering_distance = 5,                           $
      feathering_method = 'seamline',                      $
      resampling = 'Cubic',                          $
      seamline_method = 'geometry')
  
      
    IF FILE_TEST(newFile) THEN FILE_DELETE, newFile
    ; �����Ƕ���
;    mosaicRaster.Export, newFile, 'ENVI';'GeoTIFF';
;  
;    ;������ӱ���
;    mosaicRaster.SAVESEAMPOLYGONS, newFile+'_seamline.shp'
;    
;    
;    vector = e.OpenVector(newFile+'_seamline.shp')
;  
;    ; �򿪲���ʾդ��ͽӱ���
;    mosaicRaster = e.OpenRaster(newFile)
;    view = e.GetView()
;    layer = view.createlayer(mosaicRaster)
;    vlayer = view.createlayer(vector)
;    print,Datedir,'     ƴ����� !    ',newFile
  ENDFOR
  envi_batch_exit
  print,'ȫ��Ӱ��ƴ����ϣ�'
END