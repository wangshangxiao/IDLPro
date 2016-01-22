Function getSubFoldersName,InputPath,ExtentionName
  
  Allfiles=FILE_SEARCH(InputPath,ExtentionName,/FOLD_CASE,count=count)
  FolerNameArrays = !NULL
  ;收集所有上期目录，放到一个数组中，用数据做循环进行下面的拼接执行
  FOR k=0,count-1 DO BEGIN
    ;取出路径
    FullInPutPath= file_dirname(Allfiles[k])
    ;print,'FullInPutPath===:   '+FullInPutPath

    ;取出文件所在目录的上一级目录
    SubDirs = strsplit(FullInPutPath,'\', /EXTRACT,  COUNT= FileCOUNT)
    ;print,'SubDirs===:   '+SubDirs

    ;提取上一级目录名称
    Datedir = SubDirs[ FileCOUNT-2]
    ;print,'Datedir===:   '+Datedir

    FolerNameArrays = [FolerNameArrays, datedir]
    ;print,DatedirArray

  ENDFOR
  ;去掉数组中重复的名字
  FolerNameArrays=FolerNameArrays[uniq(FolerNameArrays)]
  ;print,'FolerNameArraysUniq:======  '+FolerNameArrays
  
  Return,FolerNameArrays
  
END


PRO MOSAICBATCH
  COMPILE_OPT IDL2
  ; 启动ENVI 5.1
  e = ENVI()
  ;COMPILE_OPT idl2

  ;ENVI,/RESTORE_BASE_SAVE_FILES
  ;initialize ENVI in batch mode:
  ;ENVI_BATCH_INIT
  ; 选择多个文件
;  files = DIALOG_PICKFILE(/MULTIPLE, $
;    TITLE = 'Select input scenes')
    
  inputPath='D:\77211356\Data\GF\HuBei\huanggang'
  
  DatedirArray=getSubFoldersName(InputPath,'*_rpc.tif')
  print,'DatedirArrayUniq:======  '+DatedirArray
  
  nCount = n_elements(DatedirArray)
  print,nCount
  
  FOR Num=0,nCount-1 DO BEGIN


    ;FullInPutPath= file_dirname(FileNames[Num])
    
    
    ;取出文件所在目录的上一级目录
    ;SubDirs = strsplit(FullInPutPath,'\', /EXTRACT,  COUNT= FileCOUNT)

    ;提取上一级目录名称
    ;Datedir = SubDirs[ FileCOUNT-2]
    
    subInputPath=!NULL
    ;组成新的目录，到日期
    subInputPath=inputPath+PATH_SEP()+DatedirArray[Num]

    ;搜索日期目录下所有指定文件
    files=FILE_SEARCH(subInputPath,'*_rpc.tif',/FOLD_CASE,count=count)
    
    
      
    scenes = !NULL
    ; 将每一个Raster放在一个Scenes中
    FOR i=0, N_ELEMENTS(files)-1 DO BEGIN
      raster = e.OpenRaster(files[i])
      scenes = [scenes, raster]
      
    ENDFOR
    
    GF1_Names=strsplit(SubDirs[FileCOUNT-1],'_', /EXTRACT,  COUNT= NameCOUNT)
    
    GF1=GF1_Names[0]+'_'+GF1_Names[1]
    
    
    
    ; 设置输出路径
    newFile = subInputPath+'\'+GF1+'_'+Datedir+'_Mosaic.tif'
    
    print,Datedir,'      开始拼接…………',newFile
    
    ; 创建ENVIMosaicRaster对象
    mosaicRaster = ENVIMosaicRaster(scenes,           $
      background = 0,                                       $
      color_matching_method = 'histogram matching',  $
      color_matching_stats = 'entire scene',      $
      feathering_distance = 5,                           $
      feathering_method = 'seamline',                      $
      resampling = 'Cubic',                          $
      seamline_method = 'geometry')
  
      
    IF FILE_TEST(newFile) THEN FILE_DELETE, newFile
    ; 输出镶嵌结果
;    mosaicRaster.Export, newFile, 'ENVI';'GeoTIFF';
;  
;    ;　保存接边线
;    mosaicRaster.SAVESEAMPOLYGONS, newFile+'_seamline.shp'
;    
;    
;    vector = e.OpenVector(newFile+'_seamline.shp')
;  
;    ; 打开并显示栅格和接边线
;    mosaicRaster = e.OpenRaster(newFile)
;    view = e.GetView()
;    layer = view.createlayer(mosaicRaster)
;    vlayer = view.createlayer(vector)
;    print,Datedir,'     拼接完毕 !    ',newFile
  ENDFOR
  envi_batch_exit
  print,'全部影像拼接完毕！'
END