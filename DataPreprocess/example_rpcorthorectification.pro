PRO Example_RPCOrthorectification
  ; 启动ENVI5.1
  e = ENVI()

  ; 选择输入文件
  ImageFile = DIALOG_PICKFILE(TITLE='Select an input image')
  Raster = e.OpenRaster(ImageFile)
  ; 选择DEM文件，这里使用ENVI5.1自带的DEM数据
  DEMFile = 'C:\Program Files\Exelis\ENVI51\data\GMTED2010.jp2'
  DEM = e.OpenRaster(DEMFile)

  ; 新建RPCOrthorectification ENVITask
  Task = ENVITask('RPCOrthorectification')
  ; 设置Task的输入输出参数
  Task.INPUT_RASTER = Raster
  Task.DEM_RASTER = DEM
  Task.DEM_IS_HEIGHT_ABOVE_ELLIPSOID = 0
  Task.Output_Pixel_Size = 16
  Task.OUTPUT_RASTER_URI = e.GetTemporaryFilename()

  ; 执行Task
  Task.Execute, Error=error
  ; 将输出结果添加到Data Manager中
  DataColl = e.DATA
  DataColl.Add, Task.OUTPUT_RASTER
  ; 显示结果
  View1 = e.GetView()
  Layer1 = View1.CreateLayer(Task.OUTPUT_RASTER)
END