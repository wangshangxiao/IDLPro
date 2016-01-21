PRO Example_RPCOrthorectification
  ; ����ENVI5.1
  e = ENVI()

  ; ѡ�������ļ�
  ImageFile = DIALOG_PICKFILE(TITLE='Select an input image')
  Raster = e.OpenRaster(ImageFile)
  ; ѡ��DEM�ļ�������ʹ��ENVI5.1�Դ���DEM����
  DEMFile = 'C:\Program Files\Exelis\ENVI51\data\GMTED2010.jp2'
  DEM = e.OpenRaster(DEMFile)

  ; �½�RPCOrthorectification ENVITask
  Task = ENVITask('RPCOrthorectification')
  ; ����Task�������������
  Task.INPUT_RASTER = Raster
  Task.DEM_RASTER = DEM
  Task.DEM_IS_HEIGHT_ABOVE_ELLIPSOID = 0
  Task.Output_Pixel_Size = 16
  Task.OUTPUT_RASTER_URI = e.GetTemporaryFilename()

  ; ִ��Task
  Task.Execute, Error=error
  ; ����������ӵ�Data Manager��
  DataColl = e.DATA
  DataColl.Add, Task.OUTPUT_RASTER
  ; ��ʾ���
  View1 = e.GetView()
  Layer1 = View1.CreateLayer(Task.OUTPUT_RASTER)
END