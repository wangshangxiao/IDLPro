pro imageMergeClass
  input='C:\Users\Administrator\Desktop\DEM\ASTGTM_N24E099Y1.tif'
  chimageoutpath='C:\Users\Administrator\Desktop\DEM'
  data=read_tiff(input,geotiff = geotiff)
  data = long(data)

  ;��һ��
  Class1_Index=where(data ge 480 and data lt 1000)
  data[Class1_Index]=1
  
  ;�ڶ���
  Class2_Index=where(data ge 1000 and data lt 2000)
  data[Class2_Index]=2

  ;������
  Class3_Index=where(data ge 2000)
  data[Class3_Index]=3


  result=data

  write_tiff,chimageoutpath+'\'+FILE_BASENAME(input,'.tif',/FOLD_CASE)+'_MergeClass.tif',result,geotiff = geotiff,/short


end