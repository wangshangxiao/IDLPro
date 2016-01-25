pro imageExtract
  input='D:\77211356\Data\Hubei\TM\JHPY\2015.7\分类'
  chimageoutpath='D:\77211356\Data\Hubei\TM\JHPY\2015.7'
  data=read_tiff(input,geotiff = geotiff)
  data = long(data)

  ;第一类
  classname=64
  Class1_Index=where(data eq classname)
  
  data[Class1_Index]=classname
  result=data

  write_tiff,chimageoutpath+'\'+FILE_BASENAME(input,'.tif',/FOLD_CASE)+'_'+classname+'.tif',result,geotiff = geotiff,/short


end