pro ImageMinus

  input1='D:\神农架\解译后\DEM_cun_1200.tif'
  input2='D:\神农架\解译后\cun_build_road_water.tif'
  chimageoutpath='D:\神农架\解译后'
  data1=read_tiff(input1,geotiff = geotiff)
  data2=read_tiff(input2,geotiff = geotiff)
  data=data1-data2

  
  ;data=data*1000

;  index_1 =where (data2 eq 1)
;  data2[index_1]=2
;  index_0 =where (data2 eq 0)
;  data2[index_0]=1
;  index_2 =where (data2 eq 2)
;  data2[index_2]=0
;  
;  data=data1*data2

  ;print,n_elements(crop_index)

  ;no_crop_index=where (data le 3)
  ;data[no_crop_index]=0


  result=data
  write_tiff,chimageoutpath+'\'+'cun_vegetation.tif',result,geotiff = geotiff,/short


END