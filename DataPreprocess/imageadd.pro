pro ImageAdd

input1='D:\神农架\解译后\箭竹\箭竹林2.tif'
input2='D:\神农架\解译后\箭竹\箭竹林1.tif'
;input3='D:\77211356\Data\Hongxing\GF\cropland\gf1_wfv3_e1279_n489_20130524_l2a0000118062_clip_geo_NDVI_Clip_forest.tif'
chimageoutpath='D:\神农架\解译后\箭竹'
data1=read_tiff(input1,geotiff = geotiff)
data2=read_tiff(input2,geotiff = geotiff)
;data3=read_tiff(input3,geotiff = geotiff)
data=data1+data2;+data3

;index2=where(data gt 1)
;data[index2]=0
;crop_index =where (data eq 2)
;data[crop_index]=1

;print,n_elements(crop_index)

result=data
write_tiff,chimageoutpath+'\'+'箭竹林pro.tif',result,geotiff = geotiff,/short


END