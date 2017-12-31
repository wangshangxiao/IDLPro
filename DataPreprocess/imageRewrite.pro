pro imageRewrite1

input='D:\IDLData\������pro.tif'
chimageoutpath='D:\��ũ��\�����\����'
data=read_tiff(input,geotiff = geotiff)
data = long(data)

;NoOther_Index=where(data eq 0)
    ;data[NoOther_Index]=1
NoCloud_Index1=where(data eq 2)
    data[NoCloud_Index1]=1
;Cloud_Index = where(data eq 1)
;    data[Cloud_Index]=0
;Cloud_Index1 = where(data eq 3)
;    data[Cloud_Index1]=1
result=data

write_tiff,chimageoutpath+'\'+FILE_BASENAME(input,'.tif',/FOLD_CASE)+'_ok.tif',result,geotiff = geotiff,/short


END

pro imageRewrite

  ;5��24����ȡ�ֵ�
  ;input='D:\77211356\Data\Hongxing\GF\VI_TIF_Sub\gf1_wfv3_e1279_n489_20130524_l2a0000118062_clip_geo_NDVI_Clip.tif'
  input='D:\77211356\Data\Hongxing\GF\class\isodata\iso0930_100class_1.tif'
  
  ;7��11����ȡ·������ˮ
  ;input='D:\77211356\Data\Hongxing\GF\VI_TIF_Sub\gf1_wfv1_e1270_n480_20130711_l2a0000118108_clip_geo_NDVI.tif'
  ;9.30��ȡˮ
  ;input='D:\77211356\Data\Hongxing\GF\VI_TIF_Sub\gf1_wfv2_e1265_n476_20130930_l2a0000118099_clip_geo_NDVI.tif'
  chimageoutpath='D:\77211356\Data\Hongxing\GF\class\isodata'
  data=read_tiff(input,geotiff = geotiff)
  ;data = float(data)
  
;  ;5,24�ֵ�
  forest_index =where (data eq 2)
  ;print,forest_index
  data[forest_index]=1
  Noforest_index=where(data gt 2)
  data[Noforest_index]=0
  
  ;7,11·������ˮ
;  RoadHouseWater_index =where (data le 0.475)
;  print,n_elements(RoadHouseWater_index)
;  ;print,forest_index
;  data[RoadHouseWater_index]=1
  ;NoRoadHouseWater_index=where(data ge 0.38)
  ;print,n_elements(NoRoadHouseWater_index)
  ;data[NoRoadHouseWater_index]=0
  
;  water_index =where (data lt 0)
;  print,water_index
;  data[water_index]=1
;  NoWater_index=where(data ge 0)
;  data[NoWater_index]=0


;5,24���
  ;crop_Index=where(data gt 0 and data le 0.52)
  ;data[crop_Index]=1
  ;NoCrop_Index=where(data eq 0 and data gt 0.52)
  ;data[NoCrop_Index]=0
  
  
  ;Cloud_Index = where(data eq 1)
  ;    data[Cloud_Index]=0
  ;Cloud_Index1 = where(data eq 3)
  ;    data[Cloud_Index1]=1
  result=data

  write_tiff,chimageoutpath+'\'+FILE_BASENAME(input,'.tif',/FOLD_CASE)+'_water.tif',result,geotiff = geotiff,/short


END