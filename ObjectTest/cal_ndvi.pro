pro CAL_NDVI,InputPath,OutPath

InputPath='D:\bioharvest\in_hj\gf1_20130711.tif'
OutPath='D:\bioharvest\NDVI'
Outputbase=OutPath+'\'+FILE_BASENAME(InputPath,'.tif',/FOLD_CASE)
data=read_tiff(InputPath,geotiff = geotiff)

;print,data[*,*,2]
;print,data[*,*,3]

VI_CAL_NDVI,data,2,3,geotiff,Outputbase

end

PRO VI_CAL_NDVI,data,red_band,nir_band,geotiff,outputbase


  temp1=float(data[*,*,nir_band])-float(data[*,*,red_band])
  
  temp2=float(data[*,*,nir_band])+float(data[*,*,red_band])
  
  NDVI=temp1/temp2
  print,NDVI
  index=where(temp2 eq 0.0)
  if index ne [-1] then NDVI[index]=-1.0
  index=where(ndvi ge 1.0)
  if index ne [-1] then NDVI[index]=1.0

  outputfile=outputbase+'_ndvi.tif'
  write_tiff,outputfile,NDVI,geotiff=geotiff,/float,planarconfig=2

END