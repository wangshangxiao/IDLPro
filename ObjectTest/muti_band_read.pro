pro muti_band_read

  InputPath='D:\IDLTest\GF\gf1_20130711.tif'
  OutPath='D:\IDLTest\NDVI'
  data=read_tiff(InputPath,geotiff = geotiff)
  help,data
;  blue = reform(data[0,*,*])
;  green = reform(data[1,*,*])
;  red = reform(data[2,*,*])
;  nir=reform(data[3,*,*])
;  print,'bluemin=',min(blue),'    bluemax=',max(blue)
;  print,'greenmin=',min(green),'    greenmax=',max(green)
;  print,'redmin=',min(red),'    redmax=',max(red)
;  print,'nearedmin=',min(nir),'    nearedmax=',max(nir)
  
  Outputbase=OutPath+'\'+FILE_BASENAME(InputPath,'.tif',/FOLD_CASE)
  
  ;blue = data[0,*,*]
  ;print,blue
  ;green = data[1,*,*]
  ;print,green
  red = data[*,*,2]
  help,red
  ;print,red
print,'-------------------------------------------------------------'
 
  ;print,'redmin=',min(red),'    redmax=',max(red)
  nir=data[*,*,5]
  
  ;print,nir
  
  
  outputfile=outputbase+'_red.tif'
  ;write_tiff,outputfile,red,geotiff=geotiff,/float;,planarconfig=2
end