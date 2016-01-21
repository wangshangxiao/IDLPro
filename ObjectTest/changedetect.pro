pro changedetect
    
    input1='D:\77211356\Data\turpan\Data\etgfarmland\ndvi\ZY-3\20130816_NDVI.tif'
    input2='D:\77211356\Data\turpan\Data\etgfarmland\ndvi\ZY-3\20120817_NDVI.tif'
    output='D:\77211356\Data\turpan\Data\etgfarmland\ndvi\ZY-3\ZY_change.tif'
    
    
    DATA1=READ_TIFF(input1,GEOTIFF=GEOE)
    size1=size(data1,/DIMENSIONS)
    DATA2=READ_TIFF(input2)
    size2=size(data2,/DIMENSIONS)
    
    ns=size1[0]<size2[0]
    nl=size1[1]<size2[1]

    result=data1-data2
    result=result*10000
    ;result=fix(data1)-data2
    result=fix(result)
  WRITE_TIFF,output,result,GEOTIFF=GEOE,/short,/SIGNED
  
  ok = QUERY_TIFF(output,s)
  print,ok
  
end