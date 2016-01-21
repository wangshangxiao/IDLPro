pro from65535to0

  data = read_tiff('C:\Users\Administrator\Desktop\201308_b1.tif',geotiff=geotiff)
  index = where(data eq 65535)
  
  data[index]=0
  ;print,data
  write_tiff,'C:\Users\Administrator\Desktop\201308_b1_1.tif',data,geotiff=geotiff

end