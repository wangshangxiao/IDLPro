pro imagemutip

input1='D:\share\Hongxingtest\GF\class\decisiontree\tif\0711tree_clip.tif'
input2='D:\share\Hongxingtest\GF\class\decisiontree\tif\0825tree_clip.tif'
chimageoutpath='D:\share\Hongxingtest\GF\class\decisiontree'
data1=read_tiff(input1,geotiff = geotiff)
data2=read_tiff(input2,geotiff = geotiff)

data=data1+data2

NoWheat_Index = where(data ne 4)
    data[NoWheat_Index]=0
result=data
write_tiff,chimageoutpath+'\'+FILE_BASENAME(input1,'.tif',/FOLD_CASE)+'_wheat.tif',result,geotiff = geotiff,/float

END