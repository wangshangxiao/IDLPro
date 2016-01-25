PRO TEST_RESIZEVIAEVFxiu 
cd,'d:\test' 
restore,'resizeviaevf.sav'          
                                  ;sav文件只有restore之后才能使用 
filenames =file_search('*.tif') 
                                  ;搜寻所有的tif文件 
evfname = 'shandong.evf' 
                                  ;提前定义好evf文件，也就是裁剪区域 
n=n_elements(filenames) 
                                  ;文件的个数n 
l=strlen(filenames[0]) 
                                  ;文件名的长度l 
print,filenames[0],l

for i=0,n-1 do begin 
filename =filenames[i]

out=strmid(filenames[i],0,l-4) 
                                  ;这里l－4是除去后缀后文件名的长度 
help,out 
outname =out+'resize.tif'         
                                  ;新裁剪的文件名字都加上一个'resize.tif' 
print,outname 
;outname =filenames[i]+'resize.tif'

ResizeViaEvf, filename = filename, $ 
evfName = evfName, $ 
outName = outName 
                                      ;调用函数ResizeViaEvf进行裁剪                     
endfor 
END