PRO TEST_RESIZEVIAEVFxiu 
cd,'d:\test' 
restore,'resizeviaevf.sav'          
                                  ;sav�ļ�ֻ��restore֮�����ʹ�� 
filenames =file_search('*.tif') 
                                  ;��Ѱ���е�tif�ļ� 
evfname = 'shandong.evf' 
                                  ;��ǰ�����evf�ļ���Ҳ���ǲü����� 
n=n_elements(filenames) 
                                  ;�ļ��ĸ���n 
l=strlen(filenames[0]) 
                                  ;�ļ����ĳ���l 
print,filenames[0],l

for i=0,n-1 do begin 
filename =filenames[i]

out=strmid(filenames[i],0,l-4) 
                                  ;����l��4�ǳ�ȥ��׺���ļ����ĳ��� 
help,out 
outname =out+'resize.tif'         
                                  ;�²ü����ļ����ֶ�����һ��'resize.tif' 
print,outname 
;outname =filenames[i]+'resize.tif'

ResizeViaEvf, filename = filename, $ 
evfName = evfName, $ 
outName = outName 
                                      ;���ú���ResizeViaEvf���вü�                     
endfor 
END