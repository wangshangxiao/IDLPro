  PRO cloud_detection,inputFiles=inputFiles,chimageoutpath=chimageoutpath,Message=Message
  
  inputFiles='D:\share\HongxingHJdata\cloud\hj1a-ccd2-450-57-20110718-l20000583059_repro_sub.tif'
  chimageoutpath='D:\share\HongxingHJdata\cloud\result'
  compile_opt idl2
;  Result = DIALOG_MESSAGE(chimageoutpath)
  ;读取模拟的反射率数据

  ;读取tiff影像
  N_inputFiles=N_ELEMENTS(inputFiles);读取输入影像文件的个数

  
  
  for n=0,N_inputFiles-1 do begin
    data = read_tiff(inputFiles[n],geotiff = geotiff)
    ;data 为int[4,1726,874]意为4列  1726行  874层
    blue = reform(data[0,*,*])
    green = reform(data[1,*,*])
    red = reform(data[2,*,*])
    nir=reform(data[3,*,*])
    print,'bluemin=',min(blue),'    bluemax=',max(blue)
    print,'greenmin=',min(green),'    greenmax=',max(green)
    print,'redmin=',min(red),'    redmax=',max(red)
    print,'nearedmin=',min(nir),'    nearedmax=',max(nir)

    Cloud_Index=where(red LE 2300)
    
    ;print,Cloud_Index
    red[Cloud_Index]=1
    NoCloud_Index=where(red NE 1)
    red[NoCloud_Index]=0
    ;print,blue
    result=red
    write_tiff,chimageoutpath+'\'+FILE_BASENAME(inputFiles[n],'.tif',/FOLD_CASE)+'_cloud2.tif',result,geotiff = geotiff,/float
;  ;  print,systime()
  endfor
  Message=''
  end

;  pro test_main_ch_retrieve
;  chtableinpath='D:\xujin\test\CCDA1chtable.txt'
;  inputFiles='D:\xujin\Inversion pro\XJdata\CCD_7_tiff\hj1a-ccd2-447-56-20110826-l20000600736_hx_geo_ref_crop.tif'
;  chimageoutpath='D:\xujin\test'
;  main_ch_retrieve,chtableinpath=chtableinpath,inputFiles=inputFiles,chimageoutpath=chimageoutpath,Message=Message
;  end