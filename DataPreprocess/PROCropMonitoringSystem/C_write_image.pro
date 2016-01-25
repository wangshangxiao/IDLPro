pro write_result,result_file,datainfo
;;  此进程完成的是将字符串写入到头文件中 ,将数据数组写入裸文件中
;;
;   输入文件的头文件获取
    index = STRPOS(result_file, '.')
    head1 = ''
    if index eq -1 then begin
      result_head=strtrim(result_file,2)+'.hdr'
    endif else begin
      head1 = STRMID(result_file,0,index)
      result_head=head1+'.hdr'
    endelse

;   定义字符串
    enter=string(byte(13))+string(byte(10))
    samples=strtrim(string(datainfo.xsize),2)                          ;列'5000'
    lines=strtrim(string(datainfo.ysize),2)                            ;行'4100'
    sulx=strtrim(string(ulong(datainfo.startx)),2)+'.0000'             ;起始X'906500.0000'
    suly=strtrim(string(ulong(datainfo.starty)),2)+'.0000'             ;起始Y'5906500.0000'
    bands=strtrim(string(datainfo.bandnum),2)                          ;波段数
    data_type=strtrim(string(datainfo.datatype),2)                     ;数据类型
    pixel_size=strtrim(string(datainfo.pixelsize),2)+'.00000000'       ;像元大小
    bandname=datainfo.bandname
    time=systime()                                                     ;时间
    result_headdata='ENVI'+enter+$
    'description = {' + enter+ $
    '  Create New File Result ['+time+']}'+enter+$
    'samples = '+samples+enter+$
    'lines   = '+lines+enter+$
    'bands   = '+bands+enter+$
    'header offset = 0'+enter+$
    'file type = ENVI Standard'+enter+$
    'data type = '+data_type+enter+$
    'interleave = bsq'+enter+$
    'sensor type = Unknown'+enter+$
    'byte order = 0'+enter+$
    'map info = {Geographic Lat/Lon, 1.0000, 1.0000, '+	$
    STRTRIM(datainfo.startx,2)+ ','+	$
    STRTRIM(datainfo.startY,2)+ ','+	$
    ' 8.3333333000e-003, 8.3333333000e-003, WGS-84, units=Degrees}'+enter+$
    'band names = {'+enter+$
    ' '+bandname+'}'+enter

;    写入过程
     openw,lun,result_head,/get_lun
     writeu,lun,result_headdata
     free_lun,lun

     openw,lun,result_file
     writeu,lun,datainfo.dataarr
     free_lun,lun
end