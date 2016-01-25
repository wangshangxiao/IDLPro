pro write_HDR_FILE,result_file,datainfo
;;  �˽�����ɵ��ǽ��ַ���д�뵽ͷ�ļ��� ,����������д�����ļ���
;;
;   �����ļ���ͷ�ļ���ȡ
    index = STRPOS(result_file, '.')
    head1 = ''
    if index eq -1 then begin
      result_head=strtrim(result_file,2)+'.hdr'
    endif else begin
      head1 = STRMID(result_file,0,index)
      result_head=head1+'.hdr'
    endelse

;   �����ַ���
    enter=string(byte(13))+string(byte(10))
    samples=strtrim(string(datainfo.xsize),2)                          ;��'5000'
    lines=strtrim(string(datainfo.ysize),2)                            ;��'4100'
    sulx=strtrim(string(ulong(datainfo.startx)),2)+'.0000'             ;��ʼX'906500.0000'
    suly=strtrim(string(ulong(datainfo.starty)),2)+'.0000'             ;��ʼY'5906500.0000'
    bands=strtrim(string(datainfo.bandnum),2)                          ;������
    data_type=strtrim(string(datainfo.datatype),2)                     ;��������
    pixel_size=strtrim(string(1000),2)+'.00000000'       ;��Ԫ��С
    bandname=''
    time=systime()                                                     ;ʱ��
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
    'map info = {chiesealbers, 1.0000, 1.0000, '+	$
    STRTRIM(datainfo.startx,2)+ ','+	$
    STRTRIM(datainfo.startY,2)+ ','+	$
    ' 1000.000, 1000.000, 1, units=Meters}'+enter+$
    'projection info = {9, 6378245.0, 6356863.0, 0.000000, 110.000000, 4000000.0, 0.0, 25.000000, 47.000000, PCI (ACEA), units=Meters}'+enter+$
    'pixel size = {1000.000000, 1000.000000, units=Meters}'+enter

;    д�����
     openw,lun,result_head,/get_lun
     writeu,lun,result_headdata
     free_lun,lun
end