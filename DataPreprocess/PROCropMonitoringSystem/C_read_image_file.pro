function read_file,infile

;;  此子函数完成了文件的读取,返回结果为记录，文件信息fileinfo：头文件信息和数组
;;  传入的参数为：文件名
;;------------------------------------读文件过程-------------------------------------
;;  对输入参数的判断

	on_error,2

	forward_function DC_ReadHead_file
    if (File_test(infile) eq 0) then begin
;       TEXT=DIALOG_MESSAGE('找不到文件:'+infile+',请检查路径名!')
       print,'找不到文件:'+infile+',请检查路径名!'
       fileinfo = { datatype:-1 }
       return,fileinfo
    endif
;   判断头文件的存在性
    index = STRPOS(infile, '.')
    head1 = ''
    if index eq -1 then begin
      inhead=strtrim(infile,2)+'.hdr'
      openr,lun,strtrim(infile,2),/GET_LUN
      temp_result=fstat(lun)
      free_lun,lun
    endif else begin
      head1 = STRMID(infile,0,index)
      inhead=head1+'.hdr'
      openr,lun,strtrim(head1,2),/GET_LUN
      temp_result=fstat(lun)
      free_lun,lun
    endelse
    findresult=findfile(inhead)
    if (findresult[0] eq '') then begin
;       TEXT=DIALOG_MESSAGE(inhead+'头文件不存在或文件格式错误,请检查!')
       print,inhead+'头文件不存在或文件格式错误,请检查!'
       fileinfo = { datatype:-1 }
       return,fileinfo
    endif

 ;  读头文件
    openr,lun,inhead,/GET_LUN
    result = FSTAT(lun)

	 HeadFileInfo = DC_ReadHead_file(inhead)
	 if n_tags(HeadFileInfo) eq 1 then return,{error	:	-1}
    case HeadFileInfo.datatype of
	 	'1': d=1
	 	'2': d=2
	 	'4': d=4
	 	'5': d=8
	 	'12': d=2
	 	'13': d=4
	 	'14': d=8
	 	'15': d=8
	 	else:d=1
	 endcase
    if long(temp_result.SIZE/d) ne (LONG(HeadFileInfo.samples)*LONG(HeadFileInfo.lines)) then begin
    	Info = DIALOG_MESSAGE(infile+'数据文件不正确!',TITLE='警告')
	 	return,{error	:	-1}
	 endif
    headdata = bytarr(result.size-1)
    readu,lun,headdata
    free_lun,lun
    headfile=string(headdata)
    free_lun,lun
;   获取行列号，波段数，左上角坐标
    index0 = STRPOS(headfile, 'samples = ')
    index1 = STRPOS(headfile, 'lines   = ')
    index2 = STRPOS(headfile, 'bands   = ')
    index3 = STRPOS(headfile, 'header offset = ')
    index4 = STRpos(headfile, 'map info = ')
    INDEX_MJH= STRpos(headfile, 'units=Degrees')+16
    index5 = STRpos(headfile, 'projection info = ')
    index6 = STRPOS(headfile, 'data type = ')
    index7 = STRPOS(headfile, 'interleave = ')
    index8 = STRPOS(headfile, 'pixel size = ')
    samples = STRMID(headfile,index0+10,index1-index0-10-1)
    lines =  STRMID(headfile,index1+10,index2-index1-10-1)
    bands=STRMID(headfile,index2+10,index3-index2-10-1)
    data_type=STRMID(headfile,index6+12,index7-index6-12-1)
    ;; 起始点坐标的获取

    info=strmid(headfile,index4+11,index_MJH-index4-13)
    index41= strpos(info,',')
    info2=strmid(headfile,index4+11+index41+1,index5-(index4+11+index41)-3)
    index42= strpos(info2,',')
    info3=strmid(headfile,index4+11+index41+1+index42+1,index5-(index4+11+index41+1+index42)-3)
    index43= strpos(info3,',')
    info4=strmid(headfile,index4+11+index41+1+index42+1+index43+1,index5-(index4+11+index41+1+index42+1+index43)-3)
    index44= strpos(info4,',')
    info5=strmid(headfile,index4+11+index41+1+index42+1+index43+1+index44+1,index5-(index4+11+index41+1+index42+1+index43+1+index44)-3)
    index45=strpos(info5,',')
    PRINT,'headfile',headfile
	PRINT,'INFO',INFO
    PRINT,'INFO2',INFO2
    PRINT,'INFO3',INFO3
    PRINT,'INFO4',INFO4
    PRINT,'INFO5',INFO5
    sulx=STRMID(info4,0,index44)
    suly=STRMID(info5,0,index45)
    ;; 像元大小的获取,假设X，Y是一样大小，否则得分别提取X、Y的大小
    info80=STRMID(headfile,index8+14,30)
    index81=strpos(info80,',')
    pixel_size=strmid(headfile,index8+14,index81)

;   将获取的信息转换为整型
    xsize=fix(samples)
    ysize=fix(lines)
    bandnum=fix(bands)
    datatp=fix(data_type)
    ulx=float(sulx)
    uly=float(suly)
    pixelsize=fix(float(pixel_size))

;;  根据数据类型定义数组
    case datatp of
      1: begin
             indata=bytarr(xsize,ysize,bandnum)
             tpsize=1
         end
      2: begin
             indata=intarr(xsize,ysize,bandnum)
             tpsize=2
         end
      4: begin
             indata=fltarr(xsize,ysize,bandnum)
             tpsize=4
         end
      12:begin
             indata=uintarr(xsize,ysize,bandnum)
             tpsize=2
         end
      13:begin
             indata=ulonarr(xsize,ysize,bandnum)
             tpsize=4
         end
      else:begin
;             TEXT=DIALOG_MESSAGE(data_type+':没有定义此数据类型!')
             print,INFILE+'@@'+data_type+':没有定义此数据类型!'
             fileinfo = { datatype:-1 }
             return,fileinfo
         end
    ENDCASE
;;  读文件到数组中
    openr,lun,infile,/GET_LUN
    filesize=fstat(lun)
    datasize=size(indata,/n_elements)
    if filesize.size ne datasize*tpsize then begin
;       TEXT=DIALOG_MESSAGE('头文件信息与文件大小不一致，请检查!')
       print,INFILE+'头文件信息与文件大小不一致，请检查!'
       fileinfo = { datatype:-1 }
       return,fileinfo
    end
    readu,lun,indata
    free_lun,lun
;;  返回结果
	TEMP=GET_IMAGE_INFO(infile)

    return,fileinfo={         $
      datatype:datatp       , $
      bandnum:bandnum       , $
      startx:TEMP.STARTX    , $
      starty:TEMP.STARTY    , $
      xsize:xsize           , $
      ysize:ysize           , $
      pixelsize:pixelsize   , $
      dataarr:indata          $
      }
end