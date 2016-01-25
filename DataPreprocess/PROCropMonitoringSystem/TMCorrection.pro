function getdefaultdirectorys

	return,'D:\模型程序\几何纠正\'
end

pro TM_tiff_add_tag, lun, tag, value, ifd=ifd, count=count
	s = size(value)		;Determine type from parameter
	typ = s[s[0]+1]		;IDL type code
	tiff_typ = ([ 0, 1, 3, 4, 5, 5, 0, 2])[typ]  ;Tiff types vs IDL
	TypeLen = ([0, 1, 1, 2, 4, 8])[tiff_typ]

	n = s[s[0]+2]		; # of elements
	offset = count * 12 + 2	; Offset into ifd
	ifd[offset] =   byte(fix(tag),0,2)	;integer tag
	ifd[offset+2] = byte(tiff_typ, 0, 2)	;data type
	if tiff_typ eq 5 then ifd[offset+2] = 12
	ifd[offset+4] = byte(n,0,4)		;count
	nbytes = n * TypeLen
	;处理字符串的情形
	if tiff_typ eq 2 then begin
		ifd[offset+4] = byte(strlen(value), 0, 2)	;count
		point_lun, -lun, pos   ;Get file posit
		ifd[offset+8] = byte(pos, 0, 4)  ;Set IFD ^ pointer
		writeu, lun, byte(value)
	endif else begin
		;不是字符串的情形
		if nbytes le 4 then begin	;Simple case
			ifd[offset+8] = byte(value,0,nbytes)
		endif else begin		;Array, written to file
			point_lun, -lun, pos   ;Get file posit
			ifd[offset+8] = byte(pos, 0, 4)  ;Set IFD ^ pointer
			if typ ne 4 then writeu, lun, value $	;Write the data
			else begin		;Write floating
				s = lonarr(n * 2)
				s[indgen(n)*2] = value * 10000.  ;Arbritrary scale of 10000
				s[indgen(n)*2+1] = 10000
				writeu,lun, s
			endelse
		endelse
	endelse
	count = count + 1
end

;把tiff信息和geotiff信息写入文件中去,输入的lun本身是由一个8字节的头部和数据组的文件
;info中包含数据的维度信息dimensions(宽度(cols)和高度(rows),不包含通道数(默认为7))
;geotiff包含以下两项
;resolution是一个长为三的数组，表示分辨率
;prjlocation是一个长为六的数组，表示位置
pro makegeototifffile, lun, info, geotiff = geotiff
	;文件基础信息
	cols = info.dimensions[0]
	rows = info.dimensions[1]
	;print, 'cols and rows, ', cols, rows
	count = 0
	ifd = bytarr(512)		;Image file directory
	count = 0			;# of tags
	photo = 1
	nbits = 8
	samples = 7L
	PlanarConfig = 1
	;
	TM_tiff_add_tag, lun, 254, 0L, ifd=ifd, count=count  		;New Subfile type
	TM_tiff_add_tag, lun, 256, cols, ifd=ifd, count=count  	;Image width
	TM_tiff_add_tag, lun, 257, rows, ifd=ifd, count=count  	;Image height
	TM_tiff_add_tag, lun, 258, replicate(nbits, samples), ifd=ifd, count=count    ;bit/sample
	TM_tiff_add_tag, lun, 259, 1, ifd=ifd, count=count  		;No compression
	TM_tiff_add_tag, lun, 262, photo, ifd=ifd, count=count  	;Photometric Interpretation
	;数据起始位置，固定为8
	TM_tiff_add_tag, lun,273, 8L, ifd=ifd, count=count
	;
	TM_tiff_add_tag, lun, 274, 1, ifd=ifd, count=count  ;Orientation
	TM_tiff_add_tag, lun, 277, 7, ifd=ifd, count=count 	;Samples / pixel
	TM_tiff_add_tag, lun, 278, rows, ifd=ifd, count=count 		;Rows / strip
	;文件数据大小
	tsize = samples * rows * cols
	TM_tiff_add_tag, lun, 279,  tsize, ifd=ifd, count=count   ;Strip byte counts

	if n_elements(xresol) le 0 then xresol = 100.
	if n_elements(yresol) le 0 then yresol = 100.
	TM_tiff_add_tag, lun, 282, float(xresol), ifd=ifd, count=count 		;Xresolution
	TM_tiff_add_tag, lun, 283, float(yresol), ifd=ifd, count=count 		;... and Yresolution
	TM_tiff_add_tag, lun, 284, PlanarConfig, ifd=ifd, count=count  ;PlanarConfig
	;判断是否要加入geotiff信息
	if keyword_set(geotiff) eq 1 then begin
		TM_tiff_add_tag, lun, '830E'X, double(geotiff.resolution), ifd=ifd, count=count
		TM_tiff_add_tag, lun, '8482'X, double(geotiff.prjlocation), ifd=ifd, count=count
		;以下为公共的geotiff信息
		TM_tiff_add_tag, lun, '87AF'X, [1,1,0,23,1024,0,1,1,1025,0,1,1,1026,-30799,279,0,$
			2048,0,1,32767,2049,-30799,303,279,2050,0,1,32767,2051,0,1,8901,$
			2052,0,1,9001,2054,0,1,9102,2056,0,1,32767,2057,-30800,1,6,$
			2058,-30800,1,7,3072,0,1,32767,3073,-30799,236,582,3074,0,1,32767,$
			3075,0,1,11,3076,0,1,9001,3078,-30800,1,0,3079,-30800,1,1,$
			3081,-30800,1,3,3082,-30800,1,4,3083,-30800,1,5,3088,-30800,1,2], ifd=ifd, count=count
		TM_tiff_add_tag, lun, '87B0'X, double([25.000000, 47.000000, 110.000000, 0.000000, 4000000.000000, $
			0.000000, 6378245.000000, 6356863.020000]), ifd=ifd, count=count
		TM_tiff_add_tag, lun, '87B1'X, ' IMAGINE GeoTIFF Support' $
			+ ' Copyright 1991 - 2005 by Leica Geosystems Geospatial Imaging, LLC. All Rights Reserved' $
			+ ' @(#)$RCSfile: egtf.c $ IMAGINE 9.0 $Revision: 10.0 $ $Date: 2005/07/26 15:10:00 EST $' $
			+ ' Projection Name = Albers Conical Equal Area' $
			+ ' Units = meters' $
			+ ' GeoTIFF Units = meters|IMAGINE GeoTIFF Support' $
			+ ' Copyright 1991 - 2005 by Leica Geosystems Geospatial Imaging, LLC. All Rights Reserved' $
			+ ' @(#)$RCSfile: egtf.c $ IMAGINE 9.0 $Revision: 10.0 $ $Date: 2005/07/26 15:10:00 EST $' $
			+ ' Unable to match Ellipsoid (Datum) to a GeographicTypeGeoKey value' $
			+ ' Ellipsoid = Krasovsky' $
			+ ' Datum = Krasovsky|IMAGINE GeoTIFF Support' $
			+ ' Copyright 1991 - 2005 by Leica Geosystems Geospatial Imaging, LLC. All Rights Reserved' $
			+ ' @(#)$RCSfile: egtf.c $ IMAGINE 9.0 $Revision: 10.0 $ $Date: 2005/07/26 15:10:00 EST $' $
			+ ' Projection = Albers Conical Equal Area|', ifd=ifd, count=count
	endif
	;记录当前地址准备写IFD
	point_lun, -lun, faddr		;Write IFD at and, get addr
	;print, 'IFD表位置：', faddr
	ifd[0] = byte(count, 0, 2)	;Insert count
	writeu, lun, ifd[0: count*12 + 5] ;Write IFD followed by 4 zero bytes
	;改写文件头信息
	point_lun, lun, 0		;Rewind to header
	;tiff文件头变量(8个字节)
	header = bytarr(8)
	;大端小端测试
	tst = byte(1,0,2)		;Which endian???
	if tst[0] eq 1 then header[0] = byte("II") $	;Little endian
   	else header[0] = byte("MM")	;Big endian
   	;tiff文件标志
	header[2] = byte(42, 0, 2)
	header[4] = byte(faddr,0,4)	;Write ifd offset
	writeu, lun, header		;And save it
end

;保存运行信息到结构体变量中
function TMDataTranslationGetInfo, event
	;获取变量列表
	widget_control, event.top, get_uvalue = idlist

	;逐个判断条件是否满足
	;卫星数据
	widget_control, idlist.satinfo, get_uvalue = stafileid
	widget_control, stafileid, get_value = stafile
	if file_test(stafile[0]) eq 0 then begin
		msg = dialog_message('无法打开卫星数据文件！请检查文件是否存在。', title='TM数据转换', /info)
		return, 0
	endif
	;判断输出路径是否存在
	widget_control, idlist.outpath, get_value = outpath
	if file_test(outpath[0], /directory) eq 0 then begin
		msg = dialog_message('输入目录不存在，请指定输出目录！', title='TM数据转换', /info)
		return, 0
	endif
	;文件投影信息
	widget_control, idlist.prjinfo, get_uvalue = prjfileid
	widget_control, prjfileid, get_value = prjfile
	if file_test(prjfile[0]) eq 0 then begin
		msg = dialog_message('无法打开投影数据文件！请检查文件是否存在。', title='TM数据转换', /info)
		return, 0
	endif
	;逐个读取波段文件,并逐个写输出文件
	bandfilelist = strarr(7)
	for i=0,6 do begin
		;打开原始波段文件
		widget_control, idlist.bandlist[i], get_uvalue = bandfileid
		widget_control, bandfileid, get_value = bandfile
		bandfilelist[i] = bandfile
	endfor
	;保存运行参数到文件
	tmpre_ini = {tmpre_ini, $
		stafile : stafile[0], $
		outpath : outpath[0], $
		prjfile : prjfile[0], $
		bandfile1 : bandfilelist[0], $
		bandfile2 : bandfilelist[1], $
		bandfile3 : bandfilelist[2], $
		bandfile4 : bandfilelist[3], $
		bandfile5 : bandfilelist[4], $
		bandfile6 : bandfilelist[5], $
		bandfile7 : bandfilelist[6] $
	}
	return, tmpre_ini
end

;保存运行信息到指定的文本文件中
pro TMDataTranslationSaveExeInfo, event
	filename = dialog_pickfile(title='选择输出文件', default_extension='ini', filter='*.ini', /write, /overwrite_prompt)
	if filename[0] eq '' then return
	info = TMDataTranslationGetInfo(event)
	if size(info, /type) ne 8 then return
	write_struct, filename[0], info, /ini
end

;根据执行转换的功能，参数为结构体变量
function TMDataTranslationExecuteByInfo, info
	;从卫星数据中读取信息
	linescount = file_lines(info.stafile)
	filestr = strarr(linescount)
	openr, lun, info.stafile, /get_lun
	readf, lun, filestr
	free_lun, lun
	;
	satstr = ['卫星信息']
	iWidth = 0L
	iHeight = 0L
	for i=0, linescount-1 do begin
		if strpos(filestr[i], 'path') ne -1 then begin
			spos = strpos(filestr[i], '=')
			pathname = strtrim(long(strmid(filestr[i], spos+1)), 2)
			continue
		endif
		if strpos(filestr[i], 'row') ne -1 then begin
			spos = strpos(filestr[i], '=')
			rowname = strtrim(long(strmid(filestr[i], spos+1)), 2)
			continue
		endif
		if strpos(filestr[i], 'numLines') ne -1 then begin
			spos = strpos(filestr[i], '=')
			iHeight = long(strmid(filestr[i], spos+1))
			continue
		endif
		if strpos(filestr[i], 'numPixels') ne -1 then begin
			spos = strpos(filestr[i], '=')
			iWidth = long(strmid(filestr[i], spos+1))
			continue
		endif
		if strpos(filestr[i], 'centreTime') ne -1 then begin
			spos = strpos(filestr[i], '=')
			timestr = strmid(filestr[i], spos+1)
			ptime = strmid(timestr, 2, 4) + strmid(timestr, 7, 2) $
				+ strmid(timestr, 10, 2) + strmid(timestr, 13, 2) $
				+ strmid(timestr, 16, 2) + strmid(timestr, 19, 2)
			continue
		endif
	endfor
	;输出空间分辨率
	oWidth = ceil(iWidth * 25. / 30.)
	oHeight = ceil(iHeight * 25. / 30.)	;
	;输出文件名
	outfile = info.outpath + '\tm5_' + pathname + rowname + '_' + strmid(ptime, 0, 8) + '.tif'
	temp_outfile = outfile + '.tmp'

	;开辟单个波段的数据空间
	datai = bytarr(iWidth, iHeight)
	data = bytarr(oWidth, oHeight)
	;print, 'outsize : ', oWidth, oHeight
	;打开临时文件
	openw, templun, temp_outfile, /get_lun
	;记录数据位置
	fileposlist = lonarr(7)
	;获取波段文件名列表
	bandfile = [info.bandfile1,info.bandfile2, info.bandfile3, info.bandfile4, $
		info.bandfile5, info.bandfile6, info.bandfile7]
	;逐个读取波段文件,并逐个写输出文件
	for i=0,6 do begin
		;打开原始波段文件
		openr, lun, bandfile[i], /get_lun
		readu, lun, datai
		free_lun, lun
		;调整分辨率
		data = congrid(datai, oWidth, oHeight, /interp)
		;写临时文件，并记录位置
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, data
	endfor
	;关闭临时文件
	free_lun, templun
	;重新打开临时文件
	openr, templun, temp_outfile, /get_lun
	;打开结果文件
	openw, lun, outfile, /get_lun
	print, '输出文件：' + outfile
	;写tiff文件头信息
	header = bytarr(8)
	writeu, lun, header
	;分块合成七个通道，并写入结果文件
	rows_size = 300
	data1 = bytarr(oWidth, rows_size)
	data3 = bytarr(7, oWidth, rows_size)
	for i=0, oHeight- rows_size - 1, rows_size do begin
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endfor
	;写余下的部分
	;print, 'i : ', i
	theheight = oHeight - i
	;当超过一行剩余时，写剩余的行
	if theheight gt 0 then begin
		data1 = bytarr(oWidth, theheight)
		data3 = bytarr(7, oWidth, theheight)
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endif
	;加入tiff信息
	info = { dimensions : [oWidth, oHeight] }
	geotiff = { resolution : [30., 30., 0], prjlocation : [0.,0.,0.,0.,0.,0.]}
	makegeototifffile, lun, info, geotiff = geotiff
	;关闭文件
	free_lun, templun
	free_lun, lun
	;清除临时文件
	file_delete, temp_outfile
	;更新数据表:tm_file_info, 装载文件信息
	;先删去原有的信息
	sqlstr = 'delete from tm_file_info where zoneid=''' + pathname + rowname + ''' and sensorid=''tm5''' $
		+ ' and resolution=30 and datatime=to_date(''' + strmid(ptime, 0, 8) + ''', ''YYYYMMDD'') and passtime=''11:45'''
	;print, sqlstr
	dbexecutesql, sqlstr
	;再加入新的信息，文件名小写
	sqlstr = 'insert into tm_file_info(zoneid, sensorid, resolution, datatime, passtime, original_file) values(''' + pathname + rowname + ''', ''tm5''' $
		+ ', 30, to_date(''' + strmid(ptime, 0, 8) + ''', ''YYYYMMDD''), ''11:45'', ''' $
		+ strlowcase((file_info(outfile)).name) + ''')'
	;print, sqlstr
	dbexecutesql, sqlstr
	;构造结果信息
	satstr = [satstr, 'path and row:' + pathname + ',' + rowname, '过境时间：' + ptime, $
		' ', '输入空间分辨率：25米', '输入高度：' + strtrim(iHeight, 2) + '，输入宽度：' + strtrim(iWidth, 2), $
		' ', '输出空间分辨率：30米', '输出高度：' + strtrim(oHeight, 2) + '，输出宽度：' + strtrim(oWidth, 2), $
		' ', '输出文件名：' + outfile]
	;返回提示信息和结果路径
	return, { satstr:satstr, outfile:outfile }
end

;TM数据转换的转换功能，响应面板的运行按钮
pro TMDataTranslationExecute, event
	On_Error, 2
	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		msg = dialog_message('发生错误：' + !ERROR_STATE.MSG, title='TM数据转换', /error)
		CATCH, /CANCEL
		return
	endif
	;判断是一次执行还是批量执行
	uname = widget_info(event.id, /uname)
	case uname of
		'once' : begin
			;获取参数列表
			info = TMDataTranslationGetInfo(event)
			;参数不合理则退出
			if size(info, /type) ne 8 then return
			;调用转换的函数
			re = TMDataTranslationExecuteByInfo(info)
			;设置显示信息
			widget_control, event.top, get_uvalue = idlist
			widget_control, idlist.showinfo, set_value = re.satstr
			;更新TM预处理窗口上面的数据文件路径，把处理后的结果写到路径中去
			tmbaseid = widget_info(idlist.topbase, find_by_uname = 'tm_pre')
			;如果没有找到控件则退出
			if tmbaseid ne 0 then begin
				widget_control, tmbaseid, get_uvalue = topuv
				widget_control, topuv.datafile, set_value = re.outfile
				;调用函数，设置界面的值
				call_procedure, 'tm_openfile', tmbaseid
			endif
		end
		'batch' : begin
			batchfile = dialog_pickfile(title='请选择批处理参数文件', filter='*.ini', /read, /multiple)
			if batchfile[0] eq '' then return
			infostring = '处理信息如下：'
			for i=0, n_elements(batchfile)-1 do begin
				On_Error, 2
				CATCH, Error_status
				IF Error_status NE 0 THEN BEGIN
					infostring = [infostring, '参数文件' + batchfile[i] + '运行失败，错误信息如下：' + !ERROR_STATE.MSG]
					CATCH, /CANCEL
					continue
				endif
				;读取配置文件
				info = read_ini(batchfile[i])
				;调用转换的函数
				re = TMDataTranslationExecuteByInfo(info)
				infostring = [infostring, '参数文件' + batchfile[i] + '运行成功']
			endfor
			print, errorstring
		end
		else : return
	endcase
	;提示结束
	msg = dialog_message('完成TM数据转换功能!', title='TM数据转换', /info)
end

;TM数据转换源文件目录选择框打开功能
pro TMDataTranslationAddFiles, idlist
	widget_control, idlist.basepath, get_value = pathname
	if file_test(pathname[0], /directory) eq 0 then begin
		msg = dialog_message('请指定TM5源数据文件目录！', title='TM5数据转换', /info)
		return
	endif
	;搜索该目录下面的文件
	;卫星信息
	satinfo = file_search(pathname[0], 'scene01_ProductDescription.self', /fold_case)
	widget_control, idlist.satinfo, get_uvalue = stapathid
	widget_control, stapathid, set_value = satinfo[0]
	;投影信息
	prjinfo = file_search(pathname[0], 'header.dat', /fold_case)
	widget_control, idlist.prjinfo, get_uvalue = prjpathid
	widget_control, prjpathid, set_value = prjinfo[0]
	;
	for i=1, 7 do begin
		bandfiles = file_search(pathname[0], 'band' + strtrim(i, 2) +'.dat', /fold_case)
		widget_control, idlist.bandlist[i-1], get_uvalue = bandpathid
		widget_control, bandpathid, set_value = bandfiles[0]
	endfor
end

;TM数据转换源文件目录选择框打开事件处理
pro TMDataTranslationAddFilesButton, event
	widget_control, event.top, get_uvalue = idlist
	;
	TMDataTranslationAddFiles, idlist
end

;TM数据转换源文件目录选择框打开事件处理
pro TMDataTranslationSelectTm5FolderButton, event
	pathname = dialog_pickfile(title='选择源数据文件目录', /directory)
	if pathname[0] eq '' then return
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.basepath, set_value = pathname[0]
	;
	TMDataTranslationAddFiles, idlist
end

;TM数据转换输出目录选择框打开事件处理
pro TMDataTranslationSelectOutFolderButton, event
	pathname = dialog_pickfile(title='选择输出文件目录', /directory)
	if pathname[0] eq '' then return
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.outpath, set_value = pathname[0]
end

;TM数据转换文件选择框文件打开事件处理
pro TMDataTranslationSelectFileButton, event
	;默放情况下从源文件目录中选择
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.basepath, get_value = basepath

	filename = dialog_pickfile(title='选择文件', /read, path=basepath[0])
	if filename[0] eq '' then return
	widget_control, event.id, get_uvalue = fileid
	widget_control, fileid, set_value = filename[0]
end

;TM数据转换文件选择框
function TMDataTranslationSelectFileWidget, base, label
	base0 = widget_base(base, /row)
	lab = widget_label(base0, value=label)
	path = widget_text(base0, xsize=60, /editable)
	btn = widget_button(base0, /bitmap, value='image/viewer/open.bmp', $
		uvalue=path, event_pro='TMDataTranslationSelectFileButton')
	widget_control, base0, set_uvalue = path
	return, base0
end

;TM数据转换默认事件处理程序
pro TMDataTranslation_event, event
end

;TM数据转换窗口
pro TMDataTranslation, event
	tlb = widget_base(group_leader=event.top, /modal, title = 'TM数据转换', /col, xoffset=200, yoffset=15, space=0, tlb_frame_attr=1)
	;
	tab1 = widget_tab(tlb)
	base_tm5 = widget_base(tab1, title='TM5输入数据设定', /col, space=0)
		base2 = widget_base(base_tm5, /row)
			lab = widget_label(base2, value='源数据文件根目录：')
			tm5path = widget_text(base2, xsize=60, /editable)
			btn = widget_button(base2, /bitmap, value='image/viewer/open.bmp', $
				event_pro='TMDataTranslationSelectTM5FolderButton')
			btn = widget_button(base2, /bitmap, value='image/viewer/refresh.bmp', $
				event_pro='TMDataTranslationAddFilesButton')
		base2 = widget_base(base_tm5, /col, /frame, xpad=13)
			satinfo = TMDataTranslationSelectFileWidget(base2, ' 卫星信息文件：')
			prjinfo = TMDataTranslationSelectFileWidget(base2, ' 投影信息文件：')
			band1 = TMDataTranslationSelectFileWidget(base2, '波段1数据文件：')
			band2 = TMDataTranslationSelectFileWidget(base2, '波段2数据文件：')
			band3 = TMDataTranslationSelectFileWidget(base2, '波段3数据文件：')
			band4 = TMDataTranslationSelectFileWidget(base2, '波段4数据文件：')
			band5 = TMDataTranslationSelectFileWidget(base2, '波段5数据文件：')
			band6 = TMDataTranslationSelectFileWidget(base2, '波段6数据文件：')
			band7 = TMDataTranslationSelectFileWidget(base2, '波段7数据文件：')
	;
	tab2 = widget_tab(tlb)
	base0 = widget_base(tab2, title='输出GEOTIFF文件目录设定', /col)
		base1 = widget_base(base0, /row)
			;默认放置在TM预处理的目录之下
			dirs = getdefaultdirectorys()
			lab = widget_label(base1, value='   输出文件路径：')
			outpath = widget_text(base1, xsize=60, /editable, value=dirs.tmpre_dir)
			btn = widget_button(base1, /bitmap, value='image/viewer/open.bmp', $
				event_pro='TMDataTranslationSelectOutFolderButton')
		base1 = widget_base(base0, /col)
			lab = widget_label(base1, value='处理信息：', /align_left)
			showinfo = widget_text(base1, xsize=84, ysize=12, /scroll, /wrap)
		base1 = widget_base(base0, /row, /align_right, space=10)
			btn = widget_button(base1, value='执行转换', xsize=80, ysize=23, event_pro='TMDataTranslationExecute', uname='once')
			btn = widget_button(base1, value='保存运行信息', xsize=90, ysize=23, event_pro='TMDataTranslationSaveExeInfo')
			btn = widget_button(base1, value='批量执行转换', xsize=90, ysize=23, event_pro='TMDataTranslationExecute', uname='batch')
			btn = widget_button(base1, value='帮助', xsize=60, ysize=23, event_pro='call_help')
			btn = widget_button(base1, value='关闭', xsize=60, ysize=23, event_pro='close_event')
	;设置全局的自定义变量
	widget_control, tlb, /realize, set_uvalue = {topbase:event.top, basepath:tm5path, satinfo:satinfo, prjinfo:prjinfo, $
			bandlist:[band1, band2, band3, band4, band5, band6, band7], outpath:outpath, showinfo:showinfo}
	xmanager, 'TMDataTranslation', tlb
end

;以下是TM几何纠正部份的代码

;把浮点数字格式的矩阵的第一列转换为整数，用于在表格里显示控制点
function TMCorrectionGetFormatedTable, tablevalue
	formated_coordinate = strtrim(tablevalue[*, *], 2)
	formated_coordinate[0, *] = strtrim(long(tablevalue[0, *]), 2)
	return, formated_coordinate
end

;向控制点表格里添加控制点的功能
pro TMCorrectionAddControlPoint, idlist, workid, x, y
	widget_control, idlist.plist, get_uvalue = puv
	widget_control, workid, get_uvalue = widlist
	if puv.count eq 0 or puv.active_row eq -1 then return
	;设置坐标的值，把它添加到表中去
	puv.coordinate[widlist.pindex, puv.active_row] = strtrim(x, 2)
	puv.coordinate[widlist.pindex+1, puv.active_row] = strtrim(y, 2)
	;指示是否自动添加点
	isAutoadd = 0
	;判断是否要自动加入另一个点
	autoadd = widget_info(idlist.btnautoadd, /button_set)
	;至少三个点以上才能自动添加点
	if autoadd eq 1 and puv.count gt 3 then begin
		;x坐标对应
		;x1为原始影像上的坐标，x2为参考影像上的坐标
		x1list = rotate(puv.coordinate[1, 0:puv.count-2], 3)
		x2list = rotate(puv.coordinate[3, 0:puv.count-2], 3)
		;计算X坐标的斜率、截距和相关系数
		kx = regress(x1list, x2list, const=constx, sigma=sigmax)
		print, kx[0], constx, sigmax[0]
		;y坐标对应
		;y1为原始影像上的坐标，y2为参考影像上的坐标
		y1list = rotate(puv.coordinate[2, 0:puv.count-2], 3)
		y2list = rotate(puv.coordinate[4, 0:puv.count-2], 3)
		;Y坐标的斜率、截距和相关系数
		ky = regress(y1list, y2list, const=consty, sigma=sigmay)
		print, ky[0], consty, sigmay[0]
		;用回归方法计算另一个点的坐标
		if widlist.pindex eq 1 then begin
			;由原始影像计算参考影像上的点
			newx = kx[0] * x + constx
			newy = ky[0] * y + consty
			newindex = 3
			casid = idlist.ref_id
		endif else begin
			;由参考影像计算原始影像上的点
			newx = (x - constx) / kx[0]
			newy = (y + consty) / ky[0]
			newindex = 1
			casid = idlist.ori_id
		endelse
		puv.coordinate[newindex, puv.active_row] = strtrim(newx, 2)
		puv.coordinate[newindex+1, puv.active_row] = strtrim(newy, 2)
		isAutoadd = 1
		;
	endif
	;刷新表格的显示，并保存表格的值
	tablestring = TMCorrectionGetFormatedTable(puv.coordinate)
	widget_control, idlist.plist, set_value = tablestring, set_uvalue = puv
	;判断是否要显示自动添加的的
	if isAutoadd eq 1 then begin
		;显示自动添加的点
		widget_control, casid, get_uvalue = casidlist
		TMCorrectionDrawControlPoints, idlist, casidlist.drawid
		;添加结束后，取消另一个影像的添加状态
		widget_control, casidlist.toolBar, get_uvalue = castooluvalue
		castooluvalue.isAdd = 0
		widget_control, casidlist.toolBar, set_uvalue = castooluvalue
	endif
end

;在影像上面绘制控制点的功能
pro TMCorrectionDrawControlPoints, idlist, drawid
	;获取显示变量
	widget_control, drawid, get_uvalue = drawuvalue, get_value = owindow
	;清除上一次的显示模型
	obj_destroy, drawuvalue.objPointModel
	;生成控制点图像
	drawuvalue.objPointModel = Obj_New('IDLgrModel')
	;获取控制点坐标列表
	widget_control, idlist.plist, get_uvalue = puv
	;获取当前的工作变量
	widget_control, drawuvalue.workid, get_uvalue = widlist
	;设定控制点的边框的大小
	len1 = 7 * drawuvalue.zoomfactor
	len2 = 1.2 * drawuvalue.zoomfactor
	;逐个画点
	for i=0, puv.count-1 do begin
		;设定位置
		x = puv.coordinate[widlist.pindex, i]
		y = puv.coordinate[widlist.pindex+1, i]
		;设定颜色
		color = i eq puv.active_row ? [255,255,0] : [0,0,255]
		;添加四条线
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x-len1, x-len2], [y, y], color=color)
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x+len2, x+len1], [y, y], color=color)
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x, x], [y-len1, y-len2], color=color)
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x, x], [y+len1, y+len2], color=color)
		;添加边框
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolygon', [x-len1, x-len1, x+len1, x+len1, x-len1], $
			[y-len1, y+len1, y+len1, y-len1, y-len1], color=color, STYLE=1, thick=2)
		drawuvalue.objPointModel->Add, OBJ_NEW('IDLgrText', 'GCP #' + strtrim(long(puv.coordinate[0, i]), 2), $
			Locations=[x-4*len1, y-3.6*len1], COLOR=color)
	endfor
	;向当前View中加入控制点模型
	drawuvalue.objView->Add, drawuvalue.objPointModel
	;更新绘图区的变量
	widget_control, drawid, set_uvalue = drawuvalue
	;绘制带控制点的图像
	owindow->Draw, drawuvalue.objView
end

;绘制栅格图操作
;两个参数分别是全局id列表和当前绘图对象id列表
pro TMCorrectionDrawImage, idlist, workid
	;获取当前的自定义变量
	widget_control, workid, get_uvalue = widlist
	;获取绘图区的自定义变量
	widget_control, widlist.drawid, get_uvalue = drawuvalue, get_value = objwindow
	;先清除上一次的绘图对象
	obj_destroy, drawuvalue.objModel
	obj_destroy, drawuvalue.objView
	obj_destroy, drawuvalue.objImage
	;设置图像对象
	drawuvalue.objImage = OBJ_NEW('IDLgrImage', order=1, TILING=1, TILE_LEVEL_MODE=0, $
		TILED_IMAGE_DIMENSIONS=[drawuvalue.info.dimensions[0], drawuvalue.info.dimensions[1]])
	;由缩放比例和起始位置计算出显示区域大小
	width = drawuvalue.zoomFactor * 480.
	height = drawuvalue.zoomFactor * 480.
	;设置绘图范围
	drawuvalue.objView = OBJ_NEW('IDLgrView', VIEWPLANE_RECT = drawuvalue.image_rect)
	;分块绘图
	ReqTiles = objWindow->QueryRequiredTiles(drawuvalue.objView, drawuvalue.objImage, COUNT=nTiles)
	;print, 'Tiles'' count', nTiles
	FOR i = 0, nTiles - 1 DO BEGIN
		SubRect = [ReqTiles[i].X, ReqTiles[i].Y, ReqTiles[i].Width, ReqTiles[i].Height]
		TileData = READ_TIFF(drawuvalue.filename, SUB_RECT=SubRect)
		;
		TileData[1, *, *] = BYTSCL(TileData[1, *, *], MIN=drawuvalue.minmax.minmax2[0], MAX=drawuvalue.minmax.minmax2[1])
		TileData[2, *, *] = BYTSCL(TileData[2, *, *], MIN=drawuvalue.minmax.minmax3[0], MAX=drawuvalue.minmax.minmax3[1])
		TileData[3, *, *] = BYTSCL(TileData[3, *, *], MIN=drawuvalue.minmax.minmax4[0], MAX=drawuvalue.minmax.minmax4[1])
  		;
   		drawuvalue.objImage->SetTileData, ReqTiles[i], TileData[[3,2,1], *, *]
	ENDFOR
	;影像模型
	drawuvalue.objModel = Obj_New('IDLgrModel')
    drawuvalue.objModel->Add, drawuvalue.objImage
    drawuvalue.objView->Add, drawuvalue.objModel
    ;绘制栅格影像图
    objwindow->Draw, drawuvalue.objView
    ;savView = drawuvalue.objView
    ;save, savView, filename='e:\obj.txt'
    ;更新绘图区的变量
    widget_control, widlist.drawid, set_uvalue = drawuvalue
    ;向影像上面添加控制点
    widget_control, idlist.plist, get_uvalue = puv
    if puv.count gt 0 then TMCorrectionDrawControlPoints, idlist, widlist.drawid
end

;影像控件上的鼠标事件处理
pro TMCorrectionDrawEvent, event
	;获取对象
	widget_control, event.top, get_uvalue = idlist
	;影像控件ID
	drawid = event.id
	;获取图像对象并判断是否存在
	widget_control, drawid, get_uvalue = drawuvalue, get_value=owindow
	if size(drawuvalue, /type) ne 8 then return
	;当前影像的工作ID
	workid = drawuvalue.workid
	widget_control, workid, get_uvalue = widlist
	;设置鼠标经过的点的坐标
	x = drawuvalue.image_rect[0] + event.x * drawuvalue.zoomFactor
	y = drawuvalue.image_rect[1] + event.y * drawuvalue.zoomFactor
	;print,x,y
	widget_control, widlist.xid ,set_value = strtrim(x, 2)
	widget_control, widlist.yid ,set_value = strtrim(y, 2)
	;判断绘图区按钮的状态
	widget_control, widlist.toolBar, get_uvalue = tooluvalue
	;根据绘图区的按钮的状态来确定要完成的动作
	if tooluvalue.status eq 0 then begin
		;鼠标处于光标状态，用于挪动控制点
		;获取控制点列表信息
		widget_control, idlist.plist, get_uvalue = puv
		;判断是否在挪动控制点
		if event.type eq 2 then begin
			;判断是否在拖动所选点
			if drawuvalue.mouseStatus eq 0 then return
			;处于拖动状态，修改活动点的坐标
			;防止鼠标移出当前区域
			boxsize = 480
			mousex = event.x > 0
			mousex <= boxsize
			mousey = event.y > 0
			mousey <= boxsize
			;设置标记点的坐标
			puv.coordinate[widlist.pindex, puv.active_row] = x
			puv.coordinate[widlist.pindex+1, puv.active_row] = y
			;保存标记点的坐标
			widget_control, idlist.plist, set_uvalue = puv
			;刷新对控制点的显示
			TMCorrectionDrawControlPoints, idlist, drawid
		endif
		;鼠标左键点击动作
		if event.type eq 0 and event.press eq 1 then begin
			;在控制点状态下，鼠标左键单击，需要处理两种可能的情况：
			;一是添加新的控制点
			if tooluvalue.isAdd eq 1 then begin
				;调用添加控制点的功能
				TMCorrectionAddControlPoint, idlist, workid, x ,y
				;在影像窗口上面绘制控制点
				TMCorrectionDrawControlPoints, idlist, widlist.drawid
				;添加结束后，取消添加状态
				tooluvalue.isAdd = 0
				widget_control, widlist.toolBar, set_uvalue = tooluvalue
			endif
			;添加新点以后可以立即挪动新添加的点（因为它是活动点）
			;二是移动已有的控制点
			if size(puv, /type) ne 8 then return
			if puv.count eq 0 or puv.active_row eq -1 then return
			;当前的活动标记点所在的位置（由图像坐标转换为设备坐标）
			px = puv.coordinate[widlist.pindex, puv.active_row]
			py = puv.coordinate[widlist.pindex+1, puv.active_row]
			;判段点击点是否位于所选点的区域以内
			;如是在范围以内则认为拖动开始
			x_dis = abs(x - px) / drawuvalue.zoomFactor
			y_dis = abs(y - py) / drawuvalue.zoomFactor
			if x_dis le 10 and y_dis le 10 then begin
				;设置为开始挪动的状态
				drawuvalue.mouseStatus = 1
				widget_control, drawid, set_uvalue = drawuvalue
			endif
		endif
		;挪动控制点结束（鼠标左键释放事件）
		if event.type eq 1 and event.release eq 1 then begin
			;判断是否在拖动所选点
			if drawuvalue.mouseStatus eq 0 then return
			;设置为非拖动状态
			drawuvalue.mouseStatus = 0
			widget_control, drawid, set_uvalue = drawuvalue
			;释放后，更新相应的数据点的坐标
			;获取释放点的坐标
			puv.coordinate[widlist.pindex, puv.active_row] = x
			puv.coordinate[widlist.pindex+1, puv.active_row] = y
			;更新表格数据并刷新显示
			tablestring = TMCorrectionGetFormatedTable(puv.coordinate)
			widget_control, idlist.plist, set_uvalue = puv, set_value = tablestring
		endif
	endif else if tooluvalue.status eq 1 then begin
		;漫游操作，用于移动图像
		;判断是否在拖动
		if event.type eq 2 then begin
			;判断是否在拖动所选点
			if drawuvalue.mouseStatus eq 0 then return
			;计算相对移动距离
			dx = (x - drawuvalue.startpos[0])
			dy = (y - drawuvalue.startpos[1])
			;设置新影像的区域
			new_rect = drawuvalue.image_rect[*]
			new_rect[0] -= dx
			new_rect[1] -= dy
			;画出影像
			drawuvalue.objView->SetProperty, viewplane_rect = new_rect
    		owindow->Draw, drawuvalue.objView
		endif
		;（漫游开始）鼠标左键点击动作
		if event.type eq 0 and event.press eq 1 then begin
			;设置鼠标为拖动的起始状态
			drawuvalue.mouseStatus = 1
			drawuvalue.startpos = [x, y]
			widget_control, drawid, set_uvalue = drawuvalue
		endif
		;（漫游结束）鼠标左键释放事件
		if event.type eq 1 and event.release eq 1 then begin
			;判断是否处于拖动状态
			if drawuvalue.mouseStatus eq 0 then return
			;设置为非拖动状态
			drawuvalue.mouseStatus = 0
			;计算相对移动距离
			dx = (x - drawuvalue.startpos[0])
			dy = (y - drawuvalue.startpos[1])
			;设置新影像的区域，并且保证不出界
			drawuvalue.image_rect[0] -= dx
			drawuvalue.image_rect[0] >= 0
			drawuvalue.image_rect[0] <= drawuvalue.info.dimensions[0] - 480 * drawuvalue.zoomFactor
			;
			drawuvalue.image_rect[1] -= dy
			drawuvalue.image_rect[1] >= 0
			drawuvalue.image_rect[1] <= drawuvalue.info.dimensions[1] - 480 * drawuvalue.zoomFactor
			;保存当前的位置值
			widget_control, drawid, set_uvalue = drawuvalue
			;漫游结束后需重新绘制图像
			TMCorrectionDrawImage, idlist, workid
		endif
	endif else if tooluvalue.status eq 2 or tooluvalue.status eq 3 then begin
		;鼠标处于放大或缩小状态，用于缩放图像
		;判断是否在拖动
		if event.type eq 2 then begin
			;判断是否在拖动所选点
			if drawuvalue.mouseStatus eq 0 then return
			;鼠标起始点
			x1 = drawuvalue.startpos[0]
			y1 = drawuvalue.startpos[1]
			;鼠标结束点,需要判断合法性,防止鼠标移出当前区域
			x2 = x > 0
			x2 = x2 < drawuvalue.info.dimensions[0]
			;
			y2 = y > 0
			y2 = y2 < drawuvalue.info.dimensions[1]
			;画出跟踪的线框
			oModel = Obj_New('IDLgrModel')
			pll = Obj_New('IDLgrPolyLine', [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], color=[0,0,255])
			oModel->Add, pll
			;
    		drawuvalue.objView->Add, oModel
    		owindow->Draw, drawuvalue.objView
    		obj_destroy, oModel
		endif
		;鼠标左键点击动作
		if event.type eq 0 and event.press eq 1 then begin
			;设置鼠标为拖动的起始状态
			drawuvalue.mouseStatus = 1
			drawuvalue.startpos = [x, y]
			widget_control, drawid, set_uvalue = drawuvalue
		endif
		;拖动结束（鼠标左键释放事件）
		if event.type eq 1 and event.release eq 1 then begin
			;判断是否在拖动所选点
			if drawuvalue.mouseStatus eq 0 then return
			;设置为非拖动状态
			drawuvalue.mouseStatus = 0
			;释放后，计算鼠标拖动区域
			start_x = drawuvalue.startpos[0]
			start_y = drawuvalue.startpos[1]
 			end_x = x > 0
			end_x = end_x < drawuvalue.info.dimensions[0]
			end_y = y > 0
			end_y = end_y < drawuvalue.info.dimensions[1]
			;移动的最长距离
			maxdis = max([abs(end_x - start_x), abs(end_y - start_y)])
			;确定是放大还是缩小操作
			if tooluvalue.status eq 2 then begin
				;放大操作
				;如果鼠标在同一位置点击，则放大一倍
				if maxdis le 1 then maxdis = drawuvalue.image_rect[2] / 2
				;限制最小的显示区域不低于24个像素，即20倍放大(480 / 20 = 24)
				maxdis = maxdis > 24
			endif else begin
				;缩小操作
				;如果鼠标在同一位置点击，则缩小一半
				if maxdis le 1 then maxdis = drawuvalue.image_rect[2] / 2
				maxdis = (drawuvalue.image_rect[2]^2) / maxdis
				;限制最大的显示区域为图像的最大尺寸
				maxdis = maxdis < drawuvalue.maxsize
			endelse
			;划动框的中心点
			center_x = (start_x + end_x) / 2.
			center_x = center_x < (drawuvalue.info.dimensions[0] - maxdis / 2.)
			center_x = center_x > (maxdis / 2.)
			center_y = (start_y + end_y) / 2.
			center_y = center_y < (drawuvalue.info.dimensions[1] - maxdis / 2.)
			center_y = center_y > (maxdis / 2.)
			;确定影像显示区域
			drawuvalue.image_rect = [center_x - maxdis / 2., center_y - maxdis / 2., maxdis, maxdis]
			;更新放大倍数
			drawuvalue.zoomfactor = maxdis / 480.
			print, 'zoomfactor', drawuvalue.zoomfactor
			;保存设置值
			widget_control, drawid, set_uvalue = drawuvalue
			;更新影像显示
			TMCorrectionDrawImage, idlist, workid
		endif
	endif
end

;放大、缩小、漫游、原始大小和光标形状事件
pro TMCorrectionZoomEvent, event
	widget_control, event.top, get_uvalue = idlist
	;获取当前按钮的base控件
	toolbar = widget_info(event.id, /parent)
	widget_control, toolbar, get_uvalue = tuv
	index = where(tuv.btnlist eq event.id)
	;判断状态
	if index eq 4 then begin
		;处理缩放到全图的事件
		;获取工作变量
		widget_control, tuv.workid, get_uvalue = widlist
		if size(widlist, /type) ne 8 then return
		;获取影像变量
		widget_control, widlist.drawid, get_uvalue = drawuvalue
		;设置显示区域为全图
		drawuvalue.image_rect = [0, 0, drawuvalue.maxsize, drawuvalue.maxsize]
		;更新放大倍数
		drawuvalue.zoomfactor = drawuvalue.maxsize / 480.
		;保存设置值
		widget_control, widlist.drawid, set_uvalue = drawuvalue
		;更新影像显示
		TMCorrectionDrawImage, idlist, tuv.workid
		;将按钮设置成光标状态
		tuv.status = 0
		widget_control, tuv.btnlist[0], /set_button
	endif else begin
		;更新按钮的状态
		tuv.status = index
	end
	;更新状态
	widget_control, toolbar, set_uvalue = tuv
end

;控制点列表的事件处理
pro TMCorrectionTableEvent, event
	;只处理点击事件
	if tag_names(event, /structure_name) ne 'WIDGET_TABLE_CELL_SEL' then return
	;不处理全选事件
	if event.sel_top eq -1 then return
	widget_control, event.top, get_uvalue = idlist
	;清空选择
	widget_control, idlist.plist, set_table_select=[-1,-1,-1,-1]
	;只选择选择区的第一行
	widget_control, idlist.plist, set_table_select=[0 , event.sel_top, 4, event.sel_top]
	;将该行设置为活动行
	widget_control, idlist.plist, get_uvalue = puv
	if puv.count eq 0 then return
	puv.active_row = event.sel_top
	widget_control, idlist.plist, set_uvalue = puv
	;重新绘制控制点,以显示新的活动控制点
	;原始影像
	widget_control, idlist.ori_id, get_uvalue = oriuvalue
	TMCorrectionDrawControlPoints, idlist, oriuvalue.drawid
	;参考影像
	widget_control, idlist.ref_id, get_uvalue = refuvalue
	TMCorrectionDrawControlPoints, idlist, refuvalue.drawid
end

;对控制点的添加、删除、转到和设置颜色操作
pro TMCorrectionControlPoint, event
	widget_control, event.top, get_uvalue = idlist
	;
	;获取按钮名称
	uname = widget_info(event.id, /uname)
	;获取控制点列表
	widget_control, idlist.plist, get_uvalue = puv
	;获取原始影像的打开情况
	widget_control, idlist.ori_id, get_uvalue = oriuvalue
	widget_control, oriuvalue.drawid, get_uvalue = oridrawuvalue
	if size(oridrawuvalue, /type) ne 8 then begin
		msg = dialog_message('没有原始影像，无法完成操作！', title='TM几何纠正', /infor)
		return
	endif
	;获取参考影像的打开情况
	widget_control, idlist.ref_id, get_uvalue = refuvalue
	widget_control, refuvalue.drawid, get_uvalue = refdrawuvalue
	if size(refdrawuvalue, /type) ne 8 then begin
		msg = dialog_message('没有参考影像，无法完成操作！', title='TM几何纠正', /infor)
		return
	endif
	;分别处理
	case uname of
		;添加控制点
		'add' : begin
			;设置原始影像添加控制点的标识
			widget_control, oriuvalue.toolBar, get_uvalue = oritooluv
			widget_control,  oritooluv.btnlist[0], /set_button
			oritooluv.status = 0
			oritooluv.isAdd = 1
			widget_control, oriuvalue.toolBar, set_uvalue = oritooluv
			;设置参考影像添加控制点的标识
			widget_control, refuvalue.toolBar, get_uvalue = reftooluv
			widget_control,  reftooluv.btnlist[0], /set_button
			reftooluv.status = 0
			reftooluv.isAdd = 1
			widget_control, refuvalue.toolBar, set_uvalue = reftooluv
			;设置控制点列表
			if puv.count eq 0 then begin
				;方案为空，初始化表数据
				puv_new = {count : 1, coordinate : [1, -1., -1., -1., -1.], active_row:0}
			endif else begin
				;方案不为空，需要判断上一次添加新控制点的动作有没有完成
				if (where(puv.coordinate[*, puv.count-1] eq -1))[0] ne -1 then begin
					;提示必须完成上一次的添加新控制操作
					msg = dialog_message('请完成序号为 ' + strtrim(puv.coordinate[0, puv.count-1], 2) + $
						' 的控制点的添加操作！', title='TM几何纠正', /info)
					return
				endif
				;添加新行
				new_index = strtrim(max(long(puv.coordinate[0,*])) + 1, 2)
				puv_new = {count : puv.count+1, coordinate : [[puv.coordinate],[new_index, -1, -1, -1, -1]], active_row:puv.count}
			endelse
			tablestring = TMCorrectionGetFormatedTable(puv_new.coordinate)
			;更新表格的显示和数据
			widget_control, idlist.plist, table_ysize=puv_new.count, set_value=tablestring, set_uvalue=puv_new
			widget_control, idlist.plist, set_table_select = [0,puv_new.active_row,4,puv_new.active_row]
		end
		;删除所选择的控制点
		'delete' : begin
			if puv.count eq 0 then begin
				msg = dialog_message('没有控制点数据，无法删除！', title='TM几何纠正', /information)
				return
			endif
			;所选择区域的第一行
			rowindex = (widget_info(idlist.plist, /table_select))[1]
			if rowindex eq -1 then return
			;删除选中的一行
			if puv.count eq 1 then begin
				;只有一行,删除后清空表格内容,但显示一空行
				puv_new = {count : 0, active_row:-1, coordinate:['','','','','']}
				;设置显示行数
				table_rows = 1
			endif else begin
				;多行数据，首先确定被选择的行的序号
				prowid = puv.coordinate[0, rowindex]
				;序号的列表
				rowidlist = puv.coordinate[0, *]
				;从表中删除掉所选择的行
				puv_new = {count : puv.count - 1, $
					coordinate : [[puv.coordinate[*, where(rowidlist ne prowid)]]], $
					active_row:-1}
				;设置显示行数
				table_rows = puv_new.count
			endelse
			;更新表格数据
			coordinate = strtrim(puv_new.coordinate[*, *], 2)
			coordinate[0, *] = strtrim(long(coordinate[0, *]), 2)
			widget_control, idlist.plist, table_ysize=table_rows, set_uvalue = puv_new, $
				set_value = coordinate, set_table_select=[-1,-1,-1,-1]
			;1 刷新原始影像上的控制点的显示
			widget_control, idlist.ori_id, get_uvalue = ori_widlist
			TMCorrectionDrawControlPoints, idlist, ori_widlist.drawid
    		;2 刷新参考影像上的控制点的显示
    		widget_control, idlist.ref_id, get_uvalue = ref_widlist
    		TMCorrectionDrawControlPoints, idlist, ref_widlist.drawid
		end
		;保存控制点列表到文件中
		'savelist' : begin
			if puv.count eq 0 then begin
				msg = dialog_message('当前方案表为空，不能保存！', title='TM几何纠正', /info)
				return
			endif
			;打开控制点文件
			widget_control, idlist.controlfile, get_uvalue = filename
			;提示用户选择，默认为上一次指定的文件
			filename = dialog_pickfile(title='选择控制点列表文件', filter='*.dat', default_extension='dat', file=filename)
			if filename[0] eq '' then return
			widget_control, idlist.controlfile, set_uvalue = filename[0]
			;保存控制点信息
			openw, lun, filename[0], /get_lun
			for i=0, puv.count-1 do begin
				filestr = strtrim(puv.coordinate[0, i], 2)
				filestr += ',' + strtrim(puv.coordinate[1, i], 2)
				filestr += ',' + strtrim(puv.coordinate[2, i], 2)
				filestr += ',' + strtrim(puv.coordinate[3, i], 2)
				filestr += ',' + strtrim(puv.coordinate[4, i], 2)
				printf, lun, filestr
			endfor
			free_lun, lun
		end
		;从文件中加载控制点列表
		'loadlist' : begin
			if puv.count gt 0 then begin
				msg = dialog_message('当前方案表不为空，是否继续？', title='TM几何纠正', /question)
				if msg eq 'No' then return
			endif
			;
			filename = dialog_pickfile(title='选择控制点列表文件', filter='*.dat', default_extension='dat')
			if filename[0] eq '' then return
			if file_test(filename[0]) ne 1 then begin
				msg = dialog_message('加载控制点列表文件失败！', title='TM几何纠正', /question)
				return
			endif
			;读取文件行数
			openr, lun, filename[0], /get_lun
			table_rows = file_lines(filename[0])
			if table_rows eq 0 then begin
				msg = dialog_message('文件为空，加载失败！', title='TM几何纠正', /info)
				return
			endif
			;读入数据
			filestr = strarr(table_rows)
			readf, lun, filestr
			free_lun, lun
			;拆分数据
			coordinate = strarr(5, table_rows)
			for i=0,table_rows-1 do begin
				spos = strsplit(filestr[i], ',')
				;如果不是5个分隔的逗号，则该行无效
				if n_elements(spos) ne 5 then continue
				coordinate[0, i] = strtrim(long(strmid(filestr[i], spos[0], spos[1]- spos[0]-1)), 2)
				coordinate[1, i] = strmid(filestr[i], spos[1], spos[2]- spos[1]-1)
				coordinate[2, i] = strmid(filestr[i], spos[2], spos[3]- spos[2]-1)
				coordinate[3, i] = strmid(filestr[i], spos[3], spos[4]- spos[3]-1)
				coordinate[4, i] = strmid(filestr[i], spos[4])
			endfor
			;更新表格数据
			puv_new = {count : table_rows, coordinate : float(coordinate), active_row:0}
			widget_control, idlist.plist, table_ysize=table_rows, set_uvalue = puv_new, $
				set_value = coordinate, set_table_select=[-1,-1,-1,-1]
			;刷新显示
			;1 刷新原始影像上的控制点的显示
			widget_control, idlist.ori_id, get_uvalue = ori_widlist
			TMCorrectionDrawControlPoints, idlist, ori_widlist.drawid
    		;2 刷新参考影像上的控制点的显示
    		widget_control, idlist.ref_id, get_uvalue = ref_widlist
    		TMCorrectionDrawControlPoints, idlist, ref_widlist.drawid
		end
	endcase
end

;打开文件功能
pro TMCorrection_open, idname, filename, filetype
	widget_control, idname, get_uvalue = idlist
	if filetype eq 'orifile' then begin
		;打开原始影像
		qr = query_tiff(filename, info)
		;判断是否是多通道的(不再提供支持单通道TIFF的功能)
		if info.channels ne 7 then begin
			msg = dialog_message('原始影像不是合法的7通道TIFF文件', title='TM几何纠正')
			return
		endif
		;保存影像长宽的最大值
		maxsize = max([info.dimensions[0:1]])
		;工作类型
		workid = idlist.ori_id
		;读取参考影像
		data432 = read_tiff(filename, channels=[1,2,3])
	endif

	;打开参考影像
	if filetype eq 'reffile' then begin
		qr = query_tiff(filename, info, geotiff=geotiff)
		;判断是否是多通道的(不再提供支持单通道TIFF的功能)
		if info.channels ne 7 then begin
			msg = dialog_message('参考影像不是合法的7通道TIFF文件', title='TM几何纠正')
			return
		endif
		if size(geotiff, /type) ne 8 then begin
			msg = dialog_message('参考影像的投影无效！', title='TM几何纠正')
			return
		endif
		;保存影像长宽的最大值
		maxsize = max([info.dimensions[0:1]])
		;工作类型
		workid = idlist.ref_id
		;读取参考影像
		data432 = read_tiff(filename, channels=[1,2,3])
	endif

	;分别计算4/3/2通道的拉伸参数
	minmax4 = getminmaxvalue(data432[2, *, *])
	minmax3 = getminmaxvalue(data432[1, *, *])
	minmax2 = getminmaxvalue(data432[0, *, *])
	minmax = {minmax2:minmax2, minmax3:minmax3, minmax4:minmax4}
	;计算初始的缩放比例（显示区域确定为480像素）
	zoomFactor = maxsize / 480.
	;设置默认的缩放状态
	widget_control, workid, get_uvalue = widlist
	widget_control, widlist.toolBar, get_uvalue = tuv
	widget_control, tuv.btnlist[0], /set_button
	;设置绘图对象的变量
	widget_control, widlist.drawid, set_uvalue = {workid:workid, mouseStatus:0, $
		filename : filename[0], info:info, maxsize:maxsize, zoomfactor:zoomFactor, $
		startpos : [0,0], image_rect:[0,0,maxsize, maxsize], minmax:minmax, $
		objImage:obj_new(), objModel:obj_new(), objView:obj_new(), objPointModel:obj_new() }
	;调用绘图操作
	TMCorrectionDrawImage, idlist, workid
end

;TM几何纠正处理窗口关闭事件
pro TMCorrectionOnClose, event
	;全局ID列表
	widget_control, event.top, get_uvalue = idlist
	;原始图像ID列表
	widget_control, idlist.ori_id, get_uvalue = orilist
	widget_control, orilist.drawid, get_uvalue = ori_drawobj
	if size(ori_drawobj, /type) eq 8 then $
		obj_destroy, [ori_drawobj.objImage, ori_drawobj.objView, $
			ori_drawobj.objModel, ori_drawobj.objPointModel]
	;参考图像ID列表
	widget_control, idlist.ref_id, get_uvalue = reflist
	widget_control, reflist.drawid, get_uvalue = ref_drawobj
	if size(ref_drawobj, /type) eq 8 then $
		obj_destroy, [ref_drawobj.objImage, ref_drawobj.objView, $
			ref_drawobj.objModel, ref_drawobj.objPointModel]
	;关掉窗口
	widget_control, event.top, /destroy
end

;TM几何纠正的功能
function WARP_TRI_YLD, xo, yo, xi, yi, im_in, OUTPUT_SIZE = output_size
	s = SIZE(im_in)
	if s[0] ne 2 then print, 'TM几何纠正：数据格式不正确'
	;数据大小
	if n_elements(output_size) ge 2 then begin
		nx = output_size[0]
		ny = output_size[1]
	endif else begin
		nx = s[1]
		ny = s[2]
	endelse
	;计算三角网
	TRIANGULATE, xo, yo, tr, bounds
	gs = [1,1]				;Grid spacing
	;临时变量
	im_out = bytarr(nx, ny)
	;每次若干行进行纠正
	rows_size = 30
	for i=0, ny - 1, rows_size do begin
		;计算最大的Y坐标
		bottompos = (i + rows_size) gt ny ? (ny - 1) : (i + rows_size)
		;计算当前进行纠正的区域
		b = [0, i, nx-1, bottompos]
		;计算位置参数
		xpos = TRIGRID(xo, yo, xi, tr, gs, b)
		ypos = TRIGRID(xo, yo, yi, tr, gs, b)
		;进行插值
		im_out[*, i : bottompos] = INTERPOLATE(im_in[*, *], xpos, ypos)
	endfor
	;返回结果
	return, im_out
end

;把几何纠正的信息保存到结构体变量之中
function TMCorrectionGetRunInfo, event
	;全局ID列表
	widget_control, event.top, get_uvalue = idlist

	;控制点列表
	widget_control, idlist.plist, get_uvalue = puv
	if puv.count eq 0 then begin
		msg = dialog_message('没有控制点列表，无法运行几何纠正功能！', title='TM几何纠正', /infor)
		return, 0
	endif
	;控制点的个数不能少于三个
	if puv.count lt 3 then begin
		msg = dialog_message('至少要选择6个更多的控制点，否则无法运行几何纠正功能！', title='TM几何纠正', /infor)
		return, 0
	endif

	;原始影像文件信息
	widget_control, idlist.ori_id, get_uvalue = ori_widlist
	widget_control, ori_widlist.drawid, get_uvalue = oridrawuvalue
	;参考影像文件信息
	widget_control, idlist.ref_id, get_uvalue = ref_widlist
	widget_control, ref_widlist.drawid, get_uvalue = refdrawuvalue

	;设定输出文件，存放在tm预处理的目录之下，名字在原始影像后加geo.tif
	dirs = getdefaultdirectorys()
	outfile = dirs.tmpre_dir + '\' + file_basename(oridrawuvalue.filename, '.tif') + '_geo.tif'
	;ROI文件
	rofile = ''
	if size(idlist.roifile, /type) eq 7 then roifile = idlist.roifile
	;设置要保存的变量
	tmcorrection_info = {tmcorrection_info, $
		orifilename : oridrawuvalue.filename, $
		reffilename : refdrawuvalue.filename, $
		roifile		: roifile, $	;zone区域掩码文件
		outfilename : outfile, $	;输出文件
		x1list : rotate(puv.coordinate[1, *], 3), $ ;原始控制点X坐标
		x2list : rotate(puv.coordinate[3, *], 3), $	;参考控制点X坐标
		y1list : rotate(puv.coordinate[2, *], 3), $ ;原始控制点Y坐标
		y2list : rotate(puv.coordinate[4, *], 3) $ ;参考控制点Y坐标
	}
	return, tmcorrection_info
end

;把几何纠正的信息保存到结构体变量之中，并写到参数文件里
pro TMCorrectionSaveRunInfo, event
	;提示用户选择要保存的文件
	filename = dialog_pickfile(title='请选择输出文件', filter='*.ini', /write)
	if filename[0] eq '' then return

	;保存控制点文件路径和原始影像路径
	info = TMCorrectionGetRunInfo(event)
	;构造用于保存到文件的结构体
	info1 = { orifilename : info.orifilename, $
		reffilename : info.reffilename, $
		outfilename : info.outfilename, $
		roifile		: info.roifile, $
		x1list : '', x2list : '', y1list : '', y2list : '' $
	}

	;把数字坐标转换成逗号分隔值字符串
	listlen = n_elements(info.x1list)
	for i=0, listlen-1 do begin
		info1.x1list += strtrim(info.x1list[i], 2) + ','
		info1.x2list += strtrim(info.x2list[i], 2) + ','
		info1.y1list += strtrim(info.y1list[i], 2) + ','
		info1.y2list += strtrim(info.y2list[i], 2) + ','
	endfor
	;保存结构体变量
	write_struct, filename[0], info1, /ini
end

;把几何纠正之后的文件进行切割
function do_subset, inputfile, roifile, outputifle
;说明: f1为待切割图像，f2为roi(提供切后图像的空间范围和geotiff信息),f3为输出文件名(可与f1相同)
;目前支持如下切割：
;1.等分辨率下，f1全包围f2，此时结果为f1中对应f2空间范围内的部分
;2.等分辨率下，f1比f2小，此时结果为f1位于f2空间范围内的部分，无值处留空
;3.等分辨率下，f1与f2有交叉部分，此时结果为f1与f2交集中位于f2空间范围内的部分，无值处留空
;4.不等分辨率下，f1全包围f2，此时结果为f1中对应f2空间范围内的部分
;5.不等分辨率下，f1比f2小，此时结果为f1位于f2空间范围内的部分，无值处留空
;6.不等分辨率下，f1与f2有交叉部分，此时结果为f1与f2交集中位于f2空间范围内的部分，无值处留空

	void = query_tiff(inputfile, inf1, geotiff=geo1)
	void = query_tiff(roifile, inf2, geotiff=geo2)
	datatype = size(read_tiff(inputfile, sub_rect=[0,0,1,1]), /type)
	dim1 = inf1.dimensions
	dim2 = inf2.dimensions
	ul1 = geo1.modeltiepointtag[3:4]
	ul2 = geo2.modeltiepointtag[3:4]
	scale1 = geo1.modelpixelscaletag[0:1]
	scale2 = geo2.modelpixelscaletag[0:1]
	ratio = scale2 / scale1

	box1 = [ul1, ul1 + dim1 * scale1 *  [1,-1]]
	box2 = [ul2, ul2 + dim2 * scale2 *  [1,-1]]
	insect_box = [box1[0] > box2[0], box1[1] < box2[1], box1[2] < box2[2], box1[3] > box2[3]]
	;判断如果要裁切的影像和ROI影像没有交集，则返回-1
	if insect_box[0] le insect_box[2] or insect_box[1] le insect_box[3] then return, 1
	ul = (insect_box[0:1] - ul1) / scale1 * [1,-1]
	lr = (insect_box[2:3] - ul1) / scale1 * [1,-1]
	sub_rect = [ul, lr-ul]

	ul = (insect_box[0:1] - ul2) / scale2 * [1,-1]
	lr = (insect_box[2:3] - ul2) / scale2 * [1,-1]

	dim3 = fix([lr[0] - ul[0], lr[1] - ul[1]])
	if inf1.channels gt 1 then begin
		;多通道
		;img = make_array(inf1.channels, dim2[0], dim2[1], type = datatype)
		;img[*, ul[0]:ul[0] + dim3[0] - 1, ul[1]:ul[1] + dim3[1] -1] = congrid(read_tiff(f1, sub_rect = sub_rect), inf1.channels, dim3[0], dim3[1], /interp)
		;逐通道进行数据拉伸
		tempfile = inputfile + '.tmp'
		;打开临时文件
		openw, templun, tempfile, /get_lun
		;记录数据位置
		fileposlist = lonarr(inf1.channels)
		for i=0, inf1.channels-1 do begin
			;获取一个通道的数据
			newdata = congrid(read_tiff(inputfile, sub_rect = sub_rect, channels=i), dim3[0], dim3[1], /interp)
			;写临时文件，并记录文件指针的位置
			point_lun, -templun, filepos
			fileposlist[i] = filepos
			writeu, templun, newdata
		endfor
		;
		;打开结果文件
		openw, lun, outputifle, /get_lun
		;写tiff文件头信息
		header = bytarr(8)
		writeu, lun, header
		;分块合成各个通道，并写入结果文件
		rows_size = 300
		data1 = bytarr(dim3[0], rows_size)
		data3 = bytarr(7, dim3[0], rows_size)
		for i=0, dim3[1]- rows_size - 1, rows_size do begin
			for j=0, inf1.channels-1 do begin
				point_lun, templun, fileposlist[j]
				readu, templun, data1
				data3[j, *, *] = data1
				point_lun, -templun, filepos
				fileposlist[j] = filepos
			endfor
			writeu, lun, data3
		endfor
		;写余下的部分
		theheight = dim3[1] - i
		;当超过一行剩余时，写剩余的行
		if theheight gt 0 then begin
			data1 = bytarr(dim3[0], theheight)
			data3 = bytarr(7, dim3[0], theheight)
			for j=0, 6 do begin
				point_lun, templun, fileposlist[j]
				readu, templun, data1
				data3[j, *, *] = data1
				point_lun, -templun, filepos
				fileposlist[j] = filepos
			endfor
			writeu, lun, data3
		endif
		;加入tiff投影信息
		info = { dimensions : dim3 }
		geotiff = { resolution : [geo1.modelpixelscaletag[0], geo1.modelpixelscaletag[1], 0], $
			prjlocation : [0.,0.,0.,insect_box[0], insect_box[1], 0.]}
		makegeototifffile, lun, info, geotiff = geotiff
		;关闭文件
		free_lun, templun
		free_lun, lun
		;清除临时文件
		file_delete, tempfile
	endif else begin
		;单通道
		img = make_array(dim2[0], dim2[1], type = datatype)
		img[ul[0]:ul[0] + dim3[0] - 1, ul[1]:ul[1] + dim3[1] -1] = congrid(read_tiff(inputfile, sub_rect = sub_rect), dim3[0], dim3[1], /interp)
		write_tiff, f3, img, /float,geotiff=geo2
	endelse
	;裁切成功，返回0值
	return, 0
end

;TM几何纠正进行几何纠正的功能，直接通过参数结构体运行
function TMCorrectionRunByInfo, info
;获取文件信息和原始影像的goetiff信息
	r1 = query_tiff(info.orifilename, oriinfo)
	r2 = query_tiff(info.reffilename, refinfo, geotiff=ref_geotiff)

	;计算X坐标的斜率、截距和相关系数
	kx = regress(info.x1list, info.x2list, const=constx, sigma=sigmax)
	print, kx[0], constx, sigmax[0]
	;左
	leftx1 = 0
	leftx2 = constx
	;右
	rightx2 = oriinfo.dimensions[0]
	rightx1 = (rightx2 - constx) / kx[0]
	print, 'x1', leftx1, rightx1
	print, 'x2', leftx2, rightx2
	;y坐标对应
	;y1为原始影像上的坐标，y2为参考影像上的坐标

	;计算X坐标的斜率、截距和相关系数
	ky = regress(info.y1list, info.y2list, const=consty, sigma=sigmay)
	print, ky[0], consty, sigmay[0]
	;上
	topy2 = oriinfo.dimensions[1]
	topy1 = (topy2 - consty) / ky[0]
	;下
	bottomy1 = 0
	bottomy2 = consty
	print, 'y1', bottomy1, topy1
	print, 'y2', bottomy2, topy2
	;把四角控制点加入到控制点序列中去
	x1list = [info.x1list, leftx1, leftx1, rightx1, rightx1]
	y1list = [info.y1list, topy1, bottomy1, topy1, bottomy1]

	x2list = [info.x2list, leftx2, leftx2, rightx2, rightx2] - leftx2
	y2list = [info.y2list, topy2, bottomy2, topy2, bottomy2] - bottomy2
	;x2list = x2list[*] - leftx2
	;y2list = y2list[*] - bottomy2
	print, x1list
	print, x2list
	print, y1list
	print, y2list

	;计算纠正后文件(0,topy)点在参考影像上的位置分别是
	ref_x0 = constx
	ref_ytop = ky[0] * oriinfo.dimensions[1] + consty $
		- refinfo.dimensions[1]
	print, 'ref_x0 = ', ref_x0, ', ref_ytop = ', ref_ytop
	;计算该点的大地坐标	ge
	geo_x = ref_geotiff.MODELTIEPOINTTAG[3] + 30. * ref_x0
	geo_y = ref_geotiff.MODELTIEPOINTTAG[4] + 30. * ref_ytop
	;调用几何纠正的函数
	;临时文件
	tempfile = info.outfilename + '.tmp'
	openw, templun, tempfile, /get_lun
	;记录读写位置
	fileposlist = lonarr(7)
	;逐个通道进行纠正
	for i=0, 6 do begin
		data = read_tiff(info.orifilename, channels=i)
		newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, newdata
	endfor
	;print, fileposlist
	;关闭临时文件
	free_lun, templun
	;重新打开临时文件
	openr, templun, tempfile, /get_lun
	;写输出文件
	openw, lun, info.outfilename, /get_lun
	;写tiff文件头信息
	writeu, lun, bytarr(8)
	;分块合成七个通道，并写入结果文件
	rows_size = 500
	data1 = bytarr(oriinfo.dimensions[0], rows_size)
	data3 = bytarr(7, oriinfo.dimensions[0], rows_size)
	for i=0, oriinfo.dimensions[1] - rows_size - 1, rows_size do begin
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endfor
	;写余下的部分
	;print, 'i : ', i
	theheight = oriinfo.dimensions[1] - i
	;当超过一行剩余时，写剩余的行
	if theheight gt 0 then begin
		data1 = bytarr(oriinfo.dimensions[0], theheight)
		data3 = bytarr(7, oriinfo.dimensions[0], theheight)
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endif
	;合并纠正后的文件，并写入geotiff信息
	geotiff = {	resolution : double([30., 30., 0.]), $
				prjlocation : double([0., 0., 0., geo_x, geo_y, 0.])}
	makegeototifffile, lun, oriinfo, geotiff = geotiff
	close, templun
	free_lun, templun
	close, lun
	free_lun, lun
	;清除临时文件
	file_delete, tempfile

	;如果有roi文件，则使用roi文件进行切割
	if file_test(info.roifile) eq 1 then begin
		err = do_subset(info.outfilename, info.roifile, info.outfilename)
		if err eq 1 then begin
			print, '裁切失败：输入文件与ROI文件没有重合的区域！'
		endif
	endif

	;把纠正后的结果信息写入数据库之中，用纠正前影像名称做匹配选项，用file_info的目的是防止出现'\\'的情况
	sqlstr = 'update tm_file_info set corrected_file=''' + strlowcase((file_info(info.outfilename)).name) + ''' where original_file=''' $
		+ strlowcase((file_info(info.orifilename)).name) + ''''
	print, sqlstr
	dbexecutesql, sqlstr

	;返回
	return, 1
end

;TM几何纠正进行几何纠正的功能，通过读取结构体参数文件来执行
pro TMCorrectionRunFromFile, filename
	if file_test(filename) eq 0 then return

	;从界面参数获取结构体变量
	info = read_ini(filename)
	if size(info, /type) ne 8 then return

	;构造用于保存到文件的结构体
	info1 = { orifilename : info.orifilename, $
		reffilename : info.reffilename, $
		outfilename : info.outfilename, $
		;把逗号分隔的字符串分离成浮点数组
		x1list : strsplit(info.x1list, ',', /extract), $
		x2list : strsplit(info.x2list, ',', /extract), $
		y1list : strsplit(info.y1list, ',', /extract), $
		y2list : strsplit(info.y2list, ',', /extract) $
	}
	;以参数结构体为变量，调用运行几何纠正的功能
	re = TMCorrectionRunByInfo(info1)

	;显示完成的信息
	msg = dialog_message('完成TM几何纠正操作！', title='TM几何纠正', /info)
end

;TM几何纠正进行几何纠正的功能，响应面板上面的按钮
pro TMCorrectionRun, event
	;从界面参数获取结构体变量
	info = TMCorrectionGetRunInfo(event)
	if size(info, /type) ne 8 then return

	;以参数结构体为变量，调用运行几何纠正的功能
	re = TMCorrectionRunByInfo(info)

	;显示完成的信息
	msg = dialog_message('完成TM几何纠正操作！', title='TM几何纠正', /info)
end

;TM几何纠正处理窗口默认事件
pro TMCorrection_event, event
	;help, event, /struc
	;widget_control, event.id, set_value = '添加中'
end

;TM几何纠正处理窗口
pro TMCorrection, event, datafile, reffile, roifile=roifile
	tlb = widget_base(group_leader=event.top, /modal, title = 'TM几何纠正', /col, xoffset=20, yoffset=5, space=0, tlb_frame_attr=1)
;	tlb = widget_base( title = 'TM几何纠正', /col, xoffset=20, yoffset=5, space=0, tlb_frame_attr=1)
	;
	base1 = widget_base(tlb, /row, xpad=0)
	base2 = widget_base(base1, /col, xpad=0, ypad=0)
		oridraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=480, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			ori_label = widget_label(base3, value='原始影像 ', /align_left)
			o_xid = cw_field(base3, title='X坐标 : ', value='0', xsize=12, /noedit)
			o_yid = cw_field(base3, title='Y坐标 : ', value='0', xsize=12, /noedit)
			base4 = widget_base(base3, /row)
			ori_toolBar = widget_base(base3, /exclusive, /row)
				btn1 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\arrow.bmp', /bitmap, xsize=20, ysize=20, tooltip='选择')
				btn2 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='漫游')
				btn3 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomin.bmp', /bitmap, xsize=20, ysize=20, tooltip='放大')
				btn4 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomout.bmp', /bitmap, xsize=20, ysize=20, tooltip='缩小')
				btn5 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomfit.bmp', /bitmap, xsize=20, ysize=20, tooltip='查看全图')
			widget_control, ori_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ori_label, isAdd:0}
	base2 = widget_base(base1, /col, xpad=0, ypad=0)
		refdraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=480, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			ref_label = widget_label(base3, value='参考影像 ', /align_left)
			r_xid = cw_field(base3, title='X坐标 : ', value='0', xsize=12, /noedit)
			r_yid = cw_field(base3, title='Y坐标 : ', value='0', xsize=12, /noedit)
			base4 = widget_base(base3, /row)
			ref_toolBar = widget_base(base3, /exclusive, /row)
				btn1 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\arrow.bmp', /bitmap, xsize=20, ysize=20, tooltip='选择')
				btn2 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='漫游')
				btn3 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomin.bmp', /bitmap, xsize=20, ysize=20, tooltip='放大')
				btn4 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomout.bmp', /bitmap, xsize=20, ysize=20, tooltip='缩小')
				btn5 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomfit.bmp', /bitmap, xsize=20, ysize=20, tooltip='查看全图')
			widget_control, ref_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ref_label, isAdd:0}
	;
	base1 = widget_base(tlb, /row)
		plist = widget_table(base1, xsize=5, ysize=1, column_label=['序号','原始影像X坐标','原始影像Y坐标','参考影像X坐标','参考影像Y坐标'], $
			column_widths=[46,104,104,104,104], scr_xsize=480, scr_ysize=165, event_pro='TMCorrectionTableEvent', /all_events, /no_row_headers, $
			uvalue={count:0, active_row:-1})
		base2 = widget_base(base1, /col, /frame, xpad=10, ypad=0)
			base3 = widget_base(base2, /row, space=10)
				lab = widget_label(base3, value='  X误差：')
				x_error = widget_text(base3, xsize=20)
				lab = widget_label(base3, value='  Y误差：')
				y_error = widget_text(base3, xsize=20)
			base3 = widget_base(base2, /row, space=10, /nonexcl, /align_right)
				btnautoadd = widget_button(base3, value='自动匹配控制点')
			base3 = widget_base(base2, /row, space=10)
				controlfile = widget_label(base3, value='  控制点：', uvalue = '')
				btn = widget_button(base3, value='添加', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='add')
				btn = widget_button(base3, value='删除', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='delete')
				btn = widget_button(base3, value='保存', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='savelist')
				btn = widget_button(base3, value='加载', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='loadlist')
			base3 = widget_base(base2, /row, space=10)
				lab = widget_label(base3, value='几何纠正：')
				btn = widget_button(base3, value='保存运行信息', xsize=90, ysize=23, event_pro='TMCorrectionSaveRunInfo')
				btn = widget_button(base3, value='开始纠正', xsize=80, ysize=23, event_pro='TMCorrectionRun')
			base3 = widget_base(base2, /row, space=10)
				lab = widget_label(base3, value=' ', xsize=210)
				;btn = widget_button(base3, value='设置颜色', xsize=80, ysize=23, event_pro='TMCorrectionControlPoint', uname='setcolor')
				btn = widget_button(base3, value='帮助', xsize=60, ysize=23)
				btn = widget_button(base3, value='关闭', xsize=60, ysize=23, event_pro='TMCorrectionOnClose')
	;设置原始影像的自定义变量
	widget_control, ori_label, set_uvalue = {drawid:oridraw, toolBar:ori_toolBar, xid:o_xid, yid:o_yid, pindex:1}
	;设置原始影像的自定义变量
	widget_control, ref_label, set_uvalue = {drawid:refdraw, toolBar:ref_toolBar, xid:r_xid, yid:r_yid, pindex:3}

	;检查ROI文件是否存在
	if keyword_set(roifile) eq 0 then roifile = ''

	;设置全局的自定义变量
	widget_control, tlb, /realize, set_uvalue={ori_id:ori_label, ref_id:ref_label, $
		plist:plist, btnautoadd:btnautoadd, controlfile : controlfile, roifile:roifile}
	;加载原始文件
	TMCorrection_open, tlb, datafile, 'orifile'
	;加载参考文件
	TMCorrection_open, tlb, reffile, 'reffile'
	xmanager, 'TMCorrection', tlb
end

;实现TM几何纠正中控制点的使用方法，输入为原始影像和控制点文件
pro tm_cmd
	;控制点文件
	filename = 'D:\yld\ETProject\ETViewer\cp2.dat'
	;原始影像
	oriFile = 'E:\TM_C\Data1\tm5_12332_20040417.tif'
	;参考影像
	refFile = 'D:\yld\ETProject\testdata\tm\12332_20030913atc.tif'
	;读取文件行数
	openr, lun, filename, /get_lun
	table_rows = file_lines(filename)
	if table_rows eq 0 then begin
		msg = dialog_message('文件为空，加载失败！', title='TM几何纠正', /info)
		return
	endif
	;加载原始影像的投影信息
	print, '原始影像 : ', query_tiff(oriFile, info1, geotiff= origeo)
	;加载参考影像的投影信息
	print, '参考影像 : ', query_tiff(refFile, info2, geotiff= refgeo)
	;输出文件
	outfile = 'E:\TM_C\tm5_12332_20031211_corrected.tif'
	;读入数据
	filestr = strarr(table_rows)
	readf, lun, filestr
	free_lun, lun
	;拆分数据
	coordinate = strarr(5, table_rows)
	for i=0,table_rows-1 do begin
		spos = strsplit(filestr[i], ',')
		;如果不是5个分隔的逗号，则该行无效
		if n_elements(spos) ne 5 then continue
		coordinate[0, i] = strmid(filestr[i], spos[0], spos[1]- spos[0]-1)
		coordinate[1, i] = strmid(filestr[i], spos[1], spos[2]- spos[1]-1)
		coordinate[2, i] = strmid(filestr[i], spos[2], spos[3]- spos[2]-1)
		coordinate[3, i] = strmid(filestr[i], spos[3], spos[4]- spos[3]-1)
		coordinate[4, i] = strmid(filestr[i], spos[4])
	endfor
	;x坐标对应
	;x1为原始影像上的坐标，x2为参考影像上的坐标
	x1list = rotate(coordinate[1, *], 3)
	x2list = rotate(coordinate[3, *], 3)
	;计算X坐标的斜率、截距和相关系数
	kx = regress(x1list, x2list, const=constx, sigma=sigmax)
	print, 'x parameter : ', kx[0], constx, sigmax[0]
	;左
	leftx1 = 0
	leftx2 = constx
	;右
	rightx2 = info2.dimensions[0]
	rightx1 = (rightx2 - constx) / kx[0]
	print, 'x1', leftx1, rightx1
	print, 'x2', leftx2, rightx2
	;y坐标对应
	;y1为原始影像上的坐标，y2为参考影像上的坐标
	y1list = rotate(coordinate[2, *], 3)
	y2list = rotate(coordinate[4, *], 3)
	;计算X坐标的斜率、截距和相关系数
	ky = regress(y1list, y2list, const=consty, sigma=sigmay)
	print, 'y parameter : ', ky[0], consty, sigmay[0]
	;上
	topy2 = info2.dimensions[1]
	topy1 = (topy2 - consty) / ky[0]
	;下
	bottomy1 = 0
	bottomy2 = consty
	print, 'y1', bottomy1, topy1
	print, 'y2', bottomy2, topy2
	;
	;print, (x1list[*] * kx[0] + constx) - x2list
	;
	;print, (y1list[*] * ky[0] + consty) - y2list
	strX = strtrim(info2.dimensions[0]-1, 2)
	strY = strtrim(info2.dimensions[1]-1, 2)
	print, '参考影像四角点坐标（x, y）：' + '(0,0), (0, ' + strY + '), (' + strX + ',' + strY + '), (' + strX + ',0)'
	;
	strX0 = strtrim(constx, 2)
	strY0 = strtrim(consty, 2)
	strXM = strtrim(kx[0] * (info2.dimensions[0]-1) + constx, 2)
	strYM = strtrim(ky[0] * (info2.dimensions[1]-1) + consty, 2)
	print, '原始影像大小：',info1.dimensions
	print, '角点在原始影像的位置（x, y）：' + '(' + strX0 + ',' + strY0 + '), (' + strX0 + ', ' + strYM + '), (' + strXM + ',' + strYM + '), (' + strXM + ',' + strY0 + ')'
	;把四角控制点加入到控制点序列中去
	x1list = [x1list, leftx1, leftx1, rightx1, rightx1]
	y1list = [y1list, topy1, bottomy1, topy1, bottomy1]
	;坐标平移
	x2list = [x2list, leftx2, leftx2, rightx2, rightx2] - leftx2
	y2list = [y2list, topy2, bottomy2, topy2, bottomy2] - bottomy2

	;读参考影像的goetiff信息
	print, '参考影像：', query_tiff(refFile, geotiff=ref_geotiff)
	;计算纠正后文件(0,topy)点在参考影像上的位置分别是
	ref_x0 = constx
	ref_ytop = ky[0] * info1.dimensions[1] + consty - info2.dimensions[1]
	print, 'ref_x0 = ', ref_x0, ', ref_ytop = ', ref_ytop
	;计算该点的大地坐标	ge
	geo_x = refgeo.MODELTIEPOINTTAG[3] + 30. * ref_x0
	geo_y = refgeo.MODELTIEPOINTTAG[4] + 30. * ref_ytop
	;调用几何纠正的函数
	;临时文件
	tempfile = outfile + '.tmp'
	openw, templun, tempfile, /get_lun
	;记录读写位置
	fileposlist = lonarr(7)
	;逐个通道进行纠正
	for i=0, 6 do begin
		data = read_tiff(oriFile, channels=i)
		newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, newdata
	endfor
	;关闭临时文件
	free_lun, templun
	;重新打开临时文件
	openr, templun, tempfile, /get_lun
	;写输出文件
	openw, lun, outfile[0], /get_lun
	;写tiff文件头信息
	writeu, lun, bytarr(8)
	;分块合成七个通道，并写入结果文件
	rows_size = 500
	data1 = bytarr(info1.dimensions[0], rows_size)
	data3 = bytarr(7, info1.dimensions[0], rows_size)
	for i=0, info1.dimensions[1] - rows_size - 1, rows_size do begin
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endfor
	;写余下的部分
	;print, 'i : ', i
	theheight = info1.dimensions[1] - i
	;当超过一行剩余时，写剩余的行
	if theheight gt 0 then begin
		data1 = bytarr(info1.dimensions[0], theheight)
		data3 = bytarr(7, info1.dimensions[0], theheight)
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endif
	;合并纠正后的文件，并写入geotiff信息
	geotiff = {	resolution : double([30., 30., 0.]), $
				prjlocation : double([0., 0., 0., geo_x, geo_y, 0.])}
	makegeototifffile, lun, info1, geotiff = geotiff
	free_lun, templun
	free_lun, lun
	;清除临时文件
	file_delete, tempfile
	print, '完成TM几何纠正'
end

;实现TM几何纠正中控制点的使用方法，输入为原始影像和控制点文件
pro tm2_cmd
	;控制点文件
	filename = 'E:\TM_C\Data1\0417.dat'
	;原始影像
	oriFile = 'E:\TM_C\Data1\tm5_12332_20040417.tif'
	;参考影像
	refFile = 'D:\yld\ETProject\testdata\tm\12332_20030913atc.tif'
	;读取文件行数
	openr, lun, filename, /get_lun
	table_rows = file_lines(filename)
	if table_rows eq 0 then begin
		msg = dialog_message('文件为空，加载失败！', title='TM几何纠正', /info)
		return
	endif
	;加载原始影像的投影信息
	print, '原始影像 : ', query_tiff(oriFile, info1, geotiff= origeo)
	;加载参考影像的投影信息
	print, '参考影像 : ', query_tiff(refFile, info2, geotiff= refgeo)
	;输出文件
	outfile = 'E:\TM_C\Data1\tm5_12332_20040417_geoed.tif'
	;读入数据
	filestr = strarr(table_rows)
	readf, lun, filestr
	free_lun, lun
	;拆分数据
	coordinate = strarr(5, table_rows)
	for i=0,table_rows-1 do begin
		spos = strsplit(filestr[i], ',')
		;如果不是5个分隔的逗号，则该行无效
		if n_elements(spos) ne 5 then continue
		coordinate[0, i] = strmid(filestr[i], spos[0], spos[1]- spos[0]-1)
		coordinate[1, i] = strmid(filestr[i], spos[1], spos[2]- spos[1]-1)
		coordinate[2, i] = strmid(filestr[i], spos[2], spos[3]- spos[2]-1)
		coordinate[3, i] = strmid(filestr[i], spos[3], spos[4]- spos[3]-1)
		coordinate[4, i] = strmid(filestr[i], spos[4])
	endfor
	;x坐标对应
	;x1为原始影像上的坐标，x2为参考影像上的坐标
	x1list = double(rotate(coordinate[1, *], 3))
	x2list = double(rotate(coordinate[3, *], 3))
	;计算X坐标的斜率、截距和相关系数
	kx = regress(x1list, x2list, const=constx, sigma=sigmax)
	print, 'x parameter : ', kx[0], constx, sigmax[0]
	;左
	leftx1 = 0
	leftx2 = constx
	;右
	rightx2 = info2.dimensions[0]
	rightx1 = (rightx2 - constx) / kx[0]
	print, 'x1', leftx1, rightx1
	print, 'x2', leftx2, rightx2
	;y坐标对应
	;y1为原始影像上的坐标，y2为参考影像上的坐标
	y1list = double(rotate(coordinate[2, *], 3))
	y2list = double(rotate(coordinate[4, *], 3))
	;计算X坐标的斜率、截距和相关系数
	ky = regress(y1list, y2list, const=consty, sigma=sigmay)
	print, 'y parameter : ', ky[0], consty, sigmay[0]
	;上
	topy2 = info2.dimensions[1]
	topy1 = (topy2 - consty) / ky[0]
	;下
	bottomy1 = 0
	bottomy2 = consty
	print, 'y1', bottomy1, topy1
	print, 'y2', bottomy2, topy2
	;
	;print, (x1list[*] * kx[0] + constx) - x2list
	;
	;print, (y1list[*] * ky[0] + consty) - y2list
	strX = strtrim(info2.dimensions[0]-1, 2)
	strY = strtrim(info2.dimensions[1]-1, 2)
	print, '参考影像四角点坐标（x, y）：' + '(0,0), (0, ' + strY + '), (' + strX + ',' + strY + '), (' + strX + ',0)'
	;
	strX0 = strtrim(constx, 2)
	strY0 = strtrim(consty, 2)
	strXM = strtrim(kx[0] * (info2.dimensions[0]-1) + constx, 2)
	strYM = strtrim(ky[0] * (info2.dimensions[1]-1) + consty, 2)
	print, '原始影像大小：',info1.dimensions
	print, '角点在原始影像的位置（x, y）：' + '(' + strX0 + ',' + strY0 + '), (' + strX0 + ', ' + strYM + '), (' + strXM + ',' + strYM + '), (' + strXM + ',' + strY0 + ')'
	;把四角控制点加入到控制点序列中去
	x1list = [x1list, leftx1, leftx1, rightx1, rightx1]
	y1list = [y1list, topy1, bottomy1, topy1, bottomy1]
	;坐标平移
	x2list = [x2list, leftx2, leftx2, rightx2, rightx2] - leftx2
	y2list = [y2list, topy2, bottomy2, topy2, bottomy2] - bottomy2

	;读参考影像的goetiff信息
	print, '参考影像：', query_tiff(refFile, geotiff=ref_geotiff)
	;计算纠正后文件(0,topy)点在参考影像上的位置分别是
	ref_x0 = constx
	ref_ytop = ky[0] * info1.dimensions[1] + consty - info2.dimensions[1]
	print, 'ref_x0 = ', ref_x0, ', ref_ytop = ', ref_ytop
	;计算该点的大地坐标	ge
	geo_x = refgeo.MODELTIEPOINTTAG[3] + 30. * ref_x0
	geo_y = refgeo.MODELTIEPOINTTAG[4] + 30. * ref_ytop
	return
	;调用几何纠正的函数
	;临时文件
	tempfile = outfile + '.tmp'
	openw, templun, tempfile, /get_lun
	;记录读写位置
	fileposlist = lonarr(7)
	;逐个通道进行纠正
	for i=0, 6 do begin
		data = read_tiff(oriFile, channels=i)
		;newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, data
	endfor
	;关闭临时文件
	free_lun, templun
	;重新打开临时文件
	openr, templun, tempfile, /get_lun
	;写输出文件
	openw, lun, outfile[0], /get_lun
	;写tiff文件头信息
	writeu, lun, bytarr(8)
	;分块合成七个通道，并写入结果文件
	rows_size = 500
	data1 = bytarr(info1.dimensions[0], rows_size)
	data3 = bytarr(7, info1.dimensions[0], rows_size)
	for i=0, info1.dimensions[1] - rows_size - 1, rows_size do begin
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endfor
	;写余下的部分
	;print, 'i : ', i
	theheight = info1.dimensions[1] - i
	;当超过一行剩余时，写剩余的行
	if theheight gt 0 then begin
		data1 = bytarr(info1.dimensions[0], theheight)
		data3 = bytarr(7, info1.dimensions[0], theheight)
		for j=0, 6 do begin
			point_lun, templun, fileposlist[j]
			readu, templun, data1
			data3[j, *, *] = data1
			point_lun, -templun, filepos
			fileposlist[j] = filepos
		endfor
		writeu, lun, data3
	endif
	;合并纠正后的文件，并写入geotiff信息
	geotiff = {	resolution : double([30., 30., 0.]), $
				prjlocation : double([0., 0., 0., geo_x, geo_y, 0.])}
	makegeototifffile, lun, info1, geotiff = geotiff
	free_lun, templun
	free_lun, lun
	;清除临时文件
	file_delete, tempfile
	print, '完成TM几何纠正'
end