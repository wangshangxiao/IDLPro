function getdefaultdirectorys

	return,'D:\ģ�ͳ���\���ξ���\'
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
	;�����ַ���������
	if tiff_typ eq 2 then begin
		ifd[offset+4] = byte(strlen(value), 0, 2)	;count
		point_lun, -lun, pos   ;Get file posit
		ifd[offset+8] = byte(pos, 0, 4)  ;Set IFD ^ pointer
		writeu, lun, byte(value)
	endif else begin
		;�����ַ���������
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

;��tiff��Ϣ��geotiff��Ϣд���ļ���ȥ,�����lun��������һ��8�ֽڵ�ͷ������������ļ�
;info�а������ݵ�ά����Ϣdimensions(���(cols)�͸߶�(rows),������ͨ����(Ĭ��Ϊ7))
;geotiff������������
;resolution��һ����Ϊ�������飬��ʾ�ֱ���
;prjlocation��һ����Ϊ�������飬��ʾλ��
pro makegeototifffile, lun, info, geotiff = geotiff
	;�ļ�������Ϣ
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
	;������ʼλ�ã��̶�Ϊ8
	TM_tiff_add_tag, lun,273, 8L, ifd=ifd, count=count
	;
	TM_tiff_add_tag, lun, 274, 1, ifd=ifd, count=count  ;Orientation
	TM_tiff_add_tag, lun, 277, 7, ifd=ifd, count=count 	;Samples / pixel
	TM_tiff_add_tag, lun, 278, rows, ifd=ifd, count=count 		;Rows / strip
	;�ļ����ݴ�С
	tsize = samples * rows * cols
	TM_tiff_add_tag, lun, 279,  tsize, ifd=ifd, count=count   ;Strip byte counts

	if n_elements(xresol) le 0 then xresol = 100.
	if n_elements(yresol) le 0 then yresol = 100.
	TM_tiff_add_tag, lun, 282, float(xresol), ifd=ifd, count=count 		;Xresolution
	TM_tiff_add_tag, lun, 283, float(yresol), ifd=ifd, count=count 		;... and Yresolution
	TM_tiff_add_tag, lun, 284, PlanarConfig, ifd=ifd, count=count  ;PlanarConfig
	;�ж��Ƿ�Ҫ����geotiff��Ϣ
	if keyword_set(geotiff) eq 1 then begin
		TM_tiff_add_tag, lun, '830E'X, double(geotiff.resolution), ifd=ifd, count=count
		TM_tiff_add_tag, lun, '8482'X, double(geotiff.prjlocation), ifd=ifd, count=count
		;����Ϊ������geotiff��Ϣ
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
	;��¼��ǰ��ַ׼��дIFD
	point_lun, -lun, faddr		;Write IFD at and, get addr
	;print, 'IFD��λ�ã�', faddr
	ifd[0] = byte(count, 0, 2)	;Insert count
	writeu, lun, ifd[0: count*12 + 5] ;Write IFD followed by 4 zero bytes
	;��д�ļ�ͷ��Ϣ
	point_lun, lun, 0		;Rewind to header
	;tiff�ļ�ͷ����(8���ֽ�)
	header = bytarr(8)
	;���С�˲���
	tst = byte(1,0,2)		;Which endian???
	if tst[0] eq 1 then header[0] = byte("II") $	;Little endian
   	else header[0] = byte("MM")	;Big endian
   	;tiff�ļ���־
	header[2] = byte(42, 0, 2)
	header[4] = byte(faddr,0,4)	;Write ifd offset
	writeu, lun, header		;And save it
end

;����������Ϣ���ṹ�������
function TMDataTranslationGetInfo, event
	;��ȡ�����б�
	widget_control, event.top, get_uvalue = idlist

	;����ж������Ƿ�����
	;��������
	widget_control, idlist.satinfo, get_uvalue = stafileid
	widget_control, stafileid, get_value = stafile
	if file_test(stafile[0]) eq 0 then begin
		msg = dialog_message('�޷������������ļ��������ļ��Ƿ���ڡ�', title='TM����ת��', /info)
		return, 0
	endif
	;�ж����·���Ƿ����
	widget_control, idlist.outpath, get_value = outpath
	if file_test(outpath[0], /directory) eq 0 then begin
		msg = dialog_message('����Ŀ¼�����ڣ���ָ�����Ŀ¼��', title='TM����ת��', /info)
		return, 0
	endif
	;�ļ�ͶӰ��Ϣ
	widget_control, idlist.prjinfo, get_uvalue = prjfileid
	widget_control, prjfileid, get_value = prjfile
	if file_test(prjfile[0]) eq 0 then begin
		msg = dialog_message('�޷���ͶӰ�����ļ��������ļ��Ƿ���ڡ�', title='TM����ת��', /info)
		return, 0
	endif
	;�����ȡ�����ļ�,�����д����ļ�
	bandfilelist = strarr(7)
	for i=0,6 do begin
		;��ԭʼ�����ļ�
		widget_control, idlist.bandlist[i], get_uvalue = bandfileid
		widget_control, bandfileid, get_value = bandfile
		bandfilelist[i] = bandfile
	endfor
	;�������в������ļ�
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

;����������Ϣ��ָ�����ı��ļ���
pro TMDataTranslationSaveExeInfo, event
	filename = dialog_pickfile(title='ѡ������ļ�', default_extension='ini', filter='*.ini', /write, /overwrite_prompt)
	if filename[0] eq '' then return
	info = TMDataTranslationGetInfo(event)
	if size(info, /type) ne 8 then return
	write_struct, filename[0], info, /ini
end

;����ִ��ת���Ĺ��ܣ�����Ϊ�ṹ�����
function TMDataTranslationExecuteByInfo, info
	;�����������ж�ȡ��Ϣ
	linescount = file_lines(info.stafile)
	filestr = strarr(linescount)
	openr, lun, info.stafile, /get_lun
	readf, lun, filestr
	free_lun, lun
	;
	satstr = ['������Ϣ']
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
	;����ռ�ֱ���
	oWidth = ceil(iWidth * 25. / 30.)
	oHeight = ceil(iHeight * 25. / 30.)	;
	;����ļ���
	outfile = info.outpath + '\tm5_' + pathname + rowname + '_' + strmid(ptime, 0, 8) + '.tif'
	temp_outfile = outfile + '.tmp'

	;���ٵ������ε����ݿռ�
	datai = bytarr(iWidth, iHeight)
	data = bytarr(oWidth, oHeight)
	;print, 'outsize : ', oWidth, oHeight
	;����ʱ�ļ�
	openw, templun, temp_outfile, /get_lun
	;��¼����λ��
	fileposlist = lonarr(7)
	;��ȡ�����ļ����б�
	bandfile = [info.bandfile1,info.bandfile2, info.bandfile3, info.bandfile4, $
		info.bandfile5, info.bandfile6, info.bandfile7]
	;�����ȡ�����ļ�,�����д����ļ�
	for i=0,6 do begin
		;��ԭʼ�����ļ�
		openr, lun, bandfile[i], /get_lun
		readu, lun, datai
		free_lun, lun
		;�����ֱ���
		data = congrid(datai, oWidth, oHeight, /interp)
		;д��ʱ�ļ�������¼λ��
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, data
	endfor
	;�ر���ʱ�ļ�
	free_lun, templun
	;���´���ʱ�ļ�
	openr, templun, temp_outfile, /get_lun
	;�򿪽���ļ�
	openw, lun, outfile, /get_lun
	print, '����ļ���' + outfile
	;дtiff�ļ�ͷ��Ϣ
	header = bytarr(8)
	writeu, lun, header
	;�ֿ�ϳ��߸�ͨ������д�����ļ�
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
	;д���µĲ���
	;print, 'i : ', i
	theheight = oHeight - i
	;������һ��ʣ��ʱ��дʣ�����
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
	;����tiff��Ϣ
	info = { dimensions : [oWidth, oHeight] }
	geotiff = { resolution : [30., 30., 0], prjlocation : [0.,0.,0.,0.,0.,0.]}
	makegeototifffile, lun, info, geotiff = geotiff
	;�ر��ļ�
	free_lun, templun
	free_lun, lun
	;�����ʱ�ļ�
	file_delete, temp_outfile
	;�������ݱ�:tm_file_info, װ���ļ���Ϣ
	;��ɾȥԭ�е���Ϣ
	sqlstr = 'delete from tm_file_info where zoneid=''' + pathname + rowname + ''' and sensorid=''tm5''' $
		+ ' and resolution=30 and datatime=to_date(''' + strmid(ptime, 0, 8) + ''', ''YYYYMMDD'') and passtime=''11:45'''
	;print, sqlstr
	dbexecutesql, sqlstr
	;�ټ����µ���Ϣ���ļ���Сд
	sqlstr = 'insert into tm_file_info(zoneid, sensorid, resolution, datatime, passtime, original_file) values(''' + pathname + rowname + ''', ''tm5''' $
		+ ', 30, to_date(''' + strmid(ptime, 0, 8) + ''', ''YYYYMMDD''), ''11:45'', ''' $
		+ strlowcase((file_info(outfile)).name) + ''')'
	;print, sqlstr
	dbexecutesql, sqlstr
	;��������Ϣ
	satstr = [satstr, 'path and row:' + pathname + ',' + rowname, '����ʱ�䣺' + ptime, $
		' ', '����ռ�ֱ��ʣ�25��', '����߶ȣ�' + strtrim(iHeight, 2) + '�������ȣ�' + strtrim(iWidth, 2), $
		' ', '����ռ�ֱ��ʣ�30��', '����߶ȣ�' + strtrim(oHeight, 2) + '�������ȣ�' + strtrim(oWidth, 2), $
		' ', '����ļ�����' + outfile]
	;������ʾ��Ϣ�ͽ��·��
	return, { satstr:satstr, outfile:outfile }
end

;TM����ת����ת�����ܣ���Ӧ�������а�ť
pro TMDataTranslationExecute, event
	On_Error, 2
	CATCH, Error_status
	IF Error_status NE 0 THEN BEGIN
		msg = dialog_message('��������' + !ERROR_STATE.MSG, title='TM����ת��', /error)
		CATCH, /CANCEL
		return
	endif
	;�ж���һ��ִ�л�������ִ��
	uname = widget_info(event.id, /uname)
	case uname of
		'once' : begin
			;��ȡ�����б�
			info = TMDataTranslationGetInfo(event)
			;�������������˳�
			if size(info, /type) ne 8 then return
			;����ת���ĺ���
			re = TMDataTranslationExecuteByInfo(info)
			;������ʾ��Ϣ
			widget_control, event.top, get_uvalue = idlist
			widget_control, idlist.showinfo, set_value = re.satstr
			;����TMԤ����������������ļ�·�����Ѵ����Ľ��д��·����ȥ
			tmbaseid = widget_info(idlist.topbase, find_by_uname = 'tm_pre')
			;���û���ҵ��ؼ����˳�
			if tmbaseid ne 0 then begin
				widget_control, tmbaseid, get_uvalue = topuv
				widget_control, topuv.datafile, set_value = re.outfile
				;���ú��������ý����ֵ
				call_procedure, 'tm_openfile', tmbaseid
			endif
		end
		'batch' : begin
			batchfile = dialog_pickfile(title='��ѡ������������ļ�', filter='*.ini', /read, /multiple)
			if batchfile[0] eq '' then return
			infostring = '������Ϣ���£�'
			for i=0, n_elements(batchfile)-1 do begin
				On_Error, 2
				CATCH, Error_status
				IF Error_status NE 0 THEN BEGIN
					infostring = [infostring, '�����ļ�' + batchfile[i] + '����ʧ�ܣ�������Ϣ���£�' + !ERROR_STATE.MSG]
					CATCH, /CANCEL
					continue
				endif
				;��ȡ�����ļ�
				info = read_ini(batchfile[i])
				;����ת���ĺ���
				re = TMDataTranslationExecuteByInfo(info)
				infostring = [infostring, '�����ļ�' + batchfile[i] + '���гɹ�']
			endfor
			print, errorstring
		end
		else : return
	endcase
	;��ʾ����
	msg = dialog_message('���TM����ת������!', title='TM����ת��', /info)
end

;TM����ת��Դ�ļ�Ŀ¼ѡ���򿪹���
pro TMDataTranslationAddFiles, idlist
	widget_control, idlist.basepath, get_value = pathname
	if file_test(pathname[0], /directory) eq 0 then begin
		msg = dialog_message('��ָ��TM5Դ�����ļ�Ŀ¼��', title='TM5����ת��', /info)
		return
	endif
	;������Ŀ¼������ļ�
	;������Ϣ
	satinfo = file_search(pathname[0], 'scene01_ProductDescription.self', /fold_case)
	widget_control, idlist.satinfo, get_uvalue = stapathid
	widget_control, stapathid, set_value = satinfo[0]
	;ͶӰ��Ϣ
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

;TM����ת��Դ�ļ�Ŀ¼ѡ�����¼�����
pro TMDataTranslationAddFilesButton, event
	widget_control, event.top, get_uvalue = idlist
	;
	TMDataTranslationAddFiles, idlist
end

;TM����ת��Դ�ļ�Ŀ¼ѡ�����¼�����
pro TMDataTranslationSelectTm5FolderButton, event
	pathname = dialog_pickfile(title='ѡ��Դ�����ļ�Ŀ¼', /directory)
	if pathname[0] eq '' then return
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.basepath, set_value = pathname[0]
	;
	TMDataTranslationAddFiles, idlist
end

;TM����ת�����Ŀ¼ѡ�����¼�����
pro TMDataTranslationSelectOutFolderButton, event
	pathname = dialog_pickfile(title='ѡ������ļ�Ŀ¼', /directory)
	if pathname[0] eq '' then return
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.outpath, set_value = pathname[0]
end

;TM����ת���ļ�ѡ����ļ����¼�����
pro TMDataTranslationSelectFileButton, event
	;Ĭ������´�Դ�ļ�Ŀ¼��ѡ��
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.basepath, get_value = basepath

	filename = dialog_pickfile(title='ѡ���ļ�', /read, path=basepath[0])
	if filename[0] eq '' then return
	widget_control, event.id, get_uvalue = fileid
	widget_control, fileid, set_value = filename[0]
end

;TM����ת���ļ�ѡ���
function TMDataTranslationSelectFileWidget, base, label
	base0 = widget_base(base, /row)
	lab = widget_label(base0, value=label)
	path = widget_text(base0, xsize=60, /editable)
	btn = widget_button(base0, /bitmap, value='image/viewer/open.bmp', $
		uvalue=path, event_pro='TMDataTranslationSelectFileButton')
	widget_control, base0, set_uvalue = path
	return, base0
end

;TM����ת��Ĭ���¼��������
pro TMDataTranslation_event, event
end

;TM����ת������
pro TMDataTranslation, event
	tlb = widget_base(group_leader=event.top, /modal, title = 'TM����ת��', /col, xoffset=200, yoffset=15, space=0, tlb_frame_attr=1)
	;
	tab1 = widget_tab(tlb)
	base_tm5 = widget_base(tab1, title='TM5���������趨', /col, space=0)
		base2 = widget_base(base_tm5, /row)
			lab = widget_label(base2, value='Դ�����ļ���Ŀ¼��')
			tm5path = widget_text(base2, xsize=60, /editable)
			btn = widget_button(base2, /bitmap, value='image/viewer/open.bmp', $
				event_pro='TMDataTranslationSelectTM5FolderButton')
			btn = widget_button(base2, /bitmap, value='image/viewer/refresh.bmp', $
				event_pro='TMDataTranslationAddFilesButton')
		base2 = widget_base(base_tm5, /col, /frame, xpad=13)
			satinfo = TMDataTranslationSelectFileWidget(base2, ' ������Ϣ�ļ���')
			prjinfo = TMDataTranslationSelectFileWidget(base2, ' ͶӰ��Ϣ�ļ���')
			band1 = TMDataTranslationSelectFileWidget(base2, '����1�����ļ���')
			band2 = TMDataTranslationSelectFileWidget(base2, '����2�����ļ���')
			band3 = TMDataTranslationSelectFileWidget(base2, '����3�����ļ���')
			band4 = TMDataTranslationSelectFileWidget(base2, '����4�����ļ���')
			band5 = TMDataTranslationSelectFileWidget(base2, '����5�����ļ���')
			band6 = TMDataTranslationSelectFileWidget(base2, '����6�����ļ���')
			band7 = TMDataTranslationSelectFileWidget(base2, '����7�����ļ���')
	;
	tab2 = widget_tab(tlb)
	base0 = widget_base(tab2, title='���GEOTIFF�ļ�Ŀ¼�趨', /col)
		base1 = widget_base(base0, /row)
			;Ĭ�Ϸ�����TMԤ�����Ŀ¼֮��
			dirs = getdefaultdirectorys()
			lab = widget_label(base1, value='   ����ļ�·����')
			outpath = widget_text(base1, xsize=60, /editable, value=dirs.tmpre_dir)
			btn = widget_button(base1, /bitmap, value='image/viewer/open.bmp', $
				event_pro='TMDataTranslationSelectOutFolderButton')
		base1 = widget_base(base0, /col)
			lab = widget_label(base1, value='������Ϣ��', /align_left)
			showinfo = widget_text(base1, xsize=84, ysize=12, /scroll, /wrap)
		base1 = widget_base(base0, /row, /align_right, space=10)
			btn = widget_button(base1, value='ִ��ת��', xsize=80, ysize=23, event_pro='TMDataTranslationExecute', uname='once')
			btn = widget_button(base1, value='����������Ϣ', xsize=90, ysize=23, event_pro='TMDataTranslationSaveExeInfo')
			btn = widget_button(base1, value='����ִ��ת��', xsize=90, ysize=23, event_pro='TMDataTranslationExecute', uname='batch')
			btn = widget_button(base1, value='����', xsize=60, ysize=23, event_pro='call_help')
			btn = widget_button(base1, value='�ر�', xsize=60, ysize=23, event_pro='close_event')
	;����ȫ�ֵ��Զ������
	widget_control, tlb, /realize, set_uvalue = {topbase:event.top, basepath:tm5path, satinfo:satinfo, prjinfo:prjinfo, $
			bandlist:[band1, band2, band3, band4, band5, band6, band7], outpath:outpath, showinfo:showinfo}
	xmanager, 'TMDataTranslation', tlb
end

;������TM���ξ������ݵĴ���

;�Ѹ������ָ�ʽ�ľ���ĵ�һ��ת��Ϊ�����������ڱ������ʾ���Ƶ�
function TMCorrectionGetFormatedTable, tablevalue
	formated_coordinate = strtrim(tablevalue[*, *], 2)
	formated_coordinate[0, *] = strtrim(long(tablevalue[0, *]), 2)
	return, formated_coordinate
end

;����Ƶ�������ӿ��Ƶ�Ĺ���
pro TMCorrectionAddControlPoint, idlist, workid, x, y
	widget_control, idlist.plist, get_uvalue = puv
	widget_control, workid, get_uvalue = widlist
	if puv.count eq 0 or puv.active_row eq -1 then return
	;���������ֵ��������ӵ�����ȥ
	puv.coordinate[widlist.pindex, puv.active_row] = strtrim(x, 2)
	puv.coordinate[widlist.pindex+1, puv.active_row] = strtrim(y, 2)
	;ָʾ�Ƿ��Զ���ӵ�
	isAutoadd = 0
	;�ж��Ƿ�Ҫ�Զ�������һ����
	autoadd = widget_info(idlist.btnautoadd, /button_set)
	;�������������ϲ����Զ���ӵ�
	if autoadd eq 1 and puv.count gt 3 then begin
		;x�����Ӧ
		;x1ΪԭʼӰ���ϵ����꣬x2Ϊ�ο�Ӱ���ϵ�����
		x1list = rotate(puv.coordinate[1, 0:puv.count-2], 3)
		x2list = rotate(puv.coordinate[3, 0:puv.count-2], 3)
		;����X�����б�ʡ��ؾ�����ϵ��
		kx = regress(x1list, x2list, const=constx, sigma=sigmax)
		print, kx[0], constx, sigmax[0]
		;y�����Ӧ
		;y1ΪԭʼӰ���ϵ����꣬y2Ϊ�ο�Ӱ���ϵ�����
		y1list = rotate(puv.coordinate[2, 0:puv.count-2], 3)
		y2list = rotate(puv.coordinate[4, 0:puv.count-2], 3)
		;Y�����б�ʡ��ؾ�����ϵ��
		ky = regress(y1list, y2list, const=consty, sigma=sigmay)
		print, ky[0], consty, sigmay[0]
		;�ûع鷽��������һ���������
		if widlist.pindex eq 1 then begin
			;��ԭʼӰ�����ο�Ӱ���ϵĵ�
			newx = kx[0] * x + constx
			newy = ky[0] * y + consty
			newindex = 3
			casid = idlist.ref_id
		endif else begin
			;�ɲο�Ӱ�����ԭʼӰ���ϵĵ�
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
	;ˢ�±�����ʾ�����������ֵ
	tablestring = TMCorrectionGetFormatedTable(puv.coordinate)
	widget_control, idlist.plist, set_value = tablestring, set_uvalue = puv
	;�ж��Ƿ�Ҫ��ʾ�Զ���ӵĵ�
	if isAutoadd eq 1 then begin
		;��ʾ�Զ���ӵĵ�
		widget_control, casid, get_uvalue = casidlist
		TMCorrectionDrawControlPoints, idlist, casidlist.drawid
		;��ӽ�����ȡ����һ��Ӱ������״̬
		widget_control, casidlist.toolBar, get_uvalue = castooluvalue
		castooluvalue.isAdd = 0
		widget_control, casidlist.toolBar, set_uvalue = castooluvalue
	endif
end

;��Ӱ��������ƿ��Ƶ�Ĺ���
pro TMCorrectionDrawControlPoints, idlist, drawid
	;��ȡ��ʾ����
	widget_control, drawid, get_uvalue = drawuvalue, get_value = owindow
	;�����һ�ε���ʾģ��
	obj_destroy, drawuvalue.objPointModel
	;���ɿ��Ƶ�ͼ��
	drawuvalue.objPointModel = Obj_New('IDLgrModel')
	;��ȡ���Ƶ������б�
	widget_control, idlist.plist, get_uvalue = puv
	;��ȡ��ǰ�Ĺ�������
	widget_control, drawuvalue.workid, get_uvalue = widlist
	;�趨���Ƶ�ı߿�Ĵ�С
	len1 = 7 * drawuvalue.zoomfactor
	len2 = 1.2 * drawuvalue.zoomfactor
	;�������
	for i=0, puv.count-1 do begin
		;�趨λ��
		x = puv.coordinate[widlist.pindex, i]
		y = puv.coordinate[widlist.pindex+1, i]
		;�趨��ɫ
		color = i eq puv.active_row ? [255,255,0] : [0,0,255]
		;���������
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x-len1, x-len2], [y, y], color=color)
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x+len2, x+len1], [y, y], color=color)
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x, x], [y-len1, y-len2], color=color)
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolyLine', [x, x], [y+len1, y+len2], color=color)
		;��ӱ߿�
		drawuvalue.objPointModel->Add, Obj_New('IDLgrPolygon', [x-len1, x-len1, x+len1, x+len1, x-len1], $
			[y-len1, y+len1, y+len1, y-len1, y-len1], color=color, STYLE=1, thick=2)
		drawuvalue.objPointModel->Add, OBJ_NEW('IDLgrText', 'GCP #' + strtrim(long(puv.coordinate[0, i]), 2), $
			Locations=[x-4*len1, y-3.6*len1], COLOR=color)
	endfor
	;��ǰView�м�����Ƶ�ģ��
	drawuvalue.objView->Add, drawuvalue.objPointModel
	;���»�ͼ���ı���
	widget_control, drawid, set_uvalue = drawuvalue
	;���ƴ����Ƶ��ͼ��
	owindow->Draw, drawuvalue.objView
end

;����դ��ͼ����
;���������ֱ���ȫ��id�б�͵�ǰ��ͼ����id�б�
pro TMCorrectionDrawImage, idlist, workid
	;��ȡ��ǰ���Զ������
	widget_control, workid, get_uvalue = widlist
	;��ȡ��ͼ�����Զ������
	widget_control, widlist.drawid, get_uvalue = drawuvalue, get_value = objwindow
	;�������һ�εĻ�ͼ����
	obj_destroy, drawuvalue.objModel
	obj_destroy, drawuvalue.objView
	obj_destroy, drawuvalue.objImage
	;����ͼ�����
	drawuvalue.objImage = OBJ_NEW('IDLgrImage', order=1, TILING=1, TILE_LEVEL_MODE=0, $
		TILED_IMAGE_DIMENSIONS=[drawuvalue.info.dimensions[0], drawuvalue.info.dimensions[1]])
	;�����ű�������ʼλ�ü������ʾ�����С
	width = drawuvalue.zoomFactor * 480.
	height = drawuvalue.zoomFactor * 480.
	;���û�ͼ��Χ
	drawuvalue.objView = OBJ_NEW('IDLgrView', VIEWPLANE_RECT = drawuvalue.image_rect)
	;�ֿ��ͼ
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
	;Ӱ��ģ��
	drawuvalue.objModel = Obj_New('IDLgrModel')
    drawuvalue.objModel->Add, drawuvalue.objImage
    drawuvalue.objView->Add, drawuvalue.objModel
    ;����դ��Ӱ��ͼ
    objwindow->Draw, drawuvalue.objView
    ;savView = drawuvalue.objView
    ;save, savView, filename='e:\obj.txt'
    ;���»�ͼ���ı���
    widget_control, widlist.drawid, set_uvalue = drawuvalue
    ;��Ӱ��������ӿ��Ƶ�
    widget_control, idlist.plist, get_uvalue = puv
    if puv.count gt 0 then TMCorrectionDrawControlPoints, idlist, widlist.drawid
end

;Ӱ��ؼ��ϵ�����¼�����
pro TMCorrectionDrawEvent, event
	;��ȡ����
	widget_control, event.top, get_uvalue = idlist
	;Ӱ��ؼ�ID
	drawid = event.id
	;��ȡͼ������ж��Ƿ����
	widget_control, drawid, get_uvalue = drawuvalue, get_value=owindow
	if size(drawuvalue, /type) ne 8 then return
	;��ǰӰ��Ĺ���ID
	workid = drawuvalue.workid
	widget_control, workid, get_uvalue = widlist
	;������꾭���ĵ������
	x = drawuvalue.image_rect[0] + event.x * drawuvalue.zoomFactor
	y = drawuvalue.image_rect[1] + event.y * drawuvalue.zoomFactor
	;print,x,y
	widget_control, widlist.xid ,set_value = strtrim(x, 2)
	widget_control, widlist.yid ,set_value = strtrim(y, 2)
	;�жϻ�ͼ����ť��״̬
	widget_control, widlist.toolBar, get_uvalue = tooluvalue
	;���ݻ�ͼ���İ�ť��״̬��ȷ��Ҫ��ɵĶ���
	if tooluvalue.status eq 0 then begin
		;��괦�ڹ��״̬������Ų�����Ƶ�
		;��ȡ���Ƶ��б���Ϣ
		widget_control, idlist.plist, get_uvalue = puv
		;�ж��Ƿ���Ų�����Ƶ�
		if event.type eq 2 then begin
			;�ж��Ƿ����϶���ѡ��
			if drawuvalue.mouseStatus eq 0 then return
			;�����϶�״̬���޸Ļ�������
			;��ֹ����Ƴ���ǰ����
			boxsize = 480
			mousex = event.x > 0
			mousex <= boxsize
			mousey = event.y > 0
			mousey <= boxsize
			;���ñ�ǵ������
			puv.coordinate[widlist.pindex, puv.active_row] = x
			puv.coordinate[widlist.pindex+1, puv.active_row] = y
			;�����ǵ������
			widget_control, idlist.plist, set_uvalue = puv
			;ˢ�¶Կ��Ƶ����ʾ
			TMCorrectionDrawControlPoints, idlist, drawid
		endif
		;�������������
		if event.type eq 0 and event.press eq 1 then begin
			;�ڿ��Ƶ�״̬�£���������������Ҫ�������ֿ��ܵ������
			;һ������µĿ��Ƶ�
			if tooluvalue.isAdd eq 1 then begin
				;������ӿ��Ƶ�Ĺ���
				TMCorrectionAddControlPoint, idlist, workid, x ,y
				;��Ӱ�񴰿�������ƿ��Ƶ�
				TMCorrectionDrawControlPoints, idlist, widlist.drawid
				;��ӽ�����ȡ�����״̬
				tooluvalue.isAdd = 0
				widget_control, widlist.toolBar, set_uvalue = tooluvalue
			endif
			;����µ��Ժ��������Ų������ӵĵ㣨��Ϊ���ǻ�㣩
			;�����ƶ����еĿ��Ƶ�
			if size(puv, /type) ne 8 then return
			if puv.count eq 0 or puv.active_row eq -1 then return
			;��ǰ�Ļ��ǵ����ڵ�λ�ã���ͼ������ת��Ϊ�豸���꣩
			px = puv.coordinate[widlist.pindex, puv.active_row]
			py = puv.coordinate[widlist.pindex+1, puv.active_row]
			;�жε�����Ƿ�λ����ѡ�����������
			;�����ڷ�Χ��������Ϊ�϶���ʼ
			x_dis = abs(x - px) / drawuvalue.zoomFactor
			y_dis = abs(y - py) / drawuvalue.zoomFactor
			if x_dis le 10 and y_dis le 10 then begin
				;����Ϊ��ʼŲ����״̬
				drawuvalue.mouseStatus = 1
				widget_control, drawid, set_uvalue = drawuvalue
			endif
		endif
		;Ų�����Ƶ�������������ͷ��¼���
		if event.type eq 1 and event.release eq 1 then begin
			;�ж��Ƿ����϶���ѡ��
			if drawuvalue.mouseStatus eq 0 then return
			;����Ϊ���϶�״̬
			drawuvalue.mouseStatus = 0
			widget_control, drawid, set_uvalue = drawuvalue
			;�ͷź󣬸�����Ӧ�����ݵ������
			;��ȡ�ͷŵ������
			puv.coordinate[widlist.pindex, puv.active_row] = x
			puv.coordinate[widlist.pindex+1, puv.active_row] = y
			;���±�����ݲ�ˢ����ʾ
			tablestring = TMCorrectionGetFormatedTable(puv.coordinate)
			widget_control, idlist.plist, set_uvalue = puv, set_value = tablestring
		endif
	endif else if tooluvalue.status eq 1 then begin
		;���β����������ƶ�ͼ��
		;�ж��Ƿ����϶�
		if event.type eq 2 then begin
			;�ж��Ƿ����϶���ѡ��
			if drawuvalue.mouseStatus eq 0 then return
			;��������ƶ�����
			dx = (x - drawuvalue.startpos[0])
			dy = (y - drawuvalue.startpos[1])
			;������Ӱ�������
			new_rect = drawuvalue.image_rect[*]
			new_rect[0] -= dx
			new_rect[1] -= dy
			;����Ӱ��
			drawuvalue.objView->SetProperty, viewplane_rect = new_rect
    		owindow->Draw, drawuvalue.objView
		endif
		;�����ο�ʼ���������������
		if event.type eq 0 and event.press eq 1 then begin
			;�������Ϊ�϶�����ʼ״̬
			drawuvalue.mouseStatus = 1
			drawuvalue.startpos = [x, y]
			widget_control, drawid, set_uvalue = drawuvalue
		endif
		;�����ν������������ͷ��¼�
		if event.type eq 1 and event.release eq 1 then begin
			;�ж��Ƿ����϶�״̬
			if drawuvalue.mouseStatus eq 0 then return
			;����Ϊ���϶�״̬
			drawuvalue.mouseStatus = 0
			;��������ƶ�����
			dx = (x - drawuvalue.startpos[0])
			dy = (y - drawuvalue.startpos[1])
			;������Ӱ������򣬲��ұ�֤������
			drawuvalue.image_rect[0] -= dx
			drawuvalue.image_rect[0] >= 0
			drawuvalue.image_rect[0] <= drawuvalue.info.dimensions[0] - 480 * drawuvalue.zoomFactor
			;
			drawuvalue.image_rect[1] -= dy
			drawuvalue.image_rect[1] >= 0
			drawuvalue.image_rect[1] <= drawuvalue.info.dimensions[1] - 480 * drawuvalue.zoomFactor
			;���浱ǰ��λ��ֵ
			widget_control, drawid, set_uvalue = drawuvalue
			;���ν����������»���ͼ��
			TMCorrectionDrawImage, idlist, workid
		endif
	endif else if tooluvalue.status eq 2 or tooluvalue.status eq 3 then begin
		;��괦�ڷŴ����С״̬����������ͼ��
		;�ж��Ƿ����϶�
		if event.type eq 2 then begin
			;�ж��Ƿ����϶���ѡ��
			if drawuvalue.mouseStatus eq 0 then return
			;�����ʼ��
			x1 = drawuvalue.startpos[0]
			y1 = drawuvalue.startpos[1]
			;��������,��Ҫ�жϺϷ���,��ֹ����Ƴ���ǰ����
			x2 = x > 0
			x2 = x2 < drawuvalue.info.dimensions[0]
			;
			y2 = y > 0
			y2 = y2 < drawuvalue.info.dimensions[1]
			;�������ٵ��߿�
			oModel = Obj_New('IDLgrModel')
			pll = Obj_New('IDLgrPolyLine', [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], color=[0,0,255])
			oModel->Add, pll
			;
    		drawuvalue.objView->Add, oModel
    		owindow->Draw, drawuvalue.objView
    		obj_destroy, oModel
		endif
		;�������������
		if event.type eq 0 and event.press eq 1 then begin
			;�������Ϊ�϶�����ʼ״̬
			drawuvalue.mouseStatus = 1
			drawuvalue.startpos = [x, y]
			widget_control, drawid, set_uvalue = drawuvalue
		endif
		;�϶��������������ͷ��¼���
		if event.type eq 1 and event.release eq 1 then begin
			;�ж��Ƿ����϶���ѡ��
			if drawuvalue.mouseStatus eq 0 then return
			;����Ϊ���϶�״̬
			drawuvalue.mouseStatus = 0
			;�ͷź󣬼�������϶�����
			start_x = drawuvalue.startpos[0]
			start_y = drawuvalue.startpos[1]
 			end_x = x > 0
			end_x = end_x < drawuvalue.info.dimensions[0]
			end_y = y > 0
			end_y = end_y < drawuvalue.info.dimensions[1]
			;�ƶ��������
			maxdis = max([abs(end_x - start_x), abs(end_y - start_y)])
			;ȷ���ǷŴ�����С����
			if tooluvalue.status eq 2 then begin
				;�Ŵ����
				;��������ͬһλ�õ������Ŵ�һ��
				if maxdis le 1 then maxdis = drawuvalue.image_rect[2] / 2
				;������С����ʾ���򲻵���24�����أ���20���Ŵ�(480 / 20 = 24)
				maxdis = maxdis > 24
			endif else begin
				;��С����
				;��������ͬһλ�õ��������Сһ��
				if maxdis le 1 then maxdis = drawuvalue.image_rect[2] / 2
				maxdis = (drawuvalue.image_rect[2]^2) / maxdis
				;����������ʾ����Ϊͼ������ߴ�
				maxdis = maxdis < drawuvalue.maxsize
			endelse
			;����������ĵ�
			center_x = (start_x + end_x) / 2.
			center_x = center_x < (drawuvalue.info.dimensions[0] - maxdis / 2.)
			center_x = center_x > (maxdis / 2.)
			center_y = (start_y + end_y) / 2.
			center_y = center_y < (drawuvalue.info.dimensions[1] - maxdis / 2.)
			center_y = center_y > (maxdis / 2.)
			;ȷ��Ӱ����ʾ����
			drawuvalue.image_rect = [center_x - maxdis / 2., center_y - maxdis / 2., maxdis, maxdis]
			;���·Ŵ���
			drawuvalue.zoomfactor = maxdis / 480.
			print, 'zoomfactor', drawuvalue.zoomfactor
			;��������ֵ
			widget_control, drawid, set_uvalue = drawuvalue
			;����Ӱ����ʾ
			TMCorrectionDrawImage, idlist, workid
		endif
	endif
end

;�Ŵ���С�����Ρ�ԭʼ��С�͹����״�¼�
pro TMCorrectionZoomEvent, event
	widget_control, event.top, get_uvalue = idlist
	;��ȡ��ǰ��ť��base�ؼ�
	toolbar = widget_info(event.id, /parent)
	widget_control, toolbar, get_uvalue = tuv
	index = where(tuv.btnlist eq event.id)
	;�ж�״̬
	if index eq 4 then begin
		;�������ŵ�ȫͼ���¼�
		;��ȡ��������
		widget_control, tuv.workid, get_uvalue = widlist
		if size(widlist, /type) ne 8 then return
		;��ȡӰ�����
		widget_control, widlist.drawid, get_uvalue = drawuvalue
		;������ʾ����Ϊȫͼ
		drawuvalue.image_rect = [0, 0, drawuvalue.maxsize, drawuvalue.maxsize]
		;���·Ŵ���
		drawuvalue.zoomfactor = drawuvalue.maxsize / 480.
		;��������ֵ
		widget_control, widlist.drawid, set_uvalue = drawuvalue
		;����Ӱ����ʾ
		TMCorrectionDrawImage, idlist, tuv.workid
		;����ť���óɹ��״̬
		tuv.status = 0
		widget_control, tuv.btnlist[0], /set_button
	endif else begin
		;���°�ť��״̬
		tuv.status = index
	end
	;����״̬
	widget_control, toolbar, set_uvalue = tuv
end

;���Ƶ��б���¼�����
pro TMCorrectionTableEvent, event
	;ֻ�������¼�
	if tag_names(event, /structure_name) ne 'WIDGET_TABLE_CELL_SEL' then return
	;������ȫѡ�¼�
	if event.sel_top eq -1 then return
	widget_control, event.top, get_uvalue = idlist
	;���ѡ��
	widget_control, idlist.plist, set_table_select=[-1,-1,-1,-1]
	;ֻѡ��ѡ�����ĵ�һ��
	widget_control, idlist.plist, set_table_select=[0 , event.sel_top, 4, event.sel_top]
	;����������Ϊ���
	widget_control, idlist.plist, get_uvalue = puv
	if puv.count eq 0 then return
	puv.active_row = event.sel_top
	widget_control, idlist.plist, set_uvalue = puv
	;���»��ƿ��Ƶ�,����ʾ�µĻ���Ƶ�
	;ԭʼӰ��
	widget_control, idlist.ori_id, get_uvalue = oriuvalue
	TMCorrectionDrawControlPoints, idlist, oriuvalue.drawid
	;�ο�Ӱ��
	widget_control, idlist.ref_id, get_uvalue = refuvalue
	TMCorrectionDrawControlPoints, idlist, refuvalue.drawid
end

;�Կ��Ƶ����ӡ�ɾ����ת����������ɫ����
pro TMCorrectionControlPoint, event
	widget_control, event.top, get_uvalue = idlist
	;
	;��ȡ��ť����
	uname = widget_info(event.id, /uname)
	;��ȡ���Ƶ��б�
	widget_control, idlist.plist, get_uvalue = puv
	;��ȡԭʼӰ��Ĵ����
	widget_control, idlist.ori_id, get_uvalue = oriuvalue
	widget_control, oriuvalue.drawid, get_uvalue = oridrawuvalue
	if size(oridrawuvalue, /type) ne 8 then begin
		msg = dialog_message('û��ԭʼӰ���޷���ɲ�����', title='TM���ξ���', /infor)
		return
	endif
	;��ȡ�ο�Ӱ��Ĵ����
	widget_control, idlist.ref_id, get_uvalue = refuvalue
	widget_control, refuvalue.drawid, get_uvalue = refdrawuvalue
	if size(refdrawuvalue, /type) ne 8 then begin
		msg = dialog_message('û�вο�Ӱ���޷���ɲ�����', title='TM���ξ���', /infor)
		return
	endif
	;�ֱ���
	case uname of
		;��ӿ��Ƶ�
		'add' : begin
			;����ԭʼӰ����ӿ��Ƶ�ı�ʶ
			widget_control, oriuvalue.toolBar, get_uvalue = oritooluv
			widget_control,  oritooluv.btnlist[0], /set_button
			oritooluv.status = 0
			oritooluv.isAdd = 1
			widget_control, oriuvalue.toolBar, set_uvalue = oritooluv
			;���òο�Ӱ����ӿ��Ƶ�ı�ʶ
			widget_control, refuvalue.toolBar, get_uvalue = reftooluv
			widget_control,  reftooluv.btnlist[0], /set_button
			reftooluv.status = 0
			reftooluv.isAdd = 1
			widget_control, refuvalue.toolBar, set_uvalue = reftooluv
			;���ÿ��Ƶ��б�
			if puv.count eq 0 then begin
				;����Ϊ�գ���ʼ��������
				puv_new = {count : 1, coordinate : [1, -1., -1., -1., -1.], active_row:0}
			endif else begin
				;������Ϊ�գ���Ҫ�ж���һ������¿��Ƶ�Ķ�����û�����
				if (where(puv.coordinate[*, puv.count-1] eq -1))[0] ne -1 then begin
					;��ʾ���������һ�ε�����¿��Ʋ���
					msg = dialog_message('��������Ϊ ' + strtrim(puv.coordinate[0, puv.count-1], 2) + $
						' �Ŀ��Ƶ����Ӳ�����', title='TM���ξ���', /info)
					return
				endif
				;�������
				new_index = strtrim(max(long(puv.coordinate[0,*])) + 1, 2)
				puv_new = {count : puv.count+1, coordinate : [[puv.coordinate],[new_index, -1, -1, -1, -1]], active_row:puv.count}
			endelse
			tablestring = TMCorrectionGetFormatedTable(puv_new.coordinate)
			;���±�����ʾ������
			widget_control, idlist.plist, table_ysize=puv_new.count, set_value=tablestring, set_uvalue=puv_new
			widget_control, idlist.plist, set_table_select = [0,puv_new.active_row,4,puv_new.active_row]
		end
		;ɾ����ѡ��Ŀ��Ƶ�
		'delete' : begin
			if puv.count eq 0 then begin
				msg = dialog_message('û�п��Ƶ����ݣ��޷�ɾ����', title='TM���ξ���', /information)
				return
			endif
			;��ѡ������ĵ�һ��
			rowindex = (widget_info(idlist.plist, /table_select))[1]
			if rowindex eq -1 then return
			;ɾ��ѡ�е�һ��
			if puv.count eq 1 then begin
				;ֻ��һ��,ɾ������ձ������,����ʾһ����
				puv_new = {count : 0, active_row:-1, coordinate:['','','','','']}
				;������ʾ����
				table_rows = 1
			endif else begin
				;�������ݣ�����ȷ����ѡ����е����
				prowid = puv.coordinate[0, rowindex]
				;��ŵ��б�
				rowidlist = puv.coordinate[0, *]
				;�ӱ���ɾ������ѡ�����
				puv_new = {count : puv.count - 1, $
					coordinate : [[puv.coordinate[*, where(rowidlist ne prowid)]]], $
					active_row:-1}
				;������ʾ����
				table_rows = puv_new.count
			endelse
			;���±������
			coordinate = strtrim(puv_new.coordinate[*, *], 2)
			coordinate[0, *] = strtrim(long(coordinate[0, *]), 2)
			widget_control, idlist.plist, table_ysize=table_rows, set_uvalue = puv_new, $
				set_value = coordinate, set_table_select=[-1,-1,-1,-1]
			;1 ˢ��ԭʼӰ���ϵĿ��Ƶ����ʾ
			widget_control, idlist.ori_id, get_uvalue = ori_widlist
			TMCorrectionDrawControlPoints, idlist, ori_widlist.drawid
    		;2 ˢ�²ο�Ӱ���ϵĿ��Ƶ����ʾ
    		widget_control, idlist.ref_id, get_uvalue = ref_widlist
    		TMCorrectionDrawControlPoints, idlist, ref_widlist.drawid
		end
		;������Ƶ��б��ļ���
		'savelist' : begin
			if puv.count eq 0 then begin
				msg = dialog_message('��ǰ������Ϊ�գ����ܱ��棡', title='TM���ξ���', /info)
				return
			endif
			;�򿪿��Ƶ��ļ�
			widget_control, idlist.controlfile, get_uvalue = filename
			;��ʾ�û�ѡ��Ĭ��Ϊ��һ��ָ�����ļ�
			filename = dialog_pickfile(title='ѡ����Ƶ��б��ļ�', filter='*.dat', default_extension='dat', file=filename)
			if filename[0] eq '' then return
			widget_control, idlist.controlfile, set_uvalue = filename[0]
			;������Ƶ���Ϣ
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
		;���ļ��м��ؿ��Ƶ��б�
		'loadlist' : begin
			if puv.count gt 0 then begin
				msg = dialog_message('��ǰ������Ϊ�գ��Ƿ������', title='TM���ξ���', /question)
				if msg eq 'No' then return
			endif
			;
			filename = dialog_pickfile(title='ѡ����Ƶ��б��ļ�', filter='*.dat', default_extension='dat')
			if filename[0] eq '' then return
			if file_test(filename[0]) ne 1 then begin
				msg = dialog_message('���ؿ��Ƶ��б��ļ�ʧ�ܣ�', title='TM���ξ���', /question)
				return
			endif
			;��ȡ�ļ�����
			openr, lun, filename[0], /get_lun
			table_rows = file_lines(filename[0])
			if table_rows eq 0 then begin
				msg = dialog_message('�ļ�Ϊ�գ�����ʧ�ܣ�', title='TM���ξ���', /info)
				return
			endif
			;��������
			filestr = strarr(table_rows)
			readf, lun, filestr
			free_lun, lun
			;�������
			coordinate = strarr(5, table_rows)
			for i=0,table_rows-1 do begin
				spos = strsplit(filestr[i], ',')
				;�������5���ָ��Ķ��ţ��������Ч
				if n_elements(spos) ne 5 then continue
				coordinate[0, i] = strtrim(long(strmid(filestr[i], spos[0], spos[1]- spos[0]-1)), 2)
				coordinate[1, i] = strmid(filestr[i], spos[1], spos[2]- spos[1]-1)
				coordinate[2, i] = strmid(filestr[i], spos[2], spos[3]- spos[2]-1)
				coordinate[3, i] = strmid(filestr[i], spos[3], spos[4]- spos[3]-1)
				coordinate[4, i] = strmid(filestr[i], spos[4])
			endfor
			;���±������
			puv_new = {count : table_rows, coordinate : float(coordinate), active_row:0}
			widget_control, idlist.plist, table_ysize=table_rows, set_uvalue = puv_new, $
				set_value = coordinate, set_table_select=[-1,-1,-1,-1]
			;ˢ����ʾ
			;1 ˢ��ԭʼӰ���ϵĿ��Ƶ����ʾ
			widget_control, idlist.ori_id, get_uvalue = ori_widlist
			TMCorrectionDrawControlPoints, idlist, ori_widlist.drawid
    		;2 ˢ�²ο�Ӱ���ϵĿ��Ƶ����ʾ
    		widget_control, idlist.ref_id, get_uvalue = ref_widlist
    		TMCorrectionDrawControlPoints, idlist, ref_widlist.drawid
		end
	endcase
end

;���ļ�����
pro TMCorrection_open, idname, filename, filetype
	widget_control, idname, get_uvalue = idlist
	if filetype eq 'orifile' then begin
		;��ԭʼӰ��
		qr = query_tiff(filename, info)
		;�ж��Ƿ��Ƕ�ͨ����(�����ṩ֧�ֵ�ͨ��TIFF�Ĺ���)
		if info.channels ne 7 then begin
			msg = dialog_message('ԭʼӰ���ǺϷ���7ͨ��TIFF�ļ�', title='TM���ξ���')
			return
		endif
		;����Ӱ�񳤿�����ֵ
		maxsize = max([info.dimensions[0:1]])
		;��������
		workid = idlist.ori_id
		;��ȡ�ο�Ӱ��
		data432 = read_tiff(filename, channels=[1,2,3])
	endif

	;�򿪲ο�Ӱ��
	if filetype eq 'reffile' then begin
		qr = query_tiff(filename, info, geotiff=geotiff)
		;�ж��Ƿ��Ƕ�ͨ����(�����ṩ֧�ֵ�ͨ��TIFF�Ĺ���)
		if info.channels ne 7 then begin
			msg = dialog_message('�ο�Ӱ���ǺϷ���7ͨ��TIFF�ļ�', title='TM���ξ���')
			return
		endif
		if size(geotiff, /type) ne 8 then begin
			msg = dialog_message('�ο�Ӱ���ͶӰ��Ч��', title='TM���ξ���')
			return
		endif
		;����Ӱ�񳤿�����ֵ
		maxsize = max([info.dimensions[0:1]])
		;��������
		workid = idlist.ref_id
		;��ȡ�ο�Ӱ��
		data432 = read_tiff(filename, channels=[1,2,3])
	endif

	;�ֱ����4/3/2ͨ�����������
	minmax4 = getminmaxvalue(data432[2, *, *])
	minmax3 = getminmaxvalue(data432[1, *, *])
	minmax2 = getminmaxvalue(data432[0, *, *])
	minmax = {minmax2:minmax2, minmax3:minmax3, minmax4:minmax4}
	;�����ʼ�����ű�������ʾ����ȷ��Ϊ480���أ�
	zoomFactor = maxsize / 480.
	;����Ĭ�ϵ�����״̬
	widget_control, workid, get_uvalue = widlist
	widget_control, widlist.toolBar, get_uvalue = tuv
	widget_control, tuv.btnlist[0], /set_button
	;���û�ͼ����ı���
	widget_control, widlist.drawid, set_uvalue = {workid:workid, mouseStatus:0, $
		filename : filename[0], info:info, maxsize:maxsize, zoomfactor:zoomFactor, $
		startpos : [0,0], image_rect:[0,0,maxsize, maxsize], minmax:minmax, $
		objImage:obj_new(), objModel:obj_new(), objView:obj_new(), objPointModel:obj_new() }
	;���û�ͼ����
	TMCorrectionDrawImage, idlist, workid
end

;TM���ξ��������ڹر��¼�
pro TMCorrectionOnClose, event
	;ȫ��ID�б�
	widget_control, event.top, get_uvalue = idlist
	;ԭʼͼ��ID�б�
	widget_control, idlist.ori_id, get_uvalue = orilist
	widget_control, orilist.drawid, get_uvalue = ori_drawobj
	if size(ori_drawobj, /type) eq 8 then $
		obj_destroy, [ori_drawobj.objImage, ori_drawobj.objView, $
			ori_drawobj.objModel, ori_drawobj.objPointModel]
	;�ο�ͼ��ID�б�
	widget_control, idlist.ref_id, get_uvalue = reflist
	widget_control, reflist.drawid, get_uvalue = ref_drawobj
	if size(ref_drawobj, /type) eq 8 then $
		obj_destroy, [ref_drawobj.objImage, ref_drawobj.objView, $
			ref_drawobj.objModel, ref_drawobj.objPointModel]
	;�ص�����
	widget_control, event.top, /destroy
end

;TM���ξ����Ĺ���
function WARP_TRI_YLD, xo, yo, xi, yi, im_in, OUTPUT_SIZE = output_size
	s = SIZE(im_in)
	if s[0] ne 2 then print, 'TM���ξ��������ݸ�ʽ����ȷ'
	;���ݴ�С
	if n_elements(output_size) ge 2 then begin
		nx = output_size[0]
		ny = output_size[1]
	endif else begin
		nx = s[1]
		ny = s[2]
	endelse
	;����������
	TRIANGULATE, xo, yo, tr, bounds
	gs = [1,1]				;Grid spacing
	;��ʱ����
	im_out = bytarr(nx, ny)
	;ÿ�������н��о���
	rows_size = 30
	for i=0, ny - 1, rows_size do begin
		;��������Y����
		bottompos = (i + rows_size) gt ny ? (ny - 1) : (i + rows_size)
		;���㵱ǰ���о���������
		b = [0, i, nx-1, bottompos]
		;����λ�ò���
		xpos = TRIGRID(xo, yo, xi, tr, gs, b)
		ypos = TRIGRID(xo, yo, yi, tr, gs, b)
		;���в�ֵ
		im_out[*, i : bottompos] = INTERPOLATE(im_in[*, *], xpos, ypos)
	endfor
	;���ؽ��
	return, im_out
end

;�Ѽ��ξ�������Ϣ���浽�ṹ�����֮��
function TMCorrectionGetRunInfo, event
	;ȫ��ID�б�
	widget_control, event.top, get_uvalue = idlist

	;���Ƶ��б�
	widget_control, idlist.plist, get_uvalue = puv
	if puv.count eq 0 then begin
		msg = dialog_message('û�п��Ƶ��б��޷����м��ξ������ܣ�', title='TM���ξ���', /infor)
		return, 0
	endif
	;���Ƶ�ĸ���������������
	if puv.count lt 3 then begin
		msg = dialog_message('����Ҫѡ��6������Ŀ��Ƶ㣬�����޷����м��ξ������ܣ�', title='TM���ξ���', /infor)
		return, 0
	endif

	;ԭʼӰ���ļ���Ϣ
	widget_control, idlist.ori_id, get_uvalue = ori_widlist
	widget_control, ori_widlist.drawid, get_uvalue = oridrawuvalue
	;�ο�Ӱ���ļ���Ϣ
	widget_control, idlist.ref_id, get_uvalue = ref_widlist
	widget_control, ref_widlist.drawid, get_uvalue = refdrawuvalue

	;�趨����ļ��������tmԤ�����Ŀ¼֮�£�������ԭʼӰ����geo.tif
	dirs = getdefaultdirectorys()
	outfile = dirs.tmpre_dir + '\' + file_basename(oridrawuvalue.filename, '.tif') + '_geo.tif'
	;ROI�ļ�
	rofile = ''
	if size(idlist.roifile, /type) eq 7 then roifile = idlist.roifile
	;����Ҫ����ı���
	tmcorrection_info = {tmcorrection_info, $
		orifilename : oridrawuvalue.filename, $
		reffilename : refdrawuvalue.filename, $
		roifile		: roifile, $	;zone���������ļ�
		outfilename : outfile, $	;����ļ�
		x1list : rotate(puv.coordinate[1, *], 3), $ ;ԭʼ���Ƶ�X����
		x2list : rotate(puv.coordinate[3, *], 3), $	;�ο����Ƶ�X����
		y1list : rotate(puv.coordinate[2, *], 3), $ ;ԭʼ���Ƶ�Y����
		y2list : rotate(puv.coordinate[4, *], 3) $ ;�ο����Ƶ�Y����
	}
	return, tmcorrection_info
end

;�Ѽ��ξ�������Ϣ���浽�ṹ�����֮�У���д�������ļ���
pro TMCorrectionSaveRunInfo, event
	;��ʾ�û�ѡ��Ҫ������ļ�
	filename = dialog_pickfile(title='��ѡ������ļ�', filter='*.ini', /write)
	if filename[0] eq '' then return

	;������Ƶ��ļ�·����ԭʼӰ��·��
	info = TMCorrectionGetRunInfo(event)
	;�������ڱ��浽�ļ��Ľṹ��
	info1 = { orifilename : info.orifilename, $
		reffilename : info.reffilename, $
		outfilename : info.outfilename, $
		roifile		: info.roifile, $
		x1list : '', x2list : '', y1list : '', y2list : '' $
	}

	;����������ת���ɶ��ŷָ�ֵ�ַ���
	listlen = n_elements(info.x1list)
	for i=0, listlen-1 do begin
		info1.x1list += strtrim(info.x1list[i], 2) + ','
		info1.x2list += strtrim(info.x2list[i], 2) + ','
		info1.y1list += strtrim(info.y1list[i], 2) + ','
		info1.y2list += strtrim(info.y2list[i], 2) + ','
	endfor
	;����ṹ�����
	write_struct, filename[0], info1, /ini
end

;�Ѽ��ξ���֮����ļ������и�
function do_subset, inputfile, roifile, outputifle
;˵��: f1Ϊ���и�ͼ��f2Ϊroi(�ṩ�к�ͼ��Ŀռ䷶Χ��geotiff��Ϣ),f3Ϊ����ļ���(����f1��ͬ)
;Ŀǰ֧�������и
;1.�ȷֱ����£�f1ȫ��Χf2����ʱ���Ϊf1�ж�Ӧf2�ռ䷶Χ�ڵĲ���
;2.�ȷֱ����£�f1��f2С����ʱ���Ϊf1λ��f2�ռ䷶Χ�ڵĲ��֣���ֵ������
;3.�ȷֱ����£�f1��f2�н��沿�֣���ʱ���Ϊf1��f2������λ��f2�ռ䷶Χ�ڵĲ��֣���ֵ������
;4.���ȷֱ����£�f1ȫ��Χf2����ʱ���Ϊf1�ж�Ӧf2�ռ䷶Χ�ڵĲ���
;5.���ȷֱ����£�f1��f2С����ʱ���Ϊf1λ��f2�ռ䷶Χ�ڵĲ��֣���ֵ������
;6.���ȷֱ����£�f1��f2�н��沿�֣���ʱ���Ϊf1��f2������λ��f2�ռ䷶Χ�ڵĲ��֣���ֵ������

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
	;�ж����Ҫ���е�Ӱ���ROIӰ��û�н������򷵻�-1
	if insect_box[0] le insect_box[2] or insect_box[1] le insect_box[3] then return, 1
	ul = (insect_box[0:1] - ul1) / scale1 * [1,-1]
	lr = (insect_box[2:3] - ul1) / scale1 * [1,-1]
	sub_rect = [ul, lr-ul]

	ul = (insect_box[0:1] - ul2) / scale2 * [1,-1]
	lr = (insect_box[2:3] - ul2) / scale2 * [1,-1]

	dim3 = fix([lr[0] - ul[0], lr[1] - ul[1]])
	if inf1.channels gt 1 then begin
		;��ͨ��
		;img = make_array(inf1.channels, dim2[0], dim2[1], type = datatype)
		;img[*, ul[0]:ul[0] + dim3[0] - 1, ul[1]:ul[1] + dim3[1] -1] = congrid(read_tiff(f1, sub_rect = sub_rect), inf1.channels, dim3[0], dim3[1], /interp)
		;��ͨ��������������
		tempfile = inputfile + '.tmp'
		;����ʱ�ļ�
		openw, templun, tempfile, /get_lun
		;��¼����λ��
		fileposlist = lonarr(inf1.channels)
		for i=0, inf1.channels-1 do begin
			;��ȡһ��ͨ��������
			newdata = congrid(read_tiff(inputfile, sub_rect = sub_rect, channels=i), dim3[0], dim3[1], /interp)
			;д��ʱ�ļ�������¼�ļ�ָ���λ��
			point_lun, -templun, filepos
			fileposlist[i] = filepos
			writeu, templun, newdata
		endfor
		;
		;�򿪽���ļ�
		openw, lun, outputifle, /get_lun
		;дtiff�ļ�ͷ��Ϣ
		header = bytarr(8)
		writeu, lun, header
		;�ֿ�ϳɸ���ͨ������д�����ļ�
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
		;д���µĲ���
		theheight = dim3[1] - i
		;������һ��ʣ��ʱ��дʣ�����
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
		;����tiffͶӰ��Ϣ
		info = { dimensions : dim3 }
		geotiff = { resolution : [geo1.modelpixelscaletag[0], geo1.modelpixelscaletag[1], 0], $
			prjlocation : [0.,0.,0.,insect_box[0], insect_box[1], 0.]}
		makegeototifffile, lun, info, geotiff = geotiff
		;�ر��ļ�
		free_lun, templun
		free_lun, lun
		;�����ʱ�ļ�
		file_delete, tempfile
	endif else begin
		;��ͨ��
		img = make_array(dim2[0], dim2[1], type = datatype)
		img[ul[0]:ul[0] + dim3[0] - 1, ul[1]:ul[1] + dim3[1] -1] = congrid(read_tiff(inputfile, sub_rect = sub_rect), dim3[0], dim3[1], /interp)
		write_tiff, f3, img, /float,geotiff=geo2
	endelse
	;���гɹ�������0ֵ
	return, 0
end

;TM���ξ������м��ξ����Ĺ��ܣ�ֱ��ͨ�������ṹ������
function TMCorrectionRunByInfo, info
;��ȡ�ļ���Ϣ��ԭʼӰ���goetiff��Ϣ
	r1 = query_tiff(info.orifilename, oriinfo)
	r2 = query_tiff(info.reffilename, refinfo, geotiff=ref_geotiff)

	;����X�����б�ʡ��ؾ�����ϵ��
	kx = regress(info.x1list, info.x2list, const=constx, sigma=sigmax)
	print, kx[0], constx, sigmax[0]
	;��
	leftx1 = 0
	leftx2 = constx
	;��
	rightx2 = oriinfo.dimensions[0]
	rightx1 = (rightx2 - constx) / kx[0]
	print, 'x1', leftx1, rightx1
	print, 'x2', leftx2, rightx2
	;y�����Ӧ
	;y1ΪԭʼӰ���ϵ����꣬y2Ϊ�ο�Ӱ���ϵ�����

	;����X�����б�ʡ��ؾ�����ϵ��
	ky = regress(info.y1list, info.y2list, const=consty, sigma=sigmay)
	print, ky[0], consty, sigmay[0]
	;��
	topy2 = oriinfo.dimensions[1]
	topy1 = (topy2 - consty) / ky[0]
	;��
	bottomy1 = 0
	bottomy2 = consty
	print, 'y1', bottomy1, topy1
	print, 'y2', bottomy2, topy2
	;���Ľǿ��Ƶ���뵽���Ƶ�������ȥ
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

	;����������ļ�(0,topy)���ڲο�Ӱ���ϵ�λ�÷ֱ���
	ref_x0 = constx
	ref_ytop = ky[0] * oriinfo.dimensions[1] + consty $
		- refinfo.dimensions[1]
	print, 'ref_x0 = ', ref_x0, ', ref_ytop = ', ref_ytop
	;����õ�Ĵ������	ge
	geo_x = ref_geotiff.MODELTIEPOINTTAG[3] + 30. * ref_x0
	geo_y = ref_geotiff.MODELTIEPOINTTAG[4] + 30. * ref_ytop
	;���ü��ξ����ĺ���
	;��ʱ�ļ�
	tempfile = info.outfilename + '.tmp'
	openw, templun, tempfile, /get_lun
	;��¼��дλ��
	fileposlist = lonarr(7)
	;���ͨ�����о���
	for i=0, 6 do begin
		data = read_tiff(info.orifilename, channels=i)
		newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, newdata
	endfor
	;print, fileposlist
	;�ر���ʱ�ļ�
	free_lun, templun
	;���´���ʱ�ļ�
	openr, templun, tempfile, /get_lun
	;д����ļ�
	openw, lun, info.outfilename, /get_lun
	;дtiff�ļ�ͷ��Ϣ
	writeu, lun, bytarr(8)
	;�ֿ�ϳ��߸�ͨ������д�����ļ�
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
	;д���µĲ���
	;print, 'i : ', i
	theheight = oriinfo.dimensions[1] - i
	;������һ��ʣ��ʱ��дʣ�����
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
	;�ϲ���������ļ�����д��geotiff��Ϣ
	geotiff = {	resolution : double([30., 30., 0.]), $
				prjlocation : double([0., 0., 0., geo_x, geo_y, 0.])}
	makegeototifffile, lun, oriinfo, geotiff = geotiff
	close, templun
	free_lun, templun
	close, lun
	free_lun, lun
	;�����ʱ�ļ�
	file_delete, tempfile

	;�����roi�ļ�����ʹ��roi�ļ������и�
	if file_test(info.roifile) eq 1 then begin
		err = do_subset(info.outfilename, info.roifile, info.outfilename)
		if err eq 1 then begin
			print, '����ʧ�ܣ������ļ���ROI�ļ�û���غϵ�����'
		endif
	endif

	;�Ѿ�����Ľ����Ϣд�����ݿ�֮�У��þ���ǰӰ��������ƥ��ѡ���file_info��Ŀ���Ƿ�ֹ����'\\'�����
	sqlstr = 'update tm_file_info set corrected_file=''' + strlowcase((file_info(info.outfilename)).name) + ''' where original_file=''' $
		+ strlowcase((file_info(info.orifilename)).name) + ''''
	print, sqlstr
	dbexecutesql, sqlstr

	;����
	return, 1
end

;TM���ξ������м��ξ����Ĺ��ܣ�ͨ����ȡ�ṹ������ļ���ִ��
pro TMCorrectionRunFromFile, filename
	if file_test(filename) eq 0 then return

	;�ӽ��������ȡ�ṹ�����
	info = read_ini(filename)
	if size(info, /type) ne 8 then return

	;�������ڱ��浽�ļ��Ľṹ��
	info1 = { orifilename : info.orifilename, $
		reffilename : info.reffilename, $
		outfilename : info.outfilename, $
		;�Ѷ��ŷָ����ַ�������ɸ�������
		x1list : strsplit(info.x1list, ',', /extract), $
		x2list : strsplit(info.x2list, ',', /extract), $
		y1list : strsplit(info.y1list, ',', /extract), $
		y2list : strsplit(info.y2list, ',', /extract) $
	}
	;�Բ����ṹ��Ϊ�������������м��ξ����Ĺ���
	re = TMCorrectionRunByInfo(info1)

	;��ʾ��ɵ���Ϣ
	msg = dialog_message('���TM���ξ���������', title='TM���ξ���', /info)
end

;TM���ξ������м��ξ����Ĺ��ܣ���Ӧ�������İ�ť
pro TMCorrectionRun, event
	;�ӽ��������ȡ�ṹ�����
	info = TMCorrectionGetRunInfo(event)
	if size(info, /type) ne 8 then return

	;�Բ����ṹ��Ϊ�������������м��ξ����Ĺ���
	re = TMCorrectionRunByInfo(info)

	;��ʾ��ɵ���Ϣ
	msg = dialog_message('���TM���ξ���������', title='TM���ξ���', /info)
end

;TM���ξ���������Ĭ���¼�
pro TMCorrection_event, event
	;help, event, /struc
	;widget_control, event.id, set_value = '�����'
end

;TM���ξ���������
pro TMCorrection, event, datafile, reffile, roifile=roifile
	tlb = widget_base(group_leader=event.top, /modal, title = 'TM���ξ���', /col, xoffset=20, yoffset=5, space=0, tlb_frame_attr=1)
;	tlb = widget_base( title = 'TM���ξ���', /col, xoffset=20, yoffset=5, space=0, tlb_frame_attr=1)
	;
	base1 = widget_base(tlb, /row, xpad=0)
	base2 = widget_base(base1, /col, xpad=0, ypad=0)
		oridraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=480, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			ori_label = widget_label(base3, value='ԭʼӰ�� ', /align_left)
			o_xid = cw_field(base3, title='X���� : ', value='0', xsize=12, /noedit)
			o_yid = cw_field(base3, title='Y���� : ', value='0', xsize=12, /noedit)
			base4 = widget_base(base3, /row)
			ori_toolBar = widget_base(base3, /exclusive, /row)
				btn1 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\arrow.bmp', /bitmap, xsize=20, ysize=20, tooltip='ѡ��')
				btn2 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='����')
				btn3 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomin.bmp', /bitmap, xsize=20, ysize=20, tooltip='�Ŵ�')
				btn4 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomout.bmp', /bitmap, xsize=20, ysize=20, tooltip='��С')
				btn5 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomfit.bmp', /bitmap, xsize=20, ysize=20, tooltip='�鿴ȫͼ')
			widget_control, ori_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ori_label, isAdd:0}
	base2 = widget_base(base1, /col, xpad=0, ypad=0)
		refdraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=480, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			ref_label = widget_label(base3, value='�ο�Ӱ�� ', /align_left)
			r_xid = cw_field(base3, title='X���� : ', value='0', xsize=12, /noedit)
			r_yid = cw_field(base3, title='Y���� : ', value='0', xsize=12, /noedit)
			base4 = widget_base(base3, /row)
			ref_toolBar = widget_base(base3, /exclusive, /row)
				btn1 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\arrow.bmp', /bitmap, xsize=20, ysize=20, tooltip='ѡ��')
				btn2 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='����')
				btn3 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomin.bmp', /bitmap, xsize=20, ysize=20, tooltip='�Ŵ�')
				btn4 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomout.bmp', /bitmap, xsize=20, ysize=20, tooltip='��С')
				btn5 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='image\viewer\zoomfit.bmp', /bitmap, xsize=20, ysize=20, tooltip='�鿴ȫͼ')
			widget_control, ref_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ref_label, isAdd:0}
	;
	base1 = widget_base(tlb, /row)
		plist = widget_table(base1, xsize=5, ysize=1, column_label=['���','ԭʼӰ��X����','ԭʼӰ��Y����','�ο�Ӱ��X����','�ο�Ӱ��Y����'], $
			column_widths=[46,104,104,104,104], scr_xsize=480, scr_ysize=165, event_pro='TMCorrectionTableEvent', /all_events, /no_row_headers, $
			uvalue={count:0, active_row:-1})
		base2 = widget_base(base1, /col, /frame, xpad=10, ypad=0)
			base3 = widget_base(base2, /row, space=10)
				lab = widget_label(base3, value='  X��')
				x_error = widget_text(base3, xsize=20)
				lab = widget_label(base3, value='  Y��')
				y_error = widget_text(base3, xsize=20)
			base3 = widget_base(base2, /row, space=10, /nonexcl, /align_right)
				btnautoadd = widget_button(base3, value='�Զ�ƥ����Ƶ�')
			base3 = widget_base(base2, /row, space=10)
				controlfile = widget_label(base3, value='  ���Ƶ㣺', uvalue = '')
				btn = widget_button(base3, value='���', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='add')
				btn = widget_button(base3, value='ɾ��', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='delete')
				btn = widget_button(base3, value='����', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='savelist')
				btn = widget_button(base3, value='����', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='loadlist')
			base3 = widget_base(base2, /row, space=10)
				lab = widget_label(base3, value='���ξ�����')
				btn = widget_button(base3, value='����������Ϣ', xsize=90, ysize=23, event_pro='TMCorrectionSaveRunInfo')
				btn = widget_button(base3, value='��ʼ����', xsize=80, ysize=23, event_pro='TMCorrectionRun')
			base3 = widget_base(base2, /row, space=10)
				lab = widget_label(base3, value=' ', xsize=210)
				;btn = widget_button(base3, value='������ɫ', xsize=80, ysize=23, event_pro='TMCorrectionControlPoint', uname='setcolor')
				btn = widget_button(base3, value='����', xsize=60, ysize=23)
				btn = widget_button(base3, value='�ر�', xsize=60, ysize=23, event_pro='TMCorrectionOnClose')
	;����ԭʼӰ����Զ������
	widget_control, ori_label, set_uvalue = {drawid:oridraw, toolBar:ori_toolBar, xid:o_xid, yid:o_yid, pindex:1}
	;����ԭʼӰ����Զ������
	widget_control, ref_label, set_uvalue = {drawid:refdraw, toolBar:ref_toolBar, xid:r_xid, yid:r_yid, pindex:3}

	;���ROI�ļ��Ƿ����
	if keyword_set(roifile) eq 0 then roifile = ''

	;����ȫ�ֵ��Զ������
	widget_control, tlb, /realize, set_uvalue={ori_id:ori_label, ref_id:ref_label, $
		plist:plist, btnautoadd:btnautoadd, controlfile : controlfile, roifile:roifile}
	;����ԭʼ�ļ�
	TMCorrection_open, tlb, datafile, 'orifile'
	;���زο��ļ�
	TMCorrection_open, tlb, reffile, 'reffile'
	xmanager, 'TMCorrection', tlb
end

;ʵ��TM���ξ����п��Ƶ��ʹ�÷���������ΪԭʼӰ��Ϳ��Ƶ��ļ�
pro tm_cmd
	;���Ƶ��ļ�
	filename = 'D:\yld\ETProject\ETViewer\cp2.dat'
	;ԭʼӰ��
	oriFile = 'E:\TM_C\Data1\tm5_12332_20040417.tif'
	;�ο�Ӱ��
	refFile = 'D:\yld\ETProject\testdata\tm\12332_20030913atc.tif'
	;��ȡ�ļ�����
	openr, lun, filename, /get_lun
	table_rows = file_lines(filename)
	if table_rows eq 0 then begin
		msg = dialog_message('�ļ�Ϊ�գ�����ʧ�ܣ�', title='TM���ξ���', /info)
		return
	endif
	;����ԭʼӰ���ͶӰ��Ϣ
	print, 'ԭʼӰ�� : ', query_tiff(oriFile, info1, geotiff= origeo)
	;���زο�Ӱ���ͶӰ��Ϣ
	print, '�ο�Ӱ�� : ', query_tiff(refFile, info2, geotiff= refgeo)
	;����ļ�
	outfile = 'E:\TM_C\tm5_12332_20031211_corrected.tif'
	;��������
	filestr = strarr(table_rows)
	readf, lun, filestr
	free_lun, lun
	;�������
	coordinate = strarr(5, table_rows)
	for i=0,table_rows-1 do begin
		spos = strsplit(filestr[i], ',')
		;�������5���ָ��Ķ��ţ��������Ч
		if n_elements(spos) ne 5 then continue
		coordinate[0, i] = strmid(filestr[i], spos[0], spos[1]- spos[0]-1)
		coordinate[1, i] = strmid(filestr[i], spos[1], spos[2]- spos[1]-1)
		coordinate[2, i] = strmid(filestr[i], spos[2], spos[3]- spos[2]-1)
		coordinate[3, i] = strmid(filestr[i], spos[3], spos[4]- spos[3]-1)
		coordinate[4, i] = strmid(filestr[i], spos[4])
	endfor
	;x�����Ӧ
	;x1ΪԭʼӰ���ϵ����꣬x2Ϊ�ο�Ӱ���ϵ�����
	x1list = rotate(coordinate[1, *], 3)
	x2list = rotate(coordinate[3, *], 3)
	;����X�����б�ʡ��ؾ�����ϵ��
	kx = regress(x1list, x2list, const=constx, sigma=sigmax)
	print, 'x parameter : ', kx[0], constx, sigmax[0]
	;��
	leftx1 = 0
	leftx2 = constx
	;��
	rightx2 = info2.dimensions[0]
	rightx1 = (rightx2 - constx) / kx[0]
	print, 'x1', leftx1, rightx1
	print, 'x2', leftx2, rightx2
	;y�����Ӧ
	;y1ΪԭʼӰ���ϵ����꣬y2Ϊ�ο�Ӱ���ϵ�����
	y1list = rotate(coordinate[2, *], 3)
	y2list = rotate(coordinate[4, *], 3)
	;����X�����б�ʡ��ؾ�����ϵ��
	ky = regress(y1list, y2list, const=consty, sigma=sigmay)
	print, 'y parameter : ', ky[0], consty, sigmay[0]
	;��
	topy2 = info2.dimensions[1]
	topy1 = (topy2 - consty) / ky[0]
	;��
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
	print, '�ο�Ӱ���Ľǵ����꣨x, y����' + '(0,0), (0, ' + strY + '), (' + strX + ',' + strY + '), (' + strX + ',0)'
	;
	strX0 = strtrim(constx, 2)
	strY0 = strtrim(consty, 2)
	strXM = strtrim(kx[0] * (info2.dimensions[0]-1) + constx, 2)
	strYM = strtrim(ky[0] * (info2.dimensions[1]-1) + consty, 2)
	print, 'ԭʼӰ���С��',info1.dimensions
	print, '�ǵ���ԭʼӰ���λ�ã�x, y����' + '(' + strX0 + ',' + strY0 + '), (' + strX0 + ', ' + strYM + '), (' + strXM + ',' + strYM + '), (' + strXM + ',' + strY0 + ')'
	;���Ľǿ��Ƶ���뵽���Ƶ�������ȥ
	x1list = [x1list, leftx1, leftx1, rightx1, rightx1]
	y1list = [y1list, topy1, bottomy1, topy1, bottomy1]
	;����ƽ��
	x2list = [x2list, leftx2, leftx2, rightx2, rightx2] - leftx2
	y2list = [y2list, topy2, bottomy2, topy2, bottomy2] - bottomy2

	;���ο�Ӱ���goetiff��Ϣ
	print, '�ο�Ӱ��', query_tiff(refFile, geotiff=ref_geotiff)
	;����������ļ�(0,topy)���ڲο�Ӱ���ϵ�λ�÷ֱ���
	ref_x0 = constx
	ref_ytop = ky[0] * info1.dimensions[1] + consty - info2.dimensions[1]
	print, 'ref_x0 = ', ref_x0, ', ref_ytop = ', ref_ytop
	;����õ�Ĵ������	ge
	geo_x = refgeo.MODELTIEPOINTTAG[3] + 30. * ref_x0
	geo_y = refgeo.MODELTIEPOINTTAG[4] + 30. * ref_ytop
	;���ü��ξ����ĺ���
	;��ʱ�ļ�
	tempfile = outfile + '.tmp'
	openw, templun, tempfile, /get_lun
	;��¼��дλ��
	fileposlist = lonarr(7)
	;���ͨ�����о���
	for i=0, 6 do begin
		data = read_tiff(oriFile, channels=i)
		newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, newdata
	endfor
	;�ر���ʱ�ļ�
	free_lun, templun
	;���´���ʱ�ļ�
	openr, templun, tempfile, /get_lun
	;д����ļ�
	openw, lun, outfile[0], /get_lun
	;дtiff�ļ�ͷ��Ϣ
	writeu, lun, bytarr(8)
	;�ֿ�ϳ��߸�ͨ������д�����ļ�
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
	;д���µĲ���
	;print, 'i : ', i
	theheight = info1.dimensions[1] - i
	;������һ��ʣ��ʱ��дʣ�����
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
	;�ϲ���������ļ�����д��geotiff��Ϣ
	geotiff = {	resolution : double([30., 30., 0.]), $
				prjlocation : double([0., 0., 0., geo_x, geo_y, 0.])}
	makegeototifffile, lun, info1, geotiff = geotiff
	free_lun, templun
	free_lun, lun
	;�����ʱ�ļ�
	file_delete, tempfile
	print, '���TM���ξ���'
end

;ʵ��TM���ξ����п��Ƶ��ʹ�÷���������ΪԭʼӰ��Ϳ��Ƶ��ļ�
pro tm2_cmd
	;���Ƶ��ļ�
	filename = 'E:\TM_C\Data1\0417.dat'
	;ԭʼӰ��
	oriFile = 'E:\TM_C\Data1\tm5_12332_20040417.tif'
	;�ο�Ӱ��
	refFile = 'D:\yld\ETProject\testdata\tm\12332_20030913atc.tif'
	;��ȡ�ļ�����
	openr, lun, filename, /get_lun
	table_rows = file_lines(filename)
	if table_rows eq 0 then begin
		msg = dialog_message('�ļ�Ϊ�գ�����ʧ�ܣ�', title='TM���ξ���', /info)
		return
	endif
	;����ԭʼӰ���ͶӰ��Ϣ
	print, 'ԭʼӰ�� : ', query_tiff(oriFile, info1, geotiff= origeo)
	;���زο�Ӱ���ͶӰ��Ϣ
	print, '�ο�Ӱ�� : ', query_tiff(refFile, info2, geotiff= refgeo)
	;����ļ�
	outfile = 'E:\TM_C\Data1\tm5_12332_20040417_geoed.tif'
	;��������
	filestr = strarr(table_rows)
	readf, lun, filestr
	free_lun, lun
	;�������
	coordinate = strarr(5, table_rows)
	for i=0,table_rows-1 do begin
		spos = strsplit(filestr[i], ',')
		;�������5���ָ��Ķ��ţ��������Ч
		if n_elements(spos) ne 5 then continue
		coordinate[0, i] = strmid(filestr[i], spos[0], spos[1]- spos[0]-1)
		coordinate[1, i] = strmid(filestr[i], spos[1], spos[2]- spos[1]-1)
		coordinate[2, i] = strmid(filestr[i], spos[2], spos[3]- spos[2]-1)
		coordinate[3, i] = strmid(filestr[i], spos[3], spos[4]- spos[3]-1)
		coordinate[4, i] = strmid(filestr[i], spos[4])
	endfor
	;x�����Ӧ
	;x1ΪԭʼӰ���ϵ����꣬x2Ϊ�ο�Ӱ���ϵ�����
	x1list = double(rotate(coordinate[1, *], 3))
	x2list = double(rotate(coordinate[3, *], 3))
	;����X�����б�ʡ��ؾ�����ϵ��
	kx = regress(x1list, x2list, const=constx, sigma=sigmax)
	print, 'x parameter : ', kx[0], constx, sigmax[0]
	;��
	leftx1 = 0
	leftx2 = constx
	;��
	rightx2 = info2.dimensions[0]
	rightx1 = (rightx2 - constx) / kx[0]
	print, 'x1', leftx1, rightx1
	print, 'x2', leftx2, rightx2
	;y�����Ӧ
	;y1ΪԭʼӰ���ϵ����꣬y2Ϊ�ο�Ӱ���ϵ�����
	y1list = double(rotate(coordinate[2, *], 3))
	y2list = double(rotate(coordinate[4, *], 3))
	;����X�����б�ʡ��ؾ�����ϵ��
	ky = regress(y1list, y2list, const=consty, sigma=sigmay)
	print, 'y parameter : ', ky[0], consty, sigmay[0]
	;��
	topy2 = info2.dimensions[1]
	topy1 = (topy2 - consty) / ky[0]
	;��
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
	print, '�ο�Ӱ���Ľǵ����꣨x, y����' + '(0,0), (0, ' + strY + '), (' + strX + ',' + strY + '), (' + strX + ',0)'
	;
	strX0 = strtrim(constx, 2)
	strY0 = strtrim(consty, 2)
	strXM = strtrim(kx[0] * (info2.dimensions[0]-1) + constx, 2)
	strYM = strtrim(ky[0] * (info2.dimensions[1]-1) + consty, 2)
	print, 'ԭʼӰ���С��',info1.dimensions
	print, '�ǵ���ԭʼӰ���λ�ã�x, y����' + '(' + strX0 + ',' + strY0 + '), (' + strX0 + ', ' + strYM + '), (' + strXM + ',' + strYM + '), (' + strXM + ',' + strY0 + ')'
	;���Ľǿ��Ƶ���뵽���Ƶ�������ȥ
	x1list = [x1list, leftx1, leftx1, rightx1, rightx1]
	y1list = [y1list, topy1, bottomy1, topy1, bottomy1]
	;����ƽ��
	x2list = [x2list, leftx2, leftx2, rightx2, rightx2] - leftx2
	y2list = [y2list, topy2, bottomy2, topy2, bottomy2] - bottomy2

	;���ο�Ӱ���goetiff��Ϣ
	print, '�ο�Ӱ��', query_tiff(refFile, geotiff=ref_geotiff)
	;����������ļ�(0,topy)���ڲο�Ӱ���ϵ�λ�÷ֱ���
	ref_x0 = constx
	ref_ytop = ky[0] * info1.dimensions[1] + consty - info2.dimensions[1]
	print, 'ref_x0 = ', ref_x0, ', ref_ytop = ', ref_ytop
	;����õ�Ĵ������	ge
	geo_x = refgeo.MODELTIEPOINTTAG[3] + 30. * ref_x0
	geo_y = refgeo.MODELTIEPOINTTAG[4] + 30. * ref_ytop
	return
	;���ü��ξ����ĺ���
	;��ʱ�ļ�
	tempfile = outfile + '.tmp'
	openw, templun, tempfile, /get_lun
	;��¼��дλ��
	fileposlist = lonarr(7)
	;���ͨ�����о���
	for i=0, 6 do begin
		data = read_tiff(oriFile, channels=i)
		;newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		point_lun, -templun, filepos
		fileposlist[i] = filepos
		writeu, templun, data
	endfor
	;�ر���ʱ�ļ�
	free_lun, templun
	;���´���ʱ�ļ�
	openr, templun, tempfile, /get_lun
	;д����ļ�
	openw, lun, outfile[0], /get_lun
	;дtiff�ļ�ͷ��Ϣ
	writeu, lun, bytarr(8)
	;�ֿ�ϳ��߸�ͨ������д�����ļ�
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
	;д���µĲ���
	;print, 'i : ', i
	theheight = info1.dimensions[1] - i
	;������һ��ʣ��ʱ��дʣ�����
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
	;�ϲ���������ļ�����д��geotiff��Ϣ
	geotiff = {	resolution : double([30., 30., 0.]), $
				prjlocation : double([0., 0., 0., geo_x, geo_y, 0.])}
	makegeototifffile, lun, info1, geotiff = geotiff
	free_lun, templun
	free_lun, lun
	;�����ʱ�ļ�
	file_delete, tempfile
	print, '���TM���ξ���'
end