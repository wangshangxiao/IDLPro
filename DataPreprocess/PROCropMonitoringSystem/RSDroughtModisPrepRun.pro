;��ȡmodisԤ�������������б�
function getModisPreTypes
	return, ['EV_1KM_Emissive_b0',$
		'EV_1KM_Emissive_b10',$
		'EV_1KM_Emissive_b11',$
		'EV_1KM_Emissive_b14',$
		'EV_1KM_Emissive_b2',$
		'EV_1KM_Emissive_b6',$
		'EV_1KM_RefSB_b11',$
		'EV_1KM_RefSB_b12',$
		'EV_1KM_RefSB_b13',$
		'EV_250_RefSB_b0',$
		'EV_250_RefSB_b1',$
		'EV_500_RefSB_b0',$
		'EV_500_RefSB_b1',$
		'EV_500_RefSB_b2',$
		'EV_500_RefSB_b3',$
		'EV_500_RefSB_b4',$
		'EV_Band26',$
		'SensorAzimuth',$
		'SensorZenith',$
		'SolarAzimuth',$
		'SolarZenith']
end

;��ָ��Ŀ¼�����ȡmodisԤ���������ļ��б�Ĺ���
function getModisPreFiles ,OUTPUT
	;syspath = getsyspath()
	;mpath = syspath.modis_pre
	;ipath = syspath.modis_product	;�ر����Ŀ¼

;	mpath = 'F:\dataout\'
;	ipath ='F:\dataout\'
    mpath = OUTPUT
    ipath = OUTPUT

	;�ж�ң������Ԥ����Ŀ¼�Ƿ����
	if file_test(mpath, /directory) eq 0 then begin
		return, {error:1, msg:'������ң�����ݴ���Ŀ¼��'}
	endif

	;����Ŀ¼������ļ�
	files = file_search(mpath + '????????_EV_1KM_Emissive_b0.tif')
	if files[0] eq '' then begin
		return, {error:1, msg:'ң�����ݴ���Ŀ¼��û�пɹ�ʹ�õ����ݣ�'}
	endif

	;��ȡ�����ֶ�
	n = n_elements(files)
	datelist = ['']
	for i=0, n-1 do begin
		date = strmid(file_basename(files[i]), 0, 8)
		if isdate(date) eq 1 then datelist = [datelist, date]
	endfor
	if n_elements(datelist) eq 1 then begin
		return, {error:1, msg:'ң�����ݴ���Ŀ¼��û�пɹ�ʹ�õ����ݣ���������ʽ����ȷ��'}
	endif
	datelist = datelist[1:*]
	ndate = n_elements(datelist)

	;Ԥ�����������б�
	typelist = getModisPreTypes()
	ntype = n_elements(typelist)

	datelist1 = ''
	filelist = ''
	filelist1 = strarr(ntype)

	;��������ļ�
	for i=0, ndate-1 do begin
		filelist1[*] = ''
		for j=0, ntype-1 do begin
			filelist1[j] = mpath + datelist[i] + '_' + typelist[j] + '.tif'
			if file_test(filelist1[j]) eq 0 then break
		endfor
		;��������ļ�����������������Ч��������Ч
		if j lt ntype then continue

		;���ҵر�����ļ�������в������ڱȷֲ����ļ��£�����첻����
		pfile = ipath + datelist[i] + '_cld'
		if file_test(pfile) eq 1 then begin
			;�Ƚ��ļ�����
			fi1 = file_info(filelist1[0])
			fi2 = file_info(pfile)
			if fi1.mtime lt fi2.mtime then continue
		endif

		;�����Ч����
		datelist1 = [datelist1, datelist[i]]
		filelist = [filelist, filelist1]
	endfor
	if n_elements(datelist1) eq 1 then begin
		return, {error:1, msg:'ң�����ݴ���Ŀ¼�µ����ݲ����������Ѿ����й��ر�������㣡'}
	endif

	return, {error:0, datelist:datelist1[1:*], filelist:filelist[1:*]}
end

;��ȡmodis 1B�����б�
;typeΪaqua��terra
function getModis1BFiles, modis_type
;	syspath = getsyspath()
;	dir = syspath.modis_1b
   dir= 'F:\drought\modis_1b\nasa\'
	;��ָ��Ŀ¼��������1km�����ļ�
	;MYD03.A2009186.0135.005.2009187234427.hdf
	files = file_search(dir + modis_type + '03.A???????.????.???.?????????????.hdf')
    print,files

	if files[0] eq '' then begin
		msg = 'ָ��Ŀ¼����û��Modis 1B���ݣ���û�а���ָ����ʽ����������'
		showmsg, msg, /win, /log
		return, ''
	endif

	;��modis1b�ļ������л�ȡ���ں�ʱ����Ϣ,��aqua��terra����ͬ
	;pos = modis_type eq 'AQUA' ? 0 : 1
    pos= 0
	;�����ȡʱ��
	n = n_elements(files)
	geofile = ['']
	for i=0, n-1 do begin
		year = strmid(file_basename(files[i]), pos+7, 4)
		month_day = strmid(file_basename(files[i]), pos+11, 3)
        print,month_day
		md=changedatename(year+month_day)
        print,md
	    month=strmid(md,0,2)
	    day=strmid(md,2,2)
		;day = strmid(file_basename(files[i]), pos+13, 2)
		hour = strmid(file_basename(files[i]), pos+15, 2)
		minute = strmid(file_basename(files[i]), pos+17, 2)
		;site = strmid(file_basename(files[i]), pos+22, 2)

        print,hour

		;����ʱ��8��֮ǰ,18��֮����ų�
		if fix(hour) lt 1 or fix(hour) gt 18 then continue
		geofile = [geofile, files[i]]
		;print, strjoin([site, year, month, day, hour, minute], ',')
	endfor

	ngeo = n_elements(geofile) - 1
	if ngeo le 0 then begin
		msg = 'ָ��Ŀ¼����û��Modis 1B���ݣ���û�а���ָ����ʽ����������'
		showmsg, msg, /log
		return, ''
	endif

	geofile = geofile[1:*]
	file_list = ['']
    print, geofile
	;�����ڲ����������ļ�
	for i=0, ngeo-1 do begin
		basename = file_basename(geofile[i], '.hdf')
        print, geofile[i]
		print, basename
		basedir = file_dirname(geofile[i])
		file1 = geofile[i]
		if file_test(file1) eq 0 then continue
		file2 = basedir + '\' + basename + '21KM.hdf'
        print,file2
		if file_test(file2) eq 0 then continue
		file3 = basedir + '\' + basename + '2HKM.hdf'
		if file_test(file3) eq 0 then continue
		file4 = basedir + '\' + basename + '2QKM.hdf'
		if file_test(file4) eq 0 then continue
		;������б�
		file_list = [file_list, file1, file2, file3, file4]
	endfor
	nfiles = n_elements(file_list) - 1
	if nfiles lt 0 then begin
		msg = 'ָ��Ŀ¼����û��Modis 1B���ݣ������ݲ�ȫ����û�а���ָ����ʽ����������'
		showmsg, msg, /log
		return, ''
	endif
;--------------------------------------------------------------------------------------
	;�����ṹ��
	typelist = ['GEO','1KM','HKM','QKM']
	datelist = ['']
	dlist = ['']

	tva = strarr(3, nfiles)
	for i=0, nfiles-1 do begin
		year = strmid(file_basename(file_list[i+1]), pos+5, 4)
		month = strmid(file_basename(file_list[i+1]), pos+10, 2)
		day = strmid(file_basename(file_list[i+1]), pos+13, 2)

		;���ɱ��
		tva[0, i] = TransDateToString(year, month, day)
		tva[1, i] = typelist[i mod 4]
		tva[2, i] = file_list[i+1]

		;����������б�
		datelist = [datelist, tva[0, i]]
		dlist = [dlist, TransDateToChinese(year, month, day)]
	endfor

	dlist = dlist[1:*]
	dlist = dlist[uniq(dlist, sort(dlist))]
	datelist = datelist[uniq(datelist, sort(datelist))]

	;��ȡΨһ�������б�
	n_days  = n_elements(datelist) - 1
	if n_days eq 0 then begin
		showmsg, '�޷��������ݣ�', /log
		return, ''
	endif
	datelist = datelist[1:*]

	;�ж�������Ϣ
	for i=0,n_days-1 do begin
		;�ж������Ƿ�����
		index = where(tva[0, *] eq datelist[i], count)
		if index[0] eq -1 then continue

		for j=0,count-1, 4 do begin
			tlist = tva[1, index[j]:index[j+3]]
			tlist = tlist[uniq(tlist, sort(tlist))]
			if n_elements(tlist) ne 4 then begin
				date1 = getDateOfString(datelist[i])
				date2 = TransDateToChinese(date1.year, date1.month, date1.day)
				showmsg, '�����������ļ������ͣ����ڣ�' + date2, /log
				return, ''
			endif
		endfor
	endfor
	return, {n_days:n_days, datelist:datelist, tva:tva}
end

;----------------------------------------------------------
;�ر�ָ�����㹦��
pro RSDroughtIndexRun,AC_STATUS,MIDOUT
	;��ȡ·������
	;idlist = widget_uvalue(event.top)
	;syspath = getsyspath()
;	modis_pre = 'F:\dataout\'
;	modis_product ='F:\dataout\'
	modis_pre = MIDOUT
	modis_product= MIDOUT

	mstruct = getModisPreFiles(MIDOUT)

	if mstruct.error eq 1 then begin
		showmsg, mstruct.msg, /log
		return
	endif

	;��ǰ����Ч���ڣ���ʾ�����б�
	ndate = n_elements(mstruct.datelist)

	;Ԥ�����������б�
	typelist = getModisPreTypes()
	ntype = n_elements(typelist)

	tvalue = strarr(4, ndate*ntype)
	for i=0, ndate-1 do begin
		tvalue[0, i*ntype] = strtrim(i+1, 2)
		tvalue[1, i*ntype] = mstruct.datelist[i]
		tvalue[3, i*ntype:((i+1)*ntype-1)] = mstruct.filelist[i*ntype:((i+1)*ntype-1)]
	endfor

	if Valid_Set(mstruct) eq 0 then begin
		showmsg, '������ļ��б������޷����㡣', /log
		return
	endif

	;��ʾ������
;	idlist.step = 2
;	RSDrought_Refresh, idlist
	startTime = systime(/jul)
;  if(idlist.step = 2 ) then begin
;   return
;  endif
	;���մ���
	ndate = n_elements(mstruct.datelist)
	;syspath = getsyspath()

	acr = AC_STATUS

	if acr eq 0 then begin
		showmsg, '���μ��㲻ִ�д�������������', /log
	;	print, '���μ��㲻ִ�д�������������'
	endif else begin
		showmsg, '���μ���ִ�д�������������', /log
	;	print, '���μ���ִ�д�������������', /log
	endelse
  atime = systime(/s)
	roi$ = 'data\base\china_1km_roi.tif'
	for i=0, ndate-1 do begin
		;RSDroughtSetTitle, event.top, '�ر�������� : ' + TransStringToChinese(mstruct.datelist[i])
		;dir = {modis_pre:syspath.modis_pre, date:mstruct.datelist[i], mark:'AQUA', modis_product:syspath.modis_product}
		dir = {modis_pre: modis_pre, date:mstruct.datelist[i], mark:'AQUA', modis_product: modis_product}
		;��ModisԤ�����ļ��и��5X5�ķ���
		segment, dir

		;�����д���
		for j = 0, 4 do begin
			for k = 0, 4 do begin
				part = '_' + strtrim(j, 2) + strtrim(k, 2)
				dem$='data\base\china_1km_dem' + part + '.tif'
				latitude$ = 'data\base\CHINA_1KM_LAT' + part + '.TIF'  ;�������ֵ���γ�ȣ���0��1��2�ֱ�����γ��(�ȴ�)����γ��(�´�)����γ��(����)
				;�Ƽ��
				MODIS_CLOUD_MASK, dir, part, dem$
				;
				index = where(['_00', '_01', '_02', '_03', '_04', '_10', '_11', '_12', $
					'_13', '_14', '_20', '_21', '_22', '_23', '_24', '_30', '_31', $
					'_32', '_33', '_34', '_40', '_41', '_42', '_43', '_44'] eq part, count)
				if count eq 1 and acr eq 1 then begin
					;atime = systime(/s)
					ATMOSPHERE_PRODUCT, dir, part, latitude$, /aerosol
					print, 'ATMOSPHERE_PRODUCT:', systime(/s) - atime
					;atime = systime(/s)
					MODIS_ATM_COR, dir, part, latitude$
					print, 'MODIS_ATM_COR:', systime(/s) - atime

					LAND_SURFACE_PRODUCT, dir, part, /ndvi, /acr
				endif else begin
					;���ɲ�������������ndvi
					LAND_SURFACE_PRODUCT, dir, part, /ndvi
				endelse

				;���ɵ���
				LAND_SURFACE_PRODUCT, dir, part, /lst
			endfor
		endfor

		;�ѷֿ�Ľ���ϲ�һ���ļ�
		unit, dir.modis_product, dir.date, 'ndvi', roi$, /acr
		unit, dir.modis_product, dir.date, 'lst', roi$

		;���������յ����ļ���NaNֵ
		data = read_tiff(dir.modis_product + dir.date + 'TS.tif', geotiff = geotiff)
		if(total(FINITE(data, /NAN)) gt 0) then begin
			data[where(FINITE(data, /NAN))] = 0
		endif
		write_tiff, dir.modis_product + dir.date + 'TS.tif', data, geotiff = geotiff, /short

		unit, dir.modis_product, dir.date, 'cld', roi$

		;�ֿ��ļ���ʶ�б�
		mark = ['ndvi', 'lst', 'cld', 'SolarZenith', 'SolarAzimuth', 'SensorZenith', 'SensorAzimuth', 'EV_Band26', 'EV_500_RefSB_b4', 'EV_500_RefSB_b3', $
			'EV_500_RefSB_b2', 'EV_500_RefSB_b1', 'EV_500_RefSB_b0', 'EV_250_RefSB_b1', 'EV_250_RefSB_b0', 'EV_1KM_RefSB_b13', 'EV_1KM_RefSB_b12', 'EV_1KM_RefSB_b11', $
			'EV_1KM_Emissive_b14', 'EV_1KM_Emissive_b11', 'EV_1KM_Emissive_b10', 'EV_1KM_Emissive_b6', 'EV_1KM_Emissive_b2', 'EV_1KM_Emissive_b0']

		;ɾ���ֿ��ļ�
		for seg_i = 0, n_elements(mark) - 1 do begin
			segfile_del, dir.modis_pre, dir.date, mark[seg_i]
			segfile_del, dir.modis_product, dir.date, mark[seg_i]
		endfor

		;��TIFת��ΪHDR��ʽ���ļ�
		mark = ['ndvi', 'TS', 'cld']
		tif2hdr, dir, mark, roi$

	;	showmsg, '�ر����������ɣ�' + dir.date , /log
		print, '����ر��������ʱ�䣺' ,(systime(/s) - atime)/3600
		;showmsg,'����ر��������ʱ��:' + string((systime(/s) - atime)/3600)+ 'Сʱ' ,/log
	endfor

	;������ɺ�,ɾ��modisԤ������
	clearFolder, modis_pre, '*.tif'
	;ɾ������������صĽ��
	clearFolder, modis_product, '*.tif'
end

;----------------------------------------------------------
;modis����Ԥ������
pro RSDroughtModisPrepRun, MODIS1B,AC_STATUS,OUTPUT,MRTSwath,MRTSwathData
;	idlist = widget_uvalue(event.top)
;	syspath = getsyspath()
;	dir = syspath.modis_1b
   ;dir= 'F:\drought\modis_1b\'
	;д����־
;	showmsg, 'ModisԤ�������㿪ʼִ��', /log
	;worklog, 'ModisԤ����ʼ����'
;   OUTPUT='F:\'
	;��ʾ������
;	idlist.step = 1
;	RSDrought_Refresh, idlist
	;�����ʹ���

        ;modis_type = modis eq 0 ? 'AUQA' : 'TERRA'
;		modis_type = modis eq 0 ? 'MYD' : 'MOD'
;        print, modis_type
;		tpara = getModis1BFiles(modis_type)
;		if valid_set(tpara) eq 0 then continue

		;���մ���
		for i=0, N_elements(MODIS1B)-1 do begin
;		for i=0, tpara.n_days-1 do begin
			;��ȡ��������
;			index = where(tpara.tva[0, *] eq tpara.datelist[i], count)
;			if count eq 0 then continue
;			tvalue = tpara.tva[*, index]
;			list_1km = ['']
;			list_hkm = ['']
;			list_qkm = ['']
;			list_geo = ['']
;
;			for j=0, count-1 do begin
;				;�������б����������
;				if tvalue[1, j] eq '1KM' then list_1km = [list_1km, tvalue[2, j]]
;				if tvalue[1, j] eq 'HKM' then list_hkm = [list_hkm, tvalue[2, j]]
;				if tvalue[1, j] eq 'QKM' then list_qkm = [list_qkm, tvalue[2, j]]
;				if tvalue[1, j] eq 'GEO' then list_geo = [list_geo, tvalue[2, j]]
;			endfor
			;�������յĲ���
			;help, Day1B, /struc
			;���ñ���
		;	RSDroughtSetTitle, event.top, 'Modis Ԥ���� : ' + TransStringToChinese(tpara.datelist[i])
			;���յ���ModisԤ������㹦��
			;print, MODIS1B.data
			MODIS_IRSA_AUTO, *(MODIS1B[i]),AC_STATUS,OUTPUT,MRTSwath,MRTSwathData
			;showmsg, 'Modis����Ԥ������ɣ�' + tpara.datelist[i] + '[' + modis_type + ']', /log

	endfor
end

