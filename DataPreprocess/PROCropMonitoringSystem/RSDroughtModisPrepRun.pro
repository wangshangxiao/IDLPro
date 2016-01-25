;获取modis预处理结果的类型列表
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

;从指定目录下面获取modis预处理数据文件列表的功能
function getModisPreFiles ,OUTPUT
	;syspath = getsyspath()
	;mpath = syspath.modis_pre
	;ipath = syspath.modis_product	;地表参数目录

;	mpath = 'F:\dataout\'
;	ipath ='F:\dataout\'
    mpath = OUTPUT
    ipath = OUTPUT

	;判断遥感数据预处理目录是否存在
	if file_test(mpath, /directory) eq 0 then begin
		return, {error:1, msg:'请设置遥感数据处理目录！'}
	endif

	;查找目录下面的文件
	files = file_search(mpath + '????????_EV_1KM_Emissive_b0.tif')
	if files[0] eq '' then begin
		return, {error:1, msg:'遥感数据处理目录下没有可供使用的数据！'}
	endif

	;提取日期字段
	n = n_elements(files)
	datelist = ['']
	for i=0, n-1 do begin
		date = strmid(file_basename(files[i]), 0, 8)
		if isdate(date) eq 1 then datelist = [datelist, date]
	endfor
	if n_elements(datelist) eq 1 then begin
		return, {error:1, msg:'遥感数据处理目录下没有可供使用的数据，或命名方式不正确！'}
	endif
	datelist = datelist[1:*]
	ndate = n_elements(datelist)

	;预处理结果类型列表
	typelist = getModisPreTypes()
	ntype = n_elements(typelist)

	datelist1 = ''
	filelist = ''
	filelist1 = strarr(ntype)

	;逐天加载文件
	for i=0, ndate-1 do begin
		filelist1[*] = ''
		for j=0, ntype-1 do begin
			filelist1[j] = mpath + datelist[i] + '_' + typelist[j] + '.tif'
			if file_test(filelist1[j]) eq 0 then break
		endfor
		;如果所有文件都存在则当天数据有效，否则无效
		if j lt ntype then continue

		;查找地表参数文件，如果有并且日期比分波段文件新，则该天不计算
		pfile = ipath + datelist[i] + '_cld'
		if file_test(pfile) eq 1 then begin
			;比较文件日期
			fi1 = file_info(filelist1[0])
			fi2 = file_info(pfile)
			if fi1.mtime lt fi2.mtime then continue
		endif

		;添加有效日期
		datelist1 = [datelist1, datelist[i]]
		filelist = [filelist, filelist1]
	endfor
	if n_elements(datelist1) eq 1 then begin
		return, {error:1, msg:'遥感数据处理目录下的数据不完整，或已经进行过地表参数计算！'}
	endif

	return, {error:0, datelist:datelist1[1:*], filelist:filelist[1:*]}
end

;获取modis 1B数据列表
;type为aqua和terra
function getModis1BFiles, modis_type
;	syspath = getsyspath()
;	dir = syspath.modis_1b
   dir= 'F:\drought\modis_1b\nasa\'
	;在指定目录下面搜索1km地理场文件
	;MYD03.A2009186.0135.005.2009187234427.hdf
	files = file_search(dir + modis_type + '03.A???????.????.???.?????????????.hdf')
    print,files

	if files[0] eq '' then begin
		msg = '指定目录下面没有Modis 1B数据，或没有按照指定格式进行命名！'
		showmsg, msg, /win, /log
		return, ''
	endif

	;从modis1b文件名称中获取日期和时间信息,随aqua和terra而不同
	;pos = modis_type eq 'AQUA' ? 0 : 1
    pos= 0
	;逐个提取时间
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

		;北京时间8点之前,18点之后的排除
		if fix(hour) lt 1 or fix(hour) gt 18 then continue
		geofile = [geofile, files[i]]
		;print, strjoin([site, year, month, day, hour, minute], ',')
	endfor

	ngeo = n_elements(geofile) - 1
	if ngeo le 0 then begin
		msg = '指定目录下面没有Modis 1B数据，或没有按照指定格式进行命名！'
		showmsg, msg, /log
		return, ''
	endif

	geofile = geofile[1:*]
	file_list = ['']
    print, geofile
	;由日期查找其它轨文件
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
		;添加入列表
		file_list = [file_list, file1, file2, file3, file4]
	endfor
	nfiles = n_elements(file_list) - 1
	if nfiles lt 0 then begin
		msg = '指定目录下面没有Modis 1B数据，或数据不全，或没有按照指定格式进行命名！'
		showmsg, msg, /log
		return, ''
	endif
;--------------------------------------------------------------------------------------
	;构建结构体
	typelist = ['GEO','1KM','HKM','QKM']
	datelist = ['']
	dlist = ['']

	tva = strarr(3, nfiles)
	for i=0, nfiles-1 do begin
		year = strmid(file_basename(file_list[i+1]), pos+5, 4)
		month = strmid(file_basename(file_list[i+1]), pos+10, 2)
		day = strmid(file_basename(file_list[i+1]), pos+13, 2)

		;生成表格
		tva[0, i] = TransDateToString(year, month, day)
		tva[1, i] = typelist[i mod 4]
		tva[2, i] = file_list[i+1]

		;添加入日期列表
		datelist = [datelist, tva[0, i]]
		dlist = [dlist, TransDateToChinese(year, month, day)]
	endfor

	dlist = dlist[1:*]
	dlist = dlist[uniq(dlist, sort(dlist))]
	datelist = datelist[uniq(datelist, sort(datelist))]

	;获取唯一的日期列表
	n_days  = n_elements(datelist) - 1
	if n_days eq 0 then begin
		showmsg, '无法加载数据！', /log
		return, ''
	endif
	datelist = datelist[1:*]

	;判断设置信息
	for i=0,n_days-1 do begin
		;判断类型是否设置
		index = where(tva[0, *] eq datelist[i], count)
		if index[0] eq -1 then continue

		for j=0,count-1, 4 do begin
			tlist = tva[1, index[j]:index[j+3]]
			tlist = tlist[uniq(tlist, sort(tlist))]
			if n_elements(tlist) ne 4 then begin
				date1 = getDateOfString(datelist[i])
				date2 = TransDateToChinese(date1.year, date1.month, date1.day)
				showmsg, '请设置数据文件的类型！日期：' + date2, /log
				return, ''
			endif
		endfor
	endfor
	return, {n_days:n_days, datelist:datelist, tva:tva}
end

;----------------------------------------------------------
;地表指数计算功能
pro RSDroughtIndexRun,AC_STATUS,MIDOUT
	;获取路径设置
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

	;当前有有效日期，显示数据列表
	ndate = n_elements(mstruct.datelist)

	;预处理结果类型列表
	typelist = getModisPreTypes()
	ntype = n_elements(typelist)

	tvalue = strarr(4, ndate*ntype)
	for i=0, ndate-1 do begin
		tvalue[0, i*ntype] = strtrim(i+1, 2)
		tvalue[1, i*ntype] = mstruct.datelist[i]
		tvalue[3, i*ntype:((i+1)*ntype-1)] = mstruct.filelist[i*ntype:((i+1)*ntype-1)]
	endfor

	if Valid_Set(mstruct) eq 0 then begin
		showmsg, '请加载文件列表，否则无法计算。', /log
		return
	endif

	;显示进度条
;	idlist.step = 2
;	RSDrought_Refresh, idlist
	startTime = systime(/jul)
;  if(idlist.step = 2 ) then begin
;   return
;  endif
	;逐日处理
	ndate = n_elements(mstruct.datelist)
	;syspath = getsyspath()

	acr = AC_STATUS

	if acr eq 0 then begin
		showmsg, '本次计算不执行大气纠正操作。', /log
	;	print, '本次计算不执行大气纠正操作。'
	endif else begin
		showmsg, '本次计算执行大气纠正操作。', /log
	;	print, '本次计算执行大气纠正操作。', /log
	endelse
  atime = systime(/s)
	roi$ = 'data\base\china_1km_roi.tif'
	for i=0, ndate-1 do begin
		;RSDroughtSetTitle, event.top, '地表参数计算 : ' + TransStringToChinese(mstruct.datelist[i])
		;dir = {modis_pre:syspath.modis_pre, date:mstruct.datelist[i], mark:'AQUA', modis_product:syspath.modis_product}
		dir = {modis_pre: modis_pre, date:mstruct.datelist[i], mark:'AQUA', modis_product: modis_product}
		;把Modis预处理文件切割成5X5的方格
		segment, dir

		;逐块进行处理
		for j = 0, 4 do begin
			for k = 0, 4 do begin
				part = '_' + strtrim(j, 2) + strtrim(k, 2)
				dem$='data\base\china_1km_dem' + part + '.tif'
				latitude$ = 'data\base\CHINA_1KM_LAT' + part + '.TIF'  ;用于区分地区纬度，由0、1、2分别代表低纬度(热带)、中纬度(温带)、高纬度(寒带)
				;云检测
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
					;生成不带大气纠正的ndvi
					LAND_SURFACE_PRODUCT, dir, part, /ndvi
				endelse

				;生成地温
				LAND_SURFACE_PRODUCT, dir, part, /lst
			endfor
		endfor

		;把分块的结果合并一个文件
		unit, dir.modis_product, dir.date, 'ndvi', roi$, /acr
		unit, dir.modis_product, dir.date, 'lst', roi$

		;消除最终日地温文件中NaN值
		data = read_tiff(dir.modis_product + dir.date + 'TS.tif', geotiff = geotiff)
		if(total(FINITE(data, /NAN)) gt 0) then begin
			data[where(FINITE(data, /NAN))] = 0
		endif
		write_tiff, dir.modis_product + dir.date + 'TS.tif', data, geotiff = geotiff, /short

		unit, dir.modis_product, dir.date, 'cld', roi$

		;分块文件标识列表
		mark = ['ndvi', 'lst', 'cld', 'SolarZenith', 'SolarAzimuth', 'SensorZenith', 'SensorAzimuth', 'EV_Band26', 'EV_500_RefSB_b4', 'EV_500_RefSB_b3', $
			'EV_500_RefSB_b2', 'EV_500_RefSB_b1', 'EV_500_RefSB_b0', 'EV_250_RefSB_b1', 'EV_250_RefSB_b0', 'EV_1KM_RefSB_b13', 'EV_1KM_RefSB_b12', 'EV_1KM_RefSB_b11', $
			'EV_1KM_Emissive_b14', 'EV_1KM_Emissive_b11', 'EV_1KM_Emissive_b10', 'EV_1KM_Emissive_b6', 'EV_1KM_Emissive_b2', 'EV_1KM_Emissive_b0']

		;删除分块文件
		for seg_i = 0, n_elements(mark) - 1 do begin
			segfile_del, dir.modis_pre, dir.date, mark[seg_i]
			segfile_del, dir.modis_product, dir.date, mark[seg_i]
		endfor

		;把TIF转换为HDR格式的文件
		mark = ['ndvi', 'TS', 'cld']
		tif2hdr, dir, mark, roi$

	;	showmsg, '地表参数计算完成：' + dir.date , /log
		print, '计算地表参数所需时间：' ,(systime(/s) - atime)/3600
		;showmsg,'计算地表参数所需时间:' + string((systime(/s) - atime)/3600)+ '小时' ,/log
	endfor

	;计算完成后,删除modis预处理结果
	clearFolder, modis_pre, '*.tif'
	;删除大气纠正相关的结果
	clearFolder, modis_product, '*.tif'
end

;----------------------------------------------------------
;modis数据预处理功能
pro RSDroughtModisPrepRun, MODIS1B,AC_STATUS,OUTPUT,MRTSwath,MRTSwathData
;	idlist = widget_uvalue(event.top)
;	syspath = getsyspath()
;	dir = syspath.modis_1b
   ;dir= 'F:\drought\modis_1b\'
	;写入日志
;	showmsg, 'Modis预处理运算开始执行', /log
	;worklog, 'Modis预处理开始运行'
;   OUTPUT='F:\'
	;显示进度条
;	idlist.step = 1
;	RSDrought_Refresh, idlist
	;逐类型处理

        ;modis_type = modis eq 0 ? 'AUQA' : 'TERRA'
;		modis_type = modis eq 0 ? 'MYD' : 'MOD'
;        print, modis_type
;		tpara = getModis1BFiles(modis_type)
;		if valid_set(tpara) eq 0 then continue

		;逐日处理
		for i=0, N_elements(MODIS1B)-1 do begin
;		for i=0, tpara.n_days-1 do begin
			;获取日数据量
;			index = where(tpara.tva[0, *] eq tpara.datelist[i], count)
;			if count eq 0 then continue
;			tvalue = tpara.tva[*, index]
;			list_1km = ['']
;			list_hkm = ['']
;			list_qkm = ['']
;			list_geo = ['']
;
;			for j=0, count-1 do begin
;				;向数据列表中添加数据
;				if tvalue[1, j] eq '1KM' then list_1km = [list_1km, tvalue[2, j]]
;				if tvalue[1, j] eq 'HKM' then list_hkm = [list_hkm, tvalue[2, j]]
;				if tvalue[1, j] eq 'QKM' then list_qkm = [list_qkm, tvalue[2, j]]
;				if tvalue[1, j] eq 'GEO' then list_geo = [list_geo, tvalue[2, j]]
;			endfor
			;生成逐日的参数
			;help, Day1B, /struc
			;设置标题
		;	RSDroughtSetTitle, event.top, 'Modis 预处理 : ' + TransStringToChinese(tpara.datelist[i])
			;逐日调用Modis预处理计算功能
			;print, MODIS1B.data
			MODIS_IRSA_AUTO, *(MODIS1B[i]),AC_STATUS,OUTPUT,MRTSwath,MRTSwathData
			;showmsg, 'Modis数据预处理完成：' + tpara.datelist[i] + '[' + modis_type + ']', /log

	endfor
end

