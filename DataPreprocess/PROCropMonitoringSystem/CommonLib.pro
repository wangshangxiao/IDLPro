;通用函数库

;通用的窗口默认事件
pro WidgetDefault_event, event
end

;获取一个widget的value
function widget_value, widgetid
	widget_control, widgetid ,get_value = wv
	return, wv
end

;获取一个widget的uvalue
function widget_uvalue, widgetid
	widget_control, widgetid ,get_uvalue = wuv
	return, wuv
end

;窗口关闭功能
pro WidgetClose, event
	confirm = widget_uvalue(event.id)
	if confirm eq 1 then begin
		msg = dialog_message('是否退出当前计算程序？', title='退出确认', /ques)
		if msg eq 'No' then return
	endif
	widget_control, event.top, /destroy
end

;通用的关闭窗口的按钮
function close_button, base, confirm=confirm
	return, widget_button(base, value='关闭', xsize=60, ysize=23, event_pro='WidgetClose', uvalue=keyword_set(confirm))
end

;在一维数组的某个位置插入一个新元素
;如果 index 小于0则返回原组,如果大于array的长度,则放在最后一个
function ArrayPut, array, index, element
	;小于等于0放在最前面
	if index le 0 then return, [element, array]

	;大于数组长度,放在最后面
	a_len = n_elements(array)
	if index ge a_len then return, [array, element]

	;位于数据中间,则原位置以后的元素向后挪一位
	return, [array[0:index-1], element, array[index:*]]
end

;去除掉数组中某一个位置的值
function ArrayRemove, array, index
	;数组长度为1、元素索引小于零或大于数组个数, 返回原数组
	a_len = n_elements(array)
	if a_len eq 1 or index lt 0 or index ge a_len then return, array

	;位于数据首部
	if index eq 0 then return, array[1:*]

	;位于数据尾部
	if index eq a_len-1 then return, array[0:a_len-1]

	;位于数据中间
	return, [array[0:index-1], array[index+1:*]]
end

;把当前日期转换为八位的日期或14位的时间字符串
function GetStringOfNow, time=time, date=date
	caldat, julday(), mon, day, year, hour, minute, second
	datestr = string(year, format='(I04)') + string(mon, format='(I02)') $
		+ string(day, format='(I02)')
	timestr = string(hour, format='(I02)') $
		+ string(minute, format='(I02)') + string(day, format='(I02)')
	if keyword_set(date) then return, datestr
	if keyword_set(time) then return, timestr

	return, datestr + timestr
end

;把日期转换成为八位的数字
function TransDateToString, Year, Month, Day
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    if month lt 10 then Monthstr = '0' + Monthstr
    if day lt 10 then daystr = '0' + Daystr
    datestr = Yearstr + Monthstr + Daystr
    return, datestr
end

;把日期转换成为yyyy年mm月dd日的格式
function TransDateToChinese, Year, Month, Day
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    datestr = Yearstr + '年' + Monthstr +  '月' + Daystr + '日'
    return, datestr
end

;年月日选择控件
function cw_seldate_get_value, id
	widget_control, id, get_uvalue = uv

	widget_control, uv.year, get_value = yearlist
	index = widget_info(uv.year, /droplist_select)
	year = fix(yearlist[index])

	month = fix(widget_info(uv.month, /combobox_gettext))
	day = fix(widget_info(uv.day, /combobox_gettext))

	return, { year:year, month:month, day:day }
end

;ymdlist 是八位的表示时间的字符串数组
function cw_seldate, topbase, ymdlist
	;
	datebase = widget_base(topbase, /row, func_get_value='cw_seldate_get_value')

	year = widget_droplist(datebase, xsize=50, value=strmid(ymdlist, 0, 4))
	lab = widget_label(datebase, value='年')
	month = widget_combobox(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
	lab = widget_label(datebase, value='月')
	day = widget_combobox(datebase, xsize=40, value=strtrim(indgen(31)+1, 2))
	lab = widget_label(datebase, value='日')
	widget_control, datebase, set_uvalue = {year:year, month:month, day:day}
	return, datebase
end

;获取年月选择控件的值
function cw_selmonth_getValue, id
	widget_control, id, get_uvalue = uv
	widget_control, uv.year, get_value = yearlist
	index = widget_info(uv.year, /droplist_select)
	year = yearlist[index]

	widget_control, uv.month, get_value = monthlist
	index = widget_info(uv.month, /droplist_select)
	month = monthlist[index]
	return, { year : long(year), month : long(month) }
end

;年月选择控件
;ymlist 是六位的表示时间的字符串数组
function cw_selmonth, topbase, ymlist
	;
	datebase = widget_base(topbase, /row, func_get_value='cw_selmonth_getValue')

	year = widget_droplist(datebase, xsize=50, value=strmid(ymlist, 0, 4))
	lab = widget_label(datebase, value='年')
	month = widget_droplist(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
	lab = widget_label(datebase, value='月')
	widget_control, datebase, set_uvalue = {year:year, month:month}
	return, datebase
end

;从文本文件中读取字符串
function read_string, filename
	if file_test(filename) eq 0 then return, ''
	nlines = file_lines(filename)
	flines = strarr(nlines)
	openr, lun, filename, /get_lun
	readf, lun, flines
	close, lun
	free_lun, lun
	return, flines
end

;把字符串写入文本文件之中
pro write_string, filename, flines
	openw, lun, filename, /get_lun
	nlines = n_elements(flines)
	for i=0, nlines-1 do begin
		printf, lun, flines[i]
	endfor
	close, lun
	free_lun, lun
end

;从一个ini数组中提取值,类似key=value的形式
function getIniValue, ini, key
	n_ini = n_elements(ini)
	for i=0, n_ini-1 do begin
		items = strsplit(ini[i], '=', /extract, /preserve_null)
		if n_elements(items) lt 2 then continue
		if items[0] eq key then return, items[1]
	endfor
	;没有找到则返回空值
	return, ''
end

;读取一个csv(逗号分隔)文件
;header返回第一行
;separator为指定的分隔符号,默认为逗号
;返回值为n X m的字符串组,其中列数由第一行的列数确定
;如果nohead=1，则首行不返回

function readCSV, file, header=header, seperator=separator, nohead=nohead
	if file_test(file) eq 0 then return, ''
	lines = read_string(file)
	rows = n_elements(lines)
	if keyword_set(separator) eq 0 then separator = ','
	;读第一行，确定列数
	header = strsplit(lines[0], separator, /extract, /preserve_null)
	cols = n_elements(header)

	;处理只有一行的情况
	if rows le 1 then return, header

	;构造内容列表
	contents = strarr(cols, rows-1)
	;逐行读取文件
	for i=1, rows-1 do begin
		aline = strsplit(lines[i], separator, /extract, /preserve_null)
		n_cols = n_elements(aline)
		min_cols = min([n_cols, cols])
		contents[0:min_cols-1, i-1] = aline[0:min_cols-1]
	endfor
	if keyword_set(nohead) eq 0 then contents=[[header],[contents]]
	return, contents
end

;设置日期列表
pro cw_seldate_panel_setdaylist, baseid, selday=selday
	widget_control, baseid, get_uvalue = idlist
	;获取年份
	year = widget_info(idlist.yearid, /combobox_gettext)
	;获取月份
	month = widget_info(idlist.monthid, /list_select) + 1
	dayslist = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	days = dayslist[month - 1]
	;处理二月
	if month eq 2 then days = fix(julday(3, 1, year) - julday(2, 1, year))

	;获取该月第一天是星期几
	week = julday(month, 1, year) mod 7
	total_days = week + days
	;该月的周数
	total_week = ceil(total_days / 7.)
	;日期列表
	dayarray = strarr(7, total_week)
	;设置第一周
	index = 1
	for j=week, 6 do begin
		dayarray[j, 0] = strtrim(index, 2)
		index ++
	endfor

	;设置其它周
	for i=1, total_week-1 do begin
		for j=0, 6 do begin
			dayarray[j, i] = strtrim(index, 2)
			index ++
			if index gt days then break
		endfor
	endfor
	widget_control, idlist.dayid, set_value = dayarray, ysize=total_week

	;选择设置对日的选择
	if keyword_set(selday) eq 0 then selday = 1
	col = (week + selday - 1) mod 7
	row = (week + selday - 1) / 7
	widget_control, idlist.dayid, set_table_select=[col, row, col, row]
	widget_control, idlist.dayid, set_table_view=[0,0]

	;设置显示内容
	widget_control, idlist.date, set_value = year + '年' + strtrim(month, 2) $
		+ '月' + strtrim(selday, 2) + '日', set_uvalue = {year:fix(year), $
			month:fix(month), day:fix(selday)}
end

;日期被选择的事件
pro cw_seldate_panel_dayevent, event
	if 'WIDGET_TABLE_CELL_SEL' ne tag_names(event, /structure_name) then return
	if event.sel_top eq -1 then return
	if event.sel_top ne event.sel_bottom then return
	if event.sel_left ne event.sel_right then return
	;
	widget_control, event.id, get_uvalue = datebase
	widget_control, datebase, get_uvalue = idlist
	widget_control, idlist.dayid, get_value = daysarray
	day = daysarray[event.sel_left, event.sel_top]
	if day eq '' then return
	;更新日期显示
	widget_control, idlist.date, get_uvalue = date
	date.day = day
	widget_control, idlist.date, set_uvalue = date, set_value = strtrim(date.year, 2) $
		+ '年' + strtrim(date.month, 2) + '月' + strtrim(date.day, 2) + '日'
end

;年份或月份被选择的事件
pro cw_seldate_panel_yearmonthevent, event
	widget_control, event.id, get_uvalue = datebase
	cw_seldate_panel_setdaylist, datebase
end

;年月日的选择功能框
function cw_seldate_panel, topbase
	base = widget_base(topbase, /col, space=0)
	;年份值列表，从当前年向前列出20年
	caldat, systime(/jul), month, day, year
	ylist = rotate(strtrim(indgen(20) + year - 19, 2), 2)
	base1 = widget_base(base, /row)
		lab = widget_label(base1, value='年：')
		yearid = widget_combobox(base1, value=ylist, uvalue = base, $
			event_pro='cw_seldate_panel_yearmonthevent')
		lab = widget_label(base1, value='   当前时间：')
		datetext = widget_text(base1, xsize=15)
	;列宽度
	col_width = intarr(7)
	col_width[*] = 25

	base1 = widget_base(base, /row)
		monthid = widget_list(base1, xsize=6, ysize=12, value=['一月', '二月', '三月', $
			'四月', '五月', '六月', '七月', '八月', '九月', '十月', '十一月', '十二月'], $
			uvalue = base, event_pro='cw_seldate_panel_yearmonthevent')
		dayid = widget_table(base1, /no_row_header, scr_xsize=196, scr_ysize=150, xsize=7,$
			column_width=col_width, column_labels=['一','二','三','四','五','六','日'], $
			scroll=1, event_pro='cw_seldate_panel_dayevent', /all_events, uvalue = base)
	widget_control, base, set_uvalue = { date:datetext, yearid:yearid, $
		monthid:monthid, dayid:dayid }
	;设置月份选择
	widget_control, monthid, set_list_select = month - 1
	;设置日期
	cw_seldate_panel_setdaylist, base, selday=day

	return, datetext
end

;默认事件
pro cw_seldate_panel_ui_event, event
end

pro cw_seldate_panel_ui
	base = widget_base(title='日期选择', /col, /tlb_frame_attr, xpad=0, ypad=0, space=0)
	datebase = cw_seldate_panel(base)
	base1 = widget_base(base, /row, /align_right, space=10)
		btn = widget_button(base1, value='确定', xsize=60, ysize=23)
		btn = widget_button(base1, value='取消', xsize=60, ysize=23, event_pro='WidgetClose')
	widget_control, base, /realize
	xmanager, 'cw_seldate_panel_ui', base
end

;--------------------------------------------------
;添加日期面板中默认事件处理程序
pro cw_datebox_select_ok, event
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.date, get_value = date, get_uvalue = dateuv
	widget_control, idlist.topbtn, get_uvalue = dateid
	widget_control, dateid, set_value = date, set_uvalue = dateuv
	widget_control, event.top, /destroy
end

pro cw_datebox_select_event, event
end

;打开时间选择对话框的功能
pro cw_datebox_select, event
	;计算时间窗口在父窗口中的位置
	wgeo = widget_info(event.top, /geometry)
	cx = wgeo.xoffset + wgeo.scr_xsize / 2 - 120
	cy = wgeo.yoffset + wgeo.scr_ysize / 2 - 100

	tlb = widget_base(group_leader=event.top, title='日期选择', /col, /tlb_frame_attr, xpad=0, ypad=0, $
		space=0, xoffset=cx, yoffset=cy)
	datebase = cw_seldate_panel(tlb)
	base1 = widget_base(tlb, /row, /align_right, space=10)
		btn = widget_button(base1, value='确定', xsize=60, ysize=23, event_pro='cw_datebox_select_ok')
		btn = widget_button(base1, value='关闭', xsize=60, ysize=23, event_pro='WidgetClose')
	widget_control, tlb, /realize, set_uvalue = {topbtn:event.id, date:datebase}
	xmanager, 'cw_datebox_select', tlb
end

;时间选择与显示功能
function cw_datebox, topbase, xsize=xsize, title=title
	base = widget_base(topbase, /row, xpad=0, ypad=0, space=5)
	if keyword_set(title) eq 0 then title = '时间：'
	if keyword_set(xsize) eq 0 then xsize = 20
	lab = widget_label(base, value=title)
	date = widget_text(base, xsize=xsize)
	btn = widget_button(base, value='选择日期', xsize=80, ysize=23, event_pro='cw_datebox_select')
	widget_control, btn, set_uvalue = date
	return, date
end

;------------------------------------------------
;打开文件或文件夹的控件

;文件对话框选择事件
pro cw_openfile_select, event
	widget_control, event.id, get_uvalue = idlist
	if idlist.directory eq 0 then begin
		;选择文件
		widget_control, idlist.outdir, get_value = filename
		filename = dialog_pickfile(dialog_parent=event.top, file=filename, title=idlist.title, path=idlist.path, FILTER = idlist.filter)
		if filename eq '' then return
		widget_control, idlist.outdir, set_value = filename
	endif else begin
		;选择目录
		widget_control, idlist.outdir, get_value = dirname
		dirname = dialog_pickfile(dialog_parent=event.top, /directory, title=idlist.title, path=dirname)
		if dirname eq '' then return
		widget_control, idlist.outdir, set_value = dirname
	endelse
end

;控件定义
function cw_openfile, topbase, title=title, value=value, btntitle=btntitle, path=path, $
	directory=directory, xsize=xsize, read=read, write=write, filter=filter
	;处理关键字
	if keyword_set(value) eq 0 then value = ''
	if keyword_set(xsize) eq 0 then xsize = 40
	if keyword_set(filter) eq 0 then filter=['*.*']
	if keyword_set(path) eq 0 then path=''
	;选择方式
	if keyword_set(directory) eq 0 then begin
		;文件选择
		if keyword_set(title) eq 0 then title='文件设置'
		if keyword_set(btntitle) eq 0 then btntitle='选择文件'
	endif else begin
		;目录选择
		if keyword_set(title) eq 0 then title='目录设置'
		if keyword_set(btntitle) eq 0 then btntitle='选择目录'
	endelse

	base = widget_base(topbase, /row, space=3, /align_left)
		lab = widget_label(base, value=title)
		outdir = widget_text(base, xsize=xsize, /editable, value=value)
		btn = widget_button(base, value='images/open.bmp', xsize=20, ysize=20, event_pro='cw_openfile_select', /bitmap)
	widget_control, btn, set_uvalue = { outdir : outdir, directory : keyword_set(directory), $
		read:keyword_set(read), write:keyword_set(write), title:title, path:path, filter:filter}
	return, outdir
end

;返回文件的后缀名，无法处理则返回空串
function file_extname, filename
	strlist = strsplit(filename, '.', /extract)
	n = n_elements(strlist)
	if n le 1 then return, '' else return, strlowcase(strlist[n-1])
end

;返回文件的无后缀名的格式
function file_removeext, filename
	strlist = strsplit(filename, '.', /extract)
	return, strlist[0]
end

;测试一个shape文件有没有与它同目录同名的prj文件
;返回值意义：-1 该文件不是.shp文件, 0 该shape文件没有prj文件，该shape文件有prj文件
function testshapeprj, filename
	if file_extname(filename) ne 'shp' then return, -1
	bname = file_basename(filename, '.shp')
	dir = file_dirname(filename)
	prjfile = dir + '\' + bname + '.prj'
	;print, prjfile
	return, file_test(prjfile)
end

;获取矢量文件的范围,上下左右各留5%的余量
;输入：矢量文件名
;输出：[左侧坐标(x最小值)，底部坐标(y最小值)，宽度，高度]
;如果发现了prj文件，则直接输出坐标
;如果没有prj文件，则认为是经纬度，需要经过转换
;如果转换有错误，则返回空字符串
function getShapeBounds, filename
	shapeobj = OBJ_NEW('IDLffShape', filename)
	if obj_valid(shapeobj) eq 0 then return, ''
	shapeobj->GetProperty, N_ENTITIES=num_ent, ENTITY_TYPE=ShapeType
	;print, num_ent, ShapeType
	;如果个数为零或类型不是点线多边形，则返回空串
	if num_ent eq 0 or ShapeType ne 1 and ShapeType ne 3 and ShapeType ne 5 then begin
		OBJ_DESTROY, shapeobj
		return, ''
	endif
	xlist = dblarr(num_ent * 2)
	ylist = dblarr(num_ent * 2)
	for j=0, num_ent-1 do begin
		ent = shapeobj->GetEntity(j, /ATTRIBUTES)
		xlist[2*j] = ent.BOUNDS[0]
		xlist[2*j+1] = ent.BOUNDS[4]
		ylist[2*j] = ent.BOUNDS[1]
		ylist[2*j+1] = ent.BOUNDS[5]
		shapeobj->DestroyEntity, ent
	endfor
	OBJ_DESTROY, shapeobj
	;print, xlist
	;print, 'dd'
	;print, ylist
	;判断是否是经纬度的值
	if testshapeprj(filename) eq 0 then begin
		;度转成弧度
		rad_xlist = xlist * 3.1415926d / 180d
		rad_ylist = ylist * 3.1415926d / 180d
		for j=0, num_ent-1 do begin
			xy = Albers110_Project(rad_ylist[2*j], rad_xlist[2*j])
			xlist[2*j] = xy.x
			ylist[2*j] = xy.y
			xy = Albers110_Project(rad_ylist[2*j+1], rad_xlist[2*j+1])
			xlist[2*j+1] = xy.x
			ylist[2*j+1] = xy.y
		endfor
	endif
	minx = 0.99 * min(xlist, max=maxx)
	maxx = 1.01 * maxx
	miny = 0.99 * min(ylist, max=maxy)
	maxy = 1.01 * maxy
	return, [minx, miny, maxx-minx, maxy-miny]
	;
end

;判断两个geo结构体变量是否完全一致
;一致返回1，否则返回0
function sameGeoInfor, geo1, geo2
;	Catch, theError
;	IF theError NE 0 THEN BEGIN
;	  Catch, /Cancel
;	  ok = Error_Message()
;	  RETURN,0
;	ENDIF
;
;	;先比较字段总数
;	n_tag1 = n_tags(geo1)
;	n_tag2 = n_tags(geo2)
;	if(n_tag1 ne n_tag2) then begin
;		return,0
;	end
;	;然后逐个字段比较
;	for i=0, n_tag1-1, 1 do begin
;		if(n_elements(geo1.(i)) ne n_elements(geo2.(i))) then begin
;			return,0
;		end
;		NotEquals = geo1.(i) ne geo2.(i)
;		if(total(NotEquals) ne 0) then begin
;			;IMAGINE GeoTIFF Support 字段除外
;			if(STRMID(string(geo1.(i)),0,24) eq STRMID(string(geo2.(i)),0,24)) then begin
;				continue
;			end
;			return,0
;		end
;	endfor
	return,1
end

function file_columns, filename, Pattern=Pattern
	if(N_PARAMS() ne 1) then begin
		return,-2
	endif
	if(keyword_set(Pattern) ne 1) then begin
		Pattern = ','
	endif
	if(0 eq FILE_TEST(filename)) then begin
		return,-1
	endif
	firstline = ''
	openr,lun,filename,/get_lun
    readf,lun,firstline
    close,lun
    free_lun,lun
    temp=strsplit(firstline,Pattern,/extract,count=num)
    return,num
end


;***********************************************************************************************
;以下为MODIS处理程序用到的通用模块
;***********************************************************************************************

;通用函数库
;0 批处理操作
;1 交互操作
function getVersion
	return, 0
end

;窗口关闭功能,需确认
pro WidgetCloseConfirm, event
	msg = dialog_message('是否退出当前计算程序？', title='退出确认', /ques)
	if msg eq 'No' then return
	widget_control, event.top, /destroy
end

;窗口关闭功能，无需确认
pro WidgetClose, event
	widget_control, event.top, /destroy
end

;通用的窗口默认事件
pro WidgetDefault_event, event
end

;获取一个widget的value
function widget_value, widgetid
	widget_control, widgetid ,get_value = wv
	return, wv
end

;获取一个widget的uvalue
function widget_uvalue, widgetid
	widget_control, widgetid ,get_uvalue = wuv
	return, wuv
end

;返回一个数组的最后一个值
function LastValue, array
	return, array[n_elements(array) - 1]
end

;把逗号分隔的值转成数组
function CSVToArray, csv
	items = strsplit(csv, ',', /extract, /preserve_null)
	return, items
end

;判断一个变量是否是结构体
function valid_set, theValue
	if size(theValue, /type) eq 8 then return, 1 else return, 0
end

;输入一个文件路径，如果文件存在，返回该文件路径，如果不存在，返回空串
function ExistFile, thefile
	return, file_test(thefile) eq 1 ? thefile : ''
end

;保证使目录的最后一个字符是\
function formatDir, dir
	nlen = strlen(dir)
	if nlen eq 0 then return, dir
	if strmid(dir, nlen-1) ne '\' then dir += '\'
	return, dir
end

;获取默认的工作目录
function getDefaultFolder
	file = 'config\folder.ini'
	if file_test(file) eq 0 then return, ''
	openr, lun, file, /get_lun
	n = file_lines(file)
	lines = strarr(n)
	readf, lun, lines
	free_lun, lun

	;
	if file_test(lines[0], /directory) eq 0 then return, ''
	return, FormatDir(lines[0])
end

;如果指定目录存在,则使用指定的目录,否则使用默认目录
function getDefaultFolder2, folder
	if file_test(folder, /directory) eq 1 then return, folder else return, getDefaultFolder()
end

;使一个widget相对于其父窗口居中显示
pro parent_center, tbase, pbase
	geo_p = widget_info(pbase, /geometry)
	geo_t = widget_info(tbase, /geometry)
	;获取父窗口的中心点
	cen_x = geo_p.xoffset + geo_p.xsize / 2
	cen_y = geo_p.yoffset + geo_p.ysize / 2
	;计算子窗口的offset
	xoff = cen_x - geo_t.xsize / 2
	yoff = cen_y - geo_t.ysize / 2
	widget_control, tbase, xoffset=xoff, yoffset=yoff
end

;使一个窗口居中显示
pro window_center, base
	geo = widget_info(base, /geometry)
	device, get_screen_size = ssize
	xoff = (ssize[0] - geo.xsize) / 2
	yoff = (ssize[1] - geo.ysize) / 2
	widget_control, base, xoffset=xoff, yoffset=yoff
end

;判断一个字符串是不是由数字组成
function isNumber, str
	numlist = strtrim(indgen(10), 2)
	nloops = strlen(str)
	for i=0, nloops-1 do begin
		index = where(strmid(str, i, 1) eq numlist)
		if index[0] eq -1 then return, 0
	endfor
	return, 1
end

;在一维数组的某个位置插入一个新元素
;如果 index 小于0则返回原组,如果大于array的长度,则放在最后一个
function ArrayPut, array, index, element
	;小于等于0放在最前面
	if index le 0 then return, [element, array]

	;大于数组长度,放在最后面
	a_len = n_elements(array)
	if index ge a_len then return, [array, element]

	;位于数据中间,则原位置以后的元素向后挪一位
	return, [array[0:index-1], element, array[index:*]]
end

;去除掉数组中某一个位置的值
function ArrayRemove, array, index
	;数组长度为1、元素索引小于零或大于数组个数, 返回原数组
	a_len = n_elements(array)
	if a_len eq 1 or index lt 0 or index ge a_len then return, array

	;位于数据首部
	if index eq 0 then return, array[1:*]

	;位于数据尾部
	if index eq a_len-1 then return, array[0:a_len-2]

	;位于数据中间
	return, [array[0:index-1], array[index+1:*]]
end

;计算某年某月的天数(最后一天的日期)
function DayofMonth, year, month
	year = long(year)
	month = long(month)
	if month eq 12 then begin
		nextmonth = 1
		nextyear = year + 1
	endif else begin
		nextmonth = month + 1
		nextyear = year
	endelse
	;返回值固定为int型
	return, fix(julday(nextmonth, 1, nextyear) - julday(month, 1, year))
end

;把当前日期转换为八位的日期或14位的时间字符串
function GetStringOfNow, time=time, date=date
	caldat, julday(), mon, day, year, hour, minute, second
	datestr = string(year, format='(I04)') + string(mon, format='(I02)') $
		+ string(day, format='(I02)')
	timestr = string(hour, format='(I02)') $
		+ string(minute, format='(I02)') + string(second, format='(I02)')
	if keyword_set(date) then return, datestr
	if keyword_set(time) then return, timestr

	return, datestr + timestr
end

;把形如yyyy-mm-dd或yyymmdd的字符串格式的日期转换成数字日期或儒略日
function GetDateOfString, input, jul = jul
	in_len = strlen(input)

	if in_len eq 10 then begin
		year = strmid(input, 0, 4)
		month = strmid(input, 5, 2)
		day = strmid(input, 8, 2)
	endif else if in_len eq 8 then begin
		year = strmid(input, 0, 4)
		month = strmid(input, 4, 2)
		day = strmid(input, 6, 2)
	endif else return, 0

	if keyword_set(jul) eq 0 then begin
		return, {year:year, month:month, day:day}
	endif else begin
		return, julday(month, day, year)
	endelse
end

;把日期转换成为八位的数字字符串
function TransDateToString, Year, Month, Day
    Yearstr = string(fix(year), format='(I4)')
    Monthstr = string(fix(month), format='(I02)')
    Daystr = string(fix(day), format='(I02)')

    return, strjoin([Yearstr, Monthstr, Daystr])
end

;把日期转换成为'-'分隔的格式
function TransDateToEnglish, Year, Month, Day
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    datestr = Yearstr + '-' + Monthstr + '-' + Daystr
    return, datestr
end

;把日期转换成为'_'分隔的格式
function getDateWithUL, Year, Month, Day
    Yearstr = string(year, format='(I4)')
    Monthstr = string(month, format='(I02)')
    Daystr = string(day, format='(I02)')
    datestr = Yearstr + '_' + Monthstr + '_' + Daystr
    return, datestr
end

;把-隔开的日期转换成8位标准格式
function FormateDate, date
	dstr = strsplit(date, '-', /extract, /preserve_null)
	if n_elements(dstr) lt 3 then return, date
	return, TransDateToString(dstr[0], dstr[1], dstr[2])
end

;把儒略日格式的日期转换成为八位的数字字符串
function TransJulToString, jd
	caldat, jd, month, day, year
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    if month lt 10 then Monthstr = '0' + Monthstr
    if day lt 10 then daystr = '0' + Daystr
    datestr = Yearstr + Monthstr + Daystr
    return, datestr
end

;把日期转换成为yyyy年mm月dd日的格式
function TransDateToChinese, Year, Month, Day
    Yearstr = strtrim(string(year), 2)
    Monthstr = strtrim(string(month), 2)
    Daystr = strtrim(string(day), 2)
    datestr = Yearstr + '年' + Monthstr +  '月' + Daystr + '日'
    return, datestr
end

;把YYYYMMDD或YYYYMMTT格式的日期转换成为yyyy年mm月dd日或旬的格式
function TransStringToChinese, datestr, tenday=tenday

    Yearstr = strtrim(fix(strmid(datestr, 0, 4)), 2)
    Monthstr = strtrim(fix(strmid(datestr, 4, 2)), 2)
    Daystr = strtrim(fix(strmid(datestr, 6, 2)), 2)
    ;如果是日期
    if keyword_set(tenday) eq 0 then begin
    	dstr = Yearstr + '年' + Monthstr +  '月' + Daystr + '日'
    	return, dstr
    endif
    ;旬的情况
    tlist = ['上旬', '中旬', '下旬']
	dstr = Yearstr + '年' + Monthstr +  '月' + tlist[fix(Daystr) - 1]
	return, dstr
end

;判断一个字符串是不是合法的8位格式的日期(YYYYMMDD)
function isDate, str
	;判断是否是8个字符长
	if strlen(str) ne 8 then return, 0

	;判断是否全是数字组成
	if isNumber(str) eq 0 then return, 0

	;判断它是否是合法的时间
	if TransJulToString(GetDateOfString(str, /jul)) ne str then return, 0

	return, 1
end

;输出日志功能
;写log日志,默认存于文件tempdata\et_yyyymmmdd.log中
pro writetolog, strContent
	;参数不合法则退出
	if size(strContent, /type) ne 7 then return
	;检查文件
	caldat, systime(/j), month, day, year, hour, minute, second
	;生成时分秒的字符串
	hmslist = string([hour, minute, second], format='(I02)')
	strhms = '[' + hmslist[0] + ':' + hmslist[1] + ':' + hmslist[2] + '] '
	filename = '.\text\' + TransDateToString(year, month, day) + '.txt'
	openw, lun, filename, /append, /get_lun
	lines = n_elements(strContent)
	for i=0, lines - 1 do begin
		if strContent[i] eq '' then printf, lun, ' ' else begin
			printf, lun, strhms + strContent[i]
		endelse
	endfor
	close, lun
	free_lun, lun
end

;输出错误,可以选择弹出对话框或打印到日志文件
pro showMsg, msg, title=title, win=win, log=log, noprint=noprint
	;打印到屏幕
	if keyword_set(noprint) eq 0 then print, msg

	;显示对话框
	if keyword_set(win) eq 1 then m = dialog_message(msg, title=title, /info)

	;写到日志文件
	if keyword_set(log) eq 1 then writetolog, msg
end

;工作日志功能
pro worklog, strContent, init=init
	;参数不合法则退出
	if size(strContent, /type) ne 7 then return
	;检查文件
	filename = '.\text\worklog.txt'
	openw, lun, filename, append=~keyword_set(init), /get_lun
	lines = n_elements(strContent)
	for i=0, lines - 1 do begin
		if strContent[i] eq '' then printf, lun, ' ' else begin
			printf, lun, strContent[i]
		endelse
	endfor
	close, lun
	free_lun, lun
	;
	showmsg, strContent, /log
end

;年月日选择控件
function cw_seldate_get_value, id
	widget_control, id, get_uvalue = uv

	widget_control, uv.year, get_value = yearlist
	index = widget_info(uv.year, /droplist_select)
	year = fix(yearlist[index])

	month = fix(widget_info(uv.month, /combobox_gettext))
	day = fix(widget_info(uv.day, /combobox_gettext))

	return, { year:year, month:month, day:day }
end

;;ymdlist 是八位的表示时间的字符串数组
;function cw_seldate, topbase, ymdlist
;	;
;	datebase = widget_base(topbase, /row, func_get_value='cw_seldate_get_value')
;
;	year = widget_droplist(datebase, xsize=50, value=strmid(ymdlist, 0, 4))
;	lab = widget_label(datebase, value='年')
;	month = widget_combobox(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
;	lab = widget_label(datebase, value='月')
;	day = widget_combobox(datebase, xsize=40, value=strtrim(indgen(31)+1, 2))
;	lab = widget_label(datebase, value='日')
;	widget_control, datebase, set_uvalue = {year:year, month:month, day:day}
;	return, datebase
;end

;获取年月选择控件的值
;function cw_selmonth_getValue, id
;	widget_control, id, get_uvalue = uv
;	widget_control, uv.year, get_value = yearlist
;	index = widget_info(uv.year, /droplist_select)
;	year = yearlist[index]
;
;	widget_control, uv.month, get_value = monthlist
;	index = widget_info(uv.month, /droplist_select)
;	month = monthlist[index]
;	return, { year : long(year), month : long(month) }
;end

;年月选择控件
;ymlist 是六位的表示时间的字符串数组
;function cw_selmonth, topbase, ymlist
;	;
;	datebase = widget_base(topbase, /row, func_get_value='cw_selmonth_getValue')
;
;	year = widget_droplist(datebase, xsize=50, value=strmid(ymlist, 0, 4))
;	lab = widget_label(datebase, value='年')
;	month = widget_droplist(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
;	lab = widget_label(datebase, value='月')
;	widget_control, datebase, set_uvalue = {year:year, month:month}
;	return, datebase
;end

;从文本文件中读取字符串
;function read_string, filename
;	if file_test(filename) eq 0 then return, ''
;	nlines = file_lines(filename)
;	if nlines eq 0 then return, ''
;	flines = strarr(nlines)
;	openr, lun, filename, /get_lun
;	readf, lun, flines
;	close, lun
;	free_lun, lun
;	return, flines
;end

;把字符串写入文本文件之中
;pro write_string, filename, flines
;	openw, lun, filename, /get_lun
;	nlines = n_elements(flines)
;	for i=0, nlines-1 do begin
;		printf, lun, flines[i]
;	endfor
;	close, lun
;	free_lun, lun
;end

;读取一个csv(逗号分隔)文件
;header返回第一行
;separator为指定的分隔符号,默认为逗号
;返回值为n X m的字符串组,其中列数由第一行的列数确定
;如果nohead=1，则首行不返回

;function readCSV, file, header=header, seperator=separator, nohead=nohead
;	if file_test(file) eq 0 then return, ''
;	lines = read_string(file)
;	rows = n_elements(lines)
;	if keyword_set(separator) eq 0 then separator = ','
;	;读第一行，确定列数
;	header = strsplit(lines[0], separator, /extract, /preserve_null)
;	cols = n_elements(header)
;
;	;处理只有一行的情况
;	if rows le 1 then return, header
;
;	;构造内容列表
;	contents = strarr(cols, rows-1)
;	;逐行读取文件
;	for i=1, rows-1 do begin
;		aline = strsplit(lines[i], separator, /extract, /preserve_null)
;		n_cols = n_elements(aline)
;		min_cols = min([n_cols, cols])
;		contents[0:min_cols-1, i-1] = aline[0:min_cols-1]
;	endfor
;	if keyword_set(nohead) eq 0 then contents=[[header],[contents]]
;	return, contents
;end

;设置日期列表
;pro cw_seldate_panel_setdaylist, baseid, selday=selday
;	widget_control, baseid, get_uvalue = idlist
;	;获取年份
;	year = widget_info(idlist.yearid, /combobox_gettext)
;	;获取月份
;	month = widget_info(idlist.monthid, /list_select) + 1
;	dayslist = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
;	days = dayslist[month - 1]
;	;处理二月
;	if month eq 2 then days = fix(julday(3, 1, year) - julday(2, 1, year))
;
;	;获取该月第一天是星期几
;	week = julday(month, 1, year) mod 7
;	total_days = week + days
;	;该月的周数
;	total_week = ceil(total_days / 7.)
;	;日期列表
;	dayarray = strarr(7, total_week)
;	;设置第一周
;	index = 1
;	for j=week, 6 do begin
;		dayarray[j, 0] = strtrim(index, 2)
;		index ++
;	endfor
;
;	;设置其它周
;	for i=1, total_week-1 do begin
;		for j=0, 6 do begin
;			dayarray[j, i] = strtrim(index, 2)
;			index ++
;			if index gt days then break
;		endfor
;	endfor
;	widget_control, idlist.dayid, set_value = dayarray, ysize=total_week
;
;	;选择设置对日的选择
;	if keyword_set(selday) eq 0 then selday = 1
;	col = (week + selday - 1) mod 7
;	row = (week + selday - 1) / 7
;	widget_control, idlist.dayid, set_table_select=[col, row, col, row]
;	widget_control, idlist.dayid, set_table_view=[0,0]
;
;	;设置显示内容
;	datestr = getversion() eq 0 ? ['年', '月', '日'] : ['-', '-', '']
;	widget_control, idlist.date, set_value = year + datestr[0] + strtrim(month, 2) $
;		+ datestr[1] + strtrim(selday, 2) + datestr[2], set_uvalue = {year:fix(year), $
;			month:fix(month), day:fix(selday)}
;end

;日期被选择的事件
;pro cw_seldate_panel_dayevent, event
;	if 'WIDGET_TABLE_CELL_SEL' ne tag_names(event, /structure_name) then return
;	if event.sel_top eq -1 then return
;	if event.sel_top ne event.sel_bottom then return
;	if event.sel_left ne event.sel_right then return
;	;
;	widget_control, event.id, get_uvalue = datebase
;	widget_control, datebase, get_uvalue = idlist
;	widget_control, idlist.dayid, get_value = daysarray
;	day = daysarray[event.sel_left, event.sel_top]
;	if day eq '' then return
;	;更新日期显示
;	widget_control, idlist.date, get_uvalue = date
;	date.day = day
;
;	datestr = getversion() eq 0 ? ['年', '月', '日'] : ['-', '-', '']
;
;	widget_control, idlist.date, set_uvalue = date, set_value = strtrim(date.year, 2) $
;		+ datestr[0] + strtrim(date.month, 2) + datestr[1] + strtrim(date.day, 2) + datestr[2]
;end

;年份或月份被选择的事件
;pro cw_seldate_panel_yearmonthevent, event
;	widget_control, event.id, get_uvalue = datebase
;	cw_seldate_panel_setdaylist, datebase
;end

;年月日的选择功能框
function cw_seldate_panel, topbase, thedate
	base = widget_base(topbase, /col, space=0)
	;年份值列表，从1980年开始
	caldat, systime(/jul), month, day, year
	ylist = rotate(strtrim(indgen(year - 1979) + 1980, 2), 2)

	;如果已经有了date，则使用已有的，否则使用当前日期
	if valid_set(thedate) eq 1 then begin
		year = thedate.year
		year_index = where(strtrim(year, 2) eq ylist)
		if year_index[0] eq -1 then year_index = 0
		month = thedate.month
		day = thedate.day
	endif

	base1 = widget_base(base, /row)
		lab = widget_label(base1, value='选择年份：')
		yearid = widget_combobox(base1, value=ylist, uvalue = base, $
			event_pro='cw_seldate_panel_yearmonthevent')
		widget_control, yearid, set_combobox_select = year_index
		lab = widget_label(base1, value='日期')
		datetext = widget_text(base1, xsize=15)
	;列宽度
	col_width = intarr(7)
	col_width[*] = 25

	base1 = widget_base(base, /row)
		monthid = widget_list(base1, xsize=7, ysize=12, value=['1月','2月','3月','4月',$
			'5月','6月','7月','8月','9月','10月','11月','12月'], $
			uvalue = base, event_pro='cw_seldate_panel_yearmonthevent')
		dayid = widget_table(base1, /no_row_header, scr_xsize=196, scr_ysize=150, xsize=7,$
			column_width=col_width, column_labels=['一','二','三','四','五','六','日'], $
			scroll=1, event_pro='cw_seldate_panel_dayevent', /all_events, uvalue = base)
	widget_control, base, set_uvalue = { date:datetext, yearid:yearid, $
		monthid:monthid, dayid:dayid }
	;设置月份选择
	widget_control, monthid, set_list_select = month - 1
	;设置日期
	cw_seldate_panel_setdaylist, base, selday=day

	return, datetext
end

;默认事件
;pro cw_seldate_panel_ui_event, event
;end

;pro cw_seldate_panel_ui
;	base = widget_base(title='日期选择', /col, /tlb_frame_attr, xpad=0, ypad=0, space=0)
;	datebase = cw_seldate_panel(base)
;	base1 = widget_base(base, /row, /align_right, space=10)
;		btn = widget_button(base1, value='确定', xsize=60, ysize=23)
;		btn = widget_button(base1, value='取消', xsize=60, ysize=23, event_pro='WidgetClose')
;	widget_control, base, /realize
;	xmanager, 'cw_seldate_panel_ui', base
;end

;--------------------------------------------------
;添加日期面板中默认事件处理程序
;pro cw_datebox_select_ok, event
;	widget_control, event.top, get_uvalue = idlist
;	widget_control, idlist.date, get_value = date, get_uvalue = dateuv
;	widget_control, idlist.topbtn, get_uvalue = dateid
;	widget_control, dateid, set_value = date, set_uvalue = dateuv
;	widget_control, event.top, /destroy
;end

;pro cw_datebox_select_event, event
;end

;打开时间选择对话框的功能
;pro cw_datebox_select, event
;	;计算时间窗口在父窗口中的位置
;	wgeo = widget_info(event.top, /geometry)
;	cx = wgeo.xoffset + wgeo.scr_xsize / 2 - 120
;	cy = wgeo.yoffset + wgeo.scr_ysize / 2 - 100
;
;	;获取上一次设置的时间，如果没有设置，则为空串
;	dateid = widget_uvalue(event.id)
;	date = widget_uvalue(dateid)
;
;	tlb = widget_base(group_leader=event.top, title=getstring(71), /col, /tlb_frame_attr, xpad=0, ypad=0, $
;		space=0, xoffset=cx, yoffset=cy)
;	datebase = cw_seldate_panel(tlb, date)
;	base1 = widget_base(tlb, /row, /align_right, space=10)
;		btn = widget_button(base1, value=getstring(1), xsize=60, ysize=23, event_pro='cw_datebox_select_ok')
;		btn = widget_button(base1, value=getstring(2), xsize=60, ysize=23, event_pro='WidgetClose')
;	widget_control, tlb, /realize, set_uvalue = {topbtn:event.id, date:datebase}
;	;
;	xmanager, 'cw_datebox_select', tlb
;end

;时间选择与显示功能
;function cw_datebox, topbase, xsize=xsize, title=title
;	base = widget_base(topbase, /row, xpad=0, ypad=0, space=5)
;	if keyword_set(title) eq 0 then title = getstring(102)
;	if keyword_set(xsize) eq 0 then xsize = 20
;	lab = widget_label(base, value=title)
;	date = widget_text(base, xsize=xsize, uvalue='')
;	btn = widget_button(base, value=getstring(103), xsize=80, ysize=23, event_pro='cw_datebox_select')
;	widget_control, btn, set_uvalue = date
;	return, date
;end

;------------------------------------------------
;打开文件或文件夹的控件

;文件对话框选择事件
;pro cw_openfile_select, event
;	widget_control, event.id, get_uvalue = idlist
;	if idlist.directory eq 0 then begin
;		;选择文件
;		widget_control, idlist.outdir, get_value = filename
;		filename = dialog_pickfile(dialog_parent=event.top, file=filename, $
;			filter=idlist.filter, path=getdefaultfolder())
;		if filename eq '' then return
;		widget_control, idlist.outdir, set_value = filename
;	endif else begin
;		;选择目录
;		widget_control, idlist.outdir, get_value = dirname
;		if file_test(dirname, /directory) eq 0 then dirname = getdefaultfolder()
;		dirname = dialog_pickfile(dialog_parent=event.top, /directory, path=dirname)
;		if dirname eq '' then return
;		widget_control, idlist.outdir, set_value = dirname
;	endelse
;end

;控件定义
;function cw_openfile, topbase, title=title, value=value, btntitle=btntitle, $
;	directory=directory, xsize=xsize, read=read, write=write, filter=filter
;	;处理关键字
;	if keyword_set(value) eq 0 then value = ''
;	if keyword_set(xsize) eq 0 then xsize = 40
;	;选择方式
;	vindex = getVersion()
;	if keyword_set(directory) eq 0 then begin
;		;文件选择
;		if keyword_set(title) eq 0 then title = (['文件设置：', 'Set file:'])[vindex]
;		if keyword_set(btntitle) eq 0 then btntitle = (['选择文件', 'Select'])[vindex]
;	endif else begin
;		;目录选择
;		if keyword_set(title) eq 0 then title = (['目录设置：', 'Set folder:'])[vindex]
;		if keyword_set(btntitle) eq 0 then btntitle = (['选择目录', 'Select'])[vindex]
;	endelse
;
;	;文件过滤器
;	if keyword_set(filter) eq 0 then filter=''
;
;	base = widget_base(topbase, /row, space=3, /align_left)
;		lab = widget_label(base, value=title)
;		outdir = widget_text(base, xsize=xsize, /editable, value=value)
;		btn = widget_button(base, value=btntitle, xsize=80, ysize=23, event_pro='cw_openfile_select')
;	widget_control, btn, set_uvalue = { outdir : outdir, directory : keyword_set(directory), $
;		read:keyword_set(read), write:keyword_set(write), filter:filter}
;	return, outdir
;end

;比较两个tiff文件的info的尺寸是否一致
function CheckDims, info1, info2
	if info1.dimensions[0] ne info2.dimensions[0] then return, 0
	if info1.dimensions[1] ne info2.dimensions[1] then return, 0
	return, 1
end

;比较输出文件与输入文件的时间关系
function checkFilesTimes, inputfile, outputfile
	;输入文件不存在，返回0，不可计算
	if file_test(inputfile) eq 0 then return, 0

	;输出文件不存在，返回1，可计算
	if file_test(outputfile) eq 0 then return, 1

	;读取文件时间
	fi1 = file_info(inputfile)
	fi2 = file_info(outputfile)
	;输出文件新，返回0，不必计算；输出文件旧，返回1，可计算
	if fi1.mtime lt fi2.mtime then return, 0 else return, 1
end

;鼠标自定义光标
pro mousecursor
	a = read_image('image\hand2.bmp')
	outs = ''
	for i=15, 0,-1 do begin
		b = ''
		for j=0, 15 do b += strtrim(1-a[0, j, i]/255, 2)
		;
		c = strmid(b, 0, 8)
		d = strmid(b, 8, 8)
		b = d + c
		e = b
		for j=0, 15 do begin
			strput, e, strmid(b, 15-j, 1), j
		endfor
		k = 0u
		for j=0,15 do begin
			if strmid(e, j, 1) eq '1' then k += 2^(15-j)
		endfor
		outs += strtrim(k, 2) + ', '
	endfor
	;
	print, outs
end

;计算精度
;value : 精确值
;error : 测量值与精确值之差
;返回-表示精确值为0
;返回0~100之间表示精度
;返回0表示差值大于测量值

function getPrecision, value, error
	;精度
	if float(value) eq 0 then return, '-'
	prec = abs(float(error) / float(value))
	re = prec ge 1 ? '0' : string(100*(1-prec), format='(d4.1)')
	return, re
end

function getPrecision1, v, e
	vv = float(v)
	ee = float(e)
	er = abs(vv - ee) / 2.
	mx = max([abs(vv), abs(ee)])
	;print, er, mx
	return, 100 - er * 100 /mx
end

;字符串转数据，不合法则报错
function str2value, str
	vindex = getversion()
	on_ioerror, label
	return, {error:0, value:double(str)}
	label:
		if vindex eq 0 then showmsg, '输入的字符串不是合法的数字格式！', title='用户输入错误', /win, /log
		if vindex eq 1 then showmsg, 'The input is not a valid number', title='Input Error', /win, /log
		return, {error:1}
end

;判断项是否位于数组中
function inList, item, list
	index = where(item eq list, cnt)
	return, cnt
end

;返回系统路径
function getsyspath
	;读取ini文件
	str1 = read_string('data\config.ini')
	;逐个解译并形成结构体
	n = n_elements(str1)
	str2 = strarr(2*n)
	struct = {name:'config'}
	for i=0, n-1 do begin
		pos = strpos(str1[i], '=')
		if pos[0] eq -1 then continue
		str2[2*i] = strmid(str1[i], 0, pos)
		if strlowcase(str2[2*i]) ne 'dbconnection' then begin
			str2[2*i + 1] = FormatDir(strmid(str1[i], pos+1))
		endif else begin
			str2[2*i + 1] = strmid(str1[i], pos+1)
		endelse
		struct = create_struct(struct, str2[2*i],str2[2*i+1])
	endfor

	return, struct
end

;测试java是否存在
;0 存在
;-1 不存在
function testJava
	spawn, 'java -version', res, eres
	jp = strpos(eres[0], 'java version')
	return, jp[0]
end

;删除指定目录之下的内容
pro clearFolder, folder, type
	;debug模式下,不删除文件
	if file_test('data\debug') eq 1 then return

	if keyword_set(type) eq 1 then begin
		files = file_search(folder + '\' + type)
	endif else begin
		files = file_search(folder + '\*')
	endelse
	;
	if files[0] eq '' then return
	n = n_elements(files)
	for i=0,n-1 do file_delete, files[i], /quiet
end

;五省市的坐标信息
pro getProvienceInfo, pname, geo=geo, info=info
	case pname of
	'anhui' : begin
		geo = {MODELPIXELSCALETAG:[1000., 1000.], MODELTIEPOINTTAG:[0.,0.,0.,4449000.0 , 3713000.0,0.]}
		info = {dimensions:[457,577]}
	end
	'chongqing' : begin
		geo = {MODELPIXELSCALETAG:[1000., 1000.], MODELTIEPOINTTAG:[0.,0.,0.,3548000.0,3422000.0,0.]}
		info = {dimensions:[470,455]}
	end
	'hebei' : begin
		geo = {MODELPIXELSCALETAG:[1000., 1000.], MODELTIEPOINTTAG:[0.,0.,0.,4301000.0,4619000.0,0.]}
			info = {dimensions:[527,752]}
	end
	'heilongjiang' : begin
		geo = {MODELPIXELSCALETAG:[1000., 1000.], MODELTIEPOINTTAG:[0.,0.,0.,4784000.0,5867000.0,0.]}
			info = {dimensions:[1065,1027]}
	end
	'jilin' : begin
		geo = {MODELPIXELSCALETAG:[1000., 1000.], MODELTIEPOINTTAG:[0.,0.,0.,4732000.0,4772000.0,0.]}
			info = {dimensions:[569,564]}
	end
	endcase

;	plist = ['anhui', 'chongqing', 'hebei', 'heilongjiang', 'jilin']
;	for i=0,4 do begin
;		print, query_tiff('data\province\' + plist[i] + '.tif', info, geotiff=geo)
;		print, plist[i]
;		print, geo.MODELPIXELSCALETAG[0:1]
;		print, geo.MODELTIEPOINTTAG[3:4]
;		print, info.dimensions[0:1]
;	endfor
;	return, 0
end

;把julday转换成为yyyy-mm-dd hh-mm-ss的格式
function getdbtimestring, jul
	caldat, jul, m1, d1, y1, h1, mm1, s1
	sTime = string(y1, format='(I04)') + '-' $
		+ strjoin(string([m1, d1], format='(I02)'), '-') $
		+ ' ' + strjoin(string([h1, mm1, d1], format='(I02)'), '-')
	return, sTime
end

;向数据库中写入日志
pro LogToDb, top, startTime, Status, errorReason
	return
	;读运算规则表
	idlist = widget_uvalue(top)
	if obj_valid(idlist.odbc) eq 0 then begin
		showmsg, '无法读取规则：数据库没有连接！', /log
		return
	endif

	;时间
	sTime = getdbtimestring(startTime)
	eTime = getdbtimestring(systime(/j))

	Status = strtrim(Status, 2)

	;状态判断
	isError = errorReason eq '' ? '0' : '1'

	;清除上一步正确的结果
	sqlstr = 'delete from tbl_state where modelnam=8'
	idlist.odbc->ExecuteSql, sqlstr

	;只取前100个字符
	errorReason = strmid(errorReason, 0, 200)

	sqlstr = 'insert into tbl_state(MODELNAM,STARTTIME,ENDTIME,STATUS,ISERROR,REASON) ' $
		+ ' values(''8'', ''' + sTime + ''',''' + eTime + ''', ''' + Status + ''', ' $
		+ '''' + isError + ''',''' + errorReason + ''')'
	;
	print, sqlstr
	idlist.odbc->ExecuteSql, sqlstr
end

;密码输入框获取数据的事件
function cw_password_getvalue, baseid
	widget_control, baseid, get_uvalue = pwdtxt
	widget_control, pwdtxt, get_uvalue = puvalue
	return, puvalue.char
end

;密码输入框
pro cw_password_event, event
	;help, event, /struc
	evnet_name = size(event, /sname)
	widget_control, event.id, get_uvalue = uv
	if evnet_name eq 'WIDGET_TEXT_CH' then begin
		uv.char = strmid(uv.char, 0, event.offset-1) + string(event.ch) + strmid(uv.char, event.offset-1)
	endif
	if evnet_name eq 'WIDGET_TEXT_DEL' then begin
		uv.char = strmid(uv.char, 0, event.offset) + strmid(uv.char, event.offset + event.length)
	endif
	if evnet_name eq 'WIDGET_TEXT_SEL' then return
	;设置掩码
	mask = ''
	masklen = strlen(uv.char)
	for i=0,masklen-1 do mask = mask + '*'
	widget_control, event.id, set_value = mask, set_uvalue = uv, set_text_select = [event.offset, 0]
end

;密码输入框
function cw_password, parent, xsize = xsize, value = value
	if keyword_set(value) eq 1 then begin
		vlen = strlen(value)
		mask = strarr(vlen)
		mask[*] = '*'
		uvalue = {char : value}
	endif else uvalue = {char : ''}
	;
	base = widget_base(parent, /col, func_get_value = 'cw_password_getvalue', uvalue='', space=0, xpad=0)
	pwdtxt = widget_text(base, xsize=xsize, /all_events, /editable, event_pro='cw_password_event', $
		value=mask, uvalue=uvalue)
	widget_control, base, set_uvalue = pwdtxt
	return, base
end