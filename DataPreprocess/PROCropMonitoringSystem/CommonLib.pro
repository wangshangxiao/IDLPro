;ͨ�ú�����

;ͨ�õĴ���Ĭ���¼�
pro WidgetDefault_event, event
end

;��ȡһ��widget��value
function widget_value, widgetid
	widget_control, widgetid ,get_value = wv
	return, wv
end

;��ȡһ��widget��uvalue
function widget_uvalue, widgetid
	widget_control, widgetid ,get_uvalue = wuv
	return, wuv
end

;���ڹرչ���
pro WidgetClose, event
	confirm = widget_uvalue(event.id)
	if confirm eq 1 then begin
		msg = dialog_message('�Ƿ��˳���ǰ�������', title='�˳�ȷ��', /ques)
		if msg eq 'No' then return
	endif
	widget_control, event.top, /destroy
end

;ͨ�õĹرմ��ڵİ�ť
function close_button, base, confirm=confirm
	return, widget_button(base, value='�ر�', xsize=60, ysize=23, event_pro='WidgetClose', uvalue=keyword_set(confirm))
end

;��һά�����ĳ��λ�ò���һ����Ԫ��
;��� index С��0�򷵻�ԭ��,�������array�ĳ���,��������һ��
function ArrayPut, array, index, element
	;С�ڵ���0������ǰ��
	if index le 0 then return, [element, array]

	;�������鳤��,���������
	a_len = n_elements(array)
	if index ge a_len then return, [array, element]

	;λ�������м�,��ԭλ���Ժ��Ԫ�����Ųһλ
	return, [array[0:index-1], element, array[index:*]]
end

;ȥ����������ĳһ��λ�õ�ֵ
function ArrayRemove, array, index
	;���鳤��Ϊ1��Ԫ������С���������������, ����ԭ����
	a_len = n_elements(array)
	if a_len eq 1 or index lt 0 or index ge a_len then return, array

	;λ�������ײ�
	if index eq 0 then return, array[1:*]

	;λ������β��
	if index eq a_len-1 then return, array[0:a_len-1]

	;λ�������м�
	return, [array[0:index-1], array[index+1:*]]
end

;�ѵ�ǰ����ת��Ϊ��λ�����ڻ�14λ��ʱ���ַ���
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

;������ת����Ϊ��λ������
function TransDateToString, Year, Month, Day
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    if month lt 10 then Monthstr = '0' + Monthstr
    if day lt 10 then daystr = '0' + Daystr
    datestr = Yearstr + Monthstr + Daystr
    return, datestr
end

;������ת����Ϊyyyy��mm��dd�յĸ�ʽ
function TransDateToChinese, Year, Month, Day
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    datestr = Yearstr + '��' + Monthstr +  '��' + Daystr + '��'
    return, datestr
end

;������ѡ��ؼ�
function cw_seldate_get_value, id
	widget_control, id, get_uvalue = uv

	widget_control, uv.year, get_value = yearlist
	index = widget_info(uv.year, /droplist_select)
	year = fix(yearlist[index])

	month = fix(widget_info(uv.month, /combobox_gettext))
	day = fix(widget_info(uv.day, /combobox_gettext))

	return, { year:year, month:month, day:day }
end

;ymdlist �ǰ�λ�ı�ʾʱ����ַ�������
function cw_seldate, topbase, ymdlist
	;
	datebase = widget_base(topbase, /row, func_get_value='cw_seldate_get_value')

	year = widget_droplist(datebase, xsize=50, value=strmid(ymdlist, 0, 4))
	lab = widget_label(datebase, value='��')
	month = widget_combobox(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
	lab = widget_label(datebase, value='��')
	day = widget_combobox(datebase, xsize=40, value=strtrim(indgen(31)+1, 2))
	lab = widget_label(datebase, value='��')
	widget_control, datebase, set_uvalue = {year:year, month:month, day:day}
	return, datebase
end

;��ȡ����ѡ��ؼ���ֵ
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

;����ѡ��ؼ�
;ymlist ����λ�ı�ʾʱ����ַ�������
function cw_selmonth, topbase, ymlist
	;
	datebase = widget_base(topbase, /row, func_get_value='cw_selmonth_getValue')

	year = widget_droplist(datebase, xsize=50, value=strmid(ymlist, 0, 4))
	lab = widget_label(datebase, value='��')
	month = widget_droplist(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
	lab = widget_label(datebase, value='��')
	widget_control, datebase, set_uvalue = {year:year, month:month}
	return, datebase
end

;���ı��ļ��ж�ȡ�ַ���
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

;���ַ���д���ı��ļ�֮��
pro write_string, filename, flines
	openw, lun, filename, /get_lun
	nlines = n_elements(flines)
	for i=0, nlines-1 do begin
		printf, lun, flines[i]
	endfor
	close, lun
	free_lun, lun
end

;��һ��ini��������ȡֵ,����key=value����ʽ
function getIniValue, ini, key
	n_ini = n_elements(ini)
	for i=0, n_ini-1 do begin
		items = strsplit(ini[i], '=', /extract, /preserve_null)
		if n_elements(items) lt 2 then continue
		if items[0] eq key then return, items[1]
	endfor
	;û���ҵ��򷵻ؿ�ֵ
	return, ''
end

;��ȡһ��csv(���ŷָ�)�ļ�
;header���ص�һ��
;separatorΪָ���ķָ�����,Ĭ��Ϊ����
;����ֵΪn X m���ַ�����,���������ɵ�һ�е�����ȷ��
;���nohead=1�������в�����

function readCSV, file, header=header, seperator=separator, nohead=nohead
	if file_test(file) eq 0 then return, ''
	lines = read_string(file)
	rows = n_elements(lines)
	if keyword_set(separator) eq 0 then separator = ','
	;����һ�У�ȷ������
	header = strsplit(lines[0], separator, /extract, /preserve_null)
	cols = n_elements(header)

	;����ֻ��һ�е����
	if rows le 1 then return, header

	;���������б�
	contents = strarr(cols, rows-1)
	;���ж�ȡ�ļ�
	for i=1, rows-1 do begin
		aline = strsplit(lines[i], separator, /extract, /preserve_null)
		n_cols = n_elements(aline)
		min_cols = min([n_cols, cols])
		contents[0:min_cols-1, i-1] = aline[0:min_cols-1]
	endfor
	if keyword_set(nohead) eq 0 then contents=[[header],[contents]]
	return, contents
end

;���������б�
pro cw_seldate_panel_setdaylist, baseid, selday=selday
	widget_control, baseid, get_uvalue = idlist
	;��ȡ���
	year = widget_info(idlist.yearid, /combobox_gettext)
	;��ȡ�·�
	month = widget_info(idlist.monthid, /list_select) + 1
	dayslist = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	days = dayslist[month - 1]
	;�������
	if month eq 2 then days = fix(julday(3, 1, year) - julday(2, 1, year))

	;��ȡ���µ�һ�������ڼ�
	week = julday(month, 1, year) mod 7
	total_days = week + days
	;���µ�����
	total_week = ceil(total_days / 7.)
	;�����б�
	dayarray = strarr(7, total_week)
	;���õ�һ��
	index = 1
	for j=week, 6 do begin
		dayarray[j, 0] = strtrim(index, 2)
		index ++
	endfor

	;����������
	for i=1, total_week-1 do begin
		for j=0, 6 do begin
			dayarray[j, i] = strtrim(index, 2)
			index ++
			if index gt days then break
		endfor
	endfor
	widget_control, idlist.dayid, set_value = dayarray, ysize=total_week

	;ѡ�����ö��յ�ѡ��
	if keyword_set(selday) eq 0 then selday = 1
	col = (week + selday - 1) mod 7
	row = (week + selday - 1) / 7
	widget_control, idlist.dayid, set_table_select=[col, row, col, row]
	widget_control, idlist.dayid, set_table_view=[0,0]

	;������ʾ����
	widget_control, idlist.date, set_value = year + '��' + strtrim(month, 2) $
		+ '��' + strtrim(selday, 2) + '��', set_uvalue = {year:fix(year), $
			month:fix(month), day:fix(selday)}
end

;���ڱ�ѡ����¼�
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
	;����������ʾ
	widget_control, idlist.date, get_uvalue = date
	date.day = day
	widget_control, idlist.date, set_uvalue = date, set_value = strtrim(date.year, 2) $
		+ '��' + strtrim(date.month, 2) + '��' + strtrim(date.day, 2) + '��'
end

;��ݻ��·ݱ�ѡ����¼�
pro cw_seldate_panel_yearmonthevent, event
	widget_control, event.id, get_uvalue = datebase
	cw_seldate_panel_setdaylist, datebase
end

;�����յ�ѡ���ܿ�
function cw_seldate_panel, topbase
	base = widget_base(topbase, /col, space=0)
	;���ֵ�б��ӵ�ǰ����ǰ�г�20��
	caldat, systime(/jul), month, day, year
	ylist = rotate(strtrim(indgen(20) + year - 19, 2), 2)
	base1 = widget_base(base, /row)
		lab = widget_label(base1, value='�꣺')
		yearid = widget_combobox(base1, value=ylist, uvalue = base, $
			event_pro='cw_seldate_panel_yearmonthevent')
		lab = widget_label(base1, value='   ��ǰʱ�䣺')
		datetext = widget_text(base1, xsize=15)
	;�п��
	col_width = intarr(7)
	col_width[*] = 25

	base1 = widget_base(base, /row)
		monthid = widget_list(base1, xsize=6, ysize=12, value=['һ��', '����', '����', $
			'����', '����', '����', '����', '����', '����', 'ʮ��', 'ʮһ��', 'ʮ����'], $
			uvalue = base, event_pro='cw_seldate_panel_yearmonthevent')
		dayid = widget_table(base1, /no_row_header, scr_xsize=196, scr_ysize=150, xsize=7,$
			column_width=col_width, column_labels=['һ','��','��','��','��','��','��'], $
			scroll=1, event_pro='cw_seldate_panel_dayevent', /all_events, uvalue = base)
	widget_control, base, set_uvalue = { date:datetext, yearid:yearid, $
		monthid:monthid, dayid:dayid }
	;�����·�ѡ��
	widget_control, monthid, set_list_select = month - 1
	;��������
	cw_seldate_panel_setdaylist, base, selday=day

	return, datetext
end

;Ĭ���¼�
pro cw_seldate_panel_ui_event, event
end

pro cw_seldate_panel_ui
	base = widget_base(title='����ѡ��', /col, /tlb_frame_attr, xpad=0, ypad=0, space=0)
	datebase = cw_seldate_panel(base)
	base1 = widget_base(base, /row, /align_right, space=10)
		btn = widget_button(base1, value='ȷ��', xsize=60, ysize=23)
		btn = widget_button(base1, value='ȡ��', xsize=60, ysize=23, event_pro='WidgetClose')
	widget_control, base, /realize
	xmanager, 'cw_seldate_panel_ui', base
end

;--------------------------------------------------
;������������Ĭ���¼��������
pro cw_datebox_select_ok, event
	widget_control, event.top, get_uvalue = idlist
	widget_control, idlist.date, get_value = date, get_uvalue = dateuv
	widget_control, idlist.topbtn, get_uvalue = dateid
	widget_control, dateid, set_value = date, set_uvalue = dateuv
	widget_control, event.top, /destroy
end

pro cw_datebox_select_event, event
end

;��ʱ��ѡ��Ի���Ĺ���
pro cw_datebox_select, event
	;����ʱ�䴰���ڸ������е�λ��
	wgeo = widget_info(event.top, /geometry)
	cx = wgeo.xoffset + wgeo.scr_xsize / 2 - 120
	cy = wgeo.yoffset + wgeo.scr_ysize / 2 - 100

	tlb = widget_base(group_leader=event.top, title='����ѡ��', /col, /tlb_frame_attr, xpad=0, ypad=0, $
		space=0, xoffset=cx, yoffset=cy)
	datebase = cw_seldate_panel(tlb)
	base1 = widget_base(tlb, /row, /align_right, space=10)
		btn = widget_button(base1, value='ȷ��', xsize=60, ysize=23, event_pro='cw_datebox_select_ok')
		btn = widget_button(base1, value='�ر�', xsize=60, ysize=23, event_pro='WidgetClose')
	widget_control, tlb, /realize, set_uvalue = {topbtn:event.id, date:datebase}
	xmanager, 'cw_datebox_select', tlb
end

;ʱ��ѡ������ʾ����
function cw_datebox, topbase, xsize=xsize, title=title
	base = widget_base(topbase, /row, xpad=0, ypad=0, space=5)
	if keyword_set(title) eq 0 then title = 'ʱ�䣺'
	if keyword_set(xsize) eq 0 then xsize = 20
	lab = widget_label(base, value=title)
	date = widget_text(base, xsize=xsize)
	btn = widget_button(base, value='ѡ������', xsize=80, ysize=23, event_pro='cw_datebox_select')
	widget_control, btn, set_uvalue = date
	return, date
end

;------------------------------------------------
;���ļ����ļ��еĿؼ�

;�ļ��Ի���ѡ���¼�
pro cw_openfile_select, event
	widget_control, event.id, get_uvalue = idlist
	if idlist.directory eq 0 then begin
		;ѡ���ļ�
		widget_control, idlist.outdir, get_value = filename
		filename = dialog_pickfile(dialog_parent=event.top, file=filename, title=idlist.title, path=idlist.path, FILTER = idlist.filter)
		if filename eq '' then return
		widget_control, idlist.outdir, set_value = filename
	endif else begin
		;ѡ��Ŀ¼
		widget_control, idlist.outdir, get_value = dirname
		dirname = dialog_pickfile(dialog_parent=event.top, /directory, title=idlist.title, path=dirname)
		if dirname eq '' then return
		widget_control, idlist.outdir, set_value = dirname
	endelse
end

;�ؼ�����
function cw_openfile, topbase, title=title, value=value, btntitle=btntitle, path=path, $
	directory=directory, xsize=xsize, read=read, write=write, filter=filter
	;����ؼ���
	if keyword_set(value) eq 0 then value = ''
	if keyword_set(xsize) eq 0 then xsize = 40
	if keyword_set(filter) eq 0 then filter=['*.*']
	if keyword_set(path) eq 0 then path=''
	;ѡ��ʽ
	if keyword_set(directory) eq 0 then begin
		;�ļ�ѡ��
		if keyword_set(title) eq 0 then title='�ļ�����'
		if keyword_set(btntitle) eq 0 then btntitle='ѡ���ļ�'
	endif else begin
		;Ŀ¼ѡ��
		if keyword_set(title) eq 0 then title='Ŀ¼����'
		if keyword_set(btntitle) eq 0 then btntitle='ѡ��Ŀ¼'
	endelse

	base = widget_base(topbase, /row, space=3, /align_left)
		lab = widget_label(base, value=title)
		outdir = widget_text(base, xsize=xsize, /editable, value=value)
		btn = widget_button(base, value='images/open.bmp', xsize=20, ysize=20, event_pro='cw_openfile_select', /bitmap)
	widget_control, btn, set_uvalue = { outdir : outdir, directory : keyword_set(directory), $
		read:keyword_set(read), write:keyword_set(write), title:title, path:path, filter:filter}
	return, outdir
end

;�����ļ��ĺ�׺�����޷������򷵻ؿմ�
function file_extname, filename
	strlist = strsplit(filename, '.', /extract)
	n = n_elements(strlist)
	if n le 1 then return, '' else return, strlowcase(strlist[n-1])
end

;�����ļ����޺�׺���ĸ�ʽ
function file_removeext, filename
	strlist = strsplit(filename, '.', /extract)
	return, strlist[0]
end

;����һ��shape�ļ���û������ͬĿ¼ͬ����prj�ļ�
;����ֵ���壺-1 ���ļ�����.shp�ļ�, 0 ��shape�ļ�û��prj�ļ�����shape�ļ���prj�ļ�
function testshapeprj, filename
	if file_extname(filename) ne 'shp' then return, -1
	bname = file_basename(filename, '.shp')
	dir = file_dirname(filename)
	prjfile = dir + '\' + bname + '.prj'
	;print, prjfile
	return, file_test(prjfile)
end

;��ȡʸ���ļ��ķ�Χ,�������Ҹ���5%������
;���룺ʸ���ļ���
;�����[�������(x��Сֵ)���ײ�����(y��Сֵ)����ȣ��߶�]
;���������prj�ļ�����ֱ���������
;���û��prj�ļ�������Ϊ�Ǿ�γ�ȣ���Ҫ����ת��
;���ת���д����򷵻ؿ��ַ���
function getShapeBounds, filename
	shapeobj = OBJ_NEW('IDLffShape', filename)
	if obj_valid(shapeobj) eq 0 then return, ''
	shapeobj->GetProperty, N_ENTITIES=num_ent, ENTITY_TYPE=ShapeType
	;print, num_ent, ShapeType
	;�������Ϊ������Ͳ��ǵ��߶���Σ��򷵻ؿմ�
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
	;�ж��Ƿ��Ǿ�γ�ȵ�ֵ
	if testshapeprj(filename) eq 0 then begin
		;��ת�ɻ���
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

;�ж�����geo�ṹ������Ƿ���ȫһ��
;һ�·���1�����򷵻�0
function sameGeoInfor, geo1, geo2
;	Catch, theError
;	IF theError NE 0 THEN BEGIN
;	  Catch, /Cancel
;	  ok = Error_Message()
;	  RETURN,0
;	ENDIF
;
;	;�ȱȽ��ֶ�����
;	n_tag1 = n_tags(geo1)
;	n_tag2 = n_tags(geo2)
;	if(n_tag1 ne n_tag2) then begin
;		return,0
;	end
;	;Ȼ������ֶαȽ�
;	for i=0, n_tag1-1, 1 do begin
;		if(n_elements(geo1.(i)) ne n_elements(geo2.(i))) then begin
;			return,0
;		end
;		NotEquals = geo1.(i) ne geo2.(i)
;		if(total(NotEquals) ne 0) then begin
;			;IMAGINE GeoTIFF Support �ֶγ���
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
;����ΪMODIS��������õ���ͨ��ģ��
;***********************************************************************************************

;ͨ�ú�����
;0 ���������
;1 ��������
function getVersion
	return, 0
end

;���ڹرչ���,��ȷ��
pro WidgetCloseConfirm, event
	msg = dialog_message('�Ƿ��˳���ǰ�������', title='�˳�ȷ��', /ques)
	if msg eq 'No' then return
	widget_control, event.top, /destroy
end

;���ڹرչ��ܣ�����ȷ��
pro WidgetClose, event
	widget_control, event.top, /destroy
end

;ͨ�õĴ���Ĭ���¼�
pro WidgetDefault_event, event
end

;��ȡһ��widget��value
function widget_value, widgetid
	widget_control, widgetid ,get_value = wv
	return, wv
end

;��ȡһ��widget��uvalue
function widget_uvalue, widgetid
	widget_control, widgetid ,get_uvalue = wuv
	return, wuv
end

;����һ����������һ��ֵ
function LastValue, array
	return, array[n_elements(array) - 1]
end

;�Ѷ��ŷָ���ֵת������
function CSVToArray, csv
	items = strsplit(csv, ',', /extract, /preserve_null)
	return, items
end

;�ж�һ�������Ƿ��ǽṹ��
function valid_set, theValue
	if size(theValue, /type) eq 8 then return, 1 else return, 0
end

;����һ���ļ�·��������ļ����ڣ����ظ��ļ�·������������ڣ����ؿմ�
function ExistFile, thefile
	return, file_test(thefile) eq 1 ? thefile : ''
end

;��֤ʹĿ¼�����һ���ַ���\
function formatDir, dir
	nlen = strlen(dir)
	if nlen eq 0 then return, dir
	if strmid(dir, nlen-1) ne '\' then dir += '\'
	return, dir
end

;��ȡĬ�ϵĹ���Ŀ¼
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

;���ָ��Ŀ¼����,��ʹ��ָ����Ŀ¼,����ʹ��Ĭ��Ŀ¼
function getDefaultFolder2, folder
	if file_test(folder, /directory) eq 1 then return, folder else return, getDefaultFolder()
end

;ʹһ��widget������丸���ھ�����ʾ
pro parent_center, tbase, pbase
	geo_p = widget_info(pbase, /geometry)
	geo_t = widget_info(tbase, /geometry)
	;��ȡ�����ڵ����ĵ�
	cen_x = geo_p.xoffset + geo_p.xsize / 2
	cen_y = geo_p.yoffset + geo_p.ysize / 2
	;�����Ӵ��ڵ�offset
	xoff = cen_x - geo_t.xsize / 2
	yoff = cen_y - geo_t.ysize / 2
	widget_control, tbase, xoffset=xoff, yoffset=yoff
end

;ʹһ�����ھ�����ʾ
pro window_center, base
	geo = widget_info(base, /geometry)
	device, get_screen_size = ssize
	xoff = (ssize[0] - geo.xsize) / 2
	yoff = (ssize[1] - geo.ysize) / 2
	widget_control, base, xoffset=xoff, yoffset=yoff
end

;�ж�һ���ַ����ǲ������������
function isNumber, str
	numlist = strtrim(indgen(10), 2)
	nloops = strlen(str)
	for i=0, nloops-1 do begin
		index = where(strmid(str, i, 1) eq numlist)
		if index[0] eq -1 then return, 0
	endfor
	return, 1
end

;��һά�����ĳ��λ�ò���һ����Ԫ��
;��� index С��0�򷵻�ԭ��,�������array�ĳ���,��������һ��
function ArrayPut, array, index, element
	;С�ڵ���0������ǰ��
	if index le 0 then return, [element, array]

	;�������鳤��,���������
	a_len = n_elements(array)
	if index ge a_len then return, [array, element]

	;λ�������м�,��ԭλ���Ժ��Ԫ�����Ųһλ
	return, [array[0:index-1], element, array[index:*]]
end

;ȥ����������ĳһ��λ�õ�ֵ
function ArrayRemove, array, index
	;���鳤��Ϊ1��Ԫ������С���������������, ����ԭ����
	a_len = n_elements(array)
	if a_len eq 1 or index lt 0 or index ge a_len then return, array

	;λ�������ײ�
	if index eq 0 then return, array[1:*]

	;λ������β��
	if index eq a_len-1 then return, array[0:a_len-2]

	;λ�������м�
	return, [array[0:index-1], array[index+1:*]]
end

;����ĳ��ĳ�µ�����(���һ�������)
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
	;����ֵ�̶�Ϊint��
	return, fix(julday(nextmonth, 1, nextyear) - julday(month, 1, year))
end

;�ѵ�ǰ����ת��Ϊ��λ�����ڻ�14λ��ʱ���ַ���
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

;������yyyy-mm-dd��yyymmdd���ַ�����ʽ������ת�����������ڻ�������
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

;������ת����Ϊ��λ�������ַ���
function TransDateToString, Year, Month, Day
    Yearstr = string(fix(year), format='(I4)')
    Monthstr = string(fix(month), format='(I02)')
    Daystr = string(fix(day), format='(I02)')

    return, strjoin([Yearstr, Monthstr, Daystr])
end

;������ת����Ϊ'-'�ָ��ĸ�ʽ
function TransDateToEnglish, Year, Month, Day
    Yearstr = strtrim(string(year), 1)
    Monthstr = strtrim(string(month), 1)
    Daystr = strtrim(string(day), 1)
    datestr = Yearstr + '-' + Monthstr + '-' + Daystr
    return, datestr
end

;������ת����Ϊ'_'�ָ��ĸ�ʽ
function getDateWithUL, Year, Month, Day
    Yearstr = string(year, format='(I4)')
    Monthstr = string(month, format='(I02)')
    Daystr = string(day, format='(I02)')
    datestr = Yearstr + '_' + Monthstr + '_' + Daystr
    return, datestr
end

;��-����������ת����8λ��׼��ʽ
function FormateDate, date
	dstr = strsplit(date, '-', /extract, /preserve_null)
	if n_elements(dstr) lt 3 then return, date
	return, TransDateToString(dstr[0], dstr[1], dstr[2])
end

;�������ո�ʽ������ת����Ϊ��λ�������ַ���
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

;������ת����Ϊyyyy��mm��dd�յĸ�ʽ
function TransDateToChinese, Year, Month, Day
    Yearstr = strtrim(string(year), 2)
    Monthstr = strtrim(string(month), 2)
    Daystr = strtrim(string(day), 2)
    datestr = Yearstr + '��' + Monthstr +  '��' + Daystr + '��'
    return, datestr
end

;��YYYYMMDD��YYYYMMTT��ʽ������ת����Ϊyyyy��mm��dd�ջ�Ѯ�ĸ�ʽ
function TransStringToChinese, datestr, tenday=tenday

    Yearstr = strtrim(fix(strmid(datestr, 0, 4)), 2)
    Monthstr = strtrim(fix(strmid(datestr, 4, 2)), 2)
    Daystr = strtrim(fix(strmid(datestr, 6, 2)), 2)
    ;���������
    if keyword_set(tenday) eq 0 then begin
    	dstr = Yearstr + '��' + Monthstr +  '��' + Daystr + '��'
    	return, dstr
    endif
    ;Ѯ�����
    tlist = ['��Ѯ', '��Ѯ', '��Ѯ']
	dstr = Yearstr + '��' + Monthstr +  '��' + tlist[fix(Daystr) - 1]
	return, dstr
end

;�ж�һ���ַ����ǲ��ǺϷ���8λ��ʽ������(YYYYMMDD)
function isDate, str
	;�ж��Ƿ���8���ַ���
	if strlen(str) ne 8 then return, 0

	;�ж��Ƿ�ȫ���������
	if isNumber(str) eq 0 then return, 0

	;�ж����Ƿ��ǺϷ���ʱ��
	if TransJulToString(GetDateOfString(str, /jul)) ne str then return, 0

	return, 1
end

;�����־����
;дlog��־,Ĭ�ϴ����ļ�tempdata\et_yyyymmmdd.log��
pro writetolog, strContent
	;�������Ϸ����˳�
	if size(strContent, /type) ne 7 then return
	;����ļ�
	caldat, systime(/j), month, day, year, hour, minute, second
	;����ʱ������ַ���
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

;�������,����ѡ�񵯳��Ի�����ӡ����־�ļ�
pro showMsg, msg, title=title, win=win, log=log, noprint=noprint
	;��ӡ����Ļ
	if keyword_set(noprint) eq 0 then print, msg

	;��ʾ�Ի���
	if keyword_set(win) eq 1 then m = dialog_message(msg, title=title, /info)

	;д����־�ļ�
	if keyword_set(log) eq 1 then writetolog, msg
end

;������־����
pro worklog, strContent, init=init
	;�������Ϸ����˳�
	if size(strContent, /type) ne 7 then return
	;����ļ�
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

;������ѡ��ؼ�
function cw_seldate_get_value, id
	widget_control, id, get_uvalue = uv

	widget_control, uv.year, get_value = yearlist
	index = widget_info(uv.year, /droplist_select)
	year = fix(yearlist[index])

	month = fix(widget_info(uv.month, /combobox_gettext))
	day = fix(widget_info(uv.day, /combobox_gettext))

	return, { year:year, month:month, day:day }
end

;;ymdlist �ǰ�λ�ı�ʾʱ����ַ�������
;function cw_seldate, topbase, ymdlist
;	;
;	datebase = widget_base(topbase, /row, func_get_value='cw_seldate_get_value')
;
;	year = widget_droplist(datebase, xsize=50, value=strmid(ymdlist, 0, 4))
;	lab = widget_label(datebase, value='��')
;	month = widget_combobox(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
;	lab = widget_label(datebase, value='��')
;	day = widget_combobox(datebase, xsize=40, value=strtrim(indgen(31)+1, 2))
;	lab = widget_label(datebase, value='��')
;	widget_control, datebase, set_uvalue = {year:year, month:month, day:day}
;	return, datebase
;end

;��ȡ����ѡ��ؼ���ֵ
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

;����ѡ��ؼ�
;ymlist ����λ�ı�ʾʱ����ַ�������
;function cw_selmonth, topbase, ymlist
;	;
;	datebase = widget_base(topbase, /row, func_get_value='cw_selmonth_getValue')
;
;	year = widget_droplist(datebase, xsize=50, value=strmid(ymlist, 0, 4))
;	lab = widget_label(datebase, value='��')
;	month = widget_droplist(datebase, xsize=40, value=strtrim(indgen(12)+1, 2))
;	lab = widget_label(datebase, value='��')
;	widget_control, datebase, set_uvalue = {year:year, month:month}
;	return, datebase
;end

;���ı��ļ��ж�ȡ�ַ���
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

;���ַ���д���ı��ļ�֮��
;pro write_string, filename, flines
;	openw, lun, filename, /get_lun
;	nlines = n_elements(flines)
;	for i=0, nlines-1 do begin
;		printf, lun, flines[i]
;	endfor
;	close, lun
;	free_lun, lun
;end

;��ȡһ��csv(���ŷָ�)�ļ�
;header���ص�һ��
;separatorΪָ���ķָ�����,Ĭ��Ϊ����
;����ֵΪn X m���ַ�����,���������ɵ�һ�е�����ȷ��
;���nohead=1�������в�����

;function readCSV, file, header=header, seperator=separator, nohead=nohead
;	if file_test(file) eq 0 then return, ''
;	lines = read_string(file)
;	rows = n_elements(lines)
;	if keyword_set(separator) eq 0 then separator = ','
;	;����һ�У�ȷ������
;	header = strsplit(lines[0], separator, /extract, /preserve_null)
;	cols = n_elements(header)
;
;	;����ֻ��һ�е����
;	if rows le 1 then return, header
;
;	;���������б�
;	contents = strarr(cols, rows-1)
;	;���ж�ȡ�ļ�
;	for i=1, rows-1 do begin
;		aline = strsplit(lines[i], separator, /extract, /preserve_null)
;		n_cols = n_elements(aline)
;		min_cols = min([n_cols, cols])
;		contents[0:min_cols-1, i-1] = aline[0:min_cols-1]
;	endfor
;	if keyword_set(nohead) eq 0 then contents=[[header],[contents]]
;	return, contents
;end

;���������б�
;pro cw_seldate_panel_setdaylist, baseid, selday=selday
;	widget_control, baseid, get_uvalue = idlist
;	;��ȡ���
;	year = widget_info(idlist.yearid, /combobox_gettext)
;	;��ȡ�·�
;	month = widget_info(idlist.monthid, /list_select) + 1
;	dayslist = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
;	days = dayslist[month - 1]
;	;�������
;	if month eq 2 then days = fix(julday(3, 1, year) - julday(2, 1, year))
;
;	;��ȡ���µ�һ�������ڼ�
;	week = julday(month, 1, year) mod 7
;	total_days = week + days
;	;���µ�����
;	total_week = ceil(total_days / 7.)
;	;�����б�
;	dayarray = strarr(7, total_week)
;	;���õ�һ��
;	index = 1
;	for j=week, 6 do begin
;		dayarray[j, 0] = strtrim(index, 2)
;		index ++
;	endfor
;
;	;����������
;	for i=1, total_week-1 do begin
;		for j=0, 6 do begin
;			dayarray[j, i] = strtrim(index, 2)
;			index ++
;			if index gt days then break
;		endfor
;	endfor
;	widget_control, idlist.dayid, set_value = dayarray, ysize=total_week
;
;	;ѡ�����ö��յ�ѡ��
;	if keyword_set(selday) eq 0 then selday = 1
;	col = (week + selday - 1) mod 7
;	row = (week + selday - 1) / 7
;	widget_control, idlist.dayid, set_table_select=[col, row, col, row]
;	widget_control, idlist.dayid, set_table_view=[0,0]
;
;	;������ʾ����
;	datestr = getversion() eq 0 ? ['��', '��', '��'] : ['-', '-', '']
;	widget_control, idlist.date, set_value = year + datestr[0] + strtrim(month, 2) $
;		+ datestr[1] + strtrim(selday, 2) + datestr[2], set_uvalue = {year:fix(year), $
;			month:fix(month), day:fix(selday)}
;end

;���ڱ�ѡ����¼�
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
;	;����������ʾ
;	widget_control, idlist.date, get_uvalue = date
;	date.day = day
;
;	datestr = getversion() eq 0 ? ['��', '��', '��'] : ['-', '-', '']
;
;	widget_control, idlist.date, set_uvalue = date, set_value = strtrim(date.year, 2) $
;		+ datestr[0] + strtrim(date.month, 2) + datestr[1] + strtrim(date.day, 2) + datestr[2]
;end

;��ݻ��·ݱ�ѡ����¼�
;pro cw_seldate_panel_yearmonthevent, event
;	widget_control, event.id, get_uvalue = datebase
;	cw_seldate_panel_setdaylist, datebase
;end

;�����յ�ѡ���ܿ�
function cw_seldate_panel, topbase, thedate
	base = widget_base(topbase, /col, space=0)
	;���ֵ�б���1980�꿪ʼ
	caldat, systime(/jul), month, day, year
	ylist = rotate(strtrim(indgen(year - 1979) + 1980, 2), 2)

	;����Ѿ�����date����ʹ�����еģ�����ʹ�õ�ǰ����
	if valid_set(thedate) eq 1 then begin
		year = thedate.year
		year_index = where(strtrim(year, 2) eq ylist)
		if year_index[0] eq -1 then year_index = 0
		month = thedate.month
		day = thedate.day
	endif

	base1 = widget_base(base, /row)
		lab = widget_label(base1, value='ѡ����ݣ�')
		yearid = widget_combobox(base1, value=ylist, uvalue = base, $
			event_pro='cw_seldate_panel_yearmonthevent')
		widget_control, yearid, set_combobox_select = year_index
		lab = widget_label(base1, value='����')
		datetext = widget_text(base1, xsize=15)
	;�п��
	col_width = intarr(7)
	col_width[*] = 25

	base1 = widget_base(base, /row)
		monthid = widget_list(base1, xsize=7, ysize=12, value=['1��','2��','3��','4��',$
			'5��','6��','7��','8��','9��','10��','11��','12��'], $
			uvalue = base, event_pro='cw_seldate_panel_yearmonthevent')
		dayid = widget_table(base1, /no_row_header, scr_xsize=196, scr_ysize=150, xsize=7,$
			column_width=col_width, column_labels=['һ','��','��','��','��','��','��'], $
			scroll=1, event_pro='cw_seldate_panel_dayevent', /all_events, uvalue = base)
	widget_control, base, set_uvalue = { date:datetext, yearid:yearid, $
		monthid:monthid, dayid:dayid }
	;�����·�ѡ��
	widget_control, monthid, set_list_select = month - 1
	;��������
	cw_seldate_panel_setdaylist, base, selday=day

	return, datetext
end

;Ĭ���¼�
;pro cw_seldate_panel_ui_event, event
;end

;pro cw_seldate_panel_ui
;	base = widget_base(title='����ѡ��', /col, /tlb_frame_attr, xpad=0, ypad=0, space=0)
;	datebase = cw_seldate_panel(base)
;	base1 = widget_base(base, /row, /align_right, space=10)
;		btn = widget_button(base1, value='ȷ��', xsize=60, ysize=23)
;		btn = widget_button(base1, value='ȡ��', xsize=60, ysize=23, event_pro='WidgetClose')
;	widget_control, base, /realize
;	xmanager, 'cw_seldate_panel_ui', base
;end

;--------------------------------------------------
;������������Ĭ���¼��������
;pro cw_datebox_select_ok, event
;	widget_control, event.top, get_uvalue = idlist
;	widget_control, idlist.date, get_value = date, get_uvalue = dateuv
;	widget_control, idlist.topbtn, get_uvalue = dateid
;	widget_control, dateid, set_value = date, set_uvalue = dateuv
;	widget_control, event.top, /destroy
;end

;pro cw_datebox_select_event, event
;end

;��ʱ��ѡ��Ի���Ĺ���
;pro cw_datebox_select, event
;	;����ʱ�䴰���ڸ������е�λ��
;	wgeo = widget_info(event.top, /geometry)
;	cx = wgeo.xoffset + wgeo.scr_xsize / 2 - 120
;	cy = wgeo.yoffset + wgeo.scr_ysize / 2 - 100
;
;	;��ȡ��һ�����õ�ʱ�䣬���û�����ã���Ϊ�մ�
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

;ʱ��ѡ������ʾ����
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
;���ļ����ļ��еĿؼ�

;�ļ��Ի���ѡ���¼�
;pro cw_openfile_select, event
;	widget_control, event.id, get_uvalue = idlist
;	if idlist.directory eq 0 then begin
;		;ѡ���ļ�
;		widget_control, idlist.outdir, get_value = filename
;		filename = dialog_pickfile(dialog_parent=event.top, file=filename, $
;			filter=idlist.filter, path=getdefaultfolder())
;		if filename eq '' then return
;		widget_control, idlist.outdir, set_value = filename
;	endif else begin
;		;ѡ��Ŀ¼
;		widget_control, idlist.outdir, get_value = dirname
;		if file_test(dirname, /directory) eq 0 then dirname = getdefaultfolder()
;		dirname = dialog_pickfile(dialog_parent=event.top, /directory, path=dirname)
;		if dirname eq '' then return
;		widget_control, idlist.outdir, set_value = dirname
;	endelse
;end

;�ؼ�����
;function cw_openfile, topbase, title=title, value=value, btntitle=btntitle, $
;	directory=directory, xsize=xsize, read=read, write=write, filter=filter
;	;����ؼ���
;	if keyword_set(value) eq 0 then value = ''
;	if keyword_set(xsize) eq 0 then xsize = 40
;	;ѡ��ʽ
;	vindex = getVersion()
;	if keyword_set(directory) eq 0 then begin
;		;�ļ�ѡ��
;		if keyword_set(title) eq 0 then title = (['�ļ����ã�', 'Set file:'])[vindex]
;		if keyword_set(btntitle) eq 0 then btntitle = (['ѡ���ļ�', 'Select'])[vindex]
;	endif else begin
;		;Ŀ¼ѡ��
;		if keyword_set(title) eq 0 then title = (['Ŀ¼���ã�', 'Set folder:'])[vindex]
;		if keyword_set(btntitle) eq 0 then btntitle = (['ѡ��Ŀ¼', 'Select'])[vindex]
;	endelse
;
;	;�ļ�������
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

;�Ƚ�����tiff�ļ���info�ĳߴ��Ƿ�һ��
function CheckDims, info1, info2
	if info1.dimensions[0] ne info2.dimensions[0] then return, 0
	if info1.dimensions[1] ne info2.dimensions[1] then return, 0
	return, 1
end

;�Ƚ�����ļ��������ļ���ʱ���ϵ
function checkFilesTimes, inputfile, outputfile
	;�����ļ������ڣ�����0�����ɼ���
	if file_test(inputfile) eq 0 then return, 0

	;����ļ������ڣ�����1���ɼ���
	if file_test(outputfile) eq 0 then return, 1

	;��ȡ�ļ�ʱ��
	fi1 = file_info(inputfile)
	fi2 = file_info(outputfile)
	;����ļ��£�����0�����ؼ��㣻����ļ��ɣ�����1���ɼ���
	if fi1.mtime lt fi2.mtime then return, 0 else return, 1
end

;����Զ�����
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

;���㾫��
;value : ��ȷֵ
;error : ����ֵ�뾫ȷֵ֮��
;����-��ʾ��ȷֵΪ0
;����0~100֮���ʾ����
;����0��ʾ��ֵ���ڲ���ֵ

function getPrecision, value, error
	;����
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

;�ַ���ת���ݣ����Ϸ��򱨴�
function str2value, str
	vindex = getversion()
	on_ioerror, label
	return, {error:0, value:double(str)}
	label:
		if vindex eq 0 then showmsg, '������ַ������ǺϷ������ָ�ʽ��', title='�û��������', /win, /log
		if vindex eq 1 then showmsg, 'The input is not a valid number', title='Input Error', /win, /log
		return, {error:1}
end

;�ж����Ƿ�λ��������
function inList, item, list
	index = where(item eq list, cnt)
	return, cnt
end

;����ϵͳ·��
function getsyspath
	;��ȡini�ļ�
	str1 = read_string('data\config.ini')
	;������벢�γɽṹ��
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

;����java�Ƿ����
;0 ����
;-1 ������
function testJava
	spawn, 'java -version', res, eres
	jp = strpos(eres[0], 'java version')
	return, jp[0]
end

;ɾ��ָ��Ŀ¼֮�µ�����
pro clearFolder, folder, type
	;debugģʽ��,��ɾ���ļ�
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

;��ʡ�е�������Ϣ
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

;��juldayת����Ϊyyyy-mm-dd hh-mm-ss�ĸ�ʽ
function getdbtimestring, jul
	caldat, jul, m1, d1, y1, h1, mm1, s1
	sTime = string(y1, format='(I04)') + '-' $
		+ strjoin(string([m1, d1], format='(I02)'), '-') $
		+ ' ' + strjoin(string([h1, mm1, d1], format='(I02)'), '-')
	return, sTime
end

;�����ݿ���д����־
pro LogToDb, top, startTime, Status, errorReason
	return
	;����������
	idlist = widget_uvalue(top)
	if obj_valid(idlist.odbc) eq 0 then begin
		showmsg, '�޷���ȡ�������ݿ�û�����ӣ�', /log
		return
	endif

	;ʱ��
	sTime = getdbtimestring(startTime)
	eTime = getdbtimestring(systime(/j))

	Status = strtrim(Status, 2)

	;״̬�ж�
	isError = errorReason eq '' ? '0' : '1'

	;�����һ����ȷ�Ľ��
	sqlstr = 'delete from tbl_state where modelnam=8'
	idlist.odbc->ExecuteSql, sqlstr

	;ֻȡǰ100���ַ�
	errorReason = strmid(errorReason, 0, 200)

	sqlstr = 'insert into tbl_state(MODELNAM,STARTTIME,ENDTIME,STATUS,ISERROR,REASON) ' $
		+ ' values(''8'', ''' + sTime + ''',''' + eTime + ''', ''' + Status + ''', ' $
		+ '''' + isError + ''',''' + errorReason + ''')'
	;
	print, sqlstr
	idlist.odbc->ExecuteSql, sqlstr
end

;����������ȡ���ݵ��¼�
function cw_password_getvalue, baseid
	widget_control, baseid, get_uvalue = pwdtxt
	widget_control, pwdtxt, get_uvalue = puvalue
	return, puvalue.char
end

;���������
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
	;��������
	mask = ''
	masklen = strlen(uv.char)
	for i=0,masklen-1 do mask = mask + '*'
	widget_control, event.id, set_value = mask, set_uvalue = uv, set_text_select = [event.offset, 0]
end

;���������
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