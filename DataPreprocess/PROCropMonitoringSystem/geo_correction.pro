;���ξ���ģ��
;ť����	2010��5��12��	�޸����׶��ĳ���õ�

;�ռ�����ɫ��ǿ��ʹ�÷ֶ����������㷨����ȡ��С1%�����1%�Ľ���
function getminmaxvalue, data
	;�������
	dcounts = double(n_elements(data))
	;��С/���ֵ��λ��,�������� 1% ����
	index1 = long(dcounts * 0.01d)
	index2 = long(dcounts * 0.99d)
	;����������,��ȥ���ظ�ֵ
	adata = data[sort(data)]
	;������Ӧλ�õ�ֵ
	return, adata[[index1, index2]]
end

function WARP_TRI_YLD, xo, yo, xi, yi, im_in, OUTPUT_SIZE = output_size
	s = SIZE(im_in)
	if s[0] ne 2 then print, '���ξ��������ݸ�ʽ����ȷ'
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

function TMCorrectionRunByInfo, info
;��ȡ�ļ���Ϣ��ԭʼӰ���goetiff��Ϣ
	r1 = query_tiff(info.orifilename, oriinfo)
	r2 = query_tiff(info.reffilename, refinfo, geotiff=ref_geotiff)

	pixelsize=ref_geotiff.MODELPIXELSCALETAG[0:1]

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
	geo_x = ref_geotiff.MODELTIEPOINTTAG[3] + pixelsize[0] * ref_x0
	geo_y = ref_geotiff.MODELTIEPOINTTAG[4] + pixelsize[1] * ref_ytop
	;���ü��ξ����ĺ���
	;��ʱ�ļ�
	tempfile = info.outfilename + '.tmp'
	openw, templun, tempfile, /get_lun
	;��¼��дλ��
	fileposlist = lonarr(oriinfo.channels)
	;���ͨ�����о���

	for i=0, oriinfo.channels-1 do begin
		data = read_tiff(info.orifilename, channels=i)
		newdata = WARP_TRI_YLD(x2list, y2list, x1list, y1list, data)
		if i eq 0 then begin
			dims=size(newdata,/dimensions)
			output = fltarr(oriinfo.channels,dims[0],dims[1])
		endif
		output[i,*,*]=newdata

	endfor

	write_tiff,info.outfilename,output,/float,geotiff=ref_geotiff

	;�����roi�ļ�����ʹ��roi�ļ������и�
	if file_test(info.roifile) eq 1 then begin
		err = do_subset(info.outfilename, info.roifile, info.outfilename)
		if err eq 1 then begin
			print, '����ʧ�ܣ������ļ���ROI�ļ�û���غϵ�����'
		endif
	endif

	;����
	return, 1
end

function TMCorrectionGetRunInfo, event
	;ȫ��ID�б�
	widget_control, event.top, get_uvalue = idlist

	;���Ƶ��б�
	widget_control, idlist.plist, get_uvalue = puv
	if puv.count eq 0 then begin
		msg = dialog_message('û�п��Ƶ��б��޷����м��ξ������ܣ�', title='���ξ���', /infor)
		return, 0
	endif
	;���Ƶ�ĸ���������������
	if puv.count lt 6 then begin
		msg = dialog_message('����Ҫѡ��6������Ŀ��Ƶ㣬�����޷����м��ξ������ܣ�', title='���ξ���', /infor)
		return, 0
	endif

	;ԭʼӰ���ļ���Ϣ
	widget_control, idlist.ori_id, get_uvalue = ori_widlist
	widget_control, ori_widlist.drawid, get_uvalue = oridrawuvalue
	;�ο�Ӱ���ļ���Ϣ
	widget_control, idlist.ref_id, get_uvalue = ref_widlist
	widget_control, ref_widlist.drawid, get_uvalue = refdrawuvalue

	;�趨����ļ�������ڼ��ξ���Ԥ�����Ŀ¼֮�£�������ԭʼӰ����geo.tif
	dirs = idlist.outputfile
	outfile = dirs + file_basename(oridrawuvalue.filename, '.tif') + '_geo.tif'
	;ROI�ļ�
	rofile = ''
	if size(idlist.roifile, /type) eq 7 then roifile = idlist.roifile
	;����Ҫ����ı���
	tmcorrection_info = { $
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

;���ξ������ξ��������ڹر��¼�
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

;���ξ������м��ξ����Ĺ��ܣ���Ӧ�������İ�ť
pro TMCorrectionRun, event
	;�ӽ��������ȡ�ṹ�����
	info = TMCorrectionGetRunInfo(event)
	if size(info, /type) ne 8 then return

	;�Բ����ṹ��Ϊ�������������м��ξ����Ĺ���
	re = TMCorrectionRunByInfo(info)

	;��ʾ��ɵ���Ϣ
	if re eq 1 then $
	msg = dialog_message('��ɼ��ξ���������', title='���ξ���', /info)
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

;�Կ��Ƶ����ӡ�ɾ����ת����������ɫ����
pro TMCorrectionControlPoint, event

	FORWARD_FUNCTION TMCORRECTIONGETFORMATEDTABLE
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
		msg = dialog_message('û��ԭʼӰ���޷���ɲ�����', title='���ξ���', /infor)
		return
	endif
	;��ȡ�ο�Ӱ��Ĵ����
	widget_control, idlist.ref_id, get_uvalue = refuvalue
	widget_control, refuvalue.drawid, get_uvalue = refdrawuvalue
	if size(refdrawuvalue, /type) ne 8 then begin
		msg = dialog_message('û�вο�Ӱ���޷���ɲ�����', title='���ξ���', /infor)
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
						' �Ŀ��Ƶ����Ӳ�����', title='���ξ���', /info)
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
				msg = dialog_message('û�п��Ƶ����ݣ��޷�ɾ����', title='���ξ���', /information)
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
				msg = dialog_message('��ǰ������Ϊ�գ����ܱ��棡', title='���ξ���', /info)
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
				msg = dialog_message('��ǰ������Ϊ�գ��Ƿ������', title='���ξ���', /question)
				if msg eq 'No' then return
			endif
			;
			filename = dialog_pickfile(title='ѡ����Ƶ��б��ļ�', filter='*.dat', default_extension='dat')
			if filename[0] eq '' then return
			if file_test(filename[0]) ne 1 then begin
				msg = dialog_message('���ؿ��Ƶ��б��ļ�ʧ�ܣ�', title='���ξ���', /question)
				return
			endif
			;��ȡ�ļ�����
			openr, lun, filename[0], /get_lun
			table_rows = file_lines(filename[0])
			if table_rows eq 0 then begin
				msg = dialog_message('�ļ�Ϊ�գ�����ʧ�ܣ�', title='���ξ���', /info)
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

function TMCorrectionGetFormatedTable, tablevalue
	formated_coordinate = strtrim(tablevalue[*, *], 2)
	formated_coordinate[0, *] = strtrim(long(tablevalue[0, *]), 2)
	return, formated_coordinate
end

pro TMCorrectionAddControlPoint, idlist, workid, x, y

	FORWARD_FUNCTION TMCORRECTIONGETFORMATEDTABLE

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
	if puv.count gt 3 then begin
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

		if autoadd eq 1 then begin
			puv.coordinate[newindex, puv.active_row] = strtrim(newx, 2)
			puv.coordinate[newindex+1, puv.active_row] = strtrim(newy, 2)
			isAutoadd = 1
		endif
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

		qr = query_tiff(drawuvalue.filename, info)

		case 1 of
			info.channels ge 4 : begin
				TileData[1, *, *] = BYTSCL(TileData[1, *, *], MIN=drawuvalue.minmax.minmax2[0], MAX=drawuvalue.minmax.minmax2[1])
				TileData[2, *, *] = BYTSCL(TileData[2, *, *], MIN=drawuvalue.minmax.minmax3[0], MAX=drawuvalue.minmax.minmax3[1])
				TileData[3, *, *] = BYTSCL(TileData[3, *, *], MIN=drawuvalue.minmax.minmax4[0], MAX=drawuvalue.minmax.minmax4[1])
	  			drawuvalue.objImage->SetTileData, ReqTiles[i], TileData[[3,2,1], *, *]
	  		end
			info.channels eq 3 : begin
				TileData[0, *, *] = BYTSCL(TileData[0, *, *], MIN=drawuvalue.minmax.minmax2[0], MAX=drawuvalue.minmax.minmax2[1])
				TileData[1, *, *] = BYTSCL(TileData[1, *, *], MIN=drawuvalue.minmax.minmax3[0], MAX=drawuvalue.minmax.minmax3[1])
				TileData[2, *, *] = BYTSCL(TileData[2, *, *], MIN=drawuvalue.minmax.minmax4[0], MAX=drawuvalue.minmax.minmax4[1])
	  			drawuvalue.objImage->SetTileData, ReqTiles[i], TileData[[2,1,0], *, *]
	  		end
	  		info.channels eq 2 : begin
				TileData[0, *, *] = BYTSCL(TileData[1, *, *], MIN=drawuvalue.minmax.minmax2[0], MAX=drawuvalue.minmax.minmax2[1])
	  			drawuvalue.objImage->SetTileData, ReqTiles[i], TileData[0, *, *]
	  		end
	  		else : begin
	  			TileData = BYTSCL(TileData, MIN=drawuvalue.minmax.minmax2[0], MAX=drawuvalue.minmax.minmax2[1])
	  			drawuvalue.objImage->SetTileData, ReqTiles[i], TileData
	  		end
		endcase
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

	FORWARD_FUNCTION TMCORRECTIONGETFORMATEDTABLE

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

		if puv.count gt 6 then begin
			o_xlist=transpose(puv.coordinate[1,0:puv.count-2])
			r_xlist=transpose(puv.coordinate[3,0:puv.count-2])
			o_ylist=transpose(puv.coordinate[2,0:puv.count-2])
			r_ylist=transpose(puv.coordinate[4,0:puv.count-2])

			kx = regress(o_xlist, r_xlist, const=constx)
			new_xlist= kx[0]*puv.coordinate[1,0:puv.count-1]+constx
			xerr=strtrim(string(total(new_xlist-puv.coordinate[3,0:puv.count-1])),2)

			Widget_Control,idlist.x_error,set_value=xerr

			ky = regress(o_ylist, r_ylist, const=consty)
			new_ylist= ky[0]*puv.coordinate[2,0:puv.count-1]+consty
			yerr=strtrim(string(total(new_ylist-puv.coordinate[4,0:puv.count-1])),2)
			Widget_Control,idlist.y_error,set_value=yerr

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

pro TMCorrection_open, idname, filename, filetype
	widget_control, idname, get_uvalue = idlist
	if filetype eq 'orifile' then begin
		;��ԭʼӰ��
		qr = query_tiff(filename, info)
		;�ж��Ƿ��Ƕ�ͨ����(�����ṩ֧�ֵ�ͨ��TIFF�Ĺ���)
		if info.channels eq 0 then begin
			msg = dialog_message('ԭʼӰ��ͨ��������ȷ', title='���ξ���')
			return
		endif
		;����Ӱ�񳤿�����ֵ
		maxsize = max([info.dimensions[0:1]])
		;��������
		workid = idlist.ori_id
		;��ȡ�ο�Ӱ��
		if info.channels ge 4 then begin
			data432 = read_tiff(filename, channels=[1,2,3])
		endif else begin
			data432 = read_tiff(filename, channels=[0,1,2])
		endelse
	endif

	;�򿪲ο�Ӱ��
	if filetype eq 'reffile' then begin
		qr = query_tiff(filename, info, geotiff=geotiff)
		;�ж��Ƿ��Ƕ�ͨ����(�����ṩ֧�ֵ�ͨ��TIFF�Ĺ���)
		if info.channels eq 0 then begin
			msg = dialog_message('�ο�Ӱ��ͨ��������ȷ', title='���ξ���')
			return
		endif
		if size(geotiff, /type) ne 8 then begin
			msg = dialog_message('�ο�Ӱ���ͶӰ��Ч��', title='���ξ���')
			return
		endif
		;����Ӱ�񳤿�����ֵ
		maxsize = max([info.dimensions[0:1]])
		;��������
		workid = idlist.ref_id
		;��ȡ�ο�Ӱ��
		if info.channels ge 4 then begin
			data432 = read_tiff(filename, channels=[1,2,3])
		endif else begin
			data432 = read_tiff(filename, channels=[0,1,2])
		endelse
	endif

	;�ֱ����4/3/2ͨ�����������
	if info.channels ge 3 then begin
		minmax4 = getminmaxvalue(data432[2, *, *])
		minmax3 = getminmaxvalue(data432[1, *, *])
		minmax2 = getminmaxvalue(data432[0, *, *])
	endif else begin
		minmax4 = getminmaxvalue(data432[0, *, *])
		minmax3 = getminmaxvalue(data432[0, *, *])
		minmax2 = getminmaxvalue(data432[0, *, *])
	endelse
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

PRO TMCorrection_event,event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
		widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='help_button'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'�򿪰����ĵ�'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '���ξ���ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('�Ҳ��������ĵ�',title='����')
				endelse
			endif
		end
		else:
	endcase


END

;���ξ���������
pro TMCorrection, event, datafile, reffile, roifile=roifile
	tlb = widget_base(group_leader=event.top, /modal, title = '���ξ���', /col, space=0, tlb_frame_attr=1)

	scr_dims = GET_SCREEN_SIZE()
	draw_size = (scr_dims[0]/2)-10
	base1 = widget_base(tlb, /row, xpad=0,space=5)
	base2 = widget_base(base1, /col, xpad=0, ypad=0,/base_align_center,space=0)
		oridraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=draw_size, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			base4 = widget_base(base3, /row,space=1,/base_align_center)
				ori_label = widget_label(base4, value='ԭʼӰ��', /align_left)
				o_xid = cw_field(base4, title='X����:', value='0', xsize=12, /noedit)
				o_yid = cw_field(base4, title='Y����:', value='0', xsize=12, /noedit)
			ori_toolBar = widget_base(base3,/exclusive,/row,/base_align_center)
				btn1 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\pointer.bmp', /bitmap, xsize=20, ysize=20, tooltip='ѡ��')
				btn2 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='����')
				btn3 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_in.bmp', /bitmap, xsize=20, ysize=20, tooltip='�Ŵ�')
				btn4 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_out.bmp', /bitmap, xsize=20, ysize=20, tooltip='��С')
				btn5 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\globe_7.bmp', /bitmap, xsize=20, ysize=20, tooltip='�鿴ȫͼ')

			widget_control, ori_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ori_label, isAdd:0}
	base2 = widget_base(base1, /col, xpad=0, ypad=0,/base_align_center,space=0)
		refdraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=draw_size, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			base4 = widget_base(base3, /row,space=1)
				ref_label = widget_label(base4, value='�ο�Ӱ��', /align_left)
				r_xid = cw_field(base4, title='X����:', value='0', xsize=12, /noedit)
				r_yid = cw_field(base4, title='Y����:', value='0', xsize=12, /noedit)
			ref_toolBar = widget_base(base3, /exclusive, /row)
				btn1 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\pointer.bmp', /bitmap, xsize=20, ysize=20, tooltip='ѡ��')
				btn2 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='����')
				btn3 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_in.bmp', /bitmap, xsize=20, ysize=20, tooltip='�Ŵ�')
				btn4 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_out.bmp', /bitmap, xsize=20, ysize=20, tooltip='��С')
				btn5 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\globe_7.bmp', /bitmap, xsize=20, ysize=20, tooltip='�鿴ȫͼ')

			widget_control, ref_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ref_label, isAdd:0}

	base1 = widget_base(tlb, /row,space=10,ypad=0)
		table_base = widget_base(base1,/column,/base_align_center,ypad=1)
		plist = widget_table(table_base, xsize=5, ysize=1, column_label=['���','ԭʼӰ��X����','ԭʼӰ��Y����','�ο�Ӱ��X����','�ο�Ӱ��Y����'],/frame, $
			column_widths=[50,106,106,106,106], scr_xsize=495, Y_SCROLL_SIZE=5, event_pro='TMCorrectionTableEvent', /all_events, /no_row_headers, $
			uvalue={count:0, active_row:-1})
		base2 = widget_base(base1, /col,xpad=10,ypad=1,space=1)
			base3 = widget_base(base2, /row, space=10,/frame,xpad=5)
				lab = widget_label(base3, value='  X��')
				x_error = widget_text(base3, xsize=20)
				lab = widget_label(base3, value='  Y��')
				y_error = widget_text(base3, xsize=20)
			base31 = widget_base(base2, /row, space=10,/frame)
				base32 = widget_base(base31, /column, space=2)
					base3 = widget_base(base32, /row, space=80,ypad=0)
						controlfile = widget_label(base3, value='  ���Ƶ㣺', uvalue = '')
						base4 = widget_base(base3, /row, space=10,ypad=0, /nonexcl, /align_right)
							btnautoadd = widget_button(base4, value='�Զ�ƥ����Ƶ�')
					base3 = widget_base(base32, /row, space=10,ypad=0)
						btn = widget_button(base3, value='���', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='add')
						btn = widget_button(base3, value='ɾ��', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='delete')
						btn = widget_button(base3, value='����', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='savelist')
						btn = widget_button(base3, value='����', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='loadlist')
			base3 = widget_base(base2, /row, xpad=40,ypad=1,space=40,/frame)
				btn = widget_button(base3, value='��ʼ����', xsize=80, ysize=23, event_pro='TMCorrectionRun')
;				btn = widget_button(base3, value='����������Ϣ', xsize=90, ysize=23, event_pro='TMCorrectionSaveRunInfo')
				btn = widget_button(base3, value='����', xsize=80, ysize=23,uname='help_button')
				btn = widget_button(base3, value='�ر�', xsize=80, ysize=23, event_pro='TMCorrectionOnClose')
	;����ԭʼӰ����Զ������
	widget_control, ori_label, set_uvalue = {drawid:oridraw, toolBar:ori_toolBar, xid:o_xid, yid:o_yid, pindex:1}
	;����ԭʼӰ����Զ������
	widget_control, ref_label, set_uvalue = {drawid:refdraw, toolBar:ref_toolBar, xid:r_xid, yid:r_yid, pindex:3}

	;���ROI�ļ��Ƿ����
	if keyword_set(roifile) eq 0 then roifile = ''

	Widget_Control,event.top,get_uvalue=PSTATE
	Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile
	Widget_Control,(*PSTATE).REF_FIELD,get_value=reffile
	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	;����ȫ�ֵ��Զ������
	widget_control, tlb, /realize, set_uvalue={ori_id:ori_label, ref_id:ref_label, $
		plist:plist, btnautoadd:btnautoadd, controlfile : controlfile, roifile:roifile , $
		inputfile	:	inputfile , $
		reffile	:	reffile , $
		outputfile	:	outputfile , $
		x_error	:	x_error , $
		y_error	:	y_error }

	;����ԭʼ�ļ�
	TMCorrection_open, tlb, datafile, 'orifile'
	;���زο��ļ�
	TMCorrection_open, tlb, reffile, 'reffile'
	xmanager, 'TMCorrection', tlb
end

PRO GEO_CORRECTION_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		;�������ļ��Ŀؼ�
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'������Ӱ��'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='REF_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'�򿪲ο�Ӱ��'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		pick_file_path, Event
      		common_log,'�����Ӱ��'
		end

		;ָ������������Ӧ
		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				;���г���
				Widget_Control,(*PSTATE).INPUT_FIELD,get_value=datafile
				Widget_Control,(*PSTATE).REF_FIELD,get_value=reffile
				Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outfile

				if file_test(datafile) ne 1 then begin
					CAUTION = DIALOG_MESSAGE('δ�����������Ӱ���Ӱ����ȷ��',title='����')
					return
				endif

				if file_test(reffile) ne 1 then begin
					CAUTION = DIALOG_MESSAGE('δ���òο�Ӱ���Ӱ����ȷ��',title='����')
					return
				endif

				if file_test(outfile) ne 1 then begin
					CAUTION = DIALOG_MESSAGE('δ�������·�������·�������ڣ�',title='����')
					return
				endif

				common_log,'��������'

				TMCorrection, event, datafile, reffile

		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'�򿪰����ĵ�'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '���ξ���ģ��', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('�Ҳ��������ĵ�',title='����')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'�˳�����'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'�˳�����'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF
END

PRO GEO_CORRECTION,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'���м��ξ���'

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;���������
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='���ξ���' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1, /modal)

;�����ӿ��
	FILE_BASE = Widget_Base(TLB_BASE,/COLUMN,UNAME='FILE_BASE',/FRAME)

;-----------------------------------------------------------------------------------------------------
;ѡ���ļ����
	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='����Ӱ��',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'����Ӱ���ļ�'}
		widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	REF_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='REF_BASE',/align_right)
		REF_FIELD = CW_FIELD(REF_BASE,UNAME='REF_FIELD',TITLE='�ο�Ӱ��',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		REF_BUTTON = Widget_Button(REF_BASE,UNAME='REF_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:REF_FIELD, filter:'*.tif', title:'�ο�Ӱ���ļ�'}
		widget_control,REF_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE',/align_right)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='���Ӱ��',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:OUTPUT_FIELD, title:'���Ӱ���ļ���'}
		widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY
;-----------------------------------------------------------------------------------------------------
;ָ���������
	CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,SCR_XSIZE=328,SCR_YSIZE=32,/row,space=53,XPAD=15)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='ȷ��',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='����',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='�˳�',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
;-----------------------------------------------------------------------------------------------------
	Widget_Control,TLB_BASE,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD , $
					REF_FIELD	:	REF_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=PSTATE

	Xmanager,'GEO_CORRECTION',TLB_BASE,/NO_BLOCK

END