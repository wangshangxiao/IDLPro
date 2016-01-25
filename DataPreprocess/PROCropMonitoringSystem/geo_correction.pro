;几何纠正模块
;钮立明	2010年5月12日	修改杨雷东的程序得到

;空间域颜色增强，使用分段线性拉伸算法，获取最小1%和最大1%的界限
function getminmaxvalue, data
	;计算点数
	dcounts = double(n_elements(data))
	;最小/最大值的位置,采用上下 1% 拉伸
	index1 = long(dcounts * 0.01d)
	index2 = long(dcounts * 0.99d)
	;把数据排序,不去除重复值
	adata = data[sort(data)]
	;返回相应位置的值
	return, adata[[index1, index2]]
end

function WARP_TRI_YLD, xo, yo, xi, yi, im_in, OUTPUT_SIZE = output_size
	s = SIZE(im_in)
	if s[0] ne 2 then print, '几何纠正：数据格式不正确'
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

function TMCorrectionRunByInfo, info
;获取文件信息和原始影像的goetiff信息
	r1 = query_tiff(info.orifilename, oriinfo)
	r2 = query_tiff(info.reffilename, refinfo, geotiff=ref_geotiff)

	pixelsize=ref_geotiff.MODELPIXELSCALETAG[0:1]

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
	geo_x = ref_geotiff.MODELTIEPOINTTAG[3] + pixelsize[0] * ref_x0
	geo_y = ref_geotiff.MODELTIEPOINTTAG[4] + pixelsize[1] * ref_ytop
	;调用几何纠正的函数
	;临时文件
	tempfile = info.outfilename + '.tmp'
	openw, templun, tempfile, /get_lun
	;记录读写位置
	fileposlist = lonarr(oriinfo.channels)
	;逐个通道进行纠正

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

	;如果有roi文件，则使用roi文件进行切割
	if file_test(info.roifile) eq 1 then begin
		err = do_subset(info.outfilename, info.roifile, info.outfilename)
		if err eq 1 then begin
			print, '裁切失败：输入文件与ROI文件没有重合的区域！'
		endif
	endif

	;返回
	return, 1
end

function TMCorrectionGetRunInfo, event
	;全局ID列表
	widget_control, event.top, get_uvalue = idlist

	;控制点列表
	widget_control, idlist.plist, get_uvalue = puv
	if puv.count eq 0 then begin
		msg = dialog_message('没有控制点列表，无法运行几何纠正功能！', title='几何纠正', /infor)
		return, 0
	endif
	;控制点的个数不能少于六个
	if puv.count lt 6 then begin
		msg = dialog_message('至少要选择6个更多的控制点，否则无法运行几何纠正功能！', title='几何纠正', /infor)
		return, 0
	endif

	;原始影像文件信息
	widget_control, idlist.ori_id, get_uvalue = ori_widlist
	widget_control, ori_widlist.drawid, get_uvalue = oridrawuvalue
	;参考影像文件信息
	widget_control, idlist.ref_id, get_uvalue = ref_widlist
	widget_control, ref_widlist.drawid, get_uvalue = refdrawuvalue

	;设定输出文件，存放在几何纠正预处理的目录之下，名字在原始影像后加geo.tif
	dirs = idlist.outputfile
	outfile = dirs + file_basename(oridrawuvalue.filename, '.tif') + '_geo.tif'
	;ROI文件
	rofile = ''
	if size(idlist.roifile, /type) eq 7 then roifile = idlist.roifile
	;设置要保存的变量
	tmcorrection_info = { $
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

;几何纠正几何纠正处理窗口关闭事件
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

;几何纠正进行几何纠正的功能，响应面板上面的按钮
pro TMCorrectionRun, event
	;从界面参数获取结构体变量
	info = TMCorrectionGetRunInfo(event)
	if size(info, /type) ne 8 then return

	;以参数结构体为变量，调用运行几何纠正的功能
	re = TMCorrectionRunByInfo(info)

	;显示完成的信息
	if re eq 1 then $
	msg = dialog_message('完成几何纠正操作！', title='几何纠正', /info)
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

;对控制点的添加、删除、转到和设置颜色操作
pro TMCorrectionControlPoint, event

	FORWARD_FUNCTION TMCORRECTIONGETFORMATEDTABLE
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
		msg = dialog_message('没有原始影像，无法完成操作！', title='几何纠正', /infor)
		return
	endif
	;获取参考影像的打开情况
	widget_control, idlist.ref_id, get_uvalue = refuvalue
	widget_control, refuvalue.drawid, get_uvalue = refdrawuvalue
	if size(refdrawuvalue, /type) ne 8 then begin
		msg = dialog_message('没有参考影像，无法完成操作！', title='几何纠正', /infor)
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
						' 的控制点的添加操作！', title='几何纠正', /info)
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
				msg = dialog_message('没有控制点数据，无法删除！', title='几何纠正', /information)
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
				msg = dialog_message('当前方案表为空，不能保存！', title='几何纠正', /info)
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
				msg = dialog_message('当前方案表不为空，是否继续？', title='几何纠正', /question)
				if msg eq 'No' then return
			endif
			;
			filename = dialog_pickfile(title='选择控制点列表文件', filter='*.dat', default_extension='dat')
			if filename[0] eq '' then return
			if file_test(filename[0]) ne 1 then begin
				msg = dialog_message('加载控制点列表文件失败！', title='几何纠正', /question)
				return
			endif
			;读取文件行数
			openr, lun, filename[0], /get_lun
			table_rows = file_lines(filename[0])
			if table_rows eq 0 then begin
				msg = dialog_message('文件为空，加载失败！', title='几何纠正', /info)
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
	;设置坐标的值，把它添加到表中去
	puv.coordinate[widlist.pindex, puv.active_row] = strtrim(x, 2)
	puv.coordinate[widlist.pindex+1, puv.active_row] = strtrim(y, 2)
	;指示是否自动添加点
	isAutoadd = 0
	;判断是否要自动加入另一个点
	autoadd = widget_info(idlist.btnautoadd, /button_set)
	;至少三个点以上才能自动添加点
	if puv.count gt 3 then begin
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

		if autoadd eq 1 then begin
			puv.coordinate[newindex, puv.active_row] = strtrim(newx, 2)
			puv.coordinate[newindex+1, puv.active_row] = strtrim(newy, 2)
			isAutoadd = 1
		endif
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

	FORWARD_FUNCTION TMCORRECTIONGETFORMATEDTABLE

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

pro TMCorrection_open, idname, filename, filetype
	widget_control, idname, get_uvalue = idlist
	if filetype eq 'orifile' then begin
		;打开原始影像
		qr = query_tiff(filename, info)
		;判断是否是多通道的(不再提供支持单通道TIFF的功能)
		if info.channels eq 0 then begin
			msg = dialog_message('原始影像通道数不正确', title='几何纠正')
			return
		endif
		;保存影像长宽的最大值
		maxsize = max([info.dimensions[0:1]])
		;工作类型
		workid = idlist.ori_id
		;读取参考影像
		if info.channels ge 4 then begin
			data432 = read_tiff(filename, channels=[1,2,3])
		endif else begin
			data432 = read_tiff(filename, channels=[0,1,2])
		endelse
	endif

	;打开参考影像
	if filetype eq 'reffile' then begin
		qr = query_tiff(filename, info, geotiff=geotiff)
		;判断是否是多通道的(不再提供支持单通道TIFF的功能)
		if info.channels eq 0 then begin
			msg = dialog_message('参考影像通道数不正确', title='几何纠正')
			return
		endif
		if size(geotiff, /type) ne 8 then begin
			msg = dialog_message('参考影像的投影无效！', title='几何纠正')
			return
		endif
		;保存影像长宽的最大值
		maxsize = max([info.dimensions[0:1]])
		;工作类型
		workid = idlist.ref_id
		;读取参考影像
		if info.channels ge 4 then begin
			data432 = read_tiff(filename, channels=[1,2,3])
		endif else begin
			data432 = read_tiff(filename, channels=[0,1,2])
		endelse
	endif

	;分别计算4/3/2通道的拉伸参数
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

PRO TMCorrection_event,event

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
		widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='help_button'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '几何纠正模块', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end
		else:
	endcase


END

;几何纠正处理窗口
pro TMCorrection, event, datafile, reffile, roifile=roifile
	tlb = widget_base(group_leader=event.top, /modal, title = '几何纠正', /col, space=0, tlb_frame_attr=1)

	scr_dims = GET_SCREEN_SIZE()
	draw_size = (scr_dims[0]/2)-10
	base1 = widget_base(tlb, /row, xpad=0,space=5)
	base2 = widget_base(base1, /col, xpad=0, ypad=0,/base_align_center,space=0)
		oridraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=draw_size, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			base4 = widget_base(base3, /row,space=1,/base_align_center)
				ori_label = widget_label(base4, value='原始影像', /align_left)
				o_xid = cw_field(base4, title='X坐标:', value='0', xsize=12, /noedit)
				o_yid = cw_field(base4, title='Y坐标:', value='0', xsize=12, /noedit)
			ori_toolBar = widget_base(base3,/exclusive,/row,/base_align_center)
				btn1 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\pointer.bmp', /bitmap, xsize=20, ysize=20, tooltip='选择')
				btn2 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='漫游')
				btn3 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_in.bmp', /bitmap, xsize=20, ysize=20, tooltip='放大')
				btn4 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_out.bmp', /bitmap, xsize=20, ysize=20, tooltip='缩小')
				btn5 = widget_button(ori_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\globe_7.bmp', /bitmap, xsize=20, ysize=20, tooltip='查看全图')

			widget_control, ori_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ori_label, isAdd:0}
	base2 = widget_base(base1, /col, xpad=0, ypad=0,/base_align_center,space=0)
		refdraw = widget_draw(base2, Graphics_Level = 2, retain = 2, xsize=draw_size, ysize=480, $
			event_pro='TMCorrectionDrawEvent', /button_events, /motion_events)
		base3 = widget_base(base2, /row, space=0)
			base4 = widget_base(base3, /row,space=1)
				ref_label = widget_label(base4, value='参考影像', /align_left)
				r_xid = cw_field(base4, title='X坐标:', value='0', xsize=12, /noedit)
				r_yid = cw_field(base4, title='Y坐标:', value='0', xsize=12, /noedit)
			ref_toolBar = widget_base(base3, /exclusive, /row)
				btn1 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\pointer.bmp', /bitmap, xsize=20, ysize=20, tooltip='选择')
				btn2 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\hand.bmp', /bitmap, xsize=20, ysize=20, tooltip='漫游')
				btn3 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_in.bmp', /bitmap, xsize=20, ysize=20, tooltip='放大')
				btn4 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\zoom_out.bmp', /bitmap, xsize=20, ysize=20, tooltip='缩小')
				btn5 = widget_button(ref_toolBar, event_pro='TMCorrectionZoomEvent', value='.\Image\globe_7.bmp', /bitmap, xsize=20, ysize=20, tooltip='查看全图')

			widget_control, ref_toolBar, set_uvalue = {btnlist:[btn1, btn2, btn3, btn4, btn5], status:0, workid : ref_label, isAdd:0}

	base1 = widget_base(tlb, /row,space=10,ypad=0)
		table_base = widget_base(base1,/column,/base_align_center,ypad=1)
		plist = widget_table(table_base, xsize=5, ysize=1, column_label=['序号','原始影像X坐标','原始影像Y坐标','参考影像X坐标','参考影像Y坐标'],/frame, $
			column_widths=[50,106,106,106,106], scr_xsize=495, Y_SCROLL_SIZE=5, event_pro='TMCorrectionTableEvent', /all_events, /no_row_headers, $
			uvalue={count:0, active_row:-1})
		base2 = widget_base(base1, /col,xpad=10,ypad=1,space=1)
			base3 = widget_base(base2, /row, space=10,/frame,xpad=5)
				lab = widget_label(base3, value='  X误差：')
				x_error = widget_text(base3, xsize=20)
				lab = widget_label(base3, value='  Y误差：')
				y_error = widget_text(base3, xsize=20)
			base31 = widget_base(base2, /row, space=10,/frame)
				base32 = widget_base(base31, /column, space=2)
					base3 = widget_base(base32, /row, space=80,ypad=0)
						controlfile = widget_label(base3, value='  控制点：', uvalue = '')
						base4 = widget_base(base3, /row, space=10,ypad=0, /nonexcl, /align_right)
							btnautoadd = widget_button(base4, value='自动匹配控制点')
					base3 = widget_base(base32, /row, space=10,ypad=0)
						btn = widget_button(base3, value='添加', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='add')
						btn = widget_button(base3, value='删除', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='delete')
						btn = widget_button(base3, value='保存', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='savelist')
						btn = widget_button(base3, value='加载', xsize=60, ysize=23, event_pro='TMCorrectionControlPoint', uname='loadlist')
			base3 = widget_base(base2, /row, xpad=40,ypad=1,space=40,/frame)
				btn = widget_button(base3, value='开始纠正', xsize=80, ysize=23, event_pro='TMCorrectionRun')
;				btn = widget_button(base3, value='保存运行信息', xsize=90, ysize=23, event_pro='TMCorrectionSaveRunInfo')
				btn = widget_button(base3, value='帮助', xsize=80, ysize=23,uname='help_button')
				btn = widget_button(base3, value='关闭', xsize=80, ysize=23, event_pro='TMCorrectionOnClose')
	;设置原始影像的自定义变量
	widget_control, ori_label, set_uvalue = {drawid:oridraw, toolBar:ori_toolBar, xid:o_xid, yid:o_yid, pindex:1}
	;设置原始影像的自定义变量
	widget_control, ref_label, set_uvalue = {drawid:refdraw, toolBar:ref_toolBar, xid:r_xid, yid:r_yid, pindex:3}

	;检查ROI文件是否存在
	if keyword_set(roifile) eq 0 then roifile = ''

	Widget_Control,event.top,get_uvalue=PSTATE
	Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile
	Widget_Control,(*PSTATE).REF_FIELD,get_value=reffile
	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	;设置全局的自定义变量
	widget_control, tlb, /realize, set_uvalue={ori_id:ori_label, ref_id:ref_label, $
		plist:plist, btnautoadd:btnautoadd, controlfile : controlfile, roifile:roifile , $
		inputfile	:	inputfile , $
		reffile	:	reffile , $
		outputfile	:	outputfile , $
		x_error	:	x_error , $
		y_error	:	y_error }

	;加载原始文件
	TMCorrection_open, tlb, datafile, 'orifile'
	;加载参考文件
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
		;打开输入文件的控件
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输入影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='REF_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开参考影像'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		pick_file_path, Event
      		common_log,'打开输出影像'
		end

		;指令控制区域的响应
		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				;运行程序
				Widget_Control,(*PSTATE).INPUT_FIELD,get_value=datafile
				Widget_Control,(*PSTATE).REF_FIELD,get_value=reffile
				Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outfile

				if file_test(datafile) ne 1 then begin
					CAUTION = DIALOG_MESSAGE('未输入待纠正的影像或影像不正确！',title='警告')
					return
				endif

				if file_test(reffile) ne 1 then begin
					CAUTION = DIALOG_MESSAGE('未设置参考影像或影像不正确！',title='警告')
					return
				endif

				if file_test(outfile) ne 1 then begin
					CAUTION = DIALOG_MESSAGE('未设置输出路径或输出路径不存在！',title='警告')
					return
				endif

				common_log,'进行运算'

				TMCorrection, event, datafile, reffile

		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '几何纠正模块', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'退出程序'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出程序'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF
END

PRO GEO_CORRECTION,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'进行几何纠正'

	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;设置主框架
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='几何纠正' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1, /modal)

;设置子框架
	FILE_BASE = Widget_Base(TLB_BASE,/COLUMN,UNAME='FILE_BASE',/FRAME)

;-----------------------------------------------------------------------------------------------------
;选择文件组件
	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='输入影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
		widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	REF_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='REF_BASE',/align_right)
		REF_FIELD = CW_FIELD(REF_BASE,UNAME='REF_FIELD',TITLE='参考影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		REF_BUTTON = Widget_Button(REF_BASE,UNAME='REF_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:REF_FIELD, filter:'*.tif', title:'参考影像文件'}
		widget_control,REF_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE',/align_right)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='输出影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:OUTPUT_FIELD, title:'输出影像文件夹'}
		widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY
;-----------------------------------------------------------------------------------------------------
;指令控制区域
	CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,SCR_XSIZE=328,SCR_YSIZE=32,/row,space=53,XPAD=15)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='确定',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
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