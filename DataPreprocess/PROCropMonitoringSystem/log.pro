pro updatesingal, context, task		;context传递的是log文件中的所有记录，task传递的是当天运行过的模块名称
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	Result = BIN_DATE()
	Result = strtrim(string(Result),2)
	date = Result[0]+'-'+Result[1]+'-'+Result[2]
	head = date+','+task

	Index = '0'

	matchresult = strmatch(context, head+'*')
	if total(matchresult) eq 1 then begin	;根据log文件中模块完成情况对Index进行赋值
		text = context[WHERE(STRMATCH(context, head+'*') eq 1)]
		temp = strsplit(text, ',', /EXTRACT)
		if(strtrim(temp[2],2) eq -1) then Index = '0' $
		else if(strtrim(temp[2],2) eq 1) then Index = '2' $
		else if(strtrim(temp[2],2) eq 0) then Index = '1'
	endif else begin
		Index = '0'
	endelse

	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
	WIDGET_CONTROL,BASE_TOP,GET_UVALUE=diaoduID
	WIDGET_CONTROL, diaoduID.diaodu_Modal_BASE, get_uvalue = modals

	case task of	;更新模块显示颜色
		'过程监测-统计'    :WIDGET_CONTROL, modals.PRO_SAT, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
		'过程监测-曲线重构':WIDGET_CONTROL, modals.PRO_HANTS, SET_VALUE='.\Image\曲线重构'+Index+'.bmp', /BITMAP
		'过程监测-汇总'    :WIDGET_CONTROL, modals.PRO_SUM, SET_VALUE='.\Image\汇总'+Index+'.bmp', /BITMAP
		'实时监测-差值分级':WIDGET_CONTROL, modals.RT_CLASSIFY, SET_VALUE='.\Image\差值分级'+Index+'.bmp', /BITMAP
		'实时监测-空间统计':WIDGET_CONTROL, modals.RT_SAT, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
		'实时监测-汇总'    :WIDGET_CONTROL, modals.RT_SUM, SET_VALUE='.\Image\汇总'+Index+'.bmp', /BITMAP
		'长势监测-结果分析':WIDGET_CONTROL, modals.ZS_RESULT, SET_VALUE='.\Image\结果分析'+Index+'.bmp', /BITMAP
		'种植成数-空间统计':WIDGET_CONTROL, modals.MJ_CS_TJ, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
		'种植成数-成数外推':WIDGET_CONTROL, modals.MJ_CS_WT, SET_VALUE='.\Image\成数外推'+Index+'.bmp', /BITMAP
		'种植结构-数据入库':WIDGET_CONTROL, modals.MJ_JG_RK, SET_VALUE='.\Image\数据入库'+Index+'.bmp', /BITMAP
		'种植结构-比例外推':WIDGET_CONTROL, modals.MJ_JG_WT, SET_VALUE='.\Image\比例外推'+Index+'.bmp', /BITMAP
		'面积估算-面积估算':WIDGET_CONTROL, modals.MJ_AREA, SET_VALUE='.\Image\面积估算'+Index+'.bmp', /BITMAP
		'单产预测-趋势产量':WIDGET_CONTROL, modals.DC_TREND, SET_VALUE='.\Image\趋势产量'+Index+'.bmp', /BITMAP
		'单产预测-波动产量':WIDGET_CONTROL, modals.DC_WAVE, SET_VALUE='.\Image\波动产量'+Index+'.bmp', /BITMAP
		'单产预测-产量融合':WIDGET_CONTROL, modals.DC_FUSE, SET_VALUE='.\Image\产量融合'+Index+'.bmp', /BITMAP
		'产量估算'         :WIDGET_CONTROL, modals.CL_YIELD, SET_VALUE='.\Image\产量估算'+Index+'.bmp', /BITMAP
		'农气分析-气象插值':WIDGET_CONTROL, modals.NQ_CZ, SET_VALUE='.\Image\气象插值'+Index+'.bmp', /BITMAP
		'农气分析-对比分析':WIDGET_CONTROL, modals.NQ_DB, SET_VALUE='.\Image\对比分析'+Index+'.bmp', /BITMAP
		'复种指数-指数提取':WIDGET_CONTROL, modals.FZ_TQ, SET_VALUE='.\Image\指数提取'+Index+'.bmp', /BITMAP
		'复种指数-空间统计':WIDGET_CONTROL, modals.FZ_TJ, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
		'all' : begin
			Index = '0'
			WIDGET_CONTROL, modals.PRO_SAT, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.PRO_HANTS, SET_VALUE='.\Image\曲线重构'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.PRO_SUM, SET_VALUE='.\Image\汇总'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.RT_CLASSIFY, SET_VALUE='.\Image\差值分级'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.RT_SAT, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.RT_SUM, SET_VALUE='.\Image\汇总'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.ZS_RESULT, SET_VALUE='.\Image\结果分析'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_CS_TJ, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_CS_WT, SET_VALUE='.\Image\成数外推'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_JG_RK, SET_VALUE='.\Image\数据入库'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_JG_WT, SET_VALUE='.\Image\比例外推'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_AREA, SET_VALUE='.\Image\面积估算'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.DC_TREND, SET_VALUE='.\Image\趋势产量'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.DC_WAVE, SET_VALUE='.\Image\波动产量'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.DC_FUSE, SET_VALUE='.\Image\产量融合'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.CL_YIELD, SET_VALUE='.\Image\产量估算'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.NQ_CZ, SET_VALUE='.\Image\气象插值'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.NQ_DB, SET_VALUE='.\Image\对比分析'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.FZ_TQ, SET_VALUE='.\Image\指数提取'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.FZ_TJ, SET_VALUE='.\Image\空间统计'+Index+'.bmp', /BITMAP
		end
   endcase
end

pro updateall	;获得log文件中模块的完成情况
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	logfile = '.\text\log.txt'
	if file_test(logfile) eq 0 then begin ;若log文件不存在，则新建一个log文件
		OPENW, lun, logfile, /GET_LUN
		PRINTF, lun, 'logfile,已完成1,未完成-1,部分完成0'
		CLOSE, lun
		FREE_LUN, lun
	endif

	nlines = file_lines(logfile)
	context = strarr(nlines)
	OPENR, lun, logfile, /GET_LUN
	READF, lun, context ;把log文件中的内容读到context数组中
	CLOSE, lun
	FREE_LUN, lun

	Result = BIN_DATE()
	Result = strtrim(string(Result),2)
	date = Result[0]+'-'+Result[1]+'-'+Result[2] ;提取当前日期

	matchresult = strmatch(context, date+'*')
	if total(matchresult) ge 1 then begin
		text = context[WHERE(STRMATCH(context, date+'*') eq 1)] ;找寻log文件中与当前日期匹配的记录
		for i=0, n_elements(text)-1 do begin
			temp = strsplit(text[i], ',', /EXTRACT)		;提取去','后的记录内容
			temp = strtrim(temp,2)
			task = temp[1] ;提取模块名称
			updatesingal, context, task
		endfor
	endif
end

pro log, task, state	;在log文件中记录模块的记录情况
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	logfile = '.\text\log.txt'
	if file_test(logfile) eq 0 then begin
		OPENW, lun, logfile, /GET_LUN
		PRINTF, lun, 'logfile,已完成1,未完成-1,部分完成0'
		CLOSE, lun
		FREE_LUN, lun
	endif

	nlines = file_lines(logfile)
	context = strarr(nlines)
	OPENR, lun, logfile, /GET_LUN
	READF, lun, context
	CLOSE, lun
	FREE_LUN, lun

;	Julian = SYSTIME(/JULIAN)
;	CALDAT, Julian, Month, Day, Year, Hour, Minute, Second
;	print, Julian, Month, Day, Year, Hour, Minute, Second

	Result = BIN_DATE()
	Result = strtrim(string(Result),2)
	date = Result[0]+'-'+Result[1]+'-'+Result[2]
	head = date+','+task
	text = head+','+strtrim(state,2)

	matchresult = strmatch(context, head+'*')
	if total(matchresult) eq 1 then begin
		context[WHERE(STRMATCH(context, head+'*') eq 1)] = text
	endif else begin
		context = [[context], text]
	endelse

	OPENW, lun, logfile, /GET_LUN
	PRINTF, lun, context, FORMAT='(%"%s")'
	CLOSE, lun
	FREE_LUN, lun

	updatesingal, context, task
end