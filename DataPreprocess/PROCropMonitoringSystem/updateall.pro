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