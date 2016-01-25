pro updateall	;���log�ļ���ģ���������
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
	if file_test(logfile) eq 0 then begin ;��log�ļ������ڣ����½�һ��log�ļ�
		OPENW, lun, logfile, /GET_LUN
		PRINTF, lun, 'logfile,�����1,δ���-1,�������0'
		CLOSE, lun
		FREE_LUN, lun
	endif

	nlines = file_lines(logfile)
	context = strarr(nlines)
	OPENR, lun, logfile, /GET_LUN
	READF, lun, context ;��log�ļ��е����ݶ���context������
	CLOSE, lun
	FREE_LUN, lun

	Result = BIN_DATE()
	Result = strtrim(string(Result),2)
	date = Result[0]+'-'+Result[1]+'-'+Result[2] ;��ȡ��ǰ����

	matchresult = strmatch(context, date+'*')
	if total(matchresult) ge 1 then begin
		text = context[WHERE(STRMATCH(context, date+'*') eq 1)] ;��Ѱlog�ļ����뵱ǰ����ƥ��ļ�¼
		for i=0, n_elements(text)-1 do begin
			temp = strsplit(text[i], ',', /EXTRACT)		;��ȡȥ','��ļ�¼����
			temp = strtrim(temp,2)
			task = temp[1] ;��ȡģ������
			updatesingal, context, task
		endfor
	endif
end