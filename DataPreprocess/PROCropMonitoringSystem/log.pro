pro updatesingal, context, task		;context���ݵ���log�ļ��е����м�¼��task���ݵ��ǵ������й���ģ������
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
	if total(matchresult) eq 1 then begin	;����log�ļ���ģ����������Index���и�ֵ
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

	case task of	;����ģ����ʾ��ɫ
		'���̼��-ͳ��'    :WIDGET_CONTROL, modals.PRO_SAT, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
		'���̼��-�����ع�':WIDGET_CONTROL, modals.PRO_HANTS, SET_VALUE='.\Image\�����ع�'+Index+'.bmp', /BITMAP
		'���̼��-����'    :WIDGET_CONTROL, modals.PRO_SUM, SET_VALUE='.\Image\����'+Index+'.bmp', /BITMAP
		'ʵʱ���-��ֵ�ּ�':WIDGET_CONTROL, modals.RT_CLASSIFY, SET_VALUE='.\Image\��ֵ�ּ�'+Index+'.bmp', /BITMAP
		'ʵʱ���-�ռ�ͳ��':WIDGET_CONTROL, modals.RT_SAT, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
		'ʵʱ���-����'    :WIDGET_CONTROL, modals.RT_SUM, SET_VALUE='.\Image\����'+Index+'.bmp', /BITMAP
		'���Ƽ��-�������':WIDGET_CONTROL, modals.ZS_RESULT, SET_VALUE='.\Image\�������'+Index+'.bmp', /BITMAP
		'��ֲ����-�ռ�ͳ��':WIDGET_CONTROL, modals.MJ_CS_TJ, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
		'��ֲ����-��������':WIDGET_CONTROL, modals.MJ_CS_WT, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
		'��ֲ�ṹ-�������':WIDGET_CONTROL, modals.MJ_JG_RK, SET_VALUE='.\Image\�������'+Index+'.bmp', /BITMAP
		'��ֲ�ṹ-��������':WIDGET_CONTROL, modals.MJ_JG_WT, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
		'�������-�������':WIDGET_CONTROL, modals.MJ_AREA, SET_VALUE='.\Image\�������'+Index+'.bmp', /BITMAP
		'����Ԥ��-���Ʋ���':WIDGET_CONTROL, modals.DC_TREND, SET_VALUE='.\Image\���Ʋ���'+Index+'.bmp', /BITMAP
		'����Ԥ��-��������':WIDGET_CONTROL, modals.DC_WAVE, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
		'����Ԥ��-�����ں�':WIDGET_CONTROL, modals.DC_FUSE, SET_VALUE='.\Image\�����ں�'+Index+'.bmp', /BITMAP
		'��������'         :WIDGET_CONTROL, modals.CL_YIELD, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
		'ũ������-�����ֵ':WIDGET_CONTROL, modals.NQ_CZ, SET_VALUE='.\Image\�����ֵ'+Index+'.bmp', /BITMAP
		'ũ������-�Աȷ���':WIDGET_CONTROL, modals.NQ_DB, SET_VALUE='.\Image\�Աȷ���'+Index+'.bmp', /BITMAP
		'����ָ��-ָ����ȡ':WIDGET_CONTROL, modals.FZ_TQ, SET_VALUE='.\Image\ָ����ȡ'+Index+'.bmp', /BITMAP
		'����ָ��-�ռ�ͳ��':WIDGET_CONTROL, modals.FZ_TJ, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
		'all' : begin
			Index = '0'
			WIDGET_CONTROL, modals.PRO_SAT, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.PRO_HANTS, SET_VALUE='.\Image\�����ع�'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.PRO_SUM, SET_VALUE='.\Image\����'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.RT_CLASSIFY, SET_VALUE='.\Image\��ֵ�ּ�'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.RT_SAT, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.RT_SUM, SET_VALUE='.\Image\����'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.ZS_RESULT, SET_VALUE='.\Image\�������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_CS_TJ, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_CS_WT, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_JG_RK, SET_VALUE='.\Image\�������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_JG_WT, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.MJ_AREA, SET_VALUE='.\Image\�������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.DC_TREND, SET_VALUE='.\Image\���Ʋ���'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.DC_WAVE, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.DC_FUSE, SET_VALUE='.\Image\�����ں�'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.CL_YIELD, SET_VALUE='.\Image\��������'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.NQ_CZ, SET_VALUE='.\Image\�����ֵ'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.NQ_DB, SET_VALUE='.\Image\�Աȷ���'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.FZ_TQ, SET_VALUE='.\Image\ָ����ȡ'+Index+'.bmp', /BITMAP
			WIDGET_CONTROL, modals.FZ_TJ, SET_VALUE='.\Image\�ռ�ͳ��'+Index+'.bmp', /BITMAP
		end
   endcase
end

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

pro log, task, state	;��log�ļ��м�¼ģ��ļ�¼���
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
		PRINTF, lun, 'logfile,�����1,δ���-1,�������0'
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