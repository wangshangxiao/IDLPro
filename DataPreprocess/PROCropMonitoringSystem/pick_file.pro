PRO PICK_FILE,event

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		;Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control,event.id,get_uvalue=pfinfo
	field_id = pfinfo.field_id
	filter = pfinfo.filter
	title = pfinfo.title
	filename = DIALOG_PICKFILE(title=title, filter=filter,dialog_parent=event.top);envi_pickfile

	if (filename eq '') then return else begin
		widget_control,field_id,set_value=filename
	end
END

PRO PICK_FILE_PATH,event

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		;Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control,event.id,get_uvalue=pfinfo
	field_id = pfinfo.field_id
	title = pfinfo.title
	filepath = DIALOG_PICKFILE(title=title,/DIRECTORY,dialog_parent=event.top);envi_pickfile

	if (filepath eq '') then return else begin
		widget_control,field_id,set_value=filepath
	end
END