
PRO OpenFile
  CATCH, Error_status
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    PRINT, 'Error index: ', Error_status
    PRINT, 'Error message: ', !ERROR_STATE.MSG
    ;Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
    help, /last_message, output=errtext
    Result = DIALOG_MESSAGE(errtext, /CENTER)
    CATCH, /CANCEL

  ENDIF

  filter = ['*.tif','*.jpg','*.bmp']
  title = '´ò¿ªÓ°Ïñ'
  filename = DIALOG_PICKFILE(title=title, filter=filter);
end