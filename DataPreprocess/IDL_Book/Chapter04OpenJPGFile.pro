; Chapter04OpenJPGFile.pro
PRO  Chapter04OpenJPGFile
����FileName = PICKFILE()
����Catch ,EerrorVariable
����IF EerrorVariable NE 0 THEN BEGIN
��������CATCH, / CANCEL
��������PRINT, ' �ļ����ͷ�JPGͼ���ļ������������򿪣�'
��������RETURN
����ENDIF
����READ_JPEG , FileName , ImageData, TRUE=1
����WINDOW , 10, RETAIN= 2, TITLE='My Window', XSIZE=640, YSIZE=480
����WSET , 10
����TV, ImageData, TRUE=1
����CATCH, /CANCEL
END