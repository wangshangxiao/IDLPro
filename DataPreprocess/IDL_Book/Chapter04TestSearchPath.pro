; Chapter04TestSearchPath.pro
PRO  Chapter04TestSearchPath
����MyPath = 'D:\IDL\JTIDL60'
����MyExist = STRPOS( !Path, MyPath)
����IF  MyExist NE -1  THEN  BEGIN
��������PRINT,  'Ŀ¼������·���У�'
��������PRINT,  !PATH
����ENDIF  ELSE  BEGIN
��������PRINT,  'Ŀ¼��������·���У�'
��������yn = ' '
��������READ,  PROMPT = "���Ŀ¼�𣿣�Y or N����",  yn
��������yn = STRUPCASE(yn)
��������IF  yn EQ "Y"  THEN  BEGIN
������������!PATH = !PATH + ';' + MyPath
������������PRINT, 'Ŀ¼�Ѿ���ӵ�����·���У�'
������������PRINT,  !PATH
��������ENDIF  ELSE  BEGIN
������������PRINT, 'Ŀ¼û����ӵ�����·���У�'
��������ENDELSE
����ENDELSE
END
