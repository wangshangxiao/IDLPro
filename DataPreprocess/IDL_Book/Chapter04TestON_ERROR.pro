; Chapter04TestON_ERROR.pro
PRO  Chapter04TestON_ERROR
����Number = 0D
����ValidFlag = 0
����WHILE ValidFlag EQ 0 DO BEGIN
��������ON_IOERROR, ErrorLabel
��������READ, PROMPT=' ������һ������' , Number
��������ValidFlag = 1
��������PRINT, '�����������' , Number , '��Ч��'
��������ErrorLabel: IF  ~ ValidFlag THEN BEGIN
����������������������PRINT, '�������������Ч��'
������������������ENDIF
����ENDWHILE
END

