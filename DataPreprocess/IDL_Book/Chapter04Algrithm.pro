; Chapter04Algrithm.pro
PRO  Chapter04Algrithm
yn = " "
REPEAT  BEGIN
����READ,  PROMPT = "��ѡ�����㷽ʽ��1- ��, 2- ��, 3- �� ����", n
����IF  n EQ 1 ||  n EQ 2 ||  n EQ 3  THEN BEGIN
��������READ,  PROMPT = "������X = ?", x
��������READ,  PROMPT = "������Y = ?", y
��������CASE  n  OF
������������1: PRINT, x, '+', y, '=', Chapter04Plus(x, y)
������������2: PRINT, x, '-', y, '=', Chapter04Minus(x, y)
������������3: PRINT, x, '*', y, '=', Chapter04Product(x, y)
��������ENDCASE

����ENDIF ELSE BEGIN
��������PRINT,  " ѡ�����㷽ʽ����"
����ENDELSE
����READ, PROMPT = "����������?��Y or N����",  yn
����yn = STRUPCASE(yn)
ENDREP  UNTIL  yn  NE  "Y"
END