; Chapter04MSum.pro
PRO  Chapter04MSum
yn = " "
WHILE  1  DO  BEGIN
    READ,  PROMPT = "������X = ?", x
    READ,  PROMPT = "������Y = ?", y
    PRINT,  x ,  " + " ,  y ,  " = " ,  x + y
    READ,  PROMPT = "���������𣿣�Y or N����",  yn
    yn = STRUPCASE(yn)
    IF  yn EQ "Y"  THEN  BEGIN
       CONTINUE
    ENDIF  ELSE  BEGIN
       BREAK
    ENDELSE
ENDWHILE
END