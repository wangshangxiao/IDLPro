; Chapter04MMul.pro
PRO  Chapter04MMul
yn = " "
REPEAT  BEGIN
    READ,  PROMPT = "������X = ?", x
    READ,  PROMPT = "������Y = ?", y
    PRINT,  x ,  " * " ,  y ,  " = " ,  x  *  y
    READ,  PROMPT = "���������𣿣�Y or N����",  yn
    yn = STRUPCASE(yn)
ENDREP  UNTIL  yn EQ "N"
END