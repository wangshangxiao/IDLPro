; Chapter04ScoreGrade.pro
PRO  Chapter04ScoreGrade
m = " "
READ, PROMPT="������ȼ���A , B, C, D, or E����", m
m = STRUPCASE(m)
SWITCH  m  of
       'A' :
       'B' :
       'C' :
       'D' : BEGIN
                PRINT, "Score >= 60�� ", m + "����" ,  "ͨ�����ԣ�"
                BREAK
              END
        'E' :  PRINT, " Score < 60�� ", m  + "����" ,  "û��ͨ�����ԣ�"
ENDSWITCH
END