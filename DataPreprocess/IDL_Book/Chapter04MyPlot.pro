; Chapter04MyPlot.pro
PRO Chapter04MyPlot, x, y, Name=Name, _EXTRA = EXTRA_KEYWORDS
IF (N_PARAMS() NE 2) THEN MESSAGE , 'ʹ�ø�ʽΪ��Chapter04MyPlot, x, y '
IF (N_ELEMENTS(x) EQ 0) THEN MESSAGE , '����x û�ж��壡'
IF (N_ELEMENTS(y) EQ 0) THEN MESSAGE , '����y û�ж��壡'
IF (N_ELEMENTS(name) EQ 0) THEN Name = ' Happy You '
PLOT, x, y, _EXTRA = EXTRA_KEYWORDS
Date = SYSTIME()
XYOUTS, 0.0, 0.0, Name, align = 0.0, /NORMAL
XYOUTS, 1.0, 0.0, Date, align = 1.0, /NORMAL
END