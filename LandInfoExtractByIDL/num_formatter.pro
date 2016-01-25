FUNCTION NUM_FORMATTER, inPut, delZero = delZero, decimals = decimals

  ;����inPut ��ΪDouble��

  ;����
  ;delZero : ���Ϊ1���򷵻ر�����Ч���ֵĽ��    �÷��� r = NUM_FORMATTER(inPut, /delZero)
  ;decimals : ����С�����λ                                �÷��� r = NUM_FORMATTER(inPut, decimals = 2)

  ;����ֵΪ�ַ���


  IF N_ELEMENTS(inPut) EQ 0 THEN MESSAGE, 'A number must be passed as an argument to the function.'
  
  IF N_ELEMENTS(delZero) EQ 0 AND N_ELEMENTS(decimals) EQ 0 THEN decimals = 2
  
  fixPart = LONG(inPut)
  
  inPut = STRING(input, format = '(g)')
  
  pointPos = STRPOS(inPut, '.')
  pointPart = STRMID(inPut, pointPos+1)
  
  
  IF N_ELEMENTS(delZero) NE 0 AND N_ELEMENTS(decimals) EQ 0 THEN BEGIN
  
    ;ɾ��С����������0
    len = STRLEN(pointPart)
    pos = len-1
    FOR i = len-1, 0, -1 DO BEGIN
    
      IF STRMID(pointPart, i, 1) EQ '0' THEN pos -= 1  ELSE BREAK
    ENDFOR
    
    pointPart = STRMID(pointPart, 0, pos+1)
    
    RETURN, pos EQ -1 ? STRTRIM(STRING(fixPart),2) : STRTRIM(STRING(fixPart) + '.' + pointPart, 2)
    
  ENDIF ELSE IF N_ELEMENTS(delZero) EQ 0 AND N_ELEMENTS(decimals) NE 0 THEN BEGIN
  
    ;����С�����decimalsλ
    RETURN, decimals GT 0 ? STRTRIM(STRING(fixPart) + '.' + STRMID(pointPart, 0, decimals), 2) : STRTRIM(STRING(fixPart),2)
    
  ENDIF
  
END