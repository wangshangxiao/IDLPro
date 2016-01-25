FUNCTION NUM_FORMATTER, inPut, delZero = delZero, decimals = decimals

  ;输入inPut 须为Double型

  ;参数
  ;delZero : 如果为1，则返回保留有效数字的结果    用法： r = NUM_FORMATTER(inPut, /delZero)
  ;decimals : 保留小数点后几位                                用法： r = NUM_FORMATTER(inPut, decimals = 2)

  ;返回值为字符串


  IF N_ELEMENTS(inPut) EQ 0 THEN MESSAGE, 'A number must be passed as an argument to the function.'
  
  IF N_ELEMENTS(delZero) EQ 0 AND N_ELEMENTS(decimals) EQ 0 THEN decimals = 2
  
  fixPart = LONG(inPut)
  
  inPut = STRING(input, format = '(g)')
  
  pointPos = STRPOS(inPut, '.')
  pointPart = STRMID(inPut, pointPos+1)
  
  
  IF N_ELEMENTS(delZero) NE 0 AND N_ELEMENTS(decimals) EQ 0 THEN BEGIN
  
    ;删除小数点后边最后的0
    len = STRLEN(pointPart)
    pos = len-1
    FOR i = len-1, 0, -1 DO BEGIN
    
      IF STRMID(pointPart, i, 1) EQ '0' THEN pos -= 1  ELSE BREAK
    ENDFOR
    
    pointPart = STRMID(pointPart, 0, pos+1)
    
    RETURN, pos EQ -1 ? STRTRIM(STRING(fixPart),2) : STRTRIM(STRING(fixPart) + '.' + pointPart, 2)
    
  ENDIF ELSE IF N_ELEMENTS(delZero) EQ 0 AND N_ELEMENTS(decimals) NE 0 THEN BEGIN
  
    ;返回小数点后decimals位
    RETURN, decimals GT 0 ? STRTRIM(STRING(fixPart) + '.' + STRMID(pointPart, 0, decimals), 2) : STRTRIM(STRING(fixPart),2)
    
  ENDIF
  
END