PRO TEST_RESIZEVIAEVF 
COMPILE_OPT idl2 
;获取当前路径 
IF (FLOAT(!version.RELEASE) LT 7) THEN BEGIN 
curPath = SOURCEROOT() 
ENDIF ELSE BEGIN 
curPath = FILE_DIRNAME(ROUTINE_FILEPATH('TEST_RESIZEVIAEVF')) 
ENDELSE 
;  ;测试数据带投影 
;  fileName = curpath+PATH_SEP()+'bhtmsat.img' 
;  evfName = curpath+PATH_SEP()+'bhtmsat.evf' 
;  outName = curpath+PATH_SEP()+'bhtmsat_resize.img' 
;  ;测试数据无投影 
fileName = curpath+PATH_SEP()+'test.img' 
evfName = curpath+PATH_SEP()+'test.evf' 
outName = curpath+PATH_SEP()+'test_resize.img' 
;Restore函数 
Restore,curpath+PATH_SEP()+'ResizeViaEvf.sav' 
;调用函数 
ResizeViaEvf, filename = filename, $ 
evfName = evfName, $ 
outName = outName 
END

FUNCTION SOURCEROOT 
COMPILE_OPT StrictArr 
HELP, Calls = Calls 
UpperRoutine = (StrTok(Calls[1], ' ', /Extract))[0] 
Skip = 0 
CATCH, ErrorNumber 
IF (ErrorNumber NE 0) THEN BEGIN 
CATCH, /Cancel 
ThisRoutine = ROUTINE_INFO(UpperRoutine, /Functions, /Source) 
Skip = 1 
ENDIF 
IF (Skip EQ 0) THEN BEGIN 
ThisRoutine = ROUTINE_INFO(UpperRoutine, /Source) 
IF (thisRoutine.Path EQ '') THEN BEGIN 
MESSAGE,'',/traceback 
ENDIF 
ENDIF 
CATCH,/cancel 
IF (STRPOS(thisroutine.path,PATH_SEP()) EQ -1 ) THEN BEGIN 
CD, current=current 
sourcePath = FILEPATH(thisrouitine.path, root=current) 
ENDIF ELSE BEGIN 
sourcePath = thisroutine.path 
ENDELSE 
Root = STRMID(sourcePath, 0, STRPOS(sourcePath, PATH_SEP(), /Reverse_Search) + 1) 
RETURN, Root 
END