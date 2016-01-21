;+
;:Description:
;    Describe the procedure.
;
; Author: DYQ 2010-1-27;
;
; Blog: http://hi.baidu.com/dyqwrp
; QQ: 362463351
;-
PRO TEST_PROCESS
tlb = WIDGET_BASE(xsize =200,ysize =200)
WIDGET_CONTROL,tlb,/real
;  
;IDL的iTools自带进度条
prsbar = IDLITWDPROGRESSBAR(GROUP_LEADER = tlb,title ='进度',CANCEL=cancelIn)
FOR i=0,99 DO BEGIN
;判断是否点击取消
IF WIDGET_INFO(prsbar,/valid) THEN  BEGIN
IDLITWDPROGRESSBAR_SETVALUE,prsbar,i
ENDIF ELSE BEGIN
tmp = DIALOG_MESSAGE('点击了取消,当前进度位置'+STRING(i)+'%',/info)
BREAK
ENDELSE
;等待0.1秒
WAIT,0.1
ENDFOR  

;测试ENVI进度条
ENVI,/restore_base_save_files
ENVI_BATCH_INIT
;初始化进度条
ENVI_REPORT_INIT, '处理', title="ENVI Processbar", base=base ,/INTERRUPT
ENVI_REPORT_INC, base, 100
FOR i=0,100-1 DO BEGIN
ENVI_REPORT_STAT,base, i, 100., CANCEL=cancelvar
;判断是否点击取消
IF cancelVar EQ 1 THEN BEGIN
tmp = DIALOG_MESSAGE('点击了取消'+STRING(i)+'%',/info)
ENVI_REPORT_INIT, base=base, /finish
BREAK
ENDIF
WAIT,0.1
ENDFOR
ENVI_BATCH_EXIT
END