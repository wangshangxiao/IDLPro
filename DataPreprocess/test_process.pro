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
;IDL��iTools�Դ�������
prsbar = IDLITWDPROGRESSBAR(GROUP_LEADER = tlb,title ='����',CANCEL=cancelIn)
FOR i=0,99 DO BEGIN
;�ж��Ƿ���ȡ��
IF WIDGET_INFO(prsbar,/valid) THEN  BEGIN
IDLITWDPROGRESSBAR_SETVALUE,prsbar,i
ENDIF ELSE BEGIN
tmp = DIALOG_MESSAGE('�����ȡ��,��ǰ����λ��'+STRING(i)+'%',/info)
BREAK
ENDELSE
;�ȴ�0.1��
WAIT,0.1
ENDFOR  

;����ENVI������
ENVI,/restore_base_save_files
ENVI_BATCH_INIT
;��ʼ��������
ENVI_REPORT_INIT, '����', title="ENVI Processbar", base=base ,/INTERRUPT
ENVI_REPORT_INC, base, 100
FOR i=0,100-1 DO BEGIN
ENVI_REPORT_STAT,base, i, 100., CANCEL=cancelvar
;�ж��Ƿ���ȡ��
IF cancelVar EQ 1 THEN BEGIN
tmp = DIALOG_MESSAGE('�����ȡ��'+STRING(i)+'%',/info)
ENVI_REPORT_INIT, base=base, /finish
BREAK
ENDIF
WAIT,0.1
ENDFOR
ENVI_BATCH_EXIT
END