; Chapter05DirectGrapgics.pro
PRO Chapter05DirectGrapgics
mydevice = !D.NAME    ;��ȡ��ǰ�豸�����������mydevice
SET_PLOT, 'WIN'         ;���õ�ǰ�豸Ϊ΢��Windows
DEVICE, DECOMPOSED = 0    ;ʹ��Ϊα����ʾģʽ
; ��ȡԭʼ��ɫ��
TVLCT, OriginalR, OriginalG, OriginalB, /GET
WINDOW, TITLE = 'My Window', XPOS=100, YPOS=100, XSIZE=500, YSIZE=500
DEVICE, DECOMPOSED = 0
FOR i = 1,100 DO BEGIN
    LOADCT, i MOD 41
    TVSCL, DIST(500)
    WAIT, 0.1
    ;ERASE
ENDFOR
WDELETE
; �ָ�ԭʼ��ɫ��
TVLCT, OriginalR, OriginalG, OriginalB
DEVICE, DECOMPOSED = 1    ;�ָ�Ϊ�����ʾģʽ
SET_PLOT, mydevice            ;�ָ�ԭʼ�豸mydevice
END