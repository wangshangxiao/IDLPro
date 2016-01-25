; Chapter05DirectGrapgicsLight.pro
PRO Chapter05DirectGrapgicsLight
mydevice = !D.NAME    ;��ȡ��ǰ�豸�����������mydevice
SET_PLOT, 'WIN'         ;���õ�ǰ�豸Ϊ΢��Windows
DEVICE, DECOMPOSED = 0    ;ʹ��Ϊα����ʾģʽ
; ��ȡԭʼ��ɫ��
TVLCT, OriginalR, OriginalG, OriginalB, /GET
WINDOW, TITLE = 'My Window', XPOS=100, YPOS=100, XSIZE=400, YSIZE=400
DEVICE, DECOMPOSED = 0
MyR = INDGEN(256)
MyG = 150-INDGEN(256)
MyB = 250-INDGEN(256)
FOR i = 0, 255 DO BEGIN
    FOR j = 0, 255 DO BEGIN
        MyR = (MyR + j) Mod 256
        MyG = (MyG + j) Mod 256
        MyB = (MyB + j) Mod 256
        TVLCT, MyR, MyG, MyB
        TVSCL, DIST(400)
        WAIT, 0.005
    ENDFOR
ENDFOR
WDELETE
; �ָ�ԭʼ��ɫ��
TVLCT, OriginalR, OriginalG, OriginalB
DEVICE, DECOMPOSED = 1    ;�ָ�Ϊ�����ʾģʽ
SET_PLOT, mydevice            ;�ָ�ԭʼ�豸mydevice
END