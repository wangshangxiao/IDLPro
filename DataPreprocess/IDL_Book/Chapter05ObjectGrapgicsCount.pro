; Chapter05ObjectGrapgicsCount.pro
PRO Chapter05ObjectGrapgicsCount
    MyWindow = OBJ_NEW('IDLgrWindow')     ;�������ڶ���
    MyView = OBJ_NEW('IDLgrView', VIEWPLANE_RECT=[0,-1,100,2])     ;������ͼ����
    MyModel = OBJ_NEW('IDLgrModel')      ;����ģʽ����
    MyPlot = OBJ_NEW('IDLgrPlot', SIN(FINDGEN(100)/10))     ;������������ͼԪ����
    MyModel -> ADD, MyPlot                  ;���MyPlot����MyModel
    MyView -> ADD, MyModel                ;���MyPlot����MyModel
    MyWindow -> DRAW, MyView          ;���MyPlot����MyModel
    ObjectNumber = MyView -> Count()    ;ͳ��MyView �ж���ĸ���
    Result = DIALOG_MESSAGE( STRING(ObjectNumber), /INFORMATION )  ;�������ĸ���
    WAIT, 20                                          ;��ͣ10��
    OBJ_DESTROY, MyView                   ;ɾ��MyView����
    OBJ_DESTROY, MyWindow              ;ɾ��MyWindow����
END