;
;����͸������Demo
; comment: DYQ
;
;
PRO AlphaBlend
  ;����������
  headFile = FILEPATH('head.dat', $
    SUBDIRECTORY = ['examples', 'data'])
  ;��������
  headSize = [80, 100, 57]
  head = BYTARR(headSize[0], headSize[1], headSize[2])
  imageSize = [240, 300]
  ;��ʼ����ȡ����
  OPENR, unit, headFile, /GET_LUN
  READU, unit, head
  FREE_LUN, unit
  
  ; ��ʼ����������ʾ����
  oWindow = OBJ_NEW('IDLgrWindow', RETAIN = 2, $
    DIMENSIONS = [imageSize[0], 2*imageSize[1]], $
    TITLE='ԭʼ��Ƭ��ʾ��')
  oView = OBJ_NEW('IDLgrView', $
    VIEWPLANE_RECT = [0., 0., imageSize[0], 2*imageSize[1]])
    
  ; ÿ��ͼ��һ��model
  oModel = [OBJ_NEW('IDLgrModel'), OBJ_NEW('IDLgrModel')]
  
  ; ��ȡ��Ƭ����1
  layer1 = CONGRID(head[*,*,30], imageSize[0],imageSize[1],$
    /INTERP)
  ;��ʼ��ͼ��ͼ��1
  oLayer1 = OBJ_NEW('IDLgrImage', layer1)
  
  ; ��ȡ��Ƭ����2
  layer2 = CONGRID(head[*,*,43], imageSize[0],imageSize[1],$
    /INTERP)
  ;��ʼ��ͼ��ͼ��2-������ɫ��
  oPalette = OBJ_NEW('IDLgrPalette')
  oPalette -> LOADCT, 12
  oLayer2 = OBJ_NEW('IDLgrImage', layer2, PALETTE = oPalette)
  ;�����ʾͼ��
  oModel[0] -> Add, oLayer1
  oModel[1] -> Add, oLayer2
  
  ;ͼ��λ���ƶ�.
  oModel[0] -> Translate, 0., imageSize[1], 0.
  ; �����ʾ��ϵ������.
  oView -> Add, oModel
  oWindow -> Draw, oView
    
  ; ������ö���.
  OBJ_DESTROY, [oView]
  ; ��ȡ��ɫ���RGB
  oPalette -> GetProperty, RED_VALUES = red, $
    GREEN_VALUES = green, BLUE_VALUES = blue
  ; ������Ĥͼ������
  alpha = BYTARR(4, imageSize[0], imageSize[1])
  ; ��ԭʼ���ݸ�ֵ
  alpha[0,*,*]= red[layer2]
  alpha[1,*,*]= green[layer2]
  alpha[2,*,*]= blue[layer2]
  
  ; ������Ĥ���ݲ�
  mask =layer2 GT 25
  ;�����ͼ��ķ�͸����Ϊ255����ΧΪ0-255����͸������͸����.
  alpha[3,*,*] = mask * 255
  
  ;��ʼ��image���󣬲�����Blend_FunctionΪ[3,4]��͸����Ч
  oAlpha = OBJ_NEW('IDLgrImage', alpha, $
    DIMENSIONS = imageSize, BLEND_FUNCTION = [3,4])
    
  ;��ʼ����ʾ����
  oWindow =OBJ_NEW('IDLgrWindow', DIMENSIONS = imageSize, $
    LOCATION = [300,0], RETAIN = 2,$
    TITLE ='͸�������ʾ��')
  oView = OBJ_NEW('IDLgrView', $
    VIEWPLANE_RECT = [0,0,imageSize[0], imageSize[1]])
  oModel = OBJ_NEW('IDLgrModel')
  ; ��ʼ������.
  oBase = OBJ_NEW('IDLgrImage', layer1)
  ; �����ʾ��ϵ����ʾ
  oModel -> Add, oBase
  oModel -> Add, oAlpha
  oView -> Add, oModel
  oWindow -> Draw, oView
  ; ������ö���.
  OBJ_DESTROY, [oView, oPalette]
END