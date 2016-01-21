;
;数据透明叠加Demo
; comment: DYQ
;
;
PRO AlphaBlend
  ;构建数据名
  headFile = FILEPATH('head.dat', $
    SUBDIRECTORY = ['examples', 'data'])
  ;定义数据
  headSize = [80, 100, 57]
  head = BYTARR(headSize[0], headSize[1], headSize[2])
  imageSize = [240, 300]
  ;初始化读取数据
  OPENR, unit, headFile, /GET_LUN
  READU, unit, head
  FREE_LUN, unit
  
  ; 初始化基础类显示对象
  oWindow = OBJ_NEW('IDLgrWindow', RETAIN = 2, $
    DIMENSIONS = [imageSize[0], 2*imageSize[1]], $
    TITLE='原始切片显示！')
  oView = OBJ_NEW('IDLgrView', $
    VIEWPLANE_RECT = [0., 0., imageSize[0], 2*imageSize[1]])
    
  ; 每个图像一个model
  oModel = [OBJ_NEW('IDLgrModel'), OBJ_NEW('IDLgrModel')]
  
  ; 读取切片数据1
  layer1 = CONGRID(head[*,*,30], imageSize[0],imageSize[1],$
    /INTERP)
  ;初始化图像图层1
  oLayer1 = OBJ_NEW('IDLgrImage', layer1)
  
  ; 读取切片数据2
  layer2 = CONGRID(head[*,*,43], imageSize[0],imageSize[1],$
    /INTERP)
  ;初始化图像图层2-包含颜色表
  oPalette = OBJ_NEW('IDLgrPalette')
  oPalette -> LOADCT, 12
  oLayer2 = OBJ_NEW('IDLgrImage', layer2, PALETTE = oPalette)
  ;添加显示图层
  oModel[0] -> Add, oLayer1
  oModel[1] -> Add, oLayer2
  
  ;图层位置移动.
  oModel[0] -> Translate, 0., imageSize[1], 0.
  ; 添加显示关系并绘制.
  oView -> Add, oModel
  oWindow -> Draw, oView
    
  ; 清除引用对象.
  OBJ_DESTROY, [oView]
  ; 获取颜色表的RGB
  oPalette -> GetProperty, RED_VALUES = red, $
    GREEN_VALUES = green, BLUE_VALUES = blue
  ; 定义掩膜图层数据
  alpha = BYTARR(4, imageSize[0], imageSize[1])
  ; 将原始数据赋值
  alpha[0,*,*]= red[layer2]
  alpha[1,*,*]= green[layer2]
  alpha[2,*,*]= blue[layer2]
  
  ; 定义掩膜数据层
  mask =layer2 GT 25
  ;定义该图层的非透明度为255（范围为0-255，从透明到不透明）.
  alpha[3,*,*] = mask * 255
  
  ;初始化image对象，并设置Blend_Function为[3,4]：透明有效
  oAlpha = OBJ_NEW('IDLgrImage', alpha, $
    DIMENSIONS = imageSize, BLEND_FUNCTION = [3,4])
    
  ;初始化显示对象
  oWindow =OBJ_NEW('IDLgrWindow', DIMENSIONS = imageSize, $
    LOCATION = [300,0], RETAIN = 2,$
    TITLE ='透明混合显示！')
  oView = OBJ_NEW('IDLgrView', $
    VIEWPLANE_RECT = [0,0,imageSize[0], imageSize[1]])
  oModel = OBJ_NEW('IDLgrModel')
  ; 初始化对象.
  oBase = OBJ_NEW('IDLgrImage', layer1)
  ; 添加显示关系并显示
  oModel -> Add, oBase
  oModel -> Add, oAlpha
  oView -> Add, oModel
  oWindow -> Draw, oView
  ; 清除引用对象.
  OBJ_DESTROY, [oView, oPalette]
END