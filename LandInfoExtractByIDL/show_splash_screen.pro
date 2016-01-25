;$Id: show_splash_screen.pro 4 2008-01-30 21:13:44Z mpiper $

;+

; Shows an image without a window border on the screen.

;

; @returns widget identifier of top-level base

; @param image {in}{type=2 or 3 dimensional array} an image to

;        display

; @keyword true {in}{optional}{type=integer, 0-3}{default=0} order of

;          bands, 0 if 8-bit image

; @keyword order {in}{optional}{type=boolean} orientation of image

; @keyword title {in}{optional}{type=string} title of window to

;          display in icon

;-

;启动界面

FUNCTION show_splash_screen, image, true=true, order=order

  COMPILE_OPT idl2
  
  ON_ERROR, 2
  
  true_local = N_ELEMENTS(true) EQ 0 ?  0 : true
  sz = SIZE(image, /structure)
  
  IF (true_local EQ 0 AND sz.N_DIMENSIONS NE 2) THEN $  
    MESSAGE, 'TRUE keyword must be set to 1, 2, 3 ' $
    + 'for 24-bit image'
    
  IF (true_local NE 0 AND sz.N_DIMENSIONS NE 3) THEN $  
    MESSAGE, 'TRUE keyword must be set to 0 for 8-bit image'
    
  xind = (true_local NE 1) ? 0 : 1  
  yind = ((true_local EQ 0) OR (true_local EQ 3)) ? 1 : 2
  
  ;屏幕分辨率计算  
  DEVICE, get_screen_size=screen_size
  
  ;界面偏移量  
  xoffset = (screen_size[0] - sz.DIMENSIONS[xind]) / 2  
  yoffset = (screen_size[1] - sz.DIMENSIONS[yind]) / 2
  
  tlb = WIDGET_BASE(tlb_frame_attr=5, /column, $  
    xpad=0, ypad=0, xoffset=xoffset, yoffset=yoffset)
    
  draw = WIDGET_DRAW(tlb, xsize=sz.DIMENSIONS[xind], $  
    ysize=sz.DIMENSIONS[yind])
    
  WIDGET_CONTROL, tlb, /realize  
  WIDGET_CONTROL, draw, get_value=win_id
  
  WSET, win_id  
  TV, image, true=true_local, order=KEYWORD_SET(order)
  
  RETURN, tlb
  
END





