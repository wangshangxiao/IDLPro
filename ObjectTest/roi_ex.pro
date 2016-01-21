PRO roi_ex
; Load and display an image.
img=READ_PNG(FILEPATH('mineral.png',SUBDIR=['examples','data'])) 

WINDOW, 0, XSIZE = 256, YSIZE = 400
TV, img

XYOUTS, 20, 350, 'To create a region:', CHARSIZE=1.25, /DEVICE
XYOUTS, 20, 325, 'Left mouse: select points for a region', /DEVICE
XYOUTS, 20, 310, 'Right mouse: complete the region', /DEVICE
XYOUTS, 20, 285, '(Click right mouse before closing)', /DEVICE

; Create a polygon region object.
oROI = OBJ_NEW('IDLanROI', TYPE=2)

; Collect first vertex for the region.
CURSOR, xOrig, yOrig, /UP, /DEVICE

oROI->AppendData, xOrig, yOrig

PLOTS, xOrig, yOrig, PSYM=1, /DEVICE
;;记住第一个点坐标
xxpts=[xOrig]
yypts=[yOrig]
;初始化两个一维数组(x和y)，并记住第一个点坐标x,y
xpts=[xOrig]
ypts=[yOrig]

;Continue to collect vertices for region until right mouse button.
x1 = xOrig
y1 = yOrig
while !MOUSE.BUTTON ne 4 do begin
x0 = x1
y0 = y1
CURSOR, x1, y1, /UP, /DEVICE

PLOTS, [x0,x1], [y0,y1], /DEVICE
oROI->AppendData, x1, y1

;将点击的坐标追加到数组中
xpts=[xpts,x0]
ypts=[ypts,y0]
endwhile
;去掉数组中第一个成员
xpts=xpts[1:*]
ypts=ypts[1:*]
;在最后加上第一个点，以完成闭合曲线
xpts=[xpts,xxpts]
ypts=[ypts,yypts]

PLOTS, [x1,xOrig], [y1,yOrig], /DEVICE
; Draw the the region with a line fill.
DRAW_ROI, oROI, /LINE_FILL, SPACING=0.2, ORIENTATION=45, /DEVICE


compile_opt IDL2

envi, /restore_base_save_files 

envi_batch_init, log_file='batch.txt'

  roi_id = envi_create_roi(color=5, name='Square', $
  
     ns=512, nl=512) 
     
     envi_define_roi, roi_id, /polygon, xpts=xpts, ypts=ypts
  
  ;;roi_ids = envi_get_roi_ids()
  envi_save_rois, 'cutor.roi', roi_id
  
  ENVI_DELETE_ROIS,/all
  
END

