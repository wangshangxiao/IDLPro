; 对RGB图像进行分类
; 能够提取 植被、水体、人工建筑
; 采用一系列规则进行提取，方法简单，但效果还可以

PRO Image_Class

    Compile_opt idl2;
    Envi, /RESTORE_BASE_SAVE_FILES
    Envi_batch_init

  LST_infile = dialog_pickfile()
  if LST_infile eq '' then return

  envi_open_file, LST_infile, r_fid = fid1, /NO_REALIZE
  envi_file_query, fid1,    $
    dims = dims,            $
    ns = ns,                $
    nl = nl,                $
    nb = nb
  pos = [0,1,2]

  ; 注意： 这里没有分块
  R_data = fix( envi_get_data( fid = fid1, dims = dims, pos = 0 ) )
  G_data = fix( envi_get_data( fid = fid1, dims = dims, pos = 1 ) )
  B_data = fix( envi_get_data( fid = fid1, dims = dims, pos = 2 ) )
  
  Veg_data = intarr(ns, nl)
  

  ;利用RGB的值判断植被
  index = where(G_data gt R_DATA AND $
                G_DATA GT B_DATA AND $
                G_DATA Le 150 AND    $
                R_DATA Le 80 AND     $
                B_DATA Le 80,        $
                COUNT)
                
  
  IF COUNT GT 0 THEN BEGIN
     Veg_data[index] = 1
  ENDIF
  
  WRITE_TIFF,'C:\Users\Administrator\Desktop\test\hbu20_Veg.tif',Veg_data,/short
  
  ;envi_enter_data, veg_data, bname = 'vegetation'
  
  build_data = intarr(ns, nl)
  
  ; 人工建筑, 两种规则方式，视情况选择
  index = where(G_data gt 150 AND $
                G_DATA lt 230 AND $
                R_DATA LT 205 AND $
                R_DATA GT 80 AND  $
                B_DATA LT 215 AND $
                B_DATA gT 80,     $
                COUNT)
  index = where((G_DATA GT 150) AND             $
                (ABS(G_DATA-R_DATA) LE 50) AND  $
                (ABS(G_DATA-B_DATA) LE 50),     $
                COUNT)
  IF COUNT GT 0 THEN BEGIN
     build_data[index] = 1
  ENDIF
  
  ;print,build_data
  ;envi_enter_data, build_data, bname = 'building'

  WRITE_TIFF,'C:\Users\Administrator\Desktop\test\hbu20_build.tif',build_data,/short

  sea_data = intarr(ns, nl)
  index = where(G_data ge R_data AND $
                B_DATA ge R_data,    $
                COUNT)
  IF COUNT GT 0 THEN BEGIN
     sea_data[index] = 1
  ENDIF
  ;envi_enter_data, sea_data, bname = 'water'

  
  
  envi_batch_exit
  
  WRITE_TIFF,'C:\Users\Administrator\Desktop\test\hbu20_water.tif',sea_data,/short  
  ;Veg_data = 0 & build_data = 0 & sea_data = 0

  ; CHANGE_THRESH在0-1之间，值越大，那么迭代停止的条件越容易满足。

;  envi_doit, 'class_doit', fid=fid1, dims=dims,method=7,out_bname = 'K-Means',$
;  out_name = 'F:\test.bmp',pos=pos,r_fid=r_fid,CHANGE_THRESH=0.4, NUM_CLASSES=4,$
;  ITERATIONS=5



  ;ISO_SPLIT_STD,If the standard deviation of a class is larger than this threshold then the class is split into two classes.

;  envi_doit, 'class_doit', fid=fid1, dims=dims,method=7,out_bname = 'ISODATA',$
;  out_name = 'F:\test_ISODATA.bmp',pos=pos,r_fid=r_fid,CHANGE_THRESH=1.0, NUM_CLASSES=4,ITERATIONS=5,$
;  ISO_MERGE_DIST=floating point, $
;  ISO_MERGE_PAIRS=value, ISO_MIN_PIXELS=300, ISO_SPLIT_SMULT=floating point, $
;  ISO_SPLIT_STD=floating point, MIN_CLASSES=integer, NUM_CLASSES=integer

END
