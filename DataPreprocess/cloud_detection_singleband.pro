  PRO cloud_detection_singleband,inputFiles=inputFiles,chimageoutpath=chimageoutpath,Message=Message
  
  inputFiles='D:\share\Hongxingtest\GF\geo\VI_TIF\gf1_wfv3_e1274_n488_20130825_l2a0000118077_clip_geo_Band4.tif'
  chimageoutpath='D:\share\Hongxingtest\GF\geo\cloud'
  ;;compile_opt idl2
;  Result = DIALOG_MESSAGE(chimageoutpath)
  ;∂¡»°tiff”∞œÒ
  data = read_tiff(inputFiles,geotiff = geotiff)

;print,data
    Cloud_Index=where(data GT 0.0000000 and data LT 0.1800000 )
    
    ;print,Cloud_Index
    data[Cloud_Index]=1
    NoCloud_Index=where(data NE 1)
    data[NoCloud_Index]=0
    ;print,blue
    result=data
    write_tiff,chimageoutpath+'\'+FILE_BASENAME(inputFiles,'.tif',/FOLD_CASE)+'_cloud1.tif',result,geotiff = geotiff,/float
  Message=''
  end