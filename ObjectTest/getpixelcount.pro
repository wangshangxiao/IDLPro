pro getPixelcount

  inputFiles='D:\share\Hongxingtest\GF\geo\VI_TIF\gf1_wfv1_e1270_n480_20130711_l2a0000118108_clip_geo_NDVI_plot.tif'
  ;∂¡»°tiff”∞œÒ
  data = read_tiff(inputFiles,geotiff = geotiff)
  NCount = N_ELEMENTS(data)
  print,'Totalcount=',NCount
  BackGraoud_Index=where(data eq 0,bcount)
  print,'BackGraoudcount=',bcount
  NoBackGraoud_Index=where(data NE 0,count)
  print,'NoBackGraoudcount=',count
END