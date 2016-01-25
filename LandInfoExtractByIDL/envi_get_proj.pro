FUNCTION CONVERT_LATLON, input
  
  ;小数点转化为度分秒
  COMPILE_OPT idl2
  
  du = FIX(input)
  fen = FIX((input-du)*60)
  miao = ((input-du)*60-fen)*60
  
  RETURN, [du, fen, miao]
  
END




FUNCTION ENVI_GET_PROJ, fid

  ;获取投影信息
  ;输入 - FID，ENVI_OPEN_FILE返回的FID

  ;输出 - 结构体，格式如下
  ;  result = {Proj:Proj,  $          ;投影坐标系
  ;    Pixel:Pixel,        $          ;像元大小
  ;    Datum:Datum,        $          ;基准面
  ;    UL_Geo:UL_Geo,      $          ;经纬度坐标
  ;    UL_Map:UL_Map}                 ;地理坐标
  
  COMPILE_OPT idl2

  proj_str = ENVI_GET_PROJECTION(fid = fid, pixel_size = Pixel)
  
  ;如果没有投影信息，则返回0
  IF proj_str.NAME EQ 'Arbitrary' THEN RETURN, {Proj:'Arbitrary'}
  
  map_info = ENVI_GET_MAP_INFO(fid = fid)
  
  temp = map_info.PROJ.PE_COORD_SYS_STR
  pos1 = STRPOS(temp, '"')
  pos2 = STRPOS(temp, '",')
  
  Proj = STRMID(temp, pos1+1, pos2-pos1-1)
  
  ;基准面
  Datum = proj_str.DATUM
  
  ;坐标信息
  UL_Map = (map_info.MC)[2:3]
  
  oproj = ENVI_PROJ_CREATE(/geographic)
  
  ;地理坐标转换为经纬度
  ENVI_CONVERT_PROJECTION_COORDINATES,  $
    UL_Map[0], UL_Map[1], proj_str, $
    oXmap, oYmap, oproj
    
  ;经纬度转化为度分秒格式
  UL_Geo = [CONVERT_LATLON(oXmap),CONVERT_LATLON(oYmap)]
  
  result = {Proj:Proj,  $          ;投影坐标系
    Pixel:Pixel[0],     $          ;像元大小
    Datum:Datum,        $          ;基准面
    UL_Geo:UL_Geo,      $          ;经纬度坐标
    UL_Map:UL_Map}                 ;地理坐标
    
  RETURN, result
  
END


