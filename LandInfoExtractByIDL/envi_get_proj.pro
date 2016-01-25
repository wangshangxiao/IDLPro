FUNCTION CONVERT_LATLON, input
  
  ;С����ת��Ϊ�ȷ���
  COMPILE_OPT idl2
  
  du = FIX(input)
  fen = FIX((input-du)*60)
  miao = ((input-du)*60-fen)*60
  
  RETURN, [du, fen, miao]
  
END




FUNCTION ENVI_GET_PROJ, fid

  ;��ȡͶӰ��Ϣ
  ;���� - FID��ENVI_OPEN_FILE���ص�FID

  ;��� - �ṹ�壬��ʽ����
  ;  result = {Proj:Proj,  $          ;ͶӰ����ϵ
  ;    Pixel:Pixel,        $          ;��Ԫ��С
  ;    Datum:Datum,        $          ;��׼��
  ;    UL_Geo:UL_Geo,      $          ;��γ������
  ;    UL_Map:UL_Map}                 ;��������
  
  COMPILE_OPT idl2

  proj_str = ENVI_GET_PROJECTION(fid = fid, pixel_size = Pixel)
  
  ;���û��ͶӰ��Ϣ���򷵻�0
  IF proj_str.NAME EQ 'Arbitrary' THEN RETURN, {Proj:'Arbitrary'}
  
  map_info = ENVI_GET_MAP_INFO(fid = fid)
  
  temp = map_info.PROJ.PE_COORD_SYS_STR
  pos1 = STRPOS(temp, '"')
  pos2 = STRPOS(temp, '",')
  
  Proj = STRMID(temp, pos1+1, pos2-pos1-1)
  
  ;��׼��
  Datum = proj_str.DATUM
  
  ;������Ϣ
  UL_Map = (map_info.MC)[2:3]
  
  oproj = ENVI_PROJ_CREATE(/geographic)
  
  ;��������ת��Ϊ��γ��
  ENVI_CONVERT_PROJECTION_COORDINATES,  $
    UL_Map[0], UL_Map[1], proj_str, $
    oXmap, oYmap, oproj
    
  ;��γ��ת��Ϊ�ȷ����ʽ
  UL_Geo = [CONVERT_LATLON(oXmap),CONVERT_LATLON(oYmap)]
  
  result = {Proj:Proj,  $          ;ͶӰ����ϵ
    Pixel:Pixel[0],     $          ;��Ԫ��С
    Datum:Datum,        $          ;��׼��
    UL_Geo:UL_Geo,      $          ;��γ������
    UL_Map:UL_Map}                 ;��������
    
  RETURN, result
  
END


