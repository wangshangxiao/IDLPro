PRO PROCESS_MOD04_AND_MOSAIC,DataInPath,ullon,ullat,lrlon,lrlat,date 
 
; 借助CMTK工具 对MOD04            ;
;    数据进行地理投影             ;
;   Emai：askzhanghu@126.com      ;
;     QQ:37*****7                ;
;     2012.10.4                   ;

  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_batch_init,LOG_FILE = 'reproject_mod04.log';NO_STATUS_WINDOW=1
  ;寻找文件
  MOD04_File=file_search(DataInPath + 'MOD04_L2.A'+date+'*.hdf');
  nf=n_elements(MOD04_File)   ;文件的总个数 
  fnum=0                      ;最终计算的文件个数
  IF nf GE 1 THEN BEGIN
    tmpMOD04=strarr(nf)
    FOR i=0,nf-1 DO BEGIN
      ;extract the file name
      st=strsplit(MOD04_File[i],'\',/extract)
      N_str=n_elements(st)
      out_flagName=strmid(st[N_str-1],0,22)     
      out_dir=DataInPath 
         
      hdfread ,MOD04_File[i],'Longitude',Longitude
      hdfread ,MOD04_File[i],'Latitude',Latitude     
      ;判断数据是否包括研究区
      index=where(Longitude GE ullon AND Longitude LE lrlon,COUNT_lon)
      index=where(Latitude GE lrlat AND Latitude LE ullat,COUNT_lat)     
      IF  COUNT_lon GT 0 and COUNT_lat GT 0 THEN BEGIN
        ;; 根据CMTK实现数据的投影       ;Create output map information   for China
        ;Polar_prj = ENVI_PROJ_CREATE(/geographic,datum="WGS-84")       
        name = 'China_Lambert_Conformal_Conic'
        datum = 'WGS-84'
        params = [6378137.0, 6356752.3, 0.000000,105.000000, 0.0, 0.0, 25.000000, 47.000000]
        type = 4
        Polar_prj = ENVI_PROJ_CREATE(TYPE = type, NAME = name, DATUM = datum,  PARAMS = params)
         
        ;利用CMTK对文件进行地理投影
        st=strsplit(DataInPath,'\',/extract)
        N_str=n_elements(st)
        DataFlag=st[N_str-1]
        SD_names=['Image_Optical_Depth_Land_And_Ocean']
        swt_name='mod04'
        tmpMOD04[i]=DataInPath+'\'+strcompress(out_flagName+'_Swath_2D_1_georef.img',/remove_all)
        fnum=fnum+1
        
        CONVERT_MODIS_DATA, in_file=MOD04_File[i],out_path=out_dir, out_root=out_flagName,$
             /swath, swt_name=swt_name, /bowtie, SD_names=SD_names,$
             out_method=1, num_x_pts=50, num_y_pts=50,  background=0.0,$
             INTERP_METHOD=6, FILL_REPLACE_VALUE=0,out_proj=Polar_prj 
     ENDIF
   ENDFOR 
  ENDIF
 
END  