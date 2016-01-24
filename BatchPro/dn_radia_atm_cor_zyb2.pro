pro dn_radia_atm_cor_zyb2

compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
   RADIANCE_MULT_BAND_2_value=''
   RADIANCE_MULT_BAND_3_value=''
   RADIANCE_MULT_BAND_4_value=''
   RADIANCE_MULT_BAND_5_value=''
   RADIANCE_MULT_BAND_6_value=''
   RADIANCE_MULT_BAND_7_value=''
   RADIANCE_ADD_BAND_1_value=''
   RADIANCE_ADD_BAND_2_value=''
   RADIANCE_ADD_BAND_3_value=''
   RADIANCE_ADD_BAND_4_value=''
   RADIANCE_ADD_BAND_5_value=''
   RADIANCE_ADD_BAND_6_value=''
   RADIANCE_ADD_BAND_7_value=''
   CORNER_UL_LAT_PRODUCT_value=''
   CORNER_UL_LON_PRODUCT_value=''
   CORNER_UR_LAT_PRODUCT_value=''
   CORNER_UR_LON_PRODUCT_value=''
   CORNER_LL_LAT_PRODUCT_value=''
   CORNER_LL_LON_PRODUCT_value=''
   CORNER_LR_LAT_PRODUCT_value=''
   CORNER_LR_LON_PRODUCT_value=''
   DATE_ACQUIRED_value=''
   DataGetArr=''
   DataGetMonth=''
   DataGetDay=''
   GMT=''
   Center_LAT=''
   Center_LON=''
   season=''
   iband=''
   result=file_search('H:\landset5','*',/test_directory)
   for i=0,n_elements(result)-9 do begin
     asciifile=file_search(result[i]+'\*MTL.txt')

     line_counts=FILE_LINES(asciifile)
   
     ;counts=line_counts[0]+1-1
    openr,lun,asciifile,/get_lun

     void=''
  
     ;for j=0,120 do readf,lun,void
     arr=strarr(line_counts[0])
     
     readf,lun,arr
     for j=0,line_counts[0]-1 do begin
   
       arrsuntmp = strsplit(arr[j],'=',/extract)
       DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE)
       if max(DATE_ACQUIRED_bool) eq 1 then begin
         DATE_ACQUIRED_value = arrsuntmp[1]
         DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract)
         DataGetMonth=DataGetArr[1]
         DataGetDay=DataGetArr[2]
       endif
       SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
       if max(SCENE_CENTER_TIME_bool) eq 1 then begin
         SCENE_CENTER_TIME_value = arrsuntmp[1]
         DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)
         GMT=double(DataGetTimeArr[0])+double(DataGetTimeArr[1])/60+double(strmid(DataGetTimeArr[2],0,8))/3600
       endif
       CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
         CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
         CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
         CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
         CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
         CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
         CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
         CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
         CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
       endif
      
       ;;;;;;;
       RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_1) eq 1 then begin
         RADIANCE_MULT_BAND_1_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_2) eq 1 then begin
         RADIANCE_MULT_BAND_2_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_3) eq 1 then begin
         RADIANCE_MULT_BAND_3_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_4) eq 1 then begin
         RADIANCE_MULT_BAND_4_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_5) eq 1 then begin
         RADIANCE_MULT_BAND_5_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_6) eq 1 then begin
         RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_7) eq 1 then begin
         RADIANCE_MULT_BAND_7_value = arrsuntmp[1]
       
       endif
       RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_1) eq 1 then begin
         RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_2) eq 1 then begin
         RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_3) eq 1 then begin
         RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_4) eq 1 then begin
         RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_5) eq 1 then begin
         RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_6) eq 1 then begin
        RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_7) eq 1 then begin
         RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
       endif
     endfor
     
       free_lun,lun
     ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
     ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then
     ;??????????????????????????????????????????????????????
     ;????????????????????????????????????
      Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
      Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4
     tm5b1exp = 'b1*'+strtrim(RADIANCE_MULT_BAND_1_value,2)+''+RADIANCE_ADD_BAND_1_value+''
     tm5b2exp = 'b2*'+strtrim(RADIANCE_MULT_BAND_2_value,2)+''+RADIANCE_ADD_BAND_2_value+''
     tm5b3exp = 'b3*'+strtrim(RADIANCE_MULT_BAND_3_value,2)+''+RADIANCE_ADD_BAND_3_value+''
     tm5b4exp = 'b4*'+strtrim(RADIANCE_MULT_BAND_4_value,2)+''+RADIANCE_ADD_BAND_4_value+''
     tm5b5exp = 'b5*'+strtrim(RADIANCE_MULT_BAND_5_value,2)+''+RADIANCE_ADD_BAND_5_value+''
     tm5b6exp = 'b6*'+strtrim(RADIANCE_MULT_BAND_6_value,2)+'+'+RADIANCE_ADD_BAND_6_value+''
     tm5b7exp = 'b7*'+strtrim(RADIANCE_MULT_BAND_7_value,2)+''+RADIANCE_ADD_BAND_7_value+''
   ;;;;;;
   ;判断季节
 
    if string(DataGetMonth) ge 3 && string(DataGetMonth) le 8 then begin
      season = '2 Midlatitude Summer'
    endif else begin
      season = '3 Midlatitude Winter'
      endelse
      ;;;;;;;;;;;;
     ;;;;设定气溶胶模型
     Aerosol_Modle='1 Continental Model'
     ;;;;;设定大气条件
     Atmospheric_condi='35 visibility or aot'
     ;;;;;设定目标海拔高度
     Target_Altitude='-0.108 (target level,negative value)'
     ;;;;;设定传感器高度
     Sensor_Altitude='-705 (sensor level)'
     ;;;;;;
    ; for band = 1,6 do begin
      ;根据波段数来设定6s中自定义波段的输入量，分别为1、2、3、4、5、7波段
;      if band eq 1 then begin
;        iband = '25 (chosen band)'
;      endif
;      if band eq 2 then begin
;        iband = '26 (chosen band)'
;      endif
;      if band eq 3 then begin
;        iband = '27 (chosen band)'
;      endif
;      if band eq 4 then begin
;        iband = '28 (chosen band)'
;      endif
;        if band eq 5 then begin
;        iband = '29 (chosen band)'
;      endif 
;       if band eq 6 then begin
        iband = '25 (chosen band)'
 ;     endif
      ;;;;地表反射类型
      Ground_Reflectance='0 Homogeneous surface'
      ;;;;方向性
      Directional_effects='0 No directional effects'
      ;;;;表观反射率
      Specify_surface_reflectance='2 (mean spectral value)'
      ;;;;设定大气矫正模型
      Atm_correction_mode='0 Atm. correction Lambertian'
      ;;;;;设定反射比
      Reflectance='-0.3 reflectance (negative value)'
    ; free_lun,lun
    filename= file_search(result[i]+'\*_B1.tif')
    n = N_ELEMENTS(filename) 
    ;用户提示
      print,'6s input'
      ;将输入参数打印在input.txt文件内,方便之后的6s调用   GMT=''
  ; Center_LAT=''
   ;Center_LON=''
      openw,unit,'d:\in.txt',/get_lun
      printf,unit,'7 (TM)'
      printf,unit,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1),' ','(Geometrical conditions)'
      printf,unit,season;
      printf,unit,Aerosol_Modle;
      printf,unit,Atmospheric_condi;
      printf,unit,Target_Altitude;
      printf,unit,Sensor_Altitude;
      printf,unit,iband
      printf,unit,Ground_Reflectance
      printf,unit,Directional_effects
      printf,unit,Specify_surface_reflectance
      printf,unit,Atm_correction_mode
      printf,unit,Reflectance
      free_lun,unit
      ;调用dos命令,将当前运行目录调整至C盘根目录
      cd,'d:\'
      ;调用6s程序
      spawn,'s6.bat'
      ;读取res,获取需要的参数,即表观反射率计算公式中的三个参数
      var = strarr(1)
      exist = 0
      while (~ exist) do begin
        exist = file_test('d:\res.txt');输出路径是用户文件夹;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if exist eq 1 then begin
          ;等待6s程序运行,本程序等待0.1s
          wait,0.1
          ;打开输出参数文件
          openr,lun,'d:\res.txt',/get_lun;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;将输出文件存入事先设置好的变量
          readf,lun,var
;          spawn,'taskkill /im cmd.exe',/nowait,/noshell
;          spawn,'taskkill /f /im notepad.exe',/nowait,/noshell
         ; cd,del c:\res.txt
        
;         CASE !VERSION.OS OF
;        'Windows': CMD = 'DEL'
;          ELSE: CMD = 'rm'
;         ENDCASE
;         SPAWN, CMD + 'C:\res.txt'
           free_lun,lun
         
        endif
      endwhile
      ;查询并输出三个参数
      ;for i = 130, 135 do begin
        strtmp = strsplit(var[0],':',/extract)
        ;查找字段'*       coefficients xa xb xc                 ',并从这里开始读取文件
        ;judge = strmatch(strtmp,'*       coefficients xa xb xc                 ')
        ;if judge[0] eq 1 then begin
          strtmp = strsplit(strtmp[1],' ',/extract)
          ;提取三个参数
          xa = (strtmp[0])
          xb = (strtmp[1])
          xc = (strtmp[2])
       ; endif
     ; endfor
            ;生成四个波段表达式
   expstr =tm5b1exp
   exp_b1 = '(' + xa + '*' + '(' + tm5b1exp + ')' + '-' + xb + ')'+ '/' + '(' + '1' + '+' + xc + '*' + '(' +xa + '*' + '(' + tm5b1exp + ')' + '-' + xb+ ')' + ')'
   ; FOR k=0,n-1 DO BEGIN
    in_name=filename[0]
    envi_open_file, in_name, r_fid=input_fid
        envi_file_query, input_fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = indgen(nb)
         ;indexstr = strpos(fileName[i],'.')
         out_name = 'H:\nd_atm_cor\dn_atm_cor_b1\'+strmid(fileName[0],12,21)+'DN_radi'+'.raw' 
out_fid=INTARR(nb)
    FOR t=0,nb-1 DO BEGIN
ENVI_DOIT, 'math_doit', dims=dims, exp=exp_b1, pos=pos[0], $
fid=input_fid, out_name=out_name, r_fid=r_fid
    ENDFOR
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
   RADIANCE_MULT_BAND_2_value=''
   RADIANCE_MULT_BAND_3_value=''
   RADIANCE_MULT_BAND_4_value=''
   RADIANCE_MULT_BAND_5_value=''
   RADIANCE_MULT_BAND_6_value=''
   RADIANCE_MULT_BAND_7_value=''
   RADIANCE_ADD_BAND_1_value=''
   RADIANCE_ADD_BAND_2_value=''
   RADIANCE_ADD_BAND_3_value=''
   RADIANCE_ADD_BAND_4_value=''
   RADIANCE_ADD_BAND_5_value=''
   RADIANCE_ADD_BAND_6_value=''
   RADIANCE_ADD_BAND_7_value=''
   CORNER_UL_LAT_PRODUCT_value=''
   CORNER_UL_LON_PRODUCT_value=''
   CORNER_UR_LAT_PRODUCT_value=''
   CORNER_UR_LON_PRODUCT_value=''
   CORNER_LL_LAT_PRODUCT_value=''
   CORNER_LL_LON_PRODUCT_value=''
   CORNER_LR_LAT_PRODUCT_value=''
   CORNER_LR_LON_PRODUCT_value=''
   DATE_ACQUIRED_value=''
   DataGetArr=''
   DataGetMonth=''
   DataGetDay=''
   GMT=''
   Center_LAT=''
   Center_LON=''
   season=''
   iband=''
   result=file_search('H:\landset5','*',/test_directory)
   for i=0,n_elements(result)-9 do begin
     asciifile=file_search(result[i]+'\*MTL.txt')

     line_counts=FILE_LINES(asciifile)
   
     ;counts=line_counts[0]+1-1
    openr,lun,asciifile,/get_lun

     void=''
  
     ;for j=0,120 do readf,lun,void
     arr=strarr(line_counts[0])
     
     readf,lun,arr
     for j=0,line_counts[0]-1 do begin
   
       arrsuntmp = strsplit(arr[j],'=',/extract)
       DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE)
       if max(DATE_ACQUIRED_bool) eq 1 then begin
         DATE_ACQUIRED_value = arrsuntmp[1]
         DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract)
         DataGetMonth=DataGetArr[1]
         DataGetDay=DataGetArr[2]
       endif
       SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
       if max(SCENE_CENTER_TIME_bool) eq 1 then begin
         SCENE_CENTER_TIME_value = arrsuntmp[1]
         DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)
         GMT=double(DataGetTimeArr[0])+double(DataGetTimeArr[1])/60+double(strmid(DataGetTimeArr[2],0,8))/3600
       endif
       CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
         CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
         CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
         CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
         CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
         CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
         CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
         CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
         CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
       endif
      
       ;;;;;;;
       RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_1) eq 1 then begin
         RADIANCE_MULT_BAND_1_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_2) eq 1 then begin
         RADIANCE_MULT_BAND_2_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_3) eq 1 then begin
         RADIANCE_MULT_BAND_3_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_4) eq 1 then begin
         RADIANCE_MULT_BAND_4_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_5) eq 1 then begin
         RADIANCE_MULT_BAND_5_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_6) eq 1 then begin
         RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_7) eq 1 then begin
         RADIANCE_MULT_BAND_7_value = arrsuntmp[1]
       
       endif
       RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_1) eq 1 then begin
         RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_2) eq 1 then begin
         RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_3) eq 1 then begin
         RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_4) eq 1 then begin
         RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_5) eq 1 then begin
         RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_6) eq 1 then begin
        RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_7) eq 1 then begin
         RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
       endif
     endfor
     
       free_lun,lun
     ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
     ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then
     ;??????????????????????????????????????????????????????
     ;????????????????????????????????????
      Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
      Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4
     tm5b1exp = 'b1*'+strtrim(RADIANCE_MULT_BAND_1_value,2)+''+RADIANCE_ADD_BAND_1_value+''
     tm5b2exp = 'b2*'+strtrim(RADIANCE_MULT_BAND_2_value,2)+''+RADIANCE_ADD_BAND_2_value+''
     tm5b3exp = 'b3*'+strtrim(RADIANCE_MULT_BAND_3_value,2)+''+RADIANCE_ADD_BAND_3_value+''
     tm5b4exp = 'b4*'+strtrim(RADIANCE_MULT_BAND_4_value,2)+''+RADIANCE_ADD_BAND_4_value+''
     tm5b5exp = 'b5*'+strtrim(RADIANCE_MULT_BAND_5_value,2)+''+RADIANCE_ADD_BAND_5_value+''
     tm5b6exp = 'b6*'+strtrim(RADIANCE_MULT_BAND_6_value,2)+'+'+RADIANCE_ADD_BAND_6_value+''
     tm5b7exp = 'b7*'+strtrim(RADIANCE_MULT_BAND_7_value,2)+''+RADIANCE_ADD_BAND_7_value+''
   ;;;;;;
   ;判断季节
 
    if string(DataGetMonth) ge 3 && string(DataGetMonth) le 8 then begin
      season = '2 Midlatitude Summer'
    endif else begin
      season = '3 Midlatitude Winter'
      endelse
      ;;;;;;;;;;;;
     ;;;;设定气溶胶模型
     Aerosol_Modle='1 Continental Model'
     ;;;;;设定大气条件
     Atmospheric_condi='35 visibility or aot'
     ;;;;;设定目标海拔高度
     Target_Altitude='-0.108 (target level,negative value)'
     ;;;;;设定传感器高度
     Sensor_Altitude='-705 (sensor level)'
     ;;;;;;
    ; for band = 1,6 do begin
      ;根据波段数来设定6s中自定义波段的输入量，分别为1、2、3、4、5、7波段
;      if band eq 1 then begin
;        iband = '25 (chosen band)'
;      endif
;      if band eq 2 then begin
;        iband = '26 (chosen band)'
;      endif
;      if band eq 3 then begin
;        iband = '27 (chosen band)'
;      endif
;      if band eq 4 then begin
;        iband = '28 (chosen band)'
;      endif
;        if band eq 5 then begin
;        iband = '29 (chosen band)'
;      endif 
;       if band eq 6 then begin
        iband = '26 (chosen band)'
 ;     endif
      ;;;;地表反射类型
      Ground_Reflectance='0 Homogeneous surface'
      ;;;;方向性
      Directional_effects='0 No directional effects'
      ;;;;表观反射率
      Specify_surface_reflectance='2 (mean spectral value)'
      ;;;;设定大气矫正模型
      Atm_correction_mode='0 Atm. correction Lambertian'
      ;;;;;设定反射比
      Reflectance='-0.3 reflectance (negative value)'
    ; free_lun,lun
    filename= file_search(result[i]+'\*_B2.tif')
    n = N_ELEMENTS(filename) 
    ;用户提示
      print,'6s input'
      ;将输入参数打印在input.txt文件内,方便之后的6s调用   GMT=''
  ; Center_LAT=''
   ;Center_LON=''
      openw,unit,'d:\in.txt',/get_lun
      printf,unit,'7 (TM)'
      printf,unit,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1),' ','(Geometrical conditions)'
      printf,unit,season;
      printf,unit,Aerosol_Modle;
      printf,unit,Atmospheric_condi;
      printf,unit,Target_Altitude;
      printf,unit,Sensor_Altitude;
      printf,unit,iband
      printf,unit,Ground_Reflectance
      printf,unit,Directional_effects
      printf,unit,Specify_surface_reflectance
      printf,unit,Atm_correction_mode
      printf,unit,Reflectance
      free_lun,unit
      ;调用dos命令,将当前运行目录调整至C盘根目录
      cd,'d:\'
      ;调用6s程序
      spawn,'s6.bat'
      ;读取res,获取需要的参数,即表观反射率计算公式中的三个参数
      var = strarr(1)
      exist = 0
      while (~ exist) do begin
        exist = file_test('d:\res.txt');输出路径是用户文件夹;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if exist eq 1 then begin
          ;等待6s程序运行,本程序等待0.1s
          wait,0.1
          ;打开输出参数文件
          openr,lun,'d:\res.txt',/get_lun;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;将输出文件存入事先设置好的变量
          readf,lun,var
;          spawn,'taskkill /im cmd.exe',/nowait,/noshell
;          spawn,'taskkill /f /im notepad.exe',/nowait,/noshell
         ; cd,del c:\res.txt
        
;         CASE !VERSION.OS OF
;        'Windows': CMD = 'DEL'
;          ELSE: CMD = 'rm'
;         ENDCASE
;         SPAWN, CMD + 'C:\res.txt'
           free_lun,lun
         
        endif
      endwhile
      ;查询并输出三个参数
      ;for i = 130, 135 do begin
        strtmp = strsplit(var[0],':',/extract)
        ;查找字段'*       coefficients xa xb xc                 ',并从这里开始读取文件
        ;judge = strmatch(strtmp,'*       coefficients xa xb xc                 ')
        ;if judge[0] eq 1 then begin
          strtmp = strsplit(strtmp[1],' ',/extract)
          ;提取三个参数
          xa = (strtmp[0])
          xb = (strtmp[1])
          xc = (strtmp[2])
       ; endif
     ; endfor
            ;生成四个波段表达式
   expstr =tm5b2exp
   exp_b2 = '(' + xa + '*' + '(' + tm5b2exp + ')' + '-' + xb + ')'+ '/' + '(' + '1' + '+' + xc + '*' + '(' +xa + '*' + '(' + tm5b2exp + ')' + '-' + xb+ ')' + ')'
   ; FOR k=0,n-1 DO BEGIN
    in_name=filename[0]
    envi_open_file, in_name, r_fid=input_fid
        envi_file_query, input_fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = indgen(nb)
         ;indexstr = strpos(fileName[i],'.')
         out_name = 'H:\nd_atm_cor\dn_atm_cor_b2\'+strmid(fileName[0],12,21)+'DN_radi'+'.raw' 
out_fid=INTARR(nb)
    FOR t=0,nb-1 DO BEGIN
ENVI_DOIT, 'math_doit', dims=dims, exp=exp_b2, pos=pos[0], $
fid=input_fid, out_name=out_name, r_fid=r_fid
    ENDFOR
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
   RADIANCE_MULT_BAND_2_value=''
   RADIANCE_MULT_BAND_3_value=''
   RADIANCE_MULT_BAND_4_value=''
   RADIANCE_MULT_BAND_5_value=''
   RADIANCE_MULT_BAND_6_value=''
   RADIANCE_MULT_BAND_7_value=''
   RADIANCE_ADD_BAND_1_value=''
   RADIANCE_ADD_BAND_2_value=''
   RADIANCE_ADD_BAND_3_value=''
   RADIANCE_ADD_BAND_4_value=''
   RADIANCE_ADD_BAND_5_value=''
   RADIANCE_ADD_BAND_6_value=''
   RADIANCE_ADD_BAND_7_value=''
   CORNER_UL_LAT_PRODUCT_value=''
   CORNER_UL_LON_PRODUCT_value=''
   CORNER_UR_LAT_PRODUCT_value=''
   CORNER_UR_LON_PRODUCT_value=''
   CORNER_LL_LAT_PRODUCT_value=''
   CORNER_LL_LON_PRODUCT_value=''
   CORNER_LR_LAT_PRODUCT_value=''
   CORNER_LR_LON_PRODUCT_value=''
   DATE_ACQUIRED_value=''
   DataGetArr=''
   DataGetMonth=''
   DataGetDay=''
   GMT=''
   Center_LAT=''
   Center_LON=''
   season=''
   iband=''
   result=file_search('H:\landset5','*',/test_directory)
   for i=0,n_elements(result)-9 do begin
     asciifile=file_search(result[i]+'\*MTL.txt')

     line_counts=FILE_LINES(asciifile)
   
     ;counts=line_counts[0]+1-1
    openr,lun,asciifile,/get_lun

     void=''
  
     ;for j=0,120 do readf,lun,void
     arr=strarr(line_counts[0])
     
     readf,lun,arr
     for j=0,line_counts[0]-1 do begin
   
       arrsuntmp = strsplit(arr[j],'=',/extract)
       DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE)
       if max(DATE_ACQUIRED_bool) eq 1 then begin
         DATE_ACQUIRED_value = arrsuntmp[1]
         DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract)
         DataGetMonth=DataGetArr[1]
         DataGetDay=DataGetArr[2]
       endif
       SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
       if max(SCENE_CENTER_TIME_bool) eq 1 then begin
         SCENE_CENTER_TIME_value = arrsuntmp[1]
         DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)
         GMT=double(DataGetTimeArr[0])+double(DataGetTimeArr[1])/60+double(strmid(DataGetTimeArr[2],0,8))/3600
       endif
       CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
         CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
         CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
         CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
         CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
         CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
         CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
         CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
         CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
       endif
      
       ;;;;;;;
       RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_1) eq 1 then begin
         RADIANCE_MULT_BAND_1_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_2) eq 1 then begin
         RADIANCE_MULT_BAND_2_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_3) eq 1 then begin
         RADIANCE_MULT_BAND_3_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_4) eq 1 then begin
         RADIANCE_MULT_BAND_4_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_5) eq 1 then begin
         RADIANCE_MULT_BAND_5_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_6) eq 1 then begin
         RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_7) eq 1 then begin
         RADIANCE_MULT_BAND_7_value = arrsuntmp[1]
       
       endif
       RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_1) eq 1 then begin
         RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_2) eq 1 then begin
         RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_3) eq 1 then begin
         RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_4) eq 1 then begin
         RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_5) eq 1 then begin
         RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_6) eq 1 then begin
        RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_7) eq 1 then begin
         RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
       endif
     endfor
     
       free_lun,lun
     ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
     ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then
     ;??????????????????????????????????????????????????????
     ;????????????????????????????????????
      Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
      Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4
     tm5b1exp = 'b1*'+strtrim(RADIANCE_MULT_BAND_1_value,2)+''+RADIANCE_ADD_BAND_1_value+''
     tm5b2exp = 'b2*'+strtrim(RADIANCE_MULT_BAND_2_value,2)+''+RADIANCE_ADD_BAND_2_value+''
     tm5b3exp = 'b3*'+strtrim(RADIANCE_MULT_BAND_3_value,2)+''+RADIANCE_ADD_BAND_3_value+''
     tm5b4exp = 'b4*'+strtrim(RADIANCE_MULT_BAND_4_value,2)+''+RADIANCE_ADD_BAND_4_value+''
     tm5b5exp = 'b5*'+strtrim(RADIANCE_MULT_BAND_5_value,2)+''+RADIANCE_ADD_BAND_5_value+''
     tm5b6exp = 'b6*'+strtrim(RADIANCE_MULT_BAND_6_value,2)+'+'+RADIANCE_ADD_BAND_6_value+''
     tm5b7exp = 'b7*'+strtrim(RADIANCE_MULT_BAND_7_value,2)+''+RADIANCE_ADD_BAND_7_value+''
   ;;;;;;
   ;判断季节
 
    if string(DataGetMonth) ge 3 && string(DataGetMonth) le 8 then begin
      season = '2 Midlatitude Summer'
    endif else begin
      season = '3 Midlatitude Winter'
      endelse
      ;;;;;;;;;;;;
     ;;;;设定气溶胶模型
     Aerosol_Modle='1 Continental Model'
     ;;;;;设定大气条件
     Atmospheric_condi='35 visibility or aot'
     ;;;;;设定目标海拔高度
     Target_Altitude='-0.108 (target level,negative value)'
     ;;;;;设定传感器高度
     Sensor_Altitude='-705 (sensor level)'
     ;;;;;;
    ; for band = 1,6 do begin
      ;根据波段数来设定6s中自定义波段的输入量，分别为1、2、3、4、5、7波段
;      if band eq 1 then begin
;        iband = '25 (chosen band)'
;      endif
;      if band eq 2 then begin
;        iband = '26 (chosen band)'
;      endif
;      if band eq 3 then begin
;        iband = '27 (chosen band)'
;      endif
;      if band eq 4 then begin
;        iband = '28 (chosen band)'
;      endif
;        if band eq 5 then begin
;        iband = '29 (chosen band)'
;      endif 
;       if band eq 6 then begin
        iband = '27 (chosen band)'
 ;     endif
      ;;;;地表反射类型
      Ground_Reflectance='0 Homogeneous surface'
      ;;;;方向性
      Directional_effects='0 No directional effects'
      ;;;;表观反射率
      Specify_surface_reflectance='2 (mean spectral value)'
      ;;;;设定大气矫正模型
      Atm_correction_mode='0 Atm. correction Lambertian'
      ;;;;;设定反射比
      Reflectance='-0.3 reflectance (negative value)'
    ; free_lun,lun
    filename= file_search(result[i]+'\*_B3.tif')
    n = N_ELEMENTS(filename) 
    ;用户提示
      print,'6s input'
      ;将输入参数打印在input.txt文件内,方便之后的6s调用   GMT=''
  ; Center_LAT=''
   ;Center_LON=''
      openw,unit,'d:\in.txt',/get_lun
      printf,unit,'7 (TM)'
      printf,unit,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1),' ','(Geometrical conditions)'
      printf,unit,season;
      printf,unit,Aerosol_Modle;
      printf,unit,Atmospheric_condi;
      printf,unit,Target_Altitude;
      printf,unit,Sensor_Altitude;
      printf,unit,iband
      printf,unit,Ground_Reflectance
      printf,unit,Directional_effects
      printf,unit,Specify_surface_reflectance
      printf,unit,Atm_correction_mode
      printf,unit,Reflectance
      free_lun,unit
      ;调用dos命令,将当前运行目录调整至C盘根目录
      cd,'d:\'
      ;调用6s程序
      spawn,'s6.bat'
      ;读取res,获取需要的参数,即表观反射率计算公式中的三个参数
      var = strarr(1)
      exist = 0
      while (~ exist) do begin
        exist = file_test('d:\res.txt');输出路径是用户文件夹;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if exist eq 1 then begin
          ;等待6s程序运行,本程序等待0.1s
          wait,0.1
          ;打开输出参数文件
          openr,lun,'d:\res.txt',/get_lun;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;将输出文件存入事先设置好的变量
          readf,lun,var
;          spawn,'taskkill /im cmd.exe',/nowait,/noshell
;          spawn,'taskkill /f /im notepad.exe',/nowait,/noshell
         ; cd,del c:\res.txt
        
;         CASE !VERSION.OS OF
;        'Windows': CMD = 'DEL'
;          ELSE: CMD = 'rm'
;         ENDCASE
;         SPAWN, CMD + 'C:\res.txt'
           free_lun,lun
         
        endif
      endwhile
      ;查询并输出三个参数
      ;for i = 130, 135 do begin
        strtmp = strsplit(var[0],':',/extract)
        ;查找字段'*       coefficients xa xb xc                 ',并从这里开始读取文件
        ;judge = strmatch(strtmp,'*       coefficients xa xb xc                 ')
        ;if judge[0] eq 1 then begin
          strtmp = strsplit(strtmp[1],' ',/extract)
          ;提取三个参数
          xa = (strtmp[0])
          xb = (strtmp[1])
          xc = (strtmp[2])
       ; endif
     ; endfor
            ;生成四个波段表达式
   expstr =tm5b3exp
   exp_b3 = '(' + xa + '*' + '(' + tm5b3exp + ')' + '-' + xb + ')'+ '/' + '(' + '1' + '+' + xc + '*' + '(' +xa + '*' + '(' + tm5b3exp + ')' + '-' + xb+ ')' + ')'
   ; FOR k=0,n-1 DO BEGIN
    in_name=filename[0]
    envi_open_file, in_name, r_fid=input_fid
        envi_file_query, input_fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = indgen(nb)
         ;indexstr = strpos(fileName[i],'.')
         out_name = 'H:\nd_atm_cor\dn_atm_cor_b3\'+strmid(fileName[0],12,21)+'DN_radi'+'.raw' 
out_fid=INTARR(nb)
    FOR t=0,nb-1 DO BEGIN
ENVI_DOIT, 'math_doit', dims=dims, exp=exp_b3, pos=pos[0], $
fid=input_fid, out_name=out_name, r_fid=r_fid
    ENDFOR
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
   RADIANCE_MULT_BAND_2_value=''
   RADIANCE_MULT_BAND_3_value=''
   RADIANCE_MULT_BAND_4_value=''
   RADIANCE_MULT_BAND_5_value=''
   RADIANCE_MULT_BAND_6_value=''
   RADIANCE_MULT_BAND_7_value=''
   RADIANCE_ADD_BAND_1_value=''
   RADIANCE_ADD_BAND_2_value=''
   RADIANCE_ADD_BAND_3_value=''
   RADIANCE_ADD_BAND_4_value=''
   RADIANCE_ADD_BAND_5_value=''
   RADIANCE_ADD_BAND_6_value=''
   RADIANCE_ADD_BAND_7_value=''
   CORNER_UL_LAT_PRODUCT_value=''
   CORNER_UL_LON_PRODUCT_value=''
   CORNER_UR_LAT_PRODUCT_value=''
   CORNER_UR_LON_PRODUCT_value=''
   CORNER_LL_LAT_PRODUCT_value=''
   CORNER_LL_LON_PRODUCT_value=''
   CORNER_LR_LAT_PRODUCT_value=''
   CORNER_LR_LON_PRODUCT_value=''
   DATE_ACQUIRED_value=''
   DataGetArr=''
   DataGetMonth=''
   DataGetDay=''
   GMT=''
   Center_LAT=''
   Center_LON=''
   season=''
   iband=''
   result=file_search('H:\landset5','*',/test_directory)
   for i=0,n_elements(result)-9 do begin
     asciifile=file_search(result[i]+'\*MTL.txt')

     line_counts=FILE_LINES(asciifile)
   
     ;counts=line_counts[0]+1-1
    openr,lun,asciifile,/get_lun

     void=''
  
     ;for j=0,120 do readf,lun,void
     arr=strarr(line_counts[0])
     
     readf,lun,arr
     for j=0,line_counts[0]-1 do begin
   
       arrsuntmp = strsplit(arr[j],'=',/extract)
       DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE)
       if max(DATE_ACQUIRED_bool) eq 1 then begin
         DATE_ACQUIRED_value = arrsuntmp[1]
         DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract)
         DataGetMonth=DataGetArr[1]
         DataGetDay=DataGetArr[2]
       endif
       SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
       if max(SCENE_CENTER_TIME_bool) eq 1 then begin
         SCENE_CENTER_TIME_value = arrsuntmp[1]
         DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)
         GMT=double(DataGetTimeArr[0])+double(DataGetTimeArr[1])/60+double(strmid(DataGetTimeArr[2],0,8))/3600
       endif
       CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
         CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
         CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
         CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
         CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
         CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
         CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
         CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
         CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
       endif
      
       ;;;;;;;
       RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_1) eq 1 then begin
         RADIANCE_MULT_BAND_1_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_2) eq 1 then begin
         RADIANCE_MULT_BAND_2_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_3) eq 1 then begin
         RADIANCE_MULT_BAND_3_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_4) eq 1 then begin
         RADIANCE_MULT_BAND_4_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_5) eq 1 then begin
         RADIANCE_MULT_BAND_5_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_6) eq 1 then begin
         RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_7) eq 1 then begin
         RADIANCE_MULT_BAND_7_value = arrsuntmp[1]
       
       endif
       RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_1) eq 1 then begin
         RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_2) eq 1 then begin
         RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_3) eq 1 then begin
         RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_4) eq 1 then begin
         RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_5) eq 1 then begin
         RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_6) eq 1 then begin
        RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_7) eq 1 then begin
         RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
       endif
     endfor
     
       free_lun,lun
     ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
     ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then
     ;??????????????????????????????????????????????????????
     ;????????????????????????????????????
      Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
      Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4
     tm5b1exp = 'b1*'+strtrim(RADIANCE_MULT_BAND_1_value,2)+''+RADIANCE_ADD_BAND_1_value+''
     tm5b2exp = 'b2*'+strtrim(RADIANCE_MULT_BAND_2_value,2)+''+RADIANCE_ADD_BAND_2_value+''
     tm5b3exp = 'b3*'+strtrim(RADIANCE_MULT_BAND_3_value,2)+''+RADIANCE_ADD_BAND_3_value+''
     tm5b4exp = 'b4*'+strtrim(RADIANCE_MULT_BAND_4_value,2)+''+RADIANCE_ADD_BAND_4_value+''
     tm5b5exp = 'b5*'+strtrim(RADIANCE_MULT_BAND_5_value,2)+''+RADIANCE_ADD_BAND_5_value+''
     tm5b6exp = 'b6*'+strtrim(RADIANCE_MULT_BAND_6_value,2)+'+'+RADIANCE_ADD_BAND_6_value+''
     tm5b7exp = 'b7*'+strtrim(RADIANCE_MULT_BAND_7_value,2)+''+RADIANCE_ADD_BAND_7_value+''
   ;;;;;;
   ;判断季节
 
    if string(DataGetMonth) ge 3 && string(DataGetMonth) le 8 then begin
      season = '2 Midlatitude Summer'
    endif else begin
      season = '3 Midlatitude Winter'
      endelse
      ;;;;;;;;;;;;
     ;;;;设定气溶胶模型
     Aerosol_Modle='1 Continental Model'
     ;;;;;设定大气条件
     Atmospheric_condi='35 visibility or aot'
     ;;;;;设定目标海拔高度
     Target_Altitude='-0.108 (target level,negative value)'
     ;;;;;设定传感器高度
     Sensor_Altitude='-705 (sensor level)'
     ;;;;;;
    ; for band = 1,6 do begin
      ;根据波段数来设定6s中自定义波段的输入量，分别为1、2、3、4、5、7波段
;      if band eq 1 then begin
;        iband = '25 (chosen band)'
;      endif
;      if band eq 2 then begin
;        iband = '26 (chosen band)'
;      endif
;      if band eq 3 then begin
;        iband = '27 (chosen band)'
;      endif
;      if band eq 4 then begin
;        iband = '28 (chosen band)'
;      endif
;        if band eq 5 then begin
;        iband = '29 (chosen band)'
;      endif 
;       if band eq 6 then begin
        iband = '28 (chosen band)'
 ;     endif
      ;;;;地表反射类型
      Ground_Reflectance='0 Homogeneous surface'
      ;;;;方向性
      Directional_effects='0 No directional effects'
      ;;;;表观反射率
      Specify_surface_reflectance='2 (mean spectral value)'
      ;;;;设定大气矫正模型
      Atm_correction_mode='0 Atm. correction Lambertian'
      ;;;;;设定反射比
      Reflectance='-0.3 reflectance (negative value)'
    ; free_lun,lun
    filename= file_search(result[i]+'\*_B4.tif')
    n = N_ELEMENTS(filename) 
    ;用户提示
      print,'6s input'
      ;将输入参数打印在input.txt文件内,方便之后的6s调用   GMT=''
  ; Center_LAT=''
   ;Center_LON=''
      openw,unit,'d:\in.txt',/get_lun
      printf,unit,'7 (TM)'
      printf,unit,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1),' ','(Geometrical conditions)'
      printf,unit,season;
      printf,unit,Aerosol_Modle;
      printf,unit,Atmospheric_condi;
      printf,unit,Target_Altitude;
      printf,unit,Sensor_Altitude;
      printf,unit,iband
      printf,unit,Ground_Reflectance
      printf,unit,Directional_effects
      printf,unit,Specify_surface_reflectance
      printf,unit,Atm_correction_mode
      printf,unit,Reflectance
      free_lun,unit
      ;调用dos命令,将当前运行目录调整至C盘根目录
      cd,'d:\'
      ;调用6s程序
      spawn,'s6.bat'
      ;读取res,获取需要的参数,即表观反射率计算公式中的三个参数
      var = strarr(1)
      exist = 0
      while (~ exist) do begin
        exist = file_test('d:\res.txt');输出路径是用户文件夹;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if exist eq 1 then begin
          ;等待6s程序运行,本程序等待0.1s
          wait,0.1
          ;打开输出参数文件
          openr,lun,'d:\res.txt',/get_lun;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;将输出文件存入事先设置好的变量
          readf,lun,var
;          spawn,'taskkill /im cmd.exe',/nowait,/noshell
;          spawn,'taskkill /f /im notepad.exe',/nowait,/noshell
         ; cd,del c:\res.txt
        
;         CASE !VERSION.OS OF
;        'Windows': CMD = 'DEL'
;          ELSE: CMD = 'rm'
;         ENDCASE
;         SPAWN, CMD + 'C:\res.txt'
           free_lun,lun
         
        endif
      endwhile
      ;查询并输出三个参数
      ;for i = 130, 135 do begin
        strtmp = strsplit(var[0],':',/extract)
        ;查找字段'*       coefficients xa xb xc                 ',并从这里开始读取文件
        ;judge = strmatch(strtmp,'*       coefficients xa xb xc                 ')
        ;if judge[0] eq 1 then begin
          strtmp = strsplit(strtmp[1],' ',/extract)
          ;提取三个参数
          xa = (strtmp[0])
          xb = (strtmp[1])
          xc = (strtmp[2])
       ; endif
     ; endfor
            ;生成四个波段表达式
   expstr =tm5b4exp
   exp_b4 = '(' + xa + '*' + '(' + tm5b4exp + ')' + '-' + xb + ')'+ '/' + '(' + '1' + '+' + xc + '*' + '(' +xa + '*' + '(' + tm5b4exp + ')' + '-' + xb+ ')' + ')'
   ; FOR k=0,n-1 DO BEGIN
    in_name=filename[0]
    envi_open_file, in_name, r_fid=input_fid
        envi_file_query, input_fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = indgen(nb)
         ;indexstr = strpos(fileName[i],'.')
         out_name = 'H:\nd_atm_cor\dn_atm_cor_b4\'+strmid(fileName[0],12,21)+'DN_radi'+'.raw' 
out_fid=INTARR(nb)
    FOR t=0,nb-1 DO BEGIN
ENVI_DOIT, 'math_doit', dims=dims, exp=exp_b4, pos=pos[0], $
fid=input_fid, out_name=out_name, r_fid=r_fid
    ENDFOR
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
   RADIANCE_MULT_BAND_2_value=''
   RADIANCE_MULT_BAND_3_value=''
   RADIANCE_MULT_BAND_4_value=''
   RADIANCE_MULT_BAND_5_value=''
   RADIANCE_MULT_BAND_6_value=''
   RADIANCE_MULT_BAND_7_value=''
   RADIANCE_ADD_BAND_1_value=''
   RADIANCE_ADD_BAND_2_value=''
   RADIANCE_ADD_BAND_3_value=''
   RADIANCE_ADD_BAND_4_value=''
   RADIANCE_ADD_BAND_5_value=''
   RADIANCE_ADD_BAND_6_value=''
   RADIANCE_ADD_BAND_7_value=''
   CORNER_UL_LAT_PRODUCT_value=''
   CORNER_UL_LON_PRODUCT_value=''
   CORNER_UR_LAT_PRODUCT_value=''
   CORNER_UR_LON_PRODUCT_value=''
   CORNER_LL_LAT_PRODUCT_value=''
   CORNER_LL_LON_PRODUCT_value=''
   CORNER_LR_LAT_PRODUCT_value=''
   CORNER_LR_LON_PRODUCT_value=''
   DATE_ACQUIRED_value=''
   DataGetArr=''
   DataGetMonth=''
   DataGetDay=''
   GMT=''
   Center_LAT=''
   Center_LON=''
   season=''
   iband=''
   result=file_search('H:\landset5','*',/test_directory)
   for i=0,n_elements(result)-9 do begin
     asciifile=file_search(result[i]+'\*MTL.txt')

     line_counts=FILE_LINES(asciifile)
   
     ;counts=line_counts[0]+1-1
    openr,lun,asciifile,/get_lun

     void=''
  
     ;for j=0,120 do readf,lun,void
     arr=strarr(line_counts[0])
     
     readf,lun,arr
     for j=0,line_counts[0]-1 do begin
   
       arrsuntmp = strsplit(arr[j],'=',/extract)
       DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE)
       if max(DATE_ACQUIRED_bool) eq 1 then begin
         DATE_ACQUIRED_value = arrsuntmp[1]
         DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract)
         DataGetMonth=DataGetArr[1]
         DataGetDay=DataGetArr[2]
       endif
       SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
       if max(SCENE_CENTER_TIME_bool) eq 1 then begin
         SCENE_CENTER_TIME_value = arrsuntmp[1]
         DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)
         GMT=double(DataGetTimeArr[0])+double(DataGetTimeArr[1])/60+double(strmid(DataGetTimeArr[2],0,8))/3600
       endif
       CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
         CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
         CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
         CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
         CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
         CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
         CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
         CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
         CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
       endif
      
       ;;;;;;;
       RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_1) eq 1 then begin
         RADIANCE_MULT_BAND_1_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_2) eq 1 then begin
         RADIANCE_MULT_BAND_2_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_3) eq 1 then begin
         RADIANCE_MULT_BAND_3_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_4) eq 1 then begin
         RADIANCE_MULT_BAND_4_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_5) eq 1 then begin
         RADIANCE_MULT_BAND_5_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_6) eq 1 then begin
         RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_7) eq 1 then begin
         RADIANCE_MULT_BAND_7_value = arrsuntmp[1]
       
       endif
       RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_1) eq 1 then begin
         RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_2) eq 1 then begin
         RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_3) eq 1 then begin
         RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_4) eq 1 then begin
         RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_5) eq 1 then begin
         RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_6) eq 1 then begin
        RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_7) eq 1 then begin
         RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
       endif
     endfor
     
       free_lun,lun
     ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
     ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then
     ;??????????????????????????????????????????????????????
     ;????????????????????????????????????
      Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
      Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4
     tm5b1exp = 'b1*'+strtrim(RADIANCE_MULT_BAND_1_value,2)+''+RADIANCE_ADD_BAND_1_value+''
     tm5b2exp = 'b2*'+strtrim(RADIANCE_MULT_BAND_2_value,2)+''+RADIANCE_ADD_BAND_2_value+''
     tm5b3exp = 'b3*'+strtrim(RADIANCE_MULT_BAND_3_value,2)+''+RADIANCE_ADD_BAND_3_value+''
     tm5b4exp = 'b4*'+strtrim(RADIANCE_MULT_BAND_4_value,2)+''+RADIANCE_ADD_BAND_4_value+''
     tm5b5exp = 'b5*'+strtrim(RADIANCE_MULT_BAND_5_value,2)+''+RADIANCE_ADD_BAND_5_value+''
     tm5b6exp = 'b6*'+strtrim(RADIANCE_MULT_BAND_6_value,2)+'+'+RADIANCE_ADD_BAND_6_value+''
     tm5b7exp = 'b7*'+strtrim(RADIANCE_MULT_BAND_7_value,2)+''+RADIANCE_ADD_BAND_7_value+''
   ;;;;;;
   ;判断季节
 
    if string(DataGetMonth) ge 3 && string(DataGetMonth) le 8 then begin
      season = '2 Midlatitude Summer'
    endif else begin
      season = '3 Midlatitude Winter'
      endelse
      ;;;;;;;;;;;;
     ;;;;设定气溶胶模型
     Aerosol_Modle='1 Continental Model'
     ;;;;;设定大气条件
     Atmospheric_condi='35 visibility or aot'
     ;;;;;设定目标海拔高度
     Target_Altitude='-0.108 (target level,negative value)'
     ;;;;;设定传感器高度
     Sensor_Altitude='-705 (sensor level)'
     ;;;;;;
    ; for band = 1,6 do begin
      ;根据波段数来设定6s中自定义波段的输入量，分别为1、2、3、4、5、7波段
;      if band eq 1 then begin
;        iband = '25 (chosen band)'
;      endif
;      if band eq 2 then begin
;        iband = '26 (chosen band)'
;      endif
;      if band eq 3 then begin
;        iband = '27 (chosen band)'
;      endif
;      if band eq 4 then begin
;        iband = '28 (chosen band)'
;      endif
;        if band eq 5 then begin
;        iband = '29 (chosen band)'
;      endif 
;       if band eq 6 then begin
        iband = '29 (chosen band)'
 ;     endif
      ;;;;地表反射类型
      Ground_Reflectance='0 Homogeneous surface'
      ;;;;方向性
      Directional_effects='0 No directional effects'
      ;;;;表观反射率
      Specify_surface_reflectance='2 (mean spectral value)'
      ;;;;设定大气矫正模型
      Atm_correction_mode='0 Atm. correction Lambertian'
      ;;;;;设定反射比
      Reflectance='-0.3 reflectance (negative value)'
    ; free_lun,lun
    filename= file_search(result[i]+'\*_B5.tif')
    n = N_ELEMENTS(filename) 
    ;用户提示
      print,'6s input'
      ;将输入参数打印在input.txt文件内,方便之后的6s调用   GMT=''
  ; Center_LAT=''
   ;Center_LON=''
      openw,unit,'d:\in.txt',/get_lun
      printf,unit,'7 (TM)'
      printf,unit,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1),' ','(Geometrical conditions)'
      printf,unit,season;
      printf,unit,Aerosol_Modle;
      printf,unit,Atmospheric_condi;
      printf,unit,Target_Altitude;
      printf,unit,Sensor_Altitude;
      printf,unit,iband
      printf,unit,Ground_Reflectance
      printf,unit,Directional_effects
      printf,unit,Specify_surface_reflectance
      printf,unit,Atm_correction_mode
      printf,unit,Reflectance
      free_lun,unit
      ;调用dos命令,将当前运行目录调整至C盘根目录
      cd,'d:\'
      ;调用6s程序
      spawn,'s6.bat'
      ;读取res,获取需要的参数,即表观反射率计算公式中的三个参数
      var = strarr(1)
      exist = 0
      while (~ exist) do begin
        exist = file_test('d:\res.txt');输出路径是用户文件夹;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if exist eq 1 then begin
          ;等待6s程序运行,本程序等待0.1s
          wait,0.1
          ;打开输出参数文件
          openr,lun,'d:\res.txt',/get_lun;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;将输出文件存入事先设置好的变量
          readf,lun,var
;          spawn,'taskkill /im cmd.exe',/nowait,/noshell
;          spawn,'taskkill /f /im notepad.exe',/nowait,/noshell
         ; cd,del c:\res.txt
        
;         CASE !VERSION.OS OF
;        'Windows': CMD = 'DEL'
;          ELSE: CMD = 'rm'
;         ENDCASE
;         SPAWN, CMD + 'C:\res.txt'
           free_lun,lun
         
        endif
      endwhile
      ;查询并输出三个参数
      ;for i = 130, 135 do begin
        strtmp = strsplit(var[0],':',/extract)
        ;查找字段'*       coefficients xa xb xc                 ',并从这里开始读取文件
        ;judge = strmatch(strtmp,'*       coefficients xa xb xc                 ')
        ;if judge[0] eq 1 then begin
          strtmp = strsplit(strtmp[1],' ',/extract)
          ;提取三个参数
          xa = (strtmp[0])
          xb = (strtmp[1])
          xc = (strtmp[2])
       ; endif
     ; endfor
            ;生成四个波段表达式
   expstr =tm5b5exp
   exp_b5 = '(' + xa + '*' + '(' + tm5b5exp + ')' + '-' + xb + ')'+ '/' + '(' + '1' + '+' + xc + '*' + '(' +xa + '*' + '(' + tm5b5exp + ')' + '-' + xb+ ')' + ')'
   ; FOR k=0,n-1 DO BEGIN
    in_name=filename[0]
    envi_open_file, in_name, r_fid=input_fid
        envi_file_query, input_fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = indgen(nb)
         ;indexstr = strpos(fileName[i],'.')
         out_name = 'H:\nd_atm_cor\dn_atm_cor_b5\'+strmid(fileName[0],12,21)+'DN_radi'+'.raw' 
out_fid=INTARR(nb)
    FOR t=0,nb-1 DO BEGIN
ENVI_DOIT, 'math_doit', dims=dims, exp=exp_b5, pos=pos[0], $
fid=input_fid, out_name=out_name, r_fid=r_fid
    ENDFOR
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
   RADIANCE_MULT_BAND_2_value=''
   RADIANCE_MULT_BAND_3_value=''
   RADIANCE_MULT_BAND_4_value=''
   RADIANCE_MULT_BAND_5_value=''
   RADIANCE_MULT_BAND_6_value=''
   RADIANCE_MULT_BAND_7_value=''
   RADIANCE_ADD_BAND_1_value=''
   RADIANCE_ADD_BAND_2_value=''
   RADIANCE_ADD_BAND_3_value=''
   RADIANCE_ADD_BAND_4_value=''
   RADIANCE_ADD_BAND_5_value=''
   RADIANCE_ADD_BAND_6_value=''
   RADIANCE_ADD_BAND_7_value=''
   CORNER_UL_LAT_PRODUCT_value=''
   CORNER_UL_LON_PRODUCT_value=''
   CORNER_UR_LAT_PRODUCT_value=''
   CORNER_UR_LON_PRODUCT_value=''
   CORNER_LL_LAT_PRODUCT_value=''
   CORNER_LL_LON_PRODUCT_value=''
   CORNER_LR_LAT_PRODUCT_value=''
   CORNER_LR_LON_PRODUCT_value=''
   DATE_ACQUIRED_value=''
   DataGetArr=''
   DataGetMonth=''
   DataGetDay=''
   GMT=''
   Center_LAT=''
   Center_LON=''
   season=''
   iband=''
   result=file_search('H:\landset5','*',/test_directory)
   for i=0,n_elements(result)-9 do begin
     asciifile=file_search(result[i]+'\*MTL.txt')

     line_counts=FILE_LINES(asciifile)
   
     ;counts=line_counts[0]+1-1
    openr,lun,asciifile,/get_lun

     void=''
  
     ;for j=0,120 do readf,lun,void
     arr=strarr(line_counts[0])
     
     readf,lun,arr
     for j=0,line_counts[0]-1 do begin
   
       arrsuntmp = strsplit(arr[j],'=',/extract)
       DATE_ACQUIRED_bool = strmatch(strtrim(arrsuntmp[0],2),'DATE_ACQUIRED',/FOLD_CASE)
       if max(DATE_ACQUIRED_bool) eq 1 then begin
         DATE_ACQUIRED_value = arrsuntmp[1]
         DataGetArr=strsplit(DATE_ACQUIRED_value,'-',/extract)
         DataGetMonth=DataGetArr[1]
         DataGetDay=DataGetArr[2]
       endif
       SCENE_CENTER_TIME_bool = strmatch(strtrim(arrsuntmp[0],2),'SCENE_CENTER_TIME',/FOLD_CASE)
       if max(SCENE_CENTER_TIME_bool) eq 1 then begin
         SCENE_CENTER_TIME_value = arrsuntmp[1]
         DataGetTimeArr=strsplit(SCENE_CENTER_TIME_value,':',/extract)
         GMT=double(DataGetTimeArr[0])+double(DataGetTimeArr[1])/60+double(strmid(DataGetTimeArr[2],0,8))/3600
       endif
       CORNER_UL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LAT_PRODUCT) eq 1 then begin
         CORNER_UL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UL_LON_PRODUCT) eq 1 then begin
         CORNER_UL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LAT_PRODUCT) eq 1 then begin
         CORNER_UR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_UR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_UR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_UR_LON_PRODUCT) eq 1 then begin
         CORNER_UR_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LAT_PRODUCT) eq 1 then begin
         CORNER_LL_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LL_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LL_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LL_LON_PRODUCT) eq 1 then begin
         CORNER_LL_LON_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LAT_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LAT_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LAT_PRODUCT) eq 1 then begin
         CORNER_LR_LAT_PRODUCT_value = arrsuntmp[1]
       endif
       CORNER_LR_LON_PRODUCT = strmatch(strtrim(arrsuntmp[0],2),'CORNER_LR_LON_PRODUCT',/FOLD_CASE)
       if max(CORNER_LR_LON_PRODUCT) eq 1 then begin
         CORNER_LR_LON_PRODUCT_value = arrsuntmp[1]
       endif
      
       ;;;;;;;
       RADIANCE_MULT_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_1',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_1) eq 1 then begin
         RADIANCE_MULT_BAND_1_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_2',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_2) eq 1 then begin
         RADIANCE_MULT_BAND_2_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_3',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_3) eq 1 then begin
         RADIANCE_MULT_BAND_3_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_4',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_4) eq 1 then begin
         RADIANCE_MULT_BAND_4_value = arrsuntmp[1]
        
       endif
       RADIANCE_MULT_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_5',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_5) eq 1 then begin
         RADIANCE_MULT_BAND_5_value = arrsuntmp[1]
       
       endif
       RADIANCE_MULT_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_6',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_6) eq 1 then begin
         RADIANCE_MULT_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_MULT_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_MULT_BAND_7',/FOLD_CASE)
       if max(RADIANCE_MULT_BAND_7) eq 1 then begin
         RADIANCE_MULT_BAND_7_value = arrsuntmp[1]
       
       endif
       RADIANCE_ADD_BAND_1 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_1',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_1) eq 1 then begin
         RADIANCE_ADD_BAND_1_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_2 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_2',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_2) eq 1 then begin
         RADIANCE_ADD_BAND_2_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_3 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_3',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_3) eq 1 then begin
         RADIANCE_ADD_BAND_3_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_4 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_4',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_4) eq 1 then begin
         RADIANCE_ADD_BAND_4_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_5 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_5',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_5) eq 1 then begin
         RADIANCE_ADD_BAND_5_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_6 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_6',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_6) eq 1 then begin
        RADIANCE_ADD_BAND_6_value = arrsuntmp[1]
       endif
       RADIANCE_ADD_BAND_7 = strmatch(strtrim(arrsuntmp[0],2),'RADIANCE_ADD_BAND_7',/FOLD_CASE)
       if max(RADIANCE_ADD_BAND_7) eq 1 then begin
         RADIANCE_ADD_BAND_7_value = arrsuntmp[1]
       endif
     endfor
     
       free_lun,lun
     ;str[WHERE(STRMATCH(arr[j], 'RADIANCE_MULT_BAND_1', /FOLD_CASE) EQ 1)]
     ;if(strmatch(arrsuntmp,'RADIANCE_MULT_BAND_1')!=0) then
     ;??????????????????????????????????????????????????????
     ;????????????????????????????????????
      Center_LAT=(double(CORNER_UL_LAT_PRODUCT_value)+double(CORNER_UR_LAT_PRODUCT_value)+double(CORNER_LL_LAT_PRODUCT_value)+double(CORNER_LR_LAT_PRODUCT_value))/4
      Center_LON=(double(CORNER_UL_LON_PRODUCT_value)+double(CORNER_UR_LON_PRODUCT_value)+double(CORNER_LL_LON_PRODUCT_value)+double(CORNER_LR_LON_PRODUCT_value))/4
     tm5b1exp = 'b1*'+strtrim(RADIANCE_MULT_BAND_1_value,2)+''+RADIANCE_ADD_BAND_1_value+''
     tm5b2exp = 'b2*'+strtrim(RADIANCE_MULT_BAND_2_value,2)+''+RADIANCE_ADD_BAND_2_value+''
     tm5b3exp = 'b3*'+strtrim(RADIANCE_MULT_BAND_3_value,2)+''+RADIANCE_ADD_BAND_3_value+''
     tm5b4exp = 'b4*'+strtrim(RADIANCE_MULT_BAND_4_value,2)+''+RADIANCE_ADD_BAND_4_value+''
     tm5b5exp = 'b5*'+strtrim(RADIANCE_MULT_BAND_5_value,2)+''+RADIANCE_ADD_BAND_5_value+''
     tm5b6exp = 'b6*'+strtrim(RADIANCE_MULT_BAND_6_value,2)+'+'+RADIANCE_ADD_BAND_6_value+''
     tm5b7exp = 'b7*'+strtrim(RADIANCE_MULT_BAND_7_value,2)+''+RADIANCE_ADD_BAND_7_value+''
   ;;;;;;
   ;判断季节
 
    if string(DataGetMonth) ge 3 && string(DataGetMonth) le 8 then begin
      season = '2 Midlatitude Summer'
    endif else begin
      season = '3 Midlatitude Winter'
      endelse
      ;;;;;;;;;;;;
     ;;;;设定气溶胶模型
     Aerosol_Modle='1 Continental Model'
     ;;;;;设定大气条件
     Atmospheric_condi='35 visibility or aot'
     ;;;;;设定目标海拔高度
     Target_Altitude='-0.108 (target level,negative value)'
     ;;;;;设定传感器高度
     Sensor_Altitude='-705 (sensor level)'
     ;;;;;;
    ; for band = 1,6 do begin
      ;根据波段数来设定6s中自定义波段的输入量，分别为1、2、3、4、5、7波段
;      if band eq 1 then begin
;        iband = '25 (chosen band)'
;      endif
;      if band eq 2 then begin
;        iband = '26 (chosen band)'
;      endif
;      if band eq 3 then begin
;        iband = '27 (chosen band)'
;      endif
;      if band eq 4 then begin
;        iband = '28 (chosen band)'
;      endif
;        if band eq 5 then begin
;        iband = '29 (chosen band)'
;      endif 
;       if band eq 6 then begin
        iband = '30 (chosen band)'
 ;     endif
      ;;;;地表反射类型
      Ground_Reflectance='0 Homogeneous surface'
      ;;;;方向性
      Directional_effects='0 No directional effects'
      ;;;;表观反射率
      Specify_surface_reflectance='2 (mean spectral value)'
      ;;;;设定大气矫正模型
      Atm_correction_mode='0 Atm. correction Lambertian'
      ;;;;;设定反射比
      Reflectance='-0.3 reflectance (negative value)'
    ; free_lun,lun
    filename= file_search(result[i]+'\*_B7.tif')
    n = N_ELEMENTS(filename) 
    ;用户提示
      print,'6s input'
      ;将输入参数打印在input.txt文件内,方便之后的6s调用   GMT=''
  ; Center_LAT=''
   ;Center_LON=''
      openw,unit,'d:\in.txt',/get_lun
      printf,unit,'7 (TM)'
      printf,unit,strtrim(DataGetMonth,1),' ',strtrim(DataGetDay,1),' ',strtrim(GMT,1),' ',strtrim(Center_LON,1),' ',strtrim(Center_LAT,1),' ','(Geometrical conditions)'
      printf,unit,season;
      printf,unit,Aerosol_Modle;
      printf,unit,Atmospheric_condi;
      printf,unit,Target_Altitude;
      printf,unit,Sensor_Altitude;
      printf,unit,iband
      printf,unit,Ground_Reflectance
      printf,unit,Directional_effects
      printf,unit,Specify_surface_reflectance
      printf,unit,Atm_correction_mode
      printf,unit,Reflectance
      free_lun,unit
      ;调用dos命令,将当前运行目录调整至C盘根目录
      cd,'d:\'
      ;调用6s程序
      spawn,'s6.bat'
      ;读取res,获取需要的参数,即表观反射率计算公式中的三个参数
      var = strarr(1)
      exist = 0
      while (~ exist) do begin
        exist = file_test('d:\res.txt');输出路径是用户文件夹;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        if exist eq 1 then begin
          ;等待6s程序运行,本程序等待0.1s
          wait,0.1
          ;打开输出参数文件
          openr,lun,'d:\res.txt',/get_lun;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;将输出文件存入事先设置好的变量
          readf,lun,var
;          spawn,'taskkill /im cmd.exe',/nowait,/noshell
;          spawn,'taskkill /f /im notepad.exe',/nowait,/noshell
         ; cd,del c:\res.txt
        
;         CASE !VERSION.OS OF
;        'Windows': CMD = 'DEL'
;          ELSE: CMD = 'rm'
;         ENDCASE
;         SPAWN, CMD + 'C:\res.txt'
           free_lun,lun
         
        endif
      endwhile
      ;查询并输出三个参数
      ;for i = 130, 135 do begin
        strtmp = strsplit(var[0],':',/extract)
        ;查找字段'*       coefficients xa xb xc                 ',并从这里开始读取文件
        ;judge = strmatch(strtmp,'*       coefficients xa xb xc                 ')
        ;if judge[0] eq 1 then begin
          strtmp = strsplit(strtmp[1],' ',/extract)
          ;提取三个参数
          xa = (strtmp[0])
          xb = (strtmp[1])
          xc = (strtmp[2])
       ; endif
     ; endfor
            ;生成四个波段表达式
   expstr =tm5b7exp
   exp_b7 = '(' + xa + '*' + '(' + tm5b7exp + ')' + '-' + xb + ')'+ '/' + '(' + '1' + '+' + xc + '*' + '(' +xa + '*' + '(' + tm5b7exp + ')' + '-' + xb+ ')' + ')'
   ; FOR k=0,n-1 DO BEGIN
    in_name=filename[0]
    envi_open_file, in_name, r_fid=input_fid
        envi_file_query, input_fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = indgen(nb)
         ;indexstr = strpos(fileName[i],'.')
         out_name = 'H:\nd_atm_cor\dn_atm_cor_b7\'+strmid(fileName[0],12,21)+'DN_radi'+'.raw' 
out_fid=INTARR(nb)
    FOR t=0,nb-1 DO BEGIN
ENVI_DOIT, 'math_doit', dims=dims, exp=exp_b7, pos=pos[0], $
fid=input_fid, out_name=out_name, r_fid=r_fid
    ENDFOR
ENDFOR
;;;;;;;;;;;由于第六波段没有相应的序号值 ，不做
end