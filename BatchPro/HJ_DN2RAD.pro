;*************************** HJ_DN2rad.pro程序说明 *********************************
;程序说明：输入HJ星CCD 4个波段数据和相同文件名的XML头文件,从头文件中提取增益和偏量，完成辐射校正输出辐亮度。
;编写人:shi liang
;编写时间:2012年9月6
;修改时间:2012年 月
;*************************************************************************************
 ;****************************** 输入 *************************************************
;输入项:
;用户自行选取头文件“*.XML”，程序根据头文件名字从头文件所在文件夹读取1-4波段tif文件
;如选取HJ1B-CCD1-14-84-20090510-L20000111113.XML头文件
;对应的影像文件如下：
;HJ1B-CCD1-14-84-20090510-L20000111113-1.tif   BLUE波段
;HJ1B-CCD1-14-84-20090510-L20000111113-2.tif   GREEN波段
;HJ1B-CCD1-14-84-20090510-L20000111113-3.tif   RED波段
;HJ1B-CCD1-14-84-20090510-L20000111113-4.tif   NIR波段
;*************************************************************************************;
;******************************* 输出 ************************************************
;HJ1B-CCD1-14-84-20090510-L20000111113-1-rad.tif & HJ1B-CCD1-14-84-20090510-L20000111113-1-rad.HDR
;HJ1B-CCD1-14-84-20090510-L20000111113-2-rad.tif & HJ1B-CCD1-14-84-20090510-L20000111113-2-rad.HDR
;HJ1B-CCD1-14-84-20090510-L20000111113-3-rad.tif & HJ1B-CCD1-14-84-20090510-L20000111113-3-rad.HDR
;HJ1B-CCD1-14-84-20090510-L20000111113-4-rad.tif & HJ1B-CCD1-14-84-20090510-L20000111113-4-rad.HDR
;*************************************************************************************
 
;*************************************************************************************
;************************ObtainGainAndOffset(calstring,gains,offsets)*****************
; /// <summary>
; /// 提取增益偏置函数
; /// </summary>
; /// <functionhead>ObtainGainAndOffset(string calstring,fltarr gains,
;fltarr,offsets)</functionhead>
; /// <param name="calstring">HJ星头文件absCalibType字符串</param>
; /// <returns name=gains>增益数组</returns>
; /// <returns name=offsets>偏置数组</returns>
;*************************************************************************************
FUNCTION ObtainGainAndOffset,calstring  ;,gains,offsets
;calstring:(gain1,Fielddata,L=DN/g+L0,W*m^(-2)*sr^(-1)*um^(-1))B1:g 0.5329, L0 1.6146,
;B2:g 0.52895, L0 4.0052, B3:g 0.68495, L0 6.2193, B4:g 0.72245, L0 2.8302
      gains = FLTARR(4)
      Offsets = FLTARR(4)
;B1:g 0.5329, L0 1.6146, B2:g 0.52895, L0 4.0052, B3:g 0.68495, L0 6.2193, B4:g 0.72245,
 ;L0 2.8302
     
      index1 = strpos(calstring,'B1')
      index2 = strpos(calstring,',',index1 + 5)
     
      gains[0] = float(strmid(calstring,index1 + 5,index2 - index1-5))
      index1 = strpos(calstring,'L0',index2)
      index2 = strpos(calstring,',',index1 + 2)
      offsets[0] = float(strmid(calstring,index1 + 2,index2 - index1-2))
     
      index1 = strpos(calstring,'B2')
      index2 = strpos(calstring,',',index1 + 5)
     
      gains[1] = float(strmid(calstring,index1 + 5,index2 - index1-5))
      index1 = strpos(calstring,'L0',index2)
      index2 = strpos(calstring,',',index1 + 2)
      offsets[1] = float(strmid(calstring,index1 + 2,index2 - index1-2))
      
      index1 = strpos(calstring,'B3')
      index2 = strpos(calstring,',',index1 + 5)
     
      gains[2] = float(strmid(calstring,index1 + 5,index2 - index1-5))
      index1 = strpos(calstring,'L0',index2)
      index2 = strpos(calstring,',',index1 + 2)
      offsets[2] = float(strmid(calstring,index1 + 2,index2 - index1-2))
     
      index1 = strpos(calstring,'B4')
      index2 = strpos(calstring,',',index1 + 5)
      gains[3] = float(strmid(calstring,index1 + 5,index2 - index1-5))
      index1 = strpos(calstring,'L0',index2)
      offsets[3] = float(strmid(calstring,index1 + 2,strlen(calstring) - index1-2))
      gains_offsets=[[gains],[offsets]]
      
      return,gains_offsets
END
;*************************************************************************************

;*************************************************************************************
;*************************************************************************************
;主程序
PRO HJ_DN2rad
 
  compile_opt idl2;
  ENVI,/restore_base_save_files
  ENVI_batch_INIT,log_File = 'batch.txt' ;启动ENVI
 
  ;打开存放HJ星数据的文件夹
  dirname = dialog_pickfile(title='open a folder that contain *.TIF file and *.XML head file',/DIRECTORY)
  cd,dirname
 
   files = file_search('*.XML');返回XML类型头文件个数
   FileCount = N_ELEMENTS(files);返回文件数
 
  IF FileCount EQ 0 THEN BEGIN
   ENVI_Batch_Exit
   return
  ENDIF

;***********************************************************************************
 FOR fff=0,FileCount-1 DO BEGIN
   inputfile=files[fff]
   xmlfile=inputfile
   filelen=STRLEN(inputfile)
   tiffile=STRMID(inputfile,0,filelen-4)
   tiffile1=tiffile+'-1.TIF';band1 tif文件名
   tiffile2=tiffile+'-2.TIF';band2 tif文件名
   tiffile3=tiffile+'-3.TIF';band3 tif文件名
   tiffile4=tiffile+'-4.TIF';band4 tif文件名
   outname4=tiffile+'-4-rad.TIF'  ;辐射校正后的数据文件名NIR波段
   outname3=tiffile+'-3-rad.TIF'  ;辐射校正后的数据文件名Red波段
   outname2=tiffile+'-2-rad.TIF'  ;辐射校正后的数据文件名Green波段
   outname1=tiffile+'-1-rad.TIF'  ;辐射校正后的数据文件名Blue波段
   rad_file1 = File_Search(outname1)
   rad_file2 = File_Search(outname2)
   rad_file3 = File_Search(outname3)
   rad_file4 = File_Search(outname4)
   rad_fileCount1 = SIZE(rad_file1)
   rad_fileCount2 = SIZE(rad_file2)
   rad_fileCount3 = SIZE(rad_file3)
   rad_fileCount4 = SIZE(rad_file4)
   if rad_FileCount1[0] gt 0 then begin
      print,outname1+' is already exist'
   endif
   if rad_FileCount2[0] gt 0 then begin
      print,outname1+' is already exist'
   endif
   if rad_FileCount3[0] gt 0 then begin
      print,outname1+' is already exist'
   endif
   if rad_FileCount4[0] gt 0 then begin
      print,outname1+' is already exist'
   endif
   if rad_FileCount1[0] gt 0 and rad_FileCount2[0] gt 0 and $
      rad_FileCount3[0] gt 0 and rad_FileCount4[0] gt 0 then begin
      print,tiffile+'radiance file is already exist'
      continue
   endif
 
  ;***********************************************************************************
  ;从XML头文件中获取影像对应的增益和偏差
  oDocument = OBJ_NEW('IDLffXMLDOMDocument')
  oDocument->Load, FILENAME=xmlfile
  oPlugin = oDocument->GetFirstChild()
  oNodeList = oPlugin->GetElementsByTagName('absCalibType')
  oName = oNodeList->Item(0)
  oNameText = oName->GetFirstChild()
  calstring=oNameText->GetNodeValue()
  g_L0 = ObtainGainAndOffset(calstring)
  g=g_L0[*,0]
  L0=g_L0[*,1]

  ;************************************************************
  envi_open_file, tiffile1,r_fid=fid1
  envi_open_file, tiffile2,r_fid=fid2
  envi_open_file, tiffile3,r_fid=fid3
  envi_open_file, tiffile4,r_fid=fid4
  ;如果文件打开失败退出程序
    if (fid1 eq -1 or fid2 eq -1 or fid3 eq -1 or fid4 eq -1) then return
   
  ;查询文件信息 ，用于生成ENVI标准格式头文件
  fid = fid1   ;以一个文件为例，读取其投影信息就OK
  envi_file_query, fid, data_type=data_type, xstart=xstart,$
    ystart=ystart,nb=nb,ns=ns,nl =nl,dims=dims
   
  ;获取输入影像投影信息，用于生成ENVI标准格式头文件
  mapinfo=envi_get_map_info(fid=fid)
  openw, lun4,outname4, /get_lun;输出rad-NIR
  openw, lun3,outname3, /get_lun;输出rad-Red
  openw, lun2,outname2, /get_lun;输出rad-Green
  openw, lun1,outname1, /get_lun;输出rad-Blue
 
   tile_Band4_ID = envi_init_tile(fid4, 0 , interleave=0, $
    num_tiles=num_tiles, xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4])            ;NIR
 
   tile_Band3_ID = envi_init_tile(fid3, 0 , interleave=0, $
    num_tiles=num_tiles, xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4]);RED
  
   tile_Band2_ID = envi_init_tile(fid2, 0 , interleave=0, $
    num_tiles=num_tiles, xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4]) ;GREEN

   tile_Band1_ID = envi_init_tile(fid1, 0 , interleave=0, $
    num_tiles=num_tiles, xs=dims[1], xe=dims[2], $
    ys=dims[3], ye=dims[4])  ;BLUE
 
 for i=0L, num_tiles-1 do begin
  ;对分块数据进行处理
   DN4 = envi_get_tile(tile_Band4_ID, i, band_index=band_index)
   DN3 = envi_get_tile(tile_Band3_ID, i, band_index=band_index)
   DN2 = envi_get_tile(tile_Band2_ID, i, band_index=band_index)
   DN1 = envi_get_tile(tile_Band1_ID, i, band_index=band_index)
  ;***********************************************************************
    ;计算表观反射率
    rad4 = DN4/g[3]+L0[3]
    rad3 = DN3/g[2]+L0[2]
    rad2 = DN2/g[1]+L0[1]
    rad1 = DN1/g[0]+L0[0]
   
    script4= where(DN4 eq 0 ,count4)
    if(count4 gt 0) then  rad4[script4] = -1 ;设置背景值
    script3= where(DN3 eq 0 ,count3)
    if(count3 gt 0) then  rad3[script3] = -1 ;设置背景值
    script2= where(DN2 eq 0 ,count2)
    if(count2 gt 0) then  rad2[script2] = -1 ;设置背景值
    script1= where(DN1 eq 0 ,count1)
    if(count1 gt 0) then  rad1[script1] = -1 ;设置背景值
    print, i, band_index
    ;将数据写入文件    
    writeu, lun4,rad4
    writeu, lun3,rad3
    writeu, lun2,rad2
    writeu, lun1,rad1
 
  endfor
    ;释放数组资源
    rad4 = 0    
    rad3 = 0
    rad2 = 0
    rad1 = 0
    DN4 = 0
    DN3 = 0
    DN2 = 0
    DN1 = 0
  ;释放文件单元号
  free_lun, lun1,lun2,lun3,lun4
  ;写入ENVI头文件 ,data_type=4 代表浮点数
  envi_setup_head, fname=outname4, ns=ns, nl=nl, nb=1, $
    data_type=4, offset=0, interleave=0, $
    xstart=xstart+dims[1], ystart=ystart+dims[3], map_info=mapinfo,BNAMES = ['rad_NIR'],$
    descrip='rad_NIR', /write, /open
  envi_setup_head, fname=outname3, ns=ns, nl=nl, nb=1, $
    data_type=4, offset=0, interleave=0, $
    xstart=xstart+dims[1], ystart=ystart+dims[3], map_info=mapinfo,BNAMES = ['rad_Red'],$
    descrip='rad_Red', /write, /open
  envi_setup_head, fname=outname2, ns=ns, nl=nl, nb=1, $
    data_type=4, offset=0, interleave=0, $
    xstart=xstart+dims[1], ystart=ystart+dims[3], map_info=mapinfo,BNAMES = ['rad_Green'],$
    descrip='rad_Green', /write, /open
  envi_setup_head, fname=outname1, ns=ns, nl=nl, nb=1, $
    data_type=4, offset=0, interleave=0, $
    xstart=xstart+dims[1], ystart=ystart+dims[3], map_info=mapinfo,BNAMES = ['rad_Blue'],$
    descrip='rad_Blue', /write, /open
 ;分块应用结束，释放资源
 envi_tile_done, tile_Band4_ID
 envi_tile_done, tile_Band3_ID
 envi_tile_done, tile_Band2_ID
 envi_tile_done, tile_Band1_ID
 ;多个单波段文件组合成一个多波段文件
; ENVI_DOIT,'envi_layer_stacking_doit',$
;             fid=fid,pos=pos,dims=dims,$
;             out_bName=out_bNames,$
;             out_dt=out_dt,out_name=out_name,$
;             interp=2,out_ps=out_ps,$
;             out_proj=out_proj,r_fid=r_fid,tTitle='LayerStacking...'
ENDFOR ;END OF TIFF  Files
 
     envi_batch_exit
END
