
;影像裁剪
pro CARTPROCESSING::mask,b_fid,m_fid,out_name=out_name,out_bname=out_bname
  compile_opt IDL2
  ; out_name = 'C:\Users\xuechen\Desktop\掩膜\huquyanmo24444444444'
  envi_file_query,b_fid,nb=nb,dims=dims
  pos = LINDGEN(nb)
  m_pos = [0]
  ;判断是否传入了out_name
  if (not keyword_set(out_name)) then begin
    IN_MEMORY=1
  endif else begin
    IN_MEMORY=0
  endelse
  
  ENVI_MASK_APPLY_DOIT, FID = b_fid, POS = pos, DIMS = dims, $
  
    M_FID = m_fid, M_POS = m_pos, VALUE =0, $
    out_bname=out_bname,$
    
    IN_MEMORY = IN_MEMORY, R_FID = fid , OUT_NAME = out_name 
end

;pro CARTPROCESSING::glake
;;执行该运算两个图像要大小一样
;  compile_opt idl2
;  ; 执行波段运算函数
;  t_fid = [self.fid,self.lake_fid]
;for i=0 ,self.nb-1 do begin
;  pos = [i,0]
;  exp = 'double(b1)*b2'
;  ENVI_DOIT, 'math_doit',fid=t_fid, pos=pos, dims=self.dims, $
;    exp=exp,$
;    r_fid=gr_fid,/in_memory
;    self.gr_array[i]=gr_fid
;   endfor
;   ;对裁剪的tif影像合成
;   ; print,self.gr_array
;end

;合成计算的ndvi ndwi等
pro CARTPROCESSING::layer_stacking,out_name
  compile_opt IDL2
  self.NDVI
  self.NDWI
  self.PC
  self.TASCAP
  self.TEXTURE
  
  envi_batch_status_window, /off
  
  envi_file_query,self.ndvi_fid, ns=ndvi_ns, nl=ndvi_nl, nb=ndvi_nb
  envi_file_query,self.ndwi_fid, ns=ndwi_ns, nl=ndwi_nl, nb=ndwi_nb
  envi_file_query,self.pc_fid, ns=pc_ns, nl=pc_nl, nb=pc_nb
  envi_file_query,self.tascap_fid, ns=tascap_ns, nl=tascap_nl, nb=tascap_nb
  envi_file_query,self.texture_fid, ns=texture_ns, nl=texture_nl, nb=texture_nb
  envi_file_query,self.fid, ns=huqu_ns, nl=huqu_nl
  
  nb = ndvi_nb+ndwi_nb +pc_nb+tascap_nb+texture_nb +self.huqu_nb-2
  
  fid = lonarr(nb)
 
  pos = lonarr(nb)
  
  dims = lonarr(5,nb)
  
  
  for i=0L,self.huqu_nb-1 do begin
  
    fid[i] = self.fid
    
    pos[i] = i
    
    dims[0,i] = [-1,0,huqu_ns-1,0,huqu_nl-1]
    
  endfor
  
  for i=self.huqu_nb,self.huqu_nb+ndvi_nb-1 do begin
  
    fid[i] = self.ndvi_fid
    
    pos[i] = i-self.huqu_nb
    
    dims[0,i] = [-1,0,ndvi_ns-1,0,ndvi_nl-1]
    
  endfor
  for i=self.huqu_nb+ndvi_nb,self.huqu_nb+ndvi_nb+ndwi_nb-1 do begin
  
    fid[i] = self.ndwi_fid
    
    pos[i] = i-(self.huqu_nb+ndvi_nb)
    
    dims[0,i] = [-1,0,ndwi_ns-1,0,ndwi_nl-1]
    
  endfor
  for i=self.huqu_nb+ndvi_nb+ndwi_nb,self.huqu_nb+ndvi_nb+ndwi_nb+pc_nb-2 do begin
  
    fid[i] = self.pc_fid
    
    pos[i] = i-(self.huqu_nb+ndvi_nb+ndwi_nb)
    
    dims[0,i] = [-1,0,pc_ns-1,0,pc_nl-1]
    
  endfor
  ;
  for i=self.huqu_nb+ndvi_nb+ndwi_nb+pc_nb-1,$
    self.huqu_nb+ndvi_nb+ndwi_nb+pc_nb+texture_nb-2 do begin
    
      fid[i] = self.texture_fid
      
      pos[i] = i-(self.huqu_nb+ndvi_nb+ndwi_nb+pc_nb-1)
      
      dims[0,i] = [-1,0,texture_ns-1,0,texture_nl-1]
      
    endfor
    ;
    for i=self.huqu_nb+ndvi_nb+ndwi_nb+pc_nb+texture_nb-1,nb-1 do begin
    
      fid[i] = self.tascap_fid
      
      pos[i] = i-(self.huqu_nb+ndvi_nb+ndwi_nb+pc_nb+texture_nb-1)
      
      dims[0,i] = [-1,0,tascap_ns-1,0,tascap_nl-1]
      
    endfor
    
;    OUT_BNAME=['Band 1','Band 2','Band 3','Band 4','NDVI','NDWI','PC Band 1',$
;            'PC Band 2','PC Band 3',$
;            'mean ','variance' ,'homogeneity ','contrast ',$
;            'dissimilarity' ,'entropy ','second moment' ,'correlation ',$
;            'Soil Brightness Index','Green Veg Index','Yellow Strff Index']
;    
      ;out_name='C:\Users\xuechen\Desktop\掩膜\hecheng222222222'
    
      out_proj = envi_get_projection(fid=self.ndwi_fid,pixel_size=out_ps)
    envi_doit, 'envi_layer_stacking_doit', $
    
      fid=fid, pos=pos, dims=dims, $
      
      out_dt=4, $
      interp=1, out_ps=out_ps, $
      
      out_proj=out_proj, r_fid=layer_fid,in_memory=1;,out_name=out_name
  ;print,layer_fid
      
      
  ;
  ;    ;利用band math水体提取
  ;    envi_file_query,self.ndwi_fid,dims=ndwi_dims
  ;    t_fid = [self.ndwi_fid]
  ;    pos = [0]
  ;    exp = 'double(b1 gt 0.1638)'
  ;    out_bname=['lake']
  ;    ;out_name = dialog_pickfile()
  ;    envi_doit, 'math_doit', $
  ;      fid=t_fid, pos=pos, dims=ndwi_dims, $
  ;      exp=exp,in_memory=1,$
  ;      out_bname=out_bname,$
  ;      r_fid=lake_fid2
  ;
  
  envi_batch_status_window, /on
      ;二次掩膜（对合成影像）
  
      OUT_BNAME=['Band 1','Band 2','Band 3','Band 4','NDVI','NDWI','PC Band 1',$
        'PC Band 2','PC Band 3',$
        'Mean ','Variance' ,'Homogeneity ','Contrast ',$
        'Dissimilarity' ,'Entropy ','Second moment' ,'Correlation ',$
        'Soil Brightness Index','Green Veg Index','Yellow Strff Index']
      self.mask,layer_fid,self.lake_fid,out_name=out_name,out_bname=OUT_BNAME
  ;
      
      
  end
  
  
  ;计算纹理
  PRO CARTPROCESSING::TEXTURE
    compile_opt IDL2
    envi_file_query,self.pc_fid,nb=pc_nb,dims=pc_dims
    pos = lindgen(pc_nb)
    method = lonarr(8) + 1
    direction = [1,1]
    OUT_BNAME=['Mean ','Variance' ,'Homogeneity ','Contrast ',$
      'Dissimilarity' ,'Entropy ','Second moment' ,'Correlation ']
    ;out_name = dialog_pickfile()
    envi_doit, 'texture_cooccur_doit', $
    
      fid=self.pc_fid, pos=pos[0], dims=pc_dims, $
      
      method=method, kx=3, ky=3, $
      g_levels=64,$
      
      direction=direction, $
      
      OUT_BNAME=OUT_BNAme,$
      r_fid=texture_fid,in_memory=1;,out_name=out_name
    self.texture_fid=texture_fid
  ;print,self.texture_fid
  END
  
  
  
  ;计算缨帽
  PRO CARTPROCESSING::TASCAP
    compile_opt IDL2
    pos = lindgen(self.huqu_nb)
    ;out_name = dialog_pickfile()
    envi_doit, 'tascap_doit', $
      fid=self.fid, pos=pos, dims=self.huqu_dims, $
      file_type=1, $
      r_fid=tascap_fid,in_memory=1 ;,out_name=out_name
    self.tascap_fid=tascap_fid
    
  end
  
  
  
  ;计算主成分
  pro CARTPROCESSING::PC
    compile_opt IDL2
    pos = LINDGEN(self.huqu_nb)
    ;pos = LINDGEN(3)
     ;out_name = dialog_pickfile()
    ENVI_DOIT, 'ENVI_STATS_DOIT', $
      FID = self.fid, POS = pos, DIMS = self.huqu_dims, $
      MEAN = avg, EVAL = eval, EVEC = evec, $
      COMP_FLAG = 5
      
;     out_name=dialog_pickfile()
      
    OUT_BNAME=['PC Band 1','PC Band 2','PC Band 3','PC Band 4']
    
    ENVI_DOIT, 'PC_ROTATE', $
      FID = self.fid, POS = pos, DIMS = self.huqu_dims, $
      MEAN = avg, EVAL = eval, EVEC = evec, $
      OUT_DT = 4,/no_plot,$
      OUT_BNAME=OUT_BNAME,$
      FORWARD=1,$
      R_FID = pc_fid,in_memory=1;,OUT_NAME=out_name
    self.pc_fid=pc_fid
    
  end
  
  
  
  
  ;计算水体指数
  pro CARTPROCESSING::NDWI
  
    compile_opt IDL2
    t_fid = [self.fid,self.fid]
    pos = [1,3]
    exp = 'double((float(b1)-float(b2))/(float(b1)+float(b2)))'
    out_bname=['NDWI']
    ;out_name = dialog_pickfile()
    envi_doit, 'math_doit', $
      fid=t_fid, pos=pos, dims=self.huqu_dims, $
      exp=exp,in_memory=1,$
      out_bname=out_bname,$
      r_fid=ndwi_fid  ;,out_name=out_name
    self.ndwi_fid=ndwi_fid
  ;print,self.ndwi_fid
  end
  
  
  
  ;计算归一化植被指数
  pro CARTPROCESSING::NDVI
    compile_opt IDL2
    pos = [4,3] - 1
    ; out_name = dialog_pickfile()
    out_bname=['NDVI']
    envi_doit, 'ndvi_doit', $
      fid=self.fid, pos=pos, dims=self.huqu_dims, $
      /check, OUT_DT=4, $
      out_bname=out_bname,$
      r_fid=ndvi_fid,in_memory=1;,out_name=out_name
    self.ndvi_fid=ndvi_fid
  ;print,self.ndvi_fid
  END
  
  
  
  
  pro CARTPROCESSING::Cleanup
    envi_file_mng, id=self.mask_fid, /remove
    envi_file_mng, id=self.ndvi_fid, /remove
    envi_file_mng, id=self.ndwi_fid, /remove
    envi_file_mng, id=self.pc_fid, /remove
    envi_file_mng, id=self.tascap_fid, /remove
    envi_file_mng, id=self.texture_fid, /remove
  end
  
  
  
  
  function CARTPROCESSING::Init,imagepath,lakepath;,lakepath2
  
    compile_opt IDL2
    ; First restore all the base save files.
    envi, /restore_base_save_files
    ; Initialize ENVI and send all errors
    ; and warnings to the file batch.txt
    envi_batch_init, log_file='batch.txt'
    
    ;envi_open_file,lakepath2,r_fid=lake_fid2
    envi_open_file,lakepath,r_fid=lake_fid
    envi_open_file,imagepath,r_fid=image_fid
    envi_file_query,image_fid,nb=huqu_nb,dims=huqu_dims
    ; envi_file_query, image_fid, dims=dims,nb=nb
    
    if (image_fid eq -1) then begin
      envi_batch_exit
      return,-1
    endif
    self.imagepath=imagepath
    self.lakepath=lakepath
    self.lake_fid=lake_fid
    ;self.lake_fid2=lake_fid2
    self.huqu_nb=huqu_nb
    self.huqu_dims=huqu_dims
    self.fid=image_fid
    return,1
    
  end
  
  pro CARTPROCESSING__DEFINE
    compile_opt IDL2
    
    struct={CARTPROCESSING,$
      imagepath:'',$
      fid:0,$
      huqu_dims:[0L,0L,0L,0L,0L],$
      ndwi_fid:0,$
      ; nb:0L,$
      huqu_nb:0L,$
      pc_fid:0,$
      tascap_fid:0,$
      texture_fid:0,$
      lake_fid:0,$
      ;lake_fid2:0,$
      lakepath:'',$
      ; gr_array:[0L,0L,0L,0L],$
      mask_fid:0,$
      ;m_fid:0,$
      mask_fid2:0,$
      mask_dims:[0L,0L,0L,0L,0L],$
      mask_nb:0L,$
      ndvi_fid:0}
  end