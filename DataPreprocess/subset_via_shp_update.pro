PRO Subset_via_shp_update
  COMPILE_OPT idl2
  ENVI,/restore_base_save_files
  envi_batch_init,LOG_FILE='batch.log'
  ;��Ҫ�ü���ͼ��
  image_dir='D:\SpatialData\TMtest\input\' ;�����ļ���ŵ�Ŀ¼������Ӧ�޸�
  image_files=file_search(image_dir,'*.tif',count=numfiles)  ;������Ӧ���ļ���ʽ�޸Ĺ������� 
     
  for i=0,numfiles-1 do begin
    image_file=image_files[i]
    if strlen(image_file) eq 0 then return
    
    ENVI_OPEN_FILE, image_file, r_fid=fid, /no_interactive_query, /no_realize 
    IF fid EQ -1 THEN  RETURN
    ENVI_FILE_QUERY, fid, file_type=file_type, nl=nl, ns=ns,dims=dims,nb=nb
    
    ;��shape�ļ�
    ;shapefile = DIALOG_PICKFILE(title='choose the SHP file:',filter='*.shp')
    shapefile=file_search(image_dir,'*.shp')
    
    if strlen(shapefile) eq 0 then return
    oshp = OBJ_NEW('IDLffshape',shapefile)
    oshp->Getproperty,n_entities=n_ent,Attribute_info=attr_info,$
      n_attributes=n_attr,Entity_type=ent_type
      
    roi_shp = LONARR(n_ent)
    FOR ishp = 0,n_ent-1 DO BEGIN
      entitie = oshp->Getentity(ishp)
      
      IF entitie.shape_type EQ 5 THEN BEGIN
        record = *(entitie.vertices)
        
        ;ת���ļ�����
        ENVI_CONVERT_FILE_COORDINATES,fid,xmap,ymap,record[0,*],record[1,*]
        ;����ROI
        roi_shp[ishp] = ENVI_CREATE_ROI(ns=ns,nl=nl)
        ENVI_DEFINE_ROI,roi_shp[ishp],/polygon,xpts=REFORM(xmap),ypts=REFORM(ymap)
        ;��¼X,Y�����䣬�ü���
        IF ishp EQ 0 THEN BEGIN
          xMin = ROUND(MIN(xMap,max = xMax))
          yMin = ROUND(MIN(yMap,max = yMax))
          
        ENDIF ELSE BEGIN
          xMin = xMin < ROUND(MIN(xMap))
          xMax = xMax > ROUND(MAX(xMap))
          yMin = yMin < ROUND(MIN(yMap))
          yMax = yMax > ROUND(MAX(yMap))
        ENDELSE
      ENDIF
      
      oshp->Destroyentity,entitie
    ENDFOR;ishp
    xMin = xMin >0
    xMax = xMax < ns-1
    yMin = yMin >0
    yMax = yMax < nl-1
    
    
    ;�ж�����ļ�·������ԭ�ļ������������
    outfiledir=file_dirname(image_file,/MARK_DIRECTORY)
    out_name = outfiledir +'\' +file_baseName(image_file,'.tif')+'_roi.img
    
    out_dims = [-1,xMin,xMax,yMin,yMax]
    pos = INDGEN(nb)
    
    ENVI_DOIT,'ENVI_SUBSET_VIA_ROI_DOIT',background=0,fid=fid,dims=out_dims,out_name=out_name,$
      ns = ns, nl = nl,pos=pos,roi_ids=roi_shp
      
    endfor  
      
    tmp = DIALOG_MESSAGE('���н���!',/info)
    envi_batch_exit
  END
  
  
  
  
  
  
  
