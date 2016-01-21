;+
;:Description:
;    Mask Raster data by Evf file.
;
;:Author: tiands@esrichina.com.cn
;
;:Date: 2013-9-5 10:59:58
;-

PRO maskimg_byevf
  COMPILE_OPT idl2
  
  ENVI,/RESTORE_BASE_SAVE_FILES
  ;initialize ENVI in batch mode:
  ENVI_BATCH_INIT
  
  fileextension='tif'
  ;ѡ�������ļ���
  folder=ENVI_PICKFILE(TITLE='Select image Data Folder',/DIRECTORY)
  ;ѡ��evf
  evfname=DIALOG_PICKFILE(title='Select Evf ')
  
  IF (folder EQ '') THEN RETURN
  ;��������
  imagefiles=FILE_SEARCH(folder,'*.'+fileextension,/FOLD_CASE,COUNT=count);�����fold_case��Ҫ��ѯ
  IF (count EQ 0) THEN BEGIN
    void=DIALOG_MESSAGE(['Unable to locate image tile datasets in the selected folder :',$
      '',folder])
    !QUIET=quietInit
    RETURN
  ENDIF
  ;����ļ���
  outFolder=ENVI_PICKFILE(TITLE='Select Folder For Output',/DIRECTORY)
  IF (outFolder EQ '') THEN BEGIN
    !QUIET=quietInit
    RETURN
  ENDIF
  outFolder = outFolder + PATH_SEP()
  CD, outFolder, CURRENT=current
  
  FOR i = 0 ,(count-1) DO BEGIN
    ;open dataset:
    ENVI_OPEN_FILE, imagefiles[i], R_FID=fid
    IF (fid EQ -1) THEN BEGIN
      CONTINUE
    ENDIF
    ;����ļ�
    out_name=outFolder+FILE_BASENAME(imagefiles[i],fileExtension)+'bil'
    ;��Ĥ����
    SPATIALSUBSET,fid,evfName,out_name
  ENDFOR
END


;����EVF�ļ�����Ĥ
PRO SPATIALSUBSET,data_fid,evfName,out_name
  COMPILE_OPT IDL2
  ENVI_FILE_QUERY,data_fid,BNAMES= BNAMES,ns=ns,nl=nl,nb=nb
  ;��ʸ���ļ�
  evf_id = ENVI_EVF_OPEN(evfName)
  ;��ȡ�����Ϣ
  ENVI_EVF_INFO, evf_id, num_recs=num_recs, $
    data_type=data_type, projection=projection, $
    layer_name=layer_name
  roi_ids = LONARR(num_recs)
  ;���������ϵͳ
  ;    oproj=envi_get_projection(fid=data_fid)
  ;��ȡ������¼�ĵ���
  FOR i=0,num_recs-1 DO BEGIN
    record = ENVI_EVF_READ_RECORD(evf_id, i)
    ;    ;ת��ͶӰ�����
    ;    envi_convert_projection_coordinates, $
    ;   record[0,*],record[1,*], projection, $
    ;   oxmap, oymap, oproj
    ;ת��Ϊ�ļ�����;���������oxmap, oymap
    ;ԭ������   ENVI_CONVERT_FILE_COORDINATES,data_fid,xmap,ymap,record[0,*],record[1,*]
    ENVI_CONVERT_FILE_COORDINATES,data_fid,xmap,ymap,record[0,*],record[1,*]
    ;����ROI
    roi_id = ENVI_CREATE_ROI(color=4,ns = ns , nl = nl)
    ENVI_DEFINE_ROI, roi_id, /polygon, xpts=REFORM(xMap), ypts=REFORM(yMap)
    roi_ids[i] = roi_id
    ;��¼XY�����䣬�ü���
    IF i EQ 0 THEN BEGIN
      xmin = ROUND(MIN(xMap,max = xMax))
      yMin = ROUND(MIN(yMap,max = yMax))
      
    ENDIF ELSE BEGIN
      xmin = xMin < ROUND(MIN(xMap))
      xMax = xMax > ROUND(MAX(xMap))
      yMin = yMin < ROUND(MIN(yMap))
      yMax = yMax > ROUND(MAX(yMap))
    ENDELSE
  ENDFOR
  xMin = xMin >0
  xmax = xMax < ns-1
  yMin = yMin >0
  ymax = yMax < nl-1
  ;������Ĥ���ü�����
  ENVI_MASK_DOIT,$
    AND_OR =1, $
    /IN_MEMORY, $
    ROI_IDS= roi_ids, $ ;ROI��ID
    ns = ns, nl = nl, $
    /inside, $ ;�����ڻ���
    r_fid = m_fid
  out_dims = [-1,xMin,xMax,yMin,yMax]
  
  ENVI_MASK_APPLY_DOIT, FID = data_fid, POS = INDGEN(nb), DIMS = out_dims, $
    M_FID = m_fid, M_POS = [0], VALUE = 0, $
    OUT_BNAME= BNAMES+' mask',IN_MEMORY=0,out_name=out_name,r_fid=r_fid
  ;��Ĥ�ļ�ID�Ƴ�
  ENVI_FILE_MNG, id =m_fid,/remove
  ;ENVI_FILE_MNG, id =data_fid,/remove
END
