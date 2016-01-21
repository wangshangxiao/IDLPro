;+ 
;ENVI���ο������ܴ��� 
; 
;Author: DYQ 
;�������ۣ� 
; http://hi.baidu.com/dyqwrp 
; http://bbs.esrichina-bj.cn/ESRI/?fromuid=9806 
;������ 
; ����shapeʸ���ļ��ü� 
; 
;���÷����� 
; cal_subset,infile, shapefile, resultfile 
; infile:���ü���դ���ļ� 
; shapefile��ʸ���ļ� 
; resultfile���ü���洢��� 
;- 
PRO cal_subset,infile, shapefile, resultfile 
  compile_opt idl2 
  CATCH, Error_status 
  errorshow = 'Sorry to see the error,'+ $ 
    ' please send the error Information to "dongyq@esrichina-bj.cn"' 
  IF Error_status NE 0 THEN BEGIN 
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$ 
      !ERROR_STATE.MSG,/error,title = '������ʾ!') 
    return 
  ENDIF 
  shapeobj = OBJ_NEW('IDLffShape', shapefile) 
  ENVI_OPEN_FILE,infile,r_fid = fid 
  ENVI_FILE_QUERY, fid, ns = ns, nb = nb, nl = nl, dims = dims,BNAMES = BNAMES   
  shapeobj->GETPROPERTY, N_Entities = nEntities   
  ; 
  ; shape_type =5--�����  8-- �������� 
  ;BOUNDS �߽�ֵ 
  ; 
  roi_ids = LONARR(nEntities>1) 
  FOR i=0, nEntities-1 DO BEGIN 
    entitie = shapeobj->GETENTITY(i) 
    ;����������ת�����������κβ��� 
    IF (entitie.SHAPE_TYPE EQ 5)  THEN BEGIN 
      record = *(entitie.VERTICES)       
      ;ת��Ϊ�ļ����� 
      ENVI_CONVERT_FILE_COORDINATES,fid,xmap,ymap,record[0,*],record[1,*] 
      ;����ROI 
      roi_ids[i] = ENVI_CREATE_ROI(color=4,  $ 
        ns = ns ,  nl = nl) 
      ENVI_DEFINE_ROI, roi_ids[i], /polygon, xpts=REFORM(xMap), ypts=REFORM(yMap) 
      ;roi_ids[i] = roi_id 
      ;��¼XY�����䣬�ü��� 
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
    ENDIF 
    shapeobj->DESTROYENTITY, entitie     
  ENDFOR 
  OBJ_DESTROY, shapeobj 
  ; 
  xMin = xMin >0 
  xmax = xMax < ns-1 
  yMin = yMin >0 
  ymax = yMax < nl-1 
  out_dims = [-1,xMin,xMax,yMin,yMax] 
  ;��ȡENVI�����ò��� 
  cfg = envi_get_configuration_values() 
  tmppath = cfg.DEFAULT_TMP_DIRECTORY 
  ;������Ĥ���ü����� 
  ENVI_MASK_DOIT,$ 
    AND_OR =1, $ 
    OUT_NAME = tmppath+path_sep()+'void.mask', $ 
    ROI_IDS= roi_ids, $ ;ROI��ID 
    ns = ns, nl = nl, $ 
    /inside, $ ;�����ڻ��� 
    r_fid = m_fid 
  ENVI_MASK_APPLY_DOIT, FID = fid, POS = INDGEN(nb), DIMS = out_dims, $ 
    M_FID = m_fid, M_POS = [0], VALUE = 0, $ 
    out_name = resultfile;,$ 
    ;out_bnames = BNAMES+"("+"subset by "+STRTRIM(FILE_BASENAME(shapefile),2)+")" 
  ;��Ĥ�ļ�ID�Ƴ� 
  ENVI_FILE_MNG, id =m_fid,/remove 
END