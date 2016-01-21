PRO TestProject
  inputFile='D:\77211356\Data\GF\HuBei\basedata\TM8_8\mosaic\envitif\LC81230392014279LGN00_B8_1.tif'
  ReferFile='D:\77211356\Data\GF\HuBei\basedata\TM8_8\mosaic\envitif\LC81230382014279LGN00_B8_1.tif'
  outFile='D:\77211356\Data\GF\HuBei\basedata\TM8_8\mosaic\envitif\LC81230392014279LGN00_B8_1_r.tif'
  ProjectionConvert,inputFile,ReferFile,outFile,TIFF=TIFF,Message=Message
END

PRO ProjectionConvert,inputFile,ReferFile,outFile,TIFF=TIFF,Message=Message
  ;���ܽ�inputFile�����ϵת����ReferFile�����ϵ,outFileΪ����ļ�
  COMPILE_OPT idl2
  ;    inputFile='D:\20090831.tif'
  ;    ReferFile='D:\��Ŀ\cropwatch\data\cropland_0.25degree_1%_1bit.tif'
  ;    outFile='E:\20090831.tif'
  ;    TIFF=1
  ENVI,/RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT,LOG_FILE="C:\envi_Preprocessing.Log"
  ENVI_OPEN_FILE,inputFile[0],R_FID=fid
  IF fid EQ -1 THEN BEGIN
    Message='��Ч��Ӱ���ļ�:'+inputFile[0]
    RETURN
  ENDIF
  ENVI_OPEN_FILE,ReferFile,R_FID=referfid
  IF referfid EQ -1 THEN BEGIN
    Message='��Ч��Ӱ���ļ�:'+ReferFile
    RETURN
  ENDIF
  
  outDIR=FILE_DIRNAME(outFile,/MARK_DIRECTORY)
  CATCH,error
  IF error NE 0 THEN GOTO,next
  FILE_MKDIR,outDIR
  next:
  CATCH,/CANCEL
  ;��������ļ����
  FileMainName=FILE_BASENAME(outFile,'.tif',/FOLD_CASE)
  IF KEYWORD_SET(TIFF) THEN BEGIN
    outTempFile=outDIR+FileMainName+'NewProjTemp'
    outFile=outDIR+FileMainName+'.tif'
  ENDIF ELSE BEGIN
    outTempFile=outDIR+FileMainName
  ENDELSE
  ;�ж��ļ�����Ƿ�ΪĿ�����
  outProj=ENVI_GET_PROJECTION(FID=referfid)
  ENVI_FILE_QUERY,fid,dims=dims,nb=nb
  ENVI_CONVERT_FILE_MAP_PROJECTION, fid=fid, r_fid=TempFid, $
    pos=LINDGEN(nb),dims=dims,o_proj=outProj,WARP_METHOD=3,out_name=outTempFile
  IF TempFid EQ -1 THEN BEGIN
    ENVI_BATCH_EXIT
    Message=STRARR(FILE_LINES("C:\envi_Preprocessing.Log"))
    OPENR,lun,"C:\envi_Preprocessing.Log",/GET_LUN
    READF,lun,Message
    FREE_LUN,lun
    Message=STRJOIN(Message,STRING(10b))
    RETURN
  ENDIF
  IF KEYWORD_SET(TIFF) THEN BEGIN
    ENVI_FILE_QUERY,TempFid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
      , FID=TempFid, OUT_NAME=outFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG, id=TempFid,/DELETE,/REMOVE
  ENDIF
  ENVI_BATCH_EXIT
  Message=''
END