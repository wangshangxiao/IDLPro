PRO EXAMPLE_ENVI_restore_rois 

compile_opt IDL2

envi, /restore_base_save_files 

envi_batch_init, log_file='batch.txt' 

;选择文件窗口
envi_restore_rois,envi_pickfile(title='选择文件',filter='*.roi')
;获取ROI_ids
roi_ids=envi_get_roi_ids(fid=fid)
;--------------取出ROI文件中每一个ID的ROI子集------------------------------
i=0
;获取roi_ids的长度
nROI=N_ELEMENTS(roi_ids)
;初始化二维数组
  ab=['a','b']
  data=[[ab]]
;遍历每个ID中的元素内容
For i=0,nROI-1 DO BEGIN

  ;通过id获取ROI信息
  ENVI_GET_ROI_INFORMATION,roi_ids[i],ROI_COLORS=ROI_COLORS, $
  ROI_NAMES=ROI_NAMES,NL=NL,NPTS=NPTS,NS=NS 

;~~~~~~~~~~~~~~~~~取出ROI名称和对应颜色名称~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;获取指定字符位置
  PrePos=strpos(ROI_NAMES,'[')
  PostPos=strpos(ROI_NAMES,']')
  ;取字符串中间
  ROIColorName=strmid(ROI_NAMES,PrePos+1,PostPos-PrePos-1)
  ;去掉空格
  ROIName=STRTRIM(strmid(ROI_NAMES,0,PrePos),2)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;^^^^^^^^^^^^^^^^组装二维数组^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  RNames=[ROIName,ROIColorName]

  data=[[data],[RNames]]
;print,data
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  information=ROI_NAMES
  info=dialog_message(/info,title='ROI文件信息',information)  

ENDFOR

;-------------------------------------------------------------------------
ENVI_DELETE_ROIS,/all

;envi_batch_exit 

END

