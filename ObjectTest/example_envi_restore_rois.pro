PRO EXAMPLE_ENVI_restore_rois 

compile_opt IDL2

envi, /restore_base_save_files 

envi_batch_init, log_file='batch.txt' 

;ѡ���ļ�����
envi_restore_rois,envi_pickfile(title='ѡ���ļ�',filter='*.roi')
;��ȡROI_ids
roi_ids=envi_get_roi_ids(fid=fid)
;--------------ȡ��ROI�ļ���ÿһ��ID��ROI�Ӽ�------------------------------
i=0
;��ȡroi_ids�ĳ���
nROI=N_ELEMENTS(roi_ids)
;��ʼ����ά����
  ab=['a','b']
  data=[[ab]]
;����ÿ��ID�е�Ԫ������
For i=0,nROI-1 DO BEGIN

  ;ͨ��id��ȡROI��Ϣ
  ENVI_GET_ROI_INFORMATION,roi_ids[i],ROI_COLORS=ROI_COLORS, $
  ROI_NAMES=ROI_NAMES,NL=NL,NPTS=NPTS,NS=NS 

;~~~~~~~~~~~~~~~~~ȡ��ROI���ƺͶ�Ӧ��ɫ����~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;��ȡָ���ַ�λ��
  PrePos=strpos(ROI_NAMES,'[')
  PostPos=strpos(ROI_NAMES,']')
  ;ȡ�ַ����м�
  ROIColorName=strmid(ROI_NAMES,PrePos+1,PostPos-PrePos-1)
  ;ȥ���ո�
  ROIName=STRTRIM(strmid(ROI_NAMES,0,PrePos),2)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;^^^^^^^^^^^^^^^^��װ��ά����^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  RNames=[ROIName,ROIColorName]

  data=[[data],[RNames]]
;print,data
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  information=ROI_NAMES
  info=dialog_message(/info,title='ROI�ļ���Ϣ',information)  

ENDFOR

;-------------------------------------------------------------------------
ENVI_DELETE_ROIS,/all

;envi_batch_exit 

END

