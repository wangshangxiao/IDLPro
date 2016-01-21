pro example_envi_creat_roi
  
compile_opt IDL2

envi, /restore_base_save_files 

envi_batch_init, log_file='batch.txt'

  roi_id = envi_create_roi(color=5, name='Square', $
  
     ns=512, nl=512) 
  
  xpts = [100, 200, 200, 100, 100] 
  
  ypts = [100, 100, 200, 200, 100] 
  
  envi_define_roi, roi_id, /polygon, xpts=xpts, ypts=ypts
  roi_id1 = envi_create_roi(color=4, name='xy', $
  
     ns=512, nl=512) 
  
  xpts = [50, 100, 100, 50, 50] 
  
  ypts = [50, 50, 100, 100, 50] 
  
  envi_define_roi, roi_id1, /polygon, xpts=xpts, ypts=ypts
  
  
  roi_ids=0
  roi_ids=[roi_ids,roi_id]
  roi_ids=[roi_ids,roi_id1]
  roi_ids=roi_ids[1:*]
  
  print,'roi_ids=',roi_ids
  
  ;;roi_ids = envi_get_roi_ids()
  envi_save_rois, 'test2.roi', roi_ids

  ENVI_DELETE_ROIS,/all
  
  ; Exit ENVI Classic

;envi_batch_exit 

end