PRO EXAMPLE_ROI_THRESH_DOIT

compile_opt IDL2

 

; First restore all the base save files.

envi, /restore_base_save_files

 

; Initialize ENVI Classic and send all errors

; and warnings to the file batch.txt

envi_batch_init, log_file='batch.txt'

 

; Open the input file

envi_open_file, 'D:\work\农田损毁和农作物损失评估\测试数据\分类前\ghj1b-ccd1-9-64-20100622.img', r_fid=fid

if (fid eq -1) then begin

   envi_batch_exit

   return

endif

 

; Use the POS keyword to select the first band

; to use for the ROI.

envi_file_query, fid, dims=dims

pos = [0]

roi_name = 'test ROI'

out_name = 'test.roi'

 

; Perform the ROI threshold

envi_doit, 'roi_thresh_doit', $

   fid=fid, pos=pos, dims=dims, $

   max_thresh=255, min_thresh=30, $

   /no_query, roi_color=5, $

   roi_name=roi_name, roi_id=roi_id

 

; Save the ROI to a file

envi_save_rois, out_name, roi_id

 

END

