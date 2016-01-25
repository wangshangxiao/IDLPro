PRO EXAMPLE_ENVI_DEFINE_ROI 

compile_opt IDL2

envi, /restore_base_save_files 

envi_batch_init, log_file='batch.txt' 

ENVI_SELECT, fid=fid,dims=dims,pos=pos
if (fid[0] eq -1) then return

envi_file_query, fid, dims=dims

roi_id=envi_create_roi(ns=ns, nl=nl, $

   color=4, name='Square')

; 

; Define the square and add 

; the polygon object to the ROI 

; 

xpts = [100, 200, 200, 100, 100] 

ypts = [100, 100, 200, 200, 100] 

envi_define_roi, roi_id, /polygon, $ 

   xpts=xpts, ypts=ypts


; Exit ENVI Classic

envi_batch_exit 

END

