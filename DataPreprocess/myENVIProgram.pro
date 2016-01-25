pro myENVIProgram
compile_opt idl2
on_error, 2

; General error handler
Catch, error
if (error ne 0) then begin
  Catch, /CANCEL
  if obj_valid(envi) then $
    envi.ReportError, "Error: " + !error_state.msg
  message, /RESET
  return
endif

envi=ENVI(/CURRENT)

batchfiles = ["D:\SpatialData\遥感实验数据\实验2\can_tmr.img"]

foreach file, batchfiles do begin

   rasterloop = envi.OpenRaster(file)
   rasterloop_fid = ENVIRasterToFID(rasterLoop)
   envi_file_query, rasterloop_fid, DIMS=rasterloop_dims, NB=rasterloop_nb, BNAMES=rasterloop_bnames, FNAME=rasterloop_fname

   ; Perform unsupervised classification.
   envi_doit, 'class_doit', FID=rasterloop_fid, DIMS=rasterloop_dims, POS=Lindgen(rasterloop_nb), $
            METHOD=4, NUM_CLASSES=5, MIN_CLASSES=2, ITERATIONS=1, $
            ISO_MERGE_DIST=5.0, ISO_SPLIT_STD=1, ISO_MIN_PIXELS=1, ISO_MERGE_PAIRS=2, CHANGE_THRESH=5.0, $
            OUT_BNAME='Classification ('+File_basename(rasterloop_fname)+')', $
            R_FID=classif01_fid, OUT_NAME=envi.getTemporaryFilename()
   
endforeach

end
