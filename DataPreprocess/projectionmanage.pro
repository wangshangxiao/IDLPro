pro projectionmanage

InputFile='D:\SpatialData\TMtest\input\jxtest-isodata.tif'
proj=GetProjection(InputFile)
print,proj

end

;-----------------------------GetProjectionByInputFile-----------------------------------------
Function GetProjection,InputFile

  compile_opt IDL2 
  envi,/restore_base_save_files
  envi_batch_init, log_file='batch.txt'
  ; Select a file 
  
  envi_open_file,InputFile,r_fid=fid 
  if (fid eq -1) then begin
      envi_batch_exit
      return,0
  endif
  
  ; Get the projection information 
  proj = ENVI_GET_PROJECTION(FID = fid) 
  
  ;PRINT, proj
  return,proj
  envi_batch_exit
END

