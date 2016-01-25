PRO L8CALIBRATE
  COMPILE_OPT IDL2
  ; Start the application
  e = ENVI(/HEADLESS)

  ; A Landsat 8 OLI dataset consists of one TIFF file per band,
  ; with an associated metadata file (*_MTL.txt). Open the
  ; metadata file to automatically read the gains and offsets.

  

  File = File_Search('F:\HXFarm\TM\LC81180272014164LGN00', '*_MTL.txt',/FOLD_CASE,count=count)
  
  FOR i=0,count-1 DO BEGIN
    Raster = e.OpenRaster(File[i])
    outPath= file_dirname(File[i])
    ; Get the radiometric calibration task from the catalog of ENVI tasks.
    Task = ENVITask('RadiometricCalibration')
  
    ; Define inputs. Since radiance is the default calibration method
    ; you do not need to specify it here.
    Task.Input_Raster = Raster[0] ; Bands 1-7
    Task.Output_Data_Type = 'Float'
    Task.SCALE_FACTOR =0.10
    
    outputfile=outPath+'\'+FILE_BASENAME(File[i],'_MTL.txt',/FOLD_CASE)+'_rc.dat'
    print,'第',i+1,'景      ',outputfile
    Task.OUTPUT_RASTER_URI = outputfile
    
    
    output = File_Search(outputfile,ncount=ncount)
    if ncount gt 0 then begin
      print,outputfile+' is already exist'
      break
    endif
    
    ; Define output raster URI
    ;Task.Output_Raster_URI = e.GetTemporaryFilename()
  
    ; Run the task
    Task.Execute
  
    ; Get the data collection
    ;DataColl = e.Data
  
    ; Add the output to the data collection
    ;DataColl.Add, Task.Output_Raster
  ENDFOR
  ; Close the ENVI session
  e.Close
  print,'全部影像辐射定标完毕！'
END