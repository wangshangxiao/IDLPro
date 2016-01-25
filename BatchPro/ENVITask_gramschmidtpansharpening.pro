PRO ENVITask_GramSchmidtPanSharpening
  ; Start the application
  e = ENVI(/HEADLESS)
  
  inputPath='D:\HXData\TM'

  ; find the LOW_RESOLUTION Files
  LOW_RESOLUTION_Files = FILE_SEARCH(inputPath,'*_quac.dat',/FOLD_CASE,count=count)
  print,'All ',count,'    scenes will sharpening'
  FOR i=0,count-1 DO BEGIN
    LOW_RESOLUTION_raster = e.OpenRaster(LOW_RESOLUTION_Files[i])
    
    LOW_RESOLUTION_Path= file_dirname(LOW_RESOLUTION_Files[i])
    
    ; find the HIGH_RESOLUTION Files
    HIGH_RESOLUTION_File = FILE_SEARCH(LOW_RESOLUTION_Path,'*_B8.tif',/FOLD_CASE,count=count)
    
    HIGH_RESOLUTION_raster = e.OpenRaster(HIGH_RESOLUTION_File)
  
    ; Process a spatial subset
    ;Subset = ENVISubsetRaster(msi_raster, Sub_Rect=[600,200,799,399])
  
    ; Get the task from the catalog of ENVITasks
    Task = ENVITask('GramSchmidtPanSharpening')
  
    ; Define inputs
    Task.INPUT_LOW_RESOLUTION_RASTER = LOW_RESOLUTION_raster
    Task.INPUT_HIGH_RESOLUTION_RASTER = HIGH_RESOLUTION_raster
    Task.RESAMPLING ='Cubic Convolution'
    Task.SENSOR ='landsat8_oli'
    
    outputfile=LOW_RESOLUTION_Path+'\'+FILE_BASENAME(LOW_RESOLUTION_Files[i],'.dat',/FOLD_CASE)+'_sharpen.dat'
    ; Define outputs
    Task.OUTPUT_RASTER_URI = outputfile
    
    output = File_Search(outputfile)

    fileCount1 = SIZE(output)
    if fileCount1[0] gt 0 then begin
      print,outputfile+' is already exist'
      continue
    endif
    
    print,'The',i+1,'   scene      ',outputfile
    ; Run the task
    Task.Execute
  ENDFOR  
  print,'All files Sharpening is OK!'
END