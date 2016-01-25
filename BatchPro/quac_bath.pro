PRO QUAC_Bath
  ; Start the application
  e = ENVI(/HEADLESS)
  
  inputPath='F:\HXFarm\TM\LC81180272014164LGN00'
  
  ; Open an input file
  Files = FILE_SEARCH(inputPath,'*_rc.dat',/FOLD_CASE,count=count)
  
  print,'共',count,'    景影像进行快速大气纠正'

  FOR i=0,count-1 DO BEGIN
    Raster = e.OpenRaster(Files[i])
    outPath= file_dirname(Files[i])
    ; Get the QUAC task from the catalog of ENVITasks
    Task = ENVITask('QUAC')
  
    ; Define inputs
    Task.INPUT_RASTER = Raster
    Task.SENSOR = 'Landsat TM'
    
    outputfile=outPath+'\'+FILE_BASENAME(Files[i],'.dat',/FOLD_CASE)+'_quac.dat'
    print,'第',i+1,'景      ',outputfile
    Task.OUTPUT_RASTER_URI = outputfile
    ; Run the task
    Task.Execute
  ENDFOR
  ;  ; Get the data collection
  ;  DataColl = e.Data
  ;
  ;  ; Add the output to the data collection
  ;  DataColl.Add, Task.OUTPUT_RASTER
  ;
  ;  ; Display the result
  ;  View1 = e.GetView()
  ;  Layer1 = View1.CreateLayer(Task.Output_Raster)
  print,'QUAC is OK!'
END