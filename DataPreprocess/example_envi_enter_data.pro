PRO EXAMPLE_ENVI_ENTER_DATA
  compile_opt IDL2
  ;
  ; First restore all the base save files.
  ;
  envi, /restore_base_save_files
  ;
  ; Initialize ENVI and send all errors
  ; and warnings to the file batch.txt
  ;
  envi_batch_init, log_file='batch.txt'
  ;
  ; Create a 2D ramp and then classify all values
  ; from 20 to 100 in the first class (classification
  ; data value equal to one) and classify all values
  ; from 101 to 220 into the second class
  ; (classification data value equal to two)
  ;
  data = BINDGEN(256,256)
  class = BYTE((data ge 20 and data le 100) + $
    2B * (data ge 101 and data le 220))
  ;
  ; Create the classification information
  ;
  class_names = ['Unclassified','Red','Yellow']
  lookup = [[0,0,0],[255,0,0],[255,255,0]]
  bnames = ['Ramp Classification']
  descrip = 'Example Classification Image'
  file_type = ENVI_FILE_TYPE('ENVI Classification')
  ;
  ; Enter the data into ENVI
  ENVI_ENTER_DATA, class, num_classes=3, $
    class_names=class_names, lookup=lookup, $
    file_type=file_type, bnames=bnames, $
    descrip=descrip
  ;
  ; Exit ENVI
  ;
  envi_batch_exit
END