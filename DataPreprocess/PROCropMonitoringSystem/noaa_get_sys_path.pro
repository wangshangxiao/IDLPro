function noaa_get_sys_path

    IMAGE_DIRECTORY = ''
    INDEX_DIRECTORY = ''
    INDEXPARA_DIRECTORY = ''
    SOILMOISTURE_DIRECTORY  = ''
    ;print, 'aa'

    if file_test('data\DROUGHTPath.INI') eq 1 then begin
       openu,lun,'data\DROUGHTPath.INI',/get_lun
        Result = FSTAT(lun)
        ;print,result.size
        a = bytarr(result.size)
        readu,lun,a
        free_lun,lun
        ;print, result
        index0 = strpos(a ,'IMAGE_DIRECTORY:')
        index1 = strpos(a ,'INDEX_DIRECTORY:')
        index2 = strpos(a ,'INDEXPARA_DIRECTORY:')
        index3 = strpos(a ,'SOILMOISTURE_DIRECTORY:')
       ;print,index0,index1,index2,index3
        IMAGE_DIRECTORY = strtrim(strmid(a,index0+16,index1-16-index0-2),2)
        INDEX_DIRECTORY = strtrim(strmid(a,index1+16,index2-16-index1-2),2)
        INDEXPARA_DIRECTORY = strtrim(strmid(a,index2+20,index3-20-index2-2),2)
        SOILMOISTURE_DIRECTORY = strtrim(strmid(a,index3+23,result.size-23-index3-2),2)
    endif

    if file_test('data\DROUGHTPath.INI') eq 0 then begin
    print, 'cuowu'
    endif

    path = {IMAGE_DIRECTORY:        IMAGE_DIRECTORY ,       $
            INDEX_DIRECTORY:        INDEX_DIRECTORY   ,     $
            INDEXPARA_DIRECTORY:    INDEXPARA_DIRECTORY  ,  $
            SOILMOISTURE_DIRECTORY: SOILMOISTURE_DIRECTORY  $
            }
     ;  print,path
    return,path
end