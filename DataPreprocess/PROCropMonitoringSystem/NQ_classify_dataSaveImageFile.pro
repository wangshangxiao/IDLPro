 PRO d_dataSaveImageFile,SAVE_FILE, ImageData $           ;注意ImageData是指针类型.
 				  ,samples,lines,DataType,sensortype $
 				  ,ULX,ULY,Resolution

        enter=string(byte(13))+string(byte(10))     ;回车符ASCII码值
        time=systime()                              ;生成时间
        col =STRTRIM(samples,2)                     ;生成影像列数
        line=STRTRIM(lines,2)                       ;生成影像行数
        DataType_   = STRTRIM(DataType,2)              ;数据类型
        sensortype_ = STRTRIM(sensortype,2)          ;传感器类型
        tulx  = STRTRIM(ULX,2)                         ;左上角X坐标(ALBERS110)
        tuly  = STRTRIM(ULY,2)                         ;左上角Y坐标(ALBERS110)
		Resolution_   = STRTRIM(Resolution,2)			;像素大小.也即分辨率.
;		CenterMedian_ = STRTRIM(CenterMedian,2)		;是110,还是105
;		IF CenterMedian_ EQ '110' THEN BEGIN
;		   EastingFalse = '4000000'					;东偏多少公里.
;		ENDIF ELSE EastingFalse = '0.0'

        SaveFileName = SAVE_FILE
        HeadFileName = SAVE_FILE+'.hdr'

        IF STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH) NE -1 THEN BEGIN
        	SaveFileName = STRMID(SAVE_FILE,0,STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH))
        	HeadFileName = SAVE_FILE
        ENDIF

        Bandname = STRMID(SaveFileName,STRPOS(SaveFileName,'\',/REVERSE_SEARCH)+1)  ;不包含路径的文件名.

        HeadInfomation='ENVI'+enter+$
        'description = {' + enter+ $
        '  Create New File Time ['+time+']}'+enter+$
        'samples = '+col+enter+$
        'lines   = '+line+enter+$
        'bands   = 1'+enter+$
        'header offset = 0'+enter+$
        'file type = ENVI Standard'+enter+$
        'data type = '+DataType_+enter+$
        'interleave = bsq'+enter+$
        'sensor type = '+sensortype_+enter+$
        'byte order = 0'+enter+$
        'map info = {Albers Conical Equal Area, 1.0000, 1.0000, ' +tulx+', ' +tuly+', '+Resolution_+', '+Resolution_+', Krasovsky, units=Meters}'+enter+$
        'projection info = {9, 6378245.0, 6356863.0, 0.000000, 110.000000, 4000000, 0.0, 25.000000, 47.000000, Krasovsky, Albers Conical Equal Area, units=Meters}'+enter+$
        'wavelength units = Unknown'+enter+$
        'band names = {'+Bandname+'}'+enter

         openw,lun,HeadFileName,/get_lun     ;注意:当文件已存时,这个写过程不会作任何提示,将已有文件替换掉.下同.,
         writeu,lun,HeadInfomation
         free_lun,lun

         openw,lun,SaveFileName,/get_lun
         writeu,lun,*ImageData				;	ImageData是指针类型
         free_lun,lun

END