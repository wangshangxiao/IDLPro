 PRO d_dataSaveImageFile,SAVE_FILE, ImageData $           ;ע��ImageData��ָ������.
 				  ,samples,lines,DataType,sensortype $
 				  ,ULX,ULY,Resolution

        enter=string(byte(13))+string(byte(10))     ;�س���ASCII��ֵ
        time=systime()                              ;����ʱ��
        col =STRTRIM(samples,2)                     ;����Ӱ������
        line=STRTRIM(lines,2)                       ;����Ӱ������
        DataType_   = STRTRIM(DataType,2)              ;��������
        sensortype_ = STRTRIM(sensortype,2)          ;����������
        tulx  = STRTRIM(ULX,2)                         ;���Ͻ�X����(ALBERS110)
        tuly  = STRTRIM(ULY,2)                         ;���Ͻ�Y����(ALBERS110)
		Resolution_   = STRTRIM(Resolution,2)			;���ش�С.Ҳ���ֱ���.
;		CenterMedian_ = STRTRIM(CenterMedian,2)		;��110,����105
;		IF CenterMedian_ EQ '110' THEN BEGIN
;		   EastingFalse = '4000000'					;��ƫ���ٹ���.
;		ENDIF ELSE EastingFalse = '0.0'

        SaveFileName = SAVE_FILE
        HeadFileName = SAVE_FILE+'.hdr'

        IF STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH) NE -1 THEN BEGIN
        	SaveFileName = STRMID(SAVE_FILE,0,STRPOS(SAVE_FILE,'.',/REVERSE_SEARCH))
        	HeadFileName = SAVE_FILE
        ENDIF

        Bandname = STRMID(SaveFileName,STRPOS(SaveFileName,'\',/REVERSE_SEARCH)+1)  ;������·�����ļ���.

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

         openw,lun,HeadFileName,/get_lun     ;ע��:���ļ��Ѵ�ʱ,���д���̲������κ���ʾ,�������ļ��滻��.��ͬ.,
         writeu,lun,HeadInfomation
         free_lun,lun

         openw,lun,SaveFileName,/get_lun
         writeu,lun,*ImageData				;	ImageData��ָ������
         free_lun,lun

END