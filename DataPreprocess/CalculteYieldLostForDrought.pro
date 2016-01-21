PRO CalculteYieldLostForDrought,file1,file2 $
    ,uniqType=uniqType,uniqLevel=uniqLevel,StatisArray=StatisArray
    
    ;file1='D:\hunan_sub\mutiplytestdata\syn20130703_rsi_test.tif';����
    ;file2='D:\hunan_sub\mutiplytestdata\iso20130725.tif';����
    background=0;���������ʹ���
    data1=READ_TIFF(file1,GEOTIFF=geotiff);����
    data2=READ_TIFF(file2,GEOTIFF=geotiff);����
    uniqType=data2[uniq(data2,SORT(data2))];����
    print,uniqType
    uniqLevel=data1[uniq(data1,SORT(data1))];����
    index=WHERE(uniqType NE background,count);������Ԫ������
    IF count NE 0 THEN uniqType=uniqType[index];����
    
    
    Levelindex=WHERE(uniqLevel NE background,Levelcount);������Ԫ������
    IF Levelcount NE 0 THEN uniqLevel=uniqLevel[Levelindex];����
    
    
    N_Type=N_ELEMENTS(uniqType)
    N_Level=N_ELEMENTS(uniqLevel)
    StatisArray=DBLARR(N_Type,N_Level)
    FOR iType=0,N_Type-1 DO BEGIN
        typecode=uniqType[iType]
        index=WHERE(data2 EQ typecode,count)
        IF count NE 0 THEN BEGIN
            tempDestroy=data1[index]
            FOR iLevel=0,N_Level-1 DO BEGIN
                index=WHERE(tempDestroy EQ uniqLevel[iLevel],count)
                StatisArray[iType,iLevel]=count
            ENDFOR
        ENDIF
    ENDFOR
    FOR iType=0,N_Type-1 DO BEGIN
        PRINT,'���ͱ���:',uniqType[iType]
        FOR iLevel=0,N_Level-1 DO BEGIN
            PRINT,'�ɺ�����:',uniqLevel[iLevel],'��Ԫ������',StatisArray[iType,iLevel]
        ENDFOR
    ENDFOR
END
