PRO Calculte1
    
    file1='D:\hunan_sub\droughtlevel\syn20130703_rsi_mg.tif';����
    file2='D:\hunan_sub\recode\croptype.tif';����
    background=0;���������ʹ���
    data1=READ_TIFF(file1,GEOTIFF=geotiff);����
    data2=READ_TIFF(file2,GEOTIFF=geotiff);����
    uniqType=data2[uniq(data2,SORT(data2))];����
    print,uniqType
    uniqLevel=data1[uniq(data1,SORT(data1))];����
    print,uniqLevel
    index=WHERE(uniqType NE background,count);������Ԫ������
    IF count NE 0 THEN uniqType=uniqType[index];����
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
        ;PRINT,'���ͱ���:',uniqType[iType]
        FOR iLevel=0,N_Level-1 DO BEGIN
            PRINT,'���ͱ���:',uniqType[iType],'��ټ���:',uniqLevel[iLevel],'��Ԫ������',StatisArray[iType,iLevel]
        ENDFOR
    ENDFOR
END
