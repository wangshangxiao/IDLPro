PRO Calculte1
    
    file1='D:\hunan_sub\droughtlevel\syn20130703_rsi_mg.tif';级别
    file2='D:\hunan_sub\recode\croptype.tif';类型
    background=0;非作物类型代码
    data1=READ_TIFF(file1,GEOTIFF=geotiff);级别
    data2=READ_TIFF(file2,GEOTIFF=geotiff);类型
    uniqType=data2[uniq(data2,SORT(data2))];类型
    print,uniqType
    uniqLevel=data1[uniq(data1,SORT(data1))];级别
    print,uniqLevel
    index=WHERE(uniqType NE background,count);类型像元的索引
    IF count NE 0 THEN uniqType=uniqType[index];类型
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
        ;PRINT,'类型编码:',uniqType[iType]
        FOR iLevel=0,N_Level-1 DO BEGIN
            PRINT,'类型编码:',uniqType[iType],'损毁级别:',uniqLevel[iLevel],'像元个数：',StatisArray[iType,iLevel]
        ENDFOR
    ENDFOR
END
