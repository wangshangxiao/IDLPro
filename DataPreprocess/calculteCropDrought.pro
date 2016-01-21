PRO CALCULTECROPDROUGHT
    
    file1='D:\hunan_sub\droughtlevel\syn20130703_rsi_mg.tif';级别
    file2='D:\hunan_sub\recode\croptype.tif';类型
    
    COMPILE_OPT idl2

    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    ENVI_OPEN_FILE,file2,R_FID=fid
    IF fid EQ 0 THEN BEGIN
        OPENW,lun,errorFile,/GET_LUN,/APPEND
        PRINTF,lun,'File cannot be opened. File name:',file2
        FREE_LUN,lun
    ENDIF
    mapinfo=ENVI_GET_MAP_INFO(FID=fid)
    PixelSize=mapinfo.ps[0:1]
    PixelArea=double(PixelSize[0]*PixelSize[1])
    ;print,PixelArea
    
    background=0;非作物类型代码
    data1=READ_TIFF(file1,GEOTIFF=geotiff);级别
    data2=READ_TIFF(file2,GEOTIFF=geotiff);类型
    uniqType=data2[uniq(data2,SORT(data2))];类型
    ;print,uniqType
    uniqLevel=data1[uniq(data1,SORT(data1))];级别
    index=WHERE(uniqType NE background,count);类型像元的索引
    IF count NE 0 THEN uniqType=uniqType[index];类型
    
    
    Levelindex=WHERE(uniqLevel NE background,Levelcount);类型像元的索引
    IF Levelcount NE 0 THEN uniqLevel=uniqLevel[Levelindex];类型
    
    
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
    ;PRINT,StatisArray
    
    AreaArray=PixelArea*StatisArray
    N_AreaArray=N_ELEMENTS(AreaArray)
    ;print,N_AreaArray
    DataArray=[0,0,0,0]
    FOR iType=0,N_Type-1 DO BEGIN
        ;PRINT,'类型编码:',uniqType[iType]
        
        FOR iLevel=0,N_Level-1 DO BEGIN
            ;PRINT,'类型编码:',uniqType[iType],'干旱级别:',uniqLevel[iLevel],'像元个数：',StatisArray[iType,iLevel],'面积：',AreaArray[iType,iLevel]
            DataLine=[uniqType[iType],uniqLevel[iLevel],StatisArray[iType,iLevel],AreaArray[iType,iLevel]]
            DataArray=[[DataArray],[DataLine]]
        ENDFOR
        
    ENDFOR
    ;print,DataArray
    ;DataArray=DataArray(where(DataArray[0,*] ne 0.00000000))
    print,DataArray
END
