PRO MOD04Pre,HDFFile,OutDIR
    COMPILE_OPT idl2
    ;    HDFFile='E:\test\6s\HJTest\MOD04\MOD04_L2.A2010158.0345.051.2010347142224.hdf'
    ;    OutDIR='e:\'
    HDFMainName=FILE_BASENAME(HDFFile,'.hdf',/FOLD_CASE)
    outFile=OutDIR+HDFMainName+'.tif'
    ;    ENVI,/RESTORE_BASE_SAVE_FILES
    ;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    GEOPROJ=ENVI_PROJ_CREATE(/GEOGRAPHIC,DATUM='WGS-84')
    name='Albers Conical Equal Area'
    PARAMS=[6378137.0, 6356752.314245,0,105,4000000,0,25,47]
    AlbProj=ENVI_PROJ_CREATE(name=name,DATUM='WGS-84',TYPE=9,PARAMS=PARAMS)
    CONVERT_MODIS_DATA ,IN_FILE=HDFFile,OUT_PATH=OutDIR,OUT_ROOT='MOD04' $
        ,/SWATH,SD_NAMES='Image_Optical_Depth_Land_And_Ocean',OUT_METHOD=1  $
        ,OUT_PROJ=AlbProj,OUT_PS_X=10000,OUT_PS_Y=10000,swt_name='mod04' $
        ,INTERP_METHOD=6,/BOWTIE,/NO_MSG,BACKGROUND=0,FILL_REPLACE_VALUE=0 $
        ,R_FID_ARRAY=fid,R_FNAME_ARRAY=rname,num_x_pts=50, num_y_pts=50
    ENVI_FILE_QUERY,fid,dims=dims,nb=nb
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=fid, OUT_NAME=outFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG,id=fid,/DELETE,/REMOVE
;    ENVI_BATCH_EXIT
END

PRO MOD04Pre_Batch
    inputDIR='D:\Modis\MOD04_L2\hdf\2013193\'
    outDIR='D:\Modis\MOD04_L2\tif\2013193\'
    FILE_MKDIR,outdir
    HDFFiles=FILE_SEARCH(inputDIR+'MOD04_L2*.hdf',/FOLD_CASE,COUNT=count)
    IF count EQ 0 THEN RETURN
    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    restore,'C:\Program Files\Exelis\ENVI50\classic\save_add\modis_conversion_toolkit.sav'
    FOR i=0,count-1 DO BEGIN
        ;        CATCH,error
        ;        IF error NE 0 THEN CONTINUE
        HDFFile=HDFFiles[i]
        HDFMainName=FILE_BASENAME(HDFFile,'.hdf',/FOLD_CASE)
        outFile=OutDIR+HDFMainName+'.tif'
        IF FILE_SEARCH(outFile) NE '' THEN CONTINUE
        MOD04Pre,HDFFile,OutDIR
    ENDFOR
    ENVI_BATCH_EXIT
END

PRO MosaicMOD04_Batch
    inputDIR='D:\Modis\MOD04_L2\tif\2013193\'
    outputDIR='D:\Modis\MOD04_L2\tif\2013193\'
    mod04Files=FILE_SEARCH(inputDIR+'MOD04_L2.A*.tif',/FOLD_CASE,count=Filecount)
    FilesName=FILE_BASENAME(mod04files)
    DateArray=STRMID(FilesName,10,7)
    DateArray=DateArray[SORT(DateArray)]
    DateArray=DateArray[uniq(DateArray)]
    DateNum=N_ELEMENTS(DateArray)
    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    FOR i=0,dateNum-1 DO BEGIN
        Fileseeds=FILE_SEARCH(inputDIR+'MOD04_L2.A'+DateArray[i]+'*.tif',/FOLD_CASE,count=count)
        IF count LT 1 THEN CONTINUE
        outFile=outputDIR+'MOD04_L2.A'+DateArray[i]+'.tif'
        MOD04Mosaic,fileseeds,outfile
    ENDFOR
    ENVI_BATCH_EXIT
END
PRO MOD04Mosaic,fileseeds,outfile
    COMPILE_OPT IDL2
    ;    fileseeds=file_search('E:\danjkou\MODIS\8day_reflectance\result\Layerstack\A2010025*.tif')
    ;    outputDIR='E:\danjkou\MODIS\8day_reflectance\result\Layerstack\Mosaic\'
    outputDIR=FILE_DIRNAME(outfile)
    FILE_MKDIR,outputDIR
    Nums=N_ELEMENTS(fileseeds)
    outName=FILE_BASENAME(outfile)
    ;A2010025.h26v05.005.sur_refl_b01.tif
    ;    FileMainName=file_basename(fileseeds[0],'.tif',/FOLD_CASE)
    ;    Fields=strsplit(FileMainName,'.',/EXTRACT)
    tempoutname=outputDIR+outName+'temp'
    ;    outfile=outputDIR+Fields[0]+'.tif'
    IF FILE_SEARCH(outfile,/FOLD_CASE) NE '' THEN RETURN
    IF Nums EQ 1 THEN BEGIN
        FILE_COPY,fileseeds[0],outfile,/ALLOW_SAME,/OVERWRITE
        RETURN
    ENDIF
    Ifids=LONARR(Nums)
    FOR i=0,nums-1 DO BEGIN
        ENVI_OPEN_FILE, fileseeds[i], r_fid=tfid
        Ifids[i]=tfid
    ENDFOR
    ENVI_FILE_QUERY,Ifids[0],nb=nb,DATA_TYPE=out_dt,dims=dims
    map_info = ENVI_GET_MAP_INFO(fid=tfid)
    out_ps=map_info.ps[0:1]
    georef_mosaic_setup, fids=Ifids, dims=dimsarr, out_ps=out_ps, $
        xsize=xsize, ysize=ysize, x0=x0, y0=y0, map_info=map_info
    seeTv = MAKE_ARRAY(Nums,value =0)
    pos=INDGEN(nb)
    posarr=LONARR(N_ELEMENTS(pos),Nums)
    FOR i=0,Nums-1 DO posarr[*,i]=pos
    ENVI_DOIT, 'MOSAIC_DOIT', fid=Ifids, pos=posarr, $
        dims=dimsarr, out_name=tempoutname,BACKGROUND=0,GEOREF=1,USE_SEE_THROUGH=INTARR(nums)+1, $
        SEE_THROUGH_VAL=seeTv,PIXEL_SIZE=out_ps, XSIZE=xsize,OUT_DT=out_dt,$
        X0=x0, YSIZE=ysize, Y0=y0,map_info=map_info
    ENVI_OPEN_FILE,tempoutname,r_fid=tempfid
    ENVI_FILE_QUERY,tempfid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=tempfid, OUT_NAME=outfile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG, id=tempfid,/DELETE,/REMOVE
END

PRO georef_mosaic_setup, fids=fids, dims=dims, out_ps=out_ps, $
        xsize=xsize, ysize=ysize, x0=x0, y0=y0, map_info=map_info
    COMPILE_OPT IDL2
    
    IF KEYWORD_SET(dims) THEN $
        IF N_ELEMENTS(fids) NE N_ELEMENTS(dims[0,*]) THEN dims=0
    ;
    IF N_ELEMENTS(fids) LT 2 THEN BEGIN
        xsize = -1
        ysize = -1
        x0 = -1
        y0 = -1
        RETURN
    ENDIF
    ; if no DIMS passed in
    nfiles = N_ELEMENTS(fids)
    IF (KEYWORD_SET(dims) EQ 0) THEN BEGIN
        dims = FLTARR(5, nfiles)
        FOR i=0, nfiles-1 DO BEGIN
            ENVI_FILE_QUERY, fids[i], ns=ns, nl=nl
            dims[*,i] = [-1L, 0, ns-1, 0, nl-1]
        ENDFOR
    ENDIF
    UL_corners_X = DBLARR(nfiles)
    UL_corners_Y = DBLARR(nfiles)
    east = -1e34
    west = 1e34
    north = -1e34
    south = 1e34
    FOR i=0,nfiles-1 DO BEGIN
        pts = [ [dims[1,i], dims[3,i]],   $   ; UL
            [dims[2,i], dims[3,i]],   $   ; UR
            [dims[1,i], dims[4,i]],   $   ; LL
            [dims[2,i], dims[4,i]] ]   ; LR
        ENVI_CONVERT_FILE_COORDINATES, fids[i], pts[0,*], pts[1,*], xmap, ymap, /to_map
        UL_corners_X[i] = xmap[0]
        UL_corners_Y[i] = ymap[0]
        east  = east > MAX(xmap)
        west = west < MIN(xmap)
        north = north > MAX(ymap)
        south = south < MIN(ymap)
    ENDFOR
    xsize = east - west
    ysize = north - south
    xsize_pix = FIX( xsize/out_ps[0] )+1
    ysize_pix = FIX( ysize/out_ps[1])+1
    proj = ENVI_GET_PROJECTION(fid=fids[0])
    map_info = ENVI_MAP_INFO_CREATE(proj=proj, mc=[0,0,west,north], ps=out_ps)
    temp = BYTARR(10,10)
    ENVI_ENTER_DATA, temp, map_info=map_info, /no_realize, r_fid=tmp_fid
    ; find the x and y offsets for the images
    x0 = LONARR(nfiles)
    y0 = LONARR(nfiles)
    FOR i=0,nfiles-1 DO BEGIN
        ENVI_CONVERT_FILE_COORDINATES, tmp_fid, xpix, ypix, UL_corners_X[i], UL_corners_Y[i]
        x0[i] = xpix
        y0[i] = ypix
    ENDFOR
    ; delete the tmp file
    ENVI_FILE_MNG, id=tmp_fid, /remove, /no_warning
END
PRO checkfile
    dir='E:\Tao\Mosaic\cut\'
    FOR i=0,365 DO BEGIN
        daystr=STRTRIM(STRING(i),2)
        IF i LT 10 THEN daystr='00'+daystr ELSE BEGIN
            IF i LT 100 THEN daystr='0'+daystr
        ENDELSE
        daystr='A2010'+daystr
        files=FILE_SEARCH(dir+'mod04_l2.'+daystr+'.tif',/FOLD_CASE,count=count)
        IF count LT 1 THEN BEGIN
            PRINT,i
            CONTINUE
        ENDIF
        data=READ_TIFF(files[0])
        index=WHERE(data GT 0,dcount)
        IF dcount EQ 0 THEN PRINT,files[0]
    ENDFOR
    PRINT,'done!'
    jul=julday(12,31,2009)+334
    caldat,jul,month,day,year
END

;PRO allSDName
;    HDFFile='E:\test\6s\HJTest\MOD04\MOD04_L2.A2010158.0345.051.2010347142224.hdf'
;    HdfID=HDF_SD_START(HDFFile)
;    HDF_SD_FILEINFO, HdfID, nDatasets, nAttributes
;    FOR i=0,nDatasets-1 DO BEGIN
;        SDID=HDF_SD_SELECT(HdfID, i)
;        HDF_SD_GETINFO, SDID , COORDSYS=COORDSYS $
;         , DIMS=DIMS, FILL=FILL, FORMAT=variable, HDF_TYPE=HDF_TYPE $
;         , LABEL=LABEL, NAME=NAME, NATTS=NATTS, NDIMS=NDIMS $
;         , /NOREVERSE, RANGE=RANGE, TYPE=TYPE, UNIT=UNIT
;         print,name
;    ENDFOR
;
;END
;PRO MOD04Pre,HDFFile,OutDIR
;    ;无法去除Bowtie效应
;    COMPILE_OPT idl2
;    ENVI,/RESTORE_BASE_SAVE_FILES
;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
;    HDFFile='E:\test\6s\HJTest\MOD04\MOD04_L2.A2010158.0345.051.2010347142224.hdf'
;    outDIR='E:\'
;    HDFFileMainName=FILE_BASENAME(HDFFile,'.hdf')
;    outTempFile=outDIR+HDFFileMainName
;    outFile=outDIR+HDFFileMainName+'.tif'
;    HdfID=HDF_SD_START(HDFFile)
;    LonIndex=HDF_SD_NAMETOINDEX(HdfID, 'Longitude')
;    LatIndex=HDF_SD_NAMETOINDEX(HdfID, 'Latitude')
;    AOTIndex=HDF_SD_NAMETOINDEX(HdfID, 'Optical_Depth_Land_And_Ocean')
;    LonSDID = HDF_SD_SELECT(HdfID, LonIndex)
;    LatSDID = HDF_SD_SELECT(HdfID, LatIndex)
;    AOTSDID = HDF_SD_SELECT(HdfID, AOTIndex)
;    HDF_SD_GETDATA, LonSDID, Longitude
;    HDF_SD_GETDATA, LatSDID, Latitude
;    HDF_SD_GETDATA, AOTSDID, AOT
;    HDF_SD_END, HdfID
;    AOT>=0
;    Samples=(SIZE(AOT,/DIMENSIONS))[0]
;    Lines=(SIZE(AOT,/DIMENSIONS))[1]
;    nPixels=Samples*Lines
;    Longitude=REFORM(Longitude,nPixels)
;    Latitude=REFORM(Latitude,nPixels)
;    AOT=REFORM(AOT,nPixels)
;    LonMax=MAX(Longitude,MIN=LonMin)
;    LatMax=MAX(Latitude,MIN=LatMin)
;    LonInterval=(LonMax-LonMin)/Samples
;    LatInterval=(LatMax-LatMin)/Lines
;    ;计算分辨率
;    CentLon=(LonMax+LonMin)/2
;    CentLat=(LatMax+LatMin)/2
;    UTMzone=ceil(CentLon/6.0)+30
;    UTMProj=MAP_PROJ_INIT('UTM',ELLIPSOID='WGS 84',/GCTP,ZONE=UTMzone)
;    !map=UTMProj
;    xy0=MAP_PROJ_FORWARD(CentLon, CentLat)
;    xy1=MAP_PROJ_FORWARD(CentLon+LonInterval, CentLat+LatInterval)
;    print,xy1-xy0
;    Xpts=(Longitude-LonMin)/LonInterval
;    Ypts=(Latitude-LatMin)/LatInterval
;    Ypts=lines-Ypts
;    GEOPROJ=ENVI_PROJ_CREATE(/GEOGRAPHIC,DATUM='WGS-84')
;    ENVI_DOIT, 'ENVI_GRID_DOIT',INTERP=0,O_PROJ=GEOPROJ,$
;        OUT_DT=size(AOT,/TYPE), OUT_NAME=outTempFile, PIXEL_SIZE=[1,1], R_FID=rfid, $
;        XMIN=0,YMIN=0,XMAX=Samples,Ymax=lines,$
;        X_PTS=Xpts, Y_PTS=Ypts, Z_PTS=AOT
;    ENVI_FILE_QUERY,rfid, ns=ns, nl=nl, nb=nb, $
;        data_type=data_type,interleave=interleave
;    MapInfo=ENVI_MAP_INFO_CREATE(/GEOGRAPHIC,DATUM='WGS-84',mc=[0,0,LonMin,LatMax],ps=[LonInterval,LatInterval])
;    ENVI_SETUP_HEAD, fname=outTempFile, ns=ns, nl=nl, nb=nb, $
;        data_type=data_type, offset=0, interleave=interleave, $
;        MAP_INFO=MapInfo, /write, /open
;    ENVI_BATCH_EXIT
;END