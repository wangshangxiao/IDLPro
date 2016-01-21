Function getSubFoldersName,InputPath,ExtentionName

  Allfiles=FILE_SEARCH(InputPath,ExtentionName,/FOLD_CASE,count=count)
  FolerNameArrays = !NULL
  ;收集所有上期目录，放到一个数组中，用数据做循环进行下面的拼接执行
  FOR k=0,count-1 DO BEGIN
    ;取出路径
    FullInPutPath= file_dirname(Allfiles[k])
    ;print,'FullInPutPath===:   '+FullInPutPath

    ;取出文件所在目录的上一级目录
    SubDirs = strsplit(FullInPutPath,'\', /EXTRACT,  COUNT= FileCOUNT)
    ;print,'SubDirs===:   '+SubDirs

    ;提取上一级目录名称
    Datedir = SubDirs[ FileCOUNT-2]
    ;print,'Datedir===:   '+Datedir

    FolerNameArrays = [FolerNameArrays, datedir]
    ;print,DatedirArray

  ENDFOR
  ;去掉数组中重复的名字
  FolerNameArrays=FolerNameArrays[uniq(FolerNameArrays)]
  ;print,'FolerNameArraysUniq:======  '+FolerNameArrays

  Return,FolerNameArrays

END


PRO FileMosaic
  inputPath='D:\77211356\Data\GF\HuBei\huanggang'
  
  DatedirArray=getSubFoldersName(InputPath,'*_rpc.tif')
  ;print,'DatedirArray:======  '+DatedirArray
  
  nCount = n_elements(DatedirArray)
  print,nCount+1
  
  FOR Num=0,nCount-1 DO BEGIN
    
    subInputPath=!NULL
    ;组成新的目录，到日期
    subInputPath=inputPath+PATH_SEP()+DatedirArray[Num]

    ;搜索日期目录下所有指定文件
    FileSeeds=FILE_SEARCH(subInputPath,'*_rpc.tif',/FOLD_CASE,count=count)
    
    OutFile=subInputPath+PATH_SEP()+DatedirArray[Num]+'_Mosaic.tif'
    DoFileMosaic,FileSeeds,OutFile,/TIFF,Message=Message,BACKGROUND=0
    print,'第 '+(nCount+1)+' 个 拼接完毕！'
  ENDFOR  
    print,'所有拼接完毕！'
    
END


PRO DoFileMosaic,FileSeeds,OutFile,TIFF=TIFF,Message=Message,BACKGROUND=BACKGROUND
    ;FileSeeds 要拼接的所有文件，为string数组
    ;OutFile输出文件
    ;TIFF是否以TIFF输出
    ;BACKGROUND为背景像元的值，拼接处重合区域BACKGROUND值将会被忽略
    COMPILE_OPT IDL2
    outputDIR=FILE_DIRNAME(OutFile,/MARK_DIRECTORY)
    outName=FILE_BASENAME(OutFile,'.tif')
    IF KEYWORD_SET(TIFF) THEN BEGIN
        tempoutFile=outputDIR+outName+'temp'
        TIFFFile=outputDIR+outName+'.tif'
    ENDIF ELSE BEGIN
        tempoutFile=outputDIR+outName
    ENDELSE
    outputDIR=FILE_DIRNAME(outputDIR)+PATH_SEP()+FILE_BASENAME(outputDIR)+PATH_SEP()
    CATCH,error
    IF error NE 0 THEN GOTO,next
    FILE_MKDIR,outputDIR
    next:
    CATCH,/CANCEL
    IF N_ELEMENTS(background) NE 1 THEN BACKGROUND=0
    Nums=N_ELEMENTS(fileseeds)
    IF Nums LT 1 ||  fileseeds[0] EQ '' THEN BEGIN
        Message='没有要拼接的文件!'
        RETURN
    ENDIF
    Ifids=LONARR(Nums)
    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,LOG_FILE="C:\envi_Preprocessing.Log"
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
        dims=dimsarr, out_name=tempoutFile,BACKGROUND=BACKGROUND,GEOREF=1,USE_SEE_THROUGH=INTARR(nums)+1, $
        SEE_THROUGH_VAL=seeTv,PIXEL_SIZE=out_ps, XSIZE=xsize,OUT_DT=out_dt,$
        X0=x0, YSIZE=ysize, Y0=y0,map_info=map_info
    ENVI_OPEN_FILE,tempoutFile,R_FID=tempFid
    IF tempFid EQ -1 THEN BEGIN
        ENVI_BATCH_EXIT
        Message=STRARR(FILE_LINES("C:\envi_Preprocessing.Log"))
        OPENR,lun,"C:\envi_Preprocessing.Log",/GET_LUN
        READF,lun,Message
        FREE_LUN,lun
        Message=STRJOIN(Message,STRING(10b))
        RETURN
    ENDIF
    Message=''
    IF KEYWORD_SET(TIFF) THEN BEGIN
        ENVI_FILE_QUERY,TempFid,nb=nb,DATA_TYPE=out_dt,dims=dims
        ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
            , FID=TempFid, OUT_NAME=TIFFFile,POS=INDGEN(nb), /TIFF
        ENVI_FILE_MNG, id=TempFid,/DELETE,/REMOVE
    ENDIF
    ENVI_BATCH_EXIT
END

PRO georef_mosaic_setup, fids=fids, dims=dims, out_ps=out_ps, $
        xsize=xsize, ysize=ysize, x0=x0, y0=y0, map_info=map_info
    COMPILE_OPT IDL2
    IF KEYWORD_SET(dims) THEN $
        IF N_ELEMENTS(fids) NE N_ELEMENTS(dims[0,*]) THEN dims=0
    IF N_ELEMENTS(fids) LT 2 THEN BEGIN
        xsize = -1
        ysize = -1
        x0 = -1
        y0 = -1
        RETURN
    ENDIF
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
    x0 = LONARR(nfiles)
    y0 = LONARR(nfiles)
    FOR i=0,nfiles-1 DO BEGIN
        ENVI_CONVERT_FILE_COORDINATES, tmp_fid, xpix, ypix, UL_corners_X[i], UL_corners_Y[i]
        x0[i] = xpix
        y0[i] = ypix
    ENDFOR
    ENVI_FILE_MNG, id=tmp_fid, /remove, /no_warning
END