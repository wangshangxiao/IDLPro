PRO HJPreProSysWinV3_1,InputDIR,OutPutDIR,WinrarDIR,PRODIR,ATCFlag,DeleteFlag;C#大傻瓜界面版
    COMPILE_OPT idl2
    COMMON GlobalVar,ConfigFile
        ;InputDIR='D:\work\Hongxing\basedata\HJTest2013\'
        ;OutPutDIR='D:\work\Hongxing\basedata\HJTestResultData\'
        ;WinrarDIR='C:\Program Files (x86)\WinRAR\'
        ;PRODIR='D:\share\HJ_reference\'
        ;ATCFlag=0
        ;DeleteFlag=1
    InputDIR=MarkDIR(InputDIR)
    OutPutDIR=MarkDIR(OutPutDIR)
    WinrarDIR=MarkDIR(WinrarDIR)
    PRODIRFile=FILE_DIRNAME(ROUTINE_FILEPATH('HJPreProSysWinV3_1'),/MARK_DIRECTORY)
    CFGFile=PRODIRFile+'ConfigFile.txt'
    TempDIR=OutPutDIR+'Temp'+PATH_SEP()
    OPENW,lun,CFGFile,/GET_LUN
    PRINTF,lun,'HJRARDIR='+InputDIR
    PRINTF,lun,'OrthoOutDIR='+OutPutDIR+'ortho_out'
    PRINTF,lun,'TempDIR='+OutPutDIR+'temp'
    PRINTF,lun,'DEMDIR='+PRODIR+'DEM'+PATH_SEP()
    PRINTF,lun,'TMDIR='+PRODIR+'TM2010'+PATH_SEP()
    PRINTF,lun,'TMShape='+PRODIR+'TMshape'+PATH_SEP()+'tm-orbit-window105all.shp'
    PRINTF,lun,'HJUnrarDIR='+TempDIR+'Unrar'+PATH_SEP()
    PRINTF,lun,'XmlDIR='+OutPutDIR+'Xml'+PATH_SEP()
    PRINTF,lun,'winrarDIR='+WinrarDIR
    PRINTF,lun,'orthoExe='+PRODIR+'HJ'+PATH_SEP()
    PRINTF,lun,'ReferBand=4'
    PRINTF,lun,'ReflectDIR='+OutPutDIR+'Reflectance'+PATH_SEP()
    PRINTF,lun,'TMMosaicDIR='+OutPutDIR+'TMRefer'
    PRINTF,lun,'ParaDIR='+PRODIR+'HJCalPara'
    PRINTF,lun,'TaoDIR='+PRODIR+'Tao'
    PRINTF,lun,'LookupTableDIR='+PRODIR+'LookupTable'
    PRINTF,lun,'AtmCorDIR='+OutPutDIR+'AtmCor'
    PRINTF,lun,'ATCFlag='+STRTRIM(STRING(ATCFlag),2)
    FREE_LUN,lun
    ConfigFile=CFGFile
    HJRARDIR=ReadConfigVar(CFGFile,'HJRARDIR',/NECESSARY)
    HJRARDIR=MarkDIR(HJRARDIR)
    TempDIR=ReadConfigVar(CFGFile,'TempDIR',/NECESSARY)
    TempDIR=MarkDIR(TempDIR)
    HJRarFiles=FILE_SEARCH(HJRARDIR+'*.tar.gz',count=count,/FOLD_CASE)
    HJUnrarDIR=TempDIR+'HJUnrarDIR'+PATH_SEP()
    ReflectDIR=ReadConfigVar(CFGFile,'ReflectDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    ReflectDIR=MarkDIR(ReflectDIR);;;;;;;;;;;;;;;;;;
    FILE_MKDIR,ReflectDIR
    batch_errfile=ReflectDIR+'Batch_error.txt'
    TimeFile=ReflectDIR+'TimeRecord.txt'
    ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    ENVI_REPORT_INIT, '处理中……', BASE=base, /INTERRUPT, TITLE='处理进度'
    ENVI_REPORT_INC, Base, 100
    FOR i=0,count-1 DO BEGIN
        ENVI_REPORT_STAT, Base, 100, FLOAT(i)/count*100, CANCEL=cancelvar
        IF cancelvar EQ 1 THEN BEGIN
            result=DIALOG_MESSAGE("是否终止程序？",/QUESTION)
            IF result EQ 'Yes' THEN BEGIN            
                result=DIALOG_MESSAGE("该程序将在当前影像计算完毕后终止！",/INFORMATION)
                break
            ENDIF ELSE BEGIN
            
            ENDELSE
        ENDIF              
        RarFileName=FILE_BASENAME(HJRarFiles[i],'.tar.gz',/FOLD_CASE)
        starttime=SYSTIME(1)
        CATCH,error
        IF error NE 0 THEN BEGIN
            CATCH,/CANCEL
            CATCH,error2
            IF error2 NE 0 THEN BEGIN
                PRINT,'Write error file error!'
                PRINT,'Error message: ',!ERROR_STATE.MSG
                FILE_DELETE,HJUnrarDIR+RarFileName+PATH_SEP(),/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
                CATCH,/CANCEL
                IF N_ELEMENTS(errorlun) NE 0 THEN BEGIN
                    IF errorlun GT 100 THEN BEGIN
                        FREE_LUN,errorlun
                    ENDIF
                ENDIF
                CONTINUE
            ENDIF
            OPENW,errorlun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,errorlun,'Error message: ',!ERROR_STATE.MSG
            PRINTF,errorlun,'System time:',SYSTIME()
            PRINTF,errorlun,'Error in processing HJ file:'+RarFileName
            PRINTF,errorlun,'************************************************************'
            FREE_LUN,errorlun
            FILE_DELETE,HJUnrarDIR+RarFileName+PATH_SEP(),/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
            CATCH,/CANCEL
            CONTINUE
        ENDIF
        ProcessRAR,HJRarFiles[i],batch_errfile=batch_errfile
        endtime=SYSTIME(1)
        pasttime=endtime-starttime
        OPENW,lun,TimeFile,/GET_LUN,/APPEND
        PRINTF,lun,RarFileName+' consumes time: '+STRING(pasttime)+' (s)!'
        FREE_LUN,lun
        CATCH,/CANCEL
    ENDFOR
    ENVI_REPORT_INIT, '处理中……', BASE=base, /FINISH
    ENVI_BATCH_EXIT
    IF KEYWORD_SET(DeleteFlag) THEN BEGIN
        FILE_DELETE,TempDIR,/RECURSIVE,/ALLOW_NONEXISTENT,/QUIET
    ENDIF
END

PRO ProcessRAR,HJRarFile,batch_errfile=batch_errfile
    COMPILE_OPT idl2
    OrthoRAR,HJRarFile,Band1OutFile=Band1OutFile,flag=flag,batch_errfile=batch_errfile,RARFinishDIR=RARFinishDIR
    IF flag EQ 0 THEN RETURN
    ProcessOrthoFile,Band1OutFile,batch_errfile=batch_errfile,Flag=Flag
    IF flag EQ 0 THEN RETURN
    RarFileName=FILE_BASENAME(HJRarFile,'.tar.gz',/FOLD_CASE)
    FILE_MOVE,HJRarFile,RARFinishDIR+RarFileName+'.tar.gz',/ALLOW_SAME,/OVERWRITE
END

PRO OrthoRAR,HJRarFile,Band1OutFile=Band1OutFile,flag=flag,batch_errfile=batch_errfile,RARFinishDIR=RARFinishDIR
    COMPILE_OPT idl2
    COMMON GlobalVar,ConfigFile
    flag=0
    CFGFile=(FILE_SEARCH(ConfigFile,/fold_case))[0]
    IF CFGFile EQ '' THEN BEGIN
        PRINT,'Config file can not be found! Config file: ',CFGFile
        RETURN
    ENDIF
    OrthoOutDIR=ReadConfigVar(CFGFile,'OrthoOutDIR',/NECESSARY)
    OrthoOutDIR=MarkDIR(OrthoOutDIR)
    TempDIR=ReadConfigVar(CFGFile,'TempDIR',/NECESSARY)
    TempDIR=MarkDIR(TempDIR)
    DEMDIR=ReadConfigVar(CFGFile,'DEMDIR',/NECESSARY)
    DEMDIR=MarkDIR(DEMDIR)
    TMMosaicDIR=ReadConfigVar(CFGFile,'TMMosaicDIR',/NECESSARY)
    TMMosaicDIR=MarkDIR(TMMosaicDIR)
    TMDIR=ReadConfigVar(CFGFile,'TMDIR',/NECESSARY)
    TMShape=ReadConfigVar(CFGFile,'TMShape',/NECESSARY)
    TMDIR=MarkDIR(TMDIR)
    orthoExeDIR=ReadConfigVar(CFGFile,'orthoExe',/NECESSARY)
    orthoExeDIR=MarkDIR(orthoExeDIR)
    HJUnrarDIR=TempDIR+'HJUnrar'+PATH_SEP()
    XmlDIR=ReadConfigVar(CFGFile,'XmlDIR',/NECESSARY)
    XmlDIR=MarkDIR(XmlDIR)
    RarFileName=FILE_BASENAME(HJRarFile,'.tar.gz',/FOLD_CASE)
    HJRARDIR=FILE_DIRNAME(HJRarFile,/MARK_DIRECTORY)
    RARFinishDIR=HJRARDIR+'Done'+PATH_SEP()
    SubDIR=HJUnrarDIR+RarFileName+PATH_SEP()
    FILE_MKDIR,RARFinishDIR,SubDIR
    HJBand1File=FILE_SEARCH(HJUnrarDIR+RarFileName+PATH_SEP()+'*-1.TIF',/FOLD_CASE,count=Band1Count)
    IF Band1Count NE 1 THEN BEGIN
        winrar,HJRarFile,HJUnrarDIR
    ENDIF
    HJBand1File=FILE_SEARCH(HJUnrarDIR+RarFileName+PATH_SEP()+'*-1.TIF',/FOLD_CASE,count=Band1Count)
    IF Band1Count NE 1 THEN RETURN;C:\Program Files\ITT\IDL\IDL80\products\envi48\menu\envi.cfg
    auto_ortho,HJBand1File[0],DEMDIR,TMDIR,TMShape,OrthoOutDIR,tempDIR,TMMosaicDIR,orthoExeDIR,batch_errfile=batch_errfile,flag=aflag
    IF aflag EQ 0 THEN RETURN
    gatherXMLSatFiles,HJUnrarDIR+RarFileName+PATH_SEP(),XmlDIR
    FILE_DELETE,HJUnrarDIR+RarFileName+PATH_SEP(),/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
    ;将算过的RAR文件移到计算后的文件夹中
    Band1MainName=FILE_BASENAME(HJBand1File[0],'.tif',/FOLD_CASE)
    IF FILE_SEARCH(OrthoOutDIR+Band1MainName) NE '' THEN BEGIN
        ;        FILE_MOVE,HJRarFile,RARFinishDIR+RarFileName+'.tar.gz',/ALLOW_SAME,/OVERWRITE
        Band1OutFile=OrthoOutDIR+Band1MainName
        Flag=1
    ENDIF
END

PRO ProcessOrthoFile,fileseed,batch_errfile=batch_errfile,flag=aFlag
    COMPILE_OPT idl2
    COMMON GlobalVar,ConfigFile
    aFlag=0
    ;        ENVI,/RESTORE_BASE_SAVE_FILES
    ;        ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    ;        ConfigFile='D:\djk\ConfigFile.txt'
    ;        fileseed='D:\djk\ortho_out\HJ1A-CCD1-450-53-20120423-L20000757429-1'
    ENVI_OPEN_FILE,fileseed,R_FID=fid
    IF fid EQ -1 THEN RETURN
    CFGFile=(FILE_SEARCH(ConfigFile,/fold_case))[0]
    IF CFGFile EQ '' THEN BEGIN
        PRINT,'Config file can not be found! Config file: ',CFGFile
        RETURN
    ENDIF
    ParaDIR=ReadConfigVar(CFGFile,'ParaDIR',/NECESSARY);;;;;;;;;;;;;;;
    ParaDIR=MarkDIR(ParaDIR);;;;;;;;;;;;;;;
    ReflectDIR=ReadConfigVar(CFGFile,'ReflectDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    ReflectDIR=MarkDIR(ReflectDIR);;;;;;;;;;;;;;;;;;
    TempDIR=ReadConfigVar(CFGFile,'TempDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    TempDIR=MarkDIR(TempDIR);;;;;;;;;;;;;;;;;;
    xmlDIR=ReadConfigVar(CFGFile,'XmlDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    xmlDIR=MarkDIR(xmlDIR);;;;;;;;;;;;;;;;;;
    IF N_ELEMENTS(batch_errfile) EQ 0 THEN BEGIN
        batch_errfile=ReflectDIR+'Batch_error.txt'
    ENDIF
    ;    TimeFile=ReflectDIR+'TimeRecord.txt'
    ;    LSDIR=TempDIR+'LayerStack'+PATH_SEP()
    FILE_MKDIR,ReflectDIR
    ;        xmlDIR=LSDIR+'xml'+PATH_SEP()
    ;    RaDIR=TempDIR+'Radiance'+PATH_SEP()
    ;    ZeroBackgroundDIR=TempDIR+'ZeroBackground'+PATH_SEP()
    ;    SunFileDIR=TempDIR+'Sun'+PATH_SEP()
    UTMReflecDIR=ReflectDIR
    FILE_MKDIR,xmlDIR
    ;    RangeFile=inputDIR+'RangeFile.rf'
    RangeFile=ReadConfigVar(CFGFile,'RangeFile');;;;;;;;;;;;;;;
    IF RangeFile NE '' THEN BEGIN
        RangeFile=FILE_SEARCH(RangeFile,/FOLD_CASE)
    ENDIF
    IF RangeFile NE '' THEN BEGIN
        getRange,RangeFile,FirstCoordinate=FirstCoordinate,FinalCoordinate=FinalCoordinate,FirstRangFile=FirstRangeFile,FinalRangeFile=FinalRangeFile
    ENDIF
    FileMainName=GetFileMainName(fileseed,extendName=extendName)
    FileMainName=STRMID(FileMainName,0,STRLEN(FileMainName)-2)
    ;    RadianceFile=RaDIR+FileMainName+'.tif'
    ;    zeroBackgroundfile=ZeroBackgroundDIR+FileMainName+'.tif'
    ;    SunOutputFile=SunFileDIR+FileMainName+'sun'
    UTMTempFile=UTMReflecDIR+FileMainName+'temp'
    UTMOutFile=UTMReflecDIR+FileMainName+'.tif'
    IF QUERY_TIFF(UTMOutFile) THEN BEGIN
        ReflectanceFile=UTMOutFile
        GOTO,AtmCor
    ;        aFlag=1
    ;        RETURN
    ENDIF
    Do_HJPre2,fileseed,ReflectDIR,xmlDIR,ParaDIR,TempDIR=TempDIR,overwrite=overwrite,RefFile=ReflectanceFile,flag=flag
    ;    Do_HJPre2,fileseed,ReflectDIR,xmlDIR,ParaDIR,Coordinate=FirstCoordinate,RangeFile=FirstRangeFile,RefFile=RefFile,flag=flag,batch_errfile=batch_errfile
    IF flag EQ 0 THEN RETURN
    ;    FILE_DELETE,RadianceFile,zeroBackgroundfile,SunOutputFile,UTMTempFile,/ALLOW_NONEXISTENT,/QUIET
    ;    IF N_ELEMENTS(FinalRangefile) || N_ELEMENTS(FinalCoordinate) THEN BEGIN
    ;        IF N_ELEMENTS(FinalCoordinate) EQ 0 THEN BEGIN
    ;            FinalCoordinate=rangefile2UTMcoord(FinalRangefile)
    ;        ENDIF
    ;        RangeCut,RefFile,ReflectDIR+'FinalCut'+PATH_SEP(),Coordinate=FinalCoordinate,/tiff
    ;    ENDIF
    AtmCor: TaoDIR=ReadConfigVar(CFGFile,'TaoDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    TaoDIR=MarkDIR(TaoDIR);;;;;;;;;;;;;;;;;;
    DEMDIR=ReadConfigVar(CFGFile,'DEMDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    DEMDIR=MarkDIR(DEMDIR);;;;;;;;;;;;;;;;;;
    LookupTableDIR=ReadConfigVar(CFGFile,'LookupTableDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    LookupTableDIR=MarkDIR(LookupTableDIR);;;;;;;;;;;;;;;;;;
    AtmCorDIR=ReadConfigVar(CFGFile,'AtmCorDIR',/NECESSARY);;;;;;;;;;;;;;;;;;;
    AtmCorDIR=MarkDIR(AtmCorDIR);;;;;;;;;;;;;;;;;;
    DEMMosaicDIR=TempDIR+'DEM'+PATH_SEP()
    AngleDIR=TempDIR+'Angle'+PATH_SEP()
    ATCFlag=ReadConfigVar(CFGFile,'ATCFlag',/NECESSARY);;;;;;;;;;;;;;;;;;;
    ATCFlag=FIX(ATCFlag);;;;;;;;;;;;;;;;;;
    IF ATCFlag EQ 0 THEN RETURN
    ;    if AtmCorDIR
    DO_6S_AtmCor,ReflectanceFile,XMLDIR,TaoDIR,DEMDIR,AtmCorDIR,angleDIR,DEMMosaicDIR,LookupTableDIR,flag=flag,/minZero
    IF flag EQ 0 THEN RETURN
    aFlag=1
;    ENVI_BATCH_EXIT
END
FUNCTION MarkDIR,str
    ;    return,file_dirname(str,/MARK_DIRECTORY)+file_basename(str)
    IF STRMID(str,STRLEN(str)-1,1) NE PATH_SEP() THEN value=str+PATH_SEP() ELSE value=str
    RETURN,value
END
FUNCTION ReadConfigVar,ConFile,varName,Num_element=Num_element,delimSym=delimSym,necessary=necessary;,VarValue=VarValue
    COMPILE_OPT idl2
    OPENR,lun,ConFile,/GET_LUN
    nlines=FILE_LINES(ConFile)
    StrArray=STRARR(nlines)
    READF,lun,StrArray
    FREE_LUN,lun
    IF N_ELEMENTS(Num_element) EQ 0 THEN Num_element=1
    IF N_ELEMENTS(delimSym) EQ 0 THEN delimSym=','
    FOR i=0,nlines[0]-1 DO BEGIN
        ;delete comment part
        Tempstr=StrArray[i]
        DeleteComment,Tempstr,'#'
        DeleteComment,Tempstr,';'
        DeleteComment,Tempstr,'%'
        pos=STRPOS(STRUPCASE(Tempstr),STRUPCASE(varName))
        endPos=pos+STRLEN(varName)
        IF pos NE -1 THEN BEGIN
            equal_pos=STRPOS(Tempstr,'=',endPos)
            IF equal_pos EQ -1 THEN CONTINUE
            VarValue=STRTRIM(STRMID(Tempstr,equal_pos+1),2)
            IF Num_element GT 1 THEN BEGIN
                fields=strsplit(VarValue,delimSym,/EXTRACT,count=count)
                IF count GE Num_element THEN RETURN,fields[0:Num_element-1]
            ENDIF ELSE BEGIN
                RETURN,VarValue
            ENDELSE
        ENDIF
    ENDFOR
    IF KEYWORD_SET(necessary) THEN BEGIN
        PRINT,'Warning! Variable: ',varName,' cannot read from config file!'
    ENDIF
    RETURN,''
END

PRO DeleteComment,str,commentSym
    CommentPos=STRPOS(str,STRING(commentSym))
    IF CommentPos NE -1 THEN BEGIN
        str=STRMID(str,0,CommentPos)
    ENDIF
END

PRO gatherXMLSatFiles,HJFold,outputDIR
    FILE_MKDIR,outputDIR
    XMLFiles=FILE_SEARCH(HJFold,'HJ1*.xml',/FOLD_CASE,count=count)
    FOR i=0,count-1 DO BEGIN
        file=FILE_BASENAME(XMLFiles[i])
        FILE_COPY,XMLFiles[i],outputDIR+file,/ALLOW_SAME,/OVERWRITE
    ENDFOR
    SatFiles=FILE_SEARCH(HJFold,'HJ1*-SatAngle.txt',/FOLD_CASE,count=count)
    FOR i=0,count-1 DO BEGIN
        file=FILE_BASENAME(SatFiles[i])
        FILE_COPY,SatFiles[i],outputDIR+file,/ALLOW_SAME,/OVERWRITE
    ENDFOR
END


PRO auto_ortho,HJBand1File,DEMDIR,TMDIR,TMShape,outputDIR,tempDIR,TMMosaicDIR,orthoExeDIR,batch_errfile=batch_errfile,flag=aflag
    COMPILE_OPT idl2
    COMMON GlobalVar
    aflag=0
    ;    HJBand1File='D:\djk\391661\HJ1A-CCD2-6-76-20100913-L20000391661-1.TIF'
    ;    outputDIR='d:\djk\'
    ;    TempDIR='d:\djk\temp\'
    ;    DEMDIR='D:\常用资料\ChinaDEM\'
    ;    TMDIR='\\crop50\碳专项\TM影像备份\2010\'
    ;    orthoExeDIR='C:\HJ\'
    ;    TMShape='D:\shape\tm-orbit-window105all.shp'
    HJMainName=FILE_BASENAME(HJBand1File,'-1.tif',/FOLD_CASE)
    Band1OutFile=outputDIR+HJMainName+'-1.hdr'
    Band2OutFile=outputDIR+HJMainName+'-2.hdr'
    Band3OutFile=outputDIR+HJMainName+'-3.hdr'
    Band4OutFile=outputDIR+HJMainName+'-4.hdr'
    ;判断输出文件是否有效
    ENVI_OPEN_FILE,Band1OutFile,R_FID=fid1
    ENVI_OPEN_FILE,Band2OutFile,R_FID=fid2
    ENVI_OPEN_FILE,Band3OutFile,R_FID=fid3
    ENVI_OPEN_FILE,Band4OutFile,R_FID=fid4
    IF fid1 NE -1 && fid2 NE -1 && fid3 NE -1 && fid4 NE -1 THEN BEGIN
        aflag=1
        RETURN
    ENDIF
    DEMMosaicDIR=TempDIR+'DEM'+PATH_SEP()
    ;    TMMosaicDIR=TempDIR+'TMRefer'+PATH_SEP()
    FILE_MKDIR,TempDIR,demmosaicdir,TMMosaicDIR
    GetDEMFile,HJBand1File,DEMDIR,DEMMosaicDIR,batch_errfile=batch_errfile,DEMFile=DEMFile,flag=flag
    IF flag EQ 0 THEN RETURN
    GetTMFile,HJBand1File,TMShape,TMDIR,TMMosaicDIR,batch_errfile=batch_errfile,TMRefFile=TMRefFile,flag=flag
    IF flag EQ 0 THEN RETURN
    ortho_File,HJBand1File,TMRefFile,DEMFile,outputDIR,orthoExeDIR
    ;删除TM参考影像和DEM文件
    ;    FILE_DELETE,DEMFile,/ALLOW_NONEXISTENT,/QUIET
    aflag=1
END

PRO ortho_File,warpfile,basefile,DEMFile,outputDIR,orthoExeDIR
    ;    warpfile='E:\DanjiangkouDataBackup\HJ1B-CCD2-8-76-20100811-L20000373197\373197\HJ1B-CCD2-8-76-20100811-L20000373197-1.TIF'
    ;    basefile='E:\djkbaseband4.tif'
    ;    DEMFile='E:\djk90dem.tif'
    ;    outputDIR='E:\Temp\'
    ;    orthoExeDIR='C:\HJ\'
    orthoExe=orthoExeDIR+'ortho1.exe'
    write_Para_File,warpfile,basefile,DEMFile,outputDIR,orthoExeDIR,resultFlag=resultFlag,paraFile=paraFile
    IF resultFlag THEN BEGIN
        CD,orthoExeDIR
        SPAWN,orthoExe+' -b '+paraFile,/HIDE
    ENDIF ELSE BEGIN
        PRINT,'write_Para_File failed!'
        RETURN
    ENDELSE
END

PRO write_Para_File,warpfile,basefile,DEMFile,outputDIR,orthoDIR,resultFlag=resultFlag,paraFile=paraFile
    ;    warpfile='E:\DanjiangkouDataBackup\二批\HJ1B-CCD2-8-76-20100811-L20000373197\373197\HJ1B-CCD2-8-76-20100811-L20000373197-1.TIF'
    ;    basefile='E:\djkbaseband4.tif'
    ;    DEMFile='E:\djk90dem.tif'
    ;    outputDIR='E:\Temp\'
    ;    orthoDIR='C:\HJortho\'
    COMPILE_OPT idl2
    COMMON GlobalVar
    FILE_MKDIR,outputDIR
    band=ReadConfigVar(ConfigFile,'ReferBand')
    CASE (band) OF
        '1': BEGIN
            ReferBandNum=0
        END
        '2': BEGIN
            ReferBandNum=1
        END
        '3': BEGIN
            ReferBandNum=2
        END
        '4': BEGIN
            ReferBandNum=3
        END
        ELSE: BEGIN
            ReferBandNum=3
        END
    ENDCASE
    resultFlag=0
    WarpFileName=FILE_BASENAME(warpfile,'.tif',/FOLD_CASE)
    Fields=strsplit(WarpFileName,'-',/EXTRACT,count=count)
    CCDType=Fields[1]
    SatAng=((CCDType EQ 'CCD1')?1:((CCDType EQ 'CCD2')?-1:0))*15
    IF SatAng EQ 0 THEN BEGIN
        PRINT,'CCDType must be CCD1 or CCD2!'
        RETURN
    ENDIF
    warpDIR=FILE_DIRNAME(warpfile,/MARK_DIRECTORY)
    WarpFileMainName=STRJOIN(Fields[0:count-2],'-')
    BandFile=STRARR(4)
    FOR i=0,3 DO BEGIN
        BandFile[i]=warpDIR+WarpFileMainName+'-'+STRTRIM(STRING(i+1),2)+'.TIF'
        Flag=QUERY_TIFF(BandFile[i],info,geotiff=warpgeotiff)
        IF ~Flag THEN BEGIN
            PRINT,'Warp file invalid! File name:',BandFile[i]
            RETURN
        ENDIF
    ENDFOR
    OutputMainName=outputDIR+WarpFileMainName
    paraFile=outputDIR+WarpFileMainName+'.inp'
    Flag=QUERY_TIFF(basefile,geotiff=basegeotiff)
    IF flag THEN BEGIN
        UTMNum=basegeotiff.projectedcstypegeokey-32600
        IF UTMNum GT 100 THEN UTMNum=(UTMNum-100)*(-1)
        IF UTMNum LT 1 || UTMNum GT 160 THEN BEGIN
            PRINT,'Base file projection is not UTM! Base file:',basefile
            RETURN
        ENDIF
    ENDIF ELSE BEGIN
        PRINT,'Base file invalid! File path:',basefile
        RETURN
    ENDELSE
    XMLFile=warpDIR+WarpFileMainName+'.XML'
    nlines=FILE_LINES(XMLFile)
    XMLStr=STRARR(nlines)
    OPENR,lun,XMLFile,/GET_LUN
    READF,lun,XMLStr
    FREE_LUN,lun
    FOR i=0,nlines-1 DO BEGIN
        pos=STRPOS(XMLStr[i],'dataUpperLeftX')
        IF pos NE -1 THEN BEGIN
            CoorStartLine=i
            BREAK
        ENDIF
    ENDFOR
    xyxyxyxy=FLTARR(8)
    FOR i=0,7 DO BEGIN
        pos=STRPOS(XMLStr[CoorStartLine+i],'>')
        startPos=pos+1
        pos=STRPOS(XMLStr[CoorStartLine+i],'</')
        endPos=pos-1
        xyxyxyxy[i]=FLOAT(STRMID(XMLStr[CoorStartLine+i],startPos,endPos-startPos+1))
    ENDFOR
    CenterX=mean(xyxyxyxy[[0,2,4,6]])
    CenterY=mean(xyxyxyxy[[1,3,5,7]])
    LeftupperX=warpgeotiff.modeltiepointtag[3]
    LeftupperY=warpgeotiff.modeltiepointtag[4]
    SizeX=warpgeotiff.modelpixelscaletag[0]
    SizeY=warpgeotiff.modelpixelscaletag[1]
    CenterCol=FIX((CenterX-LeftupperX)/SizeX)
    CenterRow=FIX((LeftupperY-CenterY)/SizeY)
    IF CenterCol LT 0 || CenterCol GT info.DIMENSIONS[0] $
        || CenterRow LT 0 || CenterRow GT info.DIMENSIONS[1] THEN BEGIN
        PRINT,'Center coordinate error! Please check XML file! File:',XMLFile
        RETURN
    ENDIF
    ;write para file
    OPENW,lun,paraFile,/GET_LUN
    PRINTF,lun,'PARAMETER_FILE'
    PRINTF,lun,''
    PRINTF,lun,'# LANDSAT base image '
    PRINTF,lun,'BASE_FILE_TYPE = GEOTIFF'
    PRINTF,lun,'BASE_NSAMPLE = -1'
    PRINTF,lun,'BASE_NLINE = -1'
    PRINTF,lun,'BASE_PIXEL_SIZE = -1'
    PRINTF,lun,'BASE_UPPER_LEFT_CORNER = -1, -1'
    PRINTF,lun,'BASE_LANDSAT = '+basefile
    PRINTF,lun,'UTM_ZONE = '+STRTRIM(STRING(UTMNum),2)
    PRINTF,lun,'BASE_SATELLITE = Landsat5'
    PRINTF,lun,''
    PRINTF,lun,'# Landsat warp images'
    PRINTF,lun,'WARP_FILE_TYPE = GEOTIFF'
    PRINTF,lun,'WARP_NSAMPLE = -1'
    PRINTF,lun,'WARP_NLINE = -1'
    PRINTF,lun,'WARP_PIXEL_SIZE = 30'
    PRINTF,lun,'WARP_UPPER_LEFT_CORNER = -1, -1'
    PRINTF,lun,'WARP_NBANDS = 4'
    PRINTF,lun,'# Landsat 1-5, Landsat 7, TERRA, CBERS2 #'
    PRINTF,lun,'WARP_SATELLITE = HJ'
    ;printf,lun,'WARP_SATELLITE_POINTINGANGLE = '+((CCDType eq 'CCD1')?1:((CCDType eq 'CCD2')?-1:0))*15
    PRINTF,lun,'WARP_SATELLITE_POINTINGANGLE = '+STRTRIM(STRING(SatAng),2)
    PRINTF,lun,'SCENECENTERCOL = '+STRTRIM(STRING(CenterCol),2)
    PRINTF,lun,'SCENECENTERROW = '+STRTRIM(STRING(CenterRow),2)
    PRINTF,lun,'# use negative for counterclockwise an positive for clockwise '
    PRINTF,lun,'WARP_ORIENTATION_ANGLE = 0'
    PRINTF,lun,'WARP_LANDSAT_BAND  ='+BandFile[0]+','+BandFile[1]+','+BandFile[2]+','+BandFile[3]
    PRINTF,lun,'WARP_BASE_MATCH_BAND =  '+BandFile[ReferBandNum]
    PRINTF,lun,''
    PRINTF,lun,'# Landsat orthorectied output images'
    PRINTF,lun,'OUT_PIXEL_SIZE = 30'
    PRINTF,lun,'# NN-nearest neighbor; BI-bilinear interpolation; CC-cubic convolution #'
    PRINTF,lun,'RESAMPLE_METHOD = NN'
    PRINTF,lun,'# BASE-use base map extent; WARP-use warp map extent; DEF-user defined extent #'
    PRINTF,lun,'OUT_EXTENT = WARP'
    PRINTF,lun,'OUT_LANDSAT_BAND = '+OutputMainName+'-1,'+OutputMainName+'-2,'+OutputMainName+'-3,'+OutputMainName+'-4'
    PRINTF,lun,'OUT_BASE_MATCH_BAND = '+OutputMainName+'-4'
    PRINTF,lun,'OUT_BASE_POLY_ORDER = 1'
    PRINTF,lun,''
    PRINTF,lun,'# ancillary input for orthorectification process'
    PRINTF,lun,'INPUT_DEM_FILE = '+DEMFile
    PRINTF,lun,'CP_PARAMETERS_FILE = lndortho.cps_par_mosaic.ini'
    PRINTF,lun,''
    PRINTF,lun,'END'
    FREE_LUN,lun
    resultFlag=1
END

PRO GetDEMFile,HJFile,DEMDIR,DEMMosaicDIR,batch_errfile=batch_errfile,DEMFile=DEMFile,flag=flag
    COMPILE_OPT idl2
    flag=0
    DEMLatInter=10
    DEMLonInter=20
    ;确定HJ范围
    Fields=strsplit(FILE_BASENAME(HJFile),'-',/EXTRACT)
    HJFileSign=STRJOIN(Fields[0:4],'-')
    SuboutDIR=DEMMosaicDIR+HJFileSign+PATH_SEP()
    ;清空DEM
    FILE_DELETE,SuboutDIR,/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
    FILE_MKDIR,SuboutDIR
    DEMFile=DEMMosaicDIR+'DEM'+HJFileSign+'.tif'
    IF QUERY_TIFF(DEMFile) THEN BEGIN
        flag=1
        RETURN
    ENDIF
    ENVI_OPEN_FILE,HJFile,r_fid=HJfid
    ENVI_FILE_QUERY,HJfid,ns=ns,nl=nl
    IF HJfid EQ -1 THEN BEGIN
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,lun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,lun,'File can not be read! File name:',HJFile
            PRINTF,lun,'************************************************************'
            FREE_LUN,lun
        ENDIF ELSE BEGIN
            PRINT,'File can not be read! File name:',HJFile
        ENDELSE
        RETURN
    ENDIF
    map_info=ENVI_GET_MAP_INFO(fid=HJfid)
    HJProj=ENVI_GET_PROJECTION(fid=HJfid)
    y0 = map_info.MC[3] & x0 = map_info.MC[2]
    pixel = map_info.PS[0:1]
    x1=x0+ns*pixel[0] & y1=y0-nl*pixel[1]
    GeoProj=ENVI_PROJ_CREATE(/GEOGRAPHIC)
    ENVI_CONVERT_PROJECTION_COORDINATES,x0,y0, HJProj,p0_x,p0_y, GeoProj
    ENVI_CONVERT_PROJECTION_COORDINATES,x1,y0, HJProj,p1_x,p1_y, GeoProj
    ENVI_CONVERT_PROJECTION_COORDINATES,x1,y1, HJProj,p2_x,p2_y, GeoProj
    ENVI_CONVERT_PROJECTION_COORDINATES,x0,y1, HJProj,p3_x,p3_y, GeoProj
    x_max=MAX([p0_x,p1_x,p2_x,p3_x],min=x_min)
    y_max=MAX([p0_y,p1_y,p2_y,p3_y],min=y_min)
    ;计算影像相关的DEM
    X_SGrid=FIX(x_min/DEMLonInter)*DEMLonInter
    X_EGrid=FIX(x_Max/DEMLonInter)*DEMLonInter
    Y_SGrid=FIX(y_min/DEMLatInter)*DEMLatInter
    Y_EGrid=FIX(y_Max/DEMLatInter)*DEMLatInter
    coordinate={x0:x_min,y0:y_Max,x1:x_max,y1:y_min}
    FOR X=X_SGrid,X_EGrid,DEMLonInter DO BEGIN
        FOR Y=Y_SGrid,Y_EGrid,DEMLatInter DO BEGIN
            Name_lon=x
            Name_lat=y+DEMLatInter ;DEM文件名称命名为左上角坐标
            IF Name_lon LT 0 THEN EWstr='w'+STRTRIM(STRING(ABS(Name_lon)),2) ELSE EWstr='e'+STRTRIM(STRING(ABS(Name_lon)),2)
            IF Name_lat LT 0 THEN NSstr='s'+STRTRIM(STRING(ABS(Name_lat)),2) ELSE NSstr='n'+STRTRIM(STRING(ABS(Name_lat)),2)
            NameSign=NSstr+EWstr
            ;寻找对应DEM
            SearchDEMFile=FILE_SEARCH(DEMDIR+NameSign+'.tif',count=count,/FOLD_CASE)
            IF count LT 1 THEN CONTINUE
            SearchDEMFile=SearchDEMFile[0]
            outFile=SuboutDIR+'Sub_DEM'+NameSign+'_'+HJFileSign
            ;范围裁剪
            cut_dem,SearchDEMFile,outFile,coordinate,batch_errfile=batch_errfile
        ENDFOR
    ENDFOR
    ;mosaic
    SubDEMFiles=FILE_SEARCH(SuboutDIR+'Sub_DEM*.hdr',count=count)
    IF count LT 1 THEN BEGIN
        PRINT,'Lack DEM file!  HJFile:',HJFile
    ENDIF
    DEMMosaicFile=DEMMosaicDIR+'DEMMosaic'+HJFileSign
    DEMUTMTempFile=DEMMosaicDIR+'DEMMosaic'+HJFileSign+'UTMTemp'
    DEMResizeTempFile=DEMMosaicDIR+'DEMMosaic'+HJFileSign+'ResizeTemp'
    DEMFile=DEMMosaicDIR+'DEM'+HJFileSign+'.tif'
    TM_Mosaic,SubDEMFiles,DEMMosaicFile
    ;转投影
    ENVI_OPEN_FILE,DEMMosaicFile,R_FID=DEMMosaicFid
    ENVI_FILE_QUERY,DEMMosaicFid,dims=dims,nb=nb
    ;    ENVI_CONVERT_FILE_MAP_PROJECTION, fid=DEMMosaicFid, r_fid=DEMMosaicUTMFid, $
    ;        pos=LINDGEN(nb),dims=dims,o_proj=HJProj,out_name=DEMUTMTempFile
    ENVI_CONVERT_FILE_MAP_PROJECTION, fid=DEMMosaicFid, r_fid=DEMMosaicUTMFid, $
        pos=LINDGEN(nb),dims=dims,o_proj=HJProj,WARP_METHOD=3,O_PIXEL_SIZE=[90,90],out_name=DEMUTMTempFile
    ;转分辨率
    ;    ENVI_FILE_QUERY,DEMMosaicUTMFid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ;    map_info = ENVI_GET_MAP_INFO(fid=DEMMosaicUTMFid)
    ;    oriPS=map_info.ps[0:1]
    ;    out_ps=[90,90]
    ;    rfact=[out_ps[0]/oriPS[0],out_ps[1]/oriPS[1]]
    ;    ENVI_DOIT, 'resize_doit', $
    ;        fid=DEMMosaicUTMFid, pos=INDGEN(nb), dims=dims, $
    ;        interp=0, rfact=rfact, $
    ;        out_name=DEMResizeTempFile, r_fid=resizefid
    resizefid=DEMMosaicUTMFid
    ENVI_FILE_QUERY,resizefid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=resizefid, OUT_NAME=DEMFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG, id=DEMMosaicUTMFid,/DELETE,/REMOVE
    ENVI_FILE_MNG, id=DEMMosaicFid,/DELETE,/REMOVE
    ENVI_FILE_MNG,id=resizefid,/DELETE,/REMOVE
    FILE_DELETE,SubDEMFiles,/ALLOW_NONEXISTENT,/QUIET
    FILE_DELETE,SuboutDIR,/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
    flag=1
END

PRO GetASTERFile,HJFile,ASTERDIR,DEMMosaicDIR,batch_errfile=batch_errfile,DEMFile=DEMFile,flag=flag
    COMPILE_OPT idl2
    flag=0
    HJFile=''
    ASTERDIR=''
    DEMMosaicDIR=''
    DEMLatInter=1
    DEMLonInter=1
    ;确定HJ范围
    Fields=strsplit(FILE_BASENAME(HJFile),'-',/EXTRACT)
    HJFileSign=STRJOIN(Fields[0:4],'-')
    SuboutDIR=DEMMosaicDIR+HJFileSign+PATH_SEP()
    ;清空DEM
    FILE_DELETE,SuboutDIR,/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
    FILE_MKDIR,SuboutDIR
    ENVI_OPEN_FILE,HJFile,r_fid=HJfid
    ENVI_FILE_QUERY,HJfid,ns=ns,nl=nl
    IF HJfid EQ -1 THEN BEGIN
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,lun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,lun,'File can not be read! File name:',HJFile
            PRINTF,lun,'************************************************************'
            FREE_LUN,lun
        ENDIF ELSE BEGIN
            PRINT,'File can not be read! File name:',HJFile
        ENDELSE
        RETURN
    ENDIF
    map_info=ENVI_GET_MAP_INFO(fid=HJfid)
    HJProj=ENVI_GET_PROJECTION(fid=HJfid)
    y0 = map_info.MC[3] & x0 = map_info.MC[2]
    pixel = map_info.PS[0:1]
    x1=x0+ns*pixel[0] & y1=y0-nl*pixel[1]
    GeoProj=ENVI_PROJ_CREATE(/GEOGRAPHIC)
    ENVI_CONVERT_PROJECTION_COORDINATES,x0,y0, HJProj,p0_x,p0_y, GeoProj
    ENVI_CONVERT_PROJECTION_COORDINATES,x1,y0, HJProj,p1_x,p1_y, GeoProj
    ENVI_CONVERT_PROJECTION_COORDINATES,x1,y1, HJProj,p2_x,p2_y, GeoProj
    ENVI_CONVERT_PROJECTION_COORDINATES,x0,y1, HJProj,p3_x,p3_y, GeoProj
    x_max=MAX([p0_x,p1_x,p2_x,p3_x],min=x_min)
    y_max=MAX([p0_y,p1_y,p2_y,p3_y],min=y_min)
    ;计算影像相关的DEM
    X_SGrid=FLOOR(x_min)
    X_EGrid=CEIL(x_Max)
    Y_SGrid=FLOOR(y_min)
    Y_EGrid=CEIL(y_Max)
    coordinate={x0:x_min,y0:y_Max,x1:x_max,y1:y_min}
    FOR X=X_SGrid,X_EGrid-1 DO BEGIN
        FOR Y=Y_SGrid,Y_EGrid-1 DO BEGIN
            ;Ater文件名为左下角坐标
            IF X LT 0 THEN EWstr='W'+STRTRIM(STRING(ABS(X)),2) ELSE EWstr='E'+STRTRIM(STRING(ABS(X)),2)
            IF Y LT 0 THEN NSstr='S'+STRTRIM(STRING(ABS(Y)),2) ELSE NSstr='N'+STRTRIM(STRING(ABS(Y)),2)
            NameSign='ASTGTM_'+NSstr+EWstr
            ;寻找对应DEM
            SearchZipFile=FILE_SEARCH(ASTERDIR+NameSign+'.zip',count=count,/FOLD_CASE)
            IF count LT 1 THEN CONTINUE
            SearchZipFile=SearchZipFile[0]
            SubSubDIR=SuboutDIR+'Sub_DEM'+NameSign+'\'
            FILE_MKDIR,SubSubDIR
            ;解压并转投影
            winrar,SearchZipFile,SubSubDIR
            DEMTiFF=FILE_SEARCH(SubSubDIR+NameSign+'*.tif',/FOLD_CASE,count=count)
            IF count LT 1 THEN CONTINUE
            FILE_MOVE,DEMTiFF[0],SuboutDIR+NameSign+'.tif',/ALLOW_SAME,OVERWRITE=1
            FILE_DELETE,SubSubDIR,/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
        ENDFOR
    ENDFOR
    ;mosaic
    SubDEMFiles=FILE_SEARCH(SuboutDIR+'Sub_DEM*',count=count)
    IF count LT 1 THEN BEGIN
        PRINT,'Lack DEM file!  HJFile:',HJFile
    ENDIF
    ;拼接
    
    resizefid=DEMMosaicUTMFid
    ENVI_FILE_QUERY,resizefid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=resizefid, OUT_NAME=DEMFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG, id=DEMMosaicUTMFid,/DELETE,/REMOVE
    FILE_DELETE,SubDEMFiles,/ALLOW_NONEXISTENT,/QUIET
    FILE_DELETE,SuboutDIR,/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
    flag=1
END

PRO cut_dem,demFile,outFile,coordinate,batch_errfile=batch_errfile
    COMPILE_OPT idl2
    flag=0
    ENVI_OPEN_FILE,demFile,R_FID=demfid
    ENVI_FILE_QUERY,demfid,nb=nb,dims=dims,ns=ns,nl=nl
    OriProj=ENVI_GET_PROJECTION(fid=demfid)
    mapinfo=ENVI_GET_MAP_INFO(fid=demfid)
    pixel = mapinfo.PS[0:1]
    ENVI_CONVERT_PROJECTION_COORDINATES,Coordinate.x0,Coordinate.y0, OriProj, x0, y0, OriProj
    ENVI_CONVERT_PROJECTION_COORDINATES,Coordinate.x1,Coordinate.y1, OriProj, x1, y1, OriProj
    ENVI_CONVERT_FILE_COORDINATES,demfid,x0F,y0F,x0,y0
    ENVI_CONVERT_FILE_COORDINATES,demfid,x1F,y1F,x1,y1
    ROIns=ABS(x1F-x0F)+1   & ROInl= ABS(y1F-y0F)+1
    roi_id = ENVI_CREATE_ROI(color=4, ns = ns,  nl = nl)
    x0F=ROUND(x0F>0) & x1F=ROUND(x1F<ns-1)
    y0F=ROUND(y0F>0) & y1F=ROUND(y1F<nl-1)
    ;    IF x0F GT ns-1 || y0F GT nl-1 THEN BEGIN
    IF x0F GE x1F || y0F GE y1F THEN BEGIN
        PRINT,'DEM/TM file is not in cut range, discard file. File name: ',demFile
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,errlun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,errlun,'DEM/TM file is not in cut range, discard file. File name: ',demFile
            PRINTF,errlun,'Time: ',SYSTIME()
            PRINTF,errlun,'***********************************************************'
            FREE_LUN,errlun
        ENDIF
        RETURN
    ENDIF
    ENVI_DEFINE_ROI, roi_id, /polygon, xpts=[x0F,x1F,x1F,x0F,x0F], ypts=[y0F,y0F,y1F,y1F,y0F]
    outputDIR=FILE_DIRNAME(outFile,/MARK_DIRECTORY)
    FileName=FILE_BASENAME(outFile)
    maskFile= outputDIR+FileName+'msk.mask'
    ENVI_MASK_DOIT,AND_OR =1,OUT_NAME =maskFile, $
        ROI_IDS= roi_id, ns = ns, nl = nl,/inside, r_fid = m_fid
    out_dims=[-1,x0F,x1F,Y0F,Y1F]
    ENVI_DOIT,'ENVI_MASK_APPLY_DOIT', FID = demfid, POS = INDGEN(nb), DIMS = out_dims, $
        M_FID = m_fid, M_POS = [0], VALUE = 0, $
        out_name = outFile,r_fid=cutfid;,$
    ENVI_FILE_QUERY,cutfid,ns=ns,nb=nb,nl=nl,data_type=data_type,$
        interleave=interleave
    cutmapinfo=ENVI_GET_MAP_INFO(fid=cutfid)
    ;        offset=0, interleave=0,MAP_INFO=map_info, /write, /open
    newMapinfo=ENVI_MAP_INFO_CREATE(/geographic,mc=cutmapinfo.mc, ps=cutmapinfo.ps)
    ENVI_SETUP_HEAD, fname=outFile, ns=ns, nl=nl, nb=nb, $
        data_type=data_type, offset=0, interleave=interleave, $
        MAP_INFO=newMapinfo, /write, /open
    ENVI_FILE_MNG, id =cutfid,/remove
    ENVI_FILE_MNG, id =m_fid,/remove,/DELETE
    flag=1
END

PRO cut_File,File,outFile,coordinate,batch_errfile=batch_errfile
    COMPILE_OPT idl2
    ;    IF n_elements(coordinate) NE 4 THEN return
    flag=0
    ENVI_OPEN_FILE,File,R_FID=fid
    ENVI_FILE_QUERY,fid,nb=nb,dims=dims,ns=ns,nl=nl
    OriProj=ENVI_GET_PROJECTION(fid=fid)
    mapinfo=ENVI_GET_MAP_INFO(fid=fid)
    pixel = mapinfo.PS[0:1]
    ENVI_CONVERT_PROJECTION_COORDINATES,Coordinate.x0,Coordinate.y0, OriProj, x0, y0, OriProj
    ENVI_CONVERT_PROJECTION_COORDINATES,Coordinate.x1,Coordinate.y1, OriProj, x1, y1, OriProj
    ENVI_CONVERT_FILE_COORDINATES,fid,x0F,y0F,x0,y0
    ENVI_CONVERT_FILE_COORDINATES,fid,x1F,y1F,x1,y1
    ROIns=ABS(x1F-x0F)+1   & ROInl= ABS(y1F-y0F)+1
    roi_id = ENVI_CREATE_ROI(color=4, ns = ns,  nl = nl)
    x0F=ROUND(x0F>0) & x1F=ROUND(x1F<ns-1)
    y0F=ROUND(y0F>0) & y1F=ROUND(y1F<nl-1)
    IF x0F GE x1F || y0F GE y1F THEN BEGIN
        ;    IF x0F GT ns-1 || y0F GT nl-1 THEN BEGIN
        PRINT,'DEM/TM file is not in cut range, discard file. File name: ',File
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,errlun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,errlun,'DEM/TM file is not in cut range, discard file. File name: ',File
            PRINTF,errlun,'Time: ',SYSTIME()
            PRINTF,errlun,'***********************************************************'
            FREE_LUN,errlun
        ENDIF
        RETURN
    ENDIF
    ENVI_DEFINE_ROI, roi_id, /polygon, xpts=[x0F,x1F,x1F,x0F,x0F], ypts=[y0F,y0F,y1F,y1F,y0F]
    outputDIR=FILE_DIRNAME(outFile,/MARK_DIRECTORY)
    FileName=FILE_BASENAME(outFile)
    maskFile= outputDIR+FileName+'msk.mask'
    ENVI_MASK_DOIT,AND_OR =1,OUT_NAME =maskFile, $
        ROI_IDS= roi_id, ns = ns, nl = nl,/inside, r_fid = m_fid
    out_dims=[-1,x0F,x1F,Y0F,Y1F]
    ENVI_DOIT,'ENVI_MASK_APPLY_DOIT', FID = fid, POS = INDGEN(nb), DIMS = out_dims, $
        M_FID = m_fid, M_POS = [0], VALUE = 0, $
        out_name = outFile,r_fid=cutfid;,$
    ENVI_FILE_QUERY,cutfid,ns=ns,nb=nb,nl=nl,data_type=data_type,$
        interleave=interleave
    ENVI_FILE_MNG, id =cutfid,/remove
    ENVI_FILE_MNG, id =m_fid,/remove,/DELETE
    flag=1
END

PRO GetTMFile,HJFile,TMShape,TMDIR,TMMosaicDIR,batch_errfile=batch_errfile,TMRefFile=TMRefFile,flag=flag
    flag=0
    COMPILE_OPT idl2
    COMMON GlobalVar
    ;检查SHAPE文件的有效性
    myshape=OBJ_NEW('IDLffShape',TMShape,/UPDATE)
    IF ~OBJ_VALID(myshape) THEN BEGIN
        PRINT,'TM Shape File invalid! TM shape file:',TMShape
        RETURN
    ENDIF
    HJFilename=FILE_BASENAME(HJFile)
    HJMainName=FILE_BASENAME(HJFile,'-1.tif',/FOLD_CASE)
    TMDIR=FILE_DIRNAME(TMDIR,/MARK_DIRECTORY)+FILE_BASENAME(TMDIR)+PATH_SEP()
    TMMosaicDIR=FILE_DIRNAME(TMMosaicDIR,/MARK_DIRECTORY)+FILE_BASENAME(TMMosaicDIR)+PATH_SEP()
    MainUnrarDIR=TMMosaicDIR+'Unrar'+PATH_SEP()
    TMUnRarDIR=TMMosaicDIR+'Unrar'+PATH_SEP()+HJMainName+PATH_SEP()
    ;清空TMReferUnrar文件夹
    FILE_DELETE,TMUnRarDIR,/ALLOW_NONEXISTENT,/QUIET,/RECURSIVE
    FILE_MKDIR,MainUnrarDIR,TMUnRarDIR
    fields=STRSPLIT(HJFilename,'-',/EXTRACT)
    DateStr=fields[4]
    IF STRLEN(DateStr) NE 8 THEN BEGIN
        XmlDIR=ReadConfigVar(CFGFile,'XmlDIR',/NECESSARY)
        XmlDIR=MarkDIR(XmlDIR)
        Time=Get_Time1(HJFile,xmlDIR,timezone=8)
        DateStr=STRTRIM(STRING(Time[5]),2)+FormatString(STRTRIM(STRING(Time[4]),2),2)+ $
            FormatString(STRTRIM(STRING(Time[3]),2),2)
    ENDIF
    NameStr=STRJOIN(fields[0:4],'-')
    band=ReadConfigVar(ConfigFile,'ReferBand')
    CASE (band) OF
        '1': BEGIN
        END
        '2': BEGIN
        END
        '3': BEGIN
        END
        '4': BEGIN
        END
        ELSE: BEGIN
            band='4'
        END
    ENDCASE
    TMRefFileTemp1=TMMosaicDIR+'TMRefBand'+band+'_'+NameStr+'temp1'
    TMRefFileCutTemp=TMMosaicDIR+'TMRefBand'+band+'_'+NameStr+'temp1cut'
    TMRefFileTemp2=TMMosaicDIR+'TMRefBand'+band+'_'+NameStr+'temp2'
    TMRefFile=TMMosaicDIR+'TMRefBand'+band+'_'+NameStr+'.tif'
    IF QUERY_TIFF(TMRefFile) THEN BEGIN
        flag=1
        RETURN
    ENDIF
    HJDate=INTARR(3)
    HJDate[0]=FIX(STRMID(DateStr,0,4))
    HJDate[1]=FIX(STRMID(DateStr,4,2))
    HJDate[2]=FIX(STRMID(DateStr,6,2))
    ;    batch_errfile=OutputDIR+'batch_errfile.txt'
    ENVI_OPEN_FILE,HJFile,r_fid=HJfid
    ENVI_FILE_QUERY,HJfid,ns=ns,nl=nl
    IF HJfid EQ -1 THEN BEGIN
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,lun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,lun,'File can not be read! File name:',HJFile
            PRINTF,lun,'************************************************************'
            FREE_LUN,lun
        ENDIF ELSE BEGIN
            PRINT,'File can not be read! File name:',HJFile
        ENDELSE
        RETURN
    ENDIF
    map_info=ENVI_GET_MAP_INFO(fid=HJfid)
    y0 = map_info.MC[3] & x0 = map_info.MC[2]
    pixel = map_info.PS[0:1]
    x1=x0+ns*pixel[0] & y1=y0-nl*pixel[1]
    params=[6378245.0,6356863.0188, $;a,b
        0.,105.0,$;lat0,lon0
        0.,0.,$;x0,y0
        25.,47.];sp1,sp2
    TMPROJ=ENVI_PROJ_CREATE(TYPE=9,DATUM='Krassovsky',PARAMS=Params)
    HJProj=ENVI_GET_PROJECTION(FID=HJfid)
    ENVI_CONVERT_PROJECTION_COORDINATES,x0,y0, HJProj,p0_x,p0_y, TMPROJ
    ENVI_CONVERT_PROJECTION_COORDINATES,x1,y0, HJProj,p1_x,p1_y, TMPROJ
    ENVI_CONVERT_PROJECTION_COORDINATES,x1,y1, HJProj,p2_x,p2_y, TMPROJ
    ENVI_CONVERT_PROJECTION_COORDINATES,x0,y1, HJProj,p3_x,p3_y, TMPROJ
    HJRoiObj=OBJ_NEW('IDLanROI',[p0_x,p1_x,p2_x,p3_x],[p0_y,p1_y,p2_y,p3_y])
    myshape->GETPROPERTY, N_Entities = nEntities
    FOR x=0, (nEntities-1) DO BEGIN
        ent=myshape->IDLffShape::GetEntity(x)
        TMPts=(*ent.vertices)[*,0:3]
        PtsFlag=HJRoiObj->IDLanROI::ContainsPoints(TMPts)
        IF TOTAL(~(~PtsFlag)) THEN BEGIN
            attr = myshape->IDLffShape::GetAttributes(x)
            orbit=attr.(0)
            fields=strsplit(orbit,'-',/EXTRACT)
            path=fields[0]
            Row=fields[1]
            move_TM,path,row,TMDIR,TMUnRarDIR,HJDate=HJDate
        ENDIF
        myshape->IDLffShape::DestroyEntity, ent
    ENDFOR
    OBJ_DESTROY, myshape
    ;mosaic tm 影像
    TMFiles=FILE_SEARCH(TMUnRarDIR,'*B'+band+'0.TIF',count=count,/FOLD_CASE)
    IF count GT 0 THEN BEGIN
        ;TM文件转投影
        ConverFileMapProj,TMFiles,TMUnRarDIR,HJProj
        TMFiles=FILE_SEARCH(TMUnRarDIR+'*.tif',/FOLD_CASE,count=newcount)
        IF newcount GT 0 THEN BEGIN
            TM_Mosaic,TMFiles,TMRefFileTemp1
        ENDIF
    ENDIF ELSE BEGIN
        PRINT,'None of TM refer image were found! HJ file: ',HJFile
        RETURN
    ENDELSE
    ;裁剪TM影像
    ENVI_OPEN_FILE,TMRefFileTemp1,r_fid=Temp1FID
    coordinate={x0:x0,y0:y0,x1:x1,y1:y1}
    cut_File,TMRefFileTemp1,TMRefFileCutTemp,coordinate
    ENVI_OPEN_FILE,TMRefFileCutTemp,r_fid=Temp1cutFID
    ENVI_FILE_QUERY,Temp1cutFID,nb=nb,DATA_TYPE=out_dt,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=Temp1cutFID, OUT_NAME=TMRefFile,POS=INDGEN(nb), /TIFF
    ;    envi_file_mng, id=Temp2Fid,/DELETE,/REMOVE
    ENVI_FILE_MNG, id=Temp1cutFID,/DELETE,/REMOVE
    ENVI_FILE_MNG, id=Temp1FID,/DELETE,/REMOVE
    ;删除TM解压文件;删除TM文件
    FILE_DELETE,TMFiles,/ALLOW_NONEXISTENT,/QUIET
    flag=1
END

PRO ConverFileMapProj,TMFiles,outDIR,NewProj
    Nums=N_ELEMENTS(TMFiles)
    IF nums LT 1 THEN RETURN
    IF Nums GT 1 THEN BEGIN
        FOR i=0,nums-1 DO BEGIN
            ConverFileMapProj,TMFiles[i],outDIR,NewProj
        ENDFOR
        RETURN
    ENDIF ELSE BEGIN
        ;判断输入文件的有效性
        ENVI_OPEN_FILE,TMFiles[0],R_FID=fid
        IF fid EQ -1 THEN RETURN
        FILE_MKDIR,outDIR
        ;构建输出文件名称
        FileMainName=FILE_BASENAME(TMFiles,'.tif',/FOLD_CASE)
        outTempFile=outDIR+FileMainName+'NewProjTemp'
        outFile=outDIR+FileMainName+'.tif'
        ;判断文件坐标是否为目标坐标
        TMProj=ENVI_GET_PROJECTION(fid=fid)
        ENVI_FILE_QUERY,fid,nb=nb,dims=dims
        IF TMProj.Name NE NewProj.Name || TMProj.Datum NE NewProj.Datum $
            || (TMProj.params)[0] NE (NewProj.params)[0] THEN BEGIN
            ENVI_CONVERT_FILE_MAP_PROJECTION, fid=fid, r_fid=TempFid, $
                pos=LINDGEN(nb),dims=dims,o_proj=NewProj,WARP_METHOD=3,O_PIXEL_SIZE=[30,30],out_name=outTempFile
            ENVI_FILE_QUERY,TempFid,nb=nb,DATA_TYPE=out_dt,dims=dims
            ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
                , FID=TempFid, OUT_NAME=outFile,POS=INDGEN(nb), /TIFF
            ENVI_FILE_MNG, id=TempFid,/DELETE,/REMOVE
        ENDIF ELSE BEGIN
            FILE_COPY,TMFiles,outFile,/ALLOW_SAME,/OVERWRITE
        ENDELSE
        ;删除原文件夹
        TMFold=FILE_DIRNAME(TMFiles,/MARK_DIRECTORY)
        FILE_DELETE,TMFold,/ALLOW_NONEXISTENT,/RECURSIVE,/QUIET
    ENDELSE
;    TMFiles=TMFiles[0]
END
PRO move_TM,path,row,TMDIR,DesDIR,HJDate=HJDate
    ;将目标path,row的TM移动到目标文件夹(解压)
    ;格式化PATH ROW的字符（补0）
    COMPILE_OPT idl2
    TMDIR=FILE_DIRNAME(TMDIR,/MARK_DIRECTORY)+FILE_BASENAME(TMDIR)+PATH_SEP()
    DesDIR=FILE_DIRNAME(DesDIR,/MARK_DIRECTORY)+FILE_BASENAME(DesDIR)+PATH_SEP()
    FILE_MKDIR,DesDIR
    path=FormatString(path,3,cmpStr='0');completement string
    ROW=FormatString(row,3,cmpStr='0')
    SignStr=STRJOIN([path,row])
    Files=FILE_SEARCH(TMDIR+'*'+SignStr+'*.tar.gz',count=count,/FOLD_CASE)
    ;剔除不合格文件
    IF count EQ 0 THEN RETURN
    FileNum=count
    FOR i=0,count-1 DO BEGIN
        IF i GT count-1 THEN BREAK
        Filename=FILE_BASENAME(Files[i])
        FileSignStr=STRMID(Filename,3,6)
        IF ~STRCMP(FileSignStr,SignStr,/FOLD_CASE) THEN BEGIN
            Files[i]=''
            FileNum--
        ENDIF
    ENDFOR
    IF FileNum LT 1 THEN RETURN
    ValidFiles=STRARR(FileNum)
    j=0
    FOR i=0,count-1 DO BEGIN
        IF j EQ FileNum THEN BREAK
        IF Files[i] NE '' THEN BEGIN
            ValidFiles[j]=Files[i]
            j++
        ENDIF
    ENDFOR
    IF FileNum EQ 1 THEN BEGIN
        Filename=FILE_BASENAME(ValidFiles[0])
        FileSignStr=STRMID(Filename,3,6)
        IF STRCMP(FileSignStr,SignStr,/FOLD_CASE) THEN BEGIN
            FinalFile=ValidFiles[0]
        ENDIF
    ENDIF ELSE BEGIN
        ;如果文件多于一个则选取时间近的文件
        ;首先去除异常，即除去HJ-1文件名没有时间的
        JuldaysArr=LONARR(FileNum)
        FOR i=0,FileNum-1 DO BEGIN
            Filename=FILE_BASENAME(ValidFiles[i])
            IF STRMID(Filename,9,1) EQ '-' THEN BEGIN
                TimeStr=STRMID(Filename,13,8)
                Year=FIX(STRMID(Timestr,0,4))
                month=FIX(STRMID(Timestr,4,2))
                day=FIX(STRMID(Timestr,6,2))
                JuldaysArr[i]=julday(month,day,year)
            ENDIF ELSE BEGIN
                TimeStr=STRMID(Filename,9,7)
                Year=FIX(STRMID(Timestr,0,4))
                days=FIX(STRMID(Timestr,4,3))
                JuldaysArr[i]=julday(12,31,year-1)+days
            ENDELSE
        ENDFOR
        IF N_ELEMENTS(HJDate) EQ 3 THEN BEGIN
            HJJulday=julday(HJDate[1],HJDate[2],HJDate[0])
            JuldaysArr=ABS(HJJulday-JuldaysArr)
        ENDIF
        index=SORT(JuldaysArr)
        FinalFile=ValidFiles[index[0]];
    ENDELSE
    winrar,FinalFile,DesDIR
END

PRO winrar,rarfile,outputDIR
    COMPILE_OPT idl2
    COMMON GlobalVar
    FILE_MKDIR,outputDIR
    ;    winrarDIR='c:\program files\winrar\'
    winrarDIR=ReadConfigVar(ConfigFile,'winrarDIR')
    winrarDIR=MarkDIR(winrarDIR)
    WinRAR_exe=FILE_SEARCH(winrarDIR+'winrar.exe',/FOLD_CASE,count=count)
    IF count LT 1 THEN BEGIN
        PRINT,'Directory of winrar is wrong! Directory: '+winrarDIR
        PRINT,'Decompressing file error!'
        RETURN
    ENDIF
    CD,winrarDIR
    SPAWN,'winRAR e -y -ibck '+ $
        rarfile+' -AD '+outputDIR,/HIDE
END

FUNCTION FormatString,str,length,cmpStr=cmpStr,location=location
    COMPILE_OPT idl2
    IF N_ELEMENTS(cmpStr) EQ 0 THEN cmpStr='0'
    IF N_ELEMENTS(Location) EQ 0 THEN Location=0;表示填补字符在前
    IF STRLEN(str) GE length THEN RETURN,str ELSE BEGIN
        IF location EQ 0 THEN BEGIN
            str=STRJOIN([cmpStr,str])
        ENDIF ELSE BEGIN
            str=STRJOIN([str,cmpStr])
        ENDELSE
        str=FormatString(str,length,cmpStr=cmpStr,location=location)
        RETURN,str
    ENDELSE
    
END
PRO TM_Mosaic,fileseeds,OutFile
    COMPILE_OPT IDL2
    COMMON GlobalVar
    outputDIR=FILE_DIRNAME(OutFile)
    outName=FILE_BASENAME(OutFile)
    outputDIR=FILE_DIRNAME(outputDIR)+PATH_SEP()+FILE_BASENAME(outputDIR)+PATH_SEP()
    FILE_MKDIR,outputDIR
    Nums=N_ELEMENTS(fileseeds)
    IF Nums LT 1 ||  fileseeds[0] EQ '' THEN BEGIN
        PRINT,'No files in the mosaic filelist!'
        RETURN
    ENDIF
    FileName=FILE_BASENAME(fileseeds[0],/FOLD_CASE)
    fields=strsplit(FileName,'.',count=count,/EXTRACT)
    IF count GT 1 THEN BEGIN
        IF count EQ 2 THEN FileMainName=fields[0] ELSE BEGIN
            FileMainName=STRJOIN(fields[0:count-2],'.')
            extendName='.'+fields[count-1]
        ENDELSE
    ENDIF ELSE BEGIN
        FileMainName=FileName
        extendName=''
    ENDELSE
    Fields=strsplit(FileMainName,'-',/EXTRACT)
    tempoutFile=outputDIR+outName+'temp'
    IF FILE_SEARCH(OutFile,/FOLD_CASE) NE '' THEN RETURN
    IF Nums EQ 1 THEN BEGIN
        ENVI_OPEN_FILE,fileseeds[0],R_FID=fid
        ENVI_FILE_QUERY,fid,dims=dims,nb=nb
        ENVI_OUTPUT_TO_EXTERNAL_FORMAT,DIMS=dims,fid=fid,/ENVI,OUT_NAME=OutFile, POS=INDGEN(nb)
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
        dims=dimsarr, out_name=OutFile,BACKGROUND=0,GEOREF=1,USE_SEE_THROUGH=INTARR(nums)+1, $
        SEE_THROUGH_VAL=seeTv,PIXEL_SIZE=out_ps, XSIZE=xsize,OUT_DT=out_dt,$
        X0=x0, YSIZE=ysize, Y0=y0,map_info=map_info
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


FUNCTION ReadConfigVar,ConFile,varName,Num_element=Num_element,delimSym=delimSym,necessary=necessary;,VarValue=VarValue
    COMPILE_OPT idl2
    OPENR,lun,ConFile,/GET_LUN
    nlines=FILE_LINES(ConFile)
    StrArray=STRARR(nlines)
    READF,lun,StrArray
    FREE_LUN,lun
    IF N_ELEMENTS(Num_element) EQ 0 THEN Num_element=1
    IF N_ELEMENTS(delimSym) EQ 0 THEN delimSym=','
    FOR i=0,nlines[0]-1 DO BEGIN
        ;delete comment part
        Tempstr=StrArray[i]
        DeleteComment,Tempstr,'#'
        DeleteComment,Tempstr,';'
        DeleteComment,Tempstr,'%'
        pos=STRPOS(STRUPCASE(Tempstr),STRUPCASE(varName))
        endPos=pos+STRLEN(varName)
        IF pos NE -1 THEN BEGIN
            equal_pos=STRPOS(Tempstr,'=',endPos)
            IF equal_pos EQ -1 THEN CONTINUE
            VarValue=STRTRIM(STRMID(Tempstr,equal_pos+1),2)
            IF Num_element GT 1 THEN BEGIN
                fields=strsplit(VarValue,delimSym,/EXTRACT,count=count)
                ;if count ge Num_element then return, fields[0:Num_element-1] $
                ;else return,fields
                IF count GE Num_element THEN RETURN,fields[0:Num_element-1]
            ENDIF ELSE BEGIN
                RETURN,VarValue
            ENDELSE
        ENDIF
    ENDFOR
    IF KEYWORD_SET(necessary) THEN BEGIN
        PRINT,'Warning! Variable: ',varName,' cannot read from config file!'
    ENDIF
    RETURN,''
END
PRO DeleteComment,str,commentSym
    CommentPos=STRPOS(str,STRING(commentSym))
    IF CommentPos NE -1 THEN BEGIN
        str=STRMID(str,0,CommentPos)
    ENDIF
END
FUNCTION MarkDIR,str
    ;    return,file_dirname(str,/MARK_DIRECTORY)+file_basename(str)
    IF STRMID(str,STRLEN(str)-1,1) NE PATH_SEP() THEN value=str+PATH_SEP() ELSE value=str
    RETURN,value
END

PRO DO_ENVI_QUAC_DOIT,input_file,outDIR,overwrite=overwrite,flag=flag
    ;        print,'Input file or outDIR is null. Atmosphere failure!'
    ;        return
    ;    endif
    ;    if n_elements(input_file) gt 1 then begin
    ;        Nums=n_elements(input_file)
    ;        for i=0,nums-1 do begin
    ;            if keyword_set(overwrite) then begin
    ;            DO_ENVI_QUAC_DOIT,input_file[i],outDIR,/OVERWRITE
    ;            endif else begin
    ;            DO_ENVI_QUAC_DOIT,input_file[i],outDIR
    ;            endelse
    ;        endfor
    ;        return
    ;    endif
    flag=0
    COMPILE_OPT IDL2
    fileName=FILE_BASENAME(input_file)
    fields=strsplit(FileName,'.',count=count,/EXTRACT)
    IF count GT 1 THEN BEGIN
        IF count EQ 2 THEN FileMainName=fields[0] ELSE BEGIN
            FileMainName=STRJOIN(fields[0:count-2],'.')
        ENDELSE
        extendName='.'+fields[count-1]
    ENDIF ELSE BEGIN
        FileMainName=FileName
        extendName=''
    ENDELSE
    FILE_MKDIR,outDIR
    Temp_ENVI_File=outDIR+FileMainName+'ENVI'
    out_name = outDIR+FileMainName+'temp0'
    NewTempFile=outDIR+FileMainName+'temp1'
    NewFile=outDIR+FileMainName+'.tif'
    IF ~KEYWORD_SET(overwrite) THEN BEGIN
        IF FILE_SEARCH(NewFile) NE '' THEN BEGIN
            flag=1
            RETURN
        ENDIF
    ENDIF
    fields=strsplit(FileMainName,'-',/EXTRACT)
    SatStr=STRMID(fields[0],STRLEN(fields[0])-4)
    CCDStr=fields[1]
    SensorStr=STRUPCASE(STRJOIN([SatStr,CCDStr],'-'))
    CASE SensorStr OF
        'HJ1A-CCD1': BEGIN
            WaveLength=[0.480,0.570,0.660,0.790]
        END
        'HJ1A-CCD2': BEGIN
            WaveLength=[0.500,0.560,0.640,0.780]
        END
        'HJ1B-CCD1': BEGIN
            WaveLength=[0.495,0.550,0.680,0.790]
        END
        'HJ1B-CCD2': BEGIN
            WaveLength=[0.480,0.570,0.680,0.790]
        END
        ELSE: BEGIN
            WaveLength=0
        END
    ENDCASE
    IF N_ELEMENTS(WaveLength) NE 4 THEN BEGIN
        PRINT,'Cannot get the sensor information from the file name! File name:'+input_file
        PRINT,'Atmosphere correction failure!'
        RETURN
    ENDIF
    ENVI_OPEN_FILE,input_file,r_fid=fid
    IF (fid EQ -1) THEN BEGIN
        RETURN
    ENDIF
    ENVI_FILE_QUERY, fid, dims=dims, nb=nb,ns=ns,nl=nl,DATA_TYPE=DATA_TYPE, $
        INTERLEAVE= INTERLEAVE, OFFSET=OFFSET,file_type=file_type
    map_info=ENVI_GET_MAP_INFO(fid=fid)
    pos = LINDGEN(nb)
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=fid, OUT_NAME=Temp_ENVI_File,POS=INDGEN(nb), /ENVI
    ENVI_SETUP_HEAD, FNAME= Temp_ENVI_File,WAVELENGTH_UNIT=0,WL=WaveLength, $
        nb=nb,ns=ns,nl=nl,DATA_TYPE=DATA_TYPE,file_type=file_type, $
        INTERLEAVE= INTERLEAVE, OFFSET=OFFSET, /write,MAP_INFO=map_info,/open
    ENVI_FILE_MNG,ID=fid, /REMOVE
    ENVI_OPEN_FILE,Temp_ENVI_File,r_fid=fid
    ENVI_DOIT, 'envi_quac_doit', $
        fid=fid, pos=pos, dims=dims, $
        out_name=out_name, r_fid=r_fid
    IF (r_fid EQ -1) THEN BEGIN
        RETURN
    ENDIF
    ENVI_FILE_QUERY,r_fid,nb=nb,ns=ns,nl=nl,INTERLEAVE=INTERLEAVE
    map_info=ENVI_GET_MAP_INFO(fid=r_fid)
    My_Tile=ENVI_INIT_TILE(r_fid,INDGEN(nb),NUM_TILES=Num,INTERLEAVE=1)
    OPENW,lun,NewTempFile,/GET_LUN
    FOR i=0,Num-1 DO BEGIN
        data=ENVI_GET_TILE(My_Tile,i)
        ;        data=envi_get_tile(My_Tile,0)
        data=FIX(data>0)
        WRITEU,lun,data
    ENDFOR
    data_type=SIZE(data,/type)
    data=1
    FREE_LUN,lun
    ENVI_TILE_DONE,My_Tile
    ENVI_SETUP_HEAD, fname=NewTempFile, ns=ns, nl=nl, nb=nb, $
        data_type=data_type, offset=0, interleave=1, $
        MAP_INFO=map_info, /write, /open
    ENVI_OPEN_FILE,NewTempFile,r_fid=tmpfid
    ENVI_FILE_QUERY,tmpfid,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=tmpfid, OUT_NAME=NewFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG,id=tmpfid,/REMOVE,/DELETE
    ENVI_FILE_MNG,id=r_fid,/REMOVE,/DELETE
    ENVI_FILE_MNG,id=fid,/REMOVE,/DELETE
    PRINT,'Atmosphere correction done! File name: '+input_file
    flag=1
END

PRO getRange,RangeFile,FirstCoordinate=FirstCoordinate,FinalCoordinate=FinalCoordinate,FirstRangFile=FirstRangeFile,FinalRangeFile=FinalRangeFile
    COMPILE_OPT idl2
    ;    RangeFile='/home/chenbo/test/1/RangFile.rf'
    OPENR,lun,RangeFile,/GET_LUN
    nlines=FILE_LINES(RangeFile)
    StrArray=STRARR(nlines)
    READF,lun,StrArray
    FREE_LUN,lun
    Coor='cutcoordinate'
    FOR i=0,nlines[0]-1 DO BEGIN
        ;delete comment part
        CommentPos=STRPOS(StrArray[i],'#')
        IF CommentPos NE -1 THEN BEGIN
            StrArray[i]=STRMID(StrArray[i],0,CommentPos)
        ENDIF
        ;Get First Coordinate and final coordinate
        ;        TwoCoorFlag=intarr(2)
        pos=STRPOS(STRLOWCASE(StrArray[i]),Coor)
        IF pos NE -1 THEN BEGIN
            SearchStr=['utmzone','x0','y0','x1','y1']
            result=FLTARR(5)
            startpos=STRPOS(strArray[i],'{')
            endpos=STRPOS(strArray[i],'}')
            coorStr=STRMID(strArray[i],startpos+1,endpos-startpos-1)
            FOR k=0,4 DO BEGIN
                KeyStrPos=STRPOS(STRLOWCASE(coorStr),SearchStr[k])
                lenth=STRLEN(coorStr)
                colonPos=STRPOS(coorStr,':',KeyStrPos)
                commaPos=STRPOS(coorStr,',',KeyStrPos)
                IF commaPos EQ -1 THEN commaPos=lenth
                nextColonPos=STRPOS(coorStr,':',colonPos+1)
                IF nextColonPos EQ -1 THEN nextColonPos=lenth
                IF nextColonPos LT commaPos THEN BEGIN
                    PRINT,Coor+' format error! Please checkfile: '+RangeFile
                    BREAK
                ENDIF
                numberStr=STRMID(coorStr,colonPos+1,commaPos-colonPos-1)
                result[k]=FLOAT(numberStr)
                IF k EQ 4 THEN BEGIN
                    pixels=50
                    FinalCoordinate={UTMZone:FIX(result[0]),x0:result[1],y0:result[2],x1:result[3],y1:result[4]}
                    FirstCoordinate={UTMZone:FIX(result[0]),x0:(result[1]-pixels*30),y0:(result[2]+pixels*30),$
                        x1:(result[3]+pixels*30),y1:(result[4]-pixels*30)}
                ENDIF
            ENDFOR
        ENDIF
        ;get RangeFile
        CutFile='cutrangefile'
        pos=STRPOS(STRLOWCASE(StrArray[i]),CutFile)
        IF pos NE -1 THEN BEGIN
            eqSymPos=STRPOS(StrArray[i],'=')
            IF eqSymPos EQ -1 THEN CONTINUE
            FilePathstr=STRMID(StrArray[i],eqSymPos+1)
            FilePathstr=STRTRIM(FilePathstr,2)
            ;if ' exist then delete it
            IF STRMID(FilePathstr,0,1) EQ "'" THEN FilePathstr=STRMID(FilePathstr,1)
            IF STRMID(FilePathstr,STRLEN(FilePathstr)-1,1) EQ "'" THEN FilePathstr=STRMID(FilePathstr,0,STRLEN(FilePathstr)-1)
            IF FILE_SEARCH(FilePathstr,/FOLD_CASE) NE ''  THEN BEGIN
                CutRangFile=FilePathstr
                IF N_ELEMENTS(FirstCoordinate) EQ 0 THEN BEGIN
                    FinalCoordinate=rangefile2UTMcoord(CutRangFile)
                    FirstCoordinate={UTMZone:FIX(result[0]),x0:(FinalCoordinate.x0-pixels*30),y0:(FinalCoordinate.y0+pixels*30),$
                        x1:(FinalCoordinate.x1+pixels*30),y1:(FinalCoordinate.y1-pixels*30)}
                ENDIF
            ENDIF
        ENDIF
    ENDFOR
END

PRO Do_HJPre2,fileseed,outputDIR,xmlDIR,ParaDIR,TempDIR=TempDIR,overwrite=overwrite,RefFile=ReflectanceFile,flag=flag
    flag=0
    COMPILE_OPT idl2
    ;    fileseed='D:\test\HJ\HJ1A-CCD2-444-52-20100930-L20000401052-1'
    ;    outputDIR='D:\test\HJ\123123123\'
    ;    TempDIR=outputDIR
    ;    xmlDIR='D:\test\HJ\xml\'
    ;    ParaDIR='E:\CarbonDataPre\HJCalPara\'
    IF N_ELEMENTS(TempDIR) NE 0 THEN BEGIN
        FILE_MKDIR,TempDIR,outputDIR
    ENDIF ELSE BEGIN
        TempDIR=outputDIR
        FILE_MKDIR,outputDIR
    ENDELSE
    ;    ENVI,/RESTORE_BASE_SAVE_FILES
    ;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    outputDIR=MarkDIR(outputDIR)
    FileMainName=GetFileMainName(fileseed,extendName=extendName)
    FileMainName=STRMID(FileMainName,0,STRLEN(FileMainName)-2)
    inputDIR=FILE_DIRNAME(FileSeed,/MARK_DIRECTORY)
    Band1File=inputDIR+FileMainName+'-1'+extendName
    Band2File=inputDIR+FileMainName+'-2'+extendName
    Band3File=inputDIR+FileMainName+'-3'+extendName
    Band4File=inputDIR+FileMainName+'-4'+extendName
    batch_errfile=outputDIR+'BatchError.txt'
    TempOutFile=TempDIR+FileMainName+'temp'
    SunFile=TempDIR+FileMainName+'Sun'
    ReflectanceFile=outputDIR+FileMainName+'.tif'
    TFwFile=outputDIR+FileMainName+'.tfw'
    IF ~KEYWORD_SET(overwrite) THEN BEGIN
        IF QUERY_TIFF(ReflectanceFile) THEN BEGIN
            flag=1
            RETURN
        ENDIF
    ENDIF
    ENVI_OPEN_FILE,TempOutFile,R_FID=TempFid
    IF ~KEYWORD_SET(overwrite) THEN BEGIN
        IF TempFid NE -1 THEN BEGIN
            ENVI_OPEN_FILE,SunFile,R_FID=sunfid
            GOTO,outputTiff
        ENDIF
    ENDIF
    ENVI_OPEN_FILE, Band1File, r_fid=band1_fid
    ENVI_OPEN_FILE, Band2File, r_fid=band2_fid
    ENVI_OPEN_FILE, Band3File, r_fid=band3_fid
    ENVI_OPEN_FILE, Band4File, r_fid=band4_fid
    IF (band1_fid EQ -1) || band2_fid EQ -1 || band3_fid EQ -1 || band4_fid EQ -1 THEN BEGIN
        OPENW,lun,batch_errfile,/get_lun,/APPEND
        PRINTF,lun,'**********************************************************'
        PRINTF,lun,'Error! Invalid file in fold: '+inputDIR
        PRINTF,lun,'FileName:',FileMainName
        PRINTF,lun,SYSTIME()
        FREE_LUN,lun
        RETURN
    ENDIF
    ;检查XML文件及PARA文件
    xmlFile=xmlDIR+FileMainName+'.xml'
    xmlFile=(FILE_SEARCH(xmlFile,/FOLD_CASE))[0]
    IF xmlFile EQ '' THEN BEGIN
        OPENW,lun,batch_errfile,/get_lun,/APPEND
        PRINTF,lun,'**********************************************************'
        PRINTF,lun,'Cannot find the xml file of file:'+FileMainName
        PRINTF,lun,'Path:',xmlFile
        FREE_LUN,lun
        RETURN
    ENDIF
    GetGainoff,xmlfile,Gain=Gain,offset=offset,logfile=batch_errfile,flag=goflag
    IF ~goflag THEN RETURN
    DSunFile=ParaDIR+'d_sun.txt'
    IF FILE_SEARCH(DSunFile) EQ '' THEN BEGIN
        OPENW,lun,batch_errfile,/get_lun,/APPEND
        PRINTF,lun,'**********************************************************'
        PRINTF,lun,'Cannot find parameter d_sun.txt:',DSunFile
        FREE_LUN,lun
        RETURN
    ENDIF
    ;计算太阳高度角文件
    time=get_time1(ReflectanceFile,xmlDIR,timezone=8)
    IF SIZE(Time,/n_elements) LT 6 THEN RETURN
    Year=time[5] & Month=time[4] & Day=time[3]
    hour=time[2] & minute=time[1] & second=time[0]
    map_info=ENVI_GET_MAP_INFO(fid=band1_fid)
    ENVI_FILE_QUERY,band1_fid,ns=ns,nl=nl
    Y0 = map_info.MC[3] & X0 = map_info.MC[2]
    GetXmlValue,xmlFile,'zone',Value=zoneStr
    UTMZone=FIX(STRMID(zoneStr,0,STRLEN(zonestr)-1))
    UTMProj=MAP_PROJ_INIT('UTM', ELLIPSOID='WGS 84',/GCTP,zone=UTMZone)
    !map=UTMProj
    pixel = map_info.PS[0:1]
    ZonePixel=[50,50]
    XBlocks=CEIL(ns*1.0/ZonePixel[0])
    YBlocks=CEIL(nl*1.0/ZonePixel[1])
    XS=INDGEN(XBlocks)*ZonePixel[0] & XE=INDGEN(XBlocks)*ZonePixel[0]+ZonePixel[0]-1 & XE[XBlocks-1]=ns-1
    YS=INDGEN(YBlocks)*ZonePixel[1] & YE=INDGEN(YBlocks)*ZonePixel[1]+ZonePixel[1]-1 & YE[YBlocks-1]=nl-1
    ;        !map=OriProj
    IF FILE_SEARCH(SunFile) NE '' THEN BEGIN
        ENVI_OPEN_FILE,SunFile,R_FID=sunfid
        IF sunfid NE -1 THEN GOTO,Transref
    ENDIF
    PRINT,FileMainName,' Start compute sun file...'
    OPENW,lun,SunFile,/GET_LUN
    FOR Block_Y=0,YBlocks-1 DO BEGIN
        YBlockLines=YE[Block_Y]-YS[Block_Y]+1
        SunArr=FLTARR(ns)
        Y_UL=Y0-YS[Block_Y]*pixel[1]
        Y_LR=Y0-YE[Block_Y]*pixel[1]
        FOR Block_X=0,XBlocks-1 DO BEGIN
            X_UL=X0+XS[Block_X]*pixel[0]
            X_LR=X0+XE[Block_X]*pixel[0]
            CenterX=(X_UL+X_LR)/2. & CenterY=(Y_UL+Y_LR)/2.
            CenterLonLat=MAP_PROJ_INVERSE(CenterX,CenterY)
            zenith = solpos(Year, Month, Day, hour, minute, second, 0, CenterLonLat[0], CenterLonLat[1])
            Sunarr[XS[Block_X]:XE[Block_X]]= COS(zenith * !dtor)
        ENDFOR
        FOR writeLines=0,YBlockLines-1 DO BEGIN
            WRITEU,LUN,SunArr
        ENDFOR
    ENDFOR
    SunArr=1
    FREE_LUN,lun
    ENVI_SETUP_HEAD, fname=SunFile, ns=ns, nl=nl, nb=1, $
        data_type=4, offset=0, interleave=0, $
        MAP_INFO=map_info, /write, /open
    ENVI_OPEN_FILE,SunFile,R_FID=sunfid
    Transref:Day_Num=Julday(month,day,Year)-Julday(12,31,Year-1)
    D_Array=READ_ASCII(DSunFile)
    D=D_Array.field1[1,Day_Num-1]
    Convert_Para=READ_ASCII(ParaDIR+STRMID(FileMainName,0,9)+'.txt')
    IF N_ELEMENTS(TileSize) EQ 0 THEN TileSize=100.0
    My_Tile_init,band1_fid,TileNum=TileNum,YE=YE,YS=YS,TileSize=TileSize,nb=4
    fids=[band1_fid,band2_fid,band3_fid,band4_fid]
    ;转反射率
    OPENW,reflun,TempOutFile,/GET_LUN
    FOR i=0,TileNum-1 DO BEGIN
        ;        FOR band=0,3 DO BEGIN
        data=UINTARR(4,ns,YE[i]-YS[i]+1)
        Sunarr=ENVI_GET_DATA(DIMS=[-1,0,ns-1,YS[i],YE[i]],FID=sunfid,pos=0)
        FOR band=0,3 DO BEGIN
            temparr=ENVI_GET_DATA(DIMS=[-1,0,ns-1,YS[i],YE[i]],FID=fids[band],pos=0)
            index=WHERE(temparr EQ 0,icount)
            temparr=FIX((temparr/FLOAT(gain[band])+offset[band])/(Convert_Para.field1[band,2]*Sunarr)*!PI*D*D*10000)
            IF icount GT 0 THEN temparr[index]=0
            data[band,*,*]=temparr
        ENDFOR
        WRITEU,reflun,data;BIP顺序
    ENDFOR
    data=0 & Sunarr=0 & temparr=0 & index=0
    FREE_LUN,reflun
    ENVI_SETUP_HEAD, fname=TempOutFile, ns=ns, nl=nl, nb=4, $
        data_type=2, offset=0, interleave=2, $
        MAP_INFO=map_info, /write, /open
    ;////////////////////////////////////////////////////////
    ENVI_OPEN_FILE,TempOutFile,R_Fid=TempFid
    outputTiff:
    ENVI_FILE_QUERY,TempFid,dims=dims,nb=nb
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=TempFid, OUT_NAME=ReflectanceFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG, id=TempFid,/DELETE,/REMOVE
    ENVI_FILE_MNG, id=sunfid,/DELETE,/REMOVE
    FILE_DELETE,TFwFile,/ALLOW_NONEXISTENT,/QUIET
    PRINT,FileMainName,' Pre-process done!'
    ;    ENVI_BATCH_EXIT
    flag=1
END

PRO seedcut,fileseed,cutDIR,SeedResult=SeedResult,Coordinate=Coordinate,overwrite=overwrite,flag=flag
    flag=0
    IF STRMID(cutDIR,STRLEN(cutDIR)-1,1) NE PATH_SEP() THEN cutDIR=cutDIR+PATH_SEP()
    FileName=FILE_BASENAME(FileSeed,/FOLD_CASE)
    fields=strsplit(FileName,'.',count=count,/EXTRACT)
    IF count GT 1 THEN BEGIN
        IF count EQ 2 THEN FileMainName=fields[0] ELSE BEGIN
            FileMainName=STRJOIN(fields[0:count-2],'.')
        ENDELSE
        extendName='.'+fields[count-1]
    ENDIF ELSE BEGIN
        FileMainName=FileName
        extendName=''
    ENDELSE
    FileMainName=STRMID(FileMainName,0,STRLEN(FileMainName)-2)
    inputDIR=FILE_DIRNAME(FileSeed,/MARK_DIRECTORY)
    Band1File=inputDIR+FileMainName+'-1'+extendName
    Band2File=inputDIR+FileMainName+'-2'+extendName
    Band3File=inputDIR+FileMainName+'-3'+extendName
    Band4File=inputDIR+FileMainName+'-4'+extendName
    SeedResult=cutDIR+FileMainName+'-1'
    IF ~KEYWORD_SET(overwrite) THEN BEGIN
        IF FILE_SEARCH(SeedResult) NE '' THEN BEGIN
            flag=1
            RETURN
        ENDIF
        RangeCut,[Band1File,Band2File,Band3File,Band4File],cutDIR,Coordinate=Coordinate,flag=flag
    ENDIF ELSE BEGIN
        RangeCut,[Band1File,Band2File,Band3File,Band4File],cutDIR,Coordinate=Coordinate,flag=flag,/OVERWRITE
    ENDELSE
END

FUNCTION solpos, year, month, day, hour, minute, second, timezone, longitude, latitude
    J = julday(month, day, year)-julday(12, 31, year-1)
    dayang = 360.0 * ( J - 1 ) / 365.0
    sd     = sin (!dtor * dayang)
    cd     = cos (!dtor * dayang)
    d2     = 2.0 * dayang
    c2     = cos (!dtor * d2)
    s2     = sin (!dtor * d2)
    erv  = 1.000110 + 0.034221 * cd + 0.001280 * sd
    erv  += 0.000719 * c2 + 0.000077 * s2
    interval = 0
    utime =        hour * 3600.0 +        minute * 60.0 +        second -        interval / 2.0
    utime = utime / 3600.0 - timezone
    delta    = year - 1949
    leap     = FIX( delta / 4.0 )
    julday = 32916.5 + delta * 365.0 + leap + J + utime / 24.0
    ectime = julday - 51545.0
    mnlong  = 280.460 + 0.9856474 * ectime
    mnlong -= 360.0 * FIX( mnlong / 360.0 )
    IF ( mnlong LT 0.0 )  THEN mnlong += 360.0
    mnanom  = 357.528 + 0.9856003 * ectime
    mnanom -= 360.0 * fix ( mnanom / 360.0 )
    IF ( mnanom LT 0.0 ) THEN mnanom += 360.0
    eclong  = mnlong + 1.915 * sin ( mnanom * !dtor ) + 0.020 * sin ( 2.0 * mnanom * !dtor )
    eclong -= 360.0 * fix ( eclong / 360.0 )
    IF ( eclong LT 0.0 ) THEN eclong += 360.0
    ecobli = 23.439 - 4.0e-07 * ectime
    declin =  !radeg * asin ( sin (ecobli * !dtor) * sin (eclong * !dtor) )
    top      =  cos ( !dtor * ecobli ) * sin ( !dtor * eclong )
    bottom   =  cos ( !dtor * eclong )
    rascen =  !radeg * atan ( top, bottom )
    IF ( rascen LT 0.0 ) THEN rascen += 360.0
    gmst  = 6.697375 + 0.0657098242 * ectime + utime
    gmst -= 24.0 * fix ( gmst / 24.0 )
    IF ( gmst LT 0.0 ) THEN gmst += 24.0
    lmst  = gmst * 15.0 + longitude
    lmst -= 360.0 * fix ( lmst / 360.0 )
    IF ( lmst LT 0.) THEN        lmst += 360.0
    hrang = lmst - rascen
    IF ( hrang LT -180.0 ) THEN hrang += 360.0 ELSE IF ( hrang GT  180.0 ) THEN hrang -= 360.0
    cd = cos ( !dtor * declin )
    ch = cos ( !dtor * hrang )
    cl = cos ( !dtor * latitude )
    sd = sin ( !dtor * declin )
    sl = sin ( !dtor * latitude )
    cz = sd * sl + cd * cl * ch
    IF ( abs (cz) GT 1.0 ) THEN $
        IF ( cz GE 0.0 ) THEN $
        cz =  1.0 $
    ELSE cz = -1.0
    zenetr = acos ( cz ) * !radeg
    IF ( zenetr GT 99.0 ) THEN $
        zenetr = 99.0
    RETURN, zenetr
END

;FUNCTION Get_Time1,inputfile,xmlDIR,timezone=timezone
;    fileName=FILE_BASENAME(inputfile);HJ1A-CCD2-7-76-20100523.tif
;    Time=''
;    IF STRMID(xmlDIR,STRLEN(xmlDIR)-1,1) NE PATH_SEP() THEN xmlDIR=xmlDIR+PATH_SEP()
;    StartPos=STRPOS(STRUPCASE(fileName),'HJ1')
;    EndPos=STRPOS(STRLOWCASE(fileName),'.tif')
;    IF EndPos EQ -1 THEN EndPos=STRLEN(fileName)
;    xml_name=xmlDIR+STRMID(fileName,StartPos,EndPos-StartPos)+'.XML';HJ1A-CCD2-7-76-20100523.xml
;    xml_name=(FILE_SEARCH(xml_name,/FOLD_CASE))[0]
;    IF xml_name EQ '' THEN RETURN,Time
;    File_template={$
;        VERSION : 1.00000,$
;        DATASTART : 26L,$
;        DELIMITER : 44b,$
;        MISSINGVALUE : !VALUES.F_NAN,$
;        COMMENTSYMBOL: '',$
;        FIELDCOUNT : 1L,$
;        FIELDTYPES : 7L,$
;        FIELDNAMES : 'Scene_Time',$
;        FIELDLOCATIONS : 0L,$
;        FIELDGROUPS : 0L$
;        }
;    xml_data=READ_ASCII(xml_name,num_records=1,template=file_template)
;    DateTemp=STRMID(xml_data.scene_time,11,10)
;    DateStr=STRJOIN(strsplit(DateTemp,'-',/extract))
;    IF (strsplit((strsplit(fileName,'-',/extract))[4],'.',/extract))[0] EQ DateStr THEN BEGIN
;        TimeTemp=STRMID(xml_data.scene_time,22,8)
;        Time=INTARR(6)
;        Time[5]=FIX(STRMID(DateStr,0,4))
;        Time[4]=FIX(STRMID(DateStr,4,2))
;        Time[3]=FIX(STRMID(DateStr,6,2))
;        Time[2]=FIX((strsplit(TimeTemp,':',/extract))[0])
;        Time[1]=FIX((strsplit(TimeTemp,':',/extract))[1])
;        Time[0]=FIX((strsplit(TimeTemp,':',/extract))[2])
;    ENDIF
;    IF N_ELEMENTS(timezone) THEN BEGIN
;        IF Time[2]+timezone GE 18 THEN BEGIN
;            PRINT,'HJ acquire time error! We modified it to new time according to timezone.'
;            PRINT,'Original hour: '+STRTRIM(STRING(Time[2]),2)+'---->Modifed hour: '+STRTRIM(STRING(Time[2]-timezone),2)
;            Time[2]-=timezone
;        ENDIF
;    ENDIF
;    RETURN,time
;END
;PRO FDFD
;inputfile='E:\test\HJOrtho\10277\HJ1A-CCD2-24-68-L20000010277.TIF'
;xmlDIR='E:\test\HJOrtho\10277\'
;PRINT,Get_Time1(inputfile,xmlDIR,timezone=8)
;END
FUNCTION Get_Time1,inputfile,xmlDIR,timezone=timezone
    fileName=FILE_BASENAME(inputfile);HJ1A-CCD2-7-76-20100523.tif
    Time=''
    IF STRMID(xmlDIR,STRLEN(xmlDIR)-1,1) NE PATH_SEP() THEN xmlDIR=xmlDIR+PATH_SEP()
    StartPos=STRPOS(STRUPCASE(fileName),'HJ1')
    EndPos=STRPOS(STRLOWCASE(fileName),'.tif')
    IF EndPos EQ -1 THEN EndPos=STRLEN(fileName)
    xml_name=xmlDIR+STRMID(fileName,StartPos,EndPos-StartPos)+'.XML';HJ1A-CCD2-7-76-20100523.xml
    xml_name=(FILE_SEARCH(xml_name,/FOLD_CASE))[0]
    IF xml_name EQ '' THEN RETURN,Time
    lines=FILE_LINES(xml_name)
    strarr=STRARR(lines)
    OPENR,lun,xml_name,/GET_LUN
    READF,lun,strarr
    FREE_LUN,lun
    GetMarkStrline,strarr,'sceneDate',line=startline
    Objstr=strarr[startline]
    StartPos=STRPOS(Objstr,'>')+1
    EndPos=STRPOS(Objstr,'<',/reverse_search)-1
    DateTemp=STRMID(Objstr,StartPos,EndPos-StartPos+1)
    fields=strsplit(DateTemp,'-',/extract,count=count)
    IF count LE 1 THEN BEGIN
        fields=strsplit(DateTemp,' ',/extract,count=count)
        Time=INTARR(6)
        Time[5]=FIX((strsplit(fields[0],'/',/extract))[2])
        Time[4]=FIX((strsplit(fields[0],'/',/extract))[0])
        Time[3]=FIX((strsplit(fields[0],'/',/extract))[1])
        Time[2]=FIX((strsplit(fields[1],':',/extract))[0])
        Time[1]=FIX((strsplit(fields[1],':',/extract))[1])
        Time[0]=FIX((strsplit(fields[1],':',/extract))[2])
        IF N_ELEMENTS(timezone) NE 0 THEN BEGIN
            IF Time[2]+timezone GE 18 THEN BEGIN
                PRINT,'HJ acquire time error! We modified it to new time according to timezone.'
                PRINT,'Original hour: '+STRTRIM(STRING(Time[2]),2)+'---->Modifed hour: '+STRTRIM(STRING(Time[2]-timezone),2)
                Time[2]-=timezone
            ENDIF
        ENDIF
    ENDIF ELSE BEGIN
        fields=strsplit(DateTemp,' ',/extract)
        DateTemp=fields[0]
        DateStr=STRJOIN(strsplit(DateTemp,'-',/extract))
        IF (strsplit((strsplit(fileName,'-',/extract))[4],'.',/extract))[0] EQ DateStr THEN BEGIN
            TimeTemp=fields[1]
            Time=INTARR(6)
            Time[5]=FIX(STRMID(DateStr,0,4))
            Time[4]=FIX(STRMID(DateStr,4,2))
            Time[3]=FIX(STRMID(DateStr,6,2))
            Time[2]=FIX((strsplit(TimeTemp,':',/extract))[0])
            Time[1]=FIX((strsplit(TimeTemp,':',/extract))[1])
            Time[0]=FIX((strsplit(TimeTemp,':',/extract))[2])
        ENDIF
        IF N_ELEMENTS(timezone) NE 0 THEN BEGIN
            IF Time[2]+timezone GE 18 THEN BEGIN
                PRINT,'HJ acquire time error! We modified it to new time according to timezone.'
                PRINT,'Original hour: '+STRTRIM(STRING(Time[2]),2)+'---->Modifed hour: '+STRTRIM(STRING(Time[2]-timezone),2)
                Time[2]-=timezone
            ENDIF
        ENDIF
    ENDELSE
    RETURN,time
END

FUNCTION get_bits,data_type
    CASE data_type OF
        1 : BEGIN
            RETURN,1
        END
        2 : BEGIN
            RETURN,2
        END
        3 : BEGIN
            RETURN,4
        END
        4 : BEGIN
            RETURN,4
        END
        5 : BEGIN
            RETURN,8
        END
        6 : BEGIN
            RETURN,8
        END
        9 : BEGIN
            RETURN,16
        END
        12 : BEGIN
            RETURN,2
        END
        13 : BEGIN
            RETURN,4
        END
        14 : BEGIN
            RETURN,8
        END
        15 : BEGIN
            RETURN,8
        END
    ENDCASE
END

PRO My_Tile_init,fid,TileNum=TileNum,YE=YE,YS=YS,SBand=SBand,TileSize=TileSize,nb=nb
    IF (fid EQ -1) THEN RETURN
    ENVI_FILE_QUERY, fid,nb=fnb,ns=ns,nl=nl,DATA_TYPE=DATA_TYPE
    IF N_ELEMENTS(nb) NE 0 THEN nb=nb ELSE nb=fnb
    PixelByte=get_bits(data_type)
    TotalSpace=0L
    IF N_ELEMENTS(TileSize) NE 0 THEN BEGIN
        TileSize=FLOAT(TileSize)
    ENDIF ELSE BEGIN
        TileSize=100.0
    ENDELSE
    IF KEYWORD_SET(SBand) THEN BEGIN
        TotalSpace=LONG(PixelByte)*(ns)*(nl)
    ENDIF ELSE BEGIN
        TotalSpace=LONG(PixelByte)*ns*nl*nb
    ENDELSE
    TileNum=CEIL(TotalSpace/(TileSize*1024*1024))
    TileLines=CEIL(nl/(TileNum*1.0))
    LastTileLines=nl-TileLines*(TileNum-1)
    YS=INDGEN(TileNum)*TileLines
    YE=(INDGEN(TileNum)+1)*TileLines-1
    YE[TileNum-1]=nl-1
END
PRO AnalyzeXML,xml,L1UL=L1UL,L1UR=L1UR,L1LL=L1LL,L1LR=L1LR,$
        L2UL=L2UL,L2UR=L2UR,L2LL=L2LL,L2LR=L2LR,UTMZone=UTMZone,Samples=Samples,Lines=lines
    COMPILE_OPT idl2
    ;    xml='D:\457-64-20100129-L20000243796-HJ1A-CCD2\243796\HJ1A-CCD2-457-64-20100129-L20000243796.XML'
    IF FILE_SEARCH(xml) EQ '' THEN BEGIN
        PRINT,'Xml cannot be found! Path:',xml
        RETURN
    ENDIF
    lines=FILE_LINES(xml)
    strarr=STRARR(lines)
    OPENR,lun,xml,/GET_LUN
    READF,lun,strarr
    FREE_LUN,lun
    TempArr=strarr[66:73]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    L1UL=[DOUBLE(STRMID(TempArr[0],startPos[0]+1,endpos[0]-startPos[0]-1)),$
        DOUBLE(STRMID(TempArr[1],startPos[1]+1,endpos[1]-startPos[1]-1))]
    L1UR=[DOUBLE(STRMID(TempArr[2],startPos[2]+1,endpos[2]-startPos[2]-1)),$
        DOUBLE(STRMID(TempArr[3],startPos[3]+1,endpos[3]-startPos[3]-1))]
    L1LL=[DOUBLE(STRMID(TempArr[4],startPos[4]+1,endpos[4]-startPos[4]-1)),$
        DOUBLE(STRMID(TempArr[5],startPos[5]+1,endpos[5]-startPos[5]-1))]
    L1LR=[DOUBLE(STRMID(TempArr[6],startPos[6]+1,endpos[6]-startPos[6]-1)),$
        DOUBLE(STRMID(TempArr[7],startPos[7]+1,endpos[7]-startPos[7]-1))]
    TempArr=strarr[49]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    UTMZone=UINT(STRMID(TempArr,startPos+1,endpos-startPos-2))
    TempArr=strarr[74:81]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    L2UL=[DOUBLE(STRMID(TempArr[0],startPos[0]+1,endpos[0]-startPos[0]-1)),$
        DOUBLE(STRMID(TempArr[1],startPos[1]+1,endpos[1]-startPos[1]-1))]
    L2UR=[DOUBLE(STRMID(TempArr[2],startPos[2]+1,endpos[2]-startPos[2]-1)),$
        DOUBLE(STRMID(TempArr[3],startPos[3]+1,endpos[3]-startPos[3]-1))]
    L2LL=[DOUBLE(STRMID(TempArr[4],startPos[4]+1,endpos[4]-startPos[4]-1)),$
        DOUBLE(STRMID(TempArr[5],startPos[5]+1,endpos[5]-startPos[5]-1))]
    L2LR=[DOUBLE(STRMID(TempArr[6],startPos[6]+1,endpos[6]-startPos[6]-1)),$
        DOUBLE(STRMID(TempArr[7],startPos[7]+1,endpos[7]-startPos[7]-1))]
    Dimensions=UINT(ROUND(ABS(L2UL-L2LR)/30+1))
    samples=Dimensions[0]
    Lines=Dimensions[1]
END
PRO GetXMLinfo,xml,L1UL=L1UL,L1UR=L1UR,L1LL=L1LL,L1LR=L1LR,$
        L2UL=L2UL,L2UR=L2UR,L2LL=L2LL,L2LR=L2LR,UTMZone=UTMZone,Samples=Samples,Lines=lines,GainoffStr=GainoffStr
    COMPILE_OPT idl2
    ;    xml='D:\djk\Xml\HJ1A-CCD1-1-64-20101204-L20000438517.XML'
    IF FILE_SEARCH(xml) EQ '' THEN BEGIN
        PRINT,'Xml cannot be found! Path:',xml
        RETURN
    ENDIF
    lines=FILE_LINES(xml)
    strarr=STRARR(lines)
    OPENR,lun,xml,/GET_LUN
    READF,lun,strarr
    FREE_LUN,lun
    GetMarkStrline,strarr,'dataUpperLeftX',line=startline
    TempArr=strarr[startline:startline+7]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    L1UL=[DOUBLE(STRMID(TempArr[0],startPos[0]+1,endpos[0]-startPos[0]-1)),$
        DOUBLE(STRMID(TempArr[1],startPos[1]+1,endpos[1]-startPos[1]-1))]
    L1UR=[DOUBLE(STRMID(TempArr[2],startPos[2]+1,endpos[2]-startPos[2]-1)),$
        DOUBLE(STRMID(TempArr[3],startPos[3]+1,endpos[3]-startPos[3]-1))]
    L1LL=[DOUBLE(STRMID(TempArr[4],startPos[4]+1,endpos[4]-startPos[4]-1)),$
        DOUBLE(STRMID(TempArr[5],startPos[5]+1,endpos[5]-startPos[5]-1))]
    L1LR=[DOUBLE(STRMID(TempArr[6],startPos[6]+1,endpos[6]-startPos[6]-1)),$
        DOUBLE(STRMID(TempArr[7],startPos[7]+1,endpos[7]-startPos[7]-1))]
    GetMarkStrline,strarr,'absCalibType',line=startline
    TempArr=strarr[startline]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    GainoffStr=STRMID(TempArr,startPos+1,endpos-startPos-1)
    GetMarkStrline,strarr,'zone',line=startline
    TempArr=strarr[startline]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    UTMZone=UINT(STRMID(TempArr,startPos+1,endpos-startPos-2))
    GetMarkStrline,strarr,'productUpperLeftX',line=startline
    TempArr=strarr[startline:startline+7]
    startPos=STRPOS(TempArr,'>')
    endpos=STRPOS(TempArr,'<',/REVERSE_SEARCH)
    L2UL=[DOUBLE(STRMID(TempArr[0],startPos[0]+1,endpos[0]-startPos[0]-1)),$
        DOUBLE(STRMID(TempArr[1],startPos[1]+1,endpos[1]-startPos[1]-1))]
    L2UR=[DOUBLE(STRMID(TempArr[2],startPos[2]+1,endpos[2]-startPos[2]-1)),$
        DOUBLE(STRMID(TempArr[3],startPos[3]+1,endpos[3]-startPos[3]-1))]
    L2LL=[DOUBLE(STRMID(TempArr[4],startPos[4]+1,endpos[4]-startPos[4]-1)),$
        DOUBLE(STRMID(TempArr[5],startPos[5]+1,endpos[5]-startPos[5]-1))]
    L2LR=[DOUBLE(STRMID(TempArr[6],startPos[6]+1,endpos[6]-startPos[6]-1)),$
        DOUBLE(STRMID(TempArr[7],startPos[7]+1,endpos[7]-startPos[7]-1))]
    Dimensions=UINT(ROUND(ABS(L2UL-L2LR)/30+1))
    samples=Dimensions[0]
    Lines=Dimensions[1]
;    GetMarkStrline,strarr,'absCalibType',line=line
END
PRO GetMarkStrline,strarr,MarkStr,line=line
    nlines=N_ELEMENTS(strarr)
    FOR i=0,nlines-1 DO BEGIN
        pos=STRPOS(Strarr[i],MarkStr)
        IF pos NE -1 THEN BREAK
    ENDFOR
    IF pos NE -1 THEN line=i ELSE line=-1
END
; TransHJRef,ZeroBFile,ParaDIR,OutputDIR,RefFile=RefFile
PRO GetXmlValue,xmlFile,TagName,Value=Value,errorfile=errorFile,flag=flag
    ;    xmlFile='D:\djk\Xml\HJ1A-CCD1-1-64-20100406-L20000279007.XML'
    ;    TagName='zone'
    flag=0
    oDocument = OBJ_NEW('IDLffXMLDOMDocument', FileName = xmlFile)
    oPlugin = oDocument->GetFirstChild()
    oNodeList = oPlugin->GetElementsByTagName(TagName)
    nodeLen = oNodeList->Getlength()
    IF nodeLen LT 1 THEN BEGIN
        IF N_ELEMENTS(errorfile) NE 0 THEN BEGIN
            OPENW,lun,errorFile,/GET_LUN,/APPEND
            PRINTF,lun,'Tag cannot find in xml file! TagName: ',TagName
            PRINTF,lun,'XML file: ',xmlfile
            FREE_LUN,lun
        ENDIF
        RETURN
    ENDIF
    nodeObj = oNodeList->Item(0)
    oTextObj = nodeObj->GetFirstChild()
    IF OBJ_VALID(oTextObj) THEN BEGIN
        Value= oTextObj->GetNodeValue()
        flag=1
    ENDIF ELSE BEGIN
        Value=''
    ENDELSE
END

PRO GetGainoff,xmlfile,Gain=Gain,offset=offset,logfile=logfile,flag=oflag
    oflag=0
    ;    xmlFile='D:\djk\Xml\HJ1A-CCD1-1-64-20100406-L20000279007.XML'
    ;        TagName='zone'
    xmlfile=(FILE_SEARCH(xmlFile,/FOLD_CASE))[0]
    IF xmlfile EQ '' THEN BEGIN
        IF N_ELEMENTS(logfile) NE 0 THEN BEGIN
            OPENW,lun,LogFile,/get_lun,/APPEND
            PRINTF,lun,'**********************************************************'
            PRINTF,lun,'Cannot find the xml file:'+xmlfile
            PRINTF,lun,'Get gain offset failed!'
            FREE_LUN,lun
            RETURN
        ENDIF ELSE BEGIN
            PRINT,'Cannot find the xml file:'+xmlfile
            RETURN
        ENDELSE
    ENDIF
    GetXmlValue,xmlFile,'absCalibType',Value=XmlArr,errorfile=logfile,flag=flag
    IF flag EQ 0 THEN RETURN
    pos=STRPOS(XmlArr,'B1:')
    XmlArr=STRMID(XmlArr,pos)
    Gain=FLTARR(4) & offset=FLTARR(4)
    CalStr=strsplit(XmlArr,';',count=count,/EXTRACT)
    IF count NE 4 THEN BEGIN
        FOR i=0,3 DO BEGIN
            pos=STRPOS(CalStr,':g')
            StartPos=pos+2
            EndPos=STRPOS(CalStr,',')
            Gain[i]=FLOAT(STRMID(CalStr,StartPos,EndPos-StartPos+1))
            CalStr=STRMID(CalStr,EndPos+1)
            pos=STRPOS(CalStr,'L0')
            StartPos=pos+2
            EndPos=STRPOS(CalStr,',')
            IF EndPos NE -1 THEN BEGIN
                offset[i]=FLOAT(STRMID(CalStr,StartPos,EndPos-StartPos+1))
                CalStr=STRMID(CalStr,EndPos+1)
            ENDIF ELSE BEGIN
                offset[i]=FLOAT(STRMID(CalStr,StartPos))
            ENDELSE
        ENDFOR
    ENDIF ELSE BEGIN
        FOR i=0,3 DO BEGIN
            GStr=(strsplit(CalStr[i],',',/extract))[0]
            L0Str=(strsplit(CalStr[i],',',/extract))[1]
            Gain[i]=FLOAT(STRMID(Gstr,STRPOS(Gstr,'g')+1))
            Offset[i]=FLOAT(STRMID(L0Str,STRPOS(L0Str,'L0')+2))
        ENDFOR
    ENDELSE
    oflag=1
END
FUNCTION GetFileMainName,File,extendName=extendName
    FileName=FILE_BASENAME(File)
    fields=strsplit(FileName,'.',count=count,/EXTRACT)
    IF count GT 1 THEN BEGIN
        IF count EQ 2 THEN FileMainName=fields[0] ELSE BEGIN
            FileMainName=STRJOIN(fields[0:count-2],'.')
        ENDELSE
        extendName='.'+fields[count-1]
    ENDIF ELSE BEGIN
        FileMainName=FileName
        extendName=''
    ENDELSE
    RETURN,FileMainName
END
PRO compute_NDVI,inputFile,outputDIR=outputDIR,logFile=logFile
    inputDIR=FILE_DIRNAME(inputFile,/MARK_DIRECTORY)
    FileMainName=GetFileMainName(inputFile,extendName=extendName)
    IF N_ELEMENTS(outputDIR) EQ 0 THEN outputDIR=inputDIR
    outTempFile=outputDIR+FileMainName+'-NDVI.temp'
    outFile=outputDIR+FileMainName+'-NDVI.tif'
    IF ~QUERY_TIFF(inputFile,geotiff=geotiff,info) THEN BEGIN
        PRINT,'Tiff File invalid. NDVI cannot be computed! File: '+inputFile
        IF N_ELEMENTS(logFile) NE 0THEN BEGIN
            OPENW,lun,logFile,/GET_LUN,/APPEND
            PRINTF,lun,'Tiff File invalid. NDVI cannot be computed! File: '+inputFile
            PRINTF,lun,SYSTIME()
            PRINTF,lun,'***************************************************************'
            FREE_LUN,lun
        ENDIF
    ENDIF
    Tiff_tile_init,inputFile,TileNum=TileNum,YE=YE,YS=YS,SBand=SBand
    OPENW,lun,outTempFile,/GET_LUN,/APPEND
    FOR i=0,TileNum-1 DO BEGIN
        data=READ_TIFF(inputFile,SUB_RECT=[0,YS[i],info.dimensions[0],YE[i]-YS[i]+1])
        NDVI=(data[3,*,*]-data[2,*,*])/FLOAT(data[3,*,*]+data[2,*,*])
        WRITEU,lun,NDVI
    ENDFOR
    data=0 & NDVI=0
    FREE_LUN,lun
    ENVI_OPEN_FILE,inputFile,R_FID=fid
    ENVI_FILE_QUERY,fid,ns=ns, nl=nl
    nb=1
    map_info = ENVI_GET_MAP_INFO(fid=fid)
    ENVI_SETUP_HEAD, fname=outTempFile, ns=ns, nl=nl, nb=nb, $
        data_type=4, offset=0, interleave=1, $
        MAP_INFO=map_info, /write, /open
    ENVI_OPEN_FILE,outTempFile,r_fid=tmpfid
    ENVI_FILE_QUERY,tmpfid,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=tmpfid, OUT_NAME=outFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG,id=tmpfid,/REMOVE,/DELETE
END

PRO Tiff_tile_init,File,TileNum=TileNum,YE=YE,YS=YS,SBand=SBand
    IF ~QUERY_TIFF(File,geotiff=geotiff,info) THEN  RETURN
    nb=info.CHANNELS
    ns=info.dimensions[0]
    nl=info.dimensions[1]
    MaxMemory=FLOAT(100.0)
    PixelByte=info.BITS_PER_SAMPLE
    TotalSpace=0L
    IF KEYWORD_SET(SBand) THEN BEGIN
        TotalSpace=DOUBLE(PixelByte)*(ns)*(nl)
    ENDIF ELSE BEGIN
        TotalSpace=DOUBLE(PixelByte)*ns*nl*nb
    ENDELSE
    TileNum=CEIL(TotalSpace/(MaxMemory*1024*1024))
    TileLines=CEIL(nl/(TileNum*1.0))
    YS=INDGEN(TileNum)*TileLines
    index=WHERE(YS LT nl,count)
    TileNum=count
    YS=YS[index]
    YE=YS+TileLines-1
    YE[TileNum-1]=nl-1
END

PRO DO_6S_AtmCor,HJFile,XMLDIR,TaoDIR,DEMDIR,outDIR,angleDIR,DEMMosaicDIR,LookupTableDIR,TAOZERO=TAOZERO,flag=flag,minZero=minZero
    COMPILE_OPT idl2
    flag=0
    ;    HJFile='E:\test\6s\HJTest\HJ1A-CCD1-455-56-20100607-L20000320106.tif'
    ;    XMLDIR='E:\test\6s\HJTest\XML\'
    ;    TaoDIR='E:\test\6s\HJTest\Tao\Mosaic\'
    ;    DEMDIR='D:\常用资料\ChinaDEM\'
    ;    outDIR='E:\test\6s\HJTest\Result\'
    ;    AngleDIR='E:\test\6s\HJTest\Angle\'
    ;    LookupTableDIR='E:\test\6s\LookupTable\'
    FILE_MKDIR,outDIR,angleDIR
    batch_errfile=outDIR+'BatchError.txt'
    FileMainName=FILE_BASENAME(HJFile,'.tif',/FOLD_CASE)
    xmlFile=XMLDIR+FileMainName+'.XML'
    AtCTempFile=outDIR+FileMainName+'AtCTemp'
    ;    ResizeFile=outDIR+FileMainName+'Resize'
    outFile=outDIR+FileMainName+'.tif'
    ;    ENVI,/RESTORE_BASE_SAVE_FILES
    ;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    IF QUERY_TIFF(outFile) THEN BEGIN
        flag=1
        RETURN
    ENDIF
    ;找TAO文件
    Fields=strsplit(FileMainName,'-',/EXTRACT)
    ;    datestr=Fields[4]
    ;    juldateStr=date2juldate(datestr)
    TaoFile=TaoDIR+Fields[4]+'.tif'
    ;    TaoFile=TaoDIR+'MOD04_L2.A'+juldateStr+'.tif'
    TaoFlag=1
    IF ~QUERY_TIFF(TaoFile) THEN BEGIN
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,lun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,lun,'Tao file cannot be found! Tao file name:',TaoFile
            FREE_LUN,lun
        ENDIF
        PRINT,'Tao file cannot be found! Tao file name:',TaoFile
        IF ~KEYWORD_SET(TAOZERO) THEN BEGIN
            RETURN
        ENDIF ELSE BEGIN
            TaoFlag=0
        ENDELSE
    ENDIF
    ;计算DEM
    GetDEMFile,HJFile,DEMDIR,DEMMosaicDIR,batch_errfile=batch_errfile,DEMFile=DEMFile,flag=flag
    IF ~flag THEN BEGIN
        RETURN
    ENDIF
    ;    DEMFile='E:\test\6s\HJTest\Angle\HJ1A-CCD1-455-56-20100607-DEM.tif'
    ;计算卫星天顶角
    GetSatZenith,xmlFile,AngleDIR,SatZFile=SatZFile,flag=flag
    IF ~flag THEN BEGIN
        RETURN
    ENDIF
    ;计算太阳天顶角
    GetSolZenith,HJFile,XMLDIR,AngleDIR,batch_errfile=batch_errfile,SolZFile=SolZFile
    ;影像分块处理，获取两天顶角，DEM，TAO，从而查找相应的查找表（如果全为背景像素则不处理）
    ENVI_OPEN_FILE,HJFile,R_FID=fid
    IF fid EQ -1 THEN BEGIN
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,lun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,lun,'File can not be read! File name:',Geooutfile
            PRINTF,lun,'************************************************************'
            FREE_LUN,lun
        ENDIF
        RETURN
    ENDIF
    ENVI_FILE_QUERY,fid,nb=nb,ns=ns,nl=nl,DATA_TYPE=DATA_TYPE,dims=dims,INTERLEAVE=interleave
    map_info = ENVI_GET_MAP_INFO(fid=fid)
    UTMProj=ENVI_GET_PROJECTION(fid=fid)
    Y0 = map_info.MC[3] & X0 = map_info.MC[2]
    pixel = map_info.PS[0:1]
    ZonePixel=[50,50]
    XBlocks=CEIL(ns*1.0/ZonePixel[0])
    YBlocks=CEIL(nl*1.0/ZonePixel[1])
    XS=INDGEN(XBlocks)*ZonePixel[0] & XE=INDGEN(XBlocks)*ZonePixel[0]+ZonePixel[0]-1 & XE[XBlocks-1]=ns-1
    YS=INDGEN(YBlocks)*ZonePixel[1] & YE=INDGEN(YBlocks)*ZonePixel[1]+ZonePixel[1]-1 & YE[YBlocks-1]=nl-1
    OPENW,lun,AtCTempFile,/GET_LUN
    FOR Block_Y=0,YBlocks-1 DO BEGIN
        YBlockLines=YE[Block_Y]-YS[Block_Y]+1
        Y_UL=Y0-YS[Block_Y]*pixel[1]
        Y_LR=Y0-YE[Block_Y]*pixel[1]
        result=MAKE_ARRAY(nb,ns,YE[Block_Y]-YS[Block_Y]+1,type=DATA_TYPE,value=0)
        FOR Block_X=0,XBlocks-1 DO BEGIN
            X_UL=X0+XS[Block_X]*pixel[0]
            X_LR=X0+XE[Block_X]*pixel[0]
            CenterX=(X_UL+X_LR)/2. & CenterY=(Y_UL+Y_LR)/2.
            data=MAKE_ARRAY(nb,XE[Block_X]+1-XS[Block_X],YE[Block_Y]-YS[Block_Y]+1,type=DATA_TYPE,value=0)
            FOR i=0,nb-1 DO BEGIN
                temp=ENVI_GET_DATA(fid=fid,pos=i,dims=[-1,XS[Block_X],XE[Block_X],YS[Block_Y],YE[Block_Y]])
                data[i,*,*]=temp
            ENDFOR
            temp=0
            ;对DATA的情况进行判断
            IF TOTAL(data) EQ 0 THEN CONTINUE;如果全为零值则输出零值
            ParamX=CenterX & ParamY=CenterY
            IF TOTAL(data[*,(XE[Block_X]+1-XS[Block_X])/2,(YE[Block_Y]-YS[Block_Y]+1)/2]) EQ 0 THEN BEGIN
                ;如果存在有效像素则：先判断中心像素是否为有效值，若是则取其对应参数，否则取有效角点的大气参数
                IF TOTAL(data[*,0,0]) NE 0 THEN BEGIN
                    ParamX=X_UL & ParamY=Y_UL
                    GOTO,GetCor
                ENDIF
                IF TOTAL(data[*,(XE[Block_X]+1-XS[Block_X])-1,(YE[Block_Y]-YS[Block_Y]+1)-1]) NE 0 THEN BEGIN
                    ParamX=X_LR & ParamY=Y_LR
                    GOTO,GetCor
                ENDIF
                IF TOTAL(data[*,(XE[Block_X]+1-XS[Block_X])-1,0]) NE 0 THEN BEGIN
                    ParamX=X_LR & ParamY=Y_UL
                    GOTO,GetCor
                ENDIF
                IF TOTAL(data[*,0,(YE[Block_Y]-YS[Block_Y]+1)-1]) NE 0 THEN BEGIN
                    ParamX=X_UL & ParamY=Y_LR
                    GOTO,GetCor
                ENDIF
            ENDIF
            GetCor:GetAtCorCoeff,HJFile,[ParamX,ParamY],SolZFile,SatZFile,DEMFile,TaoFile,LookupTableDIR,Coeff=Coeff,TaoFlag=TaoFlag
            ;记录背景像素的INDEX
            index=WHERE(data EQ 0,count)
            FOR i=0,nb-1 DO BEGIN
                ;                data=ENVI_GET_DATA(fid=fid,pos=i,dims=[-1,XS[Block_X],XE[Block_X],YS[Block_Y],YE[Block_Y]])
                data[i,*,*]=ROUND((data[i,*,*]-Coeff[i,0]*10000)/(Coeff[i,1]*10000+data[i,*,*]*Coeff[i,2]-Coeff[i,2]*Coeff[i,0]*10000)*10000)
            ENDFOR
            IF count NE 0 THEN BEGIN
                data[index]=0;还原背景值
            ENDIF ELSE BEGIN
            ;                data=data
            ENDELSE
            result[*,XS[Block_X]:XE[Block_X],*]=data
        ENDFOR
        IF KEYWORD_SET(minZero) THEN BEGIN
            result>=0
        ENDIF
        WRITEU,lun,result
    ENDFOR
    result=1
    FREE_LUN,lun
    ENVI_SETUP_HEAD, fname=AtCTempFile, ns=ns, nl=nl, nb=nb, $
        data_type=DATA_TYPE, offset=0, interleave=2, $
        MAP_INFO=map_info, /write, /open
    ;像素大小如果像素大小不为标准的30*30，则RESIZE+
    ;    IF TOTAL(pixel EQ 30) NE 2 THEN BEGIN
    ;        ENVI_OPEN_FILE,AtCTempFile,R_FID=AtCFId
    ;        IF AtCFid EQ -1 THEN RETURN
    ;        ENVI_FILE_QUERY,AtCFid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ;        oriPS=map_info.ps[0:1]
    ;        rfact=[30/oriPS[0],30/oriPS[1]]
    ;        ENVI_DOIT, 'resize_doit', $
    ;            fid=AtCFid, pos=INDGEN(nb), dims=dims, $
    ;            interp=0, rfact=rfact, $
    ;            out_name=ResizeFile, r_fid=Resizefid
    ;        ENVI_FILE_MNG, id=AtCFId,/DELETE,/REMOVE
    ;    ENDIF ELSE BEGIN
    ENVI_OPEN_FILE,AtCTempFile,R_FID=Resizefid
    ;    ENDELSE
    ENVI_FILE_QUERY,Resizefid,nb=nb,DATA_TYPE=out_dt,dims=dims
    ENVI_OUTPUT_TO_EXTERNAL_FORMAT, DIMS=dims $
        , FID=Resizefid, OUT_NAME=outFile,POS=INDGEN(nb), /TIFF
    ENVI_FILE_MNG, id=Resizefid,/DELETE,/REMOVE
    FILE_DELETE,SolZFile,SatZFile,DEMFile,/ALLOW_NONEXISTENT,/QUIET
    flag=1
;    ENVI_BATCH_EXIT
END

FUNCTION date2juldate,dateStr
    ;datestr='20100121'
    year=FIX(STRMID(dateStr,0,4))
    month=FIX(STRMID(dateStr,4,2))
    day=FIX(STRMID(dateStr,6,2))
    juldays=julday(month,day,year)-julday(12,31,year-1)
    IF juldays LT 10 THEN daystr='00'+STRTRIM(STRING(juldays),2) ELSE BEGIN
        IF juldays LT 100 THEN daystr='0'+STRTRIM(STRING(juldays),2) ELSE daystr=STRTRIM(STRING(juldays),2)
    ENDELSE
    RETURN,STRMID(dateStr,0,4)+daystr
END
PRO GetAtCorCoeff,HJFile,Coordinate,SolZFile,SatZFile,DEM,TaoFile,LookupTableDIR,Coeff=Coeff,TaoFlag=TaoFlag
    COMPILE_OPT idl2
    ENVI_OPEN_FILE,HJFile,R_FID=HJFid
    IF HJFId EQ -1 THEN RETURN
    map_info = ENVI_GET_MAP_INFO(fid=HJFid)
    HJProj=ENVI_GET_PROJECTION(fid=HJFid)
    SolZ=GetImageValue(SolZFile,Coordinate,CoordinateProj=HJProj,AvoidBGRange=50)
    SatZ=GetImageValue(SatZFile,Coordinate,CoordinateProj=HJProj,AvoidBGRange=50)
    SolZ=(ROUND(SolZ/2)*2>0)<60
    SatZ=(ROUND(SatZ/2)*2>0)<50
    Height=GetImageValue(DEM,Coordinate,CoordinateProj=HJProj,AvoidBGRange=50)
    ;    Height=(ROUND(Height/2000.)*2.<8.)>0.
    HeightNum=ROUND(Height/2000.)
    IF TaoFlag EQ 0 THEN BEGIN
        Tao=0.01
    ENDIF ELSE BEGIN
        Tao=GetImageValue(TaoFile,Coordinate,CoordinateProj=HJProj,AvoidBGRange=5);获取像元TAO
        ;        Tao=GetTao(taofile,HJfile)
        Tao=(ROUND(Tao/0.1)*0.1<1.0)>0.01
    ENDELSE
    SolA=0
    SatA=0
    nband=4
    HJFileMainName=FILE_BASENAME(HJFile,'.TIF',/FOLD_CASE)
    Fields=strsplit(HJFileMainName,'-',/EXTRACT)
    SensorName=STRUPCASE(STRJOIN(Fields[0:1]))
    Month=UINT(STRMID(Fields[4],4,2))
    IF month GE 3 && month LE 8 THEN month=7 ELSE month=1
    Coeff=FLTARR(4,3)
    FOR iBand=0,nband-1 DO BEGIN
        ;        TableDIR=LookupTableDIR+SensorName+PATH_SEP()+ $
        ;            'Band'+STRTRIM(STRING(iBand),2)+PATH_SEP()+ $
        ;            'SoZ'+STRTRIM(STRING(SolZ),2)+PATH_SEP()+ $
        ;            'SoA'+STRTRIM(STRING(SolA),2)+PATH_SEP()+ $
        ;            'SaZ'+STRTRIM(STRING(SatZ),2)+PATH_SEP()+ $
        ;            'SaA'+STRTRIM(STRING(SatA),2)+PATH_SEP()+ $
        ;            'Mon'+STRTRIM(STRING(month),2)+PATH_SEP()+ $
        ;            'Tao'+STRMID(STRTRIM(STRING(tao),2),0,4)+PATH_SEP()
        ;        TableFile=TableDIR+'Table.txt'
        TableDIR=LookupTableDIR+SensorName+PATH_SEP()+ $
            'Band'+STRTRIM(STRING(iBand),2)+PATH_SEP()
        TableFile=TableDIR+'SoZ'+STRTRIM(STRING(SolZ),2)+ $
            'SoA'+STRTRIM(STRING(SolA),2)+ $
            'SaZ'+STRTRIM(STRING(SatZ),2)+ $
            'SaA'+STRTRIM(STRING(SatA),2)+ $
            'Mon'+STRTRIM(STRING(month),2)+ $
            'Tao'+STRMID(STRTRIM(STRING(tao),2),0,4)+'Table.txt'
        FileLines=FILE_LINES(TableFile)
        head=STRARR(1)
        Data=FLTARR(4,FileLines-1)
        OPENR,lun,TableFile,/GET_LUN
        READF,lun,head
        READF,lun,Data
        FREE_LUN,lun
        Coeff[iBand,*]=(Data[*,HeightNum])[1:3]
    ENDFOR
END
;FUNCTION GetTao,Taofile,HJfile
;    envi,/RESTORE_BASE_SAVE_FILES
;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
;    HJfile='E:\test\6s\HJTest\inputFile\HJ1A-CCD2-2-76-20100102-L20000229821.tif'
;    Taofile='E:\test\6s\HJTest\Tao\mod04_l2.a2010002.tif'
;    COMPILE_OPT idl2
;    ENVI_OPEN_FILE,HJFile,R_FID=HJFid
;    IF HJFId EQ -1 THEN RETURN
;    HJmap_info = ENVI_GET_MAP_INFO(fid=HJFid)
;    HJProj=ENVI_GET_PROJECTION(fid=HJFid)
;;        Y0 = map_info.MC[3] & X0 = map_info.MC[2]
;;    pixel = map_info.PS[0:1]
;    HJUL_X=HJmap_info.MC[2] & HJUL_Y=HJmap_info.MC[3]
;    ENVI_OPEN_FILE,file,R_FID=fid
;    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,DATA_TYPE=data_type
;    FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
;        IF pos[i] GE nb THEN RETURN,0
;    ENDFOR
;    ;转换坐标信息
;    FileProj=ENVI_GET_PROJECTION(fid=fid)
;    IF N_ELEMENTS(CoordinateProj) THEN BEGIN
;        ENVI_CONVERT_PROJECTION_COORDINATES, coordinate[0], coordinate[1], CoordinateProj,$
;            oXmap, oYmap, FileProj
;    ENDIF ELSE BEGIN
;        oXmap=coordinate[0] & oYmap=coordinate[1]
;    ENDELSE
;    ENVI_CONVERT_FILE_COORDINATES,fid,xf,yf,oXmap,oYmap
;    xf=ROUND(xf)
;    yf=ROUND(yf)
;END
FUNCTION GetImageValue,File,coordinate,pos=pos,CoordinateProj=CoordinateProj,AvoidBGRange=AvoidBGRange,flag=flag,BGValue=BGValue
    COMPILE_OPT idl2
    ;    File='E:\test\6s\HJTest\HJ1A-CCD1-455-56-20100607-L20000320106.tif'
    ;    File='E:\test\6s\HJTest\HJ1A-CCD1-455-56-20100607-L20000320106-Zenith'
    ;    flag=0
    ;    coordinate=[293361,5448582]
    ;    AvoidBGRange=50
    ;    BGValue=0;backgroundvalue=0
    IF N_ELEMENTS(BGValue) EQ 0 THEN BGValue=0
    ;    pos=INDGEN(4)
    IF N_ELEMENTS(pos) EQ 0 THEN pos=0
    ;判断POS的合理性
    ;    ENVI,/RESTORE_BASE_SAVE_FILES
    ;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW,LOG_FILE='E:\123.txt'
    ENVI_OPEN_FILE,file,R_FID=fid
    ENVI_FILE_QUERY,fid,ns=ns,nl=nl,nb=nb,DATA_TYPE=data_type
    FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
        IF pos[i] GE nb THEN RETURN,0
    ENDFOR
    ;转换坐标信息
    FileProj=ENVI_GET_PROJECTION(fid=fid)
    IF N_ELEMENTS(CoordinateProj) NE 0 THEN BEGIN
        ENVI_CONVERT_PROJECTION_COORDINATES, coordinate[0], coordinate[1], CoordinateProj,$
            oXmap, oYmap, FileProj
    ENDIF ELSE BEGIN
        oXmap=coordinate[0] & oYmap=coordinate[1]
    ENDELSE
    ENVI_CONVERT_FILE_COORDINATES,fid,xf,yf,oXmap,oYmap
    xf=ROUND(xf)
    yf=ROUND(yf)
    value=MAKE_ARRAY(N_ELEMENTS(pos),type=data_type)
    FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
        ;        value=ENVI_GET_DATA(dims=[-1,3488,3492,-8,8],fid=fid,pos=[0])
        value[i]=ENVI_GET_DATA(dims=[-1,xf,xf,yf,yf],fid=fid,pos=pos[i])
    ENDFOR
    IF N_ELEMENTS(AvoidBGRange) EQ 0 THEN BEGIN
        RETURN,value
    ENDIF ELSE BEGIN
        IF TOTAL(value) NE nb*BGValue THEN BEGIN
            ;            flag=1
            RETURN,value
        ENDIF
        ;背景元素取附近的非零值
        AvoidBGRange=ABS(AvoidBGRange)
        data=MAKE_ARRAY([N_ELEMENTS(pos),2*AvoidBGRange+1,2*AvoidBGRange+1],type=data_type)
        FOR i=0,N_ELEMENTS(pos)-1 DO BEGIN
            ;对异常进行处理
            ULoffset=-[xf-AvoidBGRange-((xf-AvoidBGRange)>0),yf-AvoidBGRange-((yf-AvoidBGRange)>0)]
            LRoffset=[xf+AvoidBGRange-((xf+AvoidBGRange)<(ns-1)),yf+AvoidBGRange-((yf+AvoidBGRange)<(nl-1))]
            ;读数据
            ;            IF LRoffset[0] LT ULoffset[0] || LRoffset[1] LT ULoffset[1]  THEN BREAK
            IF ULoffset[0] GT AvoidBGRange*2 || ULoffset[1] GT AvoidBGRange*2  $
                || LRoffset[0] GT AvoidBGRange*2 || LRoffset[1] GT AvoidBGRange*2 THEN BREAK
            temp=ENVI_GET_DATA(dims=[-1,(xf-AvoidBGRange)>0,(xf+AvoidBGRange)<(ns-1), $
                (yf-AvoidBGRange)>0,(yf+AvoidBGRange)<(nl-1)],fid=fid,pos=pos[i])
            data[i,ULoffset[0]:2*AvoidBGRange-LRoffset[0],$
                ULoffset[1]:2*AvoidBGRange-LRoffset[1]]=temp
        ENDFOR
        value=GetCentNoneBG(data)
        ;        flag=1
        RETURN,value
    ENDELSE
;    ENVI_BATCH_EXIT
END

FUNCTION GetCentNoneBG,Array,BGvalue=BGvalue
    ;取距Array中心最近的非背景像素，同等距离取最大值
    COMPILE_OPT idl2
    ;    array=INTARR(1,5,5) & array[*,1,1]=3 & BGvalue=0
    N_Dim=SIZE(Array,/n_DIMENSIONS)
    IF N_ELEMENTS(bgvalue) EQ 0 THEN BGvalue=0
    CASE (N_Dim) OF
        1: BEGIN
            RETURN,Array
        END
        2: BEGIN
            Dim=SIZE(Array,/DIMENSIONS)
            CentIndex=[(Dim[N_Dim-2]-1)/2,(Dim[N_Dim-1]-1)/2]
            HalfWidth=[(Dim[N_Dim-2]-1)/2,(Dim[N_Dim-1]-1)/2]
            FOR i=0,MIN(HalfWidth) DO BEGIN
                value=GetArrayMaxValue(Array[(Dim[N_Dim-2]-1)/2-i:(Dim[N_Dim-2]-1)/2+i,(Dim[N_Dim-1]-1)/2-i:(Dim[N_Dim-1]-1)/2+i],2)
                IF TOTAL(value) NE BGValue THEN RETURN,value
            ENDFOR
        END
        3: BEGIN
            Dim=SIZE(Array,/DIMENSIONS)
            CentIndex=[(Dim[N_Dim-2]-1)/2,(Dim[N_Dim-1]-1)/2]
            HalfWidth=[(Dim[N_Dim-2]-1)/2,(Dim[N_Dim-1]-1)/2]
            FOR i=0,MIN(HalfWidth) DO BEGIN
                value=GetArrayMaxValue(Array[*,((Dim[N_Dim-2]-1)/2-i):((Dim[N_Dim-2]-1)/2+i),((Dim[N_Dim-1]-1)/2-i):((Dim[N_Dim-1]-1)/2+i)],3)
                IF TOTAL(value) NE BGValue THEN RETURN,value
            ENDFOR
        END
        ELSE: BEGIN
            RETURN,BGvalue
        END
    ENDCASE
END

FUNCTION GetArrayMaxValue,Array,N_DIM
    COMPILE_OPT idl2
    ;    N_DIM=SIZE(Array,/n_DIMENSIONS)
    TempArray=Array
    IF n_dim EQ 1 THEN RETURN,TempArray
    IF n_dim EQ 2 THEN RETURN,MAX(TempArray) ELSE BEGIN
        tempN_dim=SIZE(Array,/n_DIMENSIONS)
        IF tempN_dim EQ 1 THEN RETURN,TempArray
        sum=TOTAL(TempArray,1)
        maxindex=(reverse(SORT(sum)))[0]
        Dim=SIZE(TempArray,/DIMENSIONS)
        line=maxindex/Dim[2]
        sample=maxindex-line*Dim[2]
        RETURN,TempArray[*,Sample,line]
    ENDELSE
END

PRO GetTaoFile,HJFile,MOD04DIR
    ;    HJFile='E:\test\6s\HJTest\HJ1A-CCD1-455-56-20100607-L20000320106.tif'
    ;    MOD04DIR='E:\test\6s\HJTest\MOD04\'
    HJFileMainName=FILE_BASENAME(hjfile,'.tif')
    Fields=strsplit(HJFileMainName,'-',/EXTRACT)
    DateStr=Fields[4]
    Year=FIX(STRMID(DateStr,0,4))
    Mon=FIX(STRMID(DateStr,4,2))
    Day=FIX(STRMID(DateStr,6,2))
    Juldays=julday(Mon,day,year)-julday(12,31,year-1)
    HDFFileStr=MOD04DIR+'MOD04_L2.A'+STRTRIM(STRING(Year),2)+STRTRIM(STRING(Juldays),2)+'.*.hdf'
    MOD04HDFFiles=FILE_SEARCH(HDFFileStr,/FOLD_CASE,count=hdfcount)
    FOR i=0,hdfcount-1 DO BEGIN
        HDFFile=MOD04HDFFiles[i]
        HdfID=HDF_SD_START(HDFFile)
        AttrIndex = HDF_SD_ATTRFIND(HdfID, 'CoreMetadata.0')
        HDF_SD_ATTRINFO, HdfID, AttrIndex , COUNT=count, DATA=data $
            , HDF_TYPE=hdftype, NAME=name, TYPE=type
        HDF_SD_END, HdfID
        array=strsplit(DATA,STRING(10B),/extract,count=count)
    ENDFOR
END

PRO GetSolZenith,inputFile,XMLDIR,outDIR,batch_errfile=batch_errfile,SolZFile=SunOutputFile
    COMPILE_OPT idl2
    ;    inputFile='E:\test\6s\HJTest\HJ1A-CCD1-455-56-20100607-L20000320106.tif'
    ;    XMLDIR='E:\test\6s\HJTest\XML\'
    ;    outDIR='E:\test\6s\HJTest\'
    FileMainName=FILE_BASENAME(inputFile,'.tif',/FOLD_CASE)
    ;    ENVI,/RESTORE_BASE_SAVE_FILES
    ;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
    ;    TimeStr=(strsplit(FileMainName,'-',/extract,count=count))[count-1]
    ;    DateStr=STRMID(TimeStr,4,4)
    SunOutputFile=outDIR+FileMainName+'-SunZenith'
    timezone=8
    xml=XMLDIR+FileMainName+'.XML'
    time=get_time1(inputFile,xmlDIR,timezone=timezone)
    ENVI_OPEN_FILE,SunOutputFile,R_FID=outfid
    IF outfid NE -1 THEN RETURN
    ENVI_OPEN_FILE,inputFile,R_FID=fid
    IF fid EQ -1 THEN BEGIN
        IF N_ELEMENTS(batch_errfile) NE 0 THEN BEGIN
            OPENW,lun,batch_errfile,/GET_LUN,/APPEND
            PRINTF,lun,'File can not be read! File name:',inputFile
            PRINTF,lun,'************************************************************'
            FREE_LUN,lun
        ENDIF
        RETURN
    ENDIF
    ENVI_FILE_QUERY,fid,nb=nb,ns=ns,nl=nl,DATA_TYPE=out_dt,dims=dims,INTERLEAVE=interleave
    map_info = ENVI_GET_MAP_INFO(fid=fid)
    IF SIZE(Time,/n_elements) LE 1 THEN BEGIN
        PRINT,'Get xml time error!'
        RETURN
    ENDIF
    AnalyzeXML,xml,UTMZone=UTMZone
    UTMProj=MAP_PROJ_INIT('UTM', ELLIPSOID='WGS 84',/GCTP,zone=UTMZone)
    !map=UTMProj
    Year=time[5] & Month=time[4] & Day=time[3]
    hour=time[2] & minute=time[1] & second=time[0]
    Y0 = map_info.MC[3] & X0 = map_info.MC[2]
    pixel = map_info.PS[0:1]
    ZonePixel=[50,50]
    XBlocks=CEIL(ns*1.0/ZonePixel[0])
    YBlocks=CEIL(nl*1.0/ZonePixel[1])
    XS=INDGEN(XBlocks)*ZonePixel[0] & XE=INDGEN(XBlocks)*ZonePixel[0]+ZonePixel[0]-1 & XE[XBlocks-1]=ns-1
    YS=INDGEN(YBlocks)*ZonePixel[1] & YE=INDGEN(YBlocks)*ZonePixel[1]+ZonePixel[1]-1 & YE[YBlocks-1]=nl-1
    OPENW,lun,SunOutputFile,/GET_LUN
    FOR Block_Y=0,YBlocks-1 DO BEGIN
        YBlockLines=YE[Block_Y]-YS[Block_Y]+1
        SunArr=FLTARR(ns)
        Y_UL=Y0-YS[Block_Y]*pixel[1]
        Y_LR=Y0-YE[Block_Y]*pixel[1]
        FOR Block_X=0,XBlocks-1 DO BEGIN
            X_UL=X0+XS[Block_X]*pixel[0]
            X_LR=X0+XE[Block_X]*pixel[0]
            CenterX=(X_UL+X_LR)/2. & CenterY=(Y_UL+Y_LR)/2.
            CenterLonLat=MAP_PROJ_INVERSE(CenterX,CenterY)
            zenith = solpos(Year, Month, Day, hour, minute, second, 0, CenterLonLat[0], CenterLonLat[1])
            ;            Sunarr[XS[Block_X]:XE[Block_X]]= COS(zenith * !dtor)
            Sunarr[XS[Block_X]:XE[Block_X]]=zenith
        ENDFOR
        FOR writeLines=0,YBlockLines-1 DO BEGIN
            WRITEU,LUN,SunArr
        ENDFOR
    ENDFOR
    SunArr=1
    FREE_LUN,lun
    ENVI_SETUP_HEAD, fname=SunOutputFile, ns=ns, nl=nl, nb=1, $
        data_type=4, offset=0, interleave=0, $
        MAP_INFO=map_info, /write, /open
;    ENVI_BATCH_EXIT
END

PRO GetSatZenith,xml,outputDIR,SatZFile=ZenithFile,flag=flag
    COMPILE_OPT idl2
    flag=0
    ;    xml='E:\test\6s\HJTest\320106\HJ1A-CCD1-455-56-20100607-L20000320106.XML'
    ;    outputDIR='E:\test\6s\HJTest\'
    XmlDIR=FILE_DIRNAME(xml,/MARK_DIRECTORY)
    xmlMainName=FILE_BASENAME(xml,'.XML',/FOLD_CASE)
    ZenithFile=outputDIR+xmlMainName+'-SatZenith'
    ENVI_OPEN_FILE,ZenithFile,R_FID=outfid
    IF outfid NE -1 THEN BEGIN
        flag=1
        RETURN
    ENDIF
    satfile=xmlDIR+xmlMainName+'-SatAngle.txt'
    IF FILE_SEARCH(xml) EQ '' THEN BEGIN
        PRINT,'Xml file cannot be found! Path:',xml
        RETURN
    ENDIF
    IF FILE_SEARCH(satfile) EQ '' THEN BEGIN
        PRINT,'SatAngle file cannot be found! Path:',satfile
        RETURN
    ENDIF
    ;获取投影信息
    AnalyzeXML,xml,L2UL=L2UL,UTMZone=UTMZone,L2LR=L2LR,Samples=Samples,Lines=lines
    FileLines=FILE_LINES(satfile)
    xpts=UINTARR(FileLines-3)
    ypts=UINTARR(FileLines-3)
    Zenith=FLTARR(FileLines-3)
    tempstr=STRARR(1)
    OPENR,lun,satfile,/GET_LUN
    FOR i=1,3 DO READF,lun,tempstr ;读掉头
    FOR i=0,FileLines-4 DO BEGIN
        READF,lun,tempstr
        fields=strsplit(tempstr,' ',/EXTRACT)
        Xpts[i]=UINT(ROUND(FLOAT(fields[3])))
        Ypts[i]=UINT(ROUND(FLOAT(fields[2])))
        Zenith[i]=FLOAT(fields[6])
    ENDFOR
    FREE_LUN,lun
    Ypts=lines-Ypts-1
    ;    ENVI,/RESTORE_BASE_SAVE_FILES
    ;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW;,LOG_FILE='D:\lgo.txt'
    UTMInfo=ENVI_MAP_INFO_CREATE(/UTM,DATUM='WGS-84',mc=[1,1,L2UL+[15,-15]],ps=[30,30],ZONE=utmzone)
    UTMPROJ=ENVI_PROJ_CREATE(/UTM,DATUM='WGS-84',ZONE=utmzone)
    ENVI_DOIT, 'ENVI_GRID_DOIT',INTERP=0,O_PROJ=UTMPROJ,$
        OUT_DT=4, OUT_NAME=ZenithFile, PIXEL_SIZE=[1,1], R_FID=rfid, $
        XMIN=0,YMIN=0,XMAX=Samples,Ymax=lines,$
        X_PTS=Xpts, Y_PTS=Ypts, Z_PTS=Zenith
    ENVI_FILE_QUERY,rfid, ns=ns, nl=nl, nb=nb, $
        data_type=data_type,interleave=interleave
    ENVI_SETUP_HEAD, fname=ZenithFile, ns=ns, nl=nl, nb=nb, $
        data_type=data_type, offset=0, interleave=interleave, $
        MAP_INFO=UTMInfo, /write, /open
    flag=1
;    ENVI_BATCH_EXIT
END