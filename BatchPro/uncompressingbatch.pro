

FUNCTION getFileFromFolder,inputPath,ExtensionName

  ;Get all files from the folder
  Files=FILE_SEARCH(inputPath,ExtensionName,/FOLD_CASE,count=count)
  ;
;  FOR i=0,count-1 DO BEGIN
;    ;get the path name, not include the file name
;    path=file_dirname(Files[i],/MARK_DIRECTORY)
;    ;print,path
;    ;get the file name
;    file=FILE_BASENAME(Files[i])
;    ;print,file
;
;  ENDFOR
  Return,Files
END

;add '\' at the end for the input dirname
FUNCTION MarkDIR,str
  ;    return,file_dirname(str,/MARK_DIRECTORY)+file_basename(str)
  IF STRMID(str,STRLEN(str)-1,1) NE PATH_SEP() THEN value=str+PATH_SEP() ELSE value=str
  RETURN,value
END

;uncompressing the files by winrar
PRO winrar,rarfile,outputDIR
  COMPILE_OPT idl2
  ;COMMON GlobalVar
  FILE_MKDIR,outputDIR
  winrarDIR='C:\Program Files\WinRAR'
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

;do UnCompressing
Pro UnCompressingBatch
  
  ;the data folder
  inputPath='D:\HXData'
  ;ExtensionName filter
  ExtensionName='*.tar.gz'
  ;get all the Compressed files
  SourceFiles = getFileFromFolder(inputPath,ExtensionName)
  ;print,SourceFiles
  ;get the number of SourceFiles
  nCount = n_elements(SourceFiles)
  print,nCount
  FOR i=0,nCount-1 DO BEGIN
    winrar,SourceFiles[i],file_dirname(SourceFiles[i])
  ENDFOR
  print,'Finished!'
END