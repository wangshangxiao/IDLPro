;获取文件夹下所有指定格式文件
FUNCTION get_file,InputDIR,type,RECURSIVE=RECURSIVE
    files=FILE_SEARCH(InputDIR+'*'+type,count=count,/FOLD_CASE)
    ;print,'files:'+files
    IF KEYWORD_SET(RECURSIVE) THEN BEGIN
        sub_folds=FILE_SEARCH(InputDIR+'*',count=Foldscount,/TEST_DIRECTORY,/FOLD_CASE,/mark)
        ;print,'sub_folds:'+sub_folds
        FOR i=0,Foldscount-1 DO BEGIN
            newfiles=get_file(sub_folds[i],type,/RECURSIVE)
            ;print,'newfiles:'+newfiles
            files=[files,newfiles]
            ;print,'files:'+files
        ENDFOR
    END
    RETURN,files
END

FUNCTION get_out_name,filename,outpath
     
     ;PRINT, filename
     FileNameStrs= STRSPLIT(filename, '\', /EXTRACT)
     SingleFileName=FileNameStrs[N_ELEMENTS(FileNameStrs)-1]
     ;PRINT, SingleFileName
     ;in_name=filename
     ;PRINT, in_name
     out_name=outpath+SingleFileName
     ;PRINT, out_name
     return,out_name
     
end

PRO Mutiply_batch_run,inputfile, chimageoutpath
    print,'input:',inputfile
    
    DATA1=READ_TIFF(inputfile,GEOTIFF=GEOTIFF)
    size1=size(data1,/DIMENSIONS)
      nb=size1[0]
      ns=size1[1]
      nl=size1[2]
      result=fltarr(size1[0],ns,nl)
      for band=0,nb-1 do begin
        ;result[band,*,*]=data1[band,0:(ns-1),0:(nl-1)]*data2[0:(ns-1),0:(nl-1)]
        result[band,*,*]=data1[band,*,*]*0.0001
      endfor

write_tiff,chimageoutpath+'\'+FILE_BASENAME(inputfile,'.tif',/FOLD_CASE)+'_ref.tif',result,geotiff = geotiff,/float
    

end


pro MutiplyBandMathMuti

InputDIR='D:\share\Hongxingtest\HJ\2013\geobyGF_square_plot'
outpath='D:\share\Hongxingtest\HJ\2013\geobyGF_square_plot_rf'
 ;;获取目录下包括子目录下所有文件并逐一进行裁剪
    filename=get_file(InputDIR,'.tif',/RECURSIVE)
    
    n = N_ELEMENTS(filename)

    ;==========批处理=====================
    FOR i=1,n-1 DO BEGIN

     in_name=filename[i]
     PRINT,'i:',i,'---:', filename[i]
     out_name=get_out_name(filename[i],outpath)
     PRINT, 'outpath:',out_name
     ;文件是 否已经存在
     if file_test(out_name) eq 1 then begin
       CAUTION = dialog_message('输出文件已存在，是否覆盖?',title='警告',/question)
       if CAUTION EQ 'No' THEN BEGIN
         envi_batch_exit
         return
       endif
     endif

;;;调用裁剪函数
     Mutiply_batch_run,in_name,outpath


    ENDFOR


END
