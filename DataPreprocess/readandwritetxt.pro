pro readandwritetxt,array=array
  ;txtname = DIALOG_PICKFILE(FILTER='*.txt')
;  OPENR,lun,'text\crop_code.txt',/GET_LUN
;  ; Read one line at a time, saving the result into array
;  array = ''
;  line = ''
;  WHILE NOT EOF(lun) DO BEGIN & $
;    READF, lun, line & $
;    array = [[array], [line]] & $
;  ENDWHILE
  ;print,array
  
  ; Close the file and free the file unit
  ;FREE_LUN, lun
  
  txtname = DIALOG_PICKFILE(FILTER='*.txt')
  if file_test(txtname)then begin
  nLines=file_lines(txtname)         ;获取行
  tmp=''
;  打开文件
  openr,lun,txtname,/get_lun
   while(~EOF(lun))do begin
    readf,lun,tmp                      ;获取第一行
    print,tmp
    ;help,tmp
    var=strsplit(tmp,/extract)         ;拆分第一行获取列数
    rowNum=N_elements(var)            
    vararr=strarr(rowNum,nLines-1)     ;定义数组(n，n-1)
    readf,lun,vararr
    vararr=[[var],[vararr]]            ;重新组合数组(n，n-1)+第一行=（n,n）  over!
    print,'lon and lat =',vararr
    help,vararr
   endwhile
   endif
end