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
  nLines=file_lines(txtname)         ;��ȡ��
  tmp=''
;  ���ļ�
  openr,lun,txtname,/get_lun
   while(~EOF(lun))do begin
    readf,lun,tmp                      ;��ȡ��һ��
    print,tmp
    ;help,tmp
    var=strsplit(tmp,/extract)         ;��ֵ�һ�л�ȡ����
    rowNum=N_elements(var)            
    vararr=strarr(rowNum,nLines-1)     ;��������(n��n-1)
    readf,lun,vararr
    vararr=[[var],[vararr]]            ;�����������(n��n-1)+��һ��=��n,n��  over!
    print,'lon and lat =',vararr
    help,vararr
   endwhile
   endif
end