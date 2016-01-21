;This Function will Return the Start path
Function SourcePath
On_Error, 2
Help, Calls = Calls
UpperRoutine = (StrTok(Calls[1], ' ', /Extract))[0]
Skip = 0
Catch, ErrorNumber
If (ErrorNumber ne 0) then Begin
 Catch, /Cancel
 Message, /Reset
 ThisRoutine = Routine_Info(UpperRoutine, /Functions, /Source)
 Skip = 1
EndIf
If (Skip eq 0) then Begin
 ThisRoutine = Routine_Info(UpperRoutine, /Source)
 If (ThisRoutine.Path eq '') then Begin
  Message, '', /Traceback
 EndIf
EndIf
Catch, /Cancel
If (StrPos(ThisRoutine.Path, Path_Sep()) eq -1) then Begin
 CD, Current = Current
 SourcePath = FilePath(ThisRoutine.Path, Root = Current)
EndIf Else Begin
 SourcePath = ThisRoutine.Path
EndElse
Root = StrMid(SourcePath, 0, $
 StrPos(SourcePath, Path_Sep(), /Reverse_Search) + 1)
Return, Root
End

pro resize  ;�ļ����������������ͬ�������޷�����
   startpath = SourcePath()
   cd,startpath
   compile_opt idl2
   envi,/restore_base_save_files
   envi_batch_init, log_file='batch.txt'
        ;=====���������ļ�·��=====
    inpath=startpath
            ;=====��������ļ�·��=====
    outpath= startpath+"result"
        ;�����������ļ����б�
    filename = findfile('*.tif')
    n = N_ELEMENTS(filename)   ;n_elements������������������Ԫ�ص���Ŀ
    ;==========������=====================
    FOR i=0,n-1 DO BEGIN
        in_name=inpath+filename[i]
    envi_open_file, in_name, r_fid=fid
        if (fid eq -1) then begin
            envi_batch_exit
            return
        endif
        envi_file_query, fid, ns=ns, nl=nl, nb=nb
        dims = [-1, 0, ns-1, 0, nl-1]
        pos  = lindgen(nb)
        out_name = outpath+filename[i]
        envi_doit,'resize_doit',$
            fid=fid, pos=pos, dims=[0, 0,170,0,170], $
            interp=0, rfact=[1,1],out_name=out_name, r_fid=r_fid
    ENDFOR
    ;=============================�˳�������ģʽ=======================
    envi_batch_exit
end