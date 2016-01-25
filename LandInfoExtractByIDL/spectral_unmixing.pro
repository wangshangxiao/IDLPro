FUNCTION HYPER2D, M

  ;������άͼ�������ά
  COMPILE_OPT idl2
  dims = SIZE(M, /dimensions)
  
  IF SIZE(M, /n_dimensions) NE 3 AND SIZE(M, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('Input image must be 3D')
  ENDIF ELSE IF  SIZE(M, /n_dimensions) EQ 2 THEN BEGIN
    nb = 1
  ENDIF ELSE BEGIN
    nb = dims[2]
  ENDELSE
  
  ;ת�þ���
  M_temp = M
  FOR i = 0, nb-1 DO BEGIN
    M_temp[*,*,i] = TRANSPOSE(M[*,*,i])
  ENDFOR
  RETURN, REFORM(TEMPORARY(M_temp), dims[0]*dims[1], nb)
  
END



FUNCTION HYPER3D, M, ns, nl, nb

  ;ns - ����        nl - ����         nb - ������
  ;�����άͼ�������ά
  COMPILE_OPT idl2
  IF SIZE(M, /n_dimensions) NE 2 AND nb NE 1 THEN BEGIN
    mytemp = DIALOG_MESSAGE('Input image must be 2D')
  ENDIF ELSE BEGIN
    M_temp = FLTARR(ns, nl, nb)
    FOR i = 0, nb-1 DO BEGIN
      M_temp[*,*,i] = TRANSPOSE(REFORM(TRANSPOSE(M[*,i]), nl, ns))
    ENDFOR
    RETURN, TEMPORARY(M_temp)
  ENDELSE
  
END





FUNCTION INV, data

  ;��������
  COMPILE_OPT idl2
  IF SIZE(data, /n_dimensions) LT 2 THEN BEGIN
    RETURN, 1/data
  ENDIF ELSE BEGIN
    RETURN, INVERT(data)
  ENDELSE
  
END




FUNCTION HYPERFCLS, M, U
  ;
  ;�����Ԫ�ֽ� - ��ȫԼ����С���˷�
  ; Usage
  ;   [ X ] = hyperFcls( M, U )
  ; Inputs    [h, w, bandnum]
  ;   M - HSI data matrix (N x p)         p= bandnum  N=h*w
  ;   U - Matrix of endmembers (q x p)    q = endm number
  ; Outputs
  ;   X - Abundance maps (N x q)

  COMPILE_OPT idl2
  ;�ж���������M��U�Ƿ��Ϊ2ά
  IF SIZE(U, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('U must be 2D data')
  ENDIF
  
  IF SIZE(M, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('M must be 2D data', /error)
  ENDIF
  
  dims_M = SIZE(M, /dimensions)
  dims_U = SIZE(U, /dimensions)
  
  ;M �� U ��������ͬ����������
  IF dims_M[1] NE dims_U[1] THEN BEGIN
    mytemp = DIALOG_MESSAGE('M and U must have the same number of spectral bands', /error)
  ENDIF
  
  p = dims_M[1] ;������
  N = dims_M[0] ;M��������ͼ��ĳ��˿�
  q = dims_U[0]
  X = FLTARR(N, q)  ;��ʼ����ά���ͼ
  
  Mbckp = U
  
  FOR n1=0L,N-1 DO BEGIN
  
    count = q
    done = 0
    ref = INDGEN(q)
    r = M[n1,*]
    U = Mbckp
    
    WHILE ~done DO BEGIN
    
      ; inv(U.'*U)  =  imsl_inv(U#transpose(U))
      als_hat = r#TRANSPOSE(U)#INV(U#TRANSPOSE(U))
      ones = INTARR(1,count)
      ones[*,*] = 1
      s = TRANSPOSE(ones)#INV(U#TRANSPOSE(U))
      afcls_hat = als_hat - (als_hat#TRANSPOSE(ones)-1) # (INV(ones#INV(U#TRANSPOSE(U))#TRANSPOSE(ones))) # TRANSPOSE(ones)#INV(U#TRANSPOSE(U))
      
      ;See if all components are positive.  If so, then stop.
      alpha = FLTARR(1,q)
      IF TOTAL(afcls_hat GT 0) EQ count THEN BEGIN
        alpha[ref] = afcls_hat
        BREAK
      ENDIF
      
      ;Multiply negative elements by their counterpart in the s vector.
      ;Find largest abs(a_ij, s_ij) and remove entry from alpha.
      idx = WHERE(afcls_hat LT 0)
      ;      if idx lt 0 then idx = 0
      afcls_hat[idx] = afcls_hat[idx]/s[idx]
      maxIdx = (SORT(-ABS(afcls_hat[idx])))[0]
      maxIdx = idx[maxIdx]
      alpha[maxIdx] = 0
      ;size(U,2)   =    (size(U, /dimensions))[0]
      ;1:size(U,2)   =    indgen(1, (size(U, /dimensions))[0])
      idx_all = INDGEN(1, (SIZE(U, /dimensions))[0])
      keep = idx_all[WHERE(idx_all NE maxIdx)]
      U = U[keep, *]
      count = count - 1
      ref = ref[keep]
      
    ENDWHILE
    
    X[n1,*] = alpha
    
  ENDFOR
  
  RETURN, X
  
END



PRO SPECTRAL_UNMIXING, ev
  ;
  COMPILE_OPT idl2
  
  WIDGET_CONTROL, ev.TOP, get_uvalue = pState
  
  ENVI_SELECT, fid = fid, title = 'Select the input file', /no_dims
  
  IF fid EQ -1 THEN RETURN
  
  ;���fid��Ӧ�ļ�����������û�д�
  (*pState).OTREE.ADDFILE, ev, fid
  
  ENVI_FILE_QUERY, fid, nb = nb, ns = ns, nl = nl, dims = dims, file_type = ftype
  
  ;��ȡ����
  data = MAKE_ARRAY(ns, nl, nb, type = ftype)
  FOR i=0,nb-1 DO BEGIN
    data[*,*,i] = ENVI_GET_DATA(fid = fid, dims = dims, pos = i)
  ENDFOR
  
  ;ת��Ϊ��ά�������Ԫ�ֽ��㷨������Ϊ��ά
  data = HYPER2D(data)
  
  ;��ȡ��Ԫ����
  file =  dialog_PICKFILE(title = 'Select the endmember data', filter = '*.txt')
  
  IF ~FILE_TEST(file) THEN RETURN
  
  ;��ȡ��Ԫ�����У��Ͳ��������У�
  OPENR, lun, file, /get_lun
  
  ;��ȡ������
  bands = FILE_LINES(file)
  
  IF bands NE nb THEN tmp = DIALOG_MESSAGE('The input file and endmember data have different numbers of band')
  
  ;����һ��
  tmp = ''
  READF, lun, tmp
  
  ;��ȡ��Ԫ��
  endm_num = N_ELEMENTS(STRSPLIT(tmp, /extract))
  
  endm = FINDGEN(endm_num, bands)
  FREE_LUN, lun
  
  ;����Ԫ���ݶ���endm
  OPENR, lun, file, /get_lun
  READF, lun, endm
  FREE_LUN, lun
  
  ;�����Ԫ�ֽ�
  prsbar = IDLITWDPROGRESSBAR(GROUP_LEADER = ev.TOP,title ='����',CANCEL=cancelIn)
  FOR i = 1,10 DO BEGIN
    IF WIDGET_INFO(prsbar, /valid) THEN BEGIN
      IDLITWDPROGRESSBAR_SETVALUE, prsbar, i*10
    ENDIF ELSE BEGIN
      RETURN
    ENDELSE
    IF i EQ 10 THEN WIDGET_CONTROL, prsbar, /destroy
    WAIT, 0.1
    IF i EQ 4 THEN data_X2d = HYPERFCLS(TEMPORARY(data), endm)
  ENDFOR
  
  
  ;ת��Ϊ��ά
  data_X3d = HYPER3D(TEMPORARY(data_X2d), ns, nl, endm_num)
  
  ;ѡ�����·��
  base = WIDGET_AUTO_BASE(title='Please the output filename')
  wo = WIDGET_OUTF(base, uvalue='outf', /auto)
  result = AUTO_WID_MNG(base)
  IF (result.ACCEPT EQ 0) THEN RETURN
  
  ;д��ΪENVI��ʽ
  ENVI_WRITE_ENVI_FILE, data_X3d, file_type = 4, interleave = 0, out_name = result.OUTF, r_fid = fid
  
  ;���ļ���ӵ��б���
  (*pState).OTREE.ADDFILE, ev, fid
END
