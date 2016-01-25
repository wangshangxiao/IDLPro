FUNCTION HYPER2D, M

  ;输入三维图像，输出二维
  COMPILE_OPT idl2
  dims = SIZE(M, /dimensions)
  
  IF SIZE(M, /n_dimensions) NE 3 AND SIZE(M, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('Input image must be 3D')
  ENDIF ELSE IF  SIZE(M, /n_dimensions) EQ 2 THEN BEGIN
    nb = 1
  ENDIF ELSE BEGIN
    nb = dims[2]
  ENDELSE
  
  ;转置矩阵
  M_temp = M
  FOR i = 0, nb-1 DO BEGIN
    M_temp[*,*,i] = TRANSPOSE(M[*,*,i])
  ENDFOR
  RETURN, REFORM(TEMPORARY(M_temp), dims[0]*dims[1], nb)
  
END



FUNCTION HYPER3D, M, ns, nl, nb

  ;ns - 列数        nl - 行数         nb - 波段数
  ;输入二维图像，输出三维
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

  ;矩阵求逆
  COMPILE_OPT idl2
  IF SIZE(data, /n_dimensions) LT 2 THEN BEGIN
    RETURN, 1/data
  ENDIF ELSE BEGIN
    RETURN, INVERT(data)
  ENDELSE
  
END




FUNCTION HYPERFCLS, M, U
  ;
  ;混合像元分解 - 完全约束最小二乘法
  ; Usage
  ;   [ X ] = hyperFcls( M, U )
  ; Inputs    [h, w, bandnum]
  ;   M - HSI data matrix (N x p)         p= bandnum  N=h*w
  ;   U - Matrix of endmembers (q x p)    q = endm number
  ; Outputs
  ;   X - Abundance maps (N x q)

  COMPILE_OPT idl2
  ;判断输入数据M和U是否均为2维
  IF SIZE(U, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('U must be 2D data')
  ENDIF
  
  IF SIZE(M, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('M must be 2D data', /error)
  ENDIF
  
  dims_M = SIZE(M, /dimensions)
  dims_U = SIZE(U, /dimensions)
  
  ;M 和 U 的行数相同，即波段数
  IF dims_M[1] NE dims_U[1] THEN BEGIN
    mytemp = DIALOG_MESSAGE('M and U must have the same number of spectral bands', /error)
  ENDIF
  
  p = dims_M[1] ;波段数
  N = dims_M[0] ;M的列数，图像的长乘宽
  q = dims_U[0]
  X = FLTARR(N, q)  ;初始化二维丰度图
  
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
  
  ;如果fid对应文件在主程序中没有打开
  (*pState).OTREE.ADDFILE, ev, fid
  
  ENVI_FILE_QUERY, fid, nb = nb, ns = ns, nl = nl, dims = dims, file_type = ftype
  
  ;获取数据
  data = MAKE_ARRAY(ns, nl, nb, type = ftype)
  FOR i=0,nb-1 DO BEGIN
    data[*,*,i] = ENVI_GET_DATA(fid = fid, dims = dims, pos = i)
  ENDFOR
  
  ;转换为二维，混合像元分解算法中输入为二维
  data = HYPER2D(data)
  
  ;获取端元数据
  file =  dialog_PICKFILE(title = 'Select the endmember data', filter = '*.txt')
  
  IF ~FILE_TEST(file) THEN RETURN
  
  ;获取端元数（列）和波段数（行）
  OPENR, lun, file, /get_lun
  
  ;获取波段数
  bands = FILE_LINES(file)
  
  IF bands NE nb THEN tmp = DIALOG_MESSAGE('The input file and endmember data have different numbers of band')
  
  ;读第一行
  tmp = ''
  READF, lun, tmp
  
  ;获取端元数
  endm_num = N_ELEMENTS(STRSPLIT(tmp, /extract))
  
  endm = FINDGEN(endm_num, bands)
  FREE_LUN, lun
  
  ;将端元数据读入endm
  OPENR, lun, file, /get_lun
  READF, lun, endm
  FREE_LUN, lun
  
  ;混合像元分解
  prsbar = IDLITWDPROGRESSBAR(GROUP_LEADER = ev.TOP,title ='进度',CANCEL=cancelIn)
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
  
  
  ;转化为三维
  data_X3d = HYPER3D(TEMPORARY(data_X2d), ns, nl, endm_num)
  
  ;选择输出路径
  base = WIDGET_AUTO_BASE(title='Please the output filename')
  wo = WIDGET_OUTF(base, uvalue='outf', /auto)
  result = AUTO_WID_MNG(base)
  IF (result.ACCEPT EQ 0) THEN RETURN
  
  ;写出为ENVI格式
  ENVI_WRITE_ENVI_FILE, data_X3d, file_type = 4, interleave = 0, out_name = result.OUTF, r_fid = fid
  
  ;把文件添加到列表中
  (*pState).OTREE.ADDFILE, ev, fid
END
