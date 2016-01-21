;+
;ENVI���ο������ܴ���
;
;Author: DYQ
;�������ۣ�
; http://hi.baidu.com/dyqwrp
; http://bbs.esrichina-bj.cn/ESRI/?fromuid=9806
;������
; ���ദ�������ල����ͷǼල����
;
;���÷�����
;
;CAL_CLASS,inputfile,outputfile, method,...
;
;inputFile:������Ӱ��
;outpurfile:������;
;Method : 0--5Ϊ�ල���࣬6��7Ϊ�Ǽල����
;
;         1-��С����   1
;         2-�����Ȼ   2
;         3-���Ͼ���  5
;         4-��Ԫ����  ENVI_NEURAL_NET_DOIT
;         5-������     ENVI_SVM_DOIT
;         6-IsoData   4
;         7-K-Means   7
;ע�⣺ÿһ���㷨��Ҫʹ�õĲ���˵���ɲο�ENVI�����ĵ�
;
PRO CAL_CLASS,inputfile,outputfile, method,$
    ;����Ȥ���ļ�
    roifile = roifile,$
    ;ƽ������������㷨��ѡ����
    STDV = stdv, $
    STD_MULT =STD_MULT,$
    ;��Ԫ��������㷨����
    theta = theta, $
    eta = eta, $
    alpha = alpha, $
    act_type = act_type, $
    rms_crit = rms_crit, $
    num_layers = num_layers, $
    num_sweeps = num_sweeps, $
    ;������
    thresh = thresh, $
    penalty = penalty, $
    kernel_type = kernel_type, $
    kernel_degree = kernel_degree, $
    kernel_bias = kernel_bias ,$
    ;K-Means �㷨��ѡ����
    ITERATIONS = ITERATIONS, $
    NUM_CLASSES = NUM_CLASSES , $
    ;ISO�㷨����
    CHANGE_THRESH = CHANGE_THRESH, $
    ISO_MERGE_DIST = ISO_MERGE_DIST, $
    ISO_MERGE_PAIRS = ISO_MERGE_PAIRS, $
    ISO_MIN_PIXELS = ISO_MIN_PIXELS, $
    ISO_SPLIT_SMULT = ISO_SPLIT_SMULT, $
    ISO_SPLIT_STD = ISO_SPLIT_STD, $
    MIN_CLASSES = MIN_CLASSES
    
  COMPILE_OPT idl2
  CATCH, Error_status
  errorshow = 'Sorry to see the error,'+ $
    ' please send the error Information to "dongyq@esrichina-bj.cn"'
  IF Error_status NE 0 THEN BEGIN
    tmp = DIALOG_MESSAGE(errorshow+STRING(13b)+$
      !ERROR_STATE.MSG,/error,title = '������ʾ!')
    return
  ENDIF
  ;��������Ԥ����
  ENVI_OPEN_FILE, inputfile, r_fid=fid
  IF (fid EQ -1) THEN BEGIN
    RETURN
  ENDIF
  ;��ȡ�ļ���Ϣ
  ENVI_FILE_QUERY, fid, dims=dims, nb=nb
  pos  = LINDGEN(nb)
  out_name = outputfile
  CASE method OF
    ;-IsoData   4
    6: BEGIN
      IF ~KEYWORD_SET(CHANGE_THRESH) THEN CHANGE_THRESH = .05
      IF ~KEYWORD_SET(NUM_CLASSES) THEN NUM_CLASSES = 10
      IF ~KEYWORD_SET(ITERATIONS) THEN ITERATIONS = 1
      IF ~KEYWORD_SET(ISO_MERGE_DIST) THEN ISO_MERGE_DIST = 1
      IF ~KEYWORD_SET(ISO_MERGE_PAIRS) THEN ISO_MERGE_PAIRS = 2
      IF ~KEYWORD_SET(ISO_MIN_PIXELS) THEN ISO_MIN_PIXELS = 1
      IF ~KEYWORD_SET(ISO_SPLIT_SMULT) THEN ISO_SPLIT_SMULT = 1
      IF ~KEYWORD_SET(ISO_SPLIT_STD) THEN ISO_SPLIT_STD = 1
      IF ~KEYWORD_SET(MIN_CLASSES) THEN MIN_CLASSES = 5
      
      out_bname = 'IsoData'
      
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=4, $
        r_fid=r_fid, $
        NUM_CLASSES = NUM_CLASSES, $
        ITERATIONS = ITERATIONS, $
        in_memory=0, $
        CHANGE_THRESH = CHANGE_THRESH, $
        ISO_MERGE_DIST = ISO_MERGE_DIST, $
        ISO_MERGE_PAIRS = ISO_MERGE_PAIRS, $
        ISO_MIN_PIXELS = ISO_MIN_PIXELS, $
        ISO_SPLIT_SMULT = ISO_SPLIT_SMULT, $
        ISO_SPLIT_STD = ISO_SPLIT_STD, $
        MIN_CLASSES = MIN_CLASSES
    END
    ;-K-Means   7
    7: BEGIN
      IF ~KEYWORD_SET(NUM_CLASSES) THEN NUM_CLASSES = 5
      IF ~KEYWORD_SET(CHANGE_THRESH) THEN CHANGE_THRESH = .5
      IF ~KEYWORD_SET(ITERATIONS) THEN ITERATIONS = 1
      out_bname = 'K-Means'
      
      thresh=REPLICATE(0.05,num_classes)
      
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=7, $
        r_fid=r_fid, $
        lookup = BYTARR(3,num_classes+1), $
        NUM_CLASSES = NUM_CLASSES, $
        in_memory=0, CHANGE_THRESH=CHANGE_THRESH,$
        ITERATIONS = ITERATIONS
    END
    ;-ƽ�������� 0
    0: BEGIN
      ENVI_RESTORE_ROIS, roifile
      roi_ids = ENVI_GET_ROI_IDS(fid=fid, $
        roi_colors=roi_colors, roi_names=class_names)
      class_names = ['Unclassified', class_names]
      num_classes = N_ELEMENTS(roi_ids)
      ; Set the unclassified class to black and use roi colors
      lookup = BYTARR(3,num_classes+1)
      lookup[0,1] = roi_colors
      ; ������ROI�Ļ���ͳ����Ϣ
      mean = FLTARR(N_ELEMENTS(pos), num_classes)
      stdv = FLTARR(N_ELEMENTS(pos), num_classes)
      cov = FLTARR(N_ELEMENTS(pos),N_ELEMENTS(pos),num_classes)
      FOR j=0, num_classes-1 DO BEGIN
        ;
        roi_dims=[ENVI_GET_ROI_DIMS_PTR(roi_ids[j]),0,0,0,0]
        ENVI_DOIT, 'envi_stats_doit', fid=fid, pos=pos, $
          dims=roi_dims, comp_flag=4, mean=c_mean, $
          stdv=c_stdv, cov=c_cov
        MEAN[0,j] = c_mean
        stdv[0,j] = c_stdv
        cov[0,0,j] = c_cov
      ENDFOR
      ;
      thresh=REPLICATE(0.05,num_classes)
      out_bname = 'parallelepiped'
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=0, $
        mean=mean, stdv=stdv, std_mult=st_mult, $
        lookup=lookup, class_names=class_names, $
        in_memory=0;, thresh=thresh
        
    END
    ;-��С����   1
    1: BEGIN
      ENVI_RESTORE_ROIS, roifile
      roi_ids = ENVI_GET_ROI_IDS(fid=fid, $
        roi_colors=roi_colors, roi_names=class_names)
      class_names = ['Unclassified', class_names]
      num_classes = N_ELEMENTS(roi_ids)
      ; Set the unclassified class to black and use roi colors
      lookup = BYTARR(3,num_classes+1)
      lookup[0,1] = roi_colors
      ; ������ROI�Ļ���ͳ����Ϣ
      ;
      mean = FLTARR(N_ELEMENTS(pos), num_classes)
      stdv = FLTARR(N_ELEMENTS(pos), num_classes)
      cov = FLTARR(N_ELEMENTS(pos),N_ELEMENTS(pos),num_classes)
      FOR j=0, num_classes-1 DO BEGIN
        ;
        roi_dims=[ENVI_GET_ROI_DIMS_PTR(roi_ids[j]),0,0,0,0]
        ENVI_DOIT, 'envi_stats_doit', fid=fid, pos=pos, $
          dims=roi_dims, comp_flag=4, mean=c_mean, $
          stdv=c_stdv, cov=c_cov
        MEAN[0,j] = c_mean
        stdv[0,j] = c_stdv
        cov[0,0,j] = c_cov
      ENDFOR
      ;
      thresh=REPLICATE(0.05,num_classes)
      out_bname = 'MinimumDistance'
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=1, $
        mean=mean, stdv=stdv, std_mult=st_mult, $
        lookup=lookup, class_names=class_names, $
        in_memory=0
    END
    ;-�����Ȼ   2
    2: BEGIN
      ENVI_RESTORE_ROIS, roifile
      roi_ids = ENVI_GET_ROI_IDS(fid=fid, $
        roi_colors=roi_colors, roi_names=class_names)
      class_names = ['Unclassified', class_names]
      num_classes = N_ELEMENTS(roi_ids)
      ; Set the unclassified class to black and use roi colors
      lookup = BYTARR(3,num_classes+1)
      lookup[0,1] = roi_colors
      ; ������ROI�Ļ���ͳ����Ϣ
      ;
      mean = FLTARR(N_ELEMENTS(pos), num_classes)
      stdv = FLTARR(N_ELEMENTS(pos), num_classes)
      cov = FLTARR(N_ELEMENTS(pos),N_ELEMENTS(pos),num_classes)
      FOR j=0, num_classes-1 DO BEGIN
        ;
        roi_dims=[ENVI_GET_ROI_DIMS_PTR(roi_ids[j]),0,0,0,0]
        ENVI_DOIT, 'envi_stats_doit', fid=fid, pos=pos, $
          dims=roi_dims, comp_flag=4, mean=c_mean, $
          stdv=c_stdv, cov=c_cov
        MEAN[0,j] = c_mean
        stdv[0,j] = c_stdv
        cov[0,0,j] = c_cov
      ENDFOR
      ;
      thresh=REPLICATE(0.05,num_classes)
      out_bname = 'MaximumLikelihood'
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=2, $
        mean=mean, stdv=stdv, std_mult=st_mult, $
        lookup=lookup, class_names=class_names, $
        cov = cov,$
        in_memory=0
    END
    ;-���Ͼ���  5
    3: BEGIN
      ENVI_RESTORE_ROIS, roifile
      roi_ids = ENVI_GET_ROI_IDS(fid=fid, $
        roi_colors=roi_colors, roi_names=class_names)
        
      class_names = ['Unclassified', class_names]
      num_classes = N_ELEMENTS(roi_ids)
      ; Set the unclassified class to black and use roi colors
      lookup = BYTARR(3,num_classes+1)
      lookup[0,1] = roi_colors
      ; ������ROI�Ļ���ͳ����Ϣ
      ;
      mean = FLTARR(N_ELEMENTS(pos), num_classes)
      stdv = FLTARR(N_ELEMENTS(pos), num_classes)
      cov = FLTARR(N_ELEMENTS(pos),N_ELEMENTS(pos),num_classes)
      FOR j=0, num_classes-1 DO BEGIN
        ;
        roi_dims=[ENVI_GET_ROI_DIMS_PTR(roi_ids[j]),0,0,0,0]
        ENVI_DOIT, 'envi_stats_doit', fid=fid, pos=pos, $
          dims=roi_dims, comp_flag=4, mean=c_mean, $
          stdv=c_stdv, cov=c_cov
        MEAN[0,j] = c_mean
        stdv[0,j] = c_stdv
        cov[0,0,j] = c_cov
      ENDFOR
      ;
      thresh=REPLICATE(0.05,num_classes)
      out_bname = 'Mahalanobis'
      ENVI_GET_ROI_INFORMATION, roi_ids,nPts = nPts
      ENVI_DOIT, 'class_doit', fid=fid, pos=pos, dims=dims, $
        out_bname=out_bname, out_name=out_name, method=5, $
        mean=mean, stdv=stdv, std_mult=st_mult, $
        lookup=lookup, class_names=class_names, $
        cov = cov,NPTS = nPts, $
        in_memory=0
    END
    ;-��Ԫ����  ENVI_NEURAL_NET_DOIT
    4: BEGIN
      IF ~KEYWORD_SET(theta) THEN theta = .9
      IF ~KEYWORD_SET(eta) THEN eta = .2
      IF ~KEYWORD_SET(alpha) THEN alpha = .9
      IF ~KEYWORD_SET(act_type) THEN act_type = 0
      IF ~KEYWORD_SET(rms_crit) THEN rms_crit = .1
      IF ~KEYWORD_SET(num_layers) THEN num_layers = 3
      IF ~KEYWORD_SET(num_sweeps) THEN num_sweeps = 10
      
      ENVI_RESTORE_ROIS, roifile
      roi_ids = ENVI_GET_ROI_IDS(fid=fid, $
        roi_colors=lookup, roi_names=class_names)
      ; Set the classification variables
      ;
      num_classes = N_ELEMENTS(roi_ids)
      class_names = ['Unclassified', class_names]
      lookup = REFORM([0,0,0, $
        REFORM(lookup,3*num_classes)],3,num_classes+1)
      ;
      ; Call the doit
      ;
      ENVI_DOIT, 'envi_neural_net_doit', $
        fid=fid, pos=pos, dims=dims, $
        out_name=out_name, rule_out_name='', $
        theta=theta, eta=eta, alpha=alpha, $
        num_classes=num_classes, num_sweeps=num_sweeps, $
        num_layers=num_layers, act_type=act_type, $
        rms_crit=rms_crit, roi_ids=roi_ids, /train, $
        class_names=class_names, lookup=lookup
    END
    ;-������     ENVI_SVM_DOIT
    5: BEGIN
      IF ~KEYWORD_SET(thresh) THEN thresh = .5
      IF ~KEYWORD_SET(penalty) THEN penalty=75
      IF ~KEYWORD_SET(kernel_type) THEN kernel_type=1
      IF ~KEYWORD_SET(kernel_degree) THEN kernel_degree=3
      IF ~KEYWORD_SET(kernel_bias) THEN kernel_bias = 2.
      ENVI_RESTORE_ROIS, roifile
      roi_ids = ENVI_GET_ROI_IDS(fid=fid)
      ; Call the svm classification doit routine
      envi_doit, 'envi_svm_doit', $
        fid=fid, pos=pos, dims=dims, $
        out_name=out_name, $
        roi_ids=roi_ids, thresh=thresh, $
        penalty=penalty, kernel_type= kernel_type, $
        kernel_degree=kernel_degree, kernel_bias=kernel_bias
    END
    ELSE:
  ENDCASE
END
