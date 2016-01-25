;+
; NAME: EOScc_SVM
;
; PURPOSE: 
;
;   This common procedure performs Support Vector Machine classification on a list of images by using a set 
;   of 'Regions Of Interest' (ROIs) as ground reference data.
; 
; AUTHOR:
; 
;   Luc Bertels
;   TAP - Teledetection and Earth Observation Processes 
;   VITO - Flemish Institute for Technological Research
;   Boeretang 200 
;   B-2400 Mol, Belgium 
;   http://www.vito.be
;
; CALLING SEQUENCE:
;
;   EOScc_SVM, pImageList, pLibraries, Class_Dir, ACLASS_NAMES=string array, ACLASS_COLORS=array, 
;              ABAND_NAMES=string array [, BANDSIX=array] [, KERNEL_TYPE=value] [, KERNEL_DEGREE=value] 
;              [, KERNEL_BIAS=value] [, KERNEL_GAMMA=value] [, OSTATUS=object reference] [, /OVERWRITE]
;              [, PENALTY=value] [, PYRAMID_LEVELS=value] [, PYRAMID_RECLASS_TRESH=value] [, RULE_DIR=string] 
;              [, /TIFF]  [, TRESH=value]
;                 
; ARGUMENTS
; 
;   pImageList:     A pointer to a string or array of strings holding the path and name of the images to be 
;                   classified. 
;   pLibraries:     A named variable that holds an array of pointers. Each pointer points to a structure
;                   array where each element has the structure:
;                     {ImageFile:string, RoiId:value, Spectra:float array} 
;   Class_Dir:      A string holding the full path to the directory where the output results have to be 
;                   stored.
;                   
; KEYWORDS
;   
;   ACLASS_NAMES:   A named variable that holds an array of strings containing the class names for the 
;                   available classes.
;   ACLASS_COLORS:  A named variable that holds an array of RGB color values containing the class colors for 
;                   the available classes.
;   ABAND_NAMES:    A named variable that holds an array of strings containing the names for the different 
;                   bands of the rule image.
;   BANDSIX:        If present, an array of indices indicating the bands to be used in the classification. 
;                   If not present all bands will be used.                  
;   KERNEL_TYPE:    Use this keyword to specify the type of kernel to use for the SVM classification. 
;                   Allowable values are: 
;                       - 0: Linear (default)
;                       - 1: Polynomial
;                       - 2: Radial Basis Function (RBF) 
;                       - 3: Sigmoid
;   KERNEL_DEGREE:  Use this keyword to specify the degree of kernel to use for the SVM classification. This 
;                   keyword only applies if a polynomial kernel is selected. The default is 2. 
;   KERNEL_BIAS:    Use this keyword to specify the kernel bias value to use for the SVM classification. This
;                   keyword only applies if a sigmoid or polynomial kernel is selected. The default is 1. 
;   KERNEL_GAMMA:   Use this keyword to specify the kernel gamma value to use for the SVM classification. 
;                   This keyword is ignored if the kernel type is set to linear. The default is the inverse 
;                   of the number of bands in the input image. 
;   OSTATUS:        An object reference to an existing status window.
;   OVERWRITE:      Set this keyword to overwrite already existing output files.  
;   PENALTY:        Use this keyword to specify the penalty parameter to use for the SVM classification. The 
;                   default is 100. It is a floating point value greater than zero. 
;   PYRAMID_LEVELS: Use this keyword to specify the number of hierarchical resolution levels to process. This 
;                   value is an integer value from 0 (no special processing), which implies the input image 
;                   will only be processed at full resolution. If set to an integer larger than 0, this value 
;                   will specify the number of pyramid levels that will be processed. The maximum value 
;                   varies depending on the image size the user selects. It is determined by the criteria 
;                   that the highest pyramid-level image is larger than 64x64. For example, for an image size 
;                   of 24000 x 24000, the maximum level available would be 8. The default is 0. 
;   PYRAMID_RECLASS_THRESH: 
;                   Use this keyword to specify the probability threshold which a pixel must meet to avoid 
;                   being reclassified at a finer resolution. This keyword is ignored if PYRAMID_LEVELS is 
;                   not set to a value greater than 0. This is a floating point value ranging between 0.0 and 
;                   1.0 (all pixels classified). The default value is 0.9. 
;   RULE_DIR:       If present, a string holding the output directory where the rule images need to be stored.
;                   The directory will be created if it is not yet existing. The classified images are named 
;                   after the input image suffixed with '_rule'.
;   TIFF:           Set this keyword to output the classified images in tiff format.
;   THRESH:         Use this keyword to specify the minimum probability a pixel must have in order to be 
;                   classified. Pixels with all class probabilities less than this threshold will not be 
;                   classified. This value is a floating point value ranging between 0.0 (all pixels 
;                   classified) and 1.0 (no pixels classified). The default is 0.0.
;   
;   Note: The SVM specific keywords were copied from the ENVI help file 'ENVI_SVM_DOIT'.                                  
;                                   
; RETURN VALUE:
;   
;   None
;
; DEPENDENCIES:
; 
;   IOScc_Init
;         
; KNOWN ISSUES:
; 
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, January, 2012.
;
;###########################################################################
;
; LICENCE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2012 Luc Bertels, Flemish Institute for Technological Research.
;
; This software is provided "as-is", without any express or implied warranty. 
; Except in case of wilful misconduct or gross negligence the authors will 
; not be held liable for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; 4. This licence is subject to Belgian Law.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
;
;------------------------------------------------------------------------------------------------------------
pro EOScc_SVM, $
                pImageList, $
                pLibraries, $
                Class_Dir, $
                aClass_Names          = aClass_Names, $
                aCLass_Colors         = aCLass_Colors, $
                aBand_Names           = aBand_Names, $
                ;
                oStatus               = oStatus, $
                BandsIx               = BandsIx, $
                Rule_Dir              = Rule_Dir, $
                TIFF                  = TIFF , $
                OVERWRITE             = OVERWRITE, $
                ;
                kernel_type           = kernel_type, $
                kernel_degree         = kernel_degree, $
                kernel_bias           = kernel_bias, $
                kernel_gamma          = kernel_gamma, $
                penalty               = penalty, $
                pyramid_levels        = pyramid_levels, $
                pyramid_reclass_tresh = pyramid_reclass_tresh, $
                tresh                 = tresh
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module  = 'EOScc_SVM'

  envi, /restore_base_save_files  
  
  ;--------------------------------------------------------
  ;--- Creation and initialization of the status window ---
  ;--------------------------------------------------------
        
  oStatus = IOScc_Init(oStatus, $
                        Title="Image Classification using SVM", $
                        Log_DIRECTORY    = 'D:/log/', $
                        Log_FILE_PREFIX  = 'Classify')    
                        
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(aBand_Names)    then Argument = 'aBand_Names' 
  if ~keyword_set(aCLass_Colors)  then Argument = 'aCLass_Colors' 
  if ~keyword_set(aClass_Names)   then Argument = 'aClass_Names'   
  if ~keyword_set(Class_Dir)      then Argument = 'Class_Dir' 
  if ~keyword_set(pLibraries)     then Argument = 'pLibraries' 
  if ~keyword_set(pImageList)     then Argument = 'pImageList'  
  
  if keyword_set(Argument) then begin 
    
    oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log  
    
    if keyword_set(TEST) or n_params() eq 0 then begin
      if ptr_valid(pImageList) then ptr_free, pImageList
    endif
    
    return       
  endif
  ;----------------------
  ;--- Initialization ---
  ;----------------------
   
  if ~keyword_set(kernel_type) then kernel_type = 0
  if ~keyword_set(kernel_degree) then kernel_degree = 2
  if ~keyword_set(kernel_bias) then kernel_bias = 1
  if ~keyword_set(penalty) then penalty = 100.
  if ~keyword_set(pyramid_levels) then pyramid_levels = 0
  if ~keyword_set(pyramid_reclass_tresh) then pyramid_reclass_tresh = 0.9
  if ~keyword_set(tresh) then tresh = 0.
 
  nImages     = n_elements(*pImageList)
  nClasses    = n_elements(pLibraries)
  nSpectra    = intarr(nClasses)
  aRoiIds     = lonarr(nClasses)

  ;------------------
  ;--- Processing ---
  ;------------------

  for Ix=0, nClasses-1 do nSpectra[Ix] = n_elements(*pLibraries[Ix])

  TotalSpectra  = total(nSpectra)

  if keyword_set(Class_Dir) then Tmp_Mos_Dir = Class_Dir
  if keyword_set(Rule_Dir)  then Tmp_Mos_Dir = Rule_Dir

  for FileIx=0, nImages-1 do begin
    
    InFile      = (*pImageList)[FileIx]
    ImageName   = file_basename(InFile, '.img')
    
    Class_File  = Class_Dir + ImageName + '_svm'

    if ~keyword_set(overwrite) and file_test(Class_File) eq 1 then begin
    
      oStatus->Log_Status, 'Warning', 'Classification already exist: '+Class_File, Module, /Log
      continue
    endif
    
    oStatus->Log_Status, 'Ok', 'Support Vector Machine classification of: '+InFile, Module, /Log    
           
    if keyword_set(Rule_Dir) then Rule_File   = Rule_Dir  + ImageName + '_rule'
  
    Tmp_Mos_File  = Tmp_Mos_Dir + ImageName + '_tmpmos'
  
    envi_open_file, InFile, r_fid=ImFid  
    envi_file_query, ImFid, dims=dims, nb=nb, ns=ns, nl=nl, data_type=dt 
    envi_delete_rois, /all
    
    if ~keyword_set(kernel_gamma) then kernel_gamma    = 1. / nb
    
    ;---------------------------------------------
    ;--- Add extra lines to hold the ROIs data ---
    ;---------------------------------------------
    
    nAddLines = ceil(float(TotalSpectra) / ns)
    RoiLines  = make_array(ns, nAddLines, nb, type=dt)
    S_Ptr   = 0
    L_Ptr   = 0
    
    if ~keyword_set(aBand_Names) then begin
      aBand_Names = strarr(nClasses)
    
      for Ix=0, nClasses-1 do aBand_Names[Ix] = 'Band '+strtrim(string(Ix+1),2)
    endif
    
    for Ix=0, nClasses-1 do begin

      for Six=0, nSpectra[Ix]-1 do begin
        RoiLines[S_Ptr, L_Ptr, *] = (*pLibraries[Ix])[Six].Spectra
        
        if Six eq 0 then begin
          Roi_S = S_Ptr
          Roi_L = L_Ptr
        endif else begin
          Roi_S = [Roi_S, S_Ptr]
          Roi_L = [Roi_L, L_Ptr]
        endelse
        
        if ++S_Ptr eq ns then S_Ptr = 0
        if S_Ptr eq 0 then ++L_Ptr
      endfor

      aRoiIds[Ix] = envi_create_roi(name  = aBand_Names[Ix],$
                              ns    = ns , $
                              nl    = nl + nAddLines)
      
      envi_define_roi, aRoiIds[Ix], xpts=Roi_S, ypts = Roi_L+nl, /point
    endfor    
    
    map_info  = envi_get_map_info(fid=ImFid)
    
    envi_enter_data, RoiLines, r_fid= roi_fid
    envi_file_query, roi_fid, dims=roi_dims 

    envi_doit, 'mosaic_doit', fid               = [ImFid, roi_fid], $
                              dims              = [[dims], [roi_dims]], $
                              map_info          = map_info, $
                              out_dt            = dt, $
                              pixel_size        = map_info.ps, $
                              pos               = [[indgen(nb)], [indgen(nb)]], $
                              xsize             = ns * map_info.ps[0], $
                              ysize             = (nl + nAddLines) * map_info.ps[1], $
                              x0                = [0,0], $
                              y0                = [0, nl], $
                              background        = 0, $
                              use_see_through   = [0,0], $
                              r_fid             = mos_fid, $
                              out_name          = Tmp_Mos_File

      ;------------------------------
      ;--- Perform classification ---
      ;------------------------------
      
      case kernel_type of
      0: envi_doit, 'envi_svm_doit', $   
                  fid                   = mos_fid, $
                  pos                   = indgen(nb), $
                  dims                  = dims, $   
                  roi_ids               = aRoiIds, $
                  r_fid                 = Class_fid, $
                  rule_fid              = Rule_fid, $
                  out_name              = Class_File, $
                  rule_out_name         = Rule_File, $
                  penalty               = penalty, $
                  pyramid_levels        = pyramid_levels, $
                  pyramid_reclass_tresh = pyramid_reclass_tresh, $
                  tresh                 = tresh
                  
      1: envi_doit, 'envi_svm_doit', $   
                  fid                   = mos_fid, $
                  pos                   = indgen(nb), $
                  dims                  = dims, $   
                  roi_ids               = aRoiIds, $
                  r_fid                 = Class_fid, $
                  rule_fid              = Rule_fid, $
                  out_name              = Class_File, $
                  rule_out_name         = Rule_File, $
                  kernel_degree         = kernel_degree, $
                  kernel_bias           = kernel_bias, $
                  kernel_gamma          = kernel_gamma, $
                  penalty               = penalty, $
                  pyramid_levels        = pyramid_levels, $
                  pyramid_reclass_tresh = pyramid_reclass_tresh, $
                  tresh                 = tresh
    
      2: envi_doit, 'envi_svm_doit', $   
                  fid                   = mos_fid, $
                  pos                   = indgen(nb), $
                  dims                  = dims, $   
                  roi_ids               = aRoiIds, $
                  r_fid                 = Class_fid, $
                  rule_fid              = Rule_fid, $
                  out_name              = Class_File, $
                  rule_out_name         = Rule_File, $
                  kernel_gamma          = kernel_gamma, $
                  penalty               = penalty, $
                  pyramid_levels        = pyramid_levels, $
                  pyramid_reclass_tresh = pyramid_reclass_tresh, $
                  tresh                 = tresh
                  
      3: envi_doit, 'envi_svm_doit', $   
                  fid                   = mos_fid, $
                  pos                   = indgen(nb), $
                  dims                  = dims, $   
                  roi_ids               = aRoiIds, $
                  r_fid                 = Class_fid, $
                  rule_fid              = Rule_fid, $
                  out_name              = Class_File, $
                  rule_out_name         = Rule_File, $
                  kernel_bias           = kernel_bias, $
                  kernel_gamma          = kernel_gamma, $
                  penalty               = penalty, $
                  pyramid_levels        = pyramid_levels, $
                  pyramid_reclass_tresh = pyramid_reclass_tresh, $
                  tresh                 = tresh
                  
      else:
      endcase
    if keyword_set(Rule_fid) then  envi_file_mng, id=Rule_fid, /remove 

    Num_Classes = n_elements(aClass_Names)

    envi_setup_head, class_names    = aClass_Names, $
                        lookup      = aClass_Colors, $
                        file_type   = 3, $
                        num_classes = Num_Classes, $
                        data_type   = 1, $
                        interleave  = 0, $
                        nb          = 1, $
                        nl          = nl, $
                        ns          = ns, $
                        offset      = 0, $
                        map_info    = map_info, $
                        fname       = Class_File, $
                        /write
                        
    if keyword_set(tiff) then begin

      dims=[-1, 0, ns-1, 0, nl-1]

      ClassImage = envi_get_data(fid=Class_fid, pos=[0], dims=dims)

      R = bytarr(ns, nl)
      G = bytarr(ns, nl)
      B = bytarr(ns, nl)
        
      for Cix=0, Num_Classes-1 do begin
  
        i = where(ClassImage eq Cix, n)
    
        if n le 0 then continue
    
        R[i] = aCLass_Colors[0, Cix]
        G[i] = aCLass_Colors[1, Cix]
        B[i] = aCLass_Colors[2, Cix]
      endfor

      envi_enter_data, R, r_fid = R_Fid, map_info=map_info
      envi_enter_data, G, r_fid = G_Fid
      envi_enter_data, B, r_fid = B_Fid

      envi_doit, 'cf_doit', $
                    fid           = [R_Fid, G_Fid, B_Fid], $
                    pos           = [0,0,0], $
                    dims          = dims, $
                    out_dt        = 1, $  
                    remove        = 0, $
                    /in_memory, $
                    r_fid         = t_fid 
        
      envi_output_to_external_format, $
                    dims          = dims, $
                    fid           = t_fid, $
                    pos           = [0,1,2], $
                    out_name      = Class_File+'_.tiff', $
                    /tiff
                    
      envi_file_mng, id = R_Fid, /remove         
      envi_file_mng, id = G_Fid, /remove         
      envi_file_mng, id = B_Fid, /remove         
      envi_file_mng, id = t_fid, /remove         
    endif
                        
    envi_file_mng, id=ImFid, /remove
    envi_file_mng, id=roi_fid, /remove
    envi_file_mng, id=mos_fid, /remove, /delete           
    envi_file_mng, id=Class_fid, /remove                                             
  endfor
  
  oStatus->Log_Status, 'Ok'
end
