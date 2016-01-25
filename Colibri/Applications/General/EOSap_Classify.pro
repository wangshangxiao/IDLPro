;+
; NAME: EOSap_Classify
;
; PURPOSE: 
;
;   This application procedure performs image classification on a list of images by using a set of 'Regions 
;   Of Interest' (ROIs) as ground reference data. 
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
;   EOSap_Classify, pImageList, Class_method, Class_Dir [, AROICOLORS=variable] [, AROINAMES=variable]
;                   [, BANDSIX=array] [, KERNEL_TYPE=value] [, KERNEL_DEGREE=value] [, KERNEL_BIAS=value] 
;                   [, KERNEL_GAMMA=value] [, OSTATUS=object reference] [, /OVERWRITE] [, PENALTY=value], 
;                   PLIBRARIES=variable  [, PYRAMID_LEVELS=value] [, PYRAMID_RECLASS_TRESH=value] 
;                   [, RULE_DIR=string] [/TEST] [/TIFF] [, TRESH=value]
;                 
; ARGUMENTS
;  
;   pImageList:     A pointer to a string or array of strings holding the path and name of the images to be 
;                   classified.   
;   Class_method:   A string holding the classification method to be used for classifying the images. 
;                   Possible values are: 
;                       - LDA   Linear Discriminant Analysis 
;                       - SVM   Support Vector machine
;                       - PP    Parallelepiped 
;                       - MD    Minimum distance
;                       - ML    Maximum likelihood
;                       - SAM   Spectral angle mapper
;                       - MAH   Mahalanobis 
;                       - BE    Binary Encoding
;                       - SID   Spectral Information Divergence
;   Class_Dir:      A string holding the full path to the directory where the output results have to be 
;                   stored.
;                
; KEYWORDS
; 
;   ACLASS_COLORS:  A vector or array of vectors holding the RGB color values for the different classes.
;   ACLASS_NAMES:   A string or array of strings holding the names of the different classes.    
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
;   PLIBRARIES:     Set this keyword to a variable in which a pointer array is returned. Each pointer points 
;                   to a spectral library structure array with layout:
;                     - ImageFile:  a string holding the full path and name of the image file from which the 
;                                   spectrum was extracted.
;                     - RoiName:    a string holding the ROI name of the extracted spectrum.
;                     - Spectra:    a array holding the extracted spectrum.
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
;   TEST:           If set the default images, ROIs and output directory as specified in the initialization 
;                   part of the application procedure are used.
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
;   EOSocImages
;   EOSocLDA
;   EOScc_SVM
;   EOScc_ClassDoit
;         
; KNOWN ISSUES:
; 
;   None
;
; EXAMPLE:
; 
;   The default example can be run if no parameters are specified or if the keyword TEST is set. The default
;   example code will ask for three parameters:
;     - the directory location were the example data is located, i.e. the subdirectory '\_Examples\data' 
;       located in the installed directory 'ENVI-IDL Code Library';
;     - the directory location were the output results need to be written;
;     - the classification method to be applied.
;     
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
pro EOSap_Classify, $
          pImageList, $
          Class_method, $
          Class_Dir, $                    
          ; 
          BandsIx               = BandsIx, $     
          pLibraries            = pLibraries, $
          aClass_Names          = aClass_Names, $
          aCLass_Colors         = aCLass_Colors, $
          Rule_Dir              = Rule_Dir, $          
          ;
          oStatus               = oStatus, $
          ;
          kernel_type           = kernel_type, $
          kernel_degree         = kernel_degree, $
          kernel_bias           = kernel_bias, $
          kernel_gamma          = kernel_gamma, $
          penalty               = penalty, $
          pyramid_levels        = pyramid_levels, $
          pyramid_reclass_tresh = pyramid_reclass_tresh, $
          tresh                 = tresh, $
          ;      
          TIFF                  = TIFF, $
          TEST                  = TEST, $
          OVERWRITE             = OVERWRITE  
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSap_Classify'

  ;--------------------------------------------------------
  ;--- Creation and initialization of the status window ---
  ;--------------------------------------------------------
        
  oStatus = IOScc_Init(oStatus, $
                        Title="Image Classification", $
                        Log_DIRECTORY    = Class_Dir, $
                        Log_FILE_PREFIX  = 'Classify')    
                        
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
 
  if ~keyword_set(aCLass_Colors)  then Argument = 'aCLass_Colors'   
  if ~keyword_set(aClass_Names)   then Argument = 'aClass_Names' 
  if ~keyword_set(pLibraries)     then Argument = 'pLibraries'
  if ~keyword_set(Class_Dir)      then Argument = 'Class_Dir' 
  if ~keyword_set(Class_method)   then Argument = 'Class_method'   
  if ~keyword_set(pImageList)     then Argument = 'pImageList'  
  
  if keyword_set(Argument) then begin 
    
    oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log  
    
    if keyword_set(TEST) or n_params() eq 0 then begin
      if ptr_valid(pImageList) then ptr_free, pImageList
    endif
    
    return       
  endif

  case Class_method of
  
  'LDA': begin

    ;----------------------------------------------------------------
    ;--- Object: oLDA; Initialization with callibration libraries ---
    ;----------------------------------------------------------------
                         
    oLDA = obj_new('EOSocLDA', $
                pLibraries, $
                oStatus           = oStatus)                    

    ;-----------------------------------------------------
    ;--- Apply Linear Discriminant Analysis classifier ---
    ;-----------------------------------------------------

    oLDA->Classify, $
                pImageList, $
                BandsIx         = BandsIx, $
                Class_Dir       = Class_Dir, $
                Rule_Dir        = Rule_Dir, $
                aClass_Names    = aClass_Names, $
                aCLass_Colors   = aCLass_Colors, $
                aBand_Names     = aClass_Names, $
                TIFF            = TIFF , $
                OVERWRITE       = OVERWRITE   

    obj_destroy, oLDA  
  end
  
  'SVM': EOScc_SVM, $
                pImageList, $
                pLibraries, $
                Class_Dir, $
                aClass_Names    = aClass_Names, $
                aCLass_Colors   = aCLass_Colors, $
                aBand_Names     = aClass_Names, $        
                ;        
                oStatus         = oStatus, $
                BandsIx         = BandsIx, $
                Rule_Dir        = Rule_Dir, $
                TIFF            = TIFF , $
                OVERWRITE       = OVERWRITE, $
                ;
                kernel_type           = kernel_type, $
                kernel_degree         = kernel_degree, $
                kernel_bias           = kernel_bias, $
                kernel_gamma          = kernel_gamma, $
                penalty               = penalty, $
                pyramid_levels        = pyramid_levels, $
                pyramid_reclass_tresh = pyramid_reclass_tresh, $
                tresh                 = tresh
  
  else: EOScc_ClassDoit, $
                pImageList, $
                pLibraries, $
                Class_Dir, $
                Class_method, $
                aClass_Names    = aClass_Names, $
                aCLass_Colors   = aCLass_Colors, $
                aBand_Names     = aClass_Names, $
                ;
                oStatus         = oStatus, $                
                BandsIx         = BandsIx, $
                Rule_Dir        = Rule_Dir, $
                TIFF            = TIFF , $
                OVERWRITE       = OVERWRITE 
  endcase
  
  if keyword_set(TEST) or n_params() eq 0 then begin
    if ptr_valid(pImageList) then ptr_free, pImageList
  endif
end