;+
; NAME: EOSap_SSIREE
;
; PURPOSE: 
;   
;   Spatial/Spectral Iterative Rule based Endmember Extraction.
;   This application procedure extracts the Spectrally Purest Pixels from a set of images.
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
;   EOSap_SSIREE, pEndmember_ImageList, Output_Dir, [, BANDSIX=array] [, /CLEANUP] [, /KEEP] 
;     [, MASK_DIR=string] [, MASK_SUFFIX=string] [NENDMEMBERS=value] [, OSTATUS=object reference] 
;     [, /OVERWRITE] [, PENDMEMBERS=variable][, PURITY_METHOD=string] [, BASENAME=string]
;            
; ARGUMENTS:
;   
;   pEndmember_ImageList: A pointer to a string or array of strings holding the path and name of the images 
;                         to be used for endmember selection.
;   Output_Dir:           A string holding the full path name of the directory where results should be stored.
;                
; KEYWORDS:
;
;   BANDSIX:              If present, an array of indices indicating the bands to be used in the 
;                         classification. If not present all bands will be used.
;   CLEANUP               Set this keyword to delete all endmemers which were already allocated.
;   KEEP:                 Set this keyword to keep all intermediate results. These intermediate results are
;                         written to the subdirectory 'SSIREE_Temp\' located in the specified output 
;                         directory.
;   MASK_DIR:             Set this keyword to a string holding the directory location where the mask images 
;                         are located. If specified the MASK_SUFFIX keyword must be specified as well. If a
;                         valid mask image is found endmembers will be searched for those pixels for which 
;                         the corresponding mask pixels have a non zero value. If not specified no mask 
;                         image will be taken into account.
;   MASK_SUFFIX:          Set this keyword to a string holding the suffix to be added to the image file name
;                         in order to obtain the mask file name. This keyword should be specified if the 
;                         MASK_DIR keyword is specified as well.
;   NENDMEMBERS:          Set this keyword to the number of endmembers to be searched for.
;   OSTATUS:              An object reference to an existing status window. If no valid object reference is
;                         specified the default status window will be created.
;   OVERWRITE:            If this keyword is set, existing intermediat results will be overwritten.
;   PENDMEMBERS:          Set this keyword to a variable in which a pointer is returned to a structure array
;                         holding the found endmembers. This structure has following lay-out:
;                           - ImageFile:  the file basename of the image where the endmember was found
;                           - RoiName:    the name which is assigned to the endmember
;                           - S:          the sample location of the centre pixel of the found endmember
;                           - L:          the line location of the centre pixel of the found endmember
;                           - nPixels:    the number of pixels allocated to the endmember (default 3x3)
;                           - ColorIx:    the graphics color index for the endmember
;                         If a valid pointer is present already holding an array of endmembers, it will be 
;                         concatinated with the newly found endmembers.
;   PURITY_METHOD:        Set this keyword to a string holding the method to be used for purity image 
;                         calculation. Possible values are 'RMSE' and 'SA'.
;   BASENAME:             Set this keyword to a string holding the basename for the endmembers.
;                      
; RETURN VALUE:
;   
;   None
;
; DEPENDENCIES:
; 
;  IOScc_Init.pro
;  IOSocStatus 
;  OSIocImages 
;  OSIocLDA
;         
; KNOWN ISSUES:
; 
;   None
;
;   The Spatial Spectral Iterative Rule based Endmember Extraction (SSIREE) algorithm, L. Bertels, Paper in 
;   preparation.
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, May, 2012.
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
pro EOSap_SSIREE, $
          pEndmember_ImageList, $
          Output_Dir, $
          ;
          oStatus                   = oStatus, $
          BandsIx                   = BandsIx, $
          Purity_Method             = Purity_Method, $
          nEndmembers               = nEndmembers, $
          Mask_Dir                  = Mask_Dir, $
          Mask_Suffix               = Mask_Suffix, $
          BaseName                  = BaseName, $
          pEndMembers               = pEndMembers, $
          CLEANUP                   = CLEANUP, $
          KEEP                      = KEEP, $        
          OVERWRITE                 = OVERWRITE
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module    = 'EOSap_SSIREE'
  Argument  = ''                              

  if ~keyword_set(Purity_Method)  then Purity_Method = 'RMSE'
  if ~keyword_set(NDVI_threshold) then NDVI_threshold = 0.0001

  ;---------------------------------------------------------------------------
  ;--- Creation and initialization of the status window if not yet present ---
  ;---------------------------------------------------------------------------
   
  oStatus = IOScc_Init(oStatus, $
                            Title = 'Spatial Spectral Iterative Rule based Endmember Extraction (SSIREE)', $
                            Log_Directory     = Output_Dir, $
                            Log_File_Prefix   = 'SSIREE_log')

  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(nEndmembers)          then Argument = 'nEndmembers'
  if ~keyword_set(Output_Dir)           then Argument = 'Output_Dir' 
  if ~keyword_set(pEndmember_ImageList) then Argument = 'pEndmember_ImageList'  
  
  if keyword_set(Argument) then begin 
    oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log  
    return       
  endif
  
  ;----------------------
  ;--- Initialization ---
  ;---------------------- 
  
  ;- Definition of temporary directories
  
  Temp_Dir     = Output_Dir+'SSIREE_Temp\'

  tDir_Purity  = Temp_Dir+ 'purity\'
  Dir_Class    = Temp_Dir+ 'classified\'
  tDir_Rule    = Temp_Dir+ 'rule\'
   
  Roi_Dir      = Output_Dir+'Roi\'

  nImages           = n_elements(*pEndmember_ImageList)  

  ;- Restore endmembers when already present
  if ptr_valid(pEndMembers) then begin
  
    nROIs             = n_elements(*pEndMembers)
    ColorIndex        = max((*pEndMembers).ColorIx)
  endif else begin

    nROIs             = 0       ; the number of found ROIs
    ColorIndex        = 1       ; color index for the first ROI  
  endelse

  ;---------------------------------------------------------
  ;--- Creation and Initialization of the oImages object ---
  ;---------------------------------------------------------

  oImages = obj_new('EOSocImages', $
                          pEndmember_ImageList, $ 
                          oStatus               = oStatus)
  
  if ~oStatus->Check('Ok') then return

  ;-----------------------------------
  ;--- Calculation of purity image ---
  ;-----------------------------------
                          
  oImages->Calculate_Purity, $
                          Output_Dir            = tDir_Purity, $
                          Purity_Method         = Purity_Method, $
                          Radius                = 1, $
                          Overwrite             = Overwrite
                          
  if ~oStatus->Check('Ok') then return                        

  ;----------------------------------------------------
  ;--- Delete all allocated endmembers if requested ---
  ;----------------------------------------------------

  if keyword_set(CLEANUP) then oImages->Delete_Roi, Roi_Dir=Roi_Dir, /All           

  ;------------------
  ;--- Processing ---
  ;------------------

  for Ix=0, nEndmembers-1 do begin
    
    if Ix eq 0 then Rule_Dir = '' else Rule_Dir = tDir_Rule
    
    ;--------------------------------------------
    ;--- Object:oPurity; Extract purest pixel ---
    ;--------------------------------------------

    oImages->Get_Purest, $
                Purity_Dir        = tDir_Purity, $
                Purity_Method     = Purity_Method, $            
                Rule_Dir          = Rule_Dir, $
                Mask_Dir          = Mask_Dir, $
                Mask_Suffix       = Mask_Suffix, $  
                pEndMembers       = pEndMembers, $
                Purest            = Purest    
    
    if ~oStatus->Check('Ok') then return  
                
    RoiName = BaseName+strtrim(string(++nROIs),2)
  
    ;------------------------------------------------------------------------
    ;--- Object: oImages; Define new ROI point and extend with 3x3 canvas ---
    ;------------------------------------------------------------------------
  
    oImages->Add_New_Roi, $
                Purest.ImageFile, $                 ; Image
                RoiName, $                          ; Roi name
                ++ColorIndex, $                     ; Color index
                Samples           = Purest.S, $
                Lines             = Purest.L, $
                Radius            = 1, $            ; default 3x3 pixel ROI
                Roi_Dir           = Roi_Dir
 
    ;----------------------------------------------------
    ;--- Object: oImages; Extract libraries from ROIs ---
    ;----------------------------------------------------
   
    oImages->Extract_Libraries_From_Rois, $
                Roi_Dir, $
                BandsIx           = BandsIx, $ 
                pLibraries        = pLibraries,$
                aRoiNames         = aRoiNames, $
                aRoiColors        = aRoiColors
                         
    if ~oStatus->Check('Ok') then return 
  
    ;---------------------------------------------------------------------
    ;--- First iteration: devide purest ROI into two quasi equal parts ---
    ;---------------------------------------------------------------------
  
    if Ix eq 0 then begin
      
      iLib        = (where(aRoiNames eq RoiName, C))[0]
      
      n           = n_elements((*pLibraries[iLib]))        ; number of spectra in first library
      pCalLibs    = ptrarr(2)
      pCalLibs[0] = ptr_new((*pLibraries[iLib])[0:n/2-1])
      pCalLibs[1] = ptr_new((*pLibraries[iLib])[n/2:n-1])
      
      aRoiNames   = [RoiName, 'RoiTmp']
      aRoiColors  = [[255,0,0], [0,255, 0]]
    endif else begin
    
      i = where(strmatch(aRoiNames, BaseName+'*') eq 1, C)    
     
      pCalLibs    = pLibraries[i]
      aRoiNames   = aRoiNames[i]
      aRoiColors  = aRoiColors[*,i]
    endelse
  
    aClass_Names    = ['UnClassified', aRoiNames]
    aCLass_Colors   = [[0,0,0], [aRoiColors]]    
              
    ;------------------------------------
    ;--- Perform Image Classification ---
    ;------------------------------------
    
    EOSap_Classify, pEndmember_ImageList, $  
                'LDA', $     
                Dir_Class, $                 
                pLibraries        = pCalLibs, $
                aClass_Names      = aClass_Names, $
                aCLass_Colors     = aCLass_Colors, $
                oStatus           = oStatus, $
                BandsIx           = BandsIx, $
                Rule_Dir          = tDir_Rule, $
                /overwrite        
 
    if ~oStatus->Check('Ok') then return  

    ;---------------------------------------------------------
    ;--- Object: oImages; Perform endmember region growing ---
    ;---------------------------------------------------------
    
    EndMember = {ImageFile: file_basename(Purest.ImageFile, '.img'), $
                 RoiName: RoiName, $
                 S: Purest.S, $
                 L: Purest.L, $
                 nPixels: 9, $
                 ColorIx: ColorIndex}
        
    if ~ptr_valid(pEndMembers) then pEndMembers = ptr_new(EndMember) else $
      *pEndMembers = [*pEndMembers, EndMember]

    if ~keyword_set(aEndMembers) then begin
      
      aEndMembers       = '  '+RoiName+'_'+ strtrim(string(Purest.S),2)+'_'+ strtrim(string(Purest.L),2)
      aEndMember_Files  = '  '+file_basename(Purest.ImageFile)
      aEndMember_Pixels = '  9'
    endif else begin

      aEndMembers       = [aEndMembers, '  '+RoiName+'_'+ strtrim(string(Purest.S),2)+'_'+ strtrim(string(Purest.L),2)]
      aEndMember_Files  = [aEndMember_Files, '  '+file_basename(Purest.ImageFile)]
      aEndMember_Pixels = [aEndMember_Pixels, '  9']
    endelse
  endfor

  ;- Remove all ROIs from memory.
  oImages->Delete_Roi, /All

  obj_destroy, oImages

  ;--- Report endmembers ---
  oStatus->Log_Status, 'Ok', 'Found '+strtrim(string(n_elements(*pEndMembers)),2)+' endmembers:', Module, /Log 
  
  Info    = string(['ROI Id', 'File name', 'ROI Name', 'Sample', 'Line', 'N pixels', 'Color Ix'], $
            Format = '(A-10, A-15, A-15, A-10, A-10, A-10, A-10)')
  
  oStatus->Log_Status, 'Ok', Info, Module, /Log 
  
  Format  = '(I-10, A-15, A-15, I-10, I-10, I-10, I-10 )'
  
  for Ix=0, n_elements(*pEndMembers)-1 do begin

    Info  = string(Ix+1, (*pEndMembers)[Ix], format=Format)
    oStatus->Log_Status, 'Ok', Info, Module, /Log 
  endfor

  oStatus->Log_Status, 'Ok', 'Processing finished.', Module, /Log
  
  if ~keyword_set(KEEP) then File_Delete, Temp_Dir, /recursive
end  
