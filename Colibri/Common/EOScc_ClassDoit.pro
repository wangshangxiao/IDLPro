;+
; NAME: EOScc_ClassDoit
;
; PURPOSE: 
;
;   This common procedure performs supervised image classification using the procedure 'CLASS_DOIT' on a list
;   of images by using a set of 'Regions Of Interest' (ROIs) as ground reference data.
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
;   EOScc_SClassDoit, pImageList, pLibraries, Class_Dir, Class_method, ACLASS_NAMES=string array, 
;              ACLASS_COLORS=array, ABAND_NAMES=string array [, BANDSIX=array] [, OSTATUS=object reference]
;              [, /OVERWRITE] [, RULE_DIR=string] [, /TIFF]  
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
;   Class_method:   A string holding the classification method to be used for classifying the images. 
;                   Possible values are: 
;                       - PP    Parallelepiped 
;                       - MD    Minimum distance
;                       - ML    Maximum likelihood
;                       - SAM   Spectral angle mapper
;                       - MAH   Mahalanobis 
;                       - BE    Binary Encoding
;                       - SID   Spectral Information Divergence          
;                
; KEYWORDS
;   
;   ACLASS_NAMES:   A named variable that holds an array of strings containing the class names for the 
;                   available classes.
;   ACLASS_COLORS:  A named variable that holds an array of RGB color values containing the class colors for 
;                   the available classes.
;   ABAND_NAMES:    A named variable that holds an array of strings containing the names for the different 
;                   bands of the rule image.
;   OSTATUS:        An object reference to an existing status window.
;   BANDSIX:        If present, an array of indices indicating the bands to be used in the classification. 
;                   If not present all bands will be used.
;   RULE_DIR:       If present, a string holding the output directory where the rule images need to be stored.
;                   The directory will be created if it is not yet existing. The classified images are named 
;                   after the input image suffixed with '_rule'.
;   TIFF:           Set this keyword to output the classified images in tiff format.
;   OVERWRITE:      Set this keyword to overwrite already existing output files.                                           
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
;ToDo: 
;   - Several classification method specific parameters are not yet implemented.
;   - The code for TIFF output is not yet implemented.
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
pro EOScc_ClassDoit, $
                pImageList, $
                pLibraries, $
                Class_Dir, $
                Class_method, $
                aClass_Names          = aClass_Names, $
                aCLass_Colors         = aCLass_Colors, $
                aBand_Names           = aBand_Names, $
                ;
                oStatus               = oStatus, $
                BandsIx               = BandsIx, $
                Rule_Dir              = Rule_Dir, $
                TIFF                  = TIFF , $
                OVERWRITE             = OVERWRITE 
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module  = 'EOScc_ClassDoit'

  envi, /restore_base_save_files  
  
  ;--------------------------------------------------------
  ;--- Creation and initialization of the status window ---
  ;--------------------------------------------------------
        
  oStatus = IOScc_Init(oStatus, $
                        Title="Image Classification using CLASS_DOIT", $
                        Log_DIRECTORY    = 'D:/log/', $
                        Log_FILE_PREFIX  = 'Classify')    
                        
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(aBand_Names)    then Argument = 'aBand_Names' 
  if ~keyword_set(aCLass_Colors)  then Argument = 'aCLass_Colors' 
  if ~keyword_set(aClass_Names)   then Argument = 'aClass_Names'   
  if ~keyword_set(Class_method)   then Argument = 'Class_method'   
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
 
  case Class_method of
  'PP'  : Method = 0    ; Parallelepiped 
  'MD'  : Method = 1    ; Minimum distance
  'ML'  : Method = 2    ; Maximum likelihood
  'SAM' : Method = 3    ; Spectral angle mapper
  'MAH' : Method = 5    ; Mahalanobis 
  'BE'  : Method = 6    ; Binary Encoding
  'SID' : Method = 8    ; Spectral Information Divergence
  else: begin
    oStatus->Log_Status, 'Warning', 'Classification method not recognized: '+string(Class_method), /Log  
    return
  end
  endcase
  
  nImages     = n_elements(*pImageList)
  nClasses    = n_elements(pLibraries)
  nb          = n_elements((*pLibraries[0])[0].Spectra)
  aMean       = dblarr(nb, nClasses)

  for Ix=0, nClasses-1 do aMean[*, Ix] = total((*pLibraries[Ix]).Spectra,2) / n_elements(*pLibraries[Ix])

  ;------------------
  ;--- Processing ---
  ;------------------
  
  for FileIx=0, nImages-1 do begin
    
    InFile      = (*pImageList)[FileIx]
    ImageName   = file_basename(InFile, '.img')
    
    if keyword_set(Class_Dir) then begin $ 
      Class_File  = Class_Dir + ImageName + '_'+Class_method

      if ~keyword_set(overwrite) and $
        file_test(Class_File) eq 1 then continue
    endif
          
    if keyword_set(Rule_Dir) then $
          Rule_File   = Rule_Dir  + ImageName +'_rule';+ '_' + Class_method+'_rule'
  
    envi_open_file, InFile, r_fid=ImFid  
    envi_file_query, ImFid, dims=dims, nb=nb, ns=ns, nl=nl, data_type=dt 
    
    if ~keyword_set(BandsIx) then BandsIx = indgen(nb)

    if keyword_set(Rule_Dir) then begin
  
      envi_doit, 'class_doit', $   
                  fid             = ImFid, $
                  method          = method, $
                  class_names     = aClass_Names, $
                  lookup          = aClass_Colors, $
                  mean            = aMean, $
                  pos             = (indgen(nb))[BandsIx], $
                  dims            = dims, $   
                  r_fid           = Class_fid, $
                  rule_fid        = Rule_fid, $
                  out_name        = Class_File, $
                  rule_out_name   = Rule_File
                  
      envi_file_mng, id=Rule_fid, /remove 
    endif else begin
      if keyword_set(Class_Dir) then $
        envi_doit, 'class_doit', $   
                  fid             = ImFid, $
                  method          = method, $
                  class_names     = aClass_Names, $
                  lookup          = aClass_Colors, $                  
                  mean            = aMean, $
                  pos             = (indgen(nb))[BandsIx], $
                  dims            = dims, $   
                  r_fid           = Class_fid, $
                  out_name        = Class_File
    endelse
                        
    envi_file_mng, id=ImFid, /remove        
    envi_file_mng, id=Class_fid, /remove                                             
  endfor

end