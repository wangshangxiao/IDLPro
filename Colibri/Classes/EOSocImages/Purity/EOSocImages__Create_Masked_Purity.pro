;+
; NAME: 
; 
;   EOSocImages::Create_Masked_Purity
;
; PURPOSE: 
; 
;   This procedure method will create masked purity images. 
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
;   Obj->[EOSocImages::]Create_Masked_Purity, SelectIx [, /OVERWRITE], MASK_DIR=string, MASK_SUFFIX=string, 
;     PURITY_DIR=string, PURITY_SUFFIX= string   
;                   
; 
; ARGUMENTS
; 
;   SelectIx:       The image index or indices in the image list, which was specified during initialization of 
;                   the object, for which the masked purity image has to be created. If not specified all 
;                   images are used.  
; 
; KEYWORDS
; 
;   OVERWRITE:      Set this keyword to overwrite already existing masked purity images.
;   MASK_DIR:       Set this keyword to a string holding the directory were the masks are located.
;   MASK_SUFFIX:    Set this keyword to a string holding the suffix for the mask file to be applied. The mask
;                   file basename should be identical to the input image.
;   PURITY_DIR:     Set this keyword to a string holding the directory were the purity images are located.
;   PURITY_SUFFIX:  Set this keyword to a string holding the suffix of the purity file to be masked. The 
;                   purity file basename should be identical to the input image.
;
; DEPENDENCIES:
; 
;   None
;       
; KNOWN ISSUES:
;
;   None.
;   
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, February, 2012.
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
pro EOSocImages::Create_Masked_Purity, $
                  SelectIx, $
                  Purity_Dir          = Purity_Dir, $
                  Purity_Suffix       = Purity_Suffix, $   
                  Mask_Dir            = Mask_Dir, $
                  Mask_Suffix         = Mask_Suffix, $               
                  overwrite           = overwrite
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'EOSocImages::Create_Masked_Purity'
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------

  if ~keyword_set(Mask_Suffix)        then Argument = 'Mask_Suffix'  
  if ~keyword_set(Mask_Dir)           then Argument = 'Mask_Dir'  
  if ~keyword_set(Purity_Suffix)      then Argument = 'Purity_Suffix'    
  if ~keyword_set(Purity_Dir)         then Argument = 'Purity_Dir'
  
  if keyword_set(Argument) then begin
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log                
    return
  endif
  
  if ~file_test(Purity_Dir, /directory) then begin    
    self.oStatus->Log_Status, 'Error', 'Purity directory not found.', Module, /Log               
    return  
  endif
  
  ;------------------
  ;--- Processing ---
  ;------------------
    
  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)
    
  envi, /restore_base_save_files 
  
  ;-------------------------
  ;--- Handle all images ---
  ;-------------------------
  
  self.oStatus->Log_Status, 'Busy', 'Writing results to directory: '+Purity_Dir, Module, /Log
    
  for Fix=0, n_elements(SelectIx)-1 do begin

    FileIx = SelectIx[Fix]

    self.oStatus->Progress_Set, slow=[Fix, n_elements(SelectIx)]

    ;---------------------------------------
    ;--- Check if all images are present ---
    ;---------------------------------------

    InFile          =  (*self.pImageInfo)[FileIx].ImageFile
    FileName        = file_basename(InFile, '.img')
    MaskFile        = Mask_Dir + FileName + Mask_Suffix
    PurityFile      = Purity_Dir + FileName + Purity_suffix
    MkdFileBaseName = FileName + Purity_suffix + Mask_Suffix
    MaskedFile      = Purity_Dir + MkdFileBaseName
    
    self.oStatus->Log_Status, 'Busy', $
      'Calculating masked purity images for: '+FileName, $
      Module, /Log 

    if ~file_test(PurityFile) then begin       
      self.oStatus->Log_Status, 'Warning', 'Purity file not found: '+PurityFile, Module, /Log       
      continue
    endif

    if ~file_test(MaskFile) then begin    
      self.oStatus->Log_Status, 'Error', 'Mask file not found: '+MaskFile, Module, /Log        
      continue
    endif

    Mask_descrip  = 'Masked purity image generated by: ' + Module
    
    if ~keyword_set(overwrite) and $
        file_test(MaskedFile) eq 1 then continue

    envi_open_file, MaskFile, /no_realize,  R_fid=Msk_Fid
    envi_open_file, PurityFile, /no_realize, R_fid=Pty_Fid

    Expr_Mask    = '(b1) * float(b2)/b2'
    
    envi_doit, 'math_doit', $ 
                    fid       = [Pty_Fid, Msk_Fid], $
                    pos       = [0, 0], $
                    dims      = (*self.pImageInfo)[FileIx].dims, $ 
                    exp       = Expr_Mask, $
                    out_name  = MaskedFile, $ 
                    r_fid     = Masked_fid 

    envi_file_mng, id=Msk_Fid, /remove
    envi_file_mng, id=Pty_Fid, /remove
    envi_file_mng, id=Masked_fid, /remove
    
    self.oStatus->Log_Status, 'Ok', 'Masked purity image created successfully: ' + MkdFileBaseName, $
      Module, /Log    
  endfor

  self.oStatus->Progress_clear, /RESET  
end 