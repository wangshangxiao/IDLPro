;+
; NAME: 
; 
;   EOSocImages::Calculate_Ndvi
;
; PURPOSE: 
; 
;   This function method is used to create NDVI images.
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
;   Obj->[EOSocImages::]Calculate_NDVI, SelectIx, BREDNIR=value, OUTPUT_DIR=string [, OVERWRITE]
; 
; ARGUMENTS
; 
;   SelectIx:     The image index or indices in the image list, which was specified during initialization of 
;                 the object, for which data has to be retrieved. If not specified the information of the 
;                 first image is returned.
; 
; KEYWORDS
; 
;   BREDNIR:      This keyword holds a two element array with the band indices of the red and NIR wavelength 
;                 band to be used for the NDVI calculation.
;   OUTPUT_DIR:   The directory where the created NDVI images need to be stored. The output NDVI image names 
;                 are named according the input image name with the suffix '_ndvi' added.
;   OVERWRITE:    Set this keyword to overwrite already existing output files.
;                      
; RETURN VALUE:
; 
;   None
;
; DEPENDENCIES:
;   
;   None
;       
; KNOWN ISSUES:
; 
;   None
;   
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, Februari, 2012.
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
pro EOSocImages::Calculate_NDVI, $
                  SelectIx, $
                  Output_Dir      = Output_Dir, $
                  bRedNIR         = bRedNIR, $
                  overwrite       = overwrite
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'EOSocImages::Calculate_Ndvi'
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(bRedNIR)    then Argument = 'bRedNIR'    
  if ~keyword_set(Output_Dir) then Argument = 'Output_Dir'
  
  if keyword_set(Argument) then begin 
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log          
    return
  endif

  ;------------------
  ;--- Processing ---
  ;------------------
  
  envi, /restore_base_save_files 
  
  if ~file_test(Output_Dir, /directory) then file_mkdir, Output_Dir
  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)
  
  ;-------------------------
  ;--- Handle all images ---
  ;-------------------------

  self.oStatus->Log_Status, 'Busy', 'Writing results to directory: '+Output_Dir, Module, /Log
  
  for Fix=0, n_elements(SelectIx)-1 do begin

    FileIx = SelectIx[Fix]

    self.oStatus->Progress_Set, slow=[Fix, n_elements(SelectIx)]

    InFile    = (*self.pImageInfo)[FileIx].ImageFile
    FileName  = file_basename(InFile, '.img')
    
    ;----------------------
    ;--- Claculate NDVI ---
    ;----------------------
    
    self.oStatus->Log_Status, 'Busy', 'Calculating ndvi image for: '+FileName, Module, /Log
  
    NdviBaseName  = FileName+'_ndvi'
    NdviFile      = Output_Dir+NdviBaseName
    descrip       = 'Ndvi image generated by EOSocImages'
    
    if ~keyword_set(overwrite) and file_test(NdviFile) eq 1 then begin

      self.oStatus->Log_Status, 'Warning', 'Ndvi image already exist: '+NdviBaseName, Module, /Log
        
    endif else begin
    
      ImFid = (*self.pImageInfo)[FileIx].Fid
  
      if ImFid eq -1 then $    
        envi_open_file, InFile, /no_realize, R_fid=ImFid
    
      Expr_ndvi = '(float(b2) - float(b1)) / (float(b2) + float(b1))'
    
      envi_doit, 'math_doit', $ 
                    fid       = [ImFid, ImFid], $
                    pos       = bRedNIR, $
                    dims      = (*self.pImageInfo)[FileIx].dims, $ 
                    exp       = Expr_ndvi, $
                    out_name  = NdviFile, $ 
                    r_fid     = ndvi_fid 

      envi_file_mng, id=ndvi_fid, /remove
      
      if (*self.pImageInfo)[FileIx].Fid eq -1 then $
        envi_file_mng, id=ImFid, /remove
    
      self.oStatus->Log_Status, 'Ok', 'Ndvi image created successfully: '+NdviBaseName, Module, /Log      
    endelse
  endfor

  self.oStatus->Progress_clear, /RESET  
end
