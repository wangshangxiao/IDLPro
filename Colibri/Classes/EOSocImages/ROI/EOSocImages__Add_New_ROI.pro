;+
; NAME:
; 
;   EOSocImages::Add_New_Roi
;
; PURPOSE:
; 
;   This procedure method will define a new ROI according to the parameters passed.
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
;   Obj->[EOSocImages::]Add_New_Roi, ImageFile, Roi_Color, Roi_Name, LINES=array [, RADIUS=value], 
;     ROI_DIR=string, SAMPLES=array                  
;
; ARGUMENTS
; 
;   ImageFile:    A string holding the image full path and name for which the new ROI has to be created.
;   Roi_Color:    The color index of the ROI to be created.
;   Roi_Name:     A string holding the name of the ROI to be created.
;
; KEYWORDS
; 
;   LINES:        Set this keyword to an array holding the pixel's line locations for the ROI to be created.
;   RADIUS:       Set this keyword to the size by which the single point ROI has to be extended.
;   ROI_DIR:      This keyword holds a string containing the directory where the ROI files have to be written.
;                 if this keyword is not specified no output ROI file while be written.
;   SAMPLES:      Set this keyword to an array holding the pixel's sample locations for the ROI to be created.           
;                      
; DEPENDENCIES:
; 
;   None.
;       
; KNOWN ISSUES:
;
;   None.
;
; EXAMPLE:
; 
;   An extensive example can be found in the examples subdirectory: IOSex_ROI_Create
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
pro EOSocImages::Add_New_Roi, $
              ImageFile, $
              Roi_Name, $
              Roi_Color, $
              Samples         = Samples, $
              Lines           = Lines, $
              Radius          = Radius, $
              Roi_Dir         = Roi_Dir
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
   
  Module = 'EOSocImages::Add_New_Roi'
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------

;  if ~keyword_set(Roi_Dir)        then Argument = 'Roi_Dir'  
  if ~keyword_set(Lines)          then Argument = 'Lines'    
  if ~keyword_set(Samples)        then Argument = 'Samples'
  if ~keyword_set(Roi_Color)      then Argument = 'Roi_Color'  
  if ~keyword_set(Roi_Name)       then Argument = 'Roi_Name'
  if ~keyword_set(ImageFile)      then Argument = 'ImageFile'
  
  if keyword_set(Argument) then begin
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log                
    return
  endif
    
  ;----------------------
  ;--- Initialization ---
  ;----------------------
    
  FileIx = where((*self.pImageInfo).ImageFile eq ImageFile, C)
                            
  ns    = (*self.pImageInfo)[FileIx].ns
  nl    = (*self.pImageInfo)[FileIx].nl
  
  ;----------------------------------------------------------------------
  ;--- If requested expand the single pixel ROI by the requested size ---
  ;----------------------------------------------------------------------
                                
  if keyword_set(Radius) and n_elements(Samples) eq 1 then begin
  
    d   = Radius*2 + 1
    t   = indgen(d) - Radius
    i   = t * 0 + 1
    kS  = i ## t
    kL  = transpose(kS)
    
    S   = Samples + kS
    L   = Lines + kL
    
    i   = indgen(n_elements(S))

    Samples = S[i] 
    Lines   = L[i]    
    
    i   = where(Samples ge 0 and Samples lt ns and Lines ge 0 and Lines lt nl, C)
    
    if C le 0 then return
    
    Samples = Samples[i]
    Lines   = Lines[i]
  endif

  ;---------------------------------------------
  ;--- Create the requested ROI if requested ---
  ;---------------------------------------------
  
  if keyword_set(Roi_Dir) then begin
    
    if ~file_test(Roi_Dir, /directory) then file_mkdir, Roi_Dir
    
    RoiFileName = Roi_Dir+file_basename(ImageFile, '.img')+'.roi' 
    
    envi_delete_rois, /all  
    
    ImFid = (*self.pImageInfo)[FileIx].Fid
  
    if ImFid eq -1 then $     
      envi_open_file, ImageFile, /no_realize, R_fid=ImFid
  
    if file_test(RoiFileName) eq 1 then envi_restore_rois, RoiFileName
  
    RoiId = envi_create_roi( color  = Roi_Color, $
                            name  = Roi_Name, $
                            ns    = ns, $
                            nl    = nl)
                            
    envi_define_roi, RoiId, /point, xpts=Samples, ypts=Lines  

    self.oStatus->Log_Status, 'Busy', 'ROI created: '+Roi_Name+', containing '+ $
      strtrim(string(n_elements(Samples)),2)+' pixels.', Module, /Log      

    RoiIds  = envi_get_roi_ids($
                              fid         = ImFid, $
                              roi_colors  = roi_colors, $
                              roi_names   = roi_names, $
                              /short_name)
                              
    ;--- Remove dummy ROI if existing  
    i = where(roi_names eq 'Region #1', C, Complement=iValid, nComplement = nValid)

    if C gt 0 then begin    
      RoiIds      = RoiIds[iValid]
      roi_colors  = roi_colors[*, iValid]
      Roi_Names   = Roi_Names[iValid]
    endif    
  
    ;----------------------------
    ;--- Save new Roi to file ---
    ;----------------------------
    self.oStatus->Log_Status, 'Busy', 'ROI file created: '+RoiFileName, Module, /Log  
  
    envi_save_rois, RoiFileName, RoiIds
    
    if (*self.pImageInfo)[FileIx].Fid eq -1 then $
      envi_file_mng, id=ImFid, /remove    
  endif
end