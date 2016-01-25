;+
; NAME:
; 
;   EOSocImages::Delete_Roi
;
; PURPOSE:
; 
;   This example procedure demonstrates the use of the class "EOSocImages::Delete_ROI" for deleting ROIs.
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
;   Obj->[EOSocImages::]Delete_Roi [, /ALL] [, IMAGEFILE=variable] [, ROI_DIR=string] [, ROI_NAMES=variable]
;
; ARGUMENTS
; 
;   None.
;
; KEYWORDS
; 
;   ALL:            Set this keyword to delete all ROIs from memory. If the ROI_DIR keyword is specified all
;                   ROI files from this directory are deleted.
;   IMAGEFILE:      Set this keyword to a string or array of strings holding the name(s) of the image files
;                   for which the ROIs have to be deleted.
;   ROI_DIR:        Set this keyword to a string holding the directory where the ROI files are located.
;   ROI_NAMES:      Set this keyword to a string or array of strings holding the ROI names which have to be
;                   deleted.  
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
;   An extensive example can be found in the examples subdirectory: IOSex_ROI_Delete
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
pro EOSocImages::Delete_Roi, $
              ImageFile   = ImageFile, $
              Roi_Dir     = Roi_Dir, $
              Roi_Names   = Roi_Names, $
              All         = All
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'EOSocImages::Delete_Roi'

  if keyword_set(All) then begin
  
    self.oStatus->Log_Status, 'Busy', 'All ROIs deleted from memory. ', Module, /Log 
   
    ;-----------------------
    ;--- Delete all ROIs ---
    ;-----------------------
  
    envi_delete_rois, /all
  
    if keyword_set(Roi_Dir) then begin
    
      ;-- Delete all ROI files from the ROI directory
      if file_test(Roi_Dir) then begin
        self.oStatus->Log_Status, 'Busy', 'ROI files deleted: ', Module, /Log 
        self.oStatus->Log_Status, 'Busy', 'RoiFiles, Module, /Log 
        
        RoiFiles = file_search(Roi_Dir+'*.roi', count = nRoiFiles)
        if nRoiFiles gt 0 then file_delete, RoiFiles
      endif
    endif
    
    return
  endif
  
  ;----------------------------
  ;--- Delete specific ROIs ---
  ;----------------------------
  
  if keyword_set(Roi_Names) and $
     keyword_set(ImageFile) and $
     keyword_set(Roi_Dir) then begin
  
    RoiFile = Roi_Dir+file_basename(ImageFile, '.img')+'.roi'
    
    if ~file_test(RoiFile) then begin

      self.oStatus->Log_Status, 'Warning', 'ROI file not found: '+RoiFile, Module, /Log 
      return
    endif
    
    FileIx = where((*self.pImageInfo).ImageFile eq ImageFile, C)

    ImFid = (*self.pImageInfo)[FileIx].Fid
  
    if ImFid eq -1 then $
      envi_open_file, ImageFile, /no_realize, R_fid=ImFid
    
    envi_delete_rois, /all
    envi_restore_rois, RoiFile
    
    AllRoiIds  = envi_get_roi_ids(fid = ImFid, roi_names = AllRoiRames, /short_name)
    
    if AllRoiIds[0] eq -1 then return
    
    for Ix=0, n_elements(Roi_Names)-1 do begin
    
      iRemove = where(AllRoiRames eq Roi_Names[Ix], Found, Complement=iKeep, nComplement=nKeep)
      
      if Found eq 1 then begin
        envi_delete_rois, AllRoiIds[iRemove]
            
        self.oStatus->Log_Status, 'Busy', 'ROI deleted for image: '+ImageFile, Module, /Log 
        self.oStatus->Log_Status, 'Busy', 'Deleted ROI: '+Roi_Names[Ix], Module, /Log 
      endif
      
      if nKeep gt 0 then begin
        AllRoiRames = AllRoiRames[iKeep]
        AllRoiIds   = AllRoiIds[iKeep]
      endif
    endfor
    
    if nKeep gt 0 then begin
      envi_save_rois, RoiFile, AllRoiIds
    endif else begin
      file_delete, RoiFile
    endelse

    if (*self.pImageInfo)[FileIx].Fid eq -1 then $
      envi_file_mng, id=ImFid, /remove 
  endif
end  