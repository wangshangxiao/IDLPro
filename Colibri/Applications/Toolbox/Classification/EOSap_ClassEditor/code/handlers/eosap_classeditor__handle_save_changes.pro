;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_Save_Changes
;
; PURPOSE:
; 
;   This procedure method is used to save the changed classified image to the output file specified.
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
;   Obj->[EOSap_ClassEditor::]handle_save_changes
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   None
; 
; DEPENDENCIES:
;   
; KNOWN ISSUES:
; 
;   None.
;   
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, June 2010.
; - Updated for the Colibri release, February 2013.
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
pro EOSap_ClassEditor::Handle_Save_Changes
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if strlen(self.Class_Output_File) le 3 then begin
    self.oStatus->Log_Status, 'Warning', 'Specify output filename'
    return
  endif 

  if ~obj_valid(self.oClassImage) then return

  self.oClassImage->get, ns=ns, nl=nl, pMapInfo=pMapInfo, dims=dims

  OutImage        = *self.pClassImage
  NewClassesIds   = OutImage[UNIQ(OutImage, SORT(OutImage))]
  NewClassNames   = (*self.pClassNames)[NewClassesIds]  
  
  i = where(NewClassesIds eq 0, C)
  if C eq 0 then NewClassNames = ['Unclassified', NewClassNames]
  
  NumClasses = n_elements(NewClassNames)
  
  openw, lun, self.Class_Output_File, /get_lun
  writeu, lun, OutImage
  free_lun, lun

  envi_setup_head, $
    class_names   = NewClassNames, $
    descrip       = 'Classification edited with EOSap_ClassEditor. ', $
    Data_type     = 2, $
    File_type     = 3, $
    map_info      = *pMapInfo, $
    Num_classes   = NumClasses, $
    r_fid         = mFid_Class_Edit, $
    nb            = 1, $
    nl            = nl, $
    ns            = ns, $
    interleave    = 0, $
    Lookup        = *self.pLookUp, $
    fname         = self.Class_Output_File, /write

end