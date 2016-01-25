;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_Delete_New_Class
;
; PURPOSE:
; 
;   This procedure method is used to delete a previously added new class from the available classes list.
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
;   Obj->[EOSap_ClassEditor::]handle_delete_new_class
;
; ARGUMENTS 
; 
;  None
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
pro EOSap_ClassEditor::Handle_Delete_New_Class
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~obj_valid(self.oClassImage) then return
  
  widget_control, widget_info(self.wWidget, find_by_uname='tClass_Name'), get_value = Delete_Class_Name

  if strlen(Delete_Class_Name[0]) le 0 then return

  NumClasses = n_elements(*self.pClassNames)

  i = where( (*self.pOriginalClassNames) eq Delete_Class_Name[0], C)

  if C gt 0 then begin
      self.oStatus->Log_Status, 'Warning', 'Original class can not be deleted: '''+Delete_Class_Name+'''
      return  
  endif

  i = where( (*self.pClassNames) eq Delete_Class_Name[0], C, complement=iKeep, nComplement=nKeep)

  if ~C then begin
      self.oStatus->Log_Status, 'Warning', 'Class name '''+Delete_Class_Name+''' not found!'
      return
  endif 
  
  *self.pClassNames       = (*self.pClassNames)[iKeep]
  *self.pLookUp           = (*self.pLookUp)[*, iKeep]
  
  Ix = where(*self.pClassImage eq i[0], C)
  
  if C gt 0 then (*self.pClassImage)[Ix] = (*self.pRGBdata)[Ix]
    
  self->Update_Available_Classes  
  self.oImageViewer->Update_view, self.pClassImage, pLookup=self.pLookUp

  widget_control, widget_info(self.wWidget, find_by_uname='tClass_Name'), set_value = ''
  widget_control, widget_info(self.wWidget, find_by_uname='tChanged_To'), set_value = ''
  
  self.Selected_Available_Id = -1
  
  self.oStatus->Log_Status, 'Warning', 'Class name '''+Delete_Class_Name+'''deleted'
end