;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_Add_New_Class
;
; PURPOSE:
; 
;   This procedure method is used to add a new class to the available classes list.
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
;   Obj->[EOSap_ClassEditor::]handle_add_new_class
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
pro EOSap_ClassEditor::Handle_Add_New_Class
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~obj_valid(self.oClassImage) then return
  
  widget_control, widget_info(self.wWidget, find_by_uname='tClass_Name'), get_value = New_Class_Name

  if strlen(New_Class_Name) le 0 then begin
    self.oStatus->Log_Status, 'Warning', 'Missing New Class Name'
    return
  endif

  NumClasses = n_elements(*self.pClassNames)

  for Ix=0, NumClasses-1 do begin

    if (*self.pClassNames)[Ix] eq New_Class_Name then begin

      self.oStatus->Log_Status, 'Warning', 'New class name '''+New_Class_Name+''' already exists!'
      return
    endif
  endfor

  base = widget_auto_base(title='Specify New Class Color')
  we = widget_rgb(base, index=0, uvalue='rgb', /bit_24, /auto)
  result = auto_wid_mng(base)
  if (result.accept eq 0) then return

  NewColor = result.rgb

  for Ix=0, NumClasses-1 do begin

    if array_equal((*self.pLookUp)[Ix], NewColor) eq 1 then begin
      
      self.oStatus->Log_Status, 'Warning', 'New class name color already exists!'
      return
    endif
  endfor

  *self.pClassNames   = [*self.pClassNames, New_Class_Name]
  *self.pLookUp       = [[*self.pLookUp], [NewColor]]

  self->Update_Available_Classes
  
  self.oStatus->Log_Status, 'Warning', 'Class name '''+New_Class_Name+'''added'
end
