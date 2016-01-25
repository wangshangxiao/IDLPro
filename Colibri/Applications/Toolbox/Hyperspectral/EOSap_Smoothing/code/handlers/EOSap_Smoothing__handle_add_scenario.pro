;+
; NAME:
; 
;   EOSap_Smoothing::handle_add_scenario
;
; PURPOSE:
; 
;   This procedure methode is used to add a new smoothing scenario.
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
;   Obj->[EOSap_Smoothing::]handle_add_scenario
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   None
; 
; RETURN VALUE:
; 
;   None 
;
; KNOWN ISSUES:
; 
;   None.
;
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, June 2010.
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
;-------------------------------------------------------------------------
pro EOSap_Smoothing::handle_add_scenario
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~ptr_valid(self.pSmoothFac) then begin
    self.oStatus->Log_Status, 'Warning', 'No smoothing factors defined.'
    return
  endif
    
  widget_control, widget_info(self.wWidget, find_by_uname='Wt_mask_suffix'), get_value=Mask_suffix

  if strlen(Mask_suffix) le 1 then begin
    self.oStatus->Log_Status, 'Warning', 'No smoothing mask suffix specified.'
    return  
  endif

  SmoothFac = *self.pSmoothFac

  if ptr_valid(self.pScenario) then begin  
    for Ix=0, n_elements(*self.pScenario)-1 do begin
      if (*self.pScenario)[Ix].Suffix eq Mask_suffix then return
    endfor
    
    *self.pScenario = [*self.pScenario, {Suffix:Mask_suffix, SmtFac:ptr_new(SmoothFac)}]
  endif else begin
    self.pScenario = ptr_new({Suffix:Mask_suffix, SmtFac:ptr_new(SmoothFac)})
  endelse
  
  widget_control, widget_info(self.wWidget, find_by_uname='Wl_smooth_scenario'), $
      set_value=(*self.pScenario).Suffix
  widget_control, widget_info(self.wWidget, find_by_uname='Wl_smooth_scenario'), $
      set_list_select = n_elements(*self.pScenario)-1
end