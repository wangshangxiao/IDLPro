;+
; NAME:
; 
;   EOSap_Smoothing::handle_remove_scenario
;
; PURPOSE:
; 
;   This procedure methode is used to remove the selected scenario.
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
;   Obj->[EOSap_Smoothing::]handle_remove_scenario
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
pro EOSap_Smoothing::handle_remove_scenario
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~ptr_valid(self.pScenario) then return
   
  Selected = widget_info(widget_info(self.wWidget, find_by_uname='Wl_smooth_scenario'), /list_select)

  if Selected eq -1 then return

  Tmp = indgen(n_elements(*self.pScenario))
  T = where(Tmp eq Selected, complement=i, ncomplement=c)

  if c gt 0 then begin

    *self.pScenario = (*self.pScenario)[i]
    S = (*self.pScenario).Suffix
  endif else begin
  
    ptr_free, self.pScenario
    
    S = ''
  endelse
  
  widget_control, widget_info(self.wWidget, find_by_uname='Wl_smooth_scenario'), $
      set_value=S
end