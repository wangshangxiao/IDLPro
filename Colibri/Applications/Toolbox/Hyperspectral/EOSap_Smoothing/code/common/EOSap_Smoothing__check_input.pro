;+
; NAME:
; 
;   EOSap_Smoothing::check_input
;
; PURPOSE:
; 
;   This function method will check the required parameters before the actual smoothing process 
;   starts.
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
;   Obj->[EOSap_Smoothing::]check_input()
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
;   This function will return TRUE (1) if all required parameters are present else FALSE (0) is returned.
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
;------------------------------------------------------------------------------------------------------------
function EOSap_Smoothing::check_input
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
    
  if ~ptr_valid(self.pScenario) and ~ptr_valid(self.pSmoothFac) then begin   
    widget_control, widget_info(self.wWidget, find_by_uname='WID_TAB'), $
      set_tab_current=0    
    self.oStatus->Log_Status, 'Warning','Smoothing parameters missing !'
    return, 0
  endif    
  widget_control, widget_info(self.wWidget, find_by_uname='WID_TAB'), $
    set_tab_current=1
    
  if strlen(self.InDir) le 1 then begin
    self.oStatus->Log_Status, 'Warning','Input directory missing!'
    return, 0
  endif
    
  if strlen(self.OutDir) le 1 then begin
    self.oStatus->Log_Status, 'Warning','Output directory missing!'
    return, 0
  endif
  
  FilesIx = widget_info (widget_info(self.wWidget, find_by_uname='Wl_Image_list'), /list_select)

  if FilesIx[0] eq -1 then begin
    self.oStatus->Log_Status, 'Warning','Highlight images to be processed !'
    return, 0
  endif

  return, 1
end