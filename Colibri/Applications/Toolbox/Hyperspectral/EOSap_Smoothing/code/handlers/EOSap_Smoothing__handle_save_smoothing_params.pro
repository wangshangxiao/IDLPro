;+
; NAME:
; 
;   EOSap_Smoothing::handle_save_smoothing_params
;
; PURPOSE:
; 
;   This procedure methode is used to save the defined smoothing parameters to the requested smoothing
;   parameters text file.
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
;   Obj->[EOSap_Smoothing::]handle_save_smoothing_params
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
pro EOSap_Smoothing::handle_save_smoothing_params
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~ptr_valid(self.pSmoothFac) then return

  i = where(*self.pSmoothFac ne 0, C)

  if C eq 0 then begin
      self.oStatus->Log_Status, 'Warning', 'No smoothing factors defined.'
      return
  endif

  Smt_File = dialog_pickfile(Title='Select file for saving the smoothing parameters')
  
  if strlen(Smt_File) lt 3 then return
  
  Smt_File = (strsplit(Smt_File, '.', /extract))[0] + '.txt'  
  widget_control, widget_info(self.wWidget, find_by_uname='Wt_Smoothing_params'), set_value=Smt_File
  
  if ptr_valid(self.pScenario) then begin
    
    openw, lun, Smt_File, /get_lun
    printf, lun, 'Smoothing scenario'
    printf, lun, 'nWavelengths: '+strtrim(string(n_elements(*self.pWl)),2)
    
    for Ix=0, n_elements(*self.pScenario)-1 do begin
      printf, lun, 'Scenario: '+(*self.pScenario)[Ix].Suffix
  
      for Bix=0, n_elements(*(*self.pScenario)[Ix].SmtFac)-1 do begin
      
        printf, lun, string((*self.pWl)[Bix]) + ' ' + string((*(*self.pScenario)[Ix].SmtFac)[Bix])
      endfor
    endfor
    
    free_lun, lun  
    return
  endif
  
  if ptr_valid(self.pSmoothFac) then begin    
    
    openw, lun, Smt_File, /get_lun
    printf, lun, 'Smoothing factors'
  
    for Ix=0, n_elements(*self.pWl)-1 do begin
      printf, lun, string((*self.pWl)[Ix]) + ' ' + string((*self.pSmoothFac)[Ix])
    endfor

    free_lun, lun
  endif
end