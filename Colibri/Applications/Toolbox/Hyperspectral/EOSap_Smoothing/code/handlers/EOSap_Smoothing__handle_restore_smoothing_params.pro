;+
; NAME:
; 
;   EOSap_Smoothing::handle_restore_smoothing_params
;
; PURPOSE:
; 
;   This procedure methode is used to restore the smoothing parameters from the specified smoothing 
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
;   Obj->[EOSap_Smoothing::]handle_restore_smoothing_params
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
pro EOSap_Smoothing::handle_restore_smoothing_params
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Smt_File = dialog_pickfile(Title='Select file for saving the smoothing parameters')
  
  if strlen(Smt_File) lt 3 then return
  
  nLines    = file_lines(Smt_File)
  Record    = ' ' 
  lun       = 0l
  nWl       = nLines - 1
  WLc       = 0   
  Scenario  = 0
  Wl        = fltarr(nWL)
  SmoothFac = intarr(nWL)  
        
  if nLines gt 0 then begin
    openr, lun, Smt_File, /get_lun
    readf, lun, Record
  endif
  
  if nLines eq 0 or (Record ne 'Smoothing scenario' and Record ne 'Smoothing factors') then begin
    self.oStatus->Log_Status, 'Error', 'Smoothing factors file not recognized'
    if lun ne 0 then free_lun, lun
    return
  endif

  widget_control, widget_info(self.wWidget, find_by_uname='Wt_Smoothing_params'), set_value=Smt_File

  if ptr_valid(self.pScenario) then ptr_free, self.pScenario
  if ptr_valid(self.pSmoothFac) then ptr_free, self.pSmoothFac
  
  while ~eof(lun) do begin
    readf, lun, Record
    Tmp       = strsplit(Record, ': ', /extract)
    
    case Tmp[0] of
    'Scenario': begin
      if n_elements(Tmp) gt 1 then begin
        Scenario = 1
        widget_control, widget_info(self.wWidget, find_by_uname='Wt_mask_suffix'), set_value=Tmp[1]
      endif
    end  
      
    'nWavelengths': begin
      nWL         = fix(Tmp[1])
      Wl          = fltarr(nWL)
      SmoothFac   = intarr(nWL)  
    end
    
    else: begin   
      Wl[WLc]          = float(Tmp[0])
      SmoothFac[WLc++] = fix(Tmp[1])
      
    endelse
    endcase
    
    if WLc eq nWL and Scenario eq 1 then begin
      WLc = 0
      
      if ptr_valid(self.pSmoothFac) then *self.pSmoothFac = SmoothFac else $
        self.pSmoothFac = ptr_new(SmoothFac)
        
      self->handle_add_scenario
    endif
  endwhile

  free_lun, lun
  
  if ptr_valid(self.pWl) then begin
  
  endif else begin
  
    self.pWl          = ptr_new(WL)

  endelse
  
  self.pSmoothFac   = ptr_new(SmoothFac) 
     
  self->update_smoothing_factors

  if obj_valid(self.oImage_ForSmoothFac) then begin
    
    self->handle_smooth_spectrum, Spectrum=Spectrum
  
    self.oProfileViewer->SetProperty, p2nd_Profile=ptr_new(Spectrum)
  endif
end