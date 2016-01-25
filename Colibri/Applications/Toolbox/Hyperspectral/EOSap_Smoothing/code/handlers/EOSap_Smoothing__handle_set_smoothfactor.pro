;+
; NAME:
; 
;   EOSap_Smoothing::handle_set_smoothfactor
;
; PURPOSE:
; 
;   This procedure methode is used to set the smoothing factors for the selected wavelengts.
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
;   Obj->[EOSap_Smoothing::]handle_set_smoothfactor, SmoothFac
;
; ARGUMENTS 
; 
;   SmoothFac:      A string indicating the smoothing factor to be applied. This can be 'Ignore', 'Zero_out',
;                   'Remove', 'Interpolate' or 'Smooth_factor' with the smooth value optained from the GUI.
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
pro EOSap_Smoothing::handle_set_smoothfactor, SmoothFac
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate


  BandsIx = widget_info(widget_info(self.wWidget, find_by_uname='Wl_Bands'), /list_select) 
  
  if (BandsIx)[0] eq -1 then begin
      self.oStatus->Log_Status, 'Warning', 'No bands selected.'
      return
  endif  
  
  self.oStatus->Log_Status, 'Ok'
   
  case SmoothFac of

  'Ignore':       (*self.pSmoothFac)[BandsIx] = -1
  'Zero_out':     (*self.pSmoothFac)[BandsIx] = -2 
  'Remove':       (*self.pSmoothFac)[BandsIx] = -3
  'Interpolate':  (*self.pSmoothFac)[BandsIx] = -4
  
  'Smooth_factor': begin

    widget_control, widget_info(self.wWidget, find_by_uname='Wt_sFac'), get_value=SmF
    
    if strlen(SmF) eq 0 then begin
        self.oStatus->Log_Status, 'Warning', 'Enter smooth factor!'
        return
    endif

    (*self.pSmoothFac)[BandsIx] = SmF
  end
  else:
  endcase
  
  self->update_smoothing_factors   
  self->handle_smooth_spectrum, Spectrum=Spectrum
  self.oProfileViewer->SetProperty, p2nd_Profile=ptr_new(Spectrum) 
end