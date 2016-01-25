;+
; NAME:
; 
;   EOSap_Smoothing::update_smoothing_factors
;
; PURPOSE:
; 
;   This procedure method is used to update the Smoothing GUI with the new smoothing factors.
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
;   Obj->[EOSap_Smoothing::]update_smoothing_factors
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
pro EOSap_Smoothing::update_smoothing_factors
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~obj_valid(self.oImage_ForSmoothFac) then return

  self.oImage_ForSmoothFac->Get, pWL=pWL, pMapInfo=pMapInfo, pRGBdata=pRGBdata, nb=nb
  
  Smf = strarr(n_elements(*pWl))
  
  if ptr_valid(self.pSmoothFac) then begin
    
    i = where(*self.pSmoothFac eq -1, c)
    if c gt 0 then Smf[i] = 'Ignored'

    i = where(*self.pSmoothFac eq -2, c)
    if c gt 0 then Smf[i] = 'Zeroed'

    i = where(*self.pSmoothFac eq -3, c)
    if c gt 0 then Smf[i] = 'Removed'

    i = where(*self.pSmoothFac eq -4, c)
    if c gt 0 then Smf[i] = 'Interpolated'

    i = where(*self.pSmoothFac ne -1 and $
              *self.pSmoothFac ne -2 and $
              *self.pSmoothFac ne -3 and $
              *self.pSmoothFac ne -4, c)
            
    if c gt 0 then Smf[i] = 'Smooth factor: '+strtrim((*self.pSmoothFac)[i],2)
  endif

  if ptr_valid(self.pScenario) then begin         
    
    Selected    = widget_info(widget_info(self.wWidget, find_by_uname='Wl_smooth_scenario'), /list_select)
    
    *(*self.pScenario)[Selected].SmtFac = *self.pSmoothFac
  endif
  
  i = where(Smf ne '', C)

  BandsIx = widget_info(widget_info(self.wWidget, find_by_uname='Wl_Bands'), /list_select)
  
  BandsIx = BandsIx > 0
  
  widget_control, widget_info(self.wWidget, find_by_uname='Wl_Smooth_fac'), $
    set_value=Smf, set_list_top=BandsIx[0], Set_list_select=BandsIx
  widget_control, widget_info(self.wWidget, find_by_uname='Wl_Bands'), $
    set_list_top=BandsIx[0], Set_list_select=BandsIx

  widget_control, widget_info(self.wWidget, find_by_uname='Wl_AvB'), $
    set_value='Available bands : '+strtrim(string(nb),2)+' ; Selected : '+strtrim(string(C),2)
    
  self.oStatus->Log_Status, 'Ok'
end