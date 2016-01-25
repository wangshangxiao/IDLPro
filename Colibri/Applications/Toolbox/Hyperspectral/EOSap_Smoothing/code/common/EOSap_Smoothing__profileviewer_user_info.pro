;+
; NAME:
; 
;   EOSap_Smoothing::ProfileViewer_User_Info
;
; PURPOSE:
; 
;   This procedure method is called by the 'IOScocProfileViewer' object to inform the user of the object 
;   about an event that occured in the WIDGET_DRAW profile window. 
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
;   Obj->[EOSap_Smoothing::]
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
;   
;
; KNOWN ISSUES:
; 
;   None.
;
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, December 2012.
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
pro EOSap_Smoothing::ProfileViewer_User_Info, Info
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Selected = where(*self.pWl ge min(Info.plot_WL_Range) and *self.pWl  le max(Info.plot_WL_Range), C)

  if C le 0 then return

   widget_control, widget_info(self.wWidget, find_by_uname='Wl_Bands'), $
      set_list_select = Selected, set_list_top=Selected[0]
   
   widget_control, widget_info(self.wWidget, find_by_uname='Wl_Smooth_fac'), $
      set_list_select = Selected, set_list_top=Selected[0]
  
end